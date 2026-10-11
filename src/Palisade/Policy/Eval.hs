-- | Evaluation of typed policies against a ledger state and an intent.
--
-- Evaluation is exact (rationals, no floating point) and short-circuits
-- @and@/@or@. Each rule yields a 'CheckResult' that says what was observed
-- and what was required, not just true or false; that is what makes
-- certificates explainable.
module Palisade.Policy.Eval
  ( EvalErr (..)
  , renderEvalErr
  , Ctx (..)
  , eval
  , evaluateIntent
  , evaluateLaws
  , LawResult (..)
  , blockFor
  , renderQ
  ) where


import qualified Data.Map.Strict as M
import Data.Ratio (denominator, numerator)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), secondsToNominalDiffTime, todHour, timeToTimeOfDay)
import Palisade.Certificate
import Palisade.Currency
import Palisade.Ledger
import Palisade.Money
import Palisade.Policy.Expr
import Palisade.Policy.Syntax (RuleKind (..), prettyDuration, quoteText)
import Palisade.Types

data EvalErr
  = DivByZero
  | NoIntent
  | UnknownAccountE Text
  deriving (Eq, Show)

renderEvalErr :: EvalErr -> Text
renderEvalErr = \case
  DivByZero -> "division by zero"
  NoIntent -> "needs an intent"
  UnknownAccountE a -> "unknown account " <> a

data Ctx c = Ctx
  { ctxSing :: SCurrency c
  , ctxState :: LedgerState
  , ctxNow :: UTCTime
  , ctxIntent :: Maybe (Intent, Money c)
  }

q :: Currency -> Integer -> Q c
q cur n = Q (minorToMajor cur n)

eval :: forall c a. Ctx c -> Expr c a -> Either EvalErr a
eval ctx = go
  where
    cur = fromSing (ctxSing ctx)
    st = ctxState ctx
    now = ctxNow ctx
    intent :: Either EvalErr (Intent, Money c)
    intent = maybe (Left NoIntent) Right (ctxIntent ctx)
    src = intentFrom . fst <$> intent
    window :: Expr c Seconds -> Either EvalErr [OutflowRec]
    window d = do
      Seconds s <- go d
      pure [r | r <- outflowsWithin now (secondsToNominalDiffTime (fromInteger s)) st, orCurrency r == cur]
    go :: forall b. Expr c b -> Either EvalErr b
    go = \case
      EBool b -> pure b
      ENum r -> pure r
      EMoney m -> pure m
      EText t -> pure t
      ESet _ xs -> pure xs
      EDur s -> pure s
      FAmount -> q cur . toMinor . snd <$> intent
      FCounterparty -> intentTo . fst <$> intent
      FSource -> src
      FActor -> intentActor . fst <$> intent
      FMemo -> intentMemo . fst <$> intent
      FKind -> intentKindText . intentKind . fst <$> intent
      FBalance -> (\a -> q cur (settledBalance a cur st)) <$> src
      FBalanceAfter -> do
        (i, m) <- intent
        pure (q cur (settledBalance (intentFrom i) cur st - toMinor m))
      FPending -> (\a -> q cur (pendingInbound a cur st)) <$> src
      FKnownCounterparty -> do
        (i, _) <- intent
        pure (hasPaid (intentFrom i) (intentTo i) st)
      QOutflow d -> do
        a <- src
        rs <- window d
        pure (q cur (sum [orMinor r | r <- rs, orSource r == a]))
      QCount d -> do
        a <- src
        rs <- window d
        pure (fromIntegral (length [() | r <- rs, orSource r == a]))
      QDuplicates d -> do
        (i, m) <- intent
        rs <- window d
        pure . fromIntegral $
          length
            [ ()
            | r <- rs
            , orSource r == intentFrom i
            , orDest r == External (intentTo i)
            , orMinor r == toMinor m
            ]
      QTypical d -> do
        (i, _) <- intent
        rs <- window d
        pure (q cur (maximum (0 : [orMinor r | r <- rs, orSource r == intentFrom i, orDest r == External (intentTo i)])))
      FHour -> pure (fromIntegral (todHour (timeToTimeOfDay (utctDayTime now))))
      FTotalBalance -> pure (q cur (totalInternal cur st))
      QBalanceOf a -> do
        acct <- go a
        if isInternal acct st then pure (q cur (settledBalance acct cur st)) else Left (UnknownAccountE acct)
      QOutflowOf a d -> do
        acct <- go a
        rs <- window d
        pure (q cur (sum [orMinor r | r <- rs, orSource r == acct]))
      QTotalOutflow d -> do
        rs <- window d
        pure (q cur (sum [orMinor r | r <- rs, isExternal (orDest r)]))
      ENot e -> not <$> go e
      EAnd l r -> go l >>= \b -> if b then go r else pure False
      EOr l r -> go l >>= \b -> if b then pure True else go r
      ECmp o t l r -> cmpWith t o <$> go l <*> go r
      EIn x s -> S.member <$> go x <*> go s
      ENotIn x s -> (\a b -> not (S.member a b)) <$> go x <*> go s
      MAdd l r -> qop (+) <$> go l <*> go r
      MSub l r -> qop (-) <$> go l <*> go r
      MScale n m -> (\k (Q x) -> Q (k * x)) <$> go n <*> go m
      MScaleR m n -> (\(Q x) k -> Q (x * k)) <$> go m <*> go n
      MDivN m n -> do
        Q x <- go m
        k <- go n
        if k == 0 then Left DivByZero else pure (Q (x / k))
      MRatio l r -> do
        Q x <- go l
        Q y <- go r
        if y == 0 then Left DivByZero else pure (x / y)
      MNeg m -> (\(Q x) -> Q (negate x)) <$> go m
      NAdd l r -> (+) <$> go l <*> go r
      NSub l r -> (-) <$> go l <*> go r
      NMul l r -> (*) <$> go l <*> go r
      NDiv l r -> do
        x <- go l
        y <- go r
        if y == 0 then Left DivByZero else pure (x / y)
      NNeg e -> negate <$> go e
    qop f (Q x) (Q y) = Q (f x y)
    isExternal = \case
      External _ -> True
      Internal _ -> False

cmpWith :: Ty c a -> CmpOp -> a -> a -> Bool
cmpWith t o a b = case t of
  TBool -> ordCmp o a b
  TNum -> ordCmp o a b
  TMoney -> ordCmp o a b
  TText -> ordCmp o a b
  TSet -> ordCmp o a b
  TDur -> ordCmp o a b
  where
    ordCmp :: Ord x => CmpOp -> x -> x -> Bool
    ordCmp op x y = case op of
      Lt -> x < y
      Le -> x <= y
      Gt -> x > y
      Ge -> x >= y
      Eq -> x == y
      Ne -> x /= y

------------------------------------------------------------------------------
-- Explanations

renderQ :: Currency -> Rational -> Text
renderQ cur r =
  let d = minorDigits cur
      scaled = r * (10 ^ d)
   in if denominator scaled == 1
        then renderMinor cur (numerator scaled) <> " " <> currencyCode cur
        else renderRational (d + 4) r <> " " <> currencyCode cur

renderVal :: Currency -> Ty c a -> a -> Text
renderVal cur t v = case t of
  TBool -> if v then "true" else "false"
  TNum -> renderRational 6 v
  TMoney -> let Q r = v in renderQ cur r
  TText -> quoteText v
  TSet -> "{" <> T.intercalate ", " (map quoteText (S.toAscList v)) <> "}"
  TDur -> let Seconds s = v in prettyDuration s

isLiteral :: Expr c a -> Bool
isLiteral = \case
  EBool _ -> True
  ENum _ -> True
  EMoney _ -> True
  EText _ -> True
  EDur _ -> True
  ESet _ _ -> True
  _ -> False

-- | (observed, required) for a boolean expression. A single comparison
-- reads as "lhs = value" against "op rhs". A compound condition lists the
-- value of every operand it compared, so a reader can see why it came out
-- the way it did.
explain :: forall c. KnownCurrency c => Ctx c -> Expr c Bool -> (Text, Text)
explain ctx e = case e of
  ECmp o t l r -> (side t l, cmpText o <> " " <> rhs t r)
  EIn x s -> (side TText x, "in " <> setLabel s)
  ENotIn x s -> (side TText x, "not in " <> setLabel s)
  ENot (EIn x s) -> (side TText x, "not in " <> setLabel s)
  _ -> (T.intercalate "; " (dedupe (atoms e)) <> " => " <> valOrErr TBool e, prettyTyped e)
  where
    cur = fromSing (ctxSing ctx)
    valOrErr :: Ty c a -> Expr c a -> Text
    valOrErr t x = either (\err -> "error (" <> renderEvalErr err <> ")") (renderVal cur t) (eval ctx x)
    side :: Ty c a -> Expr c a -> Text
    side t x
      | isLiteral x = valOrErr t x
      | otherwise = prettyTyped x <> " = " <> valOrErr t x
    rhs :: Ty c a -> Expr c a -> Text
    rhs t x
      | isLiteral x = valOrErr t x
      | otherwise = prettyTyped x <> " (= " <> valOrErr t x <> ")"
    setLabel s = case s of
      ESet (Just n) _ -> n
      _ -> prettyTyped s
    -- The non-literal operands of every comparison inside a compound condition.
    atoms :: Expr c Bool -> [Text]
    atoms = \case
      EAnd l r -> atoms l <> atoms r
      EOr l r -> atoms l <> atoms r
      ENot x -> atoms x
      ECmp _ t l r -> [side t x | x <- [l, r], not (isLiteral x)]
      EIn x _ -> [side TText x | not (isLiteral x)]
      ENotIn x _ -> [side TText x | not (isLiteral x)]
      x -> [side TBool x | not (isLiteral x)]
    dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

evalRule :: KnownCurrency c => Ctx c -> Rule c -> CheckResult
evalRule ctx r =
  let (obs, req) = explain ctx (ruleExpr r)
      kind = case ruleKind r of
        RequireRule -> RequireCheck
        EscalateRule -> EscalateCheck
      outcome = case (eval ctx (ruleExpr r), ruleKind r) of
        (Left _, _) -> Errored
        (Right True, RequireRule) -> Pass
        (Right False, RequireRule) -> Fail
        (Right True, EscalateRule) -> Triggered
        (Right False, EscalateRule) -> Pass
      req' = case ruleKind r of
        RequireRule -> req
        EscalateRule -> "escalates when " <> req
   in CheckResult (ruleName r) kind outcome obs req'

blockFor :: Policy -> Currency -> Maybe SomeBlock
blockFor pol c = M.lookup c (policyBlocks pol)

-- | Evaluate every rule of the block for the intent's currency. 'Nothing'
-- means the policy has no block for that currency.
evaluateIntent :: Policy -> LedgerState -> Intent -> Maybe (Text, [CheckResult])
evaluateIntent pol st i = case blockFor pol (intentCurrency i) of
  Nothing -> Nothing
  Just (SomeBlock s blk) -> case matchMoney s (intentAmount i) of
    Nothing -> Nothing
    Just m ->
      let ctx = Ctx s st (intentTs i) (Just (i, m))
       in Just (pbName blk, map (evalRule ctx) (pbRules blk))

data LawResult = LawResult
  { lrName :: Text
  , lrCurrency :: Currency
  , lrHolds :: Either EvalErr Bool
  , lrObserved :: Text
  , lrRequired :: Text
  }

-- | Evaluate every law of every block against a state, at the state's own
-- time.
evaluateLaws :: Policy -> LedgerState -> [LawResult]
evaluateLaws pol st = case lsTime st of
  Nothing -> []
  Just now -> concatMap (one now) (M.elems (policyBlocks pol))
  where
    one now (SomeBlock s blk) =
      [ let ctx = Ctx s st now Nothing
            (obs, req) = explain ctx (lawExpr l)
         in LawResult (lawName l) (fromSing s) (eval ctx (lawExpr l)) obs req
      | l <- pbLaws blk
      ]

