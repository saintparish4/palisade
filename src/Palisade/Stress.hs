-- | Adversarial stress testing.
--
-- Generates event sequences shaped like the failures of 2025-2026
-- (amounts right at each policy threshold, bursts at one timestamp,
-- retries of executed intents, withdrawals racing pending deposits, new
-- payees, balance-draining amounts, extreme FX rates), runs them through
-- the kernel, checks every law after every step, and shrinks the first
-- violation of each law to a minimal counterexample.
--
-- Kernel laws are checked for every policy. Policy laws are the @law@
-- items in the policy file: properties that the rules are supposed to
-- guarantee, which the stress tester tries to break.
module Palisade.Stress
  ( Authorizer
  , StressConfig (..)
  , defaultStressConfig
  , Action (..)
  , Step (..)
  , Violation (..)
  , TraceLine (..)
  , CaseResult (..)
  , runCase
  , GenEnv (..)
  , mkGenEnv
  , genCase
  , shrinkSteps
  , shrinkCase
  , LawStatus (..)
  , Counterexample (..)
  , StressReport (..)
  , runStress
  , kernelLaws
  ) where


import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, isNothing)
import Data.Ratio ((%))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Palisade.Authorize
import Palisade.Certificate
import Palisade.Chain
import Palisade.Currency
import Palisade.Ledger
import Palisade.Money
import Palisade.Policy
import Palisade.Types
import Test.QuickCheck (Gen, chooseInt, chooseInteger, elements, frequency, shrinkList)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

type Authorizer = Policy -> LedgerState -> Intent -> Decision

data StressConfig = StressConfig
  { scPolicy :: Policy
  , scSetupLog :: [LogEntry]
  -- ^ Events that build the starting state (accounts, deposits).
  , scCount :: Int
  , scSeed :: Int
  , scMaxSteps :: Int
  , scOnlyCase :: Maybe Int
  , scAuthorizer :: Authorizer
  }

defaultStressConfig :: Policy -> [LogEntry] -> StressConfig
defaultStressConfig pol setup = StressConfig pol setup 500 42 30 Nothing authorize

data Action
  = AIntent
      { aId :: Text
      , aKind :: IntentKind
      , aFrom :: Text
      , aTo :: Text
      , aCur :: Currency
      , aMinor :: Integer
      , aShape :: Text
      }
  | ADeposit
      { aTid :: Text
      , aAccount :: Text
      , aCur :: Currency
      , aMinor :: Integer
      , aSettled :: Bool
      }
  | ASettle Text
  | AReverse Text
  deriving (Eq, Show)

-- | An action and the seconds elapsed since the previous step.
data Step = Step {stDelay :: Integer, stAction :: Action}
  deriving (Eq, Show)

data Violation = Violation
  { vLaw :: Text
  , vStep :: Int
  , vDetail :: Text
  }
  deriving (Eq, Show)

data TraceLine = TraceLine
  { tlStep :: Int
  , tlOffset :: Integer
  , tlAction :: Text
  , tlResult :: Text
  }
  deriving (Eq, Show)

data CaseResult = CaseResult
  { crViolation :: Maybe Violation
  , crTrace :: [TraceLine]
  , crVerdicts :: Map Verdict Int
  }

kernelLaws :: [(Text, Text)]
kernelLaws =
  [ ("trial-balance", "every currency sums to zero across all accounts")
  , ("no-overdraft", "no internal account's settled balance goes negative")
  , ("no-unauthorized-movement", "state changes only when the verdict is allow")
  , ("idempotency", "no intent id executes twice")
  , ("certificate-consistency", "each certificate names the state it was judged against and the state it produced")
  , ("replay-determinism", "replaying the resulting log from genesis reproduces the same state")
  ]

startTime :: LedgerState -> UTCTime
startTime st = fromMaybe (UTCTime (fromGregorian 2027 1 1) 0) (lsTime st)

actionText :: Action -> Text
actionText = \case
  AIntent i k f t c n shape ->
    intentKindText k <> " " <> i <> " " <> f <> " -> " <> t <> " " <> renderMinor c n <> " " <> currencyCode c
      <> case k of
        Convert tc r -> " into " <> currencyCode tc <> " @ " <> renderRational 9 r
        _ -> ""
      <> " [" <> shape <> "]"
  ADeposit tid a c n s ->
    "deposit " <> tid <> " -> " <> a <> " " <> renderMinor c n <> " " <> currencyCode c <> (if s then " (settled)" else " (pending)")
  ASettle tid -> "settle " <> tid
  AReverse tid -> "reverse " <> tid

toIntent :: UTCTime -> Action -> Maybe Intent
toIntent ts = \case
  AIntent i k f t c n shape -> Just (Intent i ts "stress" k f t (mkSomeMoney c n) shape)
  _ -> Nothing

toEvent :: UTCTime -> Action -> Maybe Event
toEvent ts = \case
  ADeposit tid a c n s -> Just (EvDeposit ts tid a "stress-source" (mkSomeMoney c n) s)
  ASettle tid -> Just (EvSettle ts tid)
  AReverse tid -> Just (EvReverse ts tid)
  AIntent {} -> Nothing

data Run = Run
  { rState :: !LedgerState
  , rEntries :: [LogEntry]
  , rExecuted :: !(S.Set Text)
  , rOffset :: !Integer
  , rTrace :: [TraceLine]
  , rVerdicts :: !(Map Verdict Int)
  }

-- | Run one case; stop at the first violation.
runCase :: StressConfig -> [Step] -> CaseResult
runCase cfg steps = case replayLog (scSetupLog cfg) of
  Left err -> CaseResult (Just (Violation "setup" 0 (renderReplayError err))) [] M.empty
  Right setup -> go (Run setup [] S.empty 0 [] M.empty) (zip [1 ..] steps)
  where
    pol = scPolicy cfg
    finish r v = CaseResult v (reverse (rTrace r)) (rVerdicts r)
    go r [] =
      let produced = scSetupLog cfg <> reverse (rEntries r)
       in case replayLog produced of
            Right st' | st' == rState r -> finish r Nothing
            Right _ -> finish r (Just (Violation "replay-determinism" (length steps) "replay produced a different state"))
            Left err -> finish r (Just (Violation "replay-determinism" (length steps) (renderReplayError err)))
    go r ((k, Step d a) : rest) =
      let off = rOffset r + d
          ts = addUTCTime (fromInteger off) start0
          st = rState r
          line = TraceLine k off (actionText a)
       in case (toIntent ts a, toEvent ts a) of
            (Just i, _) ->
              let dec = scAuthorizer cfg pol st i
                  v = decVerdict dec
                  cert = decCertificate dec
                  st' = decState dec
                  failing = [crRule c | c <- certChecks cert, isBlocking c]
                  res = verdictText v <> (if null failing then "" else " (" <> T.intercalate ", " failing <> ")")
                  r' =
                    r
                      { rState = st'
                      , rEntries = maybe (rEntries r) (: rEntries r) (decEntry dec)
                      , rExecuted = if v == Allow then S.insert (intentId i) (rExecuted r) else rExecuted r
                      , rOffset = off
                      , rTrace = line res : rTrace r
                      , rVerdicts = M.insertWith (+) v 1 (rVerdicts r)
                      }
                  violation
                    | v /= Allow && st' /= st =
                        Just (Violation "no-unauthorized-movement" k ("verdict " <> verdictText v <> " but the ledger changed"))
                    | v == Allow && intentId i `S.member` rExecuted r =
                        Just (Violation "idempotency" k ("intent " <> intentId i <> " executed a second time"))
                    | v == Allow && (certPreSeq cert, certPreHead cert, certPostSeq cert, certPostHead cert) /= (lsSeq st, lsHead st, Just (lsSeq st'), Just (lsHead st')) =
                        Just (Violation "certificate-consistency" k "certificate does not match the states before and after")
                    | otherwise = stateLaws k st'
               in maybe (go r' rest) (finish r' . Just) violation
            (_, Just ev) -> case appendEvent st ev of
              Right (st', e) ->
                let r' = r {rState = st', rEntries = e : rEntries r, rOffset = off, rTrace = line "applied" : rTrace r}
                 in maybe (go r' rest) (finish r' . Just) (stateLaws k st')
              Left err ->
                go r {rOffset = off, rTrace = line ("ledger refused: " <> renderApplyError err) : rTrace r} rest
            _ -> go r rest
    start0 = case replayLog (scSetupLog cfg) of
      Right s -> startTime s
      Left _ -> startTime emptyLedger
    stateLaws k st
      | any (/= 0) (M.elems (trialBalance st)) =
          Just (Violation "trial-balance" k "a currency no longer sums to zero")
      | ((a, c, n) : _) <- [x | x@(_, _, n') <- internalBalances st, n' < 0] =
          Just (Violation "no-overdraft" k (a <> " is at " <> renderMinor c n <> " " <> currencyCode c))
      | otherwise = case [lr | lr <- evaluateLaws pol st, lrHolds lr /= Right True] of
          [] -> Nothing
          (lr : _) ->
            Just
              ( Violation
                  (lawLabel lr)
                  k
                  ( case lrHolds lr of
                      Left err -> "could not be evaluated: " <> renderEvalErr err
                      _ -> "observed " <> lrObserved lr <> "; required " <> lrRequired lr
                  )
              )

lawLabel :: LawResult -> Text
lawLabel lr = "law \"" <> lrName lr <> "\" (" <> currencyCode (lrCurrency lr) <> ")"

------------------------------------------------------------------------------
-- Generation

data GenEnv = GenEnv
  { geAccounts :: [Text]
  , geCurrencies :: [Currency]
  , gePayees :: [Text]
  , geThresholds :: Map Currency [Integer]
  , geBalances :: Map (Text, Currency) Integer
  }

mkGenEnv :: Policy -> LedgerState -> GenEnv
mkGenEnv pol st =
  let accounts = S.toList (lsAccounts st)
      curs = M.keys (policyBlocks pol)
      known = concatMap S.toList (M.elems (policySets pol)) <> concatMap S.toList (M.elems (lsPayees st))
      payees = S.toList (S.fromList (known <> ["attacker-1", "attacker-2", "new-vendor"]))
      thresholds =
        M.fromList
          [ (c, S.toList (S.fromList [ceiling (r * (10 ^ minorDigits c)) | r <- lits, r > 0]))
          | (c, SomeBlock _ blk) <- M.toList (policyBlocks pol)
          , let lits = concatMap (moneyLiterals . ruleExpr) (pbRules blk) <> concatMap (moneyLiterals . lawExpr) (pbLaws blk)
          ]
      bals = M.fromList [((a, c), n) | (a, c, n) <- internalBalances st]
   in GenEnv
        (if null accounts then ["treasury"] else accounts)
        (if null curs then [USD] else curs)
        payees
        thresholds
        bals

data Shape = Single | Burst | Retry | PendingRace | Admin

genCase :: GenEnv -> Int -> Gen [Step]
genCase env maxSteps = do
  n <- chooseInt (1, max 1 maxSteps)
  go n (0 :: Int) [] []
  where
    go budget idx deps prev
      | budget <= 0 = pure []
      | otherwise = do
          shape <- frequency [(10, pure Single), (2, pure Burst), (2, pure Retry), (1, pure PendingRace), (3, pure Admin)]
          (steps, deps', prev') <- case shape of
            Single -> do
              d <- genDelay
              a <- genIntent idx
              pure ([Step d a], deps, a : prev)
            Burst -> do
              -- Salami: several payments from one source, often the same
              -- amount just under a threshold, often to the same payee.
              m <- chooseInt (2, 8)
              (from, c) <- pickSource
              samePayee <- elements [True, False]
              sameAmount <- elements [True, False]
              payee0 <- elements (gePayees env)
              amount0 <- genAmount c from
              gap <- elements [0, 0, 1, 60, 600]
              as <- mapM
                ( \j -> do
                    to <- if samePayee then pure payee0 else elements (gePayees env)
                    n <- if sameAmount then pure amount0 else genAmount c from
                    pure (AIntent ("st-" <> tshow idx <> "-" <> tshow j) Pay from to c n "salami-burst")
                )
                [1 .. m]
              d <- genDelay
              case as of
                (a1 : more) -> pure (Step d a1 : [Step gap a | a <- more], deps, reverse as <> prev)
                [] -> pure ([], deps, prev)
            Retry -> case [a | a@AIntent {} <- prev] of
              [] -> do
                a <- genIntent idx
                pure ([Step 0 a], deps, a : prev)
              ps -> do
                orig <- elements ps
                sameId <- elements [True, False]
                d <- elements [0, 1, 30, 300]
                let a = if sameId then orig {aShape = "retry-same-id"} else orig {aId = "st-" <> tshow idx, aShape = "retry-new-id"}
                pure ([Step d a], deps, a : prev)
            PendingRace -> do
              (acct, c) <- pickSource
              big <- genAmount c acct
              let tid = "dep-" <> tshow idx
              payee <- elements (gePayees env)
              let dep = ADeposit tid acct c big False
                  spend = AIntent ("st-" <> tshow idx) Pay acct payee c big "pending-race"
              revFirst <- elements [True, False]
              let tailSteps = if revFirst then [Step 1 (AReverse tid), Step 1 spend] else [Step 1 spend, Step 60 (AReverse tid)]
              pure (Step 0 dep : tailSteps, tid : deps, spend : prev)
            Admin -> do
              d <- genDelay
              case deps of
                [] -> do
                  a <- genDeposit idx
                  pure ([Step d a], aTid a : deps, prev)
                _ -> do
                  which <- chooseInt (0, 2)
                  tid <- elements deps
                  if which == 0
                    then do
                      a <- genDeposit idx
                      pure ([Step d a], aTid a : deps, prev)
                    else pure ([Step d (if which == 1 then ASettle tid else AReverse tid)], deps, prev)
          rest <- go (budget - length steps) (idx + 1) deps' prev'
          pure (steps <> rest)

    genDelay = frequency [(3, pure 0), (4, chooseInteger (1, 600)), (2, chooseInteger (3600, 86400)), (1, chooseInteger (86400, 7 * 86400))]

    genDeposit idx = do
      acct <- elements (geAccounts env)
      c <- elements (geCurrencies env)
      n <- genAmount c acct
      s <- elements [True, False]
      pure (ADeposit ("dep-" <> tshow idx) acct c n s)

    genIntent idx = do
      k <- frequency [(7, pure "pay"), (2, pure "transfer"), (1, pure "convert")]
      case k :: Text of
        "transfer" | length (geAccounts env) >= 2 -> do
          (from, c) <- pickSource
          to <- elements (case filter (/= from) (geAccounts env) of [] -> [from]; xs -> xs)
          n <- genAmount c from
          pure (AIntent ("st-" <> tshow idx) TransferInternal from to c n "transfer")
        "convert" -> do
          (from, c) <- pickSource
          tc <- elements (filter (/= c) allCurrencies)
          r <- elements [1 % 1000000000, 1 % 3, 92 % 100, 1, 10001 % 10000, 3, 1000000000]
          n <- genAmount c from
          pure (AIntent ("st-" <> tshow idx) (Convert tc r) from from c n "fx-extreme")
        _ -> genPayLike idx "pay"

    genPayLike idx shape = do
      (from, c) <- pickSource
      to <- elements (gePayees env)
      n <- genAmount c from
      pure (AIntent ("st-" <> tshow idx) Pay from to c n shape)

    -- Mostly an account that actually holds the currency, sometimes not.
    pickSource = do
      let funded = [k | (k, n) <- M.toList (geBalances env), n > 0, snd k `elem` geCurrencies env]
          anyPair = (,) <$> elements (geAccounts env) <*> elements (geCurrencies env)
      if null funded then anyPair else frequency [(6, elements funded), (1, anyPair)]

    genAmount c acct = do
      let d = minorDigits c
          ts = M.findWithDefault [] c (geThresholds env)
          bal = M.findWithDefault 0 (acct, c) (geBalances env)
          boundary = concat [[t - 1, t, t + 1, (t + 1) `div` 2, (t + 2) `div` 3] | t <- ts]
          balanceLike = [b | bal > 0, b <- [bal, bal `div` 2, bal - 1, bal + 1], b > 0]
          small = frequency [(4, chooseInteger (1, 10 ^ (d + 4))), (1, pure 1), (1, pure (10 ^ (d + 9)))]
      n <-
        frequency $
          [(5, elements boundary) | not (null boundary)]
            <> [(3, elements balanceLike) | not (null balanceLike)]
            <> [(3, small)]
      pure (max 1 n)

tshow :: Show a => a -> Text
tshow = T.pack . show

------------------------------------------------------------------------------
-- Shrinking

shrinkSteps :: [Step] -> [[Step]]
shrinkSteps = shrinkList shrinkStep
  where
    shrinkStep (Step d a) = [Step d' a | d' <- shrinkDelay d] <> [Step d a' | a' <- shrinkAction a]
    shrinkDelay d = [0 | d /= 0] <> [d `div` 2 | d > 1]
    shrinkAction = \case
      a@AIntent {aMinor = n} -> [a {aMinor = n `div` 2} | n > 1]
      a@ADeposit {aMinor = n} -> [a {aMinor = n `div` 2} | n > 1]
      _ -> []

-- | Greedily shrink a failing case while it still violates the same law.
shrinkCase :: StressConfig -> Text -> [Step] -> [Step]
shrinkCase cfg lawName = go (2000 :: Int)
  where
    stillFails s = case crViolation (runCase cfg s) of
      Just v -> vLaw v == lawName
      Nothing -> False
    go 0 s = s
    go budget s = case filter stillFails (take 400 (shrinkSteps s)) of
      (s' : _) -> go (budget - 1) s'
      [] -> s

------------------------------------------------------------------------------
-- Driver

data Counterexample = Counterexample
  { ceCase :: Int
  , ceOriginalLength :: Int
  , ceSteps :: [Step]
  , ceTrace :: [TraceLine]
  , ceViolation :: Violation
  }

data LawStatus = LawStatus
  { lsLawName :: Text
  , lsDescription :: Text
  , lsCounterexample :: Maybe Counterexample
  }

data StressReport = StressReport
  { srCases :: Int
  , srSteps :: Int
  , srSeed :: Int
  , srLaws :: [LawStatus]
  , srVerdicts :: Map Verdict Int
  }

runStress :: StressConfig -> StressReport
runStress cfg =
  let setup = fromRight emptyLedger (replayLog (scSetupLog cfg))
      env = mkGenEnv (scPolicy cfg) setup
      cases = maybe [0 .. scCount cfg - 1] pure (scOnlyCase cfg)
      gen i = unGen (genCase env (scMaxSteps cfg)) (mkQCGen (scSeed cfg + i)) 30
      (found, nSteps, verdicts) = foldl' (runOne gen) (M.empty, 0, M.empty) cases
      policyLaws =
        [ ("law \"" <> lawName l <> "\" (" <> currencyCode c <> ")", prettyTyped (lawExpr l))
        | (c, SomeBlock _ blk) <- M.toList (policyBlocks (scPolicy cfg))
        , l <- pbLaws blk
        ]
      allLaws = kernelLaws <> policyLaws
      extra = [(n, "") | n <- M.keys found, isNothing (lookup n allLaws)]
   in StressReport
        (length cases)
        nSteps
        (scSeed cfg)
        [LawStatus n d (M.lookup n found) | (n, d) <- allLaws <> extra]
        verdicts
  where
    runOne gen (found, nSteps, verdicts) i =
      let steps = gen i
          res = runCase cfg steps
          verdicts' = M.unionWith (+) verdicts (crVerdicts res)
          nSteps' = nSteps + length steps
       in case crViolation res of
            Just v
              | not (vLaw v `M.member` found) ->
                  let small = shrinkCase cfg (vLaw v) steps
                      res' = runCase cfg small
                      ce = Counterexample i (length steps) small (crTrace res') (fromMaybe v (crViolation res'))
                   in (M.insert (vLaw v) ce found, nSteps', verdicts')
            _ -> (found, nSteps', verdicts')

