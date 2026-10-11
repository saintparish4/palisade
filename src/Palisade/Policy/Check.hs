-- | Typechecking: untyped @.pal@ syntax to the typed 'Expr' GADT.
--
-- This is where "mixing dollars and euros" and "comparing money with a
-- bare number" are rejected, with the source position of the offending
-- operator.
module Palisade.Policy.Check
  ( TypeError (..)
  , renderTypeError
  , Context (..)
  , checkPolicyFile
  , checkBoolExpr
  ) where

import Control.Monad (forM, unless, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Type.Equality ((:~:) (..))
import Palisade.Currency
import Palisade.Policy.Expr
import Palisade.Policy.Syntax

data TypeError = TypeError Pos Text
  deriving (Eq, Show)

renderTypeError :: FilePath -> TypeError -> Text
renderTypeError path (TypeError p msg) = T.pack path <> ":" <> renderPos p <> ": " <> msg

-- | Rules see an intent; laws only see the ledger.
data Context = RuleContext | LawContext
  deriving (Eq, Show)

data Env c = Env
  { envSing :: SCurrency c
  , envSets :: Map Text (Set Text)
  , envCtx :: Context
  }

data SomeExpr c where
  SomeExpr :: Ty c a -> Expr c a -> SomeExpr c

typeErr :: Pos -> Text -> Either TypeError a
typeErr p = Left . TypeError p

checkPolicyFile :: PolicyFile -> Either TypeError Policy
checkPolicyFile (PolicyFile decls) = do
  sets <- collectSets [(p, n, xs) | SSetDecl p n xs <- decls]
  let policies = [(p, n, c, items) | SPolicy p n c items <- decls]
  checkUnique "currency" [(p, currencyCode c) | (p, _, c, _) <- policies] $
    \c -> "a second policy for " <> c <> "; put all " <> c <> " rules in one policy block"
  checkUnique "policy name" [(p, n) | (p, n, _, _) <- policies] $
    \n -> "a second policy named " <> quoteText n
  blocks <- forM policies $ \(_, n, c, items) -> case toSomeCurrency c of
    SomeCurrency s -> SomeBlock s <$> checkBlock s sets n items
  pure (mkPolicy blocks)

collectSets :: [(Pos, Text, [Text])] -> Either TypeError (Map Text (Set Text))
collectSets = go M.empty
  where
    go acc [] = Right acc
    go acc ((p, n, xs) : rest)
      | n `M.member` acc = typeErr p ("set " <> n <> " is declared twice")
      | n `elem` reservedNames = typeErr p (n <> " is a built-in name and cannot name a set")
      | otherwise = go (M.insert n (S.fromList xs) acc) rest

checkUnique :: Text -> [(Pos, Text)] -> (Text -> Text) -> Either TypeError ()
checkUnique _ xs msg = go S.empty xs
  where
    go _ [] = Right ()
    go seen ((p, x) : rest)
      | x `S.member` seen = typeErr p (msg x)
      | otherwise = go (S.insert x seen) rest

checkBlock :: KnownCurrency c => SCurrency c -> Map Text (Set Text) -> Text -> [SItem] -> Either TypeError (PolicyBlock c)
checkBlock s sets n items = do
  checkUnique "rule" [(p, nm) | SRule p _ nm _ <- items] $ \nm -> "a second rule named " <> quoteText nm <> " in policy " <> quoteText n
  checkUnique "law" [(p, nm) | SLaw p nm _ <- items] $ \nm -> "a second law named " <> quoteText nm <> " in policy " <> quoteText n
  rules <- forM [(k, nm, e) | SRule _ k nm e <- items] $ \(k, nm, e) ->
    Rule k nm <$> checkBoolExpr (Env s sets RuleContext) e
  laws <- forM [(nm, e) | SLaw _ nm e <- items] $ \(nm, e) ->
    Law nm <$> checkBoolExpr (Env s sets LawContext) e
  pure (PolicyBlock n rules laws)

checkBoolExpr :: KnownCurrency c => Env c -> SExpr -> Either TypeError (Expr c Bool)
checkBoolExpr env e = do
  SomeExpr t x <- infer env e
  case eqTy t TBool of
    Just Refl -> Right x
    Nothing -> typeErr (sPos e) ("a rule must be a true/false condition, but this is " <> tyName t)

check :: KnownCurrency c => Env c -> Ty c a -> SExpr -> Either TypeError (Expr c a)
check env want e = do
  SomeExpr got x <- infer env e
  case eqTy want got of
    Just Refl -> Right x
    Nothing -> typeErr (sPos e) ("expected " <> tyName want <> " but found " <> tyName got <> hint want got)

hint :: Ty c a -> Ty c b -> Text
hint want got = case (want, got) of
  (TMoney, TNum) -> " (write a money literal with its currency, e.g. 250 USD)"
  (TDur, TNum) -> " (write a duration with a unit, e.g. 24h, 30m, 7d)"
  _ -> ""

-- | Names that need an intent.
intentFields :: [Text]
intentFields =
  ["amount", "counterparty", "source", "actor", "memo", "kind", "balance", "balance_after", "pending", "known_counterparty"]

intentFunctions :: [Text]
intentFunctions = ["outflow", "count", "duplicates", "typical"]

stateNames :: [Text]
stateNames = ["hour", "total_balance"]

stateFunctions :: [Text]
stateFunctions = ["balance_of", "outflow_of", "total_outflow"]

reservedNames :: [Text]
reservedNames = intentFields <> intentFunctions <> stateNames <> stateFunctions

infer :: forall c. KnownCurrency c => Env c -> SExpr -> Either TypeError (SomeExpr c)
infer env (SExpr p node) = case node of
  SNum r -> ok TNum (ENum r)
  SMoney r c
    | c == blockCur -> ok TMoney (EMoney (Q r))
    | otherwise ->
        typeErr p $
          "this policy is for " <> currencyCode blockCur <> " but the literal is in " <> currencyCode c
            <> "; PALISADE never converts currencies implicitly"
  SDur s
    | s <= 0 -> typeErr p "durations must be positive"
    | otherwise -> ok TDur (EDur (Seconds s))
  SStr t -> ok TText (EText t)
  SBool b -> ok TBool (EBool b)
  SSetLit xs -> ok TSet (ESet Nothing (S.fromList xs))
  SVar v -> var v
  SCall f args -> callFn f args
  SUn OpNot e -> SomeExpr TBool . ENot <$> check env TBool e
  SUn OpNeg e -> do
    SomeExpr t x <- infer env e
    case t of
      TMoney -> ok TMoney (MNeg x)
      TNum -> ok TNum (NNeg x)
      _ -> typeErr p ("cannot negate " <> tyName t)
  SBin o l r -> binary o l r
  where
    blockCur = fromSing (envSing env)
    ok :: Ty c a -> Expr c a -> Either TypeError (SomeExpr c)
    ok t x = Right (SomeExpr t x)

    needIntent :: Text -> Either TypeError ()
    needIntent n =
      when (envCtx env == LawContext) $
        typeErr p (n <> " needs an intent, but laws are checked against the ledger alone; use total_balance, total_outflow, balance_of or outflow_of")

    var v
      | v `elem` intentFields = needIntent v >> intentField v
      | v == "hour" = ok TNum FHour
      | v == "total_balance" = ok TMoney FTotalBalance
      | Just xs <- M.lookup v (envSets env) = ok TSet (ESet (Just v) xs)
      | v `elem` (intentFunctions <> stateFunctions) = typeErr p (v <> " is a function; call it with arguments, e.g. " <> v <> "(24h)")
      | otherwise =
          typeErr p $
            "unknown name " <> v <> ". Known names: "
              <> T.intercalate ", " (intentFields <> stateNames <> M.keys (envSets env))

    intentField = \case
      "amount" -> ok TMoney FAmount
      "counterparty" -> ok TText FCounterparty
      "source" -> ok TText FSource
      "actor" -> ok TText FActor
      "memo" -> ok TText FMemo
      "kind" -> ok TText FKind
      "balance" -> ok TMoney FBalance
      "balance_after" -> ok TMoney FBalanceAfter
      "pending" -> ok TMoney FPending
      _ -> ok TBool FKnownCounterparty

    arity f n args =
      unless (length args == n) $
        typeErr p (f <> " takes " <> T.pack (show n) <> " argument" <> (if n == 1 then "" else "s") <> ", got " <> T.pack (show (length args)))

    callFn f args = case f of
      _ | f `elem` intentFunctions -> do
        needIntent f
        arity f 1 args
        d <- check env TDur (head args)
        case f of
          "outflow" -> ok TMoney (QOutflow d)
          "count" -> ok TNum (QCount d)
          "duplicates" -> ok TNum (QDuplicates d)
          _ -> ok TMoney (QTypical d)
      "total_outflow" -> do
        arity f 1 args
        ok TMoney . QTotalOutflow =<< check env TDur (head args)
      "balance_of" -> do
        arity f 1 args
        ok TMoney . QBalanceOf =<< check env TText (head args)
      "outflow_of" -> do
        arity f 2 args
        a <- check env TText (head args)
        d <- check env TDur (args !! 1)
        ok TMoney (QOutflowOf a d)
      _ ->
        typeErr p $
          "unknown function " <> f <> ". Known functions: " <> T.intercalate ", " (intentFunctions <> stateFunctions)

    binary o l r = case o of
      OpAnd -> SomeExpr TBool <$> (EAnd <$> check env TBool l <*> check env TBool r)
      OpOr -> SomeExpr TBool <$> (EOr <$> check env TBool l <*> check env TBool r)
      OpIn -> SomeExpr TBool <$> (EIn <$> check env TText l <*> check env TSet r)
      OpNotIn -> SomeExpr TBool <$> (ENotIn <$> check env TText l <*> check env TSet r)
      _
        | Just c <- cmpOf o -> do
            SomeExpr tl xl <- infer env l
            SomeExpr tr xr <- infer env r
            case eqTy tl tr of
              Nothing ->
                typeErr p ("cannot compare " <> tyName tl <> " with " <> tyName tr <> hint tl tr <> hint tr tl)
              Just Refl -> do
                let ordered = c `elem` [Lt, Le, Gt, Ge]
                case tl of
                  TSet -> typeErr p "sets cannot be compared; use in or not in"
                  TBool | ordered -> typeErr p "true/false values have no order; use == or !="
                  TText | ordered -> typeErr p "text has no order here; use ==, != or in"
                  _ -> ok TBool (ECmp c tl xl xr)
      _ -> do
        SomeExpr tl xl <- infer env l
        SomeExpr tr xr <- infer env r
        arith o tl xl tr xr

    arith :: BinOp -> Ty c a -> Expr c a -> Ty c b -> Expr c b -> Either TypeError (SomeExpr c)
    arith o tl xl tr xr = case (o, tl, tr) of
      (OpAdd, TMoney, TMoney) -> ok TMoney (MAdd xl xr)
      (OpSub, TMoney, TMoney) -> ok TMoney (MSub xl xr)
      (OpAdd, TNum, TNum) -> ok TNum (NAdd xl xr)
      (OpSub, TNum, TNum) -> ok TNum (NSub xl xr)
      (OpMul, TNum, TMoney) -> ok TMoney (MScale xl xr)
      (OpMul, TMoney, TNum) -> ok TMoney (MScaleR xl xr)
      (OpMul, TNum, TNum) -> ok TNum (NMul xl xr)
      (OpDiv, TMoney, TMoney) -> ok TNum (MRatio xl xr)
      (OpDiv, TMoney, TNum) -> ok TMoney (MDivN xl xr)
      (OpDiv, TNum, TNum) -> ok TNum (NDiv xl xr)
      (OpMul, TMoney, TMoney) -> typeErr p "cannot multiply money by money"
      (OpDiv, TNum, TMoney) -> typeErr p "cannot divide a number by money"
      _ -> typeErr p ("cannot apply " <> opName o <> " to " <> tyName tl <> " and " <> tyName tr <> hint tl tr <> hint tr tl)

    opName = \case
      OpAdd -> "+"
      OpSub -> "-"
      OpMul -> "*"
      OpDiv -> "/"
      other -> T.pack (show other)

    cmpOf = \case
      OpLt -> Just Lt
      OpLe -> Just Le
      OpGt -> Just Gt
      OpGe -> Just Ge
      OpEq -> Just Eq
      OpNe -> Just Ne
      _ -> Nothing
