-- | The typed policy language.
--
-- 'Expr' is indexed by the policy's currency @c@ and by the result type.
-- A money expression has type @Expr c (Q c)@, so an expression that mixes
-- currencies cannot be built, whether it comes from the @.pal@ parser (the
-- typechecker rejects it with a source position) or from Haskell (GHC
-- rejects it). The embedded combinators at the bottom of this module are
-- the Haskell form of the same language.
module Palisade.Policy.Expr
  ( Q (..)
  , Seconds (..)
  , Ty (..)
  , tyName
  , eqTy
  , CmpOp (..)
  , cmpText
  , Expr (..)
  , toSyntax
  , prettyTyped
  , moneyLiterals
    -- * Policies
  , Rule (..)
  , Law (..)
  , PolicyBlock (..)
  , SomeBlock (..)
  , Policy (..)
  , mkPolicy
    -- * Embedded DSL
  , IsTy (..)
  , require
  , escalate
  , law
  , amount
  , counterparty
  , source
  , actor
  , balance
  , balanceAfter
  , totalBalance
  , knownCounterparty
  , outflow
  , totalOutflow
  , count
  , duplicates
  , typical
  , hours
  , minutes
  , days
  , cash
  , num
  , oneOf
  , (.<=)
  , (.<)
  , (.>=)
  , (.>)
  , (.==)
  , (./=)
  , (.+)
  , (.-)
  , (*.)
  , (.&&)
  , (.||)
  , notE
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Type.Equality ((:~:) (..))
import Palisade.Canonical (sha256HexText)
import Palisade.Currency
import Palisade.Policy.Syntax

-- | An exact quantity of currency @c@ in major units. Policy arithmetic
-- such as @0.15 * total_balance@ can land between minor units, so
-- quantities are exact rationals; nothing is rounded during evaluation.
newtype Q (c :: Currency) = Q Rational
  deriving (Eq, Ord, Show)

newtype Seconds = Seconds Integer
  deriving (Eq, Ord, Show)

data Ty (c :: Currency) a where
  TBool :: Ty c Bool
  TNum :: Ty c Rational
  TMoney :: Ty c (Q c)
  TText :: Ty c Text
  TSet :: Ty c (Set Text)
  TDur :: Ty c Seconds

tyName :: forall c a. KnownCurrency c => Ty c a -> Text
tyName = \case
  TBool -> "Bool"
  TNum -> "Number"
  TMoney -> "Money (" <> currencyCode (fromSing (currencySing @c)) <> ")"
  TText -> "Text"
  TSet -> "Set"
  TDur -> "Duration"

eqTy :: Ty c a -> Ty c b -> Maybe (a :~: b)
eqTy a b = case (a, b) of
  (TBool, TBool) -> Just Refl
  (TNum, TNum) -> Just Refl
  (TMoney, TMoney) -> Just Refl
  (TText, TText) -> Just Refl
  (TSet, TSet) -> Just Refl
  (TDur, TDur) -> Just Refl
  _ -> Nothing

data CmpOp = Lt | Le | Gt | Ge | Eq | Ne
  deriving (Eq, Show, Enum, Bounded)

cmpText :: CmpOp -> Text
cmpText = \case
  Lt -> "<"
  Le -> "<="
  Gt -> ">"
  Ge -> ">="
  Eq -> "=="
  Ne -> "!="

data Expr (c :: Currency) a where
  EBool :: Bool -> Expr c Bool
  ENum :: Rational -> Expr c Rational
  EMoney :: Q c -> Expr c (Q c)
  EText :: Text -> Expr c Text
  ESet :: Maybe Text -> Set Text -> Expr c (Set Text)
  EDur :: Seconds -> Expr c Seconds
  -- Intent fields (rules only)
  FAmount :: Expr c (Q c)
  FCounterparty :: Expr c Text
  FSource :: Expr c Text
  FActor :: Expr c Text
  FMemo :: Expr c Text
  FKind :: Expr c Text
  FBalance :: Expr c (Q c)
  FBalanceAfter :: Expr c (Q c)
  FPending :: Expr c (Q c)
  FKnownCounterparty :: Expr c Bool
  QOutflow :: Expr c Seconds -> Expr c (Q c)
  QCount :: Expr c Seconds -> Expr c Rational
  QDuplicates :: Expr c Seconds -> Expr c Rational
  QTypical :: Expr c Seconds -> Expr c (Q c)
  -- State queries (rules and laws)
  FHour :: Expr c Rational
  FTotalBalance :: Expr c (Q c)
  QBalanceOf :: Expr c Text -> Expr c (Q c)
  QOutflowOf :: Expr c Text -> Expr c Seconds -> Expr c (Q c)
  QTotalOutflow :: Expr c Seconds -> Expr c (Q c)
  -- Logic
  ENot :: Expr c Bool -> Expr c Bool
  EAnd :: Expr c Bool -> Expr c Bool -> Expr c Bool
  EOr :: Expr c Bool -> Expr c Bool -> Expr c Bool
  ECmp :: CmpOp -> Ty c a -> Expr c a -> Expr c a -> Expr c Bool
  EIn :: Expr c Text -> Expr c (Set Text) -> Expr c Bool
  ENotIn :: Expr c Text -> Expr c (Set Text) -> Expr c Bool
  -- Arithmetic. Money only meets money of the same currency.
  MAdd :: Expr c (Q c) -> Expr c (Q c) -> Expr c (Q c)
  MSub :: Expr c (Q c) -> Expr c (Q c) -> Expr c (Q c)
  MScale :: Expr c Rational -> Expr c (Q c) -> Expr c (Q c)
  MScaleR :: Expr c (Q c) -> Expr c Rational -> Expr c (Q c)
  MDivN :: Expr c (Q c) -> Expr c Rational -> Expr c (Q c)
  MRatio :: Expr c (Q c) -> Expr c (Q c) -> Expr c Rational
  MNeg :: Expr c (Q c) -> Expr c (Q c)
  NAdd :: Expr c Rational -> Expr c Rational -> Expr c Rational
  NSub :: Expr c Rational -> Expr c Rational -> Expr c Rational
  NMul :: Expr c Rational -> Expr c Rational -> Expr c Rational
  NDiv :: Expr c Rational -> Expr c Rational -> Expr c Rational
  NNeg :: Expr c Rational -> Expr c Rational

-- | Back to concrete syntax. @parse . pretty . toSyntax@ typechecks to an
-- equivalent expression (property-tested).
toSyntax :: forall c a. KnownCurrency c => Expr c a -> SExpr
toSyntax = sx . go
  where
    cur = fromSing (currencySing @c)
    t :: forall b. Expr c b -> SExpr
    t = toSyntax
    call = SCall
    bin :: forall x y. BinOp -> Expr c x -> Expr c y -> SNode
    bin o l r = SBin o (t l) (t r)
    go :: Expr c a -> SNode
    go = \case
      EBool b -> SBool b
      ENum r -> SNum r
      EMoney (Q r) -> SMoney r cur
      EText x -> SStr x
      ESet (Just n) _ -> SVar n
      ESet Nothing xs -> SSetLit (S.toAscList xs)
      EDur (Seconds s) -> SDur s
      FAmount -> SVar "amount"
      FCounterparty -> SVar "counterparty"
      FSource -> SVar "source"
      FActor -> SVar "actor"
      FMemo -> SVar "memo"
      FKind -> SVar "kind"
      FBalance -> SVar "balance"
      FBalanceAfter -> SVar "balance_after"
      FPending -> SVar "pending"
      FKnownCounterparty -> SVar "known_counterparty"
      QOutflow d -> call "outflow" [t d]
      QCount d -> call "count" [t d]
      QDuplicates d -> call "duplicates" [t d]
      QTypical d -> call "typical" [t d]
      FHour -> SVar "hour"
      FTotalBalance -> SVar "total_balance"
      QBalanceOf a -> call "balance_of" [t a]
      QOutflowOf a d -> call "outflow_of" [t a, t d]
      QTotalOutflow d -> call "total_outflow" [t d]
      ENot e -> SUn OpNot (t e)
      EAnd l r -> bin OpAnd l r
      EOr l r -> bin OpOr l r
      ECmp o _ l r -> bin (cmpBin o) l r
      EIn l r -> bin OpIn l r
      ENotIn l r -> bin OpNotIn l r
      MAdd l r -> bin OpAdd l r
      MSub l r -> bin OpSub l r
      MScale l r -> bin OpMul l r
      MScaleR l r -> bin OpMul l r
      MDivN l r -> bin OpDiv l r
      MRatio l r -> bin OpDiv l r
      MNeg e -> SUn OpNeg (t e)
      NAdd l r -> bin OpAdd l r
      NSub l r -> bin OpSub l r
      NMul l r -> bin OpMul l r
      NDiv l r -> bin OpDiv l r
      NNeg e -> SUn OpNeg (t e)
    cmpBin = \case
      Lt -> OpLt
      Le -> OpLe
      Gt -> OpGt
      Ge -> OpGe
      Eq -> OpEq
      Ne -> OpNe

prettyTyped :: KnownCurrency c => Expr c a -> Text
prettyTyped = prettyExpr . toSyntax

-- | Every money literal in an expression, in major units. The stress tester
-- uses these to generate amounts right at each threshold.
moneyLiterals :: KnownCurrency c => Expr c a -> [Rational]
moneyLiterals = collect . toSyntax
  where
    collect (SExpr _ n) = case n of
      SMoney r _ -> [r]
      SCall _ as -> concatMap collect as
      SUn _ e -> collect e
      SBin _ l r -> collect l <> collect r
      _ -> []

------------------------------------------------------------------------------
-- Policies

data Rule c = Rule
  { ruleKind :: RuleKind
  , ruleName :: Text
  , ruleExpr :: Expr c Bool
  }

-- | A law must hold in every reachable state. Laws see the ledger, not an
-- intent; the stress tester checks them after every step.
data Law c = Law
  { lawName :: Text
  , lawExpr :: Expr c Bool
  }

data PolicyBlock c = PolicyBlock
  { pbName :: Text
  , pbRules :: [Rule c]
  , pbLaws :: [Law c]
  }

data SomeBlock where
  SomeBlock :: KnownCurrency c => SCurrency c -> PolicyBlock c -> SomeBlock

-- | A checked policy: one block per currency, plus the hash that
-- certificates cite.
data Policy = Policy
  { policyHash :: Text
  , policySource :: Text
  -- ^ Canonical source; the hash is SHA-256 of exactly this text.
  , policySets :: Map Text (Set Text)
  , policyBlocks :: Map Currency SomeBlock
  }

-- | Build a policy from checked blocks. The hash covers a canonical
-- rendering: every named set the rules use (sorted, with its members), then
-- each block in currency order. The same rules therefore hash the same way
-- whether they were written in Haskell or in a @.pal@ file, and editing a
-- set's members changes the hash.
mkPolicy :: [SomeBlock] -> Policy
mkPolicy blocks =
  let sets = M.toAscList (M.unions [blockSets b | b <- blocks])
      decls =
        [SSetDecl noPos n (S.toAscList xs) | (n, xs) <- sets]
          <> [blockDecl b | b <- M.elems byCur]
      byCur = M.fromList [(cur b, b) | b <- blocks]
      src = prettyFile (PolicyFile decls)
   in Policy (sha256HexText src) src (M.fromList sets) byCur
  where
    cur (SomeBlock s _) = fromSing s
    blockSets (SomeBlock _ blk) =
      M.unions ([namedSets (ruleExpr r) | r <- pbRules blk] <> [namedSets (lawExpr l) | l <- pbLaws blk])
    blockDecl (SomeBlock s blk) =
      SPolicy
        noPos
        (pbName blk)
        (fromSing s)
        ( [SRule noPos (ruleKind r) (ruleName r) (toSyntax (ruleExpr r)) | r <- pbRules blk]
            <> [SLaw noPos (lawName l) (toSyntax (lawExpr l)) | l <- pbLaws blk]
        )

data SomeE c where
  SomeE :: Expr c b -> SomeE c

children :: Expr c a -> [SomeE c]
children = \case
  QOutflow d -> [SomeE d]
  QCount d -> [SomeE d]
  QDuplicates d -> [SomeE d]
  QTypical d -> [SomeE d]
  QBalanceOf a -> [SomeE a]
  QOutflowOf a d -> [SomeE a, SomeE d]
  QTotalOutflow d -> [SomeE d]
  ENot e -> [SomeE e]
  EAnd l r -> [SomeE l, SomeE r]
  EOr l r -> [SomeE l, SomeE r]
  ECmp _ _ l r -> [SomeE l, SomeE r]
  EIn l r -> [SomeE l, SomeE r]
  ENotIn l r -> [SomeE l, SomeE r]
  MAdd l r -> [SomeE l, SomeE r]
  MSub l r -> [SomeE l, SomeE r]
  MScale l r -> [SomeE l, SomeE r]
  MScaleR l r -> [SomeE l, SomeE r]
  MDivN l r -> [SomeE l, SomeE r]
  MRatio l r -> [SomeE l, SomeE r]
  MNeg e -> [SomeE e]
  NAdd l r -> [SomeE l, SomeE r]
  NSub l r -> [SomeE l, SomeE r]
  NMul l r -> [SomeE l, SomeE r]
  NDiv l r -> [SomeE l, SomeE r]
  NNeg e -> [SomeE e]
  _ -> []

-- | Named sets referenced anywhere in an expression.
namedSets :: Expr c a -> Map Text (Set Text)
namedSets e = case e of
  ESet (Just n) xs -> M.singleton n xs
  _ -> M.unions [namedSets c | SomeE c <- children e]

------------------------------------------------------------------------------
-- Embedded DSL

class IsTy a where
  tyOf :: Ty c a

instance IsTy Bool where tyOf = TBool
instance IsTy Rational where tyOf = TNum
instance IsTy Text where tyOf = TText
instance IsTy Seconds where tyOf = TDur

-- 'Q' needs its own comparison helpers because its type mentions @c@.
require, escalate :: Text -> Expr c Bool -> Rule c
require = Rule RequireRule
escalate = Rule EscalateRule

law :: Text -> Expr c Bool -> Law c
law = Law

amount, balance, balanceAfter, totalBalance :: Expr c (Q c)
amount = FAmount
balance = FBalance
balanceAfter = FBalanceAfter
totalBalance = FTotalBalance

counterparty, source, actor :: Expr c Text
counterparty = FCounterparty
source = FSource
actor = FActor

knownCounterparty :: Expr c Bool
knownCounterparty = FKnownCounterparty

outflow, totalOutflow, typical :: Expr c Seconds -> Expr c (Q c)
outflow = QOutflow
totalOutflow = QTotalOutflow
typical = QTypical

count, duplicates :: Expr c Seconds -> Expr c Rational
count = QCount
duplicates = QDuplicates

hours, minutes, days :: Integer -> Expr c Seconds
hours n = EDur (Seconds (n * 3600))
minutes n = EDur (Seconds (n * 60))
days n = EDur (Seconds (n * 86400))

-- | A money literal in the policy's own currency. There is deliberately no
-- way to write a literal in another currency.
cash :: Rational -> Expr c (Q c)
cash = EMoney . Q

num :: Rational -> Expr c Rational
num = ENum

oneOf :: Expr c Text -> Text -> [Text] -> Expr c Bool
oneOf x n xs = EIn x (ESet (Just n) (S.fromList xs))

infix 4 .<=, .<, .>=, .>, .==, ./=

(.<=), (.<), (.>=), (.>) :: Expr c (Q c) -> Expr c (Q c) -> Expr c Bool
(.<=) = ECmp Le TMoney
(.<) = ECmp Lt TMoney
(.>=) = ECmp Ge TMoney
(.>) = ECmp Gt TMoney

(.==), (./=) :: IsTy a => Expr c a -> Expr c a -> Expr c Bool
(.==) = ECmp Eq tyOf
(./=) = ECmp Ne tyOf

infixl 6 .+, .-

(.+), (.-) :: Expr c (Q c) -> Expr c (Q c) -> Expr c (Q c)
(.+) = MAdd
(.-) = MSub

infixl 7 *.

(*.) :: Expr c Rational -> Expr c (Q c) -> Expr c (Q c)
(*.) = MScale

infixr 3 .&&

infixr 2 .||

(.&&), (.||) :: Expr c Bool -> Expr c Bool -> Expr c Bool
(.&&) = EAnd
(.||) = EOr

notE :: Expr c Bool -> Expr c Bool
notE = ENot
