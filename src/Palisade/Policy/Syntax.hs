-- | The untyped syntax of @.pal@ policy files, with source positions, and
-- a pretty printer whose output parses back to the same tree.
module Palisade.Policy.Syntax
  ( Pos (..)
  , noPos
  , renderPos
  , BinOp (..)
  , UnOp (..)
  , SExpr (..)
  , SNode (..)
  , sx
  , RuleKind (..)
  , ruleKindText
  , SItem (..)
  , SDecl (..)
  , PolicyFile (..)
  , prettyExpr
  , prettyFile
  , prettyDuration
  , prettyNumber
  , quoteText
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Palisade.Currency
import Palisade.Money (renderRational)

data Pos = Pos {posLine :: !Int, posCol :: !Int}
  deriving (Eq, Ord, Show)

noPos :: Pos
noPos = Pos 0 0

renderPos :: Pos -> Text
renderPos (Pos l c) = T.pack (show l) <> ":" <> T.pack (show c)

data BinOp
  = OpOr
  | OpAnd
  | OpLt
  | OpLe
  | OpGt
  | OpGe
  | OpEq
  | OpNe
  | OpIn
  | OpNotIn
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  deriving (Eq, Show, Enum, Bounded)

data UnOp = OpNot | OpNeg
  deriving (Eq, Show, Enum, Bounded)

-- | An expression with the position it was parsed at. Equality ignores
-- positions, so a pretty-printed and re-parsed tree compares equal.
data SExpr = SExpr {sPos :: !Pos, sNode :: !SNode}
  deriving (Show)

instance Eq SExpr where
  a == b = sNode a == sNode b

data SNode
  = SNum Rational
  | SMoney Rational Currency
  | SDur Integer
  | SStr Text
  | SBool Bool
  | SVar Text
  | SCall Text [SExpr]
  | SSetLit [Text]
  | SUn UnOp SExpr
  | SBin BinOp SExpr SExpr
  deriving (Eq, Show)

sx :: SNode -> SExpr
sx = SExpr noPos

-- | @require@ rules deny when false; @escalate@ rules send the intent to a
-- human when true.
data RuleKind = RequireRule | EscalateRule
  deriving (Eq, Show, Enum, Bounded)

ruleKindText :: RuleKind -> Text
ruleKindText = \case
  RequireRule -> "require"
  EscalateRule -> "escalate"

data SItem
  = SRule Pos RuleKind Text SExpr
  | SLaw Pos Text SExpr
  deriving (Show)

instance Eq SItem where
  SRule _ k n e == SRule _ k' n' e' = (k, n, e) == (k', n', e')
  SLaw _ n e == SLaw _ n' e' = (n, e) == (n', e')
  _ == _ = False

data SDecl
  = SSetDecl Pos Text [Text]
  | SPolicy Pos Text Currency [SItem]
  deriving (Show)

instance Eq SDecl where
  SSetDecl _ n xs == SSetDecl _ n' xs' = (n, xs) == (n', xs')
  SPolicy _ n c is == SPolicy _ n' c' is' = (n, c, is) == (n', c', is')
  _ == _ = False

newtype PolicyFile = PolicyFile [SDecl]
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Pretty printing

binPrec :: BinOp -> Int
binPrec = \case
  OpOr -> 1
  OpAnd -> 2
  OpAdd -> 5
  OpSub -> 5
  OpMul -> 6
  OpDiv -> 6
  _ -> 4

binText :: BinOp -> Text
binText = \case
  OpOr -> "or"
  OpAnd -> "and"
  OpLt -> "<"
  OpLe -> "<="
  OpGt -> ">"
  OpGe -> ">="
  OpEq -> "=="
  OpNe -> "!="
  OpIn -> "in"
  OpNotIn -> "not in"
  OpAdd -> "+"
  OpSub -> "-"
  OpMul -> "*"
  OpDiv -> "/"

nodePrec :: SNode -> Int
nodePrec = \case
  SBin op _ _ -> binPrec op
  SUn OpNot _ -> 3
  SUn OpNeg _ -> 7
  _ -> 8

prettyExpr :: SExpr -> Text
prettyExpr = go 0
  where
    go :: Int -> SExpr -> Text
    go ctx (SExpr _ n) =
      let body = node n
       in if nodePrec n < ctx then "(" <> body <> ")" else body
    node = \case
      SNum r -> prettyNumber r
      SMoney r c -> prettyNumber r <> " " <> currencyCode c
      SDur s -> prettyDuration s
      SStr t -> quoteText t
      SBool b -> if b then "true" else "false"
      SVar v -> v
      SCall f args -> f <> "(" <> T.intercalate ", " (map (go 0) args) <> ")"
      SSetLit xs -> "{" <> T.intercalate ", " (map quoteText xs) <> "}"
      SUn OpNot e -> "not " <> go 3 e
      SUn OpNeg e ->
        let inner = go 7 e
         in if "-" `T.isPrefixOf` inner then "-(" <> inner <> ")" else "-" <> inner
      SBin op l r
        | binPrec op == 4 -> go 5 l <> " " <> binText op <> " " <> go 5 r
        | otherwise -> go (binPrec op) l <> " " <> binText op <> " " <> go (binPrec op + 1) r

prettyNumber :: Rational -> Text
prettyNumber = renderRational 40

prettyDuration :: Integer -> Text
prettyDuration s
  | s >= 7 * 86400 && s `mod` 86400 == 0 = T.pack (show (s `div` 86400)) <> "d"
  | s /= 0 && s `mod` 3600 == 0 = T.pack (show (s `div` 3600)) <> "h"
  | s /= 0 && s `mod` 60 == 0 = T.pack (show (s `div` 60)) <> "m"
  | otherwise = T.pack (show s) <> "s"

quoteText :: Text -> Text
quoteText t = "\"" <> T.concatMap esc t <> "\""
  where
    esc = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      c -> T.singleton c

prettyFile :: PolicyFile -> Text
prettyFile (PolicyFile decls) =
  let sets = [d | d@SSetDecl {} <- decls]
      pols = [d | d@SPolicy {} <- decls]
   in T.intercalate "\n" ([T.concat (map decl sets) | not (null sets)] <> map decl pols)
  where
    decl = \case
      SSetDecl _ n xs -> "set " <> n <> " = {" <> T.intercalate ", " (map quoteText xs) <> "}\n"
      SPolicy _ n c items ->
        "policy " <> quoteText n <> " currency " <> currencyCode c <> " {\n"
          <> T.concat (map item items)
          <> "}\n"
    item = \case
      SRule _ k n e -> "  " <> ruleKindText k <> " " <> quoteText n <> ": " <> prettyExpr e <> "\n"
      SLaw _ n e -> "  law " <> quoteText n <> ": " <> prettyExpr e <> "\n"
