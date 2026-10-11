-- | Parser for @.pal@ policy files.
--
-- > set merchants = {"aws", "openai"}
-- >
-- > policy "agent-usd" currency USD {
-- >   require  "per-payment cap": amount <= 250 USD
-- >   require  "daily budget":    outflow(24h) + amount <= 1000 USD
-- >   escalate "new payee":       not known_counterparty
-- >   law      "fleet budget":    total_outflow(24h) <= 5000 USD
-- > }
--
-- Comments start with @#@ or @--@.
module Palisade.Policy.Parse
  ( parsePolicyFile
  , parseExpr
  ) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit, isUpper)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Palisade.Currency
import Palisade.Money (parseDecimal)
import Palisade.Policy.Syntax
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parsePolicyFile :: FilePath -> Text -> Either Text PolicyFile
parsePolicyFile path src = case parse (sc *> pFile <* eof) path src of
  Left err -> Left (T.pack (errorBundlePretty err))
  Right f -> Right f

parseExpr :: Text -> Either Text SExpr
parseExpr src = case parse (sc *> pExpr <* eof) "<expr>" src of
  Left err -> Left (T.pack (errorBundlePretty err))
  Right e -> Right e

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#" <|> L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | An operator symbol that is not the prefix of a longer one.
op :: Text -> Parser Text
op s = lexeme (try (string s <* notFollowedBy (oneOf ("=<>" :: String))))

keywords :: [Text]
keywords = ["set", "policy", "currency", "require", "escalate", "law", "and", "or", "not", "in", "true", "false"]

keyword :: Text -> Parser ()
keyword k = lexeme (try (string k *> notFollowedBy (satisfy isIdentChar)))

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

getPos :: Parser Pos
getPos = do
  p <- getSourcePos
  pure (Pos (unPos (sourceLine p)) (unPos (sourceColumn p)))

identifier :: Parser Text
identifier = lexeme . try $ do
  c <- letterChar
  rest <- takeWhileP (Just "identifier character") isIdentChar
  let w = T.cons c rest
  if w `elem` keywords
    then fail ("keyword " <> show w <> " cannot be used as a name")
    else pure w

stringLit :: Parser Text
stringLit = lexeme (char '"' *> (T.pack <$> manyTill strChar (char '"'))) <?> "string"
  where
    strChar = (char '\\' *> (char '"' <|> char '\\')) <|> satisfy (/= '\n')

currencyLit :: Parser Currency
currencyLit = lexeme . try $ do
  w <- takeWhile1P (Just "currency code") (\c -> isUpper c || isDigit c)
  notFollowedBy (satisfy isIdentChar)
  maybe (fail ("unknown currency " <> T.unpack w)) pure (parseCurrency w)

pFile :: Parser PolicyFile
pFile = PolicyFile <$> many pDecl

pDecl :: Parser SDecl
pDecl = pSetDecl <|> pPolicy

pSetDecl :: Parser SDecl
pSetDecl = do
  p <- getPos
  keyword "set"
  n <- identifier
  void (symbol "=")
  SSetDecl p n <$> setBody

setBody :: Parser [Text]
setBody = between (symbol "{") (symbol "}") (stringLit `sepEndBy` symbol ",")

pPolicy :: Parser SDecl
pPolicy = do
  p <- getPos
  keyword "policy"
  n <- stringLit
  keyword "currency"
  c <- currencyLit
  items <- between (symbol "{") (symbol "}") (many pItem)
  pure (SPolicy p n c items)

pItem :: Parser SItem
pItem = do
  p <- getPos
  choice
    [ keyword "require" *> rule p RequireRule
    , keyword "escalate" *> rule p EscalateRule
    , keyword "law" *> (SLaw p <$> stringLit <* symbol ":" <*> pExpr)
    ]
  where
    rule p k = SRule p k <$> stringLit <* symbol ":" <*> pExpr

------------------------------------------------------------------------------
-- Expressions, lowest precedence first

pExpr :: Parser SExpr
pExpr = pOr

chainLeft :: Parser SExpr -> Parser BinOp -> Parser SExpr
chainLeft operand opP = operand >>= rest
  where
    rest l =
      ( do
          p <- getPos
          o <- opP
          r <- operand
          rest (SExpr p (SBin o l r))
      )
        <|> pure l

pOr :: Parser SExpr
pOr = chainLeft pAnd (keyword "or" $> OpOr)

pAnd :: Parser SExpr
pAnd = chainLeft pNot (keyword "and" $> OpAnd)

pNot :: Parser SExpr
pNot =
  ( do
      p <- getPos
      keyword "not"
      SExpr p . SUn OpNot <$> pNot
  )
    <|> pCmp

pCmp :: Parser SExpr
pCmp = do
  l <- pAdd
  m <- optional $ do
    p <- getPos
    o <- cmpOp
    r <- pAdd
    pure (p, o, r)
  pure $ case m of
    Nothing -> l
    Just (p, o, r) -> SExpr p (SBin o l r)
  where
    cmpOp =
      choice
        [ try (keyword "not" *> keyword "in") $> OpNotIn
        , keyword "in" $> OpIn
        , op "<=" $> OpLe
        , op ">=" $> OpGe
        , op "==" $> OpEq
        , op "!=" $> OpNe
        , op "<" $> OpLt
        , op ">" $> OpGt
        ]

pAdd :: Parser SExpr
pAdd = chainLeft pMul (choice [symbol "+" $> OpAdd, symbol "-" $> OpSub])

pMul :: Parser SExpr
pMul = chainLeft pUnary (choice [symbol "*" $> OpMul, symbol "/" $> OpDiv])

pUnary :: Parser SExpr
pUnary =
  ( do
      p <- getPos
      void (symbol "-")
      SExpr p . SUn OpNeg <$> pUnary
  )
    <|> pAtom

pAtom :: Parser SExpr
pAtom = do
  p <- getPos
  choice
    [ between (symbol "(") (symbol ")") pExpr
    , SExpr p . SSetLit <$> setBody
    , SExpr p . SStr <$> stringLit
    , keyword "true" $> SExpr p (SBool True)
    , keyword "false" $> SExpr p (SBool False)
    , SExpr p <$> pNumberish
    , SExpr p <$> pNameOrCall
    ]

pNumberish :: Parser SNode
pNumberish = do
  whole <- takeWhile1P (Just "digit") (`elem` ['0' .. '9'])
  frac <- optional (try (char '.' *> takeWhile1P (Just "digit") (`elem` ['0' .. '9'])))
  let txt = whole <> maybe "" ("." <>) frac
  r <- maybe (fail "bad number") pure (parseDecimal txt)
  unit <- optional (try (oneOf ("smhd" :: String) <* notFollowedBy (satisfy isIdentChar)))
  sc
  case unit of
    Just u -> case frac of
      Just _ -> fail "durations must be whole numbers, e.g. 90m rather than 1.5h"
      Nothing ->
        let n = read (T.unpack whole) :: Integer
         in pure . SDur $ case u of
              's' -> n
              'm' -> n * 60
              'h' -> n * 3600
              _ -> n * 86400
    Nothing -> do
      cur <- optional currencyLit
      pure (maybe (SNum r) (SMoney r) cur)

pNameOrCall :: Parser SNode
pNameOrCall = do
  n <- identifier
  args <- optional (between (symbol "(") (symbol ")") (pExpr `sepBy` symbol ","))
  pure (maybe (SVar n) (SCall n) args)
