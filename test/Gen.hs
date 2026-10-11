-- | Shared generators and fixtures.
module Gen
  ( genCurrency
  , genSomeMoney
  , genDecimal
  , genSExpr
  , genPolicyFile
  , t0
  , at
  , setupLedger
  , demoSetup
  , payIntent
  , loadScenarioPolicy
  , genLedgerEvents
  ) where


import Data.List (foldl')
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Palisade.Chain
import Palisade.Currency
import Palisade.Ledger
import Palisade.Money
import Palisade.Policy
import Palisade.Types
import Test.QuickCheck

genCurrency :: Gen Currency
genCurrency = elements allCurrencies

genSomeMoney :: Gen SomeMoney
genSomeMoney = mkSomeMoney <$> genCurrency <*> (getPositive <$> arbitrary)

-- | A non-negative finite decimal, as the parser produces.
genDecimal :: Gen Rational
genDecimal = do
  n <- chooseInteger (0, 10 ^ (12 :: Int))
  k <- chooseInt (0, 6)
  pure (n % (10 ^ k))

ident :: Gen Text
ident = elements ["amount", "balance", "counterparty", "x", "merchants", "outflow", "foo_bar", "notable", "inside", "orders", "android"]

stringLit :: Gen Text
stringLit = T.pack <$> listOf (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> " :-_#\"\\" <> ['0' .. '9']))

genSExpr :: Int -> Gen SExpr
genSExpr n
  | n <= 0 = sx <$> leaf
  | otherwise =
      frequency
        [ (3, sx <$> leaf)
        , (1, (\f as -> sx (SCall f as)) <$> ident <*> resize 3 (listOf (genSExpr (n `div` 3))))
        , (1, (\o e -> sx (SUn o e)) <$> elements [minBound .. maxBound] <*> genSExpr (n - 1))
        , (4, (\o l r -> sx (SBin o l r)) <$> elements [minBound .. maxBound] <*> genSExpr (n `div` 2) <*> genSExpr (n `div` 2))
        ]
  where
    leaf =
      oneof
        [ SNum <$> genDecimal
        , SMoney <$> genDecimal <*> genCurrency
        , SDur <$> chooseInteger (1, 10 ^ (7 :: Int))
        , SStr <$> stringLit
        , SBool <$> arbitrary
        , SVar <$> ident
        , SSetLit <$> listOf stringLit
        ]

genPolicyFile :: Gen PolicyFile
genPolicyFile = do
  sets <- listOf (SSetDecl noPos <$> ident <*> listOf stringLit)
  pols <- listOf $ do
    nm <- stringLit
    c <- genCurrency
    items <- listOf $
      oneof
        [ SRule noPos <$> elements [RequireRule, EscalateRule] <*> stringLit <*> sized genSExpr
        , SLaw noPos <$> stringLit <*> sized genSExpr
        ]
    pure (SPolicy noPos nm c items)
  pure (PolicyFile (sets <> pols))

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 10 1) 0

-- | Seconds after 'at'.
at :: Integer -> UTCTime
at s = addUTCTime (fromInteger s) t0

-- | Build a log from events, failing loudly if the ledger refuses one.
setupLedger :: [Event] -> ([LogEntry], LedgerState)
setupLedger = foldl' step ([], emptyLedger)
  where
    step (es, st) ev = case appendEvent st ev of
      Right (st', e) -> (es <> [e], st')
      Left err -> error ("setup refused: " <> show err)

-- | treasury 2,000,000 USD; ap-agent 50,000 USD; agent-wallet 60,000 USDC.
demoSetup :: [Event]
demoSetup =
  [ EvOpen t0 "treasury"
  , EvOpen t0 "ap-agent"
  , EvOpen t0 "agent-wallet"
  , EvDeposit t0 "seed-1" "treasury" "bank" (mkSomeMoney USD 200000000) True
  , EvDeposit t0 "seed-2" "ap-agent" "bank" (mkSomeMoney USD 5000000) True
  , EvDeposit t0 "seed-3" "agent-wallet" "circle" (mkSomeMoney USDC 60000000000) True
  ]

payIntent :: Text -> Integer -> Text -> Text -> Currency -> Integer -> Intent
payIntent iid s from to c minor = Intent iid (at s) "test" Pay from to (mkSomeMoney c minor) ""

loadScenarioPolicy :: FilePath -> IO Policy
loadScenarioPolicy p = do
  src <- TIO.readFile p
  either (fail . T.unpack) pure (loadPolicy p src)

-- | Random administrative and execution events over two accounts. Some
-- will be refused by the ledger; callers keep the accepted ones.
genLedgerEvents :: Gen [Event]
genLedgerEvents = do
  n <- chooseInt (0, 40)
  go n 1 []
  where
    go 0 _ _ = pure []
    go k s deps = do
      dt <- chooseInteger (0, 7200)
      let ts = at (s + dt)
      c <- elements [USD, EUR, USDC]
      amt <- chooseInteger (1, 10 ^ (9 :: Int))
      acct <- elements ["a", "b"]
      ev <-
        frequency
          [ (3, pure (EvDeposit ts ("d" <> T.pack (show k)) acct "bank" (mkSomeMoney c amt) True))
          , (2, pure (EvDeposit ts ("p" <> T.pack (show k)) acct "bank" (mkSomeMoney c amt) False))
          , (1, case deps of [] -> pure (EvSettle ts "none"); _ -> EvSettle ts <$> elements deps)
          , (1, case deps of [] -> pure (EvReverse ts "none"); _ -> EvReverse ts <$> elements deps)
          , (4, (\to -> EvExecute ts (Intent ("i" <> T.pack (show k)) ts "gen" Pay acct to (mkSomeMoney c amt) "") "p") <$> elements ["v1", "v2"])
          , (1, pure (EvExecute ts (Intent ("i" <> T.pack (show k)) ts "gen" TransferInternal acct (if acct == "a" then "b" else "a") (mkSomeMoney c amt) "") "p"))
          , (1, (\r -> EvExecute ts (Intent ("i" <> T.pack (show k)) ts "gen" (Convert (if c == USD then EUR else USD) r) acct acct (mkSomeMoney c amt) "") "p") <$> elements [92 % 100, 1 % 3, 7])
          ]
      let deps' = case ev of
            EvDeposit _ tid _ _ _ False -> tid : deps
            _ -> deps
      rest <- go (k - 1) (s + dt) deps'
      pure (ev : rest)

