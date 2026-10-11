module Palisade.LedgerSpec (spec) where

import Data.Either (fromRight)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Gen
import Palisade.Currency
import Palisade.Ledger
import Palisade.Money
import Palisade.Types
import Test.Hspec
import Test.QuickCheck

-- | Apply what the ledger accepts; skip what it refuses.
applyAccepted :: [Event] -> LedgerState
applyAccepted = foldl' (\st ev -> fromRight st (applyEvent st ev)) base
  where
    base = snd (setupLedger [EvOpen t0 "a", EvOpen t0 "b"])

spec :: Spec
spec = describe "Palisade.Ledger" $ do
  it "keeps the trial balance at zero in every currency after any events" $
    property $ forAll genLedgerEvents $ \evs ->
      all (== 0) (M.elems (trialBalance (applyAccepted evs)))

  it "never lets an internal account's settled balance go negative" $
    property $ forAll genLedgerEvents $ \evs ->
      all (\(_, _, n) -> n >= 0) (internalBalances (applyAccepted evs))

  it "every posting set it produces balances" $
    property $ forAll genSomeMoney $ \m ->
      let i = Intent "i" t0 "t" Pay "a" "v" m ""
       in either (const False) balanced (postingsFor i)

  it "does not let pending deposits be spent" $ do
    let (_, st) = setupLedger [EvOpen t0 "a", EvDeposit t0 "p1" "a" "bank" (mkSomeMoney USD 10000) False]
    pendingInbound "a" USD st `shouldBe` 10000
    settledBalance "a" USD st `shouldBe` 0
    applyEvent st (EvExecute (at 1) (payIntent "i1" 1 "a" "v" USD 100) "p")
      `shouldBe` Left (InsufficientFunds "a" USD 0 100)

  it "credits a deposit when it settles, and drops it when reversed" $ do
    let (_, st) =
          setupLedger
            [ EvOpen t0 "a"
            , EvDeposit t0 "p1" "a" "bank" (mkSomeMoney USD 10000) False
            , EvDeposit t0 "p2" "a" "bank" (mkSomeMoney USD 500) False
            , EvSettle (at 5) "p1"
            , EvReverse (at 6) "p2"
            ]
    settledBalance "a" USD st `shouldBe` 10000
    pendingInbound "a" USD st `shouldBe` 0
    applyEvent st (EvSettle (at 7) "p2") `shouldBe` Left (UnknownTransfer "p2")

  it "refuses duplicate intents, backdated events and payments to internal accounts" $ do
    let (_, st) = setupLedger (demoSetup <> [EvExecute (at 10) (payIntent "i1" 10 "treasury" "aws" USD 100) "p"])
    applyEvent st (EvExecute (at 11) (payIntent "i1" 11 "treasury" "aws" USD 100) "p") `shouldBe` Left (DuplicateIntent "i1")
    applyEvent st (EvOpen (at 1) "late") `shouldBe` Left (TimeWentBackwards (at 10) (at 1))
    applyEvent st (EvExecute (at 12) (payIntent "i2" 12 "treasury" "ap-agent" USD 100) "p") `shouldBe` Left (PayToInternal "ap-agent")

  it "converts currencies with conservation in both" $ do
    let conv = Intent "fx1" (at 1) "t" (Convert EUR (92 / 100)) "treasury" "treasury" (mkSomeMoney USD 10001) ""
        (_, st0) = setupLedger demoSetup
    case applyEvent st0 (EvExecute (at 1) conv "p") of
      Left e -> expectationFailure (show e)
      Right st -> do
        settledBalance "treasury" EUR st `shouldBe` 9200 -- 100.01 USD * 0.92 = 92.0092 EUR, rounded down
        settledBalance "treasury" USD st `shouldBe` 200000000 - 10001
        M.elems (trialBalance st) `shouldSatisfy` all (== 0)

  it "answers windowed outflow queries over (now - d, now]" $ do
    let (_, st) =
          setupLedger
            ( demoSetup
                <> [ EvExecute (at 100) (payIntent "i1" 100 "treasury" "aws" USD 100) "p"
                   , EvExecute (at 200) (payIntent "i2" 200 "treasury" "aws" USD 200) "p"
                   ]
            )
    map orMinor (outflowsWithin (at 200) 100 st) `shouldBe` [200]
    map orMinor (outflowsWithin (at 200) 101 st) `shouldBe` [200, 100]
    hasPaid "treasury" "aws" st `shouldBe` True
    hasPaid "treasury" "gcp" st `shouldBe` False
