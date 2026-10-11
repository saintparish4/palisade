module Palisade.SimulateSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Either (rights)
import Gen
import Palisade.Certificate
import Palisade.Chain
import Palisade.Currency
import Palisade.Ledger
import Palisade.Simulate
import Palisade.Types
import Test.Hspec

readHistoryFile :: FilePath -> IO [(Int, HistoryItem)]
readHistoryFile p = do
  ls <- BLC.lines <$> BLC.readFile p
  pure [(n, h) | (n, Right h) <- zip [1 ..] (map parseHistoryLine ls)]

spec :: Spec
spec = describe "Palisade.Simulate" $ do
  it "parses every line of the demo history" $ do
    ls <- filter (not . BLC.null) . BLC.lines <$> BLC.readFile "scenarios/demo/history.jsonl"
    length (rights (map parseHistoryLine ls)) `shouldBe` length ls

  it "shadow mode mirrors reality: every intent is applied, verdicts are advisory" $ do
    pol <- loadScenarioPolicy "scenarios/policies/hardened.pal"
    items <- readHistoryFile "scenarios/demo/history.jsonl"
    let rep = simulate ShadowMode pol items
        outs = srOutcomes rep
    length outs `shouldBe` 22
    all oApplied outs `shouldBe` True
    length [() | o <- outs, oVerdict o /= Allow] `shouldBe` 13
    -- Reality overdrew ap-agent; shadow mode records it rather than hiding it.
    settledBalance "ap-agent" USD (srFinal rep) `shouldSatisfy` (< 0)

  it "backtest mode moves only what the policy allows" $ do
    pol <- loadScenarioPolicy "scenarios/policies/hardened.pal"
    items <- readHistoryFile "scenarios/demo/history.jsonl"
    let rep = simulate (CounterfactualMode EscalationsBlocked) pol items
    all (\o -> oApplied o == (oVerdict o == Allow)) (srOutcomes rep) `shouldBe` True
    all (\(_, _, n) -> n >= 0) (internalBalances (srFinal rep)) `shouldBe` True

  it "chains one certificate per intent into a verifiable evidence log" $ do
    pol <- loadScenarioPolicy "scenarios/policies/hardened.pal"
    items <- readHistoryFile "scenarios/demo/history.jsonl"
    let ev = srEvidence (simulate ShadowMode pol items)
    length ev `shouldBe` 22
    verifyChain "certificate" ev `shouldBe` Right ()
