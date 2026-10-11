module Palisade.StressSpec (spec) where

import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Gen
import Palisade.Authorize
import Palisade.Certificate
import Palisade.Chain
import Palisade.Ledger
import Palisade.Money
import Palisade.Stress
import Palisade.Types
import Test.Hspec

-- | A deliberately broken kernel: it counts pending deposits as spendable.
-- This is the bug the type-level settlement states rule out in the real
-- kernel; the stress tester must find it on its own.
pendingCountsAsFunds :: Authorizer
pendingCountsAsFunds pol st i =
  let d = authorize pol st i
      c = intentCurrency i
      need = someMinor (intentAmount i)
      withPending = settledBalance (intentFrom i) c st + pendingInbound (intentFrom i) c st
      onlyFundingFailed =
        [crRule r | r <- certChecks (decCertificate d), isBlocking r] == ["kernel: funded"]
   in if decVerdict d /= Allow && onlyFundingFailed && withPending >= need
        then case applyObserved st i of
          Right st' ->
            let n = lsSeq st + 1
                e = mkEntry "event" n (lsHead st) (EvExecute (intentTs i) i (policyHashOf d))
             in d {decVerdict = Allow, decState = st' {lsSeq = n, lsHead = entryHash e}, decEntry = Just e, decCertificate = (decCertificate d) {certVerdict = Allow, certPostSeq = Just n, certPostHead = Just (entryHash e)}}
          Left _ -> d
        else d
  where
    policyHashOf = certPolicyHash . decCertificate

spec :: Spec
spec = describe "Palisade.Stress" $ do
  let (setup, _) = setupLedger demoSetup

  it "the hardened policy keeps every kernel and policy law" $ do
    pol <- loadScenarioPolicy "scenarios/policies/hardened.pal"
    let rep = runStress (defaultStressConfig pol setup) {scCount = 300}
    [lsLawName l | l <- srLaws rep, isJust (lsCounterexample l)] `shouldBe` []

  it "finds that static caps do not guarantee the budget laws" $ do
    pol <- loadScenarioPolicy "scenarios/policies/naive.pal"
    let rep = runStress (defaultStressConfig pol setup) {scCount = 300}
        broken = [lsLawName l | l <- srLaws rep, isJust (lsCounterexample l)]
    broken `shouldBe` ["law \"daily outflow under 15% of assets\" (USD)", "law \"agent daily budget\" (USDC)"]

  it "finds a planted kernel bug and shrinks it to a short chain" $ do
    pol <- loadScenarioPolicy "scenarios/policies/naive.pal"
    let rep = runStress (defaultStressConfig pol setup) {scCount = 500, scAuthorizer = pendingCountsAsFunds}
        found = [ce | l <- srLaws rep, lsLawName l == "no-overdraft", Just ce <- [lsCounterexample l]]
    case found of
      [] -> expectationFailure "the stress tester did not find the planted bug"
      (ce : _) -> do
        length (ceSteps ce) `shouldSatisfy` (<= 4)
        any (T.isInfixOf "(pending)" . tlAction) (ceTrace ce) `shouldBe` True

  it "is reproducible from its seed" $ do
    pol <- loadScenarioPolicy "scenarios/policies/naive.pal"
    let cfg = (defaultStressConfig pol setup) {scCount = 50, scSeed = 7}
        summary r = [(lsLawName l, fmap (map tlAction . ceTrace) (lsCounterexample l)) | l <- srLaws r]
    summary (runStress cfg) `shouldBe` summary (runStress cfg)

  it "re-running one case reproduces its counterexample" $ do
    pol <- loadScenarioPolicy "scenarios/policies/naive.pal"
    let cfg = (defaultStressConfig pol setup) {scCount = 300}
        rep = runStress cfg
    case [ce | l <- srLaws rep, Just ce <- [lsCounterexample l]] of
      [] -> expectationFailure "expected a counterexample"
      (ce : _) -> do
        let again = runStress cfg {scOnlyCase = Just (ceCase ce)}
        any (isJust . lsCounterexample) (srLaws again) `shouldBe` True
        isNothing (crViolation (runCase cfg [])) `shouldBe` True
