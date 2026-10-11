module Palisade.AuthorizeSpec (spec) where

import Data.Aeson (eitherDecode, encode, toJSON)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Gen
import Palisade.Authorize
import Palisade.Canonical (canonical)
import Palisade.Certificate
import Palisade.Chain
import Palisade.Currency
import Palisade.Ledger
import Palisade.Money (mkSomeMoney)
import Palisade.Policy (Policy)
import Palisade.Sign
import Palisade.Types
import Test.Hspec
import Test.QuickCheck

hardened :: IO Policy
hardened = loadScenarioPolicy "scenarios/policies/hardened.pal"

spec :: Spec
spec = describe "Palisade.Authorize" $ do
  let (_, st0) = setupLedger demoSetup
  polP <- runIO hardened

  it "allows a payment that satisfies every check and commits it" $ do
    pol <- hardened
    let d = authorize pol st0 (payIntent "ok" 10 "treasury" "aws" USD 1240000)
    decVerdict d `shouldBe` Allow
    isJust (decEntry d) `shouldBe` True
    settledBalance "treasury" USD (decState d) `shouldBe` 200000000 - 1240000
    certPostSeq (decCertificate d) `shouldBe` Just (lsSeq st0 + 1)

  it "denies, without touching the ledger, when a require rule fails" $ do
    pol <- hardened
    let d = authorize pol st0 (payIntent "sanc" 10 "treasury" "ofac:garantex" USD 100)
    decVerdict d `shouldBe` Deny
    decState d `shouldBe` st0
    isNothing (decEntry d) `shouldBe` True

  it "escalates, and a human approval turns an escalation into an allow" $ do
    pol <- hardened
    let i = payIntent "new" 10 "ap-agent" "acme-supp1ies" USD 980000
    decVerdict (authorize pol st0 i) `shouldBe` Escalate
    let approved = authorizeWith pol (Just (approvedBy "cfo")) st0 i
    decVerdict approved `shouldBe` Allow
    certApprovedBy (decCertificate approved) `shouldBe` Just "cfo"

  it "an approval never overrides a denial" $ do
    pol <- hardened
    let i = payIntent "big-new" 10 "treasury" "acme-supp1ies" USD 50000000
    decVerdict (authorizeWith pol (Just (approvedBy "cfo")) st0 i) `shouldBe` Deny

  it "kernel checks stop pending funds, duplicates, backdating and unknown currencies" $ do
    pol <- hardened
    let (_, stP) = setupLedger (demoSetup <> [EvDeposit (at 5) "p" "ap-agent" "bank" (mkSomeMoney USD 100000000) False])
        rules d = [crRule c | c <- certChecks (decCertificate d), isBlocking c]
    rules (authorize pol stP (payIntent "p1" 10 "ap-agent" "landlord-llc" USD 9000000)) `shouldContain` ["kernel: funded"]
    let d1 = authorize pol st0 (payIntent "dup" 10 "treasury" "aws" USD 100)
    rules (authorize pol (decState d1) (payIntent "dup" 11 "treasury" "aws" USD 100)) `shouldContain` ["kernel: fresh"]
    rules (authorize pol (decState d1) (payIntent "old" 1 "treasury" "aws" USD 100)) `shouldContain` ["kernel: fresh"]
    rules (authorize pol st0 (payIntent "jpy" 10 "treasury" "aws" JPY 100)) `shouldContain` ["kernel: policy covers currency"]

  it "never changes the ledger unless the verdict is allow" $
    let pol = polP
     in property $ forAll (genPay st0) $ \i ->
      let d = authorize pol st0 i
       in (decVerdict d /= Allow) ==> (decState d === st0)

  it "is deterministic: the same state and intent give the same certificate" $
    let pol = polP
     in property $ forAll (genPay st0) $ \i ->
      certHash (decCertificate (authorize pol st0 i)) === certHash (decCertificate (authorize pol st0 i))

  it "certificates round-trip through JSON with the same hash" $
    let pol = polP
     in property $ forAll (genPay st0) $ \i ->
      let c = decCertificate (authorize pol st0 i)
       in fmap certHash (eitherDecode (encode c)) === Right (certHash c)

  it "signatures verify, and fail for any other hash" $ do
    k <- generateKey
    let h = "aa" <> T.replicate 62 "0"
        sig = signHash k h
    verifySignature sig h `shouldBe` Right (publicKeyHex k)
    verifySignature sig ("bb" <> T.drop 2 h) `shouldBe` Left "signature does not verify"
    fmap publicKeyHex (decodeSecret (encodeSecret k)) `shouldBe` Right (publicKeyHex k)

  it "canonical encoding is stable regardless of key order" $
    canonical (toJSON (EvOpen t0 "x")) `shouldBe` "{\"account\":\"x\",\"ts\":\"2026-10-01T00:00:00Z\",\"type\":\"open\"}"

  it "a committed allow extends the chain that replay verifies" $ do
    pol <- hardened
    let (entries, st) = setupLedger demoSetup
        d = authorize pol st (payIntent "ok" 10 "treasury" "aws" USD 100)
    case decEntry d of
      Nothing -> expectationFailure "expected an allow"
      Just e -> replayLog (entries <> [e]) `shouldBe` Right (decState d)

genPay :: LedgerState -> Gen Intent
genPay _ = do
  from <- elements ["treasury", "ap-agent", "agent-wallet", "nobody"]
  to <- elements ["aws", "acme-supplies", "attacker", "ofac:garantex", "treasury", "openai"]
  c <- elements [USD, USDC, EUR]
  n <- frequency [(3, chooseInteger (1, 10 ^ (7 :: Int))), (1, chooseInteger (1, 10 ^ (12 :: Int)))]
  s <- chooseInteger (0, 100000)
  k <- chooseInt (0, 1000000)
  pure (payIntent ("g" <> T.pack (show k)) s from to c n)
