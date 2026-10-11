module Palisade.PolicySpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Gen
import Palisade.Authorize
import Palisade.Certificate
import Palisade.Currency
import Palisade.Ledger
import Palisade.Money (mkSomeMoney)
import Palisade.Policy
import Palisade.Policy.Parse (parseExpr, parsePolicyFile)
import Palisade.Types
import Test.Hspec
import Test.QuickCheck

typeErrorOf :: Text -> Maybe Text
typeErrorOf src = either Just (const Nothing) (loadPolicy "t.pal" src)

usdPolicy :: Text -> Text
usdPolicy body = "set merchants = {\"aws\"}\npolicy \"p\" currency USD {\n" <> body <> "\n}\n"

-- | The hardened USD block, written in Haskell with the embedded DSL.
embedded :: Policy
embedded =
  mkPolicy
    [ SomeBlock SUSD $
        PolicyBlock
          "treasury-usd"
          [ require "per-payment cap" (amount .<= cash 100000)
          , require "hourly velocity" (outflow (hours 1) .+ amount .<= cash 150000)
          , require "daily outflow cap" (totalOutflow (hours 24) .+ amount .<= num 0.15 *. totalBalance)
          , escalate "dual authorization" (amount .> cash 50000)
          ]
          []
    ]

parsedEquivalent :: Text
parsedEquivalent =
  T.unlines
    [ "policy \"treasury-usd\" currency USD {"
    , "  require  \"per-payment cap\":   amount <= 100000 USD"
    , "  require  \"hourly velocity\":   outflow(1h) + amount <= 150000 USD"
    , "  require  \"daily outflow cap\": total_outflow(24h) + amount <= 0.15 * total_balance"
    , "  escalate \"dual authorization\": amount > 50000 USD"
    , "}"
    ]

spec :: Spec
spec = describe "Palisade.Policy" $ do
  describe "parsing" $ do
    it "pretty-printing then parsing returns the same expression" $
      property $ forAll (sized (genSExpr . min 12)) $ \e ->
        counterexample (T.unpack (prettyExpr e)) (parseExpr (prettyExpr e) === Right e)

    it "pretty-printing then parsing returns the same policy file" $
      property $ forAll (resize 6 genPolicyFile) $ \f ->
        counterexample (T.unpack (prettyFile f)) (parsePolicyFile "t.pal" (prettyFile f) === Right f)

    it "reads both comment styles and multi-line rules" $
      typeErrorOf (usdPolicy "  # a comment\n  -- another\n  require \"cap\":\n    amount <= 10 USD") `shouldBe` Nothing

  describe "typechecking" $ do
    it "rejects money in the wrong currency, with a position" $
      typeErrorOf (usdPolicy "  require \"cap\": amount <= 10 EUR")
        `shouldBe` Just "t.pal:3:28: this policy is for USD but the literal is in EUR; PALISADE never converts currencies implicitly"

    it "rejects comparing money with a bare number and suggests the fix" $
      fmap (T.isInfixOf "write a money literal with its currency") (typeErrorOf (usdPolicy "  require \"cap\": amount <= 10"))
        `shouldBe` Just True

    it "rejects intent fields inside laws" $
      fmap (T.isInfixOf "laws are checked against the ledger alone") (typeErrorOf (usdPolicy "  law \"l\": amount <= 10 USD"))
        `shouldBe` Just True

    it "rejects unknown names, multiplying money by money, and duplicate blocks" $ do
      fmap (T.isInfixOf "unknown name") (typeErrorOf (usdPolicy "  require \"x\": amont <= 10 USD")) `shouldBe` Just True
      fmap (T.isInfixOf "cannot multiply money by money") (typeErrorOf (usdPolicy "  require \"x\": amount * balance <= 10 USD")) `shouldBe` Just True
      fmap (T.isInfixOf "a second policy for USD") (typeErrorOf "policy \"a\" currency USD {}\npolicy \"b\" currency USD {}\n") `shouldBe` Just True

    it "requires every rule to be a condition" $
      fmap (T.isInfixOf "must be a true/false condition") (typeErrorOf (usdPolicy "  require \"x\": amount")) `shouldBe` Just True

  describe "canonical form and hashing" $ do
    it "the canonical form is a fixed point" $ do
      pol <- loadScenarioPolicy "scenarios/policies/hardened.pal"
      fmap policyHash (loadPolicy "canonical" (policySource pol)) `shouldBe` Right (policyHash pol)

    it "embedded and parsed forms of the same rules hash identically" $
      fmap policyHash (loadPolicy "p.pal" parsedEquivalent) `shouldBe` Right (policyHash embedded)

    it "changing a set's members changes the hash" $ do
      let a = loadPolicy "a" (usdPolicy "  require \"m\": counterparty in merchants")
          b = loadPolicy "b" (T.replace "{\"aws\"}" "{\"aws\", \"gcp\"}" (usdPolicy "  require \"m\": counterparty in merchants"))
      (policyHash <$> a) `shouldNotBe` (policyHash <$> b)

  describe "evaluation" $ do
    it "embedded and parsed policies give identical check results" $ do
      parsed <- either (fail . T.unpack) pure (loadPolicy "p.pal" parsedEquivalent)
      let (_, st) = setupLedger demoSetup
          intents = [payIntent ("i" <> T.pack (show n)) (10 * n) "treasury" "aws" USD (n * 1500000) | n <- [1 .. 12]]
          checks pol = map (certChecks . decCertificate . authorize pol st) intents
      checks parsed `shouldBe` checks embedded

    it "explains what was observed and what was required" $ do
      pol <- loadScenarioPolicy "scenarios/policies/hardened.pal"
      let (_, st) = setupLedger demoSetup
          dec = authorize pol st (payIntent "big" 10 "treasury" "aws" USD 12000000)
          cap = [c | c <- certChecks (decCertificate dec), crRule c == "per-payment cap"]
      map crOutcome cap `shouldBe` [Fail]
      map crObserved cap `shouldBe` ["amount = 120000.00 USD"]
      map crRequired cap `shouldBe` ["<= 100000.00 USD"]

    it "computes windows, duplicates and typical amounts from history" $ do
      pol <- either (fail . T.unpack) pure . loadPolicy "w.pal" . usdPolicy $
        T.unlines
          [ "  require \"no dup\": duplicates(1h) == 0"
          , "  escalate \"unusual\": known_counterparty and amount > 3 * typical(90d)"
          , "  require \"count\": count(1h) < 3"
          ]
      let (_, st0) = setupLedger demoSetup
          run st i = let d = authorize pol st i in (decVerdict d, decState d)
          (v1, st1) = run st0 (payIntent "a" 10 "treasury" "aws" USD 10000)
          (v2, _) = run st1 (payIntent "b" 20 "treasury" "aws" USD 10000)
          (v3, st3) = run st1 (payIntent "c" 30 "treasury" "aws" USD 20000)
          (v4, _) = run st3 (payIntent "d" 40 "treasury" "aws" USD 70000)
          (v5, _) = run st3 (payIntent "e" 7200 "treasury" "aws" USD 60000)
      [v1, v2, v3, v4, v5] `shouldBe` [Allow, Deny, Allow, Escalate, Allow]

    it "short-circuits so guarded division never fails" $ do
      pol <- either (fail . T.unpack) pure . loadPolicy "d.pal" $
        "policy \"p\" currency EUR {\n  require \"ratio\": balance == 0 EUR or amount / balance <= 0.5\n}\n"
      let (_, st) = setupLedger [EvOpen t0 "a", EvDeposit t0 "d" "a" "bank" (mkEur 1000) True]
          mkEur = mkSomeMoney EUR
      decVerdict (authorize pol st (payIntent "x" 1 "a" "v" EUR 400)) `shouldBe` Allow
      decVerdict (authorize pol st (payIntent "y" 1 "a" "v" EUR 600)) `shouldBe` Deny

    it "counts only the policy currency in totals" $ do
      let (_, st) = setupLedger demoSetup
      M.lookup USD (trialBalance st) `shouldBe` Just 0
      totalInternal USD st `shouldBe` 205000000
      totalInternal USDC st `shouldBe` 60000000000
