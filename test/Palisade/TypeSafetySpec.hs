{-# OPTIONS_GHC -O0 -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Compile-time guarantees, tested by deferring type errors to runtime.
--
-- Each binding below is ill-typed. GHC compiles it to a runtime error only
-- because of -fdefer-type-errors, and the test asserts that forcing it
-- raises a type error. If any of them ever typechecked, its test would
-- fail. Each lives in its own top-level binding so that the deferred error
-- fires when that binding is forced, not when the spec is built.
module Palisade.TypeSafetySpec (spec) where

import qualified Control.Exception as E
import qualified Data.Text as T
import Gen
import Palisade.Authorize
import Palisade.Currency
import Palisade.Ledger
import Palisade.Money
import Palisade.Policy
import Palisade.Proof
import Palisade.Settlement
import Test.Hspec hiding (pending)

shouldNotTypecheck :: a -> Expectation
shouldNotTypecheck x = do
  r <- E.try (E.evaluate x)
  case r of
    Left (E.TypeError _) -> pure ()
    Right _ -> expectationFailure "expected a type error, but the expression typechecked"

usd :: Money 'USD
usd = fromMinor 100

eur :: Money 'EUR
eur = fromMinor 100

pendingDeposit :: Transfer 'Pending
pendingDeposit = pending "t1" "treasury" "bank" (mkSomeMoney USD 100)

-- Ill-typed: dollars plus euros.
mixedCurrencies :: Integer
mixedCurrencies = toMinor (usd <> eur)

-- Ill-typed: spending a transfer that has not settled.
spendPending :: Int
spendPending = T.length (fst (spendable pendingDeposit))

-- Ill-typed: a USD policy rule compared against a EUR literal.
mixedPolicy :: Int
mixedPolicy = T.length (prettyTyped ((amount :: Expr 'USD (Q 'USD)) .<= (cash 5 :: Expr 'EUR (Q 'EUR))))

-- Ill-typed: proofs about state s1 used to commit against state s2.
staleCommit :: Policy -> LedgerState -> Int
staleCommit pol st =
  name pol $ \p -> name st $ \s1 -> name st $ \s2 -> name (payIntent "x" 10 "treasury" "aws" USD 100) $ \ni ->
    let (_, _, pp) = checkPolicy p s1 ni Nothing
     in case (snd (checkWellFormed ni), snd (checkKnown s1 ni), snd (checkFresh s1 ni), snd (checkFunded s1 ni), pp) of
          (Just a, Just b, Just c, Just d, Just e) ->
            either (const 0) (lsSeq . fst) (commit s2 (mkExecutable p s1 ni a b c d e))
          _ -> -1

spec :: Spec
spec = describe "compile-time guarantees" $ do
  it "adding dollars to euros does not typecheck" $
    shouldNotTypecheck mixedCurrencies

  it "a pending transfer cannot be spent" $
    shouldNotTypecheck spendPending

  it "a USD policy cannot compare against a EUR amount" $
    shouldNotTypecheck mixedPolicy

  it "proofs about one state cannot commit against another" $ do
    pol <- loadScenarioPolicy "scenarios/policies/hardened.pal"
    let (_, st) = setupLedger demoSetup
    shouldNotTypecheck (staleCommit pol st)
