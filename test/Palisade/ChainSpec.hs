module Palisade.ChainSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (foldl')
import Gen
import Palisade.Chain
import Palisade.Ledger
import Palisade.Types
import Test.Hspec
import Test.QuickCheck

-- | Build a log from the events the ledger accepts.
buildLog :: [Event] -> [LogEntry]
buildLog evs = reverse . snd $ foldl' step (base, reverse baseEntries) evs
  where
    (baseEntries, base) = setupLedger [EvOpen t0 "a", EvOpen t0 "b"]
    step (st, acc) ev = case appendEvent st ev of
      Right (st', e) -> (st', e : acc)
      Left _ -> (st, acc)

spec :: Spec
spec = describe "Palisade.Chain" $ do
  it "replay is deterministic" $
    property $ forAll genLedgerEvents $ \evs ->
      let es = buildLog evs in replayLog es === replayLog es

  it "replay equals the state that built the log" $
    property $ forAll genLedgerEvents $ \evs ->
      let es = buildLog evs
          direct = foldl' (\st e -> either (const st) fst (appendEvent st (entryPayload e))) emptyLedger es
       in replayLog es === Right direct

  it "is a fold homomorphism: replaying a prefix then the rest equals replaying everything" $
    property $ forAll genLedgerEvents $ \evs ->
      let es = buildLog evs
       in forAll (chooseInt (0, length es)) $ \k ->
            let prefix = replayUntil (\e -> entrySeq e > k) es
                resumed = prefix >>= \st -> foldl' (\acc e -> acc >>= \s -> either (Left . ReplayApply (entrySeq e)) (Right . fst) (appendEvent s (entryPayload e))) (Right st) (drop k es)
             in resumed === replayLog es

  it "detects any change to any entry's content" $
    property $ forAll genLedgerEvents $ \evs ->
      let es = buildLog evs
       in not (null es) ==> forAll (chooseInt (0, length es - 1)) $ \k ->
            let e = es !! k
                forged = e {entryPayload = EvOpen (eventTime (entryPayload e)) "forged-account"}
                es' = take k es <> [forged] <> drop (k + 1) es
             in either (const True) (const False) (verifyChain "event" es')

  it "detects removed and reordered entries" $
    property $ forAll genLedgerEvents $ \evs ->
      let es = buildLog evs
       in length es >= 3 ==>
            ( either (const True) (const False) (verifyChain "event" (drop 1 es))
                .&&. either (const True) (const False) (verifyChain "event" (take 1 es <> [es !! 2, es !! 1] <> drop 3 es))
            )

  it "round-trips entries through their JSON lines" $
    property $ forAll genLedgerEvents $ \evs ->
      let es = buildLog evs
          bytes = BLC.unlines (map (encodeEntry "event") es)
       in decodeEntries "event" bytes === Right es
