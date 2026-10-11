-- | Append-only hash chains.
--
-- Both the event log and the evidence log are JSON Lines files in which
-- every entry commits to the previous one:
--
-- > hash = sha256(prev <> "\n" <> canonical({"seq": n, <key>: payload}))
--
-- Changing, removing, or reordering any byte breaks every later hash.
module Palisade.Chain
  ( Entry (..)
  , mkEntry
  , entryHashOf
  , ChainError (..)
  , renderChainError
  , verifyChain
  , decodeEntries
  , encodeEntry
    -- * Event log
  , LogEntry
  , appendEvent
  , appendObserved
  , ReplayError (..)
  , renderReplayError
  , replayLog
  , replayUntil
  ) where

import Data.Aeson
import Data.Char (isSpace)
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import qualified Data.Text as T
import Palisade.Canonical
import Palisade.Ledger
import Palisade.Types

data Entry a = Entry
  { entrySeq :: !Int
  , entryPayload :: !a
  , entryPrev :: !Text
  , entryHash :: !Text
  , entrySig :: !(Maybe Value)
  -- ^ Optional signature over 'entryHash'; outside the hash by design.
  }
  deriving (Eq, Show)

entryHashOf :: ToJSON a => Text -> Int -> Text -> a -> Text
entryHashOf key n prev payload =
  sha256HexText (prev <> "\n" <> canonicalText (object ["seq" .= n, fromText key .= payload]))

mkEntry :: ToJSON a => Text -> Int -> Text -> a -> Entry a
mkEntry key n prev payload = Entry n payload prev (entryHashOf key n prev payload) Nothing

encodeEntry :: ToJSON a => Text -> Entry a -> BL.ByteString
encodeEntry key e =
  canonical . object $
    [ "seq" .= entrySeq e
    , fromText key .= entryPayload e
    , "prev" .= entryPrev e
    , "hash" .= entryHash e
    ]
      <> maybe [] (\s -> ["sig" .= s]) (entrySig e)

decodeEntries :: FromJSON a => Text -> BL.ByteString -> Either (Int, String) [Entry a]
decodeEntries key bs = traverse one (zip [1 ..] (filter (not . BL.null . BLC.dropWhile isSpace) (BLC.lines bs)))
  where
    one (ln, l) = either (\e -> Left (ln, e)) Right $ do
      v <- eitherDecode l
      flip parseEither v $ withObject "entry" $ \o ->
        Entry <$> o .: "seq" <*> o .: fromText key <*> o .: "prev" <*> o .: "hash" <*> o .:? "sig"

data ChainError
  = BadSeq Int Int
  | BadPrev Int
  | BadHash Int
  deriving (Eq, Show)

renderChainError :: ChainError -> Text
renderChainError = \case
  BadSeq want got -> "expected seq " <> tshow want <> " but found " <> tshow got
  BadPrev n -> "entry " <> tshow n <> ": prev does not match the hash of the entry before it"
  BadHash n -> "entry " <> tshow n <> ": hash does not match its contents (tampered or corrupted)"

tshow :: Show a => a -> Text
tshow = T.pack . show

verifyChain :: ToJSON a => Text -> [Entry a] -> Either ChainError ()
verifyChain key = go 1 genesisHash
  where
    go _ _ [] = Right ()
    go n prev (e : es)
      | entrySeq e /= n = Left (BadSeq n (entrySeq e))
      | entryPrev e /= prev = Left (BadPrev n)
      | entryHashOf key n prev (entryPayload e) /= entryHash e = Left (BadHash n)
      | otherwise = go (n + 1) (entryHash e) es

------------------------------------------------------------------------------
-- Event log

type LogEntry = Entry Event

-- | Apply an event and extend the chain.
appendEvent :: LedgerState -> Event -> Either ApplyError (LedgerState, LogEntry)
appendEvent st ev = do
  st' <- applyEvent st ev
  let n = lsSeq st + 1
      e = mkEntry "event" n (lsHead st) ev
  pure (st' {lsSeq = n, lsHead = entryHash e}, e)

-- | Shadow mode: record an intent that happened outside the kernel.
appendObserved :: LedgerState -> Intent -> Either ApplyError LedgerState
appendObserved st i = do
  st' <- applyObserved st i
  let n = lsSeq st + 1
      e = mkEntry "event" n (lsHead st) (EvExecute (intentTs i) i "observed")
  pure st' {lsSeq = n, lsHead = entryHash e}

data ReplayError
  = ReplayChain ChainError
  | ReplayApply Int ApplyError
  deriving (Eq, Show)

renderReplayError :: ReplayError -> Text
renderReplayError = \case
  ReplayChain c -> "chain broken: " <> renderChainError c
  ReplayApply n e -> "entry " <> T.pack (show n) <> " cannot be applied: " <> renderApplyError e

-- | Verify and fold a log from genesis. @replay (xs ++ ys)@ equals
-- replaying @ys@ from the state @xs@ produced.
replayLog :: [LogEntry] -> Either ReplayError LedgerState
replayLog = replayUntil (const False)

-- | Replay entries until the predicate says stop (the entry that triggers
-- it is not applied).
replayUntil :: (LogEntry -> Bool) -> [LogEntry] -> Either ReplayError LedgerState
replayUntil stop = go emptyLedger
  where
    go st [] = Right st
    go st (e : es)
      | stop e = Right st
      | entrySeq e /= lsSeq st + 1 = Left (ReplayChain (BadSeq (lsSeq st + 1) (entrySeq e)))
      | entryPrev e /= lsHead st = Left (ReplayChain (BadPrev (entrySeq e)))
      | otherwise = case appendEvent st (entryPayload e) of
          Left err -> Left (ReplayApply (entrySeq e) err)
          Right (st', e')
            | entryHash e' /= entryHash e -> Left (ReplayChain (BadHash (entrySeq e)))
            | otherwise -> go st' es
