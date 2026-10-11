-- | Run a history of events and intents through the kernel.
--
-- * __Shadow mode__ mirrors reality. Every intent in the history really
--   happened elsewhere, so it is applied whatever PALISADE decides; the
--   output is a certificate per intent and a "would have blocked" verdict.
--   Zero integration risk: PALISADE only reads.
--
-- * __Counterfactual mode__ (policy backtesting) applies only what the
--   policy allows, so you can see what last quarter would have looked like
--   under a proposed policy before you publish it.
module Palisade.Simulate
  ( Mode (..)
  , Escalations (..)
  , IntentOutcome (..)
  , SimReport (..)
  , simulate
  , chainEvidence
  ) where

import Data.List (foldl')
import Data.Text (Text)
import Palisade.Authorize
import Palisade.Certificate
import Palisade.Chain
import Palisade.Ledger
import Palisade.Policy (Policy)
import Palisade.Types

-- | What counterfactual mode does with an escalation.
data Escalations
  = -- | Treat it as blocked (the human never answered).
    EscalationsBlocked
  | -- | Assume a human approved it.
    EscalationsApproved
  | -- | Benchmarks only: a human approves intents labelled benign and
    -- rejects those labelled attack.
    EscalationsByLabel
  deriving (Eq, Show)

data Mode = ShadowMode | CounterfactualMode Escalations
  deriving (Eq, Show)

data IntentOutcome = IntentOutcome
  { oLine :: Int
  , oIntent :: Intent
  , oLabel :: Maybe Label
  , oVerdict :: Verdict
  -- ^ The policy's verdict before any human approval.
  , oCertificate :: Certificate
  , oApplied :: Bool
  -- ^ Did the money move (in this mode)?
  , oNote :: Maybe Text
  }

data SimReport = SimReport
  { srOutcomes :: [IntentOutcome]
  , srErrors :: [(Int, Text)]
  , srFinal :: LedgerState
  , srEvidence :: [EvidenceEntry]
  }

data Acc = Acc
  { aState :: !LedgerState
  , aOutcomes :: [IntentOutcome]
  , aErrors :: [(Int, Text)]
  }

simulate :: Mode -> Policy -> [(Int, HistoryItem)] -> SimReport
simulate mode pol items =
  let Acc final outs errs = foldl' step (Acc emptyLedger [] []) items
      outcomes = reverse outs
   in SimReport outcomes (reverse errs) final (chainEvidence (map oCertificate outcomes))
  where
    step acc (ln, item) = case item of
      HMeta _ -> acc
      HEvent ev -> case appendEvent (aState acc) ev of
        Right (st', _) -> acc {aState = st'}
        Left err -> acc {aErrors = (ln, renderApplyError err) : aErrors acc}
      HIntent i lbl ->
        let st = aState acc
            dec = authorize pol st i
            v = decVerdict dec
            approve who =
              let d2 = authorizeWith pol (Just (approvedBy who)) st i
               in if decVerdict d2 == Allow then Just d2 else Nothing
            record st' cert applied note =
              acc
                { aState = st'
                , aOutcomes = IntentOutcome ln i lbl v cert applied note : aOutcomes acc
                }
         in case mode of
              ShadowMode
                | v == Allow -> record (decState dec) (decCertificate dec) True Nothing
                | otherwise -> case appendObserved st i of
                    Right st' -> record st' (decCertificate dec) True (Just "happened anyway (shadow mode)")
                    Left err ->
                      (record st (decCertificate dec) False (Just ("could not mirror: " <> renderApplyError err)))
                        { aErrors = (ln, renderApplyError err) : aErrors acc
                        }
              CounterfactualMode esc
                | v == Allow -> record (decState dec) (decCertificate dec) True Nothing
                | v == Escalate,
                  approves esc lbl,
                  Just d2 <- approve (approver esc) ->
                    record (decState d2) (decCertificate d2) True (Just "escalated, then approved")
                | v == Escalate -> record st (decCertificate dec) False (Just "escalated; not approved")
                | otherwise -> record st (decCertificate dec) False Nothing

    approves esc lbl = case esc of
      EscalationsBlocked -> False
      EscalationsApproved -> True
      EscalationsByLabel -> lbl == Just Benign
    approver esc = case esc of
      EscalationsByLabel -> "benchmark-reviewer"
      _ -> "backtest-assumed-approval"

-- | Chain certificates into an evidence log.
chainEvidence :: [Certificate] -> [EvidenceEntry]
chainEvidence = go 1 genesis
  where
    genesis = entryPrevGenesis
    go _ _ [] = []
    go n prev (c : cs) =
      let e = mkEntry "certificate" n prev c
       in e : go (n + 1) (entryHash e) cs

entryPrevGenesis :: Text
entryPrevGenesis = lsHead emptyLedger
