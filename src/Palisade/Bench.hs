-- | PolicyBench: score a policy against reconstructions of real
-- 2025-2026 incidents.
--
-- Each incident is a history file whose intents are labelled @attack@ or
-- @benign@. A policy catches an attack by denying or escalating it. An
-- escalation is a human checkpoint, so the benchmark assumes the reviewer
-- approves benign intents and rejects attacks; escalating benign traffic
-- costs review effort, which is reported separately from false denials.
module Palisade.Bench
  ( Incident (..)
  , mkIncident
  , IncidentScore (..)
  , scoreIncident
  , BenchTotals (..)
  , benchTotals
  ) where

import qualified Data.Aeson.KeyMap as KM
import Data.Aeson (Value (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Palisade.Certificate
import Palisade.Currency
import Palisade.Money
import Palisade.Policy (Policy)
import Palisade.Simulate
import Palisade.Types

data Incident = Incident
  { incName :: Text
  , incTitle :: Text
  , incSource :: Text
  , incSummary :: Text
  , incItems :: [(Int, HistoryItem)]
  }

mkIncident :: Text -> [(Int, HistoryItem)] -> Incident
mkIncident fileName items =
  let metas = [o | (_, HMeta o) <- items]
      field k = case [t | o <- metas, Just (String t) <- [KM.lookup k o]] of
        (t : _) -> t
        [] -> ""
      nm = field "name"
   in Incident (if nm == "" then fileName else nm) (field "title") (field "source") (field "summary") items

data IncidentScore = IncidentScore
  { scIncident :: Incident
  , scAttacks :: Int
  , scDenied :: Int
  , scEscalated :: Int
  , scMissed :: Int
  , scBenign :: Int
  , scBenignEscalated :: Int
  , scBenignDenied :: Int
  , scLost :: Map Currency Integer
  , scOutcomes :: [IntentOutcome]
  , scErrors :: [(Int, Text)]
  }

scoreIncident :: Policy -> Incident -> IncidentScore
scoreIncident pol inc =
  let rep = simulate (CounterfactualMode EscalationsByLabel) pol (incItems inc)
      outs = srOutcomes rep
      attacks = [o | o <- outs, oLabel o == Just Attack]
      benign = [o | o <- outs, oLabel o == Just Benign]
      countV v os = length [() | o <- os, oVerdict o == v]
      lost =
        M.fromListWith
          (+)
          [ (someCurrency (intentAmount (oIntent o)), someMinor (intentAmount (oIntent o)))
          | o <- attacks
          , oApplied o
          ]
   in IncidentScore
        inc
        (length attacks)
        (countV Deny attacks)
        (countV Escalate attacks)
        (countV Allow attacks)
        (length benign)
        (countV Escalate benign)
        (countV Deny benign)
        lost
        outs
        (srErrors rep)

data BenchTotals = BenchTotals
  { btIncidents :: Int
  , btFullyCaught :: Int
  , btAttacks :: Int
  , btCaught :: Int
  , btBenign :: Int
  , btBenignDenied :: Int
  , btBenignEscalated :: Int
  , btLost :: Map Currency Integer
  }

benchTotals :: [IncidentScore] -> BenchTotals
benchTotals ss =
  BenchTotals
    (length ss)
    (length [() | s <- ss, scMissed s == 0])
    (sum (map scAttacks ss))
    (sum [scDenied s + scEscalated s | s <- ss])
    (sum (map scBenign ss))
    (sum (map scBenignDenied ss))
    (sum (map scBenignEscalated ss))
    (M.unionsWith (+) (map scLost ss))
