-- | Certificates: the evidence PALISADE emits for every decision.
--
-- A certificate names the intent, the exact ledger state it was judged
-- against (sequence number and hash-chain head), the policy hash, every
-- check with what was observed and what was required, the verdict, and
-- the resulting state. It contains no wall-clock time and no randomness,
-- so replaying the log to the same state with the same policy reproduces
-- it byte for byte. Certificates are chained into an evidence log and can
-- be signed (see "Palisade.Sign").
module Palisade.Certificate
  ( CheckKind (..)
  , Outcome (..)
  , CheckResult (..)
  , isBlocking
  , Verdict (..)
  , verdictText
  , Certificate (..)
  , certHash
  , certVersion
  , EvidenceEntry
  ) where

import Control.Monad (when)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Palisade.Canonical (hashValue)
import Palisade.Chain (Entry)
import Palisade.Types

data CheckKind = KernelCheck | RequireCheck | EscalateCheck
  deriving (Eq, Show)

data Outcome = Pass | Fail | Triggered | Errored
  deriving (Eq, Show)

data CheckResult = CheckResult
  { crRule :: Text
  , crKind :: CheckKind
  , crOutcome :: Outcome
  , crObserved :: Text
  , crRequired :: Text
  }
  deriving (Eq, Show)

-- | Did this check stop the intent from executing on its own?
isBlocking :: CheckResult -> Bool
isBlocking c = crOutcome c /= Pass

data Verdict = Allow | Deny | Escalate
  deriving (Eq, Ord, Show)

verdictText :: Verdict -> Text
verdictText = \case
  Allow -> "allow"
  Deny -> "deny"
  Escalate -> "escalate"

data Certificate = Certificate
  { certIntent :: Intent
  , certIntentHash :: Text
  , certPolicyHash :: Text
  , certPolicyName :: Maybe Text
  , certPreSeq :: Int
  , certPreHead :: Text
  , certChecks :: [CheckResult]
  , certVerdict :: Verdict
  , certApprovedBy :: Maybe Text
  , certPostSeq :: Maybe Int
  , certPostHead :: Maybe Text
  }
  deriving (Eq, Show)

certVersion :: Text
certVersion = "palisade.cert/v0"

certHash :: Certificate -> Text
certHash = hashValue . toJSON

type EvidenceEntry = Entry Certificate

instance ToJSON CheckKind where
  toJSON = String . \case
    KernelCheck -> "kernel"
    RequireCheck -> "require"
    EscalateCheck -> "escalate"

instance FromJSON CheckKind where
  parseJSON = withText "CheckKind" $ \case
    "kernel" -> pure KernelCheck
    "require" -> pure RequireCheck
    "escalate" -> pure EscalateCheck
    t -> fail ("unknown check kind " <> T.unpack t)

instance ToJSON Outcome where
  toJSON = String . \case
    Pass -> "pass"
    Fail -> "fail"
    Triggered -> "triggered"
    Errored -> "error"

instance FromJSON Outcome where
  parseJSON = withText "Outcome" $ \case
    "pass" -> pure Pass
    "fail" -> pure Fail
    "triggered" -> pure Triggered
    "error" -> pure Errored
    t -> fail ("unknown outcome " <> T.unpack t)

instance ToJSON CheckResult where
  toJSON c =
    object
      [ "rule" .= crRule c
      , "kind" .= crKind c
      , "outcome" .= crOutcome c
      , "observed" .= crObserved c
      , "required" .= crRequired c
      ]

instance FromJSON CheckResult where
  parseJSON = withObject "CheckResult" $ \o ->
    CheckResult <$> o .: "rule" <*> o .: "kind" <*> o .: "outcome" <*> o .: "observed" <*> o .: "required"

instance ToJSON Verdict where
  toJSON = String . verdictText

instance FromJSON Verdict where
  parseJSON = withText "Verdict" $ \case
    "allow" -> pure Allow
    "deny" -> pure Deny
    "escalate" -> pure Escalate
    t -> fail ("unknown verdict " <> T.unpack t)

instance ToJSON Certificate where
  toJSON c =
    object
      [ "version" .= certVersion
      , "intent" .= certIntent c
      , "intent_hash" .= certIntentHash c
      , "policy" .= object ["hash" .= certPolicyHash c, "name" .= certPolicyName c]
      , "pre_state" .= object ["seq" .= certPreSeq c, "head" .= certPreHead c]
      , "checks" .= certChecks c
      , "decision" .= certVerdict c
      , "approved_by" .= certApprovedBy c
      , "post_state" .= case (certPostSeq c, certPostHead c) of
          (Just s, Just h) -> object ["seq" .= s, "head" .= h]
          _ -> Null
      ]

instance FromJSON Certificate where
  parseJSON = withObject "Certificate" $ \o -> do
    v <- o .: "version"
    when (v /= certVersion) $ fail ("unsupported certificate version " <> T.unpack v)
    pol <- o .: "policy"
    pre <- o .: "pre_state"
    post <- o .:? "post_state"
    (ps, ph) <- case post of
      Nothing -> pure (Nothing, Nothing)
      Just p -> (\s h -> (Just s, Just h)) <$> p .: "seq" <*> p .: "head"
    Certificate
      <$> o .: "intent"
      <*> o .: "intent_hash"
      <*> pol .: "hash"
      <*> pol .:? "name"
      <*> pre .: "seq"
      <*> pre .: "head"
      <*> o .: "checks"
      <*> o .: "decision"
      <*> o .:? "approved_by"
      <*> pure ps
      <*> pure ph
