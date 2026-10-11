-- | Intents, events and history records, with their JSON wire formats.
--
-- An 'Intent' is a request to move money. It carries no authority: it
-- becomes an executed 'Event' only through "Palisade.Authorize".
-- Everything else that changes state (opening an account, an inbound
-- deposit, its settlement or reversal) is an administrative event that
-- money arriving from outside can produce without a policy decision.
module Palisade.Types
  ( Account (..)
  , accountText
  , IntentKind (..)
  , intentKindText
  , Intent (..)
  , intentCurrency
  , intentHash
  , Event (..)
  , eventTime
  , isAdminEvent
  , Label (..)
  , HistoryItem (..)
  , historyTime
  , parseHistoryLine
  , parseIntentJson
  , renderUtc
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Palisade.Canonical (hashValue)
import Palisade.Currency
import Palisade.Money

-- | Internal accounts hold the entity's money and can never be overdrawn.
-- External accounts stand for counterparties; their balances are the
-- mirror image of what moved in or out, so the trial balance is zero.
data Account = Internal Text | External Text
  deriving (Eq, Ord, Show)

accountText :: Account -> Text
accountText = \case
  Internal a -> a
  External a -> "ext:" <> a

data IntentKind
  = -- | Pay an external counterparty.
    Pay
  | -- | Move money between two internal accounts.
    TransferInternal
  | -- | Convert into another currency at a stated rate (units of the
    -- target per one unit of the source), rounding down.
    Convert Currency Rational
  deriving (Eq, Show)

intentKindText :: IntentKind -> Text
intentKindText = \case
  Pay -> "pay"
  TransferInternal -> "transfer"
  Convert _ _ -> "convert"

data Intent = Intent
  { intentId :: Text
  , intentTs :: UTCTime
  , intentActor :: Text
  , intentKind :: IntentKind
  , intentFrom :: Text
  , intentTo :: Text
  , intentAmount :: SomeMoney
  , intentMemo :: Text
  }
  deriving (Eq, Show)

intentCurrency :: Intent -> Currency
intentCurrency = someCurrency . intentAmount

intentHash :: Intent -> Text
intentHash = hashValue . toJSON

data Event
  = EvOpen {evTs :: UTCTime, evAccount :: Text}
  | EvDeposit
      { evTs :: UTCTime
      , evTransfer :: Text
      , evAccount :: Text
      , evFrom :: Text
      , evAmount :: SomeMoney
      , evSettled :: Bool
      }
  | EvSettle {evTs :: UTCTime, evTransfer :: Text}
  | EvReverse {evTs :: UTCTime, evTransfer :: Text}
  | -- | An intent that passed authorization, with the hash of the policy
    -- it was authorized under.
    EvExecute {evTs :: UTCTime, evIntent :: Intent, evPolicyHash :: Text}
  deriving (Eq, Show)

eventTime :: Event -> UTCTime
eventTime = evTs

isAdminEvent :: Event -> Bool
isAdminEvent = \case
  EvExecute {} -> False
  _ -> True

-- | Ground truth for benchmarks: was this intent an attack or legitimate?
data Label = Attack | Benign
  deriving (Eq, Show)

-- | One line of a history file: what happened, in order.
data HistoryItem
  = HEvent Event
  | HIntent Intent (Maybe Label)
  | HMeta Object
  deriving (Eq, Show)

historyTime :: HistoryItem -> Maybe UTCTime
historyTime = \case
  HEvent e -> Just (eventTime e)
  HIntent i _ -> Just (intentTs i)
  HMeta _ -> Nothing

renderUtc :: UTCTime -> Text
renderUtc = T.pack . iso8601Show

------------------------------------------------------------------------------
-- JSON

moneyFields :: SomeMoney -> [(Key, Value)]
moneyFields m =
  [ "amount" .= renderMinor (someCurrency m) (someMinor m)
  , "currency" .= someCurrency m
  ]

parseMoneyFields :: Object -> Parser SomeMoney
parseMoneyFields o = do
  amt <- o .: "amount" >>= amountText
  cur <- o .: "currency"
  case parseSomeMoney amt cur of
    Right m -> pure m
    Left (SubMinorPrecision c r) ->
      fail ("amount " <> T.unpack amt <> " has more precision than " <> show c <> " allows (" <> show r <> ")")
    Left (BadDecimal t) -> fail ("not a decimal amount: " <> T.unpack t)
    Left (UnknownCurrency t) -> fail ("unknown currency: " <> T.unpack t)
  where
    amountText :: Value -> Parser Text
    amountText = \case
      String t -> pure t
      _ -> fail "amount must be a decimal string such as \"1200.00\" (JSON numbers are rejected to keep money exact)"

instance ToJSON Intent where
  toJSON i =
    object $
      [ "id" .= intentId i
      , "ts" .= renderUtc (intentTs i)
      , "actor" .= intentActor i
      , "kind" .= intentKindText (intentKind i)
      , "from" .= intentFrom i
      , "to" .= intentTo i
      , "memo" .= intentMemo i
      ]
        <> moneyFields (intentAmount i)
        <> case intentKind i of
          Convert c r -> ["to_currency" .= c, "rate" .= renderRationalExact r]
          _ -> []

-- | Exact text for a rate: a decimal when it terminates, otherwise a
-- fraction such as @"1/3"@. Never an approximation, so logs round-trip and
-- two different rates never hash the same.
renderRationalExact :: Rational -> Text
renderRationalExact r =
  let t = renderRational 40 r
   in if "~" `T.isPrefixOf` t
        then T.pack (show (numerator r)) <> "/" <> T.pack (show (denominator r))
        else t

parseRationalExact :: Text -> Maybe Rational
parseRationalExact t = case T.splitOn "/" (T.strip t) of
  [n, d] -> do
    a <- parseDecimal n
    b <- parseDecimal d
    if b == 0 || denominator a /= 1 || denominator b /= 1 then Nothing else Just (a / b)
  [x] -> parseDecimal x
  _ -> Nothing

instance FromJSON Intent where
  parseJSON = withObject "Intent" parseIntentObject

parseIntentObject :: Object -> Parser Intent
parseIntentObject o = do
  iid <- o .: "id"
  ts <- o .: "ts"
  actor <- o .:? "actor" .!= "unknown"
  kindT <- o .:? "kind" .!= ("pay" :: Text)
  from <- o .: "from"
  amt <- parseMoneyFields o
  memo <- o .:? "memo" .!= ""
  kind <- case kindT of
    "pay" -> pure Pay
    "transfer" -> pure TransferInternal
    "convert" -> do
      tc <- o .: "to_currency"
      rt <- o .: "rate"
      r <- maybe (fail ("bad rate: " <> T.unpack rt)) pure (parseRationalExact rt)
      pure (Convert tc r)
    other -> fail ("unknown intent kind: " <> T.unpack other <> " (expected pay, transfer or convert)")
  to <- case kind of
    Convert _ _ -> o .:? "to" .!= from
    _ -> o .: "to"
  pure (Intent iid ts actor kind from to amt memo)

instance ToJSON Event where
  toJSON = \case
    EvOpen ts a -> object ["type" .= ("open" :: Text), "ts" .= renderUtc ts, "account" .= a]
    EvDeposit ts tid a f m s ->
      object $
        [ "type" .= ("deposit" :: Text)
        , "ts" .= renderUtc ts
        , "id" .= tid
        , "account" .= a
        , "from" .= f
        , "settled" .= s
        ]
          <> moneyFields m
    EvSettle ts tid -> object ["type" .= ("settle" :: Text), "ts" .= renderUtc ts, "id" .= tid]
    EvReverse ts tid -> object ["type" .= ("reverse" :: Text), "ts" .= renderUtc ts, "id" .= tid]
    EvExecute ts i ph ->
      object ["type" .= ("execute" :: Text), "ts" .= renderUtc ts, "intent" .= i, "policy" .= ph]

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o -> do
    ty <- o .: "type"
    parseEventObject ty o

parseEventObject :: Text -> Object -> Parser Event
parseEventObject ty o = case ty of
  "open" -> EvOpen <$> o .: "ts" <*> o .: "account"
  "deposit" ->
    EvDeposit
      <$> o .: "ts"
      <*> o .: "id"
      <*> o .: "account"
      <*> o .:? "from" .!= "unknown"
      <*> parseMoneyFields o
      <*> o .:? "settled" .!= True
  "settle" -> EvSettle <$> o .: "ts" <*> o .: "id"
  "reverse" -> EvReverse <$> o .: "ts" <*> o .: "id"
  "execute" -> EvExecute <$> o .: "ts" <*> o .: "intent" <*> o .: "policy"
  other -> fail ("unknown event type: " <> T.unpack other)

-- | Parse one line of a history file.
parseHistoryLine :: BL.ByteString -> Either String HistoryItem
parseHistoryLine bs = eitherDecode bs >>= \case
  Object o -> parseEither historyObject o
  _ -> Left "each line must be a JSON object"

historyObject :: Object -> Parser HistoryItem
historyObject obj = do
  ty <- obj .: "type"
  case ty :: Text of
    "intent" -> do
      i <- parseIntentObject obj
      lbl <- obj .:? "label"
      l <- case lbl :: Maybe Text of
        Nothing -> pure Nothing
        Just "attack" -> pure (Just Attack)
        Just "benign" -> pure (Just Benign)
        Just other -> fail ("label must be attack or benign, got " <> T.unpack other)
      pure (HIntent i l)
    "meta" -> pure (HMeta obj)
    "execute" -> fail "history files cannot contain execute events; give the intent instead"
    _ -> HEvent <$> parseEventObject ty obj

parseIntentJson :: BL.ByteString -> Either String Intent
parseIntentJson = eitherDecode
