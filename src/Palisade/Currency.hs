-- | Currencies as a closed, promoted kind.
--
-- Every amount in PALISADE carries its currency in its type
-- (@Money 'USD@), so adding dollars to euros is a compile error rather than
-- a reconciliation finding. At the edges, where currencies arrive as data,
-- 'SomeCurrency' and the 'SCurrency' singleton recover the type index.
module Palisade.Currency
  ( Currency (..)
  , SCurrency (..)
  , KnownCurrency (..)
  , SomeCurrency (..)
  , fromSing
  , toSomeCurrency
  , withKnownCurrency
  , minorDigits
  , currencyCode
  , parseCurrency
  , allCurrencies
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Type.Equality (TestEquality (..), (:~:) (..))

-- | The currencies v0.1 understands. Fiat uses ISO 4217 minor units;
-- stablecoins and crypto use their on-chain precision.
data Currency = USD | EUR | GBP | JPY | CHF | SGD | USDC | USDT | BTC | ETH
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

allCurrencies :: [Currency]
allCurrencies = [minBound .. maxBound]

-- | Singleton: a runtime witness for a type-level currency.
data SCurrency (c :: Currency) where
  SUSD :: SCurrency 'USD
  SEUR :: SCurrency 'EUR
  SGBP :: SCurrency 'GBP
  SJPY :: SCurrency 'JPY
  SCHF :: SCurrency 'CHF
  SSGD :: SCurrency 'SGD
  SUSDC :: SCurrency 'USDC
  SUSDT :: SCurrency 'USDT
  SBTC :: SCurrency 'BTC
  SETH :: SCurrency 'ETH

instance Show (SCurrency c) where
  show = show . fromSing

class KnownCurrency (c :: Currency) where
  currencySing :: SCurrency c

instance KnownCurrency 'USD where currencySing = SUSD
instance KnownCurrency 'EUR where currencySing = SEUR
instance KnownCurrency 'GBP where currencySing = SGBP
instance KnownCurrency 'JPY where currencySing = SJPY
instance KnownCurrency 'CHF where currencySing = SCHF
instance KnownCurrency 'SGD where currencySing = SSGD
instance KnownCurrency 'USDC where currencySing = SUSDC
instance KnownCurrency 'USDT where currencySing = SUSDT
instance KnownCurrency 'BTC where currencySing = SBTC
instance KnownCurrency 'ETH where currencySing = SETH

fromSing :: SCurrency c -> Currency
fromSing = \case
  SUSD -> USD
  SEUR -> EUR
  SGBP -> GBP
  SJPY -> JPY
  SCHF -> CHF
  SSGD -> SGD
  SUSDC -> USDC
  SUSDT -> USDT
  SBTC -> BTC
  SETH -> ETH

-- | Bring a singleton's 'KnownCurrency' instance into scope.
withKnownCurrency :: SCurrency c -> (KnownCurrency c => r) -> r
withKnownCurrency s k = case s of
  SUSD -> k
  SEUR -> k
  SGBP -> k
  SJPY -> k
  SCHF -> k
  SSGD -> k
  SUSDC -> k
  SUSDT -> k
  SBTC -> k
  SETH -> k

instance TestEquality SCurrency where
  testEquality a b = case (a, b) of
    (SUSD, SUSD) -> Just Refl
    (SEUR, SEUR) -> Just Refl
    (SGBP, SGBP) -> Just Refl
    (SJPY, SJPY) -> Just Refl
    (SCHF, SCHF) -> Just Refl
    (SSGD, SSGD) -> Just Refl
    (SUSDC, SUSDC) -> Just Refl
    (SUSDT, SUSDT) -> Just Refl
    (SBTC, SBTC) -> Just Refl
    (SETH, SETH) -> Just Refl
    _ -> Nothing

data SomeCurrency where
  SomeCurrency :: KnownCurrency c => SCurrency c -> SomeCurrency

toSomeCurrency :: Currency -> SomeCurrency
toSomeCurrency = \case
  USD -> SomeCurrency SUSD
  EUR -> SomeCurrency SEUR
  GBP -> SomeCurrency SGBP
  JPY -> SomeCurrency SJPY
  CHF -> SomeCurrency SCHF
  SGD -> SomeCurrency SSGD
  USDC -> SomeCurrency SUSDC
  USDT -> SomeCurrency SUSDT
  BTC -> SomeCurrency SBTC
  ETH -> SomeCurrency SETH

-- | Number of minor-unit digits (ISO 4217 exponent, or token decimals).
minorDigits :: Currency -> Int
minorDigits = \case
  USD -> 2
  EUR -> 2
  GBP -> 2
  JPY -> 0
  CHF -> 2
  SGD -> 2
  USDC -> 6
  USDT -> 6
  BTC -> 8
  ETH -> 18

currencyCode :: Currency -> Text
currencyCode = T.pack . show

parseCurrency :: Text -> Maybe Currency
parseCurrency t = lookup (T.toUpper (T.strip t)) [(currencyCode c, c) | c <- allCurrencies]

instance ToJSON Currency where
  toJSON = toJSON . currencyCode

instance FromJSON Currency where
  parseJSON = withText "Currency" $ \t ->
    maybe (fail ("unknown currency: " <> T.unpack t)) pure (parseCurrency t)
