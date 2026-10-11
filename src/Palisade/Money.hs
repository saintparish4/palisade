-- | Exact money.
--
-- An amount is an 'Integer' count of minor units, indexed by its currency.
-- There is no 'Double' and no 'Scientific' on any money path: parsing is
-- exact, conversion takes an explicit 'RoundingMode', and every conversion
-- returns the rounding remainder so conservation can be checked to the last
-- minor unit.
module Palisade.Money
  ( Money
  , fromMinor
  , toMinor
  , zero
  , minus
  , negateMoney
  , money
  , MoneyError (..)
  , SomeMoney (..)
  , mkSomeMoney
  , someCurrency
  , someMinor
  , matchMoney
  , parseSomeMoney
    -- * Decimal text
  , parseDecimal
  , renderMinor
  , renderSomeMoney
  , renderMoney
  , renderRational
  , toMinorExact
  , minorToMajor
    -- * Conversion
  , RoundingMode (..)
  , Rate
  , mkRate
  , rateValue
  , convert
  , convertMinor
  , roundWith
  ) where

import Data.Char (isDigit)
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Type.Equality (testEquality, (:~:) (..))
import Palisade.Currency

-- | An amount of currency @c@, in minor units. The constructor is hidden;
-- build values with 'fromMinor' (always exact) or 'money' (rejects
-- sub-minor-unit precision).
newtype Money (c :: Currency) = Money Integer
  deriving (Eq, Ord)

instance KnownCurrency c => Show (Money c) where
  show = T.unpack . renderMoney

instance Semigroup (Money c) where
  Money a <> Money b = Money (a + b)

instance Monoid (Money c) where
  mempty = Money 0

fromMinor :: Integer -> Money c
fromMinor = Money

toMinor :: Money c -> Integer
toMinor (Money n) = n

zero :: Money c
zero = Money 0

minus :: Money c -> Money c -> Money c
minus (Money a) (Money b) = Money (a - b)

negateMoney :: Money c -> Money c
negateMoney (Money a) = Money (negate a)

data MoneyError
  = SubMinorPrecision Currency Rational
  | BadDecimal Text
  | UnknownCurrency Text
  deriving (Eq, Show)

-- | Build an amount from a major-unit 'Rational'. Rejects input that does
-- not land on a whole minor unit (e.g. @0.001 USD@).
money :: forall c. KnownCurrency c => Rational -> Either MoneyError (Money c)
money r = Money <$> toMinorExact (fromSing (currencySing @c)) r

toMinorExact :: Currency -> Rational -> Either MoneyError Integer
toMinorExact cur r =
  let scaled = r * (10 ^ minorDigits cur)
   in if denominator scaled == 1
        then Right (numerator scaled)
        else Left (SubMinorPrecision cur r)

minorToMajor :: Currency -> Integer -> Rational
minorToMajor cur n = n % (10 ^ minorDigits cur)

-- | An amount whose currency is only known at runtime.
data SomeMoney where
  SomeMoney :: KnownCurrency c => Money c -> SomeMoney

instance Eq SomeMoney where
  a == b = (someCurrency a, someMinor a) == (someCurrency b, someMinor b)

instance Ord SomeMoney where
  compare a b = compare (someCurrency a, someMinor a) (someCurrency b, someMinor b)

instance Show SomeMoney where
  show = T.unpack . renderSomeMoney

mkSomeMoney :: Currency -> Integer -> SomeMoney
mkSomeMoney cur n = case toSomeCurrency cur of
  SomeCurrency (_ :: SCurrency c) -> SomeMoney (Money n :: Money c)

someCurrency :: SomeMoney -> Currency
someCurrency (SomeMoney (_ :: Money c)) = fromSing (currencySing @c)

someMinor :: SomeMoney -> Integer
someMinor (SomeMoney m) = toMinor m

-- | Recover a typed amount if the runtime currency matches the singleton.
-- This is the only way back from 'SomeMoney' to 'Money c'.
matchMoney :: SCurrency c -> SomeMoney -> Maybe (Money c)
matchMoney s (SomeMoney (m :: Money d)) = case testEquality s (currencySing @d) of
  Just Refl -> Just m
  Nothing -> Nothing

-- | Parse @"1234.50"@ and a currency code into an exact amount.
parseSomeMoney :: Text -> Text -> Either MoneyError SomeMoney
parseSomeMoney amt cur = do
  c <- maybe (Left (UnknownCurrency cur)) Right (parseCurrency cur)
  r <- maybe (Left (BadDecimal amt)) Right (parseDecimal amt)
  mkSomeMoney c <$> toMinorExact c r

-- | Exact decimal parser: optional sign, digits, optional fraction.
-- No exponents, so input size bounds the work done.
parseDecimal :: Text -> Maybe Rational
parseDecimal raw =
  let t = T.filter (/= '_') (T.strip raw)
      (sign, body) = case T.uncons t of
        Just ('-', rest) -> (-1, rest)
        Just ('+', rest) -> (1, rest)
        _ -> (1, t)
      (whole, frac0) = T.breakOn "." body
      frac = T.drop 1 frac0
      ok =
        not (T.null whole && T.null frac)
          && T.all isDigit whole
          && T.all isDigit frac
          && (T.null frac0 || not (T.null frac))
          && T.length body <= 80
   in if not ok
        then Nothing
        else
          let w = if T.null whole then 0 else read (T.unpack whole) :: Integer
              f = if T.null frac then 0 else read (T.unpack frac) :: Integer
              scale = 10 ^ T.length frac :: Integer
           in Just (fromInteger sign * ((w * scale + f) % scale))

-- | Render minor units as a fixed-point decimal, e.g. @123450@ in USD is
-- @"1234.50"@.
renderMinor :: Currency -> Integer -> Text
renderMinor cur n =
  let d = minorDigits cur
      s = T.pack (show (abs n))
      padded = T.justifyRight (d + 1) '0' s
      (w, f) = T.splitAt (T.length padded - d) padded
      body = if d == 0 then w else w <> "." <> f
   in if n < 0 then "-" <> body else body

renderMoney :: forall c. KnownCurrency c => Money c -> Text
renderMoney (Money n) =
  let cur = fromSing (currencySing @c)
   in renderMinor cur n <> " " <> currencyCode cur

renderSomeMoney :: SomeMoney -> Text
renderSomeMoney (SomeMoney m) = renderMoney m

-- | Render a 'Rational' as a decimal. Exact when the expansion terminates
-- within @maxDigits@ fractional digits; otherwise truncated and marked
-- with a leading @~@.
renderRational :: Int -> Rational -> Text
renderRational maxDigits r =
  let neg = r < 0
      a = abs r
      w = numerator a `div` denominator a
      rest0 = a - fromInteger w
      go :: Int -> Rational -> [Char] -> ([Char], Bool)
      go k x acc
        | x == 0 = (reverse acc, True)
        | k >= maxDigits = (reverse acc, False)
        | otherwise =
            let x10 = x * 10
                dgt = numerator x10 `div` denominator x10
             in go (k + 1) (x10 - fromInteger dgt) (toEnum (fromEnum '0' + fromInteger dgt) : acc)
      (digits, exact) = go 0 rest0 []
      body = T.pack (show w) <> (if null digits then "" else "." <> T.pack digits)
   in (if exact then "" else "~") <> (if neg then "-" else "") <> body

-- | Rounding for conversions. 'RoundDown' rounds toward negative infinity,
-- so a positive conversion never creates value.
data RoundingMode = RoundDown | RoundUp | RoundHalfEven
  deriving (Eq, Show, Enum, Bounded)

roundWith :: RoundingMode -> Rational -> Integer
roundWith mode x = case mode of
  RoundDown -> floor x
  RoundUp -> ceiling x
  RoundHalfEven -> round x

-- | Units of @b@ per one major unit of @a@. Always strictly positive.
newtype Rate (a :: Currency) (b :: Currency) = Rate Rational
  deriving (Eq, Show)

mkRate :: Rational -> Maybe (Rate a b)
mkRate r
  | r > 0 = Just (Rate r)
  | otherwise = Nothing

rateValue :: Rate a b -> Rational
rateValue (Rate r) = r

-- | Convert, returning the converted amount and the remainder in minor
-- units of @b@ that rounding discarded (positive) or added (negative).
-- Law: @fromInteger (toMinor out) + remainder == exact@.
convert ::
  forall a b.
  (KnownCurrency a, KnownCurrency b) =>
  RoundingMode ->
  Rate a b ->
  Money a ->
  (Money b, Rational)
convert mode (Rate r) (Money n) =
  let (out, rm) = convertMinor mode (fromSing (currencySing @a)) (fromSing (currencySing @b)) r n
   in (Money out, rm)

-- | Untyped core of 'convert', used where currencies are runtime data.
convertMinor :: RoundingMode -> Currency -> Currency -> Rational -> Integer -> (Integer, Rational)
convertMinor mode from to r n =
  let exact = minorToMajor from n * r * (10 ^ minorDigits to)
      out = roundWith mode exact
   in (out, exact - fromInteger out)
