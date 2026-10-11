{- HLINT ignore "Monoid law, right identity" -}
{- HLINT ignore "Monoid law, left identity" -}

-- The identities are the point of the monoid-law test.
module Palisade.MoneySpec (spec) where

import Data.Ratio ((%))
import Gen
import Palisade.Currency
import Palisade.Money
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Palisade.Money" $ do
  it "renders minor units with the currency's exponent" $ do
    renderMinor USD 123450 `shouldBe` "1234.50"
    renderMinor USD 5 `shouldBe` "0.05"
    renderMinor USD (-5) `shouldBe` "-0.05"
    renderMinor JPY 1200 `shouldBe` "1200"
    renderMinor USDC 1 `shouldBe` "0.000001"
    renderMinor BTC 100000000 `shouldBe` "1.00000000"

  it "parses decimals exactly and rejects junk" $ do
    parseDecimal "1234.50" `shouldBe` Just (123450 % 100)
    parseDecimal "-0.05" `shouldBe` Just ((-5) % 100)
    parseDecimal "1e9" `shouldBe` Nothing
    parseDecimal "1." `shouldBe` Nothing
    parseDecimal "" `shouldBe` Nothing
    parseDecimal "0.1" `shouldBe` Just (1 % 10)

  it "rejects amounts finer than the currency's minor unit" $ do
    (money (1 % 1000) :: Either MoneyError (Money 'USD)) `shouldBe` Left (SubMinorPrecision USD (1 % 1000))
    fmap toMinor (money (1 % 1000) :: Either MoneyError (Money 'USDC)) `shouldBe` Right 1000

  it "round-trips every amount through decimal text" $
    property $ forAll genSomeMoney $ \m ->
      parseSomeMoney (renderMinor (someCurrency m) (someMinor m)) (currencyCode (someCurrency m)) === Right m

  it "is a monoid" $
    property $ \(a :: Integer) b c ->
      let x = fromMinor a :: Money 'EUR
          y = fromMinor b
          z = fromMinor c
       in ((x <> y) <> z) === (x <> (y <> z)) .&&. (x <> mempty) === x .&&. (mempty <> x) === x

  it "conversion accounts for every minor unit: output + remainder == exact" $
    property $ \(Positive n) (Positive num) (Positive den) -> forAll (elements [minBound .. maxBound]) $ \mode ->
      let r = num % den
          (out, rm) = convertMinor mode USD EUR r n
          exact = (n % 100) * r * 100
       in (fromInteger out + rm) === exact

  it "RoundDown never creates value" $
    property $ \(Positive n) (Positive num) (Positive den) ->
      let (out, rm) = convertMinor RoundDown USD JPY (num % den) n
       in rm >= 0 .&&. rm < 1 .&&. out >= 0

  it "typed convert agrees with the untyped core" $
    property $ \(Positive n) (Positive num) (Positive den) ->
      case mkRate (num % den) :: Maybe (Rate 'GBP 'BTC) of
        Nothing -> property False
        Just rate ->
          let (out, rm) = convert RoundHalfEven rate (fromMinor n :: Money 'GBP)
           in (toMinor out, rm) === convertMinor RoundHalfEven GBP BTC (num % den) n
