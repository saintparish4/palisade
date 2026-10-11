module Main (main) where

import qualified Palisade.AuthorizeSpec
import qualified Palisade.BenchSpec
import qualified Palisade.ChainSpec
import qualified Palisade.LedgerSpec
import qualified Palisade.MoneySpec
import qualified Palisade.PolicySpec
import qualified Palisade.SimulateSpec
import qualified Palisade.StressSpec
import qualified Palisade.TypeSafetySpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  Palisade.MoneySpec.spec
  Palisade.TypeSafetySpec.spec
  Palisade.LedgerSpec.spec
  Palisade.ChainSpec.spec
  Palisade.PolicySpec.spec
  Palisade.AuthorizeSpec.spec
  Palisade.SimulateSpec.spec
  Palisade.StressSpec.spec
  Palisade.BenchSpec.spec
