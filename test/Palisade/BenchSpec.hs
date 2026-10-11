module Palisade.BenchSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (sort)
import qualified Data.Text as T
import Gen
import Palisade.Bench
import Palisade.Types
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Test.Hspec

loadSuite :: IO [Incident]
loadSuite = do
  let dir = "scenarios/policybench"
  files <- sort <$> listDirectory dir
  mapM
    ( \f -> do
        ls <- filter (not . BLC.null) . BLC.lines <$> BLC.readFile (dir </> f)
        items <- either fail pure (traverse parseHistoryLine ls)
        pure (mkIncident (T.pack (takeBaseName f)) (zip [1 ..] items))
    )
    files

spec :: Spec
spec = describe "Palisade.Bench" $ do
  it "every incident has a title, a source, attacks and benign traffic" $ do
    suite <- loadSuite
    length suite `shouldBe` 10
    mapM_
      ( \inc -> do
          incTitle inc `shouldNotBe` ""
          incSource inc `shouldSatisfy` T.isPrefixOf "https://"
          length [() | (_, HIntent _ (Just Attack)) <- incItems inc] `shouldSatisfy` (> 0)
          length [() | (_, HIntent _ (Just Benign)) <- incItems inc] `shouldSatisfy` (> 0)
      )
      suite

  it "the hardened policy contains every incident without false denials" $ do
    pol <- loadScenarioPolicy "scenarios/policies/hardened.pal"
    t <- benchTotals . map (scoreIncident pol) <$> loadSuite
    (btCaught t, btAttacks t, btBenignDenied t) `shouldBe` (24, 24, 0)

  it "static caps miss most attacks" $ do
    pol <- loadScenarioPolicy "scenarios/policies/naive.pal"
    scores <- map (scoreIncident pol) <$> loadSuite
    let t = benchTotals scores
    btCaught t `shouldBe` 4
    [incName (scIncident s) | s <- scores, scMissed s == 0]
      `shouldBe` ["01-openclaw-quantity-confusion", "08-pending-settlement-race"]
    all (null . scErrors) scores `shouldBe` True
