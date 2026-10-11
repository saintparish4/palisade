-- | Load policies from @.pal@ source: parse, then typecheck.
module Palisade.Policy
  ( loadPolicy
  , module Palisade.Policy.Expr
  , module Palisade.Policy.Eval
  , module Palisade.Policy.Syntax
  , TypeError (..)
  , renderTypeError
  , blockSummaries
  ) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Palisade.Currency
import Palisade.Policy.Check
import Palisade.Policy.Eval
import Palisade.Policy.Expr
import Palisade.Policy.Parse
import Palisade.Policy.Syntax

-- | Parse and typecheck. Errors carry @file:line:col@.
loadPolicy :: FilePath -> Text -> Either Text Policy
loadPolicy path src = do
  file <- parsePolicyFile path src
  either (Left . renderTypeError path) Right (checkPolicyFile file)

-- | (currency, policy name, rule count, law count) per block.
blockSummaries :: Policy -> [(Currency, Text, Int, Int)]
blockSummaries pol =
  [ (c, pbName blk, length (pbRules blk), length (pbLaws blk))
  | (c, SomeBlock _ blk) <- M.toList (policyBlocks pol)
  ]
