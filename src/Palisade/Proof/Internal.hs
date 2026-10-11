-- | The constructors behind "Palisade.Proof". This module is listed under
-- @other-modules@ in the cabal file, so code outside the library cannot
-- import it and cannot forge evidence.
module Palisade.Proof.Internal
  ( Proof (..)
  , Named (..)
  ) where

-- | Evidence that the fact @p@ holds. A value exists only if a checker in
-- this library decided @p@.
data Proof p = QED

-- | A value tagged with a type-level name. Facts mention names, so a proof
-- about one intent or state cannot be used for another.
newtype Named name a = Named a
