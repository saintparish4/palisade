-- | Unforgeable evidence ("Ghosts of Departed Proofs", Noonan 2018).
--
-- Balances and limits are runtime data, so GHC cannot prove
-- @balance >= amount@ at compile time. What the types do guarantee:
--
-- * A @Proof p@ cannot be constructed outside this library; the only way to
--   get one is to call the checker that decides @p@.
-- * Proofs name the exact values they cover. A proof about intent @i@ in
--   state @st@ cannot authorize a different intent or a different state,
--   because 'name' introduces a fresh type-level name each time.
-- * "Palisade.Authorize" builds an executable transaction only from a full
--   set of proofs, and its constructor is not exported.
module Palisade.Proof
  ( Proof
  , Named
  , name
  , the
  ) where

import Palisade.Proof.Internal

-- | Give a value a fresh type-level name for the duration of a scope.
name :: a -> (forall n. Named n a -> r) -> r
name x k = k (Named x)

the :: Named n a -> a
the (Named x) = x
