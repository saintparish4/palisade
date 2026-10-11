-- | Settlement states at the type level.
--
-- An inbound transfer starts 'Pending'. Only a 'Settled' transfer can be
-- turned into spendable funds, and there is no function from
-- @Transfer 'Pending@ to spendable money. Spending against funds that have
-- not settled (the "withdrawal races a pending deposit" failure) is
-- therefore a type error inside the kernel, not a runtime check someone
-- can forget.
module Palisade.Settlement
  ( SettlementState (..)
  , Transfer
  , transferId
  , transferAccount
  , transferFrom
  , transferAmount
  , pending
  , settle
  , reverseTransfer
  , spendable
  , Reversal (..)
  ) where

import Data.Text (Text)
import Palisade.Money (SomeMoney)

data SettlementState = Pending | Settled | Reversed

data Transfer (s :: SettlementState) = Transfer
  { transferId :: Text
  , transferAccount :: Text
  , transferFrom :: Text
  , transferAmount :: SomeMoney
  }
  deriving (Eq, Show)

-- | A newly announced inbound transfer.
pending :: Text -> Text -> Text -> SomeMoney -> Transfer 'Pending
pending = Transfer

settle :: Transfer 'Pending -> Transfer 'Settled
settle (Transfer i a f m) = Transfer i a f m

-- | A reversed transfer carries no funds; the evidence of what was
-- reversed is kept for audit.
newtype Reversal = Reversal (Transfer 'Reversed)

reverseTransfer :: Transfer 'Pending -> Reversal
reverseTransfer (Transfer i a f m) = Reversal (Transfer i a f m)

-- | The only way to obtain spendable funds from a transfer.
spendable :: Transfer 'Settled -> (Text, SomeMoney)
spendable t = (transferAccount t, transferAmount t)
