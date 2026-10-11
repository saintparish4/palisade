-- | The double-entry ledger.
--
-- 'applyEvent' is a pure, total state transition: every event either
-- produces a new state or a typed 'ApplyError'. Every money movement is a
-- set of postings that sums to zero in each currency, so the trial balance
-- is zero after every event by construction (and property-tested).
module Palisade.Ledger
  ( LedgerState (..)
  , OutflowRec (..)
  , emptyLedger
  , ApplyError (..)
  , renderApplyError
  , applyEvent
  , applyObserved
  , Posting (..)
  , postingsFor
  , balanced
    -- * Queries
  , balanceMinor
  , settledBalance
  , pendingInbound
  , totalInternal
  , outflowsWithin
  , hasPaid
  , isInternal
  , trialBalance
  , internalBalances
  , ledgerSummary
  ) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Palisade.Canonical (genesisHash)
import Palisade.Currency
import Palisade.Money
import Palisade.Settlement
import Palisade.Types

-- | One executed outflow, kept for windowed policy queries.
data OutflowRec = OutflowRec
  { orTs :: !UTCTime
  , orSource :: !Text
  , orDest :: !Account
  , orCurrency :: !Currency
  , orMinor :: !Integer
  }
  deriving (Eq, Show)

data LedgerState = LedgerState
  { lsSeq :: !Int
  -- ^ Sequence number of the last applied log entry (0 = empty).
  , lsHead :: !Text
  -- ^ Hash-chain head; commits to the whole history.
  , lsTime :: !(Maybe UTCTime)
  , lsAccounts :: !(Set Text)
  , lsBalances :: !(Map (Account, Currency) Integer)
  , lsPending :: !(Map Text (Transfer 'Pending))
  , lsClosed :: !(Set Text)
  , lsExecuted :: !(Set Text)
  , lsOutflows :: ![OutflowRec]
  -- ^ Newest first.
  , lsPayees :: !(Map Text (Set Text))
  }
  deriving (Eq, Show)

emptyLedger :: LedgerState
emptyLedger = LedgerState 0 genesisHash Nothing S.empty M.empty M.empty S.empty S.empty [] M.empty

data ApplyError
  = TimeWentBackwards UTCTime UTCTime
  | AccountExists Text
  | UnknownAccount Text
  | DuplicateTransfer Text
  | UnknownTransfer Text
  | DuplicateIntent Text
  | NonPositiveAmount
  | InsufficientFunds Text Currency Integer Integer
  | PayToInternal Text
  | SameAccount Text
  | SameCurrency Currency
  | ConversionToZero
  | UnbalancedEntry
  deriving (Eq, Show)

renderApplyError :: ApplyError -> Text
renderApplyError = \case
  TimeWentBackwards prev now -> "event time " <> renderUtc now <> " is before the ledger time " <> renderUtc prev
  AccountExists a -> "account " <> a <> " is already open"
  UnknownAccount a -> "account " <> a <> " is not an open internal account"
  DuplicateTransfer t -> "transfer id " <> t <> " was already used"
  UnknownTransfer t -> "no pending transfer with id " <> t
  DuplicateIntent i -> "intent " <> i <> " was already executed"
  NonPositiveAmount -> "amount must be positive"
  InsufficientFunds a c have need ->
    "account " <> a <> " has " <> renderMinor c have <> " " <> currencyCode c
      <> " settled but the intent needs " <> renderMinor c need
  PayToInternal a -> a <> " is an internal account; use kind \"transfer\""
  SameAccount a -> "source and destination are both " <> a
  SameCurrency c -> "conversion from " <> currencyCode c <> " to itself"
  ConversionToZero -> "conversion rounds to zero in the target currency"
  UnbalancedEntry -> "internal error: entry does not balance"

data Posting = Posting
  { pAccount :: !Account
  , pCurrency :: !Currency
  , pMinor :: !Integer
  }
  deriving (Eq, Show)

-- | Sum per currency is zero.
balanced :: [Posting] -> Bool
balanced ps = all (== 0) (M.fromListWith (+) [(pCurrency p, pMinor p) | p <- ps])

-- | The postings an intent produces.
postingsFor :: Intent -> Either ApplyError [Posting]
postingsFor i =
  let c = intentCurrency i
      n = someMinor (intentAmount i)
      from = Internal (intentFrom i)
   in case intentKind i of
        Pay -> Right [Posting from c (negate n), Posting (External (intentTo i)) c n]
        TransferInternal -> Right [Posting from c (negate n), Posting (Internal (intentTo i)) c n]
        Convert tc r ->
          let (out, _rem) = convertMinor RoundDown c tc r n
           in if out <= 0
                then Left ConversionToZero
                else
                  Right
                    [ Posting from c (negate n)
                    , Posting (External "fx") c n
                    , Posting (External "fx") tc (negate out)
                    , Posting (Internal (intentTo i)) tc out
                    ]

post :: [Posting] -> LedgerState -> LedgerState
post ps st = st {lsBalances = foldl' add (lsBalances st) ps}
  where
    add m p = M.insertWith (+) (pAccount p, pCurrency p) (pMinor p) m

advance :: UTCTime -> LedgerState -> Either ApplyError LedgerState
advance t st = case lsTime st of
  Just prev | t < prev -> Left (TimeWentBackwards prev t)
  _ -> Right st {lsTime = Just t}

requireOpen :: Text -> LedgerState -> Either ApplyError ()
requireOpen a st
  | a `S.member` lsAccounts st = Right ()
  | otherwise = Left (UnknownAccount a)

applyEvent :: LedgerState -> Event -> Either ApplyError LedgerState
applyEvent st0 ev = do
  st <- advance (eventTime ev) st0
  case ev of
    EvOpen _ a
      | a `S.member` lsAccounts st -> Left (AccountExists a)
      | otherwise -> Right st {lsAccounts = S.insert a (lsAccounts st)}
    EvDeposit _ tid a f m settledNow -> do
      requireOpen a st
      if tid `M.member` lsPending st || tid `S.member` lsClosed st
        then Left (DuplicateTransfer tid)
        else
          if someMinor m <= 0
            then Left NonPositiveAmount
            else
              let t = pending tid a f m
               in Right $
                    if settledNow
                      then creditSettled (settle t) st {lsClosed = S.insert tid (lsClosed st)}
                      else st {lsPending = M.insert tid t (lsPending st)}
    EvSettle _ tid -> case M.lookup tid (lsPending st) of
      Nothing -> Left (UnknownTransfer tid)
      Just t ->
        Right $
          creditSettled
            (settle t)
            st {lsPending = M.delete tid (lsPending st), lsClosed = S.insert tid (lsClosed st)}
    EvReverse _ tid -> case M.lookup tid (lsPending st) of
      Nothing -> Left (UnknownTransfer tid)
      Just t -> case reverseTransfer t of
        Reversal _ -> Right st {lsPending = M.delete tid (lsPending st), lsClosed = S.insert tid (lsClosed st)}
    EvExecute _ i _ -> executeStrict i st

-- | Credit settled funds. The argument type is the guarantee: only a
-- @Transfer 'Settled@ can reach the balances.
creditSettled :: Transfer 'Settled -> LedgerState -> LedgerState
creditSettled t st =
  let (acct, m) = spendable t
      c = someCurrency m
      n = someMinor m
   in post [Posting (Internal acct) c n, Posting (External (transferFrom t)) c (negate n)] st

checkShape :: Intent -> LedgerState -> Either ApplyError ()
checkShape i st = do
  requireOpen (intentFrom i) st
  case intentKind i of
    Pay ->
      if isInternal (intentTo i) st then Left (PayToInternal (intentTo i)) else Right ()
    TransferInternal -> do
      requireOpen (intentTo i) st
      if intentTo i == intentFrom i then Left (SameAccount (intentTo i)) else Right ()
    Convert tc _ -> do
      requireOpen (intentTo i) st
      if tc == intentCurrency i then Left (SameCurrency tc) else Right ()
  if someMinor (intentAmount i) <= 0 then Left NonPositiveAmount else Right ()

executeStrict :: Intent -> LedgerState -> Either ApplyError LedgerState
executeStrict i st = do
  checkShape i st
  if intentId i `S.member` lsExecuted st then Left (DuplicateIntent (intentId i)) else Right ()
  let c = intentCurrency i
      need = someMinor (intentAmount i)
      have = settledBalance (intentFrom i) c st
  if have < need then Left (InsufficientFunds (intentFrom i) c have need) else Right ()
  record i st

-- | Apply an intent that is known to have happened outside the kernel
-- (shadow mode). Funding and idempotency are not enforced, because
-- reality already decided; unknown source accounts are opened.
applyObserved :: LedgerState -> Intent -> Either ApplyError LedgerState
applyObserved st0 i = do
  let st1 = case lsTime st0 of
        Just prev | intentTs i < prev -> st0
        _ -> st0 {lsTime = Just (intentTs i)}
      openIfMissing a s = if isInternal a s then s else s {lsAccounts = S.insert a (lsAccounts s)}
      st = case intentKind i of
        Pay -> openIfMissing (intentFrom i) st1
        _ -> openIfMissing (intentTo i) (openIfMissing (intentFrom i) st1)
  checkShape i st
  record i st

record :: Intent -> LedgerState -> Either ApplyError LedgerState
record i st = do
  ps <- postingsFor i
  if balanced ps then Right () else Left UnbalancedEntry
  let dest = case intentKind i of
        Pay -> External (intentTo i)
        TransferInternal -> Internal (intentTo i)
        Convert _ _ -> External "fx"
      rec_ = OutflowRec (intentTs i) (intentFrom i) dest (intentCurrency i) (someMinor (intentAmount i))
      payees = case intentKind i of
        Pay -> M.insertWith S.union (intentFrom i) (S.singleton (intentTo i)) (lsPayees st)
        _ -> lsPayees st
  Right
    (post ps st)
      { lsExecuted = S.insert (intentId i) (lsExecuted st)
      , lsOutflows = rec_ : lsOutflows st
      , lsPayees = payees
      , lsTime = Just (maybe (intentTs i) (max (intentTs i)) (lsTime st))
      }

------------------------------------------------------------------------------
-- Queries

balanceMinor :: Account -> Currency -> LedgerState -> Integer
balanceMinor a c st = M.findWithDefault 0 (a, c) (lsBalances st)

-- | Spendable balance of an internal account. Pending deposits are not
-- included; see "Palisade.Settlement".
settledBalance :: Text -> Currency -> LedgerState -> Integer
settledBalance a = balanceMinor (Internal a)

pendingInbound :: Text -> Currency -> LedgerState -> Integer
pendingInbound a c st =
  sum
    [ someMinor (transferAmount t)
    | t <- M.elems (lsPending st)
    , transferAccount t == a
    , someCurrency (transferAmount t) == c
    ]

totalInternal :: Currency -> LedgerState -> Integer
totalInternal c st = sum [n | ((Internal _, c'), n) <- M.toList (lsBalances st), c' == c]

-- | Outflows with timestamps in the half-open window @(now - d, now]@.
outflowsWithin :: UTCTime -> NominalDiffTime -> LedgerState -> [OutflowRec]
outflowsWithin now d st =
  let cutoff = addUTCTime (negate d) now
   in filter (\r -> orTs r <= now) (takeWhile (\r -> orTs r > cutoff) (lsOutflows st))

hasPaid :: Text -> Text -> LedgerState -> Bool
hasPaid src cp st = maybe False (S.member cp) (M.lookup src (lsPayees st))

isInternal :: Text -> LedgerState -> Bool
isInternal a st = a `S.member` lsAccounts st

trialBalance :: LedgerState -> Map Currency Integer
trialBalance st = M.fromListWith (+) [(c, n) | ((_, c), n) <- M.toList (lsBalances st)]

internalBalances :: LedgerState -> [(Text, Currency, Integer)]
internalBalances st = [(a, c, n) | ((Internal a, c), n) <- M.toList (lsBalances st)]

ledgerSummary :: LedgerState -> Value
ledgerSummary st =
  object
    [ "seq" .= lsSeq st
    , "head" .= lsHead st
    , "time" .= fmap renderUtc (lsTime st)
    , "accounts"
        .= [ object
               [ "account" .= a
               , "currency" .= c
               , "settled" .= renderMinor c n
               , "pending" .= renderMinor c (pendingInbound a c st)
               ]
           | (a, c, n) <- internalBalances st
           ]
    , "pending_transfers" .= M.size (lsPending st)
    , "executed_intents" .= S.size (lsExecuted st)
    , "trial_balance" .= M.fromList [(T.unpack (currencyCode c), renderMinor c n) | (c, n) <- M.toList (trialBalance st)]
    ]

instance ToJSON LedgerState where
  toJSON = ledgerSummary
