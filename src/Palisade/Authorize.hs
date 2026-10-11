-- | The gate. Money moves only through 'authorize'.
--
-- Each kernel check and the policy check returns a 'Proof' of the fact it
-- decided, about the specific named state and intent. 'mkExecutable'
-- demands all of them, and 'ExecutableTransaction' has no exported
-- constructor, so there is no path to a committed transaction that skips
-- a check. The transaction is also indexed by the state's name, so it
-- cannot be committed against a different (for example, newer) state:
-- a proof that went stale is a type error.
module Palisade.Authorize
  ( -- * The decision
    Decision (..)
  , authorize
  , authorizeWith
  , Approval
  , approvedBy
    -- * Facts and checkers
  , WellFormed
  , Known
  , Fresh
  , Funded
  , PolicyHolds
  , checkWellFormed
  , checkKnown
  , checkFresh
  , checkFunded
  , checkPolicy
  , ExecutableTransaction
  , mkExecutable
  , commit
  ) where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Palisade.Certificate
import Palisade.Chain
import Palisade.Currency
import Palisade.Ledger
import Palisade.Money
import Palisade.Policy
import Palisade.Proof
import Palisade.Proof.Internal (Proof (..), Named (..))
import Palisade.Types

-- Facts. They have no values; they only appear inside 'Proof'.
data WellFormed i
data Known st i
data Fresh st i
data Funded st i
data PolicyHolds p st i

-- | A human sign-off that turns an escalation into an approval. It never
-- overrides a denial.
newtype Approval = Approval Text

approvedBy :: Text -> Approval
approvedBy = Approval

data ExecutableTransaction st = ExecutableTransaction Intent Text

type Check p = (CheckResult, Maybe (Proof p))

kernel :: Text -> Bool -> Text -> Text -> Check p
kernel rule ok observed required =
  (CheckResult ("kernel: " <> rule) KernelCheck (if ok then Pass else Fail) observed required, if ok then Just QED else Nothing)

amountText :: Intent -> Text
amountText = renderSomeMoney . intentAmount

checkWellFormed :: Named i Intent -> Check (WellFormed i)
checkWellFormed (Named i) =
  let n = someMinor (intentAmount i)
      problems =
        [ "empty id" | T.null (T.strip (intentId i)) ]
          <> [ "amount is not positive" | n <= 0 ]
          <> case intentKind i of
            Convert tc r ->
              [ "rate is not positive" | r <= 0 ]
                <> [ "converts " <> currencyCode tc <> " to itself" | tc == intentCurrency i ]
                <> [ "converts to zero " <> currencyCode tc | r > 0 && fst (convertMinor RoundDown (intentCurrency i) tc r n) <= 0 ]
            _ -> [ "source and destination are the same" | intentFrom i == intentTo i ]
   in kernel
        "well-formed"
        (null problems)
        (if null problems then intentKindText (intentKind i) <> " of " <> amountText i else T.intercalate "; " problems)
        "positive amount, distinct endpoints, valid rate"

checkKnown :: Named st LedgerState -> Named i Intent -> Check (Known st i)
checkKnown (Named st) (Named i) =
  let from = intentFrom i
      to = intentTo i
      (ok, req) = case intentKind i of
        Pay -> (isInternal from st && not (isInternal to st), "source is internal; payee is external")
        _ -> (isInternal from st && isInternal to st, "source and destination are internal accounts")
      describe a = a <> (if isInternal a st then " (internal)" else " (external)")
   in kernel "known accounts" ok (describe from <> " -> " <> describe to) req

checkFresh :: Named st LedgerState -> Named i Intent -> Check (Fresh st i)
checkFresh (Named st) (Named i) =
  let dup = intentId i `S.member` lsExecuted st
      backwards = maybe False (intentTs i <) (lsTime st)
      observed
        | dup = "intent " <> intentId i <> " already executed"
        | backwards = "intent time " <> renderUtc (intentTs i) <> " is before ledger time " <> maybe "" renderUtc (lsTime st)
        | otherwise = "new intent id at " <> renderUtc (intentTs i)
   in kernel "fresh" (not dup && not backwards) observed "id never executed; time not before the ledger"

checkFunded :: Named st LedgerState -> Named i Intent -> Check (Funded st i)
checkFunded (Named st) (Named i) =
  let c = intentCurrency i
      have = settledBalance (intentFrom i) c st
      pend = pendingInbound (intentFrom i) c st
      need = someMinor (intentAmount i)
      observed =
        "settled " <> renderMinor c have <> " " <> currencyCode c
          <> (if pend > 0 then " (pending " <> renderMinor c pend <> " " <> currencyCode c <> " is not spendable)" else "")
   in kernel "funded" (have >= need) observed (">= " <> amountText i)

-- | Evaluate the policy. Returns the policy block's name, every rule's
-- result, and a proof only if no @require@ rule failed and no @escalate@
-- rule fired (or a human approved the escalation).
checkPolicy ::
  Named p Policy ->
  Named st LedgerState ->
  Named i Intent ->
  Maybe Approval ->
  (Maybe Text, [CheckResult], Maybe (Proof (PolicyHolds p st i)))
checkPolicy (Named pol) (Named st) (Named i) approval =
  case evaluateIntent pol st i of
    Nothing ->
      let c = intentCurrency i
       in ( Nothing
          , [CheckResult "kernel: policy covers currency" KernelCheck Fail ("no policy block for " <> currencyCode c) ("a policy block with currency " <> currencyCode c)]
          , Nothing
          )
    Just (blockName, results) ->
      let denied = any (\r -> crKind r == RequireCheck && crOutcome r /= Pass) results
          escalated = any (\r -> crKind r == EscalateCheck && crOutcome r /= Pass) results
          ok = not denied && (not escalated || approved)
          approved = case approval of
            Just _ -> True
            Nothing -> False
       in (Just blockName, results, if ok then Just QED else Nothing)

mkExecutable ::
  Named p Policy ->
  Named st LedgerState ->
  Named i Intent ->
  Proof (WellFormed i) ->
  Proof (Known st i) ->
  Proof (Fresh st i) ->
  Proof (Funded st i) ->
  Proof (PolicyHolds p st i) ->
  ExecutableTransaction st
mkExecutable (Named pol) _ (Named i) QED QED QED QED QED = ExecutableTransaction i (policyHash pol)

-- | Commit against the very state the proofs were about.
commit :: Named st LedgerState -> ExecutableTransaction st -> Either ApplyError (LedgerState, LogEntry)
commit (Named st) (ExecutableTransaction i ph) = appendEvent st (EvExecute (intentTs i) i ph)

data Decision = Decision
  { decVerdict :: Verdict
  , decCertificate :: Certificate
  , decState :: LedgerState
  -- ^ The new state if allowed; otherwise the unchanged state.
  , decEntry :: Maybe LogEntry
  -- ^ The log entry appended, if allowed.
  }

authorize :: Policy -> LedgerState -> Intent -> Decision
authorize pol = authorizeWith pol Nothing

authorizeWith :: Policy -> Maybe Approval -> LedgerState -> Intent -> Decision
authorizeWith pol0 approval st0 i0 =
  name pol0 $ \pol -> name st0 $ \st -> name i0 $ \i ->
    let (cWf, pWf) = checkWellFormed i
        (cKn, pKn) = checkKnown st i
        (cFr, pFr) = checkFresh st i
        (cFu, pFu) = checkFunded st i
        (blockName, policyResults, pPol) = checkPolicy pol st i approval
        checks = [cWf, cKn, cFr, cFu] <> policyResults
        kernelFailed = any (\c -> crKind c == KernelCheck && crOutcome c /= Pass) checks
        requireFailed = any (\c -> crKind c == RequireCheck && crOutcome c /= Pass) checks
        escalated = any (\c -> crKind c == EscalateCheck && crOutcome c /= Pass) checks
        approver = case approval of
          Just (Approval who) | escalated && not kernelFailed && not requireFailed -> Just who
          _ -> Nothing
        cert v post =
          Certificate
            { certIntent = i0
            , certIntentHash = intentHash i0
            , certPolicyHash = policyHash pol0
            , certPolicyName = blockName
            , certPreSeq = lsSeq st0
            , certPreHead = lsHead st0
            , certChecks = checks
            , certVerdict = v
            , certApprovedBy = if v == Allow then approver else Nothing
            , certPostSeq = lsSeq <$> post
            , certPostHead = lsHead <$> post
            }
        refused v = Decision v (cert v Nothing) st0 Nothing
     in case (pWf, pKn, pFr, pFu, pPol) of
          (Just a, Just b, Just c, Just d, Just e) ->
            case commit st (mkExecutable pol st i a b c d e) of
              Right (st', entry) -> Decision Allow (cert Allow (Just st')) st' (Just entry)
              Left err ->
                -- Unreachable while the checkers agree with the ledger; kept
                -- fail-closed so a disagreement denies instead of crashing.
                let failure = CheckResult "kernel: commit" KernelCheck Fail (renderApplyError err) "ledger accepts the entry"
                 in Decision Deny ((cert Deny Nothing) {certChecks = checks <> [failure]}) st0 Nothing
          _
            | kernelFailed || requireFailed -> refused Deny
            | escalated -> refused Escalate
            | otherwise -> refused Deny
