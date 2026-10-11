-- | Human-readable output for the CLI.
module Palisade.Render
  ( renderCertificate
  , renderPolicySummary
  , renderLedger
  , renderSimReport
  , renderComparison
  , renderBaselineDiff
  , renderStress
  , renderBench
  , renderOffset
  , shortHash
  ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Palisade.Bench
import Palisade.Certificate
import Palisade.Currency
import Palisade.Ledger
import Palisade.Money
import Palisade.Policy
import Palisade.Simulate
import Palisade.Stress
import Palisade.Types

tshow :: Show a => a -> Text
tshow = T.pack . show

plural :: Int -> Text -> Text
plural n w = tshow n <> " " <> w <> (if n == 1 then "" else "s")

shortHash :: Text -> Text
shortHash = T.take 12

pad :: Int -> Text -> Text
pad n t = t <> T.replicate (n - T.length t) " "

padL :: Int -> Text -> Text
padL n t = T.replicate (n - T.length t) " " <> t

mark :: Outcome -> Text
mark = \case
  Pass -> "ok  "
  Fail -> "FAIL"
  Triggered -> "ESC "
  Errored -> "ERR "

verdictBanner :: Verdict -> Text
verdictBanner = \case
  Allow -> "ALLOW"
  Deny -> "DENY"
  Escalate -> "ESCALATE"

describeIntent :: Intent -> Text
describeIntent i =
  intentKindText (intentKind i) <> " " <> intentId i <> "  " <> intentFrom i <> " -> " <> intentTo i
    <> "  " <> renderSomeMoney (intentAmount i)
    <> case intentKind i of
      Convert tc r -> " into " <> currencyCode tc <> " @ " <> renderRational 9 r
      _ -> ""

renderCertificate :: Certificate -> Text
renderCertificate c =
  T.unlines $
    [ verdictBanner (certVerdict c) <> "  " <> describeIntent (certIntent c)
    , "  actor     " <> intentActor (certIntent c) <> "   at " <> renderUtc (intentTs (certIntent c))
    , "  policy    " <> fromMaybe "(none for this currency)" (certPolicyName c) <> "  sha256:" <> shortHash (certPolicyHash c)
    , "  state     seq " <> tshow (certPreSeq c) <> " head " <> shortHash (certPreHead c)
        <> maybe "" (\s -> "  ->  seq " <> tshow s <> " head " <> maybe "" shortHash (certPostHead c)) (certPostSeq c)
    ]
      <> maybe [] (\who -> ["  approved  by " <> who]) (certApprovedBy c)
      <> [ "  " <> mark (crOutcome r) <> "  " <> pad 34 (crRule r) <> "  " <> crObserved r <> "   [" <> crRequired r <> "]"
         | r <- certChecks c
         ]
      <> ["  cert      sha256:" <> certHash c]

renderPolicySummary :: FilePath -> Policy -> Text
renderPolicySummary path pol =
  T.unlines $
    [ "policy file  " <> T.pack path
    , "hash         sha256:" <> policyHash pol <> "  (of the canonical form below)"
    ]
      <> concat
        [ [ ""
          , currencyCode c <> "  " <> quoteText (pbName blk) <> "  " <> plural (length (pbRules blk)) "rule" <> ", "
              <> plural (length (pbLaws blk)) "law"
          ]
            <> [ "  " <> pad 9 (ruleKindText (ruleKind r)) <> pad w (ruleName r) <> prettyTyped (ruleExpr r)
               | r <- pbRules blk
               ]
            <> [ "  " <> pad 9 "law" <> pad w (lawName l) <> prettyTyped (lawExpr l)
               | l <- pbLaws blk
               ]
        | (c, SomeBlock _ blk) <- M.toList (policyBlocks pol)
        , let w = 2 + maximum (0 : map (T.length . ruleName) (pbRules blk) <> map (T.length . lawName) (pbLaws blk))
        ]
      <> ["", "canonical form:", "", policySource pol]

renderLedger :: LedgerState -> Text
renderLedger st =
  T.unlines $
    [ "seq " <> tshow (lsSeq st) <> "   head " <> lsHead st
    , "time " <> maybe "(empty)" renderUtc (lsTime st)
    , ""
    , pad 24 "account" <> pad 8 "ccy" <> padL 22 "settled" <> padL 22 "pending"
    ]
      <> [ pad 24 a <> pad 8 (currencyCode c) <> padL 22 (renderMinor c n) <> padL 22 (renderMinor c (pendingInbound a c st))
         | (a, c, n) <- internalBalances st
         ]
      <> [ ""
         , "executed intents " <> tshow (S.size (lsExecuted st)) <> "   pending transfers " <> tshow (M.size (lsPending st))
         , "trial balance    "
             <> T.intercalate
               ", "
               [currencyCode c <> " " <> renderMinor c n | (c, n) <- M.toList (trialBalance st)]
             <> (if all (== 0) (M.elems (trialBalance st)) then "  (balanced)" else "  (NOT BALANCED)")
         ]

sumByCurrency :: [Intent] -> Text
sumByCurrency is =
  let m = M.fromListWith (+) [(intentCurrency i, someMinor (intentAmount i)) | i <- is]
   in if M.null m then "none" else T.intercalate ", " [renderMinor c n <> " " <> currencyCode c | (c, n) <- M.toList m]

firstBlocking :: Certificate -> Text
firstBlocking c = case [r | r <- certChecks c, isBlocking r] of
  (r : _) -> crRule r <> ": " <> crObserved r <> " [" <> crRequired r <> "]"
  [] -> ""

renderSimReport :: Mode -> Text -> SimReport -> Text
renderSimReport mode label rep =
  let outs = srOutcomes rep
      byV v = [o | o <- outs, oVerdict o == v]
      blocked = [o | o <- outs, oVerdict o /= Allow]
      ruleHits =
        sortOn (Down . snd) . M.toList $
          M.fromListWith (+) [(crRule r, 1 :: Int) | o <- outs, r <- certChecks (oCertificate o), isBlocking r]
      modeName = case mode of
        ShadowMode -> "shadow (reality applied in full; verdicts are what PALISADE would have done)"
        CounterfactualMode EscalationsBlocked -> "backtest (only allowed intents move money; escalations stay blocked)"
        CounterfactualMode EscalationsApproved -> "backtest (only allowed intents move money; escalations assumed approved)"
        CounterfactualMode EscalationsByLabel -> "benchmark"
      wouldOrWas = case mode of
        ShadowMode -> "would have "
        _ -> ""
   in T.unlines $
        [ "mode        " <> modeName
        , "history     " <> label
        , "intents     " <> tshow (length outs) <> "   allow " <> tshow (length (byV Allow)) <> "   deny "
            <> tshow (length (byV Deny))
            <> "   escalate "
            <> tshow (length (byV Escalate))
        , "value " <> wouldOrWas <> "blocked   " <> sumByCurrency (map oIntent (byV Deny))
        , "value " <> wouldOrWas <> "escalated " <> sumByCurrency (map oIntent (byV Escalate))
        ]
          <> ( if null ruleHits
                 then []
                 else "" : "rules that stopped intents" : ["  " <> padL 5 (tshow n) <> "  " <> r | (r, n) <- take 10 ruleHits]
             )
          <> ( if null blocked
                 then []
                 else
                   ""
                     : "stopped intents"
                     : [ "  line " <> padL 4 (tshow (oLine o)) <> "  " <> pad 9 (verdictBanner (oVerdict o)) <> describeIntent (oIntent o)
                           <> "\n              " <> firstBlocking (oCertificate o)
                       | o <- take 40 blocked
                       ]
                     <> ["  ... and " <> tshow (length blocked - 40) <> " more (see --certs)" | length blocked > 40]
             )
          <> ( if null (srErrors rep)
                 then []
                 else "" : "history lines the ledger refused" : ["  line " <> tshow ln <> ": " <> e | (ln, e) <- take 20 (srErrors rep)]
             )

-- | Final balances under reality (shadow) and under the policy (backtest).
renderComparison :: SimReport -> SimReport -> Text
renderComparison reality policy =
  let bal st = M.fromList [((a, c), n) | (a, c, n) <- internalBalances st]
      r = bal (srFinal reality)
      p = bal (srFinal policy)
      keys = S.toList (S.fromList (M.keys r <> M.keys p))
   in T.unlines $
        [ ""
        , "final balances      " <> padL 22 "reality" <> padL 22 "under policy" <> padL 22 "difference"
        ]
          <> [ pad 20 (a <> " " <> currencyCode c) <> padL 22 (renderMinor c x) <> padL 22 (renderMinor c y) <> padL 22 (renderMinor c (y - x))
             | k@(a, c) <- keys
             , let x = M.findWithDefault 0 k r
                   y = M.findWithDefault 0 k p
             ]

renderBaselineDiff :: SimReport -> SimReport -> Text
renderBaselineDiff base new =
  let pairs = zip (srOutcomes base) (srOutcomes new)
      changed = [(b, n) | (b, n) <- pairs, oVerdict b /= oVerdict n]
   in T.unlines $
        [ ""
        , "against the baseline policy: " <> tshow (length changed) <> " of " <> tshow (length pairs) <> " verdicts change"
        ]
          <> [ "  line " <> padL 4 (tshow (oLine n)) <> "  " <> pad 9 (verdictBanner (oVerdict b)) <> "-> " <> pad 9 (verdictBanner (oVerdict n))
                 <> describeIntent (oIntent n)
             | (b, n) <- take 40 changed
             ]

renderOffset :: Integer -> Text
renderOffset s =
  let (d, r1) = s `divMod` 86400
      (h, r2) = r1 `divMod` 3600
      (m, sec) = r2 `divMod` 60
      two x = T.justifyRight 2 '0' (tshow x)
   in "T+" <> (if d > 0 then tshow d <> "d " else "") <> two h <> ":" <> two m <> ":" <> two sec

renderStress :: Text -> StressReport -> Text
renderStress reproduce rep =
  T.unlines $
    [ "sequences " <> tshow (srCases rep) <> "   steps " <> tshow (srSteps rep) <> "   seed " <> tshow (srSeed rep)
    , "verdicts  " <> T.intercalate "   " [verdictText v <> " " <> tshow n | (v, n) <- M.toList (srVerdicts rep)]
    , ""
    ]
      <> concatMap (lawLines w) (srLaws rep)
      <> [ ""
         , let bad = length [() | l <- srLaws rep, isJust (lsCounterexample l)]
            in if bad == 0
                 then "all " <> tshow (length (srLaws rep)) <> " laws held"
                 else tshow bad <> " of " <> tshow (length (srLaws rep)) <> " laws violated"
         ]
  where
    w = 2 + maximum (0 : map (T.length . lsLawName) (srLaws rep))
    lawLines w' l = case lsCounterexample l of
      Nothing -> ["  ok    " <> pad w' (lsLawName l) <> lsDescription l]
      Just ce ->
        let v = ceViolation ce
         in [ "  FAIL  " <> pad w' (lsLawName l) <> lsDescription l
            , "        case " <> tshow (ceCase ce) <> " (" <> tshow (ceOriginalLength ce) <> " steps) shrunk to "
                <> tshow (length (ceSteps ce))
                <> ":"
            ]
              <> [ "          " <> pad 13 (renderOffset (tlOffset t)) <> tlAction t <> "\n" <> T.replicate 23 " " <> "=> " <> tlResult t
                 | t <- ceTrace ce
                 ]
              <> [ "        violated at step " <> tshow (vStep v) <> ": " <> vDetail v
                 , "        reproduce: " <> reproduce <> " --case " <> tshow (ceCase ce)
                 , ""
                 ]

renderBench :: Text -> [IncidentScore] -> Text
renderBench policyLine scores =
  let t = benchTotals scores
      pct a b = if b == 0 then "n/a" else tshow ((100 * a) `div` b) <> "%"
      lost m = if M.null m then "-" else T.intercalate ", " [renderMinor c n <> " " <> currencyCode c | (c, n) <- M.toList m]
      row s =
        pad 34 (incName (scIncident s))
          <> padL 8 (tshow (scAttacks s))
          <> padL 8 (tshow (scDenied s))
          <> padL 8 (tshow (scEscalated s))
          <> padL 8 (tshow (scMissed s))
          <> padL 8 (tshow (scBenign s))
          <> padL 8 (tshow (scBenignDenied s))
          <> padL 8 (tshow (scBenignEscalated s))
          <> "   "
          <> lost (scLost s)
      misses =
        [ "  " <> incName (scIncident s) <> ": " <> describeIntent (oIntent o) <> " was allowed"
        | s <- scores
        , o <- scOutcomes s
        , oLabel o == Just Attack
        , oVerdict o == Allow
        ]
      falseDenials =
        [ "  " <> incName (scIncident s) <> ": " <> describeIntent (oIntent o) <> "\n      " <> firstBlocking (oCertificate o)
        | s <- scores
        , o <- scOutcomes s
        , oLabel o == Just Benign
        , oVerdict o == Deny
        ]
   in T.unlines $
        [ "PolicyBench   " <> policyLine
        , ""
        , pad 34 "incident" <> padL 8 "attacks" <> padL 8 "denied" <> padL 8 "escal." <> padL 8 "missed" <> padL 8 "benign"
            <> padL 8 "f.deny"
            <> padL 8 "review"
            <> "   lost"
        ]
          <> map row scores
          <> [ ""
             , "attacks caught       " <> tshow (btCaught t) <> " / " <> tshow (btAttacks t) <> "  (" <> pct (btCaught t) (btAttacks t) <> ")"
             , "incidents contained  " <> tshow (btFullyCaught t) <> " / " <> tshow (btIncidents t)
             , "false denials        " <> tshow (btBenignDenied t) <> " / " <> tshow (btBenign t) <> " benign intents"
             , "human reviews        " <> tshow (btBenignEscalated t) <> " / " <> tshow (btBenign t) <> " benign intents"
             , "value lost           " <> lost (btLost t)
             ]
          <> (if null misses then [] else "" : "missed attacks" : misses)
          <> (if null falseDenials then [] else "" : "false denials" : falseDenials)
          <> concat
            [ "" : ("errors in " <> incName (scIncident s)) : ["  line " <> tshow ln <> ": " <> e | (ln, e) <- scErrors s]
            | s <- scores
            , not (null (scErrors s))
            ]
