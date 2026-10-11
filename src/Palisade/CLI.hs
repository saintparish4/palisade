-- | The @palisade@ command line. This is the only module that does IO;
-- everything it calls is pure.
module Palisade.CLI
  ( main
  , run
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (toJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (isSpace)
import Data.List (isSuffixOf, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Options.Applicative
import Palisade.Authorize
import Palisade.Bench
import Palisade.Canonical
import Palisade.Certificate
import Palisade.Chain
import Palisade.Ledger
import Palisade.Policy
import Palisade.Render
import Palisade.Sign
import Palisade.Simulate
import Palisade.Stress
import Palisade.Types
import System.Directory (doesFileExist, listDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (dropExtension, takeBaseName, (</>))
import System.IO (stderr)

data Command
  = Check FilePath
  | Append FilePath FilePath
  | AuthorizeCmd AuthOpts
  | Verify FilePath (Maybe FilePath)
  | Replay FilePath (Maybe String) (Maybe Int) Bool
  | Audit FilePath FilePath [FilePath]
  | Shadow FilePath FilePath (Maybe FilePath) (Maybe FilePath)
  | Backtest FilePath FilePath (Maybe FilePath) Escalations
  | StressCmd StressOpts
  | BenchCmd FilePath FilePath
  | Keygen FilePath
  | Version

data AuthOpts = AuthOpts
  { aoLog :: FilePath
  , aoPolicy :: FilePath
  , aoIntent :: FilePath
  , aoCommit :: Bool
  , aoEvidence :: Maybe FilePath
  , aoKey :: Maybe FilePath
  , aoApprover :: Maybe Text
  , aoJson :: Bool
  }

data StressOpts = StressOpts
  { soPolicy :: FilePath
  , soSetup :: Maybe FilePath
  , soCount :: Int
  , soSeed :: Int
  , soSteps :: Int
  , soCase :: Maybe Int
  }

main :: IO ()
main = run =<< customExecParser (prefs showHelpOnEmpty) opts
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> header "palisade - a proof-gated execution kernel for machine-initiated money"
            <> progDesc "Money moves only with unforgeable evidence that every constraint holds."
        )

commandParser :: Parser Command
commandParser =
  hsubparser $
    cmd "check" "Parse and typecheck a .pal policy; print its rules and hash" (Check <$> policyArg)
      <> cmd "append" "Append administrative events (open, deposit, settle, reverse) to an event log" (Append <$> logOpt <*> strArgument (metavar "EVENTS.jsonl"))
      <> cmd "authorize" "Judge one intent against a log and a policy; print its certificate" (AuthorizeCmd <$> authOpts)
      <> cmd "verify" "Verify the hash chain of an event log (and an evidence log, with signatures)" (Verify <$> strArgument (metavar "LOG.jsonl") <*> optional (strOption (long "evidence" <> metavar "EVIDENCE.jsonl")))
      <> cmd "replay" "Rebuild the ledger from a log, optionally as of a time or sequence number" replayCmd
      <> cmd "audit" "Re-derive every certificate from the log and policies; flag executions without evidence" auditCmd
      <> cmd "shadow" "Shadow mode: judge a history that already happened; nothing is blocked" shadowCmd
      <> cmd "backtest" "Policy backtest: replay a history applying only what the policy allows" backtestCmd
      <> cmd "stress" "Adversarial stress test: generate attack sequences, check every law, shrink failures" stressCmd
      <> cmd "bench" "PolicyBench: score a policy against reconstructed 2025-2026 incidents" benchCmd
      <> cmd "keygen" "Generate an Ed25519 key for signing certificates" (Keygen <$> strOption (long "out" <> metavar "KEYFILE"))
      <> cmd "version" "Print the version" (pure Version)
  where
    cmd n d p = command n (info (p <**> helper) (progDesc d))
    policyArg = strArgument (metavar "POLICY.pal")
    policyOpt = strOption (long "policy" <> short 'p' <> metavar "POLICY.pal")
    logOpt = strOption (long "log" <> short 'l' <> metavar "LOG.jsonl")
    authOpts =
      AuthOpts
        <$> logOpt
        <*> policyOpt
        <*> strOption (long "intent" <> short 'i' <> metavar "INTENT.json")
        <*> switch (long "commit" <> help "If allowed, append the execution to the log; always append the certificate to the evidence log")
        <*> optional (strOption (long "evidence" <> metavar "EVIDENCE.jsonl" <> help "Evidence log (default: LOG with .evidence.jsonl)"))
        <*> optional (strOption (long "key" <> metavar "KEYFILE" <> help "Sign the certificate with this Ed25519 key"))
        <*> optional (strOption (long "approved-by" <> metavar "NAME" <> help "A human approves an escalation (never overrides a denial)"))
        <*> switch (long "json" <> help "Print the certificate as canonical JSON")
    replayCmd =
      Replay
        <$> strArgument (metavar "LOG.jsonl")
        <*> optional (strOption (long "at" <> metavar "TIME" <> help "ISO 8601 time, e.g. 2026-10-01T12:00:00Z"))
        <*> optional (option auto (long "seq" <> metavar "N"))
        <*> switch (long "json")
    auditCmd =
      Audit
        <$> logOpt
        <*> strOption (long "evidence" <> metavar "EVIDENCE.jsonl")
        <*> some policyOpt
    shadowCmd =
      Shadow
        <$> policyOpt
        <*> strArgument (metavar "HISTORY.jsonl")
        <*> optional (strOption (long "certs" <> metavar "OUT.jsonl" <> help "Write the certificate chain here"))
        <*> optional (strOption (long "key" <> metavar "KEYFILE"))
    backtestCmd =
      Backtest
        <$> policyOpt
        <*> strArgument (metavar "HISTORY.jsonl")
        <*> optional (strOption (long "baseline" <> metavar "POLICY.pal" <> help "Compare verdicts with another policy"))
        <*> option
          escalationReader
          (long "escalations" <> metavar "blocked|approved" <> value EscalationsBlocked <> help "How to treat escalations (default: blocked)")
    stressCmd =
      StressCmd
        <$> ( StressOpts
                <$> policyOpt
                <*> optional (strOption (long "setup" <> metavar "EVENTS.jsonl" <> help "Events that build the starting ledger"))
                <*> option auto (long "count" <> metavar "N" <> value 500 <> showDefault)
                <*> option auto (long "seed" <> metavar "N" <> value 42 <> showDefault)
                <*> option auto (long "steps" <> metavar "N" <> value 30 <> showDefault <> help "Maximum steps per sequence")
                <*> optional (option auto (long "case" <> metavar "N" <> help "Re-run a single case"))
            )
    benchCmd =
      BenchCmd
        <$> policyOpt
        <*> strArgument (metavar "SUITE_DIR" <> value "scenarios/policybench" <> showDefault)
    escalationReader = eitherReader $ \case
      "blocked" -> Right EscalationsBlocked
      "approved" -> Right EscalationsApproved
      s -> Left ("expected blocked or approved, got " <> s)

------------------------------------------------------------------------------
-- IO helpers

die' :: Text -> IO a
die' msg = do
  TIO.hPutStrLn stderr ("palisade: " <> msg)
  exitWith (ExitFailure 1)

loadPolicyFile :: FilePath -> IO Policy
loadPolicyFile path = do
  exists <- doesFileExist path
  unless exists (die' ("policy file not found: " <> T.pack path))
  src <- TIO.readFile path
  either die' pure (loadPolicy path src)

nonBlankLines :: BL.ByteString -> [(Int, BL.ByteString)]
nonBlankLines bs =
  [ (n, l)
  | (n, l) <- zip [1 ..] (BLC.lines bs)
  , let s = BLC.dropWhile isSpace l
  , not (BL.null s)
  , not ("//" `BLC.isPrefixOf` s)
  ]

readHistory :: FilePath -> IO [(Int, HistoryItem)]
readHistory path = do
  bs <- BL.readFile path
  forM (nonBlankLines bs) $ \(n, l) -> case parseHistoryLine l of
    Right h -> pure (n, h)
    Left e -> die' (T.pack path <> ":" <> T.pack (show n) <> ": " <> T.pack e)

readLog :: FilePath -> IO [LogEntry]
readLog path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      bs <- BL.readFile path
      case decodeEntries "event" bs of
        Right es -> pure es
        Left (n, e) -> die' (T.pack path <> ": entry " <> T.pack (show n) <> ": " <> T.pack e)

readEvidence :: FilePath -> IO [EvidenceEntry]
readEvidence path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      bs <- BL.readFile path
      case decodeEntries "certificate" bs of
        Right es -> pure es
        Left (n, e) -> die' (T.pack path <> ": entry " <> T.pack (show n) <> ": " <> T.pack e)

replayOrDie :: FilePath -> [LogEntry] -> IO LedgerState
replayOrDie path es = either (\e -> die' (T.pack path <> ": " <> renderReplayError e)) pure (replayLog es)

appendLines :: FilePath -> [BL.ByteString] -> IO ()
appendLines path ls = BL.appendFile path (BL.concat [l <> "\n" | l <- ls])

writeLines :: FilePath -> [BL.ByteString] -> IO ()
writeLines path ls = BL.writeFile path (BL.concat [l <> "\n" | l <- ls])

loadKey :: FilePath -> IO SigningKey
loadKey path = do
  t <- TIO.readFile path
  either die' pure (decodeSecret t)

defaultEvidence :: FilePath -> FilePath
defaultEvidence logPath = dropExtension logPath <> ".evidence.jsonl"

------------------------------------------------------------------------------
-- Commands

run :: Command -> IO ()
run = \case
  Version -> putStrLn ("palisade 0.1.0  (certificates: " <> T.unpack certVersion <> ")")
  Check path -> do
    pol <- loadPolicyFile path
    TIO.putStr (renderPolicySummary path pol)
  Append logPath eventsPath -> do
    entries <- readLog logPath
    st <- replayOrDie logPath entries
    items <- readHistory eventsPath
    let go s acc [] = Right (s, reverse acc)
        go s acc ((n, item) : rest) = case item of
          HMeta _ -> go s acc rest
          HIntent i _ -> Left (n, "intent " <> intentId i <> " cannot be appended; intents move money only through `palisade authorize`")
          HEvent ev -> case appendEvent s ev of
            Right (s', e) -> go s' (e : acc) rest
            Left err -> Left (n, renderApplyError err)
    case go st [] items of
      Left (n, msg) -> die' (T.pack eventsPath <> ":" <> T.pack (show n) <> ": " <> msg <> " (nothing was written)")
      Right (st', new) -> do
        appendLines logPath (map (encodeEntry "event") new)
        TIO.putStrLn ("appended " <> T.pack (show (length new)) <> " events; log at seq " <> T.pack (show (lsSeq st')) <> ", head " <> lsHead st')
  AuthorizeCmd o -> do
    pol <- loadPolicyFile (aoPolicy o)
    entries <- readLog (aoLog o)
    st <- replayOrDie (aoLog o) entries
    raw <- BL.readFile (aoIntent o)
    i <- either (\e -> die' (T.pack (aoIntent o) <> ": " <> T.pack e)) pure (parseIntentJson raw)
    let dec = authorizeWith pol (approvedBy <$> aoApprover o) st i
        cert = decCertificate dec
    if aoJson o
      then BLC.putStrLn (canonical (toJSON cert))
      else TIO.putStr (renderCertificate cert)
    when (aoCommit o) $ do
      let evPath = fromMaybe (defaultEvidence (aoLog o)) (aoEvidence o)
      evidence <- readEvidence evPath
      either (\e -> die' (T.pack evPath <> ": " <> renderChainError e)) pure (verifyChain "certificate" evidence)
      let prev = maybe genesisHash entryHash (lastMaybe evidence)
          entry0 = mkEntry "certificate" (length evidence + 1) prev cert
      entry <- case aoKey o of
        Nothing -> pure entry0
        Just k -> do
          key <- loadKey k
          pure entry0 {entrySig = Just (signHash key (certHash cert))}
      forM_ (decEntry dec) $ \e -> appendLines (aoLog o) [encodeEntry "event" e]
      appendLines evPath [encodeEntry "certificate" entry]
      unless (aoJson o) $
        TIO.putStrLn $
          "committed: "
            <> (if isJust (decEntry dec) then "execution appended to " <> T.pack (aoLog o) <> "; " else "nothing executed; ")
            <> "certificate appended to "
            <> T.pack evPath
    exitWith $ case decVerdict dec of
      Allow -> ExitSuccess
      Deny -> ExitFailure 2
      Escalate -> ExitFailure 3
  Verify logPath mEvidence -> do
    entries <- readLog logPath
    case replayLog entries of
      Left e -> die' (T.pack logPath <> ": " <> renderReplayError e)
      Right st -> TIO.putStrLn ("log ok: " <> T.pack (show (length entries)) <> " entries, head " <> lsHead st)
    forM_ mEvidence $ \evPath -> do
      evidence <- readEvidence evPath
      either (\e -> die' (T.pack evPath <> ": " <> renderChainError e)) pure (verifyChain "certificate" evidence)
      sigs <- forM evidence $ \e -> case entrySig e of
        Nothing -> pure Nothing
        Just s -> case verifySignature s (certHash (entryPayload e)) of
          Right pk -> pure (Just pk)
          Left err -> die' (T.pack evPath <> ": entry " <> T.pack (show (entrySeq e)) <> ": " <> err)
      let signers = M.fromListWith (+) [(pk, 1 :: Int) | Just pk <- sigs]
      TIO.putStrLn $
        "evidence ok: " <> T.pack (show (length evidence)) <> " certificates, "
          <> T.pack (show (sum (M.elems signers)))
          <> " signed"
          <> T.concat ["\n  signer " <> shortHash pk <> "...  " <> T.pack (show n) | (pk, n) <- M.toList signers]
  Replay logPath mAt mSeq asJson -> do
    entries <- readLog logPath
    stop <- case (mAt, mSeq) of
      (Just _, Just _) -> die' "use --at or --seq, not both"
      (Just t, _) -> do
        at <- maybe (die' ("cannot parse time " <> T.pack t)) pure (iso8601ParseM t :: Maybe UTCTime)
        pure (\e -> eventTime (entryPayload e) > at)
      (_, Just n) -> pure (\e -> entrySeq e > n)
      _ -> pure (const False)
    st <- either (\e -> die' (T.pack logPath <> ": " <> renderReplayError e)) pure (replayUntil stop entries)
    if asJson then BLC.putStrLn (canonical (toJSON st)) else TIO.putStr (renderLedger st)
  Audit logPath evPath policyPaths -> do
    pols <- mapM loadPolicyFile policyPaths
    entries <- readLog logPath
    _ <- replayOrDie logPath entries
    evidence <- readEvidence evPath
    either (\e -> die' (T.pack evPath <> ": " <> renderChainError e)) pure (verifyChain "certificate" evidence)
    let byHash = M.fromList [(policyHash p, p) | p <- pols]
        states = M.fromList (zip [0 ..] (scanl stepState emptyLedger entries))
        stepState s e = either (const s) fst (appendEvent s (entryPayload e))
        rederive cert = do
          pol <- maybe (Left ("policy " <> shortHash (certPolicyHash cert) <> " was not provided")) Right (M.lookup (certPolicyHash cert) byHash)
          st <- maybe (Left ("log has no state at seq " <> T.pack (show (certPreSeq cert)))) Right (M.lookup (certPreSeq cert) states)
          unless (lsHead st == certPreHead cert) (Left "pre-state head does not match the log")
          let again = decCertificate (authorizeWith pol (approvedBy <$> certApprovedBy cert) st (certIntent cert))
          if certHash again == certHash cert then Right () else Left "re-derived certificate differs"
        certProblems =
          [ "certificate " <> T.pack (show (entrySeq e)) <> " (" <> intentId (certIntent c) <> "): " <> msg
          | e <- evidence
          , let c = entryPayload e
          , Left msg <- [rederive c]
          ]
        allowedAt =
          M.fromList
            [ (s, certIntentHash c)
            | e <- evidence
            , let c = entryPayload e
            , certVerdict c == Allow
            , Just s <- [certPostSeq c]
            ]
        execProblems =
          [ "log entry " <> T.pack (show (entrySeq e)) <> " executes " <> intentId i <> " with no allowing certificate"
          | e <- entries
          , EvExecute _ i _ <- [entryPayload e]
          , M.lookup (entrySeq e) allowedAt /= Just (intentHash i)
          ]
        problems = certProblems <> execProblems
        executions = length [() | e <- entries, EvExecute {} <- [entryPayload e]]
    TIO.putStrLn $
      "audited " <> T.pack (show (length evidence)) <> " certificates against " <> T.pack (show (length entries))
        <> " log entries ("
        <> T.pack (show executions)
        <> " executions)"
    if null problems
      then TIO.putStrLn "audit ok: every certificate re-derives byte for byte, and every execution has an allowing certificate"
      else do
        mapM_ (TIO.putStrLn . ("  " <>)) problems
        exitWith (ExitFailure 1)
  Shadow polPath histPath mCerts mKey -> do
    pol <- loadPolicyFile polPath
    items <- readHistory histPath
    let rep = simulate ShadowMode pol items
    TIO.putStr (renderSimReport ShadowMode (T.pack histPath <> "   policy sha256:" <> shortHash (policyHash pol)) rep)
    forM_ mCerts $ \out -> do
      key <- traverse loadKey mKey
      let sign e = case key of
            Nothing -> e
            Just k -> e {entrySig = Just (signHash k (certHash (entryPayload e)))}
      writeLines out (map (encodeEntry "certificate" . sign) (srEvidence rep))
      TIO.putStrLn ("\nwrote " <> T.pack (show (length (srEvidence rep))) <> " certificates to " <> T.pack out)
  Backtest polPath histPath mBaseline esc -> do
    pol <- loadPolicyFile polPath
    items <- readHistory histPath
    let mode = CounterfactualMode esc
        rep = simulate mode pol items
        reality = simulate ShadowMode pol items
    TIO.putStr (renderSimReport mode (T.pack histPath <> "   policy sha256:" <> shortHash (policyHash pol)) rep)
    TIO.putStr (renderComparison reality rep)
    forM_ mBaseline $ \bp -> do
      base <- loadPolicyFile bp
      TIO.putStr (renderBaselineDiff (simulate mode base items) rep)
  StressCmd o -> do
    pol <- loadPolicyFile (soPolicy o)
    setupItems <- maybe (pure []) readHistory (soSetup o)
    setup <- buildSetup setupItems
    let cfg =
          (defaultStressConfig pol setup)
            { scCount = soCount o
            , scSeed = soSeed o
            , scMaxSteps = soSteps o
            , scOnlyCase = soCase o
            }
        rep = runStress cfg
        reproduce =
          "palisade stress --policy " <> T.pack (soPolicy o)
            <> maybe "" (\s -> " --setup " <> T.pack s) (soSetup o)
            <> " --seed "
            <> T.pack (show (soSeed o))
            <> " --steps "
            <> T.pack (show (soSteps o))
    TIO.putStrLn ("stress   policy sha256:" <> shortHash (policyHash pol))
    TIO.putStr (renderStress reproduce rep)
    when (any (isJust . lsCounterexample) (srLaws rep)) (exitWith (ExitFailure 1))
  BenchCmd polPath dir -> do
    pol <- loadPolicyFile polPath
    files <- sort . filter (".jsonl" `isSuffixOf`) <$> listDirectory dir
    when (null files) (die' ("no .jsonl incident files in " <> T.pack dir))
    scores <- forM files $ \f -> do
      items <- readHistory (dir </> f)
      pure (scoreIncident pol (mkIncident (T.pack (takeBaseName f)) items))
    TIO.putStr (renderBench (T.pack polPath <> "   sha256:" <> shortHash (policyHash pol)) scores)
  Keygen out -> do
    exists <- doesFileExist out
    when exists (die' (T.pack out <> " already exists; refusing to overwrite a key"))
    k <- generateKey
    TIO.writeFile out (encodeSecret k <> "\n")
    TIO.writeFile (out <> ".pub") (publicKeyHex k <> "\n")
    TIO.putStrLn ("wrote secret key to " <> T.pack out <> " (keep it private) and public key to " <> T.pack out <> ".pub")
    TIO.putStrLn ("public key " <> publicKeyHex k)
  where
    lastMaybe xs = if null xs then Nothing else Just (last xs)
    buildSetup = go emptyLedger []
      where
        go _ acc [] = pure (reverse acc)
        go s acc ((n, item) : rest) = case item of
          HEvent ev -> case appendEvent s ev of
            Right (s', e) -> go s' (e : acc) rest
            Left err -> die' ("setup line " <> T.pack (show n) <> ": " <> renderApplyError err)
          HIntent {} -> die' ("setup line " <> T.pack (show n) <> ": setup files hold events only (open, deposit, settle, reverse)")
          HMeta _ -> go s acc rest

