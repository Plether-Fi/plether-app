module Plether.AA.KernelReconciler
  ( KernelReconcilerConfig (..)
  , ensureKernelReconcilerSchema
  , validateKernelReconcilerConfig
  , runKernelReconciler
  , verifyKernelOperationReceipt
  , reconstructKernelReceipt
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Exception (Exception, throwIO, try)
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query, withTransaction)
import Numeric (readHex)
import qualified Plether.AA.Kernel as Kernel
import Plether.AA.Paymaster (canonicalQuantity)
import Plether.AA.Reconciler
  ( BlockHeader (..), ProviderResult (..), UserOperationEvent (..)
  , boundariesRemainCanonical, parseUserOperationEventAt, readAgreedBlock
  , readAgreedRuntimeCode, readAgreedSafeBoundary, readAgreedUserOperationLogsAt
  , readRpcChainId, targetReachesSafeBoundary, validateDeploymentAnchor
  , validateSafeHeadFreshness, validateTargetTimestamp, verifyEventBlockHeaders
  )
import Plether.Database (DbPool, withDb, withDbAdvisoryLock)
import Plether.Database.AaSponsorship (pruneAaRateWindows)
import Plether.Database.KernelOperation
  ( KernelInclusion (..), KernelScope (..), expireKernelSponsorshipsThrough
  , kernelSponsorshipStateIsEmpty, lockKernelSponsorScope, settleKernelSponsorship
  )
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall, keccak256)
import Plether.Ethereum.Client (CallParams (..), EthClient, ethCallAtBlock, rpcCall)
import Plether.Logging (field, logError, logInfoEvery, logWarnEvery)

data KernelReconcilerConfig = KernelReconcilerConfig
  { krcScope :: KernelScope
  , krcEntryPointCodeHash :: Text
  , krcPaymaster :: Text
  , krcPaymasterCodeHash :: Text
  , krcStartBlock :: Integer
  , krcStartBlockHash :: Text
  , krcPollSeconds :: Int
  , krcBatchBlocks :: Integer
  , krcMinDepositWei :: Integer
  , krcFailurePauseSeconds :: Integer
  , krcMaxSafeLagSeconds :: Integer
  }
  deriving stock (Eq, Show)

data ReconcileFailure = TemporarilyUnavailable Text | InconsistentEvidence Text
  deriving stock (Eq, Show)

instance Exception ReconcileFailure

ensureKernelReconcilerSchema :: Connection -> IO ()
ensureKernelReconcilerSchema conn = withTransaction conn $ do
  void $ (query conn
    "SELECT 1::integer FROM (SELECT pg_advisory_xact_lock(hashtextextended(?::text,0))) AS schema_lock"
    (Only ("plether/kernel/reconciler-schema/v1" :: Text)) :: IO [Only Int])
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_kernel_reconciler_state (\
    \ deployment TEXT NOT NULL, chain_id BIGINT NOT NULL, entry_point TEXT NOT NULL, paymaster TEXT NOT NULL,\
    \ entry_point_code_hash TEXT NOT NULL, paymaster_code_hash TEXT NOT NULL,\
    \ start_block BIGINT NOT NULL CHECK (start_block > 0), start_block_hash TEXT NOT NULL,\
    \ safe_block BIGINT, safe_block_hash TEXT, safe_timestamp BIGINT,\
    \ last_success_at TIMESTAMPTZ, paused_reason TEXT,\
    \ CHECK ((safe_block IS NULL AND safe_block_hash IS NULL AND safe_timestamp IS NULL AND last_success_at IS NULL) OR\
    \   (safe_block IS NOT NULL AND safe_block >= start_block - 1 AND safe_block_hash IS NOT NULL AND safe_timestamp IS NOT NULL AND safe_timestamp >= 0)),\
    \ PRIMARY KEY (deployment, chain_id, entry_point, paymaster)\
    \ )"

validateKernelReconcilerConfig :: KernelReconcilerConfig -> Either Text ()
validateKernelReconcilerConfig cfg = do
  let scope = krcScope cfg
  unless (not (T.null (ksDeployment scope)) && ksChainId scope == 421614
    && ksEntryPoint scope == "0x433709009b8330fda32311df1c2afa402ed8d009") $
    Left "Kernel reconciliation requires the Arbitrum Sepolia EntryPoint 0.9 profile"
  unless (isNonzeroCanonicalHex 20 (krcPaymaster cfg)
    && all (isNonzeroCanonicalHex 32) [krcPaymasterCodeHash cfg, krcEntryPointCodeHash cfg, krcStartBlockHash cfg]) $
    Left "Kernel reconciliation requires canonical nonzero contract and deployment pins"
  unless (inRange 1 (2 ^ (63 :: Int) - 1) (krcStartBlock cfg)
    && inRange 1 60 (krcPollSeconds cfg) && inRange 1 10_000 (krcBatchBlocks cfg)
    && inRange 1 (2 ^ (256 :: Int) - 1) (krcMinDepositWei cfg)
    && inRange (max 5 $ fromIntegral $ krcPollSeconds cfg) 300 (krcFailurePauseSeconds cfg)
    && inRange 60 1_800 (krcMaxSafeLagSeconds cfg)) $
    Left "Kernel reconciliation settings exceed the reviewed canary bounds"
 where
  inRange lo hi n = n >= lo && n <= hi

-- The caller must configure administratively independent RPC providers. A
-- leadership lock avoids duplicate scans. Scoped transaction locks still fence
-- economic mutations against concurrent issuance and process interruption.
runKernelReconciler :: DbPool -> EthClient -> EthClient -> KernelReconcilerConfig -> IO ()
runKernelReconciler pool primary secondary cfg = do
  either (fail . T.unpack) pure $ validateKernelReconcilerConfig cfg
  failedSince <- newIORef Nothing
  withDb pool $ \leader -> withDbAdvisoryLock leader leaderKey $ forever $ do
    assertLeadership leader
    result <- try @ReconcileFailure $ reconcileCycle pool primary secondary cfg
    case result of
      Right () -> writeIORef failedSince Nothing
      Left (InconsistentEvidence reason) -> do
        pauseIssuance pool cfg reason
        logError "kernel_aa_reconciliation_paused" "Kernel sponsorship paused after inconsistent chain evidence" [field "reason" reason]
        throwIO $ InconsistentEvidence reason
      Left (TemporarilyUnavailable reason) -> do
        now <- wallTime
        since <- fromMaybe now <$> readIORef failedSince
        writeIORef failedSince $ Just since
        logWarnEvery 30 "kernel_aa_reconciliation_unavailable" "Kernel reconciliation could not verify chain evidence" [field "reason" reason]
        when (now - since >= krcFailurePauseSeconds cfg) $
          pauseIssuance pool cfg "dual-provider reconciliation unavailable beyond configured threshold"
    threadDelay $ krcPollSeconds cfg * 1_000_000
 where
  scope = krcScope cfg
  digest = keccak256 $ TE.encodeUtf8 $ T.intercalate ":"
    ["plether/kernel/reconciler/v1", ksDeployment scope, T.pack $ show $ ksChainId scope, ksEntryPoint scope, krcPaymaster cfg]
  unsignedKey = decodeUint256 $ BS.replicate 24 0 <> BS.take 8 digest
  leaderKey = if unsignedKey >= 2 ^ (63 :: Int) then unsignedKey - 2 ^ (64 :: Int) else unsignedKey
  wordLimit = 2 ^ (32 :: Int)
  assertLeadership connection = do
    rows <- query connection
      "SELECT EXISTS (SELECT 1 FROM pg_locks WHERE locktype='advisory' AND pid=pg_backend_pid() \
      \ AND granted AND objsubid=1 AND classid::bigint=? AND objid::bigint=?)"
      (unsignedKey `div` wordLimit, unsignedKey `mod` wordLimit) :: IO [Only Bool]
    unless (rows == [Only True]) $ fail "Kernel reconciliation leadership lock was lost"

reconcileCycle :: DbPool -> EthClient -> EthClient -> KernelReconcilerConfig -> IO ()
reconcileCycle pool primary secondary cfg =
  runExceptT verify >>= either throwIO pure
 where
  scope = krcScope cfg
  ep = ksEntryPoint scope
  paymaster = krcPaymaster cfg
  safeBoundary = provider $ readAgreedSafeBoundary primary secondary
  headerAt height = provider $ readAgreedBlock primary secondary (canonicalQuantity height)
  codeAt height address = provider $ readAgreedRuntimeCode primary secondary height address
  verify = do
    attestChain scope primary secondary
    safe <- safeBoundary
    now <- liftIO wallTime
    consistent $ validateSafeHeadFreshness (krcMaxSafeLagSeconds cfg) now safe
    when (bhNumber safe < krcStartBlock cfg) $
      throwE $ TemporarilyUnavailable "safe head has not reached the Kernel paymaster deployment"
    deployment <- headerAt $ krcStartBlock cfg
    unless (bhHash deployment == krcStartBlockHash cfg) $
      throwE $ InconsistentEvidence "Kernel paymaster deployment anchor is no longer canonical"
    entryPointCode <- codeAt (bhNumber safe) ep
    paymasterCode <- codeAt (bhNumber safe) paymaster
    consistent $ attestCode (krcEntryPointCodeHash cfg) "EntryPoint" entryPointCode
    consistent $ attestCode (krcPaymasterCodeHash cfg) "paymaster" paymasterCode
    cursor <- findCursor safe
    when (bhNumber cursor > bhNumber safe) $
      throwE $ InconsistentEvidence "safe boundary moved behind the Kernel cursor"
    canonicalCursor <- headerAt $ bhNumber cursor
    unless (canonicalCursor == cursor) $
      throwE $ InconsistentEvidence "Kernel cursor disagrees with the canonical block"
    target <- headerAt $ min (bhNumber safe) (bhNumber cursor + krcBatchBlocks cfg)
    consistent $ validateTargetTimestamp now canonicalCursor target
    void $ consistent $ targetReachesSafeBoundary target safe
    events <- if bhNumber target == bhNumber cursor
      then pure []
      else provider $ readAgreedUserOperationLogsAt primary secondary ep paymaster (bhNumber cursor + 1) (bhNumber target)
    provider $ verifyEventBlockHeaders primary secondary events
    depositHealthy <- attestDeposit primary secondary cfg (bhNumber target)
    safeAgain <- safeBoundary
    nowAgain <- liftIO wallTime
    consistent $ validateSafeHeadFreshness (krcMaxSafeLagSeconds cfg) nowAgain safeAgain
    cursorAgain <- headerAt $ bhNumber cursor
    targetAgain <- headerAt $ bhNumber target
    safeHeaderAgain <- headerAt $ bhNumber safe
    deploymentAgain <- headerAt $ krcStartBlock cfg
    caughtUp <- consistent $ targetReachesSafeBoundary target safeAgain
    consistent $ boundariesRemainCanonical cursor target cursorAgain targetAgain
    unless (safe == safeHeaderAgain && deployment == deploymentAgain) $
      throwE $ InconsistentEvidence "Kernel paymaster attestation blocks changed during reconciliation"
    liftIO $ do
      publishProgress pool cfg cursor target events caughtUp depositHealthy
      unless depositHealthy $
        logWarnEvery 30 "kernel_aa_paymaster_unavailable" "Kernel paymaster is paused or underfunded" []
      when (caughtUp && depositHealthy) $
        logInfoEvery 60 "kernel_aa_reconciliation_caught_up" "Kernel sponsorship reconciliation reached the dual-provider safe boundary"
          [field "safe_block" (bhNumber target), field "event_count" (length events)]

  findCursor safe = do
    row <- liftIO $ withDb pool $ \conn -> readCursor conn cfg
    case row of
      Just cursor -> pure cursor
      Nothing -> do
        let start = krcStartBlock cfg
        deployed <- headerAt start
        prior <- headerAt (start - 1)
        beforeCode <- codeAt (start - 1) paymaster
        deployedCode <- codeAt start paymaster
        consistent $ validateDeploymentAnchor (krcStartBlockHash cfg) (krcPaymasterCodeHash cfg) deployed beforeCode deployedCode
        deployedAgain <- headerAt start
        priorAgain <- headerAt (start - 1)
        safeAgain <- safeBoundary
        consistent $ boundariesRemainCanonical prior deployed priorAgain deployedAgain
        void $ consistent $ targetReachesSafeBoundary deployed safeAgain
        void $ consistent $ targetReachesSafeBoundary deployed safe
        liftIO $ initializeCursor pool cfg prior

attestChain :: KernelScope -> EthClient -> EthClient -> ExceptT ReconcileFailure IO ()
attestChain scope primary secondary = do
  (firstChain, secondChain) <- liftIO $ concurrently (readRpcChainId primary) (readRpcChainId secondary)
  a <- available firstChain
  b <- available secondChain
  unless (a == ksChainId scope && b == ksChainId scope) $
    throwE $ InconsistentEvidence "Kernel RPC providers reported a different chain"

attestCode :: Text -> Text -> BS.ByteString -> Either Text ()
attestCode expected label code = unless (not (BS.null code) && hashBytes code == expected) $
  Left $ "Kernel " <> label <> " runtime code hash does not match its deployment pin"

attestDeposit :: EthClient -> EthClient -> KernelReconcilerConfig -> Integer -> ExceptT ReconcileFailure IO Bool
attestDeposit primary secondary cfg height = do
  (firstDeposit, secondDeposit) <- liftIO $ concurrently (depositInfo primary) (depositInfo secondary)
  a <- available firstDeposit
  b <- available secondDeposit
  unless (a == b) $ throwE $ InconsistentEvidence "providers disagree on the Kernel paymaster deposit"
  let (deposit, staked, stake, delay, withdrawal, paused) = a
  pure $ deposit >= krcMinDepositWei cfg && staked
    && stake >= Kernel.minimumKernelStakeWei
    && delay >= 86400 && withdrawal == 0 && not paused
 where
  depositInfo client = do
    (depositResult, pausedResult) <- concurrently
      (ethCallAtBlock client (CallParams (ksEntryPoint $ krcScope cfg) $ encodeCall "getDepositInfo(address)" [encodeAddress $ krcPaymaster cfg]) height)
      (ethCallAtBlock client (CallParams (krcPaymaster cfg) $ encodeCall "paused()" []) height)
    pure $ case (depositResult, pausedResult) of
      (Right bytes, Right pausedBytes) | BS.length bytes == 160 && BS.length pausedBytes == 32 -> do
        let word index = decodeUint256 $ BS.take 32 $ BS.drop (index * 32) bytes
        staked <- flagAt (word 1)
        paused <- flagAt (decodeUint256 pausedBytes)
        unless (word 0 < 2 ^ (112 :: Int) && word 2 < 2 ^ (112 :: Int)
          && word 3 < 2 ^ (32 :: Int) && word 4 < 2 ^ (48 :: Int)) $
          Left "Kernel paymaster deposit response is noncanonical"
        pure (word 0, staked, word 2, word 3, word 4, paused)
      _ -> Left "Kernel paymaster status could not be read"
  flagAt 0 = Right False
  flagAt 1 = Right True
  flagAt _ = Left "Kernel paymaster status returned a noncanonical flag"

type KernelIdentity = (Text, Integer, Text, Text)

identity :: KernelReconcilerConfig -> KernelIdentity
identity cfg = (ksDeployment scope, ksChainId scope, ksEntryPoint scope, krcPaymaster cfg)
 where scope = krcScope cfg

registerScope :: Connection -> KernelReconcilerConfig -> IO ()
registerScope conn cfg = do
  let (deployment, chainId, entryPoint, paymaster) = identity cfg
  void $ execute conn
    "INSERT INTO aa_kernel_reconciler_state (deployment,chain_id,entry_point,paymaster,entry_point_code_hash,\
    \paymaster_code_hash,start_block,start_block_hash) VALUES (?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"
    (deployment, chainId, entryPoint, paymaster, krcEntryPointCodeHash cfg, krcPaymasterCodeHash cfg, krcStartBlock cfg, krcStartBlockHash cfg)

readCursor :: Connection -> KernelReconcilerConfig -> IO (Maybe BlockHeader)
readCursor conn cfg = do
  rows <- query conn
    "SELECT entry_point_code_hash,paymaster_code_hash,start_block,start_block_hash,safe_block,safe_block_hash,safe_timestamp\
    \ FROM aa_kernel_reconciler_state WHERE deployment=? AND chain_id=? AND entry_point=? AND paymaster=?"
    (identity cfg) :: IO [(Text, Text, Integer, Text, Maybe Integer, Maybe Text, Maybe Integer)]
  case rows of
    [] -> pure Nothing
    [(epHash, pmHash, start, startHash, number, hashValue, timestamp)] -> do
      unless ((epHash, pmHash, start, startHash) == (krcEntryPointCodeHash cfg, krcPaymasterCodeHash cfg, krcStartBlock cfg, krcStartBlockHash cfg)) $
        throwIO $ InconsistentEvidence "Kernel reconciler scope is bound to different deployment pins"
      case (number, hashValue, timestamp) of
        (Just height, Just hash, Just time) -> pure $ Just $ BlockHeader height hash time
        (Nothing, Nothing, Nothing) -> pure Nothing
        _ -> throwIO $ InconsistentEvidence "Kernel reconciler has an incomplete cursor"
    _ -> throwIO $ InconsistentEvidence "Kernel reconciler scope is ambiguous"

initializeCursor :: DbPool -> KernelReconcilerConfig -> BlockHeader -> IO BlockHeader
initializeCursor pool cfg initial = withDb pool $ \conn -> withTransaction conn $ do
  lockKernelSponsorScope conn (krcScope cfg) (krcPaymaster cfg)
  registerScope conn cfg
  existing <- readCursor conn cfg
  case existing of
    Just _ -> throwIO $ InconsistentEvidence "Kernel cursor unexpectedly appeared under the leadership lock"
    Nothing -> do
      empty <- kernelSponsorshipStateIsEmpty conn (krcScope cfg) (krcPaymaster cfg)
      unless empty $ throwIO $ InconsistentEvidence "Kernel cursor is missing while prior sponsorship state exists"
      setCursor conn cfg initial False False
      pure initial

publishProgress :: DbPool -> KernelReconcilerConfig -> BlockHeader -> BlockHeader -> [UserOperationEvent] -> Bool -> Bool -> IO ()
publishProgress pool cfg previous target events caughtUp depositHealthy = withDb pool $ \conn -> withTransaction conn $ do
  lockKernelSponsorScope conn scope paymaster
  actual <- readCursor conn cfg
  unless (actual == Just previous) $ throwIO $ InconsistentEvidence "Kernel cursor changed before settlement"
  mapM_ (settle conn) events
  expireKernelSponsorshipsThrough conn scope paymaster (bhTimestamp target)
  void $ pruneAaRateWindows conn
  setCursor conn cfg target caughtUp depositHealthy
 where
  scope = krcScope cfg
  paymaster = krcPaymaster cfg
  settle conn event = do
    matched <- settleKernelSponsorship conn scope paymaster (uoeHash event) $ inclusion paymaster event
    unless matched $ throwIO $ InconsistentEvidence $
      "canonical Kernel sponsor event did not match its durable authorization and cost limit for " <> uoeHash event

setCursor :: Connection -> KernelReconcilerConfig -> BlockHeader -> Bool -> Bool -> IO ()
setCursor conn cfg header caughtUp depositHealthy = do
  let (deployment, chainId, entryPoint, paymaster) = identity cfg
  rows <- execute conn
    "UPDATE aa_kernel_reconciler_state SET safe_block=?, safe_block_hash=?, safe_timestamp=?,\
    \ last_success_at=CASE WHEN NOT ? OR paused_reason IS NOT NULL THEN NULL\
    \ WHEN ? THEN clock_timestamp() ELSE last_success_at END\
    \ WHERE deployment=? AND chain_id=? AND entry_point=? AND paymaster=?"
    (bhNumber header, bhHash header, bhTimestamp header, depositHealthy, caughtUp, deployment, chainId, entryPoint, paymaster)
  unless (rows == 1) $ throwIO $ InconsistentEvidence "Kernel reconciler could not persist its verified cursor"

pauseIssuance :: DbPool -> KernelReconcilerConfig -> Text -> IO ()
pauseIssuance pool cfg reason = withDb pool $ \conn -> withTransaction conn $ do
  lockKernelSponsorScope conn (krcScope cfg) (krcPaymaster cfg)
  registerScope conn cfg
  let (deployment, chainId, entryPoint, paymaster) = identity cfg
  void $ execute conn
    "UPDATE aa_kernel_reconciler_state SET paused_reason=COALESCE(paused_reason,?),last_success_at=NULL\
    \ WHERE deployment=? AND chain_id=? AND entry_point=? AND paymaster=?"
    (reason, deployment, chainId, entryPoint, paymaster)

provider :: IO (ProviderResult a) -> ExceptT ReconcileFailure IO a
provider action = ExceptT $ do
  result <- action
  pure $ case result of
    ProviderAgreed value -> Right value
    ProviderMismatch failure -> Left $ InconsistentEvidence failure
    ProviderUnavailable failure -> Left $ TemporarilyUnavailable failure

consistent :: Either Text a -> ExceptT ReconcileFailure IO a
consistent = either (throwE . InconsistentEvidence) pure

available :: Either Text a -> ExceptT ReconcileFailure IO a
available = either (throwE . TemporarilyUnavailable) pure

wallTime :: IO Integer
wallTime = floor <$> getPOSIXTime

hashBytes :: BS.ByteString -> Text
hashBytes = ("0x" <>) . TE.decodeUtf8 . B16.encode . keccak256

isNonzeroCanonicalHex :: Int -> Text -> Bool
isNonzeroCanonicalHex len value = isHexBytes len value && value /= ("0x" <> T.replicate (len * 2) "0")

isHexBytes :: Int -> Text -> Bool
isHexBytes len value = T.length value == len * 2 + 2
  && T.isPrefixOf "0x" value && T.all (`elem` ("0123456789abcdef" :: String)) (T.drop 2 value)

zeroAddress :: Text
zeroAddress = "0x0000000000000000000000000000000000000000"

inclusion :: Text -> UserOperationEvent -> KernelInclusion
inclusion paymaster event = KernelInclusion
  { kiSender = uoeSender event
  , kiNonce = uoeNonce event
  , kiPaymaster = if paymaster == zeroAddress then Nothing else Just paymaster
  , kiTransactionHash = uoeTransactionHash event
  , kiBlockHash = uoeBlockHash event
  , kiBlockNumber = uoeBlockNumber event
  , kiExecutionSuccess = uoeSuccess event
  , kiActualGasCost = uoeActualGasCost event
  }

-- Use Alto's receipt only to locate a transaction. Both chain providers must
-- agree on the relevant receipt logs and block at the shared safe boundary.
verifyKernelOperationReceipt
  :: KernelScope -> EthClient -> EthClient -> Integer -> Text -> Integer -> Text -> Text -> Value
  -> IO (Either Text (KernelInclusion, Value))
verifyKernelOperationReceipt scope primary secondary maxLag sender nonce paymaster operationHash altoReceipt = do
  result <- runExceptT $ do
    nested <- consistent $ member "receipt" altoReceipt
    tx <- consistent $ fixedText 32 "transactionHash" nested
    attestChain scope primary secondary
    safe <- provider $ readAgreedSafeBoundary primary secondary
    now <- liftIO wallTime
    consistent $ validateSafeHeadFreshness maxLag now safe
    (primaryReceipt, secondaryReceipt) <- liftIO $ concurrently (readReceipt primary tx) (readReceipt secondary tx)
    left <- available primaryReceipt
    right <- available secondaryReceipt
    (event, leftLogs, rpcValue) <- consistent $ receiptEvidence (ksEntryPoint scope) sender nonce paymaster operationHash tx left
    (rightEvent, rightLogs, _) <- consistent $ receiptEvidence (ksEntryPoint scope) sender nonce paymaster operationHash tx right
    unless (event {uoeRaw = Null} == rightEvent {uoeRaw = Null} && leftLogs == rightLogs) $
      throwE $ InconsistentEvidence "independent chain receipts disagree on the Kernel event or operation logs"
    when (uoeBlockNumber event > bhNumber safe) $
      throwE $ TemporarilyUnavailable "Kernel operation is awaiting the shared safe boundary"
    provider $ verifyEventBlockHeaders primary secondary [event]
    safeAgain <- provider $ readAgreedSafeBoundary primary secondary
    originalSafe <- provider $ readAgreedBlock primary secondary (canonicalQuantity $ bhNumber safe)
    nowAgain <- liftIO wallTime
    consistent $ validateSafeHeadFreshness maxLag nowAgain safeAgain
    unless (safe == originalSafe) $
      throwE $ InconsistentEvidence "safe boundary changed while verifying the Kernel receipt"
    void $ consistent $ targetReachesSafeBoundary safe safeAgain
    when (uoeBlockNumber event == bhNumber safe && uoeBlockHash event /= bhHash safe) $
      throwE $ InconsistentEvidence "Kernel receipt block does not match the safe boundary"
    pure (inclusion (T.toLower paymaster) event, rpcValue)
  pure $ case result of
    Left (InconsistentEvidence err) -> Left err
    Left (TemporarilyUnavailable err) -> Left err
    Right receipt -> Right receipt
 where
  readReceipt client tx = do
    answer <- rpcCall client "eth_getTransactionReceipt" $ toJSON [String tx]
    pure $ case answer of
      Right value@(Object _) -> Right value
      _ -> Left "chain receipt is unavailable"

-- Pure receipt reconstruction shared by canonical-provider validation and its
-- tests. Posting a bundler-supplied receipt never creates inclusion evidence.
reconstructKernelReceipt
  :: Text -> Text -> Integer -> Text -> Text -> Text -> Value -> Either Text (KernelInclusion, Value)
reconstructKernelReceipt entryPoint sender nonce paymaster operationHash tx receipt = do
  (event, _, rpcValue) <- receiptEvidence entryPoint sender nonce paymaster operationHash tx receipt
  pure (inclusion (T.toLower paymaster) event, rpcValue)

data ReceiptLog = ReceiptLog
  { rlIndex :: Integer
  , rlAddress :: Text
  , rlTopics :: [Text]
  , rlData :: Text
  }
  deriving stock (Eq, Show)

receiptEvidence
  :: Text -> Text -> Integer -> Text -> Text -> Text -> Value
  -> Either Text (UserOperationEvent, [ReceiptLog], Value)
receiptEvidence entryPoint expectedSender expectedNonce paymaster expectedHash expectedTx receipt = do
  tx <- fixedText 32 "transactionHash" receipt
  height <- quantityMember "blockNumber" receipt
  blockHash <- fixedText 32 "blockHash" receipt
  status <- quantityMember "status" receipt
  unless (tx == T.toLower expectedTx && status == 1) $
    Left "Kernel receipt locator or outer transaction status disagrees with the chain"
  logs <- arrayMember "logs" receipt
  parsedLogs <- traverse (parseLog tx height blockHash) logs
  let indices = map rlIndex parsedLogs
  unless (and $ zipWith (<) indices (drop 1 indices)) $
    Left "Kernel bundle has unordered or duplicate log indices"
  let isEntryPoint logEntry = rlAddress logEntry == T.toLower entryPoint
      isEvent logEntry = isEntryPoint logEntry && take 1 (rlTopics logEntry) == [operationTopic]
      isBoundary logEntry = isEvent logEntry || (isEntryPoint logEntry && rlTopics logEntry == [executionTopic])
      matching logEntry = isEvent logEntry && take 2 (rlTopics logEntry) == [operationTopic, T.toLower expectedHash]
      entries = zip parsedLogs logs
  event <- case [value | (logEntry, value) <- entries, matching logEntry] of
    [value] -> parseUserOperationEventAt entryPoint paymaster height height value
    _ -> Left "Kernel UserOperation event is missing or ambiguous"
  unless (uoeSender event == T.toLower expectedSender && uoeNonce event == expectedNonce
    && uoeHash event == T.toLower expectedHash && uoeTransactionHash event == tx && uoeBlockHash event == blockHash) $
    Left "Kernel UserOperation event does not match the accepted operation"
  start <- case reverse [rlIndex logEntry | logEntry <- parsedLogs, rlIndex logEntry < uoeLogIndex event, isBoundary logEntry] of
    index : _ -> Right index
    [] -> Left "Kernel bundle is missing the operation's execution boundary"
  let operationEntries = [(logEntry, value) | (logEntry, value) <- entries, rlIndex logEntry > start, rlIndex logEntry <= uoeLogIndex event]
      rpcValue = object
        [ "userOpHash" .= uoeHash event, "sender" .= uoeSender event, "entryPoint" .= T.toLower entryPoint
        , "nonce" .= canonicalQuantity (uoeNonce event), "paymaster" .= T.toLower paymaster
        , "actualGasCost" .= canonicalQuantity (uoeActualGasCost event)
        , "actualGasUsed" .= canonicalQuantity (uoeActualGasUsed event), "success" .= uoeSuccess event
        , "logs" .= map snd operationEntries, "receipt" .= receipt
        ]
  pure (event, map fst operationEntries, rpcValue)
 where
  operationTopic = hashBytes "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)"
  executionTopic = hashBytes "BeforeExecution()"

parseLog :: Text -> Integer -> Text -> Value -> Either Text ReceiptLog
parseLog tx height blockHash value = do
  logTx <- fixedText 32 "transactionHash" value
  logHeight <- quantityMember "blockNumber" value
  logBlockHash <- fixedText 32 "blockHash" value
  unless (tx == logTx && height == logHeight && blockHash == logBlockHash) $
    Left "Kernel bundle log locator does not match its transaction receipt"
  case value of
    Object fields | KM.lookup "removed" fields `elem` [Nothing, Just (Bool False)] -> Right ()
    _ -> Left "Kernel bundle contains a removed or malformed log"
  index <- quantityMember "logIndex" value
  address <- fixedText 20 "address" value
  topics <- arrayMember "topics" value >>= traverse (hexString 32)
  payload <- member "data" value >>= \case
    String raw | even (T.length raw) -> hexString ((T.length raw - 2) `div` 2) (String raw)
    _ -> Left "Kernel bundle contains malformed log data"
  pure $ ReceiptLog index address topics payload

member :: Text -> Value -> Either Text Value
member key (Object fields) = maybe (Left $ "missing Kernel receipt field " <> key) Right $ KM.lookup (K.fromText key) fields
member _ _ = Left "malformed Kernel receipt object"

fixedText :: Int -> Text -> Value -> Either Text Text
fixedText size key value = member key value >>= hexString size

hexString :: Int -> Value -> Either Text Text
hexString size (String raw) | isHexBytes size (T.toLower raw) = Right $ T.toLower raw
hexString _ _ = Left "malformed hex in Kernel receipt"

quantityMember :: Text -> Value -> Either Text Integer
quantityMember key value = member key value >>= \case
  String raw -> case T.stripPrefix "0x" $ T.toLower raw of
    Just digits | T.length digits <= 64 -> case readHex $ T.unpack digits of
      [(n, "")] | canonicalQuantity n == T.toLower raw -> Right n
      _ -> invalid
    _ -> invalid
  _ -> invalid
 where invalid = Left $ "invalid Kernel receipt quantity " <> key

arrayMember :: Text -> Value -> Either Text [Value]
arrayMember key value = member key value >>= \case
  Array values -> Right $ V.toList values
  _ -> Left $ "invalid Kernel receipt array " <> key
