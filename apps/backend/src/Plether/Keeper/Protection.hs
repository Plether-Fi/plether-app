-- | Durable close-attempt discovery and conservative, permissionless retry.
-- Ordinary execution/expiry cleanup remains in the order keeper. This job only
-- appends an attempt after cleanup has completed and the FIFO is empty.
module Plether.Keeper.Protection
  ( Protection (..)
  , ProtectionEvent (..)
  , decodeProtection
  , decodeProtectionEvent
  , protectionTopics
  , assessRetry
  , retryOracleReady
  , recordProtectionEvent
  , ensureProtectionSchema
  , discoverProtectionBook
  , processProtectionRetries
  ) where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query, withTransaction)
import Plether.Config (Config (..))
import Plether.Ethereum.Abi (decodeAddress, decodeUint256, encodeAddress, encodeCall, encodeUint256, keccak256)
import Plether.Ethereum.Client (EthClient, CallParams (..), ethBlockNumber, ethCallAtBlock)
import Plether.Ethereum.Rpc (RpcLog (..), RpcBlock (..), TxReceipt (..), ethGetLogsForAddresses, ethGetBlockByNumber)
import Plether.Logging (field, logInfo, logWarnEvery)

-- Enum values are the canonical PR #78 ABI ordinals.
data Protection = Protection
  { protectionId :: Integer
  , linkedOrderId :: Integer
  , protectionAccount :: Text
  , protectionSide :: Integer
  , protectionSize :: Integer
  , retainedBounty :: Integer
  , triggerTime :: Integer
  , protectionStatus :: Integer
  } deriving stock (Show, Eq)

data ProtectionEvent
  = AttemptQueued Integer Text Integer Integer
  | AttemptFailed Integer Text Integer Integer Bool
  | AttemptRegistered Integer
  deriving stock (Show, Eq)

word :: Int -> ByteString -> Integer
word n = decodeUint256 . BS.take 32 . BS.drop (n * 32)

wordBytes :: Int -> ByteString -> ByteString
wordBytes n = BS.take 32 . BS.drop (n * 32)

hex :: ByteString -> Text
hex = ("0x" <>) . TE.decodeUtf8 . B16.encode

protectionTopics :: [ByteString]
protectionTopics = map (keccak256 . TE.encodeUtf8)
  [ "PositionProtectionCloseAttemptQueued(uint64,address,uint64,uint64)"
  , "PositionProtectionCloseAttemptFailed(uint64,address,uint64,uint8,bool)"
  , "ProtectionAttemptRegistered(uint64)"
  ]

decodeProtection :: ByteString -> Either Text Protection
decodeProtection bytes
  | BS.length bytes /= 16 * 32 = Left "Invalid protection view ABI length"
  | any (> 2 ^ (64 :: Int) - 1) [word 0 bytes, word 2 bytes, word 13 bytes]
      || word 4 bytes > 1 || word 15 bytes > 8 = Left "Invalid protection view enum or ID"
  | otherwise = Right Protection
      { protectionId = word 0 bytes, linkedOrderId = word 2 bytes
      , protectionAccount = decodeAddress $ wordBytes 3 bytes
      , protectionSide = word 4 bytes, protectionSize = word 5 bytes
      , retainedBounty = word 9 bytes, triggerTime = word 13 bytes
      , protectionStatus = word 15 bytes
      }

decodeProtectionEvent :: RpcLog -> Maybe ProtectionEvent
decodeProtectionEvent RpcLog {..}
  | any ((/= 32) . BS.length) rpcLogTopics = Nothing
  | otherwise = case rpcLogTopics of
      [topic, pid, account, oid]
        | topic == protectionTopics !! 0 && BS.length rpcLogData == 32 ->
            Just $ AttemptQueued (decodeUint256 pid) (decodeAddress account) (decodeUint256 oid) (word 0 rpcLogData)
        | topic == protectionTopics !! 1 && BS.length rpcLogData == 64
        , word 0 rpcLogData <= 9, word 1 rpcLogData <= 1 ->
            Just $ AttemptFailed (decodeUint256 pid) (decodeAddress account) (decodeUint256 oid) (word 0 rpcLogData) (word 1 rpcLogData == 1)
      [topic, oid] | topic == protectionTopics !! 2 && BS.null rpcLogData ->
        Just $ AttemptRegistered (decodeUint256 oid)
      _ -> Nothing

-- | Empty-FIFO admission is deliberately conservative: no queue throughput is
-- assumed. The remaining budget includes keeper polling, with a 15s reserve.
assessRetry :: Protection -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool -> Either Text ()
assessRetry protection reason bountyDisposition positionSide positionSize pendingCount queueTail maxAge projectedSeconds oracleReady
  | protectionStatus protection /= 8 = Left "Protection is not latched"
  | reason /= 2 = Left "Latest attempt requires operator remediation"
  | bountyDisposition /= 4 || retainedBounty protection <= 0 = Left "Close bounty is not retained"
  | protectionSize protection == 0 || positionSize /= protectionSize protection || positionSide /= protectionSide protection = Left "Protected position no longer matches"
  | pendingCount /= 0 = Left "Account has a pending order"
  | queueTail /= 0 = Left "Waiting for an empty FIFO after separate expiry cleanup"
  | projectedSeconds < 0 || projectedSeconds > maxAge - 15 = Left "Projected head arrival exceeds retry budget"
  | not oracleReady = Left "Waiting for executable oracle data"
  | otherwise = Right ()

retryOracleReady :: Integer -> Integer -> Integer -> [Integer] -> [ByteString] -> Bool
retryOracleReady now maxStaleness maxDivergence publishTimes updateData =
  length publishTimes == 6 && not (null updateData) && all (not . BS.null) updateData
    && maxStaleness > 0 && maxDivergence >= 0
    && all (\t -> t > 0 && t <= now && now - t <= maxStaleness) publishTimes
    && maximum publishTimes - minimum publishTimes <= maxDivergence

ensureProtectionSchema :: Connection -> IO ()
ensureProtectionSchema conn = do
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS perps_protection_indexer_state (book TEXT PRIMARY KEY, last_block BIGINT NOT NULL, last_block_hash TEXT NOT NULL)"
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS perps_protection_attempt_events (book TEXT NOT NULL, tx_hash TEXT NOT NULL, log_index BIGINT NOT NULL, block_number BIGINT NOT NULL, block_hash TEXT NOT NULL, event_kind TEXT NOT NULL, protection_id BIGINT, account TEXT, order_id BIGINT NOT NULL, previous_order_id BIGINT, terminal_reason INTEGER, relatched BOOLEAN, PRIMARY KEY (book, tx_hash, log_index))"
  void $ execute_ conn "CREATE INDEX IF NOT EXISTS perps_protection_attempt_history ON perps_protection_attempt_events (book, protection_id, order_id)"
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS perps_protection_retry_candidates (book TEXT NOT NULL, protection_id BIGINT NOT NULL, checked_at TIMESTAMPTZ, retry_after TIMESTAMPTZ, PRIMARY KEY (book, protection_id))"

readAt :: EthClient -> Integer -> Text -> Text -> [ByteString] -> Int -> ExceptT Text IO ByteString
readAt client block target signature args expectedWords = do
  result <- liftIO $ ethCallAtBlock client (CallParams target $ encodeCall signature args) block
  bytes <- either (throwE . T.pack . show) pure result
  unless (BS.length bytes == expectedWords * 32) $ throwE $ "Invalid ABI response: " <> signature
  pure bytes

-- | No V2 fallback: bind the complete graph and V3 hash domains before any
-- retry. All reads use the same block, including the Book discovered by Router.
discoverProtectionBook :: Config -> EthClient -> IO (Either Text Text)
discoverProtectionBook cfg client = runExceptT $ do
  block <- ExceptT $ fmap (either (Left . T.pack . show) Right) $ ethBlockNumber client
  lifecycle <- maybe (throwE "PERPS_ORDER_LIFECYCLE_BOOK is required") pure $ cfgPerpsOrderLifecycleBook cfg
  let readContract = readAt client block
      address target sig = decodeAddress <$> readContract target sig [] 1
      same label actual expected = unless (T.toLower actual == T.toLower expected) $ throwE $ label <> " binding mismatch"
  schema <- readContract lifecycle "CONFIG_SCHEMA_HASH()" [] 1
  receipt <- readContract lifecycle "RECEIPT_TYPEHASH()" [] 1
  unless (schema == keccak256 "PletherExecutionConfigV3" && receipt == keccak256 "PletherOrderReceiptV3(uint256 chainId,address book,address router,uint64 terminalBlock,uint64 terminalTime,OrderReceipt receipt)") $
    throwE "V3 receipt/config schemas required; deploy a fresh complete perps stack"
  address (cfgPerpsOrderRouter cfg) "lifecycleBook()" >>= \a -> same "Router lifecycle" a lifecycle
  address lifecycle "ROUTER()" >>= \a -> same "Lifecycle Router" a (cfgPerpsOrderRouter cfg)
  address (cfgPerpsOrderRouter cfg) "engine()" >>= \a -> same "Router Engine" a (cfgPerpsCfdEngine cfg)
  address lifecycle "ENGINE()" >>= \a -> same "Lifecycle Engine" a (cfgPerpsCfdEngine cfg)
  book <- address (cfgPerpsOrderRouter cfg) "positionProtectionBook()"
  address book "ROUTER()" >>= \a -> same "Protection Router" a (cfgPerpsOrderRouter cfg)
  address book "ENGINE()" >>= \a -> same "Protection Engine" a (cfgPerpsCfdEngine cfg)
  pure book

indexAttempts :: Config -> Connection -> EthClient -> Text -> ExceptT Text IO ()
indexAttempts cfg conn client book = do
  latest <- ExceptT $ fmap (either (Left . T.pack . show) Right) $ ethBlockNumber client
  lifecycle <- maybe (throwE "Lifecycle Book missing") pure $ cfgPerpsOrderLifecycleBook cfg
  previous <- liftIO $ query conn "SELECT last_block, last_block_hash FROM perps_protection_indexer_state WHERE book = ?" (Only book)
  -- Rebuild this small, deployment-scoped journal on a confirmed reorg. Replays
  -- are idempotent and retries still require authoritative current chain state.
  cursor <- case previous of
    [(n, blockHash)] -> do
      canonical <- ExceptT $ fmap (either (Left . T.pack . show) Right) $ ethGetBlockByNumber client n
      if rpcBlockHash canonical == blockHash then pure (Just n) else do
        liftIO $ withTransaction conn $ do
          void $ execute conn "DELETE FROM perps_protection_attempt_events WHERE book = ?" (Only book)
          void $ execute conn "DELETE FROM perps_protection_retry_candidates WHERE book = ?" (Only book)
          void $ execute conn "DELETE FROM perps_protection_indexer_state WHERE book = ?" (Only book)
        pure Nothing
    _ -> pure Nothing
  let start = max (cfgPerpsIndexerStartBlock cfg) $ case cursor of
        Just n -> n + 1
        _ -> cfgPerpsIndexerStartBlock cfg
      end = min (latest - fromIntegral (cfgKeeperConfirmations cfg)) (start + 1999)
  when (start <= end) $ do
    logs <- ExceptT $ fmap (either (Left . T.pack . show) Right) $
      ethGetLogsForAddresses client [book, lifecycle] protectionTopics start end
    -- A malformed matching event must not be skipped while advancing the cursor.
    events <- mapM (decodeScoped lifecycle) $ sortOn (\l -> (rpcLogBlockNumber l, rpcLogTransactionIndex l, rpcLogIndex l)) logs
    endBlock <- ExceptT $ fmap (either (Left . T.pack . show) Right) $ ethGetBlockByNumber client end
    liftIO $ withTransaction conn $ do
      forM_ events $ \(logEntry, event) -> recordProtectionEvent conn book logEntry event
      void $ execute conn "INSERT INTO perps_protection_indexer_state (book, last_block, last_block_hash) VALUES (?, ?, ?) ON CONFLICT (book) DO UPDATE SET last_block = EXCLUDED.last_block, last_block_hash = EXCLUDED.last_block_hash" (book, end, rpcBlockHash endBlock)
 where
  decodeScoped lifecycle logEntry = case decodeProtectionEvent logEntry of
    Just event
      | T.toLower (rpcLogAddress logEntry) == T.toLower (case event of AttemptRegistered _ -> lifecycle; _ -> book) -> pure (logEntry, event)
    _ -> throwE "Invalid protection event or emitter"

recordProtectionEvent :: Connection -> Text -> RpcLog -> ProtectionEvent -> IO ()
recordProtectionEvent conn book logEntry event = do
  let (kind, pid, account, oid, previous, reason, relatched) = case event of
        AttemptQueued p a o prev -> ("queued" :: Text, Just p, Just a, o, Just prev, Nothing, Nothing)
        AttemptFailed p a o r latched -> ("failed", Just p, Just a, o, Nothing, Just r, Just latched)
        AttemptRegistered o -> ("registered", Nothing, Nothing, o, Nothing, Nothing, Nothing)
  -- Split common provenance and event data to stay within tuple ToRow limits.
  void $ execute conn "INSERT INTO perps_protection_attempt_events (book, tx_hash, log_index, block_number, block_hash, event_kind, protection_id, account, order_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"
    (book, rpcLogTxHash logEntry, rpcLogIndex logEntry, rpcLogBlockNumber logEntry, rpcLogBlockHash logEntry, kind, pid, account, oid)
  void $ execute conn "UPDATE perps_protection_attempt_events SET previous_order_id = ?, terminal_reason = ?, relatched = ? WHERE book = ? AND tx_hash = ? AND log_index = ?"
    (previous, reason, relatched, book, rpcLogTxHash logEntry, rpcLogIndex logEntry)
  case event of
    AttemptFailed p _ _ _ True -> void $ execute conn "INSERT INTO perps_protection_retry_candidates (book, protection_id) VALUES (?, ?) ON CONFLICT DO NOTHING" (book, p)
    _ -> pure ()

-- The caller provides the existing oracle selection and signer/receipt path so
-- retry and ordinary orders share one serialized nonce owner and advisory lock.
processProtectionRetries
  :: Config -> Connection -> EthClient -> Text -> Bool
  -> IO Bool
  -> (Text -> ByteString -> IO (Either Text TxReceipt))
  -> IO Bool
processProtectionRetries cfg conn client book dryRun oracleAvailable submit = do
  indexed <- runExceptT $ indexAttempts cfg conn client book
  case indexed of
    Left err -> warn "protection_index_failed" err [] >> pure False
    Right () -> do
      candidates <- query conn "SELECT protection_id FROM perps_protection_retry_candidates WHERE book = ? AND (retry_after IS NULL OR retry_after <= NOW()) ORDER BY checked_at NULLS FIRST, protection_id LIMIT 16" (Only book)
      submitted <- go candidates
      pure $ submitted || not (null candidates)
 where
  warn key message fields = logWarnEvery 60 key message fields
  go [] = pure False
  go (Only pid : rest) = do
    void $ execute conn "UPDATE perps_protection_retry_candidates SET checked_at = NOW() WHERE book = ? AND protection_id = ?" (book, pid)
    result <- runExceptT $ do
      block <- ExceptT $ fmap (either (Left . T.pack . show) Right) $ ethBlockNumber client
      let readContract = readAt client block
      bytes <- readContract book "getPositionProtection(uint64)" [encodeUint256 pid] 16
      protection <- either throwE pure $ decodeProtection bytes
      if protectionStatus protection /= 8 then do
        liftIO $ void $ execute conn "DELETE FROM perps_protection_retry_candidates WHERE book = ? AND protection_id = ?" (book, pid)
        pure Nothing
      else do
        lifecycle <- maybe (throwE "Lifecycle Book missing") pure $ cfgPerpsOrderLifecycleBook cfg
        outcome <- readContract lifecycle "outcome(uint64)" [encodeUint256 $ linkedOrderId protection] 23
        -- The retained receipt belongs to this exact account and bounty. The
        -- transient marker is already consumed; persistent event evidence stays.
        unless (word 5 outcome == 3 && decodeAddress (wordBytes 0 outcome) == protectionAccount protection
          && word 16 outcome == retainedBounty protection && word 14 outcome == 0) $
          throwE "Latest attempt does not authenticate the retained bounty"
        position <- readContract (cfgPerpsCfdEngine cfg) "positions(address)" [encodeAddress $ protectionAccount protection] 7
        active <- readContract book "activePositionProtectionId(address)" [encodeAddress $ protectionAccount protection] 1
        unless (word 0 active == pid) $ throwE "Protection is no longer active"
        pending <- readContract (cfgPerpsOrderRouter cfg) "pendingOrderCounts(address)" [encodeAddress $ protectionAccount protection] 1
        tailId <- readContract (cfgPerpsOrderRouter cfg) "globalTailOrderId()" [] 1
        ttl <- readContract (cfgPerpsOrderRouter cfg) "maxOrderAge()" [] 1
        ready <- liftIO oracleAvailable
        observed <- ExceptT $ fmap (either (Left . T.pack . show) Right) $ ethGetBlockByNumber client block
        counts <- liftIO $ query conn "SELECT COUNT(*) FROM perps_protection_attempt_events WHERE book = ? AND protection_id = ? AND event_kind = 'queued'" (book, pid)
        let attemptCount = case counts of [Only n] -> n :: Integer; _ -> 0
        let projected = fromIntegral $ max 1 (cfgKeeperPollSeconds cfg)
            decision = assessRetry protection (word 6 outcome) (word 9 outcome) (word 4 position) (word 0 position) (word 0 pending) (word 0 tailId) (word 0 ttl) projected ready
            fields = [field "protection_id" pid, field "latest_order_id" $ linkedOrderId protection,
              field "terminal_reason" $ word 6 outcome, field "failure_selector" $ hex $ wordBytes 17 outcome,
              field "failed_constraint" $ word 20 outcome, field "revert_data_hash" $ hex $ wordBytes 21 outcome,
              field "queue_tail" $ word 0 tailId, field "projected_head_arrival_seconds" projected,
              field "attempt_count" attemptCount, field "latched_age_seconds" $ max 0 (rpcBlockTimestamp observed - word 11 outcome),
              field "trigger_publish_time" $ triggerTime protection]
        case decision of
          Left reason -> liftIO (warn ("protection_retry_held_" <> T.pack (show pid) <> "_" <> T.pack (show $ word 6 outcome) <> "_" <> hex (wordBytes 21 outcome)) reason fields) >> pure Nothing
          Right () -> pure $ Just (protection, fields)
    case result of
      Left err -> warn "protection_retry_read_failed" err [field "protection_id" pid] >> go rest
      Right Nothing -> go rest
      Right (Just (_, fields)) | dryRun -> logInfo "protection_retry_dry_run" "Protection close retry is eligible" fields >> pure True
      Right (Just (_, fields)) -> do
        -- Persist backoff before broadcasting: a lost response or receipt timeout
        -- must not turn the next polling cycle into another transaction attempt.
        void $ execute conn "UPDATE perps_protection_retry_candidates SET retry_after = NOW() + INTERVAL '60 seconds' WHERE book = ? AND protection_id = ?" (book, pid)
        sent <- submit book $ encodeCall "retryPositionProtectionClose(uint64)" [encodeUint256 pid]
        case sent of
          Left err -> warn "protection_retry_submission_failed" err fields
          Right receipt -> do
            -- Keep the candidate until the next canonical read: a permissionless
            -- race or a timeout must never discard durable intent.
            logInfo "protection_retry_mined" "Protection retry transaction mined"
              (field "transaction_hash" (receiptTxHash receipt) : field "succeeded" (receiptSucceeded receipt) : fields)
        -- One submission per cycle; remaining candidates rotate fairly.
        pure True
