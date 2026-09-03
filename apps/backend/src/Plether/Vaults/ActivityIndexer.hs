module Plether.Vaults.ActivityIndexer
  ( VaultActivityIndexerConfig (..)
  , VaultTransfer (..)
  , ParsedVaultEvent (..)
  , VaultActivityCycleResult (..)
  , VaultActivityCycleStats (..)
  , transferTopic
  , depositRequestTopic
  , redeemRequestTopic
  , legacyDepositRequestedTopic
  , parseVaultLog
  , isProviderLogRangeLimit
  , runVaultActivityIndexerCycle
  , startVaultActivityIndexer
  , verifyVaultActivityBindings
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , finally
  , fromException
  , throwIO
  , try
  )
import Control.Monad (forM, forM_, forever, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (withTransaction)
import Plether.Database (DbPool, withDb)
import Plether.Database.VaultActivity
  ( VaultActivityDeployment (..)
  , VaultActivityIndexerStateRow (..)
  , VaultRequestRow (..)
  , countVaultHolders
  , countVaultEvents
  , countVaultRequests
  , getVaultActivityIndexerState
  , insertVaultLogIdentityExact
  , insertVaultRequestExact
  , insertVaultShareTransferExact
  , recomputeVaultHolderBalance
  , resetVaultActivityDeployment
  , setVaultActivityIndexerState
  , tryLockVaultActivityIndexer
  , unlockVaultActivityIndexer
  )
import Plether.Ethereum.Abi (decodeAddress, decodeUint256, encodeCall)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError (..), ethCall)
import Plether.Ethereum.Rpc
  ( RpcBlock (..)
  , RpcLog (..)
  , ethChainId
  , ethGetBlockByNumber
  , ethGetCode
  , ethGetLogs
  , ethLatestBlock
  )
import Plether.Indexer.Contracts (keccak256Text)
import Plether.Logging (field, logErrorEvery, logInfo, logInfoEvery, logWarn)

data VaultActivityIndexerConfig = VaultActivityIndexerConfig
  { vaicDeployment :: VaultActivityDeployment
  , vaicAssetAddress :: Text
  , vaicPublicLensAddress :: Text
  , vaicConfirmations :: Integer
  , vaicBatchSize :: Integer
  , vaicPollIntervalMicros :: Int
  }
  deriving stock (Eq, Show)

data ParsedVaultEvent
  = ParsedVaultTransfer VaultTransfer
  | ParsedVaultRequest VaultRequestRow
  deriving stock (Eq, Show)

data VaultTransfer = VaultTransfer
  { vtVaultAddress :: Text
  , vtFromAddress :: Text
  , vtToAddress :: Text
  , vtAmount :: Integer
  , vtTxHash :: Text
  , vtBlockNumber :: Integer
  , vtBlockHash :: Text
  , vtTxIndex :: Integer
  , vtLogIndex :: Integer
  , vtTimestamp :: Integer
  }
  deriving stock (Eq, Show)

data VaultActivityCycleResult
  = VaultActivityCycleLeaderBusy
  | VaultActivityCycleCompleted VaultActivityCycleStats
  deriving stock (Eq, Show)

data VaultActivityCycleStats = VaultActivityCycleStats
  { vacrIndexedThrough :: Integer
  , vacrSafeHead :: Integer
  , vacrBackfillComplete :: Bool
  , vacrEventCount :: Int
  , vacrEventsIngested :: Int
  , vacrLagSeconds :: Integer
  , vacrSeniorHolderCount :: Integer
  , vacrJuniorHolderCount :: Integer
  , vacrSeniorRequestCount :: Integer
  , vacrJuniorRequestCount :: Integer
  , vacrLastSuccess :: Integer
  }
  deriving stock (Eq, Show)

transferTopic, depositRequestTopic, redeemRequestTopic, legacyDepositRequestedTopic :: BS.ByteString
transferTopic = keccak256Text "Transfer(address,address,uint256)"
depositRequestTopic = keccak256Text "DepositRequest(address,address,uint256,address,uint256)"
redeemRequestTopic = keccak256Text "RedeemRequest(address,address,uint256,address,uint256)"
legacyDepositRequestedTopic = keccak256Text "DepositRequested(address,address,uint256,uint256)"

vaultTopics :: [BS.ByteString]
vaultTopics =
  [ transferTopic
  , depositRequestTopic
  , redeemRequestTopic
  , legacyDepositRequestedTopic
  ]

startVaultActivityIndexer :: EthClient -> DbPool -> VaultActivityIndexerConfig -> IO ()
startVaultActivityIndexer client pool cfg =
  forever $ do
    result <- try @SomeException $ runVaultActivityIndexerCycle client pool cfg
    case result of
      Left err ->
        case fromException err :: Maybe SomeAsyncException of
          Just _ -> throwIO err
          Nothing ->
            logErrorEvery
              60
              "vault_activity_indexer_iteration_failed"
              "Vault activity indexer iteration failed"
              [field "error" $ show err]
      Right VaultActivityCycleLeaderBusy -> pure ()
      Right (VaultActivityCycleCompleted completed) ->
        logInfoEvery
          60
          "vault_activity_indexer_heartbeat"
          "Vault activity indexer completed a canonical poll"
          [ field "state" $ if vacrBackfillComplete completed then ("ready" :: Text) else "backfilling"
          , field "last_indexed_block" $ vacrIndexedThrough completed
          , field "safe_head_block" $ vacrSafeHead completed
          , field "lag_blocks" $ max 0 $ vacrSafeHead completed - vacrIndexedThrough completed
          , field "lag_seconds" $ vacrLagSeconds completed
          , field "event_count" $ vacrEventCount completed
          , field "events_ingested" $ vacrEventsIngested completed
          , field "senior_holder_count" $ vacrSeniorHolderCount completed
          , field "junior_holder_count" $ vacrJuniorHolderCount completed
          , field "senior_request_count" $ vacrSeniorRequestCount completed
          , field "junior_request_count" $ vacrJuniorRequestCount completed
          , field "last_success" $ vacrLastSuccess completed
          ]
    threadDelay $ max 1_000_000 $ vaicPollIntervalMicros cfg

verifyVaultActivityBindings :: EthClient -> VaultActivityIndexerConfig -> IO ()
verifyVaultActivityBindings client cfg = do
  chainId <- rpcOrFail "read vault activity chain ID" $ ethChainId client
  unless (chainId == 421614) $
    fail "Vault activity indexer is reviewed only for Arbitrum Sepolia chain ID 421614"
  unless (chainId == vadChainId deployment) $
    fail "Vault activity RPC chain ID does not match configuration"
  when (normalize (vadSeniorVault deployment) == normalize (vadJuniorVault deployment)) $
    fail "Vault activity Senior and Junior vault addresses must differ"
  unless (vaicConfirmations cfg == 12) $
    fail "Vault activity indexer requires the reviewed 12-confirmation depth"
  forM_ [vadHousePool deployment, vadSeniorVault deployment, vadJuniorVault deployment, vaicPublicLensAddress cfg] $ \contract -> do
    bytecode <- rpcOrFail "read vault activity contract bytecode" $ ethGetCode client contract
    when (BS.null bytecode) $ fail "Vault activity configured contract has no deployed bytecode"
  requireAddressBinding
    "HousePool seniorVault()"
    (vadHousePool deployment)
    "seniorVault()"
    (vadSeniorVault deployment)
  requireAddressBinding
    "HousePool juniorVault()"
    (vadHousePool deployment)
    "juniorVault()"
    (vadJuniorVault deployment)
  requireAddressBinding
    "Public Lens HOUSE_POOL()"
    (vaicPublicLensAddress cfg)
    "HOUSE_POOL()"
    (vadHousePool deployment)
  forM_ [vadSeniorVault deployment, vadJuniorVault deployment] $ \vault -> do
    requireAddressBinding "vault POOL()" vault "POOL()" $ vadHousePool deployment
    requireAddressBinding "vault asset()" vault "asset()" $ vaicAssetAddress cfg
    decimalsResult <- rpcOrFail "read vault activity share decimals" $
      ethCall client CallParams {callTo = vault, callData = encodeCall "decimals()" []}
    unless (BS.length decimalsResult == 32 && decodeUint256 decimalsResult == 9) $
      fail "Vault activity configured vault does not use the reviewed 9 share decimals"
  logInfo
    "vault_activity_bindings_verified"
    "Vault activity chain and deployed bytecode bindings are verified"
    [ field "chain_id" chainId
    , field "house_pool" $ vadHousePool deployment
    , field "senior_vault" $ vadSeniorVault deployment
    , field "junior_vault" $ vadJuniorVault deployment
    , field "asset" $ vaicAssetAddress cfg
    , field "public_lens" $ vaicPublicLensAddress cfg
    ]
 where
  deployment = vaicDeployment cfg
  requireAddressBinding label contract signature expected = do
    result <- rpcOrFail ("read " <> label) $
      ethCall client CallParams {callTo = contract, callData = encodeCall signature []}
    unless
      ( BS.length result == 32
          && BS.take 12 result == BS.replicate 12 0
          && normalize (decodeAddress result) == normalize expected
      ) $
      fail $ label <> " does not match the configured deployment"

runVaultActivityIndexerCycle
  :: EthClient
  -> DbPool
  -> VaultActivityIndexerConfig
  -> IO VaultActivityCycleResult
runVaultActivityIndexerCycle client pool cfg =
  withDb pool $ \conn -> do
    locked <- tryLockVaultActivityIndexer conn
    if not locked
      then pure VaultActivityCycleLeaderBusy
      else runWithLock conn `finally` unlockVaultActivityIndexer conn
 where
  deployment = vaicDeployment cfg
  runWithLock conn = do
    latest <- rpcOrFail "read vault activity head" $ ethLatestBlock client
    let safeNumber = rpcBlockNumber latest - max 0 (vaicConfirmations cfg)
    when (safeNumber < vadDeploymentBlock deployment) $
      fail "Confirmed vault activity head predates deployment"
    safeBlock <- rpcOrFail "read vault activity safe head" $ ethGetBlockByNumber client safeNumber
    state <- getVaultActivityIndexerState conn deployment
    stateAfterReorg <- reconcileCursor conn state
    forM_ stateAfterReorg $ \current ->
      when (vaisLastIndexedBlock current > safeNumber) $
        fail "Vault activity safe head moved behind the persisted cursor"
    let initialCursor = max 0 $ vadDeploymentBlock deployment - 1
        indexedBlock = maybe initialCursor vaisLastIndexedBlock stateAfterReorg
        startBlock = max (vadDeploymentBlock deployment) (indexedBlock + 1)
        endBlock = min safeNumber $ startBlock + max 1 (vaicBatchSize cfg) - 1
    if startBlock > endBlock
      then do
        withTransaction conn $
          setVaultActivityIndexerState
            conn deployment indexedBlock (stateAfterReorg >>= vaisLastIndexedBlockHash)
            (maybe 0 vaisLastIndexedBlockTimestamp stateAfterReorg)
            safeNumber (rpcBlockHash safeBlock) (rpcBlockTimestamp safeBlock) True
        emitCounts conn indexedBlock safeNumber True 0
      else do
        endBefore <- rpcOrFail "read vault activity range end" $ ethGetBlockByNumber client endBlock
        seniorLogs <- fetchLogsAdaptive client (vadSeniorVault deployment) startBlock endBlock
        juniorLogs <- fetchLogsAdaptive client (vadJuniorVault deployment) startBlock endBlock
        let orderedLogs = sortOn logPosition $ seniorLogs <> juniorLogs
            blockNumbers = Set.toAscList $ Set.fromList $ map rpcLogBlockNumber orderedLogs
        blocks <- forM blockNumbers $ \number -> do
          block <- rpcOrFail "read vault activity log block" $ ethGetBlockByNumber client number
          pure (number, block)
        let blocksByNumber = Map.fromList blocks
        parsed <- forM orderedLogs $ \entry -> do
          block <- maybe (fail "Missing canonical block for vault log") pure $
            Map.lookup (rpcLogBlockNumber entry) blocksByNumber
          unless
            (T.toLower (rpcLogBlockHash entry) == T.toLower (rpcBlockHash block))
            (fail "Vault log block hash does not match canonical block")
          either (fail . T.unpack) pure $ parseVaultLog deployment (rpcBlockTimestamp block) entry
        endAfter <- rpcOrFail "re-read vault activity range end" $ ethGetBlockByNumber client endBlock
        unless (sameBlock endBefore endAfter) $
          fail "Vault activity range end changed during observation"
        let complete = endBlock >= safeNumber
        withTransaction conn $ do
          current <- getVaultActivityIndexerState conn deployment
          unless (current == stateAfterReorg) $
            fail "Vault activity cursor changed before commit"
          affected <- persistEvents conn parsed
          forM_ (Set.toAscList affected) $ \(vault, holder) ->
            recomputeVaultHolderBalance conn deployment vault holder
          setVaultActivityIndexerState
            conn deployment endBlock (Just $ rpcBlockHash endAfter)
            (rpcBlockTimestamp endAfter)
            safeNumber (rpcBlockHash safeBlock) (rpcBlockTimestamp safeBlock) complete
        emitCounts conn endBlock safeNumber complete $ length parsed

  reconcileCursor _ Nothing = pure Nothing
  reconcileCursor conn state@(Just row)
    | vaisLastIndexedBlock row < vadDeploymentBlock deployment - 1 =
        fail "Vault activity cursor has a gap before the deployment boundary"
    | vaisLastIndexedBlock row < vadDeploymentBlock deployment = pure state
    | otherwise =
        case vaisLastIndexedBlockHash row of
          Nothing -> fail "Vault activity cursor is missing its canonical block hash"
          Just storedHash -> do
            canonical <- rpcOrFail "verify vault activity cursor" $
              ethGetBlockByNumber client $ vaisLastIndexedBlock row
            if T.toLower storedHash == T.toLower (rpcBlockHash canonical)
              then pure state
              else do
                logWarn
                  "vault_activity_indexer_reorg_detected"
                  "Vault activity cursor changed; rebuilding the configured deployment"
                  [ field "mismatch_block" $ vaisLastIndexedBlock row
                  , field "deployment_block" $ vadDeploymentBlock deployment
                  ]
                withTransaction conn $ resetVaultActivityDeployment conn deployment
                pure Nothing

  persistEvents conn events =
    foldlM (persistEvent conn) Set.empty events

  persistEvent conn affected = \case
    ParsedVaultTransfer transfer -> do
      insertVaultLogIdentityExact
        conn deployment (vtVaultAddress transfer) "Transfer" (vtTxHash transfer)
        (vtBlockNumber transfer) (vtBlockHash transfer) (vtTxIndex transfer)
        (vtLogIndex transfer) (vtTimestamp transfer)
      insertVaultShareTransferExact
        conn deployment (vtVaultAddress transfer) (vtFromAddress transfer) (vtToAddress transfer) (vtAmount transfer)
        (vtTxHash transfer) (vtBlockNumber transfer) (vtBlockHash transfer)
        (vtTxIndex transfer) (vtLogIndex transfer) (vtTimestamp transfer)
      pure $
        Set.insert (normalize $ vtVaultAddress transfer, normalize $ vtToAddress transfer) $
          Set.insert (normalize $ vtVaultAddress transfer, normalize $ vtFromAddress transfer) affected
    ParsedVaultRequest row -> do
      insertVaultLogIdentityExact
        conn deployment (vrrVaultAddress row) (vrrEventName row) (vrrTxHash row)
        (vrrBlockNumber row) (vrrBlockHash row) (vrrTxIndex row)
        (vrrLogIndex row) (vrrTimestamp row)
      insertVaultRequestExact conn deployment row
      pure affected

  emitCounts conn indexed safe complete eventCount = do
    totalEvents <- countVaultEvents conn deployment
    seniorHolders <- countVaultHolders conn deployment $ vadSeniorVault deployment
    juniorHolders <- countVaultHolders conn deployment $ vadJuniorVault deployment
    seniorRequests <- countVaultRequests conn deployment $ vadSeniorVault deployment
    juniorRequests <- countVaultRequests conn deployment $ vadJuniorVault deployment
    state <- getVaultActivityIndexerState conn deployment
    lastSuccess <- floor <$> getPOSIXTime
    let indexedTimestamp = maybe 0 vaisLastIndexedBlockTimestamp state
        safeTimestamp = maybe 0 vaisSafeHeadTimestamp state
        lagSeconds = max 0 $ safeTimestamp - indexedTimestamp
    logInfoEvery
      60
      "vault_activity_indexer_counts"
      "Vault activity canonical dataset counts"
      [ field "last_indexed_block" indexed
      , field "safe_head_block" safe
      , field "backfill_complete" complete
      , field "event_count" totalEvents
      , field "events_ingested" eventCount
      , field "senior_holder_count" seniorHolders
      , field "junior_holder_count" juniorHolders
      , field "senior_request_count" seniorRequests
      , field "junior_request_count" juniorRequests
      ]
    pure $
      VaultActivityCycleCompleted $
        VaultActivityCycleStats
        { vacrIndexedThrough = indexed
        , vacrSafeHead = safe
        , vacrBackfillComplete = complete
        , vacrEventCount = fromIntegral totalEvents
        , vacrEventsIngested = eventCount
        , vacrLagSeconds = lagSeconds
        , vacrSeniorHolderCount = fromIntegral seniorHolders
        , vacrJuniorHolderCount = fromIntegral juniorHolders
        , vacrSeniorRequestCount = fromIntegral seniorRequests
        , vacrJuniorRequestCount = fromIntegral juniorRequests
        , vacrLastSuccess = lastSuccess
        }

fetchLogsAdaptive :: EthClient -> Text -> Integer -> Integer -> IO [RpcLog]
fetchLogsAdaptive client vault fromBlock toBlock = do
  result <- ethGetLogs client vault vaultTopics fromBlock toBlock
  case result of
    Right entries -> pure entries
    Left err
      | fromBlock < toBlock && isProviderLogRangeLimit err -> do
          let middle = fromBlock + (toBlock - fromBlock) `div` 2
          left <- fetchLogsAdaptive client vault fromBlock middle
          right <- fetchLogsAdaptive client vault (middle + 1) toBlock
          pure $ left <> right
      | otherwise -> fail $ "eth_getLogs failed for vault range: " <> show err

isProviderLogRangeLimit :: RpcError -> Bool
isProviderLogRangeLimit = \case
  RpcNodeError code message details ->
    code == -32005
      || (code `elem` [-32602, -32000, -32600] && mentionsLimit (message <> maybe "" (" " <>) details))
  RpcHttpError message -> "statuscode = 413" `T.isInfixOf` T.toLower message
  RpcJsonError _ -> False
 where
  mentionsLimit raw =
    let message = T.toLower raw
     in any
          (`T.isInfixOf` message)
          [ "block range"
          , "more than 10000"
          , "response size exceeded"
          , "log response size"
          , "too many results"
          , "limit the query"
          , "maximum block range"
          ]

parseVaultLog :: VaultActivityDeployment -> Integer -> RpcLog -> Either Text ParsedVaultEvent
parseVaultLog deployment timestamp entry = do
  let vault = normalize $ rpcLogAddress entry
      allowedVaults = map normalize [vadSeniorVault deployment, vadJuniorVault deployment]
  unless (vault `elem` allowedVaults) $ Left "Vault log emitter is not configured"
  case rpcLogTopics entry of
    topic : indexed
      | topic == transferTopic -> do
          requireShape 2 32 indexed $ rpcLogData entry
          fromAddress <- topicAddress $ indexed !! 0
          toAddress <- topicAddress $ indexed !! 1
          amount <- dataWord 0 $ rpcLogData entry
          pure $
            ParsedVaultTransfer
              VaultTransfer
                { vtVaultAddress = vault
                , vtFromAddress = fromAddress
                , vtToAddress = toAddress
                , vtAmount = amount
                , vtTxHash = normalize $ rpcLogTxHash entry
                , vtBlockNumber = rpcLogBlockNumber entry
                , vtBlockHash = normalize $ rpcLogBlockHash entry
                , vtTxIndex = rpcLogTransactionIndex entry
                , vtLogIndex = rpcLogIndex entry
                , vtTimestamp = timestamp
                }
      | topic == depositRequestTopic -> parseRequest "DepositRequest" indexed
      | topic == redeemRequestTopic -> parseRequest "RedeemRequest" indexed
      | topic == legacyDepositRequestedTopic -> do
          requireShape 3 32 indexed $ rpcLogData entry
          caller <- topicAddress $ indexed !! 0
          owner <- topicAddress $ indexed !! 1
          requestId <- topicInteger $ indexed !! 2
          amount <- dataWord 0 $ rpcLogData entry
          pure $ ParsedVaultRequest $ envelope "DepositRequested" vault caller owner requestId amount
      | otherwise -> Left "Vault log topic is not allowlisted"
    [] -> Left "Vault log has no event topic"
 where
  parseRequest eventName indexed = do
    requireShape 3 64 indexed $ rpcLogData entry
    controller <- topicAddress $ indexed !! 0
    owner <- topicAddress $ indexed !! 1
    requestId <- topicInteger $ indexed !! 2
    _sender <- dataAddress 0 $ rpcLogData entry
    amount <- dataWord 1 $ rpcLogData entry
    pure $ ParsedVaultRequest $ envelope eventName (normalize $ rpcLogAddress entry) controller owner requestId amount

  envelope eventName vault controller owner requestId amount =
    VaultRequestRow
      { vrrEventName = eventName
      , vrrVaultAddress = vault
      , vrrController = controller
      , vrrOwner = owner
      , vrrRequestId = requestId
      , vrrRawAmount = amount
      , vrrTxHash = normalize $ rpcLogTxHash entry
      , vrrBlockNumber = rpcLogBlockNumber entry
      , vrrBlockHash = normalize $ rpcLogBlockHash entry
      , vrrTxIndex = rpcLogTransactionIndex entry
      , vrrLogIndex = rpcLogIndex entry
      , vrrTimestamp = timestamp
      }

requireShape :: Int -> Int -> [BS.ByteString] -> BS.ByteString -> Either Text ()
requireShape indexedCount dataBytes indexed eventData = do
  unless (length indexed == indexedCount) $ Left "Vault event indexed topic count is invalid"
  unless (BS.length eventData == dataBytes) $ Left "Vault event data length is invalid"

topicAddress :: BS.ByteString -> Either Text Text
topicAddress topic
  | BS.length topic /= 32 = Left "Vault address topic must contain 32 bytes"
  | BS.take 12 topic /= BS.replicate 12 0 = Left "Vault address topic has non-canonical padding"
  | otherwise = Right $ "0x" <> TE.decodeUtf8 (B16.encode $ BS.drop 12 topic)

dataAddress :: Int -> BS.ByteString -> Either Text Text
dataAddress index payload = do
  let offset = index * 32
  unless (BS.length payload >= offset + 32) $ Left "Vault event address word is missing"
  topicAddress $ BS.take 32 $ BS.drop offset payload

topicInteger :: BS.ByteString -> Either Text Integer
topicInteger topic
  | BS.length topic /= 32 = Left "Vault integer topic must contain 32 bytes"
  | otherwise = Right $ bytesInteger topic

dataWord :: Int -> BS.ByteString -> Either Text Integer
dataWord index payload
  | BS.length payload < offset + 32 = Left "Vault event data word is missing"
  | otherwise = Right $ bytesInteger $ BS.take 32 $ BS.drop offset payload
 where
  offset = index * 32

bytesInteger :: BS.ByteString -> Integer
bytesInteger = BS.foldl' (\value byte -> value * 256 + fromIntegral byte) 0

logPosition :: RpcLog -> (Integer, Integer, Integer)
logPosition entry = (rpcLogBlockNumber entry, rpcLogTransactionIndex entry, rpcLogIndex entry)

sameBlock :: RpcBlock -> RpcBlock -> Bool
sameBlock left right =
  rpcBlockNumber left == rpcBlockNumber right
    && normalize (rpcBlockHash left) == normalize (rpcBlockHash right)
    && rpcBlockTimestamp left == rpcBlockTimestamp right

normalize :: Text -> Text
normalize = T.toLower . T.strip

rpcOrFail :: String -> IO (Either RpcError a) -> IO a
rpcOrFail context action = do
  result <- action
  case result of
    Left err -> fail $ context <> ": " <> show err
    Right value -> pure value

foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM _ initial [] = pure initial
foldlM action initial (value : rest) = action initial value >>= \next -> foldlM action next rest
