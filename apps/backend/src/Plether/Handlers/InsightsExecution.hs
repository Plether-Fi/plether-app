module Plether.Handlers.InsightsExecution
  ( LatencyStats (..)
  , DetectedKeeperEvent (..)
  , KeeperAggregate (..)
  , getExecutionQualityResponse
  , getExecutionTradesResponse
  , getKeepersStatusResponse
  , getKeepersActivityResponse
  , computeLatencyPercentiles
  , rateBps
  , aggregateKeepers
  , extractTxFrom
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import Data.Int (Int64)
import Data.List (nub, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple (Connection, In (..), Only (..), execute, execute_, query)
import Database.PostgreSQL.Simple.FromField (ResultError (ConversionFailed), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field, fieldWith)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadOnly)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema (getPerpsMarketVolumeSince)
import Plether.Ethereum.Client (EthClient, rpcCall)
import Plether.Ethereum.Rpc (ethGetBalance)
import Plether.Keeper.Reliability (OrderReliability (..), readOrderReliability)
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import Plether.Utils.Address (isValidAddress)
import Text.Read (readMaybe)

data LatencyStats = LatencyStats
  { lsSampleCount :: Int
  , lsP50 :: Maybe Integer
  , lsP95 :: Maybe Integer
  , lsP99 :: Maybe Integer
  , lsMax :: Maybe Integer
  }
  deriving stock (Eq, Show)

data DetectedKeeperEvent = DetectedKeeperEvent
  { dkeRole :: Text
  , dkeKeeperAddress :: Text
  , dkeTxHash :: Text
  , dkeBlockNumber :: Integer
  , dkeTimestamp :: Integer
  , dkeLatencySeconds :: Maybe Integer
  , dkeBountyAssets :: Maybe Integer
  }
  deriving stock (Eq, Show)

data KeeperAggregate = KeeperAggregate
  { kaAddress :: Text
  , kaRoles :: [Text]
  , kaExecutions24h :: Int
  , kaShareBps :: Maybe Int
  , kaLatencyP50Seconds :: Maybe Integer
  , kaLatencyP95Seconds :: Maybe Integer
  , kaBountiesEarnedAssets :: Integer
  , kaLastActiveBlock :: Integer
  , kaLastActiveTimestamp :: Integer
  }
  deriving stock (Eq, Show)

data RawKeeperTx = RawKeeperTx
  { rktRole :: Text
  , rktTxHash :: Text
  , rktBlockNumber :: Integer
  , rktTimestamp :: Integer
  , rktLatencySeconds :: Maybe Integer
  , rktBountyAssets :: Maybe Integer
  }
  deriving stock (Eq, Show)

data RecentTradeRow = RecentTradeRow
  { rtrActivityType :: Text
  , rtrAccount :: Text
  , rtrOrderId :: Maybe Integer
  , rtrSide :: Maybe Int
  , rtrPrice :: Maybe Integer
  , rtrSizeDelta :: Maybe Integer
  , rtrAmountAssets :: Maybe Integer
  , rtrPnlAssets :: Maybe Integer
  , rtrVpiAssets :: Maybe Integer
  , rtrFrozenSpreadAssets :: Maybe Integer
  , rtrOracleFrozen :: Maybe Bool
  , rtrLatencySeconds :: Maybe Integer
  , rtrKeeperBountyAssets :: Maybe Integer
  , rtrTxHash :: Text
  , rtrBlockNumber :: Integer
  , rtrLogIndex :: Integer
  , rtrTimestamp :: Integer
  }
  deriving stock (Eq, Show)

instance FromRow RecentTradeRow where
  fromRow =
    RecentTradeRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> field
      <*> field
      <*> nullableNumericIntegerField
      <*> field
      <*> field
      <*> field
      <*> field

getExecutionQualityResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getExecutionQualityResponse pool cfg = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      engine = T.toLower $ T.strip $ cfgPerpsCfdEngine cfg
      book = T.toLower Manifest.positionProtectionBookAddress
      cohortEnd = now - 120
      since24h = now - 86400
  (cohorts, pendingQueue, latencySecs, latencyBlks, protSummary, liqSummary, vol24h) <-
    withDb pool $ \conn ->
      withReadSnapshot conn $ do
        rel <- readOrderReliability conn router cohortEnd
        pending <- fetchPendingKeeperOrdersCount conn router
        (secs, blks) <- fetchOrderLatencies24h conn chainId router since24h
        prot <- fetchProtectionQuality conn chainId book since24h
        liq <- fetchLiquidationQuality conn chainId engine router since24h
        vol <- getPerpsMarketVolumeSince conn chainId router since24h
        pure (rel, pending, secs, blks, prot, liq, vol)
  let secStats = computeLatencyPercentiles latencySecs
      blkStats = computeLatencyPercentiles latencyBlks
      (protBlock, protActive, protTriggered24h, protCreated24h, protCancelled24h, protExpired24h) = protSummary
      (liqBlock, liqUpdatedAt, liqCandidates, liqCount24h, liqBounties24h) = liqSummary
      responseBlock = max protBlock liqBlock
  pure $
    Right $
      mkResponse responseBlock chainId $
        object
          [ "network" .= networkMetaJson cfg
          , "observedAt" .= isoTimestamp now
          , "cohortEndAt" .= isoTimestamp cohortEnd
          , "volume24hAssets" .= show vol24h
          , "pendingOrderQueue" .= pendingQueue
          , "orderReliability" .= map reliabilityToJson cohorts
          , "executionLatency"
              .= object
                [ "seconds" .= latencyStatsToJson secStats
                , "blocks" .= latencyStatsToJson blkStats
                ]
          , "protections"
              .= object
                [ "indexedThroughBlock" .= show protBlock
                , "activeProtections" .= protActive
                , "triggered24h" .= protTriggered24h
                , "created24h" .= protCreated24h
                , "cancelled24h" .= protCancelled24h
                , "expired24h" .= protExpired24h
                ]
          , "liquidations"
              .= object
                [ "indexedThroughBlock" .= show liqBlock
                , "updatedAt" .= (formatUtcTime <$> liqUpdatedAt)
                , "watchedCandidates" .= liqCandidates
                , "liquidations24h" .= liqCount24h
                , "bountiesPaid24hAssets" .= show liqBounties24h
                ]
          ]

getExecutionTradesResponse
  :: DbPool
  -> Config
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
getExecutionTradesResponse pool cfg requestedLimit = do
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      limit = min 100 $ max 1 requestedLimit
  rows <- withDb pool $ \conn ->
    withReadSnapshot conn $ fetchRecentTrades conn chainId router limit
  let latestBlock = maximum (0 : map rtrBlockNumber rows)
  pure $
    Right $
      mkResponse latestBlock chainId $
        object
          [ "network" .= networkMetaJson cfg
          , "trades" .= map recentTradeToJson rows
          ]

getKeepersStatusResponse
  :: DbPool
  -> EthClient
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getKeepersStatusResponse pool client cfg = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
  events <- resolvePermissionlessKeeperEvents pool client cfg (now - 86400) 250
  let aggregated = aggregateKeepers events
      topKeepers = take 30 aggregated
  balances <- mapConcurrently (fetchKeeperEthBalance client . kaAddress) topKeepers
  let allLatencies = mapMaybe dkeLatencySeconds events
      fleetLatency = computeLatencyPercentiles allLatencies
      totalBounties = sum $ mapMaybe dkeBountyAssets events
      latestBlock = maximum (0 : map dkeBlockNumber events)
  pure $
    Right $
      mkResponse latestBlock chainId $
        object
          [ "network" .= networkMetaJson cfg
          , "observedAt" .= isoTimestamp now
          , "summary"
              .= object
                [ "activeKeepers24h" .= length aggregated
                , "executions24h" .= length events
                , "latencyP50Seconds" .= lsP50 fleetLatency
                , "latencyP95Seconds" .= lsP95 fleetLatency
                , "bountiesPaid24hAssets" .= show totalBounties
                ]
          , "keepers" .= zipWith keeperAggregateToJson topKeepers balances
          ]

getKeepersActivityResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
getKeepersActivityResponse pool client cfg requestedLimit = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      limit = min 100 $ max 1 requestedLimit
  events <- resolvePermissionlessKeeperEvents pool client cfg (now - 86400) (max 50 limit)
  let visible = take limit $ sortOn (\e -> (Down (dkeBlockNumber e), Down (dkeTimestamp e))) events
      latestBlock = maximum (0 : map dkeBlockNumber visible)
  pure $
    Right $
      mkResponse latestBlock chainId $
        object
          [ "network" .= networkMetaJson cfg
          , "observedAt" .= isoTimestamp now
          , "activity" .= map keeperEventToJson visible
          ]

computeLatencyPercentiles :: [Integer] -> LatencyStats
computeLatencyPercentiles samples =
  case sort (filter (>= 0) samples) of
    [] -> LatencyStats 0 Nothing Nothing Nothing Nothing
    sorted ->
      let count = length sorted
          atPct p = sorted !! min (count - 1) ((count * p + 99) `div` 100 - 1)
       in LatencyStats
            { lsSampleCount = count
            , lsP50 = Just $ atPct 50
            , lsP95 = Just $ atPct 95
            , lsP99 = Just $ atPct 99
            , lsMax = Just $ last sorted
            }

rateBps :: Integer -> Integer -> Maybe Int
rateBps numerator denominator
  | denominator <= 0 = Nothing
  | otherwise = Just $ fromIntegral ((numerator * 10000 + denominator `div` 2) `div` denominator)

aggregateKeepers :: [DetectedKeeperEvent] -> [KeeperAggregate]
aggregateKeepers events =
  let totalCount = fromIntegral (length events) :: Integer
      grouped =
        foldr
          (\ev acc -> Map.insertWith (++) (T.toLower $ dkeKeeperAddress ev) [ev] acc)
          Map.empty
          events
      buildAgg (addr, evs) =
        let latencies = computeLatencyPercentiles $ mapMaybe dkeLatencySeconds evs
            execCount = length evs
            bounties = sum $ mapMaybe dkeBountyAssets evs
            roles = sort $ nub $ map dkeRole evs
            lastBlock = maximum $ map dkeBlockNumber evs
            lastTs = maximum $ map dkeTimestamp evs
         in KeeperAggregate
              { kaAddress = addr
              , kaRoles = roles
              , kaExecutions24h = execCount
              , kaShareBps = rateBps (fromIntegral execCount) totalCount
              , kaLatencyP50Seconds = lsP50 latencies
              , kaLatencyP95Seconds = lsP95 latencies
              , kaBountiesEarnedAssets = bounties
              , kaLastActiveBlock = lastBlock
              , kaLastActiveTimestamp = lastTs
              }
   in sortOn (\k -> (Down (kaExecutions24h k), Down (kaLastActiveBlock k), kaAddress k)) $
        map buildAgg $
          Map.toList grouped

extractTxFrom :: Value -> Maybe Text
extractTxFrom (Object obj) =
  case KM.lookup (Key.fromText "from") obj of
    Just (String addr) | isValidAddress addr -> Just $ T.toLower $ T.strip addr
    _ -> Nothing
extractTxFrom _ = Nothing

resolvePermissionlessKeeperEvents
  :: DbPool
  -> EthClient
  -> Config
  -> Integer
  -> Int
  -> IO [DetectedKeeperEvent]
resolvePermissionlessKeeperEvents pool client cfg sinceUnix maxTxs = do
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      book = T.toLower Manifest.positionProtectionBookAddress
      monitor = T.toLower $ T.strip $ cfgPerpsSettlementMonitorLens cfg
  rawTxs <- withDb pool $ \conn -> do
    ensureTxSenderCacheTable conn
    withReadSnapshot conn $ fetchRecentPermissionlessTxs conn chainId router book monitor sinceUnix maxTxs
  let uniqueHashes = nub $ map (T.toLower . rktTxHash) rawTxs
  cached <- withDb pool $ \conn -> fetchCachedTxSenders conn chainId uniqueHashes
  let missing = take 40 $ filter (`Map.notMember` cached) uniqueHashes
  resolvedPairs <- mapConcurrently (resolveTxSenderRpc client) missing
  let newlyResolved = [(h, s) | (h, Just s) <- resolvedPairs]
  if null newlyResolved
    then pure ()
    else withDb pool $ \conn -> upsertCachedTxSenders conn chainId newlyResolved
  let senderMap = Map.union cached (Map.fromList newlyResolved)
  pure $
    mapMaybe
      ( \raw -> do
          sender <- Map.lookup (T.toLower $ rktTxHash raw) senderMap
          pure
            DetectedKeeperEvent
              { dkeRole = rktRole raw
              , dkeKeeperAddress = sender
              , dkeTxHash = T.toLower $ rktTxHash raw
              , dkeBlockNumber = rktBlockNumber raw
              , dkeTimestamp = rktTimestamp raw
              , dkeLatencySeconds = rktLatencySeconds raw
              , dkeBountyAssets = rktBountyAssets raw
              }
      )
      rawTxs

resolveTxSenderRpc :: EthClient -> Text -> IO (Text, Maybe Text)
resolveTxSenderRpc client txHash = do
  res <- rpcCall client "eth_getTransactionByHash" [String txHash]
  pure $ case res of
    Right val -> (txHash, extractTxFrom val)
    Left _ -> (txHash, Nothing)

fetchKeeperEthBalance :: EthClient -> Text -> IO (Maybe Integer)
fetchKeeperEthBalance client addr = do
  res <- ethGetBalance client addr
  pure $ either (const Nothing) Just res

ensureTxSenderCacheTable :: Connection -> IO ()
ensureTxSenderCacheTable conn = do
  _ <-
    execute_
      conn
      "CREATE TABLE IF NOT EXISTS insights_tx_senders (\
      \  chain_id BIGINT NOT NULL,\
      \  tx_hash VARCHAR(66) NOT NULL,\
      \  sender_address VARCHAR(42) NOT NULL,\
      \  resolved_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
      \  PRIMARY KEY (chain_id, tx_hash)\
      \)"
  pure ()

fetchCachedTxSenders :: Connection -> Integer -> [Text] -> IO (Map.Map Text Text)
fetchCachedTxSenders _ _ [] = pure Map.empty
fetchCachedTxSenders conn chainId hashes = do
  rows <-
    query
      conn
      "SELECT tx_hash, sender_address FROM insights_tx_senders \
      \WHERE chain_id = ? AND tx_hash IN ?"
      (chainId, In hashes) :: IO [(Text, Text)]
  pure $ Map.fromList [(T.toLower h, T.toLower s) | (h, s) <- rows]

upsertCachedTxSenders :: Connection -> Integer -> [(Text, Text)] -> IO ()
upsertCachedTxSenders conn chainId =
  mapM_ $ \(txHash, sender) -> do
    _ <-
      execute
        conn
        "INSERT INTO insights_tx_senders (chain_id, tx_hash, sender_address) \
        \VALUES (?, ?, ?) ON CONFLICT (chain_id, tx_hash) DO NOTHING"
        (chainId, T.toLower txHash, T.toLower sender)
    pure ()

fetchRecentPermissionlessTxs
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Int
  -> IO [RawKeeperTx]
fetchRecentPermissionlessTxs conn chainId router book monitor sinceUnix limit = do
  orderRows <-
    query
      conn
      "SELECT terminal_tx_hash, COALESCE(terminal_block_number, 0), COALESCE(terminal_timestamp, 0), \
      \CASE WHEN commit_timestamp IS NOT NULL AND terminal_timestamp IS NOT NULL AND terminal_timestamp >= commit_timestamp \
      \  THEN terminal_timestamp - commit_timestamp ELSE NULL END \
      \FROM perps_orders \
      \WHERE chain_id = ? AND order_router = ? AND terminal_status = 'Executed' \
      \  AND terminal_tx_hash IS NOT NULL AND COALESCE(terminal_timestamp, 0) >= ? \
      \ORDER BY terminal_block_number DESC LIMIT ?"
      (chainId, router, sinceUnix, limit) :: IO [(Text, Integer, Integer, Maybe Integer)]
  liqRows <-
    query
      conn
      "SELECT tx_hash, block_number, timestamp, \
      \CASE WHEN data->>'keeperBountyAssets' ~ '^[0-9]+$' THEN data->>'keeperBountyAssets' ELSE NULL END \
      \FROM perps_account_activity \
      \WHERE chain_id = ? AND release_router = ? AND activity_type = 'Liquidated' AND timestamp >= ? \
      \ORDER BY block_number DESC LIMIT ?"
      (chainId, router, sinceUnix, limit) :: IO [(Text, Integer, Integer, Maybe String)]
  protRows <-
    query
      conn
      "SELECT transaction_hash, block_number, EXTRACT(EPOCH FROM NOW())::BIGINT \
      \FROM perps_protection_events \
      \WHERE chain_id = ? AND book = ? AND event_name = 'PositionProtectionTriggered' \
      \ORDER BY block_number DESC LIMIT ?"
      (chainId, book, min 50 limit) :: IO [(Text, Integer, Integer)]
  lpRows <-
    query
      conn
      "SELECT receipt_transaction_hash, COALESCE(receipt_block_number, 0), created_at, confirmed_at \
      \FROM perps_lp_settlement_transactions \
      \WHERE chain_id = ? AND monitor_address = ? AND status = 'confirmed_success' \
      \  AND receipt_transaction_hash IS NOT NULL AND created_at >= NOW() - INTERVAL '24 hours' \
      \ORDER BY receipt_block_number DESC LIMIT ?"
      (chainId, monitor, min 50 limit) :: IO [(Text, Integer, UTCTime, Maybe UTCTime)]
  let orders =
        [ RawKeeperTx "order_execution" h blk ts lat Nothing
        | (h, blk, ts, lat) <- orderRows
        ]
      liqs =
        [ RawKeeperTx "liquidation" h blk ts Nothing (bounty >>= readMaybe)
        | (h, blk, ts, bounty) <- liqRows
        ]
      prots =
        [ RawKeeperTx "protection" h blk ts Nothing Nothing
        | (h, blk, ts) <- protRows
        ]
      lps =
        [ RawKeeperTx
            "lp_settlement"
            h
            blk
            (floor $ utcTimeToPOSIXSeconds $ fromMaybe created confirmed)
            ((\c -> max 0 $ round (diffUTCTime c created)) <$> confirmed)
            Nothing
        | (h, blk, created, confirmed) <- lpRows
        ]
  pure $ take limit $ sortOn (Down . rktBlockNumber) (orders <> liqs <> prots <> lps)

fetchPendingKeeperOrdersCount :: Connection -> Text -> IO Int64
fetchPendingKeeperOrdersCount conn router = do
  rows <-
    query
      conn
      "SELECT COUNT(*) FROM perps_keeper_orders WHERE order_router = ? AND status = 'pending'"
      (Only router) :: IO [Only Int64]
  pure $ case rows of
    [Only n] -> n
    _ -> 0

fetchOrderLatencies24h :: Connection -> Integer -> Text -> Integer -> IO ([Integer], [Integer])
fetchOrderLatencies24h conn chainId router sinceUnix = do
  rows <-
    query
      conn
      "SELECT \
      \  GREATEST(0, terminal_timestamp - commit_timestamp), \
      \  GREATEST(0, terminal_block_number - commit_block_number) \
      \FROM perps_orders \
      \WHERE chain_id = ? AND order_router = ? AND terminal_status = 'Executed' \
      \  AND commit_timestamp IS NOT NULL AND terminal_timestamp IS NOT NULL \
      \  AND commit_block_number IS NOT NULL AND terminal_block_number IS NOT NULL \
      \  AND terminal_timestamp >= ? \
      \ORDER BY terminal_block_number DESC LIMIT 2000"
      (chainId, router, sinceUnix) :: IO [(Integer, Integer)]
  pure (map fst rows, map snd rows)

fetchProtectionQuality
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> IO (Integer, Int64, Int64, Int64, Int64, Int64)
fetchProtectionQuality conn chainId book _sinceUnix = do
  cpRows <-
    query
      conn
      "SELECT block_number FROM perps_protection_checkpoints \
      \WHERE chain_id = ? AND book = ? ORDER BY block_number DESC LIMIT 1"
      (chainId, book) :: IO [Only Integer]
  activeRows <-
    query
      conn
      "SELECT COUNT(*) FROM (\
      \  SELECT DISTINCT ON (protection_id) protection_id, event_name \
      \  FROM perps_protection_events \
      \  WHERE chain_id = ? AND book = ? \
      \  ORDER BY protection_id, block_number DESC, log_index DESC\
      \) latest WHERE event_name IN ('PositionProtectionCreated', 'PositionProtectionUpdated')"
      (chainId, book) :: IO [Only Int64]
  eventRows <-
    query
      conn
      "SELECT \
      \  COUNT(*) FILTER (WHERE event_name = 'PositionProtectionTriggered'), \
      \  COUNT(*) FILTER (WHERE event_name = 'PositionProtectionCreated'), \
      \  COUNT(*) FILTER (WHERE event_name = 'PositionProtectionCancelled'), \
      \  COUNT(*) FILTER (WHERE event_name = 'PositionProtectionExpired') \
      \FROM perps_protection_events \
      \WHERE chain_id = ? AND book = ?"
      (chainId, book) :: IO [(Int64, Int64, Int64, Int64)]
  let cpBlock = maybe 0 fromOnly $ listToMaybe cpRows
      active = maybe 0 fromOnly $ listToMaybe activeRows
      (trig, created, cancelled, expired) = fromMaybe (0, 0, 0, 0) $ listToMaybe eventRows
  pure (cpBlock, active, trig, created, cancelled, expired)

fetchLiquidationQuality
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Integer
  -> IO (Integer, Maybe UTCTime, Int64, Int64, Integer)
fetchLiquidationQuality conn chainId engine router sinceUnix = do
  stateRows <-
    query
      conn
      "SELECT last_indexed_block, updated_at FROM perps_liquidation_state \
      \WHERE chain_id = ? AND lower(cfd_engine) = ? LIMIT 1"
      (chainId, engine) :: IO [(Integer, Maybe UTCTime)]
  candRows <-
    query
      conn
      "SELECT COUNT(*) FROM perps_liquidation_candidates WHERE chain_id = ? AND lower(cfd_engine) = ?"
      (chainId, engine) :: IO [Only Int64]
  liqRows <-
    query
      conn
      "SELECT COUNT(*), COALESCE(SUM(CASE WHEN data->>'keeperBountyAssets' ~ '^[0-9]+$' THEN (data->>'keeperBountyAssets')::NUMERIC ELSE 0 END), 0)::TEXT \
      \FROM perps_account_activity \
      \WHERE chain_id = ? AND release_router = ? AND activity_type = 'Liquidated' AND timestamp >= ?"
      (chainId, router, sinceUnix) :: IO [(Int64, String)]
  let (blockNum, updatedAt) = fromMaybe (0, Nothing) $ listToMaybe stateRows
      candidates = maybe 0 fromOnly $ listToMaybe candRows
      (liqCount, bountyStr) = fromMaybe (0, "0") $ listToMaybe liqRows
      bounties = fromMaybe 0 $ readMaybe bountyStr
  pure (blockNum, updatedAt, candidates, liqCount, bounties)

fetchRecentTrades :: Connection -> Integer -> Text -> Int -> IO [RecentTradeRow]
fetchRecentTrades conn chainId router limit =
  query
    conn
    "SELECT a.activity_type, a.account, a.order_id, a.side, a.price, a.size_delta, \
    \a.amount_assets, a.pnl_assets, o.execution_vpi_assets, o.execution_frozen_close_spread_assets, \
    \o.execution_oracle_frozen, \
    \CASE WHEN o.commit_timestamp IS NOT NULL AND o.terminal_timestamp IS NOT NULL AND o.terminal_timestamp >= o.commit_timestamp \
    \  THEN o.terminal_timestamp - o.commit_timestamp ELSE NULL END, \
    \CASE WHEN a.data->>'keeperBountyAssets' ~ '^[0-9]+$' THEN (a.data->>'keeperBountyAssets')::NUMERIC ELSE NULL END, \
    \a.tx_hash, a.block_number, a.log_index, a.timestamp \
    \FROM perps_account_activity a \
    \LEFT JOIN perps_orders o \
    \  ON o.chain_id = a.chain_id AND o.order_router = a.release_router AND o.order_id = a.order_id \
    \WHERE a.chain_id = ? AND a.release_router = ? \
    \  AND a.activity_type IN ('Open', 'Close', 'Liquidated') \
    \ORDER BY a.block_number DESC, a.log_index DESC \
    \LIMIT ?"
    (chainId, router, limit)

networkMetaJson :: Config -> Value
networkMetaJson cfg =
  object
    [ "chainId" .= cfgPerpsChainId cfg
    , "releaseVersion" .= Manifest.releaseVersion
    , "settlementAssetSymbol" .= ("USDG" :: Text)
    ]

reliabilityToJson :: OrderReliability -> Value
reliabilityToJson OrderReliability {..} =
  object
    [ "windowSeconds" .= windowSeconds
    , "totalOrders" .= totalOrders
    , "executedOrders" .= executedOrders
    , "expiredOrders" .= expiredOrders
    , "otherFailedOrders" .= otherFailedOrders
    , "pendingOrders" .= pendingOrders
    , "activeAccounts" .= activeAccounts
    , "affectedAccounts" .= affectedAccounts
    , "repeatAffectedAccounts" .= repeatAffectedAccounts
    , "fillRateBps" .= rateBps executedOrders totalOrders
    , "expiryRateBps" .= rateBps expiredOrders totalOrders
    ]

latencyStatsToJson :: LatencyStats -> Value
latencyStatsToJson LatencyStats {..} =
  object
    [ "sampleCount" .= lsSampleCount
    , "p50" .= lsP50
    , "p95" .= lsP95
    , "p99" .= lsP99
    , "max" .= lsMax
    ]

recentTradeToJson :: RecentTradeRow -> Value
recentTradeToJson RecentTradeRow {..} =
  object $
    catMaybes
      [ Just $ "id" .= (rtrTxHash <> ":" <> T.pack (show rtrLogIndex))
      , Just $ "activityType" .= rtrActivityType
      , Just $ "account" .= rtrAccount
      , ("orderId" .=) . show <$> rtrOrderId
      , ("side" .=) <$> (sideText <$> rtrSide)
      , ("price" .=) . show <$> rtrPrice
      , ("sizeDelta" .=) . show <$> rtrSizeDelta
      , ("amountAssets" .=) . show <$> rtrAmountAssets
      , ("pnlAssets" .=) . show <$> rtrPnlAssets
      , ("vpiAssets" .=) . show <$> rtrVpiAssets
      , ("frozenCloseSpreadAssets" .=) . show <$> rtrFrozenSpreadAssets
      , ("oracleFrozen" .=) <$> rtrOracleFrozen
      , ("latencySeconds" .=) <$> rtrLatencySeconds
      , ("keeperBountyAssets" .=) . show <$> rtrKeeperBountyAssets
      , Just $ "txHash" .= rtrTxHash
      , Just $ "blockNumber" .= show rtrBlockNumber
      , Just $ "occurredAt" .= isoTimestamp rtrTimestamp
      ]

keeperAggregateToJson :: KeeperAggregate -> Maybe Integer -> Value
keeperAggregateToJson KeeperAggregate {..} ethBalance =
  object
    [ "address" .= kaAddress
    , "roles" .= kaRoles
    , "executions24h" .= kaExecutions24h
    , "shareBps" .= kaShareBps
    , "latencyP50Seconds" .= kaLatencyP50Seconds
    , "latencyP95Seconds" .= kaLatencyP95Seconds
    , "ethBalanceWei" .= (show <$> ethBalance)
    , "bountiesEarnedAssets" .= show kaBountiesEarnedAssets
    , "lastActiveBlock" .= show kaLastActiveBlock
    , "lastActiveAt" .= isoTimestamp kaLastActiveTimestamp
    ]

keeperEventToJson :: DetectedKeeperEvent -> Value
keeperEventToJson DetectedKeeperEvent {..} =
  object
    [ "role" .= dkeRole
    , "keeperAddress" .= dkeKeeperAddress
    , "txHash" .= dkeTxHash
    , "blockNumber" .= show dkeBlockNumber
    , "occurredAt" .= isoTimestamp dkeTimestamp
    , "latencySeconds" .= dkeLatencySeconds
    , "bountyAssets" .= (show <$> dkeBountyAssets)
    ]

sideText :: Int -> Text
sideText 0 = "long"
sideText 1 = "short"
sideText _ = "unknown"

withReadSnapshot :: Connection -> IO a -> IO a
withReadSnapshot conn =
  withTransactionMode
    TransactionMode
      { isolationLevel = RepeatableRead
      , readWriteMode = ReadOnly
      }
    conn

nullableNumericIntegerField :: RowParser (Maybe Integer)
nullableNumericIntegerField =
  fieldWith $ \column raw ->
    case raw of
      Nothing -> pure Nothing
      Just bytes -> case readMaybe (BS8.unpack bytes) of
        Just integer -> pure $ Just integer
        Nothing -> returnError ConversionFailed column "Expected nullable integer NUMERIC column"

formatUtcTime :: UTCTime -> Text
formatUtcTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

isoTimestamp :: Integer -> Text
isoTimestamp = formatUtcTime . posixSecondsToUTCTime . fromIntegral
