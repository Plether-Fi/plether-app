module Plether.Handlers.InsightsExecution
  ( LatencyStats (..)
  , ProtocolLatencyBenchmark (..)
  , OrderReliabilityBucket (..)
  , DetectedKeeperEvent (..)
  , KeeperAggregate (..)
  , KeeperOrderAggregateRow (..)
  , RawKeeperTx (..)
  , RecentTradeRow (..)
  , getExecutionQualityResponse
  , getExecutionTradesResponse
  , getTransactionResponse
  , getKeepersStatusResponse
  , getKeepersActivityResponse
  , getKeeperDetailResponse
  , ensureTxSenderCacheTable
  , fetchLiquidationQuality
  , fetchOrderLatencies24h
  , fetchProtectionQuality
  , fetchRecentTrades
  , fetchTransactionTrades
  , isValidTxHash
  , classifyTxKind
  , computeLatencyPercentiles
  , computeLatencyPercentileRank
  , buildLatencyBenchmark
  , buildTimelineBuckets
  , rateBps
  , aggregateKeepers
  , mergeKeeperFleetAggregates
  , mergeLpSettlementTxs
  , parseHexInteger
  , extractTxFrom
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (..), mapConcurrently, runConcurrently)
import Control.Exception (catch, throwIO)
import Control.Monad (forM, unless)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isHexDigit)
import Data.Either (isRight)
import Data.Int (Int64)
import Data.List (nub, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple (Connection, In (..), Only (..), Query, SqlError (sqlState), executeMany, execute_, query, query_)
import Database.PostgreSQL.Simple.FromField (ResultError (ConversionFailed), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field, fieldWith)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadOnly)
  , TransactionMode (..)
  , withTransactionMode
  )
import Numeric (readHex)
import Plether.Cache
  ( SingleFlightCache
  , newConcurrentRefreshingSingleFlightCache
  , newRefreshingSingleFlightCache
  , runSingleFlightCache
  )
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema (getPerpsMarketVolumeSince)
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall)
import Plether.Ethereum.Client (EthClient, rpcCall)
import qualified Plether.Ethereum.Contracts.Perps as Perps
import qualified Plether.Ethereum.Multicall as Multicall
import Plether.Handlers.InsightsOrderProtections
  ( OrderProtectionsBundle
  , buildOrderProtectionsForOrder
  , enrichOrderProtectionsBundleFromRpc
  , extractExecutionOrderId
  , fetchOrderProtectionsBundle
  )
import Plether.Ethereum.Rpc (ethGetBalance)
import Plether.Keeper.Reliability (OrderReliability (..), readOrderReliability)
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import qualified Plether.Types.Error as E
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

data ProtocolLatencyBenchmark = ProtocolLatencyBenchmark
  { plbWindowLabel :: Text
  , plbSamples :: [Integer]
  , plbSampleCount :: Int
  , plbMinSeconds :: Maybe Integer
  , plbP25Seconds :: Maybe Integer
  , plbP50Seconds :: Maybe Integer
  , plbP75Seconds :: Maybe Integer
  , plbP95Seconds :: Maybe Integer
  , plbP99Seconds :: Maybe Integer
  , plbMaxSeconds :: Maybe Integer
  , plbP50CommitToOracleSeconds :: Maybe Integer
  , plbP50OracleToFinalizeSeconds :: Maybe Integer
  }
  deriving stock (Eq, Show)

{-# NOINLINE protocolLatencyBenchmarkCache #-}
protocolLatencyBenchmarkCache :: SingleFlightCache (Integer, Text) (Maybe ProtocolLatencyBenchmark)
protocolLatencyBenchmarkCache =
  unsafePerformIO (newRefreshingSingleFlightCache 8 (60 * 1000000000) (300 * 1000000000))

data OrderReliabilityBucket = OrderReliabilityBucket
  { orbStartUnix :: Integer
  , orbEndUnix :: Integer
  , orbTotalOrders :: Integer
  , orbExecutedOrders :: Integer
  , orbExpiredOrders :: Integer
  , orbOtherFailedOrders :: Integer
  , orbPendingOrders :: Integer
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

data KeeperOrderAggregateRow = KeeperOrderAggregateRow
  { koarAddress :: Text
  , koarExecutions24h :: Int
  , koarLatencyP50Seconds :: Maybe Integer
  , koarLatencyP95Seconds :: Maybe Integer
  , koarLastActiveBlock :: Integer
  , koarLastActiveTimestamp :: Integer
  }
  deriving stock (Eq, Show)

data KeeperFleet24h = KeeperFleet24h
  { kfExecutions24h :: Int
  , kfLatencyP50Seconds :: Maybe Integer
  , kfLatencyP95Seconds :: Maybe Integer
  , kfBountiesPaid24hAssets :: Integer
  , kfLatestBlock :: Integer
  , kfOrderAggs :: [KeeperOrderAggregateRow]
  , kfNonOrderEvents :: [DetectedKeeperEvent]
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
  , rtrExecution :: Maybe Value
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
      <*> field

data FinalizedOrderRow = FinalizedOrderRow
  { forOrderId :: Integer
  , forAccount :: Maybe Text
  , forSide :: Maybe Int
  , forCommitTxHash :: Maybe Text
  , forCommitBlock :: Maybe Integer
  , forCommitTimestamp :: Maybe Integer
  , forTerminalBlock :: Maybe Integer
  , forTerminalTimestamp :: Maybe Integer
  , forTerminalStatus :: Text
  , forTerminalReason :: Maybe Text
  , forFailureReason :: Maybe Text
  , forFailedConstraint :: Maybe Text
  , forExecutionMode :: Maybe Text
  , forExecutionPrice :: Maybe Integer
  , forOraclePrice :: Maybe Integer
  , forOracleFrozen :: Maybe Bool
  , forOracleMinPublishTime :: Maybe Integer
  , forOracleMaxPublishTime :: Maybe Integer
  , forVpiAssets :: Maybe Integer
  , forFrozenSpreadAssets :: Maybe Integer
  }
  deriving stock (Eq, Show)

instance FromRow FinalizedOrderRow where
  fromRow =
    FinalizedOrderRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> field
      <*> field
      <*> field
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField

data ProtocolEventRow = ProtocolEventRow
  { perLogIndex :: Integer
  , perBlockNumber :: Integer
  , perTimestamp :: Integer
  , perContractAddress :: Text
  , perEventName :: Text
  , perAccount :: Maybe Text
  , perOrderId :: Maybe Integer
  , perSide :: Maybe Int
  , perData :: Value
  }
  deriving stock (Eq, Show)

instance FromRow ProtocolEventRow where
  fromRow =
    ProtocolEventRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data TxLpSettlementRow = TxLpSettlementRow
  { tlsEpoch :: Integer
  , tlsStatus :: Text
  , tlsReceiptBlock :: Maybe Integer
  , tlsConfirmedAt :: Maybe UTCTime
  , tlsCreatedAt :: UTCTime
  , tlsSeniorDepositAssets :: Maybe Integer
  , tlsSeniorRedeemAssets :: Maybe Integer
  , tlsJuniorDepositAssets :: Maybe Integer
  , tlsJuniorRedeemAssets :: Maybe Integer
  , tlsSeniorBacklog :: Maybe Bool
  , tlsJuniorBacklog :: Maybe Bool
  , tlsEntriesDeferred :: Maybe Bool
  }
  deriving stock (Eq, Show)

instance FromRow TxLpSettlementRow where
  fromRow =
    TxLpSettlementRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> field
      <*> field
      <*> field

{-# NOINLINE executionQualityCache #-}
executionQualityCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
executionQualityCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (15 * 1000000000) (300 * 1000000000))

{-# NOINLINE executionTradesCache #-}
executionTradesCache :: SingleFlightCache Int (Either ApiError (ApiResponse Value))
executionTradesCache =
  unsafePerformIO (newConcurrentRefreshingSingleFlightCache 16 (15 * 1000000000) (300 * 1000000000))

{-# NOINLINE txDetailCache #-}
txDetailCache :: SingleFlightCache Text (Either ApiError (ApiResponse Value))
txDetailCache =
  unsafePerformIO (newConcurrentRefreshingSingleFlightCache 256 (120 * 1000000000) (1800 * 1000000000))

getExecutionQualityResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getExecutionQualityResponse pool cfg =
  snd <$> runSingleFlightCache executionQualityCache () isRight (computeExecutionQualityResponse pool cfg)

computeExecutionQualityResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
computeExecutionQualityResponse pool cfg = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      engine = T.toLower $ T.strip $ cfgPerpsCfdEngine cfg
      book = T.toLower Manifest.positionProtectionBookAddress
      cohortEnd = now - 120
      since24h = now - 86400
  (cohorts, timelineByWindow, pendingQueue, latencySecs, latencyBlks, protSummary, liqSummary, vol24h) <-
    withDb pool $ \conn ->
      withReadSnapshot conn $ do
        rel <- readOrderReliability conn router cohortEnd
        tl <- fetchOrderReliabilityBuckets conn router cohortEnd
        pending <- fetchPendingKeeperOrdersCount conn router
        (secs, blks) <- fetchOrderLatencies24h conn chainId router since24h
        prot <- fetchProtectionQuality conn chainId book since24h
        liq <- fetchLiquidationQuality conn chainId engine router since24h
        vol <- getPerpsMarketVolumeSince conn chainId router since24h
        pure (rel, tl, pending, secs, blks, prot, liq, vol)
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
          , "orderReliability"
              .= map
                (\c -> reliabilityToJson c (Map.findWithDefault [] (windowSeconds c) timelineByWindow))
                cohorts
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
getExecutionTradesResponse pool cfg requestedLimit =
  let limit = min 100 $ max 1 requestedLimit
   in snd
        <$> runSingleFlightCache
          executionTradesCache
          limit
          isRight
          (computeExecutionTradesResponse pool cfg limit)

computeExecutionTradesResponse
  :: DbPool
  -> Config
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
computeExecutionTradesResponse pool cfg limit = do
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      protBook = T.toLower Manifest.positionProtectionBookAddress
  (rows, protBundle) <- withDb pool $ \conn ->
    withReadSnapshot conn $ do
      fetched <- fetchRecentTrades conn chainId router limit
      let openOrderIds =
            mapMaybe
              (\r -> rtrOrderId r <|> extractExecutionOrderId (rtrExecution r))
              (filter (\r -> rtrActivityType r == "Open") fetched)
      bundle <- fetchOrderProtectionsBundle conn chainId router protBook openOrderIds
      pure (fetched, bundle)
  let latestBlock = maximum (0 : map rtrBlockNumber rows)
  pure $
    Right $
      mkResponse latestBlock chainId $
        object
          [ "network" .= networkMetaJson cfg
          , "trades" .= map (recentTradeToJson protBundle) rows
          ]

isValidTxHash :: Text -> Bool
isValidTxHash raw =
  let normalized = T.toLower (T.strip raw)
   in T.length normalized == 66
        && "0x" `T.isPrefixOf` normalized
        && T.all isHexDigit (T.drop 2 normalized)

getTransactionResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> IO (Either ApiError (ApiResponse Value))
getTransactionResponse pool client cfg rawTxHash = do
  let txHash = T.toLower (T.strip rawTxHash)
  if not (isValidTxHash txHash)
    then pure $ Left $ E.invalidAmount "txHash must be a 0x-prefixed 32-byte hex string"
    else
      snd
        <$> runSingleFlightCache
          txDetailCache
          txHash
          isRight
          (computeTransactionResponse pool client cfg txHash)

computeTransactionResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> IO (Either ApiError (ApiResponse Value))
computeTransactionResponse pool client cfg txHash = do
      let chainId = cfgPerpsChainId cfg
          router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
          protBook = T.toLower Manifest.positionProtectionBookAddress
      (rows, finalizedOrders, protocolEvents, mLpSettlement, cachedSender, baseProtBundle, oracleUpdateMap) <- withDb pool $ \conn ->
        withReadSnapshot conn $ do
          txRows <- fetchTransactionTrades conn chainId router txHash
          orders <- fetchTransactionFinalizedOrders conn chainId router txHash
          protoEvs <- fetchTransactionProtocolEvents conn chainId router txHash
          lpRow <- fetchTransactionLpSettlement conn chainId txHash
          senders <- fetchCachedTxSenders conn chainId [txHash]
          let txOrderIds =
                mapMaybe (\r -> rtrOrderId r <|> extractExecutionOrderId (rtrExecution r)) txRows
                  <> map forOrderId orders
          bundle <- fetchOrderProtectionsBundle conn chainId router protBook txOrderIds
          oracleIds <- fetchOracleUpdateIdsForOrders conn orders
          pure (txRows, orders, protoEvs, lpRow, Map.lookup txHash senders, bundle, oracleIds)
      if null rows && null finalizedOrders && null protocolEvents && maybe True (const False) mLpSettlement
        then
          pure $ Left $ E.notFound ("No indexed Plether Perps activity found for transaction " <> txHash)
        else do
          (executorAddr, protBundle, mLatencyBenchmark) <-
            runConcurrently $
              (,,)
                <$> Concurrently
                  ( case cachedSender of
                      Just addr -> pure (Just addr)
                      Nothing -> do
                        (_, mResolved) <- resolveTxSenderRpc client txHash
                        case mResolved of
                          Just resolved ->
                            withDb pool (\conn -> upsertCachedTxSenders conn chainId [(txHash, resolved)])
                              `catch` (\(_ :: SqlError) -> pure ())
                          Nothing -> pure ()
                        pure mResolved
                  )
                <*> Concurrently (enrichOrderProtectionsBundleFromRpc client baseProtBundle)
                <*> Concurrently
                  ( if null finalizedOrders
                      then pure Nothing
                      else getCachedProtocolLatencyBenchmark pool chainId router
                  )
          let blockNum =
                fromMaybe 0 $
                  (rtrBlockNumber <$> listToMaybe rows)
                    <|> (listToMaybe finalizedOrders >>= forTerminalBlock)
                    <|> (perBlockNumber <$> listToMaybe protocolEvents)
                    <|> (mLpSettlement >>= tlsReceiptBlock)
              occurredTs =
                fromMaybe 0 $
                  (rtrTimestamp <$> listToMaybe rows)
                    <|> (listToMaybe finalizedOrders >>= forTerminalTimestamp)
                    <|> (perTimestamp <$> listToMaybe protocolEvents)
                    <|> ((\lp -> floor (utcTimeToPOSIXSeconds (fromMaybe (tlsCreatedAt lp) (tlsConfirmedAt lp)))) <$> mLpSettlement)
              txKind = classifyTxKind (length finalizedOrders) (maybe False (const True) mLpSettlement) (any (\r -> rtrActivityType r == "Liquidated") rows)
              mBatchSummary =
                if null finalizedOrders
                  then Nothing
                  else Just (batchSummaryToJson oracleUpdateMap finalizedOrders)
          pure $
            Right $
              mkResponse blockNum chainId $
                object
                  [ "network" .= networkMetaJson cfg
                  , "txHash" .= txHash
                  , "txKind" .= txKind
                  , "blockNumber" .= show blockNum
                  , "occurredAt" .= isoTimestamp occurredTs
                  , "executorAddress" .= executorAddr
                  , "batchSummary" .= mBatchSummary
                  , "latencyBenchmark" .= (protocolLatencyBenchmarkToJson <$> mLatencyBenchmark)
                  , "finalizedOrders" .= map (finalizedOrderToJson oracleUpdateMap protBundle mLatencyBenchmark) finalizedOrders
                  , "protocolEvents" .= map protocolEventToJson protocolEvents
                  , "lpSettlement" .= (txLpSettlementToJson <$> mLpSettlement)
                  , "events" .= map (recentTradeToJson protBundle) rows
                  ]

classifyTxKind :: Int -> Bool -> Bool -> Text
classifyTxKind finalizedCount hasLpSettlement hasLiquidation
  | finalizedCount > 1 = "batch_finalization"
  | finalizedCount == 1 = "order_finalization"
  | hasLpSettlement = "lp_settlement"
  | hasLiquidation = "liquidation"
  | otherwise = "protocol_tx"

{-# NOINLINE keepersStatusCache #-}
keepersStatusCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
keepersStatusCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (15 * 1000000000) (300 * 1000000000))

{-# NOINLINE keeperFleet24hCache #-}
keeperFleet24hCache :: SingleFlightCache () KeeperFleet24h
keeperFleet24hCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (15 * 1000000000) (300 * 1000000000))

{-# NOINLINE keepersActivityCache #-}
keepersActivityCache :: SingleFlightCache Int (Either ApiError (ApiResponse Value))
keepersActivityCache =
  unsafePerformIO (newConcurrentRefreshingSingleFlightCache 16 (15 * 1000000000) (300 * 1000000000))

{-# NOINLINE keeperDetailCache #-}
keeperDetailCache :: SingleFlightCache (Text, Int) (Either ApiError (ApiResponse Value))
keeperDetailCache =
  unsafePerformIO (newConcurrentRefreshingSingleFlightCache 64 (15 * 1000000000) (300 * 1000000000))

getCachedKeeperFleet24h :: DbPool -> EthClient -> Config -> Integer -> IO KeeperFleet24h
getCachedKeeperFleet24h pool client cfg sinceUnix =
  snd <$> runSingleFlightCache keeperFleet24hCache () (const True) (fetchKeeperFleet24h pool client cfg sinceUnix)

getKeepersStatusResponse
  :: DbPool
  -> EthClient
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getKeepersStatusResponse pool client cfg =
  snd <$> runSingleFlightCache keepersStatusCache () isRight (computeKeepersStatusResponse pool client cfg)

computeKeepersStatusResponse
  :: DbPool
  -> EthClient
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
computeKeepersStatusResponse pool client cfg = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
  fleet <- getCachedKeeperFleet24h pool client cfg (now - 86400)
  let aggregated =
        mergeKeeperFleetAggregates
          (kfOrderAggs fleet)
          (kfNonOrderEvents fleet)
          (fromIntegral (kfExecutions24h fleet))
      topKeepers = take 30 aggregated
  balances <- fetchKeeperEthBalancesMulticall client (map kaAddress topKeepers)
  pure $
    Right $
      mkResponse (kfLatestBlock fleet) chainId $
        object
          [ "network" .= networkMetaJson cfg
          , "observedAt" .= isoTimestamp now
          , "summary"
              .= object
                [ "activeKeepers24h" .= length aggregated
                , "executions24h" .= kfExecutions24h fleet
                , "latencyP50Seconds" .= kfLatencyP50Seconds fleet
                , "latencyP95Seconds" .= kfLatencyP95Seconds fleet
                , "bountiesPaid24hAssets" .= show (kfBountiesPaid24hAssets fleet)
                ]
          , "keepers" .= zipWith keeperAggregateToJson topKeepers balances
          ]

getKeepersActivityResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
getKeepersActivityResponse pool client cfg requestedLimit =
  let limit = min 100 $ max 1 requestedLimit
   in snd
        <$> runSingleFlightCache
          keepersActivityCache
          limit
          isRight
          (computeKeepersActivityResponse pool client cfg limit)

computeKeepersActivityResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
computeKeepersActivityResponse pool client cfg limit = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
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

getKeeperDetailResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
getKeeperDetailResponse pool client cfg rawAddress requestedLimit = do
  let addr = T.toLower (T.strip rawAddress)
      limit = min 100 $ max 1 requestedLimit
  if not (isValidAddress addr)
    then pure $ Left $ E.invalidAddress rawAddress
    else
      snd
        <$> runSingleFlightCache
          keeperDetailCache
          (addr, limit)
          isRight
          (computeKeeperDetailResponse pool client cfg addr limit)

computeKeeperDetailResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
computeKeeperDetailResponse pool client cfg addr limit = do
      now <- floor <$> getPOSIXTime
      let chainId = cfgPerpsChainId cfg
          router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
          sinceUnix = now - 86400
      (fleet, ethBal) <-
        runConcurrently $
          (,)
            <$> Concurrently (getCachedKeeperFleet24h pool client cfg sinceUnix)
            <*> Concurrently (fetchKeeperEthBalance client addr)
      (orderLatStats, recentOrderEvents) <-
        fetchKeeperOrderDetail24h pool chainId router addr sinceUnix limit
      let mOrderAgg = listToMaybe [row | row <- kfOrderAggs fleet, T.toLower (koarAddress row) == addr]
          nonOrderForKeeper =
            filter (\e -> T.toLower (dkeKeeperAddress e) == addr) (kfNonOrderEvents fleet)
          orderExecCount = maybe 0 koarExecutions24h mOrderAgg
          execCount = orderExecCount + length nonOrderForKeeper
          totalFleet = max (fromIntegral (kfExecutions24h fleet) :: Integer) (fromIntegral execCount)
          nonOrderLatStats = computeLatencyPercentiles $ mapMaybe dkeLatencySeconds nonOrderForKeeper
          latencyStats =
            if lsSampleCount orderLatStats > 0
              then orderLatStats
              else nonOrderLatStats
          bounties = sum $ mapMaybe dkeBountyAssets nonOrderForKeeper
          roles =
            sort $
              nub $
                (if orderExecCount > 0 then ["order_execution"] else [])
                  <> map dkeRole nonOrderForKeeper
          allBlocks =
            maybe [] (pure . koarLastActiveBlock) mOrderAgg
              <> map dkeBlockNumber nonOrderForKeeper
          allTimestamps =
            maybe [] (pure . koarLastActiveTimestamp) mOrderAgg
              <> map dkeTimestamp nonOrderForKeeper
          mLastBlock = case allBlocks of
            [] -> Nothing
            xs -> Just (maximum xs)
          mLastTs = case allTimestamps of
            [] -> Nothing
            xs -> Just (maximum xs)
          orderRoleBreakdown =
            [ object
                [ "role" .= ("order_execution" :: Text)
                , "executions24h" .= orderExecCount
                , "shareBps" .= rateBps (fromIntegral orderExecCount) (fromIntegral execCount)
                , "latencyP50Seconds" .= (lsP50 orderLatStats <|> (mOrderAgg >>= koarLatencyP50Seconds))
                , "latencyP95Seconds" .= (lsP95 orderLatStats <|> (mOrderAgg >>= koarLatencyP95Seconds))
                , "bountiesEarnedAssets" .= ("0" :: Text)
                ]
            | orderExecCount > 0
            ]
          nonOrderRoles = sort $ nub $ map dkeRole nonOrderForKeeper
          nonOrderRoleBreakdown =
            [ let rEvs = filter (\e -> dkeRole e == r) nonOrderForKeeper
                  rLat = computeLatencyPercentiles $ mapMaybe dkeLatencySeconds rEvs
                  rCount = length rEvs
                  rBounty = sum $ mapMaybe dkeBountyAssets rEvs
               in object
                    [ "role" .= r
                    , "executions24h" .= rCount
                    , "shareBps" .= rateBps (fromIntegral rCount) (fromIntegral execCount)
                    , "latencyP50Seconds" .= lsP50 rLat
                    , "latencyP95Seconds" .= lsP95 rLat
                    , "bountiesEarnedAssets" .= show rBounty
                    ]
            | r <- nonOrderRoles
            ]
          roleBreakdown =
            sortOn
              ( \v -> case v of
                  Object o -> case KM.lookup (Key.fromText "role") o of
                    Just (String r) -> r
                    _ -> ""
                  _ -> ""
              )
              (orderRoleBreakdown <> nonOrderRoleBreakdown)
          activity =
            take limit $
              sortOn (\e -> (Down (dkeBlockNumber e), Down (dkeTimestamp e))) $
                recentOrderEvents <> nonOrderForKeeper
          latestBlock = fromMaybe 0 mLastBlock
      pure $
        Right $
          mkResponse latestBlock chainId $
            object
              [ "network" .= networkMetaJson cfg
              , "observedAt" .= isoTimestamp now
              , "keeper"
                  .= object
                    [ "address" .= addr
                    , "roles" .= roles
                    , "executions24h" .= execCount
                    , "shareBps" .= rateBps (fromIntegral execCount) totalFleet
                    , "latency" .= latencyStatsToJson latencyStats
                    , "ethBalanceWei" .= (show <$> ethBal)
                    , "bountiesEarnedAssets" .= show bounties
                    , "lastActiveBlock" .= (show <$> mLastBlock)
                    , "lastActiveAt" .= (isoTimestamp <$> mLastTs)
                    ]
              , "roleBreakdown" .= roleBreakdown
              , "activity" .= map keeperEventToJson activity
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

computeLatencyPercentileRank :: [Integer] -> Integer -> Maybe Int
computeLatencyPercentileRank [] _ = Nothing
computeLatencyPercentileRank samples value =
  let n = length samples
      less = length (filter (< value) samples)
      equal = length (filter (== value) samples)
      rawPct =
        round
          (100.0 * (fromIntegral less + 0.5 * fromIntegral equal) / fromIntegral n :: Double)
          :: Int
   in Just (max 1 (min 99 rawPct))

buildLatencyBenchmark
  :: Text
  -> [Integer]
  -> [Integer]
  -> [Integer]
  -> ProtocolLatencyBenchmark
buildLatencyBenchmark windowLabel rawTotal rawCommitToOracle rawOracleToFinalize =
  let sortedTotal = sort (filter (>= 0) rawTotal)
      count = length sortedTotal
      atPct xs p =
        let c = length xs
         in xs !! min (c - 1) ((c * p + 99) `div` 100 - 1)
      medianSigned xs =
        case sort xs of
          [] -> Nothing
          ys -> Just (atPct ys 50)
   in case sortedTotal of
        [] ->
          ProtocolLatencyBenchmark
            windowLabel [] 0 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        _ ->
          ProtocolLatencyBenchmark
            { plbWindowLabel = windowLabel
            , plbSamples = sortedTotal
            , plbSampleCount = count
            , plbMinSeconds = Just (head sortedTotal)
            , plbP25Seconds = Just (atPct sortedTotal 25)
            , plbP50Seconds = Just (atPct sortedTotal 50)
            , plbP75Seconds = Just (atPct sortedTotal 75)
            , plbP95Seconds = Just (atPct sortedTotal 95)
            , plbP99Seconds = Just (atPct sortedTotal 99)
            , plbMaxSeconds = Just (last sortedTotal)
            , plbP50CommitToOracleSeconds = medianSigned rawCommitToOracle
            , plbP50OracleToFinalizeSeconds = medianSigned rawOracleToFinalize
            }

getCachedProtocolLatencyBenchmark :: DbPool -> Integer -> Text -> IO (Maybe ProtocolLatencyBenchmark)
getCachedProtocolLatencyBenchmark pool chainId router =
  snd
    <$> runSingleFlightCache
      protocolLatencyBenchmarkCache
      (chainId, router)
      (maybe False (\b -> plbSampleCount b > 0))
      (fetchProtocolLatencyBenchmark pool chainId router)

fetchProtocolLatencyBenchmark :: DbPool -> Integer -> Text -> IO (Maybe ProtocolLatencyBenchmark)
fetchProtocolLatencyBenchmark pool chainId router = do
  now <- floor <$> getPOSIXTime :: IO Integer
  let since7d = now - 7 * 86400
      since30d = now - 30 * 86400
      fetchWindow :: Connection -> Integer -> IO [(Integer, Maybe Integer, Maybe Integer)]
      fetchWindow conn sinceTs =
        query
          conn
          "SELECT \
          \  GREATEST(0, terminal_timestamp - commit_timestamp), \
          \  CASE WHEN oracle_max_publish_time IS NOT NULL THEN oracle_max_publish_time - commit_timestamp ELSE NULL END, \
          \  CASE WHEN oracle_max_publish_time IS NOT NULL THEN terminal_timestamp - oracle_max_publish_time ELSE NULL END \
          \FROM perps_orders \
          \WHERE chain_id = ? AND order_router = ? \
          \  AND terminal_status = 'Executed' \
          \  AND commit_timestamp IS NOT NULL AND terminal_timestamp IS NOT NULL \
          \  AND terminal_timestamp >= commit_timestamp \
          \  AND terminal_timestamp >= ? \
          \ORDER BY terminal_timestamp DESC \
          \LIMIT 5000"
          (chainId, router, sinceTs)
  (windowLabel, rows) <-
    withDb pool $ \conn -> do
      rows7d <- fetchWindow conn since7d
      if length rows7d >= 20
        then pure ("7d" :: Text, rows7d)
        else do
          rows30d <- fetchWindow conn since30d
          pure (if null rows30d then "7d" else "30d", rows30d)
  let totals = [t | (t, _, _) <- rows]
      cToO = [d | (_, Just d, _) <- rows]
      oToF = [d | (_, _, Just d) <- rows]
      bench = buildLatencyBenchmark windowLabel totals cToO oToF
  pure $ if plbSampleCount bench > 0 then Just bench else Nothing

protocolLatencyBenchmarkToJson :: ProtocolLatencyBenchmark -> Value
protocolLatencyBenchmarkToJson ProtocolLatencyBenchmark {..} =
  object
    [ "windowLabel" .= plbWindowLabel
    , "sampleCount" .= plbSampleCount
    , "minSeconds" .= plbMinSeconds
    , "p25Seconds" .= plbP25Seconds
    , "p50Seconds" .= plbP50Seconds
    , "p75Seconds" .= plbP75Seconds
    , "p95Seconds" .= plbP95Seconds
    , "p99Seconds" .= plbP99Seconds
    , "maxSeconds" .= plbMaxSeconds
    , "p50CommitToOracleSeconds" .= plbP50CommitToOracleSeconds
    , "p50OracleToFinalizeSeconds" .= plbP50OracleToFinalizeSeconds
    ]

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

mergeKeeperFleetAggregates
  :: [KeeperOrderAggregateRow]
  -> [DetectedKeeperEvent]
  -> Integer
  -> [KeeperAggregate]
mergeKeeperFleetAggregates orderAggs nonOrderEvents fleetTotal =
  let orderMap =
        Map.fromList
          [(T.toLower (koarAddress row), row) | row <- orderAggs, koarExecutions24h row > 0]
      nonOrderMap =
        foldr
          (\ev acc -> Map.insertWith (++) (T.toLower (dkeKeeperAddress ev)) [ev] acc)
          Map.empty
          nonOrderEvents
      allAddrs = nub (Map.keys orderMap <> Map.keys nonOrderMap)
      sumAttributed =
        sum
          [ maybe 0 koarExecutions24h (Map.lookup addr orderMap)
              + length (Map.findWithDefault [] addr nonOrderMap)
          | addr <- allAddrs
          ]
      shareDenom = max fleetTotal (fromIntegral sumAttributed)
      buildOne addr =
        let mOrder = Map.lookup addr orderMap
            nonOrderEvs = Map.findWithDefault [] addr nonOrderMap
            orderCount = maybe 0 koarExecutions24h mOrder
            totalExecs = orderCount + length nonOrderEvs
            nonOrderLat = computeLatencyPercentiles $ mapMaybe dkeLatencySeconds nonOrderEvs
            roles =
              sort $
                nub $
                  (if orderCount > 0 then ["order_execution"] else [])
                    <> map dkeRole nonOrderEvs
            bounties = sum $ mapMaybe dkeBountyAssets nonOrderEvs
            lastBlock =
              maximum $
                0
                  : maybe [] (pure . koarLastActiveBlock) mOrder
                    <> map dkeBlockNumber nonOrderEvs
            lastTs =
              maximum $
                0
                  : maybe [] (pure . koarLastActiveTimestamp) mOrder
                    <> map dkeTimestamp nonOrderEvs
         in KeeperAggregate
              { kaAddress = addr
              , kaRoles = roles
              , kaExecutions24h = totalExecs
              , kaShareBps = rateBps (fromIntegral totalExecs) shareDenom
              , kaLatencyP50Seconds = (mOrder >>= koarLatencyP50Seconds) <|> lsP50 nonOrderLat
              , kaLatencyP95Seconds = (mOrder >>= koarLatencyP95Seconds) <|> lsP95 nonOrderLat
              , kaBountiesEarnedAssets = bounties
              , kaLastActiveBlock = lastBlock
              , kaLastActiveTimestamp = lastTs
              }
   in sortOn (\k -> (Down (kaExecutions24h k), Down (kaLastActiveBlock k), kaAddress k)) $
        map buildOne allAddrs

mergeLpSettlementTxs :: [RawKeeperTx] -> [RawKeeperTx] -> [RawKeeperTx]
mergeLpSettlementTxs journalTxs canonicalLogTxs =
  let toMap =
        foldl
          ( \acc tx ->
              let key = T.toLower $ T.strip $ rktTxHash tx
               in Map.insertWith mergeOne key (tx {rktTxHash = key}) acc
          )
          Map.empty
      -- When a settlement tx exists in both the keeper journal and canonical
      -- on-chain logs, preserve journal timing/latency while taking the max
      -- non-zero block number and never double-counting the transaction.
      mergeOne incoming existing =
        existing
          { rktBlockNumber = max (rktBlockNumber existing) (rktBlockNumber incoming)
          , rktTimestamp =
              if rktTimestamp existing > 0
                then rktTimestamp existing
                else rktTimestamp incoming
          , rktLatencySeconds = rktLatencySeconds existing <|> rktLatencySeconds incoming
          , rktBountyAssets = rktBountyAssets existing <|> rktBountyAssets incoming
          }
      merged = toMap (journalTxs <> canonicalLogTxs)
   in sortOn (Down . rktBlockNumber) (Map.elems merged)

parseHexInteger :: Text -> Maybe Integer
parseHexInteger raw =
  let stripped = T.strip raw
      hexBody = fromMaybe stripped (T.stripPrefix "0x" stripped <|> T.stripPrefix "0X" stripped)
   in if T.null hexBody
        then Nothing
        else case readHex (T.unpack hexBody) of
          [(n, "")] -> Just n
          _ -> Nothing

extractTxFrom :: Value -> Maybe Text
extractTxFrom (Object obj) =
  case KM.lookup (Key.fromText "from") obj of
    Just (String addr) | isValidAddress addr -> Just $ T.toLower $ T.strip addr
    _ -> Nothing
extractTxFrom _ = Nothing

fetchKeeperFleet24h
  :: DbPool
  -> EthClient
  -> Config
  -> Integer
  -> IO KeeperFleet24h
fetchKeeperFleet24h pool client cfg sinceUnix = do
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      housePool = T.toLower $ T.strip $ cfgPerpsHousePool cfg
      book = T.toLower Manifest.positionProtectionBookAddress
      monitor = T.toLower $ T.strip $ cfgPerpsSettlementMonitorLens cfg
  ((orderCount, mOrderP50, mOrderP95, maxOrderBlock), nonOrderTxs, uncachedOrderHashes, cachedNonOrder) <-
    withDb pool $ \conn ->
      withReadSnapshot conn $ do
        fleetSummaryRows <-
          query
            conn
            "SELECT \
            \  COUNT(*)::INT, \
            \  percentile_cont(0.50) WITHIN GROUP (ORDER BY terminal_timestamp - commit_timestamp) \
            \    FILTER (WHERE commit_timestamp IS NOT NULL AND terminal_timestamp IS NOT NULL AND terminal_timestamp >= commit_timestamp)::DOUBLE PRECISION, \
            \  percentile_cont(0.95) WITHIN GROUP (ORDER BY terminal_timestamp - commit_timestamp) \
            \    FILTER (WHERE commit_timestamp IS NOT NULL AND terminal_timestamp IS NOT NULL AND terminal_timestamp >= commit_timestamp)::DOUBLE PRECISION, \
            \  COALESCE(MAX(terminal_block_number), 0)::BIGINT \
            \FROM perps_orders \
            \WHERE chain_id = ? AND order_router = ? AND terminal_status = 'Executed' \
            \  AND terminal_tx_hash IS NOT NULL AND terminal_timestamp >= ?"
            (chainId, router, sinceUnix)
            :: IO [(Int, Maybe Double, Maybe Double, Integer)]
        let summary = fromMaybe (0, Nothing, Nothing, 0) (listToMaybe fleetSummaryRows)
        nonOrders <- fetchNonOrderPermissionlessTxs conn chainId router housePool book monitor sinceUnix 500
        missingOrders <-
          query
            conn
            "SELECT o.terminal_tx_hash \
            \FROM perps_orders o \
            \LEFT JOIN insights_tx_senders s \
            \  ON s.chain_id = o.chain_id AND s.tx_hash = o.terminal_tx_hash \
            \WHERE o.chain_id = ? AND o.order_router = ? AND o.terminal_status = 'Executed' \
            \  AND o.terminal_tx_hash IS NOT NULL AND o.terminal_timestamp >= ? \
            \  AND s.tx_hash IS NULL \
            \GROUP BY o.terminal_tx_hash \
            \ORDER BY MAX(COALESCE(o.terminal_block_number, 0)) DESC \
            \LIMIT ?"
            (chainId, router, sinceUnix, 60 :: Int)
            :: IO [Only Text]
        let nonOrderHashes = nub $ map (T.toLower . rktTxHash) nonOrders
        cached <- fetchCachedTxSenders conn chainId nonOrderHashes
        pure (summary, nonOrders, [T.toLower h | Only h <- missingOrders], cached)
  -- Always prioritize resolving low-volume roles (liquidations, protections, LP settlements)
  -- before filling the remaining RPC budget with newest uncached order-execution hashes.
  let missingNonOrder =
        filter (`Map.notMember` cachedNonOrder) (nub $ map (T.toLower . rktTxHash) nonOrderTxs)
      missingToResolve = take 60 $ nub (missingNonOrder <> uncachedOrderHashes)
  resolvedPairs <- mapConcurrently (resolveTxSenderRpc client) missingToResolve
  let newlyResolved = [(h, s) | (h, Just s) <- resolvedPairs]
  unless (null newlyResolved) $
    withDb pool $ \conn -> upsertCachedTxSenders conn chainId newlyResolved
  let nonOrderSenderMap = Map.union cachedNonOrder (Map.fromList newlyResolved)
  orderAggRows <- withDb pool $ \conn ->
    query
      conn
      "SELECT s.sender_address, \
      \  COUNT(*)::INT, \
      \  percentile_cont(0.50) WITHIN GROUP (ORDER BY o.terminal_timestamp - o.commit_timestamp) \
      \    FILTER (WHERE o.commit_timestamp IS NOT NULL AND o.terminal_timestamp IS NOT NULL AND o.terminal_timestamp >= o.commit_timestamp)::DOUBLE PRECISION, \
      \  percentile_cont(0.95) WITHIN GROUP (ORDER BY o.terminal_timestamp - o.commit_timestamp) \
      \    FILTER (WHERE o.commit_timestamp IS NOT NULL AND o.terminal_timestamp IS NOT NULL AND o.terminal_timestamp >= o.commit_timestamp)::DOUBLE PRECISION, \
      \  COALESCE(MAX(o.terminal_block_number), 0)::BIGINT, \
      \  COALESCE(MAX(o.terminal_timestamp), 0)::BIGINT \
      \FROM perps_orders o \
      \JOIN insights_tx_senders s \
      \  ON s.chain_id = o.chain_id AND s.tx_hash = o.terminal_tx_hash \
      \WHERE o.chain_id = ? AND o.order_router = ? AND o.terminal_status = 'Executed' \
      \  AND o.terminal_tx_hash IS NOT NULL AND o.terminal_timestamp >= ? \
      \GROUP BY s.sender_address"
      (chainId, router, sinceUnix)
      :: IO [(Text, Int, Maybe Double, Maybe Double, Integer, Integer)]
  let orderAggs =
        [ KeeperOrderAggregateRow addr cnt (round <$> p50) (round <$> p95) maxBlk maxTs
        | (addr, cnt, p50, p95, maxBlk, maxTs) <- orderAggRows
        ]
      nonOrderEvents =
        mapMaybe
          ( \raw -> do
              sender <- Map.lookup (T.toLower $ rktTxHash raw) nonOrderSenderMap
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
          nonOrderTxs
      nonOrderLat = computeLatencyPercentiles $ mapMaybe rktLatencySeconds nonOrderTxs
      fleetP50 = (round <$> mOrderP50) <|> lsP50 nonOrderLat
      fleetP95 = (round <$> mOrderP95) <|> lsP95 nonOrderLat
      totalBounties = sum $ mapMaybe rktBountyAssets nonOrderTxs
      latestBlock = maximum (0 : maxOrderBlock : map rktBlockNumber nonOrderTxs)
  pure
    KeeperFleet24h
      { kfExecutions24h = orderCount + length nonOrderTxs
      , kfLatencyP50Seconds = fleetP50
      , kfLatencyP95Seconds = fleetP95
      , kfBountiesPaid24hAssets = totalBounties
      , kfLatestBlock = latestBlock
      , kfOrderAggs = orderAggs
      , kfNonOrderEvents = nonOrderEvents
      }

fetchKeeperOrderDetail24h
  :: DbPool
  -> Integer
  -> Text
  -> Text
  -> Integer
  -> Int
  -> IO (LatencyStats, [DetectedKeeperEvent])
fetchKeeperOrderDetail24h pool chainId router addr sinceUnix limit =
  withDb pool $ \conn ->
    withReadSnapshot conn $ do
      latRows <-
        query
          conn
          "SELECT \
          \  COUNT(*) FILTER (WHERE o.commit_timestamp IS NOT NULL AND o.terminal_timestamp IS NOT NULL AND o.terminal_timestamp >= o.commit_timestamp)::INT, \
          \  percentile_cont(0.50) WITHIN GROUP (ORDER BY o.terminal_timestamp - o.commit_timestamp) \
          \    FILTER (WHERE o.commit_timestamp IS NOT NULL AND o.terminal_timestamp IS NOT NULL AND o.terminal_timestamp >= o.commit_timestamp)::DOUBLE PRECISION, \
          \  percentile_cont(0.95) WITHIN GROUP (ORDER BY o.terminal_timestamp - o.commit_timestamp) \
          \    FILTER (WHERE o.commit_timestamp IS NOT NULL AND o.terminal_timestamp IS NOT NULL AND o.terminal_timestamp >= o.commit_timestamp)::DOUBLE PRECISION, \
          \  percentile_cont(0.99) WITHIN GROUP (ORDER BY o.terminal_timestamp - o.commit_timestamp) \
          \    FILTER (WHERE o.commit_timestamp IS NOT NULL AND o.terminal_timestamp IS NOT NULL AND o.terminal_timestamp >= o.commit_timestamp)::DOUBLE PRECISION, \
          \  MAX(o.terminal_timestamp - o.commit_timestamp) \
          \    FILTER (WHERE o.commit_timestamp IS NOT NULL AND o.terminal_timestamp IS NOT NULL AND o.terminal_timestamp >= o.commit_timestamp)::BIGINT \
          \FROM perps_orders o \
          \JOIN insights_tx_senders s \
          \  ON s.chain_id = o.chain_id AND s.tx_hash = o.terminal_tx_hash \
          \WHERE o.chain_id = ? AND o.order_router = ? AND o.terminal_status = 'Executed' \
          \  AND o.terminal_tx_hash IS NOT NULL AND o.terminal_timestamp >= ? \
          \  AND s.sender_address = ?"
          (chainId, router, sinceUnix, addr)
          :: IO [(Int, Maybe Double, Maybe Double, Maybe Double, Maybe Integer)]
      let latStats = case latRows of
            [(cnt, p50, p95, p99, mx)] | cnt > 0 ->
              LatencyStats cnt (round <$> p50) (round <$> p95) (round <$> p99) mx
            _ -> LatencyStats 0 Nothing Nothing Nothing Nothing
      recentRows <-
        query
          conn
          "SELECT o.terminal_tx_hash, COALESCE(o.terminal_block_number, 0), COALESCE(o.terminal_timestamp, 0), \
          \  CASE WHEN o.commit_timestamp IS NOT NULL AND o.terminal_timestamp IS NOT NULL AND o.terminal_timestamp >= o.commit_timestamp \
          \    THEN o.terminal_timestamp - o.commit_timestamp ELSE NULL END \
          \FROM perps_orders o \
          \JOIN insights_tx_senders s \
          \  ON s.chain_id = o.chain_id AND s.tx_hash = o.terminal_tx_hash \
          \WHERE o.chain_id = ? AND o.order_router = ? AND o.terminal_status = 'Executed' \
          \  AND o.terminal_tx_hash IS NOT NULL AND o.terminal_timestamp >= ? \
          \  AND s.sender_address = ? \
          \ORDER BY o.terminal_block_number DESC \
          \LIMIT ?"
          (chainId, router, sinceUnix, addr, limit)
          :: IO [(Text, Integer, Integer, Maybe Integer)]
      let events =
            [ DetectedKeeperEvent "order_execution" addr h blk ts lat Nothing
            | (h, blk, ts, lat) <- recentRows
            ]
      pure (latStats, events)

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
      housePool = T.toLower $ T.strip $ cfgPerpsHousePool cfg
      book = T.toLower Manifest.positionProtectionBookAddress
      monitor = T.toLower $ T.strip $ cfgPerpsSettlementMonitorLens cfg
  (rawTxs, cached) <- withDb pool $ \conn ->
    withReadSnapshot conn $ do
      txs <- fetchRecentPermissionlessTxs conn chainId router housePool book monitor sinceUnix maxTxs
      let hashes = nub $ map (T.toLower . rktTxHash) txs
      senders <- fetchCachedTxSenders conn chainId hashes
      pure (txs, senders)
  let uniqueHashes = nub $ map (T.toLower . rktTxHash) rawTxs
      missing = take 40 $ filter (`Map.notMember` cached) uniqueHashes
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
  mRes <- timeout 2500000 $ rpcCall client "eth_getTransactionByHash" (toJSON [txHash])
  pure $ case mRes of
    Just (Right val) -> (txHash, extractTxFrom val)
    _ -> (txHash, Nothing)

fetchKeeperEthBalance :: EthClient -> Text -> IO (Maybe Integer)
fetchKeeperEthBalance client addr = do
  mRes <- timeout 2500000 $ ethGetBalance client addr
  pure $ case mRes of
    Just (Right bal) -> Just bal
    _ -> Nothing

-- | Batch up to 30 keeper ETH balance queries into 1 atomic Multicall3 call.
fetchKeeperEthBalancesMulticall :: EthClient -> [Text] -> IO [Maybe Integer]
fetchKeeperEthBalancesMulticall _ [] = pure []
fetchKeeperEthBalancesMulticall client addrs = do
  let calls =
        [ Multicall.Call
            { Multicall.callTarget = Multicall.multicallAddress
            , Multicall.callAllowFailure = True
            , Multicall.callCalldata = encodeCall "getEthBalance(address)" [encodeAddress a]
            }
        | a <- addrs
        ]
      dec r =
        if Multicall.resultSuccess r && BS8.length (Multicall.resultData r) == 32
          then Just (decodeUint256 (Multicall.resultData r))
          else Nothing
  mRes <- timeout 3000000 $ Multicall.multicall client calls
  pure $ case mRes of
    Just (Right results) | length results == length addrs -> map dec results
    _ -> map (const Nothing) addrs

ensureTxSenderCacheTable :: Connection -> IO ()
ensureTxSenderCacheTable conn = do
  existing <-
    query_
      conn
      "SELECT to_regclass(format('%I.insights_tx_senders', current_schema())) IS NOT NULL"
      :: IO [Only Bool]
  unless (existing == [Only True]) $
    catch
      ( do
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
      )
      ( \(err :: SqlError) ->
          if sqlState err `elem` ["23505", "42P07", "42710"]
            then pure ()
            else throwIO err
      )
  catch
    ( do
        _ <-
          execute_
            conn
            "CREATE INDEX IF NOT EXISTS idx_insights_tx_senders_sender \
            \ON insights_tx_senders (chain_id, sender_address, tx_hash)"
        pure ()
    )
    ( \(err :: SqlError) ->
        if sqlState err `elem` ["23505", "42P07", "42710"]
          then pure ()
          else throwIO err
    )

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
upsertCachedTxSenders _ _ [] = pure ()
upsertCachedTxSenders conn chainId pairs = do
  _ <-
    executeMany
      conn
      "INSERT INTO insights_tx_senders (chain_id, tx_hash, sender_address) \
      \VALUES (?, ?, ?) ON CONFLICT (chain_id, tx_hash) DO NOTHING"
      [(chainId, T.toLower txHash, T.toLower sender) | (txHash, sender) <- pairs]
  pure ()

fetchNonOrderPermissionlessTxs
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Int
  -> IO [RawKeeperTx]
fetchNonOrderPermissionlessTxs conn chainId router housePool book monitor sinceUnix limit = do
  let lpTopicHex = "0x" <> T.toLower (TE.decodeUtf8 (B16.encode Perps.lpEpochSettledTopic))
      nonOrderLimit = max 50 limit
  liqRows <-
    query
      conn
      "SELECT tx_hash, block_number, timestamp, \
      \CASE WHEN data->>'keeperBountyAssets' ~ '^[0-9]+$' THEN data->>'keeperBountyAssets' ELSE NULL END \
      \FROM perps_account_activity \
      \WHERE chain_id = ? AND release_router = ? AND activity_type = 'Liquidated' AND timestamp >= ? \
      \ORDER BY block_number DESC LIMIT ?"
      (chainId, router, sinceUnix, nonOrderLimit) :: IO [(Text, Integer, Integer, Maybe String)]
  protRows <-
    query
      conn
      "SELECT p.transaction_hash, p.block_number, \
      \COALESCE(\
      \  (SELECT e.timestamp FROM perps_events e \
      \   WHERE e.chain_id = p.chain_id AND e.tx_hash = p.transaction_hash LIMIT 1), \
      \  (SELECT s.block_timestamp FROM vault_performance_snapshots s \
      \   WHERE s.chain_id = p.chain_id AND s.house_pool_address = ? AND s.block_number <= p.block_number \
      \   ORDER BY s.block_number DESC LIMIT 1), \
      \  0\
      \) \
      \FROM perps_protection_events p \
      \WHERE p.chain_id = ? AND p.book = ? AND p.event_name = 'PositionProtectionTriggered' \
      \ORDER BY p.block_number DESC LIMIT ?"
      (housePool, chainId, book, nonOrderLimit) :: IO [(Text, Integer, Integer)]
  lpJournalRows <-
    query
      conn
      "SELECT receipt_transaction_hash, COALESCE(receipt_block_number, 0), created_at, confirmed_at \
      \FROM perps_lp_settlement_transactions \
      \WHERE chain_id = ? AND monitor_address = ? AND status = 'confirmed_success' \
      \  AND receipt_transaction_hash IS NOT NULL AND created_at >= NOW() - INTERVAL '24 hours' \
      \ORDER BY receipt_block_number DESC LIMIT ?"
      (chainId, monitor, min 200 nonOrderLimit) :: IO [(Text, Integer, UTCTime, Maybe UTCTime)]
  -- Canonical on-chain LpEpochSettled logs capture every permissionless caller,
  -- not only Plether-operated keeper journal entries.
  lpCanonicalRows <-
    query
      conn
      "SELECT l.transaction_hash, l.block_number::TEXT, l.payload->>'blockTimestamp', \
      \CASE WHEN l.payload->>'blockTimestamp' IS NULL THEN \
      \  (SELECT s.block_timestamp FROM vault_performance_snapshots s \
      \   WHERE s.chain_id = ? AND s.house_pool_address = ? AND s.block_number <= l.block_number \
      \   ORDER BY s.block_number DESC LIMIT 1) \
      \ELSE NULL END \
      \FROM vault_return_logs l \
      \WHERE LOWER(l.payload->>'address') = ? \
      \  AND LOWER(l.payload->'topics'->>0) = ? \
      \ORDER BY l.block_number DESC LIMIT ?"
      (chainId, housePool, housePool, lpTopicHex, min 200 nonOrderLimit) :: IO [(Text, String, Maybe Text, Maybe Integer)]
  let liqs =
        [ RawKeeperTx "liquidation" h blk ts Nothing (bounty >>= readMaybe)
        | (h, blk, ts, bounty) <- liqRows
        ]
      prots =
        [ RawKeeperTx "protection" h blk ts Nothing Nothing
        | (h, blk, ts) <- protRows
        , ts >= sinceUnix
        ]
      lpJournal =
        [ RawKeeperTx
            "lp_settlement"
            h
            blk
            (floor $ utcTimeToPOSIXSeconds $ fromMaybe created confirmed)
            ((\c -> max 0 $ round (diffUTCTime c created)) <$> confirmed)
            Nothing
        | (h, blk, created, confirmed) <- lpJournalRows
        ]
      lpCanonical =
        [ RawKeeperTx "lp_settlement" h blk ts Nothing Nothing
        | (h, blkStr, blockTsHex, snapshotTs) <- lpCanonicalRows
        , Just blk <- [readMaybe blkStr]
        , let ts = fromMaybe 0 ((blockTsHex >>= parseHexInteger) <|> snapshotTs)
        , ts >= sinceUnix
        ]
      lps = mergeLpSettlementTxs lpJournal lpCanonical
  pure (liqs <> prots <> lps)

fetchRecentPermissionlessTxs
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Int
  -> IO [RawKeeperTx]
fetchRecentPermissionlessTxs conn chainId router housePool book monitor sinceUnix limit = do
  orderRows <-
    query
      conn
      "SELECT terminal_tx_hash, COALESCE(terminal_block_number, 0), COALESCE(terminal_timestamp, 0), \
      \CASE WHEN commit_timestamp IS NOT NULL AND terminal_timestamp IS NOT NULL AND terminal_timestamp >= commit_timestamp \
      \  THEN terminal_timestamp - commit_timestamp ELSE NULL END \
      \FROM perps_orders \
      \WHERE chain_id = ? AND order_router = ? AND terminal_status = 'Executed' \
      \  AND terminal_tx_hash IS NOT NULL AND terminal_timestamp >= ? \
      \ORDER BY terminal_block_number DESC LIMIT ?"
      (chainId, router, sinceUnix, limit) :: IO [(Text, Integer, Integer, Maybe Integer)]
  nonOrders <- fetchNonOrderPermissionlessTxs conn chainId router housePool book monitor sinceUnix (min 50 limit)
  let orders =
        [ RawKeeperTx "order_execution" h blk ts lat Nothing
        | (h, blk, ts, lat) <- orderRows
        ]
  pure $ take limit $ sortOn (Down . rktBlockNumber) (orders <> nonOrders)

fetchOrderReliabilityBuckets
  :: Connection
  -> Text
  -> Integer
  -> IO (Map.Map Integer [OrderReliabilityBucket])
fetchOrderReliabilityBuckets conn router cohortEnd = do
  rows <-
    query
      conn
      "WITH cohort AS MATERIALIZED (\
      \ SELECT commit_time, status, failure_reason \
      \ FROM perps_keeper_orders \
      \ WHERE order_router = ? AND commit_time >= ? - 86400 AND commit_time < ?\
      \), \
      \specs(window_seconds, bucket_count, bucket_seconds) AS (\
      \ VALUES (3600::bigint, 30::int, 120::bigint), \
      \        (86400::bigint, 48::int, 1800::bigint)\
      \) \
      \SELECT \
      \ s.window_seconds, \
      \ LEAST(s.bucket_count - 1, GREATEST(0, ((c.commit_time - (? - s.window_seconds)) / s.bucket_seconds)::int)) AS bucket_idx, \
      \ COUNT(*)::bigint, \
      \ COUNT(*) FILTER (WHERE c.status = 'executed')::bigint, \
      \ COUNT(*) FILTER (WHERE c.status = 'failed' AND c.failure_reason = 2)::bigint, \
      \ COUNT(*) FILTER (WHERE c.status = 'failed' AND c.failure_reason IS DISTINCT FROM 2)::bigint, \
      \ COUNT(*) FILTER (WHERE c.status NOT IN ('executed', 'failed'))::bigint \
      \FROM specs s \
      \JOIN cohort c ON c.commit_time >= ? - s.window_seconds \
      \GROUP BY s.window_seconds, bucket_idx \
      \ORDER BY s.window_seconds, bucket_idx"
      (router, cohortEnd, cohortEnd, cohortEnd, cohortEnd)
      :: IO [(Integer, Int, Integer, Integer, Integer, Integer, Integer)]
  let rowsFor win =
        [ (idx, tot, execC, expC, failC, pendC)
        | (w, idx, tot, execC, expC, failC, pendC) <- rows
        , w == win
        ]
  pure $
    Map.fromList
      [ (3600, buildTimelineBuckets cohortEnd 3600 30 (rowsFor 3600))
      , (86400, buildTimelineBuckets cohortEnd 86400 48 (rowsFor 86400))
      ]

buildTimelineBuckets
  :: Integer
  -> Integer
  -> Int
  -> [(Int, Integer, Integer, Integer, Integer, Integer)]
  -> [OrderReliabilityBucket]
buildTimelineBuckets cohortEnd windowSecs bucketCount rawRows
  | bucketCount <= 0 || windowSecs <= 0 = []
  | otherwise =
      let windowStart = cohortEnd - windowSecs
          bucketWidth = windowSecs `div` fromIntegral bucketCount
          byIdx =
            Map.fromListWith
              (\(t1, e1, x1, f1, p1) (t2, e2, x2, f2, p2) -> (t1 + t2, e1 + e2, x1 + x2, f1 + f2, p1 + p2))
              [ (max 0 (min (bucketCount - 1) idx), (tot, execC, expC, failC, pendC))
              | (idx, tot, execC, expC, failC, pendC) <- rawRows
              ]
          mkBucket i =
            let startTs = windowStart + fromIntegral i * bucketWidth
                endTs = if i == bucketCount - 1 then cohortEnd else startTs + bucketWidth
                (tot, execC, expC, failC, pendC) = Map.findWithDefault (0, 0, 0, 0, 0) i byIdx
             in OrderReliabilityBucket startTs endTs tot execC expC failC pendC
       in map mkBucket [0 .. bucketCount - 1]

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
      "SELECT last_indexed_block, updated_at AT TIME ZONE 'UTC' FROM perps_liquidation_state \
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

recentTradeSelectSql :: Query
recentTradeSelectSql =
  "SELECT a.activity_type, a.account, a.order_id, a.side, a.price, a.size_delta, \
  \a.amount_assets, a.pnl_assets, o.execution_vpi_assets, o.execution_frozen_close_spread_assets, \
  \o.execution_oracle_frozen, \
  \CASE WHEN o.commit_timestamp IS NOT NULL AND o.terminal_timestamp IS NOT NULL AND o.terminal_timestamp >= o.commit_timestamp \
  \  THEN o.terminal_timestamp - o.commit_timestamp ELSE NULL END, \
  \CASE WHEN a.data->>'keeperBountyAssets' ~ '^[0-9]+$' THEN (a.data->>'keeperBountyAssets')::NUMERIC ELSE NULL END, \
  \a.tx_hash, a.block_number, a.log_index, a.timestamp, execution.payload \
  \FROM perps_account_activity a \
  \LEFT JOIN perps_orders o \
  \  ON o.chain_id = a.chain_id AND o.order_router = a.release_router AND o.order_id = a.order_id \
  \LEFT JOIN LATERAL (\
  \  SELECT CASE WHEN COUNT(*) = 1 THEN (jsonb_agg(candidate.payload))->0 ELSE NULL END AS payload \
  \  FROM (\
  \    SELECT jsonb_build_object(\
  \      'orderId', o2.order_id::text, 'protocolVersion', 'v1.2.3', \
  \      'status', CASE WHEN o2.settlement_evidence_version = 1 THEN 'complete' ELSE 'pending' END, \
  \      'receipt', o2.receipt_economics, \
  \      'settlements', COALESCE((\
  \        SELECT jsonb_agg(e.data || jsonb_build_object('kind', e.event_name) ORDER BY e.log_index) \
  \        FROM perps_events e \
  \        WHERE e.chain_id = a.chain_id AND e.release_router = a.release_router \
  \          AND e.tx_hash = a.tx_hash AND e.block_hash = a.block_hash AND e.block_number = a.block_number \
  \          AND e.account = a.account AND e.order_id = o2.order_id \
  \          AND e.event_name IN ('ActionChargeSettled', 'ActionRebateSettled', 'FrozenCloseSpreadSettled')\
  \      ), '[]'::jsonb)\
  \    ) AS payload \
  \    FROM perps_events terminal \
  \    JOIN perps_orders o2 ON o2.chain_id = terminal.chain_id AND o2.order_router = terminal.release_router \
  \      AND o2.order_id = terminal.order_id AND o2.terminal_status = 'Executed' \
  \      AND o2.account = a.account AND o2.terminal_tx_hash = a.tx_hash AND o2.terminal_block_number = a.block_number \
  \      AND o2.receipt_economics IS NOT NULL AND o2.receipt_hash = terminal.data->>'receiptHash' \
  \    WHERE terminal.chain_id = a.chain_id AND terminal.release_router = a.release_router \
  \      AND terminal.tx_hash = a.tx_hash AND terminal.block_hash = a.block_hash AND terminal.block_number = a.block_number \
  \      AND terminal.account = a.account AND terminal.event_name = 'OrderFinalized' \
  \      AND terminal.data->>'status' = '2' AND terminal.data->>'terminalReason' = 'Executed' \
  \      AND terminal.log_index > a.log_index AND a.activity_type IN ('Open', 'Close') \
  \      AND NOT EXISTS (\
  \        SELECT 1 FROM perps_events prior WHERE prior.chain_id = a.chain_id AND prior.release_router = a.release_router \
  \          AND prior.tx_hash = a.tx_hash AND prior.block_hash = a.block_hash \
  \          AND prior.event_name = 'OrderFinalized' AND prior.log_index > a.log_index AND prior.log_index < terminal.log_index\
  \      ) \
  \      AND 1 = (\
  \        SELECT COUNT(*) FROM perps_account_activity sibling \
  \        WHERE sibling.chain_id = a.chain_id AND sibling.release_router = a.release_router \
  \          AND sibling.block_number = a.block_number \
  \          AND sibling.tx_hash = a.tx_hash AND sibling.block_hash = a.block_hash AND sibling.account = a.account \
  \          AND sibling.activity_type IN ('Open', 'Close') AND sibling.log_index < terminal.log_index \
  \          AND sibling.log_index > COALESCE((\
  \            SELECT MAX(prior.log_index) FROM perps_events prior \
  \            WHERE prior.chain_id = a.chain_id AND prior.release_router = a.release_router \
  \              AND prior.tx_hash = a.tx_hash AND prior.block_hash = a.block_hash \
  \              AND prior.event_name = 'OrderFinalized' AND prior.log_index < terminal.log_index\
  \          ), -1)\
  \      )\
  \  ) candidate\
  \) execution ON TRUE "

fetchRecentTrades :: Connection -> Integer -> Text -> Int -> IO [RecentTradeRow]
fetchRecentTrades conn chainId router limit =
  query
    conn
    ( recentTradeSelectSql
        <> "WHERE a.chain_id = ? AND a.release_router = ? \
           \  AND a.activity_type IN ('Open', 'Close', 'Liquidated') \
           \ORDER BY a.block_number DESC, a.log_index DESC \
           \LIMIT ?"
    )
    (chainId, router, limit)

fetchTransactionTrades :: Connection -> Integer -> Text -> Text -> IO [RecentTradeRow]
fetchTransactionTrades conn chainId router txHash =
  query
    conn
    ( recentTradeSelectSql
        <> "WHERE a.chain_id = ? AND a.release_router = ? \
           \  AND a.tx_hash = ? \
           \ORDER BY a.log_index ASC \
           \LIMIT 100"
    )
    (chainId, router, txHash)

fetchTransactionFinalizedOrders :: Connection -> Integer -> Text -> Text -> IO [FinalizedOrderRow]
fetchTransactionFinalizedOrders conn chainId router txHash =
  query
    conn
    "SELECT order_id, account, side, commit_tx_hash, commit_block_number, commit_timestamp, \
    \  terminal_block_number, terminal_timestamp, terminal_status, terminal_reason, \
    \  failure_reason, failed_constraint, execution_mode, execution_price, execution_oracle_price, \
    \  execution_oracle_frozen, oracle_min_publish_time, oracle_max_publish_time, \
    \  execution_vpi_assets, execution_frozen_close_spread_assets \
    \FROM perps_orders \
    \WHERE chain_id = ? AND order_router = ? AND terminal_tx_hash = ? \
    \ORDER BY order_id ASC \
    \LIMIT 200"
    (chainId, router, txHash)

fetchTransactionProtocolEvents :: Connection -> Integer -> Text -> Text -> IO [ProtocolEventRow]
fetchTransactionProtocolEvents conn chainId router txHash =
  query
    conn
    "SELECT log_index, block_number, timestamp, contract_address, event_name, \
    \  account, order_id, side, data \
    \FROM perps_events \
    \WHERE chain_id = ? AND release_router = ? AND tx_hash = ? \
    \ORDER BY log_index ASC \
    \LIMIT 200"
    (chainId, router, txHash)

fetchTransactionLpSettlement :: Connection -> Integer -> Text -> IO (Maybe TxLpSettlementRow)
fetchTransactionLpSettlement conn chainId txHash = do
  rows <-
    query
      conn
      "SELECT epoch, status, receipt_block_number, confirmed_at, created_at, \
      \  senior_deposit_assets, senior_redeem_assets, junior_deposit_assets, junior_redeem_assets, \
      \  senior_backlog, junior_backlog, entries_deferred \
      \FROM perps_lp_settlement_transactions \
      \WHERE chain_id = ? AND receipt_transaction_hash = ? \
      \ORDER BY id DESC LIMIT 1"
      (chainId, txHash)
  pure (listToMaybe rows)

fetchOracleUpdateIdsForOrders :: Connection -> [FinalizedOrderRow] -> IO (Map.Map (Integer, Integer) Integer)
fetchOracleUpdateIdsForOrders conn orders = do
  let windows =
        nub
          [ (mn, mx)
          | o <- orders
          , Just mn <- [forOracleMinPublishTime o]
          , Just mx <- [forOracleMaxPublishTime o]
          ]
  pairs <-
    forM windows $ \(mn, mx) -> do
      rows <-
        query
          conn
          "SELECT id FROM perps_pyth_update_payloads \
          \WHERE min_publish_time = ? AND max_publish_time = ? \
          \ORDER BY id DESC LIMIT 1"
          (mn, mx)
          :: IO [Only Integer]
      pure $ case rows of
        (Only pid : _) -> Just ((mn, mx), pid)
        [] -> Nothing
  pure $ Map.fromList (catMaybes pairs)

lookupOrderOracleUpdateId :: Map.Map (Integer, Integer) Integer -> FinalizedOrderRow -> Maybe Integer
lookupOrderOracleUpdateId updateMap order = do
  mn <- forOracleMinPublishTime order
  mx <- forOracleMaxPublishTime order
  Map.lookup (mn, mx) updateMap

batchSummaryToJson :: Map.Map (Integer, Integer) Integer -> [FinalizedOrderRow] -> Value
batchSummaryToJson updateMap orders =
  let totalOrders = length orders
      executedOrders = length $ filter (\o -> forTerminalStatus o == "Executed") orders
      failedOrders = totalOrders - executedOrders
      uniqueAccounts = length $ nub $ map T.toLower $ mapMaybe forAccount orders
      latencies =
        mapMaybe
          (\o -> (\t c -> max 0 (t - c)) <$> forTerminalTimestamp o <*> forCommitTimestamp o)
          orders
      latStats = computeLatencyPercentiles latencies
      minLat = case sort latencies of
        (x : _) -> Just x
        [] -> Nothing
      totalVpi = sum $ mapMaybe forVpiAssets orders
      oracleMin = case mapMaybe forOracleMinPublishTime orders of
        [] -> Nothing
        xs -> Just (minimum xs)
      oracleMax = case mapMaybe forOracleMaxPublishTime orders of
        [] -> Nothing
        xs -> Just (maximum xs)
      oraclePrice = listToMaybe $ mapMaybe forOraclePrice orders
      oracleUpdateId =
        ((,) <$> oracleMin <*> oracleMax >>= (`Map.lookup` updateMap))
          <|> listToMaybe (mapMaybe (lookupOrderOracleUpdateId updateMap) orders)
      oracleFrozen = any (== Just True) (map forOracleFrozen orders)
   in object
        [ "totalOrders" .= totalOrders
        , "executedOrders" .= executedOrders
        , "failedOrExpiredOrders" .= failedOrders
        , "uniqueAccounts" .= uniqueAccounts
        , "minLatencySeconds" .= minLat
        , "medianLatencySeconds" .= lsP50 latStats
        , "maxLatencySeconds" .= lsMax latStats
        , "totalVpiAssets" .= show totalVpi
        , "oracleMinPublishTime" .= (isoTimestamp <$> oracleMin)
        , "oracleMaxPublishTime" .= (isoTimestamp <$> oracleMax)
        , "oracleSpreadSeconds" .= ((\mx mn -> max 0 (mx - mn)) <$> oracleMax <*> oracleMin)
        , "oraclePrice" .= (show <$> oraclePrice)
        , "oracleUpdateId" .= (show <$> oracleUpdateId)
        , "oracleFrozen" .= oracleFrozen
        ]

finalizedOrderToJson :: Map.Map (Integer, Integer) Integer -> OrderProtectionsBundle -> Maybe ProtocolLatencyBenchmark -> FinalizedOrderRow -> Value
finalizedOrderToJson updateMap protBundle mBench row@FinalizedOrderRow {..} =
  let latencySecs = (\t c -> max 0 (t - c)) <$> forTerminalTimestamp <*> forCommitTimestamp
      latencyBlks = (\t c -> max 0 (t - c)) <$> forTerminalBlock <*> forCommitBlock
      oracleSpread = (\mx mn -> max 0 (mx - mn)) <$> forOracleMaxPublishTime <*> forOracleMinPublishTime
      commitToOracleMin = (\mn c -> mn - c) <$> forOracleMinPublishTime <*> forCommitTimestamp
      commitToOracleMax = (\mx c -> mx - c) <$> forOracleMaxPublishTime <*> forCommitTimestamp
      oracleToFinalize = (\t mx -> t - mx) <$> forTerminalTimestamp <*> forOracleMaxPublishTime
      mLatencyPct = latencySecs >>= \s -> mBench >>= \b -> computeLatencyPercentileRank (plbSamples b) s
      mOrderProtections = buildOrderProtectionsForOrder protBundle forOrderId
      mOracleUpdateId = lookupOrderOracleUpdateId updateMap row
   in object
        [ "orderId" .= show forOrderId
        , "account" .= forAccount
        , "side" .= (sideText <$> forSide)
        , "commitTxHash" .= forCommitTxHash
        , "commitBlock" .= (show <$> forCommitBlock)
        , "commitAt" .= (isoTimestamp <$> forCommitTimestamp)
        , "finalizedBlock" .= (show <$> forTerminalBlock)
        , "finalizedAt" .= (isoTimestamp <$> forTerminalTimestamp)
        , "oracleMinPublishTime" .= (isoTimestamp <$> forOracleMinPublishTime)
        , "oracleMaxPublishTime" .= (isoTimestamp <$> forOracleMaxPublishTime)
        , "oracleSpreadSeconds" .= oracleSpread
        , "commitToOracleMinSeconds" .= commitToOracleMin
        , "commitToOracleMaxSeconds" .= commitToOracleMax
        , "oracleToFinalizeSeconds" .= oracleToFinalize
        , "latencySeconds" .= latencySecs
        , "latencyBlocks" .= latencyBlks
        , "latencyPercentile" .= mLatencyPct
        , "terminalStatus" .= forTerminalStatus
        , "terminalReason" .= forTerminalReason
        , "failureReason" .= forFailureReason
        , "failedConstraint" .= forFailedConstraint
        , "executionMode" .= forExecutionMode
        , "executionPrice" .= (show <$> forExecutionPrice)
        , "oraclePrice" .= (show <$> forOraclePrice)
        , "oracleUpdateId" .= (show <$> mOracleUpdateId)
        , "oracleFrozen" .= forOracleFrozen
        , "vpiAssets" .= (show <$> forVpiAssets)
        , "frozenCloseSpreadAssets" .= (show <$> forFrozenSpreadAssets)
        , "orderProtections" .= mOrderProtections
        ]

protocolEventToJson :: ProtocolEventRow -> Value
protocolEventToJson ProtocolEventRow {..} =
  object
    [ "logIndex" .= perLogIndex
    , "contractAddress" .= perContractAddress
    , "eventName" .= perEventName
    , "account" .= perAccount
    , "orderId" .= (show <$> perOrderId)
    , "side" .= (sideText <$> perSide)
    , "data" .= perData
    ]

txLpSettlementToJson :: TxLpSettlementRow -> Value
txLpSettlementToJson TxLpSettlementRow {..} =
  object
    [ "epoch" .= tlsEpoch
    , "status" .= tlsStatus
    , "seniorDepositAssets" .= (show <$> tlsSeniorDepositAssets)
    , "seniorRedeemAssets" .= (show <$> tlsSeniorRedeemAssets)
    , "juniorDepositAssets" .= (show <$> tlsJuniorDepositAssets)
    , "juniorRedeemAssets" .= (show <$> tlsJuniorRedeemAssets)
    , "seniorBacklog" .= tlsSeniorBacklog
    , "juniorBacklog" .= tlsJuniorBacklog
    , "entriesDeferred" .= tlsEntriesDeferred
    ]

networkMetaJson :: Config -> Value
networkMetaJson cfg =
  object
    [ "chainId" .= cfgPerpsChainId cfg
    , "releaseVersion" .= Manifest.releaseVersion
    , "settlementAssetSymbol" .= ("USDG" :: Text)
    ]

reliabilityToJson :: OrderReliability -> [OrderReliabilityBucket] -> Value
reliabilityToJson OrderReliability {..} timeline =
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
    , "timeline" .= map bucketToJson timeline
    ]

bucketToJson :: OrderReliabilityBucket -> Value
bucketToJson OrderReliabilityBucket {..} =
  object
    [ "bucketStart" .= isoTimestamp orbStartUnix
    , "bucketEnd" .= isoTimestamp orbEndUnix
    , "totalOrders" .= orbTotalOrders
    , "executedOrders" .= orbExecutedOrders
    , "expiredOrders" .= orbExpiredOrders
    , "otherFailedOrders" .= orbOtherFailedOrders
    , "pendingOrders" .= orbPendingOrders
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

recentTradeToJson :: OrderProtectionsBundle -> RecentTradeRow -> Value
recentTradeToJson protBundle RecentTradeRow {..} =
  let effectiveOrderId = rtrOrderId <|> extractExecutionOrderId rtrExecution
      mOrderProtections =
        if rtrActivityType == "Open"
          then effectiveOrderId >>= buildOrderProtectionsForOrder protBundle
          else Nothing
   in object $
        catMaybes
          [ Just $ "id" .= (rtrTxHash <> ":" <> T.pack (show rtrLogIndex))
          , Just $ "activityType" .= rtrActivityType
          , Just $ "account" .= rtrAccount
          , ("orderId" .=) . show <$> effectiveOrderId
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
          , ("execution" .=) <$> rtrExecution
          , ("orderProtections" .=) <$> mOrderProtections
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
