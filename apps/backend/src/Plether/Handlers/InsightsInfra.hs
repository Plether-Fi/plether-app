module Plether.Handlers.InsightsInfra
  ( getInfraStatusResponse
  , getOverviewResponse
  , classifyActionReadiness
  , classifyServiceHealth
  , classifyEventDrivenServiceHealth
  , classifyOverallInfra
  , pickFresherHeartbeat
  , withTtlCache
  , parseEthSyncing
  , parsePeerCount
  ) where

import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar, readMVar)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Either (isRight)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Word (Word64)
import Database.PostgreSQL.Simple (Connection, Only (..), Query, ToRow, query, query_)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (Manager)
import Plether.AA.Kernel (KernelProfile (..))
import Plether.AA.KernelGateway (KernelGatewayConfig (..), KernelSponsorshipConfig (..), kernelScope)
import Plether.AA.OracleReadiness (oracleReadiness)
import Plether.AA.Readiness (Check (..), aggregateFunding)
import Plether.Cache (SingleFlightCache, newRefreshingSingleFlightCache, runSingleFlightCache)
import Plether.Config (Config (..), NativeAaConfig (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.KernelOperation (KernelScope (..))
import Plether.Ethereum.Client
  ( EthClient
  , RpcClientOptions (..)
  , newClientWithManager
  , parseRpcQuantity
  , rpcCall
  )
import Plether.Ethereum.Rpc (RpcBlock (..), ethLatestBlock)
import Plether.Handlers.InsightsSponsored (checkBundlerEntryPoint)
import Plether.Infra.AwsHealth
  ( AwsControlPlaneSnapshot (..)
  , AwsEc2Status (..)
  , AwsEcsServiceStatus (..)
  , AwsRdsStatus (..)
  , fetchAwsControlPlaneSnapshot
  )
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

{-# NOINLINE processStartedAtEpoch #-}
processStartedAtEpoch :: Integer
processStartedAtEpoch = unsafePerformIO (floor <$> getPOSIXTime)

cacheFreshTtlNSec :: Word64
cacheFreshTtlNSec = 20 * 1000000000 -- 20 seconds fresh

cacheStaleTtlNSec :: Word64
cacheStaleTtlNSec = 300 * 1000000000 -- 5 minutes stale-while-revalidate

{-# NOINLINE infraStatusCache #-}
infraStatusCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
infraStatusCache = unsafePerformIO (newRefreshingSingleFlightCache 1 cacheFreshTtlNSec cacheStaleTtlNSec)

{-# NOINLINE overviewCache #-}
overviewCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
overviewCache = unsafePerformIO (newRefreshingSingleFlightCache 1 cacheFreshTtlNSec cacheStaleTtlNSec)

withTtlCache
  :: MVar (Maybe (Word64, a))
  -> Word64
  -> IO (Either e a)
  -> IO (Either e a)
withTtlCache cacheVar ttlNsec compute = do
  nowNs <- getMonotonicTimeNSec
  mCached <- readMVar cacheVar
  case mCached of
    Just (storedNs, val) | nowNs >= storedNs && (nowNs - storedNs) < ttlNsec ->
      pure (Right val)
    _ ->
      modifyMVar cacheVar $ \current -> do
        now2 <- getMonotonicTimeNSec
        case current of
          Just (storedNs, val) | now2 >= storedNs && (now2 - storedNs) < ttlNsec ->
            pure (current, Right val)
          _ -> do
            res <- compute
            case res of
              Right fresh -> do
                doneNs <- getMonotonicTimeNSec
                pure (Just (doneNs, fresh), Right fresh)
              Left err ->
                pure (current, Left err)

nativeEntryPoint :: Text
nativeEntryPoint = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"

data DbHealthProbe = DbHealthProbe
  { dhpReachable :: Bool
  , dhpPingMs :: Maybe Int
  , dhpActiveConns :: Maybe Int
  , dhpIdleConns :: Maybe Int
  , dhpTotalConns :: Maybe Int
  , dhpMaxConns :: Maybe Int
  , dhpSizeBytes :: Maybe Integer
  , dhpInRecovery :: Maybe Bool
  }

unavailableDbProbe :: DbHealthProbe
unavailableDbProbe = DbHealthProbe False Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data NodeHealthProbe = NodeHealthProbe
  { nhpReachable :: Bool
  , nhpLatencyMs :: Maybe Int
  , nhpHeadBlock :: Maybe Integer
  , nhpHeadTimestamp :: Maybe Integer
  , nhpHeadLagSeconds :: Maybe Integer
  , nhpSyncing :: Maybe Bool
  , nhpPeerCount :: Maybe Integer
  }

unavailableNodeProbe :: NodeHealthProbe
unavailableNodeProbe = NodeHealthProbe False Nothing Nothing Nothing Nothing Nothing Nothing

data AltoHealthProbe = AltoHealthProbe
  { ahpConfigured :: Bool
  , ahpReachable :: Bool
  , ahpLatencyMs :: Maybe Int
  , ahpEntryPointVersion :: Maybe Text
  , ahpEntryPointSupported :: Bool
  }

data WorkerHeartbeat = WorkerHeartbeat
  { whKey :: Text
  , whEcsKey :: Text
  , whName :: Text
  , whCategory :: Text
  , whLastBlock :: Maybe Integer
  , whLastSeenAt :: Maybe UTCTime
  , whFreshThresholdSeconds :: Integer
  , whFundingComponent :: Maybe Text
  , whEventDriven :: Bool
  , whHasActiveFault :: Bool
  }

data OracleTelemetry = OracleTelemetry
  { otLatestPublishTime :: Maybe Integer
  , otAgeSeconds :: Maybe Integer
  , otFeedSpreadSeconds :: Maybe Integer
  , otOpenStatus :: Text
  , otOpenReason :: Text
  , otCloseStatus :: Text
  , otCloseReason :: Text
  , otProtectionStatus :: Text
  , otProtectionReason :: Text
  }

getInfraStatusResponse
  :: DbPool
  -> EthClient
  -> Manager
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
getInfraStatusResponse pool client manager cfg mKernel =
  snd <$> runSingleFlightCache infraStatusCache () isRight (computeInfraStatusResponse pool client manager cfg mKernel)

computeInfraStatusResponse
  :: DbPool
  -> EthClient
  -> Manager
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
computeInfraStatusResponse pool client manager cfg mKernel = do
  nowPosix <- getPOSIXTime
  let nowSec = floor nowPosix :: Integer
      nowUtc = posixSecondsToUTCTime (fromIntegral nowSec)
      chainId = cfgPerpsChainId cfg
      uptimeSec = max 0 (nowSec - processStartedAtEpoch)

  (awsSnap, dbProbe, nodeProbe, altoProbe, fundingMap, oracleInfo, heartbeats) <-
    runConcurrently $
      (,,,,,,)
        <$> Concurrently (fetchAwsControlPlaneSnapshot manager)
        <*> Concurrently (probeDatabase pool)
        <*> Concurrently (probeArbitrumNode client nowSec)
        <*> Concurrently (probeAltoBundler manager cfg mKernel)
        <*> Concurrently (fetchFundingMap pool cfg)
        <*> Concurrently (probeOraclePipeline pool client cfg nowSec)
        <*> Concurrently (fetchWorkerHeartbeats pool cfg mKernel)

  let ecsMap = acpsEcsServices awsSnap
      mHeadBlock = nhpHeadBlock nodeProbe
      respBlock = fromMaybe Manifest.releaseDeploymentBlock mHeadBlock
      backendEcs = Map.lookup "api" ecsMap
      gatewayEcs = Map.lookup "rpc_gateway" ecsMap
      altoEcs = Map.lookup "alto" ecsMap
      altoFunding = fromMaybe ("unknown", "FUNDING_UNVERIFIED") (Map.lookup "alto" fundingMap)
      serviceRows = map (buildServiceRow ecsMap fundingMap mHeadBlock nowUtc) heartbeats
      serviceHealthMap = Map.fromList [(k, st) | (_, k, st) <- serviceRows]
      healthyServices = length [() | (_, _, st) <- serviceRows, st == "healthy"]
      totalServices = length serviceRows

      backendHealthy =
        dhpReachable dbProbe
          && maybe True (\e -> aessRunningCount e >= aessDesiredCount e && aessDesiredCount e > 0) backendEcs
      nodeHealthy =
        nhpReachable nodeProbe
          && nhpSyncing nodeProbe /= Just True
          && maybe False (<= 30) (nhpHeadLagSeconds nodeProbe)
      altoHealthy = not (ahpConfigured altoProbe) || (ahpReachable altoProbe && ahpEntryPointSupported altoProbe)
      keeperStatus = Map.findWithDefault "unknown" "keeper" serviceHealthMap
      protectionStatus = Map.findWithDefault "unknown" "protection_worker" serviceHealthMap
      overallStatus =
        classifyOverallInfra
          backendHealthy
          (dhpReachable dbProbe)
          nodeHealthy
          altoHealthy
          healthyServices
          totalServices

      depositAction =
        classifyActionReadiness
          "Deposit margin"
          [ (if dhpReachable dbProbe then "ready" else "blocked", "DB_UNAVAILABLE")
          , (if nodeHealthy then "ready" else "degraded", "RPC_NODE_LAG")
          , (if altoHealthy then "ready" else "blocked", "BUNDLER_UNAVAILABLE")
          ]
      openAction =
        classifyActionReadiness
          "Open position"
          [ (if nodeHealthy then "ready" else "degraded", "RPC_NODE_LAG")
          , (if keeperStatus == "healthy" then "ready" else if keeperStatus == "degraded" then "degraded" else "blocked", "KEEPER_STALE")
          , (otOpenStatus oracleInfo, otOpenReason oracleInfo)
          , (if altoHealthy then "ready" else "blocked", "BUNDLER_UNAVAILABLE")
          ]
      closeAction =
        classifyActionReadiness
          "Close position"
          [ (if nodeHealthy then "ready" else "degraded", "RPC_NODE_LAG")
          , (if keeperStatus == "healthy" then "ready" else if keeperStatus == "degraded" then "degraded" else "blocked", "KEEPER_STALE")
          , (otCloseStatus oracleInfo, otCloseReason oracleInfo)
          ]
      protectionAction =
        classifyActionReadiness
          "TP/SL protection"
          [ (if protectionStatus == "healthy" then "ready" else if protectionStatus == "degraded" then "degraded" else "unknown", "PROTECTION_WORKER_LAG")
          , (otProtectionStatus oracleInfo, otProtectionReason oracleInfo)
          ]

  pure $
    Right $
      mkResponse respBlock chainId $
        object
          [ "network" .= networkJson chainId
          , "observedAt" .= isoTimestamp nowSec
          , "overallStatus" .= overallStatus
          , "actions"
              .= object
                [ "deposit" .= depositAction
                , "open" .= openAction
                , "close" .= closeAction
                , "protection" .= protectionAction
                ]
          , "aws"
              .= object
                [ "controlPlaneAvailable" .= acpsAvailable awsSnap
                , "backend"
                    .= object
                      [ "status" .= (if backendHealthy then ("healthy" :: Text) else "degraded")
                      , "uptimeSeconds" .= uptimeSec
                      , "releaseVersion" .= Manifest.releaseVersion
                      , "ecs" .= (ecsJson <$> backendEcs)
                      ]
                , "database"
                    .= object
                      [ "reachable" .= dhpReachable dbProbe
                      , "pingLatencyMs" .= dhpPingMs dbProbe
                      , "activeConnections" .= dhpActiveConns dbProbe
                      , "idleConnections" .= dhpIdleConns dbProbe
                      , "totalConnections" .= dhpTotalConns dbProbe
                      , "maxConnections" .= dhpMaxConns dbProbe
                      , "databaseSizeBytes" .= (T.pack . show <$> dhpSizeBytes dbProbe)
                      , "inRecovery" .= dhpInRecovery dbProbe
                      , "rds" .= (rdsJson <$> acpsRds awsSnap)
                      ]
                , "node"
                    .= object
                      [ "reachable" .= nhpReachable nodeProbe
                      , "rpcLatencyMs" .= nhpLatencyMs nodeProbe
                      , "headBlock" .= (T.pack . show <$> nhpHeadBlock nodeProbe)
                      , "headTimestamp" .= (isoTimestamp <$> nhpHeadTimestamp nodeProbe)
                      , "headLagSeconds" .= nhpHeadLagSeconds nodeProbe
                      , "syncing" .= nhpSyncing nodeProbe
                      , "peerCount" .= nhpPeerCount nodeProbe
                      , "ec2" .= (ec2Json <$> acpsRpcEc2 awsSnap)
                      , "gatewayEcs" .= (ecsJson <$> gatewayEcs)
                      ]
                , "alto"
                    .= object
                      [ "configured" .= ahpConfigured altoProbe
                      , "reachable" .= ahpReachable altoProbe
                      , "probeLatencyMs" .= ahpLatencyMs altoProbe
                      , "entryPointVersion" .= ahpEntryPointVersion altoProbe
                      , "entryPointSupported" .= ahpEntryPointSupported altoProbe
                      , "fundingState" .= fst altoFunding
                      , "fundingReason" .= snd altoFunding
                      , "ecs" .= (ecsJson <$> altoEcs)
                      ]
                , "services" .= map (\(v, _, _) -> v) serviceRows
                , "summary"
                    .= object
                      [ "healthyServices" .= healthyServices
                      , "totalServices" .= totalServices
                      ]
                ]
          , "oracle"
              .= object
                [ "latestPublishTime" .= (isoTimestamp <$> otLatestPublishTime oracleInfo)
                , "ageSeconds" .= otAgeSeconds oracleInfo
                , "feedSpreadSeconds" .= otFeedSpreadSeconds oracleInfo
                , "openPolicyStatus" .= otOpenStatus oracleInfo
                , "closePolicyStatus" .= otCloseStatus oracleInfo
                , "protectionPolicyStatus" .= otProtectionStatus oracleInfo
                ]
          ]

getOverviewResponse
  :: DbPool
  -> EthClient
  -> Manager
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
getOverviewResponse pool client manager cfg mKernel =
  snd <$> runSingleFlightCache overviewCache () isRight (computeOverviewResponse pool client manager cfg mKernel)

computeOverviewResponse
  :: DbPool
  -> EthClient
  -> Manager
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
computeOverviewResponse pool client manager cfg mKernel = do
  nowPosix <- getPOSIXTime
  let nowSec = floor nowPosix :: Integer
      nowUtc = posixSecondsToUTCTime (fromIntegral nowSec)
      chainId = cfgPerpsChainId cfg

  (awsSnap, dbProbe, nodeProbe, execAgg, liqAgg, sponsorAgg, fundingMap, heartbeats) <-
    runConcurrently $
      (,,,,,,,)
        <$> Concurrently (fetchAwsControlPlaneSnapshot manager)
        <*> Concurrently (probeDatabase pool)
        <*> Concurrently (probeArbitrumNode client nowSec)
        <*> Concurrently (fetchOverviewExecution pool cfg nowSec)
        <*> Concurrently (fetchOverviewLiquidity pool cfg)
        <*> Concurrently (fetchOverviewSponsored pool cfg mKernel)
        <*> Concurrently (fetchFundingMap pool cfg)
        <*> Concurrently (fetchWorkerHeartbeats pool cfg mKernel)

  let ecsMap = acpsEcsServices awsSnap
      mHeadBlock = nhpHeadBlock nodeProbe
      respBlock = fromMaybe Manifest.releaseDeploymentBlock mHeadBlock
      serviceRows = map (buildServiceRow ecsMap fundingMap mHeadBlock nowUtc) heartbeats
      healthyServices = length [() | (_, _, st) <- serviceRows, st == "healthy"]
      totalServices = length serviceRows
      nodeHealthy = nhpReachable nodeProbe && nhpSyncing nodeProbe /= Just True
      overallInfra =
        classifyOverallInfra
          (dhpReachable dbProbe)
          (dhpReachable dbProbe)
          nodeHealthy
          True
          healthyServices
          totalServices

  pure $
    Right $
      mkResponse respBlock chainId $
        object
          [ "network" .= networkJson chainId
          , "observedAt" .= isoTimestamp nowSec
          , "headBlock" .= (T.pack . show <$> mHeadBlock)
          , "execution" .= execAgg
          , "liquidity" .= liqAgg
          , "sponsored" .= sponsorAgg
          , "protocol"
              .= object
                [ "releaseVersion" .= Manifest.releaseVersion
                , "deploymentBlock" .= T.pack (show Manifest.releaseDeploymentBlock)
                , "pinnedContracts" .= (30 :: Int)
                ]
          , "infra"
              .= object
                [ "overallStatus" .= overallInfra
                , "dbReachable" .= dhpReachable dbProbe
                , "dbPingLatencyMs" .= dhpPingMs dbProbe
                , "rpcReachable" .= nhpReachable nodeProbe
                , "rpcLatencyMs" .= nhpLatencyMs nodeProbe
                , "healthyServices" .= healthyServices
                , "totalServices" .= totalServices
                , "awsControlPlaneAvailable" .= acpsAvailable awsSnap
                ]
          ]

-- Probes

probeDatabase :: DbPool -> IO DbHealthProbe
probeDatabase pool = do
  t0 <- getMonotonicTimeNSec
  mRes <- observeIo $ withDb pool $ \conn -> do
    [Only _] <- query_ conn "SELECT 1::INT" :: IO [Only Int]
    t1 <- getMonotonicTimeNSec
    let pingMs = fromIntegral ((t1 - t0) `div` 1000000) :: Int
    mStats <- observeIo $
      query_
        conn
        "SELECT \
        \  COUNT(*) FILTER (WHERE state = 'active')::INT, \
        \  COUNT(*) FILTER (WHERE state = 'idle')::INT, \
        \  COUNT(*)::INT, \
        \  current_setting('max_connections')::INT, \
        \  pg_database_size(current_database())::BIGINT, \
        \  pg_is_in_recovery() \
        \FROM pg_stat_activity \
        \WHERE datname = current_database()"
        :: IO (Maybe [(Int, Int, Int, Int, Integer, Bool)])
    pure $ case mStats of
      Just [(act, idle, tot, maxC, sz, rec)] ->
        DbHealthProbe True (Just pingMs) (Just act) (Just idle) (Just tot) (Just maxC) (Just sz) (Just rec)
      _ ->
        DbHealthProbe True (Just pingMs) Nothing Nothing Nothing Nothing Nothing Nothing
  pure $ fromMaybe unavailableDbProbe mRes

probeArbitrumNode :: EthClient -> Integer -> IO NodeHealthProbe
probeArbitrumNode client nowSec = do
  t0 <- getMonotonicTimeNSec
  (blkRes, syncRes, peerRes) <-
    runConcurrently $
      (,,)
        <$> Concurrently (joinTimeout <$> timeout 3000000 (observeIo $ ethLatestBlock client))
        <*> Concurrently (joinTimeout <$> timeout 3000000 (observeIo $ rpcCall client "eth_syncing" (toJSON ([] :: [Text]))))
        <*> Concurrently (joinTimeout <$> timeout 3000000 (observeIo $ rpcCall client "net_peerCount" (toJSON ([] :: [Text]))))
  t1 <- getMonotonicTimeNSec
  let latencyMs = fromIntegral ((t1 - t0) `div` 1000000) :: Int
  pure $ case blkRes of
    Just (Right blk) ->
      let headLag = max 0 (nowSec - rpcBlockTimestamp blk)
          syncing = case syncRes of
            Just (Right val) -> parseEthSyncing val
            _ -> Nothing
          peers = case peerRes of
            Just (Right val) -> parsePeerCount val
            _ -> Nothing
       in NodeHealthProbe
            True
            (Just latencyMs)
            (Just (rpcBlockNumber blk))
            (Just (rpcBlockTimestamp blk))
            (Just headLag)
            syncing
            peers
    _ -> unavailableNodeProbe

probeAltoBundler :: Manager -> Config -> Maybe KernelGatewayConfig -> IO AltoHealthProbe
probeAltoBundler manager cfg mKernel =
  case activeAltoTarget cfg mKernel of
    Nothing -> pure $ AltoHealthProbe False False Nothing Nothing False
    Just (altoUrl, epAddr, epVer) -> do
      reqId <- newIORef 1
      altoClient <-
        newClientWithManager
          manager
          reqId
          RpcClientOptions
            { rcoEndpoint = altoUrl
            , rcoBearerToken = Nothing
            , rcoRole = "insights_infra_alto_probe"
            }
      t0 <- getMonotonicTimeNSec
      mRes <- timeout 3000000 $ observeIo $ rpcCall altoClient "eth_supportedEntryPoints" (toJSON ([] :: [Text]))
      t1 <- getMonotonicTimeNSec
      let latMs = fromIntegral ((t1 - t0) `div` 1000000) :: Int
      pure $ case mRes of
        Just (Just (Right val)) ->
          AltoHealthProbe True True (Just latMs) (Just epVer) (checkBundlerEntryPoint epAddr val)
        _ ->
          AltoHealthProbe True False Nothing (Just epVer) False

activeAltoTarget :: Config -> Maybe KernelGatewayConfig -> Maybe (Text, Text, Text)
activeAltoTarget cfg mKernel =
  case mKernel of
    Just kgc | not (T.null (T.strip (kgcAltoRpcUrl kgc))) ->
      Just (kgcAltoRpcUrl kgc, T.toLower (kpEntryPoint (kgcProfile kgc)), "0.9")
    _ ->
      case cfgNativeAaConfig cfg of
        Just native | not (T.null (T.strip (naaAltoRpcUrl native))) ->
          Just (naaAltoRpcUrl native, nativeEntryPoint, "0.8")
        _ -> Nothing

fetchFundingMap :: DbPool -> Config -> IO (Map.Map Text (Text, Text))
fetchFundingMap pool cfg = do
  mInv <- lookupEnv "AA_FUNDING_INVENTORY_ID"
  let invId = maybe "" T.pack mInv
  if T.null invId
    then pure Map.empty
    else do
      mRows <- observeIo $ withDb pool $ \conn ->
        query
          conn
          "SELECT component, \
          \  CASE WHEN observed_at > clock_timestamp() - interval '15 seconds' AND observed_at <= clock_timestamp() THEN state ELSE 'unknown' END, \
          \  CASE WHEN observed_at > clock_timestamp() - interval '15 seconds' AND observed_at <= clock_timestamp() THEN reason ELSE 'FUNDING_UNVERIFIED' END \
          \FROM aa_funding_observations \
          \WHERE chain_id = ? AND deployment = ? AND inventory_id = ? LIMIT 32"
          (cfgPerpsChainId cfg, T.toLower (cfgPerpsOrderRouter cfg), invId)
          :: IO [(Text, Text, Text)]
      pure $ case mRows of
        Nothing -> Map.empty
        Just rows ->
          let comps = ["alto", "keeper", "oracle", "liquidation", "protection", "lp_settlement"]
           in Map.fromList
                [ (c, let Check _ st r = aggregateFunding c [(s, re) | (comp, s, re) <- rows, comp == c] in (st, r))
                | c <- comps
                ]

probeOraclePipeline :: DbPool -> EthClient -> Config -> Integer -> IO OracleTelemetry
probeOraclePipeline pool client cfg nowSec = do
  (mReadiness, mPayload) <-
    runConcurrently $
      (,)
        <$> Concurrently (joinTimeout <$> timeout 3500000 (observeIo $ oracleReadiness cfg pool client))
        <*> Concurrently
          ( observeIo $ withDb pool $ \conn ->
              query_
                conn
                "SELECT min_publish_time, max_publish_time FROM perps_pyth_update_payloads ORDER BY max_publish_time DESC LIMIT 1"
                :: IO [(Integer, Integer)]
          )
  let ((os, orr), (cs, cr), (ps, pr)) =
        fromMaybe (("unknown", "ORACLE_UNAVAILABLE"), ("unknown", "ORACLE_UNAVAILABLE"), ("unknown", "ORACLE_UNAVAILABLE")) mReadiness
      (mPub, mAge, mSpread) = case mPayload of
        Just [(minT, maxT)] ->
          (Just maxT, Just (max 0 (nowSec - maxT)), Just (max 0 (maxT - minT)))
        _ -> (Nothing, Nothing, Nothing)
  pure $ OracleTelemetry mPub mAge mSpread os orr cs cr ps pr

-- | Run all worker heartbeat queries over a single DbPool connection (instead of
-- 10 concurrent checkouts that self-exhausted the pool).
fetchWorkerHeartbeats :: DbPool -> Config -> Maybe KernelGatewayConfig -> IO [WorkerHeartbeat]
fetchWorkerHeartbeats pool cfg mKernel = do
  let chainId = cfgPerpsChainId cfg
      router = T.toLower (cfgPerpsOrderRouter cfg)
      engine = T.toLower (cfgPerpsCfdEngine cfg)
      protBook = T.toLower Manifest.positionProtectionBookAddress
  mTuple <-
    observeIo $ withDb pool $ \conn -> do
      keeperHb <- querySingleHbConn conn "SELECT last_indexed_block, updated_at::timestamptz FROM perps_keeper_state WHERE order_router = ? LIMIT 1" (Only router)
      liqHb <- querySingleHbConn conn "SELECT last_indexed_block, updated_at::timestamptz FROM perps_liquidation_state WHERE chain_id = ? AND cfd_engine = ? LIMIT 1" (chainId, engine)
      protHb <- querySingleHbConn conn "SELECT block_number, indexed_at FROM perps_protection_checkpoints WHERE chain_id = ? AND book = ? ORDER BY block_number DESC LIMIT 1" (chainId, protBook)
      basketHb <- querySingleHbNoParamsConn conn "SELECT NULL::BIGINT, to_timestamp(max_publish_time) FROM perps_pyth_update_payloads ORDER BY max_publish_time DESC LIMIT 1"
      lpHb <- queryLpSettlementHbConn conn chainId
      aaRecHb <- queryReconcilerHbConn conn cfg mKernel
      perpsIdxHb <- querySingleHbConn conn "SELECT last_indexed_block, updated_at FROM perps_indexer_state WHERE chain_id = ? ORDER BY updated_at DESC LIMIT 1" (Only chainId)
      vaultIdxHb <- querySingleHbConn conn "SELECT last_indexed_block::BIGINT, updated_at FROM vault_activity_indexer_state WHERE chain_id = ? ORDER BY updated_at DESC LIMIT 1" (Only chainId)
      insightsHb <- queryInsightsWorkerHbConn conn chainId
      pure (keeperHb, liqHb, protHb, basketHb, lpHb, aaRecHb, perpsIdxHb, vaultIdxHb, insightsHb)
  let emptyHb = (Nothing, Nothing)
      (keeperHb, liqHb, protHb, basketHb, lpHb, aaRecHb, perpsIdxHb, vaultIdxHb, insightsHb) =
        fromMaybe (emptyHb, emptyHb, emptyHb, emptyHb, (Nothing, Nothing, False), emptyHb, emptyHb, emptyHb, emptyHb) mTuple
      (lpBlock, lpSeenAt, lpHasFault) = lpHb
  pure
    [ WorkerHeartbeat "keeper" "keeper" "Order Execution Keeper" "execution" (fst keeperHb) (snd keeperHb) 120 (Just "keeper") False False
    , WorkerHeartbeat "liquidation_worker" "liquidation_worker" "Liquidation Worker" "execution" (fst liqHb) (snd liqHb) 180 (Just "liquidation") False False
    , WorkerHeartbeat "protection_worker" "protection_worker" "TP/SL Protection Worker" "execution" (fst protHb) (snd protHb) 180 (Just "protection") False False
    , WorkerHeartbeat "basket_worker" "basket_worker" "Pyth Basket / Oracle Worker" "oracle" (fst basketHb) (snd basketHb) 60 (Just "oracle") False False
    , WorkerHeartbeat "lp_settlement" "keeper" "LP Settlement Worker" "liquidity" lpBlock lpSeenAt 600 (Just "lp_settlement") True lpHasFault
    , WorkerHeartbeat "aa_reconciler" "aa_reconciler" "AA Sponsorship Reconciler" "sponsorship" (fst aaRecHb) (snd aaRecHb) 600 Nothing False False
    , WorkerHeartbeat "perps_indexer" "perps_indexer" "Perps History Indexer" "indexing" (fst perpsIdxHb) (snd perpsIdxHb) 180 Nothing False False
    , WorkerHeartbeat "vault_indexers" "workers" "Vault Activity & Attribution" "indexing" (fst vaultIdxHb) (snd vaultIdxHb) 180 Nothing False False
    , WorkerHeartbeat "insights_worker" "insights_worker" "Insights & Divisions Worker" "indexing" (fst insightsHb) (snd insightsHb) 900 Nothing False False
    ]

queryLpSettlementHbConn :: Connection -> Integer -> IO (Maybe Integer, Maybe UTCTime, Bool)
queryLpSettlementHbConn conn chainId = do
  mRows <-
    observeIo $
      query
        conn
        "SELECT observed_block, last_observed_at, \
        \  (critical_fault_mask > 0 OR dependency_failure_mask > 0 OR operational_blocker_mask > 0 OR COALESCE(lp_epoch_settlement_paused, FALSE)) \
        \FROM perps_lp_settlement_observations \
        \WHERE chain_id = ? \
        \ORDER BY last_observed_at DESC \
        \LIMIT 1"
        (Only chainId)
        :: IO (Maybe [(Maybe Integer, Maybe UTCTime, Bool)])
  pure $ case mRows of
    Just ((blk, ts, fault) : _) -> (blk, ts, fault)
    _ -> (Nothing, Nothing, False)

queryInsightsWorkerHbConn :: Connection -> Integer -> IO (Maybe Integer, Maybe UTCTime)
queryInsightsWorkerHbConn conn chainId = do
  divHb <-
    querySingleHbConn
      conn
      "SELECT snapshot_block, captured_at FROM divisions_live_snapshots WHERE chain_id = ? ORDER BY captured_at DESC LIMIT 1"
      (Only chainId)
  batchHb <-
    querySingleHbConn
      conn
      "SELECT block_number, published_at FROM insights_snapshot_batches WHERE chain_id = ? ORDER BY published_at DESC LIMIT 1"
      (Only chainId)
  pure (pickFresherHeartbeat divHb batchHb)

pickFresherHeartbeat
  :: (Maybe Integer, Maybe UTCTime)
  -> (Maybe Integer, Maybe UTCTime)
  -> (Maybe Integer, Maybe UTCTime)
pickFresherHeartbeat left@(_, mTsLeft) right@(_, mTsRight) =
  case (mTsLeft, mTsRight) of
    (Just tl, Just tr) -> if tl >= tr then left else right
    (Just _, Nothing) -> left
    (Nothing, Just _) -> right
    (Nothing, Nothing) -> left

queryReconcilerHbConn :: Connection -> Config -> Maybe KernelGatewayConfig -> IO (Maybe Integer, Maybe UTCTime)
queryReconcilerHbConn conn cfg mKernel =
  case mKernel of
    Just kgc
      | Just ksc <- kgcSponsorship kgc ->
          let KernelScope {..} = kernelScope (kgcProfile kgc)
           in querySingleHbConn
                conn
                "SELECT safe_block, last_success_at FROM aa_kernel_reconciler_state WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? LIMIT 1"
                (ksDeployment, ksChainId, ksEntryPoint, T.toLower (kscPaymaster ksc))
    _ ->
      case cfgNativeAaConfig cfg of
        Just native ->
          querySingleHbConn
            conn
            "SELECT safe_block, last_success_at FROM aa_reconciler_health WHERE chain_id = ? AND paymaster = ? LIMIT 1"
            (cfgPerpsChainId cfg, T.toLower (naaPaymasterAddress native))
        Nothing -> pure (Nothing, Nothing)

querySingleHbConn :: (ToRow q) => Connection -> Query -> q -> IO (Maybe Integer, Maybe UTCTime)
querySingleHbConn conn sql params = do
  mRows <- observeIo $ (query conn sql params :: IO [(Maybe Integer, Maybe UTCTime)])
  pure $ case mRows of
    Just (pair : _) -> pair
    _ -> (Nothing, Nothing)

querySingleHbNoParamsConn :: Connection -> Query -> IO (Maybe Integer, Maybe UTCTime)
querySingleHbNoParamsConn conn sql = do
  mRows <- observeIo $ (query_ conn sql :: IO [(Maybe Integer, Maybe UTCTime)])
  pure $ case mRows of
    Just (pair : _) -> pair
    _ -> (Nothing, Nothing)

joinTimeout :: Maybe (Maybe a) -> Maybe a
joinTimeout (Just (Just x)) = Just x
joinTimeout _ = Nothing

fetchOverviewExecution :: DbPool -> Config -> Integer -> IO Value
fetchOverviewExecution pool cfg nowSec = do
  let chainId = cfgPerpsChainId cfg
      router = T.toLower (cfgPerpsOrderRouter cfg)
      since24h = nowSec - 86400
  mRow <- observeIo $ withDb pool $ \conn ->
    query
      conn
      "SELECT \
      \  COUNT(*)::INT, \
      \  COUNT(*) FILTER (WHERE terminal_status = 'Executed')::INT, \
      \  percentile_cont(0.50) WITHIN GROUP (ORDER BY GREATEST(0, terminal_timestamp - commit_timestamp)) FILTER (WHERE terminal_status = 'Executed' AND commit_timestamp IS NOT NULL AND terminal_timestamp IS NOT NULL)::DOUBLE PRECISION, \
      \  percentile_cont(0.95) WITHIN GROUP (ORDER BY GREATEST(0, terminal_timestamp - commit_timestamp)) FILTER (WHERE terminal_status = 'Executed' AND commit_timestamp IS NOT NULL AND terminal_timestamp IS NOT NULL)::DOUBLE PRECISION \
      \FROM perps_orders \
      \WHERE chain_id = ? AND order_router = ? AND commit_timestamp >= ?"
      (chainId, router, since24h)
      :: IO [(Int, Int, Maybe Double, Maybe Double)]
  pure $ case mRow of
    Just [(tot, execC, p50, p95)] ->
      object
        [ "orders24h" .= tot
        , "executed24h" .= execC
        , "fillRate24hBps" .= (if tot > 0 then Just ((execC * 10000) `div` tot) else (Nothing :: Maybe Int))
        , "latencyP50Seconds" .= (round <$> p50 :: Maybe Int)
        , "latencyP95Seconds" .= (round <$> p95 :: Maybe Int)
        ]
    _ ->
      object
        [ "orders24h" .= (Nothing :: Maybe Int)
        , "executed24h" .= (Nothing :: Maybe Int)
        , "fillRate24hBps" .= (Nothing :: Maybe Int)
        , "latencyP50Seconds" .= (Nothing :: Maybe Int)
        , "latencyP95Seconds" .= (Nothing :: Maybe Int)
        ]

fetchOverviewLiquidity :: DbPool -> Config -> IO Value
fetchOverviewLiquidity pool cfg = do
  let chainId = cfgPerpsChainId cfg
      monitor = T.toLower (T.strip (cfgPerpsSettlementMonitorLens cfg))
  mPair <- observeIo $ withDb pool $ \conn -> do
    obs <-
      query
        conn
        "SELECT epoch, lp_epoch_settlement_paused, \
        \  (critical_fault_mask > 0 OR dependency_failure_mask > 0), \
        \  (operational_blocker_mask > 0 OR warning_mask > 0) \
        \FROM perps_lp_settlement_observations \
        \WHERE chain_id = ? AND monitor_address = ? \
        \ORDER BY observed_block DESC, last_observed_at DESC LIMIT 1"
        (chainId, monitor)
        :: IO [(Integer, Maybe Bool, Bool, Bool)]
    confirmed <-
      query
        conn
        "SELECT COUNT(*)::INT FROM perps_lp_settlement_transactions \
        \WHERE chain_id = ? AND monitor_address = ? AND status = 'confirmed_success' AND confirmed_at >= NOW() - interval '24 hours'"
        (chainId, monitor)
        :: IO [Only Int]
    pure (obs, confirmed)
  let (mObs, mConfirmed) = case mPair of
        Just (o, c) -> (Just o, Just c)
        Nothing -> (Nothing, Nothing)
  let (mEpoch, mReadiness) = case mObs of
        Just [(ep, mPaused, hasCritical, hasWarn)] ->
          let readiness :: Text
              readiness
                | mPaused == Just True = "paused"
                | hasCritical = "critical"
                | hasWarn = "degraded"
                | otherwise = "healthy"
           in (Just ep, Just readiness)
        _ -> (Nothing, Nothing)
      mConf = case mConfirmed of
        Just [Only c] -> Just c
        _ -> Nothing
  pure $
    object
      [ "latestEpoch" .= mEpoch
      , "readiness" .= mReadiness
      , "confirmedSettlements24h" .= mConf
      ]

fetchOverviewSponsored :: DbPool -> Config -> Maybe KernelGatewayConfig -> IO Value
fetchOverviewSponsored pool cfg mKernel =
  case mKernel of
    Just kgc | Just ksc <- kgcSponsorship kgc -> do
      let KernelScope {..} = kernelScope (kgcProfile kgc)
      mRow <- observeIo $ withDb pool $ \conn ->
        query
          conn
          "SELECT COUNT(*)::INT, COUNT(*) FILTER (WHERE COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE))::INT \
          \FROM aa_kernel_sponsorships \
          \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? \
          \  AND status = 'settled' AND settled_at >= NOW() - interval '24 hours'"
          (ksDeployment, ksChainId, ksEntryPoint, T.toLower (kscPaymaster ksc))
          :: IO [(Int, Int)]
      pure $ sponsorSummaryJson True "kernel" (kscEnabled ksc) mRow
    _ ->
      case cfgNativeAaConfig cfg of
        Just native -> do
          mRow <- observeIo $ withDb pool $ \conn ->
            query_
              conn
              "SELECT COUNT(*)::INT, COUNT(*) FILTER (WHERE success)::INT \
              \FROM aa_user_operation_events \
              \WHERE finalized_at >= NOW() - interval '24 hours'"
              :: IO [(Int, Int)]
          pure $ sponsorSummaryJson True "native" (naaSponsorshipEnabled native) mRow
        Nothing ->
          pure $
            object
              [ "configured" .= False
              , "stack" .= ("none" :: Text)
              , "enabled" .= False
              , "ops24h" .= (0 :: Int)
              , "successRate24hBps" .= (Nothing :: Maybe Int)
              ]

sponsorSummaryJson :: Bool -> Text -> Bool -> Maybe [(Int, Int)] -> Value
sponsorSummaryJson configured stackName enabled mRow =
  let (ops, rate) = case mRow of
        Just [(tot, ok)] -> (tot, if tot > 0 then Just ((ok * 10000) `div` tot) else Nothing)
        _ -> (0, Nothing)
   in object
        [ "configured" .= configured
        , "stack" .= stackName
        , "enabled" .= enabled
        , "ops24h" .= ops
        , "successRate24hBps" .= rate
        ]

buildServiceRow
  :: Map.Map Text AwsEcsServiceStatus
  -> Map.Map Text (Text, Text)
  -> Maybe Integer
  -> UTCTime
  -> WorkerHeartbeat
  -> (Value, Text, Text)
buildServiceRow ecsMap fundingMap mHead nowUtc WorkerHeartbeat {..} =
  let mEcs = Map.lookup whEcsKey ecsMap
      mFunding = whFundingComponent >>= (`Map.lookup` fundingMap)
      mHeartbeatAge = (\ts -> max 0 (floor (diffUTCTime nowUtc ts) :: Integer)) <$> whLastSeenAt
      mBlocksBehind = (\h b -> max 0 (h - b)) <$> mHead <*> whLastBlock
      health =
        if whEventDriven
          then classifyEventDrivenServiceHealth mEcs mHeartbeatAge whFreshThresholdSeconds (fst <$> mFunding) whHasActiveFault
          else classifyServiceHealth mEcs mHeartbeatAge whFreshThresholdSeconds (fst <$> mFunding)
      val =
        object
          [ "key" .= whKey
          , "name" .= whName
          , "category" .= whCategory
          , "status" .= health
          , "lastBlock" .= (T.pack . show <$> whLastBlock)
          , "blocksBehind" .= mBlocksBehind
          , "lastHeartbeatAt" .= (formatUtcIso <$> whLastSeenAt)
          , "heartbeatAgeSeconds" .= mHeartbeatAge
          , "freshThresholdSeconds" .= whFreshThresholdSeconds
          , "fundingState" .= (fst <$> mFunding)
          , "fundingReason" .= (snd <$> mFunding)
          , "ecs" .= (ecsJson <$> mEcs)
          ]
   in (val, whKey, health)

-- Pure classification & parsing helpers (unit-tested in InsightsInfraSpec)

classifyServiceHealth
  :: Maybe AwsEcsServiceStatus
  -> Maybe Integer
  -> Integer
  -> Maybe Text
  -> Text
classifyServiceHealth mEcs mAgeSec thresholdSec mFundingState =
  let ecsDown = maybe False (\e -> aessDesiredCount e > 0 && aessRunningCount e == 0) mEcs
      ecsDegraded = maybe False (\e -> aessDesiredCount e > 0 && aessRunningCount e < aessDesiredCount e) mEcs
      ecsScaledZero = maybe False (\e -> aessDesiredCount e == 0) mEcs
      fundingBlocked = mFundingState == Just "blocked"
      hbFresh = maybe False (<= thresholdSec) mAgeSec
      hbStale = maybe False (> thresholdSec) mAgeSec
   in if ecsDown || fundingBlocked
        then "down"
      else if ecsScaledZero
        then "scaled_zero"
      else if ecsDegraded || hbStale
        then "degraded"
      else if hbFresh || maybe False (\e -> aessRunningCount e > 0) mEcs
        then "healthy"
      else "unknown"

classifyEventDrivenServiceHealth
  :: Maybe AwsEcsServiceStatus
  -> Maybe Integer
  -> Integer
  -> Maybe Text
  -> Bool
  -> Text
classifyEventDrivenServiceHealth mEcs mAgeSec thresholdSec mFundingState hasActiveFault =
  let ecsDown = maybe False (\e -> aessDesiredCount e > 0 && aessRunningCount e == 0) mEcs
      ecsDegraded = maybe False (\e -> aessDesiredCount e > 0 && aessRunningCount e < aessDesiredCount e) mEcs
      ecsScaledZero = maybe False (\e -> aessDesiredCount e == 0) mEcs
      fundingBlocked = mFundingState == Just "blocked"
      fundingReady = mFundingState == Just "ready"
      hbFresh = maybe False (<= thresholdSec) mAgeSec
   in if ecsDown || fundingBlocked
        then "down"
      else if ecsScaledZero
        then "scaled_zero"
      else if ecsDegraded || hasActiveFault
        then "degraded"
      else if fundingReady || hbFresh || maybe False (\e -> aessRunningCount e > 0) mEcs || isJust mAgeSec
        then "healthy"
      else "unknown"

classifyOverallInfra :: Bool -> Bool -> Bool -> Bool -> Int -> Int -> Text
classifyOverallInfra backendOk dbOk nodeOk altoOk healthySvc totalSvc
  | not dbOk || not nodeOk = "down"
  | not backendOk || not altoOk = "degraded"
  | totalSvc > 0 && healthySvc * 2 < totalSvc = "degraded"
  | totalSvc > 0 && healthySvc < totalSvc = "degraded"
  | otherwise = "healthy"

classifyActionReadiness :: Text -> [(Text, Text)] -> Value
classifyActionReadiness label checks =
  let (st, reason)
        | any (\(s, _) -> s == "blocked") checks =
            ("blocked" :: Text, headReason "blocked" checks)
        | any (\(s, _) -> s == "degraded") checks =
            ("degraded", headReason "degraded" checks)
        | any (\(s, _) -> s == "unknown") checks =
            ("unknown", headReason "unknown" checks)
        | otherwise = ("ready", "READY")
   in object
        [ "label" .= label
        , "status" .= st
        , "reason" .= reason
        ]
  where
    headReason target xs =
      case [r | (s, r) <- xs, s == target] of
        (r : _) -> r
        [] -> "UNAVAILABLE"

parseEthSyncing :: Value -> Maybe Bool
parseEthSyncing (Bool b) = Just b
parseEthSyncing (Object _) = Just True
parseEthSyncing _ = Nothing

parsePeerCount :: Value -> Maybe Integer
parsePeerCount (String hexTxt) = either (const Nothing) Just (parseRpcQuantity "peerCount" hexTxt)
parsePeerCount (Number n) =
  case floatingOrInteger n :: Either Double Integer of
    Right i | i >= 0 -> Just i
    _ -> Nothing
parsePeerCount _ = Nothing

-- JSON helpers

networkJson :: Integer -> Value
networkJson chainId =
  object
    [ "chainId" .= chainId
    , "releaseVersion" .= Manifest.releaseVersion
    , "settlementAssetSymbol" .= ("USDG" :: Text)
    ]

ecsJson :: AwsEcsServiceStatus -> Value
ecsJson AwsEcsServiceStatus {..} =
  object
    [ "status" .= aessStatus
    , "desiredCount" .= aessDesiredCount
    , "runningCount" .= aessRunningCount
    , "pendingCount" .= aessPendingCount
    , "rolloutState" .= aessRolloutState
    ]

rdsJson :: AwsRdsStatus -> Value
rdsJson AwsRdsStatus {..} =
  object
    [ "status" .= arsStatus
    , "engineVersion" .= arsEngineVersion
    , "multiAz" .= arsMultiAz
    , "storageEncrypted" .= arsStorageEncrypted
    , "allocatedStorageGb" .= arsAllocatedStorageGb
    ]

ec2Json :: AwsEc2Status -> Value
ec2Json AwsEc2Status {..} =
  object
    [ "instanceState" .= aecInstanceState
    , "systemStatus" .= aecSystemStatus
    , "instanceStatus" .= aecInstanceStatus
    ]

observeIo :: IO a -> IO (Maybe a)
observeIo action = do
  res <- try action
  case res of
    Right val -> pure (Just val)
    Left (err :: SomeException) ->
      case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> pure Nothing

formatUtcIso :: UTCTime -> Text
formatUtcIso = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

isoTimestamp :: Integer -> Text
isoTimestamp = formatUtcIso . posixSecondsToUTCTime . fromIntegral
