module Plether.Handlers.InsightsSponsored
  ( getSponsoredStatusResponse
  , getSponsoredOperationsResponse
  , sanitizePublicReason
  , computeUtilizationBps
  , computeRunwayHours
  , computeSuccessRateBps
  , classifySponsorshipGate
  , decodeEntryPointBalance
  , decodePaymasterPaused
  , checkBundlerEntryPoint
  ) where

import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isAscii)
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Connection, Only (..), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Network.HTTP.Client (Manager)
import Plether.AA.Kernel (KernelProfile (..))
import Plether.AA.KernelGateway (KernelGatewayConfig (..), KernelSponsorshipConfig (..), kernelScope)
import Plether.AA.Readiness (Check (..), aggregateFunding)
import Plether.Config (Config (..), NativeAaConfig (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.KernelOperation (KernelScope (..), KernelSponsorBudget (..))
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall)
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , RpcClientOptions (..)
  , ethBlockNumber
  , ethCallAtBlock
  , newClientWithManager
  , rpcCall
  )
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Text.Read (readMaybe)

nativeEntryPoint :: Text
nativeEntryPoint = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"

defaultMinDepositWei :: Integer
defaultMinDepositWei = 50000000000000000 -- 0.05 ETH

data ActiveStack
  = StackKernel KernelGatewayConfig KernelSponsorshipConfig
  | StackNative NativeAaConfig Integer
  | StackNone

data WindowMetrics = WindowMetrics
  { wmTotalOps :: Int
  , wmSucceededOps :: Int
  , wmRevertedOps :: Int
  , wmGasSpentWei :: Integer
  , wmReservedMaxWei :: Integer
  }

data WindowTripleRow = WindowTripleRow WindowMetrics WindowMetrics WindowMetrics

instance FromRow WindowTripleRow where
  fromRow = do
    w1 <- WindowMetrics <$> field <*> field <*> field <*> (parseIntegerOrZero <$> field) <*> (parseIntegerOrZero <$> field)
    w24 <- WindowMetrics <$> field <*> field <*> field <*> (parseIntegerOrZero <$> field) <*> (parseIntegerOrZero <$> field)
    w7 <- WindowMetrics <$> field <*> field <*> field <*> (parseIntegerOrZero <$> field) <*> (parseIntegerOrZero <$> field)
    pure (WindowTripleRow w1 w24 w7)

emptyWindowMetrics :: WindowMetrics
emptyWindowMetrics = WindowMetrics 0 0 0 0 0

data FunnelCounts = FunnelCounts
  { fcReserved :: Int
  , fcSigned :: Int
  , fcSubmitted :: Int
  , fcSettled :: Int
  , fcExpired :: Int
  , fcCancelled :: Int
  }

emptyFunnelCounts :: FunnelCounts
emptyFunnelCounts = FunnelCounts 0 0 0 0 0 0

data ControlState = ControlState
  { csIssuancePaused :: Bool
  , csPausedReason :: Maybe Text
  , csUpdatedAt :: Maybe UTCTime
  }

data ReconcilerRow = ReconcilerRow
  { rrSafeBlock :: Integer
  , rrLastSuccessAt :: UTCTime
  }

data CloseAssistanceSummary = CloseAssistanceSummary
  { casAvailable :: Bool
  , casVerified24h :: Int
  , casVerified7d :: Int
  , casTotalAssets7d :: Integer
  , casPendingUnverified :: Int
  , casTotalAttempts7d :: Int
  }

unavailableCloseAssistance :: CloseAssistanceSummary
unavailableCloseAssistance = CloseAssistanceSummary False 0 0 0 0 0

data SponsoredOpRow = SponsoredOpRow
  { sorUserOpHash :: Text
  , sorTxHash :: Text
  , sorSender :: Text
  , sorBlockNumber :: Text
  , sorSuccess :: Bool
  , sorActualGasCostWei :: Integer
  , sorMaxCostWei :: Integer
  , sorLatencySeconds :: Maybe Int
  , sorOccurredAt :: UTCTime
  , sorFinalizedAt :: UTCTime
  }

instance FromRow SponsoredOpRow where
  fromRow =
    SponsoredOpRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> (parseIntegerOrZero <$> field)
      <*> (parseIntegerOrZero <$> field)
      <*> field
      <*> field
      <*> field

data ControlEventRow = ControlEventRow
  { cerId :: Integer
  , cerAction :: Text
  , cerReason :: Text
  , cerCreatedAt :: UTCTime
  }

resolveActiveStack :: Config -> Maybe KernelGatewayConfig -> IO ActiveStack
resolveActiveStack cfg mKernel =
  case mKernel of
    Just kgc
      | Just ksc <- kgcSponsorship kgc -> pure (StackKernel kgc ksc)
    _ ->
      case cfgNativeAaConfig cfg of
        Just native -> do
          minDep <- readMinDepositWei
          pure (StackNative native minDep)
        Nothing -> pure StackNone

readMinDepositWei :: IO Integer
readMinDepositWei = do
  mRaw <- lookupEnv "AA_PAYMASTER_MIN_DEPOSIT_WEI"
  pure $ case mRaw >>= readMaybe of
    Just n | n > 0 -> n
    _ -> defaultMinDepositWei

getSponsoredStatusResponse
  :: DbPool
  -> EthClient
  -> Manager
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
getSponsoredStatusResponse pool client manager cfg mKernel = do
  nowPosix <- getPOSIXTime
  let nowSec = floor nowPosix :: Integer
      nowUtc = posixSecondsToUTCTime (fromIntegral nowSec)
      chainId = cfgPerpsChainId cfg
  stack <- resolveActiveStack cfg mKernel
  case stack of
    StackNone -> do
      blockRes <- ethBlockNumber client
      let headBlock = either (const Manifest.releaseDeploymentBlock) id blockRes
      pure $ Right $ mkResponse headBlock chainId $ unconfiguredStatusJson chainId nowSec
    StackNative native minDepositWei ->
      buildNativeStatusResponse pool client manager cfg native minDepositWei nowSec nowUtc
    StackKernel kgc ksc ->
      buildKernelStatusResponse pool client manager cfg kgc ksc nowSec nowUtc

getSponsoredOperationsResponse
  :: DbPool
  -> Config
  -> Maybe KernelGatewayConfig
  -> Int
  -> Maybe Text
  -> IO (Either ApiError (ApiResponse Value))
getSponsoredOperationsResponse pool cfg mKernel rawLimit mStatusFilter = do
  nowPosix <- getPOSIXTime
  let nowSec = floor nowPosix :: Integer
      chainId = cfgPerpsChainId cfg
      limitCount = max 1 (min 100 rawLimit)
      statusFilter = normalizeStatusFilter mStatusFilter
  stack <- resolveActiveStack cfg mKernel
  case stack of
    StackNone ->
      pure $
        Right $
          mkResponse Manifest.releaseDeploymentBlock chainId $
            object
              [ "network" .= networkJson chainId
              , "observedAt" .= isoTimestamp nowSec
              , "configured" .= False
              , "stack" .= ("none" :: Text)
              , "operations" .= ([] :: [Value])
              , "controlEvents" .= ([] :: [Value])
              ]
    StackNative _ _ -> do
      (mOps, mEvents) <-
        runConcurrently $
          (,)
            <$> Concurrently (observeIo $ withDb pool $ \conn -> fetchNativeOperations conn limitCount statusFilter)
            <*> Concurrently (observeIo $ withDb pool $ \conn -> fetchNativeControlEvents conn 20)
      let ops = fromMaybe [] mOps
          events = fromMaybe [] mEvents
      pure $
        Right $
          mkResponse Manifest.releaseDeploymentBlock chainId $
            object
              [ "network" .= networkJson chainId
              , "observedAt" .= isoTimestamp nowSec
              , "configured" .= True
              , "stack" .= ("native" :: Text)
              , "operations" .= map opRowToJson ops
              , "controlEvents" .= map controlEventToJson events
              ]
    StackKernel kgc ksc -> do
      let scope = kernelScope (kgcProfile kgc)
          paymaster = T.toLower (kscPaymaster ksc)
      mOps <- observeIo $ withDb pool $ \conn -> fetchKernelOperations conn scope paymaster limitCount statusFilter
      let ops = fromMaybe [] mOps
      pure $
        Right $
          mkResponse Manifest.releaseDeploymentBlock chainId $
            object
              [ "network" .= networkJson chainId
              , "observedAt" .= isoTimestamp nowSec
              , "configured" .= True
              , "stack" .= ("kernel" :: Text)
              , "operations" .= map opRowToJson ops
              , "controlEvents" .= ([] :: [Value])
              ]

buildNativeStatusResponse
  :: DbPool
  -> EthClient
  -> Manager
  -> Config
  -> NativeAaConfig
  -> Integer
  -> Integer
  -> UTCTime
  -> IO (Either ApiError (ApiResponse Value))
buildNativeStatusResponse pool client manager cfg native minDepositWei nowSec nowUtc = do
  let chainId = cfgPerpsChainId cfg
      paymasterAddr = T.toLower (naaPaymasterAddress native)
  blockRes <- ethBlockNumber client
  let mHeadBlock = either (const Nothing) Just blockRes
      respBlock = fromMaybe Manifest.releaseDeploymentBlock mHeadBlock

  (chainReads, bundlerRes, dbReads, altoFunding, closeAssist) <-
    runConcurrently $
      (,,,,)
        <$> Concurrently (fetchPaymasterOnchain client mHeadBlock nativeEntryPoint paymasterAddr)
        <*> Concurrently (probeBundler manager (naaAltoRpcUrl native) nativeEntryPoint)
        <*> Concurrently (fetchNativeDbTelemetry pool chainId paymasterAddr)
        <*> Concurrently (fetchAltoFundingObservation pool cfg)
        <*> Concurrently (fetchCloseAssistanceSummary pool cfg)

  let (mDepositWei, mOnchainPaused) = chainReads
      (mControl, mReconciler, budgetUsed, win1h, win24h, win7d, latencyPair, funnel24h, activeInfo) = dbReads
      (usedOutWei, used1hWei, used24hWei) = budgetUsed
      (mLatP50, mLatP95) = latencyPair
      (activeCount, mOldestAgeSec) = activeInfo
      maxLag = naaMaxSafeLagSeconds native
      (reconcilerFresh, mLagSec, mBlocksBehind) =
        evaluateReconcilerFreshness mReconciler mHeadBlock nowUtc maxLag
      (bundlerReachable, bundlerEpOk, bundlerStatus) = bundlerRes
      issuancePaused = csIssuancePaused <$> mControl
      pausedReason = sanitizePublicReason <$> (mControl >>= csPausedReason)
      rolloutMode :: Text
      rolloutMode
        | not (naaSponsorshipEnabled native) = "disabled"
        | naaGlobalRolloutEnabled native = "global"
        | otherwise = "canary"
      gateStatus =
        classifySponsorshipGate
          (naaSponsorshipEnabled native)
          (naaSubmissionEnabled native)
          issuancePaused
          mOnchainPaused
          reconcilerFresh
          (Just bundlerReachable)
      freeHeadroom = (\dep -> max 0 (dep - usedOutWei)) <$> mDepositWei
      depositHealthy = (\dep -> dep >= minDepositWei && dep >= usedOutWei) <$> mDepositWei
      runwayHours = mDepositWei >>= \dep -> computeRunwayHours dep (wmGasSpentWei win24h)
      (altoState, altoReason) = altoFunding

  pure $
    Right $
      mkResponse respBlock chainId $
        object
          [ "network" .= networkJson chainId
          , "observedAt" .= isoTimestamp nowSec
          , "configured" .= True
          , "stack" .= ("native" :: Text)
          , "entryPointVersion" .= ("0.8" :: Text)
          , "entryPointAddress" .= nativeEntryPoint
          , "paymasterAddress" .= paymasterAddr
          , "gate"
              .= object
                [ "status" .= gateStatus
                , "sponsorshipEnabled" .= naaSponsorshipEnabled native
                , "submissionEnabled" .= naaSubmissionEnabled native
                , "rolloutMode" .= rolloutMode
                , "issuancePaused" .= issuancePaused
                , "pausedReason" .= pausedReason
                , "paymasterOnchainPaused" .= mOnchainPaused
                , "updatedAt" .= (formatUtcIso <$> (mControl >>= csUpdatedAt))
                ]
          , "paymaster"
              .= object
                [ "entryPointDepositWei" .= (T.pack . show <$> mDepositWei)
                , "minDepositWei" .= T.pack (show minDepositWei)
                , "freeHeadroomWei" .= (T.pack . show <$> freeHeadroom)
                , "depositHealthy" .= depositHealthy
                , "burn24hWei" .= T.pack (show (wmGasSpentWei win24h))
                , "estimatedRunwayHours" .= runwayHours
                , "altoFundingState" .= altoState
                , "altoFundingReason" .= altoReason
                ]
          , "budgets"
              .= object
                [ "maxCostPerOpWei" .= T.pack (show (naaMaxCostWei native))
                , "accountHourlyCapWei" .= T.pack (show (naaAccountHourlyWei native))
                , "globalOutstanding" .= budgetGaugeJson usedOutWei (naaGlobalOutstandingWei native)
                , "globalHourly" .= budgetGaugeJson used1hWei (naaGlobalHourlyWei native)
                , "globalDaily" .= budgetGaugeJson used24hWei (naaGlobalDailyWei native)
                ]
          , "reconciler"
              .= object
                [ "safeBlock" .= (T.pack . show . rrSafeBlock <$> mReconciler)
                , "headBlock" .= (T.pack . show <$> mHeadBlock)
                , "blocksBehind" .= mBlocksBehind
                , "lastSuccessAt" .= (formatUtcIso . rrLastSuccessAt <$> mReconciler)
                , "lagSeconds" .= mLagSec
                , "maxSafeLagSeconds" .= maxLag
                , "fresh" .= reconcilerFresh
                ]
          , "bundler"
              .= object
                [ "reachable" .= bundlerReachable
                , "entryPointSupported" .= bundlerEpOk
                , "status" .= bundlerStatus
                ]
          , "execution"
              .= object
                [ "window1h" .= windowMetricsJson win1h
                , "window24h" .= windowMetricsJson win24h
                , "window7d" .= windowMetricsJson win7d
                , "latencyP50Seconds" .= mLatP50
                , "latencyP95Seconds" .= mLatP95
                , "funnel24h" .= funnelJson funnel24h
                , "activeUnfinalizedCount" .= activeCount
                , "oldestUnfinalizedAgeSeconds" .= mOldestAgeSec
                ]
          , "closeAssistance" .= closeAssistanceJson closeAssist
          ]

buildKernelStatusResponse
  :: DbPool
  -> EthClient
  -> Manager
  -> Config
  -> KernelGatewayConfig
  -> KernelSponsorshipConfig
  -> Integer
  -> UTCTime
  -> IO (Either ApiError (ApiResponse Value))
buildKernelStatusResponse pool client manager cfg kgc ksc nowSec nowUtc = do
  let chainId = cfgPerpsChainId cfg
      scope = kernelScope (kgcProfile kgc)
      entryPointAddr = T.toLower (kpEntryPoint (kgcProfile kgc))
      paymasterAddr = T.toLower (kscPaymaster ksc)
      budget = kscBudget ksc
      minDepositWei = kscMinDepositWei ksc
  blockRes <- ethBlockNumber client
  let mHeadBlock = either (const Nothing) Just blockRes
      respBlock = fromMaybe Manifest.releaseDeploymentBlock mHeadBlock

  (chainReads, bundlerRes, dbReads, altoFunding, closeAssist) <-
    runConcurrently $
      (,,,,)
        <$> Concurrently (fetchPaymasterOnchain client mHeadBlock entryPointAddr paymasterAddr)
        <*> Concurrently (probeBundler manager (kgcAltoRpcUrl kgc) entryPointAddr)
        <*> Concurrently (fetchKernelDbTelemetry pool scope paymasterAddr)
        <*> Concurrently (fetchAltoFundingObservation pool cfg)
        <*> Concurrently (fetchCloseAssistanceSummary pool cfg)

  let (mDepositWei, mOnchainPaused) = chainReads
      (mReconciler, mPausedReasonRaw, budgetUsed, win1h, win24h, win7d, latencyPair, funnel24h, activeInfo) = dbReads
      (usedOutWei, used1hWei, used24hWei) = budgetUsed
      (mLatP50, mLatP95) = latencyPair
      (activeCount, mOldestAgeSec) = activeInfo
      maxLag = kgcMaxSafeLagSeconds kgc
      (reconcilerFresh, mLagSec, mBlocksBehind) =
        evaluateReconcilerFreshness mReconciler mHeadBlock nowUtc maxLag
      (bundlerReachable, bundlerEpOk, bundlerStatus) = bundlerRes
      issuancePaused = case (mReconciler, mPausedReasonRaw) of
        (Nothing, Nothing) -> Nothing
        (_, Just _) -> Just True
        (Just _, Nothing) -> Just False
      pausedReason = sanitizePublicReason <$> mPausedReasonRaw
      rolloutMode :: Text
      rolloutMode
        | not (kscEnabled ksc) = "disabled"
        | null (kscCanaryOwners ksc) = "global"
        | otherwise = "canary"
      submissionEnabled = kscEnabled ksc || kgcEthModeEnabled kgc
      gateStatus =
        classifySponsorshipGate
          (kscEnabled ksc)
          submissionEnabled
          issuancePaused
          mOnchainPaused
          reconcilerFresh
          (Just bundlerReachable)
      freeHeadroom = (\dep -> max 0 (dep - usedOutWei)) <$> mDepositWei
      depositHealthy = (\dep -> dep >= minDepositWei && dep >= usedOutWei) <$> mDepositWei
      runwayHours = mDepositWei >>= \dep -> computeRunwayHours dep (wmGasSpentWei win24h)
      (altoState, altoReason) = altoFunding

  pure $
    Right $
      mkResponse respBlock chainId $
        object
          [ "network" .= networkJson chainId
          , "observedAt" .= isoTimestamp nowSec
          , "configured" .= True
          , "stack" .= ("kernel" :: Text)
          , "entryPointVersion" .= ("0.9" :: Text)
          , "entryPointAddress" .= entryPointAddr
          , "paymasterAddress" .= paymasterAddr
          , "gate"
              .= object
                [ "status" .= gateStatus
                , "sponsorshipEnabled" .= kscEnabled ksc
                , "submissionEnabled" .= submissionEnabled
                , "rolloutMode" .= rolloutMode
                , "issuancePaused" .= issuancePaused
                , "pausedReason" .= pausedReason
                , "paymasterOnchainPaused" .= mOnchainPaused
                , "updatedAt" .= (formatUtcIso . rrLastSuccessAt <$> mReconciler)
                ]
          , "paymaster"
              .= object
                [ "entryPointDepositWei" .= (T.pack . show <$> mDepositWei)
                , "minDepositWei" .= T.pack (show minDepositWei)
                , "freeHeadroomWei" .= (T.pack . show <$> freeHeadroom)
                , "depositHealthy" .= depositHealthy
                , "burn24hWei" .= T.pack (show (wmGasSpentWei win24h))
                , "estimatedRunwayHours" .= runwayHours
                , "altoFundingState" .= altoState
                , "altoFundingReason" .= altoReason
                ]
          , "budgets"
              .= object
                [ "maxCostPerOpWei" .= T.pack (show (ksbMaxCostWei budget))
                , "accountHourlyCapWei" .= T.pack (show (ksbAccountHourlyWei budget))
                , "globalOutstanding" .= budgetGaugeJson usedOutWei (ksbGlobalOutstandingWei budget)
                , "globalHourly" .= budgetGaugeJson used1hWei (ksbGlobalHourlyWei budget)
                , "globalDaily" .= budgetGaugeJson used24hWei (ksbGlobalDailyWei budget)
                ]
          , "reconciler"
              .= object
                [ "safeBlock" .= (T.pack . show . rrSafeBlock <$> mReconciler)
                , "headBlock" .= (T.pack . show <$> mHeadBlock)
                , "blocksBehind" .= mBlocksBehind
                , "lastSuccessAt" .= (formatUtcIso . rrLastSuccessAt <$> mReconciler)
                , "lagSeconds" .= mLagSec
                , "maxSafeLagSeconds" .= maxLag
                , "fresh" .= reconcilerFresh
                ]
          , "bundler"
              .= object
                [ "reachable" .= bundlerReachable
                , "entryPointSupported" .= bundlerEpOk
                , "status" .= bundlerStatus
                ]
          , "execution"
              .= object
                [ "window1h" .= windowMetricsJson win1h
                , "window24h" .= windowMetricsJson win24h
                , "window7d" .= windowMetricsJson win7d
                , "latencyP50Seconds" .= mLatP50
                , "latencyP95Seconds" .= mLatP95
                , "funnel24h" .= funnelJson funnel24h
                , "activeUnfinalizedCount" .= activeCount
                , "oldestUnfinalizedAgeSeconds" .= mOldestAgeSec
                ]
          , "closeAssistance" .= closeAssistanceJson closeAssist
          ]

unconfiguredStatusJson :: Integer -> Integer -> Value
unconfiguredStatusJson chainId nowSec =
  object
    [ "network" .= networkJson chainId
    , "observedAt" .= isoTimestamp nowSec
    , "configured" .= False
    , "stack" .= ("none" :: Text)
    , "entryPointVersion" .= (Nothing :: Maybe Text)
    , "entryPointAddress" .= (Nothing :: Maybe Text)
    , "paymasterAddress" .= (Nothing :: Maybe Text)
    , "gate"
        .= object
          [ "status" .= ("disabled" :: Text)
          , "sponsorshipEnabled" .= False
          , "submissionEnabled" .= False
          , "rolloutMode" .= ("disabled" :: Text)
          , "issuancePaused" .= (Nothing :: Maybe Bool)
          , "pausedReason" .= (Nothing :: Maybe Text)
          , "paymasterOnchainPaused" .= (Nothing :: Maybe Bool)
          , "updatedAt" .= (Nothing :: Maybe Text)
          ]
    , "paymaster"
        .= object
          [ "entryPointDepositWei" .= (Nothing :: Maybe Text)
          , "minDepositWei" .= (Nothing :: Maybe Text)
          , "freeHeadroomWei" .= (Nothing :: Maybe Text)
          , "depositHealthy" .= (Nothing :: Maybe Bool)
          , "burn24hWei" .= (Nothing :: Maybe Text)
          , "estimatedRunwayHours" .= (Nothing :: Maybe Integer)
          , "altoFundingState" .= ("unknown" :: Text)
          , "altoFundingReason" .= ("FUNDING_UNVERIFIED" :: Text)
          ]
    , "budgets" .= (Nothing :: Maybe Value)
    , "reconciler"
        .= object
          [ "safeBlock" .= (Nothing :: Maybe Text)
          , "headBlock" .= (Nothing :: Maybe Text)
          , "blocksBehind" .= (Nothing :: Maybe Integer)
          , "lastSuccessAt" .= (Nothing :: Maybe Text)
          , "lagSeconds" .= (Nothing :: Maybe Integer)
          , "maxSafeLagSeconds" .= (Nothing :: Maybe Integer)
          , "fresh" .= False
          ]
    , "bundler"
        .= object
          [ "reachable" .= False
          , "entryPointSupported" .= False
          , "status" .= ("unavailable" :: Text)
          ]
    , "execution"
        .= object
          [ "window1h" .= windowMetricsJson emptyWindowMetrics
          , "window24h" .= windowMetricsJson emptyWindowMetrics
          , "window7d" .= windowMetricsJson emptyWindowMetrics
          , "latencyP50Seconds" .= (Nothing :: Maybe Int)
          , "latencyP95Seconds" .= (Nothing :: Maybe Int)
          , "funnel24h" .= funnelJson emptyFunnelCounts
          , "activeUnfinalizedCount" .= (0 :: Int)
          , "oldestUnfinalizedAgeSeconds" .= (Nothing :: Maybe Int)
          ]
    , "closeAssistance" .= closeAssistanceJson unavailableCloseAssistance
    ]

fetchPaymasterOnchain :: EthClient -> Maybe Integer -> Text -> Text -> IO (Maybe Integer, Maybe Bool)
fetchPaymasterOnchain _ Nothing _ _ = pure (Nothing, Nothing)
fetchPaymasterOnchain client (Just blockNum) entryPointAddr paymasterAddr =
  runConcurrently $
    (,)
      <$> Concurrently
        ( either (const Nothing) decodeEntryPointBalance
            <$> ethCallAtBlock
              client
              (CallParams entryPointAddr (encodeCall "balanceOf(address)" [encodeAddress paymasterAddr]))
              blockNum
        )
      <*> Concurrently
        ( either (const Nothing) decodePaymasterPaused
            <$> ethCallAtBlock
              client
              (CallParams paymasterAddr (encodeCall "paused()" []))
              blockNum
        )

probeBundler :: Manager -> Text -> Text -> IO (Bool, Bool, Text)
probeBundler manager altoUrl expectedEntryPoint
  | T.null (T.strip altoUrl) = pure (False, False, "unavailable")
  | otherwise = do
      reqId <- newIORef 1
      altoClient <-
        newClientWithManager
          manager
          reqId
          RpcClientOptions
            { rcoEndpoint = altoUrl
            , rcoBearerToken = Nothing
            , rcoRole = "insights_alto_probe"
            }
      mRes <- timeout 3000000 $ observeIo $ rpcCall altoClient "eth_supportedEntryPoints" (toJSON ([] :: [Text]))
      pure $ case mRes of
        Just (Just (Right val))
          | checkBundlerEntryPoint expectedEntryPoint val -> (True, True, "ready")
          | otherwise -> (True, False, "unsupported_entrypoint")
        _ -> (False, False, "unavailable")

fetchAltoFundingObservation :: DbPool -> Config -> IO (Text, Text)
fetchAltoFundingObservation pool cfg = do
  mInventoryId <- lookupEnv "AA_FUNDING_INVENTORY_ID"
  let invId = maybe "" T.pack mInventoryId
  if T.null invId
    then pure ("unknown", "FUNDING_UNVERIFIED")
    else do
      mRows <- observeIo $ withDb pool $ \conn ->
        query
          conn
          "SELECT CASE WHEN observed_at > clock_timestamp() - interval '15 seconds' AND observed_at <= clock_timestamp() THEN state ELSE 'unknown' END, \
          \CASE WHEN observed_at > clock_timestamp() - interval '15 seconds' AND observed_at <= clock_timestamp() THEN reason ELSE 'FUNDING_UNVERIFIED' END \
          \FROM aa_funding_observations \
          \WHERE chain_id = ? AND deployment = ? AND inventory_id = ? AND component = 'alto' \
          \LIMIT 16"
          (cfgPerpsChainId cfg, T.toLower (cfgPerpsOrderRouter cfg), invId)
          :: IO [(Text, Text)]
      pure $ case mRows of
        Just rows ->
          let Check _ state reason = aggregateFunding "alto" rows
           in (state, reason)
        Nothing -> ("unknown", "FUNDING_UNVERIFIED")

fetchCloseAssistanceSummary :: DbPool -> Config -> IO CloseAssistanceSummary
fetchCloseAssistanceSummary pool cfg = do
  mRow <- observeIo $ withDb pool $ \conn ->
    query
      conn
      "SELECT \
      \  COUNT(*) FILTER (WHERE g.verified AND a.settled_at >= NOW() - interval '24 hours')::INT, \
      \  COUNT(*) FILTER (WHERE g.verified AND a.settled_at >= NOW() - interval '7 days')::INT, \
      \  COALESCE(SUM(g.amount_assets) FILTER (WHERE g.verified AND a.settled_at >= NOW() - interval '7 days'), 0)::TEXT, \
      \  COUNT(*) FILTER (WHERE a.state IN ('reserved','signed','submitted') OR (COALESCE(e.success, FALSE) AND NOT g.verified))::INT, \
      \  COUNT(*) FILTER (WHERE a.created_at >= NOW() - interval '7 days')::INT \
      \FROM aa_close_assistance g \
      \JOIN aa_sponsorship_authorizations a USING (digest) \
      \LEFT JOIN aa_user_operation_events e USING (digest) \
      \WHERE g.chain_id = ? AND g.router = ?"
      (cfgPerpsChainId cfg, T.toLower (cfgPerpsOrderRouter cfg))
      :: IO [(Int, Int, Text, Int, Int)]
  pure $ case mRow of
    Just [(v24, v7d, totalTxt, pend, attempts7d)] ->
      CloseAssistanceSummary True v24 v7d (parseIntegerOrZero totalTxt) pend attempts7d
    _ -> unavailableCloseAssistance

fetchNativeDbTelemetry
  :: DbPool
  -> Integer
  -> Text
  -> IO
      ( Maybe ControlState
      , Maybe ReconcilerRow
      , (Integer, Integer, Integer)
      , WindowMetrics
      , WindowMetrics
      , WindowMetrics
      , (Maybe Int, Maybe Int)
      , FunnelCounts
      , (Int, Maybe Int)
      )
fetchNativeDbTelemetry pool chainId paymasterAddr = do
  mRes <- observeIo $ withDb pool $ \conn -> do
    controlRows <-
      query_
        conn
        "SELECT issuance_paused, paused_reason, updated_at FROM aa_sponsorship_control WHERE singleton = TRUE LIMIT 1"
        :: IO [(Bool, Maybe Text, UTCTime)]
    let mControl = case controlRows of
          [(paused, reason, ts)] -> Just (ControlState paused reason (Just ts))
          _ -> Nothing

    recRows <-
      query
        conn
        "SELECT safe_block, last_success_at FROM aa_reconciler_health WHERE chain_id = ? AND LOWER(paymaster) = ? LIMIT 1"
        (chainId, paymasterAddr)
        :: IO [(Integer, UTCTime)]
    let mReconciler = case recRows of
          [(sb, ts)] -> Just (ReconcilerRow sb ts)
          _ -> Nothing

    budgetRows <-
      query_
        conn
        "SELECT \
        \  COALESCE((SELECT SUM(max_cost_wei) FROM aa_sponsorship_authorizations WHERE state IN ('reserved','signed','submitted')), 0)::TEXT, \
        \  COALESCE((SELECT SUM(actual_gas_cost_wei) FROM aa_user_operation_events WHERE finalized_at >= NOW() - interval '1 hour'), 0)::TEXT, \
        \  COALESCE((SELECT SUM(actual_gas_cost_wei) FROM aa_user_operation_events WHERE finalized_at >= NOW() - interval '24 hours'), 0)::TEXT"
        :: IO [(Text, Text, Text)]
    let budgetUsed = case budgetRows of
          [(o, h, d)] -> (parseIntegerOrZero o, parseIntegerOrZero h, parseIntegerOrZero d)
          _ -> (0, 0, 0)

    winRows <-
      query_
        conn
        "SELECT \
        \  COUNT(*) FILTER (WHERE e.finalized_at >= NOW() - interval '1 hour')::INT, \
        \  COUNT(*) FILTER (WHERE e.finalized_at >= NOW() - interval '1 hour' AND e.success)::INT, \
        \  COUNT(*) FILTER (WHERE e.finalized_at >= NOW() - interval '1 hour' AND NOT e.success)::INT, \
        \  COALESCE(SUM(e.actual_gas_cost_wei) FILTER (WHERE e.finalized_at >= NOW() - interval '1 hour'), 0)::TEXT, \
        \  COALESCE(SUM(a.max_cost_wei) FILTER (WHERE e.finalized_at >= NOW() - interval '1 hour'), 0)::TEXT, \
        \  COUNT(*) FILTER (WHERE e.finalized_at >= NOW() - interval '24 hours')::INT, \
        \  COUNT(*) FILTER (WHERE e.finalized_at >= NOW() - interval '24 hours' AND e.success)::INT, \
        \  COUNT(*) FILTER (WHERE e.finalized_at >= NOW() - interval '24 hours' AND NOT e.success)::INT, \
        \  COALESCE(SUM(e.actual_gas_cost_wei) FILTER (WHERE e.finalized_at >= NOW() - interval '24 hours'), 0)::TEXT, \
        \  COALESCE(SUM(a.max_cost_wei) FILTER (WHERE e.finalized_at >= NOW() - interval '24 hours'), 0)::TEXT, \
        \  COUNT(*) FILTER (WHERE e.finalized_at >= NOW() - interval '7 days')::INT, \
        \  COUNT(*) FILTER (WHERE e.finalized_at >= NOW() - interval '7 days' AND e.success)::INT, \
        \  COUNT(*) FILTER (WHERE e.finalized_at >= NOW() - interval '7 days' AND NOT e.success)::INT, \
        \  COALESCE(SUM(e.actual_gas_cost_wei) FILTER (WHERE e.finalized_at >= NOW() - interval '7 days'), 0)::TEXT, \
        \  COALESCE(SUM(a.max_cost_wei) FILTER (WHERE e.finalized_at >= NOW() - interval '7 days'), 0)::TEXT \
        \FROM aa_user_operation_events e \
        \JOIN aa_sponsorship_authorizations a USING (digest) \
        \WHERE e.finalized_at >= NOW() - interval '7 days'"
        :: IO [WindowTripleRow]
    let (w1, w24, w7) = case winRows of
          [WindowTripleRow m1 m24 m7] -> (m1, m24, m7)
          _ -> (emptyWindowMetrics, emptyWindowMetrics, emptyWindowMetrics)

    latRows <-
      query_
        conn
        "SELECT \
        \  percentile_cont(0.50) WITHIN GROUP (ORDER BY GREATEST(0, EXTRACT(EPOCH FROM (e.observed_at - COALESCE(a.submitted_at, a.signed_at, a.created_at)))))::DOUBLE PRECISION, \
        \  percentile_cont(0.95) WITHIN GROUP (ORDER BY GREATEST(0, EXTRACT(EPOCH FROM (e.observed_at - COALESCE(a.submitted_at, a.signed_at, a.created_at)))))::DOUBLE PRECISION \
        \FROM aa_user_operation_events e \
        \JOIN aa_sponsorship_authorizations a USING (digest) \
        \WHERE e.finalized_at >= NOW() - interval '24 hours'"
        :: IO [(Maybe Double, Maybe Double)]
    let latencyPair = case latRows of
          [(p50, p95)] -> (round <$> p50, round <$> p95)
          _ -> (Nothing, Nothing)

    funnelRows <-
      query_
        conn
        "SELECT \
        \  COUNT(*) FILTER (WHERE state = 'reserved')::INT, \
        \  COUNT(*) FILTER (WHERE state = 'signed')::INT, \
        \  COUNT(*) FILTER (WHERE state = 'submitted')::INT, \
        \  COUNT(*) FILTER (WHERE state = 'settled')::INT, \
        \  COUNT(*) FILTER (WHERE state = 'expired')::INT, \
        \  COUNT(*) FILTER (WHERE state = 'cancelled')::INT \
        \FROM aa_sponsorship_authorizations \
        \WHERE created_at >= NOW() - interval '24 hours'"
        :: IO [(Int, Int, Int, Int, Int, Int)]
    let funnel = case funnelRows of
          [(res, sig, sub, set, expC, can)] -> FunnelCounts res sig sub set expC can
          _ -> emptyFunnelCounts

    activeRows <-
      query_
        conn
        "SELECT \
        \  COUNT(*)::INT, \
        \  MAX(GREATEST(0, EXTRACT(EPOCH FROM (NOW() - created_at))))::DOUBLE PRECISION \
        \FROM aa_sponsorship_authorizations \
        \WHERE state IN ('reserved','signed','submitted')"
        :: IO [(Int, Maybe Double)]
    let activeInfo = case activeRows of
          [(cnt, mAge)] -> (cnt, round <$> mAge)
          _ -> (0, Nothing)

    pure (mControl, mReconciler, budgetUsed, w1, w24, w7, latencyPair, funnel, activeInfo)

  pure $
    fromMaybe
      ( Nothing
      , Nothing
      , (0, 0, 0)
      , emptyWindowMetrics
      , emptyWindowMetrics
      , emptyWindowMetrics
      , (Nothing, Nothing)
      , emptyFunnelCounts
      , (0, Nothing)
      )
      mRes

fetchKernelDbTelemetry
  :: DbPool
  -> KernelScope
  -> Text
  -> IO
      ( Maybe ReconcilerRow
      , Maybe Text
      , (Integer, Integer, Integer)
      , WindowMetrics
      , WindowMetrics
      , WindowMetrics
      , (Maybe Int, Maybe Int)
      , FunnelCounts
      , (Int, Maybe Int)
      )
fetchKernelDbTelemetry pool KernelScope {..} paymasterAddr = do
  mRes <- observeIo $ withDb pool $ \conn -> do
    recRows <-
      query
        conn
        "SELECT safe_block, last_success_at, paused_reason \
        \FROM aa_kernel_reconciler_state \
        \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? LIMIT 1"
        (ksDeployment, ksChainId, ksEntryPoint, paymasterAddr)
        :: IO [(Maybe Integer, Maybe UTCTime, Maybe Text)]
    let (mReconciler, mPausedReason) = case recRows of
          [(Just sb, Just ts, mReason)] -> (Just (ReconcilerRow sb ts), mReason)
          [(_, _, mReason)] -> (Nothing, mReason)
          _ -> (Nothing, Nothing)

    budgetRows <-
      query
        conn
        "SELECT \
        \  COALESCE(SUM(max_cost_wei) FILTER (WHERE status IN ('reserved','signed')), 0)::TEXT, \
        \  COALESCE(SUM(actual_gas_cost) FILTER (WHERE status = 'settled' AND settled_at >= NOW() - interval '1 hour'), 0)::TEXT, \
        \  COALESCE(SUM(actual_gas_cost) FILTER (WHERE status = 'settled' AND settled_at >= NOW() - interval '24 hours'), 0)::TEXT \
        \FROM aa_kernel_sponsorships \
        \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ?"
        (ksDeployment, ksChainId, ksEntryPoint, paymasterAddr)
        :: IO [(Text, Text, Text)]
    let budgetUsed = case budgetRows of
          [(o, h, d)] -> (parseIntegerOrZero o, parseIntegerOrZero h, parseIntegerOrZero d)
          _ -> (0, 0, 0)

    winRows <-
      query
        conn
        "SELECT \
        \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '1 hour')::INT, \
        \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '1 hour' AND COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE))::INT, \
        \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '1 hour' AND NOT COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE))::INT, \
        \  COALESCE(SUM(actual_gas_cost) FILTER (WHERE settled_at >= NOW() - interval '1 hour'), 0)::TEXT, \
        \  COALESCE(SUM(max_cost_wei) FILTER (WHERE settled_at >= NOW() - interval '1 hour'), 0)::TEXT, \
        \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '24 hours')::INT, \
        \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '24 hours' AND COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE))::INT, \
        \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '24 hours' AND NOT COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE))::INT, \
        \  COALESCE(SUM(actual_gas_cost) FILTER (WHERE settled_at >= NOW() - interval '24 hours'), 0)::TEXT, \
        \  COALESCE(SUM(max_cost_wei) FILTER (WHERE settled_at >= NOW() - interval '24 hours'), 0)::TEXT, \
        \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '7 days')::INT, \
        \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '7 days' AND COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE))::INT, \
        \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '7 days' AND NOT COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE))::INT, \
        \  COALESCE(SUM(actual_gas_cost) FILTER (WHERE settled_at >= NOW() - interval '7 days'), 0)::TEXT, \
        \  COALESCE(SUM(max_cost_wei) FILTER (WHERE settled_at >= NOW() - interval '7 days'), 0)::TEXT \
        \FROM aa_kernel_sponsorships \
        \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? \
        \  AND status = 'settled' AND settled_at >= NOW() - interval '7 days'"
        (ksDeployment, ksChainId, ksEntryPoint, paymasterAddr)
        :: IO [WindowTripleRow]
    let (w1, w24, w7) = case winRows of
          [WindowTripleRow m1 m24 m7] -> (m1, m24, m7)
          _ -> (emptyWindowMetrics, emptyWindowMetrics, emptyWindowMetrics)

    latRows <-
      query
        conn
        "SELECT \
        \  percentile_cont(0.50) WITHIN GROUP (ORDER BY GREATEST(0, EXTRACT(EPOCH FROM (settled_at - created_at))))::DOUBLE PRECISION, \
        \  percentile_cont(0.95) WITHIN GROUP (ORDER BY GREATEST(0, EXTRACT(EPOCH FROM (settled_at - created_at))))::DOUBLE PRECISION \
        \FROM aa_kernel_sponsorships \
        \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? \
        \  AND status = 'settled' AND settled_at >= NOW() - interval '24 hours'"
        (ksDeployment, ksChainId, ksEntryPoint, paymasterAddr)
        :: IO [(Maybe Double, Maybe Double)]
    let latencyPair = case latRows of
          [(p50, p95)] -> (round <$> p50, round <$> p95)
          _ -> (Nothing, Nothing)

    funnelRows <-
      query
        conn
        "SELECT \
        \  COUNT(*) FILTER (WHERE status = 'reserved')::INT, \
        \  COUNT(*) FILTER (WHERE status = 'signed')::INT, \
        \  0::INT, \
        \  COUNT(*) FILTER (WHERE status = 'settled')::INT, \
        \  COUNT(*) FILTER (WHERE status = 'expired')::INT, \
        \  0::INT \
        \FROM aa_kernel_sponsorships \
        \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? \
        \  AND created_at >= NOW() - interval '24 hours'"
        (ksDeployment, ksChainId, ksEntryPoint, paymasterAddr)
        :: IO [(Int, Int, Int, Int, Int, Int)]
    let funnel = case funnelRows of
          [(res, sig, sub, set, expC, can)] -> FunnelCounts res sig sub set expC can
          _ -> emptyFunnelCounts

    activeRows <-
      query
        conn
        "SELECT \
        \  COUNT(*)::INT, \
        \  MAX(GREATEST(0, EXTRACT(EPOCH FROM (NOW() - created_at))))::DOUBLE PRECISION \
        \FROM aa_kernel_sponsorships \
        \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? \
        \  AND status IN ('reserved','signed')"
        (ksDeployment, ksChainId, ksEntryPoint, paymasterAddr)
        :: IO [(Int, Maybe Double)]
    let activeInfo = case activeRows of
          [(cnt, mAge)] -> (cnt, round <$> mAge)
          _ -> (0, Nothing)

    pure (mReconciler, mPausedReason, budgetUsed, w1, w24, w7, latencyPair, funnel, activeInfo)

  pure $
    fromMaybe
      ( Nothing
      , Nothing
      , (0, 0, 0)
      , emptyWindowMetrics
      , emptyWindowMetrics
      , emptyWindowMetrics
      , (Nothing, Nothing)
      , emptyFunnelCounts
      , (0, Nothing)
      )
      mRes

fetchNativeOperations :: Connection -> Int -> Maybe Bool -> IO [SponsoredOpRow]
fetchNativeOperations conn limitCount mSuccessFilter =
  query
    conn
    "SELECT \
    \  e.user_operation_hash, \
    \  e.transaction_hash, \
    \  a.sender, \
    \  e.block_number::TEXT, \
    \  e.success, \
    \  e.actual_gas_cost_wei::TEXT, \
    \  a.max_cost_wei::TEXT, \
    \  ROUND(GREATEST(0, EXTRACT(EPOCH FROM (e.observed_at - COALESCE(a.submitted_at, a.signed_at, a.created_at)))))::INT, \
    \  e.observed_at, \
    \  e.finalized_at \
    \FROM aa_user_operation_events e \
    \JOIN aa_sponsorship_authorizations a USING (digest) \
    \WHERE (?::BOOLEAN IS NULL OR e.success = ?) \
    \ORDER BY e.finalized_at DESC, e.block_number DESC \
    \LIMIT ?"
    (mSuccessFilter, mSuccessFilter, limitCount)

fetchKernelOperations :: Connection -> KernelScope -> Text -> Int -> Maybe Bool -> IO [SponsoredOpRow]
fetchKernelOperations conn KernelScope {..} paymasterAddr limitCount mSuccessFilter =
  query
    conn
    "SELECT \
    \  COALESCE(user_operation_hash, ''), \
    \  COALESCE(terminal_evidence->>'transactionHash', ''), \
    \  sender, \
    \  COALESCE(terminal_evidence->>'blockNumber', '0'), \
    \  COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE), \
    \  COALESCE(actual_gas_cost, 0)::TEXT, \
    \  max_cost_wei::TEXT, \
    \  ROUND(GREATEST(0, EXTRACT(EPOCH FROM (settled_at - created_at))))::INT, \
    \  settled_at, \
    \  settled_at \
    \FROM aa_kernel_sponsorships \
    \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? \
    \  AND status = 'settled' \
    \  AND (?::BOOLEAN IS NULL OR COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE) = ?) \
    \ORDER BY settled_at DESC \
    \LIMIT ?"
    (ksDeployment, ksChainId, ksEntryPoint, paymasterAddr, mSuccessFilter, mSuccessFilter, limitCount)

fetchNativeControlEvents :: Connection -> Int -> IO [ControlEventRow]
fetchNativeControlEvents conn limitCount = do
  rows <-
    query
      conn
      "SELECT id, action, reason, created_at \
      \FROM aa_sponsorship_control_events \
      \ORDER BY created_at DESC, id DESC \
      \LIMIT ?"
      (Only limitCount)
      :: IO [(Integer, Text, Text, UTCTime)]
  pure
    [ ControlEventRow eid action (sanitizePublicReason reason) ts
    | (eid, action, reason, ts) <- rows
    ]

evaluateReconcilerFreshness
  :: Maybe ReconcilerRow
  -> Maybe Integer
  -> UTCTime
  -> Integer
  -> (Bool, Maybe Integer, Maybe Integer)
evaluateReconcilerFreshness Nothing _ _ _ = (False, Nothing, Nothing)
evaluateReconcilerFreshness (Just ReconcilerRow {..}) mHead nowUtc maxLagSec =
  let lagSec = max 0 (floor (diffUTCTime nowUtc rrLastSuccessAt) :: Integer)
      fresh = maxLagSec > 0 && lagSec <= maxLagSec
      blocksBehind = (\h -> max 0 (h - rrSafeBlock)) <$> mHead
   in (fresh, Just lagSec, blocksBehind)

-- Pure helpers (unit-tested in InsightsSponsoredSpec)

sanitizePublicReason :: Text -> Text
sanitizePublicReason raw =
  let lowered = T.toLower (T.strip raw)
   in if T.null lowered
        then "Safety pause active"
        else if "deposit" `T.isInfixOf` lowered || "insufficient" `T.isInfixOf` lowered
          then "Paymaster EntryPoint deposit below minimum floor"
        else if "bootstrap" `T.isInfixOf` lowered
          then "Fail-closed bootstrap pause"
        else if "disagreement" `T.isInfixOf` lowered
          then "RPC provider disagreement detected"
        else if "unknown" `T.isInfixOf` lowered && "operation" `T.isInfixOf` lowered
          then "Unknown paymaster operation detected"
        else if "cursor" `T.isInfixOf` lowered || "discontinuity" `T.isInfixOf` lowered
          then "Reconciler cursor discontinuity"
        else if "exceed" `T.isInfixOf` lowered && "cost" `T.isInfixOf` lowered
          then "Actual gas cost exceeded reserved maximum"
        else if "resume" `T.isInfixOf` lowered
          then "Sponsorship issuance resumed"
        else if "manual" `T.isInfixOf` lowered || "operator" `T.isInfixOf` lowered
          then "Operator safety control update"
        else if isSafeCodeToken raw
          then T.strip raw
        else "Safety pause active"
  where
    isSafeCodeToken t =
      let s = T.strip t
       in not (T.null s)
            && T.length s <= 64
            && not ("http" `T.isInfixOf` T.toLower s)
            && not ("arn:" `T.isInfixOf` T.toLower s)
            && not ("//" `T.isInfixOf` s)
            && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("_ -" :: String))) s

computeUtilizationBps :: Integer -> Integer -> Maybe Int
computeUtilizationBps used cap
  | cap <= 0 || used < 0 = Nothing
  | otherwise = Just $ fromIntegral (min 100000 ((used * 10000) `div` cap))

computeRunwayHours :: Integer -> Integer -> Maybe Integer
computeRunwayHours depositWei burn24hWei
  | depositWei < 0 || burn24hWei <= 0 = Nothing
  | otherwise = Just ((depositWei * 24) `div` burn24hWei)

computeSuccessRateBps :: Int -> Int -> Maybe Int
computeSuccessRateBps succeeded total
  | total <= 0 || succeeded < 0 = Nothing
  | otherwise = Just ((min succeeded total * 10000) `div` total)

classifySponsorshipGate
  :: Bool
  -> Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Bool
  -> Maybe Bool
  -> Text
classifySponsorshipGate sponsorshipEnabled submissionEnabled mIssuancePaused mOnchainPaused reconcilerFresh mBundlerReachable
  | not sponsorshipEnabled || not submissionEnabled = "disabled"
  | mIssuancePaused == Just True || mOnchainPaused == Just True = "paused"
  | not reconcilerFresh || mBundlerReachable == Just False = "degraded"
  | mIssuancePaused == Nothing && mOnchainPaused == Nothing = "degraded"
  | otherwise = "issuing"

decodeEntryPointBalance :: ByteString -> Maybe Integer
decodeEntryPointBalance bytes
  | BS.length bytes /= 32 = Nothing
  | otherwise = Just (decodeUint256 bytes)

decodePaymasterPaused :: ByteString -> Maybe Bool
decodePaymasterPaused bytes
  | BS.length bytes /= 32 = Nothing
  | otherwise = case decodeUint256 bytes of
      0 -> Just False
      1 -> Just True
      _ -> Nothing

checkBundlerEntryPoint :: Text -> Value -> Bool
checkBundlerEntryPoint expected (Array entries) =
  let needle = T.toLower expected
   in any
        ( \case
            String ep -> T.toLower ep == needle
            _ -> False
        )
        (V.toList entries)
checkBundlerEntryPoint _ _ = False

-- JSON serializers

networkJson :: Integer -> Value
networkJson chainId =
  object
    [ "chainId" .= chainId
    , "releaseVersion" .= Manifest.releaseVersion
    , "settlementAssetSymbol" .= ("USDG" :: Text)
    ]

budgetGaugeJson :: Integer -> Integer -> Value
budgetGaugeJson used cap =
  object
    [ "usedWei" .= T.pack (show used)
    , "capWei" .= T.pack (show cap)
    , "utilizationBps" .= computeUtilizationBps used cap
    ]

windowMetricsJson :: WindowMetrics -> Value
windowMetricsJson WindowMetrics {..} =
  object
    [ "totalOps" .= wmTotalOps
    , "succeededOps" .= wmSucceededOps
    , "revertedOps" .= wmRevertedOps
    , "successRateBps" .= computeSuccessRateBps wmSucceededOps wmTotalOps
    , "gasSpentWei" .= T.pack (show wmGasSpentWei)
    , "gasEfficiencyBps" .= computeUtilizationBps wmGasSpentWei wmReservedMaxWei
    ]

funnelJson :: FunnelCounts -> Value
funnelJson FunnelCounts {..} =
  object
    [ "reserved" .= fcReserved
    , "signed" .= fcSigned
    , "submitted" .= fcSubmitted
    , "settled" .= fcSettled
    , "expired" .= fcExpired
    , "cancelled" .= fcCancelled
    ]

closeAssistanceJson :: CloseAssistanceSummary -> Value
closeAssistanceJson CloseAssistanceSummary {..} =
  object
    [ "available" .= casAvailable
    , "verifiedCount24h" .= casVerified24h
    , "verifiedCount7d" .= casVerified7d
    , "totalAssisted7dAssets" .= T.pack (show casTotalAssets7d)
    , "pendingUnverifiedCount" .= casPendingUnverified
    , "verificationRateBps" .= computeSuccessRateBps casVerified7d casTotalAttempts7d
    ]

opRowToJson :: SponsoredOpRow -> Value
opRowToJson SponsoredOpRow {..} =
  object
    [ "userOpHash" .= T.toLower sorUserOpHash
    , "txHash" .= T.toLower sorTxHash
    , "sender" .= T.toLower sorSender
    , "blockNumber" .= sorBlockNumber
    , "success" .= sorSuccess
    , "actualGasCostWei" .= T.pack (show sorActualGasCostWei)
    , "maxCostWei" .= T.pack (show sorMaxCostWei)
    , "efficiencyBps" .= computeUtilizationBps sorActualGasCostWei sorMaxCostWei
    , "latencySeconds" .= sorLatencySeconds
    , "occurredAt" .= formatUtcIso sorOccurredAt
    , "finalizedAt" .= formatUtcIso sorFinalizedAt
    ]

controlEventToJson :: ControlEventRow -> Value
controlEventToJson ControlEventRow {..} =
  object
    [ "id" .= cerId
    , "action" .= cerAction
    , "reason" .= cerReason
    , "createdAt" .= formatUtcIso cerCreatedAt
    ]

normalizeStatusFilter :: Maybe Text -> Maybe Bool
normalizeStatusFilter (Just raw) = case T.toLower (T.strip raw) of
  "succeeded" -> Just True
  "reverted" -> Just False
  _ -> Nothing
normalizeStatusFilter Nothing = Nothing

parseIntegerOrZero :: Text -> Integer
parseIntegerOrZero = fromMaybe 0 . readMaybe . T.unpack . T.strip

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
