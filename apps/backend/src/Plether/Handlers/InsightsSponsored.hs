module Plether.Handlers.InsightsSponsored
  ( getSponsoredStatusResponse
  , getSponsoredOperationsResponse
  , getTradingAccountsOverviewResponse
  , getTradingAccountDetailResponse
  , classifyKernelHealth
  , classifyAccountBytecode
  , sanitizePublicReason
  , sanitizeControlEventReason
  , parseLogBlockTimestamp
  , resolveInclusionEpoch
  , computeInclusionLatencySeconds
  , computeIntLatencyPercentiles
  , computeUtilizationBps
  , computeRunwayHours
  , computeSuccessRateBps
  , classifySponsorshipGate
  , decodeEntryPointBalance
  , decodePaymasterPaused
  , checkBundlerEntryPoint
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Char (isAlphaNum, isAscii)
import Data.Either (isRight)
import Data.IORef (newIORef)
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Connection, Only (..), (:.) (..), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Network.HTTP.Client (Manager)
import Numeric (readHex)
import Plether.AA.Kernel (KernelProfile (..))
import Plether.AA.KernelGateway (KernelGatewayConfig (..), KernelSponsorshipConfig (..), kernelScope)
import Plether.AA.Readiness (Check (..), aggregateFunding)
import Plether.Cache (SingleFlightCache, newRefreshingSingleFlightCache, runSingleFlightCache)
import Plether.Config (Config (..), NativeAaConfig (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.KernelOperation (KernelScope (..), KernelSponsorBudget (..))
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall, encodeUint256, keccak256)
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , RpcClientOptions (..)
  , ethBlockNumber
  , ethCall
  , ethCallAtBlock
  , newClientWithManager
  , rpcCall
  )
import Plether.Ethereum.Rpc (ethGetBalance, ethGetCode)
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
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

{-# NOINLINE sponsoredStatusCache #-}
sponsoredStatusCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
sponsoredStatusCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (15 * 1000000000) (300 * 1000000000))

getSponsoredStatusResponse
  :: DbPool
  -> EthClient
  -> Manager
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
getSponsoredStatusResponse pool client manager cfg mKernel =
  snd <$> runSingleFlightCache sponsoredStatusCache () isRight (computeSponsoredStatusResponse pool client manager cfg mKernel)

computeSponsoredStatusResponse
  :: DbPool
  -> EthClient
  -> Manager
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
computeSponsoredStatusResponse pool client manager cfg mKernel = do
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
            <$> Concurrently (observeIo $ withDb pool $ \conn -> fetchNativeOperations conn chainId limitCount statusFilter)
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
      query
        conn
        "SELECT \
        \  e.event_json->>'blockTimestamp', \
        \  (SELECT pe.timestamp FROM perps_events pe WHERE pe.chain_id = ? AND pe.tx_hash = e.transaction_hash LIMIT 1), \
        \  FLOOR(EXTRACT(EPOCH FROM COALESCE(a.submitted_at, a.signed_at, a.created_at)))::BIGINT \
        \FROM aa_user_operation_events e \
        \JOIN aa_sponsorship_authorizations a USING (digest) \
        \WHERE e.finalized_at >= NOW() - interval '24 hours' \
        \ORDER BY e.finalized_at DESC \
        \LIMIT 2000"
        (Only chainId)
        :: IO [(Maybe Text, Maybe Integer, Integer)]
    let inclusionSamples =
          mapMaybe
            ( \(hexTs, peTs, submitTs) ->
                computeInclusionLatencySeconds (resolveInclusionEpoch hexTs peTs) submitTs
            )
            latRows
        latencyPair = computeIntLatencyPercentiles inclusionSamples

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

data NativeOpQueryRow = NativeOpQueryRow
  { noqrUserOpHash :: Text
  , noqrTxHash :: Text
  , noqrSender :: Text
  , noqrBlockNumber :: Text
  , noqrSuccess :: Bool
  , noqrActualGasCost :: Text
  , noqrMaxCost :: Text
  , noqrEventBlockTsHex :: Maybe Text
  , noqrPerpsEventTs :: Maybe Integer
  , noqrSubmitEpoch :: Integer
  , noqrObservedAt :: UTCTime
  , noqrFinalizedAt :: UTCTime
  }

instance FromRow NativeOpQueryRow where
  fromRow =
    NativeOpQueryRow
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

fetchNativeOperations :: Connection -> Integer -> Int -> Maybe Bool -> IO [SponsoredOpRow]
fetchNativeOperations conn chainId limitCount mSuccessFilter = do
  rows <-
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
      \  e.event_json->>'blockTimestamp', \
      \  (SELECT pe.timestamp FROM perps_events pe WHERE pe.chain_id = ? AND pe.tx_hash = e.transaction_hash LIMIT 1), \
      \  FLOOR(EXTRACT(EPOCH FROM COALESCE(a.submitted_at, a.signed_at, a.created_at)))::BIGINT, \
      \  e.observed_at, \
      \  e.finalized_at \
      \FROM aa_user_operation_events e \
      \JOIN aa_sponsorship_authorizations a USING (digest) \
      \WHERE (?::BOOLEAN IS NULL OR e.success = ?) \
      \ORDER BY e.finalized_at DESC, e.block_number DESC \
      \LIMIT ?"
      (chainId, mSuccessFilter, mSuccessFilter, limitCount)
      :: IO [NativeOpQueryRow]
  pure
    [ let mIncEpoch = resolveInclusionEpoch noqrEventBlockTsHex noqrPerpsEventTs
          occurredAt = maybe noqrObservedAt (posixSecondsToUTCTime . fromIntegral) mIncEpoch
          latSec = computeInclusionLatencySeconds mIncEpoch noqrSubmitEpoch
       in SponsoredOpRow
            { sorUserOpHash = noqrUserOpHash
            , sorTxHash = noqrTxHash
            , sorSender = noqrSender
            , sorBlockNumber = noqrBlockNumber
            , sorSuccess = noqrSuccess
            , sorActualGasCostWei = parseIntegerOrZero noqrActualGasCost
            , sorMaxCostWei = parseIntegerOrZero noqrMaxCost
            , sorLatencySeconds = latSec
            , sorOccurredAt = occurredAt
            , sorFinalizedAt = noqrFinalizedAt
            }
    | NativeOpQueryRow {..} <- rows
    ]

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
    [ ControlEventRow eid action (sanitizeControlEventReason action reason) ts
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

sanitizeControlEventReason :: Text -> Text -> Text
sanitizeControlEventReason action raw =
  let sanitized = sanitizePublicReason raw
   in case T.toLower (T.strip action) of
        "resume"
          | sanitized == "Safety pause active" || sanitized == "Sponsorship issuance resumed" ->
              "Sponsorship issuance resumed"
          | otherwise -> "Resumed after: " <> sanitized
        _ -> sanitized

parseLogBlockTimestamp :: Maybe Text -> Maybe Integer
parseLogBlockTimestamp Nothing = Nothing
parseLogBlockTimestamp (Just raw) =
  let stripped = T.strip raw
      hexBody = fromMaybe stripped (T.stripPrefix "0x" stripped <|> T.stripPrefix "0X" stripped)
   in if T.null hexBody
        then Nothing
        else case readHex (T.unpack hexBody) of
          [(n, "")] | n > 0 -> Just n
          _ -> Nothing

resolveInclusionEpoch :: Maybe Text -> Maybe Integer -> Maybe Integer
resolveInclusionEpoch mHexTs mPerpsTs =
  parseLogBlockTimestamp mHexTs <|> (mPerpsTs >>= (\ts -> if ts > 0 then Just ts else Nothing))

computeInclusionLatencySeconds :: Maybe Integer -> Integer -> Maybe Int
computeInclusionLatencySeconds Nothing _ = Nothing
computeInclusionLatencySeconds (Just inclusionEpoch) submitEpoch =
  Just (fromIntegral (max 0 (inclusionEpoch - submitEpoch)))

computeIntLatencyPercentiles :: [Int] -> (Maybe Int, Maybe Int)
computeIntLatencyPercentiles samples =
  case sort (filter (>= 0) samples) of
    [] -> (Nothing, Nothing)
    sorted ->
      let count = length sorted
          atPct p = sorted !! min (count - 1) ((count * p + 99) `div` 100 - 1)
       in (Just (atPct 50), Just (atPct 95))

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

-- ============================================================================
-- ZeroDev Kernel v4 Trading Accounts Overview
-- GET /api/insights/v1/trading-accounts/overview
-- ============================================================================

{-# NOINLINE tradingAccountsCache #-}
tradingAccountsCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
tradingAccountsCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (15 * 1000000000) (300 * 1000000000))

-- | Classify overall ZeroDev Kernel v4 Trading Account stack health.
classifyKernelHealth
  :: Bool -- ^ ethModeEnabled
  -> Bool -- ^ sponsorshipEnabled
  -> Bool -- ^ bundlerReachable
  -> Bool -- ^ bundlerEntryPointSupported
  -> Bool -- ^ reconcilerFresh
  -> Maybe Text -- ^ reconcilerPausedReason
  -> Text
classifyKernelHealth ethEnabled sponsorEnabled bundlerOk epOk recFresh mPaused
  | not ethEnabled && not sponsorEnabled = "disabled"
  | maybe False (const True) mPaused = "paused"
  | not bundlerOk || not epOk || not recFresh = "degraded"
  | otherwise = "healthy"

data KernelAdoptionRow = KernelAdoptionRow
  { karUniqueAccounts :: Int
  , karUniqueOwners :: Int
  , karActiveAccounts24h :: Int
  , karTotalPreparations :: Int
  , karTotalOperations :: Int
  , karOperations24h :: Int
  , karSponsoredOpsTotal :: Int
  , karEthOpsTotal :: Int
  , karSponsoredOps24h :: Int
  , karEthOps24h :: Int
  , karCancellationOpsTotal :: Int
  , karPendingOpsCount :: Int
  , karOldestPendingAgeSec :: Maybe Int
  , karIncludedSucceeded :: Int
  , karIncludedReverted :: Int
  , karNonceConsumed :: Int
  , karSponsorshipExpired :: Int
  , karCancellationsPrepared :: Int
  , karCancellationsAccepted :: Int
  , karTotalGasCostWei :: Text
  , karSponsoredGasCostWei :: Text
  , karEthGasCostWei :: Text
  }
  deriving stock (Eq, Show)

instance FromRow KernelAdoptionRow where
  fromRow =
    KernelAdoptionRow
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field

emptyKernelAdoptionRow :: KernelAdoptionRow
emptyKernelAdoptionRow =
  KernelAdoptionRow 0 0 0 0 0 0 0 0 0 0 0 0 Nothing 0 0 0 0 0 0 "0" "0" "0"

data KernelSmartAccountRow = KernelSmartAccountRow
  { ksarSender :: Text
  , ksarOwner :: Text
  , ksarDiscordUsername :: Maybe Text
  , ksarSponsoredCount :: Int
  , ksarEthCount :: Int
  , ksarIncludedCount :: Int
  , ksarPendingCount :: Int
  , ksarRetiredCount :: Int
  , ksarHighestNonce :: Text
  , ksarLastMode :: Text
  , ksarFirstSeenAt :: UTCTime
  , ksarLastActivityAt :: UTCTime
  }
  deriving stock (Eq, Show)

instance FromRow KernelSmartAccountRow where
  fromRow =
    KernelSmartAccountRow
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field

data KernelOpJournalRow = KernelOpJournalRow
  { kojrUserOpHash :: Text
  , kojrSender :: Text
  , kojrOwner :: Text
  , kojrMode :: Text
  , kojrCancellationOnly :: Bool
  , kojrNonce :: Text
  , kojrStatus :: Text
  , kojrExecutionSuccess :: Maybe Bool
  , kojrTxHash :: Maybe Text
  , kojrBlockNumber :: Maybe Text
  , kojrActualGasCostWei :: Maybe Text
  , kojrCancellationHash :: Maybe Text
  , kojrCreatedAt :: UTCTime
  }
  deriving stock (Eq, Show)

instance FromRow KernelOpJournalRow where
  fromRow =
    KernelOpJournalRow
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field

getTradingAccountsOverviewResponse
  :: DbPool
  -> EthClient
  -> Manager
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
getTradingAccountsOverviewResponse pool client manager cfg mKernel =
  snd <$> runSingleFlightCache tradingAccountsCache () isRight (computeTradingAccountsOverview pool client manager cfg mKernel)

computeTradingAccountsOverview
  :: DbPool
  -> EthClient
  -> Manager
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
computeTradingAccountsOverview pool client manager cfg mKernel = do
  nowPosix <- getPOSIXTime
  let nowSec = floor nowPosix :: Integer
      nowUtc = posixSecondsToUTCTime (fromIntegral nowSec)
      chainId = cfgPerpsChainId cfg
  blockRes <- ethBlockNumber client
  let mHeadBlock = either (const Nothing) Just blockRes
      respBlock = fromMaybe Manifest.releaseDeploymentBlock mHeadBlock
  case mKernel of
    Nothing ->
      pure $
        Right $
          mkResponse respBlock chainId $
            object
              [ "network" .= networkJson chainId
              , "observedAt" .= isoTimestamp nowSec
              , "configured" .= False
              , "status" .= ("unconfigured" :: Text)
              , "profile" .= Null
              , "controls" .= Null
              , "pipeline" .= Null
              , "adoption" .= kernelAdoptionJson emptyKernelAdoptionRow
              , "lifecycle" .= kernelLifecycleJson emptyKernelAdoptionRow
              , "accounts" .= ([] :: [Value])
              , "operations" .= ([] :: [Value])
              ]
    Just kgc -> do
      let prof = kgcProfile kgc
          scope = kernelScope prof
          entryPointAddr = T.toLower (kpEntryPoint prof)
          mPaymaster = T.toLower . kscPaymaster <$> kgcSponsorship kgc
          mPolicyId = kscPolicyId <$> kgcSponsorship kgc
          sponsorshipEnabled = maybe False kscEnabled (kgcSponsorship kgc)
          (admissionMode, mAdmittedCount) = case kgcAllowedOwners kgc of
            Nothing -> ("open" :: Text, Nothing :: Maybe Int)
            Just owners -> ("canary" :: Text, Just (length owners))
      (bundlerRes, dbRes) <-
        runConcurrently $
          (,)
            <$> Concurrently (probeBundler manager (kgcAltoRpcUrl kgc) entryPointAddr)
            <*> Concurrently (fetchKernelTradingAccountsDb pool scope mPaymaster)
      let (bundlerOk, bundlerEpOk, bundlerStatus) = bundlerRes
          (mRec, mPausedRaw, adopt, accounts, ops) = fromMaybe (Nothing, Nothing, emptyKernelAdoptionRow, [], []) dbRes
          (recFresh, mLagSec, mBlocksBehind) =
            evaluateReconcilerFreshness mRec mHeadBlock nowUtc (kgcMaxSafeLagSeconds kgc)
          pausedReason = sanitizePublicReason <$> mPausedRaw
          overallStatus =
            classifyKernelHealth
              (kgcEthModeEnabled kgc)
              sponsorshipEnabled
              bundlerOk
              bundlerEpOk
              (maybe True (const recFresh) mPaymaster)
              pausedReason
      pure $
        Right $
          mkResponse respBlock chainId $
            object
              [ "network" .= networkJson chainId
              , "observedAt" .= isoTimestamp nowSec
              , "configured" .= True
              , "status" .= overallStatus
              , "profile"
                  .= object
                    [ "deployment" .= kpDeployment prof
                    , "accountVersion" .= ("kernel-v4-uups" :: Text)
                    , "entryPointVersion" .= ("0.9" :: Text)
                    , "entryPoint" .= entryPointAddr
                    , "entryPointCodeHash" .= kpEntryPointCodeHash prof
                    , "factory" .= T.toLower (kpFactory prof)
                    , "factoryCodeHash" .= kpFactoryCodeHash prof
                    , "implementation" .= T.toLower (kpImplementation prof)
                    , "implementationCodeHash" .= kpImplementationCodeHash prof
                    , "validator" .= T.toLower (kpValidator prof)
                    , "validatorCodeHash" .= kpValidatorCodeHash prof
                    , "accountCodeHash" .= kpAccountCodeHash prof
                    , "paymaster" .= mPaymaster
                    , "policyId" .= mPolicyId
                    ]
              , "controls"
                  .= object
                    [ "ethModeEnabled" .= kgcEthModeEnabled kgc
                    , "sponsorshipEnabled" .= sponsorshipEnabled
                    , "protectionCommitsEnabled" .= kgcProtectionCommitsEnabled kgc
                    , "ownerAdmissionMode" .= admissionMode
                    , "admittedOwnerCount" .= mAdmittedCount
                    ]
              , "pipeline"
                  .= object
                    [ "bundlerReachable" .= bundlerOk
                    , "bundlerEntryPointSupported" .= bundlerEpOk
                    , "bundlerStatus" .= bundlerStatus
                    , "reconcilerSafeBlock" .= (T.pack . show . rrSafeBlock <$> mRec)
                    , "reconcilerHeadBlock" .= (T.pack . show <$> mHeadBlock)
                    , "reconcilerBlocksBehind" .= mBlocksBehind
                    , "reconcilerLagSeconds" .= mLagSec
                    , "reconcilerMaxSafeLagSeconds" .= kgcMaxSafeLagSeconds kgc
                    , "reconcilerFresh" .= recFresh
                    , "reconcilerPausedReason" .= pausedReason
                    ]
              , "adoption" .= kernelAdoptionJson adopt
              , "lifecycle" .= kernelLifecycleJson adopt
              , "accounts" .= map kernelSmartAccountToJson accounts
              , "operations" .= map kernelOpJournalToJson ops
              ]

fetchKernelTradingAccountsDb
  :: DbPool
  -> KernelScope
  -> Maybe Text
  -> IO
      ( Maybe
          ( Maybe ReconcilerRow
          , Maybe Text
          , KernelAdoptionRow
          , [KernelSmartAccountRow]
          , [KernelOpJournalRow]
          )
      )
fetchKernelTradingAccountsDb pool KernelScope {..} mPaymaster =
  observeIo $ withDb pool $ \conn -> do
    (mRec, mPausedRaw) <- case mPaymaster of
      Nothing -> pure (Nothing, Nothing)
      Just pm -> do
        recRows <-
          query
            conn
            "SELECT safe_block, last_success_at, paused_reason \
            \FROM aa_kernel_reconciler_state \
            \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? LIMIT 1"
            (ksDeployment, ksChainId, ksEntryPoint, pm)
            :: IO [(Maybe Integer, Maybe UTCTime, Maybe Text)]
        pure $ case recRows of
          [(Just sb, Just ts, mReason)] -> (Just (ReconcilerRow sb ts), mReason)
          [(_, _, mReason)] -> (Nothing, mReason)
          _ -> (Nothing, Nothing)

    adoptRows <-
      query
        conn
        "WITH prep_agg AS ( \
        \  SELECT \
        \    COUNT(DISTINCT sender)::INT AS unique_accounts, \
        \    COUNT(DISTINCT owner_address)::INT AS unique_owners, \
        \    COUNT(DISTINCT sender) FILTER (WHERE created_at >= NOW() - interval '24 hours')::INT AS active_24h, \
        \    COUNT(*)::INT AS total_preps \
        \  FROM aa_kernel_preparations \
        \  WHERE deployment = ? AND chain_id = ? AND entry_point = ? \
        \), op_agg AS ( \
        \  SELECT \
        \    COUNT(*)::INT AS total_ops, \
        \    COUNT(*) FILTER (WHERE created_at >= NOW() - interval '24 hours')::INT AS ops_24h, \
        \    COUNT(*) FILTER (WHERE mode = 'sponsored')::INT AS sponsored_total, \
        \    COUNT(*) FILTER (WHERE mode = 'eth')::INT AS eth_total, \
        \    COUNT(*) FILTER (WHERE mode = 'sponsored' AND created_at >= NOW() - interval '24 hours')::INT AS sponsored_24h, \
        \    COUNT(*) FILTER (WHERE mode = 'eth' AND created_at >= NOW() - interval '24 hours')::INT AS eth_24h, \
        \    COUNT(*) FILTER (WHERE cancellation_only)::INT AS cancel_ops_total, \
        \    COUNT(*) FILTER (WHERE status = 'pending')::INT AS pending_count, \
        \    MAX(GREATEST(0, EXTRACT(EPOCH FROM (NOW() - created_at)))) FILTER (WHERE status = 'pending')::INT AS oldest_pending_sec, \
        \    COUNT(*) FILTER (WHERE status = 'included' AND COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE))::INT AS inc_ok, \
        \    COUNT(*) FILTER (WHERE status = 'included' AND NOT COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE))::INT AS inc_rev, \
        \    COUNT(*) FILTER (WHERE status = 'nonce_consumed')::INT AS nonce_consumed, \
        \    COUNT(*) FILTER (WHERE status = 'sponsorship_expired')::INT AS sponsor_expired, \
        \    COALESCE(SUM(CASE WHEN (terminal_evidence->>'actualGasCost') ~ '^[0-9]+$' THEN (terminal_evidence->>'actualGasCost')::NUMERIC ELSE 0 END), 0)::TEXT AS tot_gas, \
        \    COALESCE(SUM(CASE WHEN mode = 'sponsored' AND (terminal_evidence->>'actualGasCost') ~ '^[0-9]+$' THEN (terminal_evidence->>'actualGasCost')::NUMERIC ELSE 0 END), 0)::TEXT AS spon_gas, \
        \    COALESCE(SUM(CASE WHEN mode = 'eth' AND (terminal_evidence->>'actualGasCost') ~ '^[0-9]+$' THEN (terminal_evidence->>'actualGasCost')::NUMERIC ELSE 0 END), 0)::TEXT AS eth_gas \
        \  FROM aa_kernel_operations \
        \  WHERE deployment = ? AND chain_id = ? AND entry_point = ? \
        \), cancel_agg AS ( \
        \  SELECT \
        \    COUNT(*)::INT AS c_prep, \
        \    COUNT(*) FILTER (WHERE cancellation_hash IS NOT NULL)::INT AS c_acc \
        \  FROM aa_kernel_cancellations \
        \  WHERE deployment = ? AND chain_id = ? AND entry_point = ? \
        \) \
        \SELECT \
        \  p.unique_accounts, p.unique_owners, p.active_24h, p.total_preps, \
        \  o.total_ops, o.ops_24h, o.sponsored_total, o.eth_total, o.sponsored_24h, o.eth_24h, \
        \  o.cancel_ops_total, o.pending_count, o.oldest_pending_sec, \
        \  o.inc_ok, o.inc_rev, o.nonce_consumed, o.sponsor_expired, \
        \  c.c_prep, c.c_acc, o.tot_gas, o.spon_gas, o.eth_gas \
        \FROM prep_agg p, op_agg o, cancel_agg c"
        ( ksDeployment, ksChainId, ksEntryPoint
        , ksDeployment, ksChainId, ksEntryPoint
        , ksDeployment, ksChainId, ksEntryPoint
        ) :: IO [KernelAdoptionRow]
    let adopt = case adoptRows of
          (r : _) -> r
          [] -> emptyKernelAdoptionRow

    acctRows <-
      query
        conn
        "SELECT \
        \  p.sender, \
        \  MAX(p.owner_address) AS owner_address, \
        \  MAX(l.discord_username) AS discord_username, \
        \  COUNT(o.user_operation_hash) FILTER (WHERE o.mode = 'sponsored')::INT AS sponsored_count, \
        \  COUNT(o.user_operation_hash) FILTER (WHERE o.mode = 'eth')::INT AS eth_count, \
        \  COUNT(o.user_operation_hash) FILTER (WHERE o.status = 'included')::INT AS included_count, \
        \  COUNT(o.user_operation_hash) FILTER (WHERE o.status = 'pending')::INT AS pending_count, \
        \  COUNT(o.user_operation_hash) FILTER (WHERE o.status IN ('nonce_consumed', 'sponsorship_expired'))::INT AS retired_count, \
        \  COALESCE(MAX(COALESCE(o.nonce, p.nonce)), 0)::TEXT AS highest_nonce, \
        \  (ARRAY_AGG(COALESCE(o.mode, p.mode) ORDER BY COALESCE(o.created_at, p.created_at) DESC))[1] AS last_mode, \
        \  MIN(p.created_at) AS first_seen_at, \
        \  MAX(COALESCE(o.created_at, p.created_at)) AS last_activity_at \
        \FROM aa_kernel_preparations p \
        \LEFT JOIN aa_kernel_operations o \
        \  ON o.deployment = p.deployment AND o.chain_id = p.chain_id \
        \  AND o.entry_point = p.entry_point AND o.preparation_id = p.preparation_id \
        \LEFT JOIN insights_divisions_links l \
        \  ON LOWER(l.trading_account) = LOWER(p.sender) \
        \WHERE p.deployment = ? AND p.chain_id = ? AND p.entry_point = ? \
        \GROUP BY p.sender \
        \ORDER BY last_activity_at DESC \
        \LIMIT 50"
        (ksDeployment, ksChainId, ksEntryPoint) :: IO [KernelSmartAccountRow]

    opRows <-
      query
        conn
        "SELECT \
        \  o.user_operation_hash, \
        \  o.sender, \
        \  p.owner_address, \
        \  o.mode, \
        \  o.cancellation_only, \
        \  o.nonce::TEXT, \
        \  o.status, \
        \  (o.terminal_evidence->>'executionSuccess')::BOOLEAN, \
        \  o.terminal_evidence->>'transactionHash', \
        \  o.terminal_evidence->>'blockNumber', \
        \  o.terminal_evidence->>'actualGasCost', \
        \  o.terminal_evidence->>'cancellationHash', \
        \  o.created_at \
        \FROM aa_kernel_operations o \
        \JOIN aa_kernel_preparations p \
        \  USING (deployment, chain_id, entry_point, preparation_id) \
        \WHERE o.deployment = ? AND o.chain_id = ? AND o.entry_point = ? \
        \ORDER BY o.created_at DESC \
        \LIMIT 60"
        (ksDeployment, ksChainId, ksEntryPoint) :: IO [KernelOpJournalRow]

    pure (mRec, mPausedRaw, adopt, acctRows, opRows)

kernelAdoptionJson :: KernelAdoptionRow -> Value
kernelAdoptionJson KernelAdoptionRow {..} =
  object
    [ "uniqueAccounts" .= karUniqueAccounts
    , "uniqueOwners" .= karUniqueOwners
    , "activeAccounts24h" .= karActiveAccounts24h
    , "totalPreparations" .= karTotalPreparations
    , "totalOperations" .= karTotalOperations
    , "operations24h" .= karOperations24h
    , "sponsoredOpsTotal" .= karSponsoredOpsTotal
    , "ethOpsTotal" .= karEthOpsTotal
    , "sponsoredOps24h" .= karSponsoredOps24h
    , "ethOps24h" .= karEthOps24h
    , "cancellationOpsTotal" .= karCancellationOpsTotal
    , "pendingOpsCount" .= karPendingOpsCount
    , "oldestPendingAgeSeconds" .= karOldestPendingAgeSec
    ]

kernelLifecycleJson :: KernelAdoptionRow -> Value
kernelLifecycleJson KernelAdoptionRow {..} =
  object
    [ "includedSucceeded" .= karIncludedSucceeded
    , "includedReverted" .= karIncludedReverted
    , "pending" .= karPendingOpsCount
    , "nonceConsumed" .= karNonceConsumed
    , "sponsorshipExpired" .= karSponsorshipExpired
    , "cancellationsPrepared" .= karCancellationsPrepared
    , "cancellationsAccepted" .= karCancellationsAccepted
    , "successRateBps" .= computeSuccessRateBps karIncludedSucceeded (karIncludedSucceeded + karIncludedReverted)
    , "totalGasCostWei" .= karTotalGasCostWei
    , "sponsoredGasCostWei" .= karSponsoredGasCostWei
    , "ethGasCostWei" .= karEthGasCostWei
    ]

kernelSmartAccountToJson :: KernelSmartAccountRow -> Value
kernelSmartAccountToJson KernelSmartAccountRow {..} =
  object
    [ "sender" .= T.toLower ksarSender
    , "ownerAddress" .= T.toLower ksarOwner
    , "discordUsername" .= ksarDiscordUsername
    , "sponsoredCount" .= ksarSponsoredCount
    , "ethCount" .= ksarEthCount
    , "includedCount" .= ksarIncludedCount
    , "pendingCount" .= ksarPendingCount
    , "retiredCount" .= ksarRetiredCount
    , "highestNonce" .= ksarHighestNonce
    , "lastMode" .= ksarLastMode
    , "firstSeenAt" .= formatUtcIso ksarFirstSeenAt
    , "lastActivityAt" .= formatUtcIso ksarLastActivityAt
    ]

kernelOpJournalToJson :: KernelOpJournalRow -> Value
kernelOpJournalToJson KernelOpJournalRow {..} =
  object
    [ "userOpHash" .= T.toLower kojrUserOpHash
    , "sender" .= T.toLower kojrSender
    , "ownerAddress" .= T.toLower kojrOwner
    , "mode" .= kojrMode
    , "cancellationOnly" .= kojrCancellationOnly
    , "nonce" .= kojrNonce
    , "status" .= kojrStatus
    , "executionSuccess" .= kojrExecutionSuccess
    , "txHash" .= (T.toLower <$> kojrTxHash)
    , "blockNumber" .= kojrBlockNumber
    , "actualGasCostWei" .= kojrActualGasCostWei
    , "cancellationHash" .= (T.toLower <$> kojrCancellationHash)
    , "createdAt" .= formatUtcIso kojrCreatedAt
    ]

-- ---------------------------------------------------------------------------
-- ZeroDev Kernel v4 Per-Account Detail (/api/insights/v1/trading-accounts/:address)
-- ---------------------------------------------------------------------------

classifyAccountBytecode :: ByteString -> Text -> (Bool, Maybe Text, Maybe Bool)
classifyAccountBytecode code expectedCodeHash
  | BS.null code = (False, Nothing, Nothing)
  | otherwise =
      let actualHash = "0x" <> TE.decodeUtf8 (B16.encode (keccak256 code))
       in (True, Just actualHash, Just (T.toLower actualHash == T.toLower expectedCodeHash))

data AccountKernelSummaryRow = AccountKernelSummaryRow
  { aksOwner :: Maybe Text
  , aksDiscordUsername :: Maybe Text
  , aksFirstSeenAt :: Maybe UTCTime
  , aksLastActivityAt :: Maybe UTCTime
  , aksTotalPreps :: Int
  , aksUnsubmittedPreps :: Int
  , aksTotalOps :: Int
  , aksOps24h :: Int
  , aksSponsoredCount :: Int
  , aksEthCount :: Int
  , aksCancelOpsCount :: Int
  , aksIncludedSucceeded :: Int
  , aksIncludedReverted :: Int
  , aksPendingCount :: Int
  , aksNonceConsumed :: Int
  , aksSponsorshipExpired :: Int
  , aksCancellationsPrepared :: Int
  , aksCancellationsAccepted :: Int
  , aksTotalGasWei :: Text
  , aksSponsoredGasWei :: Text
  , aksEthGasWei :: Text
  , aksOldestPendingSec :: Maybe Int
  , aksHighestNonce :: Maybe Text
  , aksHighestIncludedNonce :: Maybe Text
  , aksLowestPendingNonce :: Maybe Text
  }
  deriving stock (Eq, Show)

instance FromRow AccountKernelSummaryRow where
  fromRow =
    AccountKernelSummaryRow
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field

emptyAccountKernelSummaryRow :: AccountKernelSummaryRow
emptyAccountKernelSummaryRow =
  AccountKernelSummaryRow
    Nothing Nothing Nothing Nothing
    0 0 0 0 0 0 0 0 0 0 0 0 0 0
    "0" "0" "0" Nothing Nothing Nothing Nothing

data AccountPendingOpRow = AccountPendingOpRow
  { apoUserOpHash :: Text
  , apoPrepId :: Text
  , apoMode :: Text
  , apoCancellationOnly :: Bool
  , apoNonce :: Text
  , apoAgeSeconds :: Int
  , apoPreparedSafeBlock :: Maybe Text
  , apoNonceScanThroughBlock :: Maybe Text
  , apoSponsorshipValidUntil :: Maybe Integer
  , apoReservedMaxCostWei :: Maybe Text
  , apoCancellationPrepared :: Bool
  , apoCancellationHash :: Maybe Text
  , apoCreatedAt :: UTCTime
  }
  deriving stock (Eq, Show)

instance FromRow AccountPendingOpRow where
  fromRow =
    AccountPendingOpRow
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field

data AccountOpRow = AccountOpRow
  { aorUserOpHash :: Text
  , aorPrepId :: Text
  , aorMode :: Text
  , aorCancellationOnly :: Bool
  , aorNonce :: Text
  , aorStatus :: Text
  , aorExecutionSuccess :: Maybe Bool
  , aorReservedMaxCostWei :: Maybe Text
  , aorActualGasCostWei :: Maybe Text
  , aorTxHash :: Maybe Text
  , aorBlockNumber :: Maybe Text
  , aorCancellationHash :: Maybe Text
  , aorCreatedAt :: UTCTime
  }
  deriving stock (Eq, Show)

instance FromRow AccountOpRow where
  fromRow =
    AccountOpRow
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field

data AccountCancelRow = AccountCancelRow
  { acrPrepId :: Text
  , acrOriginalHash :: Text
  , acrCancellationHash :: Maybe Text
  , acrNonce :: Text
  , acrExpiresAt :: UTCTime
  , acrCreatedAt :: UTCTime
  }
  deriving stock (Eq, Show)

instance FromRow AccountCancelRow where
  fromRow = AccountCancelRow <$> field <*> field <*> field <*> field <*> field <*> field

data AccountUnsubmittedPrepRow = AccountUnsubmittedPrepRow
  { aupPrepId :: Text
  , aupMode :: Text
  , aupNonce :: Text
  , aupPreparedSafeBlock :: Maybe Text
  , aupNonceScanThroughBlock :: Maybe Text
  , aupExpired :: Bool
  , aupExpiresAt :: UTCTime
  , aupCreatedAt :: UTCTime
  }
  deriving stock (Eq, Show)

instance FromRow AccountUnsubmittedPrepRow where
  fromRow =
    AccountUnsubmittedPrepRow
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field

getTradingAccountDetailResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Maybe KernelGatewayConfig
  -> Text
  -> IO (Either ApiError (ApiResponse Value))
getTradingAccountDetailResponse pool client cfg mKernel rawSender = do
  nowPosix <- getPOSIXTime
  let nowSec = floor nowPosix :: Integer
      chainId = cfgPerpsChainId cfg
      sender = T.toLower rawSender
  blockRes <- ethBlockNumber client
  let mHeadBlock = either (const Nothing) Just blockRes
      respBlock = fromMaybe Manifest.releaseDeploymentBlock mHeadBlock
  case mKernel of
    Nothing ->
      pure $
        Right $
          mkResponse respBlock chainId $
            object
              [ "network" .= networkJson chainId
              , "observedAt" .= isoTimestamp nowSec
              , "configured" .= False
              , "sender" .= sender
              , "found" .= False
              , "ownerAddress" .= Null
              , "discordUsername" .= Null
              , "firstSeenAt" .= Null
              , "lastActivityAt" .= Null
              , "profile" .= Null
              , "onchain"
                  .= object
                    [ "rpcAvailable" .= False
                    , "deployed" .= False
                    , "codeHash" .= Null
                    , "codeHashMatchesPinned" .= Null
                    , "ethBalanceWei" .= Null
                    , "entryPointNonce" .= Null
                    , "dbHighestNonce" .= Null
                    , "nonceSynced" .= Null
                    ]
              , "sponsorship" .= Null
              , "summary" .= accountSummaryJson emptyAccountKernelSummaryRow
              , "pendingOperations" .= ([] :: [Value])
              , "operations" .= ([] :: [Value])
              , "cancellations" .= ([] :: [Value])
              , "unsubmittedPreparations" .= ([] :: [Value])
              ]
    Just kgc -> do
      let prof = kgcProfile kgc
          scope = kernelScope prof
          entryPointAddr = T.toLower (kpEntryPoint prof)
          mSponsor = kgcSponsorship kgc
          mPaymaster = T.toLower . kscPaymaster <$> mSponsor
      (onchainRes, dbRes) <-
        runConcurrently $
          (,)
            <$> Concurrently (fetchAccountOnchainState client entryPointAddr sender (kpAccountCodeHash prof))
            <*> Concurrently (fetchAccountKernelDb pool scope mPaymaster sender)
      let (rpcOk, deployed, mCodeHash, mCodeMatch, mBalanceWei, mEpNonce) = onchainRes
          (summary, sponsorUsage, pendingOps, ops, cancels, unsubPreps) =
            fromMaybe (emptyAccountKernelSummaryRow, (0, 0, 0), [], [], [], []) dbRes
          (usedHourlyWei, usedOutWei, activeReservedCnt) = sponsorUsage
          found = aksTotalPreps summary > 0 || aksTotalOps summary > 0
          mDbHighestNonce = aksHighestNonce summary
          mNonceSynced = case mEpNonce of
            Nothing -> Nothing
            Just epN ->
              let stalePending = case aksLowestPendingNonce summary >>= readMaybe . T.unpack of
                    Just pendN -> pendN < epN
                    Nothing -> False
                  behindIncluded = case aksHighestIncludedNonce summary >>= readMaybe . T.unpack of
                    Just incN -> epN <= incN
                    Nothing -> False
               in Just (not stalePending && not behindIncluded)
          sponsorshipJson = case mSponsor of
            Nothing -> Null
            Just ksc ->
              let budget = kscBudget ksc
                  hourlyCap = ksbAccountHourlyWei budget
                  outCap = ksbAccountOutstandingWei budget
               in object
                    [ "enabled" .= kscEnabled ksc
                    , "hourlyUsedWei" .= show usedHourlyWei
                    , "hourlyCapWei" .= show hourlyCap
                    , "hourlyUtilizationBps" .= computeUtilizationBps usedHourlyWei hourlyCap
                    , "outstandingUsedWei" .= show usedOutWei
                    , "outstandingCapWei" .= show outCap
                    , "outstandingUtilizationBps" .= computeUtilizationBps usedOutWei outCap
                    , "maxCostPerOpWei" .= show (ksbMaxCostWei budget)
                    , "activeReservedCount" .= activeReservedCnt
                    ]
      pure $
        Right $
          mkResponse respBlock chainId $
            object
              [ "network" .= networkJson chainId
              , "observedAt" .= isoTimestamp nowSec
              , "configured" .= True
              , "sender" .= sender
              , "found" .= found
              , "ownerAddress" .= (T.toLower <$> aksOwner summary)
              , "discordUsername" .= aksDiscordUsername summary
              , "firstSeenAt" .= (formatUtcIso <$> aksFirstSeenAt summary)
              , "lastActivityAt" .= (formatUtcIso <$> aksLastActivityAt summary)
              , "profile"
                  .= object
                    [ "deployment" .= kpDeployment prof
                    , "accountVersion" .= ("kernel-v4-uups" :: Text)
                    , "entryPointVersion" .= ("0.9" :: Text)
                    , "entryPoint" .= entryPointAddr
                    , "accountCodeHash" .= kpAccountCodeHash prof
                    , "paymaster" .= mPaymaster
                    ]
              , "onchain"
                  .= object
                    [ "rpcAvailable" .= rpcOk
                    , "deployed" .= deployed
                    , "codeHash" .= mCodeHash
                    , "codeHashMatchesPinned" .= mCodeMatch
                    , "ethBalanceWei" .= (T.pack . show <$> mBalanceWei)
                    , "entryPointNonce" .= (T.pack . show <$> mEpNonce)
                    , "dbHighestNonce" .= mDbHighestNonce
                    , "nonceSynced" .= mNonceSynced
                    ]
              , "sponsorship" .= sponsorshipJson
              , "summary" .= accountSummaryJson summary
              , "pendingOperations" .= map accountPendingOpToJson pendingOps
              , "operations" .= map accountOpToJson ops
              , "cancellations" .= map accountCancelToJson cancels
              , "unsubmittedPreparations" .= map accountUnsubmittedPrepToJson unsubPreps
              ]

fetchAccountOnchainState
  :: EthClient
  -> Text
  -> Text
  -> Text
  -> IO (Bool, Bool, Maybe Text, Maybe Bool, Maybe Integer, Maybe Integer)
fetchAccountOnchainState client entryPointAddr sender expectedCodeHash = do
  let nonceCall =
        CallParams
          entryPointAddr
          (encodeCall "getNonce(address,uint192)" [encodeAddress sender, encodeUint256 0])
  (codeRes, balRes, nonceRes) <-
    runConcurrently $
      (,,)
        <$> Concurrently (observeIo (ethGetCode client sender))
        <*> Concurrently (observeIo (ethGetBalance client sender))
        <*> Concurrently (observeIo (ethCall client nonceCall))
  let mCode = codeRes >>= either (const Nothing) Just
      mBal = balRes >>= either (const Nothing) Just
      mNonce = nonceRes >>= either (const Nothing) decodeEntryPointBalance
      (deployed, mCodeHash, mCodeMatch) = case mCode of
        Nothing -> (False, Nothing, Nothing)
        Just bs -> classifyAccountBytecode bs expectedCodeHash
      rpcOk = isJust mCode || isJust mBal || isJust mNonce
  pure (rpcOk, deployed, mCodeHash, mCodeMatch, mBal, mNonce)
  where
    isJust (Just _) = True
    isJust Nothing = False

fetchAccountKernelDb
  :: DbPool
  -> KernelScope
  -> Maybe Text
  -> Text
  -> IO
      ( Maybe
          ( AccountKernelSummaryRow
          , (Integer, Integer, Int)
          , [AccountPendingOpRow]
          , [AccountOpRow]
          , [AccountCancelRow]
          , [AccountUnsubmittedPrepRow]
          )
      )
fetchAccountKernelDb pool KernelScope {..} mPaymaster sender =
  observeIo $ withDb pool $ \conn -> do
    sumRows <-
      query
        conn
        "WITH prep_agg AS ( \
        \  SELECT \
        \    MAX(p.owner_address) AS owner_address, \
        \    MIN(p.created_at) AS first_seen_at, \
        \    MAX(p.created_at) AS last_prep_at, \
        \    COUNT(*)::INT AS total_preps, \
        \    COUNT(*) FILTER (WHERE o.user_operation_hash IS NULL)::INT AS unsubmitted_preps, \
        \    MAX(p.nonce)::TEXT AS highest_prep_nonce \
        \  FROM aa_kernel_preparations p \
        \  LEFT JOIN aa_kernel_operations o \
        \    ON o.deployment = p.deployment AND o.chain_id = p.chain_id \
        \    AND o.entry_point = p.entry_point AND o.preparation_id = p.preparation_id \
        \  WHERE p.deployment = ? AND p.chain_id = ? AND p.entry_point = ? AND LOWER(p.sender) = ? \
        \), op_agg AS ( \
        \  SELECT \
        \    MAX(created_at) AS last_op_at, \
        \    COUNT(*)::INT AS total_ops, \
        \    COUNT(*) FILTER (WHERE created_at >= NOW() - interval '24 hours')::INT AS ops_24h, \
        \    COUNT(*) FILTER (WHERE mode = 'sponsored')::INT AS sponsored_count, \
        \    COUNT(*) FILTER (WHERE mode = 'eth')::INT AS eth_count, \
        \    COUNT(*) FILTER (WHERE cancellation_only)::INT AS cancel_ops_count, \
        \    COUNT(*) FILTER (WHERE status = 'included' AND COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE))::INT AS inc_ok, \
        \    COUNT(*) FILTER (WHERE status = 'included' AND NOT COALESCE((terminal_evidence->>'executionSuccess')::BOOLEAN, FALSE))::INT AS inc_rev, \
        \    COUNT(*) FILTER (WHERE status = 'pending')::INT AS pending_count, \
        \    COUNT(*) FILTER (WHERE status = 'nonce_consumed')::INT AS nonce_consumed, \
        \    COUNT(*) FILTER (WHERE status = 'sponsorship_expired')::INT AS sponsor_expired, \
        \    COALESCE(SUM(CASE WHEN (terminal_evidence->>'actualGasCost') ~ '^[0-9]+$' THEN (terminal_evidence->>'actualGasCost')::NUMERIC ELSE 0 END), 0)::TEXT AS tot_gas, \
        \    COALESCE(SUM(CASE WHEN mode = 'sponsored' AND (terminal_evidence->>'actualGasCost') ~ '^[0-9]+$' THEN (terminal_evidence->>'actualGasCost')::NUMERIC ELSE 0 END), 0)::TEXT AS spon_gas, \
        \    COALESCE(SUM(CASE WHEN mode = 'eth' AND (terminal_evidence->>'actualGasCost') ~ '^[0-9]+$' THEN (terminal_evidence->>'actualGasCost')::NUMERIC ELSE 0 END), 0)::TEXT AS eth_gas, \
        \    MAX(GREATEST(0, EXTRACT(EPOCH FROM (NOW() - created_at)))) FILTER (WHERE status = 'pending')::INT AS oldest_pending_sec, \
        \    MAX(nonce)::TEXT AS highest_op_nonce, \
        \    MAX(nonce) FILTER (WHERE status = 'included')::TEXT AS highest_inc_nonce, \
        \    MIN(nonce) FILTER (WHERE status = 'pending')::TEXT AS lowest_pend_nonce \
        \  FROM aa_kernel_operations \
        \  WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND LOWER(sender) = ? \
        \), cancel_agg AS ( \
        \  SELECT \
        \    COUNT(*)::INT AS c_prep, \
        \    COUNT(*) FILTER (WHERE c.cancellation_hash IS NOT NULL)::INT AS c_acc \
        \  FROM aa_kernel_cancellations c \
        \  JOIN aa_kernel_operations o \
        \    ON o.deployment = c.deployment AND o.chain_id = c.chain_id \
        \    AND o.entry_point = c.entry_point AND o.user_operation_hash = c.original_hash \
        \  WHERE c.deployment = ? AND c.chain_id = ? AND c.entry_point = ? AND LOWER(o.sender) = ? \
        \), link_agg AS ( \
        \  SELECT MAX(discord_username) AS discord_username \
        \  FROM insights_divisions_links \
        \  WHERE LOWER(trading_account) = ? \
        \) \
        \SELECT \
        \  p.owner_address, l.discord_username, p.first_seen_at, \
        \  GREATEST(p.last_prep_at, o.last_op_at), \
        \  p.total_preps, p.unsubmitted_preps, \
        \  o.total_ops, o.ops_24h, o.sponsored_count, o.eth_count, o.cancel_ops_count, \
        \  o.inc_ok, o.inc_rev, o.pending_count, o.nonce_consumed, o.sponsor_expired, \
        \  c.c_prep, c.c_acc, o.tot_gas, o.spon_gas, o.eth_gas, o.oldest_pending_sec, \
        \  COALESCE(o.highest_op_nonce, p.highest_prep_nonce), \
        \  o.highest_inc_nonce, o.lowest_pend_nonce \
        \FROM prep_agg p, op_agg o, cancel_agg c, link_agg l"
        ( (ksDeployment, ksChainId, ksEntryPoint, sender)
        :. (ksDeployment, ksChainId, ksEntryPoint, sender)
        :. (ksDeployment, ksChainId, ksEntryPoint, sender)
        :. Only sender
        ) :: IO [AccountKernelSummaryRow]
    let summary = case sumRows of
          (r : _) -> r
          [] -> emptyAccountKernelSummaryRow

    sponsorUsage <- case mPaymaster of
      Nothing -> pure (0, 0, 0)
      Just pm -> do
        spRows <-
          query
            conn
            "SELECT \
            \  COALESCE(SUM(CASE WHEN status IN ('reserved','signed') THEN max_cost_wei WHEN status = 'settled' AND settled_at >= NOW() - interval '1 hour' THEN actual_gas_cost ELSE 0 END), 0)::TEXT, \
            \  COALESCE(SUM(max_cost_wei) FILTER (WHERE status IN ('reserved','signed')), 0)::TEXT, \
            \  COUNT(*) FILTER (WHERE status IN ('reserved','signed'))::INT \
            \FROM aa_kernel_sponsorships \
            \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? AND LOWER(sender) = ?"
            (ksDeployment, ksChainId, ksEntryPoint, pm, sender)
            :: IO [(Text, Text, Int)]
        pure $ case spRows of
          [(hTxt, oTxt, cnt)] -> (parseIntegerOrZero hTxt, parseIntegerOrZero oTxt, cnt)
          _ -> (0, 0, 0)

    pendingRows <-
      query
        conn
        "SELECT \
        \  o.user_operation_hash, \
        \  o.preparation_id, \
        \  o.mode, \
        \  o.cancellation_only, \
        \  o.nonce::TEXT, \
        \  GREATEST(0, EXTRACT(EPOCH FROM (NOW() - o.created_at)))::INT, \
        \  p.prepared_safe_block::TEXT, \
        \  p.nonce_scan_through_block::TEXT, \
        \  s.valid_until, \
        \  s.max_cost_wei::TEXT, \
        \  EXISTS ( \
        \    SELECT 1 FROM aa_kernel_cancellations c \
        \    WHERE c.deployment = o.deployment AND c.chain_id = o.chain_id \
        \      AND c.entry_point = o.entry_point AND c.original_hash = o.user_operation_hash \
        \  ), \
        \  ( \
        \    SELECT c.cancellation_hash FROM aa_kernel_cancellations c \
        \    WHERE c.deployment = o.deployment AND c.chain_id = o.chain_id \
        \      AND c.entry_point = o.entry_point AND c.original_hash = o.user_operation_hash \
        \      AND c.cancellation_hash IS NOT NULL \
        \    LIMIT 1 \
        \  ), \
        \  o.created_at \
        \FROM aa_kernel_operations o \
        \JOIN aa_kernel_preparations p \
        \  USING (deployment, chain_id, entry_point, preparation_id) \
        \LEFT JOIN aa_kernel_sponsorships s \
        \  ON s.deployment = o.deployment AND s.chain_id = o.chain_id \
        \  AND s.entry_point = o.entry_point AND s.preparation_id = o.preparation_id \
        \WHERE o.deployment = ? AND o.chain_id = ? AND o.entry_point = ? AND LOWER(o.sender) = ? \
        \  AND o.status = 'pending' \
        \ORDER BY o.nonce ASC, o.created_at ASC \
        \LIMIT 25"
        (ksDeployment, ksChainId, ksEntryPoint, sender) :: IO [AccountPendingOpRow]

    opRows <-
      query
        conn
        "SELECT \
        \  o.user_operation_hash, \
        \  o.preparation_id, \
        \  o.mode, \
        \  o.cancellation_only, \
        \  o.nonce::TEXT, \
        \  o.status, \
        \  (o.terminal_evidence->>'executionSuccess')::BOOLEAN, \
        \  s.max_cost_wei::TEXT, \
        \  COALESCE(o.terminal_evidence->>'actualGasCost', s.actual_gas_cost::TEXT), \
        \  o.terminal_evidence->>'transactionHash', \
        \  o.terminal_evidence->>'blockNumber', \
        \  o.terminal_evidence->>'cancellationHash', \
        \  o.created_at \
        \FROM aa_kernel_operations o \
        \LEFT JOIN aa_kernel_sponsorships s \
        \  ON s.deployment = o.deployment AND s.chain_id = o.chain_id \
        \  AND s.entry_point = o.entry_point AND s.preparation_id = o.preparation_id \
        \WHERE o.deployment = ? AND o.chain_id = ? AND o.entry_point = ? AND LOWER(o.sender) = ? \
        \ORDER BY o.created_at DESC \
        \LIMIT 80"
        (ksDeployment, ksChainId, ksEntryPoint, sender) :: IO [AccountOpRow]

    cancelRows <-
      query
        conn
        "SELECT \
        \  c.cancellation_preparation_id, \
        \  c.original_hash, \
        \  c.cancellation_hash, \
        \  o.nonce::TEXT, \
        \  c.expires_at, \
        \  c.created_at \
        \FROM aa_kernel_cancellations c \
        \JOIN aa_kernel_operations o \
        \  ON o.deployment = c.deployment AND o.chain_id = c.chain_id \
        \  AND o.entry_point = c.entry_point AND o.user_operation_hash = c.original_hash \
        \WHERE c.deployment = ? AND c.chain_id = ? AND c.entry_point = ? AND LOWER(o.sender) = ? \
        \ORDER BY c.created_at DESC \
        \LIMIT 25"
        (ksDeployment, ksChainId, ksEntryPoint, sender) :: IO [AccountCancelRow]

    unsubRows <-
      query
        conn
        "SELECT \
        \  p.preparation_id, \
        \  p.mode, \
        \  p.nonce::TEXT, \
        \  p.prepared_safe_block::TEXT, \
        \  p.nonce_scan_through_block::TEXT, \
        \  (p.expires_at < NOW()), \
        \  p.expires_at, \
        \  p.created_at \
        \FROM aa_kernel_preparations p \
        \LEFT JOIN aa_kernel_operations o \
        \  ON o.deployment = p.deployment AND o.chain_id = p.chain_id \
        \  AND o.entry_point = p.entry_point AND o.preparation_id = p.preparation_id \
        \WHERE p.deployment = ? AND p.chain_id = ? AND p.entry_point = ? AND LOWER(p.sender) = ? \
        \  AND o.user_operation_hash IS NULL \
        \ORDER BY p.created_at DESC \
        \LIMIT 25"
        (ksDeployment, ksChainId, ksEntryPoint, sender) :: IO [AccountUnsubmittedPrepRow]

    pure (summary, sponsorUsage, pendingRows, opRows, cancelRows, unsubRows)

accountSummaryJson :: AccountKernelSummaryRow -> Value
accountSummaryJson AccountKernelSummaryRow {..} =
  object
    [ "totalPreparations" .= aksTotalPreps
    , "unsubmittedPreparations" .= aksUnsubmittedPreps
    , "totalOperations" .= aksTotalOps
    , "operations24h" .= aksOps24h
    , "sponsoredCount" .= aksSponsoredCount
    , "ethCount" .= aksEthCount
    , "cancellationOpsCount" .= aksCancelOpsCount
    , "includedSucceeded" .= aksIncludedSucceeded
    , "includedReverted" .= aksIncludedReverted
    , "pendingCount" .= aksPendingCount
    , "nonceConsumed" .= aksNonceConsumed
    , "sponsorshipExpired" .= aksSponsorshipExpired
    , "cancellationsPrepared" .= aksCancellationsPrepared
    , "cancellationsAccepted" .= aksCancellationsAccepted
    , "successRateBps" .= computeSuccessRateBps aksIncludedSucceeded (aksIncludedSucceeded + aksIncludedReverted)
    , "totalGasCostWei" .= aksTotalGasWei
    , "sponsoredGasCostWei" .= aksSponsoredGasWei
    , "ethGasCostWei" .= aksEthGasWei
    , "oldestPendingAgeSeconds" .= aksOldestPendingSec
    ]

accountPendingOpToJson :: AccountPendingOpRow -> Value
accountPendingOpToJson AccountPendingOpRow {..} =
  object
    [ "userOpHash" .= T.toLower apoUserOpHash
    , "preparationId" .= apoPrepId
    , "mode" .= apoMode
    , "cancellationOnly" .= apoCancellationOnly
    , "nonce" .= apoNonce
    , "ageSeconds" .= apoAgeSeconds
    , "preparedSafeBlock" .= apoPreparedSafeBlock
    , "nonceScanThroughBlock" .= apoNonceScanThroughBlock
    , "sponsorshipValidUntil" .= (isoTimestamp <$> apoSponsorshipValidUntil)
    , "reservedMaxCostWei" .= apoReservedMaxCostWei
    , "cancellationPrepared" .= apoCancellationPrepared
    , "cancellationHash" .= (T.toLower <$> apoCancellationHash)
    , "createdAt" .= formatUtcIso apoCreatedAt
    ]

accountOpToJson :: AccountOpRow -> Value
accountOpToJson AccountOpRow {..} =
  object
    [ "userOpHash" .= T.toLower aorUserOpHash
    , "preparationId" .= aorPrepId
    , "mode" .= aorMode
    , "cancellationOnly" .= aorCancellationOnly
    , "nonce" .= aorNonce
    , "status" .= aorStatus
    , "executionSuccess" .= aorExecutionSuccess
    , "reservedMaxCostWei" .= aorReservedMaxCostWei
    , "actualGasCostWei" .= aorActualGasCostWei
    , "txHash" .= (T.toLower <$> aorTxHash)
    , "blockNumber" .= aorBlockNumber
    , "cancellationHash" .= (T.toLower <$> aorCancellationHash)
    , "createdAt" .= formatUtcIso aorCreatedAt
    ]

accountCancelToJson :: AccountCancelRow -> Value
accountCancelToJson AccountCancelRow {..} =
  object
    [ "cancellationPreparationId" .= acrPrepId
    , "originalHash" .= T.toLower acrOriginalHash
    , "cancellationHash" .= (T.toLower <$> acrCancellationHash)
    , "nonce" .= acrNonce
    , "accepted" .= case acrCancellationHash of Just _ -> True; Nothing -> False
    , "expiresAt" .= formatUtcIso acrExpiresAt
    , "createdAt" .= formatUtcIso acrCreatedAt
    ]

accountUnsubmittedPrepToJson :: AccountUnsubmittedPrepRow -> Value
accountUnsubmittedPrepToJson AccountUnsubmittedPrepRow {..} =
  object
    [ "preparationId" .= aupPrepId
    , "mode" .= aupMode
    , "nonce" .= aupNonce
    , "preparedSafeBlock" .= aupPreparedSafeBlock
    , "nonceScanThroughBlock" .= aupNonceScanThroughBlock
    , "expired" .= aupExpired
    , "expiresAt" .= formatUtcIso aupExpiresAt
    , "createdAt" .= formatUtcIso aupCreatedAt
    ]
