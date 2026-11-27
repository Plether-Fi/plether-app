module Plether.Handlers.InsightsProtocol
  ( LiveProtocolStatus (..)
  , LiveEngineRiskParams (..)
  , ManifestContractSpec (..)
  , ContractCodeCheck (..)
  , ProtocolTvlSummary (..)
  , getProtocolManifestResponse
  , computeProtocolTvl
  , computeTvlShareBps
  , decodeProtocolStatus
  , decodeEngineRiskParams
  , decodeSingleUint
  , decodeSingleBool
  , classifyParameterDrift
  , extractManifestContracts
  , extractManifestEconomics
  , verifyRuntimeCodeHash
  , phaseLabel
  ) where

import Control.Concurrent.Async (Concurrently (..), mapConcurrently, runConcurrently)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Char (isHexDigit)
import Data.Either (isRight)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Scientific (Scientific, floatingOrInteger)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Database.PostgreSQL.Simple (Only (..), query, query_)
import Plether.AA.KernelGateway (KernelGatewayConfig (..), kernelScope)
import Plether.Cache (SingleFlightCache, newRefreshingSingleFlightCache, runSingleFlightCache)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.KernelOperation (KernelScope (..))
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall, keccak256, selector)
import Plether.Ethereum.Client
  ( EthClient
  , ethBlockNumber
  , rpcCall
  )
import qualified Plether.Ethereum.Multicall as Multicall
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import Plether.Utils.Hex (intToHex)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

data LiveProtocolStatus = LiveProtocolStatus
  { lpsPhase :: Integer
  , lpsLastMarkPrice :: Integer
  , lpsLastMarkTime :: Integer
  , lpsOracleFrozen :: Bool
  , lpsFadWindow :: Bool
  , lpsTradingActive :: Bool
  , lpsWithdrawalLive :: Bool
  , lpsLpEpochSettlementPaused :: Bool
  }
  deriving stock (Eq, Show)

data LiveEngineRiskParams = LiveEngineRiskParams
  { lerVpiFactorWad :: Integer
  , lerMaxSkewRatioWad :: Integer
  , lerMaintenanceMarginBps :: Integer
  , lerInitialMarginBps :: Integer
  , lerFadMarginBps :: Integer
  , lerBaseCarryBps :: Integer
  , lerMinimumLiquidationChargeAssets :: Integer
  , lerLiquidationChargeBps :: Integer
  , lerKeeperShareBps :: Integer
  , lerProtocolShareBps :: Integer
  }
  deriving stock (Eq, Show)

data ManifestContractSpec = ManifestContractSpec
  { mcsKey :: Text
  , mcsAddress :: Text
  , mcsExpectedCodeHash :: Text
  }
  deriving stock (Eq, Show)

data ContractCodeCheck = ContractCodeCheck
  { cccActualCodeHash :: Maybe Text
  , cccVerified :: Maybe Bool
  }
  deriving stock (Eq, Show)

data ProtocolTvlSummary = ProtocolTvlSummary
  { ptsTotalTvlAssets :: Integer
  , ptsProtocolContractsTvlAssets :: Integer
  , ptsLpVaultsTvlAssets :: Integer
  , ptsHousePoolAssets :: Integer
  , ptsSeniorVaultAssets :: Integer
  , ptsJuniorVaultAssets :: Integer
  , ptsMarginClearinghouseAssets :: Integer
  , ptsOtherContractsAssets :: Integer
  , ptsKernelTradingAccountsAssets :: Integer
  , ptsTotalTraderMarginAssets :: Integer
  , ptsContractsTracked :: Int
  , ptsContractsWithBalance :: Int
  , ptsContractsFailedReads :: Int
  , ptsKernelAccountsAvailable :: Bool
  , ptsKernelAccountsTracked :: Int
  , ptsKernelAccountsFunded :: Int
  , ptsKernelAccountsFailedReads :: Int
  , ptsIsComplete :: Bool
  , ptsContractHoldings :: [(ManifestContractSpec, Integer)]
  , ptsTopKernelAccounts :: [(Text, Integer)]
  }
  deriving stock (Eq, Show)

data ParameterSpec = ParameterSpec
  { psKey :: Text
  , psCategory :: Text
  , psLabel :: Text
  , psUnit :: Text
  , psContract :: Text
  , psDescription :: Text
  , psManifestDefault :: Maybe Text
  , psLiveValue :: Maybe Text
  }

{-# NOINLINE protocolManifestCache #-}
protocolManifestCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
protocolManifestCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (30 * 1000000000) (300 * 1000000000))

-- | Runtime bytecode of pinned release contracts is immutable; cache the 30
-- eth_getCode + keccak256 verifications for 15 minutes (60 min SWR) instead of
-- downloading 30 contract bytecodes every 30 seconds.
{-# NOINLINE protocolCodeChecksCache #-}
protocolCodeChecksCache :: SingleFlightCache Integer [ContractCodeCheck]
protocolCodeChecksCache =
  unsafePerformIO (newRefreshingSingleFlightCache 4 (900 * 1000000000) (3600 * 1000000000))

getCachedContractCodeChecks :: EthClient -> Integer -> [ManifestContractSpec] -> IO [ContractCodeCheck]
getCachedContractCodeChecks client blockNum contracts =
  snd
    <$> runSingleFlightCache
      protocolCodeChecksCache
      0
      (all (\c -> maybe False (const True) (cccVerified c)))
      (mapConcurrently (fetchContractCodeCheck client blockNum) contracts)

-- | Batch all 19 protocol status & risk parameter eth_calls into 1 single
-- atomic Multicall3 aggregate3 call at blockNum.
fetchProtocolParamsMulticall
  :: EthClient
  -> Integer
  -> IO (Maybe LiveProtocolStatus, Maybe Bool, Maybe Bool, LiveParameterReads)
fetchProtocolParamsMulticall client blockNum = do
  let engine = Manifest.cfdEngineAddress
      router = Manifest.orderRouterAddress
      housePool = Manifest.housePoolAddress
      oracle = Manifest.pletherOracleAddress
      publicLens = Manifest.perpsPublicLensAddress
      mk target sig =
        Multicall.Call
          { Multicall.callTarget = target
          , Multicall.callAllowFailure = True
          , Multicall.callCalldata = selector sig
          }
      calls =
        [ mk publicLens "getProtocolStatus()"
        , mk housePool "paused()"
        , mk engine "degradedMode()"
        , mk engine "riskParams()"
        , mk engine "executionFeeBps()"
        , mk engine "settlementBufferBps()"
        , mk engine "frozenCloseSpreadBps()"
        , mk router "minOpenNotionalAssets()"
        , mk router "maxPendingOrders()"
        , mk router "maxExecutionWindowSeconds()"
        , mk router "positionProtectionTriggerBountyAssets()"
        , mk housePool "maxSeniorExposureAssets()"
        , mk housePool "maxSeniorShareBps()"
        , mk housePool "LP_EPOCH_DURATION()"
        , mk housePool "TIMELOCK_DELAY()"
        , mk oracle "basketMaxConfidenceRatioBps()"
        , mk oracle "adverseConfidenceMultiplierBps()"
        , mk oracle "orderExecutionStalenessLimit()"
        , mk oracle "liquidationStalenessLimit()"
        ]
      decWith f r = if Multicall.resultSuccess r then f (Multicall.resultData r) else Nothing
      decU = decWith decodeSingleUint
      decB = decWith decodeSingleBool
  mRes <- timeout 4000000 $ observeIo $ Multicall.multicallAtBlock client calls blockNum
  pure $ case mRes of
    Just (Just (Right [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18])) ->
      ( decWith decodeProtocolStatus r0
      , decB r1
      , decB r2
      , LiveParameterReads
          (decWith decodeEngineRiskParams r3, decU r4, decU r5, decU r6)
          (decU r7, decU r8, decU r9, decU r10)
          (decU r11, decU r12, decU r13, decU r14)
          (decU r15, decU r16, decU r17, decU r18)
      )
    _ -> (Nothing, Nothing, Nothing, emptyLiveParams)

getProtocolManifestResponse
  :: Maybe DbPool
  -> EthClient
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
getProtocolManifestResponse mPool client cfg mKernel =
  snd
    <$> runSingleFlightCache
      protocolManifestCache
      ()
      isRight
      (computeProtocolManifestResponse mPool client cfg mKernel)

computeProtocolManifestResponse
  :: Maybe DbPool
  -> EthClient
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
computeProtocolManifestResponse mPool client cfg mKernel = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      root = Manifest.manifestValue
      contracts = extractManifestContracts root
      economics = extractManifestEconomics root
      contractAddressSet = Set.fromList (map (T.toLower . mcsAddress) contracts)
      settlementToken = T.toLower Manifest.settlementTokenAddress

  (latestBlockRes, (kernelDbAvailable, kernelAddresses)) <-
    runConcurrently $
      (,)
        <$> Concurrently (ethBlockNumber client)
        <*> Concurrently (fetchKnownKernelTradingAccounts mPool cfg mKernel contractAddressSet)

  (rpcAvailable, verifiedBlock, liveStatus, housePoolPaused, degradedMode, liveParams, codeChecks, contractBalanceMap, mTvlSummary) <-
    case latestBlockRes of
      Left _ ->
        pure
          ( False
          , Manifest.releaseDeploymentBlock
          , Nothing
          , Nothing
          , Nothing
          , emptyLiveParams
          , map (const (ContractCodeCheck Nothing Nothing)) contracts
          , Map.empty
          , Nothing
          )
      Right blockNum -> do
        ((statusRes, hpPaused, degMode, liveReads), checks, contractBalances, kernelBalances) <-
          runConcurrently $
            (,,,)
              <$> Concurrently (fetchProtocolParamsMulticall client blockNum)
              <*> Concurrently (getCachedContractCodeChecks client blockNum contracts)
              <*> Concurrently (fetchUsdgBalancesAtBlock client settlementToken blockNum (map mcsAddress contracts))
              <*> Concurrently
                ( if kernelDbAvailable
                    then fetchUsdgBalancesAtBlock client settlementToken blockNum kernelAddresses
                    else pure []
                )

        let contractPairs = zip contracts (map snd contractBalances)
            balMap = Map.fromList [(T.toLower addr, mBal) | (addr, mBal) <- contractBalances]
            tvlSummary = computeProtocolTvl contractPairs kernelDbAvailable kernelBalances

        pure
          ( True
          , blockNum
          , statusRes
          , hpPaused
          , degMode
          , liveReads
          , checks
          , balMap
          , Just tvlSummary
          )

  let contractEntries =
        zipWith
          (\spec chk -> contractEntryToJson spec chk (joinMaybe (Map.lookup (T.toLower (mcsAddress spec)) contractBalanceMap)))
          contracts
          codeChecks
      verifiedCount = length [() | ContractCodeCheck _ (Just True) <- codeChecks]
      mismatchCount = length [() | ContractCodeCheck _ (Just False) <- codeChecks]
      uncheckedCount = length [() | ContractCodeCheck _ Nothing <- codeChecks]
      paramSpecs = buildParameterSpecs economics liveParams
      paramEntries = map parameterSpecToJson paramSpecs
      matchedParams = length [() | p <- paramSpecs, classifyParameterDrift (psManifestDefault p) (psLiveValue p) == "match"]
      driftedParams = length [() | p <- paramSpecs, classifyParameterDrift (psManifestDefault p) (psLiveValue p) == "drifted"]
      unavailableParams = length [() | p <- paramSpecs, classifyParameterDrift (psManifestDefault p) (psLiveValue p) == "unavailable"]

  pure $
    Right $
      mkResponse verifiedBlock chainId $
        object
          [ "network"
              .= object
                [ "chainId" .= chainId
                , "releaseVersion" .= Manifest.releaseVersion
                , "settlementAssetSymbol" .= ("USDG" :: Text)
                ]
          , "observedAt" .= isoTimestamp now
          , "rpcAvailable" .= rpcAvailable
          , "verifiedAtBlock" .= show verifiedBlock
          , "release" .= extractReleaseMetadata root
          , "settlementToken" .= extractSettlementMetadata root
          , "safety" .= safetyToJson liveStatus housePoolPaused degradedMode
          , "tvl" .= tvlSummaryToJson verifiedBlock mTvlSummary
          , "verification"
              .= object
                [ "totalContracts" .= length contracts
                , "verifiedCount" .= verifiedCount
                , "mismatchCount" .= mismatchCount
                , "uncheckedCount" .= uncheckedCount
                , "matchedParameters" .= matchedParams
                , "driftedParameters" .= driftedParams
                , "unavailableParameters" .= unavailableParams
                ]
          , "parameters" .= paramEntries
          , "contracts" .= contractEntries
          , "releaseHistory" .= canonicalReleaseHistory Manifest.releaseVersion Manifest.releaseDeploymentBlock
          ]

data LiveParameterReads = LiveParameterReads
  { lprEngine :: (Maybe LiveEngineRiskParams, Maybe Integer, Maybe Integer, Maybe Integer)
  , lprRouter :: (Maybe Integer, Maybe Integer, Maybe Integer, Maybe Integer)
  , lprPool :: (Maybe Integer, Maybe Integer, Maybe Integer, Maybe Integer)
  , lprOracle :: (Maybe Integer, Maybe Integer, Maybe Integer, Maybe Integer)
  }

emptyLiveParams :: LiveParameterReads
emptyLiveParams =
  LiveParameterReads
    (Nothing, Nothing, Nothing, Nothing)
    (Nothing, Nothing, Nothing, Nothing)
    (Nothing, Nothing, Nothing, Nothing)
    (Nothing, Nothing, Nothing, Nothing)

buildParameterSpecs :: Map.Map Text Text -> LiveParameterReads -> [ParameterSpec]
buildParameterSpecs econ LiveParameterReads {..} =
  let (mRisk, mExecFee, mSettleBuf, mFrozenSpread) = lprEngine
      (mMinOpen, mMaxPending, mMaxExecWin, mProtBounty) = lprRouter
      (mMaxSeniorExp, mMaxSeniorShare, mLpEpochDur, mTimelock) = lprPool
      (mBasketConf, mAdverseMult, mOrderStale, mLiqStale) = lprOracle
      mDef k = Map.lookup k econ
      showM = fmap (T.pack . show)
   in [ ParameterSpec
          "initialMarginBps"
          "trader"
          "Initial margin"
          "bps"
          "CfdEngine"
          "Minimum collateral ratio required to open or increase a perpetual position."
          (mDef "initialMarginBps")
          (showM (lerInitialMarginBps <$> mRisk))
      , ParameterSpec
          "maintenanceMarginBps"
          "trader"
          "Maintenance margin"
          "bps"
          "CfdEngine"
          "Equity floor below which an open position becomes eligible for permissionless liquidation."
          (mDef "maintenanceMarginBps")
          (showM (lerMaintenanceMarginBps <$> mRisk))
      , ParameterSpec
          "fadMarginBps"
          "trader"
          "Weekend / FAD margin"
          "bps"
          "CfdEngine"
          "Elevated margin requirement enforced during Forex market closure (FAD) windows."
          (mDef "fadMarginBps")
          (showM (lerFadMarginBps <$> mRisk))
      , ParameterSpec
          "executionFeeBps"
          "trader"
          "Protocol execution fee"
          "bps"
          "CfdEngine"
          "Fee assessed on executed trade notional for opens, increases, and closes."
          (mDef "executionFeeBps")
          (showM mExecFee)
      , ParameterSpec
          "vpiFactorWad"
          "trader"
          "VPI impact factor"
          "wad"
          "CfdEngine"
          "Scaling constant governing Virtual Price Impact charges for skew-increasing trades and rebates for skew-reducing trades."
          (mDef "vpiFactorWad")
          (showM (lerVpiFactorWad <$> mRisk))
      , ParameterSpec
          "maxSkewRatioWad"
          "trader"
          "Max net skew ratio"
          "wad"
          "CfdEngine"
          "Maximum allowed net directional open-interest skew relative to backing HousePool liquidity."
          (mDef "maxSkewRatioWad")
          (showM (lerMaxSkewRatioWad <$> mRisk))
      , ParameterSpec
          "baseCarryBps"
          "trader"
          "Base annualized carry"
          "bps"
          "CfdEngine"
          "Annualized funding / carry rate applied to open directional positions."
          (mDef "baseCarryBps")
          (showM (lerBaseCarryBps <$> mRisk))
      , ParameterSpec
          "frozenCloseSpreadBps"
          "trader"
          "Frozen-oracle close spread"
          "bps"
          "CfdEngine"
          "Protective spread charged when closing positions while the oracle is frozen."
          (mDef "frozenCloseSpreadBps")
          (showM mFrozenSpread)
      , ParameterSpec
          "minimumOpenNotionalAssets"
          "trader"
          "Minimum open notional"
          "assets"
          "OrderRouter"
          "Smallest notional trade size permitted when committing an opening order."
          (mDef "minimumOpenNotionalAssets")
          (showM mMinOpen)
      , ParameterSpec
          "maxPendingOrders"
          "trader"
          "Max pending orders / account"
          "count"
          "OrderRouter"
          "Upper bound on concurrent uncommitted/pending orders per Trading Account."
          (mDef "maxPendingOrders")
          (showM mMaxPending)
      , ParameterSpec
          "liquidationChargeBps"
          "liquidation"
          "Liquidation penalty"
          "bps"
          "CfdEngine"
          "Percentage charge assessed on liquidated notional to incentivize fast liquidations and protect LPs."
          (mDef "liquidationChargeBps")
          (showM (lerLiquidationChargeBps <$> mRisk))
      , ParameterSpec
          "minimumLiquidationChargeAssets"
          "liquidation"
          "Minimum liquidation charge"
          "assets"
          "CfdEngine"
          "Floor on total liquidation penalty so small positions still pay keeper gas."
          (mDef "minimumLiquidationChargeAssets")
          (showM (lerMinimumLiquidationChargeAssets <$> mRisk))
      , ParameterSpec
          "keeperShareBps"
          "liquidation"
          "Keeper bounty share"
          "bps"
          "CfdEngine"
          "Portion of liquidation penalties paid directly to the permissionless executing keeper."
          (mDef "keeperShareBps")
          (showM (lerKeeperShareBps <$> mRisk))
      , ParameterSpec
          "protocolShareBps"
          "liquidation"
          "Protocol liquidation share"
          "bps"
          "CfdEngine"
          "Portion of liquidation penalties routed to protocol treasury (remainder accrues to LPs)."
          (mDef "protocolShareBps")
          (showM (lerProtocolShareBps <$> mRisk))
      , ParameterSpec
          "positionProtectionTriggerBountyAssets"
          "liquidation"
          "TP/SL trigger bounty"
          "assets"
          "OrderRouter"
          "Fixed USDG bounty paid to permissionless keepers triggering armed take-profit or stop-loss protections."
          (mDef "positionProtectionTriggerBountyAssets")
          (showM mProtBounty)
      , ParameterSpec
          "maxSeniorExposureAssets"
          "lp"
          "Max Senior tranche cap"
          "assets"
          "HousePool"
          "Hard absolute ceiling on Senior vault USDG deposits accepted by HousePool."
          (mDef "maxSeniorExposureAssets")
          (showM mMaxSeniorExp)
      , ParameterSpec
          "maxSeniorShareBps"
          "lp"
          "Max Senior tranche share"
          "bps"
          "HousePool"
          "Maximum share of total HousePool liquidity that Senior LPs may represent before Junior cushion limits new Senior deposits."
          (mDef "maxSeniorShareBps")
          (showM mMaxSeniorShare)
      , ParameterSpec
          "settlementBufferBps"
          "lp"
          "Settlement safety buffer"
          "bps"
          "CfdEngine"
          "Liquidity reserve held back during LP epoch settlement to absorb active trader obligations."
          (mDef "settlementBufferBps")
          (showM mSettleBuf)
      , ParameterSpec
          "lpEpochDurationSeconds"
          "lp"
          "LP settlement epoch duration"
          "seconds"
          "HousePool"
          "Cadence at which queued LP deposit and withdrawal requests mature for permissionless epoch settlement."
          Nothing
          (showM mLpEpochDur)
      , ParameterSpec
          "timelockDelaySeconds"
          "lp"
          "Pool config timelock"
          "seconds"
          "HousePool"
          "Minimum delay between proposing and activating HousePool parameter updates."
          Nothing
          (showM mTimelock)
      , ParameterSpec
          "basketMaxConfidenceRatioBps"
          "oracle"
          "Max Pyth confidence ratio"
          "bps"
          "PletherOracle"
          "Tightest acceptable Pyth confidence-to-price ratio across the 6 DXY basket components."
          (mDef "basketMaxConfidenceRatioBps")
          (showM mBasketConf)
      , ParameterSpec
          "adverseConfidenceMultiplierBps"
          "oracle"
          "Adverse confidence multiplier"
          "bps"
          "PletherOracle"
          "Confidence band multiplier applied against trade direction to prevent oracle latency arbitrage."
          (mDef "adverseConfidenceMultiplierBps")
          (showM mAdverseMult)
      , ParameterSpec
          "orderExecutionStalenessLimitSeconds"
          "oracle"
          "Order oracle staleness limit"
          "seconds"
          "PletherOracle"
          "Maximum age of a Pyth basket update accepted when executing trader orders."
          Nothing
          (showM mOrderStale)
      , ParameterSpec
          "liquidationStalenessLimitSeconds"
          "oracle"
          "Liquidation staleness limit"
          "seconds"
          "PletherOracle"
          "Maximum age of a Pyth basket update accepted for liquidation execution."
          Nothing
          (showM mLiqStale)
      , ParameterSpec
          "maxExecutionWindowSeconds"
          "oracle"
          "Max order execution window"
          "seconds"
          "OrderRouter"
          "Upper bound on how long a committed order remains executable before expiring."
          Nothing
          (showM mMaxExecWin)
      ]

parameterSpecToJson :: ParameterSpec -> Value
parameterSpecToJson ParameterSpec {..} =
  object
    [ "key" .= psKey
    , "category" .= psCategory
    , "label" .= psLabel
    , "unit" .= psUnit
    , "contract" .= psContract
    , "description" .= psDescription
    , "manifestDefault" .= psManifestDefault
    , "liveValue" .= psLiveValue
    , "status" .= classifyParameterDrift psManifestDefault psLiveValue
    ]

classifyParameterDrift :: Maybe Text -> Maybe Text -> Text
classifyParameterDrift mManifest mLive =
  case (mManifest, mLive) of
    (_, Nothing) -> "unavailable"
    (Nothing, Just _) -> "live_only"
    (Just defVal, Just liveVal)
      | T.strip defVal == T.strip liveVal -> "match"
      | otherwise -> "drifted"

decodeProtocolStatus :: ByteString -> Maybe LiveProtocolStatus
decodeProtocolStatus bytes
  | BS.length bytes /= 256 = Nothing
  | any (`notElem` [0, 1]) [w 3, w 4, w 5, w 6, w 7] = Nothing
  | otherwise =
      Just
        LiveProtocolStatus
          { lpsPhase = w 0
          , lpsLastMarkPrice = w 1
          , lpsLastMarkTime = w 2
          , lpsOracleFrozen = w 3 == 1
          , lpsFadWindow = w 4 == 1
          , lpsTradingActive = w 5 == 1
          , lpsWithdrawalLive = w 6 == 1
          , lpsLpEpochSettlementPaused = w 7 == 1
          }
  where
    w n = decodeUint256 $ BS.take 32 $ BS.drop (n * 32) bytes

decodeEngineRiskParams :: ByteString -> Maybe LiveEngineRiskParams
decodeEngineRiskParams bytes
  | BS.length bytes /= 320 = Nothing
  | otherwise =
      Just
        LiveEngineRiskParams
          { lerVpiFactorWad = w 0
          , lerMaxSkewRatioWad = w 1
          , lerMaintenanceMarginBps = w 2
          , lerInitialMarginBps = w 3
          , lerFadMarginBps = w 4
          , lerBaseCarryBps = w 5
          , lerMinimumLiquidationChargeAssets = w 6
          , lerLiquidationChargeBps = w 7
          , lerKeeperShareBps = w 8
          , lerProtocolShareBps = w 9
          }
  where
    w n = decodeUint256 $ BS.take 32 $ BS.drop (n * 32) bytes

decodeSingleUint :: ByteString -> Maybe Integer
decodeSingleUint bytes
  | BS.length bytes /= 32 = Nothing
  | otherwise = Just (decodeUint256 bytes)

decodeSingleBool :: ByteString -> Maybe Bool
decodeSingleBool bytes
  | BS.length bytes /= 32 = Nothing
  | otherwise = case decodeUint256 bytes of
      0 -> Just False
      1 -> Just True
      _ -> Nothing

phaseLabel :: Integer -> Text
phaseLabel 0 = "bootstrap"
phaseLabel 1 = "active"
phaseLabel 2 = "settlement"
phaseLabel 3 = "paused"
phaseLabel _ = "unknown"

safetyToJson :: Maybe LiveProtocolStatus -> Maybe Bool -> Maybe Bool -> Value
safetyToJson mStatus mHousePoolPaused mDegraded =
  object
    [ "available" .= maybe False (const True) mStatus
    , "phase" .= (phaseLabel . lpsPhase <$> mStatus)
    , "lastMarkPrice" .= (show . lpsLastMarkPrice <$> mStatus)
    , "lastMarkTime" .= (isoTimestamp . lpsLastMarkTime <$> mStatus)
    , "tradingActive" .= (lpsTradingActive <$> mStatus)
    , "withdrawalLive" .= (lpsWithdrawalLive <$> mStatus)
    , "lpEpochSettlementPaused" .= (lpsLpEpochSettlementPaused <$> mStatus)
    , "oracleFrozen" .= (lpsOracleFrozen <$> mStatus)
    , "fadWindow" .= (lpsFadWindow <$> mStatus)
    , "housePoolPaused" .= mHousePoolPaused
    , "degradedMode" .= mDegraded
    ]

fetchContractCodeCheck :: EthClient -> Integer -> ManifestContractSpec -> IO ContractCodeCheck
fetchContractCodeCheck client blockNum ManifestContractSpec {..} = do
  res <- rpcCall client "eth_getCode" $ toJSON [String mcsAddress, String $ "0x" <> intToHex blockNum]
  pure $ case res of
    Left _ -> ContractCodeCheck Nothing Nothing
    Right val -> verifyRuntimeCodeHash mcsExpectedCodeHash val

verifyRuntimeCodeHash :: Text -> Value -> ContractCodeCheck
verifyRuntimeCodeHash expectedHash (String encoded)
  | not ("0x" `T.isPrefixOf` encoded) || odd (T.length encoded - 2) =
      ContractCodeCheck Nothing Nothing
  | otherwise =
      case B16.decode $ TE.encodeUtf8 $ T.toLower $ T.drop 2 encoded of
        Left _ -> ContractCodeCheck Nothing Nothing
        Right runtimeCode
          | BS.null runtimeCode -> ContractCodeCheck (Just "0x") (Just False)
          | otherwise ->
              let actual = "0x" <> TE.decodeUtf8 (B16.encode $ keccak256 runtimeCode)
               in ContractCodeCheck (Just actual) (Just (T.toLower actual == T.toLower expectedHash))
verifyRuntimeCodeHash _ _ = ContractCodeCheck Nothing Nothing

extractManifestContracts :: Value -> [ManifestContractSpec]
extractManifestContracts root =
  case lookupObject "contracts" root of
    Nothing -> []
    Just entries ->
      sortOn (contractSortRank . mcsKey)
        [ ManifestContractSpec
            { mcsKey = Key.toText k
            , mcsAddress = T.toLower addr
            , mcsExpectedCodeHash = T.toLower codeHash
            }
        | (k, Object obj) <- KM.toList entries
        , Just addr <- [lookupText "address" obj]
        , Just codeHash <- [lookupText "runtimeCodeHash" obj]
        ]

extractManifestEconomics :: Value -> Map.Map Text Text
extractManifestEconomics root =
  case lookupObject "economics" root of
    Nothing -> Map.empty
    Just entries ->
      Map.fromList
        [ (Key.toText k, scalar)
        | (k, val) <- KM.toList entries
        , Just scalar <- [jsonScalarToText val]
        ]

extractReleaseMetadata :: Value -> Value
extractReleaseMetadata root =
  let rel = fromMaybe KM.empty $ lookupObject "release" root
      schemaVer = lookupInt "schemaVersion" root
      orderIfVer = lookupInt "orderInterfaceVersion" root
   in object
        [ "version" .= lookupText "version" rel
        , "date" .= lookupText "date" rel
        , "phase" .= lookupText "phase" rel
        , "deploymentBlock" .= (show <$> lookupIntFromObj "deploymentBlock" rel)
        , "sourceCommit" .= lookupText "sourceCommit" rel
        , "forgeVersion" .= lookupText "forgeVersion" rel
        , "manifestSha256" .= lookupText "manifestSha256" rel
        , "bundleSha256" .= lookupText "bundleSha256" rel
        , "pythAddress" .= lookupText "pyth" rel
        , "schemaVersion" .= schemaVer
        , "orderInterfaceVersion" .= orderIfVer
        , "aaManifestVersion" .= Manifest.releaseAaManifestVersion
        ]

extractSettlementMetadata :: Value -> Value
extractSettlementMetadata root =
  let tok = fromMaybe KM.empty $ lookupObject "settlementToken" root
   in object
        [ "name" .= lookupText "name" tok
        , "symbol" .= lookupText "symbol" tok
        , "decimals" .= lookupIntFromObj "decimals" tok
        , "isMock" .= lookupBoolFromObj "isMock" tok
        , "address" .= T.toLower Manifest.settlementTokenAddress
        ]

contractEntryToJson :: ManifestContractSpec -> ContractCodeCheck -> Maybe Integer -> Value
contractEntryToJson ManifestContractSpec {..} ContractCodeCheck {..} mUsdgBalance =
  let (displayName, category, role) = contractMetadata mcsKey
   in object
        [ "key" .= mcsKey
        , "name" .= displayName
        , "category" .= category
        , "role" .= role
        , "address" .= mcsAddress
        , "expectedCodeHash" .= mcsExpectedCodeHash
        , "actualCodeHash" .= cccActualCodeHash
        , "verified" .= cccVerified
        , "usdgBalanceAssets" .= (show <$> mUsdgBalance)
        ]

computeTvlShareBps :: Integer -> Integer -> Maybe Int
computeTvlShareBps part total
  | total <= 0 || part < 0 = Nothing
  | otherwise = Just $ fromInteger ((part * 10000 + total `div` 2) `div` total)

computeProtocolTvl
  :: [(ManifestContractSpec, Maybe Integer)]
  -> Bool
  -> [(Text, Maybe Integer)]
  -> ProtocolTvlSummary
computeProtocolTvl rawContractPairs kernelAvailable rawKernelPairs =
  let dedupContracts = dedupByKey (T.toLower . mcsAddress . fst) rawContractPairs
      contractAddrSet = Set.fromList [T.toLower (mcsAddress spec) | (spec, _) <- dedupContracts]
      contractFailed = length [() | (_, Nothing) <- dedupContracts]
      contractValFor k =
        sum [bal | (spec, Just bal) <- dedupContracts, mcsKey spec == k, bal > 0]
      housePoolBal = contractValFor "housePool"
      seniorVaultBal = contractValFor "seniorVault"
      juniorVaultBal = contractValFor "juniorVault"
      lpVaultsBal = housePoolBal + seniorVaultBal + juniorVaultBal
      clearinghouseBal = contractValFor "marginClearinghouse"
      primaryKeys = Set.fromList ["housePool", "seniorVault", "juniorVault", "marginClearinghouse"]
      otherContractsBal =
        sum
          [ bal
          | (spec, Just bal) <- dedupContracts
          , not (Set.member (mcsKey spec) primaryKeys)
          , bal > 0
          ]
      protocolContractsTvl = lpVaultsBal + clearinghouseBal + otherContractsBal
      contractHoldings =
        sortOn
          (Down . snd)
          [(spec, bal) | (spec, Just bal) <- dedupContracts, bal > 0]
      dedupKernel =
        filter
          (\(addr, _) -> not (Set.member (T.toLower addr) contractAddrSet))
          (dedupByKey (T.toLower . fst) rawKernelPairs)
      kernelFailed = length [() | (_, Nothing) <- dedupKernel]
      fundedKernel =
        sortOn
          (Down . snd)
          [(T.toLower addr, bal) | (addr, Just bal) <- dedupKernel, bal > 0]
      kernelTotalBal = sum (map snd fundedKernel)
      totalTvl = protocolContractsTvl + kernelTotalBal
      totalTraderMargin = clearinghouseBal + kernelTotalBal
      isComplete = kernelAvailable && contractFailed == 0 && kernelFailed == 0
   in ProtocolTvlSummary
        { ptsTotalTvlAssets = totalTvl
        , ptsProtocolContractsTvlAssets = protocolContractsTvl
        , ptsLpVaultsTvlAssets = lpVaultsBal
        , ptsHousePoolAssets = housePoolBal
        , ptsSeniorVaultAssets = seniorVaultBal
        , ptsJuniorVaultAssets = juniorVaultBal
        , ptsMarginClearinghouseAssets = clearinghouseBal
        , ptsOtherContractsAssets = otherContractsBal
        , ptsKernelTradingAccountsAssets = kernelTotalBal
        , ptsTotalTraderMarginAssets = totalTraderMargin
        , ptsContractsTracked = length dedupContracts
        , ptsContractsWithBalance = length contractHoldings
        , ptsContractsFailedReads = contractFailed
        , ptsKernelAccountsAvailable = kernelAvailable
        , ptsKernelAccountsTracked = length dedupKernel
        , ptsKernelAccountsFunded = length fundedKernel
        , ptsKernelAccountsFailedReads = kernelFailed
        , ptsIsComplete = isComplete
        , ptsContractHoldings = contractHoldings
        , ptsTopKernelAccounts = take 10 fundedKernel
        }

tvlSummaryToJson :: Integer -> Maybe ProtocolTvlSummary -> Value
tvlSummaryToJson blockNum Nothing =
  object
    [ "available" .= False
    , "isComplete" .= False
    , "verifiedAtBlock" .= show blockNum
    , "settlementAssetSymbol" .= ("USDG" :: Text)
    ]
tvlSummaryToJson blockNum (Just ProtocolTvlSummary {..}) =
  let lpCount =
        length
          [ ()
          | (spec, _) <- ptsContractHoldings
          , mcsKey spec `elem` ["housePool", "seniorVault", "juniorVault"]
          ]
      chCount =
        length [() | (spec, _) <- ptsContractHoldings, mcsKey spec == "marginClearinghouse"]
      otherCount =
        length
          [ ()
          | (spec, _) <- ptsContractHoldings
          , mcsKey spec `notElem` ["housePool", "seniorVault", "juniorVault", "marginClearinghouse"]
          ]
      buckets =
        [ object
            [ "key" .= ("lp_vaults" :: Text)
            , "label" .= ("LP Liquidity Pools" :: Text)
            , "detail" .= ("HousePool, Senior Vault & Junior Vault" :: Text)
            , "assets" .= show ptsLpVaultsTvlAssets
            , "shareBps" .= computeTvlShareBps ptsLpVaultsTvlAssets ptsTotalTvlAssets
            , "addressCount" .= lpCount
            ]
        , object
            [ "key" .= ("margin_clearinghouse" :: Text)
            , "label" .= ("Margin Clearinghouse" :: Text)
            , "detail" .= ("Deposited trader collateral, locked position margin & claims" :: Text)
            , "assets" .= show ptsMarginClearinghouseAssets
            , "shareBps" .= computeTvlShareBps ptsMarginClearinghouseAssets ptsTotalTvlAssets
            , "addressCount" .= chCount
            ]
        , object
            [ "key" .= ("kernel_v4_accounts" :: Text)
            , "label" .= ("Kernel v4 Trading Accounts" :: Text)
            , "detail" .= ("User margin held directly in ZeroDev Kernel v4 smart wallets" :: Text)
            , "assets" .= show ptsKernelTradingAccountsAssets
            , "shareBps" .= computeTvlShareBps ptsKernelTradingAccountsAssets ptsTotalTvlAssets
            , "addressCount" .= ptsKernelAccountsFunded
            ]
        , object
            [ "key" .= ("other_contracts" :: Text)
            , "label" .= ("Other Protocol Contracts" :: Text)
            , "detail" .= ("Remaining pinned protocol release contracts" :: Text)
            , "assets" .= show ptsOtherContractsAssets
            , "shareBps" .= computeTvlShareBps ptsOtherContractsAssets ptsTotalTvlAssets
            , "addressCount" .= otherCount
            ]
        ]
      contractHoldingsJson =
        [ let (displayName, category, _) = contractMetadata (mcsKey spec)
           in object
                [ "key" .= mcsKey spec
                , "name" .= displayName
                , "category" .= category
                , "address" .= mcsAddress spec
                , "assets" .= show bal
                , "shareBps" .= computeTvlShareBps bal ptsTotalTvlAssets
                ]
        | (spec, bal) <- ptsContractHoldings
        ]
      topKernelJson =
        [ object
            [ "address" .= addr
            , "assets" .= show bal
            , "shareBps" .= computeTvlShareBps bal ptsTotalTvlAssets
            ]
        | (addr, bal) <- ptsTopKernelAccounts
        ]
   in object
        [ "available" .= True
        , "isComplete" .= ptsIsComplete
        , "verifiedAtBlock" .= show blockNum
        , "settlementAssetSymbol" .= ("USDG" :: Text)
        , "totalTvlAssets" .= show ptsTotalTvlAssets
        , "protocolContractsTvlAssets" .= show ptsProtocolContractsTvlAssets
        , "lpVaultsTvlAssets" .= show ptsLpVaultsTvlAssets
        , "housePoolAssets" .= show ptsHousePoolAssets
        , "seniorVaultAssets" .= show ptsSeniorVaultAssets
        , "juniorVaultAssets" .= show ptsJuniorVaultAssets
        , "marginClearinghouseAssets" .= show ptsMarginClearinghouseAssets
        , "otherContractsAssets" .= show ptsOtherContractsAssets
        , "kernelTradingAccountsAssets" .= show ptsKernelTradingAccountsAssets
        , "totalTraderMarginAssets" .= show ptsTotalTraderMarginAssets
        , "contractsTracked" .= ptsContractsTracked
        , "contractsWithBalance" .= ptsContractsWithBalance
        , "contractsFailedReads" .= ptsContractsFailedReads
        , "kernelAccountsAvailable" .= ptsKernelAccountsAvailable
        , "kernelAccountsTracked" .= ptsKernelAccountsTracked
        , "kernelAccountsFunded" .= ptsKernelAccountsFunded
        , "kernelAccountsFailedReads" .= ptsKernelAccountsFailedReads
        , "buckets" .= buckets
        , "contractHoldings" .= contractHoldingsJson
        , "topKernelAccounts" .= topKernelJson
        ]

fetchUsdgBalancesAtBlock :: EthClient -> Text -> Integer -> [Text] -> IO [(Text, Maybe Integer)]
fetchUsdgBalancesAtBlock _ _ _ [] = pure []
fetchUsdgBalancesAtBlock client tokenAddr blockNum addrs = do
  let batches = chunksOf 100 addrs
  batchResults <- mapConcurrently fetchBatch batches
  pure (concat batchResults)
  where
    fetchBatch :: [Text] -> IO [(Text, Maybe Integer)]
    fetchBatch batch = do
      let calls =
            [ Multicall.Call
                { Multicall.callTarget = tokenAddr
                , Multicall.callAllowFailure = True
                , Multicall.callCalldata = encodeCall "balanceOf(address)" [encodeAddress addr]
                }
            | addr <- batch
            ]
      mRes <- timeout 4000000 $ observeIo $ Multicall.multicallAtBlock client calls blockNum
      pure $ case mRes of
        Just (Just (Right results)) | length results == length batch ->
          zipWith decodeOne batch results
        _ ->
          map (\addr -> (addr, Nothing)) batch

    decodeOne :: Text -> Multicall.CallResult -> (Text, Maybe Integer)
    decodeOne addr res
      | Multicall.resultSuccess res && BS.length (Multicall.resultData res) == 32 =
          (addr, Just (decodeUint256 (Multicall.resultData res)))
      | otherwise = (addr, Nothing)

fetchKnownKernelTradingAccounts
  :: Maybe DbPool
  -> Config
  -> Maybe KernelGatewayConfig
  -> Set.Set Text
  -> IO (Bool, [Text])
fetchKnownKernelTradingAccounts Nothing _ _ _ = pure (False, [])
fetchKnownKernelTradingAccounts (Just pool) cfg mKernel excludeContracts = do
  let chainId = cfgPerpsChainId cfg
  mRows <-
    timeout 3000000 $
      observeIo $
        withDb pool $ \conn -> do
          prepRows <-
            case mKernel of
              Just kgc -> do
                let KernelScope {..} = kernelScope (kgcProfile kgc)
                fromMaybe []
                  <$> observeIo
                    ( query
                        conn
                        "SELECT DISTINCT sender FROM aa_kernel_preparations \
                        \WHERE deployment = ? AND chain_id = ? AND entry_point = ?"
                        (ksDeployment, ksChainId, ksEntryPoint)
                        :: IO [Only Text]
                    )
              Nothing ->
                fromMaybe []
                  <$> observeIo
                    ( query
                        conn
                        "SELECT DISTINCT sender FROM aa_kernel_preparations WHERE chain_id = ?"
                        (Only chainId)
                        :: IO [Only Text]
                    )
          opRows <-
            case mKernel of
              Just kgc -> do
                let KernelScope {..} = kernelScope (kgcProfile kgc)
                fromMaybe []
                  <$> observeIo
                    ( query
                        conn
                        "SELECT DISTINCT sender FROM aa_kernel_operations \
                        \WHERE deployment = ? AND chain_id = ? AND entry_point = ?"
                        (ksDeployment, ksChainId, ksEntryPoint)
                        :: IO [Only Text]
                    )
              Nothing ->
                fromMaybe []
                  <$> observeIo
                    ( query
                        conn
                        "SELECT DISTINCT sender FROM aa_kernel_operations WHERE chain_id = ?"
                        (Only chainId)
                        :: IO [Only Text]
                    )
          divRows <-
            fromMaybe []
              <$> observeIo
                ( query_
                    conn
                    "SELECT DISTINCT LOWER(trading_account) FROM insights_divisions_links \
                    \WHERE trading_account IS NOT NULL"
                    :: IO [Only Text]
                )
          pure (prepRows ++ opRows ++ divRows)
  case mRows of
    Just (Just rows) ->
      let validAddrs =
            Set.toList $
              Set.fromList
                [ norm
                | Only raw <- rows
                , let norm = T.toLower (T.strip raw)
                , isHexAddress40 norm
                , not (Set.member norm excludeContracts)
                ]
       in pure (True, validAddrs)
    _ -> pure (False, [])

isHexAddress40 :: Text -> Bool
isHexAddress40 addr =
  T.length addr == 42
    && "0x" `T.isPrefixOf` addr
    && T.all isHexDigit (T.drop 2 addr)

dedupByKey :: (Ord k) => (a -> k) -> [a] -> [a]
dedupByKey keyFn = go Set.empty
  where
    go _ [] = []
    go seen (x : xs) =
      let k = keyFn x
       in if Set.member k seen
            then go seen xs
            else x : go (Set.insert k seen) xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (h, t) = splitAt (max 1 n) xs
   in h : chunksOf n t

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe (Just (Just a)) = Just a
joinMaybe _ = Nothing

observeIo :: IO a -> IO (Maybe a)
observeIo action = do
  res <- try action
  case res of
    Right val -> pure (Just val)
    Left (err :: SomeException) ->
      case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> pure Nothing

contractMetadata :: Text -> (Text, Text, Text)
contractMetadata key = case key of
  "cfdEngine" -> ("CfdEngine", "core", "Core perpetuals accounting, margin, VPI, carry, and PnL settlement engine.")
  "marginClearinghouse" -> ("MarginClearinghouse", "core", "Custodies trader USDG collateral, locked position margins, and settlement claims.")
  "settlementToken" -> ("Settlement Token (USDG)", "core", "Canonical 6-decimal settlement asset across trading accounts and LP vaults.")
  "terminalNavBookV2" -> ("TerminalNavBookV2", "core", "Tracks terminal NAV snapshots and settlement obligations.")
  "cfdEnginePlanner" -> ("CfdEnginePlanner", "core", "Computes bounded execution and risk state transitions for CfdEngine.")
  "cfdEnginePlanningReader" -> ("CfdEnginePlanningReader", "core", "Read-only planning view helper bound to CfdEngine.")
  "cfdEngineSettlementSidecar" -> ("CfdEngineSettlementSidecar", "core", "Executes post-trade fee, rebate, and settlement ledger transfers.")
  "cfdEngineOpenQuoter" -> ("CfdEngineOpenQuoter", "core", "Quotes pre-trade open/increase margin, VPI, and leverage impacts.")
  "cfdClosePreview" -> ("CfdClosePreview", "core", "Previews exact close settlement, VPI, carry, and net account change.")
  "orderRouter" -> ("OrderRouter", "orders", "Public entry point for committing, executing, and liquidating bounded V3 orders.")
  "orderLifecycleBook" -> ("OrderLifecycleBook", "orders", "Stores signed V3 order intents, terminal outcomes, and execution config hash.")
  "cfdOrderPolicyEvaluator" -> ("CfdOrderPolicyEvaluator", "orders", "Enforces bounded execution constraints (price, leverage, fees, and min equity).")
  "positionProtectionBook" -> ("PositionProtectionBook", "orders", "Manages armed on-chain take-profit and stop-loss protection triggers.")
  "orderRouterExecutionSidecar" -> ("OrderRouterExecutionSidecar", "orders", "Delegate execution helper for OrderRouter fills.")
  "orderRecoverySidecar" -> ("OrderRecoverySidecar", "orders", "Handles expired order recovery and stuck-order pruning.")
  "orderRouterLiquidationBatchSidecar" -> ("OrderRouterLiquidationBatchSidecar", "orders", "Permissionless batch liquidation executor.")
  "housePool" -> ("HousePool", "liquidity", "Counterparty liquidity pool managing Senior/Junior tranche accounting and hourly LP epochs.")
  "seniorVault" -> ("Senior Vault", "liquidity", "ERC-7540 async Senior LP tranche vault with protected priority coupon.")
  "juniorVault" -> ("Junior Vault", "liquidity", "ERC-7540 async Junior LP tranche vault absorbing trader PnL and fee revenue.")
  "housePoolRedemptionMathSidecar" -> ("HousePoolRedemptionMathSidecar", "liquidity", "Computes exact Senior/Junior epoch settlement shares and redemption math.")
  "pletherOracle" -> ("PletherOracle", "oracle", "Verifies 6-component Pyth DXY basket updates, staleness bounds, and FAD windows.")
  "perpsPublicLens" -> ("PerpsPublicLens", "oracle", "Aggregated public read-only lens for protocol status and tranche metrics.")
  "cfdEngineLens" -> ("CfdEngineLens", "oracle", "Read-only lens exposing engine positions and market state.")
  "cfdEngineAccountLens" -> ("CfdEngineAccountLens", "oracle", "Read-only lens for Trading Account collateral buckets, equity, and position health.")
  "cfdEngineProtocolLens" -> ("CfdEngineProtocolLens", "oracle", "Read-only protocol parameter and accounting lens.")
  "settlementMonitorLens" -> ("SettlementMonitorLens", "oracle", "Evaluates LP epoch settlement readiness, blocker masks, and warning masks.")
  "settlementMonitorLensSidecar" -> ("SettlementMonitorLensSidecar", "oracle", "Extended diagnostic reader for SettlementMonitorLens.")
  "emergencyPauseCoordinator" -> ("EmergencyPauseCoordinator", "safety", "Coordinates emergency pause actions across trading and LP settlement surfaces.")
  "cfdEngineAdmin" -> ("CfdEngineAdmin", "safety", "Timelocked governance administration module for CfdEngine parameters.")
  "orderRouterAdmin" -> ("OrderRouterAdmin", "safety", "Governance administration module for OrderRouter limits and bounties.")
  _ -> (key, "core", "Pinned protocol release contract.")

contractSortRank :: Text -> (Int, Text)
contractSortRank key = case key of
  "cfdEngine" -> (1, key)
  "marginClearinghouse" -> (2, key)
  "orderRouter" -> (3, key)
  "orderLifecycleBook" -> (4, key)
  "housePool" -> (5, key)
  "seniorVault" -> (6, key)
  "juniorVault" -> (7, key)
  "pletherOracle" -> (8, key)
  "positionProtectionBook" -> (9, key)
  "emergencyPauseCoordinator" -> (10, key)
  "settlementToken" -> (11, key)
  _ -> (50, key)

canonicalReleaseHistory :: Text -> Integer -> [Value]
canonicalReleaseHistory activeVersion activeBlock =
  [ object
      [ "version" .= ("v1.2.3" :: Text)
      , "status" .= (if activeVersion == "v1.2.3" then "active" else "superseded" :: Text)
      , "date" .= ("2026-09-01" :: Text)
      , "summary" .= ("Initial Sepolia Core perpetuals release with deterministic trade receipts, VPI pricing, and September competition accounting." :: Text)
      ]
  , object
      [ "version" .= ("v1.3.1" :: Text)
      , "status" .= (if activeVersion == "v1.3.1" then "active" else "superseded" :: Text)
      , "date" .= ("2026-10-05" :: Text)
      , "summary" .= ("Hardened LP epoch settlement math, Senior/Junior async vaults, and expanded execution policy controls." :: Text)
      ]
  , object
      [ "version" .= activeVersion
      , "status" .= ("active" :: Text)
      , "date" .= ("2026-10-28" :: Text)
      , "deploymentBlock" .= show activeBlock
      , "summary" .= ("Schema-4 USDG settlement migration, bounded V3 signed order intents, PositionProtectionBook TP/SL triggers, and 30-contract runtime code-hash pinning." :: Text)
      ]
  ]

lookupObject :: Text -> Value -> Maybe (KM.KeyMap Value)
lookupObject key (Object obj) =
  case KM.lookup (Key.fromText key) obj of
    Just (Object sub) -> Just sub
    _ -> Nothing
lookupObject _ _ = Nothing

lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText key obj =
  case KM.lookup (Key.fromText key) obj of
    Just (String t) -> Just t
    _ -> Nothing

lookupBoolFromObj :: Text -> KM.KeyMap Value -> Maybe Bool
lookupBoolFromObj key obj =
  case KM.lookup (Key.fromText key) obj of
    Just (Bool b) -> Just b
    _ -> Nothing

lookupInt :: Text -> Value -> Maybe Integer
lookupInt key (Object obj) = lookupIntFromObj key obj
lookupInt _ _ = Nothing

lookupIntFromObj :: Text -> KM.KeyMap Value -> Maybe Integer
lookupIntFromObj key obj =
  case KM.lookup (Key.fromText key) obj of
    Just (Number n) -> scientificToInteger n
    _ -> Nothing

jsonScalarToText :: Value -> Maybe Text
jsonScalarToText (String t) = Just t
jsonScalarToText (Number n) =
  case scientificToInteger n of
    Just i -> Just (T.pack (show i))
    Nothing -> Just (T.pack (show n))
jsonScalarToText (Bool b) = Just (if b then "true" else "false")
jsonScalarToText _ = Nothing

scientificToInteger :: Scientific -> Maybe Integer
scientificToInteger n =
  case floatingOrInteger n :: Either Double Integer of
    Right i -> Just i
    Left _ -> Nothing

isoTimestamp :: Integer -> Text
isoTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . posixSecondsToUTCTime . fromIntegral
