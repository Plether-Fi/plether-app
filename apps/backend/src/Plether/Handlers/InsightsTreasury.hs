{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plether.Handlers.InsightsTreasury
  ( TreasuryWindowSummary (..)
  , DailyRevenuePoint (..)
  , PendingTreasuryRotation (..)
  , getTreasuryOverviewResponse
  , decodeAddressWord
  , decodePendingTreasury
  , decodeRiskProtocolShareBps
  , computeEffectiveFeeBps
  , computeAnnualizedRunRate7d
  , buildDailyRevenueSeries
  , extractManifestTreasuryAddress
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Word (Word64)
import Database.PostgreSQL.Simple (Connection, Only (..), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadOnly)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.AA.Kernel (KernelProfile (..))
import Plether.AA.KernelGateway (KernelGatewayConfig (..), KernelSponsorshipConfig (..), kernelScope)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.KernelOperation (KernelScope (..))
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall, encodeUint256)
import Plether.Ethereum.Client (CallParams (..), EthClient, ethBlockNumber, ethCallAtBlock)
import Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( AccountLedgerSnapshot (..)
  , getAccountLedgerSnapshotAtBlock
  )
import Plether.Ethereum.Rpc (ethGetBalance)
import Plether.Handlers.InsightsInfra (withTtlCache)
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Text.Read (readMaybe)

zeroAddress :: Text
zeroAddress = "0x0000000000000000000000000000000000000000"

treasuryCacheTtlNSec :: Word64
treasuryCacheTtlNSec = 10000000000 -- 10 seconds

{-# NOINLINE treasuryOverviewCache #-}
treasuryOverviewCache :: MVar (Maybe (Word64, ApiResponse Value))
treasuryOverviewCache = unsafePerformIO (newMVar Nothing)

data PendingTreasuryRotation = PendingTreasuryRotation
  { ptrTreasury :: Text
  , ptrActivationTime :: Integer
  , ptrLockedUntil :: Integer
  }
  deriving stock (Eq, Show)

data TreasuryWindowSummary = TreasuryWindowSummary
  { twsExecutedOrders :: Int
  , twsVolumeAssets :: Integer
  , twsExecutionFeeAssessedAssets :: Integer
  , twsExecutionFeeCreditedAssets :: Integer
  , twsFrozenSpreadAssessedAssets :: Integer
  , twsKeeperBountyAssets :: Integer
  , twsLiquidationCount :: Int
  , twsLiquidationKeeperBountyAssets :: Integer
  , twsCloseAssistanceCount :: Int
  , twsCloseAssistanceUsdgAssets :: Integer
  , twsSponsoredOpsCount :: Int
  , twsSponsoredGasSpentWei :: Integer
  }
  deriving stock (Eq, Show)

data DailyRevenuePoint = DailyRevenuePoint
  { drpDayStartUnix :: Integer
  , drpDayUtc :: Text
  , drpExecutedOrders :: Int
  , drpVolumeAssets :: Integer
  , drpExecutionFeeAssets :: Integer
  , drpCumulativeExecutionFeeAssets :: Integer
  , drpCloseAssistanceAssets :: Integer
  , drpSponsoredGasWei :: Integer
  }
  deriving stock (Eq, Show)

data OrderWindowRow = OrderWindowRow
  { owrOrders24h :: Int
  , owrVol24h :: Text
  , owrFee24h :: Text
  , owrSpread24h :: Text
  , owrBounty24h :: Text
  , owrOrders7d :: Int
  , owrVol7d :: Text
  , owrFee7d :: Text
  , owrSpread7d :: Text
  , owrBounty7d :: Text
  }

instance FromRow OrderWindowRow where
  fromRow =
    OrderWindowRow
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

data OrderLongWindowRow = OrderLongWindowRow
  { olwrOrders30d :: Int
  , olwrVol30d :: Text
  , olwrFee30d :: Text
  , olwrSpread30d :: Text
  , olwrBounty30d :: Text
  , olwrOrdersAll :: Int
  , olwrVolAll :: Text
  , olwrFeeAll :: Text
  , olwrSpreadAll :: Text
  , olwrBountyAll :: Text
  }

instance FromRow OrderLongWindowRow where
  fromRow =
    OrderLongWindowRow
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

data TopFeeOrderRow = TopFeeOrderRow
  { tforOrderId :: Integer
  , tforTxHash :: Text
  , tforAccount :: Text
  , tforSide :: Maybe Int
  , tforVolumeAssets :: Integer
  , tforExecutionFeeAssets :: Integer
  , tforBlockNumber :: Integer
  , tforTimestamp :: Integer
  }

data TreasuryMovementRow = TreasuryMovementRow
  { tmrKind :: Text
  , tmrDirection :: Text
  , tmrAmountAssets :: Integer
  , tmrCounterparty :: Maybe Text
  , tmrTxHash :: Text
  , tmrBlockNumber :: Integer
  , tmrTimestamp :: Integer
  }

data OnChainTreasuryState = OnChainTreasuryState
  { octRpcAvailable :: Bool
  , octBlockNumber :: Integer
  , octTreasuryAddress :: Maybe Text
  , octAddressSource :: Text
  , octPendingRotation :: Maybe PendingTreasuryRotation
  , octLockedUntil :: Maybe Integer
  , octExecutionFeeBps :: Maybe Integer
  , octLiquidationProtocolShareBps :: Maybe Integer
  , octLiquidationKeeperShareBps :: Maybe Integer
  , octMaintenanceFeeAprBps :: Maybe Integer
  , octMaintenanceFeeRecipient :: Maybe Text
  , octPendingMaintenanceFeeShares :: Maybe Integer
  , octPendingMaintenanceFeeAssets :: Maybe Integer
  , octClearinghouseSnapshot :: Maybe AccountLedgerSnapshot
  , octWalletUsdgBalance :: Maybe Integer
  , octWalletEthBalanceWei :: Maybe Integer
  , octJuniorVaultShares :: Maybe Integer
  , octJuniorVaultAssets :: Maybe Integer
  , octSeniorVaultShares :: Maybe Integer
  , octSeniorVaultAssets :: Maybe Integer
  }

getTreasuryOverviewResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
getTreasuryOverviewResponse pool client cfg mKernel =
  withTtlCache treasuryOverviewCache treasuryCacheTtlNSec $
    computeTreasuryOverviewResponse pool client cfg mKernel

computeTreasuryOverviewResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Maybe KernelGatewayConfig
  -> IO (Either ApiError (ApiResponse Value))
computeTreasuryOverviewResponse pool client cfg mKernel = do
  nowSec <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      settlementToken = T.toLower Manifest.settlementTokenAddress
      manifestTreasury = extractManifestTreasuryAddress Manifest.manifestValue
      since24h = nowSec - 86400
      since7d = nowSec - 7 * 86400
      since30d = nowSec - 30 * 86400
      todayStart = (nowSec `div` 86400) * 86400
      seriesStart = todayStart - 29 * 86400

  onChain <- fetchOnChainTreasuryState client manifestTreasury
  let effectiveTreasury = octTreasuryAddress onChain <|> manifestTreasury

  (win24, win7, win30, winAll, dailySeries, recentFeeOrders, movements) <-
    fetchTreasuryDbLedger
      pool
      cfg
      mKernel
      chainId
      router
      settlementToken
      effectiveTreasury
      since24h
      since7d
      since30d
      seriesStart
      todayStart

  let clearinghouseBal = alsSettlementBalanceAssets <$> octClearinghouseSnapshot onChain
      clearinghouseFree = alsFreeSettlementAssets <$> octClearinghouseSnapshot onChain
      clearinghouseClaim = alsTraderClaimBalanceAssets <$> octClearinghouseSnapshot onChain
      walletUsdg = octWalletUsdgBalance onChain
      jrAssets = octJuniorVaultAssets onChain
      srAssets = octSeniorVaultAssets onChain
      pendingMaintAssets = octPendingMaintenanceFeeAssets onChain
      hasAnyHolding =
        any
          (maybe False (const True))
          [clearinghouseBal, walletUsdg, jrAssets, srAssets]
      totalTreasuryUsdg :: Maybe Integer
      totalTreasuryUsdg =
        if hasAnyHolding
          then
            Just $
              fromMaybe 0 clearinghouseBal
                + fromMaybe 0 walletUsdg
                + fromMaybe 0 jrAssets
                + fromMaybe 0 srAssets
          else Nothing
      runRate7d = computeAnnualizedRunRate7d (twsExecutionFeeAssessedAssets win7)

  pure $
    Right $
      mkResponse (octBlockNumber onChain) chainId $
        object
          [ "network"
              .= object
                [ "chainId" .= chainId
                , "releaseVersion" .= Manifest.releaseVersion
                , "settlementAssetSymbol" .= ("USDG" :: Text)
                ]
          , "observedAt" .= isoTimestamp nowSec
          , "rpcAvailable" .= octRpcAvailable onChain
          , "treasury"
              .= object
                [ "address" .= effectiveTreasury
                , "addressSource" .= octAddressSource onChain
                , "deploymentTreasuryAddress" .= manifestTreasury
                , "lockedUntil" .= (isoTimestamp <$> octLockedUntil onChain)
                , "isLocked" .= maybe False (> nowSec) (octLockedUntil onChain)
                , "pendingRotation" .= (pendingRotationToJson <$> octPendingRotation onChain)
                ]
          , "feePolicy"
              .= object
                [ "executionFeeBps" .= octExecutionFeeBps onChain
                , "liquidationProtocolShareBps" .= octLiquidationProtocolShareBps onChain
                , "liquidationKeeperShareBps" .= octLiquidationKeeperShareBps onChain
                , "juniorMaintenanceFeeAprBps" .= octMaintenanceFeeAprBps onChain
                , "juniorMaintenanceFeeRecipient" .= octMaintenanceFeeRecipient onChain
                , "pendingMaintenanceFeeShares" .= (show <$> octPendingMaintenanceFeeShares onChain)
                , "pendingMaintenanceFeeAssets" .= (show <$> pendingMaintAssets)
                ]
          , "holdings"
              .= object
                [ "totalUsdgAssets" .= (show <$> totalTreasuryUsdg)
                , "clearinghouseSettlementAssets" .= (show <$> clearinghouseBal)
                , "clearinghouseFreeAssets" .= (show <$> clearinghouseFree)
                , "clearinghouseClaimAssets" .= (show <$> clearinghouseClaim)
                , "clearinghouseHasPosition" .= (alsHasPosition <$> octClearinghouseSnapshot onChain)
                , "walletUsdgAssets" .= (show <$> walletUsdg)
                , "walletEthWei" .= (show <$> octWalletEthBalanceWei onChain)
                , "juniorVaultShares" .= (show <$> octJuniorVaultShares onChain)
                , "juniorVaultAssets" .= (show <$> jrAssets)
                , "seniorVaultShares" .= (show <$> octSeniorVaultShares onChain)
                , "seniorVaultAssets" .= (show <$> srAssets)
                , "pendingMaintenanceFeeAssets" .= (show <$> pendingMaintAssets)
                ]
          , "profitOverview"
              .= object
                [ "annualizedExecutionFeeRunRate7dAssets" .= (show <$> runRate7d)
                , "window24h" .= windowSummaryToJson win24
                , "window7d" .= windowSummaryToJson win7
                , "window30d" .= windowSummaryToJson win30
                , "allTime" .= windowSummaryToJson winAll
                ]
          , "dailySeries" .= map dailyPointToJson dailySeries
          , "recentFeeOrders" .= map topFeeOrderToJson recentFeeOrders
          , "treasuryMovements" .= map movementToJson movements
          ]

fetchOnChainTreasuryState :: EthClient -> Maybe Text -> IO OnChainTreasuryState
fetchOnChainTreasuryState client manifestTreasury = do
  mBlock <- timeout 3000000 $ observeIo $ ethBlockNumber client
  case mBlock of
    Just (Just (Right blk)) -> do
      let engine = Manifest.cfdEngineAddress
          jrVault = Manifest.juniorVaultAddress
          srVault = Manifest.seniorVaultAddress
          token = Manifest.settlementTokenAddress
          accountLens = Manifest.cfdEngineAccountLensAddress
          callAt target calldata = do
            res <- timeout 2500000 $ observeIo $ ethCallAtBlock client (CallParams target calldata) blk
            pure $ case res of
              Just (Just (Right bytes)) -> Just bytes
              _ -> Nothing
          callNoArg target sig = callAt target (encodeCall sig [])

      (mLiveTreasuryBytes, mPendingBytes, mLockedBytes, mExecFeeBytes, mRiskBytes, mMaintAprBytes, mMaintRecipBytes, mPendingMaintSharesBytes) <-
        runConcurrently $
          (,,,,,,,)
            <$> Concurrently (callNoArg engine "protocolTreasury()")
            <*> Concurrently (callNoArg engine "pendingProtocolTreasury()")
            <*> Concurrently (callNoArg engine "protocolTreasuryLockedUntil()")
            <*> Concurrently (callNoArg engine "executionFeeBps()")
            <*> Concurrently (callNoArg engine "riskParams()")
            <*> Concurrently (callNoArg jrVault "maintenanceFeeAprBps()")
            <*> Concurrently (callNoArg jrVault "maintenanceFeeRecipient()")
            <*> Concurrently (callNoArg jrVault "pendingMaintenanceFeeShares()")

      let mOnChainTreasury = mLiveTreasuryBytes >>= decodeAddressWord
          (mTreasuryAddr, addrSource) = case mOnChainTreasury of
            Just a -> (Just a, "onchain" :: Text)
            Nothing -> (manifestTreasury, maybe "unavailable" (const "manifest") manifestTreasury)
          mPendingRot = mPendingBytes >>= decodePendingTreasury
          mLockedRaw = mLockedBytes >>= decodeSingleUint
          mLockedUntil = mLockedRaw >>= (\t -> if t > 0 then Just t else Nothing)
          mExecFeeBps = mExecFeeBytes >>= decodeSingleUint
          (mKeeperShareBps, mProtShareBps) = maybe (Nothing, Nothing) id (mRiskBytes >>= decodeRiskProtocolShareBps)
          mMaintAprBps = mMaintAprBytes >>= decodeSingleUint
          mMaintRecip = mMaintRecipBytes >>= decodeAddressWord
          mPendingMaintShares = mPendingMaintSharesBytes >>= decodeSingleUint

      mPendingMaintAssets <- case mPendingMaintShares of
        Just sh | sh > 0 -> do
          mBytes <- callAt jrVault (encodeCall "convertToAssets(uint256)" [encodeUint256 sh])
          pure (mBytes >>= decodeSingleUint)
        Just 0 -> pure (Just 0)
        _ -> pure Nothing

      (mSnap, mWalletUsdg, mWalletEth, mJrShares, mSrShares) <- case mTreasuryAddr of
        Nothing -> pure (Nothing, Nothing, Nothing, Nothing, Nothing)
        Just addr ->
          runConcurrently $
            (,,,,)
              <$> Concurrently
                ( do
                    r <- timeout 2500000 $ observeIo $ getAccountLedgerSnapshotAtBlock client accountLens addr blk
                    pure $ case r of
                      Just (Just (Right snap)) -> Just snap
                      _ -> Nothing
                )
              <*> Concurrently
                ( do
                    b <- callAt token (encodeCall "balanceOf(address)" [encodeAddress addr])
                    pure (b >>= decodeSingleUint)
                )
              <*> Concurrently
                ( do
                    r <- timeout 2500000 $ observeIo $ ethGetBalance client addr
                    pure $ case r of
                      Just (Just (Right wei)) -> Just wei
                      _ -> Nothing
                )
              <*> Concurrently
                ( do
                    b <- callAt jrVault (encodeCall "balanceOf(address)" [encodeAddress addr])
                    pure (b >>= decodeSingleUint)
                )
              <*> Concurrently
                ( do
                    b <- callAt srVault (encodeCall "balanceOf(address)" [encodeAddress addr])
                    pure (b >>= decodeSingleUint)
                )

      (mJrAssets, mSrAssets) <-
        runConcurrently $
          (,)
            <$> Concurrently
              ( case mJrShares of
                  Just sh | sh > 0 -> (>>= decodeSingleUint) <$> callAt jrVault (encodeCall "convertToAssets(uint256)" [encodeUint256 sh])
                  Just 0 -> pure (Just 0)
                  _ -> pure Nothing
              )
            <*> Concurrently
              ( case mSrShares of
                  Just sh | sh > 0 -> (>>= decodeSingleUint) <$> callAt srVault (encodeCall "convertToAssets(uint256)" [encodeUint256 sh])
                  Just 0 -> pure (Just 0)
                  _ -> pure Nothing
              )

      pure
        OnChainTreasuryState
          { octRpcAvailable = True
          , octBlockNumber = blk
          , octTreasuryAddress = mTreasuryAddr
          , octAddressSource = addrSource
          , octPendingRotation = mPendingRot
          , octLockedUntil = mLockedUntil
          , octExecutionFeeBps = mExecFeeBps
          , octLiquidationProtocolShareBps = mProtShareBps
          , octLiquidationKeeperShareBps = mKeeperShareBps
          , octMaintenanceFeeAprBps = mMaintAprBps
          , octMaintenanceFeeRecipient = mMaintRecip
          , octPendingMaintenanceFeeShares = mPendingMaintShares
          , octPendingMaintenanceFeeAssets = mPendingMaintAssets
          , octClearinghouseSnapshot = mSnap
          , octWalletUsdgBalance = mWalletUsdg
          , octWalletEthBalanceWei = mWalletEth
          , octJuniorVaultShares = mJrShares
          , octJuniorVaultAssets = mJrAssets
          , octSeniorVaultShares = mSrShares
          , octSeniorVaultAssets = mSrAssets
          }
    _ ->
      pure
        OnChainTreasuryState
          { octRpcAvailable = False
          , octBlockNumber = Manifest.releaseDeploymentBlock
          , octTreasuryAddress = manifestTreasury
          , octAddressSource = maybe "unavailable" (const "manifest") manifestTreasury
          , octPendingRotation = Nothing
          , octLockedUntil = Nothing
          , octExecutionFeeBps = Nothing
          , octLiquidationProtocolShareBps = Nothing
          , octLiquidationKeeperShareBps = Nothing
          , octMaintenanceFeeAprBps = Nothing
          , octMaintenanceFeeRecipient = Nothing
          , octPendingMaintenanceFeeShares = Nothing
          , octPendingMaintenanceFeeAssets = Nothing
          , octClearinghouseSnapshot = Nothing
          , octWalletUsdgBalance = Nothing
          , octWalletEthBalanceWei = Nothing
          , octJuniorVaultShares = Nothing
          , octJuniorVaultAssets = Nothing
          , octSeniorVaultShares = Nothing
          , octSeniorVaultAssets = Nothing
          }

fetchTreasuryDbLedger
  :: DbPool
  -> Config
  -> Maybe KernelGatewayConfig
  -> Integer
  -> Text
  -> Text
  -> Maybe Text
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> IO
      ( TreasuryWindowSummary
      , TreasuryWindowSummary
      , TreasuryWindowSummary
      , TreasuryWindowSummary
      , [DailyRevenuePoint]
      , [TopFeeOrderRow]
      , [TreasuryMovementRow]
      )
fetchTreasuryDbLedger pool _cfg mKernel chainId router tokenAddr mTreasury since24h since7d since30d seriesStart todayStart = do
  (ordShort, ordLong, creditedTuple, liqTuple, dailyOrders, priorFeeTotal, topFeeOrders, movements) <-
    withDb pool $ \conn ->
      withReadSnapshot conn $ do
        sRows <-
          query
            conn
            "WITH bounds AS (SELECT ?::BIGINT AS s24, ?::BIGINT AS s7) \
            \SELECT \
            \  COUNT(*) FILTER (WHERE o.terminal_timestamp >= b.s24)::INT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionNotionalAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionNotionalAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s24), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionFeeAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionFeeAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s24), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'frozenSpreadAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'frozenSpreadAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s24), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionBountyAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionBountyAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s24), 0)::TEXT, \
            \  COUNT(*) FILTER (WHERE o.terminal_timestamp >= b.s7)::INT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionNotionalAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionNotionalAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s7), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionFeeAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionFeeAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s7), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'frozenSpreadAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'frozenSpreadAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s7), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionBountyAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionBountyAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s7), 0)::TEXT \
            \FROM perps_orders o \
            \CROSS JOIN bounds b \
            \WHERE o.chain_id = ? AND o.order_router = ? AND o.terminal_status = 'Executed' AND o.terminal_timestamp IS NOT NULL"
            (since24h, since7d, chainId, router)
            :: IO [OrderWindowRow]
        lRows <-
          query
            conn
            "WITH bounds AS (SELECT ?::BIGINT AS s30) \
            \SELECT \
            \  COUNT(*) FILTER (WHERE o.terminal_timestamp >= b.s30)::INT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionNotionalAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionNotionalAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s30), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionFeeAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionFeeAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s30), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'frozenSpreadAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'frozenSpreadAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s30), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionBountyAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionBountyAssets')::NUMERIC ELSE 0 END) FILTER (WHERE o.terminal_timestamp >= b.s30), 0)::TEXT, \
            \  COUNT(*)::INT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionNotionalAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionNotionalAssets')::NUMERIC ELSE 0 END), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionFeeAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionFeeAssets')::NUMERIC ELSE 0 END), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'frozenSpreadAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'frozenSpreadAssets')::NUMERIC ELSE 0 END), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN o.receipt_economics->>'executionBountyAssets' ~ '^[0-9]+$' THEN (o.receipt_economics->>'executionBountyAssets')::NUMERIC ELSE 0 END), 0)::TEXT \
            \FROM perps_orders o \
            \CROSS JOIN bounds b \
            \WHERE o.chain_id = ? AND o.order_router = ? AND o.terminal_status = 'Executed' AND o.terminal_timestamp IS NOT NULL"
            (since30d, chainId, router)
            :: IO [OrderLongWindowRow]
        credRows <-
          query
            conn
            "SELECT \
            \  COALESCE(SUM(CASE WHEN data->>'protocolFeeCollectedAssets' ~ '^[0-9]+$' THEN (data->>'protocolFeeCollectedAssets')::NUMERIC ELSE 0 END) FILTER (WHERE timestamp >= ?), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN data->>'protocolFeeCollectedAssets' ~ '^[0-9]+$' THEN (data->>'protocolFeeCollectedAssets')::NUMERIC ELSE 0 END) FILTER (WHERE timestamp >= ?), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN data->>'protocolFeeCollectedAssets' ~ '^[0-9]+$' THEN (data->>'protocolFeeCollectedAssets')::NUMERIC ELSE 0 END) FILTER (WHERE timestamp >= ?), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN data->>'protocolFeeCollectedAssets' ~ '^[0-9]+$' THEN (data->>'protocolFeeCollectedAssets')::NUMERIC ELSE 0 END), 0)::TEXT \
            \FROM perps_events \
            \WHERE chain_id = ? AND release_router = ? AND event_name = 'ActionChargeSettled'"
            (since24h, since7d, since30d, chainId, router)
            :: IO [(Text, Text, Text, Text)]
        liqRows <-
          query
            conn
            "SELECT \
            \  COUNT(*) FILTER (WHERE timestamp >= ?)::INT, \
            \  COALESCE(SUM(CASE WHEN data->>'keeperBountyAssets' ~ '^[0-9]+$' THEN (data->>'keeperBountyAssets')::NUMERIC ELSE 0 END) FILTER (WHERE timestamp >= ?), 0)::TEXT, \
            \  COUNT(*) FILTER (WHERE timestamp >= ?)::INT, \
            \  COALESCE(SUM(CASE WHEN data->>'keeperBountyAssets' ~ '^[0-9]+$' THEN (data->>'keeperBountyAssets')::NUMERIC ELSE 0 END) FILTER (WHERE timestamp >= ?), 0)::TEXT, \
            \  COUNT(*) FILTER (WHERE timestamp >= ?)::INT, \
            \  COALESCE(SUM(CASE WHEN data->>'keeperBountyAssets' ~ '^[0-9]+$' THEN (data->>'keeperBountyAssets')::NUMERIC ELSE 0 END) FILTER (WHERE timestamp >= ?), 0)::TEXT, \
            \  COUNT(*)::INT, \
            \  COALESCE(SUM(CASE WHEN data->>'keeperBountyAssets' ~ '^[0-9]+$' THEN (data->>'keeperBountyAssets')::NUMERIC ELSE 0 END), 0)::TEXT \
            \FROM perps_account_activity \
            \WHERE chain_id = ? AND release_router = ? AND activity_type = 'Liquidated'"
            (since24h, since24h, since7d, since7d, since30d, since30d, chainId, router)
            :: IO [(Int, Text, Int, Text, Int, Text, Int, Text)]
        priorRows <-
          query
            conn
            "SELECT COALESCE(SUM(CASE WHEN receipt_economics->>'executionFeeAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'executionFeeAssets')::NUMERIC ELSE 0 END), 0)::TEXT \
            \FROM perps_orders \
            \WHERE chain_id = ? AND order_router = ? AND terminal_status = 'Executed' AND terminal_timestamp IS NOT NULL AND terminal_timestamp < ?"
            (chainId, router, seriesStart)
            :: IO [Only Text]
        dayOrdRows <-
          query
            conn
            "SELECT \
            \  ((terminal_timestamp / 86400) * 86400)::BIGINT AS day_unix, \
            \  COUNT(*)::INT, \
            \  COALESCE(SUM(CASE WHEN receipt_economics->>'executionNotionalAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'executionNotionalAssets')::NUMERIC ELSE 0 END), 0)::TEXT, \
            \  COALESCE(SUM(CASE WHEN receipt_economics->>'executionFeeAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'executionFeeAssets')::NUMERIC ELSE 0 END), 0)::TEXT \
            \FROM perps_orders \
            \WHERE chain_id = ? AND order_router = ? AND terminal_status = 'Executed' \
            \  AND terminal_timestamp IS NOT NULL AND terminal_timestamp >= ? \
            \GROUP BY day_unix \
            \ORDER BY day_unix ASC"
            (chainId, router, seriesStart)
            :: IO [(Integer, Int, Text, Text)]
        topRows <-
          query
            conn
            "SELECT \
            \  order_id, \
            \  COALESCE(terminal_tx_hash, ''), \
            \  COALESCE(account, ''), \
            \  side, \
            \  COALESCE(CASE WHEN receipt_economics->>'executionNotionalAssets' ~ '^[0-9]+$' THEN receipt_economics->>'executionNotionalAssets' ELSE '0' END, '0'), \
            \  COALESCE(CASE WHEN receipt_economics->>'executionFeeAssets' ~ '^[0-9]+$' THEN receipt_economics->>'executionFeeAssets' ELSE '0' END, '0'), \
            \  COALESCE(terminal_block_number, 0), \
            \  COALESCE(terminal_timestamp, 0) \
            \FROM perps_orders \
            \WHERE chain_id = ? AND order_router = ? AND terminal_status = 'Executed' \
            \  AND receipt_economics->>'executionFeeAssets' ~ '^[0-9]+$' \
            \  AND (receipt_economics->>'executionFeeAssets')::NUMERIC > 0 \
            \ORDER BY COALESCE(terminal_block_number, 0) DESC, order_id DESC \
            \LIMIT 20"
            (chainId, router)
            :: IO [(Integer, Text, Text, Maybe Int, Text, Text, Integer, Integer)]
        movRows <- case mTreasury of
          Nothing -> pure []
          Just addr -> fetchTreasuryMovements conn chainId router tokenAddr (T.toLower addr)
        let sRow = fromMaybe (OrderWindowRow 0 "0" "0" "0" "0" 0 "0" "0" "0" "0") (listToMaybe sRows)
            lRow = fromMaybe (OrderLongWindowRow 0 "0" "0" "0" "0" 0 "0" "0" "0" "0") (listToMaybe lRows)
            credT = fromMaybe ("0", "0", "0", "0") (listToMaybe credRows)
            liqT = fromMaybe (0, "0", 0, "0", 0, "0", 0, "0") (listToMaybe liqRows)
            priorFee = maybe 0 (\(Only t) -> parseIntegerOrZero t) (listToMaybe priorRows)
            parsedTop =
              [ TopFeeOrderRow oid txH acc sd (parseIntegerOrZero volT) (parseIntegerOrZero feeT) blk ts
              | (oid, txH, acc, sd, volT, feeT, blk, ts) <- topRows
              ]
        pure (sRow, lRow, credT, liqT, dayOrdRows, priorFee, parsedTop, movRows)

  -- Subsidy & AA sponsorship costs live in optional tables depending on stack;
  -- query them best-effort via observeIo so missing tables fail softly to 0.
  (closeAssistWin, dailyCloseAssist) <- fetchCloseAssistanceTelemetry pool chainId router seriesStart
  (gasWin, dailyGas) <- fetchSponsoredGasTelemetry pool mKernel seriesStart

  let (cred24, cred7, cred30, credAll) = creditedTuple
      (liqC24, liqB24, liqC7, liqB7, liqC30, liqB30, liqCAll, liqBAll) = liqTuple
      ((caC24, caA24), (caC7, caA7), (caC30, caA30), (caCAll, caAAll)) = closeAssistWin
      ((gC24, gW24), (gC7, gW7), (gC30, gW30), (gCAll, gWAll)) = gasWin
      win24 =
        TreasuryWindowSummary
          { twsExecutedOrders = owrOrders24h ordShort
          , twsVolumeAssets = parseIntegerOrZero (owrVol24h ordShort)
          , twsExecutionFeeAssessedAssets = parseIntegerOrZero (owrFee24h ordShort)
          , twsExecutionFeeCreditedAssets = parseIntegerOrZero cred24
          , twsFrozenSpreadAssessedAssets = parseIntegerOrZero (owrSpread24h ordShort)
          , twsKeeperBountyAssets = parseIntegerOrZero (owrBounty24h ordShort)
          , twsLiquidationCount = liqC24
          , twsLiquidationKeeperBountyAssets = parseIntegerOrZero liqB24
          , twsCloseAssistanceCount = caC24
          , twsCloseAssistanceUsdgAssets = caA24
          , twsSponsoredOpsCount = gC24
          , twsSponsoredGasSpentWei = gW24
          }
      win7 =
        TreasuryWindowSummary
          { twsExecutedOrders = owrOrders7d ordShort
          , twsVolumeAssets = parseIntegerOrZero (owrVol7d ordShort)
          , twsExecutionFeeAssessedAssets = parseIntegerOrZero (owrFee7d ordShort)
          , twsExecutionFeeCreditedAssets = parseIntegerOrZero cred7
          , twsFrozenSpreadAssessedAssets = parseIntegerOrZero (owrSpread7d ordShort)
          , twsKeeperBountyAssets = parseIntegerOrZero (owrBounty7d ordShort)
          , twsLiquidationCount = liqC7
          , twsLiquidationKeeperBountyAssets = parseIntegerOrZero liqB7
          , twsCloseAssistanceCount = caC7
          , twsCloseAssistanceUsdgAssets = caA7
          , twsSponsoredOpsCount = gC7
          , twsSponsoredGasSpentWei = gW7
          }
      win30 =
        TreasuryWindowSummary
          { twsExecutedOrders = olwrOrders30d ordLong
          , twsVolumeAssets = parseIntegerOrZero (olwrVol30d ordLong)
          , twsExecutionFeeAssessedAssets = parseIntegerOrZero (olwrFee30d ordLong)
          , twsExecutionFeeCreditedAssets = parseIntegerOrZero cred30
          , twsFrozenSpreadAssessedAssets = parseIntegerOrZero (olwrSpread30d ordLong)
          , twsKeeperBountyAssets = parseIntegerOrZero (olwrBounty30d ordLong)
          , twsLiquidationCount = liqC30
          , twsLiquidationKeeperBountyAssets = parseIntegerOrZero liqB30
          , twsCloseAssistanceCount = caC30
          , twsCloseAssistanceUsdgAssets = caA30
          , twsSponsoredOpsCount = gC30
          , twsSponsoredGasSpentWei = gW30
          }
      winAll =
        TreasuryWindowSummary
          { twsExecutedOrders = olwrOrdersAll ordLong
          , twsVolumeAssets = parseIntegerOrZero (olwrVolAll ordLong)
          , twsExecutionFeeAssessedAssets = parseIntegerOrZero (olwrFeeAll ordLong)
          , twsExecutionFeeCreditedAssets = parseIntegerOrZero credAll
          , twsFrozenSpreadAssessedAssets = parseIntegerOrZero (olwrSpreadAll ordLong)
          , twsKeeperBountyAssets = parseIntegerOrZero (olwrBountyAll ordLong)
          , twsLiquidationCount = liqCAll
          , twsLiquidationKeeperBountyAssets = parseIntegerOrZero liqBAll
          , twsCloseAssistanceCount = caCAll
          , twsCloseAssistanceUsdgAssets = caAAll
          , twsSponsoredOpsCount = gCAll
          , twsSponsoredGasSpentWei = gWAll
          }
      orderDayMap =
        Map.fromList
          [ (dayU, (cnt, parseIntegerOrZero volT, parseIntegerOrZero feeT))
          | (dayU, cnt, volT, feeT) <- dailyOrders
          ]
      dailySeries =
        buildDailyRevenueSeries
          seriesStart
          todayStart
          priorFeeTotal
          orderDayMap
          dailyCloseAssist
          dailyGas
  pure (win24, win7, win30, winAll, dailySeries, topFeeOrders, movements)

fetchCloseAssistanceTelemetry
  :: DbPool
  -> Integer
  -> Text
  -> Integer
  -> IO (((Int, Integer), (Int, Integer), (Int, Integer), (Int, Integer)), Map.Map Integer Integer)
fetchCloseAssistanceTelemetry pool chainId router seriesStart = do
  mRes <- observeIo $ withDb pool $ \conn -> do
    winRows <-
      query
        conn
        "SELECT \
        \  COUNT(*) FILTER (WHERE g.verified AND a.settled_at >= NOW() - interval '24 hours')::INT, \
        \  COALESCE(SUM(g.amount_assets) FILTER (WHERE g.verified AND a.settled_at >= NOW() - interval '24 hours'), 0)::TEXT, \
        \  COUNT(*) FILTER (WHERE g.verified AND a.settled_at >= NOW() - interval '7 days')::INT, \
        \  COALESCE(SUM(g.amount_assets) FILTER (WHERE g.verified AND a.settled_at >= NOW() - interval '7 days'), 0)::TEXT, \
        \  COUNT(*) FILTER (WHERE g.verified AND a.settled_at >= NOW() - interval '30 days')::INT, \
        \  COALESCE(SUM(g.amount_assets) FILTER (WHERE g.verified AND a.settled_at >= NOW() - interval '30 days'), 0)::TEXT, \
        \  COUNT(*) FILTER (WHERE g.verified)::INT, \
        \  COALESCE(SUM(g.amount_assets) FILTER (WHERE g.verified), 0)::TEXT \
        \FROM aa_close_assistance g \
        \JOIN aa_sponsorship_authorizations a USING (digest) \
        \WHERE g.chain_id = ? AND g.router = ?"
        (chainId, router)
        :: IO [(Int, Text, Int, Text, Int, Text, Int, Text)]
    dayRows <-
      query
        conn
        "SELECT \
        \  ((FLOOR(EXTRACT(EPOCH FROM a.settled_at))::BIGINT / 86400) * 86400)::BIGINT AS day_unix, \
        \  COALESCE(SUM(g.amount_assets), 0)::TEXT \
        \FROM aa_close_assistance g \
        \JOIN aa_sponsorship_authorizations a USING (digest) \
        \WHERE g.chain_id = ? AND g.router = ? AND g.verified \
        \  AND a.settled_at IS NOT NULL AND EXTRACT(EPOCH FROM a.settled_at) >= ? \
        \GROUP BY day_unix"
        (chainId, router, seriesStart)
        :: IO [(Integer, Text)]
    pure (winRows, dayRows)
  pure $ case mRes of
    Just ([(c24, a24, c7, a7, c30, a30, cAll, aAll)], dayRows) ->
      ( ( (c24, parseIntegerOrZero a24)
        , (c7, parseIntegerOrZero a7)
        , (c30, parseIntegerOrZero a30)
        , (cAll, parseIntegerOrZero aAll)
        )
      , Map.fromList [(d, parseIntegerOrZero amt) | (d, amt) <- dayRows]
      )
    _ -> (((0, 0), (0, 0), (0, 0), (0, 0)), Map.empty)

fetchSponsoredGasTelemetry
  :: DbPool
  -> Maybe KernelGatewayConfig
  -> Integer
  -> IO (((Int, Integer), (Int, Integer), (Int, Integer), (Int, Integer)), Map.Map Integer Integer)
fetchSponsoredGasTelemetry pool mKernel seriesStart =
  case mKernel of
    Just kgc | Just ksc <- kgcSponsorship kgc -> do
      let KernelScope {..} = kernelScope (kgcProfile kgc)
          paymaster = T.toLower (kscPaymaster ksc)
      mRes <- observeIo $ withDb pool $ \conn -> do
        winRows <-
          query
            conn
            "SELECT \
            \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '24 hours')::INT, \
            \  COALESCE(SUM(actual_gas_cost) FILTER (WHERE settled_at >= NOW() - interval '24 hours'), 0)::TEXT, \
            \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '7 days')::INT, \
            \  COALESCE(SUM(actual_gas_cost) FILTER (WHERE settled_at >= NOW() - interval '7 days'), 0)::TEXT, \
            \  COUNT(*) FILTER (WHERE settled_at >= NOW() - interval '30 days')::INT, \
            \  COALESCE(SUM(actual_gas_cost) FILTER (WHERE settled_at >= NOW() - interval '30 days'), 0)::TEXT, \
            \  COUNT(*)::INT, \
            \  COALESCE(SUM(actual_gas_cost), 0)::TEXT \
            \FROM aa_kernel_sponsorships \
            \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? AND status = 'settled'"
            (ksDeployment, ksChainId, ksEntryPoint, paymaster)
            :: IO [(Int, Text, Int, Text, Int, Text, Int, Text)]
        dayRows <-
          query
            conn
            "SELECT \
            \  ((FLOOR(EXTRACT(EPOCH FROM settled_at))::BIGINT / 86400) * 86400)::BIGINT AS day_unix, \
            \  COALESCE(SUM(actual_gas_cost), 0)::TEXT \
            \FROM aa_kernel_sponsorships \
            \WHERE deployment = ? AND chain_id = ? AND entry_point = ? AND paymaster = ? \
            \  AND status = 'settled' AND settled_at IS NOT NULL AND EXTRACT(EPOCH FROM settled_at) >= ? \
            \GROUP BY day_unix"
            (ksDeployment, ksChainId, ksEntryPoint, paymaster, seriesStart)
            :: IO [(Integer, Text)]
        pure (winRows, dayRows)
      pure (parseGasResult mRes)
    _ -> do
      mRes <- observeIo $ withDb pool $ \conn -> do
        winRows <-
          query_
            conn
            "SELECT \
            \  COUNT(*) FILTER (WHERE finalized_at >= NOW() - interval '24 hours')::INT, \
            \  COALESCE(SUM(actual_gas_cost_wei) FILTER (WHERE finalized_at >= NOW() - interval '24 hours'), 0)::TEXT, \
            \  COUNT(*) FILTER (WHERE finalized_at >= NOW() - interval '7 days')::INT, \
            \  COALESCE(SUM(actual_gas_cost_wei) FILTER (WHERE finalized_at >= NOW() - interval '7 days'), 0)::TEXT, \
            \  COUNT(*) FILTER (WHERE finalized_at >= NOW() - interval '30 days')::INT, \
            \  COALESCE(SUM(actual_gas_cost_wei) FILTER (WHERE finalized_at >= NOW() - interval '30 days'), 0)::TEXT, \
            \  COUNT(*)::INT, \
            \  COALESCE(SUM(actual_gas_cost_wei), 0)::TEXT \
            \FROM aa_user_operation_events"
            :: IO [(Int, Text, Int, Text, Int, Text, Int, Text)]
        dayRows <-
          query
            conn
            "SELECT \
            \  ((FLOOR(EXTRACT(EPOCH FROM finalized_at))::BIGINT / 86400) * 86400)::BIGINT AS day_unix, \
            \  COALESCE(SUM(actual_gas_cost_wei), 0)::TEXT \
            \FROM aa_user_operation_events \
            \WHERE EXTRACT(EPOCH FROM finalized_at) >= ? \
            \GROUP BY day_unix"
            (Only seriesStart)
            :: IO [(Integer, Text)]
        pure (winRows, dayRows)
      pure (parseGasResult mRes)
  where
    parseGasResult = \case
      Just ([(c24, w24, c7, w7, c30, w30, cAll, wAll)], dayRows) ->
        ( ( (c24, parseIntegerOrZero w24)
          , (c7, parseIntegerOrZero w7)
          , (c30, parseIntegerOrZero w30)
          , (cAll, parseIntegerOrZero wAll)
          )
        , Map.fromList [(d, parseIntegerOrZero wei) | (d, wei) <- dayRows]
        )
      _ -> (((0, 0), (0, 0), (0, 0), (0, 0)), Map.empty)

fetchTreasuryMovements
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> IO [TreasuryMovementRow]
fetchTreasuryMovements conn chainId router tokenAddr treasuryAddr = do
  clearingRows <-
    query
      conn
      "SELECT activity_type, COALESCE(amount_assets, 0)::TEXT, tx_hash, block_number, timestamp \
      \FROM perps_account_activity \
      \WHERE chain_id = ? AND release_router = ? AND LOWER(account) = ? \
      \  AND activity_type IN ('Deposit', 'Withdraw') \
      \ORDER BY block_number DESC, log_index DESC \
      \LIMIT 25"
      (chainId, router, treasuryAddr)
      :: IO [(Text, Text, Text, Integer, Integer)]
  transferRows <-
    query
      conn
      "SELECT LOWER(from_address), LOWER(to_address), amount::TEXT, tx_hash, block_number, timestamp \
      \FROM perps_asset_transfers \
      \WHERE chain_id = ? AND release_router = ? AND LOWER(token_address) = ? \
      \  AND (LOWER(from_address) = ? OR LOWER(to_address) = ?) \
      \ORDER BY block_number DESC, log_index DESC \
      \LIMIT 25"
      (chainId, router, tokenAddr, treasuryAddr, treasuryAddr)
      :: IO [(Text, Text, Text, Text, Integer, Integer)]
  let clearingMovs =
        [ TreasuryMovementRow
            { tmrKind = if kind == "Withdraw" then "clearinghouse_withdraw" else "clearinghouse_deposit"
            , tmrDirection = if kind == "Withdraw" then "outflow" else "inflow"
            , tmrAmountAssets = parseIntegerOrZero amtT
            , tmrCounterparty = Just (T.toLower Manifest.marginClearinghouseAddress)
            , tmrTxHash = T.toLower txH
            , tmrBlockNumber = blk
            , tmrTimestamp = ts
            }
        | (kind, amtT, txH, blk, ts) <- clearingRows
        ]
      clearingTxSet = Map.fromList [(tmrTxHash m, ()) | m <- clearingMovs]
      transferMovs =
        [ let isIn = toA == treasuryAddr
           in TreasuryMovementRow
                { tmrKind = if isIn then "usdg_transfer_in" else "usdg_transfer_out"
                , tmrDirection = if isIn then "inflow" else "outflow"
                , tmrAmountAssets = parseIntegerOrZero amtT
                , tmrCounterparty = Just (if isIn then fromA else toA)
                , tmrTxHash = T.toLower txH
                , tmrBlockNumber = blk
                , tmrTimestamp = ts
                }
        | (fromA, toA, amtT, txH, blk, ts) <- transferRows
        , not (Map.member (T.toLower txH) clearingTxSet)
        ]
  pure $
    take 25 $
      sortOn (\m -> (Down (tmrBlockNumber m), Down (tmrTimestamp m))) (clearingMovs <> transferMovs)

-- Pure helpers (unit-tested in InsightsTreasurySpec)

extractManifestTreasuryAddress :: Value -> Maybe Text
extractManifestTreasuryAddress (Object root) = do
  Object rel <- KM.lookup (Key.fromText "release") root
  String raw <- KM.lookup (Key.fromText "deploymentTimeProtocolTreasury") rel
  let clean = T.toLower (T.strip raw)
  if T.length clean == 42 && T.isPrefixOf "0x" clean && clean /= zeroAddress
    then Just clean
    else Nothing
extractManifestTreasuryAddress _ = Nothing

decodeAddressWord :: ByteString -> Maybe Text
decodeAddressWord bytes
  | BS.length bytes /= 32 = Nothing
  | otherwise =
      let addrHex = "0x" <> T.toLower (TE.decodeUtf8 (B16.encode (BS.drop 12 bytes)))
       in if addrHex == zeroAddress then Nothing else Just addrHex

decodePendingTreasury :: ByteString -> Maybe PendingTreasuryRotation
decodePendingTreasury bytes
  | BS.length bytes /= 96 = Nothing
  | otherwise = do
      addr <- decodeAddressWord (BS.take 32 bytes)
      let actTime = decodeUint256 (BS.take 32 (BS.drop 32 bytes))
          lockUntil = decodeUint256 (BS.take 32 (BS.drop 64 bytes))
      pure (PendingTreasuryRotation addr actTime lockUntil)

decodeRiskProtocolShareBps :: ByteString -> Maybe (Maybe Integer, Maybe Integer)
decodeRiskProtocolShareBps bytes
  | BS.length bytes /= 320 = Nothing
  | otherwise =
      let keeperShare = decodeUint256 (BS.take 32 (BS.drop (8 * 32) bytes))
          protocolShare = decodeUint256 (BS.take 32 (BS.drop (9 * 32) bytes))
       in Just (Just keeperShare, Just protocolShare)

decodeSingleUint :: ByteString -> Maybe Integer
decodeSingleUint bytes
  | BS.length bytes /= 32 = Nothing
  | otherwise = Just (decodeUint256 bytes)

computeEffectiveFeeBps :: Integer -> Integer -> Maybe Double
computeEffectiveFeeBps feeAssets volumeAssets
  | volumeAssets <= 0 || feeAssets < 0 = Nothing
  | otherwise =
      let bpsX100 = (feeAssets * 1000000 + volumeAssets `div` 2) `div` volumeAssets
       in Just (fromIntegral bpsX100 / 100.0)

computeAnnualizedRunRate7d :: Integer -> Maybe Integer
computeAnnualizedRunRate7d fee7dAssets
  | fee7dAssets <= 0 = Nothing
  | otherwise = Just ((fee7dAssets * 365) `div` 7)

buildDailyRevenueSeries
  :: Integer
  -> Integer
  -> Integer
  -> Map.Map Integer (Int, Integer, Integer)
  -> Map.Map Integer Integer
  -> Map.Map Integer Integer
  -> [DailyRevenuePoint]
buildDailyRevenueSeries startDayUnix endDayUnix priorCumulativeFees orderDayMap closeAssistMap gasDayMap
  | endDayUnix < startDayUnix = []
  | otherwise = go priorCumulativeFees [startDayUnix, startDayUnix + 86400 .. endDayUnix]
  where
    go _ [] = []
    go running (d : rest) =
      let (ordCnt, vol, fee) = Map.findWithDefault (0, 0, 0) d orderDayMap
          ca = Map.findWithDefault 0 d closeAssistMap
          gas = Map.findWithDefault 0 d gasDayMap
          nextCum = running + fee
          pt =
            DailyRevenuePoint
              { drpDayStartUnix = d
              , drpDayUtc = dayIso d
              , drpExecutedOrders = ordCnt
              , drpVolumeAssets = vol
              , drpExecutionFeeAssets = fee
              , drpCumulativeExecutionFeeAssets = nextCum
              , drpCloseAssistanceAssets = ca
              , drpSponsoredGasWei = gas
              }
       in pt : go nextCum rest

-- JSON serializers

pendingRotationToJson :: PendingTreasuryRotation -> Value
pendingRotationToJson PendingTreasuryRotation {..} =
  object
    [ "treasury" .= ptrTreasury
    , "activationTime" .= isoTimestamp ptrActivationTime
    , "lockedUntil" .= (if ptrLockedUntil > 0 then Just (isoTimestamp ptrLockedUntil) else (Nothing :: Maybe Text))
    ]

windowSummaryToJson :: TreasuryWindowSummary -> Value
windowSummaryToJson TreasuryWindowSummary {..} =
  let netUsdgSurplus = twsExecutionFeeAssessedAssets - twsCloseAssistanceUsdgAssets
      effFeeBps = computeEffectiveFeeBps twsExecutionFeeAssessedAssets twsVolumeAssets
   in object
        [ "executedOrders" .= twsExecutedOrders
        , "volumeAssets" .= T.pack (show twsVolumeAssets)
        , "executionFeeAssessedAssets" .= T.pack (show twsExecutionFeeAssessedAssets)
        , "executionFeeCreditedAssets" .= T.pack (show twsExecutionFeeCreditedAssets)
        , "effectiveFeeBps" .= effFeeBps
        , "frozenSpreadAssessedAssets" .= T.pack (show twsFrozenSpreadAssessedAssets)
        , "keeperBountyAssets" .= T.pack (show twsKeeperBountyAssets)
        , "liquidationCount" .= twsLiquidationCount
        , "liquidationKeeperBountyAssets" .= T.pack (show twsLiquidationKeeperBountyAssets)
        , "closeAssistanceCount" .= twsCloseAssistanceCount
        , "closeAssistanceUsdgAssets" .= T.pack (show twsCloseAssistanceUsdgAssets)
        , "sponsoredOpsCount" .= twsSponsoredOpsCount
        , "sponsoredGasSpentWei" .= T.pack (show twsSponsoredGasSpentWei)
        , "netUsdgSurplusAssets" .= T.pack (show netUsdgSurplus)
        ]

dailyPointToJson :: DailyRevenuePoint -> Value
dailyPointToJson DailyRevenuePoint {..} =
  object
    [ "dayStartUnix" .= drpDayStartUnix
    , "dayUtc" .= drpDayUtc
    , "executedOrders" .= drpExecutedOrders
    , "volumeAssets" .= T.pack (show drpVolumeAssets)
    , "executionFeeAssets" .= T.pack (show drpExecutionFeeAssets)
    , "cumulativeExecutionFeeAssets" .= T.pack (show drpCumulativeExecutionFeeAssets)
    , "closeAssistanceAssets" .= T.pack (show drpCloseAssistanceAssets)
    , "sponsoredGasWei" .= T.pack (show drpSponsoredGasWei)
    ]

topFeeOrderToJson :: TopFeeOrderRow -> Value
topFeeOrderToJson TopFeeOrderRow {..} =
  object
    [ "orderId" .= T.pack (show tforOrderId)
    , "txHash" .= T.toLower tforTxHash
    , "account" .= T.toLower tforAccount
    , "side" .= sideLabel tforSide
    , "volumeAssets" .= T.pack (show tforVolumeAssets)
    , "executionFeeAssets" .= T.pack (show tforExecutionFeeAssets)
    , "effectiveFeeBps" .= computeEffectiveFeeBps tforExecutionFeeAssets tforVolumeAssets
    , "blockNumber" .= T.pack (show tforBlockNumber)
    , "occurredAt" .= isoTimestamp tforTimestamp
    ]
  where
    sideLabel (Just 0) = ("Long" :: Text)
    sideLabel (Just 1) = ("Short" :: Text)
    sideLabel _ = ("Unknown" :: Text)

movementToJson :: TreasuryMovementRow -> Value
movementToJson TreasuryMovementRow {..} =
  object
    [ "kind" .= tmrKind
    , "direction" .= tmrDirection
    , "amountAssets" .= T.pack (show tmrAmountAssets)
    , "counterparty" .= tmrCounterparty
    , "txHash" .= tmrTxHash
    , "blockNumber" .= T.pack (show tmrBlockNumber)
    , "occurredAt" .= isoTimestamp tmrTimestamp
    ]

withReadSnapshot :: Connection -> IO a -> IO a
withReadSnapshot conn =
  withTransactionMode (TransactionMode RepeatableRead ReadOnly) conn

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

isoTimestamp :: Integer -> Text
isoTimestamp =
  T.pack
    . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
    . posixSecondsToUTCTime
    . fromIntegral

dayIso :: Integer -> Text
dayIso =
  T.pack
    . formatTime defaultTimeLocale "%Y-%m-%d"
    . posixSecondsToUTCTime
    . fromIntegral
