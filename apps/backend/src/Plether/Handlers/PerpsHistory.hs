module Plether.Handlers.PerpsHistory
  ( getPerpsAccountOrders
  , getPerpsAccountActivity
  , getPerpsMarketStatsResponse
  , perpsMarketStatsChainId
  , perpsHistoryRouter
  , getPerpsIndexerStatusResponse
  , waitForPerpsOrderTerminal
  , orderRowToJson
  , perpsOrdersIndexedThroughBlock
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, SomeAsyncException, try, fromException, throwIO)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTimeNSec)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.ExecutionProgress
  ( getExecutionProgress, executionProgressJson, executionProgressVersion
  , readExecutionConfirmations
  )
import Plether.Logging (logWarnEvery)
import System.Timeout (timeout)
import Plether.Database.Schema
  ( PerpsActivityRow (..)
  , PerpsIndexerStatusRow (..)
  , PerpsOrderRow (..)
  , executionModeOracleFrozen
  , getPerpsActivityByAccount
  , getPerpsIndexerStatus
  , getPerpsMarketVolumeSince
  , getPerpsOrderById
  , getPerpsOrdersByAccount
  )
import Plether.Perps.HistoryIndexer (perpsIndexerNameForLifecycleBook)
import Plether.Types (ApiError, ApiResponse, mkResponse)
import qualified Plether.Types.Error as E

getPerpsAccountOrders
  :: DbPool
  -> Config
  -> Maybe Text
  -> Text
  -> Int
  -> Maybe (Integer, Integer)
  -> IO (Either ApiError (ApiResponse Value))
getPerpsAccountOrders pool cfg mRouter account limit cursor = do
  let pageLimit = clampLimit limit
      chainId = cfgPerpsChainId cfg
      orderRouter = perpsHistoryRouter cfg mRouter
  (mIndexerStatus, rows) <- withDb pool $ \conn -> do
    indexerStatus <-
      getPerpsIndexerStatus
        conn
        chainId
        (perpsIndexerNameForLifecycleBook $ cfgPerpsOrderLifecycleBook cfg)
        orderRouter
    orderRows <- getPerpsOrdersByAccount conn chainId orderRouter account pageLimit cursor
    pure (indexerStatus, orderRows)
  pure $
    Right $
      mkResponse (latestOrderBlock rows) chainId $
        object $
          catMaybes
            [ Just $ "orders" .= map orderRowToJson rows
            , ("indexedThroughBlock" .=) . show
                <$> perpsOrdersIndexedThroughBlock mIndexerStatus
            , ("nextCursor" .=) <$> nextOrderCursor pageLimit rows
            ]

getPerpsAccountActivity
  :: DbPool
  -> Config
  -> Maybe Text
  -> Text
  -> Int
  -> Maybe (Integer, Integer)
  -> IO (Either ApiError (ApiResponse Value))
getPerpsAccountActivity pool cfg mRouter account limit cursor = do
  let pageLimit = clampLimit limit
      chainId = cfgPerpsChainId cfg
      orderRouter = perpsHistoryRouter cfg mRouter
  rows <- withDb pool $ \conn ->
    getPerpsActivityByAccount
      conn
      chainId
      orderRouter
      account
      (cfgPerpsIndexerStartBlock cfg)
      pageLimit
      cursor
  pure $
    Right $
      mkResponse (latestActivityBlock rows) chainId $
        object $
          catMaybes
            [ Just $ "activity" .= map activityRowToJson rows
            , ("nextCursor" .=) <$> nextActivityCursor pageLimit rows
            ]

getPerpsMarketStatsResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getPerpsMarketStatsResponse pool cfg = do
  now <- round <$> getPOSIXTime
  let rangeSeconds = 24 * 60 * 60
      fromTimestamp = now - rangeSeconds
      chainId = perpsMarketStatsChainId cfg
      orderRouter = cfgPerpsOrderRouter cfg
  volume24hAssets <- withDb pool $ \conn ->
    getPerpsMarketVolumeSince conn chainId orderRouter fromTimestamp
  pure $
    Right $
      mkResponse 0 chainId $
        object
          [ "rangeSeconds" .= rangeSeconds
          , "generatedAt" .= now
          , "volume24hAssets" .= show volume24hAssets
          ]

perpsMarketStatsChainId :: Config -> Integer
perpsMarketStatsChainId = cfgPerpsChainId

getPerpsIndexerStatusResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getPerpsIndexerStatusResponse pool cfg = do
  let chainId = cfgPerpsChainId cfg
  mStatus <- withDb pool $ \conn ->
    getPerpsIndexerStatus
      conn
      chainId
      (perpsIndexerNameForLifecycleBook $ cfgPerpsOrderLifecycleBook cfg)
      (cfgPerpsOrderRouter cfg)
  pure $ case mStatus of
    Nothing ->
      Left $ E.internalError "Perps history indexer has not written state yet. Start plether-perps-indexer --once or --loop."
    Just row ->
      Right $
        mkResponse (pisLastIndexedBlock row) chainId $
          indexerStatusToJson row

waitForPerpsOrderTerminal
  :: DbPool
  -> Config
  -> Maybe Text
  -> Integer
  -> Maybe Text
  -> Int
  -> Maybe Text
  -> IO (Either ApiError (ApiResponse Value))
waitForPerpsOrderTerminal pool cfg mRouter orderId mAccount timeoutSeconds afterExecution = do
  let waitSeconds = min 60 $ max 1 timeoutSeconds
      account = T.toLower <$> mAccount
      chainId = cfgPerpsChainId cfg
      orderRouter = perpsHistoryRouter cfg mRouter
  confirmations <- readExecutionConfirmations cfg
  started <- getMonotonicTimeNSec
  let deadline = started + fromIntegral waitSeconds * 1_000_000_000
      -- Progress is only a transaction locator. An unchanged cursor holds the
      -- long poll, while replacement/clearing is observable without busy loops.
      go nextHistory cached = do
        now <- getMonotonicTimeNSec
        progress <- case confirmations of
          Nothing -> pure Nothing
          Just _ -> do
            result <- try $ timeout 250_000 $ withDb pool $ \conn ->
              getExecutionProgress conn chainId orderRouter orderId account
            case result of
              Right (Just value) -> pure value
              Left (err :: SomeException)
                | Just cancellation <- (fromException err :: Maybe SomeAsyncException) -> throwIO cancellation
              _ -> do
                logWarnEvery 60 "execution_progress_read_unavailable"
                  "Optional execution locator unavailable; canonical history continues" []
                pure Nothing
        let version = executionProgressVersion <$> progress
            changed = case (afterExecution, version) of
              (Nothing, Just value) -> value /= "none"
              (Just cursor, Just value) -> T.toLower cursor /= value
              _ -> False
            refresh = now >= nextHistory || changed
        mOrder <-
          if refresh
            then withDb pool $ \conn -> fmap historyOrderSnapshot <$>
              getPerpsOrderById conn chainId orderRouter orderId account
            else pure cached
        let next = if refresh then now + 1_000_000_000 else nextHistory
        finished <- getMonotonicTimeNSec
        if maybe False ((/= "Committed") . wosTerminalStatus) mOrder
          then pure (False, mOrder, progress)
          else if changed then pure (False, mOrder, progress)
          else if finished >= deadline then pure (True, mOrder, progress)
          else threadDelay 250_000 >> go next mOrder
  (timedOut, mOrder, progress) <- go 0 Nothing
  pure $
    Right $
      mkResponse (maybe 0 wosBlock mOrder) chainId $
        object $
          [ "timedOut" .= timedOut
          , "order" .= fmap wosJson mOrder
          ] <> case (confirmations, progress) of
            (Just required, Just known) ->
              [ "execution" .= fmap (executionProgressJson required) known
              , "executionVersion" .= executionProgressVersion known
              ]
            _ -> []

data WaitOrderSnapshot = WaitOrderSnapshot
  { wosBlock :: Integer
  , wosTerminalStatus :: Text
  , wosJson :: Value
  }

historyOrderSnapshot :: PerpsOrderRow -> WaitOrderSnapshot
historyOrderSnapshot row =
  WaitOrderSnapshot
    { wosBlock = porSortBlock row
    , wosTerminalStatus = porTerminalStatus row
    , wosJson = orderRowToJson row
    }

orderRowToJson :: PerpsOrderRow -> Value
orderRowToJson PerpsOrderRow {..} =
  object $
    catMaybes
      [ Just $ "orderId" .= show porOrderId
      , Just $ "orderRouter" .= porOrderRouter
      , ("account" .=) <$> porAccount
      , ("side" .=) <$> porSide
      , ("commitTxHash" .=) <$> porCommitTxHash
      , ("commitBlockNumber" .=) . show <$> porCommitBlockNumber
      , ("commitTimestamp" .=) <$> porCommitTimestamp
      , ("terminalTxHash" .=) <$> porTerminalTxHash
      , ("terminalBlockNumber" .=) . show <$> porTerminalBlockNumber
      , ("terminalBlockHash" .=) <$> porTerminalBlockHash
      , ("terminalTimestamp" .=) <$> porTerminalTimestamp
      , Just $ "terminalStatus" .= porTerminalStatus
      , ("executionPrice" .=) . show <$> porExecutionPrice
      , ("vpiAssets" .=) . show <$> porExecutionVpiAssets
      , ("frozenCloseSpreadAssets" .=) . show <$> porExecutionFrozenCloseSpreadAssets
      , ("executionEconomicsVersion" .=) <$> porExecutionEconomicsVersion
      , ("executionOraclePrice" .=) . show <$> porExecutionOraclePrice
      , ("executionOracleFrozen" .=) <$>
          (porExecutionOracleFrozen <|> (porExecutionMode >>= executionModeOracleFrozen))
      , ("oracleMinPublishTime" .=) . show <$> porOracleMinPublishTime
      , ("oracleMaxPublishTime" .=) . show <$> porOracleMaxPublishTime
      , ("oracleDerivationVersion" .=) <$> porOracleDerivationVersion
      , ("clientOrderId" .=) <$> porClientOrderId
      , ("receiptHash" .=) <$> porReceiptHash
      , ("terminalReason" .=) <$> porTerminalReason
      , ("pendingReason" .=) <$> porPendingReason
      , ("executionMode" .=) <$> porExecutionMode
      , ("failedConstraint" .=) <$> porFailedConstraint
      , ("receiptEconomics" .=) <$> porReceiptEconomics
      , ("cleanupActor" .=) <$> porCleanupActor
      , ("activityType" .=) <$> porActivityType
      , ("activitySizeDelta" .=) . show <$> porActivitySizeDelta
      , ("activityPrice" .=) . show <$> porActivityPrice
      , ("activityVpiAssets" .=) . show <$> porActivityVpiAssets
      , ("activityPnlAssets" .=) . show <$> porActivityPnlAssets
      ]

activityRowToJson :: PerpsActivityRow -> Value
activityRowToJson PerpsActivityRow {..} =
  object $
    catMaybes
      [ Just $ "activityType" .= parActivityType
      , Just $ "orderRouter" .= parOrderRouter
      , ("contractAddress" .=) <$> parContractAddress
      , Just $ "account" .= parAccount
      , ("actor" .=) <$> parActor
      , ("orderId" .=) . show <$> parOrderId
      , ("side" .=) <$> parSide
      , ("price" .=) . show <$> parPrice
      , ("sizeDelta" .=) . show <$> parSizeDelta
      , ("amountAssets" .=) . show <$> parAmountAssets
      , ("pnlAssets" .=) . show <$> parPnlAssets
      , Just $ "txHash" .= parTxHash
      , Just $ "blockNumber" .= show parBlockNumber
      , Just $ "timestamp" .= parTimestamp
      , Just $ "data" .= parData
      ]

indexerStatusToJson :: PerpsIndexerStatusRow -> Value
indexerStatusToJson PerpsIndexerStatusRow {..} =
  object $
    catMaybes
      [ Just $ "indexerName" .= pisIndexerName
      , Just $ "chainId" .= show pisChainId
      , Just $ "releaseRouter" .= pisReleaseRouter
      , Just $ "lastIndexedBlock" .= show pisLastIndexedBlock
      , ("lastIndexedBlockHash" .=) <$> pisLastIndexedBlockHash
      ]

latestOrderBlock :: [PerpsOrderRow] -> Integer
latestOrderBlock rows =
  maximum (0 : map porSortBlock rows)

perpsOrdersIndexedThroughBlock :: Maybe PerpsIndexerStatusRow -> Maybe Integer
perpsOrdersIndexedThroughBlock = fmap pisLastIndexedBlock

latestActivityBlock :: [PerpsActivityRow] -> Integer
latestActivityBlock rows =
  maximum (0 : map parBlockNumber rows)

nextOrderCursor :: Int -> [PerpsOrderRow] -> Maybe Text
nextOrderCursor pageLimit rows
  | length rows < pageLimit = Nothing
  | otherwise = do
      row <- safeLast rows
      pure $ T.pack (show $ porSortBlock row) <> ":" <> T.pack (show $ porOrderId row)

nextActivityCursor :: Int -> [PerpsActivityRow] -> Maybe Text
nextActivityCursor pageLimit rows
  | length rows < pageLimit = Nothing
  | otherwise = do
      row <- safeLast rows
      pure $ T.pack (show $ parBlockNumber row) <> ":" <> T.pack (show $ parLogIndex row)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

clampLimit :: Int -> Int
clampLimit = min 100 . max 1

perpsHistoryRouter :: Config -> Maybe Text -> Text
perpsHistoryRouter cfg =
  maybe (normalizeAddress $ cfgPerpsOrderRouter cfg) normalizeAddress
  where
    normalizeAddress = T.toLower . T.strip
