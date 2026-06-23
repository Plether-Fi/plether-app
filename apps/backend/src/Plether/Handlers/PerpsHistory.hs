module Plether.Handlers.PerpsHistory
  ( getPerpsAccountOrders
  , getPerpsAccountActivity
  , getPerpsMarketStatsResponse
  , perpsMarketStatsChainId
  , getPerpsIndexerStatusResponse
  , waitForPerpsOrderTerminal
  ) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( PerpsActivityRow (..)
  , PerpsIndexerStatusRow (..)
  , PerpsKeeperTerminalOrderRow (..)
  , PerpsOrderRow (..)
  , getPerpsActivityByAccount
  , getPerpsIndexerStatus
  , getPerpsKeeperOrderById
  , getPerpsMarketVolumeSince
  , getPerpsOrderById
  , getPerpsOrdersByAccount
  )
import Plether.Perps.HistoryIndexer (orderFailReasonName, terminalStatus)
import Plether.Types (ApiError, ApiResponse, mkResponse)
import qualified Plether.Types.Error as E

getPerpsAccountOrders
  :: DbPool
  -> Config
  -> Text
  -> Int
  -> Maybe (Integer, Integer)
  -> IO (Either ApiError (ApiResponse Value))
getPerpsAccountOrders pool cfg account limit cursor = do
  let pageLimit = clampLimit limit
      chainId = cfgPerpsChainId cfg
  rows <- withDb pool $ \conn ->
    getPerpsOrdersByAccount conn chainId account pageLimit cursor
  pure $
    Right $
      mkResponse (latestOrderBlock rows) chainId $
        object $
          catMaybes
            [ Just $ "orders" .= map orderRowToJson rows
            , ("nextCursor" .=) <$> nextOrderCursor pageLimit rows
            ]

getPerpsAccountActivity
  :: DbPool
  -> Config
  -> Text
  -> Int
  -> Maybe (Integer, Integer)
  -> IO (Either ApiError (ApiResponse Value))
getPerpsAccountActivity pool cfg account limit cursor = do
  let pageLimit = clampLimit limit
      chainId = cfgPerpsChainId cfg
  rows <- withDb pool $ \conn ->
    getPerpsActivityByAccount conn chainId account pageLimit cursor
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
  volume24hUsdc <- withDb pool $ \conn ->
    getPerpsMarketVolumeSince conn chainId fromTimestamp
  pure $
    Right $
      mkResponse 0 chainId $
        object
          [ "rangeSeconds" .= rangeSeconds
          , "generatedAt" .= now
          , "volume24hUsdc" .= show volume24hUsdc
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
    getPerpsIndexerStatus conn chainId "perps-history"
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
  -> Integer
  -> Maybe Text
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
waitForPerpsOrderTerminal pool cfg orderId mAccount timeoutSeconds = do
  let waitSeconds = min 60 $ max 1 timeoutSeconds
      account = T.toLower <$> mAccount
      chainId = cfgPerpsChainId cfg
  (timedOut, mOrder) <- go account waitSeconds
  pure $
    Right $
      mkResponse (maybe 0 wosBlock mOrder) chainId $
        object
          [ "timedOut" .= timedOut
          , "order" .= fmap wosJson mOrder
          ]
  where
    go :: Maybe Text -> Int -> IO (Bool, Maybe WaitOrderSnapshot)
    go account remainingSeconds = do
      mOrder <- withDb pool $ \conn -> do
        mKeeperOrder <- getPerpsKeeperOrderById conn orderId account
        mHistoryOrder <- getPerpsOrderById conn (cfgPerpsChainId cfg) orderId account
        pure $ case mHistoryOrder of
          Just historyOrder | isTerminalHistoryOrder historyOrder ->
            Just $ historyOrderSnapshot historyOrder
          _ ->
            case mKeeperOrder >>= keeperTerminalOrderSnapshot mHistoryOrder of
              Just terminalOrder ->
                Just terminalOrder
              Nothing ->
                case mHistoryOrder of
                  Just historyOrder ->
                    Just $ historyOrderSnapshot historyOrder
                  Nothing ->
                    keeperOrderSnapshot Nothing <$> mKeeperOrder
      case mOrder of
        Just row | isTerminalOrder row ->
          pure (False, Just row)
        _ | remainingSeconds <= 0 ->
          pure (True, mOrder)
        _ -> do
          threadDelay 1_000_000
          go account (remainingSeconds - 1)

    isTerminalOrder :: WaitOrderSnapshot -> Bool
    isTerminalOrder row = wosTerminalStatus row /= "Committed"

    isTerminalHistoryOrder :: PerpsOrderRow -> Bool
    isTerminalHistoryOrder row = porTerminalStatus row /= "Committed"

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

keeperTerminalOrderSnapshot :: Maybe PerpsOrderRow -> PerpsKeeperTerminalOrderRow -> Maybe WaitOrderSnapshot
keeperTerminalOrderSnapshot mHistoryOrder row =
  case T.toLower $ pktoStatus row of
    "executed" -> Just $ keeperOrderSnapshot mHistoryOrder row
    "failed" -> Just $ keeperOrderSnapshot mHistoryOrder row
    _ -> Nothing

keeperOrderSnapshot :: Maybe PerpsOrderRow -> PerpsKeeperTerminalOrderRow -> WaitOrderSnapshot
keeperOrderSnapshot mHistoryOrder row =
  WaitOrderSnapshot
    { wosBlock = maybe commitBlockNumber id terminalBlock
    , wosTerminalStatus = status
    , wosJson =
        object $
          catMaybes
            [ Just $ "orderId" .= show (pktoOrderId row)
            , Just $ "account" .= pktoAccount row
            , Just $ "side" .= pktoSide row
            , Just $ "commitTxHash" .= pktoCommitTxHash row
            , Just $ "commitBlockNumber" .= show commitBlockNumber
            , Just $ "commitTimestamp" .= commitTimestamp
            , ("terminalTxHash" .=) <$> terminalTxHash
            , ("terminalBlockNumber" .=) . show <$> terminalBlock
            , ("terminalTimestamp" .=) <$> historyField porTerminalTimestamp
            , Just $ "terminalStatus" .= status
            , ("failureReason" .=) <$> failureReason
            , ("executionPrice" .=) . show <$> pktoExecutionPrice row
            ]
    }
  where
    keeperStatus = T.toLower $ pktoStatus row
    failureReason = orderFailReasonName <$> pktoFailureReason row
    historyField selector = mHistoryOrder >>= selector
    commitBlockNumber = maybe (maybe (pktoCommitBlock row) id (pktoCommitEventBlock row)) id (historyField porCommitBlockNumber)
    commitTimestamp = maybe (pktoCommitTime row) id (historyField porCommitTimestamp)
    status
      | keeperStatus == "executed" = "Executed"
      | keeperStatus == "failed" = terminalStatus $ maybe "Unknown" id failureReason
      | otherwise = "Committed"
    terminalTxHash
      | keeperStatus == "executed" = pktoExecutionTxHash row
      | keeperStatus == "failed" = pktoFailureTxHash row
      | otherwise = Nothing
    terminalBlock
      | keeperStatus == "executed" = pktoExecutionBlock row
      | keeperStatus == "failed" = pktoFailureBlock row
      | otherwise = Nothing

orderRowToJson :: PerpsOrderRow -> Value
orderRowToJson PerpsOrderRow {..} =
  object $
    catMaybes
      [ Just $ "orderId" .= show porOrderId
      , ("account" .=) <$> porAccount
      , ("side" .=) <$> porSide
      , ("commitTxHash" .=) <$> porCommitTxHash
      , ("commitBlockNumber" .=) . show <$> porCommitBlockNumber
      , ("commitTimestamp" .=) <$> porCommitTimestamp
      , ("terminalTxHash" .=) <$> porTerminalTxHash
      , ("terminalBlockNumber" .=) . show <$> porTerminalBlockNumber
      , ("terminalTimestamp" .=) <$> porTerminalTimestamp
      , Just $ "terminalStatus" .= porTerminalStatus
      , ("failureReason" .=) <$> porFailureReason
      , ("executionPrice" .=) . show <$> porExecutionPrice
      , ("cleanupActor" .=) <$> porCleanupActor
      , ("activityType" .=) <$> porActivityType
      , ("activitySizeDelta" .=) . show <$> porActivitySizeDelta
      , ("activityPrice" .=) . show <$> porActivityPrice
      , ("activityPnlUsdc" .=) . show <$> porActivityPnlUsdc
      ]

activityRowToJson :: PerpsActivityRow -> Value
activityRowToJson PerpsActivityRow {..} =
  object $
    catMaybes
      [ Just $ "activityType" .= parActivityType
      , Just $ "account" .= parAccount
      , ("actor" .=) <$> parActor
      , ("orderId" .=) . show <$> parOrderId
      , ("side" .=) <$> parSide
      , ("price" .=) . show <$> parPrice
      , ("sizeDelta" .=) . show <$> parSizeDelta
      , ("amountUsdc" .=) . show <$> parAmountUsdc
      , ("pnlUsdc" .=) . show <$> parPnlUsdc
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
      , Just $ "lastIndexedBlock" .= show pisLastIndexedBlock
      , ("lastIndexedBlockHash" .=) <$> pisLastIndexedBlockHash
      ]

latestOrderBlock :: [PerpsOrderRow] -> Integer
latestOrderBlock rows =
  maximum (0 : map porSortBlock rows)

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
