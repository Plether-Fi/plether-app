module Plether.Handlers.PerpsHistory
  ( getPerpsAccountOrders
  , getPerpsAccountActivity
  , getPerpsIndexerStatusResponse
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( PerpsActivityRow (..)
  , PerpsIndexerStatusRow (..)
  , PerpsOrderRow (..)
  , getPerpsActivityByAccount
  , getPerpsIndexerStatus
  , getPerpsOrdersByAccount
  )
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
  rows <- withDb pool $ \conn ->
    getPerpsOrdersByAccount conn (cfgChainId cfg) account pageLimit cursor
  pure $
    Right $
      mkResponse (latestOrderBlock rows) (cfgChainId cfg) $
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
  rows <- withDb pool $ \conn ->
    getPerpsActivityByAccount conn (cfgChainId cfg) account pageLimit cursor
  pure $
    Right $
      mkResponse (latestActivityBlock rows) (cfgChainId cfg) $
        object $
          catMaybes
            [ Just $ "activity" .= map activityRowToJson rows
            , ("nextCursor" .=) <$> nextActivityCursor pageLimit rows
            ]

getPerpsIndexerStatusResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getPerpsIndexerStatusResponse pool cfg = do
  mStatus <- withDb pool $ \conn ->
    getPerpsIndexerStatus conn (cfgChainId cfg) "perps-history"
  pure $ case mStatus of
    Nothing ->
      Left $ E.internalError "Perps history indexer has not written state yet. Start plether-perps-indexer --once or --loop."
    Just row ->
      Right $
        mkResponse (pisLastIndexedBlock row) (cfgChainId cfg) $
          indexerStatusToJson row

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
