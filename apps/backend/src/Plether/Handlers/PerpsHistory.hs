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
  , keeperTerminalIsCanonicallyRejected
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
import Plether.Perps.HistoryIndexer (orderFailReasonName, perpsIndexerName, terminalStatus)
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
    indexerStatus <- getPerpsIndexerStatus conn chainId perpsIndexerName orderRouter
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
    getPerpsActivityByAccount conn chainId orderRouter account pageLimit cursor
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
  volume24hUsdc <- withDb pool $ \conn ->
    getPerpsMarketVolumeSince conn chainId orderRouter fromTimestamp
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
    getPerpsIndexerStatus conn chainId perpsIndexerName (cfgPerpsOrderRouter cfg)
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
  -> IO (Either ApiError (ApiResponse Value))
waitForPerpsOrderTerminal pool cfg mRouter orderId mAccount timeoutSeconds = do
  let waitSeconds = min 60 $ max 1 timeoutSeconds
      account = T.toLower <$> mAccount
      chainId = cfgPerpsChainId cfg
      orderRouter = perpsHistoryRouter cfg mRouter
  (timedOut, mOrder) <- go orderRouter account waitSeconds
  pure $
    Right $
      mkResponse (maybe 0 wosBlock mOrder) chainId $
        object
          [ "timedOut" .= timedOut
          , "order" .= fmap wosJson mOrder
          ]
  where
    go :: Text -> Maybe Text -> Int -> IO (Bool, Maybe WaitOrderSnapshot)
    go orderRouter account remainingSeconds = do
      mOrder <- withDb pool $ \conn -> do
        mKeeperOrder <- getPerpsKeeperOrderById conn orderRouter orderId account
        mHistoryOrder <- getPerpsOrderById conn (cfgPerpsChainId cfg) orderRouter orderId account
        -- Read the monotonic cursor last. If the indexer advances between
        -- these statements, a stale Committed row can delay a keeper result
        -- for one polling iteration, but an old cursor can never resurrect a
        -- keeper terminal that canonical history has already disproved.
        mIndexerStatus <-
          getPerpsIndexerStatus
            conn
            (cfgPerpsChainId cfg)
            perpsIndexerName
            orderRouter
        let indexedThrough = pisLastIndexedBlock <$> mIndexerStatus
        pure $ case mHistoryOrder of
          Just historyOrder | isTerminalHistoryOrder historyOrder ->
            Just $ historyOrderSnapshot historyOrder
          _ ->
            case mKeeperOrder of
              Just keeperOrder
                | keeperTerminalIsCanonicallyRejected
                    (cfgPerpsIndexerStartBlock cfg)
                    indexedThrough
                    mHistoryOrder
                    keeperOrder ->
                    historyOrderSnapshot <$> mHistoryOrder
              _ ->
                case
                  mKeeperOrder
                    >>= keeperTerminalOrderSnapshot mHistoryOrder of
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
          go orderRouter account (remainingSeconds - 1)

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

keeperTerminalOrderSnapshot
  :: Maybe PerpsOrderRow
  -> PerpsKeeperTerminalOrderRow
  -> Maybe WaitOrderSnapshot
keeperTerminalOrderSnapshot mHistoryOrder row =
  case T.toLower $ pktoStatus row of
    "executed" -> Just $ keeperOrderSnapshot mHistoryOrder row
    "failed" -> Just $ keeperOrderSnapshot mHistoryOrder row
    _ -> Nothing

keeperTerminalIsCanonicallyRejected
  :: Integer
  -> Maybe Integer
  -> Maybe PerpsOrderRow
  -> PerpsKeeperTerminalOrderRow
  -> Bool
keeperTerminalIsCanonicallyRejected indexerStartBlock indexedThrough mHistoryOrder keeperOrder =
  case (indexedThrough, mHistoryOrder, keeperTerminalBlock keeperOrder) of
    (Just indexedBlock, Just historyOrder, Just keeperBlock) ->
      porTerminalStatus historyOrder == "Committed"
        && porOrderId historyOrder == pktoOrderId keeperOrder
        && normalizeAddress (porOrderRouter historyOrder)
          == normalizeAddress (pktoOrderRouter keeperOrder)
        && indexedBlock >= keeperBlock
    (Just indexedBlock, Nothing, Just keeperBlock) ->
      indexedBlock >= keeperBlock
        && keeperCommitCoverageBlock keeperOrder >= indexerStartBlock
    _ -> False
  where
    normalizeAddress = T.toLower . T.strip

keeperCommitCoverageBlock :: PerpsKeeperTerminalOrderRow -> Integer
keeperCommitCoverageBlock row =
  maybe (pktoCommitBlock row) id (pktoCommitEventBlock row)

keeperTerminalBlock :: PerpsKeeperTerminalOrderRow -> Maybe Integer
keeperTerminalBlock row =
  case T.toLower $ pktoStatus row of
    "executed" -> pktoExecutionBlock row
    "failed" -> pktoFailureBlock row
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
            , Just $ "orderRouter" .= pktoOrderRouter row
            , Just $ "account" .= pktoAccount row
            , Just $ "side" .= pktoSide row
            , Just $ "commitTxHash" .= pktoCommitTxHash row
            , Just $ "commitBlockNumber" .= show commitBlockNumber
            , Just $ "commitTimestamp" .= commitTimestamp
            , ("terminalTxHash" .=) <$> terminalTxHash
            , ("terminalBlockNumber" .=) . show <$> terminalBlock
            , ("terminalBlockHash" .=) <$> terminalHistoryField porTerminalBlockHash
            , ("terminalTimestamp" .=) <$> terminalHistoryField porTerminalTimestamp
            , Just $ "terminalStatus" .= status
            , ("failureReason" .=) <$> failureReason
            , ("executionPrice" .=) . show <$> pktoExecutionPrice row
            , ("vpiUsdc" .=) . show <$> terminalHistoryField porExecutionVpiUsdc
            , ("frozenCloseSpreadUsdc" .=) . show <$> terminalHistoryField porExecutionFrozenCloseSpreadUsdc
            , ("executionEconomicsVersion" .=) <$> terminalHistoryField porExecutionEconomicsVersion
            , ("executionOraclePrice" .=) . show <$> terminalHistoryField porExecutionOraclePrice
            , ("executionOracleFrozen" .=) <$> terminalHistoryField porExecutionOracleFrozen
            , ("oracleMinPublishTime" .=) . show <$> terminalHistoryField porOracleMinPublishTime
            , ("oracleMaxPublishTime" .=) . show <$> terminalHistoryField porOracleMaxPublishTime
            , ("oracleDerivationVersion" .=) <$> terminalHistoryField porOracleDerivationVersion
            , ("activityVpiUsdc" .=) . show <$> terminalHistoryField porActivityVpiUsdc
            ]
    }
  where
    keeperStatus = T.toLower $ pktoStatus row
    failureReason = orderFailReasonName <$> pktoFailureReason row
    historyField selector = mHistoryOrder >>= selector
    terminalHistoryField selector = matchingTerminalHistoryOrder >>= selector
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
      = keeperTerminalBlock row
    matchingTerminalHistoryOrder = do
      historyOrder <- mHistoryOrder
      keeperHash <- terminalTxHash
      historyHash <- porTerminalTxHash historyOrder
      keeperBlock <- terminalBlock
      historyBlock <- porTerminalBlockNumber historyOrder
      if T.toLower keeperHash == T.toLower historyHash
          && keeperBlock == historyBlock
          && porTerminalStatus historyOrder == status
        then Just historyOrder
        else Nothing

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
      , ("failureReason" .=) <$> porFailureReason
      , ("executionPrice" .=) . show <$> porExecutionPrice
      , ("vpiUsdc" .=) . show <$> porExecutionVpiUsdc
      , ("frozenCloseSpreadUsdc" .=) . show <$> porExecutionFrozenCloseSpreadUsdc
      , ("executionEconomicsVersion" .=) <$> porExecutionEconomicsVersion
      , ("executionOraclePrice" .=) . show <$> porExecutionOraclePrice
      , ("executionOracleFrozen" .=) <$> porExecutionOracleFrozen
      , ("oracleMinPublishTime" .=) . show <$> porOracleMinPublishTime
      , ("oracleMaxPublishTime" .=) . show <$> porOracleMaxPublishTime
      , ("oracleDerivationVersion" .=) <$> porOracleDerivationVersion
      , ("cleanupActor" .=) <$> porCleanupActor
      , ("activityType" .=) <$> porActivityType
      , ("activitySizeDelta" .=) . show <$> porActivitySizeDelta
      , ("activityPrice" .=) . show <$> porActivityPrice
      , ("activityVpiUsdc" .=) . show <$> porActivityVpiUsdc
      , ("activityPnlUsdc" .=) . show <$> porActivityPnlUsdc
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
