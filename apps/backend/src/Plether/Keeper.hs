module Plether.Keeper
  ( KeeperMode (..)
  , runKeeper
  , isOrderExpired
  , isOrderRevealReady
  , isFrozenClosePayloadReady
  , isSameBlockMevGuardError
  , selectBatchCandidates
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, displayException, try)
import Control.Monad (foldM, forM_, unless, void, when)
import Data.Aeson (FromJSON, Result (..), Value, fromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( PerpsKeeperOrderRow (..)
  , PythUpdatePayloadRow (..)
  , getPendingPerpsKeeperOrders
  , getPerpsKeeperLastIndexedBlock
  , getLatestPythUpdatePayload
  , getPythUpdatePayloadForWindow
  , isHistoricalRevealPayload
  , markPerpsKeeperOrderExecuted
  , markPerpsKeeperOrderFailed
  , recordPerpsKeeperOrderAttempt
  , recordPerpsKeeperOrderError
  , recordPerpsKeeperOrderImmediateRetryError
  , setPerpsKeeperLastIndexedBlock
  , tryPerpsKeeperLock
  , unlockPerpsKeeperLock
  , upsertPerpsKeeperOrderCommitted
  )
import Plether.Ethereum.Client (EthClient, RpcError (..), ethBlockNumber)
import qualified Plether.Ethereum.Contracts.Perps as Perps
import Plether.Ethereum.Rpc
  ( TxReceipt (..)
  , ethBlockTimestamp
  , ethEstimateGas
  , ethGasPrice
  , ethGetLogs
  , ethGetTransactionCount
  , ethGetTransactionReceipt
  , ethLatestBlockTimestamp
  , ethMaxPriorityFeePerGas
  , ethSendRawTransaction
  )
import Plether.Ethereum.Transaction
  ( SignedTransaction (..)
  , Tx1559 (..)
  , deriveAddress
  , signTransaction
  )
import Plether.Pyth.RevealPayload (validateRevealWindow)

data KeeperMode
  = KeeperLoop
  | KeeperOnce
  deriving stock (Show, Eq)

data ExecutionIntent
  = CleanupExpired PerpsKeeperOrderRow
  | ExecuteReady [PerpsKeeperOrderRow] PythUpdatePayloadRow [Integer] [ByteString]

data FreshPendingOrder = FreshPendingOrder
  { fpoOrder :: PerpsKeeperOrderRow
  , fpoIsClose :: Bool
  }
  deriving stock (Show)

runKeeper :: Config -> DbPool -> EthClient -> KeeperMode -> Bool -> IO ()
runKeeper cfg pool client mode dryRun =
  withDb pool $ \conn ->
    bracket
      (tryPerpsKeeperLock conn)
      (\acquired -> when acquired $ unlockPerpsKeeperLock conn)
      $ \acquired ->
        if not acquired
          then putStrLn "Another plether-keeper instance already holds the advisory lock"
          else do
            putStrLn "plether-keeper acquired advisory lock"
            case mode of
              KeeperOnce -> void $ runKeeperIteration cfg conn client dryRun
              KeeperLoop -> loop conn
  where
    loop conn = do
      continue <- runKeeperIteration cfg conn client dryRun
      when continue $ do
        threadDelay (cfgKeeperPollSeconds cfg * 1_000_000)
        loop conn

runKeeperIteration :: Config -> Connection -> EthClient -> Bool -> IO Bool
runKeeperIteration cfg conn client dryRun = do
  result <- try $ do
    indexNewLogs cfg conn client
    processQueueHead cfg conn client dryRun
  case result of
    Left (err :: SomeException) -> do
      putStrLn $ "keeper iteration failed: " <> displayException err
      pure True
    Right () -> pure True

indexNewLogs :: Config -> Connection -> EthClient -> IO ()
indexNewLogs cfg conn client = do
  latestResult <- ethBlockNumber client
  case latestResult of
    Left err -> putStrLn $ "perps log indexing skipped: " <> T.unpack (rpcErrorText err)
    Right latestBlock -> do
      lastIndexed <- getPerpsKeeperLastIndexedBlock conn
      let confirmedLatest = max 0 $ latestBlock - fromIntegral (cfgKeeperConfirmations cfg)
          startBlock = max (cfgPerpsIndexerStartBlock cfg) (lastIndexed + 1)
          endBlock = min confirmedLatest (startBlock + 1_999)
      if startBlock > confirmedLatest
        then pure ()
        else do
          logsResult <-
            ethGetLogs
              client
              (cfgPerpsOrderRouter cfg)
              Perps.perpsOrderTopics
              startBlock
              endBlock
          case logsResult of
            Left err -> putStrLn $ "perps log indexing failed: " <> T.unpack (rpcErrorText err)
            Right logs -> do
              forM_ (mapMaybe Perps.decodePerpsOrderEvent logs) (applyOrderEvent cfg conn client)
              setPerpsKeeperLastIndexedBlock conn endBlock
              unless (null logs) $
                putStrLn $
                  "indexed "
                    <> show (length logs)
                    <> " perps order logs through block "
                    <> show endBlock

applyOrderEvent :: Config -> Connection -> EthClient -> Perps.PerpsOrderEvent -> IO ()
applyOrderEvent cfg conn client = \case
  Perps.OrderCommitted {..} -> do
    metadataResult <- readCommitMetadata cfg client poeOrderId poeBlockNumber
    case metadataResult of
      Nothing -> pure ()
      Just (commitBlock, commitTime) ->
        upsertPerpsKeeperOrderCommitted
          conn
          poeOrderId
          poeAccount
          poeSide
          commitBlock
          poeBlockNumber
          commitTime
          poeTxHash
  Perps.OrderExecuted {..} ->
    markPerpsKeeperOrderExecuted conn poeOrderId poeTxHash poeBlockNumber poeExecutionPrice
  Perps.OrderFailed {..} ->
    markPerpsKeeperOrderFailed conn poeOrderId poeTxHash poeBlockNumber poeFailureReason

readCommitMetadata :: Config -> EthClient -> Integer -> Integer -> IO (Maybe (Integer, Integer))
readCommitMetadata cfg client orderId fallbackBlock = do
  viewResult <- Perps.getPendingOrderView client (cfgPerpsOrderRouter cfg) orderId
  case viewResult of
    Right view | Perps.povOrderId view == orderId ->
      pure $ Just (Perps.povCommitBlock view, Perps.povCommitTime view)
    _ -> do
      timestampResult <- ethBlockTimestamp client fallbackBlock
      case timestampResult of
        Left err -> do
          putStrLn $
            "could not fetch commit metadata for order "
              <> show orderId
              <> ": "
              <> T.unpack (rpcErrorText err)
          pure Nothing
        Right commitTime -> pure $ Just (fallbackBlock, commitTime)

processQueueHead :: Config -> Connection -> EthClient -> Bool -> IO ()
processQueueHead cfg conn client dryRun = do
  pending <- getPendingPerpsKeeperOrders conn (cfgKeeperMaxBatchSize cfg)
  case pending of
    [] -> pure ()
    headOrder : _ -> do
      maxAgeResult <- Perps.maxOrderAge client (cfgPerpsOrderRouter cfg)
      settlementWindowResult <- Perps.orderSettlementWindow client (cfgPerpsPletherOracle cfg)
      chainNowResult <- ethLatestBlockTimestamp client
      latestBlockResult <- ethBlockNumber client
      case (maxAgeResult, settlementWindowResult, chainNowResult, latestBlockResult) of
        (Right maxAge, Right settlementWindow, Right chainNow, Right latestBlock) ->
          decideExecution cfg conn client dryRun pending headOrder maxAge settlementWindow chainNow latestBlock
        _ -> do
          let errors =
                [ either (Just . rpcErrorText) (const Nothing) maxAgeResult
                , either (Just . rpcErrorText) (const Nothing) settlementWindowResult
                , either (Just . rpcErrorText) (const Nothing) chainNowResult
                , either (Just . rpcErrorText) (const Nothing) latestBlockResult
                ]
          putStrLn $
            "queue processing skipped: "
              <> T.unpack (T.intercalate "; " $ catMaybes errors)

decideExecution
  :: Config
  -> Connection
  -> EthClient
  -> Bool
  -> [PerpsKeeperOrderRow]
  -> PerpsKeeperOrderRow
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
decideExecution cfg conn client dryRun pending headOrder maxAge settlementWindow chainNow latestBlock = do
  freshHeadResult <- refreshPendingOrder cfg client headOrder
  case freshHeadResult of
    Left err -> do
      recordPerpsKeeperOrderError conn (pkorOrderId headOrder) err
      putStrLn $ "queue head re-read skipped execution: " <> T.unpack err
    Right FreshPendingOrder {fpoOrder = freshHead, fpoIsClose = freshHeadIsClose}
      | not (isPastCommitBlock latestBlock freshHead) ->
          putStrLn $
            "queue head order "
              <> show (pkorOrderId freshHead)
              <> " is waiting for the post-commit block"
      | isOrderExpired chainNow maxAge freshHead ->
          submitIntent cfg conn client dryRun $ CleanupExpired freshHead
      | chainNow < pkorCommitTime freshHead + 1 ->
          putStrLn $
            "queue head order "
              <> show (pkorOrderId freshHead)
              <> " is waiting for reveal window"
      | otherwise ->
          executeReadyHead (freshHead : drop 1 pending) freshHead freshHeadIsClose
  where
    executeReadyHead pendingWithFreshHead freshHead freshHeadIsClose = do
      frozenCloseResult <- tryFrozenClosePayload freshHead freshHeadIsClose
      case frozenCloseResult of
        Left err -> do
          recordPerpsKeeperOrderError conn (pkorOrderId freshHead) err
          putStrLn $
            "frozen close payload selection failed for order "
              <> show (pkorOrderId freshHead)
              <> ": "
              <> T.unpack err
        Right (Just (payload, publishTimes, updateData)) ->
          submitIntent cfg conn client dryRun $
            ExecuteReady [freshHead] payload publishTimes updateData
        Right Nothing ->
          executeHistoricalReadyHead pendingWithFreshHead freshHead

    executeHistoricalReadyHead pendingWithFreshHead freshHead = do
      mPayload <-
        getPythUpdatePayloadForWindow
          conn
          (pkorCommitTime freshHead + 1)
          (pkorCommitTime freshHead + settlementWindow)
      case mPayload of
        Nothing ->
          putStrLn $
            "queue head order "
              <> show (pkorOrderId freshHead)
              <> " is waiting for first post-commit cached Pyth payload"
        Just payload
          | not (isHistoricalRevealPayload payload) ->
              putStrLn $
                "queue head order "
                  <> show (pkorOrderId freshHead)
                  <> " is waiting for exact historical Pyth payload; cached source was "
                  <> T.unpack (puprSource payload)
        Just payload ->
          case decodePayload payload of
            Left err -> do
              recordPerpsKeeperOrderError conn (pkorOrderId freshHead) err
              putStrLn $ "cached Pyth payload could not be decoded: " <> T.unpack err
            Right (publishTimes, updateData) ->
              case validateRevealWindow (pkorCommitTime freshHead) settlementWindow publishTimes of
                Left err -> do
                  putStrLn $
                    "cached Pyth payload is not valid for order "
                      <> show (pkorOrderId freshHead)
                      <> ": "
                      <> T.unpack err
                Right _ -> do
                  let candidates =
                        selectBatchCandidates
                          chainNow
                          latestBlock
                          maxAge
                          settlementWindow
                          publishTimes
                          (cfgKeeperMaxBatchSize cfg)
                          pendingWithFreshHead
                  refreshed <- refreshContiguousOrders cfg client candidates
                  let selected =
                        selectBatchCandidates
                          chainNow
                          latestBlock
                          maxAge
                          settlementWindow
                          publishTimes
                          (cfgKeeperMaxBatchSize cfg)
                          refreshed
                  case selected of
                    [] ->
                      putStrLn $
                        "cached Pyth payload is not the first post-commit payload for queue head order "
                          <> show (pkorOrderId freshHead)
                    orders ->
                      submitIntent cfg conn client dryRun $
                        ExecuteReady orders payload publishTimes updateData

    tryFrozenClosePayload freshHead freshHeadIsClose
      | not freshHeadIsClose = pure $ Right Nothing
      | otherwise = do
          policyResult <- Perps.getOrderExecutionPolicy client (cfgPerpsPletherOracle cfg) True
          divergenceResult <- Perps.orderExecutionStalenessLimit client (cfgPerpsPletherOracle cfg)
          case (policyResult, divergenceResult) of
            (Right policy, Right maxDivergence)
              | not (Perps.oepOracleFrozen policy) -> pure $ Right Nothing
              | otherwise -> do
                  mPayload <- getLatestPythUpdatePayload conn
                  case mPayload of
                    Nothing -> do
                      putStrLn $
                        "queue head close order "
                          <> show (pkorOrderId freshHead)
                          <> " is waiting for latest cached Pyth payload"
                      pure $ Right Nothing
                    Just payload ->
                      case decodePayload payload of
                        Left err -> pure $ Left err
                        Right (publishTimes, updateData)
                          | isFrozenClosePayloadReady chainNow (Perps.oepMaxStaleness policy) maxDivergence publishTimes ->
                              pure $ Right $ Just (payload, publishTimes, updateData)
                          | otherwise -> do
                              putStrLn $
                                "queue head close order "
                                  <> show (pkorOrderId freshHead)
                                  <> " is waiting for frozen-policy Pyth payload"
                              pure $ Right Nothing
            _ ->
              pure $
                Left $
                  T.intercalate
                    "; "
                    $ catMaybes
                      [ either (Just . rpcErrorText) (const Nothing) policyResult
                      , either (Just . rpcErrorText) (const Nothing) divergenceResult
                      ]

refreshPendingOrder :: Config -> EthClient -> PerpsKeeperOrderRow -> IO (Either Text FreshPendingOrder)
refreshPendingOrder cfg client order = do
  viewResult <- Perps.getPendingOrderView client (cfgPerpsOrderRouter cfg) (pkorOrderId order)
  pure $ case viewResult of
    Right view | Perps.povOrderId view == pkorOrderId order ->
      Right
        FreshPendingOrder
          { fpoOrder =
              order
                { pkorSide = Perps.povSide view
                , pkorCommitBlock = Perps.povCommitBlock view
                , pkorCommitTime = Perps.povCommitTime view
                }
          , fpoIsClose = Perps.povIsClose view
          }
    Right view ->
      Left $
        "router returned pending order "
          <> T.pack (show $ Perps.povOrderId view)
          <> " while re-reading order "
          <> T.pack (show $ pkorOrderId order)
    Left err ->
      Left $
        "could not re-read pending order "
          <> T.pack (show $ pkorOrderId order)
          <> ": "
          <> rpcErrorText err

refreshContiguousOrders :: Config -> EthClient -> [PerpsKeeperOrderRow] -> IO [PerpsKeeperOrderRow]
refreshContiguousOrders _ _ [] = pure []
refreshContiguousOrders cfg client (order : orders) = do
  result <- refreshPendingOrder cfg client order
  case result of
    Left err -> do
      putStrLn $ "batch re-read stopped: " <> T.unpack err
      pure []
    Right freshOrder -> (fpoOrder freshOrder :) <$> refreshContiguousOrders cfg client orders

submitIntent :: Config -> Connection -> EthClient -> Bool -> ExecutionIntent -> IO ()
submitIntent cfg conn client dryRun intent = do
  let targetOrders = intentOrders intent
      targetIds = map pkorOrderId targetOrders
      callData =
        case intent of
          CleanupExpired order -> Perps.executeOrderCall (pkorOrderId order) []
          ExecuteReady [order] _ _ updateData -> Perps.executeOrderCall (pkorOrderId order) updateData
          ExecuteReady orders _ _ updateData ->
            Perps.executeOrderBatchCall (maximum $ map pkorOrderId orders) updateData
  valueResult <- intentValue cfg client intent
  case valueResult of
    Left err -> recordAllErrors conn targetIds err
    Right value ->
      if dryRun
        then
          putStrLn $
            "dry-run: would submit "
              <> describeIntent intent
              <> " with value "
              <> show value
        else do
          forM_ targetIds (recordPerpsKeeperOrderAttempt conn)
          sent <- submitKeeperTransaction cfg client value callData
          case sent of
            Left err -> recordAllErrors conn targetIds err
            Right receipt -> applyReceipt conn targetIds receipt

intentValue :: Config -> EthClient -> ExecutionIntent -> IO (Either Text Integer)
intentValue _ _ (CleanupExpired _) = pure $ Right 0
intentValue cfg client (ExecuteReady orders _ _ updateData) = do
  feeResult <- Perps.getUpdateFee client (cfgPerpsPletherOracle cfg) updateData
  pure $
    case feeResult of
      Left err -> Left $ rpcErrorText err
      Right updateFee -> Right $ updateFee * fromIntegral (length orders)

submitKeeperTransaction :: Config -> EthClient -> Integer -> ByteString -> IO (Either Text TxReceipt)
submitKeeperTransaction cfg client value callData =
  case cfgKeeperPrivateKey cfg of
    Nothing -> pure $ Left "KEEPER_PRIVATE_KEY is not configured"
    Just privateKey ->
      deriveAddress privateKey >>= \case
        Left err -> pure $ Left err
        Right fromAddr -> do
          nonceResult <- ethGetTransactionCount client fromAddr
          gasResult <- ethEstimateGas client fromAddr (cfgPerpsOrderRouter cfg) value callData
          gasPriceResult <- ethGasPrice client
          priorityResult <- ethMaxPriorityFeePerGas client
          case (nonceResult, gasResult, gasPriceResult) of
            (Right nonce, Right estimatedGas, Right gasPrice) -> do
              let priorityBase = fromRight gasPrice priorityResult
                  maxFeeBase = max gasPrice priorityBase
                  gasLimit = max 21_000 $ applyBuffer estimatedGas (cfgKeeperGasBufferBps cfg)
                  maxPriorityFee = applyBuffer priorityBase (cfgKeeperFeeBufferBps cfg)
                  maxFee = max maxPriorityFee $ applyBuffer maxFeeBase (cfgKeeperFeeBufferBps cfg)
                  tx =
                    Tx1559
                      { txChainId = cfgPerpsChainId cfg
                      , txNonce = nonce
                      , txMaxPriorityFeePerGas = maxPriorityFee
                      , txMaxFeePerGas = maxFee
                      , txGasLimit = gasLimit
                      , txTo = cfgPerpsOrderRouter cfg
                      , txValue = value
                      , txData = callData
                      }
              signResult <- signTransaction privateKey tx
              case signResult of
                Left err -> pure $ Left err
                Right signed -> do
                  sendResult <- ethSendRawTransaction client (signedRawTransaction signed)
                  case sendResult of
                    Left err -> pure $ Left $ rpcErrorText err
                    Right txHash -> waitForReceipt client txHash 60
            _ ->
              pure $
                Left $
                  T.intercalate
                    "; "
                    $ catMaybes
                      [ either (Just . rpcErrorText) (const Nothing) nonceResult
                      , either (Just . rpcErrorText) (const Nothing) gasResult
                      , either (Just . rpcErrorText) (const Nothing) gasPriceResult
                      ]

waitForReceipt :: EthClient -> Text -> Int -> IO (Either Text TxReceipt)
waitForReceipt _ txHash 0 = pure $ Left $ "timed out waiting for receipt " <> txHash
waitForReceipt client txHash attempts = do
  receiptResult <- ethGetTransactionReceipt client txHash
  case receiptResult of
    Left err -> pure $ Left $ rpcErrorText err
    Right (Just receipt) -> pure $ Right receipt
    Right Nothing -> do
      threadDelay 2_000_000
      waitForReceipt client txHash (attempts - 1)

applyReceipt :: Connection -> [Integer] -> TxReceipt -> IO ()
applyReceipt conn targetIds receipt = do
  let orderEvents = mapMaybe Perps.decodePerpsOrderEvent (receiptLogs receipt)
  seenIds <- foldM applyEvent [] orderEvents
  let missingIds = filter (`notElem` seenIds) targetIds
  if receiptSucceeded receipt
    then
      forM_ missingIds $ \orderId ->
        recordPerpsKeeperOrderError
          conn
          orderId
          ("confirmed in " <> receiptTxHash receipt <> " without target order event")
    else
      forM_ targetIds $ \orderId ->
        recordPerpsKeeperOrderError conn orderId ("transaction reverted: " <> receiptTxHash receipt)
  where
    applyEvent seen = \case
      Perps.OrderExecuted {..} -> do
        markPerpsKeeperOrderExecuted conn poeOrderId poeTxHash poeBlockNumber poeExecutionPrice
        pure $ poeOrderId : seen
      Perps.OrderFailed {..} -> do
        markPerpsKeeperOrderFailed conn poeOrderId poeTxHash poeBlockNumber poeFailureReason
        putStrLn $
          "order "
            <> show poeOrderId
            <> " failed with "
            <> T.unpack (Perps.orderFailureReasonText poeFailureReason)
        pure $ poeOrderId : seen
      Perps.OrderCommitted {} -> pure seen

recordAllErrors :: Connection -> [Integer] -> Text -> IO ()
recordAllErrors conn orderIds err = do
  forM_ orderIds $ \orderId ->
    if isSameBlockMevGuardError err
      then recordPerpsKeeperOrderImmediateRetryError conn orderId err
      else recordPerpsKeeperOrderError conn orderId err
  putStrLn $ "keeper transaction skipped/failed: " <> T.unpack err

decodePayload :: PythUpdatePayloadRow -> Either Text ([Integer], [ByteString])
decodePayload PythUpdatePayloadRow {puprPublishTimes, puprUpdateData} = do
  publishTimes <- parseValue "publish_times" puprPublishTimes
  updateHex <- parseValue "update_data" puprUpdateData
  updateData <- traverse decodeHexUpdate updateHex
  pure (publishTimes, updateData)

parseValue :: (FromJSON a) => Text -> Value -> Either Text a
parseValue label value =
  case fromJSON value of
    Success decoded -> Right decoded
    Error err -> Left $ label <> " JSON decode failed: " <> T.pack err

decodeHexUpdate :: Text -> Either Text ByteString
decodeHexUpdate value =
  case B16.decode (TE.encodeUtf8 $ T.toLower $ strip0x value) of
    Right bytes -> Right bytes
    Left err -> Left $ "invalid updateData hex: " <> T.pack err

isOrderExpired :: Integer -> Integer -> PerpsKeeperOrderRow -> Bool
isOrderExpired chainNow maxAge order =
  maxAge > 0 && chainNow > pkorCommitTime order + maxAge

isOrderRevealReady :: Integer -> [Integer] -> PerpsKeeperOrderRow -> Bool
isOrderRevealReady settlementWindow publishTimes order =
  either (const False) (const True) $
    validateRevealWindow (pkorCommitTime order) settlementWindow publishTimes

isFrozenClosePayloadReady :: Integer -> Integer -> Integer -> [Integer] -> Bool
isFrozenClosePayloadReady chainNow maxStaleness maxDivergence publishTimes =
  case publishTimes of
    [] -> False
    _ ->
      all (\publishTime -> publishTime <= chainNow && chainNow - publishTime <= maxStaleness) publishTimes
        && maximum publishTimes <= minimum publishTimes + maxDivergence

isSameBlockMevGuardError :: Text -> Bool
isSameBlockMevGuardError err =
  "0x7abb32d5" `T.isInfixOf` T.toLower err

selectBatchCandidates
  :: Integer -- chain now
  -> Integer -- current block
  -> Integer -- max order age
  -> Integer -- settlement window
  -> [Integer] -- payload publish times
  -> Int -- max batch size
  -> [PerpsKeeperOrderRow]
  -> [PerpsKeeperOrderRow]
selectBatchCandidates chainNow currentBlock maxAge settlementWindow publishTimes maxBatchSize =
  take maxBatchSize
    . takeWhile
      ( \order ->
          isPastCommitBlock currentBlock order
            && ( isOrderExpired chainNow maxAge order
                  || isOrderRevealReady settlementWindow publishTimes order
               )
      )

isPastCommitBlock :: Integer -> PerpsKeeperOrderRow -> Bool
isPastCommitBlock currentBlock order =
  currentBlock > pkorCommitBlock order

intentOrders :: ExecutionIntent -> [PerpsKeeperOrderRow]
intentOrders = \case
  CleanupExpired order -> [order]
  ExecuteReady orders _ _ _ -> orders

describeIntent :: ExecutionIntent -> String
describeIntent = \case
  CleanupExpired order -> "expired-order cleanup for " <> show (pkorOrderId order)
  ExecuteReady [order] _ _ _ -> "single-order execution for " <> show (pkorOrderId order)
  ExecuteReady orders _ _ _ ->
    "batch execution through order "
      <> show (maximum $ map pkorOrderId orders)
      <> " ("
      <> show (length orders)
      <> " orders)"

applyBuffer :: Integer -> Integer -> Integer
applyBuffer value bufferBps =
  ((value * (10_000 + bufferBps)) + 9_999) `div` 10_000

fromRight :: a -> Either e a -> a
fromRight fallback = \case
  Left _ -> fallback
  Right value -> value

rpcErrorText :: RpcError -> Text
rpcErrorText = \case
  RpcHttpError err -> "RPC HTTP error: " <> err
  RpcJsonError err -> "RPC JSON error: " <> err
  RpcNodeError code message mData ->
    "RPC node error "
      <> T.pack (show code)
      <> ": "
      <> message
      <> maybe "" ("; data: " <>) mData

strip0x :: Text -> Text
strip0x value =
  fromMaybe value $ T.stripPrefix "0x" value

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f =
  foldr
    ( \value acc ->
        case f value of
          Just result -> result : acc
          Nothing -> acc
    )
    []

catMaybes :: [Maybe a] -> [a]
catMaybes =
  foldr
    ( \value acc ->
        case value of
          Just result -> result : acc
          Nothing -> acc
    )
    []
