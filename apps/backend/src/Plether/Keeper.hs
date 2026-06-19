module Plether.Keeper
  ( KeeperMode (..)
  , runKeeper
  , isOrderExpired
  , isOrderRevealReady
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
  ( PerpsOrderRow (..)
  , PythUpdatePayloadRow (..)
  , getPendingPerpsOrders
  , getPerpsKeeperLastIndexedBlock
  , getPythUpdatePayloadForWindow
  , markPerpsOrderExecuted
  , markPerpsOrderFailed
  , recordPerpsOrderAttempt
  , recordPerpsOrderError
  , setPerpsKeeperLastIndexedBlock
  , tryPerpsKeeperLock
  , unlockPerpsKeeperLock
  , upsertPerpsOrderCommitted
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
  = CleanupExpired PerpsOrderRow
  | ExecuteReady [PerpsOrderRow] PythUpdatePayloadRow [Integer] [ByteString]

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
      let startBlock = max (cfgPerpsIndexerStartBlock cfg) (lastIndexed + 1)
          endBlock = min latestBlock (startBlock + 1_999)
      if startBlock > latestBlock
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
applyOrderEvent _cfg conn client = \case
  Perps.OrderCommitted {..} -> do
    timestampResult <- ethBlockTimestamp client poeBlockNumber
    case timestampResult of
      Left err ->
        putStrLn $
          "could not fetch commit block timestamp for order "
            <> show poeOrderId
            <> ": "
            <> T.unpack (rpcErrorText err)
      Right commitTime ->
        upsertPerpsOrderCommitted
          conn
          poeOrderId
          poeAccount
          poeSide
          poeBlockNumber
          commitTime
          poeTxHash
  Perps.OrderExecuted {..} ->
    markPerpsOrderExecuted conn poeOrderId poeTxHash poeBlockNumber poeExecutionPrice
  Perps.OrderFailed {..} ->
    markPerpsOrderFailed conn poeOrderId poeTxHash poeBlockNumber poeFailureReason

processQueueHead :: Config -> Connection -> EthClient -> Bool -> IO ()
processQueueHead cfg conn client dryRun = do
  pending <- getPendingPerpsOrders conn (cfgKeeperMaxBatchSize cfg)
  case pending of
    [] -> pure ()
    headOrder : _ -> do
      maxAgeResult <- Perps.maxOrderAge client (cfgPerpsOrderRouter cfg)
      settlementWindowResult <- Perps.orderSettlementWindow client (cfgPerpsPletherOracle cfg)
      chainNowResult <- ethLatestBlockTimestamp client
      case (maxAgeResult, settlementWindowResult, chainNowResult) of
        (Right maxAge, Right settlementWindow, Right chainNow) ->
          decideExecution cfg conn client dryRun pending headOrder maxAge settlementWindow chainNow
        _ -> do
          let errors =
                [ either (Just . rpcErrorText) (const Nothing) maxAgeResult
                , either (Just . rpcErrorText) (const Nothing) settlementWindowResult
                , either (Just . rpcErrorText) (const Nothing) chainNowResult
                ]
          putStrLn $
            "queue processing skipped: "
              <> T.unpack (T.intercalate "; " $ catMaybes errors)

decideExecution
  :: Config
  -> Connection
  -> EthClient
  -> Bool
  -> [PerpsOrderRow]
  -> PerpsOrderRow
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
decideExecution cfg conn client dryRun pending headOrder maxAge settlementWindow chainNow
  | isOrderExpired chainNow maxAge headOrder =
      submitIntent cfg conn client dryRun $ CleanupExpired headOrder
  | chainNow < porCommitTime headOrder + 1 =
      putStrLn $
        "queue head order "
          <> show (porOrderId headOrder)
          <> " is waiting for reveal window"
  | otherwise = do
      mPayload <-
        getPythUpdatePayloadForWindow
          conn
          (porCommitTime headOrder + 1)
          (porCommitTime headOrder + settlementWindow)
      case mPayload of
        Nothing ->
          putStrLn $
            "queue head order "
              <> show (porOrderId headOrder)
              <> " is waiting for cached Pyth payload"
        Just payload ->
          case decodePayload payload of
            Left err -> do
              recordPerpsOrderError conn (porOrderId headOrder) err
              putStrLn $ "cached Pyth payload could not be decoded: " <> T.unpack err
            Right (publishTimes, updateData) ->
              case validateRevealWindow (porCommitTime headOrder) settlementWindow publishTimes of
                Left err -> do
                  putStrLn $
                    "cached Pyth payload is not valid for order "
                      <> show (porOrderId headOrder)
                      <> ": "
                      <> T.unpack err
                Right _ -> do
                  let selected =
                        selectBatchCandidates
                          chainNow
                          maxAge
                          settlementWindow
                          publishTimes
                          (cfgKeeperMaxBatchSize cfg)
                          pending
                  case selected of
                    [] ->
                      putStrLn $
                        "cached Pyth payload is not valid for queue head order "
                          <> show (porOrderId headOrder)
                    orders ->
                      submitIntent cfg conn client dryRun $
                        ExecuteReady orders payload publishTimes updateData

submitIntent :: Config -> Connection -> EthClient -> Bool -> ExecutionIntent -> IO ()
submitIntent cfg conn client dryRun intent = do
  let targetOrders = intentOrders intent
      targetIds = map porOrderId targetOrders
      callData =
        case intent of
          CleanupExpired order -> Perps.executeOrderCall (porOrderId order) []
          ExecuteReady [order] _ _ updateData -> Perps.executeOrderCall (porOrderId order) updateData
          ExecuteReady orders _ _ updateData ->
            Perps.executeOrderBatchCall (maximum $ map porOrderId orders) updateData
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
          forM_ targetIds (recordPerpsOrderAttempt conn)
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
        recordPerpsOrderError
          conn
          orderId
          ("confirmed in " <> receiptTxHash receipt <> " without target order event")
    else
      forM_ targetIds $ \orderId ->
        recordPerpsOrderError conn orderId ("transaction reverted: " <> receiptTxHash receipt)
  where
    applyEvent seen = \case
      Perps.OrderExecuted {..} -> do
        markPerpsOrderExecuted conn poeOrderId poeTxHash poeBlockNumber poeExecutionPrice
        pure $ poeOrderId : seen
      Perps.OrderFailed {..} -> do
        markPerpsOrderFailed conn poeOrderId poeTxHash poeBlockNumber poeFailureReason
        pure $ poeOrderId : seen
      Perps.OrderCommitted {} -> pure seen

recordAllErrors :: Connection -> [Integer] -> Text -> IO ()
recordAllErrors conn orderIds err = do
  forM_ orderIds $ \orderId -> recordPerpsOrderError conn orderId err
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

isOrderExpired :: Integer -> Integer -> PerpsOrderRow -> Bool
isOrderExpired chainNow maxAge order =
  maxAge > 0 && chainNow > porCommitTime order + maxAge

isOrderRevealReady :: Integer -> [Integer] -> PerpsOrderRow -> Bool
isOrderRevealReady settlementWindow publishTimes order =
  either (const False) (const True) $
    validateRevealWindow (porCommitTime order) settlementWindow publishTimes

selectBatchCandidates
  :: Integer -- chain now
  -> Integer -- max order age
  -> Integer -- settlement window
  -> [Integer] -- payload publish times
  -> Int -- max batch size
  -> [PerpsOrderRow]
  -> [PerpsOrderRow]
selectBatchCandidates chainNow maxAge settlementWindow publishTimes maxBatchSize =
  take maxBatchSize
    . takeWhile
      ( \order ->
          not (isOrderExpired chainNow maxAge order)
            && isOrderRevealReady settlementWindow publishTimes order
      )

intentOrders :: ExecutionIntent -> [PerpsOrderRow]
intentOrders = \case
  CleanupExpired order -> [order]
  ExecuteReady orders _ _ _ -> orders

describeIntent :: ExecutionIntent -> String
describeIntent = \case
  CleanupExpired order -> "expired-order cleanup for " <> show (porOrderId order)
  ExecuteReady [order] _ _ _ -> "single-order execution for " <> show (porOrderId order)
  ExecuteReady orders _ _ _ ->
    "batch execution through order "
      <> show (maximum $ map porOrderId orders)
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
  RpcNodeError code message ->
    "RPC node error "
      <> T.pack (show code)
      <> ": "
      <> message

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
