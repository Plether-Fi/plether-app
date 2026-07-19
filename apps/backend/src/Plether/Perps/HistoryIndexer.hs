module Plether.Perps.HistoryIndexer
  ( PerpsAddresses (..)
  , PerpsIndexerConfig (..)
  , PerpsIndexerMode (..)
  , defaultPerpsAddresses
  , runPerpsIndexer
  , perpsEventTopics
  , parsePerpsLog
  , RpcLog (..)
  , ParsedPerpsLog (..)
  , orderFailReasonName
  , terminalStatus
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, forever, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , httpLbs
  , parseRequest
  , responseBody
  )
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( deletePerpsHistoryFromBlock
  , getPerpsIndexerLastBlock
  , insertPerpsExpiredCleanupActivityIfReady
  , insertPerpsActivity
  , insertPerpsEvent
  , setPerpsIndexerState
  , upsertPerpsOrderCommitted
  , upsertPerpsOrderTerminal
  )
import Plether.Indexer.Contracts (keccak256Text)
import Plether.Logging (field, logErrorEvery, logInfoEvery, logWarn, logWarnEvery)
import Plether.Utils.Hex (hexToInteger, intToHex)

data PerpsAddresses = PerpsAddresses
  { paOrderRouter :: Text
  , paCfdEngine :: Text
  , paMarginClearinghouse :: Text
  }
  deriving stock (Show)

defaultPerpsAddresses :: PerpsAddresses
defaultPerpsAddresses =
  PerpsAddresses
    { paOrderRouter = "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E"
    , paCfdEngine = "0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a"
    , paMarginClearinghouse = "0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e"
    }

data PerpsIndexerMode
  = PerpsIndexerLoop
  | PerpsIndexerOnce
  | PerpsIndexerBackfill Integer Integer
  deriving stock (Show, Eq)

data PerpsIndexerConfig = PerpsIndexerConfig
  { picRpcUrls :: [Text]
  , picChainId :: Integer
  , picAddresses :: PerpsAddresses
  , picStartBlock :: Integer
  , picConfirmations :: Integer
  , picBatchSize :: Integer
  , picPollIntervalMicros :: Int
  , picIndexerName :: Text
  , picMode :: PerpsIndexerMode
  }
  deriving stock (Show)

data RpcLog = RpcLog
  { rlAddress :: Text
  , rlTopics :: [ByteString]
  , rlData :: ByteString
  , rlTxHash :: Text
  , rlBlockNumber :: Integer
  , rlBlockHash :: Text
  , rlTxIndex :: Integer
  , rlLogIndex :: Integer
  }
  deriving stock (Show)

data BlockInfo = BlockInfo
  { biNumber :: Integer
  , biHash :: Text
  , biTimestamp :: Integer
  }
  deriving stock (Show)

data ParsedPerpsLog
  = ParsedOrderCommitted Integer Text Int Value
  | ParsedOrderExecuted Integer Integer Value
  | ParsedOrderFailed Integer Int Text Value
  | ParsedPositionActivity Text Text Int (Maybe Integer) (Maybe Integer) (Maybe Integer) (Maybe Integer) Value
  | ParsedMarginActivity Text Text Integer Value
  deriving stock (Show, Eq)

orderCommittedTopic :: ByteString
orderCommittedTopic = keccak256Text "OrderCommitted(uint64,address,uint8)"

orderExecutedTopic :: ByteString
orderExecutedTopic = keccak256Text "OrderExecuted(uint64,uint256)"

orderFailedTopic :: ByteString
orderFailedTopic = keccak256Text "OrderFailed(uint64,uint8)"

positionOpenedTopic :: ByteString
positionOpenedTopic = keccak256Text "PositionOpened(address,uint8,uint256,uint256,uint256)"

positionClosedTopic :: ByteString
positionClosedTopic = keccak256Text "PositionClosed(address,uint8,uint256,uint256,int256)"

positionLiquidatedTopic :: ByteString
positionLiquidatedTopic = keccak256Text "PositionLiquidated(address,uint8,uint256,uint256,uint256)"

marginAddedTopic :: ByteString
marginAddedTopic = keccak256Text "MarginAdded(address,uint256)"

depositTopic :: ByteString
depositTopic = keccak256Text "Deposit(address,address,uint256)"

withdrawTopic :: ByteString
withdrawTopic = keccak256Text "Withdraw(address,address,uint256)"

perpsEventTopics :: [ByteString]
perpsEventTopics =
  [ orderCommittedTopic
  , orderExecutedTopic
  , orderFailedTopic
  , positionOpenedTopic
  , positionClosedTopic
  , positionLiquidatedTopic
  , marginAddedTopic
  , depositTopic
  , withdrawTopic
  ]

runPerpsIndexer :: Manager -> DbPool -> PerpsIndexerConfig -> IO ()
runPerpsIndexer manager pool cfg =
  case picMode cfg of
    PerpsIndexerLoop -> forever $ do
      result <- try @SomeException $ runOneRange manager pool cfg Nothing Nothing
      case result of
        Left err -> do
          logErrorEvery
            60
            "perps_indexer_iteration_failed"
            "Perps indexer iteration failed"
            [field "error" $ show err]
          threadDelay (picPollIntervalMicros cfg * 2)
        Right indexed ->
          when (not indexed) $ threadDelay (picPollIntervalMicros cfg)
    PerpsIndexerOnce -> do
      _ <- runOneRange manager pool cfg Nothing Nothing
      pure ()
    PerpsIndexerBackfill fromBlock toBlock -> do
      runBackfill fromBlock toBlock
  where
    runBackfill fromBlock toBlock
      | fromBlock > toBlock = pure ()
      | otherwise = do
          let endBlock = min toBlock (fromBlock + picBatchSize cfg - 1)
          _ <- runOneRange manager pool cfg (Just fromBlock) (Just endBlock)
          runBackfill (endBlock + 1) toBlock

runOneRange :: Manager -> DbPool -> PerpsIndexerConfig -> Maybe Integer -> Maybe Integer -> IO Bool
runOneRange manager pool cfg explicitFrom explicitTo = do
  reqIdRef <- newIORef 1
  currentBlock <- requireRpc "eth_blockNumber" $ getCurrentBlockNumber manager (picRpcUrls cfg) reqIdRef
  let safeBlock = max 0 (currentBlock - picConfirmations cfg)
  (storedLastBlock, storedLastHash) <- withDb pool $ \conn ->
    getPerpsIndexerLastBlock conn (picChainId cfg) (picIndexerName cfg) (paOrderRouter $ picAddresses cfg)
  verifyCursor manager pool cfg reqIdRef storedLastBlock storedLastHash
  (lastBlock, _) <- withDb pool $ \conn ->
    getPerpsIndexerLastBlock conn (picChainId cfg) (picIndexerName cfg) (paOrderRouter $ picAddresses cfg)
  let startBlock = fromMaybe (max (picStartBlock cfg) (lastBlock + 1)) explicitFrom
      cappedToBlock = maybe safeBlock (min safeBlock) explicitTo
      endBlock = min cappedToBlock (startBlock + picBatchSize cfg - 1)
  if startBlock > endBlock
    then pure False
    else do
      logs <- requireRpc "eth_getLogs" $
        getLogs manager (picRpcUrls cfg) reqIdRef (perpsAddresses cfg) startBlock endBlock
      let orderedLogs = sortOn (\logEntry -> (rlBlockNumber logEntry, rlTxIndex logEntry, rlLogIndex logEntry)) logs
      forM_ orderedLogs $ \logEntry -> do
        blockInfo <- requireRpc "eth_getBlockByNumber" $
          getBlockByNumber manager (picRpcUrls cfg) reqIdRef (rlBlockNumber logEntry)
        mTxFrom <- getTransactionFrom manager (picRpcUrls cfg) reqIdRef (rlTxHash logEntry)
        processLog pool cfg blockInfo mTxFrom logEntry
      endInfo <- requireRpc "eth_getBlockByNumber" $
        getBlockByNumber manager (picRpcUrls cfg) reqIdRef endBlock
      withDb pool $ \conn -> do
        (currentCursor, _) <- getPerpsIndexerLastBlock conn (picChainId cfg) (picIndexerName cfg) (paOrderRouter $ picAddresses cfg)
        when (endBlock >= currentCursor) $
          setPerpsIndexerState conn (picChainId cfg) (picIndexerName cfg) (paOrderRouter $ picAddresses cfg) endBlock (Just $ biHash endInfo)
      logInfoEvery
        300
        "perps_indexer_progress"
        "Perps history indexer advanced"
        [ field "from_block" startBlock
        , field "to_block" endBlock
        , field "safe_head_block" safeBlock
        , field "event_count" $ length orderedLogs
        ]
      pure True

verifyCursor :: Manager -> DbPool -> PerpsIndexerConfig -> IORef Integer -> Integer -> Maybe Text -> IO ()
verifyCursor _ _ _ _ 0 _ = pure ()
verifyCursor _ _ _ _ _ Nothing = pure ()
verifyCursor manager pool cfg reqIdRef lastBlock (Just storedHash) = do
  eBlock <- getBlockByNumber manager (picRpcUrls cfg) reqIdRef lastBlock
  case eBlock of
    Right blockInfo | normalizeHex (biHash blockInfo) == normalizeHex storedHash -> pure ()
    Right _ -> rewind
    Left err ->
      logWarnEvery
        60
        "perps_indexer_cursor_verification_failed"
        "Perps indexer could not verify its cursor block hash"
        [ field "cursor_block" lastBlock
        , field "error" err
        ]
  where
    rewind = do
      let rewindBlock = max (picStartBlock cfg) lastBlock
          newCursor = max 0 (rewindBlock - 1)
      logWarn
        "perps_indexer_reorg_detected"
        "Perps indexer detected a block hash mismatch and rewound its cursor"
        [ field "mismatch_block" lastBlock
        , field "rewind_to_block" newCursor
        ]
      withDb pool $ \conn -> do
        deletePerpsHistoryFromBlock conn (picChainId cfg) (paOrderRouter $ picAddresses cfg) rewindBlock
        setPerpsIndexerState conn (picChainId cfg) (picIndexerName cfg) (paOrderRouter $ picAddresses cfg) newCursor Nothing

processLog :: DbPool -> PerpsIndexerConfig -> BlockInfo -> Maybe Text -> RpcLog -> IO ()
processLog pool cfg blockInfo txFrom logEntry =
  case parsePerpsLog logEntry of
    Nothing -> pure ()
    Just parsed -> withDb pool $ \conn -> do
      let eventName = parsedEventName parsed
          account = parsedAccount parsed
          orderId = parsedOrderId parsed
          side = parsedSide parsed
          eventPayload = parsedPayload parsed
          releaseRouter = paOrderRouter $ picAddresses cfg
      insertPerpsEvent conn (picChainId cfg) releaseRouter (rlAddress logEntry) eventName (rlTxHash logEntry)
        (rlBlockNumber logEntry) (rlBlockHash logEntry) (rlTxIndex logEntry) (rlLogIndex logEntry)
        (biTimestamp blockInfo) account orderId side eventPayload
      case parsed of
        ParsedOrderCommitted oid account' side' _ ->
          do
            upsertPerpsOrderCommitted conn (picChainId cfg) releaseRouter oid account' side' (rlTxHash logEntry)
              (rlBlockNumber logEntry) (biTimestamp blockInfo)
            insertPerpsExpiredCleanupActivityIfReady conn (picChainId cfg) releaseRouter oid
        ParsedOrderExecuted oid executionPrice _ ->
          upsertPerpsOrderTerminal conn (picChainId cfg) releaseRouter oid "Executed" Nothing (Just executionPrice) Nothing
            (rlTxHash logEntry) (rlBlockNumber logEntry) (biTimestamp blockInfo)
        ParsedOrderFailed oid reason reasonName _ -> do
          upsertPerpsOrderTerminal conn (picChainId cfg) releaseRouter oid (terminalStatus reasonName) (Just reasonName) Nothing txFrom
            (rlTxHash logEntry) (rlBlockNumber logEntry) (biTimestamp blockInfo)
          when (reason == 0) $
            insertPerpsExpiredCleanupActivityIfReady conn (picChainId cfg) releaseRouter oid
        ParsedPositionActivity kind account' side' price sizeDelta amountUsdc pnl payload ->
          insertPerpsActivity conn (picChainId cfg) releaseRouter (activityKey logEntry kind Nothing) account'
            kind Nothing Nothing (Just side') price sizeDelta amountUsdc pnl (rlTxHash logEntry)
            (rlBlockNumber logEntry) (rlBlockHash logEntry) (rlTxIndex logEntry) (rlLogIndex logEntry)
            (biTimestamp blockInfo) payload
        ParsedMarginActivity kind account' amount payload ->
          insertPerpsActivity conn (picChainId cfg) releaseRouter (activityKey logEntry kind Nothing) account'
            kind Nothing Nothing Nothing Nothing Nothing (Just amount) Nothing (rlTxHash logEntry)
            (rlBlockNumber logEntry) (rlBlockHash logEntry) (rlTxIndex logEntry) (rlLogIndex logEntry)
            (biTimestamp blockInfo) payload

parsePerpsLog :: RpcLog -> Maybe ParsedPerpsLog
parsePerpsLog logEntry =
  case rlTopics logEntry of
    topic : _
      | topic == orderCommittedTopic -> parseOrderCommitted logEntry
      | topic == orderExecutedTopic -> parseOrderExecuted logEntry
      | topic == orderFailedTopic -> parseOrderFailed logEntry
      | topic == positionOpenedTopic -> parsePositionOpened logEntry
      | topic == positionClosedTopic -> parsePositionClosed logEntry
      | topic == positionLiquidatedTopic -> parsePositionLiquidated logEntry
      | topic == marginAddedTopic -> parseMarginAdded logEntry
      | topic == depositTopic -> parseDepositWithdraw "Deposit" logEntry
      | topic == withdrawTopic -> parseDepositWithdraw "Withdraw" logEntry
    _ -> Nothing

parseOrderCommitted :: RpcLog -> Maybe ParsedPerpsLog
parseOrderCommitted logEntry = do
  oid <- indexedUint (rlTopics logEntry) 1
  account <- indexedAddress (rlTopics logEntry) 2
  let side = fromInteger $ wordAt (rlData logEntry) 0
  pure $ ParsedOrderCommitted oid account side $
    object ["orderId" .= show oid, "account" .= account, "side" .= side]

parseOrderExecuted :: RpcLog -> Maybe ParsedPerpsLog
parseOrderExecuted logEntry = do
  oid <- indexedUint (rlTopics logEntry) 1
  let executionPrice = wordAt (rlData logEntry) 0
  pure $ ParsedOrderExecuted oid executionPrice $
    object ["orderId" .= show oid, "executionPrice" .= show executionPrice]

parseOrderFailed :: RpcLog -> Maybe ParsedPerpsLog
parseOrderFailed logEntry = do
  oid <- indexedUint (rlTopics logEntry) 1
  let reason = fromInteger $ wordAt (rlData logEntry) 0
      reasonName = orderFailReasonName reason
  pure $ ParsedOrderFailed oid reason reasonName $
    object ["orderId" .= show oid, "reason" .= reason, "reasonName" .= reasonName]

parsePositionOpened :: RpcLog -> Maybe ParsedPerpsLog
parsePositionOpened logEntry = do
  account <- indexedAddress (rlTopics logEntry) 1
  let side = fromInteger $ wordAt (rlData logEntry) 0
      sizeDelta = wordAt (rlData logEntry) 1
      price = wordAt (rlData logEntry) 2
      marginDelta = wordAt (rlData logEntry) 3
      payload = object
        [ "account" .= account
        , "side" .= side
        , "sizeDelta" .= show sizeDelta
        , "price" .= show price
        , "marginDelta" .= show marginDelta
        ]
  pure $ ParsedPositionActivity "Open" account side (Just price) (Just sizeDelta) (Just marginDelta) Nothing payload

parsePositionClosed :: RpcLog -> Maybe ParsedPerpsLog
parsePositionClosed logEntry = do
  account <- indexedAddress (rlTopics logEntry) 1
  let side = fromInteger $ wordAt (rlData logEntry) 0
      sizeDelta = wordAt (rlData logEntry) 1
      price = wordAt (rlData logEntry) 2
      pnl = intWordAt (rlData logEntry) 3
      payload = object
        [ "account" .= account
        , "side" .= side
        , "sizeDelta" .= show sizeDelta
        , "price" .= show price
        , "pnl" .= show pnl
        ]
  pure $ ParsedPositionActivity "Close" account side (Just price) (Just sizeDelta) Nothing (Just pnl) payload

parsePositionLiquidated :: RpcLog -> Maybe ParsedPerpsLog
parsePositionLiquidated logEntry = do
  account <- indexedAddress (rlTopics logEntry) 1
  let side = fromInteger $ wordAt (rlData logEntry) 0
      sizeDelta = wordAt (rlData logEntry) 1
      price = wordAt (rlData logEntry) 2
      keeperBounty = wordAt (rlData logEntry) 3
      payload = object
        [ "account" .= account
        , "side" .= side
        , "sizeDelta" .= show sizeDelta
        , "price" .= show price
        , "keeperBountyUsdc" .= show keeperBounty
        ]
  pure $ ParsedPositionActivity "Liquidated" account side (Just price) (Just sizeDelta) (Just keeperBounty) Nothing payload

parseMarginAdded :: RpcLog -> Maybe ParsedPerpsLog
parseMarginAdded logEntry = do
  account <- indexedAddress (rlTopics logEntry) 1
  let amount = wordAt (rlData logEntry) 0
  pure $ ParsedMarginActivity "Add margin" account amount $
    object ["account" .= account, "amountUsdc" .= show amount]

parseDepositWithdraw :: Text -> RpcLog -> Maybe ParsedPerpsLog
parseDepositWithdraw kind logEntry = do
  account <- indexedAddress (rlTopics logEntry) 1
  let amount = wordAt (rlData logEntry) 0
  pure $ ParsedMarginActivity kind account amount $
    object ["account" .= account, "amountUsdc" .= show amount]

parsedEventName :: ParsedPerpsLog -> Text
parsedEventName = \case
  ParsedOrderCommitted {} -> "OrderCommitted"
  ParsedOrderExecuted {} -> "OrderExecuted"
  ParsedOrderFailed {} -> "OrderFailed"
  ParsedPositionActivity kind _ _ _ _ _ _ _
    | kind == "Open" -> "PositionOpened"
    | kind == "Close" -> "PositionClosed"
    | kind == "Liquidated" -> "PositionLiquidated"
    | otherwise -> kind
  ParsedMarginActivity kind _ _ _
    | kind == "Add margin" -> "MarginAdded"
    | otherwise -> kind

parsedAccount :: ParsedPerpsLog -> Maybe Text
parsedAccount = \case
  ParsedOrderCommitted _ account _ _ -> Just account
  ParsedPositionActivity _ account _ _ _ _ _ _ -> Just account
  ParsedMarginActivity _ account _ _ -> Just account
  _ -> Nothing

parsedOrderId :: ParsedPerpsLog -> Maybe Integer
parsedOrderId = \case
  ParsedOrderCommitted oid _ _ _ -> Just oid
  ParsedOrderExecuted oid _ _ -> Just oid
  ParsedOrderFailed oid _ _ _ -> Just oid
  _ -> Nothing

parsedSide :: ParsedPerpsLog -> Maybe Int
parsedSide = \case
  ParsedOrderCommitted _ _ side _ -> Just side
  ParsedPositionActivity _ _ side _ _ _ _ _ -> Just side
  _ -> Nothing

parsedPayload :: ParsedPerpsLog -> Value
parsedPayload = \case
  ParsedOrderCommitted _ _ _ payload -> payload
  ParsedOrderExecuted _ _ payload -> payload
  ParsedOrderFailed _ _ _ payload -> payload
  ParsedPositionActivity _ _ _ _ _ _ _ payload -> payload
  ParsedMarginActivity _ _ _ payload -> payload

terminalStatus :: Text -> Text
terminalStatus "Expired" = "Expired / Cleaned up"
terminalStatus _ = "Failed"

orderFailReasonName :: Int -> Text
orderFailReasonName = \case
  0 -> "Expired"
  1 -> "CloseOnly"
  2 -> "SlippageExceeded"
  3 -> "EnginePanic"
  4 -> "AccountLiquidated"
  5 -> "EngineRevert"
  n -> "Unknown(" <> T.pack (show n) <> ")"

activityKey :: RpcLog -> Text -> Maybe Integer -> Text
activityKey logEntry kind orderId =
  T.intercalate ":"
    [ normalizeHex (rlTxHash logEntry)
    , T.pack $ show (rlLogIndex logEntry)
    , T.replace " " "_" kind
    , maybe "" (T.pack . show) orderId
    ]

perpsAddresses :: PerpsIndexerConfig -> [Text]
perpsAddresses cfg =
  [ paOrderRouter (picAddresses cfg)
  , paCfdEngine (picAddresses cfg)
  , paMarginClearinghouse (picAddresses cfg)
  ]

requireRpc :: Text -> IO (Either Text a) -> IO a
requireRpc label action = do
  result <- action
  case result of
    Right value -> pure value
    Left err -> fail $ T.unpack $ label <> " failed: " <> err

getCurrentBlockNumber :: Manager -> [Text] -> IORef Integer -> IO (Either Text Integer)
getCurrentBlockNumber manager rpcUrls reqIdRef = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_blockNumber" ([] :: [Value])
  pure $ case result of
    Left err -> Left err
    Right (String hex) -> Right $ hexToInteger $ strip0x hex
    Right _ -> Left "Expected hex string"

getBlockByNumber :: Manager -> [Text] -> IORef Integer -> Integer -> IO (Either Text BlockInfo)
getBlockByNumber manager rpcUrls reqIdRef blockNumber = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_getBlockByNumber" [String $ "0x" <> intToHex blockNumber, Bool False]
  pure $ case result of
    Left err -> Left err
    Right (Object obj) -> Right $
      BlockInfo
        { biNumber = blockNumber
        , biHash = getString "hash" obj
        , biTimestamp = hexToInteger $ strip0x $ getString "timestamp" obj
        }
    Right _ -> Left "Expected block object"

getTransactionFrom :: Manager -> [Text] -> IORef Integer -> Text -> IO (Maybe Text)
getTransactionFrom manager rpcUrls reqIdRef txHash = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_getTransactionByHash" [String txHash]
  pure $ case result of
    Right (Object obj) ->
      case KM.lookup (Key.fromText "from") obj of
        Just (String fromAddr) -> Just fromAddr
        _ -> Nothing
    _ -> Nothing

getLogs :: Manager -> [Text] -> IORef Integer -> [Text] -> Integer -> Integer -> IO (Either Text [RpcLog])
getLogs manager rpcUrls reqIdRef addresses fromBlock toBlock = do
  let topics = map (String . ("0x" <>) . bytesToHex) perpsEventTopics
      filterObject = object
        [ "address" .= addresses
        , "topics" .= [topics]
        , "fromBlock" .= ("0x" <> intToHex fromBlock)
        , "toBlock" .= ("0x" <> intToHex toBlock)
        ]
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_getLogs" [filterObject]
  pure $ case result of
    Left err -> Left err
    Right (Array arr) -> Right $ catMaybes $ map parseLogEntry (toList arr)
    Right _ -> Left "Expected logs array"

parseLogEntry :: Value -> Maybe RpcLog
parseLogEntry = \case
  Object obj -> Just $
    RpcLog
      { rlAddress = getString "address" obj
      , rlTopics = map decodeHex $ getStringArray "topics" obj
      , rlData = decodeHex $ getString "data" obj
      , rlTxHash = getString "transactionHash" obj
      , rlBlockNumber = hexToInteger $ strip0x $ getString "blockNumber" obj
      , rlBlockHash = getString "blockHash" obj
      , rlTxIndex = hexToInteger $ strip0x $ getString "transactionIndex" obj
      , rlLogIndex = hexToInteger $ strip0x $ getString "logIndex" obj
      }
  _ -> Nothing

rpcCallAny :: (Aeson.ToJSON params) => Manager -> [Text] -> IORef Integer -> Text -> params -> IO (Either Text Value)
rpcCallAny manager rpcUrls reqIdRef method params = tryUrls rpcUrls
  where
    tryUrls [] = pure $ Left "No RPC URLs configured"
    tryUrls [url] = rpcCall manager url reqIdRef method params
    tryUrls (url : rest) = do
      result <- rpcCall manager url reqIdRef method params
      case result of
        Right value -> pure $ Right value
        Left err -> do
          logWarnEvery
            60
            "perps_indexer_rpc_fallback"
            "Perps indexer RPC request failed; trying a fallback provider"
            [ field "rpc_method" method
            , field "remaining_provider_count" $ length rest
            , field "error" err
            ]
          tryUrls rest

rpcCall :: (Aeson.ToJSON params) => Manager -> Text -> IORef Integer -> Text -> params -> IO (Either Text Value)
rpcCall manager rpcUrl reqIdRef methodName params = do
  reqId <- nextId reqIdRef
  let payload = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= methodName
        , "params" .= params
        , "id" .= reqId
        ]
  eResult <- try @SomeException $ do
    req <- parseRequest $ T.unpack rpcUrl
    let req' = req
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS $ Aeson.encode payload
          }
    responseBody <$> httpLbs req' manager
  case eResult of
    Left err -> pure $ Left $ T.pack $ show err
    Right body ->
      case Aeson.decode body of
        Just (Object obj) ->
          case KM.lookup (Key.fromText "result") obj of
            Just value -> pure $ Right value
            Nothing -> pure $ Left $ "RPC error: " <> T.pack (show $ KM.lookup (Key.fromText "error") obj)
        _ -> pure $ Left "Invalid JSON-RPC response"

nextId :: IORef Integer -> IO Integer
nextId ref = atomicModifyIORef' ref $ \n -> (n + 1, n)

indexedUint :: [ByteString] -> Int -> Maybe Integer
indexedUint topics idx
  | idx < length topics = Just $ bytesToInteger (topics !! idx)
  | otherwise = Nothing

indexedAddress :: [ByteString] -> Int -> Maybe Text
indexedAddress topics idx
  | idx < length topics = Just $ "0x" <> T.drop 24 (bytesToHex (topics !! idx))
  | otherwise = Nothing

wordAt :: ByteString -> Int -> Integer
wordAt bytes index = bytesToInteger $ BS.take 32 $ BS.drop (index * 32) bytes

intWordAt :: ByteString -> Int -> Integer
intWordAt bytes index =
  let unsigned = wordAt bytes index
      signThreshold = 2 ^ (255 :: Int)
      modulo = 2 ^ (256 :: Int)
  in if unsigned >= signThreshold then unsigned - modulo else unsigned

bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

bytesToHex :: ByteString -> Text
bytesToHex = TE.decodeUtf8 . B16.encode

decodeHex :: Text -> ByteString
decodeHex txt =
  case B16.decode (TE.encodeUtf8 $ T.toLower $ strip0x txt) of
    Right bs -> bs
    Left _ -> ""

strip0x :: Text -> Text
strip0x txt
  | "0x" `T.isPrefixOf` txt = T.drop 2 txt
  | "0X" `T.isPrefixOf` txt = T.drop 2 txt
  | otherwise = txt

normalizeHex :: Text -> Text
normalizeHex txt = "0x" <> T.toLower (strip0x txt)

getString :: Text -> Aeson.Object -> Text
getString key obj = case KM.lookup (Key.fromText key) obj of
  Just (String s) -> s
  _ -> ""

getStringArray :: Text -> Aeson.Object -> [Text]
getStringArray key obj = case KM.lookup (Key.fromText key) obj of
  Just (Array arr) -> [s | String s <- toList arr]
  _ -> []
