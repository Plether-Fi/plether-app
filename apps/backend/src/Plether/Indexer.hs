module Plether.Indexer
  ( startIndexer
  , IndexerConfig (..)
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
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , httpLbs
  , parseRequest
  , responseBody
  )
import Plether.Config (Addresses (..), Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema (getLastIndexedBlock, insertTransaction, setLastIndexedBlock)
import Plether.Indexer.Contracts (allEventSignatures, esTopic)
import Plether.Indexer.Events (EventLog (..), ParsedEvent (..), parseEventLog)

data IndexerConfig = IndexerConfig
  { icRpcUrl :: Text
  , icAddresses :: Addresses
  , icStartBlock :: Integer
  , icBatchSize :: Integer
  , icPollInterval :: Int
  }

startIndexer :: Manager -> DbPool -> IndexerConfig -> IO ()
startIndexer manager pool cfg = do
  putStrLn "Starting event indexer..."
  reqIdRef <- newIORef 1
  forever $ do
    result <- try @SomeException $ runIndexerLoop manager pool cfg reqIdRef
    case result of
      Left err -> do
        putStrLn $ "Indexer error: " <> show err
        threadDelay (icPollInterval cfg * 2)
      Right () -> pure ()

runIndexerLoop :: Manager -> DbPool -> IndexerConfig -> IORef Integer -> IO ()
runIndexerLoop manager pool cfg reqIdRef = do
  lastBlock <- withDb pool getLastIndexedBlock
  let startBlock = max (icStartBlock cfg) (lastBlock + 1)

  eCurrentBlock <- getCurrentBlockNumber manager (icRpcUrl cfg) reqIdRef
  case eCurrentBlock of
    Left err -> do
      putStrLn $ "Failed to get current block: " <> T.unpack err
      threadDelay (icPollInterval cfg)
    Right currentBlock -> do
      if startBlock > currentBlock
        then threadDelay (icPollInterval cfg)
        else do
          let endBlock = min (startBlock + icBatchSize cfg - 1) currentBlock
          putStrLn $ "Indexing blocks " <> show startBlock <> " to " <> show endBlock

          let contracts = getContractAddresses (icAddresses cfg)
          eLogs <- getLogs manager (icRpcUrl cfg) reqIdRef contracts startBlock endBlock
          case eLogs of
            Left err -> do
              putStrLn $ "Failed to get logs: " <> T.unpack err
              threadDelay (icPollInterval cfg)
            Right logs -> do
              let bearContracts = [addrStakingBear (icAddresses cfg), addrLeverageRouter (icAddresses cfg)]
                  bullContracts = [addrStakingBull (icAddresses cfg), addrBullLeverageRouter (icAddresses cfg)]

              forM_ logs $ \log -> do
                let mParsed = parseEventLog log bearContracts bullContracts
                case mParsed of
                  Nothing -> pure ()
                  Just parsed -> do
                    timestamp <- getBlockTimestamp manager (icRpcUrl cfg) reqIdRef (elBlockNumber log)
                    withDb pool $ \conn ->
                      insertTransaction conn
                        (elTxHash log)
                        (elBlockNumber log)
                        timestamp
                        (peUserAddress parsed)
                        (peTxType parsed)
                        (peSide parsed)
                        "success"
                        (peData parsed)

              withDb pool $ \conn -> setLastIndexedBlock conn endBlock
              putStrLn $ "Indexed " <> show (length logs) <> " events through block " <> show endBlock

              when (endBlock < currentBlock) $
                runIndexerLoop manager pool cfg reqIdRef

getContractAddresses :: Addresses -> [Text]
getContractAddresses addrs =
  [ addrSyntheticSplitter addrs
  , addrCurvePool addrs
  , addrZapRouter addrs
  , addrStakingBear addrs
  , addrStakingBull addrs
  , addrLeverageRouter addrs
  , addrBullLeverageRouter addrs
  ]

getCurrentBlockNumber :: Manager -> Text -> IORef Integer -> IO (Either Text Integer)
getCurrentBlockNumber manager rpcUrl reqIdRef = do
  reqId <- nextId reqIdRef
  let payload = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= ("eth_blockNumber" :: Text)
        , "params" .= ([] :: [Value])
        , "id" .= reqId
        ]
  result <- rpcCall manager rpcUrl payload
  pure $ case result of
    Left err -> Left err
    Right (String hex) -> Right $ hexToInteger $ T.drop 2 hex
    Right _ -> Left "Expected hex string"

getBlockTimestamp :: Manager -> Text -> IORef Integer -> Integer -> IO Integer
getBlockTimestamp manager rpcUrl reqIdRef blockNum = do
  reqId <- nextId reqIdRef
  let payload = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= ("eth_getBlockByNumber" :: Text)
        , "params" .= [String $ "0x" <> intToHex blockNum, Bool False]
        , "id" .= reqId
        ]
  result <- rpcCall manager rpcUrl payload
  case result of
    Right (Object obj) ->
      case KM.lookup (Key.fromText "timestamp") obj of
        Just (String ts) -> pure $ hexToInteger $ T.drop 2 ts
        _ -> pure 0
    _ -> pure 0

getLogs :: Manager -> Text -> IORef Integer -> [Text] -> Integer -> Integer -> IO (Either Text [EventLog])
getLogs manager rpcUrl reqIdRef addresses fromBlock toBlock = do
  reqId <- nextId reqIdRef
  let topics = map (String . ("0x" <>) . TE.decodeUtf8 . B16.encode . esTopic) allEventSignatures
      payload = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= ("eth_getLogs" :: Text)
        , "params" .= [object
            [ "address" .= addresses
            , "topics" .= [[Aeson.toJSON topics]]
            , "fromBlock" .= ("0x" <> intToHex fromBlock)
            , "toBlock" .= ("0x" <> intToHex toBlock)
            ]]
        , "id" .= reqId
        ]
  result <- rpcCall manager rpcUrl payload
  pure $ case result of
    Left err -> Left err
    Right (Array arr) -> Right $ map parseLogEntry (toVec arr)
    Right _ -> Left "Expected array of logs"
  where
    toVec v = foldr (:) [] v

parseLogEntry :: Value -> EventLog
parseLogEntry = \case
  Object obj ->
    EventLog
      { elTxHash = getString "transactionHash" obj
      , elBlockNumber = getHexInt "blockNumber" obj
      , elAddress = getString "address" obj
      , elTopics = map decodeHex $ getStringArray "topics" obj
      , elData = decodeHex $ getString "data" obj
      }
  _ -> EventLog "" 0 "" [] ""

getString :: Text -> Aeson.Object -> Text
getString key obj = case KM.lookup (Key.fromText key) obj of
  Just (String s) -> s
  _ -> ""

getHexInt :: Text -> Aeson.Object -> Integer
getHexInt key obj = case KM.lookup (Key.fromText key) obj of
  Just (String s) -> hexToInteger $ T.drop 2 s
  _ -> 0

getStringArray :: Text -> Aeson.Object -> [Text]
getStringArray key obj = case KM.lookup (Key.fromText key) obj of
  Just (Array arr) -> [s | String s <- foldr (:) [] arr]
  _ -> []

decodeHex :: Text -> ByteString
decodeHex txt =
  let stripped = if T.isPrefixOf "0x" txt then T.drop 2 txt else txt
  in case B16.decode (TE.encodeUtf8 $ T.toLower stripped) of
    Right bs -> bs
    Left _ -> ""

rpcCall :: Manager -> Text -> Value -> IO (Either Text Value)
rpcCall manager rpcUrl payload = do
  eResult <- try @SomeException $ do
    req <- parseRequest $ T.unpack rpcUrl
    let req' = req
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS $ Aeson.encode payload
          }
    response <- httpLbs req' manager
    pure $ responseBody response
  case eResult of
    Left err -> pure $ Left $ T.pack $ show err
    Right body ->
      case Aeson.decode body of
        Just (Object obj) ->
          case KM.lookup (Key.fromText "result") obj of
            Just r -> pure $ Right r
            Nothing ->
              case KM.lookup (Key.fromText "error") obj of
                Just errVal -> pure $ Left $ T.pack $ show errVal
                Nothing -> pure $ Left "No result in response"
        _ -> pure $ Left "Invalid JSON response"

nextId :: IORef Integer -> IO Integer
nextId ref = atomicModifyIORef' ref $ \n -> (n + 1, n)

hexToInteger :: Text -> Integer
hexToInteger = T.foldl' (\acc c -> acc * 16 + fromIntegral (hexDigit c)) 0
  where
    hexDigit c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = 0

intToHex :: Integer -> Text
intToHex n = T.pack $ showHex n ""
  where
    showHex 0 s = if null s then "0" else s
    showHex x s = showHex (x `div` 16) (hexChar (x `mod` 16) : s)
    hexChar d
      | d < 10 = toEnum (fromEnum '0' + fromIntegral d)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral d - 10)
