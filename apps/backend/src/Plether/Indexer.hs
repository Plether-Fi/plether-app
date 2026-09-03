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
import qualified Data.ByteString.Base16 as B16
import Data.IORef (newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (Manager)
import Data.List (nub)
import Plether.Config (Addresses (..), Deployment (..))
import Plether.Utils.Hex (hexToInteger, intToHex)
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema (getLastIndexedBlock, insertTransaction, setLastIndexedBlock)
import Plether.Indexer.Contracts (allEventSignatures, esTopic)
import Plether.Indexer.Events (EventLog (..), MorphoMarkets (..), ParsedEvent (..), parseEventLog)
import Plether.Logging (field, logErrorEvery, logInfo, logInfoEvery, logWarnEvery)
import Plether.Ethereum.Client
  ( EthClient
  , RpcClientOptions (..)
  , newClientWithManager
  , rpcCall
  )

data IndexerConfig = IndexerConfig
  { icRpcUrl :: Text
  , icRpcAuthToken :: Maybe Text
  , icDeployments :: [Deployment]
  , icStartBlock :: Integer
  , icBatchSize :: Integer
  , icPollInterval :: Int
  }

startIndexer :: Manager -> DbPool -> IndexerConfig -> IO ()
startIndexer manager pool cfg = do
  logInfo
    "ethereum_indexer_started"
    "Ethereum event indexer started"
    [ field "start_block" $ icStartBlock cfg
    , field "batch_size" $ icBatchSize cfg
    , field "poll_interval_ms" $ icPollInterval cfg `div` 1000
    ]
  reqIdRef <- newIORef 1
  client <-
    newClientWithManager
      manager
      reqIdRef
      (RpcClientOptions (icRpcUrl cfg) (icRpcAuthToken cfg) "core-indexer")
  forever $ do
    result <- try @SomeException $ runIndexerLoop pool cfg client
    case result of
      Left err -> do
        logErrorEvery
          60
          "ethereum_indexer_iteration_failed"
          "Ethereum indexer iteration failed"
          [field "error" $ show err]
        threadDelay (icPollInterval cfg * 2)
      Right () -> pure ()

runIndexerLoop :: DbPool -> IndexerConfig -> EthClient -> IO ()
runIndexerLoop pool cfg client = do
  lastBlock <- withDb pool getLastIndexedBlock
  let startBlock = max (icStartBlock cfg) (lastBlock + 1)

  eCurrentBlock <- getCurrentBlockNumber client
  case eCurrentBlock of
    Left err -> do
      logWarnEvery
        60
        "ethereum_indexer_head_fetch_failed"
        "Ethereum indexer could not fetch the current block"
        [field "error" err]
      threadDelay (icPollInterval cfg)
    Right currentBlock -> do
      if startBlock > currentBlock
        then threadDelay (icPollInterval cfg)
        else do
          let endBlock = min (startBlock + icBatchSize cfg - 1) currentBlock

          let allAddrs = map deployAddresses (icDeployments cfg)
              contracts = nub $ concatMap getContractAddresses allAddrs
          eLogs <- getLogs client contracts startBlock endBlock
          case eLogs of
            Left err -> do
              logWarnEvery
                60
                "ethereum_indexer_logs_fetch_failed"
                "Ethereum indexer could not fetch event logs"
                [ field "from_block" startBlock
                , field "to_block" endBlock
                , field "error" err
                ]
              threadDelay (icPollInterval cfg)
            Right logs -> do
              let bearContracts = nub $ concatMap (\a -> [addrStakingBear a, addrLeverageRouter a]) allAddrs
                  bullContracts = nub $ concatMap (\a -> [addrStakingBull a, addrBullLeverageRouter a]) allAddrs
                  morphoMarkets = MorphoMarkets
                    { mmBearMarketIds = nub $ map addrMorphoMarketBear allAddrs
                    , mmBullMarketIds = nub $ map addrMorphoMarketBull allAddrs
                    }

              forM_ logs $ \log -> do
                let mParsed = parseEventLog log bearContracts bullContracts morphoMarkets
                case mParsed of
                  Nothing -> pure ()
                  Just parsed -> do
                    timestamp <- getBlockTimestamp client (elBlockNumber log)
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
              logInfoEvery
                300
                "ethereum_indexer_progress"
                "Ethereum event indexer advanced"
                [ field "from_block" startBlock
                , field "to_block" endBlock
                , field "chain_head_block" currentBlock
                , field "event_count" $ length logs
                ]

              when (endBlock < currentBlock) $
                runIndexerLoop pool cfg client

getContractAddresses :: Addresses -> [Text]
getContractAddresses addrs =
  [ addrSyntheticSplitter addrs
  , addrCurvePool addrs
  , addrZapRouter addrs
  , addrStakingBear addrs
  , addrStakingBull addrs
  , addrLeverageRouter addrs
  , addrBullLeverageRouter addrs
  , addrMorpho addrs
  ]

getCurrentBlockNumber :: EthClient -> IO (Either Text Integer)
getCurrentBlockNumber client = do
  result <- rpcCall client "eth_blockNumber" $ Aeson.toJSON ([] :: [Value])
  pure $ case result of
    Left err -> Left $ T.pack $ show err
    Right (String hex) -> Right $ hexToInteger $ T.drop 2 hex
    Right _ -> Left "Expected hex string"

getBlockTimestamp :: EthClient -> Integer -> IO Integer
getBlockTimestamp client blockNum = do
  result <-
    rpcCall
      client
      "eth_getBlockByNumber"
      (Aeson.toJSON [String $ "0x" <> intToHex blockNum, Bool False])
  case result of
    Right (Object obj) ->
      case KM.lookup (Key.fromText "timestamp") obj of
        Just (String ts) -> pure $ hexToInteger $ T.drop 2 ts
        _ -> pure 0
    _ -> pure 0

getLogs :: EthClient -> [Text] -> Integer -> Integer -> IO (Either Text [EventLog])
getLogs client addresses fromBlock toBlock = do
  let topics = map (String . ("0x" <>) . TE.decodeUtf8 . B16.encode . esTopic) allEventSignatures
      params = Aeson.toJSON
        [ object
            [ "address" .= addresses
            , "topics" .= [topics]
            , "fromBlock" .= ("0x" <> intToHex fromBlock)
            , "toBlock" .= ("0x" <> intToHex toBlock)
            ]
        ]
  result <- rpcCall client "eth_getLogs" params
  pure $ case result of
    Left err -> Left $ T.pack $ show err
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
