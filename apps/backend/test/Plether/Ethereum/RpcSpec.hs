module Plether.Ethereum.RpcSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256)
import Plether.Ethereum.Client (EthClient, RpcError (..), ethBlockNumber, newClient)
import Plether.Ethereum.Contracts.SettlementMonitor
  ( SettlementDeployment (..)
  , SettlementCodeHashes (..)
  , getCurrentEpochAtBlock
  , supportedConfigSchemaVersion
  , verifySettlementDeployment
  )
import Plether.Ethereum.Rpc
import Test.Hspec

spec :: Spec
spec =
  describe "JSON-RPC keeper primitives" $ do
    it "reads chain id and deployed bytecode" $
      withRpcClient $ \client -> do
        ethChainId client `shouldReturn` Right 421_614
        ethGetCode client housePool `shouldReturn` Right (BS.pack [0x60, 0x01, 0x60, 0x00])

    it "estimates gas with the exact transaction object at an explicit block" $
      withRpcClient $ \client ->
        ethEstimateGasAtBlock client estimateFrom estimateTo 7 (BS.pack [0x01, 0x02]) 123
          `shouldReturn` Right 21_000

    it "fails closed on malformed estimate results and negative values or block numbers" $ do
      withRpcResult "eth_estimateGas" (String "0xgg") $ \client -> do
        result <- ethEstimateGasAtBlock client estimateFrom estimateTo 7 BS.empty 123
        result `shouldSatisfy` isRpcJsonError
      withRpcClient $ \client -> do
        negativeValue <- ethEstimateGasAtBlock client estimateFrom estimateTo (-1) BS.empty 123
        negativeBlock <- ethEstimateGasAtBlock client estimateFrom estimateTo 0 BS.empty (-1)
        negativeValue `shouldSatisfy` isRpcJsonError
        negativeBlock `shouldSatisfy` isRpcJsonError

    it "retains receipt and log block identity" $
      withRpcClient $ \client -> do
        result <- ethGetTransactionReceipt client txHash
        case result of
          Right (Just receipt) -> do
            receiptBlockHash receipt `shouldBe` blockHash
            receiptTransactionIndex receipt `shouldBe` 3
            case receiptLogs receipt of
              [entry] -> do
                rpcLogBlockHash entry `shouldBe` blockHash
                rpcLogTransactionIndex entry `shouldBe` 3
                rpcLogIndex entry `shouldBe` 7
              entries -> expectationFailure $ "unexpected receipt logs: " <> show entries
          other -> expectationFailure $ "unexpected receipt result: " <> show other

    it "verifies the complete settlement deployment graph and bytecode" $
      withRpcClient $ \client ->
        verifySettlementDeployment client deployment mockCodeHashes `shouldReturn` Right ()

    it "fails deployment verification when an execution-critical contract has no code" $
      withRpcClientFor (rpcApplication (Just juniorVault) []) $ \client -> do
        result <- verifySettlementDeployment client deployment mockCodeHashes
        result `shouldSatisfy` isLeft

    it "fails deployment verification when reviewed runtime bytecode drifts" $
      withRpcClient $ \client -> do
        let wrongHashes =
              mockCodeHashes
                { schRouter = "0x0000000000000000000000000000000000000000000000000000000000000000"
                }
        result <- verifySettlementDeployment client deployment wrongHashes
        result `shouldSatisfy` isLeft

    it "reads the HousePool epoch at the requested exact block" $
      withRpcClient $ \client ->
        getCurrentEpochAtBlock client housePool 123 `shouldReturn` Right 500_000

    it "rejects shifted, malformed, and odd-length eth_call data" $
      mapM_
        ( \malformed ->
            withRpcResult "eth_call" (String malformed) $ \client -> do
              result <- getCurrentEpochAtBlock client housePool 123
              result `shouldSatisfy` isRpcJsonError
        )
        [ TE.decodeUtf8 $ B16.encode $ encodeUint256 500_000
        , "0xgg"
        , "0x0"
        ]

    it "keeps null as the only pending-receipt representation" $
      withRpcResult "eth_getTransactionReceipt" Null $ \client ->
        ethGetTransactionReceipt client txHash `shouldReturn` Right Nothing

    it "accepts a canonical reverted receipt with an empty log array" $
      let reverted =
            setObjectField "logs" (toJSON ([] :: [Value])) $
              setObjectField "status" (String "0x0") receiptValue
       in withRpcResult "eth_getTransactionReceipt" reverted $ \client -> do
            result <- ethGetTransactionReceipt client txHash
            case result of
              Right (Just receipt) -> do
                receiptSucceeded receipt `shouldBe` False
                receiptLogs receipt `shouldBe` []
              other -> expectationFailure $ "unexpected reverted receipt result: " <> show other

    it "rejects missing or malformed receipt identity, quantity, status, and logs fields" $
      mapM_
        ( \(label, malformed) ->
            withRpcResult "eth_getTransactionReceipt" malformed $ \client -> do
              result <- ethGetTransactionReceipt client txHash
              expectRpcJsonError label result
        )
        [ ("missing transaction hash", removeObjectField "transactionHash" receiptValue)
        , ("short transaction hash", setObjectField "transactionHash" (String "0x12") receiptValue)
        , ("mismatched transaction hash", setObjectField "transactionHash" (String otherTxHash) receiptValue)
        , ("missing block number", removeObjectField "blockNumber" receiptValue)
        , ("malformed block number", setObjectField "blockNumber" (String "0xgg") receiptValue)
        , ("non-canonical block number", setObjectField "blockNumber" (String "0x07b") receiptValue)
        , ("missing block hash", removeObjectField "blockHash" receiptValue)
        , ("short block hash", setObjectField "blockHash" (String "0x12") receiptValue)
        , ("missing transaction index", removeObjectField "transactionIndex" receiptValue)
        , ("malformed transaction index", setObjectField "transactionIndex" (String "0xgg") receiptValue)
        , ("missing status", removeObjectField "status" receiptValue)
        , ("malformed status", setObjectField "status" (String "0xgg") receiptValue)
        , ("out-of-range status", setObjectField "status" (String "0x2") receiptValue)
        , ("missing logs", removeObjectField "logs" receiptValue)
        , ("non-array logs", setObjectField "logs" Null receiptValue)
        ]

    it "rejects every malformed log field without silently defaulting or dropping topics" $
      mapM_
        ( \(label, malformedLog) ->
            withRpcResult "eth_getTransactionReceipt" (receiptWithLog malformedLog) $ \client -> do
              result <- ethGetTransactionReceipt client txHash
              expectRpcJsonError label result
        )
        [ ("non-object log", Null)
        , ("missing log transaction hash", removeObjectField "transactionHash" logValue)
        , ("short log transaction hash", setObjectField "transactionHash" (String "0x12") logValue)
        , ("mismatched log transaction hash", setObjectField "transactionHash" (String otherTxHash) logValue)
        , ("missing log block number", removeObjectField "blockNumber" logValue)
        , ("invalid log block number", setObjectField "blockNumber" (String "0xgg") logValue)
        , ("mismatched log block number", setObjectField "blockNumber" (String "0x7c") logValue)
        , ("missing log block hash", removeObjectField "blockHash" logValue)
        , ("short log block hash", setObjectField "blockHash" (String "0x12") logValue)
        , ("mismatched log block hash", setObjectField "blockHash" (String otherBlockHash) logValue)
        , ("missing log transaction index", removeObjectField "transactionIndex" logValue)
        , ("invalid log transaction index", setObjectField "transactionIndex" (String "0xgg") logValue)
        , ("mismatched log transaction index", setObjectField "transactionIndex" (String "0x4") logValue)
        , ("missing log index", removeObjectField "logIndex" logValue)
        , ("invalid log index", setObjectField "logIndex" (String "0xgg") logValue)
        , ("missing log address", removeObjectField "address" logValue)
        , ("short log address", setObjectField "address" (String "0x12") logValue)
        , ("missing log topics", removeObjectField "topics" logValue)
        , ("non-array log topics", setObjectField "topics" Null logValue)
        , ( "non-string log topic"
          , setObjectField "topics" (toJSON [String txHash, Number 1]) logValue
          )
        , ("short log topic", setObjectField "topics" (toJSON [String "0x12"]) logValue)
        , ("invalid log topic hex", setObjectField "topics" (toJSON [String "0xgg"]) logValue)
        , ("missing log data", removeObjectField "data" logValue)
        , ("invalid log data hex", setObjectField "data" (String "0xgg") logValue)
        , ("odd-length log data hex", setObjectField "data" (String "0x0") logValue)
        ]

    it "rejects a malformed eth_getLogs entry instead of advancing with a default log" $
      withRpcResult "eth_getLogs" (toJSON [Null]) $ \client -> do
        result <- ethGetLogs client housePool [] 100 101
        result `shouldSatisfy` isRpcJsonError

    it "rejects malformed general JSON-RPC quantities instead of decoding them as zero" $
      withRpcResult "eth_chainId" (String "0xnot-hex") $ \client -> do
        result <- ethChainId client
        result `shouldSatisfy` isRpcJsonError

    it "rejects malformed and non-canonical chain-head quantities" $
      mapM_
        ( \malformed ->
            withRpcResult "eth_blockNumber" (String malformed) $ \client -> do
              result <- ethBlockNumber client
              result `shouldSatisfy` isRpcJsonError
        )
        ["0x", "0x01", "0xgg", "1"]

    it "accepts the requested block only when its decoded number matches" $
      withRpcClient $ \client ->
        ethGetBlockByNumber client 123
          `shouldReturn` Right (RpcBlock 123 blockHash 1_800_000_000)

    it "rejects mismatched or malformed block identities and quantities" $
      mapM_
        ( \(label, malformed) ->
            withRpcResult "eth_getBlockByNumber" malformed $ \client -> do
              result <- ethGetBlockByNumber client 123
              expectRpcJsonError label result
        )
        [ ("different response number", blockValue 124)
        , ("invalid response number", setObjectField "number" (String "0xgg") $ blockValue 123)
        , ("non-canonical response number", setObjectField "number" (String "0x07b") $ blockValue 123)
        , ("short response hash", setObjectField "hash" (String "0x12") $ blockValue 123)
        , ("invalid response timestamp", setObjectField "timestamp" (String "0xgg") $ blockValue 123)
        ]

withRpcClient :: (EthClient -> IO a) -> IO a
withRpcClient = withRpcClientFor $ rpcApplication Nothing []

withRpcResult :: Text -> Value -> (EthClient -> IO a) -> IO a
withRpcResult method result = withRpcClientFor $ rpcApplication Nothing [(method, result)]

withRpcClientFor :: Application -> (EthClient -> IO a) -> IO a
withRpcClientFor application action =
  testWithApplication (pure application) $ \port -> do
    client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
    action client

rpcApplication :: Maybe Text -> [(Text, Value)] -> Application
rpcApplication missingCodeAddress overrides request respond = do
  body <- strictRequestBody request
  let result = case rpcRequest body of
        Just method
          | Just override <- lookup method overrides -> override
        Just "eth_chainId" -> String "0x66eee"
        Just "eth_getCode"
          | rpcFirstParam body == missingCodeAddress -> String "0x"
          | otherwise -> String "0x60016000"
        Just "eth_call" -> maybe Null String $ ethCallResult body
        Just "eth_estimateGas"
          | isExpectedEstimateGasRequest body -> String "0x5208"
          | otherwise -> String "0xinvalid-request"
        Just "eth_getTransactionReceipt" -> receiptValue
        Just "eth_getBlockByNumber" -> blockValue 123
        _ -> Null
  respond $
    responseLBS
      status200
      [("Content-Type", "application/json")]
      (encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Integer), "result" .= result])

rpcMethod :: LBS.ByteString -> Maybe Text
rpcMethod body = do
  Object value <- decode body
  String method <- KeyMap.lookup (Key.fromText "method") value
  pure method

rpcRequest :: LBS.ByteString -> Maybe Text
rpcRequest = rpcMethod

rpcFirstParam :: LBS.ByteString -> Maybe Text
rpcFirstParam body = do
  Object value <- decode body
  Array params <- KeyMap.lookup (Key.fromText "params") value
  String first <- case toList params of
    first : _ -> Just first
    [] -> Nothing
  pure first

isExpectedEstimateGasRequest :: LBS.ByteString -> Bool
isExpectedEstimateGasRequest body =
  case decode body of
    Just (Object value) ->
      case KeyMap.lookup (Key.fromText "params") value of
        Just (Array params) ->
          toList params
            == [ object
                   [ "from" .= estimateFrom
                   , "to" .= estimateTo
                   , "value" .= ("0x7" :: Text)
                   , "data" .= ("0x0102" :: Text)
                   ]
               , String "0x7b"
               ]
        _ -> False
    _ -> False

ethCallResult :: LBS.ByteString -> Maybe Text
ethCallResult body = do
  Object value <- decode body
  Array params <- KeyMap.lookup (Key.fromText "params") value
  (target, calldata, blockTag) <- case toList params of
    [Object call, String tag] -> do
      String target <- KeyMap.lookup (Key.fromText "to") call
      String calldata <- KeyMap.lookup (Key.fromText "data") call
      pure (T.toLower target, calldata, tag)
    _ -> Nothing
  result <- case (target, calldata, blockTag) of
    (address, call, _)
      | address == monitor && call == callHex "CONFIG_SCHEMA_VERSION()" ->
          Just $ encodeUint256 supportedConfigSchemaVersion
      | address == monitor && call == callHex "ROUTER()" -> Just $ encodeAddress router
      | address == monitor && call == callHex "ENGINE()" -> Just $ encodeAddress engine
      | address == monitor && call == callHex "HOUSE_POOL()" -> Just $ encodeAddress housePool
      | address == monitor && call == callHex "SENIOR_VAULT()" -> Just $ encodeAddress seniorVault
      | address == monitor && call == callHex "JUNIOR_VAULT()" -> Just $ encodeAddress juniorVault
      | address == router && call == callHex "engine()" -> Just $ encodeAddress engine
      | address == router && call == callHex "pletherOracle()" -> Just $ encodeAddress pletherOracle
      | address == engine && call == callHex "orderRouter()" -> Just $ encodeAddress router
      | address == housePool && call == callHex "ENGINE()" -> Just $ encodeAddress engine
      | address == housePool && call == callHex "seniorVault()" -> Just $ encodeAddress seniorVault
      | address == housePool && call == callHex "juniorVault()" -> Just $ encodeAddress juniorVault
      | address == seniorVault && call == callHex "POOL()" -> Just $ encodeAddress housePool
      | address == juniorVault && call == callHex "POOL()" -> Just $ encodeAddress housePool
      | address == seniorVault && call == callHex "IS_SENIOR()" -> Just $ encodeUint256 1
      | address == juniorVault && call == callHex "IS_SENIOR()" -> Just $ encodeUint256 0
      | address == housePool && call == callHex "currentLpEpoch()" && blockTag == "0x7b" ->
          Just $ encodeUint256 500_000
    _ -> Nothing
  pure $ bytesHex result

callHex :: Text -> Text
callHex signature = bytesHex $ encodeCall signature []

bytesHex :: BS.ByteString -> Text
bytesHex = ("0x" <>) . TE.decodeUtf8 . B16.encode

receiptValue :: Value
receiptValue =
  object
    [ "transactionHash" .= txHash
    , "blockNumber" .= ("0x7b" :: Text)
    , "blockHash" .= blockHash
    , "transactionIndex" .= ("0x3" :: Text)
    , "status" .= ("0x1" :: Text)
    , "logs" .= [logValue]
    ]

logValue :: Value
logValue =
  object
    [ "transactionHash" .= txHash
    , "blockNumber" .= ("0x7b" :: Text)
    , "blockHash" .= blockHash
    , "transactionIndex" .= ("0x3" :: Text)
    , "logIndex" .= ("0x7" :: Text)
    , "address" .= housePool
    , "topics" .= ([] :: [Text])
    , "data" .= ("0x" :: Text)
    ]

receiptWithLog :: Value -> Value
receiptWithLog entry = setObjectField "logs" (toJSON [entry]) receiptValue

blockValue :: Integer -> Value
blockValue number =
  object
    [ "number" .= quantityHex number
    , "hash" .= blockHash
    , "timestamp" .= quantityHex 1_800_000_000
    ]

quantityHex :: Integer -> Text
quantityHex value = "0x" <> T.pack (showHex value)

showHex :: Integer -> String
showHex value
  | value < 16 = [hexDigit value]
  | otherwise = showHex (value `div` 16) <> [hexDigit $ value `mod` 16]
 where
  hexDigit digit
    | digit < 10 = toEnum $ fromEnum '0' + fromIntegral digit
    | otherwise = toEnum $ fromEnum 'a' + fromIntegral digit - 10

setObjectField :: Text -> Value -> Value -> Value
setObjectField field value (Object values) =
  Object $ KeyMap.insert (Key.fromText field) value values
setObjectField _ _ other = other

removeObjectField :: Text -> Value -> Value
removeObjectField field (Object values) = Object $ KeyMap.delete (Key.fromText field) values
removeObjectField _ other = other

deployment :: SettlementDeployment
deployment =
  SettlementDeployment
    { sdConfigSchemaVersion = supportedConfigSchemaVersion
    , sdMonitor = monitor
    , sdRouter = router
    , sdEngine = engine
    , sdHousePool = housePool
    , sdSeniorVault = seniorVault
    , sdJuniorVault = juniorVault
    , sdPletherOracle = pletherOracle
    }

mockCodeHashes :: SettlementCodeHashes
mockCodeHashes =
  SettlementCodeHashes
    { schMonitor = mockCodeHash
    , schRouter = mockCodeHash
    , schEngine = mockCodeHash
    , schHousePool = mockCodeHash
    , schSeniorVault = mockCodeHash
    , schJuniorVault = mockCodeHash
    , schPletherOracle = mockCodeHash
    }

mockCodeHash :: Text
mockCodeHash = "0xcf61a6eb3b9b89e75f1dadf3dcd16509616896cb50eac765a68fa27bbbc6de82"

monitor, router, engine, housePool, seniorVault, juniorVault, pletherOracle :: Text
monitor = "0x1111111111111111111111111111111111111111"
router = "0x2222222222222222222222222222222222222222"
engine = "0x3333333333333333333333333333333333333333"
housePool = "0x4444444444444444444444444444444444444444"
seniorVault = "0x5555555555555555555555555555555555555555"
juniorVault = "0x6666666666666666666666666666666666666666"
pletherOracle = "0x7777777777777777777777777777777777777777"

estimateFrom, estimateTo :: Text
estimateFrom = "0x8888888888888888888888888888888888888888"
estimateTo = "0x9999999999999999999999999999999999999999"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRpcJsonError :: Either RpcError a -> Bool
isRpcJsonError (Left (RpcJsonError _)) = True
isRpcJsonError _ = False

expectRpcJsonError :: Show a => Text -> Either RpcError a -> Expectation
expectRpcJsonError _ (Left (RpcJsonError _)) = pure ()
expectRpcJsonError label other =
  expectationFailure $ T.unpack label <> ": expected RpcJsonError, observed " <> show other

txHash :: Text
txHash = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

otherTxHash :: Text
otherTxHash = "0xcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

blockHash :: Text
blockHash = "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

otherBlockHash :: Text
otherBlockHash = "0xdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
