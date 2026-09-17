module Plether.AA.RecoveryReceiptSpec (spec) where

import Data.Aeson (Value(..), object, (.=), encode, eitherDecode, eitherDecodeFileStrict', toJSON)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (Key)
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Either (isLeft)
import qualified Data.Foldable as V
import Numeric (readHex)
import Network.Wai (strictRequestBody, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.HTTP.Types.Status (status200)
import Plether.AA.RecoveryReceipt
import Plether.AA.Gateway (observePreparationInclusion)
import Plether.AA.PaymasterSpec (fixtureConfig)
import Plether.Config (NativeAaConfig(..))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Plether.AA.Paymaster (canonicalQuantity)
import Plether.Database.AaSponsorship (ReceiptLocator(..))
import Plether.Ethereum.Abi (encodeUint256, keccak256)
import Plether.Ethereum.Client (newClient)
import Test.Hspec

spec :: Spec
spec = describe "durable canonical UserOperation receipts" $ do
  mapM_ (\reorged -> it ("reports only canonical observed inclusion, reorged=" <> show reorged) $ do
    let (_,receipt) = fixture True
        app request respond = do
          bytes <- strictRequestBody request
          case eitherDecode bytes of
            Right (Object input) -> do
              let value = case KM.lookup "method" input of
                    Just (String "eth_getUserOperationReceipt") -> object ["receipt" .= receipt]
                    Just (String "eth_getTransactionReceipt") -> receipt
                    Just (String "eth_getBlockByNumber") -> object ["number" .= ("0x64" :: Text),
                      "hash" .= (if reorged then hash "9" else blockHash),"timestamp" .= ("0x100" :: Text),"baseFeePerGas" .= ("0x1" :: Text)]
                    _ -> Null
              respond $ responseLBS status200 [] $ encode $ object ["jsonrpc" .= ("2.0" :: Text),"id" .= KM.lookup "id" input,"result" .= value]
            _ -> respond $ responseLBS status200 [] "{}"
    testWithApplication (pure app) $ \port -> do
      let url = "http://127.0.0.1:" <> T.pack (show port)
      client <- newClient url
      manager <- newManager defaultManagerSettings
      case operation of
        Object payload -> observePreparationInclusion (fixtureConfig {naaPaymasterAddress=paymaster,naaAltoRpcUrl=url}) client manager opHash payload
          `shouldReturn` (if reorged then Nothing else Just (txHash,True))
        _ -> expectationFailure "missing operation") [False,True]
  it "uses the fallback after Alto eviction, restart errors or network failures, but not for a located receipt" $ do
    needsReceiptFallback (Left () :: Either () (Value, ())) `shouldBe` True
    needsReceiptFallback (Right (object ["result" .= Null], ())) `shouldBe` True
    needsReceiptFallback (Right (object ["error" .= object []], ())) `shouldBe` True
    needsReceiptFallback (Right (object ["result" .= object ["success" .= True]], ())) `shouldBe` False
  it "reconstructs both recorded smoke operations from receipt/event fixtures without resubmitting" $ do
    captured <- eitherDecodeFileStrict' "../../scripts/fixtures/aa-recovery-smoke-20260913.json"
    case captured of
      Right (Object root) | Just (Array scenarios) <- KM.lookup "scenarios" root -> do
        let textAt key (Object fields) | Just (String value) <- KM.lookup key fields = value
            textAt _ _ = error "invalid smoke fixture text"
            quantityAt key value = case readHex $ T.unpack $ T.drop 2 $ textAt key value of
              [(number,"")] -> number
              _ -> error "invalid smoke fixture quantity"
            valueAt key (Object fields) = maybe Null id $ KM.lookup key fields
            valueAt _ _ = Null
            pm = textAt "paymaster" (Object root)
            account = textAt "sender" (Object root)
        mapM_ (\scenario -> do
          let receipt = valueAt "receipt" scenario
              expected = textAt "operationHash" scenario
              logs = case valueAt "logs" receipt of Array values -> V.toList values; _ -> []
              matching value = textAt "address" value == entryPoint && case valueAt "topics" value of
                Array topics -> take 2 (V.toList topics) == [String opTopic,String expected]
                _ -> False
          case filter matching logs of
            [included] -> do
              let nonce = case valueAt "nonce" scenario of Number n -> floor n; _ -> error "invalid nonce"
                  payload = T.drop 2 $ textAt "data" included
                  cost = case readHex $ T.unpack $ T.take 64 $ T.drop 128 payload of [(n,"")] -> n; _ -> 0
                  locator = ReceiptLocator account nonce (textAt "transactionHash" receipt)
                    (quantityAt "blockNumber" receipt) (textAt "blockHash" receipt) True cost included
                    (set "sender" (String account) $ set "nonce" (String $ canonicalQuantity nonce) operation)
              case reconstructReceipt pm expected locator receipt of
                Right (Object result) -> KM.lookup "success" result `shouldBe` Just (Bool True)
                result -> expectationFailure $ show result
            _ -> expectationFailure "missing exact fixture event") scenarios
      _ -> expectationFailure "missing smoke receipt fixture"
  mapM_ (\success -> it ("reconstructs event success=" <> show success <> " independently of bundle status") $ do
    let (locator,receipt) = fixture success
    case reconstructReceipt paymaster opHash locator receipt of
      Left err -> expectationFailure $ T.unpack err
      Right (Object result) -> do
        KM.lookup "success" result `shouldBe` Just (Bool success)
        KM.lookup "entryPoint" result `shouldBe` Just (String entryPoint)
        KM.lookup "logs" result `shouldBe` Just (toLogs [event success])
      _ -> expectationFailure "expected standard receipt object") [True,False]
  it "ignores provider-specific extensions in stored evidence" $ do
    let (locator,receipt) = fixture True
    reconstructReceipt paymaster opHash locator {rlEvent = set "extra" Null $ rlEvent locator} receipt
      `shouldBe` reconstructReceipt paymaster opHash locator receipt
  it "returns only this operation's logs in a multi-operation bundle" $ do
    let (locator,receipt) = fixture True
        previous = set "logIndex" (String "0x1") $ set "topics"
          (toLogs [String opTopic, String $ hash "9", String $ addressTopic sender, String $ addressTopic paymaster]) $ event True
    case reconstructReceipt paymaster opHash locator $ set "logs" (toLogs [boundary,previous,event True]) receipt of
      Right (Object result) -> KM.lookup "logs" result `shouldBe` Just (toLogs [event True])
      other -> expectationFailure $ show other
  mapM_ (\(name,mutate) -> it ("rejects " <> name) $ do
    let (locator,receipt) = fixture True
    reconstructReceipt paymaster opHash locator (set "logs" (toLogs $ mutate $ event True) receipt)
      `shouldSatisfy` isLeft)
    [ ("duplicate events", \e -> [boundary,e,set "logIndex" (String "0x3") e])
    , ("wrong emitter", \e -> [boundary,set "address" (String sender) e])
    , ("removed event", \e -> [boundary,set "removed" (Bool True) e])
    , ("wrong nonce", \e -> [boundary,set "data" (String $ eventData 8 True) e])
    , ("wrong transaction", \e -> [boundary,set "transactionHash" (String $ hash "9") e])
    , ("wrong block", \e -> [boundary,set "blockHash" (String $ hash "9") e])
    , ("missing bundle boundary", \e -> [e])
    ]
  it "rejects conflicting authorization, hash and paymaster" $ do
    let (locator,receipt) = fixture True
    reconstructReceipt paymaster (hash "9") locator receipt `shouldSatisfy` isLeft
    reconstructReceipt sender opHash locator receipt `shouldSatisfy` isLeft
    reconstructReceipt paymaster opHash locator {rlNonce = 8} receipt `shouldSatisfy` isLeft
    reconstructReceipt paymaster opHash locator {rlSender = paymaster} receipt `shouldSatisfy` isLeft
    reconstructReceipt paymaster opHash locator {rlSuccess = False} receipt `shouldSatisfy` isLeft
  mapM_ (\mode -> it ("checks canonical RPC boundaries: " <> T.unpack mode) $ do
    let (locator,receipt) = fixture True
        header n h = object ["number" .= (n :: Text),"hash" .= (h :: Text),"timestamp" .= ("0x3e8" :: Text)]
        app request respond = do
          body <- strictRequestBody request
          let (identifier,result) = case eitherDecode body of
                Right (Object fields) -> (KM.lookup "id" fields, case KM.lookup "method" fields of
                  Just (String "eth_chainId") -> String $ if mode == "wrong-chain" then "0xa4b1" else "0x66eee"
                  Just (String "eth_getTransactionReceipt") -> if mode == "missing" then Null else receipt
                  Just (String "eth_getBlockByNumber") -> case KM.lookup "params" fields of
                    Just params | params == toLogs [String "0x64",Bool False] -> header "0x64" $ if mode == "reorg" then hash "9" else blockHash
                    _ -> header "0x65" $ hash "8"
                  _ -> Null)
                _ -> (Nothing,Null)
          respond $ responseLBS status200 [] $ encode $ object ["jsonrpc" .= ("2.0" :: Text),"id" .= identifier,"result" .= result]
    testWithApplication (pure app) $ \port -> do
      client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
      result <- recoverReceipt client 421614 paymaster opHash locator
      if mode == "canonical" then result `shouldBe` reconstructReceipt paymaster opHash locator receipt
        else result `shouldSatisfy` isLeft) ["canonical","reorg","wrong-chain","missing"]

set :: Key -> Value -> Value -> Value
set key value (Object fields) = Object $ KM.insert key value fields
set _ _ value = value
toLogs :: [Value] -> Value
toLogs = toJSON
hash :: Text -> Text
hash value = "0x" <> T.replicate 64 value
addressTopic :: Text -> Text
addressTopic address = "0x" <> T.replicate 24 "0" <> T.drop 2 address
entryPoint, sender, paymaster, opHash, txHash, blockHash, opTopic :: Text
entryPoint = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108" :: Text
sender = "0x3333333333333333333333333333333333333333" :: Text
paymaster = "0x1111111111111111111111111111111111111111" :: Text
opHash = hash "4"
txHash = hash "5"
blockHash = hash "6"
opTopic = "0x" <> TE.decodeUtf8 (B16.encode $ keccak256 "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)")
eventData :: Integer -> Bool -> Text
eventData nonce success = "0x" <> TE.decodeUtf8 (B16.encode $ encodeUint256 nonce <> encodeUint256 (if success then 1 else 0) <> encodeUint256 1000 <> encodeUint256 900)
event :: Bool -> Value
event success = object ["address" .= entryPoint,"topics" .= [opTopic,opHash,addressTopic sender,addressTopic paymaster],
  "data" .= eventData 7 success,"transactionHash" .= txHash,"blockNumber" .= ("0x64" :: Text),
  "blockHash" .= blockHash,"logIndex" .= ("0x2" :: Text),"removed" .= False]
boundary :: Value
boundary = set "logIndex" (String "0x0") $ set "topics"
  (toLogs [String $ "0x" <> TE.decodeUtf8 (B16.encode $ keccak256 "BeforeExecution()")]) $ event True
fixture :: Bool -> (ReceiptLocator, Value)
fixture success = (ReceiptLocator sender 7 txHash 100 blockHash success 1000 (event success) operation,
  object ["transactionHash" .= txHash,"blockNumber" .= ("0x64" :: Text),"blockHash" .= blockHash,
    "status" .= ("0x1" :: Text),"logs" .= [boundary,event success]])
operation :: Value
operation = object ["sender" .= sender,"nonce" .= ("0x7" :: Text),"callData" .= ("0x" :: Text),
  "callGasLimit" .= ("0x100000" :: Text),"verificationGasLimit" .= ("0x186a0" :: Text),
  "preVerificationGas" .= ("0xea60" :: Text),"maxFeePerGas" .= ("0x1" :: Text),"maxPriorityFeePerGas" .= ("0x1" :: Text)]
