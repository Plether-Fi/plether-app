module Plether.Ethereum.PublicRpcSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, encode, eitherDecode, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.IORef (newIORef, modifyIORef', readIORef)
import qualified Data.Text as T
import Plether.Ethereum.PublicRpc
import Plether.Ethereum.Client (newClient)
import Plether.AA.Pimlico (newPimlicoProxyState)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (status200, statusCode)
import Network.Wai (responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Web.Scotty (scottyApp, post)
import Test.Hspec

call :: Value -> T.Text -> [Value] -> Value
call identifier method params = object
  ["jsonrpc" .= ("2.0" :: T.Text), "id" .= identifier, "method" .= method, "params" .= params]

spec :: Spec
spec = describe "bounded browser execution RPC" $ do
  it "authenticates before upstream access, bounds the body, and correlates batch replies" $ do
    count <- newIORef (0 :: Int)
    let upstream req respond = do
          modifyIORef' count (+1)
          bytes <- strictRequestBody req
          let Right (Object fields) = eitherDecode bytes
          respond $ responseLBS status200 [] $ encode $ object
            ["jsonrpc" .= ("2.0" :: T.Text), "id" .= KM.lookup "id" fields, "result" .= ("0x66eee" :: T.Text)]
    testWithApplication (pure upstream) $ \upstreamPort -> do
      client <- newClient $ "http://127.0.0.1:" <> T.pack (show upstreamPort)
      limits <- newPimlicoProxyState
      app <- scottyApp $ post "/rpc" $ handlePublicRpc 421614 (Just "private-origin-token") client limits
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      testWithApplication (pure app) $ \port -> do
        base <- HTTP.parseRequest $ "http://127.0.0.1:" <> show port <> "/rpc"
        let send token body = HTTP.httpLbs (base
              { HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS body
              , HTTP.requestHeaders = [("X-Plether-AA-Proxy-Token",token),("CF-Connecting-IP","192.0.2.1")]
              }) manager
        denied <- send "forged" $ encode $ call (Number 1) "eth_chainId" []
        statusCode (HTTP.responseStatus denied) `shouldBe` 403
        readIORef count `shouldReturn` 0
        oversized <- send "private-origin-token" $ LBS.replicate 65537 32
        statusCode (HTTP.responseStatus oversized) `shouldBe` 413
        blocked <- send "private-origin-token" $ encode $ call (Number 1) "debug_traceCall" []
        statusCode (HTTP.responseStatus blocked) `shouldBe` 400
        readIORef count `shouldReturn` 0
        reply <- send "private-origin-token" $ encode $ toJSON [call (String "chain") "eth_chainId" [],call (Number 2) "eth_chainId" []]
        statusCode (HTTP.responseStatus reply) `shouldBe` 200
        eitherDecode (HTTP.responseBody reply) `shouldBe` Right (toJSON
          [object ["jsonrpc" .= ("2.0" :: T.Text),"id" .= String "chain","result" .= ("0x66eee" :: T.Text)],
           object ["jsonrpc" .= ("2.0" :: T.Text),"id" .= Number 2,"result" .= ("0x66eee" :: T.Text)]])
        readIORef count `shouldReturn` 2
  it "correlates a valid batch and preserves numeric and string IDs" $ do
    let input = toJSON [call (String "balance") "eth_getBalance" [String "0x123", String "latest"], call (Number 2) "eth_chainId" []]
    fmap (map prcId) (parsePublicRpc input) `shouldBe` Right [String "balance", Number 2]
  it "rejects notifications, duplicate IDs, unknown fields and empty or oversized batches" $ do
    parsePublicRpc (object ["jsonrpc" .= ("2.0" :: T.Text), "method" .= ("eth_chainId" :: T.Text), "params" .= ([] :: [Value])]) `shouldSatisfy` isLeft
    parsePublicRpc (toJSON [call (Number 1) "eth_chainId" [],call (Number 1) "eth_chainId" []]) `shouldSatisfy` isLeft
    parsePublicRpc (toJSON ([] :: [Value])) `shouldSatisfy` isLeft
    parsePublicRpc (toJSON [call (Number $ fromIntegral n) "eth_chainId" [] | n <- [1..21 :: Int]]) `shouldSatisfy` isLeft
  it "rejects signing, administration, traces and managed bundler methods before RPC" $ do
    mapM_ (\method -> parsePublicRpc (call (Number 1) method []) `shouldSatisfy` isLeft)
      ["eth_sendTransaction", "personal_sign", "admin_peers", "debug_traceCall", "debug_traceTransaction", "eth_getUserOperationReceipt"]
  it "caps call gas and refuses state overrides" $ do
    let tx = object ["to" .= ("0x123" :: T.Text)]
    fmap (map prcParams) (parsePublicRpc $ call (Number 1) "eth_call" [tx,String "latest"])
      `shouldBe` Right [toJSON [object ["to" .= ("0x123" :: T.Text),"gas" .= ("0x1e8480" :: T.Text)],String "latest"]]
    parsePublicRpc (call (Number 1) "eth_call" [tx,String "latest",object []]) `shouldSatisfy` isLeft
    parsePublicRpc (call (Number 1) "eth_call" [object ["gas" .= ("0xffffff" :: T.Text)],String "latest"]) `shouldSatisfy` isLeft
  it "requires addressed bounded log scans and bounds fee history" $ do
    let filterAt from to = object ["address" .= ("0x123" :: T.Text),"fromBlock" .= (from :: T.Text),"toBlock" .= (to :: T.Text)]
    parsePublicRpc (call (Number 1) "eth_getLogs" [filterAt "0x1" "0x3e8"]) `shouldBe` Right [PublicRpcCall (Number 1) "eth_getLogs" (toJSON [filterAt "0x1" "0x3e8"])]
    parsePublicRpc (call (Number 1) "eth_getLogs" [filterAt "0x1" "0x3e9"]) `shouldSatisfy` isLeft
    parsePublicRpc (call (Number 1) "eth_getLogs" [object ["fromBlock" .= ("earliest" :: T.Text),"toBlock" .= ("latest" :: T.Text)]]) `shouldSatisfy` isLeft
    parsePublicRpc (call (Number 1) "eth_feeHistory" [String "0x1000",String "latest",toJSON ([] :: [Int])]) `shouldSatisfy` isLeft
