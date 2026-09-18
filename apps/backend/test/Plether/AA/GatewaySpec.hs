module Plether.AA.GatewaySpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value(..), object, (.=), encode, eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Data.Either (isLeft)
import Network.Wai (strictRequestBody, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.HTTP.Types (status200)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Plether.AA.Gateway (forwardAlto, classifyAltoResult, securityFailureCategory)
import qualified Plether.AA.Pimlico as Rpc
import Test.Hspec

spec :: Spec
spec = describe "Alto request ID normalization" $ do
  it "classifies security failures with bounded, credential-free categories" $ do
    securityFailureCategory "the dual-provider security snapshot is stale" `shouldBe` "SNAPSHOT_STALE"
    securityFailureCategory "security RPC providers disagree on the explicit block header" `shouldBe` "PROVIDER_HEADER_DISAGREEMENT"
    securityFailureCategory "primary security RPC: https://secret.invalid/key" `shouldBe` "PRIMARY_HEADER_READ_FAILED"
    securityFailureCategory "raw signed operation or secret" `shouldBe` "ATTESTATION_UNAVAILABLE"
  forM_ [("0x024ec6ee", "INSUFFICIENT_FREE_EQUITY"), ("0xe37e62c6", "INVALID_ORDER_DEADLINE"),
         ("0x2ae052ed" <> T.replicate 63 "0" <> "1", "MUST_CLOSE_OPPOSING"),
         ("0x2ae052ed" <> T.replicate 63 "0" <> "2", "SIMULATION_FAILED"),
         ("0x2ae052ed01", "SIMULATION_FAILED"),
         ("0x2ae052ed" <> T.replicate 63 "0" <> "100", "SIMULATION_FAILED"),
         ("0xdeadbeef", "SIMULATION_FAILED")] $ \(selector, reason) ->
    it ("classifies an actual Alto simulation reply as " <> T.unpack reason) $ do
      let app _ respond = respond $ responseLBS status200 [] $ encode $ object
            ["jsonrpc" .= ("2.0" :: T.Text), "id" .= (1 :: Int), "error" .= object
              ["code" .= (-32521 :: Int), "message" .= ("UserOperation reverted during simulation with reason: " <> selector :: T.Text),
               "data" .= ("private calldata" :: T.Text)]]
      testWithApplication (pure app) $ \port -> do
        manager <- newManager defaultManagerSettings
        let Right request = Rpc.parseRpcRequest $ object ["jsonrpc" .= ("2.0" :: T.Text),
              "id" .= (1 :: Int), "method" .= ("eth_estimateUserOperationGas" :: T.Text)]
        response <- forwardAlto manager ("http://127.0.0.1:" <> T.pack (show port)) request
        case response of
          Right (value, _) -> case classifyAltoResult "eth_estimateUserOperationGas" value of
            Left failure -> do
              Rpc.pfReason failure `shouldBe` reason
              Rpc.pfRetryable failure `shouldBe` False
              T.isInfixOf "private" (Rpc.pfMessage failure) `shouldBe` False
            _ -> expectationFailure "simulation rejection was accepted"
          _ -> expectationFailure "valid RPC error was not forwarded"
  it "does not classify receipt errors or infrastructure errors as rejected trades" $ do
    forM_ [("eth_getUserOperationReceipt", -32521), ("eth_estimateUserOperationGas", -32603)] $ \(method, code) ->
      case classifyAltoResult method (object ["error" .= object ["code" .= (code :: Int), "message" .= ("private provider data" :: T.Text)]]) of
        Left failure -> do
          Rpc.pfReason failure `shouldBe` "BUNDLER_UNAVAILABLE"
          Rpc.pfRetryable failure `shouldBe` True
        _ -> expectationFailure "upstream failure was accepted"
  it "preserves successful results and safely categorizes unknown validation errors" $ do
    classifyAltoResult "eth_estimateUserOperationGas" (object ["result" .= Null]) `shouldBe` Right Null
    case classifyAltoResult "eth_estimateUserOperationGas" (object ["error" .= object ["code" .= (-32500 :: Int), "message" .= ("private" :: T.Text)]]) of
      Left failure -> do
        Rpc.pfReason failure `shouldBe` "SIMULATION_FAILED"
        Rpc.pfRetryable failure `shouldBe` False
      _ -> expectationFailure "validation rejection was accepted"
  forM_ [String "receipt-recovery", Number 42] $ \identifier ->
    forM_ [False, True] $ \upstreamError ->
      it ("round trips " <> show identifier <> ", upstream error=" <> show upstreamError) $ do
        let app request respond = do
              bytes <- strictRequestBody request
              case eitherDecode bytes of
                Right (Object input) -> do
                  KM.lookup "id" input `shouldBe` Just (Number 1)
                  KM.lookup "method" input `shouldBe` Just (String "pimlico_getUserOperationGasPrice")
                _ -> expectationFailure "not a JSON request"
              respond $ responseLBS status200 [] $ encode $ object $
                ["jsonrpc" .= ("2.0" :: T.Text), "id" .= (1 :: Int)] ++
                if upstreamError then ["error" .= object ["code" .= (-32602 :: Int), "message" .= ("Invalid params" :: T.Text)]]
                else ["result" .= Null]
        testWithApplication (pure app) $ \port -> do
          manager <- newManager defaultManagerSettings
          let Right request = Rpc.parseRpcRequest $ object ["jsonrpc" .= ("2.0" :: T.Text),
                "id" .= identifier, "method" .= ("pimlico_getUserOperationGasPrice" :: T.Text), "params" .= ([] :: [Value])]
          result <- forwardAlto manager ("http://127.0.0.1:" <> T.pack (show port)) request
          case result of
            Right (Object output, _) -> do
              KM.lookup "id" output `shouldBe` Just identifier
              KM.member "error" output `shouldBe` upstreamError
            _ -> expectationFailure "upstream response was not forwarded"
  forM_ [Null, Number 2, String "1"] $ \badId ->
    it ("rejects mismatched upstream ID " <> show badId) $ do
      let app _ respond = respond $ responseLBS status200 [] $ encode $ object
            ["jsonrpc" .= ("2.0" :: T.Text), "id" .= badId, "result" .= Null]
      testWithApplication (pure app) $ \port -> do
        manager <- newManager defaultManagerSettings
        let Right request = Rpc.parseRpcRequest $ object ["jsonrpc" .= ("2.0" :: T.Text),
              "id" .= ("caller" :: T.Text), "method" .= ("pimlico_getUserOperationGasPrice" :: T.Text)]
        forwardAlto manager ("http://127.0.0.1:" <> T.pack (show port)) request >>= (`shouldSatisfy` isLeft)
