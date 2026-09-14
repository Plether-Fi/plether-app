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
import Plether.AA.Gateway (forwardAlto)
import qualified Plether.AA.Pimlico as Rpc
import Test.Hspec

spec :: Spec
spec = describe "Alto request ID normalization" $ do
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
