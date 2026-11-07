module Plether.Ethereum.ClientSpec (spec) where

import Data.Aeson (Value (..))
import Plether.Ethereum.Client
  ( RpcError (..)
  , decodeRpcResponseEnvelope
  , directSecurityRpcEndpoint
  )
import Test.Hspec

spec :: Spec
spec =
  describe "strict JSON-RPC response correlation" $ do
    it "pins Sepolia AA to direct primary and leaves unrelated providers unchanged" $ do
      directSecurityRpcEndpoint "https://rpc-internal.sepolia.plether.com/" `shouldBe` "https://rpc-internal.sepolia.plether.com/primary"
      directSecurityRpcEndpoint "https://rpc-internal.sepolia.plether.com" `shouldBe` "https://rpc-internal.sepolia.plether.com/primary"
      directSecurityRpcEndpoint "https://rpc-internal.sepolia.plether.com/primary" `shouldBe` "https://rpc-internal.sepolia.plether.com/primary"
      directSecurityRpcEndpoint "https://other.invalid/" `shouldBe` "https://other.invalid/"
      directSecurityRpcEndpoint "https://rpc-internal.sepolia.plether.com.evil.invalid/" `shouldBe` "https://rpc-internal.sepolia.plether.com.evil.invalid/"
    it "accepts exactly one result with version 2.0 and the expected id" $
      decodeRpcResponseEnvelope 7 "{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":\"0x1\"}"
        `shouldBe` Right (String "0x1")

    it "rejects a stale id, wrong version, or result/error ambiguity" $ do
      decodeRpcResponseEnvelope 7 "{\"jsonrpc\":\"2.0\",\"id\":8,\"result\":\"0x1\"}"
        `shouldSatisfy` isJsonError
      decodeRpcResponseEnvelope 7 "{\"jsonrpc\":\"1.0\",\"id\":7,\"result\":\"0x1\"}"
        `shouldSatisfy` isJsonError
      decodeRpcResponseEnvelope 7 "{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":null,\"error\":{\"code\":-1,\"message\":\"bad\"}}"
        `shouldSatisfy` isJsonError

    it "preserves a canonical correlated node error" $
      decodeRpcResponseEnvelope 7 "{\"jsonrpc\":\"2.0\",\"id\":7,\"error\":{\"code\":-32000,\"message\":\"denied\"}}"
        `shouldBe` Left (RpcNodeError (-32000) "denied" Nothing)

isJsonError :: Either RpcError Value -> Bool
isJsonError (Left (RpcJsonError _)) = True
isJsonError _ = False
