module Plether.Ethereum.ClientSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import Plether.Ethereum.Client
  ( CallParams (..)
  , RpcChainBindingError (..)
  , RpcError (..)
  , canonicalEthCallParams
  , canonicalBlockHash
  , canonicalBlockNumber
  , decodeCanonicalBlockRef
  , decodeChainIdResult
  , mkCanonicalBlockRef
  , renderCanonicalBlockIdentifier
  , selectRpcUrlsForChain
  , validateRpcChainId
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeChainIdResult" $ do
    it "decodes the Arbitrum Sepolia eth_chainId quantity" $
      decodeChainIdResult (String "0x66eee")
        `shouldBe` Right 421614

    it "accepts upper-case hex digits in a canonical quantity" $
      decodeChainIdResult (String "0xAB")
        `shouldBe` Right 171

    it "accepts the canonical zero quantity" $
      decodeChainIdResult (String "0x0")
        `shouldBe` Right 0

    it "rejects malformed and non-canonical quantities" $ do
      map decodeChainIdResult
        [ String "66eee"
        , String "0x"
        , String "0x066eee"
        , String "0x66eeg"
        , String "0X66eee"
        , Number 421614
        ]
        `shouldSatisfy` all isLeft

  describe "validateRpcChainId" $ do
    it "binds only an exact release chain match" $ do
      validateRpcChainId 421614 (Right 421614)
        `shouldBe` Right ()
      validateRpcChainId 421614 (Right 1)
        `shouldBe` Left RpcChainIdMismatch

    it "redacts all provider failures to an unavailable classification" $
      validateRpcChainId
        421614
        (Left $ RpcHttpError "https://private-rpc.invalid/secret")
        `shouldBe` Left RpcChainIdUnavailable

  describe "selectRpcUrlsForChain" $ do
    it "removes mismatching and unavailable providers from fallback rotation" $ do
      let probes =
            [ ("https://matching-1.invalid", Right 421614)
            , ("https://wrong-chain.invalid", Right 1)
            , ("https://unavailable.invalid", Left $ RpcHttpError "timeout")
            , ("https://matching-2.invalid", Right 421614)
            ]
      selectRpcUrlsForChain 421614 probes
        `shouldBe`
          [ "https://matching-1.invalid"
          , "https://matching-2.invalid"
          ]

    it "returns no provider when none is positively release-bound" $
      selectRpcUrlsForChain
        421614
        [ ("https://wrong.invalid", Right 42161)
        , ("https://failed.invalid", Left $ RpcJsonError "bad result")
        ]
        `shouldBe` []

  describe "CanonicalBlockRef" $ do
    it "binds a strict block number/hash pair to an EIP-1898 identifier" $ do
      let blockHash =
            "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          result = mkCanonicalBlockRef 123 blockHash
      case result of
        Left err -> expectationFailure $ show err
        Right blockRef -> do
          canonicalBlockNumber blockRef `shouldBe` 123
          canonicalBlockHash blockRef `shouldBe` blockHash
          renderCanonicalBlockIdentifier blockRef
            `shouldBe`
              object
                [ "blockHash" .= blockHash
                , "requireCanonical" .= True
                ]
          canonicalEthCallParams
            CallParams
              { callTo = "0x1111111111111111111111111111111111111111"
              , callData = BS.pack [0x12, 0x34]
              }
            blockRef
            `shouldBe`
              Aeson.toJSON
                [ object
                    [ "to" .= ("0x1111111111111111111111111111111111111111" :: String)
                    , "data" .= ("0x1234" :: String)
                    ]
                , object
                    [ "blockHash" .= blockHash
                    , "requireCanonical" .= True
                    ]
                ]

    it "rejects negative numbers and malformed block hashes" $ do
      let validHash =
            "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      mkCanonicalBlockRef (-1) validHash `shouldSatisfy` isLeft
      map (mkCanonicalBlockRef 123)
        [ ""
        , "0x1234"
        , "0Xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        , "0xgggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"
        ]
        `shouldSatisfy` all isLeft

    it "strictly anchors a returned header to the requested block number" $ do
      let blockHash =
            "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
          response =
            object
              [ "number" .= ("0x7b" :: String)
              , "hash" .= blockHash
              ]
      case decodeCanonicalBlockRef 123 response of
        Left err -> expectationFailure $ show err
        Right blockRef -> do
          canonicalBlockNumber blockRef `shouldBe` 123
          canonicalBlockHash blockRef `shouldBe` blockHash

    it "rejects mismatching, noncanonical, missing, and null headers" $ do
      let blockHash :: Text
          blockHash =
            "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
      map (decodeCanonicalBlockRef 123)
        [ object ["number" .= ("0x7c" :: String), "hash" .= blockHash]
        , object ["number" .= ("0x07b" :: String), "hash" .= blockHash]
        , object ["number" .= ("0x7b" :: String), "hash" .= ("0x1234" :: String)]
        , object ["number" .= ("0x7b" :: String)]
        , Null
        ]
        `shouldSatisfy` all isLeft

isLeft :: Either a b -> Bool
isLeft = \case
  Left _ -> True
  Right _ -> False
