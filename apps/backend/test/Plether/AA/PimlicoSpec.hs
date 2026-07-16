module Plether.AA.PimlicoSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.AA.Pimlico
  ( RpcRequest (..)
  , SmartCall (..)
  , decodeSmartAccountCalls
  , injectSponsorshipPolicy
  , parseRpcRequest
  , validateActionSequence
  , validateMethodParams
  )
import Plether.Config (AaConfig (..), Config (..))
import Plether.Ethereum.Abi
  ( encodeAddress
  , encodeCall
  , encodeUint256
  , selector
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "strict JSON-RPC handling" $ do
    it "rejects batch requests and unknown methods" $ do
      parseRpcRequest (toJSON [gasPriceRequest]) `shouldSatisfy` isLeft
      parseRpcRequest
        ( object
            [ "jsonrpc" .= ("2.0" :: String)
            , "id" .= (1 :: Int)
            , "method" .= ("eth_sendRawTransaction" :: String)
            , "params" .= ([] :: [Value])
            ]
        )
        `shouldSatisfy` isLeft

    it "requires the approved paymaster chain and replaces client context" $ do
      let requestValue =
            object
              [ "jsonrpc" .= ("2.0" :: String)
              , "id" .= (1 :: Int)
              , "method" .= ("pm_getPaymasterStubData" :: String)
              , "params"
                  .= [ object []
                     , String entryPoint
                     , String "0x66eee"
                     , object
                        [ "sponsorshipPolicyId" .= ("attacker" :: String)
                        , "extra" .= True
                        ]
                     ]
              ]
      parsed <-
        case parseRpcRequest requestValue of
          Right value -> pure value
          Left failure -> do
            expectationFailure $ showFailure failure
            pure $ error "unreachable"
      rewritten <-
        case injectSponsorshipPolicy testAaConfig parsed of
          Right value -> pure value
          Left failure -> do
            expectationFailure $ showFailure failure
            pure $ error "unreachable"
      case rrParams rewritten of
        [_, _, _, Object policyContext] -> do
          KM.lookup "sponsorshipPolicyId" policyContext
            `shouldBe` Just (String "approved-policy")
          KM.size policyContext `shouldBe` 1
        _ -> expectationFailure "unexpected rewritten paymaster parameters"

      let wrongChain =
            parsed
              { rrParams =
                  [ object []
                  , String entryPoint
                  , String "0x1"
                  , object []
                  ]
              }
      validateMethodParams wrongChain `shouldSatisfy` isLeft

    it "accepts the serialized permissionless.js v0.8 stub shape" $ do
      let factoryData =
            selector "createAccount(address,uint256)"
              <> encodeAddress owner
              <> encodeUint256 0
          operation =
            object
              [ "sender" .= sender
              , "nonce" .= ("0x0" :: T.Text)
              , "factory" .= simpleAccountFactory
              , "factoryData" .= hex factoryData
              , "callData" .= hex (encodeExecuteBatch depositCalls)
              , "callGasLimit" .= ("0x0" :: T.Text)
              , "verificationGasLimit" .= ("0x0" :: T.Text)
              , "preVerificationGas" .= ("0x0" :: T.Text)
              , "maxFeePerGas" .= ("0x1" :: T.Text)
              , "maxPriorityFeePerGas" .= ("0x1" :: T.Text)
              , "signature" .= permissionlessDummySignature
              ]
          requestValue =
            object
              [ "jsonrpc" .= ("2.0" :: T.Text)
              , "id" .= (7 :: Int)
              , "method" .= ("pm_getPaymasterStubData" :: T.Text)
              , "params"
                  .= [ operation
                     , String entryPoint
                     , String "0x66eee"
                     , object []
                     ]
              ]
      case parseRpcRequest requestValue of
        Left failure -> expectationFailure $ showFailure failure
        Right parsed ->
          validateMethodParams parsed `shouldSatisfy` isRight

  describe "SimpleAccount calldata policy" $ do
    it "decodes the canonical v0.8 executeBatch encoding" $ do
      let calls =
            [ smartCall usdc $ encodeCall "approve(address,uint256)"
                [encodeAddress clearinghouse, encodeUint256 25]
            , smartCall clearinghouse $ encodeCall "depositMargin(uint256)"
                [encodeUint256 25]
            ]
      decodeSmartAccountCalls (encodeExecuteBatch calls) `shouldBe` Right calls

    it "rejects non-canonical trailing account calldata" $ do
      let call =
            smartCall engine $
              encodeCall "settleTraderClaim(address)" [encodeAddress sender]
      decodeSmartAccountCalls (encodeExecute call <> BS.singleton 0)
        `shouldSatisfy` isLeft

  describe "Plether whole-operation policy" $ do
    it "accepts the five frontend action shapes" $ do
      validate depositCalls `shouldSatisfy` isRight
      validate withdrawalCalls `shouldSatisfy` isRight
      validate [orderCall] `shouldSatisfy` isRight
      validate [addMarginCall] `shouldSatisfy` isRight
      validate [claimCall] `shouldSatisfy` isRight

    it "rejects standalone withdrawal and arbitrary token recipients" $ do
      validate [head withdrawalCalls] `shouldSatisfy` isLeft
      let badTransfer =
            smartCall usdc $
              encodeCall "transfer(address,uint256)"
                [encodeAddress attacker, encodeUint256 19]
      validate [head withdrawalCalls, badTransfer] `shouldSatisfy` isLeft

    it "rejects mismatched deposits, nonzero native value, and wrong account args" $ do
      let mismatchedDeposit =
            [ head depositCalls
            , smartCall clearinghouse $
                encodeCall "depositMargin(uint256)" [encodeUint256 8]
            ]
          nonzero = (head depositCalls) {smartCallValue = 1}
          wrongAccount =
            smartCall engine $
              encodeCall "addMargin(address,uint256)"
                [encodeAddress attacker, encodeUint256 10]
      validate mismatchedDeposit `shouldSatisfy` isLeft
      validate [nonzero, depositCalls !! 1] `shouldSatisfy` isLeft
      validate [wrongAccount] `shouldSatisfy` isLeft
  where
    validate = validateActionSequence testConfig sender owner

gasPriceRequest :: Value
gasPriceRequest =
  object
    [ "jsonrpc" .= ("2.0" :: String)
    , "id" .= (1 :: Int)
    , "method" .= ("pimlico_getUserOperationGasPrice" :: String)
    , "params" .= ([] :: [Value])
    ]

depositCalls :: [SmartCall]
depositCalls =
  [ smartCall usdc $
      encodeCall "approve(address,uint256)"
        [encodeAddress clearinghouse, encodeUint256 7]
  , smartCall clearinghouse $
      encodeCall "depositMargin(uint256)" [encodeUint256 7]
  ]

withdrawalCalls :: [SmartCall]
withdrawalCalls =
  [ smartCall clearinghouse $
      encodeCall "withdrawMargin(uint256)" [encodeUint256 19]
  , smartCall usdc $
      encodeCall "transfer(address,uint256)"
        [encodeAddress owner, encodeUint256 19]
  ]

orderCall :: SmartCall
orderCall =
  smartCall router $
    encodeCall
      "commitOrder(uint8,uint256,uint256,uint256,bool)"
      [ encodeUint256 0
      , encodeUint256 100
      , encodeUint256 10
      , encodeUint256 1234
      , encodeUint256 0
      ]

addMarginCall :: SmartCall
addMarginCall =
  smartCall engine $
    encodeCall "addMargin(address,uint256)"
      [encodeAddress sender, encodeUint256 10]

claimCall :: SmartCall
claimCall =
  smartCall engine $
    encodeCall "settleTraderClaim(address)" [encodeAddress sender]

smartCall :: T.Text -> ByteString -> SmartCall
smartCall target dataBytes =
  SmartCall
    { smartCallTarget = T.toLower target
    , smartCallValue = 0
    , smartCallData = dataBytes
    }

encodeExecute :: SmartCall -> ByteString
encodeExecute call =
  selector "execute(address,uint256,bytes)"
    <> encodeAddress (smartCallTarget call)
    <> encodeUint256 (smartCallValue call)
    <> encodeUint256 96
    <> encodeDynamicBytes (smartCallData call)

encodeExecuteBatch :: [SmartCall] -> ByteString
encodeExecuteBatch calls =
  selector "executeBatch((address,uint256,bytes)[])"
    <> encodeUint256 32
    <> encodeUint256 (fromIntegral $ length calls)
    <> mconcat (map encodeUint256 offsets)
    <> mconcat tuples
  where
    tuples = map encodeTuple calls
    firstOffset = fromIntegral (length calls * 32)
    offsets = init $ scanl (+) firstOffset $ map (fromIntegral . BS.length) tuples

encodeTuple :: SmartCall -> ByteString
encodeTuple call =
  encodeAddress (smartCallTarget call)
    <> encodeUint256 (smartCallValue call)
    <> encodeUint256 96
    <> encodeDynamicBytes (smartCallData call)

encodeDynamicBytes :: ByteString -> ByteString
encodeDynamicBytes bytes =
  encodeUint256 (fromIntegral $ BS.length bytes)
    <> bytes
    <> BS.replicate padding 0
  where
    padding = (32 - BS.length bytes `mod` 32) `mod` 32

testAaConfig :: AaConfig
testAaConfig =
  AaConfig
    { aaProxyOriginToken = "origin-token"
    , aaPimlicoApiKey = "api-key"
    , aaSponsorshipPolicyId = "approved-policy"
    , aaSponsorshipEnabled = True
    , aaIpRateLimitPerMinute = 120
    , aaAccountRateLimitPerMinute = 30
    , aaMaxRequestBytes = 262144
    , aaSponsoredGasAlertWeiPerHour = 0
    }

testConfig :: Config
testConfig =
  Config
    { cfgRpcUrl = ""
    , cfgChainId = 11155111
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Nothing
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = ""
    , cfgPythHermesUrl = ""
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 7
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythIngestionEnabled = False
    , cfgPerpsRpcUrl = ""
    , cfgPerpsChainId = 421614
    , cfgPerpsUsdc = usdc
    , cfgPerpsOrderRouter = router
    , cfgPerpsCfdEngine = engine
    , cfgPerpsMarginClearinghouse = clearinghouse
    , cfgPerpsPletherOracle = ""
    , cfgPerpsIndexerStartBlock = 0
    , cfgAaConfig = Just testAaConfig
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    }

entryPoint, sender, owner, attacker, usdc, clearinghouse, router, engine, simpleAccountFactory :: T.Text
entryPoint = "0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108"
sender = "0x1111111111111111111111111111111111111111"
owner = "0x2222222222222222222222222222222222222222"
attacker = "0x9999999999999999999999999999999999999999"
usdc = "0xf1e1B188b87525C51ECe4bae8627ae621D769651"
clearinghouse = "0x731bb0939CE531728459394A277B28Cbff8df049"
router = "0x4A0a6c028164A1254e10C3e39cc89Af45090069e"
engine = "0xA1Ebfb8aD9C90367eA30A29592419d447E3f8224"
simpleAccountFactory = "0x13E9ed32155810FDbd067D4522C492D6f68E5944"

permissionlessDummySignature :: T.Text
permissionlessDummySignature =
  "0xfffffffffffffffffffffffffffffff000000000000000000000000000000000\
  \7aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1c"

hex :: ByteString -> T.Text
hex bytes = "0x" <> TE.decodeUtf8 (B16.encode bytes)

showFailure :: a -> String
showFailure _ = "AA proxy validation unexpectedly failed"
