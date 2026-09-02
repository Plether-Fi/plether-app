module Plether.AA.PimlicoSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (addUTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Plether.AA.Pimlico
  ( PimlicoMethod (..)
  , RpcRequest (..)
  , SmartCall (..)
  , decodeSmartAccountCalls
  , injectSponsorshipPolicy
  , isRecoveryReadAuthorized
  , newPimlicoProxyState
  , parseRpcRequest
  , recordSubmittedOperation
  , validateActionSequence
  , validateMethodParams
  )
import Plether.Config
  ( AaConfig (..)
  , Config (..)
  , LpSettlementMode (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  )
import Plether.Ethereum.Abi
  ( encodeAddress
  , encodeCall
  , encodeUint256
  , selector
  )
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , july2026Competition
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

  describe "recovery read authorization" $ do
    it "accepts only recent hashes from the original trusted client IP" $ do
      proxyState <- newPimlicoProxyState
      let now =
            parseTimeOrError
              True
              defaultTimeLocale
              "%Y-%m-%dT%H:%M:%SZ"
              "2026-08-04T12:00:00Z"
          userOperationHash = "0x" <> T.replicate 64 "a"
          trustedIp = "203.0.113.10"
          recoveryRequest requestMethod =
            RpcRequest
              Null
              requestMethod
              [String $ "0x" <> T.toUpper (T.drop 2 userOperationHash)]
              KM.empty
          recoveryMethods =
            [ GetUserOperationReceipt
            , GetUserOperationByHash
            , GetUserOperationStatus
            ]

      mapM_
        (\requestMethod ->
          isRecoveryReadAuthorized
            proxyState
            now
            trustedIp
            (recoveryRequest requestMethod)
            `shouldReturn` False
        )
        recoveryMethods
      recordSubmittedOperation
        proxyState
        now
        trustedIp
        (object ["result" .= userOperationHash])
      mapM_
        (\requestMethod ->
          isRecoveryReadAuthorized
            proxyState
            now
            trustedIp
            (recoveryRequest requestMethod)
            `shouldReturn` True
        )
        recoveryMethods
      isRecoveryReadAuthorized
        proxyState
        now
        "203.0.113.11"
        (recoveryRequest GetUserOperationReceipt)
        `shouldReturn` False
      mapM_
        (\requestMethod ->
          isRecoveryReadAuthorized
            proxyState
            (addUTCTime (24 * 60 * 60) now)
            trustedIp
            (recoveryRequest requestMethod)
            `shouldReturn` False
        )
        recoveryMethods
      isRecoveryReadAuthorized
        proxyState
        now
        trustedIp
        (RpcRequest Null GetGasPrice [] KM.empty)
        `shouldReturn` True

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

    it "rejects invalid V2 client identities and unpinned execution modes" $ do
      validate [orderCallWith (BS.replicate 32 0) 1] `shouldSatisfy` isLeft
      validate
        [orderCallWith (reservedClientPrefix <> BS.replicate 24 0) 1]
        `shouldSatisfy` isLeft
      validate [orderCallWith (BS.replicate 32 0x11) 7]
        `shouldSatisfy` isLeft
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
orderCall = orderCallWith (BS.replicate 32 0x11) 1

orderCallWith :: ByteString -> Integer -> SmartCall
orderCallWith clientOrderId allowedExecutionModes =
  smartCall router $
    encodeCall
      "commitOrder((bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)))"
      [ clientOrderId
      , encodeUint256 0
      , encodeUint256 100
      , encodeUint256 10
      , encodeUint256 1234
      , encodeUint256 0
      , encodeUint256 2000000000
      , encodeUint256 allowedExecutionModes
      , BS.replicate 32 0x22
      , encodeUint256 1
      , encodeUint256 100
      , encodeUint256 20
      , encodeUint256 5
      , encodeUint256 5
      , encodeUint256 100
      , encodeUint256 1
      , encodeUint256 1
      , encodeUint256 50000
      ]

reservedClientPrefix :: ByteString
reservedClientPrefix = BS.pack [0x50, 0x4c, 0x45, 0x54, 0x48, 0x45, 0x52, 0x21]

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
    , cfgPythHistoryUrl = ""
    , cfgPythHermesUrl = ""
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 7
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgProtocolExplorerEnabled = True
    , cfgPerpsCandleWriteMode = PerpsCandleWritesOff
    , cfgPerpsCandleReadMode = PerpsCandleReadsLegacy
    , cfgPerpsCandleReadIntervals = []
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = ""
    , cfgPerpsChainId = 421614
    , cfgPerpsUsdc = usdc
    , cfgPerpsOrderRouter = router
    , cfgPerpsOrderLifecycleBook = Nothing
    , cfgPerpsCfdEngine = engine
    , cfgPerpsCfdEngineLens = zeroAddress
    , cfgPerpsCfdEngineSettlementSidecar = zeroAddress
    , cfgPerpsMarginClearinghouse = clearinghouse
    , cfgPerpsPletherOracle = ""
    , cfgPerpsAccountLens = zeroAddress
    , cfgPerpsPublicLens = zeroAddress
    , cfgPerpsHousePool = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgPerpsSeniorVault = zeroAddress
    , cfgPerpsJuniorVault = zeroAddress
    , cfgPerpsOrderRouterAdmin = zeroAddress
    , cfgPerpsCfdEngineAdmin = zeroAddress
    , cfgPerpsSettlementMonitorLens = "0xd251AC0BD90780c48F31F575152808315200664E"
    , cfgPerpsIndexerStartBlock = 0
    , cfgVaultHistoryHousePoolAddress = "0x0000000000000000000000000000000000000001"
    , cfgVaultHistorySeniorVaultAddress = "0x0000000000000000000000000000000000000002"
    , cfgVaultHistoryJuniorVaultAddress = "0x0000000000000000000000000000000000000003"
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryRpcUrl = "https://archive.example"
    , cfgVaultHistoryConfirmations = 12
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = testReleaseManifest
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Just testAaConfig
    , cfgFaucetGuardConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    , cfgLpSettlementMode = LpSettlementOff
    , cfgLpSettlementPrivateKey = Nothing
    , cfgLpSettlementSeniorVault = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
    , cfgLpSettlementJuniorVault = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
    , cfgLpSettlementPollSeconds = 15
    , cfgLpSettlementMaxDrainTransactions = 4
    , cfgLpSettlementPendingReplacementSeconds = 60
    , cfgLpSettlementMaxReplacements = 3
    , cfgLpSettlementMaxTxCostWei = 0
    }

testReleaseManifest :: CompetitionReleaseManifest
testReleaseManifest =
  CompetitionReleaseManifest
    { crmReleaseId = "pimlico-test"
    , crmChainId = 421614
    , crmUsdc = usdc
    , crmOrderRouter = router
    , crmMarginClearinghouse = clearinghouse
    , crmAccountLens = zeroAddress
    , crmCfdEngine = engine
    , crmCfdEngineLens = zeroAddress
    , crmSettlementSidecar = zeroAddress
    , crmPletherOracle = zeroAddress
    , crmIndexerStartBlock = 0
    }

entryPoint, sender, owner, attacker, usdc, clearinghouse, router, engine, simpleAccountFactory, zeroAddress :: T.Text
entryPoint = "0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108"
sender = "0x1111111111111111111111111111111111111111"
owner = "0x2222222222222222222222222222222222222222"
attacker = "0x9999999999999999999999999999999999999999"
usdc = "0x1647e41f49ED6D688936092B5a291c4B28106343"
clearinghouse = "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211"
router = "0x97A901dE2B267c307E264FD5F71403F8072F73e7"
engine = "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D"
simpleAccountFactory = "0x13E9ed32155810FDbd067D4522C492D6f68E5944"
zeroAddress = "0x0000000000000000000000000000000000000000"

permissionlessDummySignature :: T.Text
permissionlessDummySignature =
  "0xfffffffffffffffffffffffffffffff000000000000000000000000000000000\
  \7aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1c"

hex :: ByteString -> T.Text
hex bytes = "0x" <> TE.decodeUtf8 (B16.encode bytes)

showFailure :: a -> String
showFailure _ = "AA proxy validation unexpectedly failed"
