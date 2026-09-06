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
          protectedRecoveryMethods =
            [ GetUserOperationByHash
            , GetUserOperationStatus
            ]

      isRecoveryReadAuthorized
        proxyState
        now
        trustedIp
        (recoveryRequest GetUserOperationReceipt)
        `shouldReturn` True
      mapM_
        (\requestMethod ->
          isRecoveryReadAuthorized
            proxyState
            now
            trustedIp
            (recoveryRequest requestMethod)
            `shouldReturn` False
        )
        protectedRecoveryMethods
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
        protectedRecoveryMethods
      isRecoveryReadAuthorized
        proxyState
        now
        "203.0.113.11"
        (recoveryRequest GetUserOperationReceipt)
        `shouldReturn` True
      mapM_
        (\requestMethod ->
          isRecoveryReadAuthorized
            proxyState
            (addUTCTime (24 * 60 * 60) now)
            trustedIp
            (recoveryRequest requestMethod)
            `shouldReturn` False
        )
        protectedRecoveryMethods
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
    it "accepts canonical v1.2.1 Book calls and rejects malformed protection calldata" $ do
      let book = "0x63973eb0b5a862dfc95348d4d575fc55c9546f04"
          create = smartCall book $ encodeCall "createPositionProtection((uint256,uint256))" [encodeUint256 68000000, encodeUint256 92000000]
          replace = smartCall book $ encodeCall "replacePositionProtection(uint64,(uint256,uint256))" [encodeUint256 42, encodeUint256 0, encodeUint256 92000000]
          cancel = smartCall book $ encodeCall "cancelPositionProtection(uint64)" [encodeUint256 42]
          protectedOpen = smartCall book $ encodeCall "commitOpenOrderWithProtection((bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)),(uint256,uint256))" [BS.drop 4 $ smartCallData orderCall, encodeUint256 68000000, encodeUint256 92000000]
          disabledConfig = testConfig { cfgAaConfig = Just testAaConfig { aaProtectionCommitsEnabled = False } }
      mapM_ (\call -> validate [call] `shouldSatisfy` isRight) [create, replace, cancel, protectedOpen]
      validateActionSequence disabledConfig sender owner [create] `shouldSatisfy` isLeft
      validateActionSequence disabledConfig sender owner [cancel] `shouldSatisfy` isRight
      validate [create { smartCallValue = 1 }] `shouldSatisfy` isLeft
      validate [create { smartCallTarget = router }] `shouldSatisfy` isLeft
      validate [create { smartCallData = smartCallData create <> BS.singleton 0 }] `shouldSatisfy` isLeft
      validate [smartCall book $ encodeCall "createPositionProtection((uint256,uint256))" [encodeUint256 0, encodeUint256 0]] `shouldSatisfy` isLeft
      validate [smartCall book $ encodeCall "cancelPositionProtection(uint64)" [encodeUint256 (2 ^ (64 :: Int))]] `shouldSatisfy` isLeft
      validate [smartCall book $ encodeCall "retryPositionProtectionClose(uint64)" [encodeUint256 42]] `shouldSatisfy` isLeft
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
    , aaProtectionCommitsEnabled = True
    , aaIpRateLimitPerMinute = 120
    , aaAccountRateLimitPerMinute = 30
    , aaMaxRequestBytes = 262144
    , aaSponsoredGasAlertWeiPerHour = 0
    }

testConfig :: Config
testConfig =
  Config
    { cfgRpcUrl = ""
    , cfgRpcAuthToken = Nothing
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
    , cfgPerpsCandleWriteMode = PerpsCandleWritesOff
    , cfgPerpsCandleReadMode = PerpsCandleReadsLegacy
    , cfgPerpsCandleReadIntervals = []
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = ""
    , cfgPerpsRpcAuthToken = Nothing
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
    , cfgPerpsHousePool = "0x7b8b851cb3783611bcDA4CF2F7D5A2F8C6106F98"
    , cfgPerpsSettlementMonitorLens = "0x3d6E6407F23fc41899180C7dC699F02a1BB2926B"
    , cfgPerpsIndexerStartBlock = 0
    , cfgVaultHistoryHousePoolAddress = "0x0000000000000000000000000000000000000001"
    , cfgVaultHistorySeniorVaultAddress = "0x0000000000000000000000000000000000000002"
    , cfgVaultHistoryJuniorVaultAddress = "0x0000000000000000000000000000000000000003"
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryConfirmations = 12
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = testReleaseManifest
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Just testAaConfig
    , cfgFaucetGuardConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperIdlePollSeconds = 5
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    , cfgLpSettlementMode = LpSettlementOff
    , cfgLpSettlementPrivateKey = Nothing
    , cfgLpSettlementSeniorVault = "0xF98e69d808F8c22fCE4210516E2F0B2dAa4CC0B2"
    , cfgLpSettlementJuniorVault = "0xd6B662D75B102eA360C1B083E1f332e6c1634832"
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
usdc = "0xAbEe441b564DC084857468fA244AEE0A444B07DF"
clearinghouse = "0x91c85540A1f64C9AEC2C801fcc927F037d619f17"
router = "0x2b9790AD11cE5fB1B91aC3415B08cD1Ec7D0cE0B"
engine = "0x2CEDc3f0059f0E9C1099bE96974f459E58c428d6"
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
