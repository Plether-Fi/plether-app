module Plether.AA.PimlicoSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Either (isLeft, isRight)
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Ethereum.Client (EthClient, newClient)
import Plether.AA.Gateway (agreeAccountIdentity)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (addUTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Plether.AA.Pimlico
  ( PimlicoMethod (..)
  , ParsedUserOperation (..)
  , ProxyFailure (..)
  , RpcRequest (..)
  , SmartCall (..)
  , decodeSmartAccountCalls
  , injectSponsorshipPolicy
  , isRecoveryReadAuthorized
  , newPimlicoProxyState
  , parseRpcRequest
  , recordSubmittedOperation
  , validateActionSequence
  , validateNativeActionSequence
  , validateMethodParams
  , verifyAccountIdentity
  , verifyAccountIdentityAtBlock
  , unavailable
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
import qualified Plether.Perps.Manifest as Manifest
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

  describe "account deployment confirmation" $ do
    let deployed = ParsedUserOperation sender Nothing []
        reason = either pfReason (const "AUTHORIZED")
    it "reports pending only for a trusted latest account absent at the safe snapshot" $
      withIdentityRpc False "valid" $ \client calls -> do
        result <- verifyAccountIdentityAtBlock client 100 deployed
        reason result `shouldBe` "ACCOUNT_DEPLOYMENT_PENDING"
        result `shouldSatisfy` isLeft
        length <$> readIORef calls `shouldReturn` 13
    it "does not add latest reads to an already safe account" $
      withIdentityRpc True "valid" $ \client calls -> do
        verifyAccountIdentityAtBlock client 100 deployed `shouldReturn` Right owner
        length <$> readIORef calls `shouldReturn` 7
    it "verifies the account normally only after the safe snapshot advances" $
      withIdentityRpc False "valid" $ \client calls -> do
        reason <$> verifyAccountIdentityAtBlock client 100 deployed `shouldReturn` "ACCOUNT_DEPLOYMENT_PENDING"
        verifyAccountIdentityAtBlock client 101 deployed `shouldReturn` Right owner
        length <$> readIORef calls `shouldReturn` 20
    it "does not treat missing safe RPC evidence as pending or authorize using latest" $
      withIdentityRpc False "safe-rpc-failure" $ \client calls -> do
        reason <$> verifyAccountIdentityAtBlock client 100 deployed `shouldReturn` "SPONSOR_UNAVAILABLE"
        length <$> readIORef calls `shouldReturn` 6
    it "preserves counterfactual first-operation verification without latest reads" $
      withIdentityRpc False "valid" $ \client calls -> do
        verifyAccountIdentityAtBlock client 100 (deployed {puoFactoryOwner = Just owner}) `shouldReturn` Right owner
        length <$> readIORef calls `shouldReturn` 3
    it "never labels an unknown or untrusted latest account as pending" $
      mapM_ (\mode -> withIdentityRpc False mode $ \client _ -> do
        result <- verifyAccountIdentityAtBlock client 100 deployed
        result `shouldSatisfy` isLeft
        reason result `shouldNotBe` "ACCOUNT_DEPLOYMENT_PENDING") ["missing", "wrong-entrypoint", "rpc-failure"]
    it "keeps latest-only (Pimlico) validation unchanged" $
      withIdentityRpc False "valid" $ \client calls -> do
        verifyAccountIdentity client deployed `shouldReturn` Right owner
        length <$> readIORef calls `shouldReturn` 7
    it "requires agreement on pending and does not authorize either provider mode" $ do
      let pending = Left $ unavailable "ACCOUNT_DEPLOYMENT_PENDING" "pending"
          outage = Left $ unavailable "SPONSOR_UNAVAILABLE" "unavailable"
      agreeAccountIdentity pending pending `shouldBe` pending
      reason (agreeAccountIdentity pending (Right owner)) `shouldBe` "SECURITY_ATTESTATION_UNAVAILABLE"
      reason (agreeAccountIdentity pending outage) `shouldBe` "SECURITY_ATTESTATION_UNAVAILABLE"
      reason (agreeAccountIdentity (Right owner) pending) `shouldBe` "SECURITY_ATTESTATION_UNAVAILABLE"

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
    it "accepts canonical Book calls at the deployed address and rejects malformed protection calldata" $ do
      let book = T.toLower Manifest.positionProtectionBookAddress
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

    it "rejects protection calls to the retired v1.2.1 Book" $ do
      let retiredBook = "0x63973eb0b5a862dfc95348d4d575fc55c9546f04"
          call = smartCall retiredBook $ encodeCall "createPositionProtection((uint256,uint256))" [encodeUint256 68000000, encodeUint256 92000000]
      validate [call] `shouldSatisfy` isLeft

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
  describe "native close assistance" $ do
    it "allows only the native guarded exact-amount batch" $ do
      let calls = assistedCalls 198000 sender
      validateNativeActionSequence (Just attacker) testConfig sender owner calls `shouldSatisfy` isRight
      validateActionSequence testConfig sender owner calls `shouldSatisfy` isLeft
      validateNativeActionSequence Nothing testConfig sender owner calls `shouldSatisfy` isLeft
    it "rejects excessive amounts, wrong recipients, and changed requests" $ do
      validateNativeActionSequence (Just attacker) testConfig sender owner (assistedCalls 200001 sender) `shouldSatisfy` isLeft
      validateNativeActionSequence (Just attacker) testConfig sender owner (assistedCalls 198000 owner) `shouldSatisfy` isLeft
      let calls = assistedCalls 198000 sender
      validateNativeActionSequence (Just attacker) testConfig sender owner (take 4 calls ++ [orderCall]) `shouldSatisfy` isLeft

    it "rejects native value, target changes, extra calls and unequal funding legs" $ do
      let calls = assistedCalls 198000 sender
          replace index call = take index calls ++ [call] ++ drop (index + 1) calls
          validateNative = validateNativeActionSequence (Just attacker) testConfig sender owner
      mapM_ (\index -> do
        validateNative (replace index ((calls !! index) {smartCallValue = 1})) `shouldSatisfy` isLeft
        validateNative (replace index ((calls !! index) {smartCallTarget = owner})) `shouldSatisfy` isLeft
        ) [0..4]
      validateNative (calls ++ [orderCall]) `shouldSatisfy` isLeft
      validateNative (assistedCalls 0 sender) `shouldSatisfy` isLeft
      mapM_ (\index -> validateNative (replace index (assistedCalls 197999 sender !! index)) `shouldSatisfy` isLeft) [0..3]

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
    , cfgPerpsHousePool = "0x87622630fb1941fe02731d4a9fcdec0388efd78b"
    , cfgPerpsSettlementMonitorLens = "0x52f9621446650ab663f2f1665f28817924c96826"
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
    , cfgNativeAaConfig = Nothing
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
    , cfgLpSettlementSeniorVault = "0x970ac2cfe9a19d4318806812719a5c291711b33a"
    , cfgLpSettlementJuniorVault = "0x2075a46921fc5fbcf5fca808e3a2c66c6f812d79"
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
usdc = "0xf7cbfcc74f2d9eb6fa7dc11941b3bef9fd7f8eb8"
clearinghouse = "0xfa6e677ec1062757c1194d411a5e61e1e9644499"
router = "0x6215d36fcbd610ca1525252eebcbfd8b223a6072"
engine = "0xafece93321be41aa73474457e2f47cf7b2fb738f"
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

-- Offline RPC fixture: a trusted account exists at latest, but can be absent
-- at the exact safe block. Empty owner() responses mirror the reported incident.
withIdentityRpc :: Bool -> T.Text -> (EthClient -> IORef [Value] -> IO ()) -> IO ()
withIdentityRpc safePresent mode action = do
  calls <- newIORef []
  let impl = "0x28426d752372d68d34340bd94390950dce3c9ec3"
      word = String . hex . encodeAddress
      app request respond = do
        body <- strictRequestBody request
        let obj = case decode body of Just (Object o) -> o; _ -> KM.empty
            method = KM.lookup "method" obj
            params = case KM.lookup "params" obj of Just (Array xs) -> toList xs; _ -> []
            atLatest = String "latest" `elem` params
            present = if atLatest then mode /= "missing" else safePresent || String "0x65" `elem` params
            result = case (method, params) of
              (Just (String "eth_getCode"), _) ->
                if (atLatest && mode == "rpc-failure") || (not atLatest && mode == "safe-rpc-failure")
                  then Null else String $ if present then "0x6000" else "0x"
              (Just (String "eth_getStorageAt"), [_, String slot, _]) ->
                if present && T.isPrefixOf "0x3608" slot then word impl else word zeroAddress
              (Just (String "eth_call"), Object call : _) ->
                let target = KM.lookup "to" call
                    dat = KM.lookup "data" call
                in if target == Just (String $ T.toLower simpleAccountFactory)
                  then if dat == Just (String $ hex $ encodeCall "accountImplementation()" []) then word impl else word sender
                  else if not present then String "0x"
                  else if dat == Just (String $ hex $ encodeCall "owner()" []) then word owner
                  else word $ if mode == "wrong-entrypoint" then attacker else entryPoint
              _ -> Null
        atomicModifyIORef' calls $ \xs -> (toJSON params : xs, ())
        respond $ responseLBS status200 [] $ encode $ object
          ["jsonrpc" .= ("2.0" :: T.Text), "id" .= KM.lookup "id" obj, "result" .= result]
  testWithApplication (pure app) $ \port -> do
    client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
    action client calls

assistedCalls :: Integer -> T.Text -> [SmartCall]
assistedCalls amount recipient =
  let raw = BS.drop 4 $ smartCallData orderCall
      closeRequest = BS.take (3*32) raw <> encodeUint256 0 <> BS.take 32 (BS.drop (4*32) raw)
        <> encodeUint256 1 <> BS.drop (6*32) raw
      close = smartCall router $ selector "commitOrder((bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)))" <> closeRequest
      guard = smartCall attacker $ selector "validateSponsoredClose(address,(bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)),uint256)" <> encodeAddress engine <> closeRequest <> encodeUint256 amount
  in [guard,smartCall usdc $ encodeCall "mint(address,uint256)" [encodeAddress recipient,encodeUint256 amount],
      smartCall usdc $ encodeCall "approve(address,uint256)" [encodeAddress clearinghouse,encodeUint256 amount],
      smartCall clearinghouse $ encodeCall "depositMargin(uint256)" [encodeUint256 amount],close]
