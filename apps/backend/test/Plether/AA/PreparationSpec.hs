module Plether.AA.PreparationSpec (spec) where

import Control.Concurrent.MVar
import Control.Concurrent.Async (async, wait)
import Data.Aeson (Value (..), object, (.=), encode, eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.IORef
import Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.AA.Preparation
import qualified Plether.AA.Pimlico as Legacy
import Plether.AA.EvidenceCache
import Plether.AA.Gateway (SecurityBlockHeader (..), advanceEvidenceSnapshots, attestNativePaymasterProfile, buildPreparedOperation, revalidateSecuritySnapshot)
import Plether.AA.Timing (newTiming)
import qualified Plether.AA.Paymaster as Paymaster
import Plether.AA.PaymasterSpec (fixtureConfig)
import Plether.Config (NativeAaConfig (..), AaRpcMode (..))
import Plether.Ethereum.Client (newClient)
import Network.Wai (strictRequestBody, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.HTTP.Types (status200)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import System.Timeout (timeout)
import Numeric (showHex)
import Plether.Ethereum.Abi (encodeCall, encodeAddress, encodeUint256)
import Test.Hspec

spec :: Spec
spec = do
  describe "fresh authorization-boundary reads" $ do
    mapM_ (\(mode, expectedCalls) ->
      it ("runs independent header reads concurrently in " <> show mode) $ do
        arrived <- newIORef (0 :: Int)
        gate <- newEmptyMVar
        now <- floor <$> getPOSIXTime
        let app request respond = do
              bytes <- strictRequestBody request
              case eitherDecode bytes of
                Right (Object input) -> do
                  count <- atomicModifyIORef' arrived $ \n -> (n+1,n+1)
                  if count == expectedCalls then putMVar gate () else pure ()
                  readMVar gate -- sequential implementations cannot cross this barrier
                  respond $ responseLBS status200 [] $ encode $ object
                    ["jsonrpc" .= ("2.0" :: T.Text), "id" .= KM.lookup "id" input,
                     "result" .= securityHeader 100 snapshotHash now]
                _ -> respond $ responseLBS status200 [] "{}"
        testWithApplication (pure app) $ \port -> do
          client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
          result <- timeout 5_000_000 $ revalidateSecuritySnapshot mode 600 client client
            (SecurityBlockHeader 100 snapshotHash now 1)
          result `shouldBe` Just (Right ())
          readIORef arrived `shouldReturn` expectedCalls)
      [(SingleProviderSepolia,2),(DualIndependent,4)]
    mapM_ (\(label,safeNumber,safeHash,explicitNumber,explicitHash,age) ->
      it ("rejects " <> label <> " at the parallel boundary") $ do
        now <- floor <$> getPOSIXTime
        let app request respond = do
              bytes <- strictRequestBody request
              case eitherDecode bytes of
                Right (Object input) -> do
                  let safe = case KM.lookup "params" input of
                        Just (Array values) -> take 1 (toList values) == [String "safe"]
                        _ -> False
                  respond $ responseLBS status200 [] $ encode $ object
                    ["jsonrpc" .= ("2.0" :: T.Text), "id" .= KM.lookup "id" input,
                     "result" .= if safe then securityHeader safeNumber safeHash (now-age)
                       else securityHeader explicitNumber explicitHash (now-age)]
                _ -> respond $ responseLBS status200 [] "{}"
        testWithApplication (pure app) $ \port -> do
          client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
          revalidateSecuritySnapshot SingleProviderSepolia 600 client client
            (SecurityBlockHeader 100 snapshotHash (now-age) 1) >>= (`shouldSatisfy` isLeft))
      [("safe-head regression",99,snapshotHash,100,snapshotHash,0),
       ("same-height safe reorg",100,otherHash,100,snapshotHash,0),
       ("canonical reorg",101,otherHash,100,otherHash,0),
       ("wrong explicit block number",100,snapshotHash,99,snapshotHash,0),
       ("stale captured evidence",100,snapshotHash,100,snapshotHash,601),
       ("future captured evidence",100,snapshotHash,100,snapshotHash,-1000)]
    it "rejects provider disagreement despite individually well-formed headers" $ do
      now <- floor <$> getPOSIXTime
      let app blockHash request respond = do
            bytes <- strictRequestBody request
            case eitherDecode bytes of
              Right (Object input) -> respond $ responseLBS status200 [] $ encode $ object
                ["jsonrpc" .= ("2.0" :: T.Text), "id" .= KM.lookup "id" input,
                 "result" .= securityHeader 100 blockHash now]
              _ -> respond $ responseLBS status200 [] "{}"
      testWithApplication (pure $ app snapshotHash) $ \firstPort ->
        testWithApplication (pure $ app otherHash) $ \secondPort -> do
          first <- newClient $ "http://127.0.0.1:" <> T.pack (show firstPort)
          second <- newClient $ "http://127.0.0.1:" <> T.pack (show secondPort)
          revalidateSecuritySnapshot DualIndependent 600 first second
            (SecurityBlockHeader 100 snapshotHash now 1) >>= (`shouldSatisfy` isLeft)
  describe "bounded native execution gas headroom" $ do
    it "covers the historical reverted deposit" $
      executionGasWithHeadroom 419_155 `shouldBe` Right 628_733
    it "rounds up and applies a minimum 100,000 gas allowance" $ do
      executionGasWithHeadroom 1 `shouldBe` Right 100_001
      executionGasWithHeadroom 100_000 `shouldBe` Right 200_000
      executionGasWithHeadroom 200_000 `shouldBe` Right 300_000
      executionGasWithHeadroom 200_001 `shouldBe` Right 300_002
    it "rejects rather than clips requirements above the hard cap" $ do
      executionGasWithHeadroom 1_333_333 `shouldBe` Right 2_000_000
      executionGasWithHeadroom 1_349_330 `shouldBe` Right 2_023_995
      executionGasWithHeadroom 1_400_000 `shouldBe` Right sepoliaExecutionGasCap
      mapM_ (\gas -> executionGasWithHeadroom gas `shouldSatisfy` isLeft)
        [-1,0,1_400_001,2_100_000,2^(128::Int)]
    it "prepares with exactly two Alto calls and one nonce read, preserving other gas fields" $ do
      calls <- newIORef ([] :: [T.Text])
      let app request respond = do
            bytes <- strictRequestBody request
            case eitherDecode bytes of
              Right (Object input) | Just (String method) <- KM.lookup "method" input -> do
                atomicModifyIORef' calls $ \xs -> (method:xs,())
                let result = case method of
                      "pimlico_getUserOperationGasPrice" -> object ["fast" .= object
                        ["maxFeePerGas" .= ("0x3b9aca00" :: T.Text), "maxPriorityFeePerGas" .= ("0x1" :: T.Text)]]
                      "eth_call" -> String $ "0x" <> T.replicate 64 "0"
                      "eth_estimateUserOperationGas" -> object
                        ["callGasLimit" .= ("0x66553" :: T.Text), "verificationGasLimit" .= ("0x7815" :: T.Text), "preVerificationGas" .= ("0xd734" :: T.Text)]
                      _ -> Null
                respond $ responseLBS status200 [] $ encode $ object
                  ["jsonrpc" .= ("2.0" :: T.Text), "id" .= KM.lookup "id" input, "result" .= result]
              _ -> respond $ responseLBS status200 [] "{}"
      testWithApplication (pure app) $ \port -> do
        let url = "http://127.0.0.1:" <> T.pack (show port)
            cfg = fixtureConfig {naaAltoRpcUrl=url, naaPostOpGasLimit=0}
        client <- newClient url
        manager <- newManager defaultManagerSettings
        timing <- newTiming
        let intent = PreparationIntent "id" address (hex callData) Nothing Nothing
        result <- buildPreparedOperation timing cfg client manager intent
        case result of
          Left err -> expectationFailure $ show err
          Right op -> do
            KM.lookup "callGasLimit" op `shouldBe` Just (String "0x997fd")
            KM.lookup "verificationGasLimit" op `shouldBe` Just (String "0x7815")
            KM.lookup "preVerificationGas" op `shouldBe` Just (String "0xd734")
            KM.lookup "paymasterPostOpGasLimit" op `shouldBe` Just (String "0x0")
            KM.lookup "signature" op `shouldBe` Nothing
            case Paymaster.parsePackedUserOperation op of
              Left err -> expectationFailure $ T.unpack err
              Right parsed -> do
                Paymaster.puoCallGasLimit parsed `shouldBe` 628_733
                let provisional = Paymaster.makeSponsorshipEnvelope cfg 10 100 (naaMaxCostWei cfg) BS.empty
                    unpadded = parsed {Paymaster.puoCallGasLimit=419_155}
                    liability = Paymaster.maximumUserOperationCost parsed provisional
                    envelope = Paymaster.makeSponsorshipEnvelope cfg 10 100 liability Paymaster.dummyPaymasterSignature
                liability - Paymaster.maximumUserOperationCost unpadded provisional
                  `shouldBe` (628_733-419_155) * Paymaster.puoMaxFeePerGas parsed
                Paymaster.seMaxCost envelope `shouldBe` liability
                Paymaster.sponsorshipDigest parsed envelope `shouldNotBe` Paymaster.sponsorshipDigest unpadded envelope
                Paymaster.userOperationHash (Paymaster.applyPaymasterEnvelope parsed envelope)
                  `shouldNotBe` Paymaster.userOperationHash (Paymaster.applyPaymasterEnvelope unpadded envelope)
        observed <- readIORef calls
        length observed `shouldBe` 3
        mapM_ (\method -> length (filter (==method) observed) `shouldBe` 1)
          ["pimlico_getUserOperationGasPrice","eth_call","eth_estimateUserOperationGas"]
  describe "native preparation intent" $ do
    it "uses numeric IDs compatible with the pinned Alto request schema" $
      case internalRequest "pimlico_getUserOperationGasPrice" [] of
        Left err -> expectationFailure $ show err
        Right request -> do
          Legacy.rrId request `shouldBe` Number 1
          KM.lookup "id" (Legacy.rrObject request) `shouldBe` Just (Number 1)
    it "accepts a versioned unsigned deployed-account intent" $
      parsePreparationIntent [Object fields] `shouldSatisfy` isRight
    it "rejects signatures, caller fees, paymasters, unsupported chains and versions" $ do
      mapM_ (\(key,value) -> parsePreparationIntent [Object $ KM.insert key value fields] `shouldSatisfy` isLeft)
        [("signature",String "0x"),("paymasterData",String "0x"),("maxFeePerGas",String "0x1"),
         ("chainId",String "0x1"),("version",Number 2),("nonce",String "0x0")]
    it "rejects unpaired factories and arbitrary account calldata" $ do
      parsePreparationIntent [Object $ KM.insert "factory" (String address) fields] `shouldSatisfy` isLeft
      parsePreparationIntent [Object $ KM.insert "callData" (String "0xdeadbeef") fields] `shouldSatisfy` isLeft
    it "accepts only the pinned counterfactual factory and index zero" $ do
      let withFactory index = KM.insert "factory" (String "0x13e9ed32155810fdbd067d4522c492d6f68e5944") $
            KM.insert "factoryData" (String $ hex $ encodeCall "createAccount(address,uint256)" [encodeAddress address,encodeUint256 index]) fields
      parsePreparationIntent [Object $ withFactory 0] `shouldSatisfy` isRight
      parsePreparationIntent [Object $ withFactory 1] `shouldSatisfy` isLeft
    it "excludes retry ID but commits all intent bytes in the hash" $ do
      case parsePreparationIntent [Object fields] of
        Left err -> expectationFailure $ show err
        Right intent -> do
          intentHash intent `shouldBe` intentHash (intent {piIdentifier = "different"})
          intentHash intent `shouldNotBe` intentHash (intent {piCallData = "0x1234"})
          matchesIntent intent (unsignedSkeleton intent) `shouldBe` True
          matchesIntent intent (KM.insert "callData" (String "0x1234") $ unsignedSkeleton intent) `shouldBe` False
  describe "block-specific evidence" $ do
    it "launches independent profile reads concurrently and performs one provider pass in single mode" $ do
      calls <- newIORef (0 :: Int)
      release <- newEmptyMVar
      now <- (floor <$> getPOSIXTime) :: IO Integer
      let app request respond = do
            bytes <- strictRequestBody request
            case eitherDecode bytes of
              Right (Object fields') -> do
                let identifier = maybe Null id $ KM.lookup "id" fields'
                    method = KM.lookup "method" fields'
                value <- case method of
                  Just (String "eth_chainId") -> pure $ String "0x66eee"
                  Just (String "eth_getBlockByNumber") -> pure $ object
                    ["number" .= ("0x1" :: T.Text),"hash" .= ("0x" <> T.replicate 64 "a"),
                     "timestamp" .= ("0x" <> T.pack (showHex now "")),"baseFeePerGas" .= ("0x1" :: T.Text)]
                  _ -> do
                    count <- atomicModifyIORef' calls $ \n -> (n+1,n+1)
                    if count == 6 then putMVar release () else pure ()
                    readMVar release
                    pure $ String $ "0x" <> T.replicate 64 "0"
                respond $ responseLBS status200 [("Content-Type","application/json")] $
                  encode $ object ["jsonrpc" .= ("2.0" :: T.Text), "id" .= identifier, "result" .= value]
              _ -> respond $ responseLBS status200 [] "{}"
      testWithApplication (pure app) $ \port -> do
        client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
        result <- timeout 5_000_000 $ attestNativePaymasterProfile (fixtureConfig {naaRpcMode=SingleProviderSepolia}) client client
        result `shouldSatisfy` maybe False isLeft -- synthetic code deliberately does not attest
        readIORef calls `shouldReturn` 16
    it "rejects regressing and replaced snapshots, retaining only two heights" $ do
      advanceEvidenceSnapshots [(10,"a"),(9,"b")] (8,"c") `shouldBe` (False,[(10,"a"),(9,"b")])
      advanceEvidenceSnapshots [(10,"a"),(9,"b")] (10,"changed") `shouldBe` (False,[(10,"changed"),(9,"b")])
      advanceEvidenceSnapshots [(10,"a"),(9,"b")] (11,"c") `shouldBe` (True,[(11,"c"),(10,"a")])
    it "shares a successful read and invalidates snapshots" $ do
      cache <- newEvidenceCache 2
      calls <- newIORef (0 :: Int)
      let fetch = modifyIORef' calls (+1) >> pure (Right "owner" :: Either T.Text T.Text)
      evidence cache "hash-a" "account" fetch `shouldReturn` Right "owner"
      evidence cache "hash-a" "account" fetch `shouldReturn` Right "owner"
      readIORef calls `shouldReturn` 1
      retainSnapshots cache ["hash-b"]
      evidence cache "hash-a" "account" fetch `shouldReturn` Right "owner"
      readIORef calls `shouldReturn` 2
    it "does not cache failed verification" $ do
      cache <- newEvidenceCache 2
      evidence cache "hash" "key" (pure $ Left "failed") `shouldReturn` (Left "failed" :: Either T.Text T.Text)
      evidence cache "hash" "key" (pure $ Right "verified") `shouldReturn` Right "verified"
    it "coalesces concurrent work without timing-dependent assertions" $ do
      cache <- newEvidenceCache 2 :: IO (EvidenceCache T.Text T.Text)
      started <- newEmptyMVar
      release <- newEmptyMVar
      first <- async $ evidence cache "hash" "key" (putMVar started () >> takeMVar release >> pure (Right "verified"))
      takeMVar started
      second <- async $ evidence cache "hash" "key" (expectationFailure "duplicate upstream work" >> pure (Right "wrong"))
      putMVar release ()
      wait first `shouldReturn` Right ("verified" :: T.Text)
      wait second `shouldReturn` Right "verified"
    it "evicts the least recently used key at capacity" $ do
      cache <- newEvidenceCache 1 :: IO (EvidenceCache T.Text T.Text)
      evidence cache "hash" "a" (pure $ Right "a") `shouldReturn` Right ("a" :: T.Text)
      evidence cache "hash" "b" (pure $ Right "b") `shouldReturn` Right "b"
      evidence cache "hash" "a" (pure $ Right "new") `shouldReturn` Right "new"
    it "waits for capacity without evicting in-flight evidence" $ do
      cache <- newEvidenceCache 1 :: IO (EvidenceCache T.Text T.Text)
      started <- newEmptyMVar
      release <- newEmptyMVar
      waiting <- newEmptyMVar
      first <- async $ evidence cache "hash" "a" (putMVar started () >> takeMVar release >> pure (Right "a"))
      takeMVar started
      second <- async $ evidenceObserved cache
        (\event -> if event == "capacity_wait" then putMVar waiting () else pure ())
        "hash" "b" (pure $ Right "b")
      takeMVar waiting
      putMVar release ()
      wait first `shouldReturn` Right "a"
      wait second `shouldReturn` Right "b"
 where
  snapshotHash = "0x" <> T.replicate 64 "a"
  otherHash = "0x" <> T.replicate 64 "b"
  securityHeader :: Integer -> T.Text -> Integer -> Value
  securityHeader number blockHash timestamp = object
    ["number" .= ("0x" <> T.pack (showHex number "")), "hash" .= blockHash,
     "timestamp" .= ("0x" <> T.pack (showHex timestamp "")), "baseFeePerGas" .= ("0x1" :: T.Text)]
  address = "0x2222222222222222222222222222222222222222"
  hex = ("0x" <>) . TE.decodeUtf8 . B16.encode
  payload = encodeCall "approve(address,uint256)" [encodeAddress address,encodeUint256 1]
  callData = encodeCall "execute(address,uint256,bytes)"
    [encodeAddress address, encodeUint256 0, encodeUint256 96, encodeUint256 $ fromIntegral $ BS.length payload, payload <> BS.replicate 28 0]
  fields = case object ["version" .= (1::Int),"preparationId" .= ("0x" <> T.replicate 64 "a"),
    "chainId" .= ("0x66eee" :: T.Text), "entryPoint" .= ("0x4337084d9e255ff0702461cf8895ce9e3b5ff108" :: T.Text),
    "sender" .= address,"callData" .= hex callData] of Object value -> value; _ -> error "object"
  isRight (Right _) = True
  isRight _ = False
  isLeft (Left _) = True
  isLeft _ = False
