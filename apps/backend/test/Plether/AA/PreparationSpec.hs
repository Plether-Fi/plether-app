module Plether.AA.PreparationSpec (spec) where

import Control.Concurrent.MVar
import Control.Concurrent.Async (async, wait)
import Data.Aeson (Value (..), object, (.=), encode, eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.AA.Preparation
import Plether.AA.EvidenceCache
import Plether.AA.Gateway (advanceEvidenceSnapshots, attestNativePaymasterProfile)
import Plether.AA.PaymasterSpec (fixtureConfig)
import Plether.Config (NativeAaConfig (..), AaRpcMode (..))
import Plether.Ethereum.Client (newClient)
import Network.Wai (strictRequestBody, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.HTTP.Types (status200)
import System.Timeout (timeout)
import Numeric (showHex)
import Plether.Ethereum.Abi (encodeCall, encodeAddress, encodeUint256)
import Test.Hspec

spec :: Spec
spec = do
  describe "native preparation intent" $ do
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
