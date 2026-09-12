module Plether.AA.OracleReadinessSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Aeson (Value(..), decode, encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Numeric (showHex)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Database.Schema (PythUpdatePayloadRow(..))
import Plether.Ethereum.Client (newClient)
import Plether.AA.OracleReadiness
import Plether.Ethereum.Contracts.Perps (OrderExecutionPolicy(..))
import Plether.Ethereum.Abi (encodeUint256, encodeCall, encodeBool)

live :: OrderExecutionPolicy
live = OrderExecutionPolicy False False False 30 False False

spec :: Spec
spec = describe "Action-specific oracle readiness" $ do
  it "accepts LIVE opens and closes with independently validated basket evidence" $ do
    classifyOracle False True 1000 10 (Just live) True (replicate 6 990) `shouldBe` ("ready","READY")
    classifyOracle True True 1000 10 (Just live) True (replicate 6 990) `shouldBe` ("ready","READY")
  it "blocks FAD opens but leaves FAD closes ready and applies live age limits" $ do
    let open = live {oepCloseOnly=True,oepIsFadWindow=True}
        close = live {oepIsFadWindow=True}
    classifyOracle False True 1000 10 (Just open) True (replicate 6 990) `shouldBe` ("blocked","OPEN_EXECUTION_UNAVAILABLE")
    classifyOracle True False 1000 10 (Just close) True (replicate 6 990) `shouldBe` ("ready","READY")
    classifyOracle True False 1000 10 (Just close) True (replicate 6 960) `shouldBe` ("unknown","ORACLE_UNAVAILABLE")
  it "accepts a frozen close with its actual wider policy, without requiring open-market availability" $ do
    let frozen = live {oepOracleFrozen=True,oepMaxStaleness=3600}
    classifyOracle True False 1000 10 (Just frozen) True (replicate 6 100) `shouldBe` ("ready","READY")
    classifyOracle False False 1000 10 (Just frozen) True (replicate 6 100) `shouldBe` ("blocked","OPEN_EXECUTION_UNAVAILABLE")
  it "never mistakes a stored mark fallback for an executable frozen-close price" $
    classifyOracle True False 1000 10 (Just live {oepRequireStoredMark=True,oepAllowAnyStoredMark=True}) True (replicate 6 990) `shouldBe` ("unknown","ORACLE_UNAVAILABLE")
  it "reports unavailable evidence honestly without hard-blocking exits" $ do
    classifyOracle True False 1000 10 Nothing True (replicate 6 990) `shouldBe` ("unknown","ORACLE_UNAVAILABLE")
    classifyOracle True False 1000 10 (Just live) False (replicate 6 990) `shouldBe` ("unknown","ORACLE_UNAVAILABLE")
    mapM_ (\times -> classifyOracle True False 1000 10 (Just live) True times `shouldBe` ("unknown","ORACLE_UNAVAILABLE"))
      [[],[990],replicate 6 1001,replicate 6 969,[980,980,980,980,980,991]]
  it "accepts exact age and divergence boundaries" $
    classifyOracle True False 1000 10 (Just live) True [970,970,970,970,970,980] `shouldBe` ("ready","READY")
  it "distinguishes frozen protection triggers from voluntary closes and latched retries" $ do
    classifyProtectionOracle 1000 (replicate 6 990) (Just live {oepOracleFrozen=True}) ("ready","READY") `shouldBe` ("unknown","PROTECTION_TRIGGER_UNAVAILABLE")
    classifyProtectionOracle 1000 (replicate 6 990) (Just live {oepIsFadWindow=True}) ("ready","READY") `shouldBe` ("ready","READY")
    classifyProtectionOracle 1000 (replicate 6 984) (Just live) ("ready","READY") `shouldBe` ("unknown","ORACLE_UNAVAILABLE")
  it "rejects malformed policy lengths and noncanonical ABI booleans" $ do
    let words' = [0,0,0,30,0,0]
    decodePolicy (mconcat $ map encodeUint256 words') `shouldBe` Just live
    decodePolicy (mconcat $ map encodeUint256 [0,0,0,30,2,0]) `shouldBe` Nothing
    decodePolicy BS.empty `shouldBe` Nothing
    decodePolicy (mconcat (map encodeUint256 words') <> BS.singleton 0) `shouldBe` Nothing
  it "reads exact-block action policies and verifies the canonical header after parallel reads" $
    withOracleRpc False False $ \result calls -> do
      result `shouldBe` (("blocked","OPEN_EXECUTION_UNAVAILABLE"),("ready","READY"),("ready","READY"))
      length calls `shouldBe` 8
      let params = [p | ("eth_call",p) <- calls]
      all (\p -> case p of Array xs -> last (toList xs) == String "0xa"; _ -> False) params `shouldBe` True
  it "does not require the open-only lens to work before allowing a verified FAD close" $
    withOracleRpc False True $ \(open,close,_) _ -> do
      open `shouldBe` ("unknown","ORACLE_UNAVAILABLE")
      close `shouldBe` ("ready","READY")
  it "invalidates all action evidence on a same-height reorg" $
    withOracleRpc True False $ \(open,close,protection) _ ->
      mapM_ (`shouldBe` ("unknown","ORACLE_UNAVAILABLE")) [open,close,protection]

withOracleRpc :: Bool -> Bool -> (((Text,Text),(Text,Text),(Text,Text)) -> [(Text,Value)] -> IO ()) -> IO ()
withOracleRpc reorg brokenLens action = do
  now <- floor <$> getPOSIXTime
  calls <- newIORef []
  let hex = ("0x" <>) . TE.decodeUtf8 . B16.encode
      wordsHex = String . hex . mconcat . map encodeUint256
      payload = PythUpdatePayloadRow (now-1) (now-1) (toJSON $ replicate 6 (now-1)) (toJSON (["0x1234"] :: [Text])) now "backend_hermes_latest_v2"
      app request respond = do
        body <- strictRequestBody request
        let obj = case decode body of Just (Object o) -> o; _ -> KM.empty
            method = case KM.lookup "method" obj of Just (String m) -> m; _ -> ""
            params = fromMaybe Null $ KM.lookup "params" obj
            calldata = case params of
              Array xs | Object p : _ <- toList xs -> KM.lookup "data" p
              _ -> Nothing
            isCall name args = calldata == Just (String $ hex $ encodeCall name args)
            result
              | method == "eth_getBlockByNumber" = object ["number" .= ("0xa" :: Text), "timestamp" .= ("0x" <> T.pack (showHex now "")), "hash" .= ("0x" <> T.replicate 64 (if reorg && params == toJSON [String "0xa",Bool False] then "b" else "a"))]
              | isCall "getOrderExecutionPolicy(bool)" [encodeBool False] = wordsHex [1,0,0,30,0,1]
              | isCall "getOrderExecutionPolicy(bool)" [encodeBool True] = wordsHex [0,0,0,30,0,1]
              | isCall "orderExecutionStalenessLimit()" [] = wordsHex [10]
              | isCall "getProtocolStatus()" [] = if brokenLens then Null else wordsHex [1,100,now,0,1,0,0,0]
              | isCall "getLatestPrice(uint8)" [encodeUint256 0] || isCall "getLatestPrice(uint8)" [encodeUint256 1] = wordsHex [100,100,now-1,0,30,0,0,1]
              | otherwise = Null
        atomicModifyIORef' calls $ \xs -> ((method,params):xs,())
        respond $ responseLBS status200 [] $ encode $ object ["jsonrpc" .= ("2.0" :: Text),"id" .= fromMaybe Null (KM.lookup "id" obj),"result" .= result]
  testWithApplication (pure app) $ \port -> do
    client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
    result <- readOracleReadiness "0x1111111111111111111111111111111111111111" client (pure $ Just payload)
    readIORef calls >>= action result
