module Plether.Keeper.FundingSpec (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (when)
import Data.Aeson (Value(..), decode, encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Ethereum.Client (EthClient, newClient)
import Plether.Ethereum.Rpc (ethGetBalanceAtBlock)
import Plether.Ethereum.Transaction (applyBpsBuffer)
import Plether.Keeper.Funding
import System.Timeout (timeout)
import Test.Hspec

sample :: FundingEvidence
sample = FundingEvidence 1000 10000000 7 7 100 10 2

spec :: Spec
spec = describe "Idle keeper funding" $ do
  it "can establish a reserve before any transaction has supplied a quote" $ do
    let cost = keeperReserveCost 1000 20 2500 sample
    cost `shouldBe` Just 125040
    classifyKeeperReserve 1000 sample cost `shouldBe` ("ready", "READY")
  it "reprices reserves on a fresh gas price increase" $ do
    let spike = sample { fundingGasPrice = 1000000 }
    classifyKeeperReserve 1000 spike (keeperReserveCost 1000 20 2500 spike)
      `shouldBe` ("unknown", "FUNDING_LOW")
  it "does not claim a conservative upper bound proves an order unaffordable" $
    classifyKeeperReserve 1000 sample (Just 10000001) `shouldBe` ("unknown", "FUNDING_LOW")
  it "warns below ten reserves and accepts the exact boundary" $ do
    classifyKeeperReserve 1000 sample (Just 1000001) `shouldBe` ("ready", "FUNDING_LOW")
    classifyKeeperReserve 1000 sample (Just 1000000) `shouldBe` ("ready", "READY")
  it "blocks a verified empty account and recovers after funding" $ do
    classifyKeeperReserve 1000 (sample { fundingBalance = 0 }) (Just 100) `shouldBe` ("blocked", "KEEPER_INSUFFICIENT_FUNDS")
    classifyKeeperReserve 1000 sample (Just 100) `shouldBe` ("ready", "READY")
  it "does not assume unknown pending liabilities are zero" $
    mapM_ (\nonce -> classifyKeeperReserve 1000 (sample { fundingPendingNonce = nonce }) (Just 100)
      `shouldBe` ("unknown", "FUNDING_UNVERIFIED")) [6,8]
  it "rejects stale or future headers, even with an empty account" $
    mapM_ (\now -> classifyKeeperReserve now (sample { fundingBalance = 0 }) (Just 100)
      `shouldBe` ("unknown", "FUNDING_UNVERIFIED")) [1016,997]
  it "accepts the exact freshness and clock-skew boundaries" $
    mapM_ (\now -> classifyKeeperReserve now sample (Just 100)
      `shouldBe` ("ready", "READY")) [1015,998]
  it "never substitutes zero for a missing or invalid reserve" $
    mapM_ (\cost -> classifyKeeperReserve 1000 sample cost `shouldBe` ("unknown", "FUNDING_UNVERIFIED"))
      [Nothing, Just 0, Just (-1)]
  it "rejects invalid gas, batch and fee inputs" $ do
    keeperReserveCost 0 20 2500 sample `shouldBe` Nothing
    keeperReserveCost 1000 0 2500 sample `shouldBe` Nothing
    keeperReserveCost 1000 20 (-1) sample `shouldBe` Nothing
    keeperReserveCost 1000 20 2500 (sample { fundingGasPrice = 0 }) `shouldBe` Nothing
  it "matches the existing EIP-1559 buffering including rounding and high tips" $
    mapM_ (\(gas,tip,buffer) -> keeperFeeCaps buffer gas tip `shouldBe`
      (max (applyBpsBuffer tip buffer) (applyBpsBuffer (max gas tip) buffer), applyBpsBuffer tip buffer))
      [(100,10,2500), (1,2,1), (100,200,2500), (1,0,0)]
  it "reads all six independent inputs concurrently at an exact block without estimating or sending" $
    withRpc [] False True $ \client calls -> do
      result <- timeout 2_000_000 $ readFundingEvidence client signer oracle [BS.singleton 1]
      result `shouldBe` Just (Right sample)
      requests <- readIORef calls
      length requests `shouldBe` 8
      requests `shouldContain` [("eth_getBalance", array [String signer,String "0x7b"])]
      requests `shouldContain` [("eth_getTransactionCount", array [String signer,String "0x7b"])]
      requests `shouldContain` [("eth_getTransactionCount", array [String signer,String "pending"])]
      case lookup "eth_call" requests of
        Just (Array params) -> last (toList params) `shouldBe` String "0x7b"
        _ -> expectationFailure "missing fixed-block oracle fee read"
  it "rejects a same-height reorg" $
    withRpc [] True False $ \client _ ->
      readFundingEvidence client signer oracle [BS.singleton 1] >>= (`shouldSatisfy` isLeft)
  it "fails closed on malformed or unavailable required RPC evidence" $
    mapM_ (\method -> withRpc [(method,Null)] False False $ \client _ ->
      readFundingEvidence client signer oracle [BS.singleton 1] >>= (`shouldSatisfy` isLeft))
      ["eth_getBalance","eth_getTransactionCount","eth_gasPrice","eth_call","eth_getBlockByNumber"]
  it "uses the same priority-fee fallback as transaction submission" $
    withRpc [("eth_maxPriorityFeePerGas",Null)] False False $ \client _ ->
      readFundingEvidence client signer oracle [BS.singleton 1]
        `shouldReturn` Right (sample { fundingPriorityFee = 100 })
  it "rejects empty payloads and negative block numbers without making RPC requests" $
    withRpc [] False False $ \client calls -> do
      readFundingEvidence client signer oracle [] >>= (`shouldSatisfy` isLeft)
      readFundingEvidence client signer oracle [BS.empty] >>= (`shouldSatisfy` isLeft)
      ethGetBalanceAtBlock client signer (-1) >>= (`shouldSatisfy` isLeft)
      readIORef calls `shouldReturn` []

signer, oracle :: Text
signer = "0x1111111111111111111111111111111111111111"
oracle = "0x2222222222222222222222222222222222222222"

array :: [Value] -> Value
array = toJSON

-- The barrier makes sequential evidence fetching fail deterministically rather
-- than asserting a fragile millisecond speedup against a loaded test machine.
withRpc :: [(Text,Value)] -> Bool -> Bool -> (EthClient -> IORef [(Text,Value)] -> IO ()) -> IO ()
withRpc overrides reorg barrier action = do
  calls <- newIORef []
  count <- newIORef (0 :: Int)
  gate <- newEmptyMVar
  let app request respond = do
        body <- strictRequestBody request
        let obj = case decode body of Just (Object o) -> o; _ -> KM.empty
            method = case KM.lookup "method" obj of Just (String m) -> m; _ -> ""
            params = fromMaybe Null $ KM.lookup "params" obj
        atomicModifyIORef' calls $ \xs -> ((method,params):xs,())
        when (barrier && method /= "eth_getBlockByNumber") $ do
          n <- atomicModifyIORef' count $ \n -> (n+1,n+1)
          when (n == 6) $ putMVar gate ()
          readMVar gate
        let changed = reorg && params == array [String "0x7b",Bool False]
            result = fromMaybe (case method of
              "eth_getBlockByNumber" -> object
                ["number" .= ("0x7b" :: Text), "timestamp" .= ("0x3e8" :: Text)
                ,"hash" .= ("0x" <> T.replicate 64 (if changed then "b" else "a"))]
              "eth_getBalance" -> String "0x989680"
              "eth_getTransactionCount" -> String "0x7"
              "eth_gasPrice" -> String "0x64"
              "eth_maxPriorityFeePerGas" -> String "0xa"
              "eth_call" -> String $ "0x" <> T.replicate 63 "0" <> "2"
              _ -> Null) (lookup method overrides)
        respond $ responseLBS status200 [] $ encode $ object
          ["jsonrpc" .= ("2.0" :: Text),"id" .= fromMaybe Null (KM.lookup "id" obj),"result" .= result]
  testWithApplication (pure app) $ \port -> do
    client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
    action client calls
