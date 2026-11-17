module Plether.Ethereum.LogRangeSpec (spec) where

import Data.Aeson (Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import Network.HTTP.Types (status200, status400)
import Network.Wai (responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Ethereum.Client (RpcError (..), newClient, rpcCall)
import Plether.Ethereum.LogRange
import Test.Hspec

spec :: Spec
spec = describe "bounded log-range catch-up" $ do
  it "subdivides a real gateway range rejection and retains every block exactly once" $ do
    requests <- newIORef []
    let app request respond = do
          body <- strictRequestBody request
          case eitherDecode body of
            Right (Object fields)
              | Just (Number requestId) <- KM.lookup "id" fields
              , Just (Array params) <- KM.lookup "params" fields
              , Object filterFields : _ <- toList params
              , Just firstRaw <- KM.lookup "fromBlock" filterFields
              , Just lastRaw <- KM.lookup "toBlock" filterFields
              , Success first <- (fromJSON firstRaw :: Result Integer)
              , Success lastBlock <- (fromJSON lastRaw :: Result Integer) -> do
                  modifyIORef' requests (<> [(first, lastBlock)])
                  if lastBlock - first + 1 > 1_000
                    then respond $ responseLBS status400 [] "{\"error\":\"log_range_requires_explicit_bounded_blocks\"}"
                    else respond $ responseLBS status200 [] $ encode $ object
                      [ "jsonrpc" .= ("2.0" :: T.Text), "id" .= requestId
                      , "result" .= [first .. lastBlock]
                      ]
            _ -> respond $ responseLBS status400 [] "{\"error\":\"invalid_request\"}"
    testWithApplication (pure app) $ \port -> do
      client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
      let fetch first lastBlock = do
            result <- rpcCall client "eth_getLogs" $ toParams first lastBlock
            pure $ case result of
              Left err -> Left $ rpcFailureCategory err
              Right value -> case fromJSON value :: Result [Integer] of
                Success blocks -> Right blocks
                Error _ -> Left RpcInvalidResponse
      scanLogRange fetch 1 3_000 `shouldReturn` Right [1 .. 3_000]
    actual <- readIORef requests
    length actual `shouldBe` 7
    filter (\(first,lastBlock) -> lastBlock - first + 1 <= 1_000) actual
      `shouldBe` [(1,750),(751,1500),(1501,2250),(2251,3000)]

  it "replays an incomplete batch without returning a partial cursor result" $ do
    requests <- newIORef []
    let fetch first lastBlock = do
          modifyIORef' requests (<> [(first,lastBlock)])
          pure $ if lastBlock - first + 1 > 2 then Left RpcRangeLimit
            else if first == 3 then Left RpcRateLimit else Right [first .. lastBlock]
    scanLogRange fetch 1 4 `shouldReturn` Left RpcRateLimit
    readIORef requests `shouldReturn` [(1,4),(1,2),(3,4)]
    scanLogRange (\first lastBlock -> pure $ Right [first .. lastBlock]) 1 4
      `shouldReturn` Right [1 .. 4 :: Integer]

  it "drains a multi-hour backlog in complete contiguous batches" $ do
    let fetch first lastBlock = pure $
          if lastBlock - first + 1 > 1_000 then Left RpcRangeLimit else Right [first .. lastBlock]
        catchUp cursor target seen
          | cursor == target = pure seen
          | otherwise = do
              let next = min target $ cursor + maxLogRangeBlocks
              result <- scanLogRange fetch (cursor + 1) next
              case result of
                Left err -> expectationFailure (show err) >> pure seen
                Right blocks -> catchUp next target (seen <> blocks)
    catchUp 0 43_000 [] `shouldReturn` [1 .. 43_000 :: Integer]

  it "bounds provider work and never splits rate limits or unavailable errors" $ do
    calls <- newIORef (0 :: Int)
    let counted result _ _ = modifyIORef' calls (+ 1) >> pure result
    mapM_ (\failure -> do
      scanLogRange (counted $ Left failure :: Integer -> Integer -> IO (Either RpcFailureCategory [Integer])) 1 5_000
        `shouldReturn` Left failure) [RpcRateLimit,RpcTimeout,RpcUnavailable,RpcInvalidResponse]
    readIORef calls `shouldReturn` 4
    scanLogRange (\first lastBlock -> do
      modifyIORef' calls (+ 1)
      pure $ if first == lastBlock then Right [first] else Left RpcRangeLimit) 1 5_000
      `shouldReturn` Left RpcRangeLimit
    total <- readIORef calls
    total `shouldSatisfy` (<= 67)

  it "rejects invalid or oversized intervals without making a request" $ do
    let noRequest _ _ = expectationFailure "unexpected RPC request" >> pure (Right [] :: Either RpcFailureCategory [Integer])
    mapM_ (\(first,lastBlock) -> scanLogRange noRequest first lastBlock `shouldReturn` Left RpcRangeLimit)
      [(-1,1),(2,1),(1,maxLogRangeBlocks + 1)]

  it "only recognizes specific range errors and emits closed diagnostic codes" $ do
    rpcFailureCategory (RpcNodeError (-32005) "limit exceeded" Nothing) `shouldBe` RpcUnavailable
    rpcFailureCategory (RpcNodeError (-32005) "rate limit exceeded" Nothing) `shouldBe` RpcRateLimit
    rpcFailureCategory (RpcNodeError (-32602) "block range too large" Nothing) `shouldBe` RpcRangeLimit
    rpcFailureCategory (RpcNodeError (-32000) "execution reverted" Nothing) `shouldBe` RpcUnavailable
    rpcFailureCategory (RpcHttpError "statusCode = 413") `shouldBe` RpcRangeLimit
    rpcFailureCategory (RpcHttpError "response timeout") `shouldBe` RpcTimeout
    rpcFailureCode (rpcFailureCategory $ RpcNodeError (-32000) "https://secret.invalid/key" (Just "private payload"))
      `shouldBe` "RPC_UNAVAILABLE"

  it "backs off transient read failures within the bounded delay and leaves other failures unchanged" $ do
    map (\attempt -> reconcilerRetryDelayMicros 2 attempt 0 "primary: RPC_RATE_LIMIT") [1..6]
      `shouldBe` [2_000_000,4_000_000,8_000_000,16_000_000,32_000_000,60_000_000]
    reconcilerRetryDelayMicros 2 6 250_000 "RPC_TIMEOUT" `shouldBe` 60_000_000
    reconcilerRetryDelayMicros 2 6 250_000 "RPC_RANGE_LIMIT" `shouldBe` 2_000_000
    reconcilerRetryDelayMicros 60 6 250_000 "RPC_UNAVAILABLE" `shouldBe` 60_000_000

toParams :: Integer -> Integer -> Value
toParams first lastBlock = toJSON [object ["fromBlock" .= first, "toBlock" .= lastBlock]]
