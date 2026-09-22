module Plether.Keeper.BroadcastSpec (broadcastSpec) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Ethereum.Client (newClient)
import Plether.Ethereum.Transaction (SignedTransaction (..), rawTransactionHash)
import Plether.Keeper.Broadcast
import Test.Hspec

broadcastSpec :: Text -> Spec
broadcastSpec url = describe "durable order broadcast PostgreSQL" $ do
  it "reconciles a lost response after worker restart without allocating another nonce" $
    fixture $ \conn -> do
      let raw = BS.pack [2,128,255,0,42]
          hash = rawTransactionHash raw
          signed = SignedTransaction raw hash "0x1111111111111111111111111111111111111111"
      saveBroadcast conn "router" signed
      void $ execute_ conn "UPDATE perps_keeper_broadcasts SET last_broadcast_at=now()-interval '10 seconds'"
      observed <- newIORef ([] :: [Value])
      mined <- newIORef False
      rpcFails <- newIORef False
      applied <- newIORef (0 :: Int)
      let app request respond = do
            body <- strictRequestBody request
            let Just (Object fields) = decode body
                method = KM.lookup "method" fields
            modifyIORef' observed (<> [Object fields])
            isMined <- readIORef mined
            fails <- readIORef rpcFails
            let receipt = object ["transactionHash" .= hash, "blockNumber" .= ("0x1" :: Text),
                  "blockHash" .= ("0x" <> T.replicate 64 "a"), "transactionIndex" .= ("0x0" :: Text),
                  "status" .= ("0x1" :: Text), "logs" .= ([] :: [Value])]
                result = case method of
                  Just (String "eth_getTransactionReceipt") -> if isMined then receipt else Null
                  Just (String "eth_sendRawTransaction") -> String hash
                  _ -> error "The reconciler must never request a new nonce or sign another transaction"
            respond $ responseLBS status200 [("Content-Type","application/json")] $
              encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= KM.lookup "id" fields, (if fails then "error" else "result") .= (if fails then object ["code" .= (-32000 :: Int), "message" .= ("unavailable" :: Text)] else result)]
      testWithApplication (pure app) $ \port -> do
        -- A fresh client has no in-memory state from the original submission.
        client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
        reconcileBroadcast conn "router" client (\_ -> modifyIORef' applied (+1)) `shouldReturn` False
        requests <- readIORef observed
        let sends = [KM.lookup "params" fields | Object fields <- requests, KM.lookup "method" fields == Just (String "eth_sendRawTransaction")]
        sends `shouldBe` [Just $ toArray [String $ "0x" <> TE.decodeUtf8 (B16.encode raw)]]
        (query_ conn "SELECT count(*) FROM perps_keeper_broadcasts" :: IO [Only Int]) `shouldReturn` [Only 1]
        -- The unique row cannot be overwritten by a replacement/new operation.
        saveBroadcast conn "router" signed `shouldThrow` anySqlError
        writeIORef rpcFails True
        reconcileBroadcast conn "router" client (\_ -> expectationFailure "RPC failure cannot confirm") `shouldReturn` False
        (query_ conn "SELECT count(*) FROM perps_keeper_broadcasts" :: IO [Only Int]) `shouldReturn` [Only 1]
        writeIORef rpcFails False
        writeIORef mined True
        reconcileBroadcast conn "router" client (\_ -> fail "receipt persistence interrupted") `shouldThrow` anyIOException
        (query_ conn "SELECT count(*) FROM perps_keeper_broadcasts" :: IO [Only Int]) `shouldReturn` [Only 1]
        restartedClient <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
        reconcileBroadcast conn "router" restartedClient (\_ -> modifyIORef' applied (+1)) `shouldReturn` True
        readIORef applied `shouldReturn` 1
        (query_ conn "SELECT count(*) FROM perps_keeper_broadcasts" :: IO [Only Int]) `shouldReturn` [Only 0]

  it "keeps a corrupt journal blocked without contacting the RPC" $
    fixture $ \conn -> do
      saveBroadcast conn "router" $ SignedTransaction "bytes" "wrong-hash" "sender"
      client <- newClient "http://127.0.0.1:1"
      reconcileBroadcast conn "router" client (\_ -> expectationFailure "Must not apply a corrupt journal") `shouldReturn` False
 where
  fixture action = bracket (connectPostgreSQL $ TE.encodeUtf8 url) close $ \conn -> do
    void $ execute_ conn "CREATE TEMP TABLE perps_keeper_broadcasts(order_router text PRIMARY KEY,tx_hash text NOT NULL,raw_tx bytea NOT NULL,last_broadcast_at timestamptz NOT NULL DEFAULT now())"
    action conn
  anySqlError :: SqlError -> Bool
  anySqlError _ = True
  toArray values = case decode (encode values) of
    Just value -> value
    Nothing -> error "invalid fixture"
