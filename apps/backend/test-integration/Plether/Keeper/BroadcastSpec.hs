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
import Plether.Ethereum.Transaction (SignedTransaction (..), Tx1559 (..), signTransaction)
import Plether.Keeper.Broadcast
import Test.Hspec

broadcastSpec :: Text -> Spec
broadcastSpec url = describe "durable order broadcast PostgreSQL" $ do
  it "reconciles a lost response after worker restart without allocating another nonce" $
    fixture $ \conn -> do
      signed <- signedFixture
      let raw = signedRawTransaction signed
          hash = signedTransactionHash signed
      saveBroadcast conn router signed
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
                  Just (String "eth_chainId") -> String "0x66eee"
                  Just (String "eth_getBlockByNumber") -> safeBlock
                  Just (String "eth_getTransactionCount") -> String "0x11"
                  _ -> error "The reconciler must never request a new nonce or sign another transaction"
            respond $ responseLBS status200 [("Content-Type","application/json")] $
              encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= KM.lookup "id" fields, (if fails then "error" else "result") .= (if fails then object ["code" .= (-32000 :: Int), "message" .= ("unavailable" :: Text)] else result)]
      testWithApplication (pure app) $ \port -> do
        -- A fresh client has no in-memory state from the original submission.
        client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
        reconcileBroadcast conn router client (\_ -> modifyIORef' applied (+1)) `shouldReturn` False
        requests <- readIORef observed
        let sends = [KM.lookup "params" fields | Object fields <- requests, KM.lookup "method" fields == Just (String "eth_sendRawTransaction")]
        sends `shouldBe` [Just $ toArray [String $ "0x" <> TE.decodeUtf8 (B16.encode raw)]]
        (query_ conn "SELECT count(*) FROM perps_keeper_broadcasts" :: IO [Only Int]) `shouldReturn` [Only 1]
        -- The unique row cannot be overwritten by a replacement/new operation.
        saveBroadcast conn router signed `shouldThrow` anySqlError
        writeIORef rpcFails True
        reconcileBroadcast conn router client (\_ -> expectationFailure "RPC failure cannot confirm") `shouldReturn` False
        (query_ conn "SELECT count(*) FROM perps_keeper_broadcasts" :: IO [Only Int]) `shouldReturn` [Only 1]
        writeIORef rpcFails False
        writeIORef mined True
        reconcileBroadcast conn router client (\_ -> fail "receipt persistence interrupted") `shouldThrow` anyIOException
        (query_ conn "SELECT count(*) FROM perps_keeper_broadcasts" :: IO [Only Int]) `shouldReturn` [Only 1]
        restartedClient <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
        reconcileBroadcast conn router restartedClient (\_ -> modifyIORef' applied (+1)) `shouldReturn` True
        readIORef applied `shouldReturn` 1
        (query_ conn "SELECT count(*) FROM perps_keeper_broadcasts" :: IO [Only Int]) `shouldReturn` [Only 0]

  it "keeps a corrupt journal blocked without contacting the RPC" $
    fixture $ \conn -> do
      saveBroadcast conn "router" $ SignedTransaction "bytes" "wrong-hash" "sender"
      client <- newClient "http://127.0.0.1:1"
      reconcileBroadcast conn "router" client (\_ -> expectationFailure "Must not apply a corrupt journal") `shouldReturn` False

  mapM_ (\(label, mode, expected) -> it label $ fixture $ \conn -> do
      signed <- signedFixture
      saveBroadcast conn router signed
      observed <- newIORef ([] :: [Text])
      receiptReads <- newIORef (0 :: Int)
      blockReads <- newIORef (0 :: Int)
      let app request respond = do
            body <- strictRequestBody request
            let Just (Object fields) = decode body
                Just (String method) = KM.lookup "method" fields
            modifyIORef' observed (<> [method])
            reads <- readIORef receiptReads
            blocks <- readIORef blockReads
            whenMethod method "eth_getTransactionReceipt" $ modifyIORef' receiptReads (+1)
            whenMethod method "eth_getBlockByNumber" $ modifyIORef' blockReads (+1)
            let result = case method of
                  "eth_getTransactionReceipt" | mode == "receipt-race" && reads > 0 -> object
                    ["transactionHash" .= signedTransactionHash signed, "blockNumber" .= ("0x1" :: Text),
                     "blockHash" .= ("0x" <> T.replicate 64 "a"), "transactionIndex" .= ("0x0" :: Text),
                     "status" .= ("0x1" :: Text), "logs" .= ([] :: [Value])]
                  "eth_getTransactionReceipt" -> Null
                  "eth_chainId" -> String $ if mode == "chain" then "0x1" else "0x66eee"
                  "eth_getBlockByNumber" | mode == "missing-safe" -> Null
                  "eth_getBlockByNumber" | mode == "reorg" && blocks > 0 -> object
                    ["number" .= ("0x64" :: Text), "hash" .= ("0x" <> T.replicate 64 "b"), "timestamp" .= ("0x64" :: Text)]
                  "eth_getBlockByNumber" -> safeBlock
                  "eth_getTransactionCount" -> String $ if mode == "equal" then "0x11" else "0x12"
                  "eth_sendRawTransaction" -> String $ signedTransactionHash signed
                  _ -> error "Unexpected recovery RPC"
                fails = mode == "rpc-failure" && method == "eth_getTransactionCount"
            respond $ responseLBS status200 [("Content-Type","application/json")] $ encode $
              object ["jsonrpc" .= ("2.0" :: Text), "id" .= KM.lookup "id" fields,
                (if fails then "error" else "result") .= (if fails then object
                  ["code" .= (-32000 :: Int), "message" .= ("unavailable" :: Text)] else result)]
      testWithApplication (pure app) $ \port -> do
        client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
        reconcileBroadcast conn router client (\_ -> expectationFailure "Nonce consumption is not execution") `shouldReturn` expected
      (query_ conn "SELECT count(*) FROM perps_keeper_broadcasts" :: IO [Only Int]) `shouldReturn` [Only $ if expected then 0 else 1]
      (query_ conn "SELECT count(*) FROM perps_keeper_broadcast_recoveries" :: IO [Only Int]) `shouldReturn` [Only $ if expected then 1 else 0]
      whenMethod expected True $ do
        rows <- query_ conn "SELECT raw_tx FROM perps_keeper_broadcast_recoveries" :: IO [Only BS.ByteString]
        rows `shouldBe` [Only $ signedRawTransaction signed]
        requests <- readIORef observed
        requests `shouldNotContain` ["eth_sendRawTransaction"])
    [("archives a consumed nonce proven at a canonical safe block", "recover", True),
     ("keeps an equal nonce blocked", "equal", False),
     ("keeps a wrong-chain nonce blocked", "chain", False),
     ("keeps a safe-block reorg blocked", "reorg", False),
     ("keeps failed nonce reads blocked", "rpc-failure", False),
     ("keeps unavailable safe finality blocked", "missing-safe", False),
     ("keeps a receipt appearing during recovery for normal reconciliation", "receipt-race", False)]
 where
  fixture action = bracket (connectPostgreSQL $ TE.encodeUtf8 url) close $ \conn -> do
    void $ execute_ conn "CREATE TEMP TABLE perps_keeper_broadcasts(order_router text PRIMARY KEY,tx_hash text NOT NULL,raw_tx bytea NOT NULL,last_broadcast_at timestamptz NOT NULL DEFAULT now())"
    void $ execute_ conn "CREATE TEMP TABLE perps_keeper_broadcast_recoveries(tx_hash text PRIMARY KEY,order_router text NOT NULL,raw_tx bytea NOT NULL,proof jsonb NOT NULL,recovered_at timestamptz NOT NULL DEFAULT now())"
    action conn
  anySqlError :: SqlError -> Bool
  anySqlError _ = True
  toArray values = case decode (encode values) of
    Just value -> value
    Nothing -> error "invalid fixture"

router :: Text
router = "0x2222222222222222222222222222222222222222"

signedFixture :: IO SignedTransaction
signedFixture = signTransaction ("0x" <> T.replicate 63 "0" <> "1")
  (Tx1559 421614 17 1 10 100000 router 0 "call") >>= either (fail . T.unpack) pure

safeBlock :: Value
safeBlock = object ["number" .= ("0x64" :: Text), "hash" .= ("0x" <> T.replicate 64 "a"), "timestamp" .= ("0x64" :: Text)]

whenMethod :: Eq a => a -> a -> IO () -> IO ()
whenMethod actual expected action = if actual == expected then action else pure ()
