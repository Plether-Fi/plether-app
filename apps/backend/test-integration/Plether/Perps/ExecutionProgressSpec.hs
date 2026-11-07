module Plether.Perps.ExecutionProgressSpec (executionProgressSpec) where

import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (status200, status400)
import Network.Wai (Application)
import Network.Wai.Test
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema (upsertPerpsKeeperOrderCommitted)
import Plether.Perps.CriticalPathSpec (withCriticalPathDatabase, makeApiApplication, testConfig)
import Plether.Perps.CriticalPathFixture
import Test.Hspec

executionProgressSpec :: Text -> Spec
executionProgressSpec url = describe "Canonical-bound execution progress API" $ do
  it "returns submitting then acknowledged locator without promoting committed history" $ fixture url $ \pool app -> do
    seed pool
    first <- get app path
    at first ["data", "timedOut"] `shouldBe` Just (Bool False)
    at first ["data", "order", "terminalStatus"] `shouldBe` Just (String "Committed")
    at first ["data", "execution", "state"] `shouldBe` Just (String "submitting")
    at first ["data", "execution", "clientOrderId"] `shouldBe` Just (String testClientOrderId)
    withDb pool $ \c -> void $ execute c "UPDATE perps_keeper_broadcasts SET submitted_at=clock_timestamp() WHERE order_router=?" (Only testRouter)
    acknowledged <- get app $ path <> "&afterExecution=" <> hashA <> ":submitting"
    at acknowledged ["data", "execution", "state"] `shouldBe` Just (String "submitted")
    at acknowledged ["data", "executionVersion"] `shouldBe` Just (String $ hashA <> ":submitted")
    at acknowledged ["data", "order", "terminalStatus"] `shouldBe` Just (String "Committed")
  it "holds an unchanged progress cursor instead of returning in a busy loop" $ fixture url $ \pool app -> do
    seed pool
    started <- getMonotonicTimeNSec
    response <- get app $ path <> "&afterExecution=" <> hashA <> ":submitting"
    finished <- getMonotonicTimeNSec
    at response ["data", "timedOut"] `shouldBe` Just (Bool True)
    finished - started `shouldSatisfy` (>= 900_000_000)
  it "reports replacement and authoritative clearing while preserving canonical order semantics" $ fixture url $ \pool app -> do
    seed pool
    withDb pool $ \c -> void $ execute c "UPDATE perps_keeper_broadcasts SET tx_hash=? WHERE order_router=?" (hashB, testRouter)
    replacement <- get app $ path <> "&afterExecution=" <> hashA <> ":submitting"
    at replacement ["data", "execution", "transactionHash"] `shouldBe` Just (String hashB)
    withDb pool $ \c -> void $ execute c "DELETE FROM perps_keeper_broadcasts WHERE order_router=?" (Only testRouter)
    cleared <- get app $ path <> "&afterExecution=" <> hashB <> ":submitting"
    at cleared ["data", "execution"] `shouldBe` Just Null
    at cleared ["data", "executionVersion"] `shouldBe` Just (String "none")
    at cleared ["data", "order", "terminalStatus"] `shouldBe` Just (String "Committed")
  it "rejects stale commit identity and another account even when the journal targets that order" $ fixture url $ \pool app -> do
    seed pool
    withDb pool $ \c -> void $ execute c "UPDATE perps_keeper_orders SET commit_tx_hash=? WHERE order_router=? AND order_id=?" (hashB,testRouter,testOrderId)
    stale <- get app path
    at stale ["data", "execution"] `shouldBe` Just Null
    withDb pool $ \c -> void $ execute c "UPDATE perps_keeper_orders SET commit_tx_hash=? WHERE order_router=? AND order_id=?" (commitTxHashA,testRouter,testOrderId)
    other <- get app $ T.replace testAccount ("0x" <> T.replicate 40 "f") path
    at other ["data", "order"] `shouldBe` Just Null
    at other ["data", "execution"] `shouldBe` Just Null
  it "retracts a candidate when the canonical commit disappears and ignores V1 keeper-only status" $ fixture url $ \pool app -> do
    seed pool
    withDb pool $ \c -> do
      void $ execute c "DELETE FROM perps_orders WHERE chain_id=? AND order_router=? AND order_id=?" (testChainId,testRouter,testOrderId)
      void $ execute c "UPDATE perps_keeper_orders SET status='executed',execution_tx_hash=? WHERE order_router=? AND order_id=?" (hashA,testRouter,testOrderId)
    response <- get app $ path <> "&afterExecution=" <> hashA <> ":submitting"
    at response ["data", "order"] `shouldBe` Just Null
    at response ["data", "execution"] `shouldBe` Just Null
    at response ["data", "executionVersion"] `shouldBe` Just (String "none")
  it "returns a keeper terminal hash only as a candidate while history remains committed" $ fixture url $ \pool app -> do
    seed pool
    withDb pool $ \c -> do
      void $ execute c "DELETE FROM perps_keeper_broadcasts WHERE order_router=?" (Only testRouter)
      void $ execute c "UPDATE perps_keeper_orders SET status='executed',execution_tx_hash=? WHERE order_router=? AND order_id=?" (hashA,testRouter,testOrderId)
    response <- get app path
    at response ["data", "execution", "transactionHash"] `shouldBe` Just (String hashA)
    at response ["data", "execution", "state"] `shouldBe` Just (String "submitted")
    at response ["data", "order", "terminalStatus"] `shouldBe` Just (String "Committed")
  it "omits unknown progress during a blocked optional lookup without clearing the browser cursor" $ fixture url $ \pool app -> do
    seed pool
    bracket (connectPostgreSQL $ BS.pack $ T.unpack url) close $ \blocker -> do
      void $ execute_ blocker "BEGIN"
      void $ execute_ blocker "LOCK TABLE perps_keeper_broadcasts IN ACCESS EXCLUSIVE MODE"
      finally (do
        response <- get app $ path <> "&afterExecution=" <> hashA <> ":submitting"
        at response ["data", "timedOut"] `shouldBe` Just (Bool True)
        at response ["data", "order", "terminalStatus"] `shouldBe` Just (String "Committed")
        at response ["data", "execution"] `shouldBe` Nothing
        at response ["data", "executionVersion"] `shouldBe` Nothing)
        (void $ execute_ blocker "ROLLBACK")
    withDb pool (\c -> query_ c "SELECT 1::integer" :: IO [Only Int]) `shouldReturn` [Only 1]
  it "rejects malformed cursors before waiting" $ fixture url $ \_ app -> do
    response <- runSession (request $ setPath defaultRequest $ TE.encodeUtf8 $ path <> "&afterExecution=bad") app
    simpleStatus response `shouldBe` status400

fixture :: Text -> (DbPool -> Application -> IO a) -> IO a
fixture url action = withCriticalPathDatabase url $ \pool -> do
  manager <- newManager defaultManagerSettings
  let rpc = "http://127.0.0.1:1"
  app <- makeApiApplication True manager pool (testConfig url rpc) rpc
  action pool app

seed :: DbPool -> IO ()
seed pool = withDb pool $ \c -> do
  upsertPerpsKeeperOrderCommitted c testRouter testOrderId testAccount 1 100 100 1000 commitTxHashA
  void $ execute c "INSERT INTO perps_orders(chain_id,order_router,order_id,account,side,commit_tx_hash,client_order_id,commit_block_number,commit_timestamp) VALUES(?,?,?,?,1,?,?,100,1000)"
    (testChainId,testRouter,testOrderId,testAccount,commitTxHashA,testClientOrderId)
  void $ execute c "INSERT INTO perps_keeper_broadcasts(order_router,tx_hash,raw_tx,target_order_ids) VALUES(?,?,decode('','hex'),ARRAY[?]::bigint[])"
    (testRouter,hashA,testOrderId)

path :: Text
path = "/api/perps/orders/" <> T.pack (show testOrderId) <> "/wait?router=" <> testRouter <> "&account=" <> testAccount <> "&timeoutSeconds=1"

hashA,hashB :: Text
hashA = "0x" <> T.replicate 64 "a"
hashB = "0x" <> T.replicate 64 "b"

get :: Application -> Text -> IO Value
get app url = do
  response <- runSession (request $ setPath defaultRequest $ TE.encodeUtf8 url) app
  simpleStatus response `shouldBe` status200
  case Aeson.eitherDecode $ simpleBody response of
    Right value -> pure value
    Left _ -> expectationFailure "Expected JSON response" >> pure Null

at :: Value -> [Text] -> Maybe Value
at value [] = Just value
at (Object fields) (key:keys) = KM.lookup (fromStringKey key) fields >>= (\value -> at value keys)
at _ _ = Nothing

fromStringKey :: Text -> Key.Key
fromStringKey = Key.fromText
