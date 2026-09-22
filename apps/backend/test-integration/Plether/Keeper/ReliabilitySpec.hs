module Plether.Keeper.ReliabilitySpec (reliabilitySpec) where

import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Plether.Keeper.Reliability
import Test.Hspec

reliabilitySpec :: Text -> Spec
reliabilitySpec url = describe "Order reliability PostgreSQL" $ do
  it "counts canonical outcomes, repeated accounts and router/time boundaries" $
    fixture $ \conn -> do
      -- Each router owns its own order namespace. Account casing is irrelevant.
      void $ execute_ conn
        "INSERT INTO perps_keeper_orders VALUES \
        \ ('router',1,'0xAA',99999,'failed',2),\
        \ ('router',2,'0xaa',99998,'failed',2),\
        \ ('router',3,'0xBB',96400,'executed',NULL),\
        \ ('router',4,'0xCC',99997,'failed',8),\
        \ ('router',5,'0xDD',99996,'pending',NULL),\
        \ ('router',6,'0xEE',13600,'failed',2),\
        \ ('router',7,'0xFF',13599,'failed',2),\
        \ ('router',8,'0xGG',100000,'failed',2),\
        \ ('other',1,'0xHH',99999,'failed',2)"
      readOrderReliability conn "router" 100000 `shouldReturn`
        [OrderReliability 3600 5 1 2 1 1 4 1 1, OrderReliability 86400 6 1 3 1 1 5 2 1]
      -- Replaying an observation does not inflate counts.
      readOrderReliability conn "router" 100000 `shouldReturn`
        [OrderReliability 3600 5 1 2 1 1 4 1 1, OrderReliability 86400 6 1 3 1 1 5 2 1]
      void $ execute_ conn "UPDATE perps_keeper_orders SET status='executed' WHERE order_router='router' AND order_id=5"
      readOrderReliability conn "router" 100000 `shouldReturn`
        [OrderReliability 3600 5 2 2 1 0 4 1 1, OrderReliability 86400 6 2 3 1 0 5 2 1]
  it "returns two explicit zero snapshots for an empty cohort" $
    fixture $ \conn -> readOrderReliability conn "router" 100000 `shouldReturn`
      [OrderReliability 3600 0 0 0 0 0 0 0 0, OrderReliability 86400 0 0 0 0 0 0 0 0]
 where
  -- A session-local temporary table shadows any real table; no shared fixture
  -- schema or production rows are changed, even if the test URL is misconfigured.
  fixture action = bracket (connectPostgreSQL $ Text.encodeUtf8 url) close $ \conn -> do
    void $ execute_ conn
      "CREATE TEMP TABLE perps_keeper_orders (order_router text,order_id bigint,account text,commit_time bigint,status text,failure_reason integer,PRIMARY KEY(order_router,order_id))"
    action conn
