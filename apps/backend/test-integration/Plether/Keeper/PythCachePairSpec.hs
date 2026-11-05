module Plether.Keeper.PythCachePairSpec (pythCachePairSpec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (concurrently_, wait, withAsync)
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple
import Plether.Database.Schema
import Test.Hspec

pythCachePairSpec :: Text -> Spec
pythCachePairSpec url = describe "Pyth cache pairs PostgreSQL" $ do
  it "migrates populated legacy tables safely under concurrent startup" $
    withFixture url $ \reader writer -> do
      legacy writer 101 10
      void $ execute_ writer "ALTER TABLE perps_pyth_update_payloads DROP COLUMN basket_price, DROP COLUMN component_prices"
      concurrently_ (ensureBasketSnapshotSchema reader) (ensureBasketSnapshotSchema writer)
      Just row <- getLatestPairedPythUpdatePayload reader
      puprUpdateData (ppuprPayload row) `shouldBe` bytes 10
      ppuprBasket row `shouldSatisfy` isNothing

  it "keeps a complete pair while a writer commits between legacy reads" $
    withFixture url $ \reader writer -> do
      paired writer 101 10 latest
      Just oldPayload <- getLatestPythUpdatePayload reader
      paired writer 102 20 latest
      Just newBasket <- getLatestBasketSnapshot reader
      puprPublishTimes oldPayload `shouldNotBe` times 102
      bsrComponents newBasket `shouldBe` components 102 20
      assertPair reader 102 20
      insertBasketSnapshotWithSource writer 120 60 999 (components 120 999) "pyth_benchmarks"
      assertPair reader 102 20
      paired writer 121 30 latest
      assertPair reader 121 30

  it "sees the old pair during an uncommitted same-window replacement and the new pair after commit" $
    withFixture url $ \reader writer -> do
      paired writer 101 10 latest
      written <- newEmptyMVar
      commit <- newEmptyMVar
      withAsync (withTransaction writer $ do
        paired writer 101 20 latest
        putMVar written ()
        takeMVar commit) $ \worker -> do
          takeMVar written
          assertPair reader 101 10
          putMVar commit ()
          wait worker
          assertPair reader 101 20

  it "rolls back every field of a failed replacement" $
    withFixture url $ \reader writer -> do
      paired writer 101 10 latest
      (withTransaction writer $ do
        paired writer 101 20 latest
        void $ execute_ writer "SELECT 1 / 0") `shouldThrow` anyException
      assertPair reader 101 10

  it "preserves source precedence for both payload and basket" $
    withFixture url $ \reader writer -> do
      paired writer 101 10 historical
      paired writer 101 20 latest
      rows <- query_ reader "SELECT source, basket_price, component_prices, update_data FROM perps_pyth_update_payloads"
        :: IO [(Text, Integer, Value, Value)]
      rows `shouldBe` [(historical, 10, components 101 10, bytes 10)]
      getLatestPairedPythUpdatePayload reader >>= (`shouldSatisfy` isNothing)
      paired writer 102 30 latest
      paired writer 102 40 historical
      rows' <- query_ reader "SELECT basket_price, component_prices, update_data FROM perps_pyth_update_payloads WHERE min_publish_time = 102"
        :: IO [(Integer, Value, Value)]
      rows' `shouldBe` [(40, components 102 40, bytes 40)]

  it "does not hide an unpaired newest payload behind an older paired row" $
    withFixture url $ \reader writer -> do
      paired writer 101 10 latest
      legacy writer 102 20
      ensureBasketSnapshotSchema writer
      Just row <- getLatestPairedPythUpdatePayload reader
      puprMinPublishTime (ppuprPayload row) `shouldBe` 102
      ppuprBasket row `shouldSatisfy` isNothing
      Just oldConsumer <- getLatestPythUpdatePayload reader
      puprUpdateData oldConsumer `shouldBe` bytes 20

  it "clears both paired fields on a legacy helper replacement" $
    withFixture url $ \reader writer -> do
      paired writer 101 10 latest
      legacy writer 101 20
      Just row <- getLatestPairedPythUpdatePayload reader
      puprUpdateData (ppuprPayload row) `shouldBe` bytes 20
      ppuprBasket row `shouldSatisfy` isNothing

  it "hydrates only an exact signed legacy payload without changing its freshness or chart history" $
    withFixture url $ \reader writer -> do
      legacy writer 101 10
      let hydrate publishTimes updateData source =
            hydratePythUpdatePayloadBasket writer 101 101 publishTimes updateData source 10 (components 101 10)
      hydrate (times 102) (bytes 10) latest `shouldReturn` False
      hydrate (times 101) (bytes 20) latest `shouldReturn` False
      hydrate (times 101) (bytes 10) historical `shouldReturn` False
      hydratePythUpdatePayloadBasket writer 102 102 (times 102) (bytes 10) latest 10 (components 102 10)
        `shouldReturn` False
      hydrate (times 101) (bytes 10) latest `shouldReturn` True
      hydrate (times 101) (bytes 10) latest `shouldReturn` False
      assertPair reader 101 10
      Just row <- getLatestPythUpdatePayload reader
      puprFetchedAt row `shouldBe` 104
      count <- query_ reader "SELECT count(*) FROM perps_basket_snapshots" :: IO [Only Int]
      count `shouldBe` [Only 0]

  it "rejects half-populated pairs and unadmitted writers" $
    withFixture url $ \_ writer -> do
      legacy writer 101 10
      execute_ writer "UPDATE perps_pyth_update_payloads SET basket_price = 1"
        `shouldThrow` (\err -> sqlState err == "23514")
      execute_ writer "UPDATE perps_pyth_update_payloads SET component_prices = '[]'::jsonb"
        `shouldThrow` (\err -> sqlState err == "23514")
      paired writer 102 10 "backend_hermes_latest" `shouldThrow` anyIOException

withFixture :: Text -> (Connection -> Connection -> IO a) -> IO a
withFixture url action =
  bracket (connectPostgreSQL $ encodeUtf8 url) close $ \reader ->
    bracket (connectPostgreSQL $ encodeUtf8 url) close $ \writer -> do
      names <- query_ reader "SELECT current_database()" :: IO [Only Text]
      case names of
        [Only name] | "critical_path" `T.isInfixOf` name -> pure ()
        _ -> fail "Pyth pair tests require a dedicated critical_path database"
      void $ execute_ reader "CREATE SCHEMA liquidation_pair_fixture"
      flip finally (void $ execute_ reader "DROP SCHEMA liquidation_pair_fixture CASCADE") $ do
        void $ execute_ reader "SET search_path TO liquidation_pair_fixture"
        void $ execute_ writer "SET search_path TO liquidation_pair_fixture"
        ensureBasketSnapshotSchema reader
        action reader writer

paired :: Connection -> Integer -> Integer -> Text -> IO ()
paired conn timestamp price source = do
  insertBasketSnapshotWithSource conn (timestamp `div` 60 * 60) 60 price (components timestamp price) source
  insertPairedPythUpdatePayload conn timestamp timestamp (times timestamp) (bytes price) 104 source price (components timestamp price)

legacy :: Connection -> Integer -> Integer -> IO ()
legacy conn timestamp price =
  insertPythUpdatePayload conn timestamp timestamp (times timestamp) (bytes price) 104 latest

assertPair :: Connection -> Integer -> Integer -> IO ()
assertPair conn timestamp price = do
  Just row <- getLatestPairedPythUpdatePayload conn
  puprMinPublishTime (ppuprPayload row) `shouldBe` timestamp
  puprUpdateData (ppuprPayload row) `shouldBe` bytes price
  case ppuprBasket row of
    Nothing -> expectationFailure "expected paired components"
    Just basket -> do
      bsrBasketPrice basket `shouldBe` price
      bsrComponents basket `shouldBe` components timestamp price

times :: Integer -> Value
times timestamp = toJSON [timestamp]

bytes :: Integer -> Value
bytes price = toJSON ["0x" <> T.pack (show price)]

components :: Integer -> Integer -> Value
components timestamp price = toJSON [object ["publish_time" .= timestamp, "price" .= price]]

latest, historical :: Text
latest = "backend_hermes_latest_v2"
historical = "backend_hermes_historical_v2"
