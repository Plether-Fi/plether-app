{-# LANGUAGE OverloadedStrings #-}
-- Local-only end-to-end smoke: PostgreSQL + actual deployed contract bytecode
-- on Anvil. Never point this test at the production database or a public RPC.
import Control.Exception (bracket)
import Control.Monad (forM_, unless, void)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Only (..), execute_, query_)
import Plether.Database
import Plether.Database.Schema
import Plether.Ethereum.Client
import qualified Plether.Ethereum.Contracts.Perps as Perps
import Plether.Ethereum.Rpc
import Plether.LiquidationWorker
import Plether.LiquidationWorker.Monitoring
import Plether.Pyth.Basket
import System.Environment (getArgs)

main :: IO ()
main = do
  [fixtureFile] <- getArgs
  bytes <- BL.readFile fixtureFile
  fixture <- either fail pure $ eitherDecode bytes
  (engine, router, oracle, lens, accounts, points, payload) <- either fail pure $ parseEither parseFixture fixture
  client <- newClient "http://127.0.0.1:58546"
  version <- rpcCall client "web3_clientVersion" (toJSON ([] :: [Value]))
  unless (case version of Right (String v) -> "anvil" `T.isInfixOf` T.toLower v; _ -> False) $
    fail "This smoke test only runs on local Anvil"
  Right startBlock <- ethBlockNumber client
  let cfg = LiquidationWorkerConfig
        { lwcChainId = 421614, lwcOrderRouter = router, lwcPletherOracle = oracle
        , lwcCfdEngine = engine, lwcAccountLens = lens
        -- Public, unfunded-on-public-chains Anvil test key. Local fork only.
        , lwcPrivateKey = "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"
        , lwcPollSeconds = 5, lwcScanBatchSize = 20, lwcMulticallSize = 100
        , lwcExecutionBatchSize = 20, lwcIndexerStartBlock = startBlock
        , lwcIndexerConfirmations = 1, lwcIndexerBatchSize = 5000
        , lwcIndexerOverlapBlocks = 12, lwcPendingReplacementSeconds = 120
        , lwcGasBufferBps = 2000, lwcFeeBufferBps = 2500
        , lwcFuturePublishMaxRetries = 2, lwcFuturePublishRetryMaxSeconds = 10
        , lwcMaxTransactionGas = 25000000, lwcPythLatestMaxAgeSeconds = 10
        }
      database = "postgresql://keeper_test@127.0.0.1:58439/keeper_fork_critical_path"
      times = map pppPublishTime points
  bracket (newDbPool database) destroyDbPool $ \pool -> do
    withDb pool $ \conn -> do
      names <- query_ conn "SELECT current_database()" :: IO [Only Text]
      unless (names == [Only "keeper_fork_critical_path"]) $ fail "Unexpected smoke database"
      ensureBasketSnapshotSchema conn
      ensurePerpsLiquidationSchema conn
      -- Dedicated disposable database only. Re-running the test requires a
      -- fresh fork; positions from a previous successful run are already closed.
      void $ execute_ conn "TRUNCATE perps_liquidation_candidates, perps_liquidation_state, perps_pyth_update_payloads, perps_basket_snapshots"
      let Right (price, components) = computeBasketSnapshot points
      insertBasketSnapshotWithSource conn (maximum times) 1 price (toJSON components) "backend_hermes_latest_v2"
      insertPythUpdatePayload conn (minimum times) (maximum times) (toJSON times) (toJSON payload) (maximum times) "backend_hermes_latest_v2"
      forM_ accounts $ \account -> upsertPerpsLiquidationCandidate conn 421614 engine account startBlock
      observeRisk conn 421614 engine [(a, Just True) | a <- accounts]
      void $ execute_ conn "UPDATE perps_liquidation_candidates SET risk_first_observed_at = clock_timestamp() - interval '90 seconds'"
    started <- getPOSIXTime
    runLiquidationWorker cfg pool client LiquidationWorkerOnce False
    ended <- getPOSIXTime
    Right endBlock <- ethBlockNumber client
    Right logs <- ethGetLogs client engine [Perps.positionLiquidatedTopic] startBlock endBlock
    let transactions = nub $ map rpcLogTxHash logs
    health <- withDb pool $ \conn -> readBacklogHealth conn 421614 engine
    pending <- withDb pool $ \conn -> getPendingPerpsLiquidationCandidates conn 421614 engine 120 60
    putStrLn $ "RESULT: elapsed_seconds=" ++ show (realToFrac (ended - started) :: Double)
      ++ " liquidations=" ++ show (length logs) ++ " transactions=" ++ show transactions
      ++ " pending=" ++ show (length pending) ++ " health=" ++ show health
    unless (length logs == length accounts && length transactions >= 2 && null pending && bhRiskCount health == 0) $
      fail "End-to-end fork criteria failed: inspect receipt and candidate logs"
    putStrLn "PASS: actual fork transactions confirmed, all fixture positions liquidated, DB pending state and backlog cleared"
  where
    parseFixture = withObject "fixture" $ \o -> do
      addresses <- o .: "addresses"
      engine <- addresses .: "cfdEngine"
      router <- addresses .: "orderRouter"
      oracle <- addresses .: "pletherOracle"
      lens <- addresses .: "cfdEngineAccountLens"
      accounts <- o .: "accounts"
      feeds <- o .: "feeds"
      points <- traverse (withObject "feed" $ \f -> PythPricePoint
        <$> f .: "id" <*> f .: "price" <*> f .: "confidence" <*> f .: "exponent" <*> f .: "publish_time") feeds
      payload <- o .: "payload"
      pure (engine, router, oracle, lens, accounts :: [Text], points, payload :: [Text])
