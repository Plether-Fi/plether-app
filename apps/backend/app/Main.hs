module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plether.Api (app)
import Plether.Cache (newAppCache)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (newDbPool, withDb)
import Plether.Database.Schema (ensureBasketSnapshotSchema)
import Plether.Ethereum.Client (newClient)
import Plether.Indexer (IndexerConfig (..), startIndexer)
import Plether.Pyth.History (BasketIngestorConfig (..), startBasketHistoryIngestor)
import Web.Scotty (scotty)

main :: IO ()
main = do
  putStrLn "Loading configuration..."
  eConfig <- loadConfig

  case eConfig of
    Left err -> do
      putStrLn $ "Configuration error: " <> err
      putStrLn ""
      putStrLn "Required environment variables:"
      putStrLn "  RPC_URL      - Ethereum RPC endpoint (e.g., https://eth-sepolia.g.alchemy.com/v2/...)"
      putStrLn ""
      putStrLn "Optional environment variables:"
      putStrLn "  CHAIN_ID           - Chain ID (default: 11155111 for Sepolia)"
      putStrLn "  PORT               - Server port (default: 3001)"
      putStrLn "  CORS_ORIGINS       - Space-separated CORS origins (default: http://localhost:5173)"
      putStrLn "  DATABASE_URL       - PostgreSQL connection string (enables transaction history)"
      putStrLn "  INDEXER_START_BLOCK - Block to start indexing from (default: 0)"
    Right cfg -> do
      putStrLn $ "Starting Plether API server on port " <> show (cfgPort cfg)
      putStrLn $ "Chain ID: " <> show (cfgChainId cfg)
      putStrLn ""

      manager <- newManager tlsManagerSettings
      mPool <- case cfgDatabaseUrl cfg of
        Just dbUrl -> do
          putStrLn "Database configured - enabling transaction history"
          pool <- newDbPool dbUrl
          withDb pool ensureBasketSnapshotSchema
          let indexerCfg = IndexerConfig
                { icRpcUrl = cfgRpcUrl cfg
                , icDeployments = cfgDeployments cfg
                , icStartBlock = cfgIndexerStartBlock cfg
                , icBatchSize = 10000
                , icPollInterval = 12000000
                }
          _ <- forkIO $ startIndexer manager pool indexerCfg
          when (cfgPythIngestionEnabled cfg) $ do
            let basketCfg = BasketIngestorConfig
                  { bicBenchmarksUrl = cfgPythBenchmarksUrl cfg
                  , bicBackfillDays = cfgPythBackfillDays cfg
                  , bicSampleIntervalSeconds = cfgPythSampleIntervalSeconds cfg
                  , bicPollSeconds = 15 * 60
                  }
            putStrLn $
              "Pyth basket ingestor enabled - "
                <> show (bicBackfillDays basketCfg)
                <> "d backfill every "
                <> show (bicSampleIntervalSeconds basketCfg)
                <> "s"
            _ <- forkIO $ startBasketHistoryIngestor manager pool basketCfg
            pure ()
          pure $ Just pool
        Nothing -> do
          putStrLn "DATABASE_URL not set - transaction history disabled"
          pure Nothing

      putStrLn ""
      putStrLn "Endpoints:"
      putStrLn "  GET /api/protocol/status"
      putStrLn "  GET /api/protocol/config"
      putStrLn "  GET /api/user/:address/dashboard"
      putStrLn "  GET /api/user/:address/balances"
      putStrLn "  GET /api/user/:address/positions"
      putStrLn "  GET /api/user/:address/allowances"
      putStrLn "  GET /api/quotes/mint?amount="
      putStrLn "  GET /api/quotes/burn?amount="
      putStrLn "  GET /api/quotes/zap?direction=&amount="
      putStrLn "  GET /api/quotes/trade?from=&amount="
      putStrLn "  GET /api/quotes/leverage?side=&principal=&leverage="
      case mPool of
        Just _ -> do
          putStrLn "  GET /api/user/:address/history"
          putStrLn "  GET /api/user/:address/history/leverage"
          putStrLn "  GET /api/user/:address/history/lending"
          putStrLn "  GET /api/perps/basket/history?range="
          putStrLn "  GET /api/perps/basket/latest"
          putStrLn "  GET /api/perps/orders/:orderId/reveal-payload"
          putStrLn "  GET /api/perps/pyth/cached-latest"
        Nothing -> pure ()
      putStrLn "  GET /api/perps/pyth/update?publishTime="
      putStrLn ""

      client <- newClient (cfgRpcUrl cfg)
      cache <- newAppCache
      scotty (cfgPort cfg) (app cache client cfg mPool manager)
