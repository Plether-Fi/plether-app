module Main (main) where

import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plether.AA.Kms (newKmsPaymasterSigner)
import Plether.Config (Config (..),loadConfig)
import Plether.Database
import Plether.Ethereum.Client
import Plether.Perps.Funding.Store (ensureFundingSchema)
import Plether.Perps.Funding.Types (loadFundingDeployment)
import Plether.Perps.Funding.Worker
import System.Environment (getArgs,lookupEnv)
import Text.Read (readMaybe)

main :: IO ()
main = do
  cfg <- loadConfig >>= either fail pure
  deployment <- loadFundingDeployment >>= either (fail . T.unpack) (maybe (fail "PERPS_FUNDING_DEPLOYMENT_JSON is required") pure)
  database <- maybe (fail "DATABASE_URL is required") pure $ cfgDatabaseUrl cfg
  pool <- newDbPool database
  withDb pool ensureFundingSchema
  client <- newClientWithOptions $ RpcClientOptions (cfgPerpsRpcUrl cfg) (cfgPerpsRpcAuthToken cfg) "funding-worker"
  execute <- (== Just "true") <$> lookupEnv "PERPS_FUNDING_WORKER_EXECUTE"
  signer <- if not execute then pure Nothing else do
    keyId <- lookupEnv "PERPS_FUNDING_KMS_KEY_ID" >>= maybe (fail "PERPS_FUNDING_KMS_KEY_ID is required") (pure . T.pack)
    address <- lookupEnv "PERPS_FUNDING_SIGNER_ADDRESS" >>= maybe (fail "PERPS_FUNDING_SIGNER_ADDRESS is required") (pure . T.pack)
    manager <- newManager tlsManagerSettings
    attested <- newKmsPaymasterSigner manager keyId address >>= either (fail . T.unpack) pure
    verifyFundingSigner attested >>= either (fail . T.unpack) pure
    pure $ Just attested
  maxCost <- lookupEnv "PERPS_FUNDING_MAX_TX_COST_WEI" >>= \case
    Nothing -> pure 1_000_000_000_000_000
    Just raw -> case readMaybe raw of
      Just value | value > 0 && value <= 100_000_000_000_000_000 -> pure value
      _ -> fail "Invalid PERPS_FUNDING_MAX_TX_COST_WEI"
  args <- getArgs
  case args of
    ["--once"] -> reconcileFundingOnce client pool deployment signer maxCost >>= either (fail . T.unpack) pure
    [] -> runFundingWorker client pool deployment signer maxCost
    ["--loop"] -> runFundingWorker client pool deployment signer maxCost
    _ -> fail "Usage: plether-funding-worker [--once|--loop]"
