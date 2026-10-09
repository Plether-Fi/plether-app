module Main (main) where

import qualified Data.Text as T
import Plether.Config (Config (..),loadConfig)
import Plether.Database
import Plether.Ethereum.Client
import Plether.Perps.Funding.Store (ensureFundingSchema)
import Plether.Perps.Funding.Types (loadFundingDeployment,validateFundingReleaseBinding)
import Plether.Perps.Funding.Worker
import System.Environment (getArgs,lookupEnv)

-- This executable has no signer configuration or transaction submission path.
main :: IO ()
main = do
  cfg <- loadConfig >>= either fail pure
  loaded <- loadFundingDeployment >>= either (fail . T.unpack) (maybe (fail "PERPS_FUNDING_DEPLOYMENT_JSON is required") pure)
  deployment <- either (fail . T.unpack) pure $ validateFundingReleaseBinding (cfgPerpsChainId cfg) (cfgPerpsMarginClearinghouse cfg) (cfgPerpsUsdc cfg) loaded
  database <- maybe (fail "DATABASE_URL is required") pure $ cfgDatabaseUrl cfg
  pool <- newDbPool database
  withDb pool ensureFundingSchema
  destination <- newClientWithOptions $ RpcClientOptions (cfgPerpsRpcUrl cfg) (cfgPerpsRpcAuthToken cfg) "funding-destination"
  sourceUrl <- lookupEnv "PERPS_FUNDING_SOURCE_RPC_URL" >>= \case
    Just url | "https://" `T.isPrefixOf` T.pack url -> pure $ T.pack url
    _ -> fail "PERPS_FUNDING_SOURCE_RPC_URL must be an HTTPS Ethereum RPC"
  auth <- fmap T.pack <$> lookupEnv "PERPS_FUNDING_SOURCE_RPC_AUTH_TOKEN"
  source <- newClientWithOptions $ RpcClientOptions sourceUrl auth "funding-source"
  getArgs >>= \case
    ["--once"] -> reconcileFundingOnce destination source pool deployment >>= either (fail . T.unpack) pure
    [] -> runFundingWorker destination source pool deployment
    ["--loop"] -> runFundingWorker destination source pool deployment
    _ -> fail "Usage: plether-funding-worker [--once|--loop]"
