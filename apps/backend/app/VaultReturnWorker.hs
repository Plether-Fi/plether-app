module VaultReturnWorker (startVaultReturnWorker) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (filterM, forever)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Plether.Config (Config (..))
import Plether.Logging (field, logInfo, logWarn)
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Process (CreateProcess (..), proc, waitForProcess, withCreateProcess)

-- Read-only RPC worker; no signer or transaction-submission credentials are
-- passed to this child. PostgreSQL elects one leader across API replicas.
startVaultReturnWorker :: Config -> IO ()
startVaultReturnWorker cfg = forever $ do
  outcome <- try cycleWorker :: IO (Either SomeException ())
  case outcome of
    Left err -> case (fromException err :: Maybe SomeAsyncException) of
      Just async -> throwIO async
      Nothing -> logWarn "vault_return_worker_failed" "Return attribution worker will retry" []
    Right () -> pure ()
  threadDelay (30 * 1000000)
 where
  cycleWorker = do
    paths <- filterM doesFileExist ["/app/return-attribution/main.mjs", "return-attribution/main.mjs", "apps/backend/return-attribution/main.mjs"]
    script <- maybe (fail "Return attribution worker is missing") pure $ listToMaybe paths
    inherited <- getEnvironment
    let allowed = ["PATH", "HOME", "SSL_CERT_FILE", "SSL_CERT_DIR", "NODE_EXTRA_CA_CERTS", "PGSSLROOTCERT", "VAULT_RETURN_ATTRIBUTION_RPC_URL", "VAULT_RETURN_ATTRIBUTION_RPC_AUTH_TOKEN"]
        deployment = object
          [ "housePool" .= cfgVaultHistoryHousePoolAddress cfg
          , "seniorVault" .= cfgVaultHistorySeniorVaultAddress cfg
          , "juniorVault" .= cfgVaultHistoryJuniorVaultAddress cfg
          ]
        variables = filter ((`elem` allowed) . fst) inherited <>
          [("PERPS_RPC_URL", T.unpack $ cfgPerpsRpcUrl cfg), ("VAULT_RETURN_DEPLOYMENT", BL.unpack $ encode deployment)] <>
          maybe [] (\value -> [("DATABASE_URL", T.unpack value)]) (cfgDatabaseUrl cfg) <>
          maybe [] (\value -> [("PERPS_RPC_AUTH_TOKEN", T.unpack value)]) (cfgPerpsRpcAuthToken cfg)
    logInfo "vault_return_worker_started" "Starting canonical junior return attribution" [field "chain_id" $ cfgPerpsChainId cfg]
    withCreateProcess (proc "node" [script]) {env = Just variables} $ \_ _ _ process -> do
      _ <- waitForProcess process
      pure ()
