module Main (main) where

import qualified Data.Text as T
import Plether.Insights.DatabaseSpec (insightsDatabaseSpec)
import Plether.Insights.RegistrationDatabaseSpec (registrationDatabaseSpec)
import Plether.Keeper.ProtectionDatabaseSpec (protectionDatabaseSpec)
import Plether.Keeper.LpSettlementDatabaseSpec (lpSettlementDatabaseSpec)
import Plether.Keeper.LpSettlementWorkerSpec (lpSettlementWorkerSpec)
import Plether.Perps.CandleRollupSpec (candleRollupSpec)
import Plether.Perps.CriticalPathSpec (criticalPathSpec)
import Plether.Vaults.ActivityDatabaseSpec (vaultActivityDatabaseSpec)
import Plether.Vaults.ActivityIndexerIntegrationSpec (vaultActivityIndexerIntegrationSpec)
import System.Environment (lookupEnv)
import System.Exit (die)
import Test.Hspec (hspec)

main :: IO ()
main = do
  required <- (== Just "1") <$> lookupEnv "PERPS_CRITICAL_PATH_REQUIRED"
  databaseUrl <- lookupEnv "PERPS_CRITICAL_PATH_DATABASE_URL"
  case databaseUrl of
    Just value ->
      hspec $ do
        criticalPathSpec $ T.pack value
        protectionDatabaseSpec $ T.pack value
        lpSettlementDatabaseSpec $ T.pack value
        lpSettlementWorkerSpec $ T.pack value
        candleRollupSpec $ T.pack value
        insightsDatabaseSpec $ T.pack value
        registrationDatabaseSpec $ T.pack value
        vaultActivityDatabaseSpec $ T.pack value
        vaultActivityIndexerIntegrationSpec $ T.pack value
    Nothing
      | required ->
          die
            "PERPS_CRITICAL_PATH_DATABASE_URL is required when \
            \PERPS_CRITICAL_PATH_REQUIRED=1"
      | otherwise ->
          putStrLn
            "Perps critical-path integration test not requested. Set \
            \PERPS_CRITICAL_PATH_DATABASE_URL to run it."
