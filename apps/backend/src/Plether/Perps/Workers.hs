-- | The runtime orchestration shared by the indexer executable and integration
-- tests. Vault workers use the same pool as Perps history, but independent loops.
module Plether.Perps.Workers
  ( runPerpsIndexerWithVaults
  , runPerpsIndexerWithVaultsObserved
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Network.HTTP.Client (Manager)
import Plether.Database (DbPool)
import Plether.Ethereum.Client (EthClient)
import Plether.Perps.HistoryIndexer (PerpsIndexerConfig, runPerpsIndexer)
import Plether.Vaults.ActivityIndexer
  ( VaultActivityIndexerConfig
  , startVaultActivityIndexer
  , verifyVaultActivityBindings
  )
import Plether.Vaults.DepositAttributionIndexer (startVaultDepositAttributionIndexer)

runPerpsIndexerWithVaults
  :: Manager -> DbPool -> PerpsIndexerConfig
  -> EthClient -> VaultActivityIndexerConfig -> IO ()
runPerpsIndexerWithVaults = runPerpsIndexerWithVaultsObserved (const $ pure ())

-- The observer makes spawned thread identities available for test cleanup.
-- Normal startup retains the original forkIO ownership and thread lifetimes.
runPerpsIndexerWithVaultsObserved
  :: ([ThreadId] -> IO ())
  -> Manager -> DbPool -> PerpsIndexerConfig
  -> EthClient -> VaultActivityIndexerConfig -> IO ()
runPerpsIndexerWithVaultsObserved started manager pool perpsConfig vaultClient vaultConfig = do
  verifyVaultActivityBindings vaultClient vaultConfig
  activity <- forkIO $ startVaultActivityIndexer vaultClient pool vaultConfig
  attribution <- forkIO $ startVaultDepositAttributionIndexer vaultClient pool vaultConfig
  started [activity, attribution]
  runPerpsIndexer manager pool perpsConfig
