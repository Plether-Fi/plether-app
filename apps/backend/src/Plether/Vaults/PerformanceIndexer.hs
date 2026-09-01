module Plether.Vaults.PerformanceIndexer
  ( VaultPerformanceIndexerConfig (..)
  , VaultPerformanceCycleResult (..)
  , VaultPerformanceLoopDecision (..)
  , SnapshotReconciliationDecision (..)
  , vaultHistoryEpochSeconds
  , vaultHistoryPointCount
  , vaultEpochBoundaries
  , findLastBlockAtOrBeforeTimestamp
  , snapshotNeedsRepair
  , decideSnapshotReconciliation
  , validateSampledBlockIdentity
  , decideVaultPerformanceLoop
  , runVaultPerformanceIndexerCycle
  , startVaultPerformanceIndexer
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , displayException
  , finally
  , fromException
  , throwIO
  , try
  )
import Control.Monad (foldM, forever, unless)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import Plether.Database (DbPool, withDb)
import Plether.Database.VaultPerformance
  ( VaultPerformanceSnapshotRow (..)
  , getVaultPerformanceSnapshots
  , upsertVaultPerformanceSnapshot
  )
import Plether.Ethereum.Client (EthClient, RpcError)
import Plether.Ethereum.Contracts.TrancheVault
  ( TrancheVaultSnapshot (..)
  , decodeTrancheVaultSnapshotResults
  , decodeVaultDecimalsResults
  , trancheVaultSnapshotCalls
  , vaultDecimalsCalls
  )
import qualified Plether.Ethereum.Multicall as Multicall
import Plether.Ethereum.Rpc (RpcBlock (..), ethGetBlockByNumber, ethLatestBlock)
import Plether.Logging (field, logErrorEvery, logInfo)

data VaultPerformanceIndexerConfig = VaultPerformanceIndexerConfig
  { vpicChainId :: Integer
  , vpicAssetAddress :: Text
  , vpicHousePoolAddress :: Text
  , vpicSeniorVaultAddress :: Text
  , vpicJuniorVaultAddress :: Text
  , vpicDeploymentBlock :: Integer
  , vpicConfirmations :: Integer
  }
  deriving stock (Eq, Show)

data VaultPerformanceCycleResult
  = VaultPerformanceCycleCompleted Integer
  | VaultPerformanceCycleLeaderBusy
  deriving stock (Eq, Show)

data VaultPerformanceLoopDecision
  = RetryVaultPerformanceCycle
  | RecordCompletedVaultBoundary Integer
  deriving stock (Eq, Show)

data SnapshotReconciliationDecision
  = KeepCanonicalSnapshot RpcBlock
  | ResampleSnapshot
  deriving stock (Eq, Show)

vaultHistoryEpochSeconds :: Integer
vaultHistoryEpochSeconds = 3_600

vaultHistoryPointCount :: Int
vaultHistoryPointCount = 169

vaultHistoryPollSeconds :: Int
vaultHistoryPollSeconds = 30

vaultPerformanceLeaderLockId :: Integer
vaultPerformanceLeaderLockId = 8_612_047_531

-- | Hourly UTC boundaries with a complete seven-day interval (169 points),
-- clipped so no request predates the configured deployment block timestamp.
vaultEpochBoundaries :: Integer -> Integer -> [Integer]
vaultEpochBoundaries deploymentTimestamp safeTimestamp
  | safeTimestamp < 0 = []
  | firstBoundary > latestBoundary = []
  | otherwise = [firstBoundary, firstBoundary + vaultHistoryEpochSeconds .. latestBoundary]
  where
    latestBoundary = alignDown safeTimestamp vaultHistoryEpochSeconds
    earliestDesired =
      latestBoundary
        - fromIntegral (vaultHistoryPointCount - 1) * vaultHistoryEpochSeconds
    firstBoundary = max (alignUp deploymentTimestamp vaultHistoryEpochSeconds) earliestDesired

-- | Find the greatest block whose timestamp is at or before the boundary.
-- Both endpoints are fetched and validated, which keeps the helper safe for
-- archive providers with non-genesis lower bounds.
findLastBlockAtOrBeforeTimestamp
  :: (Integer -> IO (Either e RpcBlock))
  -> Integer
  -> Integer
  -> Integer
  -> IO (Either e (Maybe RpcBlock))
findLastBlockAtOrBeforeTimestamp fetch lowerNumber upperNumber targetTimestamp
  | lowerNumber < 0 || upperNumber < lowerNumber = pure $ Right Nothing
  | otherwise = do
      lowerResult <- fetch lowerNumber
      case lowerResult of
        Left err -> pure $ Left err
        Right lower
          | rpcBlockTimestamp lower > targetTimestamp -> pure $ Right Nothing
          | otherwise -> do
              upperResult <- fetch upperNumber
              case upperResult of
                Left err -> pure $ Left err
                Right upper
                  | rpcBlockTimestamp upper <= targetTimestamp -> pure $ Right $ Just upper
                  | otherwise -> search lower upper
  where
    search lower upper
      | rpcBlockNumber upper - rpcBlockNumber lower <= 1 = pure $ Right $ Just lower
      | otherwise = do
          let candidateNumber = interpolatedBlockNumber lower upper targetTimestamp
          candidateResult <- fetch candidateNumber
          case candidateResult of
            Left err -> pure $ Left err
            Right candidate
              | rpcBlockTimestamp candidate <= targetTimestamp -> search candidate upper
              | otherwise -> search lower candidate

-- Timestamp interpolation converges much faster than a numeric midpoint on
-- chains with stable block cadence. The clamp guarantees progress even across
-- duplicated timestamps or a temporarily irregular block rate.
interpolatedBlockNumber :: RpcBlock -> RpcBlock -> Integer -> Integer
interpolatedBlockNumber lower upper targetTimestamp =
  max lowerBound $ min upperBound estimate
 where
  lowerBound = rpcBlockNumber lower + 1
  upperBound = rpcBlockNumber upper - 1
  timestampSpan = rpcBlockTimestamp upper - rpcBlockTimestamp lower
  estimate
    | timestampSpan <= 0 =
        rpcBlockNumber lower
          + (rpcBlockNumber upper - rpcBlockNumber lower) `div` 2
    | otherwise =
        rpcBlockNumber lower
          + (targetTimestamp - rpcBlockTimestamp lower)
            * (rpcBlockNumber upper - rpcBlockNumber lower)
            `div` timestampSpan

snapshotNeedsRepair :: VaultPerformanceSnapshotRow -> RpcBlock -> Bool
snapshotNeedsRepair row block =
  vpsBlockNumber row /= rpcBlockNumber block
    || normalizeHash (vpsBlockHash row) /= normalizeHash (rpcBlockHash block)
    || vpsBlockTimestamp row /= rpcBlockTimestamp block
    || rpcBlockTimestamp block > vpsEpochTimestamp row

-- | Decide whether an existing checkpoint is already the canonical value.
-- Missing rows, failed canonical block reads, and changed block identities all
-- flow through the same exact-block resampling/upsert path.
decideSnapshotReconciliation
  :: Maybe VaultPerformanceSnapshotRow
  -> Maybe RpcBlock
  -> SnapshotReconciliationDecision
decideSnapshotReconciliation (Just row) (Just block)
  | not $ snapshotNeedsRepair row block = KeepCanonicalSnapshot block
decideSnapshotReconciliation _ _ = ResampleSnapshot

validateSampledBlockIdentity :: RpcBlock -> RpcBlock -> Either Text RpcBlock
validateSampledBlockIdentity discovered verified
  | sameBlock discovered verified = Right verified
  | otherwise = Left "Vault snapshot block changed during exact-block sampling"

-- | Only a fully completed reconciliation advances the in-process epoch
-- cursor. Archive failures and advisory-lock contention deliberately leave it
-- unchanged so the 30-second loop retries the same boundary.
decideVaultPerformanceLoop
  :: Either e VaultPerformanceCycleResult
  -> VaultPerformanceLoopDecision
decideVaultPerformanceLoop = \case
  Right (VaultPerformanceCycleCompleted boundary) -> RecordCompletedVaultBoundary boundary
  Right VaultPerformanceCycleLeaderBusy -> RetryVaultPerformanceCycle
  Left _ -> RetryVaultPerformanceCycle

-- | Perform a full backfill/repair pass for the latest seven-day window. The
-- primary client establishes a numeric confirmation-delayed upper block. The
-- history client performs old exact-block reads and may be the same client.
runVaultPerformanceIndexerCycle
  :: EthClient
  -> EthClient
  -> DbPool
  -> VaultPerformanceIndexerConfig
  -> IO VaultPerformanceCycleResult
runVaultPerformanceIndexerCycle primaryClient historyClient pool cfg = do
  safeBlock <- loadConfirmedBlock primaryClient cfg
  withVaultPerformanceLeadership pool $ \conn -> do
    validateVaultDecimals primaryClient cfg safeBlock
    deploymentBlock <- rpcOrFail "read vault deployment block" $
      ethGetBlockByNumber historyClient (vpicDeploymentBlock cfg)
    let boundaries =
          vaultEpochBoundaries
            (rpcBlockTimestamp deploymentBlock)
            (rpcBlockTimestamp safeBlock)
        latestBoundary =
          case reverse boundaries of
            latest : _ -> latest
            [] -> alignDown (rpcBlockTimestamp safeBlock) vaultHistoryEpochSeconds
    stored <-
      getVaultPerformanceSnapshots
        conn
        (vpicChainId cfg)
        (vpicHousePoolAddress cfg)
        (vpicSeniorVaultAddress cfg)
        (vpicJuniorVaultAddress cfg)
        vaultHistoryPointCount
    let storedByEpoch = Map.fromList [(vpsEpochTimestamp row, row) | row <- stored]
    -- Publish the newest checkpoint first. A history-provider outage can then
    -- delay old backfill without making an otherwise current series stale.
    -- After that, fill missing checkpoints before revalidating stored rows so
    -- a partial archive-provider budget always increases usable coverage.
    case reverse boundaries of
      [] -> pure ()
      latest : _ -> do
        _ <-
          reconcileBoundary
            conn
            primaryClient
            historyClient
            cfg
            safeBlock
            storedByEpoch
            latestBoundary
            deploymentBlock
            latest
        let historicalBoundaries = init boundaries
            missingBoundaries =
              filter (`Map.notMember` storedByEpoch) historicalBoundaries
            storedBoundaries =
              filter (`Map.member` storedByEpoch) historicalBoundaries
            missingLowerBlock =
              case missingBoundaries of
                firstMissing : _ ->
                  maybe deploymentBlock (snapshotRowBlock . snd) $
                    Map.lookupLT firstMissing storedByEpoch
                [] -> deploymentBlock
        _ <-
          foldM
            (reconcileBoundary conn primaryClient historyClient cfg safeBlock storedByEpoch latestBoundary)
            missingLowerBlock
            missingBoundaries
        _ <-
          foldM
            (reconcileBoundary conn primaryClient historyClient cfg safeBlock storedByEpoch latestBoundary)
            deploymentBlock
            storedBoundaries
        pure ()
    logInfo
      "vault_performance_indexer_cycle_complete"
      "Vault performance checkpoints are reconciled"
      [ field "chain_id" $ vpicChainId cfg
      , field "house_pool" $ normalizeAddress $ vpicHousePoolAddress cfg
      , field "boundary_count" $ length boundaries
      , field "latest_epoch_timestamp" latestBoundary
      ]
    pure latestBoundary

startVaultPerformanceIndexer
  :: EthClient
  -> EthClient
  -> DbPool
  -> VaultPerformanceIndexerConfig
  -> IO ()
startVaultPerformanceIndexer primaryClient historyClient pool cfg = do
  lastCompletedBoundary <- newIORef Nothing
  forever $ do
    safeResult <- try @SomeException $ loadConfirmedBlock primaryClient cfg
    case safeResult of
      Left err -> logIndexerException err
      Right safeBlock -> do
        let currentBoundary = alignDown (rpcBlockTimestamp safeBlock) vaultHistoryEpochSeconds
        previousBoundary <- readIORef lastCompletedBoundary
        unless (previousBoundary == Just currentBoundary) $ do
          cycleResult <-
            try @SomeException $
              runVaultPerformanceIndexerCycle primaryClient historyClient pool cfg
          case cycleResult of
            Left err -> logIndexerException err
            Right _ -> pure ()
          case decideVaultPerformanceLoop cycleResult of
            RetryVaultPerformanceCycle -> pure ()
            RecordCompletedVaultBoundary completedBoundary ->
              writeIORef lastCompletedBoundary $ Just completedBoundary
    threadDelay $ vaultHistoryPollSeconds * 1_000_000

reconcileBoundary
  :: Connection
  -> EthClient
  -> EthClient
  -> VaultPerformanceIndexerConfig
  -> RpcBlock
  -> Map.Map Integer VaultPerformanceSnapshotRow
  -> Integer
  -> RpcBlock
  -> Integer
  -> IO RpcBlock
reconcileBoundary conn primaryClient historyClient cfg safeBlock storedByEpoch latestBoundary lowerBlock epochTimestamp = do
  let existing = Map.lookup epochTimestamp storedByEpoch
      primaryPredecessor = do
        (_, predecessor) <- Map.lookupLT epochTimestamp storedByEpoch
        if epochTimestamp - vpsEpochTimestamp predecessor <= vaultHistoryEpochSeconds
          then Just predecessor
          else Nothing
      usePrimary = epochTimestamp == latestBoundary && maybe False (const True) primaryPredecessor
      client = if usePrimary then primaryClient else historyClient
  searchLowerBlock <-
    case primaryPredecessor of
      Just predecessor | usePrimary ->
        rpcOrFail "read recent vault-history predecessor" $
          ethGetBlockByNumber primaryClient $ vpsBlockNumber predecessor
      _ -> pure lowerBlock
  canonicalExisting <- case existing of
    Nothing -> pure Nothing
    Just row -> do
      blockResult <- pacedArchiveBlockLookup client $ vpsBlockNumber row
      pure $ either (const Nothing) Just blockResult
  case decideSnapshotReconciliation existing canonicalExisting of
    KeepCanonicalSnapshot block -> pure block
    ResampleSnapshot -> do
      mBlock <-
        rpcOrFail "resolve vault hourly boundary" $
          findLastBlockAtOrBeforeTimestamp
            (\blockNumber ->
              pacedArchiveBlockLookup client blockNumber
            )
            (rpcBlockNumber searchLowerBlock)
            (rpcBlockNumber safeBlock)
            epochTimestamp
      block <- maybe (fail "No block exists at or before the vault epoch boundary") pure mBlock
      snapshot <- sampleVaultPerformanceAtBlock client cfg epochTimestamp block
      upsertVaultPerformanceSnapshot conn snapshot
      pure block

sampleVaultPerformanceAtBlock
  :: EthClient
  -> VaultPerformanceIndexerConfig
  -> Integer
  -> RpcBlock
  -> IO VaultPerformanceSnapshotRow
sampleVaultPerformanceAtBlock client cfg epochTimestamp block = do
  results <-
    rpcOrFail "sample both tranche vaults" $
      pacedRpcResult $
        Multicall.multicallAtBlock
          client
          (trancheVaultSnapshotCalls (vpicSeniorVaultAddress cfg) (vpicJuniorVaultAddress cfg))
          (rpcBlockNumber block)
  (senior, junior) <- either (fail . T.unpack) pure $ decodeTrancheVaultSnapshotResults results
  -- A reorg between block discovery and eth_call must not publish values under
  -- a stale hash. Re-read the numeric block after the Multicall and retry the
  -- entire cycle if its identity changed.
  verifiedBlock <- rpcOrFail "verify sampled vault block" $
    pacedArchiveBlockLookup client $ rpcBlockNumber block
  canonicalBlock <-
    either (fail . T.unpack) pure $
      validateSampledBlockIdentity block verifiedBlock
  pure $
    VaultPerformanceSnapshotRow
      { vpsChainId = vpicChainId cfg
      , vpsHousePoolAddress = normalizeAddress $ vpicHousePoolAddress cfg
      , vpsSeniorVaultAddress = normalizeAddress $ vpicSeniorVaultAddress cfg
      , vpsJuniorVaultAddress = normalizeAddress $ vpicJuniorVaultAddress cfg
      , vpsEpochTimestamp = epochTimestamp
      , vpsBlockNumber = rpcBlockNumber canonicalBlock
      , vpsBlockHash = normalizeHash $ rpcBlockHash canonicalBlock
      , vpsBlockTimestamp = rpcBlockTimestamp canonicalBlock
      , vpsSeniorTotalAssets = tvsTotalAssets senior
      , vpsSeniorTotalSupply = tvsTotalSupply senior
      , vpsSeniorSharePriceWad = tvsSharePriceWad senior
      , vpsJuniorTotalAssets = tvsTotalAssets junior
      , vpsJuniorTotalSupply = tvsTotalSupply junior
      , vpsJuniorSharePriceWad = tvsSharePriceWad junior
      }

loadConfirmedBlock :: EthClient -> VaultPerformanceIndexerConfig -> IO RpcBlock
loadConfirmedBlock client cfg = do
  headBlock <- rpcOrFail "read live vault-history head" $ ethLatestBlock client
  let confirmedNumber = rpcBlockNumber headBlock - max 0 (vpicConfirmations cfg)
  if confirmedNumber < vpicDeploymentBlock cfg
    then fail "Confirmed vault-history head predates the configured deployment block"
    else rpcOrFail "read confirmation-delayed vault-history head" $
      ethGetBlockByNumber client confirmedNumber

withVaultPerformanceLeadership
  :: DbPool
  -> (Connection -> IO Integer)
  -> IO VaultPerformanceCycleResult
withVaultPerformanceLeadership pool action =
  withDb pool $ \conn -> do
    rows <-
      query conn "SELECT pg_try_advisory_lock(?)" (Only vaultPerformanceLeaderLockId)
        :: IO [Only Bool]
    case rows of
      [Only True] -> VaultPerformanceCycleCompleted <$> (action conn `finally` release conn)
      [Only False] -> do
        logInfo
          "vault_performance_indexer_leader_busy"
          "Another API replica owns vault performance indexing"
          []
        pure VaultPerformanceCycleLeaderBusy
      _ -> fail "Vault performance leader lock lookup was not unique"
 where
  release conn = do
    _ <-
      query conn "SELECT pg_advisory_unlock(?)" (Only vaultPerformanceLeaderLockId)
        :: IO [Only Bool]
    pure ()

rpcOrFail :: String -> IO (Either RpcError a) -> IO a
rpcOrFail context action = do
  result <- action
  case result of
    Left err -> fail $ context <> ": " <> show err
    Right value -> pure value

-- Public archive RPCs can transiently report an indexed historical block as
-- missing. Retrying the individual lookup avoids abandoning an otherwise
-- healthy 169-checkpoint reconciliation and waiting for the next poll cycle.
retryRpcResult :: Int -> IO (Either e a) -> IO (Either e a)
retryRpcResult retries action = do
  result <- action
  case result of
    Left _ | retries > 0 -> do
      threadDelay 250_000
      retryRpcResult (retries - 1) action
    _ -> pure result

-- The public Blockscout archive endpoint is deliberately rate-limited. Pace
-- the binary-search reads so a full seven-day backfill completes in one pass
-- instead of repeatedly exhausting the provider's burst allowance.
pacedArchiveBlockLookup :: EthClient -> Integer -> IO (Either RpcError RpcBlock)
pacedArchiveBlockLookup client blockNumber =
  pacedRpcResult $ ethGetBlockByNumber client blockNumber

pacedRpcResult :: IO (Either e a) -> IO (Either e a)
pacedRpcResult action = do
  result <- retryRpcResult 4 action
  threadDelay 125_000
  pure result

validateVaultDecimals
  :: EthClient
  -> VaultPerformanceIndexerConfig
  -> RpcBlock
  -> IO ()
validateVaultDecimals client cfg safeBlock = do
  results <-
    rpcOrFail "read vault token decimals" $
      Multicall.multicallAtBlock
        client
        ( vaultDecimalsCalls
            (vpicAssetAddress cfg)
            (vpicSeniorVaultAddress cfg)
            (vpicJuniorVaultAddress cfg)
        )
        (rpcBlockNumber safeBlock)
  either (fail . T.unpack) pure $ decodeVaultDecimalsResults results

sameBlock :: RpcBlock -> RpcBlock -> Bool
sameBlock left right =
  rpcBlockNumber left == rpcBlockNumber right
    && normalizeHash (rpcBlockHash left) == normalizeHash (rpcBlockHash right)
    && rpcBlockTimestamp left == rpcBlockTimestamp right

normalizeHash :: Text -> Text
normalizeHash = T.toLower . T.strip

snapshotRowBlock :: VaultPerformanceSnapshotRow -> RpcBlock
snapshotRowBlock row =
  RpcBlock
    { rpcBlockNumber = vpsBlockNumber row
    , rpcBlockL1Number = Nothing
    , rpcBlockHash = vpsBlockHash row
    , rpcBlockTimestamp = vpsBlockTimestamp row
    }

normalizeAddress :: Text -> Text
normalizeAddress = T.toLower . T.strip

alignDown :: Integer -> Integer -> Integer
alignDown timestamp interval = timestamp - timestamp `mod` interval

alignUp :: Integer -> Integer -> Integer
alignUp timestamp interval
  | timestamp `mod` interval == 0 = timestamp
  | otherwise = alignDown timestamp interval + interval

logIndexerException :: SomeException -> IO ()
logIndexerException err =
  case fromException err :: Maybe SomeAsyncException of
    Just _ -> throwIO err
    Nothing ->
      logErrorEvery
        60
        "vault_performance_indexer_failed"
        "Vault performance indexer failed"
        [field "error" $ displayException err]
