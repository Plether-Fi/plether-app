module Plether.Vaults.DepositAttributionIndexer
  ( VaultDepositAttributionCycleResult (..)
  , VaultDepositAttributionCycleStats (..)
  , decodeLpRequestState
  , lpRequestStateCall
  , runVaultDepositAttributionCycle
  , startVaultDepositAttributionIndexer
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , finally
  , fromException
  , throwIO
  , try
  )
import Control.Monad (forM, forever, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadWrite)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.Database (DbPool, withDb)
import Plether.Database.VaultActivity
  ( VaultActivityDeployment (..)
  , VaultActivityIndexerStateRow (..)
  , VaultDepositAttributionStateRow (..)
  , VaultDepositRequestKey (..)
  , VaultDepositRequestStateRow (..)
  , countActiveVaultDepositRequests
  , getActiveVaultDepositRequestKeys
  , getVaultActivityIndexerState
  , getVaultDepositAttributionState
  , getVaultDepositRequestKeys
  , resetVaultDepositAttribution
  , recomputeVaultAttributedHolderBalances
  , setVaultDepositAttributionState
  , tryLockVaultDepositAttributionIndexer
  , unlockVaultDepositAttributionIndexer
  , upsertVaultDepositRequestStateExact
  )
import Plether.Ethereum.Abi
  ( decodeUint256
  , encodeAddress
  , encodeBool
  , encodeCall
  , encodeUint256
  )
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , RpcError
  , ethCallAtBlock
  )
import Plether.Ethereum.Rpc (RpcBlock (..), ethGetBlockByNumber)
import Plether.Logging (field, logErrorEvery, logInfoEvery, logWarn)
import Plether.Vaults.ActivityIndexer (VaultActivityIndexerConfig (..))

data VaultDepositAttributionCycleResult
  = VaultDepositAttributionCycleLeaderBusy
  | VaultDepositAttributionCycleWaiting
  | VaultDepositAttributionCycleCompleted VaultDepositAttributionCycleStats
  deriving stock (Eq, Show)

data VaultDepositAttributionCycleStats = VaultDepositAttributionCycleStats
  { vdacrConfirmedThrough :: Integer
  , vdacrSafeHead :: Integer
  , vdacrBackfillComplete :: Bool
  , vdacrRequestsObserved :: Int
  , vdacrActiveRequestCount :: Integer
  , vdacrLagSeconds :: Integer
  , vdacrLastSuccess :: Integer
  }
  deriving stock (Eq, Show)

requestBatchSize :: Int
requestBatchSize = 25

startVaultDepositAttributionIndexer
  :: EthClient
  -> DbPool
  -> VaultActivityIndexerConfig
  -> IO ()
startVaultDepositAttributionIndexer client pool cfg =
  forever $ do
    result <- try @SomeException $ runVaultDepositAttributionCycle client pool cfg
    case result of
      Left err ->
        case fromException err :: Maybe SomeAsyncException of
          Just _ -> throwIO err
          Nothing ->
            logErrorEvery
              60
              "vault_deposit_attribution_iteration_failed"
              "Vault deposit attribution iteration failed"
              [field "error" $ show err]
      Right VaultDepositAttributionCycleLeaderBusy -> pure ()
      Right VaultDepositAttributionCycleWaiting ->
        logInfoEvery
          60
          "vault_deposit_attribution_heartbeat"
          "Vault deposit attribution is waiting for canonical request backfill"
          [field "state" ("backfilling" :: Text)]
      Right (VaultDepositAttributionCycleCompleted completed) ->
        logInfoEvery
          60
          "vault_deposit_attribution_heartbeat"
          "Vault deposit attribution completed a pinned Public Lens poll"
          [ field "state" $ if vdacrBackfillComplete completed then ("ready" :: Text) else "backfilling"
          , field "confirmed_through_block" $ vdacrConfirmedThrough completed
          , field "safe_head_block" $ vdacrSafeHead completed
          , field "lag_blocks" $ max 0 $ vdacrSafeHead completed - vdacrConfirmedThrough completed
          , field "lag_seconds" $ vdacrLagSeconds completed
          , field "requests_observed" $ vdacrRequestsObserved completed
          , field "active_request_count" $ vdacrActiveRequestCount completed
          , field "last_success" $ vdacrLastSuccess completed
          ]
    threadDelay $ max 1_000_000 $ vaicPollIntervalMicros cfg

runVaultDepositAttributionCycle
  :: EthClient
  -> DbPool
  -> VaultActivityIndexerConfig
  -> IO VaultDepositAttributionCycleResult
runVaultDepositAttributionCycle client pool cfg =
  withDb pool $ \conn -> do
    locked <- tryLockVaultDepositAttributionIndexer conn
    if not locked
      then pure VaultDepositAttributionCycleLeaderBusy
      else runWithLock conn `finally` unlockVaultDepositAttributionIndexer conn
 where
  deployment = vaicDeployment cfg

  runWithLock conn = do
    activityState <- getVaultActivityIndexerState conn deployment
    case activityState of
      Just current
        | vaisBackfillComplete current
            && vaisLastIndexedBlock current >= vadDeploymentBlock deployment
            && maybe False (const True) (vaisLastIndexedBlockHash current) -> do
              attributionState <- getVaultDepositAttributionState conn deployment
              reconciled <- reconcileAttributionCursor conn current attributionState
              observeAt current reconciled conn
      _ -> pure VaultDepositAttributionCycleWaiting

  reconcileAttributionCursor
    :: Connection
    -> VaultActivityIndexerStateRow
    -> Maybe VaultDepositAttributionStateRow
    -> IO (Maybe VaultDepositAttributionStateRow)
  reconcileAttributionCursor _ _ Nothing = pure Nothing
  reconcileAttributionCursor conn activityState state@(Just row)
    | vdasConfirmedThroughBlock row > vaisLastIndexedBlock activityState = reset conn row
    | otherwise =
        case vdasConfirmedThroughBlockHash row of
          Nothing -> fail "Vault deposit attribution cursor is missing its canonical block hash"
          Just storedHash -> do
            canonical <- rpcOrFail "verify vault deposit attribution cursor" $
              ethGetBlockByNumber client $ vdasConfirmedThroughBlock row
            if normalize storedHash == normalize (rpcBlockHash canonical)
              then pure state
              else reset conn row
   where
    reset targetConn cursorRow = do
      logWarn
        "vault_deposit_attribution_reorg_detected"
        "Vault deposit attribution cursor changed; rebuilding its deployment-scoped materialization"
        [ field "mismatch_block" $ vdasConfirmedThroughBlock cursorRow
        , field "deployment_block" $ vadDeploymentBlock deployment
        ]
      withTransaction targetConn $ resetVaultDepositAttribution targetConn deployment
      pure Nothing

  observeAt activityState attributionState conn = do
    let targetBlock = vaisLastIndexedBlock activityState
        targetTimestamp = vaisLastIndexedBlockTimestamp activityState
        previousBlock = vdasConfirmedThroughBlock <$> attributionState
    targetHash <- maybe (fail "Vault activity cursor is missing its canonical block hash") pure $
      vaisLastIndexedBlockHash activityState
    newKeys <- getVaultDepositRequestKeys conn deployment previousBlock targetBlock
    activeKeys <- getActiveVaultDepositRequestKeys conn deployment
    let keys = Set.toAscList $ Set.fromList $ newKeys <> activeKeys
    observations <- fmap concat $ forM (chunksOf requestBatchSize keys) $ \batch ->
      mapConcurrently (readRequestState targetBlock targetHash) batch
    canonicalAfter <- rpcOrFail "re-read vault deposit attribution block" $
      ethGetBlockByNumber client targetBlock
    unless
      ( rpcBlockNumber canonicalAfter == targetBlock
          && normalize (rpcBlockHash canonicalAfter) == normalize targetHash
          && rpcBlockTimestamp canonicalAfter == targetTimestamp
      ) $
      fail "Vault deposit attribution block changed during observation"
    withTransactionMode
      TransactionMode
        { isolationLevel = RepeatableRead
        , readWriteMode = ReadWrite
        }
      conn $ do
      currentActivity <- getVaultActivityIndexerState conn deployment
      currentAttribution <- getVaultDepositAttributionState conn deployment
      unless (currentActivity == Just activityState) $
        fail "Vault activity cursor changed before deposit attribution commit"
      unless (currentAttribution == attributionState) $
        fail "Vault deposit attribution cursor changed before commit"
      mapM_ (upsertVaultDepositRequestStateExact conn deployment) observations
      recomputeVaultAttributedHolderBalances
        conn deployment (vadSeniorVault deployment) targetBlock targetHash
      recomputeVaultAttributedHolderBalances
        conn deployment (vadJuniorVault deployment) targetBlock targetHash
      setVaultDepositAttributionState
        conn deployment targetBlock targetHash targetTimestamp True
    activeCount <- countActiveVaultDepositRequests conn deployment
    now <- floor <$> getPOSIXTime
    pure $
      VaultDepositAttributionCycleCompleted
        VaultDepositAttributionCycleStats
          { vdacrConfirmedThrough = targetBlock
          , vdacrSafeHead = vaisSafeHeadBlock activityState
          , vdacrBackfillComplete = targetBlock >= vaisSafeHeadBlock activityState
          , vdacrRequestsObserved = length observations
          , vdacrActiveRequestCount = fromIntegral activeCount
          , vdacrLagSeconds = max 0 $ vaisSafeHeadTimestamp activityState - targetTimestamp
          , vdacrLastSuccess = now
          }

  readRequestState targetBlock targetHash key = do
    calldata <- either (fail . T.unpack) pure $ lpRequestStateCall deployment key
    bytes <- rpcOrFail "read pinned Public Lens LP request state" $
      ethCallAtBlock
        client
        CallParams
          { callTo = vaicPublicLensAddress cfg
          , callData = calldata
          }
        targetBlock
    either (fail . T.unpack) pure $ decodeLpRequestState key targetBlock targetHash bytes

lpRequestStateCall
  :: VaultActivityDeployment
  -> VaultDepositRequestKey
  -> Either Text BS.ByteString
lpRequestStateCall deployment VaultDepositRequestKey {..} = do
  isSenior <-
    if normalize vdrkVaultAddress == normalize (vadSeniorVault deployment)
      then Right True
      else if normalize vdrkVaultAddress == normalize (vadJuniorVault deployment)
        then Right False
        else Left "Vault deposit request belongs to an unknown vault"
  pure $
    encodeCall
      "getLpRequestState(bool,uint256,address)"
      [ encodeBool isSenior
      , encodeUint256 vdrkRequestId
      , encodeAddress vdrkController
      ]

decodeLpRequestState
  :: VaultDepositRequestKey
  -> Integer
  -> Text
  -> BS.ByteString
  -> Either Text VaultDepositRequestStateRow
decodeLpRequestState key@VaultDepositRequestKey {..} observedBlock observedHash bytes = do
  unless (BS.length bytes == 14 * 32) $
    Left "Public Lens LP request state must contain exactly fourteen ABI words"
  vault <- canonicalAddressAt "vault" 0
  controller <- canonicalAddressAt "controller" 2
  let requestId = uintAt 1
      pendingDepositAssets = uintAt 3
      claimableDepositAssets = uintAt 5
      claimableDepositShares = uintAt 6
      refundableDepositAssets = uintAt 11
      redeemRefundPending = uintAt 13
  unless (normalize vault == normalize vdrkVaultAddress) $
    Left "Public Lens LP request state returned a different vault"
  unless (requestId == vdrkRequestId) $
    Left "Public Lens LP request state returned a different request ID"
  unless (normalize controller == normalize vdrkController) $
    Left "Public Lens LP request state returned a different controller"
  unless (redeemRefundPending `elem` [0, 1]) $
    Left "Public Lens LP request state returned a non-canonical boolean"
  pure
    VaultDepositRequestStateRow
      { vdrsKey = key
      , vdrsPendingDepositAssets = pendingDepositAssets
      , vdrsClaimableDepositAssets = claimableDepositAssets
      , vdrsClaimableDepositShares = claimableDepositShares
      , vdrsRefundableDepositAssets = refundableDepositAssets
      , vdrsActive = pendingDepositAssets > 0 || claimableDepositShares > 0
      , vdrsObservedBlock = observedBlock
      , vdrsObservedBlockHash = normalize observedHash
      }
 where
  wordAt index = BS.take 32 $ BS.drop (index * 32) bytes
  uintAt = decodeUint256 . wordAt
  canonicalAddressAt label index =
    let value = wordAt index
     in if BS.take 12 value /= BS.replicate 12 0
          then Left $ "Public Lens LP request state " <> label <> " has non-canonical address padding"
          else Right $ "0x" <> TE.decodeUtf8 (B16.encode $ BS.drop 12 value)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size values =
  let (prefix, suffix) = splitAt (max 1 size) values
   in prefix : chunksOf size suffix

normalize :: Text -> Text
normalize = T.toLower . T.strip

rpcOrFail :: String -> IO (Either RpcError a) -> IO a
rpcOrFail context action = do
  result <- action
  case result of
    Left err -> fail $ context <> ": " <> show err
    Right value -> pure value
