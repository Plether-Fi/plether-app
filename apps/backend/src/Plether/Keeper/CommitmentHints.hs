-- Optional, bounded, read-only chain observations. These hints may wake the
-- ordinary keeper or prewarm signed Pyth data; they can never execute an order.
module Plether.Keeper.CommitmentHints
  ( CommitmentHintSink, startCommitmentHints, stopCommitmentHints, enqueueCommitmentHint
  , loadProvisionalRevealCandidates, ensureCommitmentHintSchema
  , validCommitmentHash, expectedClientOrderId, verifyCommitmentReceipt
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM
import Control.Exception (SomeException, SomeAsyncException, fromException, throwIO, try)
import Control.Monad (forM_, forever, void, when, guard)
import Data.Aeson (Value(..), toJSON)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Char (isHexDigit)
import Data.List (sortOn)
import Data.IORef (newIORef,readIORef,writeIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTimeNSec)
import Database.PostgreSQL.Simple (withTransaction)
import Plether.AA.OrderDiagnostics (receiptOrder)
import qualified Plether.AA.Pimlico as Policy
import Plether.Config (Config(..), NativeAaConfig(..))
import Plether.Database (DbPool, withDb)
import Plether.Database.CommitmentHints
import Plether.Ethereum.Abi (keccak256)
import Plether.Ethereum.Client (EthClient, RpcError(..), newClient, rpcCall, withoutRpcFallback)
import qualified Plether.Ethereum.Contracts.Perps as Perps
import Plether.Ethereum.Rpc
import Plether.Keeper.Wakeup
import Plether.Logging (field,logInfo,logInfoEvery,logWarnEvery)
import qualified Plether.Perps.Manifest as Manifest
import System.Timeout (timeout)

data HintMetrics = HintMetrics { acceptedCount :: Int, droppedCount :: Int, coalescedCount :: Int, recoveredCount :: Int, verifiedCount :: Int }
data CommitmentHintSink = CommitmentHintSink (TBQueue Text) (TVar HintMetrics) ThreadId

-- Process-scoped ownership also lets bounded callers stop optional work before
-- releasing its database pool. Cancellation is preserved by the watcher loops.
stopCommitmentHints :: CommitmentHintSink -> IO ()
stopCommitmentHints (CommitmentHintSink _ _ worker) = killThread worker

validCommitmentHash :: Text -> Bool
validCommitmentHash value = T.length value == 66 && "0x" `T.isPrefixOf` value
  && T.all isHexDigit (T.drop 2 value)

-- No database/network operations, waits, forks per request, or swallowed async
-- exceptions. The existing submitted journal repairs a dropped optional signal.
enqueueCommitmentHint :: Maybe CommitmentHintSink -> Text -> IO ()
enqueueCommitmentHint Nothing _ = pure ()
enqueueCommitmentHint (Just (CommitmentHintSink queue metrics _)) hash = when (validCommitmentHash hash) $
  atomically $ do
    full <- isFullTBQueue queue
    if full then modifyTVar' metrics (\m -> m{droppedCount=min 1000000000 (droppedCount m+1)})
      else writeTBQueue queue (T.toLower hash) >> modifyTVar' metrics (\m -> m{acceptedCount=min 1000000000 (acceptedCount m+1)})

startCommitmentHints :: Config -> DbPool -> EthClient -> IO (Maybe CommitmentHintSink)
startCommitmentHints cfg pool supplied = do
  mode <- readWakeupMode cfg
  case (mode,cfgNativeAaConfig cfg,cfgPerpsOrderLifecycleBook cfg) of
    (WakeupsOff,_,_) -> pure Nothing
    (_,Just native,Just lifecycle) | cfgPerpsChainId cfg == 421614
      && same (cfgPerpsOrderRouter cfg) Manifest.orderRouterAddress
      && same lifecycle Manifest.orderLifecycleBookAddress -> do
        queue <- newTBQueueIO 128
        metrics <- newTVarIO $ HintMetrics 0 0 0 0 0
        -- All bootstrap, schema, provider and recovery work is off the signed
        -- submission path. A missing table/failed worker cannot block trading.
        worker <- forkIO $ forever $ do
          safe $ do
            withDb pool ensureCommitmentHintSchema
            alto <- newClient (naaAltoRpcUrl native)
            logInfo "commitment_hint_watcher_started" "Optional commitment watcher started" [field "mode" (T.pack $ show mode),field "batch_limit" (4::Int),field "queue_capacity" (128::Int)]
            watcher mode native lifecycle queue metrics alto
          threadDelay 5_000_000
        pure $ Just $ CommitmentHintSink queue metrics worker
    _ -> pure Nothing
 where
  primary = withoutRpcFallback supplied
  router = cfgPerpsOrderRouter cfg
  safe action = do
    outcome <- try action
    case outcome of
      Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
        Just cancellation -> throwIO cancellation
        Nothing -> logWarnEvery 60 "commitment_hint_unavailable" "Optional commitment hint work unavailable; ordinary scanning continues" []
      Right () -> pure ()
  watcher mode native lifecycle queue metrics alto = do
    recoveryAt <- newIORef 0
    active <- newIORef False
    forever $ do
      busy <- readIORef active
      tick <- registerDelay (if busy then 250_000 else 1_000_000)
      preferred <- atomically $
        (Just <$> readTBQueue queue) `orElse` (readTVar tick >>= check >> pure Nothing)
      -- Coalesce queued duplicates; only one preferred hash is needed because
      -- every cycle also recovers missing work from the durable submission table.
      drained <- atomically $ drain queue 127
      let values=maybe [] (:[]) preferred<>drained
      atomically $ modifyTVar' metrics $ \m -> m{coalescedCount=coalescedCount m+length values-Set.size(Set.fromList values)}
      safe $ do
        now <- getPOSIXTime
        previous <- readIORef recoveryAt
        recovered <- if preferred/=Nothing || now-previous>=5 then do
          writeIORef recoveryAt now
          result <- timeout 2_000_000 $ withDb pool $ \conn -> recoverSubmittedHints conn router (naaPaymasterAddress native) preferred
          forM_ result $ \count -> atomically $ modifyTVar' metrics (\m -> m{recoveredCount=recoveredCount m+fromIntegral count})
          pure result
         else pure $ Just 0
        case recovered of
          Nothing -> pure ()
          Just _ -> do
            claimStarted <- getMonotonicTimeNSec
            claims <- withDb pool $ \conn -> claimCommitmentHints conn router
            busyNow <- if null claims then withDb pool (\conn -> hasActiveCommitmentHints conn router) else pure True
            writeIORef active busyNow
            when (not $ null claims) $ do
              boundary <- timeout 1_000_000 $ do
                chain <- ethChainId primary
                case chain of
                  Right 421614 -> ethLatestBlock primary
                  _ -> pure $ Left $ RpcJsonError "Commitment hint primary chain mismatch"
              case boundary of
                Just (Right headBlock) -> forM_ (pairs claims) $ \batch ->
                  mapConcurrently_ (\claim -> safe $ do
                    -- Four claims in two parallel pairs cost at most two
                    -- seconds, keeping healthy evidence inside its 3s window.
                    result <- timeout 1_000_000 $ observe mode native lifecycle metrics alto headBlock claimStarted claim
                    when (result == Nothing) $ withDb pool $ \conn -> retryCommitmentHint conn router claim False) batch
                _ -> forM_ claims $ \claim -> withDb pool $ \conn -> retryCommitmentHint conn router claim False
        counts <- readTVarIO metrics
        logInfoEvery 30 "commitment_hint_summary" "Optional commitment hint work totals" [field "queued" (acceptedCount counts),field "dropped" (droppedCount counts),field "coalesced" (coalescedCount counts),field "recovered" (recoveredCount counts),field "verified" (verifiedCount counts)]
  drain queue remaining
    | remaining <= (0 :: Int) = pure []
    | otherwise = tryReadTBQueue queue >>= maybe (pure []) (\hash -> (hash:) <$> drain queue (remaining-1))
  pairs [] = []
  pairs values = take 2 values : pairs (drop 2 values)
  observe mode native lifecycle metrics alto headBlock claimStarted claim = do
    let retry clear = withDb pool $ \conn -> retryCommitmentHint conn router claim clear
        complete = withDb pool $ \conn -> completeCommitmentHint conn router claim
    case expectedClientOrderId router (hintOperation claim) of
      Nothing -> complete
      Just clientId -> do
        transaction <- case hintTransaction claim of
          Just known -> pure $ Just known
          Nothing -> do
            result <- rpcCall alto "eth_getUserOperationReceipt" $ toJSON [hintHash claim]
            pure $ either (const Nothing) (receiptLocator (hintHash claim) (hintSender claim)) result
        case transaction of
          Nothing -> retry False -- unknown/ambiguous acknowledgement remains tracked
          Just tx -> do
            receipt <- ethGetTransactionReceipt primary tx
            case receipt of
              Right (Just found) | receiptSucceeded found -> do
                header <- ethGetBlockByNumber primary (receiptBlockNumber found)
                case header of
                  Right block | same (rpcBlockHash block) (receiptBlockHash found) ->
                    case receiptOrder lifecycle (hintSender claim) (hintHash claim) found of
                      Just (Just order) -> do
                        pending <- Perps.getPendingOrderViewAtBlock primary router order (receiptBlockNumber found)
                        case pending of
                          Right view | Just (orderId,commitTime) <- verifyCommitmentReceipt router lifecycle (naaPaymasterAddress native)
                            (hintSender claim) (hintHash claim) clientId found block view -> do
                              -- Recheck the exact receipt boundary after the pinned
                              -- state read. A reorg discards all provisional evidence.
                              again <- ethGetBlockByNumber primary (receiptBlockNumber found)
                              case again of
                                Right checked | same (rpcBlockHash checked) (receiptBlockHash found)
                                  && rpcBlockNumber headBlock >= receiptBlockNumber found -> withDb pool $ \conn -> withTransaction conn $ do
                                    let confirmed = rpcBlockNumber headBlock >= receiptBlockNumber found + fromIntegral (cfgKeeperConfirmations cfg)
                                        stage = if confirmed && rpcBlockTimestamp headBlock>commitTime then 3 else if confirmed then 2 else 1
                                    written <- recordProvisionalCommitment conn router claim tx (receiptBlockNumber found)
                                      (receiptBlockHash found) orderId commitTime (rpcBlockNumber headBlock) stage
                                    when (written && hintWakeStage claim < stage) $ do
                                      when (mode == WakeupsActive) $ notifyCommitmentWakeup conn router
                                      atomically $ modifyTVar' metrics (\m -> m{verifiedCount=verifiedCount m+1})
                                      observed <- getMonotonicTimeNSec
                                      let submissionAge = hintSubmissionAgeMs claim + fromIntegral ((observed-claimStarted) `div` 1_000_000)
                                      logInfo "commitment_hint_inclusion_verified" "Canonical commitment observed; ordinary keeper policy remains authoritative" [field "stage" stage,field "submission_to_observation_ms" submissionAge,field "mode" (T.pack $ show mode)]
                                _ -> retry True
                          _ -> retry True
                      Just Nothing -> complete -- deposit/withdrawal/non-order operation
                      _ -> retry True
                  _ -> retry True
              Right (Just found) -> do
                header <- ethGetBlockByNumber primary (receiptBlockNumber found)
                case header of
                  Right block | same (rpcBlockHash block) (receiptBlockHash found) -> complete
                  _ -> retry True
              _ -> retry True

-- Alto is only a locator, never authoritative inclusion evidence. Exact
-- success, sender and EntryPoint identity are rechecked in primary-chain logs.
receiptLocator :: Text -> Text -> Value -> Maybe Text
receiptLocator hash sender (Object value) = do
  String actual <- KM.lookup "userOpHash" value
  String account <- KM.lookup "sender" value
  String entry <- KM.lookup "entryPoint" value
  guard $ same actual hash && same sender account && same entry entryPoint
  Object receipt <- KM.lookup "receipt" value
  String transaction <- KM.lookup "transactionHash" receipt
  guard $ validCommitmentHash transaction
  pure $ T.toLower transaction
receiptLocator _ _ _ = Nothing

-- The original authorized operation binds clientOrderId even for smart-account
-- batches and protected opens. Targets/selectors/ABI lengths are release-bound.
expectedClientOrderId :: Text -> Value -> Maybe Text
expectedClientOrderId router (Object operation) = do
  String raw <- KM.lookup "callData" operation
  bytes <- unhex raw
  calls <- either (const Nothing) Just $ Policy.decodeSmartAccountCalls bytes
  let commits = [Policy.smartCallData call | call <- calls
        , (same (Policy.smartCallTarget call) router && BS.take 4 (Policy.smartCallData call) == BS.pack [0xe4,0x27,0x58,0x66] && BS.length (Policy.smartCallData call)==612)
          || (same (Policy.smartCallTarget call) Manifest.positionProtectionBookAddress && BS.take 4 (Policy.smartCallData call)==BS.pack [0x56,0xf7,0x50,0xf0] && BS.length (Policy.smartCallData call)==804)]
  case commits of
    [commit] -> let identifier=BS.take 32 $ BS.drop 4 commit
      in if BS.all (==0) identifier then Nothing else Just $ hex identifier
    _ -> Nothing
expectedClientOrderId _ _ = Nothing

verifyCommitmentReceipt :: Text -> Text -> Text -> Text -> Text -> Text -> TxReceipt -> RpcBlock -> Perps.PendingOrderView -> Maybe (Integer,Integer)
verifyCommitmentReceipt router lifecycle paymaster sender operation clientId receipt block pending = do
  guard $ receiptSucceeded receipt && validCommitmentHash operation && validCommitmentHash (receiptTxHash receipt)
    && rpcBlockNumber block==receiptBlockNumber receipt && same (rpcBlockHash block) (receiptBlockHash receipt)
  Just order <- receiptOrder lifecycle sender operation receipt
  let ordered=sortOn rpcLogIndex (receiptLogs receipt)
      events=[x | x<-ordered, same (rpcLogAddress x) entryPoint, take 1 (rpcLogTopics x)==[operationTopic]]
      matchesOperation x = case rpcLogTopics x of
        [_topic,hash,account,sponsor] -> hex hash==T.toLower operation
          && account==addressWord sender && sponsor==addressWord paymaster
        _ -> False
      matches=filter matchesOperation events
  current <- case matches of [x] -> Just x; _ -> Nothing
  let previous=maximum $ (-1):[rpcLogIndex x | x<-events,rpcLogIndex x<rpcLogIndex current]
      segment=[x | x<-ordered,rpcLogIndex x>previous,rpcLogIndex x<rpcLogIndex current]
      intents=[x | x<-segment,same (rpcLogAddress x) lifecycle,take 1 (rpcLogTopics x)==[Perps.intentRegisteredTopic]]
      commits=[x | x<-segment,same (rpcLogAddress x) router,take 1 (rpcLogTopics x)==[Perps.orderCommittedTopic]]
  intent <- case intents of [x] -> Just x; _ -> Nothing
  guard $ case Perps.decodePerpsOrderEvent intent of
    Just Perps.IntentRegistered{Perps.poeOrderId=i,Perps.poeClientOrderId=c,Perps.poeAccount=a} -> i==order && same c clientId && same a sender
    _ -> False
  commit <- case commits of [x] -> Just x; _ -> Nothing
  guard $ length (rpcLogTopics commit)==3 && all ((==32).BS.length) (rpcLogTopics commit) && BS.length (rpcLogData commit)==32
  side <- case Perps.decodePerpsOrderEvent commit of
    Just Perps.OrderCommitted{Perps.poeOrderId=i,Perps.poeAccount=a,Perps.poeSide=s} | i==order && same a sender -> Just s
    _ -> Nothing
  guard $ order>0 && order<2^(64::Int) && side `elem` [0,1] && Perps.povOrderId pending==order
    && Perps.povSide pending==side && Perps.povCommitTime pending==rpcBlockTimestamp block
    -- This release records ExecutionBlockNumberLib.current(): ArbSys L2 block
    -- number on Arbitrum, distinct from the ancestor l1BlockNumber RPC field.
    && rpcBlockTimestamp block>0 && Perps.povCommitBlock pending==receiptBlockNumber receipt
  pure (order,Perps.povCommitTime pending)

same :: Text -> Text -> Bool
same a b = T.toLower a == T.toLower b
entryPoint :: Text
entryPoint = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"
operationTopic :: BS.ByteString
operationTopic = keccak256 "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)"
unhex :: Text -> Maybe BS.ByteString
unhex value = T.stripPrefix "0x" (T.toLower value) >>= either (const Nothing) Just . B16.decode . TE.encodeUtf8
hex :: BS.ByteString -> Text
hex value = "0x" <> TE.decodeUtf8 (B16.encode value)
addressWord :: Text -> BS.ByteString
addressWord value = maybe BS.empty (BS.replicate 12 0 <>) $ unhex value
