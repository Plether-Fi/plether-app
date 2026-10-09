-- | Read-only bridge observer. Across performs the destination action; this
-- process never signs, estimates gas, deploys contracts or broadcasts a call.
module Plether.Perps.Funding.Worker (reconcileFundingOnce, runFundingWorker) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try, fromException, SomeAsyncException, throwIO, onException)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.Database
import Plether.Ethereum.Abi (encodeUint256)
import Plether.Ethereum.Client
import Plether.Ethereum.Rpc
import Plether.Logging (logWarn, field)
import Plether.Perps.Funding.Chain
import Plether.Perps.Funding.Source
import Plether.Perps.Funding.Store
import Plether.Perps.Funding.Types

runFundingWorker :: EthClient -> EthClient -> DbPool -> FundingDeployment -> IO ()
runFundingWorker destination source pool deployment = loop
  where
    loop = do
      outcome <- try @SomeException $ reconcileFundingOnce destination source pool deployment
      case outcome of
        Left err -> case fromException err :: Maybe SomeAsyncException of
          Just _ -> throwIO err
          Nothing -> logWarn "funding_observer_retry" "Funding observation failed; durable intent retained" []
        Right (Left reason) -> logWarn "funding_observer_retry" "Funding observation paused" [field "reason" reason]
        Right (Right ()) -> pure ()
      threadDelay 5_000_000
      loop

reconcileFundingOnce :: EthClient -> EthClient -> DbPool -> FundingDeployment -> IO (Either Text ())
reconcileFundingOnce destination source pool deployment = withDb pool $ \conn -> withFundingStateLock conn $ trackReadiness conn $ runExceptT $ do
  ExceptT $ verifyFundingDeployment destination deployment
  sourceChain <- requireRpc $ ethChainId source
  unless (sourceChain == 1) $ throwE "SOURCE_CHAIN_MISMATCH"
  -- Every pin is immutable per intent, including provider implementations and
  -- confirmation policy. Reusing a release label cannot change old evidence.
  intents <- liftIO $ listPendingIntentsForDeployment conn (toJSON deployment) 16
  mapM_ (observeSafely conn) intents
  liftIO $ setWorkerReadiness conn (deploymentReadinessKey deployment) (fdChainId deployment) True
  where
    trackReadiness conn action = (do
      result <- action
      case result of
        Left _ -> setWorkerReadiness conn (deploymentReadinessKey deployment) (fdChainId deployment) False
        Right _ -> pure ()
      pure result) `onException` setWorkerReadiness conn (deploymentReadinessKey deployment) (fdChainId deployment) False
    -- An invalid or dropped source transaction affects its own intent. Only
    -- shared RPC/release health failures disable new quotes for everyone.
    observeSafely conn original = do
      outcome <- liftIO $ runExceptT $ observe conn original
      case outcome of
        Right () -> pure ()
        Left reason | reason `elem` ["SOURCE_RPC_UNAVAILABLE","DESTINATION_RPC_UNAVAILABLE"
          ,"SOURCE_CHAIN_MISMATCH","FUNDING_DEPLOYMENT_MISMATCH","ACROSS_IMPLEMENTATION_MISMATCH"
          ,"FUNDING_DEPLOYMENT_REORGED"] -> throwE reason
        Left reason -> do
          identifier <- required "intentId" original
          current <- liftIO (getIntent conn identifier) >>= maybe (throwE "INTENT_NOT_FOUND") pure
          now <- liftIO $ floor <$> getPOSIXTime
          liftIO $ updateIntent conn identifier $ setFields
            [("status",String "retryable"),("lastError",String reason),("lastCheckedAt",toJSON (now :: Integer))
            ,("sourceTerminal",Bool False)] $ clearOutcome current
    observe conn original = case fieldText "sourceTxHash" original of
      Nothing -> pure ()
      Just sourceHash -> do
        identifier <- required "intentId" original
        now <- liftIO $ floor <$> getPOSIXTime
        let save value = liftIO $ updateIntent conn identifier $ setFields [("lastCheckedAt",toJSON (now :: Integer))] value
        -- Only positive canonical block evidence releases an orphaned source
        -- claim. A timeout or missing receipt cannot transfer relay ownership.
        old <- liftIO $ getSourceRelay conn identifier
        case old of
          Nothing -> pure ()
          Just claim -> do
            oldBlock <- number "sourceBlockNumber" claim
            oldHash <- required "sourceBlockHash" claim
            canonical <- requireRpc $ ethGetBlockByNumber source oldBlock
            when (rpcBlockHash canonical /= oldHash) $ do
              relayHash <- required "relayHash" claim
              _ <- liftIO $ invalidateSourceRelay conn identifier relayHash
              pure ()
        current <- liftIO (getIntent conn identifier) >>= maybe (throwE "INTENT_NOT_FOUND") pure
        ExceptT $ validateSourceTransaction source current sourceHash
        mined <- requireRpc $ ethGetTransactionReceipt source sourceHash
        case mined of
          Nothing -> save $ setFields [("sourceStatus",String "pending"),("sourceTerminal",Bool False)] $ clearOutcome current
          Just receipt -> do
            block <- requireRpc $ ethGetBlockByNumber source $ receiptBlockNumber receipt
            headBlock <- requireRpc $ ethBlockNumber source
            let confirmations = max 0 $ headBlock - receiptBlockNumber receipt + 1
                sourceStatus = if confirmations < 2 then "pending" else if receiptSucceeded receipt then "confirmed" else "reverted"
                evidence = [("sourceStatus",String sourceStatus),("sourceTerminal",Bool $ sourceStatus == "reverted")
                  ,("sourceBlockNumber",String $ T.pack $ show $ receiptBlockNumber receipt)
                  ,("sourceBlockHash",String $ receiptBlockHash receipt),("sourceConfirmations",toJSON confirmations)]
            unless (rpcBlockHash block == receiptBlockHash receipt) $ throwE "SOURCE_RECEIPT_REORGED"
            relay <- either throwE pure $ sourceDepositProof deployment current headBlock block receipt
            case relay of
              Nothing -> save $ setFields evidence $ clearOutcome current
              Just proof -> do
                claim <- liftIO $ claimSourceRelay conn identifier proof
                either throwE pure claim
                bound <- liftIO (getIntent conn identifier) >>= maybe (throwE "INTENT_NOT_FOUND") pure
                reconciled <- observeDestination bound proof
                save $ setFields evidence reconciled
    observeDestination original relay = do
      headBlock <- requireRpc $ ethBlockNumber destination
      let safeBlock = headBlock - fdConfirmations deployment + 1
          initial = fromMaybe (fdStartBlock deployment) $ fieldInteger "observationFromBlock" original
          oldNext = fromMaybe initial $ fieldInteger "scanFromBlock" original
      previous <- case fieldText "fillTxHash" original of
        Nothing -> pure Nothing
        Just hash -> verifyOutcome False original relay headBlock hash
      case previous of
        Just proof -> pure $ mergeOutcome original proof
        Nothing -> do
          cursorValid <- case fieldText "scanBoundaryHash" original of
            Just hash | oldNext > initial -> (== hash) . rpcBlockHash <$> requireRpc (ethGetBlockByNumber destination $ oldNext-1)
            _ -> pure True
          let rewind = not cursorValid || fieldText "fillTxHash" original /= Nothing
              fromBlock = if rewind then initial else oldNext
              toBlock = min safeBlock (fromBlock+4999)
              clean = clearOutcome original
          if fromBlock > toBlock then pure clean else do
            before <- requireRpc $ ethGetBlockByNumber destination toBlock
            entries <- requireRpc $ ethGetLogs destination (fdDestinationSpokePool deployment) [filledRelayTopic] fromBlock toBlock
            depositId <- number "depositId" relay
            let hashes = nub [rpcLogTxHash entry | entry <- entries
                  ,take 3 (rpcLogTopics entry) == [filledRelayTopic,encodeUint256 1,encodeUint256 depositId]]
            candidates <- mapM (verifyOutcome True original relay headBlock) hashes
            after <- requireRpc $ ethGetBlockByNumber destination toBlock
            unless (rpcBlockHash before == rpcBlockHash after) $ throwE "DEPOSIT_REORGED"
            let updated = setFields [("scanFromBlock",toJSON $ toBlock+1),("scanBoundaryHash",String $ rpcBlockHash after)] clean
            case [proof | Just proof <- candidates] of
              [] -> pure updated
              [proof] -> pure $ mergeOutcome updated proof
              _ -> throwE "DUPLICATE_RELAY_FILL"
    verifyOutcome discovered original relay headBlock hash = do
      mined <- requireRpc $ ethGetTransactionReceipt destination hash
      case mined of
        Nothing | discovered -> throwE "DESTINATION_RECEIPT_NOT_VISIBLE"
                | otherwise -> pure Nothing
        Just receipt -> do
          block <- requireRpc $ ethGetBlockByNumber destination $ receiptBlockNumber receipt
          if rpcBlockHash block /= receiptBlockHash receipt
            then if discovered then throwE "DESTINATION_RECEIPT_REORGED" else pure Nothing
            else do
              ExceptT $ verifyFundingDeploymentAt destination deployment block
              either throwE pure $ depositProof deployment original relay headBlock block receipt
    required key value = maybe (throwE "INVALID_STORED_INTENT") pure $ fieldText key value
    number key value = required key value >>= \raw -> case reads (T.unpack raw) of
      [(n,"")] | n >= 0 -> pure n
      _ -> throwE "INVALID_STORED_INTENT"

clearOutcome :: Value -> Value
clearOutcome original = setFields (rewind <> [("status",String "bridging"),("creditedAmount",String "0"),("lastError",Null)] <>
  [(key,Null) | key <- ["fillTxHash","fillBlockNumber","fillBlockHash","fillLogIndex"
    ,"depositTxHash","depositBlockNumber","depositBlockHash","depositLogIndices"
    ,"fallbackTxHash","fallbackBlockNumber","fallbackBlockHash","fallbackAmount","fallbackLogIndices"]]) original
  where
    -- Retraction must permit rediscovery after a temporary missing receipt.
    -- Normal unfilled polling retains its bounded scan progress.
    rewind = if fieldText "fillTxHash" original /= Nothing
      then [("scanFromBlock",Null),("scanBoundaryHash",Null)] else []
mergeOutcome :: Value -> Value -> Value
mergeOutcome original (Object proof) = case clearOutcome original of
  Object fields -> Object $ KM.union proof fields
  value -> value
mergeOutcome original _ = clearOutcome original
