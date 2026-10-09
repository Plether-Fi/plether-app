-- | A single durable signer lane. Persist raw bytes before broadcast; uncertain
-- broadcasts only resend those bytes. Public flush calls remain recoverable even
-- when this worker is unavailable.
module Plether.Perps.Funding.Worker (reconcileFundingOnce, runFundingWorker, shouldFlushBalance, verifyFundingSigner) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try, fromException, SomeAsyncException, throwIO, onException)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.AA.Kms (PaymasterSigner (..))
import Plether.Database
import Plether.Ethereum.Abi
import Plether.Ethereum.Client
import Plether.Ethereum.Contracts.ERC20 (balanceOf)
import Plether.Ethereum.Rpc
import Plether.Ethereum.Transaction
import Plether.Logging (logWarn, field)
import Plether.Perps.Funding.Chain
import Plether.Perps.Funding.Store
import Plether.Perps.Funding.Types

runFundingWorker :: EthClient -> DbPool -> FundingDeployment -> Maybe PaymasterSigner -> Integer -> IO ()
runFundingWorker client pool deployment key maxCost = loop
  where
    loop = do
      outcome <- try @SomeException $ reconcileFundingOnce client pool deployment key maxCost
      case outcome of
        Left err -> case fromException err :: Maybe SomeAsyncException of
          Just _ -> throwIO err
          Nothing -> logWarn "funding_worker_retry" "Funding reconciliation failed; durable work retained" []
        Right (Left reason) -> logWarn "funding_worker_retry" "Funding reconciliation paused" [field "reason" reason]
        Right (Right ()) -> pure ()
      threadDelay 5_000_000
      loop

reconcileFundingOnce :: EthClient -> DbPool -> FundingDeployment -> Maybe PaymasterSigner -> Integer -> IO (Either Text ())
reconcileFundingOnce client pool deployment configuredSigner maxCost = withDb pool $ \conn -> withFundingWorkerLock conn $ trackReadiness conn $ runExceptT $ do
  ExceptT $ verifyFundingDeployment client deployment
  executorReady <- case configuredSigner of
    Nothing -> pure False
    Just signer -> do
      ExceptT $ verifyFundingSigner signer
      (>= maxCost) <$> requireRpc (ethGetBalance client $ psAddress signer)
  active <- liftIO $ getActiveTransaction conn
  intents <- case active of
    Just pending -> pure [pending]
    Nothing -> liftIO $ listPendingIntentsForDeployment conn
      (object ["destinationChainId" .= fdChainId deployment,"releaseId" .= fdReleaseId deployment
        ,"clearinghouse" .= fdClearinghouse deployment,"token" .= fdToken deployment,"receiverFactory" .= fdFactory deployment]) 1
  mapM_ (\original -> do
    identifier <- required "intentId" original
    let save = liftIO . updateIntent conn identifier
    unless (bindingsMatch original) $ throwE "STORED_RELEASE_BINDING_MISMATCH"
    updated <- reconcileProofs original
    save updated
    case fieldText "signedRawTransaction" updated of
      Just raw -> do
        hash <- required "transactionHash" updated
        bytes <- either throwE pure $ decodeHex raw
        unless (rawTransactionHash bytes == hash) $ throwE "PERSISTED_TRANSACTION_HASH_MISMATCH"
        (decoded,recoveredSender) <- ExceptT $ decodeSignedTransaction bytes
        storedSender <- required "sender" updated
        storedNonce <- required "nonce" updated >>= maybe (throwE "INVALID_STORED_NONCE") pure . readNonnegative
        kind <- required "transactionKind" updated
        receiver <- required "receiver" updated
        beneficiary <- required "beneficiary" updated
        salt <- required "intentSalt" updated >>= either throwE pure . decodeHex
        let expectedTarget = if kind == "flush" then receiver else fdFactory deployment
            expectedCalldata = if kind == "flush" then encodeCall "flush()" [] else encodeCall "createReceiver(address,bytes32)" [encodeAddress beneficiary,encodeBytes32 salt]
        unless (kind `elem` ["flush","create-receiver"] && txChainId decoded == fdChainId deployment
          && txTo decoded == expectedTarget && txData decoded == expectedCalldata && txValue decoded == 0
          && txNonce decoded == storedNonce && recoveredSender == storedSender) $ throwE "PERSISTED_TRANSACTION_BINDING_MISMATCH"
        receipt <- requireRpc $ ethGetTransactionReceipt client hash
        case receipt of
          Nothing -> when (maybe False ((== recoveredSender) . psAddress) configuredSigner) $ do
            unless (txGasLimit decoded * txMaxFeePerGas decoded <= maxCost) $ throwE "DESTINATION_GAS_BUDGET_EXCEEDED"
            -- Keep pending even on nonce-too-low: only the exact receipt proves
            -- what happened. Operator recovery is preferable to a duplicate tx.
            broadcast hash bytes
          Just mined -> do
            block <- requireRpc $ ethGetBlockByNumber client $ receiptBlockNumber mined
            headBlock <- requireRpc $ ethBlockNumber client
            unless (receiptTxHash mined == hash && receiptBlockHash mined == rpcBlockHash block) $
              throwE "PENDING_TRANSACTION_REORGED"
            when (headBlock - receiptBlockNumber mined + 1 >= fdConfirmations deployment) $ do
              let clean = setFields [(k,Null) | k <- ["signedRawTransaction","transactionHash","transactionKind","sender","nonce"]] updated
              save $ if receiptSucceeded mined then clean else setFields
                [("status",String "retryable"),("lastError",String "DESTINATION_TRANSACTION_REVERTED")] clean
      Nothing -> do
        receiver <- required "receiver" updated
        minimumAmount <- amount "minimumAmount" updated
        balance <- requireRpc $ balanceOf client (fdToken deployment) receiver
        let credited = fromMaybe 0 $ fieldText "creditedAmount" updated >>= readNonnegative
        when (shouldFlushBalance minimumAmount credited balance) $ do
          exists <- ExceptT $ verifyReceiver client deployment updated
          case configuredSigner of
            Nothing -> save $ setFields [("status",String "received"),("lastError",String "DESTINATION_EXECUTOR_UNAVAILABLE")] updated
            Just signer -> do
              unless executorReady $ throwE "DESTINATION_EXECUTOR_UNFUNDED"
              beneficiary <- required "beneficiary" updated
              salt <- required "intentSalt" updated >>= either throwE pure . decodeHex
              let target = if exists then receiver else fdFactory deployment
                  calldata = if exists then encodeCall "flush()" [] else encodeCall "createReceiver(address,bytes32)" [encodeAddress beneficiary,encodeBytes32 salt]
              let sender = psAddress signer
              nonce <- requireRpc $ ethGetTransactionCount client sender
              gas <- requireRpc $ ethEstimateGas client sender target 0 calldata
              fee <- requireRpc $ ethGasPrice client
              priority <- requireRpc $ ethMaxPriorityFeePerGas client
              let gasLimit = max 21_000 (gas * 12 `div` 10)
                  maxFee = max priority (fee * 2)
              unless (gasLimit <= 2_000_000 && gasLimit * maxFee <= maxCost) $ throwE "DESTINATION_GAS_BUDGET_EXCEEDED"
              signed <- ExceptT $ signTransactionWithDigestSigner sender (psSignDigest signer) $ Tx1559 (fdChainId deployment) nonce priority maxFee gasLimit target 0 calldata
              let pending = setFields
                    [("status",String "depositing"),("lastError",Null)
                    ,("signedRawTransaction",String $ encodeHex $ signedRawTransaction signed)
                    ,("transactionHash",String $ signedTransactionHash signed)
                    ,("transactionKind",String $ if exists then "flush" else "create-receiver")
                    ,("sender",String sender),("nonce",String $ T.pack $ show nonce)] updated
              save pending
              broadcast (signedTransactionHash signed) (signedRawTransaction signed)
    ) intents
  -- Publish readiness only after the full reconciliation succeeds. A failed
  -- durable nonce must not briefly advertise a usable executor on each retry.
  liftIO $ setWorkerReadiness conn (deploymentReadinessKey deployment) (fdChainId deployment) executorReady
  where
    broadcast expectedHash bytes = do
      response <- liftIO $ ethSendRawTransaction client bytes
      case response of
        Left (RpcNodeError _ _ _) -> throwE "DESTINATION_BROADCAST_REJECTED"
        Left _ -> throwE "DESTINATION_BROADCAST_UNCERTAIN"
        Right actualHash -> unless (actualHash == expectedHash) $ throwE "DESTINATION_BROADCAST_HASH_MISMATCH"
    trackReadiness conn action = (do
      result <- action
      case result of
        Left _ -> setWorkerReadiness conn (deploymentReadinessKey deployment) (fdChainId deployment) False
        Right _ -> pure ()
      pure result) `onException` setWorkerReadiness conn (deploymentReadinessKey deployment) (fdChainId deployment) False
    required key value = maybe (throwE "INVALID_STORED_INTENT") pure $ fieldText key value
    amount key value = required key value >>= either throwE pure . validateAmount
    bindingsMatch value = fieldInteger "destinationChainId" value == Just (fdChainId deployment)
      && all (\(key,expected) -> fieldText key value == Just expected)
        [("releaseId",fdReleaseId deployment),("token",fdToken deployment),("clearinghouse",fdClearinghouse deployment),("receiverFactory",fdFactory deployment)]
    reconcileProofs original = do
      headBlock <- requireRpc $ ethBlockNumber client
      let safeBlock = headBlock - fdConfirmations deployment + 1
          initial = fromMaybe (fdStartBlock deployment) $ fieldInteger "observationFromBlock" original
          oldNext = fromMaybe initial $ fieldInteger "scanFromBlock" original
      canonicalCursor <- case fieldText "scanBoundaryHash" original of
        Just hash | oldNext > initial -> do
          block <- requireRpc $ ethGetBlockByNumber client (oldNext - 1)
          pure $ rpcBlockHash block == hash
        _ -> pure True
      previous <- mapM (recheck headBlock) $ fromMaybe [] $ parseMaybe (withObject "intent" (.: "creditEvents")) original
      let validPrevious = sequence previous
          rewind = not canonicalCursor || validPrevious == Nothing
          fromBlock = if rewind then initial else oldNext
          proofs = if rewind then [] else fromMaybe [] validPrevious
          toBlock = min safeBlock (fromBlock + 4999)
      (newProofs,boundary) <- if fromBlock > toBlock then pure ([],Nothing) else do
        receiver <- required "receiver" original
        beneficiary <- required "beneficiary" original
        boundaryBefore <- requireRpc $ ethGetBlockByNumber client toBlock
        logs <- requireRpc $ ethGetLogs client (fdClearinghouse deployment) [depositForTopic] fromBlock toBlock
        let transactions = nub [rpcLogTxHash entry | entry <- logs,
              rpcLogTopics entry == [depositForTopic,encodeAddress receiver,encodeAddress beneficiary]]
        verified <- mapM (\hash -> do
          mined <- requireRpc $ ethGetTransactionReceipt client hash
          case mined of
            Nothing -> throwE "DESTINATION_RECEIPT_UNAVAILABLE"
            Just receipt -> do
              canonical <- requireRpc $ ethGetBlockByNumber client $ receiptBlockNumber receipt
              either throwE pure $ depositProof deployment original headBlock canonical receipt) transactions
        boundaryAfter <- requireRpc $ ethGetBlockByNumber client toBlock
        unless (rpcBlockHash boundaryBefore == rpcBlockHash boundaryAfter) $ throwE "DEPOSIT_REORGED"
        pure ([proof | Just proof <- verified],Just $ rpcBlockHash boundaryAfter)
      let events = nub $ proofs <> newProofs
          credited = sum [n | proof <- events, Just raw <- [fieldText "creditedAmount" proof], Right n <- [validateAmount raw]]
      minimumAmount <- amount "minimumAmount" original
      let complete = credited >= minimumAmount
          proofFields = case reverse events of
            Object latest : _ | complete -> [("depositTxHash",fromMaybe Null $ KM.lookup "depositTxHash" latest)
              ,("depositBlockNumber",fromMaybe Null $ KM.lookup "depositBlockNumber" latest)
              ,("depositBlockHash",fromMaybe Null $ KM.lookup "depositBlockHash" latest)]
            _ -> [("depositTxHash",Null),("depositBlockNumber",Null),("depositBlockHash",Null)]
          scanFields = case boundary of
            Nothing -> []
            Just hash -> [("scanFromBlock",toJSON $ toBlock + 1),("scanBoundaryHash",String hash)]
          oldStatus = fromMaybe "awaiting-source" $ fieldText "status" original
          status = if complete then "confirmed" else if oldStatus == "confirmed" then "bridging" else oldStatus
      pure $ setFields ([("creditEvents",toJSON events),("creditedAmount",String $ T.pack $ show credited),("status",String status)] <> proofFields <> scanFields) original
      where
        recheck headBlock proof = case fieldText "depositTxHash" proof of
          Nothing -> pure Nothing
          Just hash -> do
            receipt <- requireRpc $ ethGetTransactionReceipt client hash
            case receipt of
              Nothing -> pure Nothing
              Just mined -> do
                canonical <- requireRpc $ ethGetBlockByNumber client $ receiptBlockNumber mined
                pure $ either (const Nothing) id $ depositProof deployment original headBlock canonical mined

-- Already credited partial fills count toward the route minimum. Once a route
-- completed, late arrivals of at least one USDC are swept automatically. Smaller
-- dust remains available for permissionless flush or beneficiary recovery.
shouldFlushBalance :: Integer -> Integer -> Integer -> Bool
shouldFlushBalance minimumAmount credited balance = balance > 0 && credited + balance >= minimumAmount
  && (credited < minimumAmount || balance >= 1_000_000)

readNonnegative :: Text -> Maybe Integer
readNonnegative raw = case reads (T.unpack raw) of
  [(value, "")] | value >= 0 -> Just value
  _ -> Nothing

-- Sign an application-domain digest, never a transaction or wallet authorization.
-- GetPublicKey permission alone does not demonstrate executor readiness.
verifyFundingSigner :: PaymasterSigner -> IO (Either Text ())
verifyFundingSigner signer = do
  let digest = keccak256 "Plether bridge funding executor readiness v1"
  signature <- psSignDigest signer digest
  case signature of
    Left _ -> pure $ Left "DESTINATION_SIGNER_UNAVAILABLE"
    Right bytes | BS.length bytes == 65 -> do
      let encoded = fromIntegral $ BS.last bytes :: Int
          parity = if encoded >= 27 then encoded - 27 else encoded
      if parity < 0 || parity > 1 then pure $ Left "DESTINATION_SIGNER_UNAVAILABLE" else do
        recovered <- recoverSignerAddress digest (BS.take 64 bytes) parity
        pure $ case recovered of
          Right address | address == psAddress signer -> Right ()
          _ -> Left "DESTINATION_SIGNER_UNAVAILABLE"
    _ -> pure $ Left "DESTINATION_SIGNER_UNAVAILABLE"
