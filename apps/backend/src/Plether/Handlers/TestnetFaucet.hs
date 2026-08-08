module Plether.Handlers.TestnetFaucet
  ( TestnetFaucetResponse (..)
  , FaucetClaimDisposition (..)
  , claimTestnetFaucet
  , classifyTestnetFaucetClaim
  , testnetFaucetAmount
  , testnetFaucetEnabled
  , faucetMintCall
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (AsyncException, SomeException, fromException, onException, throwIO, try)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Database.PostgreSQL.Simple (Connection)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb, withDbAdvisoryLock)
import Plether.Database.Schema
  ( TestnetFaucetClaimRow (..)
  , beginTestnetFaucetClaim
  , getTestnetFaucetClaim
  , markTestnetFaucetClaimFailed
  , markTestnetFaucetClaimReconciled
  , markTestnetFaucetClaimSubmitted
  , markTestnetFaucetClaimSuccess
  )
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256)
import Plether.Ethereum.Client (EthClient, RpcError (..), ethBlockNumber)
import qualified Plether.Ethereum.Contracts.ERC20 as ERC20
import Plether.Ethereum.Rpc
  ( TxReceipt (..)
  , ethEstimateGas
  , ethGasPrice
  , ethGetTransactionCount
  , ethGetTransactionReceipt
  , ethMaxPriorityFeePerGas
  , ethSendRawTransaction
  )
import Plether.Ethereum.Transaction
  ( SignedTransaction (..)
  , Tx1559 (..)
  , deriveAddress
  , signTransaction
  )
import Plether.Logging (field, logInfo)
import Plether.Types (ApiError, ApiResponse, mkResponse)
import qualified Plether.Types.Error as E
import System.Timeout (timeout)

testnetFaucetAmount :: Integer
testnetFaucetAmount = 100_000 * 1_000_000

-- Keep receipt polling bounded so the frontend's faucet-specific timeout can
-- cover this stage plus the handler's bounded database and signer-lock stages.
faucetReceiptTimeoutMicros :: Int
faucetReceiptTimeoutMicros = 120_000_000

data TestnetFaucetResponse = TestnetFaucetResponse
  { tfrAddress :: Text
  , tfrAmount :: Integer
  , tfrToken :: Text
  , tfrTxHash :: Maybe Text
  , tfrStatus :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TestnetFaucetResponse where
  toJSON TestnetFaucetResponse {..} =
    object
      [ "address" .= tfrAddress
      , "amount" .= show tfrAmount
      , "token" .= tfrToken
      , "txHash" .= tfrTxHash
      , "status" .= tfrStatus
      ]

data FaucetClaimDisposition
  = FaucetAlreadyClaimed
  | FaucetReconcileLegacy
  | FaucetResumeSubmitted
  | FaucetBeginOrWait
  | FaucetInvalidState
  deriving stock (Show, Eq)

data FaucetSubmissionAttempt
  = FaucetPreparationFailed Text
  | FaucetSubmissionStateChanged
  | FaucetTransactionSubmitted Text FaucetChainProgress

data FaucetChainProgress
  = FaucetReceiptFound TxReceipt
  | FaucetBroadcastFinished (Either RpcError Text)
  | FaucetReceiptLookupFailed RpcError
  | FaucetRawTransactionInvalid Text

classifyTestnetFaucetClaim :: Maybe TestnetFaucetClaimRow -> FaucetClaimDisposition
classifyTestnetFaucetClaim = \case
  Nothing -> FaucetBeginOrWait
  Just claim ->
    case tfcStatus claim of
      "success" -> FaucetAlreadyClaimed
      "pending" -> FaucetReconcileLegacy
      "submitted"
        | Just _ <- tfcTxHash claim
        , Just _ <- tfcRawTx claim ->
            FaucetResumeSubmitted
        | otherwise -> FaucetInvalidState
      "preparing" -> FaucetBeginOrWait
      "failed" -> FaucetBeginOrWait
      _ -> FaucetInvalidState

testnetFaucetEnabled :: Config -> Bool
testnetFaucetEnabled cfg = cfgPerpsChainId cfg == 421614

faucetMintCall :: Text -> ByteString
faucetMintCall recipient =
  encodeCall "mint(address,uint256)"
    [ encodeAddress recipient
    , encodeUint256 testnetFaucetAmount
    ]

claimTestnetFaucet
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
claimTestnetFaucet pool client cfg rawAddress
  | not (testnetFaucetEnabled cfg) =
      pure $ Left $ E.internalError "Testnet faucet is only available for Arbitrum Sepolia perps"
  | otherwise =
      case cfgFaucetPrivateKey cfg of
        Nothing ->
          pure $ Left $ E.internalError "FAUCET_PRIVATE_KEY is not configured"
        Just privateKey -> do
          let address = T.toLower rawAddress
              token = T.toLower $ cfgPerpsUsdc cfg
          existingResult <-
            runFaucetStage "claim_lookup" eitherOutcome $
              withFaucetDb pool $ \conn -> getTestnetFaucetClaim conn address token
          case existingResult of
            Left err -> pure $ Left err
            Right Nothing ->
              beginOrObserveClaim pool client cfg privateKey address token
            Right (Just claim) ->
              handlePersistedClaim pool client cfg privateKey address token claim

handlePersistedClaim
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> Text
  -> TestnetFaucetClaimRow
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
handlePersistedClaim pool client cfg privateKey address token claim =
  case classifyTestnetFaucetClaim $ Just claim of
    FaucetAlreadyClaimed ->
      alreadyClaimedResponse client (cfgPerpsChainId cfg) address token claim
    FaucetReconcileLegacy ->
      reconcileLegacyPending pool client cfg address token claim
    FaucetResumeSubmitted ->
      resumeSubmittedClaim pool client cfg address token claim
    FaucetBeginOrWait ->
      beginOrObserveClaim pool client cfg privateKey address token
    FaucetInvalidState ->
      pure $ Left $ E.internalError "Faucet claim has incomplete persisted transaction state"

beginOrObserveClaim
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> Text
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
beginOrObserveClaim pool client cfg privateKey address token = do
  startedResult <-
    runFaucetStage "claim_reservation" eitherOutcome $
      withFaucetDb pool $ \conn ->
        beginTestnetFaucetClaim conn address testnetFaucetAmount token
  case startedResult of
    Left err -> pure $ Left err
    Right True ->
      prepareAndSubmitClaim pool client cfg privateKey address token
    Right False -> do
      observedResult <-
        runFaucetStage "claim_observation" eitherOutcome $
          withFaucetDb pool $ \conn -> getTestnetFaucetClaim conn address token
      case observedResult of
        Left err -> pure $ Left err
        Right Nothing ->
          pure $ Left $ E.mkError E.RateLimited "Faucet claim is already in progress for this address"
        Right (Just observed) ->
          case classifyTestnetFaucetClaim $ Just observed of
            FaucetAlreadyClaimed ->
              alreadyClaimedResponse client (cfgPerpsChainId cfg) address token observed
            FaucetReconcileLegacy ->
              reconcileLegacyPending pool client cfg address token observed
            FaucetResumeSubmitted ->
              resumeSubmittedClaim pool client cfg address token observed
            FaucetInvalidState ->
              pure $ Left $ E.internalError "Faucet claim has incomplete persisted transaction state"
            FaucetBeginOrWait ->
              pure $ Left $ E.mkError E.RateLimited "Faucet claim is already in progress for this address"

prepareAndSubmitClaim
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> Text
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
prepareAndSubmitClaim pool client cfg privateKey address token = do
  attemptResult <-
    runFaucetStage "prepare_and_submit" submissionAttemptOutcome $
      withFaucetSignerLock pool $ \conn -> do
        prepareResult <-
          runFaucetStage "prepare_transaction" eitherOutcome $
            prepareFaucetMint cfg client privateKey token address
        case prepareResult of
          Left err -> do
            _ <-
              runFaucetStage "persist_preparation_failure" (const "completed") $
                markTestnetFaucetClaimFailed conn address token err
            pure $ FaucetPreparationFailed err
          Right signed -> do
            let txHash = T.toLower $ signedTransactionHash signed
                rawTx = encodeRawTransaction $ signedRawTransaction signed
            persisted <-
              runFaucetStage "persist_submitted_transaction" booleanOutcome $
                markTestnetFaucetClaimSubmitted conn address token txHash rawTx
            if not persisted
              then pure FaucetSubmissionStateChanged
              else do
                progress <- advanceSubmittedTransaction client txHash rawTx
                pure $ FaucetTransactionSubmitted txHash progress
  case attemptResult of
    Left err -> pure $ Left err
    Right (FaucetPreparationFailed err) -> pure $ Left $ E.rpcError err
    Right FaucetSubmissionStateChanged ->
      pure $ Left $ E.internalError "Faucet claim changed state before its transaction could be persisted"
    Right (FaucetTransactionSubmitted txHash progress) ->
      handleChainProgress pool client cfg address token txHash progress

resumeSubmittedClaim
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> TestnetFaucetClaimRow
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
resumeSubmittedClaim pool client cfg address token claim =
  case (tfcTxHash claim, tfcRawTx claim) of
    (Just txHash, Just rawTx) ->
      resumeSubmittedTransaction pool client cfg address token txHash rawTx
    _ ->
      pure $ Left $ E.internalError "Faucet claim has incomplete submitted transaction state"

resumeSubmittedTransaction
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> Text
  -> Text
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
resumeSubmittedTransaction pool client cfg address token txHash rawTx = do
  progressResult <-
    runFaucetStage "resume_submitted_transaction" eitherOutcome $
      withFaucetSignerLock pool $ \_ ->
        advanceSubmittedTransaction client txHash rawTx
  case progressResult of
    Left err -> pure $ Left err
    Right progress ->
      handleChainProgress pool client cfg address token txHash progress

advanceSubmittedTransaction :: EthClient -> Text -> Text -> IO FaucetChainProgress
advanceSubmittedTransaction client txHash rawTx = do
  receiptResult <-
    runFaucetStage "receipt_lookup" receiptLookupOutcome $
      ethGetTransactionReceipt client txHash
  case receiptResult of
    Left err -> pure $ FaucetReceiptLookupFailed err
    Right (Just receipt) -> pure $ FaucetReceiptFound receipt
    Right Nothing ->
      case decodeRawTransaction rawTx of
        Left err -> pure $ FaucetRawTransactionInvalid err
        Right rawBytes ->
          FaucetBroadcastFinished
            <$> runFaucetStage "broadcast_transaction" eitherOutcome
              (ethSendRawTransaction client rawBytes)

handleChainProgress
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> Text
  -> FaucetChainProgress
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
handleChainProgress pool client cfg address token txHash = \case
  FaucetReceiptFound receipt ->
    finalizeSubmittedClaim pool cfg address token txHash receipt
  FaucetReceiptLookupFailed err ->
    pure $ Left $ E.rpcError $ rpcErrorText err
  FaucetRawTransactionInvalid err ->
    pure $ Left $ E.internalError err
  FaucetBroadcastFinished sendResult ->
    case sendResult of
      Right returnedHash
        | T.toLower returnedHash /= T.toLower txHash ->
            pure $
              Left $
                E.internalError
                  "Faucet RPC returned a transaction hash that did not match the persisted signed transaction"
        | otherwise ->
            waitAndFinalizeClaim pool client cfg address token txHash
      Left err
        | isKnownTransactionError err ->
            waitAndFinalizeClaim pool client cfg address token txHash
        | otherwise ->
            pure $
              Left $
                E.rpcError $
                  "faucet transaction remains safely submitted for reconciliation: "
                    <> rpcErrorText err

waitAndFinalizeClaim
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> Text
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
waitAndFinalizeClaim pool client cfg address token txHash = do
  receiptResult <-
    runFaucetStage "receipt_poll" eitherOutcome $
      waitForReceipt client txHash 60
  case receiptResult of
    Left err -> pure $ Left $ E.rpcError err
    Right receipt ->
      finalizeSubmittedClaim pool cfg address token txHash receipt

finalizeSubmittedClaim
  :: DbPool
  -> Config
  -> Text
  -> Text
  -> Text
  -> TxReceipt
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
finalizeSubmittedClaim pool cfg address token txHash receipt
  | receiptSucceeded receipt = do
      successResult <-
        runFaucetStage "persist_success" eitherOutcome $
          withFaucetDb pool $ \conn ->
            markTestnetFaucetClaimSuccess conn address token txHash
      pure $ case successResult of
        Left err -> Left err
        Right _ ->
          Right $
            mkResponse
              (receiptBlockNumber receipt)
              (cfgPerpsChainId cfg)
              (faucetResponse address token (Just txHash) "minted")
  | otherwise = do
      let err = "faucet mint transaction reverted: " <> txHash
      failedResult <-
        runFaucetStage "persist_revert" eitherOutcome $
          withFaucetDb pool $ \conn ->
            markTestnetFaucetClaimFailed conn address token err
      pure $ case failedResult of
        Left dbErr -> Left dbErr
        Right _ -> Left $ E.rpcError err

reconcileLegacyPending
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> TestnetFaucetClaimRow
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
reconcileLegacyPending pool client cfg address token claim = do
  balanceResult <-
    runFaucetStage "legacy_balance_lookup" eitherOutcome $
      ERC20.balanceOf client token address
  case balanceResult of
    Left err -> pure $ Left $ E.rpcError $ rpcErrorText err
    Right balance
      | balance >= tfcAmount claim -> do
          reconciledResult <-
            withFaucetDb pool $ \conn ->
              markTestnetFaucetClaimReconciled conn address token
          case reconciledResult of
            Left err -> pure $ Left err
            Right _ ->
              alreadyFundedResponse client (cfgPerpsChainId cfg) address token
      | otherwise ->
          pure $
            Left $
              E.internalError
                "Legacy faucet claim has no recoverable transaction and requires manual reconciliation"

alreadyClaimedResponse
  :: EthClient
  -> Integer
  -> Text
  -> Text
  -> TestnetFaucetClaimRow
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
alreadyClaimedResponse client chainId address token claim =
  case tfcTxHash claim of
    Just txHash -> do
      blockResult <- ethBlockNumber client
      let blockNum = either (const 0) id blockResult
      pure $
        Right $
          mkResponse blockNum chainId (faucetResponse address token (Just txHash) "already_claimed")
    Nothing -> alreadyFundedResponse client chainId address token

alreadyFundedResponse
  :: EthClient
  -> Integer
  -> Text
  -> Text
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
alreadyFundedResponse client chainId address token = do
  blockResult <- ethBlockNumber client
  let blockNum = either (const 0) id blockResult
  pure $
    Right $
      mkResponse blockNum chainId (faucetResponse address token Nothing "already_funded")

faucetResponse :: Text -> Text -> Maybe Text -> Text -> TestnetFaucetResponse
faucetResponse address token txHash status =
  TestnetFaucetResponse
    { tfrAddress = address
    , tfrAmount = testnetFaucetAmount
    , tfrToken = token
    , tfrTxHash = txHash
    , tfrStatus = status
    }

prepareFaucetMint
  :: Config
  -> EthClient
  -> Text
  -> Text
  -> Text
  -> IO (Either Text SignedTransaction)
prepareFaucetMint cfg client privateKey token recipient =
  deriveAddress privateKey >>= \case
    Left err -> pure $ Left err
    Right fromAddr -> do
      let callData = faucetMintCall recipient
      nonceResult <- ethGetTransactionCount client fromAddr
      gasResult <- ethEstimateGas client fromAddr token 0 callData
      gasPriceResult <- ethGasPrice client
      priorityResult <- ethMaxPriorityFeePerGas client
      case (nonceResult, gasResult, gasPriceResult) of
        (Right nonce, Right estimatedGas, Right gasPrice) -> do
          let priorityBase = either (const gasPrice) id priorityResult
              maxFeeBase = max gasPrice priorityBase
              gasLimit = max 21_000 $ applyBuffer estimatedGas (cfgKeeperGasBufferBps cfg)
              maxPriorityFee = applyBuffer priorityBase (cfgKeeperFeeBufferBps cfg)
              maxFee = max maxPriorityFee $ applyBuffer maxFeeBase (cfgKeeperFeeBufferBps cfg)
              tx =
                Tx1559
                  { txChainId = cfgPerpsChainId cfg
                  , txNonce = nonce
                  , txMaxPriorityFeePerGas = maxPriorityFee
                  , txMaxFeePerGas = maxFee
                  , txGasLimit = gasLimit
                  , txTo = token
                  , txValue = 0
                  , txData = callData
                  }
          signTransaction privateKey tx
        _ ->
          pure $
            Left $
              T.intercalate
                "; "
                $ concat
                  [ either ((: []) . rpcErrorText) (const []) nonceResult
                  , either ((: []) . rpcErrorText) (const []) gasResult
                  , either ((: []) . rpcErrorText) (const []) gasPriceResult
                  ]

waitForReceipt :: EthClient -> Text -> Int -> IO (Either Text TxReceipt)
waitForReceipt client txHash attempts = do
  result <- timeout faucetReceiptTimeoutMicros $ pollForReceipt client txHash attempts
  pure $ maybe (Left $ "timed out waiting for receipt " <> txHash) id result

pollForReceipt :: EthClient -> Text -> Int -> IO (Either Text TxReceipt)
pollForReceipt _ txHash 0 = pure $ Left $ "timed out waiting for receipt " <> txHash
pollForReceipt client txHash attempts = do
  receiptResult <- ethGetTransactionReceipt client txHash
  case receiptResult of
    Left err -> pure $ Left $ rpcErrorText err
    Right (Just receipt) -> pure $ Right receipt
    Right Nothing -> do
      threadDelay 2_000_000
      pollForReceipt client txHash (attempts - 1)

runFaucetStage :: Text -> (result -> Text) -> IO result -> IO result
runFaucetStage stage outcomeFor action = do
  startedAt <- getMonotonicTimeNSec
  logInfo
    "testnet_faucet_stage_started"
    "Testnet faucet stage started"
    [field "stage" stage]
  result <-
    action
      `onException` logFaucetStageFinished startedAt stage "exception"
  logFaucetStageFinished startedAt stage $ outcomeFor result
  pure result

logFaucetStageFinished :: Word64 -> Text -> Text -> IO ()
logFaucetStageFinished startedAt stage outcome = do
  finishedAt <- getMonotonicTimeNSec
  logInfo
    "testnet_faucet_stage_finished"
    "Testnet faucet stage finished"
    [ field "stage" stage
    , field "outcome" outcome
    , field "duration_ms" $ (finishedAt - startedAt) `div` 1_000_000
    ]

eitherOutcome :: Either left right -> Text
eitherOutcome = either (const "failure") (const "success")

booleanOutcome :: Bool -> Text
booleanOutcome True = "success"
booleanOutcome False = "state_changed"

receiptLookupOutcome :: Either RpcError (Maybe TxReceipt) -> Text
receiptLookupOutcome = \case
  Left _ -> "failure"
  Right Nothing -> "not_found"
  Right (Just _) -> "found"

submissionAttemptOutcome
  :: Either ApiError FaucetSubmissionAttempt
  -> Text
submissionAttemptOutcome = \case
  Left _ -> "lock_or_database_failure"
  Right (FaucetPreparationFailed _) -> "preparation_failed"
  Right FaucetSubmissionStateChanged -> "state_changed"
  Right (FaucetTransactionSubmitted _ _) -> "submitted"

withFaucetDb :: DbPool -> (Connection -> IO a) -> IO (Either ApiError a)
withFaucetDb = withFaucetDbWithin 5_000_000

withFaucetSignerLock :: DbPool -> (Connection -> IO a) -> IO (Either ApiError a)
withFaucetSignerLock pool action =
  withFaucetDbWithin 30_000_000 pool $ \conn ->
    withDbAdvisoryLock conn faucetSignerLockId $ action conn

withFaucetDbWithin :: Int -> DbPool -> (Connection -> IO a) -> IO (Either ApiError a)
withFaucetDbWithin timeoutMicros pool action = do
  result <- try @SomeException $ timeout timeoutMicros $ withDb pool action
  case result of
    Left err ->
      case fromException err :: Maybe AsyncException of
        Just _ -> throwIO err
        Nothing -> pure $ Left $ E.networkError "Faucet claim database operation failed"
    Right Nothing -> pure $ Left $ E.networkError "Faucet claim database operation timed out"
    Right (Just value) -> pure $ Right value

faucetSignerLockId :: Integer
faucetSignerLockId = 4_216_140_100_000_001

encodeRawTransaction :: ByteString -> Text
encodeRawTransaction value = "0x" <> TE.decodeUtf8 (B16.encode value)

decodeRawTransaction :: Text -> Either Text ByteString
decodeRawTransaction value =
  case B16.decode $ TE.encodeUtf8 $ T.toLower $ strip0x value of
    Left err -> Left $ "Persisted faucet transaction is invalid hex: " <> T.pack err
    Right bytes
      | BS.null bytes -> Left "Persisted faucet transaction is empty"
      | otherwise -> Right bytes

isKnownTransactionError :: RpcError -> Bool
isKnownTransactionError err =
  let message = T.toLower $ rpcErrorText err
   in any
        (`T.isInfixOf` message)
        [ "already known"
        , "known transaction"
        , "already imported"
        , "nonce too low"
        ]

strip0x :: Text -> Text
strip0x value =
  case T.stripPrefix "0x" $ T.strip value of
    Just stripped -> stripped
    Nothing -> T.strip value

applyBuffer :: Integer -> Integer -> Integer
applyBuffer value bufferBps = (value * (10_000 + bufferBps)) `div` 10_000

rpcErrorText :: RpcError -> Text
rpcErrorText = \case
  RpcHttpError msg -> msg
  RpcJsonError msg -> msg
  RpcNodeError _ msg _ -> msg
