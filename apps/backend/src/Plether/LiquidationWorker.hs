module Plether.LiquidationWorker
  ( LiquidationWorkerMode (..)
  , LiquidationWorkerConfig (..)
  , loadLiquidationWorkerConfig
  , runLiquidationWorker
  , processCandidates
  , decodeCachedPythPayload
  , validateLiquidationExecutionPayload
  , loadFreshLiquidationExecutionPayload
  , LiquidationPayloadCircuitDecision (..)
  , liquidationPayloadCircuitDecision
  , LiquidationSignerCircuitDecision (..)
  , liquidationSignerCircuitDecision
  , LiquidationPendingSignerAction (..)
  , liquidationPendingSignerAction
  , isInsufficientFundsRpcError
  , liquidationPayloadFingerprint
  , LiquidationBatchProgress (..)
  , validateLiquidationBatchReceipt
  , payloadGlobalSimulationRevertSelector
  , isLiquidationReceiptFor
  , isExpectedLiquidationSimulationRevert
  , liquidationIndexRange
  , sameNonceReplacementFees
  , checkLiveSignerBalance
  , transactionMaximumCost
  , canAffordTransaction
  , liquidationTransactionGasLimit
  , FreshLiquidationRiskInputs (..)
  , LiquidationRiskInputError (..)
  , LiquidationFuturePublishTime (..)
  , liquidationSnapshotCalls
  , decodeLiquidationSnapshotResults
  , selectLiquidationSimulationCandidates
  , liquidationFuturePublishRetryDelaySeconds
  , retryLiquidationRiskInputs
  , processLiquidationBatches
  , runLiquidationSweep
  , BatchOutcome (..)
  , LiquidationExecutionPolicy (..)
  , decodeLiquidationExecutionPolicy
  , liquidationPayloadMaximumAge
  , sizeLiquidationBatchGas
  , waitForLiquidationConfirmation
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, link)
import qualified Plether.LiquidationWorker.Monitoring as Monitoring
import Control.Exception (bracket)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson
  ( FromJSON
  , Result (..)
  , Value
  , fromJSON
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Char (isHexDigit)
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( PerpsLiquidationCandidateRow (..)
  , PerpsLiquidationRejectedPayloadRow (..)
  , PerpsLiquidationSignerRetryRow (..)
  , PythUpdatePayloadRow (..)
  , clearPerpsLiquidationCandidatePending
  , clearPerpsLiquidationRejectedPayload
  , clearPerpsLiquidationSignerRetry
  , deletePerpsLiquidationCandidate
  , getLatestPythUpdatePayload
  , getPerpsLiquidationCandidates
  , getPerpsLiquidationLastIndexedBlock
  , getPerpsLiquidationRejectedPayload
  , getPerpsLiquidationSignerRetry
  , getPendingPerpsLiquidationCandidates
  , markPerpsLiquidationCandidateChecked
  , recordPerpsLiquidationCandidateError
  , recordPerpsLiquidationCandidateRetryableError
  , recordPerpsLiquidationCandidateBroadcastAttempt
  , recordPerpsLiquidationCandidatePending
  , recordPerpsLiquidationRejectedPayload
  , recordPerpsLiquidationSignerRetry
  , setPerpsLiquidationLastIndexedBlock
  , tryPerpsLiquidationLock
  , unlockPerpsLiquidationLock
  , upsertPerpsLiquidationCandidate
  )
import Plether.Ethereum.Abi (decodeUint256, encodeCall, encodeUint256, keccak256)
import Plether.Ethereum.Client (EthClient, RpcError (..), CallParams (..), ethBlockNumber, ethCallAtBlock, ethCallWithTransactionGasAtBlock)
import Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( AccountLedgerSnapshot (..)
  , decodeAccountLedgerSnapshot
  , getAccountLedgerSnapshotCall
  )
import qualified Plether.Ethereum.Contracts.Perps as Perps
import qualified Plether.Ethereum.Multicall as Multicall
import Plether.Ethereum.Rpc
  ( RpcLog (..)
  , TxReceipt (..)
  , ethEstimateGasAtBlock
  , ethGasPrice
  , ethGetLogs
  , ethGetBalance
  , ethGetTransactionCount
  , ethGetTransactionCountAtBlock
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
import Plether.Logging
  ( LogField
  , field
  , logError
  , logErrorEvery
  , logInfo
  , logInfoEvery
  , logWarn
  , logWarnEvery
  )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data LiquidationWorkerMode
  = LiquidationWorkerLoop
  | LiquidationWorkerOnce
  deriving stock (Show, Eq)

data LiquidationPayloadCircuitDecision
  = ProcessLiquidationPayload
  | ClearRejectedLiquidationPayload
  | SuppressRejectedLiquidationPayload
  deriving stock (Show, Eq)

data LiquidationSignerCircuitDecision
  = SignerTransactionReady
  | RecheckSignerTransaction
  | SuppressSignerTransaction
  deriving stock (Show, Eq)

data LiquidationPendingSignerAction
  = ReplacePendingSignerTransaction
  | RebroadcastPendingSignerTransaction
  | WaitForPendingSignerTransaction
  deriving stock (Show, Eq)

data LiquidationBatchProgress = LiquidationBatchProgress
  { lbpItems :: [Perps.LiquidationBatchItem]
  , lbpNextIndex :: Integer
  }
  deriving stock (Show, Eq)

data LiquidationExecutionPolicy = LiquidationExecutionPolicy
  { lepFrozen :: Bool
  , lepMaxAge :: Integer
  , lepTimestamp :: Integer
  , lepBlockGasLimit :: Integer
  } deriving stock (Show, Eq)

liquidationPayloadMaximumAge :: Integer -> LiquidationExecutionPolicy -> Integer
liquidationPayloadMaximumAge liveAge policy
  | lepFrozen policy = lepMaxAge policy
  | otherwise = min liveAge (lepMaxAge policy)

loadLiquidationExecutionPolicy :: LiquidationWorkerConfig -> EthClient -> IO (Either Text (Integer, LiquidationExecutionPolicy))
loadLiquidationExecutionPolicy cfg client = ethBlockNumber client >>= \case
  Left err -> pure $ Left $ rpcErrorText err
  Right blockNumber -> do
    result <- Multicall.multicallAtBlock client (liquidationExecutionPolicyCalls cfg) blockNumber
    pure $ fmap ((,) blockNumber) $ firstRpcError "execution policy" result >>= decodeLiquidationExecutionPolicy

liquidationExecutionPolicyCalls :: LiquidationWorkerConfig -> [Multicall.Call]
liquidationExecutionPolicyCalls cfg =
  [ policyCall (lwcPletherOracle cfg) "isOracleFrozen()"
  , policyCall (lwcPletherOracle cfg) "liquidationStalenessLimit()"
  , policyCall (lwcCfdEngine cfg) "fadMaxStaleness()"
  , policyCall Multicall.multicallAddress "getCurrentBlockTimestamp()"
  , policyCall Multicall.multicallAddress "getCurrentBlockGasLimit()"
  ]

policyCall :: Text -> Text -> Multicall.Call
policyCall target signature = Multicall.Call target True (encodeCall signature [])

decodeLiquidationExecutionPolicy :: [Multicall.CallResult] -> Either Text LiquidationExecutionPolicy
decodeLiquidationExecutionPolicy results
  | length results /= 5 || any (\r -> not (Multicall.resultSuccess r) || BS.length (Multicall.resultData r) /= 32) results =
      Left "Incomplete liquidation execution policy"
  | otherwise = case map (decodeUint256 . Multicall.resultData) results of
      [frozen, liveAge, frozenAge, timestamp, gasLimit]
        | frozen `elem` [0, 1] && liveAge > 0 && frozenAge > 0 && timestamp > 0 && gasLimit > 0 ->
            Right $ LiquidationExecutionPolicy (frozen == 1) (if frozen == 1 then frozenAge else liveAge) timestamp gasLimit
      _ -> Left "Invalid liquidation execution policy"

-- Nothing means split the batch (or isolate a single-account failure).
-- RPC errors remain errors, never evidence of successful full execution.
sizeLiquidationBatchGas :: Integer -> Integer -> Int -> (Integer -> IO (Either RpcError ByteString)) -> IO (Either RpcError (Maybe Integer))
sizeLiquidationBatchGas cap initial count simulate
  | cap <= 0 || count <= 0 = pure $ Left $ RpcJsonError "Invalid liquidation gas sizing inputs"
  | otherwise = go $ min cap (max 1 initial)
  where
    go gas = simulate gas >>= \case
      Left err -> pure $ Left err
      Right bytes
        | BS.length bytes /= 32 -> pure $ Left $ RpcJsonError "Invalid liquidation batch simulation return data"
        | decodeUint256 bytes > fromIntegral count -> pure $ Left $ RpcJsonError "Invalid liquidation batch nextIndex"
        | decodeUint256 bytes == fromIntegral count -> pure $ Right $ Just gas
        | gas >= cap -> pure $ Right Nothing
        | otherwise -> go $ min cap (gas * 2)

data LiquidationWorkerConfig = LiquidationWorkerConfig
  { lwcChainId :: Integer
  , lwcOrderRouter :: Text
  , lwcPletherOracle :: Text
  , lwcCfdEngine :: Text
  , lwcAccountLens :: Text
  , lwcPrivateKey :: Text
  , lwcPollSeconds :: Int
  , lwcScanBatchSize :: Int
  , lwcMulticallSize :: Int
  , lwcExecutionBatchSize :: Int
  , lwcIndexerStartBlock :: Integer
  , lwcIndexerConfirmations :: Int
  , lwcIndexerBatchSize :: Integer
  , lwcIndexerOverlapBlocks :: Integer
  , lwcPendingReplacementSeconds :: Int
  , lwcGasBufferBps :: Integer
  , lwcFeeBufferBps :: Integer
  , lwcFuturePublishMaxRetries :: Int
  , lwcFuturePublishRetryMaxSeconds :: Int
  , lwcMaxTransactionGas :: Integer
  , lwcPythLatestMaxAgeSeconds :: Integer
  }
  deriving stock (Show)

loadLiquidationWorkerConfig :: Config -> Text -> IO LiquidationWorkerConfig
loadLiquidationWorkerConfig cfg privateKey = do
  pollSeconds <- readEnv "LIQUIDATION_WORKER_POLL_SECONDS" 5
  maxTransactionGas <- readEnv "LIQUIDATION_WORKER_MAX_TRANSACTION_GAS" 25_000_000
  scanBatchSize <- readEnv "LIQUIDATION_WORKER_SCAN_BATCH_SIZE" 1_000
  multicallSize <- readEnv "LIQUIDATION_WORKER_MULTICALL_SIZE" 100
  executionBatchSize <- readEnv "LIQUIDATION_WORKER_EXECUTION_BATCH_SIZE" 20
  indexerStartBlock <- readEnv "LIQUIDATION_WORKER_START_BLOCK" (cfgPerpsIndexerStartBlock cfg)
  indexerConfirmations <- readEnv "LIQUIDATION_WORKER_CONFIRMATIONS" 1
  indexerBatchSize <- readEnv "LIQUIDATION_WORKER_INDEX_BATCH_SIZE" 5_000
  indexerOverlapBlocks <- readEnv "LIQUIDATION_WORKER_REORG_OVERLAP_BLOCKS" 12
  pendingReplacementSeconds <- readEnv "LIQUIDATION_WORKER_PENDING_REPLACEMENT_SECONDS" 120
  gasBufferBps <- readEnv "LIQUIDATION_WORKER_GAS_BUFFER_BPS" (cfgKeeperGasBufferBps cfg)
  feeBufferBps <- readEnv "LIQUIDATION_WORKER_FEE_BUFFER_BPS" (cfgKeeperFeeBufferBps cfg)
  futurePublishMaxRetries <- readEnv "LIQUIDATION_WORKER_FUTURE_PUBLISH_MAX_RETRIES" 2
  futurePublishRetryMaxSeconds <- readEnv "LIQUIDATION_WORKER_FUTURE_PUBLISH_RETRY_MAX_SECONDS" 10
  pure
    LiquidationWorkerConfig
      { lwcChainId = cfgPerpsChainId cfg
      , lwcOrderRouter = cfgPerpsOrderRouter cfg
      , lwcPletherOracle = cfgPerpsPletherOracle cfg
      , lwcCfdEngine = cfgPerpsCfdEngine cfg
      , lwcAccountLens = cfgPerpsAccountLens cfg
      , lwcPrivateKey = privateKey
      , lwcPollSeconds = max 1 pollSeconds
      , lwcScanBatchSize = max 1 scanBatchSize
      , lwcMulticallSize = max 1 $ min 100 multicallSize
      , lwcExecutionBatchSize = max 1 $ min 256 executionBatchSize
      , lwcIndexerStartBlock = max 0 indexerStartBlock
      , lwcIndexerConfirmations = max 0 indexerConfirmations
      , lwcIndexerBatchSize = max 1 indexerBatchSize
      , lwcIndexerOverlapBlocks = max 0 indexerOverlapBlocks
      , lwcPendingReplacementSeconds = max 1 pendingReplacementSeconds
      , lwcGasBufferBps = max 0 gasBufferBps
      , lwcFeeBufferBps = max 0 feeBufferBps
      , lwcFuturePublishMaxRetries = max 0 $ min 5 futurePublishMaxRetries
      , lwcFuturePublishRetryMaxSeconds = max 1 $ min 30 futurePublishRetryMaxSeconds
      , lwcMaxTransactionGas = max 1 maxTransactionGas
      , lwcPythLatestMaxAgeSeconds = cfgPythLatestMaxAgeSeconds cfg
      }

readEnv :: (Read a) => String -> a -> IO a
readEnv name fallback = do
  value <- lookupEnv name
  pure $ fromMaybe fallback (value >>= readMaybe)

-- | Side-specific prices returned by Core and the engine's margin policy
-- at the block used for the candidate snapshots.
data FreshLiquidationRiskInputs = FreshLiquidationRiskInputs
  { flriLongPrice :: Integer
  , flriShortPrice :: Integer
  , flriRequiredMarginBps :: Integer
  , flriRiskBufferBps :: Integer
  }
  deriving stock (Show, Eq)

data LiquidationOracleQuote = LiquidationOracleQuote
  { loqLongPrice :: Integer
  , loqShortPrice :: Integer
  , loqUpdateData :: [ByteString]
  , loqUpdateFee :: Integer
  }

data LiquidationFuturePublishTime = LiquidationFuturePublishTime
  { lfptBlockTimestamp :: Integer
  , lfptMinimumPublishTime :: Integer
  , lfptMaximumPublishTime :: Integer
  , lfptFutureSkewSeconds :: Integer
  }
  deriving stock (Show, Eq)

data LiquidationRiskInputError
  = LiquidationRiskInputUnavailable Text
  | LiquidationRiskInputFuturePublishTime LiquidationFuturePublishTime
  deriving stock (Show, Eq)

liquidationSnapshotCalls :: Text -> [Text] -> [Multicall.Call]
liquidationSnapshotCalls accountLens accounts =
  [ Multicall.Call
      { Multicall.callTarget = accountLens
      , Multicall.callAllowFailure = True
      , Multicall.callCalldata = getAccountLedgerSnapshotCall account
      }
  | account <- accounts
  ]

decodeLiquidationSnapshotResults
  :: Int
  -> [Multicall.CallResult]
  -> Either Text [Either Text AccountLedgerSnapshot]
decodeLiquidationSnapshotResults expectedCount results
  | length results /= expectedCount =
      Left $
        "Expected "
          <> tshow expectedCount
          <> " liquidation snapshot results, received "
          <> tshow (length results)
  | otherwise = Right $ zipWith decodeResult [0 :: Int ..] results
  where
    decodeResult index result
      | not $ Multicall.resultSuccess result =
          Left $ "Liquidation snapshot subcall " <> tshow index <> " failed"
      | otherwise =
          case decodeAccountLedgerSnapshot $ Multicall.resultData result of
            Left err -> Left $ "Liquidation snapshot subcall " <> tshow index <> ": " <> err
            Right snapshot -> Right snapshot

selectLiquidationSimulationCandidates
  :: Maybe FreshLiquidationRiskInputs
  -> [(candidate, Either Text AccountLedgerSnapshot)]
  -> [candidate]
selectLiquidationSimulationCandidates riskInputs =
  map fst . filter (liquidationSimulationRequired riskInputs . snd)

data LiquidationRiskDecision
  = LiquidationPositionClosed
  | LiquidationPositionHealthy
  | LiquidationPositionRisky
  | LiquidationRiskUnknown Text
  deriving stock (Show, Eq)

liquidationSimulationRequired
  :: Maybe FreshLiquidationRiskInputs
  -> Either Text AccountLedgerSnapshot
  -> Bool
liquidationSimulationRequired riskInputs =
  (== LiquidationPositionRisky) . liquidationRiskDecision riskInputs

liquidationRiskDecision
  :: Maybe FreshLiquidationRiskInputs
  -> Either Text AccountLedgerSnapshot
  -> LiquidationRiskDecision
liquidationRiskDecision _ (Left err) = LiquidationRiskUnknown err
liquidationRiskDecision riskInputs (Right snapshot)
  | not (alsHasPosition snapshot) && alsSize snapshot == 0 = LiquidationPositionClosed
  | not (alsHasPosition snapshot) = LiquidationRiskUnknown "Snapshot reported a nonzero position without hasPosition"
  | alsSize snapshot <= 0 = LiquidationRiskUnknown "Snapshot reported a non-positive open-position size"
  | alsEntryPrice snapshot <= 0 = LiquidationRiskUnknown "Snapshot reported a non-positive entry price"
  | alsSide snapshot `notElem` [0, 1] = LiquidationRiskUnknown "Snapshot reported an unsupported position side"
  | otherwise =
      case riskInputs of
        -- The lens provides a stored-mark fallback when Core cannot quote
        -- the current liquidation prices.
        Nothing
          | alsLiquidatable snapshot -> LiquidationPositionRisky
          | otherwise -> LiquidationRiskUnknown "Fresh liquidation risk inputs were unavailable"
        Just inputs ->
          case freshLiquidationRisky inputs snapshot of
            Left err -> LiquidationRiskUnknown err
            Right True -> LiquidationPositionRisky
            Right False -> LiquidationPositionHealthy

freshLiquidationRisky
  :: FreshLiquidationRiskInputs
  -> AccountLedgerSnapshot
  -> Either Text Bool
freshLiquidationRisky FreshLiquidationRiskInputs {..} snapshot
  | not (alsHasPosition snapshot) = Left "Cannot classify a snapshot without an open position"
  | alsSize snapshot <= 0 = Left "Cannot classify a non-positive position size"
  | alsEntryPrice snapshot <= 0 = Left "Cannot classify a non-positive entry price"
  | alsSide snapshot `notElem` [0, 1] = Left "Cannot classify an unsupported position side"
  | flriLongPrice <= 0 || flriShortPrice < 0 = Left "Invalid liquidation side prices"
  | flriRequiredMarginBps <= 0 = Left "Liquidation margin requirement must be positive"
  | flriRiskBufferBps < 0 = Left "Liquidation risk buffer cannot be negative"
  | otherwise = Right $ freshEquity <= maintenanceRequirement + riskBuffer
  where
    adversePrice
      | alsSide snapshot == 0 = flriLongPrice
      | otherwise = flriShortPrice
    priceDifference = abs $ adversePrice - alsEntryPrice snapshot
    unsignedPnl = alsSize snapshot * priceDifference `div` tokenPriceScale
    positionProfits
      | alsSide snapshot == 0 = adversePrice <= alsEntryPrice snapshot
      | otherwise = adversePrice >= alsEntryPrice snapshot
    signedPnl
      | positionProfits = unsignedPnl
      | otherwise = negate unsignedPnl
    baseEquity = alsNetEquityAssets snapshot - alsUnrealizedPnlAssets snapshot
    freshEquity = baseEquity + signedPnl
    currentNotional = alsSize snapshot * adversePrice `div` tokenPriceScale
    maintenanceRequirement =
      currentNotional * flriRequiredMarginBps `div` basisPointScale
    riskBuffer =
      max 1 $ currentNotional * flriRiskBufferBps `div` basisPointScale

basisPointScale :: Integer
basisPointScale = 10_000

tokenPriceScale :: Integer
tokenPriceScale = 10 ^ (20 :: Int)

loadFreshLiquidationRiskInputs
  :: LiquidationWorkerConfig
  -> EthClient
  -> Text
  -> Integer
  -> Maybe PythUpdatePayloadRow
  -> IO (Either LiquidationRiskInputError FreshLiquidationRiskInputs)
loadFreshLiquidationRiskInputs cfg client workerAddress blockNumber cachedPayload = do
  let calls =
        [ policyCall (lwcCfdEngine cfg) "riskParams()"
        , policyCall (lwcCfdEngine cfg) "isFadWindow()"
        ] <> liquidationExecutionPolicyCalls cfg
  result <- Multicall.multicallAtBlock client calls blockNumber
  case firstRpcError "liquidation risk policy" result >>= decodeRiskPolicy of
    Left err -> pure $ Left $ LiquidationRiskInputUnavailable err
    Right (marginBps, policy) -> do
      quote <- loadLiquidationOracleQuote cfg client workerAddress blockNumber policy cachedPayload
      pure $ toRiskInputs marginBps <$> quote
  where
    decodeRiskPolicy (riskParams : fadWindow : policyResults) = do
      let params = Multicall.resultData riskParams
          fadFlag = Multicall.resultData fadWindow
      whenEither
        (not (Multicall.resultSuccess riskParams) || BS.length params < 8 * 32 || BS.length params `mod` 32 /= 0)
        "Invalid liquidation riskParams response"
      whenEither (not (Multicall.resultSuccess fadWindow) || BS.length fadFlag /= 32)
        "Invalid liquidation FAD status response"
      let fad = decodeUint256 fadFlag
          marginBps = abiWord (if fad == 1 then 4 else 2) params
      whenEither (fad > 1 || marginBps <= 0) "Invalid liquidation margin policy"
      policy <- decodeLiquidationExecutionPolicy policyResults
      pure (marginBps, policy)
    decodeRiskPolicy _ = Left "Incomplete liquidation risk policy"

    toRiskInputs marginBps quote =
      FreshLiquidationRiskInputs
        { flriLongPrice = loqLongPrice quote
        , flriShortPrice = loqShortPrice quote
        , flriRequiredMarginBps = marginBps
        , flriRiskBufferBps = 5
        }

-- Use Core's price resolution for both discovery and preparation. The accepted
-- basket is a separately validated fallback when the signed cache trails Core.
loadLiquidationOracleQuote
  :: LiquidationWorkerConfig
  -> EthClient
  -> Text
  -> Integer
  -> LiquidationExecutionPolicy
  -> Maybe PythUpdatePayloadRow
  -> IO (Either LiquidationRiskInputError LiquidationOracleQuote)
loadLiquidationOracleQuote cfg client workerAddress blockNumber policy cachedPayload
  | lepFrozen policy = quote []
  | otherwise = do
      signed <- case admittedPayload of
        Left err -> pure $ Left err
        Right updateData -> quote updateData
      case signed of
        Right result -> pure $ Right result
        Left signedError -> quote [] >>= \case
          Right result -> pure $ Right result
          Left acceptedError -> pure $ Left $ case signedError of
            LiquidationRiskInputFuturePublishTime {} -> signedError
            LiquidationRiskInputUnavailable reason -> LiquidationRiskInputUnavailable $
              "Signed payload: " <> reason <> ". Accepted basket: " <> liquidationRiskInputErrorText acceptedError
  where
    oracle = lwcPletherOracle cfg
    maximumAge = liquidationPayloadMaximumAge (lwcPythLatestMaxAgeSeconds cfg) policy
    gasLimit = min (lwcMaxTransactionGas cfg) (lepBlockGasLimit policy * 4 `div` 5)
    admittedPayload = case cachedPayload of
      Nothing -> Left $ LiquidationRiskInputUnavailable "No admitted cached Pyth payload was available"
      Just payload -> validateLiquidationExecutionPayloadDetailed (lepTimestamp policy) maximumAge payload

    quote updateData = do
      fee <- if null updateData
        then pure $ Right 0
        else do
          result <- ethCallAtBlock client (CallParams oracle $ Perps.getUpdateFeeCall updateData) blockNumber
          pure $ case result of
            Left err -> Left $ oracleQuoteError "Oracle fee quote" err
            Right bytes
              | BS.length bytes == 32 -> Right $ decodeUint256 bytes
              | otherwise -> Left $ LiquidationRiskInputUnavailable "Invalid oracle fee response"
      case fee of
        Left err -> pure $ Left err
        Right value -> do
          let params = CallParams oracle $ Perps.updateLiquidationBatchPriceCall workerAddress updateData
          result <- ethCallWithTransactionGasAtBlock client params (lwcOrderRouter cfg) value gasLimit blockNumber
          case result of
            Left err -> pure $ Left $ oracleQuoteError "Oracle liquidation price quote" err
            Right bytes ->
              case decodeLiquidationOracleQuote (lepTimestamp policy) updateData value bytes of
                Right price | null updateData && not (lepFrozen policy) && maximumAge < lepMaxAge policy -> do
                  times <- ethCallAtBlock client
                    (CallParams oracle $ encodeCall "getAcceptedFeedPublishTimes()" []) blockNumber
                  pure $ case times of
                    Left err -> Left $ oracleQuoteError "Accepted feed timestamps" err
                    Right response -> price <$ validateAcceptedLiquidationAge
                      (lepTimestamp policy) maximumAge (abiWord 3 bytes) response
                decoded -> pure decoded

    oracleQuoteError label err = LiquidationRiskInputUnavailable $ label <> case err of
      RpcHttpError _ -> " had a transport error"
      RpcJsonError _ -> " returned an invalid RPC response"
      RpcNodeError code _ revertData ->
        let selector = revertData >>= T.stripPrefix "0x" . T.toLower
            safeSelector = fmap (T.take 8) selector >>= \value ->
              if T.length value == 8 && T.all isHexDigit value then Just (" (0x" <> value <> ")") else Nothing
        in " failed with RPC code " <> tshow code <> fromMaybe "" safeSelector

validateAcceptedLiquidationAge
  :: Integer -> Integer -> Integer -> ByteString -> Either LiquidationRiskInputError ()
validateAcceptedLiquidationAge blockTime maximumAge publishTime bytes
  | BS.length bytes < 96 || BS.length bytes `mod` 32 /= 0 = invalid
  | abiWord 0 bytes /= 32 || abiWord 1 bytes /= toInteger (wordCount - 2) = invalid
  | oldest <= 0 || maximum feedTimes /= publishTime = invalid
  | blockTime - oldest > maximumAge =
      Left $ LiquidationRiskInputUnavailable "Accepted basket was outside the latest freshness window"
  | otherwise = Right ()
  where
    wordCount = BS.length bytes `div` 32
    feedTimes = [abiWord index bytes | index <- [2 .. wordCount - 1]]
    oldest = minimum feedTimes
    invalid = Left $ LiquidationRiskInputUnavailable "Invalid accepted basket timestamps"

decodeLiquidationOracleQuote
  :: Integer
  -> [ByteString]
  -> Integer
  -> ByteString
  -> Either LiquidationRiskInputError LiquidationOracleQuote
decodeLiquidationOracleQuote blockTime updateData expectedFee bytes
  | BS.length bytes /= 5 * 32 = invalid
  | markPrice <= 0 || shortPrice > markPrice || markPrice > longPrice = invalid
  | publishTime <= 0 || publishTime > min blockTime (2 ^ (64 :: Int) - 1) = invalid
  | fee /= expectedFee = invalid
  | otherwise = Right $ LiquidationOracleQuote longPrice shortPrice updateData fee
  where
    longPrice = abiWord 0 bytes
    shortPrice = abiWord 1 bytes
    markPrice = abiWord 2 bytes
    publishTime = abiWord 3 bytes
    fee = abiWord 4 bytes
    invalid = Left $ LiquidationRiskInputUnavailable "Invalid oracle liquidation price response"

abiWord :: Int -> ByteString -> Integer
abiWord index = decodeUint256 . BS.take 32 . BS.drop (index * 32)

liquidationRiskInputErrorText :: LiquidationRiskInputError -> Text
liquidationRiskInputErrorText = \case
  LiquidationRiskInputUnavailable err -> err
  LiquidationRiskInputFuturePublishTime _ ->
    "Executable Pyth payload contained a future publish time"

liquidationRiskInputErrorLogFields :: LiquidationRiskInputError -> [LogField]
liquidationRiskInputErrorLogFields = \case
  LiquidationRiskInputUnavailable _ ->
    [field "failure_reason" ("risk_inputs_unavailable" :: Text)]
  LiquidationRiskInputFuturePublishTime LiquidationFuturePublishTime {..} ->
    [ field "failure_reason" ("future_publish_time" :: Text)
    , field "block_timestamp" lfptBlockTimestamp
    , field "minimum_publish_time" lfptMinimumPublishTime
    , field "maximum_publish_time" lfptMaximumPublishTime
    , field "future_skew_seconds" lfptFutureSkewSeconds
    ]

liquidationFuturePublishRetryDelaySeconds
  :: Int
  -> LiquidationRiskInputError
  -> Maybe Int
liquidationFuturePublishRetryDelaySeconds maximumDelay = \case
  LiquidationRiskInputUnavailable _ -> Nothing
  LiquidationRiskInputFuturePublishTime LiquidationFuturePublishTime {..} ->
    Just . fromInteger $
      min
        (toInteger $ max 1 maximumDelay)
        (max 1 $ lfptFutureSkewSeconds + 1)

retryLiquidationRiskInputs
  :: Int
  -> Int
  -> (Integer -> Int -> Int -> LiquidationRiskInputError -> IO ())
  -> (Int -> IO ())
  -> IO (Either Text Integer)
  -> (Integer -> IO (Either LiquidationRiskInputError value))
  -> Integer
  -> IO (Integer, Int, Either LiquidationRiskInputError value)
retryLiquidationRiskInputs maximumRetries maximumDelay onRetry sleepSeconds getNextBlock loadAtBlock =
  go 0
  where
    retryLimit = max 0 maximumRetries

    go retriesUsed blockNumber = do
      result <- loadAtBlock blockNumber
      case result of
        Left err
          | retriesUsed < retryLimit
          , Just delaySeconds <- liquidationFuturePublishRetryDelaySeconds maximumDelay err -> do
              let nextRetry = retriesUsed + 1
              onRetry blockNumber nextRetry delaySeconds err
              sleepSeconds delaySeconds
              nextBlockResult <- getNextBlock
              case nextBlockResult of
                Left blockError ->
                  pure
                    ( blockNumber
                    , nextRetry
                    , Left . LiquidationRiskInputUnavailable $
                        "Future-publish retry could not resolve a newer block: " <> blockError
                    )
                Right nextBlock -> go nextRetry nextBlock
        _ -> pure (blockNumber, retriesUsed, result)

whenEither :: Bool -> Text -> Either Text ()
whenEither condition err
  | condition = Left err
  | otherwise = Right ()

loadCandidateSnapshots
  :: LiquidationWorkerConfig
  -> EthClient
  -> Integer
  -> [PerpsLiquidationCandidateRow]
  -> IO [(PerpsLiquidationCandidateRow, Either Text AccountLedgerSnapshot)]
loadCandidateSnapshots cfg client blockNumber candidates =
  fmap concat $ forM (chunksOf (lwcMulticallSize cfg) candidates) $ \candidateChunk -> do
    let calls =
          liquidationSnapshotCalls
            (lwcAccountLens cfg)
            (map plcrAccount candidateChunk)
    result <- Multicall.multicallAtBlock client calls blockNumber
    pure $
      case result of
        Left err ->
          zip candidateChunk $
            repeat $ Left $ "snapshot Multicall failed: " <> rpcErrorText err
        Right callResults ->
          case decodeLiquidationSnapshotResults (length candidateChunk) callResults of
            Left err -> zip candidateChunk $ repeat $ Left err
            Right snapshots -> zip candidateChunk snapshots

loadConfirmedPositionSizes
  :: LiquidationWorkerConfig
  -> EthClient
  -> Integer
  -> [PerpsLiquidationCandidateRow]
  -> IO [(PerpsLiquidationCandidateRow, Either Text Integer)]
loadConfirmedPositionSizes cfg client blockNumber candidates =
  fmap concat $ forM (chunksOf (lwcMulticallSize cfg) candidates) $ \candidateChunk -> do
    let calls =
          [ Multicall.Call
              { Multicall.callTarget = lwcCfdEngine cfg
              , Multicall.callAllowFailure = True
              , Multicall.callCalldata = Perps.positionsCall $ plcrAccount candidate
              }
          | candidate <- candidateChunk
          ]
    result <- Multicall.multicallAtBlock client calls blockNumber
    pure $
      case result of
        Left err ->
          zip candidateChunk $
            repeat $ Left $ "confirmed-position Multicall failed: " <> rpcErrorText err
        Right callResults
          | length callResults /= length candidateChunk ->
              zip candidateChunk $
                repeat $
                  Left $
                    "Expected "
                      <> tshow (length candidateChunk)
                      <> " confirmed-position results, received "
                      <> tshow (length callResults)
          | otherwise -> zip candidateChunk $ zipWith decodePosition [0 :: Int ..] callResults
  where
    decodePosition index result
      | not $ Multicall.resultSuccess result =
          Left $ "Confirmed-position subcall " <> tshow index <> " failed"
      | otherwise =
          case Perps.decodePositionSize $ Multicall.resultData result of
            Left err ->
              Left $
                "Confirmed-position subcall "
                  <> tshow index
                  <> " could not be decoded: "
                  <> rpcErrorText err
            Right size -> Right size

chunksOf :: Int -> [value] -> [[value]]
chunksOf size values
  | null values = []
  | otherwise =
      let (next, remaining) = splitAt (max 1 size) values
       in next : chunksOf size remaining

firstRpcError
  :: Text
  -> Either RpcError value
  -> Either Text value
firstRpcError label = \case
  Left err -> Left $ label <> ": " <> rpcErrorText err
  Right value -> Right value

tshow :: (Show value) => value -> Text
tshow = T.pack . show

runLiquidationWorker :: LiquidationWorkerConfig -> DbPool -> EthClient -> LiquidationWorkerMode -> Bool -> IO ()
runLiquidationWorker cfg pool client mode dryRun =
  deriveAddress (lwcPrivateKey cfg) >>= \case
    Left err ->
      logError
        "liquidation_worker_signer_invalid"
        "Liquidation worker signer is invalid"
        (workerLogFields cfg <> [field "error" err])
    Right workerAddress ->
      withDb pool $ \conn ->
        bracket
          (tryPerpsLiquidationLock conn (lwcChainId cfg) (lwcCfdEngine cfg))
          (\acquired ->
              when acquired $
                unlockPerpsLiquidationLock conn (lwcChainId cfg) (lwcCfdEngine cfg)
          )
          $ \acquired ->
            if not acquired
              then
                logWarn
                  "liquidation_worker_lock_unavailable"
                  "Another liquidation worker instance already holds the advisory lock"
                  (workerLogFields cfg)
              else do
                logInfo
                  "liquidation_worker_lock_acquired"
                  "Liquidation worker acquired the advisory lock"
                  ( workerLogFields cfg
                      <> [ field "worker_address" workerAddress
                         , field "mode" $ show mode
                         , field "dry_run" dryRun
                         ]
                  )
                let work = case mode of
                      LiquidationWorkerOnce -> runIteration cfg conn client workerAddress dryRun
                      LiquidationWorkerLoop -> loop conn workerAddress
                -- Dry runs do not emit production health signals; real pending
                -- receipts still use canonical reconciliation.
                if dryRun then work else
                  withAsync (Monitoring.runBacklogWatchdog pool (lwcChainId cfg) (lwcCfdEngine cfg)) $ \watchdog -> do
                    link watchdog
                    work
                    Monitoring.readBacklogHealth conn (lwcChainId cfg) (lwcCfdEngine cfg)
                      >>= Monitoring.emitBacklogHealth (lwcChainId cfg) (lwcCfdEngine cfg)
  where
    loop conn workerAddress = do
      runIteration cfg conn client workerAddress dryRun
      waitForNextSweep conn workerAddress $ lwcPollSeconds cfg
      loop conn workerAddress

    -- The idle delay follows a complete sweep, never an individual page.
    -- Pending transactions are reconciled even during the short idle delay.
    waitForNextSweep _ _ remainingSeconds | remainingSeconds <= 0 = pure ()
    waitForNextSweep conn workerAddress remainingSeconds = do
      let delaySeconds = min pendingReconciliationPollSeconds remainingSeconds
      threadDelay $ delaySeconds * 1_000_000
      pending <-
        getPendingPerpsLiquidationCandidates
          conn
          (lwcChainId cfg)
          (lwcCfdEngine cfg)
          (lwcPendingReplacementSeconds cfg)
          pendingBroadcastRetrySeconds
      unless (null pending) $
        reconcilePendingCandidates cfg conn client workerAddress pending
      waitForNextSweep conn workerAddress $ remainingSeconds - delaySeconds

runIteration :: LiquidationWorkerConfig -> Connection -> EthClient -> Text -> Bool -> IO ()
runIteration cfg conn client workerAddress dryRun = do
  indexNewCandidates cfg conn client
  processCandidates cfg conn client workerAddress dryRun
  logInfoEvery
    300
    "liquidation_worker_heartbeat"
    "Liquidation worker completed an iteration"
    (workerLogFields cfg <> [field "dry_run" dryRun])

indexNewCandidates :: LiquidationWorkerConfig -> Connection -> EthClient -> IO ()
indexNewCandidates cfg conn client = do
  latestResult <- ethBlockNumber client
  case latestResult of
    Left err ->
      logWarnEvery
        60
        "liquidation_chain_head_fetch_failed"
        "Liquidation worker could not fetch the chain head"
        (workerLogFields cfg <> [field "error" $ rpcErrorText err])
    Right latestBlock -> indexPages liquidationDiscoveryCatchupPageLimit latestBlock
  where
    indexPages remainingPages latestBlock = do
      lastIndexed <- getPerpsLiquidationLastIndexedBlock conn (lwcChainId cfg) (lwcCfdEngine cfg)
      let indexRange =
            liquidationIndexRange
              (lwcIndexerStartBlock cfg)
              (lwcIndexerConfirmations cfg)
              (lwcIndexerBatchSize cfg)
              (lwcIndexerOverlapBlocks cfg)
              lastIndexed
              latestBlock
          confirmedLatest =
            max 0 $ latestBlock - fromIntegral (max 0 $ lwcIndexerConfirmations cfg)
      case indexRange of
        Nothing -> pure ()
        Just (startBlock, endBlock) -> do
          logsResult <-
            ethGetLogs
              client
              (lwcCfdEngine cfg)
              [Perps.positionOpenedTopic]
              startBlock
              endBlock
          case logsResult of
            Left err ->
              logWarnEvery
                60
                "liquidation_candidate_logs_fetch_failed"
                "Liquidation worker could not fetch position-opening logs"
                ( workerLogFields cfg
                    <> [ field "from_block" startBlock
                       , field "to_block" endBlock
                       , field "error" $ rpcErrorText err
                       ]
                )
            Right logs -> do
              let discovered = mapMaybePositionOpened logs
              forM_ discovered $ \(account, blockNumber) ->
                upsertPerpsLiquidationCandidate
                  conn
                  (lwcChainId cfg)
                  (lwcCfdEngine cfg)
                  account
                  blockNumber
              setPerpsLiquidationLastIndexedBlock conn (lwcChainId cfg) (lwcCfdEngine cfg) endBlock
              unless (null logs) $
                logInfoEvery
                  300
                  "liquidation_candidates_indexed"
                  "Liquidation worker indexed a position-opening log batch"
                  ( workerLogFields cfg
                      <> [ field "from_block" startBlock
                         , field "to_block" endBlock
                         , field "event_count" $ length logs
                         , field "candidate_count" $ length discovered
                         ]
                  )
              when (endBlock < confirmedLatest) $
                if remainingPages > 1
                  then indexPages (remainingPages - 1) latestBlock
                  else
                    logWarnEvery
                      60
                      "liquidation_candidate_catchup_limited"
                      "Liquidation discovery reached its bounded catch-up page limit before the confirmed head"
                      ( workerLogFields cfg
                          <> [ field "last_indexed_block" endBlock
                             , field "confirmed_head_block" confirmedLatest
                             , field "page_limit" liquidationDiscoveryCatchupPageLimit
                             ]
                      )

liquidationIndexRange
  :: Integer -- configured start block
  -> Int -- confirmations
  -> Integer -- maximum batch span
  -> Integer -- reorg overlap
  -> Integer -- last indexed block
  -> Integer -- latest chain block
  -> Maybe (Integer, Integer)
liquidationIndexRange configuredStart confirmations batchSize overlapBlocks lastIndexed latestBlock =
  if startBlock > confirmedLatest
    then Nothing
    else Just (startBlock, min confirmedLatest $ startBlock + safeBatchSize - 1)
  where
    safeStart = max 0 configuredStart
    safeBatchSize = max 1 batchSize
    safeOverlap = min (max 0 overlapBlocks) (safeBatchSize - 1)
    confirmedLatest = max 0 $ latestBlock - fromIntegral (max 0 confirmations)
    startBlock
      | lastIndexed < safeStart = safeStart
      | otherwise = max safeStart (lastIndexed + 1 - safeOverlap)

processCandidates :: LiquidationWorkerConfig -> Connection -> EthClient -> Text -> Bool -> IO ()
processCandidates cfg conn client workerAddress dryRun = do
  pending <-
    getPendingPerpsLiquidationCandidates
      conn
      (lwcChainId cfg)
      (lwcCfdEngine cfg)
      (lwcPendingReplacementSeconds cfg)
      pendingBroadcastRetrySeconds
  case pending of
    candidates@(_ : _) -> reconcilePendingCandidates cfg conn client workerAddress candidates
    [] -> do
      signerReady <-
        if dryRun
          then pure True
          else checkSignerTransactionReadiness cfg conn
      when signerReady processAvailableCandidates
  where
    processAvailableCandidates = do
      -- Freeze membership/order for this sweep. Newly discovered accounts join
      -- the next sweep; retries cannot monopolize page one. No database lock is
      -- held while calling RPC or waiting for receipts.
      candidates <- getPerpsLiquidationCandidates conn (lwcChainId cfg) (lwcCfdEngine cfg) 2_147_483_647
      started <- getPOSIXTime
      completed <- runLiquidationSweep (lwcScanBatchSize cfg) candidates processPage
      finished <- getPOSIXTime
      logInfo "liquidation_sweep_finished" "Liquidation discovery sweep finished" $
        workerLogFields cfg <> [field "completed" completed, field "candidate_count" (length candidates), field "duration_seconds" (realToFrac (finished - started) :: Double)]
      when (finished - started > 60) $
        logWarn "liquidation_sweep_slow" "Liquidation sweep exceeded 60 seconds" (workerLogFields cfg)

    processPage candidates = do
      pending <- getPendingPerpsLiquidationCandidates conn (lwcChainId cfg) (lwcCfdEngine cfg)
        (lwcPendingReplacementSeconds cfg) pendingBroadcastRetrySeconds
      -- A timeout or reconciliation failure must never permit a second nonce.
      if not (null pending)
        then pure False
        else do
          blockResult <- ethBlockNumber client
          case blockResult of
            Left err -> do
              logWarnEvery 60 "liquidation_snapshot_block_fetch_failed"
                "Liquidation worker could not resolve an exact block" $
                  workerLogFields cfg <> [field "error" (rpcErrorText err)]
              pure False
            Right snapshotBlock -> do
              processCandidatesAtBlock snapshotBlock candidates
              pure True

    processCandidatesAtBlock initialBlock candidates = do
      cachedPayload <- getLatestPythUpdatePayload conn
      (snapshotBlock, retryCount, riskResult) <-
        retryLiquidationRiskInputs
          (lwcFuturePublishMaxRetries cfg)
          (lwcFuturePublishRetryMaxSeconds cfg)
          logFuturePublishRetry
          (\seconds -> threadDelay $ seconds * 1_000_000)
          fetchRetryBlock
          (\blockNumber -> loadFreshLiquidationRiskInputs cfg client workerAddress blockNumber cachedPayload)
          initialBlock
      case riskResult of
        Right _
          | retryCount > 0 ->
              logInfo
                "liquidation_risk_inputs_future_retry_recovered"
                "Liquidation risk inputs became valid at a newer exact block"
                ( workerLogFields cfg
                    <> [ field "candidate_count" $ length candidates
                       , field "initial_snapshot_block" initialBlock
                       , field "snapshot_block" snapshotBlock
                       , field "risk_retry_count" retryCount
                       ]
                )
        _ -> pure ()
      (snapshots, flatCandidates) <-
        loadAndReconcileSnapshots snapshotBlock candidates
      processClassifiedCandidates snapshotBlock retryCount snapshots flatCandidates riskResult
      where
        fetchRetryBlock = do
          blockResult <- ethBlockNumber client
          pure $ case blockResult of
            Left err -> Left $ rpcErrorText err
            Right blockNumber -> Right blockNumber

        logFuturePublishRetry blockNumber retryNumber delaySeconds err =
          logWarn
            "liquidation_risk_inputs_future_retry"
            "Liquidation risk inputs were ahead of the selected block; retrying at a newer exact block"
            ( workerLogFields cfg
                <> [ field "candidate_count" $ length candidates
                   , field "snapshot_block" blockNumber
                   , field "risk_retry_number" retryNumber
                   , field "retry_delay_seconds" delaySeconds
                   , field "error" $ liquidationRiskInputErrorText err
                   ]
                <> liquidationRiskInputErrorLogFields err
            )

    loadAndReconcileSnapshots snapshotBlock candidates = do
      snapshots <- loadCandidateSnapshots cfg client snapshotBlock candidates
      forM_ snapshots $ \(candidate, snapshotResult) ->
        case snapshotResult of
          Left err ->
            recordCandidateError cfg conn candidate "snapshot_read" err
          Right _ -> pure ()
      let flatCandidates =
            [ candidate
            | (candidate, Right snapshot) <- snapshots
            , not $ alsHasPosition snapshot
            , alsSize snapshot == 0
            ]
      reconcileFlatCandidates snapshotBlock flatCandidates
      pure (snapshots, flatCandidates)

    processClassifiedCandidates
      snapshotBlock
      retryCount
      snapshots
      flatCandidates
      riskResult = do
        let riskInputs = either (const Nothing) Just riskResult
            classified =
              [ (candidate, snapshotResult, liquidationRiskDecision riskInputs snapshotResult)
              | (candidate, snapshotResult) <- snapshots
              ]
            simulationCandidates =
              [ candidate
              | (candidate, _, LiquidationPositionRisky) <- classified
              ]
        unless dryRun $ Monitoring.observeRisk conn (lwcChainId cfg) (lwcCfdEngine cfg)
          [ (plcrAccount candidate, case decision of
                LiquidationPositionRisky -> Just True
                LiquidationPositionHealthy -> Just False
                LiquidationPositionClosed -> Just False
                LiquidationRiskUnknown _ -> Nothing)
          | (candidate, _, decision) <- classified
          ]
        case riskResult of
          Left err ->
            logWarnEvery
              60
              "liquidation_risk_inputs_unavailable"
              "Liquidation risk inputs were unavailable; only exact-block stored-risk positions will be simulated"
              ( workerLogFields cfg
                  <> [ field "candidate_count" $ length snapshots
                     , field "snapshot_block" snapshotBlock
                     , field "risk_retry_count" retryCount
                     , field "error" $ liquidationRiskInputErrorText err
                     ]
                  <> liquidationRiskInputErrorLogFields err
              )
          Right _ -> pure ()

        -- Rotate every classified healthy or unknown account. The DB query
        -- orders by last_checked_at, so leaving these untouched would
        -- permanently starve candidates beyond the first page. Unknown state
        -- is retained and surfaced as an error, but never spends an
        -- eth_estimateGas call.
        forM_ classified $ \(candidate, snapshotResult, decision) ->
          case decision of
            LiquidationPositionHealthy ->
              markPerpsLiquidationCandidateChecked
                conn
                (lwcChainId cfg)
                (lwcCfdEngine cfg)
                (plcrAccount candidate)
            LiquidationRiskUnknown err ->
              case snapshotResult of
                -- Snapshot read failures were recorded above already.
                Left _ -> pure ()
                Right _ ->
                  recordCandidateError
                    cfg
                    conn
                    candidate
                    "risk_classification"
                    err
            LiquidationPositionClosed -> pure ()
            LiquidationPositionRisky -> pure ()

        let unknownCandidateCount =
              length
                [ ()
                | (_, _, LiquidationRiskUnknown _) <- classified
                ]
            healthyCandidateCount =
              length
                [ ()
                | (_, _, LiquidationPositionHealthy) <- classified
                ]

        logInfoEvery
          300
          "liquidation_candidates_classified"
          "Liquidation worker classified a batched candidate sweep"
          ( workerLogFields cfg
              <> [ field "candidate_count" $ length snapshots
                 , field "snapshot_block" snapshotBlock
                 , field "risk_retry_count" retryCount
                 , field "multicall_size" $ lwcMulticallSize cfg
                 , field "snapshot_failure_count" $
                     length [() | (_, Left _) <- snapshots]
                 , field "flat_candidate_count" $ length flatCandidates
                 , field "healthy_candidate_count" healthyCandidateCount
                 , field "unknown_candidate_count" unknownCandidateCount
                 , field "simulation_candidate_count" $ length simulationCandidates
                 ]
          )

        unless (null simulationCandidates) $
          processClassifiedPayload simulationCandidates

    reconcileFlatCandidates _ [] = pure ()
    reconcileFlatCandidates snapshotBlock flatCandidates = do
      let confirmedBlock =
            max 0 $
              snapshotBlock - fromIntegral (max 0 $ lwcIndexerConfirmations cfg)
      confirmed <-
        loadConfirmedPositionSizes cfg client confirmedBlock flatCandidates
      forM_ confirmed $ \(candidate, positionResult) ->
        case positionResult of
          Left err ->
            recordCandidateError
              cfg
              conn
              candidate
              "confirmed_position_read"
              err
          Right 0 ->
            deletePerpsLiquidationCandidate
              conn
              (lwcChainId cfg)
              (lwcCfdEngine cfg)
              (plcrAccount candidate)
          Right _ ->
            -- A close visible at the scan block can still be reorged out.
            -- Retain the opening candidate until the confirmed read is zero.
            markPerpsLiquidationCandidateChecked
              conn
              (lwcChainId cfg)
              (lwcCfdEngine cfg)
              (plcrAccount candidate)

    processClassifiedPayload candidates =
      processLiquidationBatches
        (chunksOf (lwcExecutionBatchSize cfg) candidates)
        prepareExecutionBatch
        executePreparedBatch

    prepareExecutionBatch candidates = do
      rejectedPayload <-
        getPerpsLiquidationRejectedPayload
          conn
          (lwcChainId cfg)
          (lwcCfdEngine cfg)
      policyResult <- loadLiquidationExecutionPolicy cfg client
      executionPayload <- case policyResult of
        Left err -> pure $ Left err
        Right (blockNumber, policy) -> do
          cachedPayload <- getLatestPythUpdatePayload conn
          quote <- loadLiquidationOracleQuote cfg client workerAddress blockNumber policy cachedPayload
          pure $ case quote of
            Left err -> Left $ liquidationRiskInputErrorText err
            Right result -> Right (blockNumber, policy, result)
      case executionPayload of
        Left err -> do
          forM_ candidates $ \candidate ->
            recordCandidateError
              cfg
              conn
              candidate
              "execution_payload"
              err
          logWarnEvery
            60
            "liquidation_execution_pyth_payload_unavailable"
            "Liquidation worker could not resolve executable oracle prices"
            ( workerLogFields cfg
                <> [ field "candidate_count" $ length candidates
                   , field "error" err
                   ]
            )
          pure Nothing
        Right (blockNumber, policy, LiquidationOracleQuote {loqUpdateData = updateData, loqUpdateFee = updateFee}) -> do
          -- A payload rejected before freezing can become admissible when the
          -- policy changes; do not suppress it throughout the weekend.
          let payloadKey = liquidationPayloadFingerprint (lwcPletherOracle cfg) (lwcOrderRouter cfg) updateData
                <> ":" <> tshow (lepFrozen policy, lepMaxAge policy)
          logInfoEvery 60 "liquidation_execution_policy" "Resolved liquidation oracle policy" $
            workerLogFields cfg <> [field "oracle_frozen" (lepFrozen policy), field "maximum_age_seconds" (lepMaxAge policy)]
          let prepared = Just (blockNumber, policy, payloadKey, updateData, updateFee)
              -- An empty replay represents mutable accepted state. A rejection
              -- of that state must never suppress later canonical baskets.
              circuitDecision
                | null updateData = ProcessLiquidationPayload
                | otherwise = liquidationPayloadCircuitDecision (plrprPayloadKey <$> rejectedPayload) payloadKey
          case circuitDecision of
            SuppressRejectedLiquidationPayload ->
              case rejectedPayload of
                Just rejected -> do
                  logWarnEvery
                    60
                    "liquidation_pyth_payload_suppressed"
                    "Liquidation scan is waiting for a new Pyth payload after a deterministic oracle rejection"
                    ( workerLogFields cfg
                        <> [ field "candidate_count" $ length candidates
                           , field "payload_key" payloadKey
                           , field "revert_selector" $ plrprSelector rejected
                           , field "rejected_at" $ plrprRejectedAt rejected
                           , field "error" $ plrprError rejected
                           ]
                    )
                  pure Nothing
                Nothing -> pure prepared
            ClearRejectedLiquidationPayload -> do
              clearPerpsLiquidationRejectedPayload
                conn
                (lwcChainId cfg)
                (lwcCfdEngine cfg)
              logInfo
                "liquidation_pyth_payload_changed"
                "Liquidation scan resumed with a new Pyth payload"
                (workerLogFields cfg <> [field "payload_key" payloadKey])
              pure prepared
            ProcessLiquidationPayload -> pure prepared

    executePreparedBatch candidates (blockNumber, policy, payloadKey, updateData, updateFee) =
      processLiquidationBatch
        cfg
        conn
        client
        workerAddress
        dryRun
        blockNumber
        policy
        payloadKey
        updateData
        updateFee
        candidates

-- Membership is captured once by the caller; page processing cannot reorder
-- or starve the remainder when it updates last_checked_at.
runLiquidationSweep :: Int -> [candidate] -> ([candidate] -> IO Bool) -> IO Bool
runLiquidationSweep pageSize candidates processPage = go $ chunksOf pageSize candidates
  where
    go [] = pure True
    go (page : rest) = processPage page >>= \continue -> if continue then go rest else pure False

data BatchOutcome
  = BatchComplete
  | BatchStopped
  | BatchSplit
  | BatchRetryFrom Int
  deriving stock (Show, Eq)

processLiquidationBatches
  :: [[candidate]]
  -> ([candidate] -> IO (Maybe prepared))
  -> ([candidate] -> prepared -> IO BatchOutcome)
  -> IO ()
processLiquidationBatches batches prepareBatch executeBatch = go [(batch, 0 :: Int) | batch <- batches]
  where
    go [] = pure ()
    go (([], _) : rest) = go rest
    go ((batch, retries) : rest) = do
      prepared <- prepareBatch batch
      case prepared of
        Nothing -> pure ()
        Just inputs -> executeBatch batch inputs >>= \case
          BatchStopped -> pure ()
          BatchComplete -> go rest
          BatchSplit
            | length batch > 1 ->
                let (left, right) = splitAt (length batch `div` 2) batch
                 in go $ (left, 0) : (right, 0) : rest
            | otherwise -> go rest -- isolated failure was recorded by executor
          BatchRetryFrom index
            | index >= 0 && index < length batch && retries < 3 ->
                go $ (drop index batch, retries + 1) : rest
            | otherwise -> go rest -- bounded retries; next sweep rechecks it

processLiquidationBatch
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> Text
  -> Bool
  -> Integer
  -> LiquidationExecutionPolicy
  -> Text
  -> [ByteString]
  -> Integer
  -> [PerpsLiquidationCandidateRow]
  -> IO BatchOutcome
processLiquidationBatch cfg conn client workerAddress dryRun blockNumber policy payloadKey updateData updateFee candidates = do
  let accounts = map plcrAccount candidates
      callData = Perps.executeLiquidationBatchCall accounts updateData
  estimate <- ethEstimateGasAtBlock client workerAddress (lwcOrderRouter cfg) updateFee callData blockNumber
  gasResult <- case estimate of
    Left err -> pure $ Left err
    Right estimated -> sizeLiquidationBatchGas
      (min (lwcMaxTransactionGas cfg) (lepBlockGasLimit policy * 4 `div` 5))
      (liquidationTransactionGasLimit (lwcGasBufferBps cfg) estimated)
      (length accounts)
      (\gas -> ethCallWithTransactionGasAtBlock client (CallParams (lwcOrderRouter cfg) callData)
        workerAddress updateFee gas blockNumber)
  case gasResult of
    Right Nothing -> do
      when (length candidates == 1) $
        forM_ candidates $ \candidate -> recordCandidateError cfg conn candidate "batch_gas_cap"
          "Account could not complete within the transaction gas cap; continuing other accounts"
      pure BatchSplit
    Left err
      | not (null updateData)
      , Just selectorText <- payloadGlobalSimulationRevertSelector err -> do
          let failure = "liquidation simulation rejected Pyth payload: " <> rpcErrorText err
          recordPerpsLiquidationRejectedPayload
            conn
            (lwcChainId cfg)
            (lwcCfdEngine cfg)
            payloadKey
            selectorText
            failure
          forM_ candidates $ \candidate ->
            recordCandidateError cfg conn candidate "batch_simulation" failure
          logError
            "liquidation_pyth_payload_rejected"
            "Liquidation worker suppressed a deterministic Pyth payload until the cache changes"
            ( workerLogFields cfg
                <> [ field "candidate_count" $ length candidates
                   , field "accounts" accounts
                   , field "payload_key" payloadKey
                   , field "revert_selector" selectorText
                   , field "error" failure
                   ]
            )
          pure BatchStopped
      | otherwise -> do
          forM_ candidates $ \candidate ->
            recordCandidateError cfg conn candidate "batch_simulation" $
              "liquidation batch simulation failed: " <> rpcErrorText err
          pure BatchStopped
    Right (Just estimatedGas) -> do
      logInfo
        "liquidation_batch_opportunity_detected"
        "Liquidation batch passed transaction simulation"
        ( workerLogFields cfg
            <> [ field "candidate_count" $ length candidates
               , field "accounts" accounts
               , field "estimated_gas" estimatedGas
               , field "update_fee_wei" $ show updateFee
               , field "dry_run" dryRun
               ]
        )
      if dryRun
        then do
          forM_ candidates $ \candidate ->
            markPerpsLiquidationCandidateChecked
              conn
              (lwcChainId cfg)
              (lwcCfdEngine cfg)
              (plcrAccount candidate)
          pure BatchComplete
        else do
          prepared <-
            prepareLiquidationTransaction cfg client workerAddress estimatedGas updateFee callData
          case prepared of
            Left err -> do
              forM_ candidates $ \candidate ->
                recordCandidateError cfg conn candidate "batch_transaction_prepare" err
              pure BatchStopped
            Right (tx, signed) -> do
              affordabilityResult <- checkTransactionAffordability client workerAddress tx
              case affordabilityResult of
                Left err -> do
                  recordSignerTransactionRetry cfg conn tx err
                  forM_ candidates $ \candidate ->
                    recordCandidateError cfg conn candidate "batch_transaction_affordability" err
                  pure BatchStopped
                Right _ -> do
                  let rawTx = signedRawTransaction signed
                      txHash = signedTransactionHash signed
                      pendingCandidates =
                        [ candidate
                            { plcrAttemptCount = plcrAttemptCount candidate + 1
                            , plcrPendingTxHash = Just txHash
                            , plcrPendingNonce = Just $ txNonce tx
                            }
                        | candidate <- candidates
                        ]
                  -- Persist the deterministic signed hash before broadcast. If the
                  -- RPC response is lost, the next iteration reconciles this nonce
                  -- instead of creating a transaction behind it.
                  persistPendingTransactionBatch cfg conn workerAddress candidates tx signed
                  recordPendingBroadcastAttemptBatch cfg conn candidates
                  sendResult <- ethSendRawTransaction client rawTx
                  case sendResult of
                    Left err -> do
                      when (isInsufficientFundsRpcError err) $
                        recordSignerTransactionRetry cfg conn tx (rpcErrorText err)
                      forM_ pendingCandidates $ \candidate ->
                        recordCandidateError cfg conn candidate "batch_transaction_broadcast" $
                          "batch broadcast result uncertain for " <> txHash <> ": " <> rpcErrorText err
                      pure BatchStopped
                    Right returnedHash
                      | normalizeAddress returnedHash /= normalizeAddress txHash -> do
                          forM_ pendingCandidates $ \candidate ->
                            recordCandidateErrorWith
                              cfg
                              conn
                              candidate
                              "batch_broadcast_hash_mismatch"
                              [field "returned_transaction_hash" returnedHash]
                              "RPC returned a transaction hash that did not match the signed batch transaction hash"
                          pure BatchStopped
                      | otherwise -> do
                          logInfo
                            "liquidation_batch_transaction_submitted"
                            "Liquidation batch transaction was submitted"
                            ( workerLogFields cfg
                                <> [ field "candidate_count" $ length candidates
                                   , field "accounts" accounts
                                   , field "transaction_hash" txHash
                                   , field "nonce" $ txNonce tx
                                   , field "gas_limit" $ txGasLimit tx
                                   , field "value_wei" $ show $ txValue tx
                                   , field "max_priority_fee_per_gas_wei" $ show $ txMaxPriorityFeePerGas tx
                                   , field "max_fee_per_gas_wei" $ show $ txMaxFeePerGas tx
                                   ]
                            )
                          receiptResult <- waitForReceipt client txHash 60
                          case receiptResult of
                            Left err -> do
                              forM_ pendingCandidates $ \candidate ->
                                recordCandidateError cfg conn candidate "batch_receipt_wait" err
                              pure BatchStopped
                            Right receipt -> do
                              reconciled <- handleLiquidationBatchReceipt cfg conn client pendingCandidates receipt
                              if not reconciled then pure BatchStopped else
                                case validateLiquidationBatchReceipt (lwcOrderRouter cfg) accounts receipt of
                                  Right progress | lbpNextIndex progress < fromIntegral (length candidates) ->
                                    pure $ BatchRetryFrom $ fromIntegral $ lbpNextIndex progress
                                  _ -> pure BatchComplete

prepareLiquidationTransaction
  :: LiquidationWorkerConfig
  -> EthClient
  -> Text
  -> Integer
  -> Integer
  -> ByteString
  -> IO (Either Text (Tx1559, SignedTransaction))
prepareLiquidationTransaction cfg client workerAddress estimatedGas value callData = do
  nonceResult <- ethGetTransactionCount client workerAddress
  gasPriceResult <- ethGasPrice client
  priorityResult <- ethMaxPriorityFeePerGas client
  case (nonceResult, gasPriceResult) of
    (Right nonce, Right gasPrice) -> do
      let priorityBase = either (const gasPrice) id priorityResult
          maxFeeBase = max gasPrice priorityBase
          gasLimit = estimatedGas -- exact envelope already simulated; do not buffer twice
          maxPriorityFee = applyBuffer priorityBase (lwcFeeBufferBps cfg)
          maxFee = max maxPriorityFee $ applyBuffer maxFeeBase (lwcFeeBufferBps cfg)
          tx =
            Tx1559
              { txChainId = lwcChainId cfg
              , txNonce = nonce
              , txMaxPriorityFeePerGas = maxPriorityFee
              , txMaxFeePerGas = maxFee
              , txGasLimit = gasLimit
              , txTo = lwcOrderRouter cfg
              , txValue = value
              , txData = callData
              }
      signResult <- signTransaction (lwcPrivateKey cfg) tx
      pure $ fmap (\signed -> (tx, signed)) signResult
    _ ->
      pure $
        Left $
          T.intercalate
            "; "
            [ rpcErrorText err
            | Left err <- [nonceResult, gasPriceResult]
            ]

persistPendingTransaction
  :: LiquidationWorkerConfig
  -> Connection
  -> Text
  -> Text
  -> Tx1559
  -> SignedTransaction
  -> IO ()
persistPendingTransaction cfg conn sender account tx signed =
  recordPerpsLiquidationCandidatePending
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    account
    (txNonce tx)
    sender
    (signedTransactionHash signed)
    (encodeHex $ signedRawTransaction signed)
    (encodeHex $ txData tx)
    (txValue tx)
    (txGasLimit tx)
    (txMaxPriorityFeePerGas tx)
    (txMaxFeePerGas tx)

persistPendingTransactionBatch
  :: LiquidationWorkerConfig
  -> Connection
  -> Text
  -> [PerpsLiquidationCandidateRow]
  -> Tx1559
  -> SignedTransaction
  -> IO ()
persistPendingTransactionBatch cfg conn sender candidates tx signed =
  withTransaction conn $
    forM_ candidates $ \candidate ->
      persistPendingTransaction cfg conn sender (plcrAccount candidate) tx signed

recordPendingBroadcastAttempt :: LiquidationWorkerConfig -> Connection -> Text -> IO ()
recordPendingBroadcastAttempt cfg conn account =
  recordPerpsLiquidationCandidateBroadcastAttempt
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    account

recordPendingBroadcastAttemptBatch
  :: LiquidationWorkerConfig
  -> Connection
  -> [PerpsLiquidationCandidateRow]
  -> IO ()
recordPendingBroadcastAttemptBatch cfg conn candidates =
  withTransaction conn $
    forM_ candidates $ \candidate ->
      recordPendingBroadcastAttempt cfg conn (plcrAccount candidate)

reconcilePendingCandidates
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> Text
  -> [PerpsLiquidationCandidateRow]
  -> IO ()
reconcilePendingCandidates _ _ _ _ [] = pure ()
reconcilePendingCandidates cfg conn client workerAddress candidates@(_ : _) =
  case sharedPendingTransaction candidates of
    Just (txHash, nonce, pendingSender, rawTxHex)
      | normalizeAddress pendingSender /= normalizeAddress workerAddress ->
          forM_ candidates $ \pendingCandidate ->
            recordCandidateCritical cfg conn pendingCandidate "liquidation_signer_mismatch" $
              "pending liquidation batch was signed by "
                <> pendingSender
                <> " but the configured key resolves to "
                <> workerAddress
                <> "; refusing automatic rebroadcast or replacement until manually reconciled"
      | otherwise -> do
          receiptResult <- ethGetTransactionReceipt client txHash
          case receiptResult of
            Left err ->
              forM_ candidates $ \pendingCandidate ->
                recordCandidateError cfg conn pendingCandidate "pending_batch_receipt_lookup" $
                  "pending batch receipt lookup failed for " <> txHash <> ": " <> rpcErrorText err
            Right (Just receipt) -> do
              _ <- handleLiquidationBatchReceipt cfg conn client candidates receipt
              pure ()
            Right Nothing ->
              reconcileMissingBatchReceipt cfg conn client pendingSender candidates nonce txHash rawTxHex
    Nothing ->
      -- Never clear partially persisted batch state automatically: the shared
      -- transaction may still be live and a second nonce lane would be unsafe.
      forM_ candidates $ \pendingCandidate ->
        recordCandidateCritical cfg conn pendingCandidate "liquidation_pending_batch_state_invalid" $
          "inconsistent or incomplete pending liquidation batch requires manual reconciliation"

sharedPendingTransaction
  :: [PerpsLiquidationCandidateRow]
  -> Maybe (Text, Integer, Text, Text)
sharedPendingTransaction [] = Nothing
sharedPendingTransaction candidates@(candidate : _) = do
  txHash <- plcrPendingTxHash candidate
  nonce <- plcrPendingNonce candidate
  sender <- plcrPendingSender candidate
  rawTx <- plcrPendingRawTx candidate
  let expected = (Just txHash, Just nonce, Just sender, Just rawTx)
  if all (\row -> pendingIdentity row == expected) candidates
    then Just (txHash, nonce, sender, rawTx)
    else Nothing
  where
    pendingIdentity row =
      ( plcrPendingTxHash row
      , plcrPendingNonce row
      , plcrPendingSender row
      , plcrPendingRawTx row
      )

reconcileMissingBatchReceipt
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> Text
  -> [PerpsLiquidationCandidateRow]
  -> Integer
  -> Text
  -> Text
  -> IO ()
reconcileMissingBatchReceipt cfg conn client pendingSender candidates nonce txHash rawTxHex = do
  latestResult <- ethBlockNumber client
  confirmedNonceResult <-
    case latestResult of
      Left err -> pure $ Left err
      Right latestBlock ->
        ethGetTransactionCountAtBlock
          client
          pendingSender
          (max 0 $ latestBlock - fromIntegral (lwcIndexerConfirmations cfg))
  case confirmedNonceResult of
    Right confirmedNonce
      | confirmedNonce > nonce ->
          resolveConsumedPendingBatchNonce cfg conn client candidates txHash nonce
    _
      | any plcrPendingStale candidates -> do
          signerReady <- checkSignerTransactionReadiness cfg conn
          case liquidationPendingSignerAction signerReady (any plcrPendingBroadcastDue candidates) of
            ReplacePendingSignerTransaction ->
              replacePendingBatchTransaction cfg conn client pendingSender candidates nonce txHash rawTxHex
            RebroadcastPendingSignerTransaction ->
              rebroadcastPendingBatchTransaction cfg conn client candidates nonce txHash rawTxHex
            WaitForPendingSignerTransaction -> pure ()
      | otherwise ->
          when (any plcrPendingBroadcastDue candidates) $
            rebroadcastPendingBatchTransaction cfg conn client candidates nonce txHash rawTxHex

rebroadcastPendingBatchTransaction
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> [PerpsLiquidationCandidateRow]
  -> Integer
  -> Text
  -> Text
  -> IO ()
rebroadcastPendingBatchTransaction cfg conn client candidates nonce txHash rawTxHex =
  case decodeHexUpdate rawTxHex of
    Left err ->
      forM_ candidates $ \candidate ->
        recordCandidateCritical cfg conn candidate "liquidation_pending_batch_transaction_invalid" $
          "pending batch raw transaction could not be decoded for " <> txHash <> ": " <> err
    Right rawTx -> do
      recordPendingBroadcastAttemptBatch cfg conn candidates
      rebroadcastResult <- ethSendRawTransaction client rawTx
      case rebroadcastResult of
        Left err -> do
          when (isInsufficientFundsRpcError err) $
            forM_ (pendingCandidateMaximumCost =<< firstCandidate candidates) $ \requiredBalance ->
              recordSignerReadinessFailure cfg conn requiredBalance (rpcErrorText err)
          forM_ candidates $ \candidate ->
            recordCandidateError cfg conn candidate "batch_transaction_rebroadcast" $
              "waiting for pending batch transaction " <> txHash <> " after rebroadcast: " <> rpcErrorText err
        Right returnedHash
          | normalizeAddress returnedHash == normalizeAddress txHash ->
              logInfoEvery
                60
                "liquidation_batch_transaction_rebroadcast"
                "Liquidation worker rebroadcast the persisted batch transaction"
                ( workerLogFields cfg
                    <> [ field "candidate_count" $ length candidates
                       , field "transaction_hash" txHash
                       , field "nonce" nonce
                       ]
                )
          | otherwise ->
              forM_ candidates $ \candidate ->
                recordCandidateErrorWith
                  cfg
                  conn
                  candidate
                  "batch_rebroadcast_hash_mismatch"
                  [field "returned_transaction_hash" returnedHash]
                  "Rebroadcast RPC hash did not match the persisted batch transaction hash"

replacePendingBatchTransaction
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> Text
  -> [PerpsLiquidationCandidateRow]
  -> Integer
  -> Text
  -> Text
  -> IO ()
replacePendingBatchTransaction cfg conn client pendingSender candidates nonce txHash rawTxHex =
  case firstCandidate candidates >>= sharedReplacementState candidates of
    Just (Just callDataHex, Just value, Just gasLimit, Just oldPriorityFee, Just oldMaxFee) ->
      case decodeHexUpdate callDataHex of
        Left err ->
          forM_ candidates $ \candidate ->
            recordCandidateCritical cfg conn candidate "liquidation_batch_replacement_state_invalid" $
              "pending batch calldata could not be decoded for same-nonce replacement: " <> err
        Right callData -> do
          gasPriceResult <- ethGasPrice client
          priorityResult <- ethMaxPriorityFeePerGas client
          case gasPriceResult of
            Left err ->
              forM_ candidates $ \candidate ->
                recordCandidateError cfg conn candidate "batch_replacement_fee_quote" $
                  "could not price same-nonce batch replacement: " <> rpcErrorText err
            Right gasPrice -> do
              let priorityBase = either (const gasPrice) id priorityResult
                  (replacementPriorityFee, replacementMaxFee) =
                    sameNonceReplacementFees
                      (lwcFeeBufferBps cfg)
                      gasPrice
                      priorityBase
                      oldPriorityFee
                      oldMaxFee
                  replacementTx =
                    Tx1559
                      { txChainId = lwcChainId cfg
                      , txNonce = nonce
                      , txMaxPriorityFeePerGas = replacementPriorityFee
                      , txMaxFeePerGas = replacementMaxFee
                      , txGasLimit = liquidationTransactionGasLimit 0 gasLimit
                      , txTo = lwcOrderRouter cfg
                      , txValue = value
                      , txData = callData
                      }
              signResult <- signTransaction (lwcPrivateKey cfg) replacementTx
              case signResult of
                Left err ->
                  forM_ candidates $ \candidate ->
                    recordCandidateError cfg conn candidate "batch_replacement_sign" err
                Right signed -> do
                  affordabilityResult <- checkTransactionAffordability client pendingSender replacementTx
                  case affordabilityResult of
                    Left err -> do
                      recordSignerTransactionRetry cfg conn replacementTx err
                      forM_ candidates $ \candidate ->
                        recordCandidateError cfg conn candidate "batch_replacement_affordability" err
                      when (any plcrPendingBroadcastDue candidates) $
                        rebroadcastPendingBatchTransaction cfg conn client candidates nonce txHash rawTxHex
                    Right _ -> do
                      let replacementHash = signedTransactionHash signed
                          replacementCandidates =
                            [ candidate
                                { plcrAttemptCount = plcrAttemptCount candidate + 1
                                , plcrPendingTxHash = Just replacementHash
                                , plcrPendingNonce = Just nonce
                                }
                            | candidate <- candidates
                            ]
                      persistPendingTransactionBatch cfg conn pendingSender candidates replacementTx signed
                      recordPendingBroadcastAttemptBatch cfg conn candidates
                      sendResult <- ethSendRawTransaction client (signedRawTransaction signed)
                      case sendResult of
                        Left err -> do
                          when (isInsufficientFundsRpcError err) $
                            recordSignerTransactionRetry cfg conn replacementTx (rpcErrorText err)
                          forM_ replacementCandidates $ \candidate ->
                            recordCandidateError cfg conn candidate "batch_replacement_broadcast" $
                              "same-nonce batch replacement broadcast is uncertain for "
                                <> replacementHash
                                <> ": "
                                <> rpcErrorText err
                        Right returnedHash
                          | normalizeAddress returnedHash == normalizeAddress replacementHash ->
                              logWarn
                                "liquidation_batch_transaction_replaced"
                                "Liquidation worker replaced a stale batch transaction at the same nonce"
                                ( workerLogFields cfg
                                    <> [ field "candidate_count" $ length candidates
                                       , field "previous_transaction_hash" txHash
                                       , field "transaction_hash" returnedHash
                                       , field "nonce" nonce
                                       , field "max_priority_fee_per_gas_wei" $ show replacementPriorityFee
                                       , field "max_fee_per_gas_wei" $ show replacementMaxFee
                                       ]
                                )
                          | otherwise ->
                              forM_ replacementCandidates $ \candidate ->
                                recordCandidateErrorWith
                                  cfg
                                  conn
                                  candidate
                                  "batch_replacement_hash_mismatch"
                                  [field "returned_transaction_hash" returnedHash]
                                  "Replacement RPC hash did not match the signed batch transaction hash"
    _ ->
      forM_ candidates $ \candidate ->
        recordCandidateCritical cfg conn candidate "liquidation_batch_replacement_state_incomplete" $
          "pending liquidation batch lacks consistent fee or calldata fields required for same-nonce replacement"

sharedReplacementState
  :: [PerpsLiquidationCandidateRow]
  -> PerpsLiquidationCandidateRow
  -> Maybe (Maybe Text, Maybe Integer, Maybe Integer, Maybe Integer, Maybe Integer)
sharedReplacementState candidates candidate =
  let state = replacementState candidate
   in if all ((== state) . replacementState) candidates then Just state else Nothing
  where
    replacementState row =
      ( plcrPendingCallData row
      , plcrPendingValue row
      , plcrPendingGasLimit row
      , plcrPendingMaxPriorityFeePerGas row
      , plcrPendingMaxFeePerGas row
      )

resolveConsumedPendingBatchNonce
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> [PerpsLiquidationCandidateRow]
  -> Text
  -> Integer
  -> IO ()
resolveConsumedPendingBatchNonce cfg conn client candidates txHash nonce =
  ethBlockNumber client >>= \case
    Left err ->
      forM_ candidates $ \candidate ->
        recordCandidateError cfg conn candidate "consumed_batch_nonce_head_read" $
          "nonce " <> T.pack (show nonce) <> " was consumed but the confirmed head could not be read: " <> rpcErrorText err
    Right latestBlock -> do
      let confirmedBlock = max 0 $ latestBlock - fromIntegral (lwcIndexerConfirmations cfg)
      positions <- loadConfirmedPositionSizes cfg client confirmedBlock candidates
      let positionFailures = [(candidate, err) | (candidate, Left err) <- positions]
      if null positionFailures
        then do
          withTransaction conn $
            forM_ positions $ \(candidate, confirmedPosition) ->
              case confirmedPosition of
                Left _ -> pure ()
                Right 0 -> deleteCandidate cfg conn candidate
                Right _ -> do
                  clearPendingCandidate cfg conn candidate
                  markCandidateChecked cfg conn candidate
          logWarn
            "liquidation_batch_nonce_reconciled"
            "Consumed liquidation batch nonce was reconciled from confirmed position state"
            ( workerLogFields cfg
                <> [ field "candidate_count" $ length candidates
                   , field "transaction_hash" txHash
                   , field "nonce" nonce
                   ]
            )
        else
          forM_ positionFailures $ \(candidate, err) ->
            recordCandidateError cfg conn candidate "consumed_batch_nonce_position_read" $
              "nonce " <> T.pack (show nonce) <> " was consumed but confirmed position verification failed: " <> err

handleLiquidationBatchReceipt
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> [PerpsLiquidationCandidateRow]
  -> TxReceipt
  -> IO Bool
handleLiquidationBatchReceipt cfg conn client candidates receipt = do
  confirmed <- waitForLiquidationConfirmation 60
    (receiptBlockNumber receipt + fromIntegral (lwcIndexerConfirmations cfg))
    (ethBlockNumber client) (threadDelay 1_000_000)
  case confirmed of
    Left err -> do
      forM_ candidates $ \candidate -> recordCandidateError cfg conn candidate "batch_confirmation_wait" err
      pure False
    Right latestBlock -> do
      current <- ethGetTransactionReceipt client (receiptTxHash receipt)
      case current of
        Right (Just verified)
          | receiptBlockHash verified == receiptBlockHash receipt ->
              handleConfirmedLiquidationBatchReceipt cfg conn client candidates latestBlock verified
        _ -> do
          logWarn "liquidation_receipt_changed" "Receipt changed while awaiting confirmations; retaining pending nonce"
            (workerLogFields cfg <> receiptLogFields receipt)
          pure False

-- Bounded wait: a slow chain retains the persisted transaction for recovery.
waitForLiquidationConfirmation :: Int -> Integer -> IO (Either RpcError Integer) -> IO () -> IO (Either Text Integer)
waitForLiquidationConfirmation attempts requiredHead readHead sleep = do
  result <- readHead
  case result of
    Left err -> pure $ Left $ rpcErrorText err
    Right headBlock
      | headBlock >= requiredHead -> pure $ Right headBlock
      | attempts <= 0 -> pure $ Left "Timed out waiting for liquidation confirmation depth"
      | otherwise -> sleep >> waitForLiquidationConfirmation (attempts - 1) requiredHead readHead sleep

handleConfirmedLiquidationBatchReceipt
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> [PerpsLiquidationCandidateRow]
  -> Integer
  -> TxReceipt
  -> IO Bool
handleConfirmedLiquidationBatchReceipt cfg conn client candidates latestBlock receipt
  | not (receiptSucceeded receipt) = do
      let err = "liquidation batch transaction reverted: " <> receiptTxHash receipt
      withTransaction conn $
        forM_ candidates $ \candidate -> do
          clearPendingCandidate cfg conn candidate
          persistCandidateError cfg conn candidate err
      logError "liquidation_batch_transaction_reverted" "Liquidation batch transaction reverted on-chain" $
        workerLogFields cfg <> receiptLogFields receipt <> [field "candidate_count" $ length candidates, field "error" err]
      pure True
  | otherwise =
      case validateLiquidationBatchReceipt (lwcOrderRouter cfg) (map plcrAccount candidates) receipt of
        Left err -> do
          forM_ candidates $ \candidate ->
            recordCandidateCriticalWith cfg conn candidate "liquidation_batch_receipt_invariant_failed" (receiptLogFields receipt) err
          pure False
        Right progress -> do
          let confirmedBlock = max 0 $ latestBlock - fromIntegral (lwcIndexerConfirmations cfg)
          positions <- loadConfirmedPositionSizes cfg client confirmedBlock candidates
          let positionFailures =
                [(candidate, err) | (candidate, Left err) <- positions]
              invalidLiquidations =
                [ candidate
                | candidate <- candidates
                , Just item <- [itemForAccount progress $ plcrAccount candidate]
                , Perps.lbiResult item == Perps.LiquidationBatchLiquidated
                , not $ isLiquidationReceiptFor (lwcCfdEngine cfg) (plcrAccount candidate) receipt
                ]
          case (positionFailures, invalidLiquidations) of
            (_ : _, _) -> do
              forM_ positionFailures $ \(candidate, err) ->
                recordCandidateErrorWith cfg conn candidate "batch_post_receipt_position_read" (receiptLogFields receipt) $
                  "liquidation batch post-state verification failed: " <> err
              pure False
            (_, _ : _) -> do
              forM_ candidates $ \candidate ->
                recordCandidateCriticalWith
                  cfg conn candidate "liquidation_batch_item_invariant_failed" (receiptLogFields receipt)
                  "batch reported a liquidation without the matching engine PositionLiquidated event"
              pure False
            ([], []) -> do
              outcomes <- withTransaction conn $ do
                results <- forM positions $ \(candidate, positionResult) ->
                  reconcileBatchItem cfg conn receipt progress candidate positionResult
                let liquidated = any ((== Perps.LiquidationBatchLiquidated) . Perps.lbiResult) (lbpItems progress)
                when (and results && liquidated) $
                  Monitoring.recordConfirmedProgress conn (lwcChainId cfg) (lwcCfdEngine cfg) (receiptBlockNumber receipt)
                pure results
              logInfo
                "liquidation_batch_confirmed"
                "Liquidation batch transaction was reconciled account by account"
                ( workerLogFields cfg
                    <> receiptLogFields receipt
                    <> [ field "candidate_count" $ length candidates
                       , field "attempted_count" $ lbpNextIndex progress
                       , field "reconciled_count" $ length [() | True <- outcomes]
                       ]
                )
              pure $ and outcomes

reconcileBatchItem
  :: LiquidationWorkerConfig
  -> Connection
  -> TxReceipt
  -> LiquidationBatchProgress
  -> PerpsLiquidationCandidateRow
  -> Either Text Integer
  -> IO Bool
reconcileBatchItem cfg conn receipt progress candidate positionResult =
  case positionResult of
    Left err -> do
      recordCandidateErrorWith cfg conn candidate "batch_post_receipt_position_read" (receiptLogFields receipt) $
        "liquidation batch post-state verification failed: " <> err
      pure False
    Right positionSize ->
      case itemForAccount progress (plcrAccount candidate) of
        Nothing -> do
          clearPendingCandidate cfg conn candidate
          retryCandidate cfg conn candidate "liquidation batch stopped before attempting this account"
          pure True
        Just item -> reconcileAttemptedItem item positionSize
  where
    reconcileAttemptedItem item positionSize =
      case Perps.lbiResult item of
        Perps.LiquidationBatchLiquidated
          | not $ isLiquidationReceiptFor (lwcCfdEngine cfg) (plcrAccount candidate) receipt -> do
              recordCandidateCriticalWith
                cfg conn candidate "liquidation_batch_item_invariant_failed" (receiptLogFields receipt)
                "batch item reported liquidation without the matching engine PositionLiquidated event"
              pure False
          | positionSize == 0 -> deleteCandidate cfg conn candidate >> pure True
          | otherwise -> clearAndCheck >> pure True
        Perps.LiquidationBatchSkippedNoPosition
          | positionSize == 0 -> deleteCandidate cfg conn candidate >> pure True
          | otherwise -> do
              clearPendingCandidate cfg conn candidate
              retryCandidate cfg conn candidate "batch reported no position but confirmed state is still open"
              pure True
        Perps.LiquidationBatchSkippedSolvent
          | positionSize == 0 -> deleteCandidate cfg conn candidate >> pure True
          | otherwise -> do
              Monitoring.observeRisk conn (lwcChainId cfg) (lwcCfdEngine cfg) [(plcrAccount candidate, Just False)]
              clearAndCheck
              pure True
        Perps.LiquidationBatchFailed -> do
          clearPendingCandidate cfg conn candidate
          retryCandidate cfg conn candidate $
            "isolated liquidation batch item failed with selector " <> encodeHex (Perps.lbiErrorSelector item)
          pure True
      where
        clearAndCheck = do
          clearPendingCandidate cfg conn candidate
          markCandidateChecked cfg conn candidate

itemForAccount :: LiquidationBatchProgress -> Text -> Maybe Perps.LiquidationBatchItem
itemForAccount progress account =
  firstCandidate $
    filter
      ((== normalizeAddress account) . normalizeAddress . Perps.lbiAccount)
      (lbpItems progress)

firstCandidate :: [a] -> Maybe a
firstCandidate = \case
  value : _ -> Just value
  [] -> Nothing

clearPendingCandidate :: LiquidationWorkerConfig -> Connection -> PerpsLiquidationCandidateRow -> IO ()
clearPendingCandidate cfg conn candidate =
  clearPerpsLiquidationCandidatePending
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    (plcrAccount candidate)

markCandidateChecked :: LiquidationWorkerConfig -> Connection -> PerpsLiquidationCandidateRow -> IO ()
markCandidateChecked cfg conn candidate =
  markPerpsLiquidationCandidateChecked
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    (plcrAccount candidate)

deleteCandidate :: LiquidationWorkerConfig -> Connection -> PerpsLiquidationCandidateRow -> IO ()
deleteCandidate cfg conn candidate =
  deletePerpsLiquidationCandidate
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    (plcrAccount candidate)

retryCandidate :: LiquidationWorkerConfig -> Connection -> PerpsLiquidationCandidateRow -> Text -> IO ()
retryCandidate cfg conn candidate err = do
  recordPerpsLiquidationCandidateRetryableError
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    (plcrAccount candidate)
    err
  logWarnEvery
    60
    "liquidation_batch_item_retryable"
    "Liquidation batch left an account eligible for a later retry"
    (candidateLogFields cfg candidate <> [field "error" err])

recordCandidateError
  :: LiquidationWorkerConfig
  -> Connection
  -> PerpsLiquidationCandidateRow
  -> Text
  -> Text
  -> IO ()
recordCandidateError cfg conn candidate failureStage =
  recordCandidateErrorWith cfg conn candidate failureStage []

recordCandidateErrorWith
  :: LiquidationWorkerConfig
  -> Connection
  -> PerpsLiquidationCandidateRow
  -> Text
  -> [LogField]
  -> Text
  -> IO ()
recordCandidateErrorWith cfg conn candidate failureStage contextFields err = do
  persistCandidateError cfg conn candidate err
  logErrorEvery
    60
    ("liquidation_candidate_" <> failureStage <> "_failed")
    "Liquidation candidate processing failed"
    ( candidateLogFields cfg candidate
        <> contextFields
        <> [field "failure_stage" failureStage, field "error" err]
    )

recordCandidateCritical
  :: LiquidationWorkerConfig
  -> Connection
  -> PerpsLiquidationCandidateRow
  -> Text
  -> Text
  -> IO ()
recordCandidateCritical cfg conn candidate eventName =
  recordCandidateCriticalWith cfg conn candidate eventName []

recordCandidateCriticalWith
  :: LiquidationWorkerConfig
  -> Connection
  -> PerpsLiquidationCandidateRow
  -> Text
  -> [LogField]
  -> Text
  -> IO ()
recordCandidateCriticalWith cfg conn candidate eventName contextFields err = do
  persistCandidateError cfg conn candidate err
  logError
    eventName
    "Liquidation worker stopped automatic processing for a pending candidate"
    ( candidateLogFields cfg candidate
        <> contextFields
        <> [field "error" err, field "manual_intervention_required" True]
    )

persistCandidateError
  :: LiquidationWorkerConfig
  -> Connection
  -> PerpsLiquidationCandidateRow
  -> Text
  -> IO ()
persistCandidateError cfg conn candidate err =
  recordPerpsLiquidationCandidateError
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    (plcrAccount candidate)
    err

workerLogFields :: LiquidationWorkerConfig -> [LogField]
workerLogFields cfg =
  [ field "chain_id" $ lwcChainId cfg
  , field "order_router" $ lwcOrderRouter cfg
  , field "cfd_engine" $ lwcCfdEngine cfg
  , field "account_lens" $ lwcAccountLens cfg
  , field "poll_seconds" $ lwcPollSeconds cfg
  , field "scan_batch_size" $ lwcScanBatchSize cfg
  , field "multicall_size" $ lwcMulticallSize cfg
  , field "execution_batch_size" $ lwcExecutionBatchSize cfg
  , field "max_transaction_gas" $ lwcMaxTransactionGas cfg
  ]

candidateLogFields :: LiquidationWorkerConfig -> PerpsLiquidationCandidateRow -> [LogField]
candidateLogFields cfg candidate =
  workerLogFields cfg
    <> [ field "account" $ plcrAccount candidate
       , field "attempt_count" $ plcrAttemptCount candidate
       ]
    <> maybe
      []
      (\transactionHash -> [field "pending_transaction_hash" transactionHash])
      (plcrPendingTxHash candidate)
    <> maybe [] (\nonce -> [field "pending_nonce" nonce]) (plcrPendingNonce candidate)

receiptLogFields :: TxReceipt -> [LogField]
receiptLogFields receipt =
  [ field "transaction_hash" $ receiptTxHash receipt
  , field "receipt_block_number" $ receiptBlockNumber receipt
  ]

waitForReceipt :: EthClient -> Text -> Int -> IO (Either Text TxReceipt)
waitForReceipt _ txHash 0 = pure $ Left $ "timed out waiting for receipt " <> txHash
waitForReceipt client txHash attempts = do
  receiptResult <- ethGetTransactionReceipt client txHash
  case receiptResult of
    Left err -> pure $ Left $ rpcErrorText err
    Right (Just receipt) -> pure $ Right receipt
    Right Nothing -> do
      threadDelay 1_000_000
      waitForReceipt client txHash (attempts - 1)

decodeCachedPythPayload :: PythUpdatePayloadRow -> Either Text ([Integer], [ByteString])
decodeCachedPythPayload PythUpdatePayloadRow {puprPublishTimes, puprUpdateData} = do
  publishTimes <- parseValue "publish_times" puprPublishTimes
  updateHex <- parseValue "update_data" puprUpdateData
  updateData <- traverse decodeHexUpdate updateHex
  pure (publishTimes, updateData)

validateLiquidationExecutionPayload
  :: Integer
  -> Integer
  -> PythUpdatePayloadRow
  -> Either Text [ByteString]
validateLiquidationExecutionPayload currentTime maximumAge =
  either (Left . liquidationRiskInputErrorText) Right
    . validateLiquidationExecutionPayloadDetailed currentTime maximumAge

validateLiquidationExecutionPayloadDetailed
  :: Integer
  -> Integer
  -> PythUpdatePayloadRow
  -> Either LiquidationRiskInputError [ByteString]
validateLiquidationExecutionPayloadDetailed currentTime maximumAge payload = do
  (minimumPublishTime, maximumPublishTime, updateData) <-
    either (Left . LiquidationRiskInputUnavailable) Right $ do
      (publishTimes, updateData) <- decodeCachedPythPayload payload
      whenEither
        (puprSource payload /= "backend_hermes_latest_v2")
        "Executable Pyth payload did not come from the admitted latest cache"
      whenEither
        (null publishTimes)
        "Executable Pyth payload did not contain publish times"
      whenEither
        (null updateData || any BS.null updateData)
        "Executable Pyth payload did not contain update data"
      let minimumPublishTime = minimum publishTimes
          maximumPublishTime = maximum publishTimes
      whenEither
        ( minimumPublishTime <= 0
            || minimumPublishTime /= puprMinPublishTime payload
            || maximumPublishTime /= puprMaxPublishTime payload
        )
        "Executable Pyth payload publish-time metadata did not match its data"
      pure (minimumPublishTime, maximumPublishTime, updateData)
  when (maximumPublishTime > currentTime) $
    Left $ LiquidationRiskInputFuturePublishTime $
      LiquidationFuturePublishTime currentTime minimumPublishTime maximumPublishTime (maximumPublishTime - currentTime)
  when (currentTime - minimumPublishTime > max 1 maximumAge) $
    Left $ LiquidationRiskInputUnavailable "Executable Pyth payload was outside the latest freshness window"
  pure updateData

loadFreshLiquidationExecutionPayload
  :: Integer
  -> IO Integer
  -> IO (Maybe PythUpdatePayloadRow)
  -> IO (Either Text [ByteString])
loadFreshLiquidationExecutionPayload maximumAge getCurrentTime getLatestPayload = do
  latestPayload <- getLatestPayload
  case latestPayload of
    Nothing -> pure $ Left "No cached latest Pyth payload was available for execution"
    Just payload -> do
      currentTime <- getCurrentTime
      pure $ validateLiquidationExecutionPayload currentTime maximumAge payload

parseValue :: (FromJSON a) => Text -> Value -> Either Text a
parseValue label value =
  case fromJSON value of
    Success decoded -> Right decoded
    Error err -> Left $ label <> " JSON decode failed: " <> T.pack err

decodeHexUpdate :: Text -> Either Text ByteString
decodeHexUpdate value =
  case B16.decode (TE.encodeUtf8 $ T.toLower $ strip0x value) of
    Right bytes -> Right bytes
    Left err -> Left $ "invalid updateData hex: " <> T.pack err

encodeHex :: ByteString -> Text
encodeHex value = "0x" <> TE.decodeUtf8 (B16.encode value)

isLiquidationReceiptFor :: Text -> Text -> TxReceipt -> Bool
isLiquidationReceiptFor cfdEngine account receipt =
  receiptSucceeded receipt
    && any
      ( \logEntry ->
          normalizeAddress (rpcLogAddress logEntry) == normalizedEngine
            && fmap normalizeAddress (Perps.decodePositionLiquidatedAccount logEntry) == Just normalizedAccount
      )
      (receiptLogs receipt)
  where
    normalizedEngine = normalizeAddress cfdEngine
    normalizedAccount = normalizeAddress account

validateLiquidationBatchReceipt
  :: Text
  -> [Text]
  -> TxReceipt
  -> Either Text LiquidationBatchProgress
validateLiquidationBatchReceipt orderRouter accounts receipt
  | not (receiptSucceeded receipt) = Left "liquidation batch transaction reverted"
  | null accounts = Left "liquidation batch receipt has no submitted accounts"
  | length stops > 1 = Left "liquidation batch receipt emitted more than one stop marker"
  | null items && null stops = Left "liquidation batch receipt omitted all result and stop events"
  | map Perps.lbiIndex items /= [0 .. fromIntegral (length items) - 1] =
      Left "liquidation batch item indices are not a contiguous prefix"
  | length items > length accounts = Left "liquidation batch emitted more items than submitted accounts"
  | length itemAccounts /= length (nub itemAccounts) =
      Left "liquidation batch emitted a duplicate account result"
  | any (`notElem` submittedAccounts) itemAccounts =
      Left "liquidation batch emitted a result for an account that was not submitted"
  | nextIndex /= fromIntegral (length items) =
      Left "liquidation batch stop index does not match the attempted prefix"
  | null stops && length items /= length accounts =
      Left "liquidation batch ended without accounting for every submitted account"
  | otherwise = Right $ LiquidationBatchProgress items nextIndex
  where
    routerLogs =
      filter
        ((== normalizeAddress orderRouter) . normalizeAddress . rpcLogAddress)
        (receiptLogs receipt)
    items = sortOn Perps.lbiIndex $ mapMaybe Perps.decodeLiquidationBatchItem routerLogs
    itemAccounts = map (normalizeAddress . Perps.lbiAccount) items
    submittedAccounts = map normalizeAddress accounts
    stops = mapMaybe Perps.decodeLiquidationBatchStoppedIndex routerLogs
    nextIndex = case stops of
      stop : _ -> stop
      [] -> fromIntegral $ length accounts

isExpectedLiquidationSimulationRevert :: RpcError -> Bool
isExpectedLiquidationSimulationRevert = \case
  RpcNodeError _ message revertData ->
    let normalizedError = normalizedNodeError message revertData
     in any
          (`T.isInfixOf` normalizedError)
          [ "0x451cebb2" -- CfdEngine__PositionIsSolvent()
          , "0x4565ea0c" -- CfdEngine__NoPositionToLiquidate()
          ]
  _ -> False

payloadGlobalSimulationRevertSelector :: RpcError -> Maybe Text
payloadGlobalSimulationRevertSelector = \case
  RpcNodeError _ message revertData ->
    findKnownSelector $ normalizedNodeError message revertData
  _ -> Nothing
  where
    findKnownSelector revertData =
      case filter (`T.isInfixOf` revertData) payloadGlobalRevertSelectors of
        selectorText : _ -> Just selectorText
        [] -> Nothing

    payloadGlobalRevertSelectors =
      [ "0x2acbe915" -- InvalidWormholeVaa()
      , "0xf4a25e0f" -- PletherOracle__StalePrice()
      ]

normalizedNodeError :: Text -> Maybe Text -> Text
normalizedNodeError message revertData =
  T.toLower $ message <> maybe "" (" " <>) revertData

isInsufficientFundsRpcError :: RpcError -> Bool
isInsufficientFundsRpcError = \case
  RpcNodeError _ message errData ->
    let normalizedError = normalizedNodeError message errData
     in any
          (`T.isInfixOf` normalizedError)
          [ "insufficient funds"
          , "insufficient balance for transfer"
          ]
  _ -> False

liquidationPayloadFingerprint :: Text -> Text -> [ByteString] -> Text
liquidationPayloadFingerprint pletherOracle orderRouter updateData =
  encodeHex $
    keccak256 $
      framed "plether:liquidation-pyth-payload:v1"
        <> labelled "plether-oracle" (TE.encodeUtf8 $ normalizeAddress pletherOracle)
        <> labelled "order-router" (TE.encodeUtf8 $ normalizeAddress orderRouter)
        <> encodeUint256 (fromIntegral $ length updateData)
        <> mconcat (map framed updateData)
  where
    labelled label value = framed label <> framed value
    framed value = encodeUint256 (fromIntegral $ BS.length value) <> value

liquidationPayloadCircuitDecision :: Maybe Text -> Text -> LiquidationPayloadCircuitDecision
liquidationPayloadCircuitDecision maybeRejectedKey payloadKey =
  case normalizeAddress <$> maybeRejectedKey of
    Nothing -> ProcessLiquidationPayload
    Just rejectedKey
      | rejectedKey == normalizeAddress payloadKey -> SuppressRejectedLiquidationPayload
      | otherwise -> ClearRejectedLiquidationPayload

mapMaybePositionOpened :: [RpcLog] -> [(Text, Integer)]
mapMaybePositionOpened =
  foldr
    (\logEntry found -> case Perps.decodePositionOpenedAccount logEntry of
        Just account -> (account, rpcLogBlockNumber logEntry) : found
        Nothing -> found
    )
    []

applyBuffer :: Integer -> Integer -> Integer
applyBuffer value bufferBps =
  ((value * (10_000 + bufferBps)) + 9_999) `div` 10_000

-- | The V2 liquidation router returns successfully with
-- @LiquidationBatchStopped(0)@ when the transaction does not retain enough gas
-- to enter its first account. That makes @eth_estimateGas@ observe the cheap
-- early-stop path instead of the liquidation path. The deployed release caps
-- pending orders at five and requires, before item zero, 600k engine gas, 250k
-- router gas, 600k per pending order, and a 250k tail reserve. Five million
-- covers that 4.1m gate plus the oracle update and router preamble.
liquidationTransactionGasFloor :: Integer
liquidationTransactionGasFloor = 5_000_000

liquidationTransactionGasLimit
  :: Integer -- configured estimate buffer, in basis points
  -> Integer -- node-provided gas estimate
  -> Integer
liquidationTransactionGasLimit bufferBps estimatedGas =
  max liquidationTransactionGasFloor $
    applyBuffer (max 0 estimatedGas) (max 0 bufferBps)

sameNonceReplacementFees
  :: Integer -- current fee buffer bps
  -> Integer -- current gas price
  -> Integer -- current priority fee
  -> Integer -- previous priority fee
  -> Integer -- previous max fee
  -> (Integer, Integer)
sameNonceReplacementFees feeBufferBps gasPrice priorityBase oldPriorityFee oldMaxFee =
  (replacementPriorityFee, replacementMaxFee)
  where
    currentPriorityFee = applyBuffer priorityBase feeBufferBps
    currentMaxFee =
      max currentPriorityFee $
        applyBuffer (max gasPrice priorityBase) feeBufferBps
    replacementPriorityFee =
      max currentPriorityFee $ applyBuffer oldPriorityFee 1_250
    replacementMaxFee =
      max replacementPriorityFee $
        max currentMaxFee (applyBuffer oldMaxFee 1_250)

liquidationSignerCircuitDecision :: Maybe Bool -> LiquidationSignerCircuitDecision
liquidationSignerCircuitDecision = \case
  Nothing -> SignerTransactionReady
  Just True -> RecheckSignerTransaction
  Just False -> SuppressSignerTransaction

liquidationPendingSignerAction :: Bool -> Bool -> LiquidationPendingSignerAction
liquidationPendingSignerAction signerReady broadcastDue
  | signerReady = ReplacePendingSignerTransaction
  | broadcastDue = RebroadcastPendingSignerTransaction
  | otherwise = WaitForPendingSignerTransaction

-- Keep the cooldown in PostgreSQL so restarts and repeated --once invocations
-- cannot turn an unfunded signer into a simulation/replacement RPC storm.
signerTransactionRetrySeconds :: Int
signerTransactionRetrySeconds = 60

pendingBroadcastRetrySeconds :: Int
pendingBroadcastRetrySeconds = 60

pendingReconciliationPollSeconds :: Int
pendingReconciliationPollSeconds = 1

-- Bound startup/outage catch-up without advancing only one 5,000-block page
-- per health sweep. At the default page size this covers five
-- million blocks in one iteration while still terminating on bad state.
liquidationDiscoveryCatchupPageLimit :: Int
liquidationDiscoveryCatchupPageLimit = 1_000

checkSignerTransactionReadiness
  :: LiquidationWorkerConfig
  -> Connection
  -> IO Bool
checkSignerTransactionReadiness cfg conn = do
  retry <-
    getPerpsLiquidationSignerRetry
      conn
      (lwcChainId cfg)
      (lwcCfdEngine cfg)
      signerTransactionRetrySeconds
  case liquidationSignerCircuitDecision (plrsrRetryDue <$> retry) of
    SignerTransactionReady -> pure True
    SuppressSignerTransaction -> do
      forM_ retry $ \blocked ->
        logWarnEvery
          signerTransactionRetrySeconds
          "liquidation_signer_transaction_suppressed"
          "Liquidation worker is waiting before rechecking signer transaction affordability"
          ( signerRetryLogFields cfg blocked
              <> [field "retry_seconds" signerTransactionRetrySeconds]
          )
      pure False
    RecheckSignerTransaction -> do
      clearPerpsLiquidationSignerRetry
        conn
        (lwcChainId cfg)
        (lwcCfdEngine cfg)
      forM_ retry $ \blocked ->
        logInfo
          "liquidation_signer_transaction_retrying"
          "Liquidation worker is allowing one freshly priced signer transaction attempt"
          (signerRetryLogFields cfg blocked)
      pure True

recordSignerTransactionRetry :: LiquidationWorkerConfig -> Connection -> Tx1559 -> Text -> IO ()
recordSignerTransactionRetry cfg conn tx =
  recordSignerReadinessFailure cfg conn (transactionMaximumCost tx)

recordSignerReadinessFailure :: LiquidationWorkerConfig -> Connection -> Integer -> Text -> IO ()
recordSignerReadinessFailure cfg conn requiredBalance err = do
  recordPerpsLiquidationSignerRetry
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    requiredBalance
    err
  logErrorEvery
    signerTransactionRetrySeconds
    "liquidation_signer_transaction_unready"
    "Liquidation worker paused new signer transaction attempts"
    ( workerLogFields cfg
        <> [ field "required_balance_wei" $ show requiredBalance
           , field "retry_seconds" signerTransactionRetrySeconds
           , field "error" err
           ]
    )

signerRetryLogFields :: LiquidationWorkerConfig -> PerpsLiquidationSignerRetryRow -> [LogField]
signerRetryLogFields cfg retry =
  workerLogFields cfg
    <> [ field "required_balance_wei" $ maybe "unknown" (T.pack . show) $ plrsrRequiredBalance retry
       , field "retry_recorded_at" $ plrsrRecordedAt retry
       , field "error" $ plrsrError retry
       ]

-- | Skip live-balance readiness entirely in dry-run mode. In live mode, make
-- startup fail closed when the signer balance cannot be read or is zero.
checkLiveSignerBalance
  :: Bool
  -> IO (Either RpcError Integer)
  -> IO (Either Text (Maybe Integer))
checkLiveSignerBalance dryRun fetchBalance
  | dryRun = pure $ Right Nothing
  | otherwise =
      fetchBalance >>= \case
        Left err ->
          pure $ Left $ "could not read liquidation signer balance: " <> rpcErrorText err
        Right balance
          | balance <= 0 -> pure $ Left "liquidation signer has zero ETH balance"
          | otherwise -> pure $ Right $ Just balance

transactionMaximumCost :: Tx1559 -> Integer
transactionMaximumCost tx =
  txValue tx + txGasLimit tx * txMaxFeePerGas tx

pendingCandidateMaximumCost :: PerpsLiquidationCandidateRow -> Maybe Integer
pendingCandidateMaximumCost candidate = do
  value <- plcrPendingValue candidate
  gasLimit <- plcrPendingGasLimit candidate
  maxFee <- plcrPendingMaxFeePerGas candidate
  pure $ value + gasLimit * maxFee

canAffordTransaction :: Integer -> Tx1559 -> Bool
canAffordTransaction balance tx = balance >= transactionMaximumCost tx

checkTransactionAffordability :: EthClient -> Text -> Tx1559 -> IO (Either Text Integer)
checkTransactionAffordability client signer tx =
  ethGetBalance client signer >>= \case
    Left err ->
      pure $ Left $ "could not recheck liquidation signer balance: " <> rpcErrorText err
    Right balance
      | canAffordTransaction balance tx -> pure $ Right balance
      | otherwise ->
          pure $
            Left $
              "liquidation signer balance "
                <> T.pack (show balance)
                <> " wei is below the transaction maximum cost "
                <> T.pack (show $ transactionMaximumCost tx)
                <> " wei"

rpcErrorText :: RpcError -> Text
rpcErrorText = \case
  RpcHttpError err -> "RPC HTTP error: " <> err
  RpcJsonError err -> "RPC JSON error: " <> err
  RpcNodeError code message mData ->
    "RPC node error "
      <> T.pack (show code)
      <> ": "
      <> message
      <> maybe "" ("; data: " <>) mData

normalizeAddress :: Text -> Text
normalizeAddress = T.toLower . T.strip

strip0x :: Text -> Text
strip0x value =
  fromMaybe value $ T.stripPrefix "0x" value
