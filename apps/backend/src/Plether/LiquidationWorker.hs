module Plether.LiquidationWorker
  ( LiquidationWorkerMode (..)
  , LiquidationWorkerConfig (..)
  , loadLiquidationWorkerConfig
  , runLiquidationWorker
  , decodeCachedPythPayload
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
  , LiquidationRiskGlobals (..)
  , LiquidationBasketComponent (..)
  , PythStoredPrice (..)
  , liquidationRiskCalls
  , decodeLiquidationRiskGlobals
  , liquidationSnapshotCalls
  , decodeLiquidationSnapshotResults
  , selectLiquidationSimulationCandidates
  , decodeCachedLiquidationComponents
  , pythStoredPriceCalls
  , decodePythStoredPriceResults
  , mergeLiquidationBasketComponents
  , validateMergedLiquidationBasket
  , freshLiquidationRiskInputsFromCache
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson
  ( FromJSON (..)
  , Result (..)
  , Value (..)
  , fromJSON
  , withObject
  , (.:)
  )
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.List (nub, sort, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( BasketSnapshotRow (..)
  , PerpsLiquidationCandidateRow (..)
  , PerpsLiquidationRejectedPayloadRow (..)
  , PerpsLiquidationSignerRetryRow (..)
  , PythUpdatePayloadRow (..)
  , clearPerpsLiquidationCandidatePending
  , clearPerpsLiquidationRejectedPayload
  , clearPerpsLiquidationSignerRetry
  , deletePerpsLiquidationCandidate
  , getLatestBasketSnapshot
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
import Plether.Ethereum.Abi (decodeBool, decodeInt256, decodeUint256, encodeCall, encodeUint256, keccak256)
import Plether.Ethereum.Client (EthClient, RpcError (..), ethBlockNumber)
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
  , ethEstimateGas
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
import Plether.Pyth.Basket (invertPythPrice, normalizeFeedId, normalizePythPrice)
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
  }
  deriving stock (Show)

loadLiquidationWorkerConfig :: Config -> Text -> IO LiquidationWorkerConfig
loadLiquidationWorkerConfig cfg privateKey = do
  pollSeconds <- readEnv "LIQUIDATION_WORKER_POLL_SECONDS" 600
  scanBatchSize <- readEnv "LIQUIDATION_WORKER_SCAN_BATCH_SIZE" 1_000
  multicallSize <- readEnv "LIQUIDATION_WORKER_MULTICALL_SIZE" 10
  executionBatchSize <- readEnv "LIQUIDATION_WORKER_EXECUTION_BATCH_SIZE" 20
  indexerStartBlock <- readEnv "LIQUIDATION_WORKER_START_BLOCK" (cfgPerpsIndexerStartBlock cfg)
  indexerConfirmations <- readEnv "LIQUIDATION_WORKER_CONFIRMATIONS" 1
  indexerBatchSize <- readEnv "LIQUIDATION_WORKER_INDEX_BATCH_SIZE" 5_000
  indexerOverlapBlocks <- readEnv "LIQUIDATION_WORKER_REORG_OVERLAP_BLOCKS" 12
  pendingReplacementSeconds <- readEnv "LIQUIDATION_WORKER_PENDING_REPLACEMENT_SECONDS" 120
  gasBufferBps <- readEnv "LIQUIDATION_WORKER_GAS_BUFFER_BPS" (cfgKeeperGasBufferBps cfg)
  feeBufferBps <- readEnv "LIQUIDATION_WORKER_FEE_BUFFER_BPS" (cfgKeeperFeeBufferBps cfg)
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
      }

readEnv :: (Read a) => String -> a -> IO a
readEnv name fallback = do
  value <- lookupEnv name
  pure $ fromMaybe fallback (value >>= readMaybe)

-- | Inputs needed to conservatively reproduce the liquidation check against
-- the exact Pyth payload the worker will submit. Prices and basket confidence
-- use the protocol's 8-decimal price scale.
data FreshLiquidationRiskInputs = FreshLiquidationRiskInputs
  { flriNeutralPrice :: Integer
  , flriBasketConfidence :: Integer
  , flriCapPrice :: Integer
  , flriRequiredMarginBps :: Integer
  , flriAdverseConfidenceMultiplierBps :: Integer
  , flriRiskBufferBps :: Integer
  }
  deriving stock (Show, Eq)

data LiquidationRiskGlobals = LiquidationRiskGlobals
  { lrgCapPrice :: Integer
  , lrgRequiredMarginBps :: Integer
  , lrgAdverseConfidenceMultiplierBps :: Integer
  , lrgPythContract :: Text
  , lrgMaxStaleness :: Integer
  , lrgMaxConfidenceRatioBps :: Integer
  , lrgBlockTimestamp :: Integer
  , lrgLastMarkTime :: Integer
  }
  deriving stock (Show, Eq)

data LiquidationBasketComponent = LiquidationBasketComponent
  { lbcFeedId :: Text
  , lbcPrice :: Integer
  , lbcRawPrice :: Integer
  , lbcConfidence :: Integer
  , lbcExponent :: Int
  , lbcPublishTime :: Integer
  , lbcInverted :: Bool
  , lbcWeightBps :: Integer
  , lbcBasePrice :: Integer
  }
  deriving stock (Show, Eq)

instance FromJSON LiquidationBasketComponent where
  parseJSON = withObject "LiquidationBasketComponent" $ \value ->
    LiquidationBasketComponent
      <$> value .: "feedId"
      <*> (value .: "price" >>= parseIntegerValue)
      <*> (value .: "rawPrice" >>= parseIntegerValue)
      <*> (value .: "confidence" >>= parseIntegerValue)
      <*> (value .: "exponent" >>= parseIntValue)
      <*> (value .: "publishTime" >>= parseIntegerValue)
      <*> value .: "inverted"
      <*> (value .: "weightBps" >>= parseIntegerValue)
      <*> (value .: "basePrice" >>= parseIntegerValue)

data PythStoredPrice = PythStoredPrice
  { pspPrice :: Integer
  , pspConfidence :: Integer
  , pspExponent :: Int
  , pspPublishTime :: Integer
  }
  deriving stock (Show, Eq)

parseIntegerValue :: Value -> Parser Integer
parseIntegerValue = \case
  String txt ->
    case readMaybe $ T.unpack txt of
      Just value -> pure value
      Nothing -> fail $ "expected integer string, got " <> T.unpack txt
  Number number ->
    case floatingOrInteger number :: Either Double Integer of
      Right value -> pure value
      Left _ -> fail "expected integer number"
  value -> fail $ "expected integer, got " <> show value

parseIntValue :: Value -> Parser Int
parseIntValue value = do
  integer <- parseIntegerValue value
  if integer < toInteger (minBound :: Int) || integer > toInteger (maxBound :: Int)
    then fail "integer exceeds Int bounds"
    else pure $ fromInteger integer

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
        -- The exact-block lens is a useful fallback when fresh-price inputs
        -- cannot be reconstructed. Once those inputs are available, however,
        -- the submitted Pyth price is authoritative and a stale stored-mark
        -- flag must not create a false-positive gas estimate.
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
  | flriNeutralPrice <= 0 = Left "Fresh liquidation basket price must be positive"
  | flriBasketConfidence < 0 = Left "Fresh liquidation basket confidence cannot be negative"
  | flriCapPrice <= 0 = Left "Liquidation price cap must be positive"
  | flriRequiredMarginBps <= 0 = Left "Liquidation margin requirement must be positive"
  | flriAdverseConfidenceMultiplierBps < 0 = Left "Adverse confidence multiplier cannot be negative"
  | flriRiskBufferBps < 0 = Left "Liquidation risk buffer cannot be negative"
  | otherwise = Right $ freshEquity <= maintenanceRequirement + riskBuffer
  where
    neutralPrice = min flriNeutralPrice flriCapPrice
    confidenceShift =
      flriBasketConfidence
        * flriAdverseConfidenceMultiplierBps
        `div` basisPointScale
    adversePrice
      | alsSide snapshot == 0 = min flriCapPrice $ neutralPrice + confidenceShift
      | otherwise = max 0 $ neutralPrice - confidenceShift
    priceDifference = abs $ adversePrice - alsEntryPrice snapshot
    unsignedPnl = alsSize snapshot * priceDifference `div` tokenPriceScale
    positionProfits
      | alsSide snapshot == 0 = adversePrice <= alsEntryPrice snapshot
      | otherwise = adversePrice >= alsEntryPrice snapshot
    signedPnl
      | positionProfits = unsignedPnl
      | otherwise = negate unsignedPnl
    baseEquity = alsNetEquityUsdc snapshot - alsUnrealizedPnlUsdc snapshot
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

liquidationRiskCalls :: LiquidationWorkerConfig -> [Multicall.Call]
liquidationRiskCalls cfg =
  [ riskCall (lwcCfdEngine cfg) "riskParams()"
  , riskCall (lwcCfdEngine cfg) "CAP_PRICE()"
  , riskCall (lwcPletherOracle cfg) "adverseConfidenceMultiplierBps()"
  , Multicall.Call
      { Multicall.callTarget = lwcPletherOracle cfg
      , Multicall.callAllowFailure = True
      , Multicall.callCalldata = Perps.pythCall
      }
  , riskCall (lwcPletherOracle cfg) "isOracleFrozen()"
  , riskCall (lwcPletherOracle cfg) "liquidationStalenessLimit()"
  , riskCall (lwcPletherOracle cfg) "basketMaxConfidenceRatioBps()"
  , riskCall (lwcCfdEngine cfg) "fadMaxStaleness()"
  , riskCall (lwcCfdEngine cfg) "isFadWindow()"
  , riskCall (lwcCfdEngine cfg) "lastMarkTime()"
  , riskCall Multicall.multicallAddress "getCurrentBlockTimestamp()"
  ]
  where
    riskCall target signature =
      Multicall.Call
        { Multicall.callTarget = target
        , Multicall.callAllowFailure = True
        , Multicall.callCalldata = encodeCall signature []
        }

decodeLiquidationRiskGlobals
  :: [Multicall.CallResult]
  -> Either Text LiquidationRiskGlobals
decodeLiquidationRiskGlobals results =
  case results of
    [ riskParamsResult
      , capPriceResult
      , multiplierResult
      , pythResult
      , oracleFrozenResult
      , liquidationStalenessResult
      , maxConfidenceRatioResult
      , fadMaxStalenessResult
      , isFadWindowResult
      , lastMarkTimeResult
      , blockTimestampResult
      ] -> do
      riskParams <- successfulResult "riskParams()" (8 * 32) riskParamsResult
      capPriceBytes <- successfulResult "CAP_PRICE()" 32 capPriceResult
      multiplierBytes <-
        successfulResult
          "adverseConfidenceMultiplierBps()"
          32
          multiplierResult
      pythBytes <- successfulResult "pyth()" 32 pythResult
      oracleFrozenBytes <- successfulResult "isOracleFrozen()" 32 oracleFrozenResult
      liquidationStalenessBytes <-
        successfulResult "liquidationStalenessLimit()" 32 liquidationStalenessResult
      maxConfidenceRatioBytes <-
        successfulResult "basketMaxConfidenceRatioBps()" 32 maxConfidenceRatioResult
      fadMaxStalenessBytes <-
        successfulResult "fadMaxStaleness()" 32 fadMaxStalenessResult
      isFadWindowBytes <- successfulResult "isFadWindow()" 32 isFadWindowResult
      lastMarkTimeBytes <- successfulResult "lastMarkTime()" 32 lastMarkTimeResult
      blockTimestampBytes <-
        successfulResult "getCurrentBlockTimestamp()" 32 blockTimestampResult
      pythContract <-
        case Perps.decodePythContract pythBytes of
          Left err -> Left $ "pyth() could not be decoded: " <> rpcErrorText err
          Right address -> Right address
      let maintenanceMarginBps = decodeWord riskParams 2
          fadMarginBps = decodeWord riskParams 4
          capPrice = decodeWord capPriceBytes 0
          multiplierBps = decodeWord multiplierBytes 0
          oracleFrozen = decodeBool oracleFrozenBytes
          liquidationStaleness = decodeWord liquidationStalenessBytes 0
          fadMaxStaleness = decodeWord fadMaxStalenessBytes 0
          maxStaleness = if oracleFrozen then fadMaxStaleness else liquidationStaleness
          maxConfidenceRatioBps = decodeWord maxConfidenceRatioBytes 0
          isFadWindow = decodeBool isFadWindowBytes
          requiredMarginBps = if isFadWindow then fadMarginBps else maintenanceMarginBps
          lastMarkTime = decodeWord lastMarkTimeBytes 0
          blockTimestamp = decodeWord blockTimestampBytes 0
      if maintenanceMarginBps <= 0 || fadMarginBps <= 0 || capPrice <= 0 || maxStaleness <= 0 || blockTimestamp <= 0
        then Left "Liquidation risk globals contained a non-positive margin rate, price cap, freshness limit, or block timestamp"
        else
          Right
            LiquidationRiskGlobals
              { lrgCapPrice = capPrice
              , lrgRequiredMarginBps = requiredMarginBps
              , lrgAdverseConfidenceMultiplierBps = multiplierBps
              , lrgPythContract = pythContract
              , lrgMaxStaleness = maxStaleness
              , lrgMaxConfidenceRatioBps = maxConfidenceRatioBps
              , lrgBlockTimestamp = blockTimestamp
              , lrgLastMarkTime = lastMarkTime
              }
    _ ->
      Left $
        "Expected 11 liquidation risk results, received " <> tshow (length results)
  where
    successfulResult label minimumLength result
      | not $ Multicall.resultSuccess result = Left $ label <> " subcall failed"
      | BS.length (Multicall.resultData result) < minimumLength =
          Left $
            label
              <> " returned "
              <> tshow (BS.length $ Multicall.resultData result)
              <> " bytes; expected at least "
              <> tshow minimumLength
      | otherwise = Right $ Multicall.resultData result

    decodeWord bytes index =
      decodeUint256 $ BS.take 32 $ BS.drop (index * 32) bytes

freshLiquidationRiskInputs
  :: PythUpdatePayloadRow
  -> BasketSnapshotRow
  -> LiquidationRiskGlobals
  -> Either Text FreshLiquidationRiskInputs
freshLiquidationRiskInputs payload basket globals = do
  components <- decodeCachedLiquidationComponents payload basket
  freshLiquidationRiskInputsFromComponents components globals

decodeCachedLiquidationComponents
  :: PythUpdatePayloadRow
  -> BasketSnapshotRow
  -> Either Text [LiquidationBasketComponent]
decodeCachedLiquidationComponents payload basket = do
  payloadPublishTimes <- decodeJson "Pyth payload publish times" $ puprPublishTimes payload
  components <- decodeJson "basket components" $ bsrComponents basket
  whenEither (null components) "Basket snapshot did not contain any components"
  whenEither
    (length components /= length payloadPublishTimes)
    "Basket component count did not match the Pyth payload publish-time count"
  let componentPublishTimes = map lbcPublishTime components
  whenEither
    (sort componentPublishTimes /= sort payloadPublishTimes)
    "Basket component publish times did not match the Pyth payload"
  whenEither
    (minimum componentPublishTimes /= puprMinPublishTime payload)
    "Basket component minimum publish time did not match the Pyth payload"
  whenEither
    (maximum componentPublishTimes /= puprMaxPublishTime payload)
    "Basket component maximum publish time did not match the Pyth payload"
  feedIds <- traverse decodeLiquidationFeedId components
  whenEither
    (length (nub feedIds) /= length feedIds)
    "Basket snapshot contained duplicate Pyth feed IDs"
  contributions <- traverse basketComponentContribution components
  let reconstructedPrice = sum contributions
  whenEither
    (reconstructedPrice /= bsrBasketPrice basket)
    "Basket components did not reconstruct the cached basket price"
  pure components
  where
    decodeJson :: (FromJSON value) => Text -> Value -> Either Text value
    decodeJson label value =
      case fromJSON value of
        Error err -> Left $ label <> " could not be decoded: " <> T.pack err
        Success decoded -> Right decoded

freshLiquidationRiskInputsFromComponents
  :: [LiquidationBasketComponent]
  -> LiquidationRiskGlobals
  -> Either Text FreshLiquidationRiskInputs
freshLiquidationRiskInputsFromComponents components globals = do
  whenEither (null components) "Liquidation basket did not contain any components"
  contributions <- traverse basketComponentContribution components
  confidenceContributions <-
    traverse (uncurry basketComponentConfidenceContribution) $ zip components contributions
  let reconstructedPrice = sum contributions
      aggregateConfidence = sum confidenceContributions
  whenEither (reconstructedPrice <= 0) "Liquidation basket price must be positive"
  pure
    FreshLiquidationRiskInputs
      { flriNeutralPrice = reconstructedPrice
      , flriBasketConfidence = aggregateConfidence
      , flriCapPrice = lrgCapPrice globals
      , flriRequiredMarginBps = lrgRequiredMarginBps globals
      , flriAdverseConfidenceMultiplierBps = lrgAdverseConfidenceMultiplierBps globals
      , flriRiskBufferBps = liquidationRiskBufferBps
      }

decodeLiquidationFeedId :: LiquidationBasketComponent -> Either Text ByteString
decodeLiquidationFeedId component = do
  let feedId = T.strip $ lbcFeedId component
  decoded <-
    case B16.decode (TE.encodeUtf8 $ normalizeFeedId feedId) of
      Left err -> Left $ "Invalid Pyth feed ID " <> feedId <> ": " <> T.pack err
      Right bytes -> Right bytes
  whenEither
    (BS.length decoded /= 32)
    ("Pyth feed ID must be exactly 32 bytes: " <> feedId)
  pure decoded

pythStoredPriceCalls
  :: Text
  -> [LiquidationBasketComponent]
  -> Either Text [Multicall.Call]
pythStoredPriceCalls pythContract components =
  forM components $ \component -> do
    feedId <- decodeLiquidationFeedId component
    pure
      Multicall.Call
        { Multicall.callTarget = pythContract
        , Multicall.callAllowFailure = True
        , Multicall.callCalldata = encodeCall "getPriceUnsafe(bytes32)" [feedId]
        }

decodePythStoredPriceResults
  :: Int
  -> [Multicall.CallResult]
  -> Either Text [PythStoredPrice]
decodePythStoredPriceResults expectedCount results
  | length results /= expectedCount =
      Left $
        "Expected "
          <> tshow expectedCount
          <> " Pyth stored-price results, received "
          <> tshow (length results)
  | otherwise = traverse decodeResult $ zip [0 :: Int ..] results
  where
    decodeResult (index, result)
      | not $ Multicall.resultSuccess result =
          Left $ "Pyth stored-price subcall " <> tshow index <> " failed"
      | BS.length bytes /= 4 * 32 =
          Left $
            "Pyth stored-price subcall "
              <> tshow index
              <> " returned "
              <> tshow (BS.length bytes)
              <> " bytes; expected 128"
      | rawPrice < 1 || rawPrice > maxInt64 =
          Left $ "Pyth stored-price subcall " <> tshow index <> " returned an invalid int64 price"
      | confidence > maxUint64 =
          Left $ "Pyth stored-price subcall " <> tshow index <> " returned an invalid uint64 confidence"
      | exponentInteger < minInt32 || exponentInteger > maxInt32 =
          Left $ "Pyth stored-price subcall " <> tshow index <> " returned an invalid int32 exponent"
      | otherwise =
          Right
            PythStoredPrice
              { pspPrice = rawPrice
              , pspConfidence = confidence
              , pspExponent = fromInteger exponentInteger
              , pspPublishTime = publishTime
              }
      where
        bytes = Multicall.resultData result
        wordAt offset = BS.take 32 $ BS.drop offset bytes
        rawPrice = decodeInt256 $ wordAt 0
        confidence = decodeUint256 $ wordAt 32
        exponentInteger = decodeInt256 $ wordAt 64
        publishTime = decodeUint256 $ wordAt 96

    minInt32 = negate $ 2 ^ (31 :: Int)
    maxInt32 = 2 ^ (31 :: Int) - 1
    maxInt64 = 2 ^ (63 :: Int) - 1
    maxUint64 = 2 ^ (64 :: Int) - 1

mergeLiquidationBasketComponents
  :: [LiquidationBasketComponent]
  -> [PythStoredPrice]
  -> Either Text [LiquidationBasketComponent]
mergeLiquidationBasketComponents components storedPrices = do
  whenEither
    (length components /= length storedPrices)
    "Pyth stored-price count did not match the liquidation basket"
  sequence $ zipWith mergeComponent components storedPrices
  where
    -- Pyth's updatePriceFeeds keeps the existing value when its publish time is
    -- equal to or newer than the submitted update. Reproduce that component by
    -- component; comparing only whole-basket snapshots misses mixed states.
    mergeComponent component stored
      | pspPublishTime stored < lbcPublishTime component = Right component
      | otherwise = do
          normalizedPrice <-
            if lbcInverted component
              then invertPythPrice (pspPrice stored) (pspExponent stored)
              else normalizePythPrice (pspPrice stored) (pspExponent stored)
          pure
            component
              { lbcPrice = normalizedPrice
              , lbcRawPrice = pspPrice stored
              , lbcConfidence = pspConfidence stored
              , lbcExponent = pspExponent stored
              , lbcPublishTime = pspPublishTime stored
              }

validateMergedLiquidationBasket
  :: LiquidationRiskGlobals
  -> [LiquidationBasketComponent]
  -> Either Text ()
validateMergedLiquidationBasket globals components = do
  whenEither (null components) "Liquidation basket did not contain any components"
  let publishTimes = map lbcPublishTime components
      minPublishTime = minimum publishTimes
      maxPublishTime = maximum publishTimes
      blockTimestamp = lrgBlockTimestamp globals
      allowedAge = lrgMaxStaleness globals
  whenEither (minPublishTime <= 0) "Liquidation basket contained a non-positive publish time"
  whenEither
    (maxPublishTime > blockTimestamp)
    "Merged Pyth basket contained a future publish time"
  whenEither
    (blockTimestamp - minPublishTime > allowedAge)
    "Merged Pyth basket was outside the liquidation freshness window"
  whenEither
    (maxPublishTime - minPublishTime > allowedAge)
    "Merged Pyth basket exceeded the liquidation publish-time divergence window"
  whenEither
    (minPublishTime < lrgLastMarkTime globals)
    "Merged Pyth basket predated the engine's last mark"
  forM_ components $ \component -> do
    whenEither
      (lbcRawPrice component <= 0)
      "Merged Pyth basket contained a non-positive component price"
    whenEither
      ( lbcConfidence component * basisPointScale
          > lbcRawPrice component * lrgMaxConfidenceRatioBps globals
      )
      "Merged Pyth basket contained a component with confidence wider than the oracle policy"

freshLiquidationRiskInputsFromCache
  :: PythUpdatePayloadRow
  -> BasketSnapshotRow
  -> Integer -- price cap
  -> Integer -- conservative maintenance margin bps
  -> Integer -- adverse confidence multiplier bps
  -> Either Text FreshLiquidationRiskInputs
freshLiquidationRiskInputsFromCache payload basket capPrice requiredMarginBps multiplierBps =
  freshLiquidationRiskInputs
    payload
    basket
    LiquidationRiskGlobals
      { lrgCapPrice = capPrice
      , lrgRequiredMarginBps = requiredMarginBps
      , lrgAdverseConfidenceMultiplierBps = multiplierBps
      , lrgPythContract = ""
      , lrgMaxStaleness = 0
      , lrgMaxConfidenceRatioBps = 0
      , lrgBlockTimestamp = 0
      , lrgLastMarkTime = 0
      }

basketComponentContribution
  :: LiquidationBasketComponent
  -> Either Text Integer
basketComponentContribution component
  | lbcPrice component <= 0 = Left "Basket component price must be positive"
  | lbcWeightBps component <= 0 = Left "Basket component weight must be positive"
  | lbcBasePrice component <= 0 = Left "Basket component base price must be positive"
  | otherwise =
      Right $
        lbcPrice component
          * lbcWeightBps component
          * basisPointScale
          `div` lbcBasePrice component

basketComponentConfidenceContribution
  :: LiquidationBasketComponent
  -> Integer
  -> Either Text Integer
basketComponentConfidenceContribution component contribution
  | lbcConfidence component < 0 = Left "Basket component confidence cannot be negative"
  | lbcRawPrice component <= 0 = Left "Basket component raw price must be positive"
  | otherwise =
      Right $ contribution * lbcConfidence component `div` lbcRawPrice component

whenEither :: Bool -> Text -> Either Text ()
whenEither condition err
  | condition = Left err
  | otherwise = Right ()

liquidationRiskBufferBps :: Integer
liquidationRiskBufferBps = 5

loadFreshLiquidationRiskInputs
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> Integer
  -> PythUpdatePayloadRow
  -> IO (Either Text FreshLiquidationRiskInputs)
loadFreshLiquidationRiskInputs cfg conn client blockNumber payload = do
  basket <- getLatestBasketSnapshot conn
  case basket of
    Nothing -> pure $ Left "No basket snapshot was available for the cached Pyth payload"
    Just snapshot ->
      case decodeCachedLiquidationComponents payload snapshot of
        Left err -> pure $ Left err
        Right cachedComponents -> do
          globalsResult <-
            Multicall.multicallAtBlock client (liquidationRiskCalls cfg) blockNumber
          case firstRpcError "risk Multicall failed" globalsResult >>= decodeLiquidationRiskGlobals of
            Left err -> pure $ Left err
            Right globals ->
              case pythStoredPriceCalls (lrgPythContract globals) cachedComponents of
                Left err -> pure $ Left err
                Right calls -> do
                  storedResult <- Multicall.multicallAtBlock client calls blockNumber
                  pure $ do
                    storedCallResults <- firstRpcError "Pyth stored-price Multicall failed" storedResult
                    storedPrices <-
                      decodePythStoredPriceResults
                        (length cachedComponents)
                        storedCallResults
                    mergedComponents <-
                      mergeLiquidationBasketComponents cachedComponents storedPrices
                    validateMergedLiquidationBasket globals mergedComponents
                    freshLiquidationRiskInputsFromComponents mergedComponents globals

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
                case mode of
                  LiquidationWorkerOnce -> runIteration cfg conn client workerAddress dryRun
                  LiquidationWorkerLoop -> loop conn workerAddress
  where
    loop conn workerAddress = do
      runIteration cfg conn client workerAddress dryRun
      waitForNextSweep conn workerAddress $ lwcPollSeconds cfg
      loop conn workerAddress

    -- Discovery and health reads run every ten minutes, but a transaction that
    -- was already submitted still needs timely receipt, rebroadcast, and nonce
    -- reconciliation. An empty pending check is database-only and makes no RPC.
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
      candidates <-
        getPerpsLiquidationCandidates
          conn
          (lwcChainId cfg)
          (lwcCfdEngine cfg)
          (lwcScanBatchSize cfg)
      unless (null candidates) $ do
        blockResult <- ethBlockNumber client
        case blockResult of
          Left err ->
            logWarnEvery
              60
              "liquidation_snapshot_block_fetch_failed"
              "Liquidation worker could not resolve an exact block for its batched position reads"
              ( workerLogFields cfg
                  <> [ field "candidate_count" $ length candidates
                     , field "error" $ rpcErrorText err
                     ]
              )
          Right snapshotBlock -> processCandidatesAtBlock snapshotBlock candidates

    processCandidatesAtBlock snapshotBlock candidates = do
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

      mPayload <- getLatestPythUpdatePayload conn
      case mPayload of
        Nothing -> do
          recordUnclassifiedOpenCandidates
            snapshots
            "No cached latest Pyth payload was available"
          logWarnEvery
            60
            "liquidation_pyth_payload_missing"
            "Liquidation scan is waiting for a cached latest Pyth payload"
            (workerLogFields cfg <> [field "candidate_count" $ length candidates])
        Just payload ->
          case decodeCachedPythPayload payload of
            Left err -> do
              recordUnclassifiedOpenCandidates
                snapshots
                ("Latest cached Pyth payload could not be decoded: " <> err)
              logErrorEvery
                60
                "liquidation_pyth_payload_invalid"
                "Latest cached Pyth payload could not be decoded"
                (workerLogFields cfg <> [field "error" err])
            Right (_, updateData) -> do
              riskResult <-
                loadFreshLiquidationRiskInputs
                  cfg
                  conn
                  client
                  snapshotBlock
                  payload
              let riskInputs = either (const Nothing) Just riskResult
                  classified =
                    [ (candidate, snapshotResult, liquidationRiskDecision riskInputs snapshotResult)
                    | (candidate, snapshotResult) <- snapshots
                    ]
                  simulationCandidates =
                    [ candidate
                    | (candidate, _, LiquidationPositionRisky) <- classified
                    ]
              case riskResult of
                Left err ->
                  logWarnEvery
                    60
                    "liquidation_risk_inputs_unavailable"
                    "Liquidation risk inputs were unavailable; only exact-block stored-risk positions will be simulated"
                    ( workerLogFields cfg
                        <> [ field "candidate_count" $ length candidates
                           , field "snapshot_block" snapshotBlock
                           , field "error" err
                           ]
                    )
                Right _ -> pure ()

              -- Rotate every classified healthy or unknown account. The DB
              -- query orders by last_checked_at, so leaving these untouched
              -- would permanently starve candidates beyond the first page.
              -- Unknown state is retained and surfaced as an error, but never
              -- spends an eth_estimateGas call.
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
                    <> [ field "candidate_count" $ length candidates
                       , field "snapshot_block" snapshotBlock
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
                processClassifiedPayload
                  simulationCandidates
                  updateData

    recordUnclassifiedOpenCandidates snapshots reason =
      forM_ snapshots $ \(candidate, snapshotResult) ->
        case snapshotResult of
          Right snapshot
            | alsHasPosition snapshot || alsSize snapshot /= 0 ->
                recordCandidateError
                  cfg
                  conn
                  candidate
                  "risk_classification"
                  reason
          _ -> pure ()

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

    processClassifiedPayload candidates updateData = do
      let payloadKey =
            liquidationPayloadFingerprint
              (lwcPletherOracle cfg)
              (lwcOrderRouter cfg)
              updateData
      rejectedPayload <-
        getPerpsLiquidationRejectedPayload
          conn
          (lwcChainId cfg)
          (lwcCfdEngine cfg)
      case
          liquidationPayloadCircuitDecision
            (plrprPayloadKey <$> rejectedPayload)
            payloadKey
        of
        SuppressRejectedLiquidationPayload ->
          case rejectedPayload of
            Just rejected ->
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
            Nothing -> processPayload candidates payloadKey updateData
        ClearRejectedLiquidationPayload -> do
          clearPerpsLiquidationRejectedPayload
            conn
            (lwcChainId cfg)
            (lwcCfdEngine cfg)
          logInfo
            "liquidation_pyth_payload_changed"
            "Liquidation scan resumed with a new Pyth payload"
            (workerLogFields cfg <> [field "payload_key" payloadKey])
          processPayload candidates payloadKey updateData
        ProcessLiquidationPayload ->
          processPayload candidates payloadKey updateData

    processPayload candidates payloadKey updateData = do
      feeResult <- Perps.getUpdateFee client (lwcPletherOracle cfg) updateData
      case feeResult of
        Left err ->
          logWarnEvery
            60
            "liquidation_update_fee_fetch_failed"
            "Liquidation worker could not fetch the Pyth update fee"
            ( workerLogFields cfg
                <> [ field "candidate_count" $ length candidates
                   , field "payload_key" payloadKey
                   , field "error" $ rpcErrorText err
                   ]
            )
        Right updateFee ->
          processExecutionBatches
            (chunksOf (lwcExecutionBatchSize cfg) candidates)
            payloadKey
            updateData
            updateFee

    processExecutionBatches [] _ _ _ = pure ()
    processExecutionBatches (batch : rest) payloadKey updateData updateFee = do
      canContinue <-
        processLiquidationBatch cfg conn client workerAddress dryRun payloadKey updateData updateFee batch
      when canContinue $ processExecutionBatches rest payloadKey updateData updateFee

processLiquidationBatch
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> Text
  -> Bool
  -> Text
  -> [ByteString]
  -> Integer
  -> [PerpsLiquidationCandidateRow]
  -> IO Bool
processLiquidationBatch cfg conn client workerAddress dryRun payloadKey updateData updateFee candidates = do
  let accounts = map plcrAccount candidates
      callData = Perps.executeLiquidationBatchCall accounts updateData
  gasResult <- ethEstimateGas client workerAddress (lwcOrderRouter cfg) updateFee callData
  case gasResult of
    Left err
      | Just selectorText <- payloadGlobalSimulationRevertSelector err -> do
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
          pure False
      | otherwise -> do
          forM_ candidates $ \candidate ->
            recordCandidateError cfg conn candidate "batch_simulation" $
              "liquidation batch simulation failed: " <> rpcErrorText err
          pure False
    Right estimatedGas -> do
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
          pure True
        else do
          prepared <-
            prepareLiquidationTransaction cfg client workerAddress estimatedGas updateFee callData
          case prepared of
            Left err -> do
              forM_ candidates $ \candidate ->
                recordCandidateError cfg conn candidate "batch_transaction_prepare" err
              pure False
            Right (tx, signed) -> do
              affordabilityResult <- checkTransactionAffordability client workerAddress tx
              case affordabilityResult of
                Left err -> do
                  recordSignerTransactionRetry cfg conn tx err
                  forM_ candidates $ \candidate ->
                    recordCandidateError cfg conn candidate "batch_transaction_affordability" err
                  pure False
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
                      pure False
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
                          pure False
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
                              pure False
                            Right receipt ->
                              handleLiquidationBatchReceipt cfg conn client pendingCandidates receipt

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
          gasLimit = liquidationTransactionGasLimit (lwcGasBufferBps cfg) estimatedGas
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
  latestResult <- ethBlockNumber client
  case latestResult of
    Left err -> do
      forM_ candidates $ \candidate ->
        recordCandidateErrorWith cfg conn candidate "batch_confirmation_depth_read" (receiptLogFields receipt) $
          "could not verify batch confirmation depth: " <> rpcErrorText err
      pure False
    Right latestBlock
      | latestBlock < receiptBlockNumber receipt + fromIntegral (lwcIndexerConfirmations cfg) -> do
          logInfoEvery
            60
            "liquidation_batch_receipt_confirmations_pending"
            "Liquidation batch receipt is waiting for confirmation depth"
            ( workerLogFields cfg
                <> [ field "candidate_count" $ length candidates
                   , field "transaction_hash" $ receiptTxHash receipt
                   , field "receipt_block_number" $ receiptBlockNumber receipt
                   , field "chain_head_block" latestBlock
                   , field "required_confirmations" $ lwcIndexerConfirmations cfg
                   ]
            )
          pure False
      | otherwise -> handleConfirmedLiquidationBatchReceipt cfg conn client candidates latestBlock receipt

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
              outcomes <-
                withTransaction conn $
                  forM positions $ \(candidate, positionResult) ->
                    reconcileBatchItem cfg conn receipt progress candidate positionResult
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
          | otherwise -> clearAndCheck >> pure True
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
      threadDelay 2_000_000
      waitForReceipt client txHash (attempts - 1)

decodeCachedPythPayload :: PythUpdatePayloadRow -> Either Text ([Integer], [ByteString])
decodeCachedPythPayload PythUpdatePayloadRow {puprPublishTimes, puprUpdateData} = do
  publishTimes <- parseValue "publish_times" puprPublishTimes
  updateHex <- parseValue "update_data" puprUpdateData
  updateData <- traverse decodeHexUpdate updateHex
  pure (publishTimes, updateData)

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
pendingReconciliationPollSeconds = 60

-- Bound startup/outage catch-up without advancing only one 5,000-block page
-- per ten-minute health sweep. At the default page size this covers five
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
