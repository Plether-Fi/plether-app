{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plether.Handlers.InsightsOrderProtections
  ( IntentBoundsData (..)
  , OrderOutcomeMeta (..)
  , LinkedPositionProtection (..)
  , OrderProtectionsBundle (..)
  , emptyOrderProtectionsBundle
  , extractExecutionOrderId
  , parseIntentBoundsFromJson
  , decodeIntentFromRpcLog
  , parsePositionProtectionSnapshot
  , fetchOrderProtectionsBundle
  , enrichOrderProtectionsBundleFromRpc
  , buildOrderProtectionsForOrder
  , formatAllowedModes
  , priceWithinTarget
  , intentRegisteredTopicBytes
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, In (..), query)
import Database.PostgreSQL.Simple.FromField (ResultError (ConversionFailed), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field, fieldWith)
import Plether.Ethereum.Abi (keccak256)
import Plether.Ethereum.Client (EthClient)
import Plether.Ethereum.Rpc (RpcLog (..), TxReceipt (..), ethGetTransactionReceipt)
import System.Timeout (timeout)
import Text.Read (readMaybe)

uint256Max :: Integer
uint256Max = (2 :: Integer) ^ (256 :: Int) - 1

uint32Max :: Integer
uint32Max = (2 :: Integer) ^ (32 :: Int) - 1

zeroBytes32 :: Text
zeroBytes32 = "0x" <> T.replicate 64 "0"

intentRegisteredSignature :: Text
intentRegisteredSignature =
  "IntentRegistered(uint64,address,bytes32,bytes32,uint256,(bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint32,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)),(uint64,uint32,uint64,uint64))"

intentRegisteredTopicBytes :: BS.ByteString
intentRegisteredTopicBytes = keccak256 (TE.encodeUtf8 intentRegisteredSignature)

data IntentBoundsData = IntentBoundsData
  { ibdOrderId :: Integer
  , ibdSide :: Maybe Int
  , ibdIsClose :: Maybe Bool
  , ibdTargetPrice :: Maybe Integer
  , ibdSubmitBy :: Maybe Integer
  , ibdExecutionWindowSeconds :: Maybe Integer
  , ibdCommitTimestamp :: Maybe Integer
  , ibdExecutionDeadline :: Maybe Integer
  , ibdAllowedExecutionModes :: Maybe Int
  , ibdExpectedConfigHash :: Maybe Text
  , ibdExecutionBountyAssets :: Maybe Integer
  , ibdMaxExecutionBountyAssets :: Maybe Integer
  , ibdMaxExecutionNotionalAssets :: Maybe Integer
  , ibdMaxGrossAccountDebitAssets :: Maybe Integer
  , ibdMaxActionChargeAssets :: Maybe Integer
  , ibdMaxExplicitFeesAssets :: Maybe Integer
  , ibdMaxPostPositionSize :: Maybe Integer
  , ibdMinPostSettlementBalanceAssets :: Maybe Integer
  , ibdMinPostPositionEquityAssets :: Maybe Integer
  , ibdMaxPostLeverageBps :: Maybe Integer
  , ibdSource :: Text
  }
  deriving stock (Eq, Show)

data OrderOutcomeMeta = OrderOutcomeMeta
  { oomOrderId :: Integer
  , oomSide :: Maybe Int
  , oomCommitTxHash :: Maybe Text
  , oomCommitTimestamp :: Maybe Integer
  , oomTerminalTimestamp :: Maybe Integer
  , oomTerminalStatus :: Text
  , oomTerminalReason :: Maybe Text
  , oomExecutionMode :: Maybe Text
  , oomFailedConstraint :: Maybe Text
  , oomExecutionPrice :: Maybe Integer
  , oomReceipt :: Maybe Value
  }
  deriving stock (Eq, Show)

instance FromRow OrderOutcomeMeta where
  fromRow =
    OrderOutcomeMeta
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> nullableNumericIntegerField
      <*> field

data LinkedPositionProtection = LinkedPositionProtection
  { lppProtectionId :: Text
  , lppParentOrderId :: Maybe Integer
  , lppLinkedOrderId :: Maybe Text
  , lppStatus :: Text
  , lppTriggeredLeg :: Maybe Text
  , lppTakeProfitTriggerPrice :: Maybe Text
  , lppStopLossTriggerPrice :: Maybe Text
  , lppTriggerMarkPrice :: Maybe Text
  , lppTriggerBountyAssets :: Maybe Text
  , lppArmedBlock :: Maybe Text
  , lppUpdatedBlock :: Maybe Text
  , lppTxHash :: Maybe Text
  }
  deriving stock (Eq, Show)

data OrderProtectionsBundle = OrderProtectionsBundle
  { opbIntents :: Map.Map Integer IntentBoundsData
  , opbOutcomes :: Map.Map Integer OrderOutcomeMeta
  , opbPositionProtections :: Map.Map Integer LinkedPositionProtection
  }
  deriving stock (Eq, Show)

emptyOrderProtectionsBundle :: OrderProtectionsBundle
emptyOrderProtectionsBundle = OrderProtectionsBundle Map.empty Map.empty Map.empty

hasFullBounds :: IntentBoundsData -> Bool
hasFullBounds IntentBoundsData {..} =
  maybe False (const True) ibdMaxExecutionNotionalAssets

extractExecutionOrderId :: Maybe Value -> Maybe Integer
extractExecutionOrderId mVal = do
  Object obj <- mVal
  lookupInteger "orderId" obj

fetchOrderProtectionsBundle
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> [Integer]
  -> IO OrderProtectionsBundle
fetchOrderProtectionsBundle conn chainId router protectionBook rawIds = do
  let orderIds = nub rawIds
  if null orderIds
    then pure emptyOrderProtectionsBundle
    else do
      intents <- fetchOrderIntentDataMap conn chainId router orderIds
      outcomes <- fetchOrderOutcomeMetaMap conn chainId router orderIds
      protections <- fetchLinkedPositionProtectionsMap conn chainId protectionBook orderIds
      pure $ OrderProtectionsBundle intents outcomes protections

fetchOrderIntentDataMap
  :: Connection
  -> Integer
  -> Text
  -> [Integer]
  -> IO (Map.Map Integer IntentBoundsData)
fetchOrderIntentDataMap conn chainId router orderIds = do
  rows <-
    query
      conn
      "SELECT DISTINCT ON (order_id) order_id, data \
      \FROM perps_events \
      \WHERE chain_id = ? AND release_router = ? \
      \  AND event_name = 'IntentRegistered' AND order_id IN ? \
      \ORDER BY order_id, block_number DESC, log_index DESC"
      (chainId, router, In orderIds) :: IO [(Integer, Value)]
  pure $
    Map.fromList
      [ (oid, parsed)
      | (oid, payload) <- rows
      , Just parsed <- [parseIntentBoundsFromJson oid "indexed_event" payload]
      ]

fetchOrderOutcomeMetaMap
  :: Connection
  -> Integer
  -> Text
  -> [Integer]
  -> IO (Map.Map Integer OrderOutcomeMeta)
fetchOrderOutcomeMetaMap conn chainId router orderIds = do
  rows <-
    query
      conn
      "SELECT order_id, side, commit_tx_hash, commit_timestamp, terminal_timestamp, \
      \  terminal_status, terminal_reason, execution_mode, failed_constraint, \
      \  execution_price, receipt_economics \
      \FROM perps_orders \
      \WHERE chain_id = ? AND order_router = ? AND order_id IN ?"
      (chainId, router, In orderIds) :: IO [OrderOutcomeMeta]
  pure $ Map.fromList [(oomOrderId row, row) | row <- rows]

fetchLinkedPositionProtectionsMap
  :: Connection
  -> Integer
  -> Text
  -> [Integer]
  -> IO (Map.Map Integer LinkedPositionProtection)
fetchLinkedPositionProtectionsMap conn chainId protectionBook orderIds = do
  let idTexts = map (T.pack . show) orderIds
  rows <-
    query
      conn
      "SELECT DISTINCT ON (protection_id) \
      \  protection_id::TEXT, snapshot, block_number::TEXT, transaction_hash \
      \FROM perps_protection_events \
      \WHERE chain_id = ? AND book = ? AND snapshot->>'parentOrderId' IN ? \
      \ORDER BY protection_id, block_number DESC, log_index DESC"
      (chainId, protectionBook, In idTexts) :: IO [(Text, Value, Text, Text)]
  pure $
    Map.fromList
      [ (parentOid, parsed)
      | (pid, snap, blk, txHash) <- rows
      , Just parsed <- [parsePositionProtectionSnapshot pid (Just blk) (Just txHash) snap]
      , Just parentOid <- [lppParentOrderId parsed]
      ]

enrichOrderProtectionsBundleFromRpc
  :: EthClient
  -> OrderProtectionsBundle
  -> IO OrderProtectionsBundle
enrichOrderProtectionsBundleFromRpc client bundle@OrderProtectionsBundle {..} = do
  let missingCommitHashes =
        take 8 $
          nub $
            mapMaybe
              (\(oid, outcome) ->
                case Map.lookup oid opbIntents of
                  Just intent | hasFullBounds intent -> Nothing
                  _ -> oomCommitTxHash outcome
              )
              (Map.toList opbOutcomes)
  if null missingCommitHashes
    then pure bundle
    else do
      fetched <- mapConcurrently (fetchIntentsFromCommitReceipt client) missingCommitHashes
      let decodedMap = Map.fromList [(ibdOrderId item, item) | batch <- fetched, item <- batch]
      pure bundle {opbIntents = Map.union decodedMap opbIntents}

fetchIntentsFromCommitReceipt :: EthClient -> Text -> IO [IntentBoundsData]
fetchIntentsFromCommitReceipt client txHash = do
  mReceipt <- timeout 2500000 (ethGetTransactionReceipt client txHash)
  pure $ case mReceipt of
    Just (Right (Just receipt)) -> mapMaybe decodeIntentFromRpcLog (receiptLogs receipt)
    _ -> []

decodeIntentFromRpcLog :: RpcLog -> Maybe IntentBoundsData
decodeIntentFromRpcLog RpcLog {..} =
  case rpcLogTopics of
    (t0 : t1 : _ : _ : _)
      | t0 == intentRegisteredTopicBytes
          && BS.length t1 == 32
          && BS.length rpcLogData == 25 * 32 ->
          let oid = wordAtBytes t1 0
           in Just
                IntentBoundsData
                  { ibdOrderId = oid
                  , ibdSide = Just (fromInteger (wordAtBytes rpcLogData 3))
                  , ibdIsClose = Just (wordAtBytes rpcLogData 7 == 1)
                  , ibdTargetPrice = Just (wordAtBytes rpcLogData 6)
                  , ibdSubmitBy = Just (wordAtBytes rpcLogData 8)
                  , ibdExecutionWindowSeconds = Just (wordAtBytes rpcLogData 9)
                  , ibdCommitTimestamp = Just (wordAtBytes rpcLogData 23)
                  , ibdExecutionDeadline = Just (wordAtBytes rpcLogData 24)
                  , ibdAllowedExecutionModes = Just (fromInteger (wordAtBytes rpcLogData 10))
                  , ibdExpectedConfigHash = Just (hexWordAtBytes rpcLogData 11)
                  , ibdExecutionBountyAssets = Just (wordAtBytes rpcLogData 1)
                  , ibdMaxExecutionBountyAssets = Just (wordAtBytes rpcLogData 12)
                  , ibdMaxExecutionNotionalAssets = Just (wordAtBytes rpcLogData 13)
                  , ibdMaxGrossAccountDebitAssets = Just (wordAtBytes rpcLogData 14)
                  , ibdMaxActionChargeAssets = Just (wordAtBytes rpcLogData 15)
                  , ibdMaxExplicitFeesAssets = Just (wordAtBytes rpcLogData 16)
                  , ibdMaxPostPositionSize = Just (wordAtBytes rpcLogData 17)
                  , ibdMinPostSettlementBalanceAssets = Just (wordAtBytes rpcLogData 18)
                  , ibdMinPostPositionEquityAssets = Just (wordAtBytes rpcLogData 19)
                  , ibdMaxPostLeverageBps = Just (wordAtBytes rpcLogData 20)
                  , ibdSource = "commit_receipt"
                  }
    _ -> Nothing

parseIntentBoundsFromJson :: Integer -> Text -> Value -> Maybe IntentBoundsData
parseIntentBoundsFromJson oid source (Object obj) =
  Just
    IntentBoundsData
      { ibdOrderId = oid
      , ibdSide = fromInteger <$> lookupInteger "side" obj
      , ibdIsClose = lookupBool "isClose" obj
      , ibdTargetPrice = lookupInteger "targetPrice" obj
      , ibdSubmitBy = lookupInteger "submitBy" obj
      , ibdExecutionWindowSeconds = lookupInteger "executionWindowSeconds" obj
      , ibdCommitTimestamp = lookupInteger "commitTimestamp" obj
      , ibdExecutionDeadline = lookupInteger "executionDeadline" obj
      , ibdAllowedExecutionModes = fromInteger <$> lookupInteger "allowedExecutionModes" obj
      , ibdExpectedConfigHash = lookupText "expectedConfigHash" obj
      , ibdExecutionBountyAssets = lookupInteger "executionBountyAssets" obj
      , ibdMaxExecutionBountyAssets = lookupInteger "maxExecutionBountyAssets" obj
      , ibdMaxExecutionNotionalAssets = lookupInteger "maxExecutionNotionalAssets" obj
      , ibdMaxGrossAccountDebitAssets = lookupInteger "maxGrossAccountDebitAssets" obj
      , ibdMaxActionChargeAssets = lookupInteger "maxActionChargeAssets" obj
      , ibdMaxExplicitFeesAssets = lookupInteger "maxExplicitFeesAssets" obj
      , ibdMaxPostPositionSize = lookupInteger "maxPostPositionSize" obj
      , ibdMinPostSettlementBalanceAssets = lookupInteger "minPostSettlementBalanceAssets" obj
      , ibdMinPostPositionEquityAssets = lookupInteger "minPostPositionEquityAssets" obj
      , ibdMaxPostLeverageBps = lookupInteger "maxPostLeverageBps" obj
      , ibdSource = source
      }
parseIntentBoundsFromJson _ _ _ = Nothing

parsePositionProtectionSnapshot
  :: Text
  -> Maybe Text
  -> Maybe Text
  -> Value
  -> Maybe LinkedPositionProtection
parsePositionProtectionSnapshot fallbackId mBlock mTxHash (Object obj) =
  let pid = fromMaybe fallbackId (lookupStringOrNumber "protectionId" obj)
      parentOid = lookupInteger "parentOrderId" obj
      linkedOid = nonZeroString (lookupStringOrNumber "linkedOrderId" obj)
      statusTxt =
        fromMaybe
          (maybe "Unknown" statusCodeName (lookupInteger "status" obj))
          (lookupText "statusName" obj)
      triggeredLegTxt =
        case lookupText "triggeredLegName" obj <|> (triggeredLegCodeName <$> lookupInteger "triggeredLeg" obj) of
          Just "None" -> Nothing
          other -> other
      tp = nonZeroString (lookupStringOrNumber "takeProfitTriggerPrice" obj)
      sl = nonZeroString (lookupStringOrNumber "stopLossTriggerPrice" obj)
      markP = nonZeroString (lookupStringOrNumber "triggerMarkPrice" obj)
      bounty = nonZeroString (lookupStringOrNumber "triggerBountyAssets" obj)
      armedBlk = nonZeroString (lookupStringOrNumber "armedBlock" obj)
      updBlk = lookupStringOrNumber "updatedBlock" obj <|> mBlock
   in Just
        LinkedPositionProtection
          { lppProtectionId = pid
          , lppParentOrderId = parentOid
          , lppLinkedOrderId = linkedOid
          , lppStatus = statusTxt
          , lppTriggeredLeg = triggeredLegTxt
          , lppTakeProfitTriggerPrice = tp
          , lppStopLossTriggerPrice = sl
          , lppTriggerMarkPrice = markP
          , lppTriggerBountyAssets = bounty
          , lppArmedBlock = armedBlk
          , lppUpdatedBlock = updBlk
          , lppTxHash = mTxHash
          }
parsePositionProtectionSnapshot _ _ _ _ = Nothing

data CheckItem = CheckItem
  { ciKey :: Text
  , ciCategory :: Text
  , ciLabel :: Text
  , ciStatus :: Text
  , ciLimitValue :: Maybe Text
  , ciActualValue :: Maybe Text
  , ciUnit :: Text
  , ciDetail :: Text
  }
  deriving stock (Eq, Show)

checkItemToJson :: CheckItem -> Value
checkItemToJson CheckItem {..} =
  object
    [ "key" .= ciKey
    , "category" .= ciCategory
    , "label" .= ciLabel
    , "status" .= ciStatus
    , "limitValue" .= ciLimitValue
    , "actualValue" .= ciActualValue
    , "unit" .= ciUnit
    , "detail" .= ciDetail
    ]

buildOrderProtectionsForOrder :: OrderProtectionsBundle -> Integer -> Maybe Value
buildOrderProtectionsForOrder OrderProtectionsBundle {..} oid =
  let mIntent = Map.lookup oid opbIntents
      mOutcome = Map.lookup oid opbOutcomes
      mPosProt = Map.lookup oid opbPositionProtections
   in case (mIntent, mOutcome, mPosProt) of
        (Nothing, Nothing, Nothing) -> Nothing
        _ ->
          let checks = buildExecutionChecks mIntent mOutcome
              enforced = filter (\c -> ciStatus c == "passed" || ciStatus c == "failed") checks
              passedCount = length $ filter (\c -> ciStatus c == "passed") checks
              failedCount = length $ filter (\c -> ciStatus c == "failed") checks
              unboundedCount = length $ filter (\c -> ciStatus c == "not_set") checks
              fullBounds = maybe False hasFullBounds mIntent
              sourceTxt = maybe "outcome_only" ibdSource mIntent
              hasTp = maybe False (maybe False (const True) . lppTakeProfitTriggerPrice) mPosProt
              hasSl = maybe False (maybe False (const True) . lppStopLossTriggerPrice) mPosProt
           in Just $
                object
                  [ "orderId" .= show oid
                  , "boundsSource" .= sourceTxt
                  , "fullBoundsAvailable" .= fullBounds
                  , "summary"
                      .= object
                        [ "v3EnforcedCount" .= length enforced
                        , "v3PassedCount" .= passedCount
                        , "v3FailedCount" .= failedCount
                        , "v3UnboundedCount" .= unboundedCount
                        , "hasTakeProfit" .= hasTp
                        , "hasStopLoss" .= hasSl
                        , "positionProtectionStatus" .= (lppStatus <$> mPosProt)
                        ]
                  , "executionChecks" .= map checkItemToJson checks
                  , "positionProtection" .= (positionProtectionToJson <$> mPosProt)
                  ]

buildExecutionChecks :: Maybe IntentBoundsData -> Maybe OrderOutcomeMeta -> [CheckItem]
buildExecutionChecks mIntent mOutcome =
  let isExecuted = maybe False ((== "Executed") . oomTerminalStatus) mOutcome
      termReason = mOutcome >>= oomTerminalReason
      failConstraint = mOutcome >>= oomFailedConstraint
      execMode = mOutcome >>= oomExecutionMode
      execPrice = mOutcome >>= oomExecutionPrice
      mSide = (mIntent >>= ibdSide) <|> (mOutcome >>= oomSide)
      isClose = fromMaybe False (mIntent >>= ibdIsClose)
      receiptObj = case mOutcome >>= oomReceipt of
        Just (Object o) -> Just o
        _ -> Nothing
      receiptInt k = receiptObj >>= lookupInteger k
      commitTs = (mOutcome >>= oomCommitTimestamp) <|> (mIntent >>= ibdCommitTimestamp)
      termTs = mOutcome >>= oomTerminalTimestamp
      latencySec = case (commitTs, termTs) of
        (Just c, Just t) | t >= c -> Just (t - c)
        _ -> Nothing

      windowCheck = case mIntent >>= ibdExecutionWindowSeconds of
        Nothing -> Nothing
        Just winSec ->
          let deadlineFailed = termReason == Just "Expired"
              withinDeadline =
                case (termTs, mIntent >>= ibdExecutionDeadline) of
                  (Just t, Just d) -> t <= d
                  _ -> isExecuted
              st
                | deadlineFailed = "failed"
                | isExecuted || withinDeadline = "passed"
                | otherwise = "unavailable"
              detail = case (st, latencySec) of
                ("failed", _) -> "Expired after " <> showT winSec <> "s execution window"
                ("passed", Just l) -> "Filled in " <> showT l <> "s within " <> showT winSec <> "s window"
                ("passed", Nothing) -> "Filled within " <> showT winSec <> "s window"
                _ -> "Execution window " <> showT winSec <> "s"
           in Just $
                CheckItem
                  "execution_window"
                  "core"
                  "Execution window"
                  st
                  (Just (showT winSec))
                  (showT <$> latencySec)
                  "seconds"
                  detail

      modeCheck = case mIntent >>= ibdAllowedExecutionModes of
        Nothing -> Nothing
        Just mask ->
          let modeLabel = formatAllowedModes mask
              modeFailed = termReason == Just "Mode disallowed"
              modeMatches = maybe isExecuted (modeAllowed mask) execMode
              st
                | modeFailed = "failed"
                | modeMatches = "passed"
                | otherwise = "unavailable"
              detail = case (st, execMode) of
                ("failed", Just m) -> "Rejected: " <> m <> " outside allowed " <> modeLabel
                ("failed", Nothing) -> "Rejected: execution regime disallowed (" <> modeLabel <> ")"
                ("passed", Just m) -> "Filled in " <> m <> " regime (" <> modeLabel <> ")"
                _ -> "Allowed: " <> modeLabel
           in Just $
                CheckItem
                  "execution_mode"
                  "core"
                  "Execution regime"
                  st
                  (Just modeLabel)
                  execMode
                  "mode"
                  detail

      configCheck = case mIntent >>= ibdExpectedConfigHash of
        Nothing -> Nothing
        Just hashTxt
          | T.toLower hashTxt == zeroBytes32 ->
              Just $
                CheckItem
                  "config_hash"
                  "core"
                  "Protocol config pin"
                  "not_set"
                  Nothing
                  Nothing
                  "hash"
                  "No config hash pinned"
          | otherwise ->
              let failed = termReason == Just "Config mismatch"
                  st = if failed then "failed" else if isExecuted then "passed" else "unavailable"
                  shortHash = T.take 10 hashTxt <> "…" <> T.takeEnd 6 hashTxt
                  detail =
                    if failed
                      then "Rejected: on-chain config drifted from " <> shortHash
                      else "Matched pinned config " <> shortHash
               in Just $
                    CheckItem
                      "config_hash"
                      "core"
                      "Protocol config pin"
                      st
                      (Just hashTxt)
                      (if isExecuted then Just hashTxt else Nothing)
                      "hash"
                      detail

      priceCheck = case mIntent >>= ibdTargetPrice of
        Nothing -> Nothing
        Just 0 ->
          Just $
            CheckItem
              "price_limit"
              "core"
              "Price / slippage limit"
              "not_set"
              Nothing
              (showT <$> execPrice)
              "price"
              "Unbounded slippage tolerance"
        Just tp ->
          let failed = termReason == Just "Slippage"
              passedByPrice = case (execPrice, mSide) of
                (Just ep, Just side) -> priceWithinTarget side isClose ep tp
                _ -> isExecuted
              st
                | failed = "failed"
                | isExecuted = if passedByPrice then "passed" else "failed"
                | otherwise = "unavailable"
              detail =
                if failed
                  then "Rejected: fill price breached committed limit"
                  else "Fill price cleared committed slippage limit"
           in Just $
                CheckItem
                  "price_limit"
                  "core"
                  "Price / slippage limit"
                  st
                  (Just (showT tp))
                  (showT <$> execPrice)
                  "price"
                  detail

      bountyActual = receiptInt "executionBountyAssets" <|> (mIntent >>= ibdExecutionBountyAssets)
      bountyCheck =
        case mIntent >>= ibdMaxExecutionBountyAssets of
          Just maxB ->
            upperBoundCheck
              "max_execution_bounty"
              "core"
              "Max keeper bounty"
              "Execution bounty"
              "usdg"
              uint256Max
              maxB
              bountyActual
              failConstraint
              isExecuted
          Nothing -> Nothing

      explicitFeesActual = case (receiptInt "executionFeeAssets", receiptInt "frozenSpreadAssets") of
        (Just fee, Just spread) -> Just (fee + spread)
        (Just fee, Nothing) -> Just fee
        _ -> Nothing

      strictChecks =
        catMaybes
          [ (\lim -> upperBoundCheck "max_execution_notional" "accounting" "Max execution notional" "Execution notional" "usdg" uint256Max lim (receiptInt "executionNotionalAssets") failConstraint isExecuted) <$> (mIntent >>= ibdMaxExecutionNotionalAssets)
          , (\lim -> upperBoundCheck "max_gross_debit" "accounting" "Max gross debit" "Gross account debit" "usdg" uint256Max lim (receiptInt "grossAccountDebitAssets") failConstraint isExecuted) <$> (mIntent >>= ibdMaxGrossAccountDebitAssets)
          , (\lim -> upperBoundCheck "max_action_charge" "accounting" "Max action charge (VPI)" "Action charge" "usdg" uint256Max lim (receiptInt "actionChargeAssessedAssets") failConstraint isExecuted) <$> (mIntent >>= ibdMaxActionChargeAssets)
          , (\lim -> upperBoundCheck "max_explicit_fees" "accounting" "Max explicit fees" "Explicit fees" "usdg" uint256Max lim explicitFeesActual failConstraint isExecuted) <$> (mIntent >>= ibdMaxExplicitFeesAssets)
          , (\lim -> upperBoundCheck "max_post_position_size" "accounting" "Max post-trade size" "Post-position size" "size" uint256Max lim (receiptInt "postPositionSize") failConstraint isExecuted) <$> (mIntent >>= ibdMaxPostPositionSize)
          , (\lim -> lowerBoundCheck "min_post_balance" "accounting" "Min post-trade balance" "USDG balance after settlement" "usdg" lim (receiptInt "postSettlementBalanceAssets") failConstraint isExecuted) <$> (mIntent >>= ibdMinPostSettlementBalanceAssets)
          , (\lim -> lowerBoundCheck "min_post_equity" "accounting" "Min post-trade equity" "Post-position equity" "usdg" lim (receiptInt "postPositionEquityAssets") failConstraint isExecuted) <$> (mIntent >>= ibdMinPostPositionEquityAssets)
          , (\lim -> upperBoundCheck "max_post_leverage" "accounting" "Max post-trade leverage" "Post leverage" "bps" uint32Max lim (receiptInt "postLeverageBps") failConstraint isExecuted) <$> (mIntent >>= ibdMaxPostLeverageBps)
          ]
   in catMaybes [priceCheck, windowCheck, modeCheck, configCheck, bountyCheck] <> strictChecks

upperBoundCheck
  :: Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Maybe Integer
  -> Maybe Text
  -> Bool
  -> CheckItem
upperBoundCheck key cat label constraintName unit sentinel lim mActual mFailedConstraint isExecuted
  | lim >= sentinel =
      CheckItem key cat label "not_set" Nothing (showT <$> mActual) unit "Unbounded"
  | mFailedConstraint == Just constraintName =
      CheckItem key cat label "failed" (Just (showT lim)) (showT <$> mActual) unit ("Rejected: exceeded " <> T.toLower label)
  | otherwise =
      let st = case mActual of
            Just actual -> if actual <= lim then "passed" else "failed"
            Nothing -> if isExecuted then "passed" else "unavailable"
          detail = case st of
            "passed" -> "Within committed ceiling"
            "failed" -> "Exceeded committed ceiling"
            _ -> "Ceiling committed"
       in CheckItem key cat label st (Just (showT lim)) (showT <$> mActual) unit detail

lowerBoundCheck
  :: Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Maybe Integer
  -> Maybe Text
  -> Bool
  -> CheckItem
lowerBoundCheck key cat label constraintName unit lim mActual mFailedConstraint isExecuted
  | lim <= 0 =
      CheckItem key cat label "not_set" Nothing (showT <$> mActual) unit "Unbounded"
  | mFailedConstraint == Just constraintName =
      CheckItem key cat label "failed" (Just (showT lim)) (showT <$> mActual) unit ("Rejected: below " <> T.toLower label)
  | otherwise =
      let st = case mActual of
            Just actual -> if actual >= lim then "passed" else "failed"
            Nothing -> if isExecuted then "passed" else "unavailable"
          detail = case st of
            "passed" -> "Met committed floor"
            "failed" -> "Below committed floor"
            _ -> "Floor committed"
       in CheckItem key cat label st (Just (showT lim)) (showT <$> mActual) unit detail

-- Raw plDXY prices are inverted (display = 2 - raw / 1e8).
-- Long Open & Short Close: lower raw price = higher display price, so raw execPrice >= targetPrice.
-- Short Open & Long Close: higher raw price = lower display price, so raw execPrice <= targetPrice.
priceWithinTarget :: Int -> Bool -> Integer -> Integer -> Bool
priceWithinTarget side isClose execPrice targetPrice =
  case (side, isClose) of
    (0, False) -> execPrice >= targetPrice
    (1, True) -> execPrice >= targetPrice
    (1, False) -> execPrice <= targetPrice
    (0, True) -> execPrice <= targetPrice
    _ -> True

modeAllowed :: Int -> Text -> Bool
modeAllowed mask modeTxt =
  case T.toLower modeTxt of
    "live" -> (mask .&. 1) /= 0
    "fad" -> (mask .&. 2) /= 0
    "frozen" -> (mask .&. 4) /= 0
    _ -> False

formatAllowedModes :: Int -> Text
formatAllowedModes mask =
  let parts =
        catMaybes
          [ if (mask .&. 1) /= 0 then Just ("Live" :: Text) else Nothing
          , if (mask .&. 2) /= 0 then Just "FAD" else Nothing
          , if (mask .&. 4) /= 0 then Just "Frozen" else Nothing
          ]
   in case parts of
        [] -> "None"
        [single] -> single <> " only"
        [_, _, _] -> "All regimes"
        xs -> T.intercalate " + " xs

positionProtectionToJson :: LinkedPositionProtection -> Value
positionProtectionToJson LinkedPositionProtection {..} =
  object
    [ "protectionId" .= lppProtectionId
    , "parentOrderId" .= (show <$> lppParentOrderId)
    , "linkedOrderId" .= lppLinkedOrderId
    , "status" .= lppStatus
    , "triggeredLeg" .= lppTriggeredLeg
    , "takeProfitTriggerPrice" .= lppTakeProfitTriggerPrice
    , "stopLossTriggerPrice" .= lppStopLossTriggerPrice
    , "triggerMarkPrice" .= lppTriggerMarkPrice
    , "triggerBountyAssets" .= lppTriggerBountyAssets
    , "armedBlock" .= lppArmedBlock
    , "updatedBlock" .= lppUpdatedBlock
    , "txHash" .= lppTxHash
    ]

statusCodeName :: Integer -> Text
statusCodeName = \case
  0 -> "None"
  1 -> "PendingOpen"
  2 -> "Armed"
  3 -> "Triggered"
  4 -> "Executed"
  5 -> "Failed"
  6 -> "Cancelled"
  7 -> "Liquidated"
  8 -> "Latched"
  n -> "Status(" <> showT n <> ")"

triggeredLegCodeName :: Integer -> Text
triggeredLegCodeName = \case
  1 -> "TakeProfit"
  2 -> "StopLoss"
  _ -> "None"

wordAtBytes :: BS.ByteString -> Int -> Integer
wordAtBytes bs idx =
  BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 $ BS.take 32 (BS.drop (idx * 32) bs)

hexWordAtBytes :: BS.ByteString -> Int -> Text
hexWordAtBytes bs idx =
  "0x" <> TE.decodeUtf8 (B16.encode (BS.take 32 (BS.drop (idx * 32) bs)))

lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText k obj = case KM.lookup (Key.fromText k) obj of
  Just (String s) | not (T.null (T.strip s)) -> Just (T.strip s)
  _ -> Nothing

lookupBool :: Text -> KM.KeyMap Value -> Maybe Bool
lookupBool k obj = case KM.lookup (Key.fromText k) obj of
  Just (Bool b) -> Just b
  _ -> Nothing

lookupInteger :: Text -> KM.KeyMap Value -> Maybe Integer
lookupInteger k obj = case KM.lookup (Key.fromText k) obj of
  Just (Number n) ->
    case floatingOrInteger n :: Either Double Integer of
      Right i -> Just i
      Left _ -> Nothing
  Just (String s) -> readMaybe (T.unpack (T.strip s))
  _ -> Nothing

lookupStringOrNumber :: Text -> KM.KeyMap Value -> Maybe Text
lookupStringOrNumber k obj = case KM.lookup (Key.fromText k) obj of
  Just (String s) | not (T.null (T.strip s)) -> Just (T.strip s)
  Just (Number n) ->
    case floatingOrInteger n :: Either Double Integer of
      Right i -> Just (showT i)
      Left d -> Just (T.pack (show d))
  _ -> Nothing

nonZeroString :: Maybe Text -> Maybe Text
nonZeroString (Just s) | s /= "0" && not (T.null s) = Just s
nonZeroString _ = Nothing

showT :: (Show a) => a -> Text
showT = T.pack . show

nullableNumericIntegerField :: RowParser (Maybe Integer)
nullableNumericIntegerField =
  fieldWith $ \column raw ->
    case raw of
      Nothing -> pure Nothing
      Just bytes -> case readMaybe (BS8.unpack bytes) of
        Just integer -> pure $ Just integer
        Nothing -> returnError ConversionFailed column "Expected nullable integer NUMERIC column"
