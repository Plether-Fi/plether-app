module Plether.Perps.ExecutionTrace
  ( TradeExecutionKind (..)
  , TradeExecutionEvidence (..)
  , processOrderTypedSelector
  , executeOpenSelector
  , executeCloseSelector
  , executeOracleSelector
  , executeFrozenOracleSelector
  , decodeTradeExecutionEvidence
  ) where

import Control.Monad (unless, when)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Plether.Ethereum.Abi (decodeInt256, decodeUint256)

data TradeExecutionKind
  = TradeOpen
  | TradeClose
  deriving stock (Show, Eq)

data TradeExecutionEvidence = TradeExecutionEvidence
  { teeKind :: TradeExecutionKind
  , teeVpiUsdc :: Integer
  , teeFrozenCloseSpreadUsdc :: Maybe Integer
  , teeExecutionOraclePrice :: Integer
  , teeOraclePublishTime :: Integer
  , teeOracleFrozen :: Bool
  }
  deriving stock (Show, Eq)

-- | Extract execution economics from a geth @callTracer@ or Blockscout
-- @raw-trace@ response. Evidence is accepted only along the authenticated
-- OrderRouter -> Engine -> SettlementSidecar call path and is bound to the
-- corresponding OrderRouter -> PletherOracle response by execution price,
-- publish time, and trace occurrence.
--
-- The caller remains responsible for verifying that the trace response belongs
-- to the requested canonical transaction.
decodeTradeExecutionEvidence
  :: Text
  -- ^ Configured OrderRouter address.
  -> Text
  -- ^ Configured Engine address.
  -> Text
  -- ^ Configured SettlementSidecar address.
  -> Text
  -- ^ Configured PletherOracle address.
  -> Value
  -> Either Text (Map Integer TradeExecutionEvidence)
decodeTradeExecutionEvidence orderRouter engine settlementSidecar pletherOracle traceValue = do
  expectedRouter <- normalizeAddress "configured OrderRouter" orderRouter
  expectedEngine <- normalizeAddress "configured Engine" engine
  expectedSidecar <- normalizeAddress "configured SettlementSidecar" settlementSidecar
  expectedOracle <- normalizeAddress "configured PletherOracle" pletherOracle
  root <- parseTraceNode "trace root" traceValue
  events <-
    collectTraceEvents
      expectedRouter
      expectedEngine
      expectedSidecar
      expectedOracle
      True
      root
  pairTraceEvents events

data TraceNode = TraceNode
  { tnFrom :: Maybe Text
  , tnTo :: Maybe Text
  , tnCallType :: Maybe Text
  , tnInput :: Maybe ByteString
  , tnOutput :: Maybe ByteString
  , tnSucceeded :: Bool
  , tnCalls :: [TraceNode]
  }

parseTraceNode :: Text -> Value -> Either Text TraceNode
parseTraceNode label = \case
  Object objectValue -> do
    fromAddressText <- optionalTextField label "from" objectValue
    toAddressText <- optionalTextField label "to" objectValue
    fromAddress <-
      traverse (normalizeAddress $ label <> ".from") fromAddressText
    toAddress <-
      traverse (normalizeAddress $ label <> ".to") toAddressText
    callType <- optionalTextField label "type" objectValue
    inputText <- optionalTextField label "input" objectValue
    outputText <- optionalTextField label "output" objectValue
    input <- traverse (decodeHexBytes $ label <> ".input") inputText
    output <- traverse (decodeHexBytes $ label <> ".output") outputText
    errorValue <- traceError label objectValue
    failed <- optionalBoolField label "failed" objectValue
    calls <-
      case KeyMap.lookup (Key.fromText "calls") objectValue of
        Nothing -> Right []
        Just Null -> Right []
        Just (Array values) ->
          traverse
            (\(index, value) ->
              parseTraceNode
                (label <> ".calls[" <> Text.pack (show index) <> "]")
                value
            )
            (zip [(0 :: Int) ..] $ toList values)
        Just _ -> Left $ label <> ".calls must be an array"
    pure
      TraceNode
        { tnFrom = fromAddress
        , tnTo = toAddress
        , tnCallType = callType
        , tnInput = input
        , tnOutput = output
        , tnSucceeded = not errorValue && not failed
        , tnCalls = calls
        }
  _ -> Left $ label <> " must be an object"

optionalTextField
  :: Text
  -> Text
  -> KeyMap.KeyMap Value
  -> Either Text (Maybe Text)
optionalTextField label field objectValue =
  case KeyMap.lookup (Key.fromText field) objectValue of
    Nothing -> Right Nothing
    Just Null -> Right Nothing
    Just (String value) -> Right $ Just value
    Just _ -> Left $ label <> "." <> field <> " must be a string or null"

optionalBoolField
  :: Text
  -> Text
  -> KeyMap.KeyMap Value
  -> Either Text Bool
optionalBoolField label field objectValue =
  case KeyMap.lookup (Key.fromText field) objectValue of
    Nothing -> Right False
    Just Null -> Right False
    Just (Bool value) -> Right value
    Just _ -> Left $ label <> "." <> field <> " must be a boolean or null"

traceError :: Text -> KeyMap.KeyMap Value -> Either Text Bool
traceError label objectValue =
  case KeyMap.lookup (Key.fromText "error") objectValue of
    Nothing -> Right False
    Just Null -> Right False
    Just (String value) -> Right $ not $ Text.null value
    Just _ -> Left $ label <> ".error must be a string or null"

data SettlementEvidence = SettlementEvidence
  { seKind :: TradeExecutionKind
  , seVpiUsdc :: Integer
  , seFrozenCloseSpreadUsdc :: Maybe Integer
  }

data ProcessTrace = ProcessTrace
  { ptOrderId :: Integer
  , ptExecutionPrice :: Integer
  , ptPublishTime :: Integer
  , ptSettlement :: Maybe SettlementEvidence
  }

data OracleTrace = OracleTrace
  { otExecutionPrice :: Integer
  , otMarkPrice :: Integer
  , otPublishTime :: Integer
  , otOracleFrozen :: Bool
  }

data TraceEvent
  = OracleEvent OracleTrace
  | ProcessEvent ProcessTrace

collectTraceEvents
  :: Text
  -> Text
  -> Text
  -> Text
  -> Bool
  -> TraceNode
  -> Either Text [TraceEvent]
collectTraceEvents
  expectedRouter
  expectedEngine
  expectedSidecar
  expectedOracle
  ancestorsSucceeded
  node = do
    current <-
      if not ancestorsSucceeded
        then Right []
        else
          if isProcessFrame expectedRouter expectedEngine node
            then
              (: [])
                . ProcessEvent
                <$> decodeProcessFrame
                  expectedEngine
                  expectedSidecar
                  nodeSucceeded
                  node
            else
              if nodeSucceeded && isOracleFrame expectedRouter expectedOracle node
                then (: []) . OracleEvent <$> decodeOracleFrame node
                else Right []
    descendants <-
      concat
        <$> traverse
          ( collectTraceEvents
              expectedRouter
              expectedEngine
              expectedSidecar
              expectedOracle
              nodeSucceeded
          )
          (tnCalls node)
    pure $ current <> descendants
    where
      nodeSucceeded = ancestorsSucceeded && tnSucceeded node

pairTraceEvents
  :: [TraceEvent]
  -> Either Text (Map Integer TradeExecutionEvidence)
pairTraceEvents = go [] Map.empty
  where
    go
      :: [OracleTrace]
      -> Map Integer TradeExecutionEvidence
      -> [TraceEvent]
      -> Either Text (Map Integer TradeExecutionEvidence)
    go _ indexed [] = Right indexed
    go pending indexed (OracleEvent oracleTrace : remaining) =
      go (oracleTrace : pending) indexed remaining
    go pending indexed (ProcessEvent processTrace : remaining) =
      case takeMatchingOracle processTrace pending of
        Nothing ->
          case ptSettlement processTrace of
            Nothing -> go pending indexed remaining
            Just _ ->
              Left $
                "successful processOrderTyped frame for order "
                  <> Text.pack (show $ ptOrderId processTrace)
                  <> " has no matching preceding PletherOracle response"
        Just (oracleTrace, unmatched) ->
          case ptSettlement processTrace of
            Nothing -> go unmatched indexed remaining
            Just settlement -> do
              when (Map.member (ptOrderId processTrace) indexed) $
                Left $
                  "trace contains duplicate execution evidence for order "
                    <> Text.pack (show $ ptOrderId processTrace)
              let evidence =
                    TradeExecutionEvidence
                      { teeKind = seKind settlement
                      , teeVpiUsdc = seVpiUsdc settlement
                      , teeFrozenCloseSpreadUsdc =
                          seFrozenCloseSpreadUsdc settlement
                      , teeExecutionOraclePrice = otMarkPrice oracleTrace
                      , teeOraclePublishTime = otPublishTime oracleTrace
                      , teeOracleFrozen = otOracleFrozen oracleTrace
                      }
              go
                unmatched
                (Map.insert (ptOrderId processTrace) evidence indexed)
                remaining

takeMatchingOracle
  :: ProcessTrace
  -> [OracleTrace]
  -> Maybe (OracleTrace, [OracleTrace])
takeMatchingOracle processTrace = go []
  where
    go _ [] = Nothing
    go skipped (candidate : remaining)
      | otExecutionPrice candidate == ptExecutionPrice processTrace
          && otPublishTime candidate == ptPublishTime processTrace =
          Just (candidate, reverse skipped <> remaining)
      | otherwise = go (candidate : skipped) remaining

isProcessFrame :: Text -> Text -> TraceNode -> Bool
isProcessFrame expectedRouter expectedEngine TraceNode {..} =
  maybe False ((== processOrderTypedSelector) . BS.take selectorLength) tnInput
    && addressMatches expectedRouter tnFrom
    && addressMatches expectedEngine tnTo

isOracleFrame :: Text -> Text -> TraceNode -> Bool
isOracleFrame expectedRouter expectedOracle TraceNode {..} =
  maybe False (isOracleSelector . BS.take selectorLength) tnInput
    && addressMatches expectedRouter tnFrom
    && addressMatches expectedOracle tnTo

isOracleSelector :: ByteString -> Bool
isOracleSelector actualSelector =
  actualSelector == executeOracleSelector
    || actualSelector == executeFrozenOracleSelector

decodeProcessFrame
  :: Text
  -> Text
  -> Bool
  -> TraceNode
  -> Either Text ProcessTrace
decodeProcessFrame expectedEngine expectedSidecar frameSucceeded processFrame = do
  requireCallType "processOrderTyped" processFrame
  processInput <-
    maybe
      (Left "processOrderTyped frame is missing calldata")
      Right
      (tnInput processFrame)
  processArguments <-
    decodeWordArguments
      "processOrderTyped"
      processWordCount
      processInput
  orderId <- wordAt "processOrderTyped order ID" processArguments orderIdWordIndex
  unless (orderId <= maxUint64) $
    Left "processOrderTyped order ID is outside the uint64 range"
  isClose <- wordAt "processOrderTyped isClose" processArguments isCloseWordIndex
  expectedKind <-
    case isClose of
      0 -> Right TradeOpen
      1 -> Right TradeClose
      _ -> Left "processOrderTyped isClose is not a canonical ABI boolean"
  executionPrice <-
    wordAt
      "processOrderTyped execution price"
      processArguments
      processExecutionPriceWordIndex
  publishTime <-
    wordAt
      "processOrderTyped publish time"
      processArguments
      processPublishTimeWordIndex
  unless (publishTime <= maxUint64) $
    Left "processOrderTyped publish time is outside the uint64 range"
  settlement <-
    if frameSucceeded
      then Just <$> decodeProcessSettlement expectedEngine expectedSidecar expectedKind processFrame
      else Right Nothing
  pure
    ProcessTrace
      { ptOrderId = orderId
      , ptExecutionPrice = executionPrice
      , ptPublishTime = publishTime
      , ptSettlement = settlement
      }

decodeProcessSettlement
  :: Text
  -> Text
  -> TradeExecutionKind
  -> TraceNode
  -> Either Text SettlementEvidence
decodeProcessSettlement expectedEngine expectedSidecar expectedKind processFrame = do
  directSidecarCalls <-
    traverse
      decodeMatchingSidecar
      (filter (isSidecarRoute expectedEngine expectedSidecar) $ tnCalls processFrame)
  let successfulEvidence =
        [ evidence
        | Just evidence <- directSidecarCalls
        ]
  settlement <-
    case successfulEvidence of
      [evidence] -> Right evidence
      [] ->
        Left
          "successful processOrderTyped frame does not contain exactly one successful direct settlement call"
      _ ->
        Left
          "successful processOrderTyped frame contains ambiguous direct settlement calls"
  unless (seKind settlement == expectedKind) $
    Left "processOrderTyped isClose does not match the settlement selector"
  pure settlement
  where
    decodeMatchingSidecar :: TraceNode -> Either Text (Maybe SettlementEvidence)
    decodeMatchingSidecar sidecarFrame =
      case tnInput sidecarFrame of
        Nothing -> Left "direct Engine-to-Sidecar frame is missing calldata"
        Just calldata
          | BS.length calldata < selectorLength ->
              Left "direct Engine-to-Sidecar calldata is missing its selector"
          | actualSelector == executeOpenSelector ->
              decodeIfSuccessful TradeOpen openWordCount calldata sidecarFrame
          | actualSelector == executeCloseSelector ->
              decodeIfSuccessful TradeClose closeWordCount calldata sidecarFrame
          | otherwise -> Right Nothing
          where
            actualSelector = BS.take selectorLength calldata

    decodeIfSuccessful
      :: TradeExecutionKind
      -> Int
      -> ByteString
      -> TraceNode
      -> Either Text (Maybe SettlementEvidence)
    decodeIfSuccessful kind expectedWordCount calldata sidecarFrame = do
      requireCallType "settlement" sidecarFrame
      if not $ tnSucceeded sidecarFrame
        then Right Nothing
        else
          Just
            <$> decodeSidecarEvidence
              expectedEngine
              kind
              expectedWordCount
              calldata

isSidecarRoute :: Text -> Text -> TraceNode -> Bool
isSidecarRoute expectedEngine expectedSidecar TraceNode {..} =
  addressMatches expectedEngine tnFrom
    && addressMatches expectedSidecar tnTo

decodeSidecarEvidence
  :: Text
  -> TradeExecutionKind
  -> Int
  -> ByteString
  -> Either Text SettlementEvidence
decodeSidecarEvidence expectedEngine kind expectedWordCount calldata = do
  arguments <- decodeWordArguments "settlement" expectedWordCount calldata
  hostWord <- byteWordAt "settlement host" arguments hostWordIndex
  hostAddress <- decodeAddressWord "settlement host" hostWord
  unless (hostAddress == expectedEngine) $
    Left "settlement host does not match the configured Engine"
  validDelta <- wordAt "settlement valid delta" arguments validDeltaWordIndex
  unless (validDelta == 1) $
    Left "settlement delta is not marked valid"
  revertCode <- wordAt "settlement revert code" arguments revertCodeWordIndex
  unless (revertCode == 0) $
    Left "settlement delta has a nonzero revert code"
  vpiWord <- byteWordAt "settlement VPI" arguments vpiWordIndex
  frozenCloseSpread <-
    case kind of
      TradeOpen -> Right Nothing
      TradeClose -> do
        spread <-
          wordAt
            "settlement frozen-close spread"
            arguments
            frozenCloseSpreadWordIndex
        pure $ Just spread
  pure
    SettlementEvidence
      { seKind = kind
      , seVpiUsdc = decodeInt256 vpiWord
      , seFrozenCloseSpreadUsdc = frozenCloseSpread
      }

decodeOracleFrame :: TraceNode -> Either Text OracleTrace
decodeOracleFrame oracleFrame = do
  requireCallType "PletherOracle execution" oracleFrame
  output <-
    maybe
      (Left "successful PletherOracle execution frame is missing output")
      Right
      (tnOutput oracleFrame)
  outputWords <- decodeExactWords "PletherOracle execution output" oracleOutputWordCount output
  valid <- wordAt "PletherOracle valid result" outputWords oracleValidWordIndex
  unless (valid == 1) $
    Left "PletherOracle execution output is not marked valid"
  executionPrice <-
    wordAt
      "PletherOracle execution price"
      outputWords
      oracleExecutionPriceWordIndex
  markPrice <-
    wordAt
      "PletherOracle mark price"
      outputWords
      oracleMarkPriceWordIndex
  publishTime <-
    wordAt
      "PletherOracle publish time"
      outputWords
      oraclePublishTimeWordIndex
  unless (publishTime <= maxUint64) $
    Left "PletherOracle publish time is outside the uint64 range"
  oracleFrozenWord <-
    wordAt
      "PletherOracle frozen flag"
      outputWords
      oracleFrozenWordIndex
  oracleFrozen <-
    decodeCanonicalBool "PletherOracle frozen flag" oracleFrozenWord
  isFadWord <-
    wordAt
      "PletherOracle FAD flag"
      outputWords
      oracleFadWordIndex
  _ <- decodeCanonicalBool "PletherOracle FAD flag" isFadWord
  pure
    OracleTrace
      { otExecutionPrice = executionPrice
      , otMarkPrice = markPrice
      , otPublishTime = publishTime
      , otOracleFrozen = oracleFrozen
      }

decodeCanonicalBool :: Text -> Integer -> Either Text Bool
decodeCanonicalBool label value =
  case value of
    0 -> Right False
    1 -> Right True
    _ -> Left $ label <> " is not a canonical ABI boolean"

decodeWordArguments
  :: Text
  -> Int
  -> ByteString
  -> Either Text ByteString
decodeWordArguments label expectedWordCount calldata = do
  unless (BS.length calldata >= selectorLength) $
    Left $ label <> " calldata is missing its selector"
  decodeExactWords label expectedWordCount $ BS.drop selectorLength calldata

decodeExactWords
  :: Text
  -> Int
  -> ByteString
  -> Either Text ByteString
decodeExactWords label expectedWordCount bytes = do
  unless (BS.length bytes == expectedWordCount * abiWordLength) $
    Left $
      label
        <> " calldata does not have exactly "
        <> Text.pack (show expectedWordCount)
        <> " ABI words"
  pure bytes

wordAt :: Text -> ByteString -> Int -> Either Text Integer
wordAt label arguments wordIndex =
  decodeUint256 <$> byteWordAt label arguments wordIndex

byteWordAt :: Text -> ByteString -> Int -> Either Text ByteString
byteWordAt label arguments wordIndex =
  let offset = wordIndex * abiWordLength
      value = BS.take abiWordLength $ BS.drop offset arguments
   in if BS.length value == abiWordLength
        then Right value
        else Left $ label <> " ABI word is truncated"

decodeAddressWord :: Text -> ByteString -> Either Text Text
decodeAddressWord label word = do
  unless (BS.length word == abiWordLength) $
    Left $ label <> " ABI word is truncated"
  unless (BS.all (== 0) $ BS.take 12 word) $
    Left $ label <> " address has nonzero ABI padding"
  pure $ "0x" <> Text.decodeUtf8 (Base16.encode $ BS.drop 12 word)

requireCallType :: Text -> TraceNode -> Either Text ()
requireCallType label TraceNode {..} =
  case Text.toUpper <$> tnCallType of
    Just "CALL" -> Right ()
    _ -> Left $ label <> " frame is not a CALL"

addressMatches :: Text -> Maybe Text -> Bool
addressMatches expected =
  maybe False $
    either (const False) (== expected) . normalizeAddress "trace address"

normalizeAddress :: Text -> Text -> Either Text Text
normalizeAddress label value =
  let stripped
        | "0x" `Text.isPrefixOf` Text.toLower value = Text.drop 2 value
        | otherwise = value
   in if Text.length stripped == addressHexLength
        && Text.all isAsciiHexDigit stripped
        then Right $ "0x" <> Text.toLower stripped
        else Left $ label <> " is not a 20-byte hex address"

decodeHexBytes :: Text -> Text -> Either Text ByteString
decodeHexBytes label value = do
  unless ("0x" `Text.isPrefixOf` Text.toLower value) $
    Left $ label <> " must have a 0x prefix"
  let stripped = Text.drop 2 value
  unless (Text.length stripped `mod` 2 == 0) $
    Left $ label <> " has an odd number of hex digits"
  unless (Text.all isAsciiHexDigit stripped) $
    Left $ label <> " contains non-hex characters"
  case Base16.decode (Text.encodeUtf8 $ Text.toLower stripped) of
    Left err -> Left $ label <> " is invalid hex: " <> Text.pack err
    Right bytes -> Right bytes

isAsciiHexDigit :: Char -> Bool
isAsciiHexDigit char =
  (char >= '0' && char <= '9')
    || (char >= 'a' && char <= 'f')
    || (char >= 'A' && char <= 'F')

processOrderTypedSelector, executeOpenSelector, executeCloseSelector :: ByteString
processOrderTypedSelector = BS.pack [0xb7, 0x45, 0x4c, 0x3a]
executeOpenSelector = BS.pack [0x41, 0xe3, 0x9b, 0xc7]
executeCloseSelector = BS.pack [0x4e, 0x8e, 0x4f, 0xbc]

executeOracleSelector, executeFrozenOracleSelector :: ByteString
executeOracleSelector = BS.pack [0x08, 0x83, 0x5e, 0x57]
executeFrozenOracleSelector = BS.pack [0xb3, 0x69, 0xf2, 0xae]

selectorLength, abiWordLength, addressHexLength :: Int
selectorLength = 4
abiWordLength = 32
addressHexLength = 40

processWordCount, openWordCount, closeWordCount, oracleOutputWordCount :: Int
processWordCount = 12
openWordCount = 45
closeWordCount = 67
oracleOutputWordCount = 9

orderIdWordIndex, isCloseWordIndex, processExecutionPriceWordIndex, processPublishTimeWordIndex :: Int
orderIdWordIndex = 6
isCloseWordIndex = 8
processExecutionPriceWordIndex = 9
processPublishTimeWordIndex = 11

hostWordIndex, validDeltaWordIndex, revertCodeWordIndex :: Int
hostWordIndex = 0
validDeltaWordIndex = 1
revertCodeWordIndex = 2

vpiWordIndex, frozenCloseSpreadWordIndex :: Int
vpiWordIndex = 9
frozenCloseSpreadWordIndex = 11

oracleValidWordIndex, oracleExecutionPriceWordIndex, oracleMarkPriceWordIndex :: Int
oracleValidWordIndex = 0
oracleExecutionPriceWordIndex = 1
oracleMarkPriceWordIndex = 2

oraclePublishTimeWordIndex, oracleFrozenWordIndex, oracleFadWordIndex :: Int
oraclePublishTimeWordIndex = 3
oracleFrozenWordIndex = 7
oracleFadWordIndex = 8

maxUint64 :: Integer
maxUint64 = 2 ^ (64 :: Integer) - 1
