module Plether.Protocol.Snapshots
  ( SnapshotArgument (..)
  , SnapshotAvailability (..)
  , SnapshotBuildContext (..)
  , SnapshotCallPlan (..)
  , SnapshotContract (..)
  , SnapshotDocument (..)
  , SnapshotEvidence (..)
  , SnapshotField (..)
  , SnapshotFieldValue (..)
  , SnapshotPlan (..)
  , SnapshotRead (..)
  , SnapshotSourceBlock (..)
  , SnapshotUnavailable (..)
  , SnapshotValueEncoding (..)
  , MarketSide (..)
  , Tranche (..)
  , snapshotModelVersion
  , housePoolLiquidityPlan
  , protocolStatusPlan
  , sideSnapshotPlan
  , trancheSnapshotPlan
  , parameterSnapshotPlan
  , accountLedgerSnapshotPlan
  , globalSnapshotPlans
  , decodeSnapshotCall
  , buildSnapshot
  , snapshotDocumentToJson
  , snapshotEvidenceToJson
  , snapshotAvailabilityToJson
  ) where

import qualified Data.Aeson.Key as Key
import Data.Aeson (Value (..), object, (.=))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List (find, foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Abi (decodeUint256)
import Plether.Protocol.Parameters
  ( ParameterDefinition (..)
  , parameterCatalog
  )

-- | Version of the pure ABI-to-snapshot model. It is independent of the
-- release's calculation version so a projection can record both.
snapshotModelVersion :: Text
snapshotModelVersion = "protocol-snapshots-v1"

data SnapshotContract
  = ReleaseContract Text
  deriving stock (Show, Eq, Ord)

data SnapshotArgument
  = UintArgument Integer
  | AddressArgument Text
  deriving stock (Show, Eq)

data SnapshotValueEncoding
  = UnsignedWord
  | SignedWord
  | BooleanWord
  | Uint8Word
  | Uint64Word
  deriving stock (Show, Eq)

data SnapshotField = SnapshotField
  { sfKey :: Text
  , sfWordIndex :: Int
  , sfEncoding :: SnapshotValueEncoding
  , sfRawScale :: Text
  , sfDisplayUnit :: Text
  }
  deriving stock (Show, Eq)

data SnapshotCallPlan = SnapshotCallPlan
  { scpId :: Text
  , scpContract :: SnapshotContract
  , scpSignature :: Text
  , scpArguments :: [SnapshotArgument]
  , scpExpectedWordCount :: Int
  , scpFields :: [SnapshotField]
  }
  deriving stock (Show, Eq)

data SnapshotPlan = SnapshotPlan
  { spScope :: Text
  , spFormulaIdentifier :: Text
  , spFormula :: Text
  , spCalls :: [SnapshotCallPlan]
  }
  deriving stock (Show, Eq)

data SnapshotSourceBlock = SnapshotSourceBlock
  { ssbNumber :: Integer
  , ssbHash :: Maybe Text
  , ssbTimestamp :: Maybe Integer
  }
  deriving stock (Show, Eq)

data SnapshotBuildContext = SnapshotBuildContext
  { sbcReleaseId :: Text
  , sbcCalculationVersion :: Text
  , sbcSourceBlock :: SnapshotSourceBlock
  }
  deriving stock (Show, Eq)

data SnapshotUnavailable = SnapshotUnavailable
  { suReason :: Text
  , suDetail :: Maybe Text
  }
  deriving stock (Show, Eq)

-- | A call result keyed by 'scpId'. RPC/archive failures are supplied by the
-- caller; ABI-shape failures are produced by 'decodeSnapshotCall'.
data SnapshotRead = SnapshotRead
  { srCallId :: Text
  , srResult :: Either SnapshotUnavailable ByteString
  }
  deriving stock (Show, Eq)

data SnapshotFieldValue
  = UnsignedValue Integer
  | SignedValue Integer
  | BooleanValue Bool
  deriving stock (Show, Eq)

data SnapshotEvidence = SnapshotEvidence
  { seCallId :: Text
  , seContract :: SnapshotContract
  , seSignature :: Text
  , seProvenance :: Text
  , seExpectedWordCount :: Int
  , seObservedByteLength :: Maybe Int
  , seSourceBlock :: SnapshotSourceBlock
  }
  deriving stock (Show, Eq)

data SnapshotAvailability = SnapshotAvailability
  { saCallId :: Text
  , saAffectedFields :: [Text]
  , saReason :: Text
  , saDetail :: Maybe Text
  }
  deriving stock (Show, Eq)

data SnapshotDocument = SnapshotDocument
  { sdScope :: Text
  , sdReleaseId :: Text
  , sdModelVersion :: Text
  , sdCalculationVersion :: Text
  , sdFormulaIdentifier :: Text
  , sdFormula :: Text
  , sdSourceBlock :: SnapshotSourceBlock
  , sdValues :: [(Text, SnapshotField, SnapshotFieldValue)]
  , sdEvidence :: [SnapshotEvidence]
  , sdAvailability :: [SnapshotAvailability]
  }
  deriving stock (Show, Eq)

data MarketSide = LongSide | ShortSide
  deriving stock (Show, Eq)

data Tranche = SeniorTranche | JuniorTranche
  deriving stock (Show, Eq)

housePoolLiquidityPlan :: SnapshotPlan
housePoolLiquidityPlan =
  singleCallPlan
    "house-pool.liquidity"
    "protocol.snapshot.house_pool_liquidity.v1"
    "ABI-decode HousePool.getPoolLiquidityView() at the source block."
    SnapshotCallPlan
      { scpId = "house-pool.liquidity"
      , scpContract = ReleaseContract "housePool"
      , scpSignature = "getPoolLiquidityView()"
      , scpArguments = []
      , scpExpectedWordCount = 11
      , scpFields =
          [ usdc "totalAssetsUsdc" 0
          , usdc "freeUsdc" 1
          , usdc "withdrawalReservedUsdc" 2
          , usdc "pendingRecapitalizationUsdc" 3
          , usdc "pendingTradingRevenueUsdc" 4
          , usdc "seniorPrincipalUsdc" 5
          , usdc "juniorPrincipalUsdc" 6
          , usdc "seniorHighWaterMarkUsdc" 7
          , boolean "markFresh" 8
          , boolean "oracleFrozen" 9
          , boolean "degradedMode" 10
          ]
      }

protocolStatusPlan :: SnapshotPlan
protocolStatusPlan =
  singleCallPlan
    "protocol.status"
    "protocol.snapshot.protocol_status.v1"
    "ABI-decode PublicLens.getProtocolStatus() at the source block."
    SnapshotCallPlan
      { scpId = "protocol.status"
      , scpContract = ReleaseContract "publicLens"
      , scpSignature = "getProtocolStatus()"
      , scpArguments = []
      , scpExpectedWordCount = 7
      , scpFields =
          [ field "phase" 0 Uint8Word "1" "protocol_phase"
          , field "lastMarkPrice" 1 UnsignedWord "1e8" "index_price"
          , field "lastMarkTimestamp" 2 Uint64Word "1" "unix_seconds"
          , boolean "oracleFrozen" 3
          , boolean "fadWindow" 4
          , boolean "tradingActive" 5
          , boolean "withdrawalLive" 6
          ]
      }

sideSnapshotPlan :: MarketSide -> SnapshotPlan
sideSnapshotPlan side =
  singleCallPlan
    ("market." <> sideName side)
    ("protocol.snapshot.market_side." <> sideName side <> ".v1")
    "ABI-decode CfdEngine.sides(uint256) at the source block."
    SnapshotCallPlan
      { scpId = "market." <> sideName side <> ".totals"
      , scpContract = ReleaseContract "cfdEngine"
      , scpSignature = "sides(uint256)"
      , scpArguments = [UintArgument $ sideIndex side]
      , scpExpectedWordCount = 4
      , scpFields =
          [ usdc "maxProfitUsdc" 0
          , field "openInterest" 1 UnsignedWord "1e18" "position_size"
          , usdc "entryNotionalUsdc" 2
          , usdc "totalMarginUsdc" 3
          ]
      }

trancheSnapshotPlan :: Tranche -> SnapshotPlan
trancheSnapshotPlan tranche =
  SnapshotPlan
    { spScope = "tranche." <> trancheName tranche
    , spFormulaIdentifier =
        "protocol.snapshot.tranche." <> trancheName tranche <> ".v1"
    , spFormula =
        "Collect exact ERC-4626 totals and deposit lifecycle counters from one tranche at the source block."
    , spCalls =
        [ oneWord "total-assets" "totalAssets()" (usdc "totalAssetsUsdc" 0)
        , oneWord
            "total-supply"
            "totalSupply()"
            (field "totalSupply" 0 UnsignedWord "1e18" "vault_shares")
        , oneWord
            "current-deposit-epoch"
            "currentDepositEpoch()"
            (field "currentDepositEpoch" 0 UnsignedWord "1" "epoch")
        , oneWord
            "deposit-cooldown"
            "DEPOSIT_COOLDOWN()"
            (field "depositCooldownSeconds" 0 UnsignedWord "1" "seconds")
        ]
    }
  where
    callPrefix = "tranche." <> trancheName tranche <> "."
    contract = ReleaseContract $ trancheName tranche <> "Vault"
    oneWord suffix signature valueField =
      SnapshotCallPlan
        { scpId = callPrefix <> suffix
        , scpContract = contract
        , scpSignature = signature
        , scpArguments = []
        , scpExpectedWordCount = 1
        , scpFields = [valueField]
        }

-- | One scope containing one call per unique @(contract, getter)@ in the
-- checked-in parameter catalog. Tuple getters therefore have one exact ABI
-- shape rather than being read once per field.
parameterSnapshotPlan :: SnapshotPlan
parameterSnapshotPlan =
  SnapshotPlan
    { spScope = "parameters.current"
    , spFormulaIdentifier = "protocol.snapshot.parameters.v1"
    , spFormula =
        "ABI-decode each catalog getter at one source block; tuple word indexes are defined by the versioned parameter catalog."
    , spCalls = map parameterCall $ orderedParameterGroups parameterCatalog
    }
  where
    parameterCall definitions@(firstDefinition : _) =
      SnapshotCallPlan
        { scpId =
            "parameter."
              <> pdContract firstDefinition
              <> "."
              <> getterName (pdGetter firstDefinition)
        , scpContract = ReleaseContract $ pdContract firstDefinition
        , scpSignature = pdGetter firstDefinition
        , scpArguments = []
        , scpExpectedWordCount =
            1 + maximum (map pdWordIndex definitions)
        , scpFields = map parameterField definitions
        }
    parameterCall [] = error "orderedParameterGroups cannot contain an empty group"

    parameterField definition =
      field
        (pdKey definition)
        (pdWordIndex definition)
        UnsignedWord
        (pdRawScale definition)
        (pdDisplayUnit definition)

accountLedgerSnapshotPlan :: Text -> SnapshotPlan
accountLedgerSnapshotPlan account =
  singleCallPlan
    ("account." <> T.toLower account <> ".ledger")
    "protocol.snapshot.account_ledger.v1"
    "ABI-decode AccountLens.getAccountLedgerSnapshot(address) at the source block."
    SnapshotCallPlan
      { scpId = "account." <> T.toLower account <> ".ledger"
      , scpContract = ReleaseContract "accountLens"
      , scpSignature = "getAccountLedgerSnapshot(address)"
      , scpArguments = [AddressArgument account]
      , scpExpectedWordCount = 23
      , scpFields =
          [ usdc "settlementBalanceUsdc" 0
          , usdc "freeSettlementUsdc" 1
          , usdc "activePositionMarginUsdc" 2
          , usdc "otherLockedMarginUsdc" 3
          , usdc "positionMarginBucketUsdc" 4
          , usdc "committedOrderMarginBucketUsdc" 5
          , usdc "reservedSettlementBucketUsdc" 6
          , usdc "executionBountyReserveUsdc" 7
          , usdc "committedMarginUsdc" 8
          , usdc "traderClaimBalanceUsdc" 9
          , field "pendingOrderCount" 10 UnsignedWord "1" "orders"
          , usdc "closeReachableUsdc" 11
          , usdc "terminalReachableUsdc" 12
          , usdc "accountEquityUsdc" 13
          , usdc "freeBuyingPowerUsdc" 14
          , boolean "hasPosition" 15
          , field "side" 16 Uint8Word "1" "position_side"
          , field "size" 17 UnsignedWord "1e18" "position_size"
          , usdc "marginUsdc" 18
          , field "entryPrice" 19 UnsignedWord "1e8" "index_price"
          , signedUsdc "unrealizedPnlUsdc" 20
          , signedUsdc "netEquityUsdc" 21
          , boolean "liquidatable" 22
          ]
      }

globalSnapshotPlans :: [SnapshotPlan]
globalSnapshotPlans =
  [ housePoolLiquidityPlan
  , protocolStatusPlan
  , sideSnapshotPlan LongSide
  , sideSnapshotPlan ShortSide
  , trancheSnapshotPlan SeniorTranche
  , trancheSnapshotPlan JuniorTranche
  , parameterSnapshotPlan
  ]

decodeSnapshotCall
  :: SnapshotCallPlan
  -> ByteString
  -> Either SnapshotUnavailable [(SnapshotField, SnapshotFieldValue)]
decodeSnapshotCall callPlan bytes
  | BS.null bytes =
      Left $ unavailable "empty_contract_return" Nothing
  | BS.length bytes `mod` abiWordBytes /= 0 =
      Left $
        unavailable
          "malformed_abi_return_bytes"
          (Just $ "observedBytes=" <> showText (BS.length bytes))
  | observedWordCount /= scpExpectedWordCount callPlan =
      Left $
        unavailable
          "malformed_abi_return_word_count"
          ( Just $
              "expectedWords="
                <> showText (scpExpectedWordCount callPlan)
                <> ",observedWords="
                <> showText observedWordCount
          )
  | otherwise = traverse decodeField $ scpFields callPlan
  where
    observedWordCount = BS.length bytes `div` abiWordBytes
    decodeField definition
      | sfWordIndex definition < 0
          || sfWordIndex definition >= scpExpectedWordCount callPlan =
          Left $
            unavailable
              "invalid_snapshot_call_plan"
              (Just $ "field=" <> sfKey definition)
      | otherwise = do
          value <- decodeFieldValue definition $ abiWordAt bytes (sfWordIndex definition)
          pure (definition, value)

buildSnapshot
  :: SnapshotBuildContext
  -> SnapshotPlan
  -> [SnapshotRead]
  -> SnapshotDocument
buildSnapshot context plan snapshotReads =
  SnapshotDocument
    { sdScope = spScope plan
    , sdReleaseId = sbcReleaseId context
    , sdModelVersion = snapshotModelVersion
    , sdCalculationVersion = sbcCalculationVersion context
    , sdFormulaIdentifier = spFormulaIdentifier plan
    , sdFormula = spFormula plan
    , sdSourceBlock = sbcSourceBlock context
    , sdValues = concatMap callValues callResults
    , sdEvidence = map callEvidence callResults
    , sdAvailability =
        sourceAvailability (sbcSourceBlock context)
          <> concatMap callAvailability callResults
    }
  where
    callResults = map evaluateCall $ spCalls plan

    evaluateCall callPlan =
      case find ((== scpId callPlan) . srCallId) snapshotReads of
        Nothing ->
          ( callPlan
          , Nothing
          , Left $ unavailable "call_result_missing" Nothing
          )
        Just SnapshotRead {srResult = Left failure} ->
          (callPlan, Nothing, Left failure)
        Just SnapshotRead {srResult = Right bytes} ->
          (callPlan, Just $ BS.length bytes, decodeSnapshotCall callPlan bytes)

    callValues (_, _, Left _) = []
    callValues (_, _, Right decoded) =
      map
        (\(definition, value) -> (sfKey definition, definition, value))
        decoded

    callEvidence (callPlan, byteLength, result) =
      SnapshotEvidence
        { seCallId = scpId callPlan
        , seContract = scpContract callPlan
        , seSignature = scpSignature callPlan
        , seProvenance =
            case result of
              Right _ -> "exact_historical_contract_read"
              Left _ -> "unavailable"
        , seExpectedWordCount = scpExpectedWordCount callPlan
        , seObservedByteLength = byteLength
        , seSourceBlock = sbcSourceBlock context
        }

    callAvailability (_, _, Right _) = []
    callAvailability (callPlan, _, Left failure) =
      [ SnapshotAvailability
          { saCallId = scpId callPlan
          , saAffectedFields = map sfKey $ scpFields callPlan
          , saReason = suReason failure
          , saDetail = suDetail failure
          }
      ]

snapshotDocumentToJson :: SnapshotDocument -> Value
snapshotDocumentToJson document =
  object
    [ "scope" .= sdScope document
    , "releaseId" .= sdReleaseId document
    , "snapshotModelVersion" .= sdModelVersion document
    , "calculationVersion" .= sdCalculationVersion document
    , "formulaIdentifier" .= sdFormulaIdentifier document
    , "formula" .= sdFormula document
    , "sourceBlock" .= sourceBlockToJson (sdSourceBlock document)
    , "complete" .= null (sdAvailability document)
    , "values" .=
        object
          [ Key.fromText key .= fieldValueToJson definition value
          | (key, definition, value) <- sdValues document
          ]
    , "evidence" .= map snapshotEvidenceToJson (sdEvidence document)
    , "availability" .=
        map snapshotAvailabilityToJson (sdAvailability document)
    ]

snapshotEvidenceToJson :: SnapshotEvidence -> Value
snapshotEvidenceToJson evidence =
  object
    [ "callId" .= seCallId evidence
    , "contract" .= contractName (seContract evidence)
    , "signature" .= seSignature evidence
    , "provenance" .= seProvenance evidence
    , "expectedWordCount" .= seExpectedWordCount evidence
    , "observedByteLength" .= seObservedByteLength evidence
    , "sourceBlock" .= sourceBlockToJson (seSourceBlock evidence)
    ]

snapshotAvailabilityToJson :: SnapshotAvailability -> Value
snapshotAvailabilityToJson availability =
  object
    [ "callId" .= saCallId availability
    , "affectedFields" .= saAffectedFields availability
    , "reason" .= saReason availability
    , "detail" .= saDetail availability
    ]

decodeFieldValue
  :: SnapshotField
  -> ByteString
  -> Either SnapshotUnavailable SnapshotFieldValue
decodeFieldValue definition bytes =
  let unsigned = decodeUint256 bytes
   in case sfEncoding definition of
        UnsignedWord -> Right $ UnsignedValue unsigned
        SignedWord -> Right $ SignedValue $ signed256 unsigned
        BooleanWord
          | unsigned == 0 -> Right $ BooleanValue False
          | unsigned == 1 -> Right $ BooleanValue True
          | otherwise ->
              Left $
                unavailable
                  "malformed_abi_boolean_word"
                  (Just $ "field=" <> sfKey definition <> ",raw=" <> showText unsigned)
        Uint8Word
          | unsigned <= 255 -> Right $ UnsignedValue unsigned
          | otherwise ->
              Left $
                unavailable
                  "malformed_abi_uint8_word"
                  (Just $ "field=" <> sfKey definition <> ",raw=" <> showText unsigned)
        Uint64Word
          | unsigned <= maxUint64 -> Right $ UnsignedValue unsigned
          | otherwise ->
              Left $
                unavailable
                  "malformed_abi_uint64_word"
                  (Just $ "field=" <> sfKey definition <> ",raw=" <> showText unsigned)

fieldValueToJson :: SnapshotField -> SnapshotFieldValue -> Value
fieldValueToJson definition value =
  object
    [ "value" .= snapshotValueToJson value
    , "raw" .= snapshotValueRaw value
    , "rawScale" .= sfRawScale definition
    , "displayUnit" .= sfDisplayUnit definition
    , "encoding" .= encodingName (sfEncoding definition)
    ]

snapshotValueToJson :: SnapshotFieldValue -> Value
snapshotValueToJson = \case
  UnsignedValue value -> String $ showText value
  SignedValue value -> String $ showText value
  BooleanValue value -> Bool value

snapshotValueRaw :: SnapshotFieldValue -> Text
snapshotValueRaw = \case
  UnsignedValue value -> showText value
  SignedValue value -> showText value
  BooleanValue False -> "0"
  BooleanValue True -> "1"

sourceBlockToJson :: SnapshotSourceBlock -> Value
sourceBlockToJson source =
  object
    [ "number" .= showText (ssbNumber source)
    , "hash" .= ssbHash source
    , "timestamp" .= ssbTimestamp source
    ]

sourceAvailability :: SnapshotSourceBlock -> [SnapshotAvailability]
sourceAvailability source =
  [ SnapshotAvailability
      { saCallId = "source-block"
      , saAffectedFields = ["sourceBlock.hash"]
      , saReason = "source_block_hash_unavailable"
      , saDetail = Nothing
      }
  | ssbHash source == Nothing
  ]
    <> [ SnapshotAvailability
          { saCallId = "source-block"
          , saAffectedFields = ["sourceBlock.timestamp"]
          , saReason = "source_block_timestamp_unavailable"
          , saDetail = Nothing
          }
       | ssbTimestamp source == Nothing
       ]

singleCallPlan :: Text -> Text -> Text -> SnapshotCallPlan -> SnapshotPlan
singleCallPlan scope formulaIdentifier formula callPlan =
  SnapshotPlan
    { spScope = scope
    , spFormulaIdentifier = formulaIdentifier
    , spFormula = formula
    , spCalls = [callPlan]
    }

field
  :: Text
  -> Int
  -> SnapshotValueEncoding
  -> Text
  -> Text
  -> SnapshotField
field key wordIndex encoding rawScale displayUnit =
  SnapshotField
    { sfKey = key
    , sfWordIndex = wordIndex
    , sfEncoding = encoding
    , sfRawScale = rawScale
    , sfDisplayUnit = displayUnit
    }

usdc :: Text -> Int -> SnapshotField
usdc key wordIndex =
  field key wordIndex UnsignedWord "1e6" "USDC"

signedUsdc :: Text -> Int -> SnapshotField
signedUsdc key wordIndex =
  field key wordIndex SignedWord "1e6" "USDC"

boolean :: Text -> Int -> SnapshotField
boolean key wordIndex =
  field key wordIndex BooleanWord "1" "boolean"

orderedParameterGroups
  :: [ParameterDefinition]
  -> [[ParameterDefinition]]
orderedParameterGroups definitions =
  reverse groups
  where
    (_, groups) = foldl' addDefinition (Set.empty, []) definitions

    addDefinition
      :: (Set (Text, Text), [[ParameterDefinition]])
      -> ParameterDefinition
      -> (Set (Text, Text), [[ParameterDefinition]])
    addDefinition (seen, currentGroups) definition =
      let key = (pdContract definition, pdGetter definition)
       in if Set.member key seen
            then
              ( seen
              , map
                  (\group ->
                    case group of
                      firstDefinition : _
                        | (pdContract firstDefinition, pdGetter firstDefinition) == key ->
                            group <> [definition]
                      _ -> group)
                  currentGroups
              )
            else (Set.insert key seen, [definition] : currentGroups)

getterName :: Text -> Text
getterName =
  T.map
    (\character ->
      if character `elem` ['A' .. 'Z']
        || character `elem` ['a' .. 'z']
        || character `elem` ['0' .. '9']
        then character
        else '-')

sideName :: MarketSide -> Text
sideName LongSide = "long"
sideName ShortSide = "short"

sideIndex :: MarketSide -> Integer
sideIndex LongSide = 0
sideIndex ShortSide = 1

trancheName :: Tranche -> Text
trancheName SeniorTranche = "senior"
trancheName JuniorTranche = "junior"

contractName :: SnapshotContract -> Text
contractName (ReleaseContract name) = name

encodingName :: SnapshotValueEncoding -> Text
encodingName = \case
  UnsignedWord -> "uint256"
  SignedWord -> "int256"
  BooleanWord -> "bool"
  Uint8Word -> "uint8"
  Uint64Word -> "uint64"

abiWordAt :: ByteString -> Int -> ByteString
abiWordAt bytes index =
  BS.take abiWordBytes $ BS.drop (index * abiWordBytes) bytes

signed256 :: Integer -> Integer
signed256 unsigned
  | unsigned >= twoTo255 = unsigned - twoTo256
  | otherwise = unsigned

unavailable :: Text -> Maybe Text -> SnapshotUnavailable
unavailable reason detail =
  SnapshotUnavailable
    { suReason = reason
    , suDetail = detail
    }

showText :: Show value => value -> Text
showText = T.pack . show

abiWordBytes :: Int
abiWordBytes = 32

maxUint64 :: Integer
maxUint64 = 2 ^ (64 :: Int) - 1

twoTo255 :: Integer
twoTo255 = 2 ^ (255 :: Int)

twoTo256 :: Integer
twoTo256 = 2 ^ (256 :: Int)
