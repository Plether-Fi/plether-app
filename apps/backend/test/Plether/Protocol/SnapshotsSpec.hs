{-# LANGUAGE LambdaCase #-}

module Plether.Protocol.SnapshotsSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Plether.Protocol.Parameters (parameterCatalog)
import Plether.Protocol.Snapshots
import Test.Hspec

spec :: Spec
spec = do
  describe "versioned snapshot call plans" $ do
    it "pins exact ABI word counts for core protocol scopes" $ do
      expectedWords housePoolLiquidityPlan `shouldBe` [11]
      expectedWords protocolStatusPlan `shouldBe` [7]
      expectedWords (sideSnapshotPlan LongSide) `shouldBe` [4]
      expectedWords (sideSnapshotPlan ShortSide) `shouldBe` [4]
      expectedWords (trancheSnapshotPlan SeniorTranche) `shouldBe` [1, 1, 1, 1]
      expectedWords (trancheSnapshotPlan JuniorTranche) `shouldBe` [1, 1, 1, 1]
      expectedWords (accountLedgerSnapshotPlan account) `shouldBe` [23]

    it "describes explicit scales and encodings for every field" $ do
      let fields = concatMap scpFields $ concatMap spCalls globalSnapshotPlans

      fields `shouldSatisfy` all (not . nullText . sfRawScale)
      fields `shouldSatisfy` all (not . nullText . sfDisplayUnit)
      findField "totalAssetsUsdc" housePoolLiquidityPlan
        `shouldBe` SnapshotField "totalAssetsUsdc" 0 UnsignedWord "1e6" "USDC"
      findField "lastMarkPrice" protocolStatusPlan
        `shouldBe` SnapshotField "lastMarkPrice" 1 UnsignedWord "1e8" "index_price"
      findField "openInterest" (sideSnapshotPlan LongSide)
        `shouldBe` SnapshotField "openInterest" 1 UnsignedWord "1e18" "position_size"

    it "scopes account snapshots by normalized address" $ do
      spScope (accountLedgerSnapshotPlan "0xABCDEFabcdefABCDEFabcdefABCDEFabcdefABCD")
        `shouldBe` "account.0xabcdefabcdefabcdefabcdefabcdefabcdefabcd.ledger"
      spScope (accountLedgerSnapshotPlan account)
        `shouldNotBe` spScope (accountLedgerSnapshotPlan "0x2222222222222222222222222222222222222222")

    it "groups tuple parameter getters into one exact call" $ do
      let calls = spCalls parameterSnapshotPlan
          riskCall =
            fromJust $ find ((== "riskParams()") . scpSignature) calls

      scpExpectedWordCount riskCall `shouldBe` 8
      map sfWordIndex (scpFields riskCall) `shouldBe` [0 .. 7]
      sum (map (length . scpFields) calls) `shouldBe` length parameterCatalog

  describe "decodeSnapshotCall" $ do
    it "rejects truncated word arrays without manufacturing zero values" $ do
      let callPlan = onlyCall housePoolLiquidityPlan
          result = decodeSnapshotCall callPlan $ encodeWords [1 .. 10]

      result
        `shouldBe` Left
          SnapshotUnavailable
            { suReason = "malformed_abi_return_word_count"
            , suDetail = Just "expectedWords=11,observedWords=10"
            }

    it "rejects non-word-aligned contract output" $ do
      let result =
            decodeSnapshotCall
              (onlyCall protocolStatusPlan)
              (BS.replicate 33 0)

      result
        `shouldBe` Left
          SnapshotUnavailable
            { suReason = "malformed_abi_return_bytes"
            , suDetail = Just "observedBytes=33"
            }

    it "rejects invalid ABI booleans for the whole call" $ do
      let invalidStatus = [0, 100, 200, 2, 0, 1, 1]
          result =
            decodeSnapshotCall
              (onlyCall protocolStatusPlan)
              (encodeWords invalidStatus)

      result `shouldSatisfy` \case
        Left failure -> suReason failure == "malformed_abi_boolean_word"
        Right _ -> False

    it "decodes int256 account values without losing the sign" $ do
      let accountWords =
            replicate 20 0
              <> [twoTo256 - 25, twoTo256 - 7, 1]
          result =
            decodeSnapshotCall
              (onlyCall $ accountLedgerSnapshotPlan account)
              (encodeWords accountWords)

      result `shouldSatisfy` \case
        Left _ -> False
        Right fields ->
          lookupValue "unrealizedPnlUsdc" fields == Just (SignedValue (-25))
            && lookupValue "netEquityUsdc" fields == Just (SignedValue (-7))
            && lookupValue "liquidatable" fields == Just (BooleanValue True)

  describe "buildSnapshot" $ do
    it "exposes exact evidence plus calculation and source-block metadata" $ do
      let plan = protocolStatusPlan
          document =
            buildSnapshot
              snapshotContext
              plan
              [ SnapshotRead
                  { srCallId = scpId $ onlyCall plan
                  , srResult = Right $ encodeWords [1, 123, 456, 0, 1, 1, 0]
                  }
              ]

      sdCalculationVersion document `shouldBe` "protocol-transparency-v1"
      sdFormulaIdentifier document `shouldBe` "protocol.snapshot.protocol_status.v1"
      sdAvailability document `shouldBe` []
      sdEvidence document `shouldSatisfy` \case
        [evidence] ->
          seProvenance evidence == "exact_historical_contract_read"
            && seObservedByteLength evidence == Just (7 * 32)
            && seSourceBlock evidence == sourceBlock
        _ -> False

      let json = snapshotDocumentToJson document
      lookupObjectField "complete" json `shouldBe` Just (Bool True)
      lookupObjectField "snapshotModelVersion" json
        `shouldBe` Just (String snapshotModelVersion)
      lookupNestedObjectField ["sourceBlock", "number"] json
        `shouldBe` Just (String "123456")
      lookupNestedObjectField ["values", "lastMarkPrice", "rawScale"] json
        `shouldBe` Just (String "1e8")

    it "omits all fields from malformed calls and records a machine-readable reason" $ do
      let document =
            buildSnapshot
              snapshotContext
              housePoolLiquidityPlan
              [ SnapshotRead
                  { srCallId = "house-pool.liquidity"
                  , srResult = Right $ encodeWords [999]
                  }
              ]

      sdValues document `shouldBe` []
      map saReason (sdAvailability document)
        `shouldBe` ["malformed_abi_return_word_count"]
      map seProvenance (sdEvidence document) `shouldBe` ["unavailable"]
      lookupObjectField "complete" (snapshotDocumentToJson document)
        `shouldBe` Just (Bool False)

    it "distinguishes a missing archive read from malformed ABI output" $ do
      let document =
            buildSnapshot
              snapshotContext
              (sideSnapshotPlan ShortSide)
              [ SnapshotRead
                  { srCallId = "market.short.totals"
                  , srResult =
                      Left
                        SnapshotUnavailable
                          { suReason = "archive_state_unavailable"
                          , suDetail = Just "provider does not retain block"
                          }
                  }
              ]

      map saReason (sdAvailability document)
        `shouldBe` ["archive_state_unavailable"]
      sdValues document `shouldBe` []

    it "marks the source hash unavailable rather than inventing one" $ do
      let sourceWithoutHash =
            sourceBlock
              { ssbHash = Nothing
              }
          document =
            buildSnapshot
              snapshotContext {sbcSourceBlock = sourceWithoutHash}
              protocolStatusPlan
              []

      map saReason (sdAvailability document)
        `shouldBe`
          [ "source_block_hash_unavailable"
          , "call_result_missing"
          ]

    it "marks a missing source timestamp unavailable" $ do
      let document =
            buildSnapshot
              snapshotContext
                { sbcSourceBlock =
                    sourceBlock
                      { ssbTimestamp = Nothing
                      }
                }
              protocolStatusPlan
              []

      map saReason (sdAvailability document)
        `shouldBe`
          [ "source_block_timestamp_unavailable"
          , "call_result_missing"
          ]

expectedWords :: SnapshotPlan -> [Int]
expectedWords = map scpExpectedWordCount . spCalls

onlyCall :: SnapshotPlan -> SnapshotCallPlan
onlyCall plan =
  case spCalls plan of
    [callPlan] -> callPlan
    _ -> error "test expected one call"

findField :: Text -> SnapshotPlan -> SnapshotField
findField key plan =
  fromJust $ find ((== key) . sfKey) $ concatMap scpFields $ spCalls plan

lookupValue
  :: Text
  -> [(SnapshotField, SnapshotFieldValue)]
  -> Maybe SnapshotFieldValue
lookupValue key =
  fmap snd . find ((== key) . sfKey . fst)

encodeWords :: [Integer] -> BS.ByteString
encodeWords = foldMap encodeWord

encodeWord :: Integer -> BS.ByteString
encodeWord value =
  let bytes = integerBytes value
   in BS.replicate (32 - BS.length bytes) 0 <> bytes

integerBytes :: Integer -> BS.ByteString
integerBytes 0 = BS.singleton 0
integerBytes value = BS.pack $ reverse $ go value
  where
    go 0 = []
    go current =
      fromIntegral (current `mod` 256) : go (current `div` 256)

lookupObjectField :: Text -> Value -> Maybe Value
lookupObjectField key (Object fields) =
  KeyMap.lookup (Key.fromText key) fields
lookupObjectField _ _ = Nothing

lookupNestedObjectField :: [Text] -> Value -> Maybe Value
lookupNestedObjectField [] value = Just value
lookupNestedObjectField (key : rest) value =
  lookupObjectField key value >>= lookupNestedObjectField rest

nullText :: Text -> Bool
nullText = (== "")

account :: Text
account = "0x1111111111111111111111111111111111111111"

sourceBlock :: SnapshotSourceBlock
sourceBlock =
  SnapshotSourceBlock
    { ssbNumber = 123456
    , ssbHash = Just "0xabc"
    , ssbTimestamp = Just 1_750_000_000
    }

snapshotContext :: SnapshotBuildContext
snapshotContext =
  SnapshotBuildContext
    { sbcReleaseId = "arbitrum-sepolia-2026-07"
    , sbcCalculationVersion = "protocol-transparency-v1"
    , sbcSourceBlock = sourceBlock
    }

twoTo256 :: Integer
twoTo256 = 2 ^ (256 :: Int)
