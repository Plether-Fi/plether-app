{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Plether.Handlers.InsightsOrderProtectionsSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Plether.Ethereum.Rpc (RpcLog (..))
import Plether.Handlers.InsightsOrderProtections
  ( IntentBoundsData (..)
  , LinkedPositionProtection (..)
  , OrderOutcomeMeta (..)
  , OrderProtectionsBundle (..)
  , buildOrderProtectionsForOrder
  , decodeIntentFromRpcLog
  , formatAllowedModes
  , intentRegisteredTopicBytes
  , parsePositionProtectionSnapshot
  , priceWithinTarget
  )
import Test.Hspec

word32 :: Integer -> BS.ByteString
word32 n = BS.pack [fromInteger ((n `div` (256 ^ (31 - i))) `mod` 256) | i <- [0 :: Int .. 31]]

lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object obj) = KM.lookup (Key.fromText k) obj
lookupKey _ _ = Nothing

spec :: Spec
spec = describe "Plether.Handlers.InsightsOrderProtections" $ do
  it "formats allowed execution regime bitmasks" $ do
    formatAllowedModes 1 `shouldBe` "Live only"
    formatAllowedModes 2 `shouldBe` "FAD only"
    formatAllowedModes 3 `shouldBe` "Live + FAD"
    formatAllowedModes 7 `shouldBe` "All regimes"

  it "evaluates inverted plDXY targetPrice bounds for all open/close directions" $ do
    -- Long open: display <= displayTarget <=> raw >= targetPrice
    priceWithinTarget 0 False 94_800_000 94_700_000 `shouldBe` True
    priceWithinTarget 0 False 94_600_000 94_700_000 `shouldBe` False
    -- Short open: display >= displayTarget <=> raw <= targetPrice
    priceWithinTarget 1 False 94_700_000 94_800_000 `shouldBe` True
    priceWithinTarget 1 False 94_900_000 94_800_000 `shouldBe` False
    -- Long close & short close flip signs
    priceWithinTarget 0 True 94_700_000 94_800_000 `shouldBe` True
    priceWithinTarget 1 True 94_800_000 94_700_000 `shouldBe` True

  it "decodes all 25 words from an IntentRegistered RpcLog" $ do
    let uint256MaxVal = (2 :: Integer) ^ (256 :: Int) - 1
        uint32MaxVal = (2 :: Integer) ^ (32 :: Int) - 1
        wordsList =
          [ word32 0x11 -- 0 intentHash
          , word32 200_000 -- 1 executionBountyAssets
          , word32 0x22 -- 2 clientOrderId
          , word32 1 -- 3 side (short)
          , word32 50_000 -- 4 sizeDelta
          , word32 50_000_000 -- 5 marginDelta
          , word32 95_000_000 -- 6 targetPrice
          , word32 0 -- 7 isClose (false)
          , word32 1_795_766_989 -- 8 submitBy
          , word32 60 -- 9 executionWindowSeconds
          , word32 1 -- 10 allowedExecutionModes (Live only)
          , word32 0x33 -- 11 expectedConfigHash
          , word32 210_000 -- 12 maxExecutionBountyAssets
          , word32 uint256MaxVal -- 13 maxExecutionNotionalAssets
          , word32 uint256MaxVal -- 14 maxGrossAccountDebitAssets
          , word32 uint256MaxVal -- 15 maxActionChargeAssets
          , word32 uint256MaxVal -- 16 maxExplicitFeesAssets
          , word32 uint256MaxVal -- 17 maxPostPositionSize
          , word32 0 -- 18 minPostSettlementBalanceAssets
          , word32 0 -- 19 minPostPositionEquityAssets
          , word32 uint32MaxVal -- 20 maxPostLeverageBps
          , word32 1_795_766_989 -- 21 timing.submitBy
          , word32 60 -- 22 timing.executionWindowSeconds
          , word32 1_795_766_878 -- 23 timing.commitTimestamp
          , word32 1_795_766_938 -- 24 timing.executionDeadline
          ]
        rpcLog =
          RpcLog
            { rpcLogTxHash = "0x1111111111111111111111111111111111111111111111111111111111111111"
            , rpcLogBlockNumber = 333100195
            , rpcLogBlockHash = "0x2222222222222222222222222222222222222222222222222222222222222222"
            , rpcLogTransactionIndex = 1
            , rpcLogIndex = 2
            , rpcLogAddress = "0x327d5fd04e17d96dd5c4f845a0b4256a9e19645c"
            , rpcLogTopics = [intentRegisteredTopicBytes, word32 62301, word32 0xabc, word32 0xdef]
            , rpcLogData = BS.concat wordsList
            }
    case decodeIntentFromRpcLog rpcLog of
      Nothing -> expectationFailure "Expected IntentRegistered RpcLog to decode"
      Just decoded -> do
        ibdOrderId decoded `shouldBe` 62301
        ibdSide decoded `shouldBe` Just 1
        ibdIsClose decoded `shouldBe` Just False
        ibdTargetPrice decoded `shouldBe` Just 95_000_000
        ibdExecutionWindowSeconds decoded `shouldBe` Just 60
        ibdAllowedExecutionModes decoded `shouldBe` Just 1
        ibdMaxExecutionBountyAssets decoded `shouldBe` Just 210_000
        ibdMaxPostLeverageBps decoded `shouldBe` Just uint32MaxVal
        ibdSource decoded `shouldBe` "commit_receipt"

  it "builds a checklist with 5 passed web core guards, 8 unbounded agent bounds, and attached TP/SL" $ do
    let uint256MaxVal = (2 :: Integer) ^ (256 :: Int) - 1
        uint32MaxVal = (2 :: Integer) ^ (32 :: Int) - 1
        intent =
          IntentBoundsData
            { ibdOrderId = 62301
            , ibdSide = Just 1
            , ibdIsClose = Just False
            , ibdTargetPrice = Just 95_000_000
            , ibdSubmitBy = Just 1_795_766_989
            , ibdExecutionWindowSeconds = Just 60
            , ibdCommitTimestamp = Just 1_795_766_878
            , ibdExecutionDeadline = Just 1_795_766_938
            , ibdAllowedExecutionModes = Just 1
            , ibdExpectedConfigHash = Just "0xff9a3bbf31030658aa5644732816f101ba033ba1b980f84bf30c0fa408072805"
            , ibdExecutionBountyAssets = Just 200_000
            , ibdMaxExecutionBountyAssets = Just 210_000
            , ibdMaxExecutionNotionalAssets = Just uint256MaxVal
            , ibdMaxGrossAccountDebitAssets = Just uint256MaxVal
            , ibdMaxActionChargeAssets = Just uint256MaxVal
            , ibdMaxExplicitFeesAssets = Just uint256MaxVal
            , ibdMaxPostPositionSize = Just uint256MaxVal
            , ibdMinPostSettlementBalanceAssets = Just 0
            , ibdMinPostPositionEquityAssets = Just 0
            , ibdMaxPostLeverageBps = Just uint32MaxVal
            , ibdSource = "commit_receipt"
            }
        outcome =
          OrderOutcomeMeta
            { oomOrderId = 62301
            , oomSide = Just 1
            , oomCommitTxHash = Just "0xcommit"
            , oomCommitTimestamp = Just 1_795_766_878
            , oomTerminalTimestamp = Just 1_795_766_880
            , oomTerminalStatus = "Executed"
            , oomTerminalReason = Just "Executed"
            , oomExecutionMode = Just "Live"
            , oomFailedConstraint = Nothing
            , oomExecutionPrice = Just 94_756_243
            , oomReceipt =
                Just $
                  object
                    [ "executionBountyAssets" .= ("200000" :: Text)
                    , "executionNotionalAssets" .= ("49936540061" :: Text)
                    , "grossAccountDebitAssets" .= ("200000" :: Text)
                    , "actionChargeAssessedAssets" .= ("0" :: Text)
                    , "executionFeeAssets" .= ("19974616" :: Text)
                    , "frozenSpreadAssets" .= ("0" :: Text)
                    , "postPositionSize" .= ("52700000000000000000000" :: Text)
                    , "postSettlementBalanceAssets" .= ("99589268697" :: Text)
                    , "postPositionEquityAssets" .= ("49950063460" :: Text)
                    , "postLeverageBps" .= ("9998" :: Text)
                    ]
            }
        protSnap =
          object
            [ "protectionId" .= ("42" :: Text)
            , "parentOrderId" .= ("62301" :: Text)
            , "linkedOrderId" .= ("0" :: Text)
            , "status" .= (2 :: Int)
            , "statusName" .= ("Armed" :: Text)
            , "triggeredLeg" .= (0 :: Int)
            , "triggeredLegName" .= ("None" :: Text)
            , "takeProfitTriggerPrice" .= ("92500000" :: Text)
            , "stopLossTriggerPrice" .= ("96500000" :: Text)
            , "triggerBountyAssets" .= ("200000" :: Text)
            , "armedBlock" .= ("333100202" :: Text)
            ]
        mProt = parsePositionProtectionSnapshot "42" (Just "333100202") (Just "0xtx") protSnap
    case mProt of
      Nothing -> expectationFailure "Expected TP/SL snapshot to parse"
      Just prot -> do
        lppStatus prot `shouldBe` "Armed"
        lppTakeProfitTriggerPrice prot `shouldBe` Just "92500000"
        lppStopLossTriggerPrice prot `shouldBe` Just "96500000"
        let bundle =
              OrderProtectionsBundle
                { opbIntents = Map.singleton 62301 intent
                , opbOutcomes = Map.singleton 62301 outcome
                , opbPositionProtections = Map.singleton 62301 prot
                }
        case buildOrderProtectionsForOrder bundle 62301 of
          Nothing -> expectationFailure "Expected orderProtections payload"
          Just val -> do
            let mSummary = lookupKey "summary" val
            (mSummary >>= lookupKey "v3EnforcedCount") `shouldBe` Just (Number 5)
            (mSummary >>= lookupKey "v3PassedCount") `shouldBe` Just (Number 5)
            (mSummary >>= lookupKey "v3FailedCount") `shouldBe` Just (Number 0)
            (mSummary >>= lookupKey "v3UnboundedCount") `shouldBe` Just (Number 8)
            (mSummary >>= lookupKey "hasTakeProfit") `shouldBe` Just (Bool True)
            (mSummary >>= lookupKey "hasStopLoss") `shouldBe` Just (Bool True)
            (mSummary >>= lookupKey "positionProtectionStatus") `shouldBe` Just (String "Armed")
            case lookupKey "executionChecks" val of
              Just (Array arr) -> length (toList arr) `shouldBe` 13
              _ -> expectationFailure "Expected 13 executionChecks"

  it "flags a violated strict leverage bound as failed" $ do
    let intent =
          IntentBoundsData
            { ibdOrderId = 62302
            , ibdSide = Just 0
            , ibdIsClose = Just False
            , ibdTargetPrice = Just 94_000_000
            , ibdSubmitBy = Just 1_795_766_989
            , ibdExecutionWindowSeconds = Just 60
            , ibdCommitTimestamp = Just 1_795_766_878
            , ibdExecutionDeadline = Just 1_795_766_938
            , ibdAllowedExecutionModes = Just 1
            , ibdExpectedConfigHash = Just "0xff9a3bbf31030658aa5644732816f101ba033ba1b980f84bf30c0fa408072805"
            , ibdExecutionBountyAssets = Just 200_000
            , ibdMaxExecutionBountyAssets = Just 210_000
            , ibdMaxExecutionNotionalAssets = Just 100_000_000_000
            , ibdMaxGrossAccountDebitAssets = Just 50_000_000_000
            , ibdMaxActionChargeAssets = Just 5_000_000
            , ibdMaxExplicitFeesAssets = Just 25_000_000
            , ibdMaxPostPositionSize = Just 100_000_000_000_000_000_000_000
            , ibdMinPostSettlementBalanceAssets = Just 1_000_000
            , ibdMinPostPositionEquityAssets = Just 1_000_000
            , ibdMaxPostLeverageBps = Just 20_000
            , ibdSource = "indexed_event"
            }
        outcome =
          OrderOutcomeMeta
            { oomOrderId = 62302
            , oomSide = Just 0
            , oomCommitTxHash = Just "0xcommit2"
            , oomCommitTimestamp = Just 1_795_766_878
            , oomTerminalTimestamp = Just 1_795_766_882
            , oomTerminalStatus = "Failed"
            , oomTerminalReason = Just "Constraint violation"
            , oomExecutionMode = Just "Live"
            , oomFailedConstraint = Just "Post leverage"
            , oomExecutionPrice = Just 94_500_000
            , oomReceipt = Nothing
            }
        bundle =
          OrderProtectionsBundle
            { opbIntents = Map.singleton 62302 intent
            , opbOutcomes = Map.singleton 62302 outcome
            , opbPositionProtections = Map.empty
            }
    case buildOrderProtectionsForOrder bundle 62302 of
      Nothing -> expectationFailure "Expected orderProtections payload"
      Just val -> do
        let mSummary = lookupKey "summary" val
        (mSummary >>= lookupKey "v3FailedCount") `shouldBe` Just (Number 1)
