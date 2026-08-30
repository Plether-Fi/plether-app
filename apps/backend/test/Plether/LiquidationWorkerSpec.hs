{-# LANGUAGE RecordWildCards #-}

module Plether.LiquidationWorkerSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (Value, object, toJSON, (.=))
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Plether.Config
  ( Config (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  )
import Plether.Database.Schema
  ( BasketSnapshotRow (..)
  , PythUpdatePayloadRow (..)
  )
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256)
import Plether.Ethereum.Client (RpcError (..))
import Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( AccountLedgerSnapshot (..)
  , getAccountLedgerSnapshotCall
  )
import Plether.Ethereum.Contracts.Perps
  ( LiquidationBatchResult (..)
  , lbiAccount
  , lbiResult
  , liquidationBatchItemTopic
  , liquidationBatchStoppedTopic
  , positionLiquidatedTopic
  )
import qualified Plether.Ethereum.Multicall as Multicall
import Plether.Ethereum.Rpc (RpcLog (..), TxReceipt (..))
import Plether.Ethereum.Transaction (Tx1559 (..))
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , july2026Competition
  )
import Plether.LiquidationWorker
  ( FreshLiquidationRiskInputs (..)
  , LiquidationBatchProgress (..)
  , LiquidationBasketComponent (..)
  , LiquidationRiskGlobals (..)
  , PythStoredPrice (..)
  , LiquidationPayloadCircuitDecision (..)
  , LiquidationPendingSignerAction (..)
  , LiquidationSignerCircuitDecision (..)
  , LiquidationWorkerConfig (..)
  , canAffordTransaction
  , checkLiveSignerBalance
  , decodeCachedPythPayload
  , decodeCachedLiquidationComponents
  , decodePythStoredPriceResults
  , freshLiquidationRiskInputsFromCache
  , isExpectedLiquidationSimulationRevert
  , isInsufficientFundsRpcError
  , isLiquidationReceiptFor
  , decodeLiquidationSnapshotResults
  , liquidationIndexRange
  , liquidationPayloadCircuitDecision
  , liquidationPayloadFingerprint
  , liquidationPendingSignerAction
  , liquidationSignerCircuitDecision
  , liquidationSnapshotCalls
  , mergeLiquidationBasketComponents
  , payloadGlobalSimulationRevertSelector
  , loadLiquidationWorkerConfig
  , sameNonceReplacementFees
  , selectLiquidationSimulationCandidates
  , pythStoredPriceCalls
  , transactionMaximumCost
  , validateLiquidationBatchReceipt
  , validateMergedLiquidationBasket
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = do
  describe "loadLiquidationWorkerConfig" $ do
    it "uses the CFD engine already resolved by shared backend configuration" $ do
      withUnsetEnv "PERPS_CFD_ENGINE" $ do
        workerCfg <- loadLiquidationWorkerConfig testConfig "private-key"
        lwcCfdEngine workerCfg `shouldBe` configuredCfdEngine
        lwcCfdEngine workerCfg `shouldNotBe` retiredCfdEngine

    it "polls every ten minutes by default" $ do
      withUnsetEnv "LIQUIDATION_WORKER_POLL_SECONDS" $ do
        workerCfg <- loadLiquidationWorkerConfig testConfig "private-key"
        lwcPollSeconds workerCfg `shouldBe` 600

    it "inherits the account lens and defaults to bounded scan, read, and execution batches" $ do
      withUnsetEnv "PERPS_ACCOUNT_LENS" $
        withUnsetEnv "LIQUIDATION_WORKER_SCAN_BATCH_SIZE" $
          withUnsetEnv "LIQUIDATION_WORKER_MULTICALL_SIZE" $
            withUnsetEnv "LIQUIDATION_WORKER_EXECUTION_BATCH_SIZE" $ do
              workerCfg <- loadLiquidationWorkerConfig testConfig "private-key"
              lwcAccountLens workerCfg `shouldBe` configuredAccountLens
              lwcScanBatchSize workerCfg `shouldBe` 1_000
              lwcMulticallSize workerCfg `shouldBe` 10
              lwcExecutionBatchSize workerCfg `shouldBe` 20

    it "clamps execution batches to the router's 256-account limit" $ do
      withEnv "LIQUIDATION_WORKER_EXECUTION_BATCH_SIZE" "999" $ do
        workerCfg <- loadLiquidationWorkerConfig testConfig "private-key"
        lwcExecutionBatchSize workerCfg `shouldBe` 256

  describe "liquidation snapshot batching" $ do
    it "builds ordered, allow-failure account-lens calls" $ do
      let accounts = [account, otherAccount]
          calls = liquidationSnapshotCalls accountLens accounts
      map Multicall.callTarget calls `shouldBe` replicate 2 accountLens
      map Multicall.callAllowFailure calls `shouldBe` [True, True]
      map Multicall.callCalldata calls
        `shouldBe` map getAccountLedgerSnapshotCall accounts

    it "decodes successful snapshots without changing account order" $ do
      let results =
            [ Multicall.CallResult True $ encodedAccountSnapshot openHealthySnapshot
            , Multicall.CallResult True $ encodedAccountSnapshot openRiskySnapshot
            ]
      case decodeLiquidationSnapshotResults 2 results of
        Right [Right first, Right second] ->
          map alsSize [first, second] `shouldBe` [alsSize openHealthySnapshot, alsSize openRiskySnapshot]
        other -> expectationFailure $ "expected two ordered snapshots, got " <> show other

    it "isolates failed and malformed subcalls instead of treating them as safe" $ do
      let results =
            [ Multicall.CallResult True $ encodedAccountSnapshot openHealthySnapshot
            , Multicall.CallResult False BS.empty
            , Multicall.CallResult True BS.empty
            ]
      case decodeLiquidationSnapshotResults 3 results of
        Right [Right _, Left _, Left _] -> pure ()
        other -> expectationFailure $ "expected isolated subcall failures, got " <> show other

    it "rejects missing or extra Multicall results" $ do
      decodeLiquidationSnapshotResults
        2
        [Multicall.CallResult True $ encodedAccountSnapshot openHealthySnapshot]
        `shouldSatisfy` isLeft
      decodeLiquidationSnapshotResults
        1
        [ Multicall.CallResult True $ encodedAccountSnapshot openHealthySnapshot
        , Multicall.CallResult True $ encodedAccountSnapshot openRiskySnapshot
        ]
        `shouldSatisfy` isLeft

  describe "selectLiquidationSimulationCandidates" $ do
    it "excludes a definitive no-position snapshot even if its liquidatable bit is stale" $ do
      let staleClosedSnapshot = closedSnapshot {alsLiquidatable = True}
      selectLiquidationSimulationCandidates
        (Just neutralRiskInputs)
        [("closed" :: Text, Right staleClosedSnapshot)]
        `shouldBe` []

    it "uses the submitted fresh price instead of a stale stored-risk flag" $ do
      let freshPriceRisk =
            openHealthySnapshot
              { alsAccountEquityUsdc = 2_000_000
              , alsNetEquityUsdc = 2_000_000
              , alsLiquidatable = False
              }
          storedRisk = openHealthySnapshot {alsLiquidatable = True}
      selectLiquidationSimulationCandidates
        (Just adverseRiskInputs)
        [ ("closed" :: Text, Right closedSnapshot)
        , ("healthy", Right openHealthySnapshot)
        , ("fresh-price-risk", Right freshPriceRisk)
        , ("stored-risk", Right storedRisk)
        ]
        `shouldBe` ["fresh-price-risk"]

    it "includes the exact maintenance boundary but excludes a position safely above it" $ do
      let atBoundary =
            openHealthySnapshot
              { alsAccountEquityUsdc = 1_000_000
              , alsNetEquityUsdc = 1_000_000
              , alsLiquidatable = False
              }
          aboveBoundary =
            atBoundary
              { alsAccountEquityUsdc = 1_000_010
              , alsNetEquityUsdc = 1_000_010
              }
      selectLiquidationSimulationCandidates
        (Just neutralRiskInputs)
        [ ("at-boundary" :: Text, Right atBoundary)
        , ("above-boundary", Right aboveBoundary)
        ]
        `shouldBe` ["at-boundary"]

    it "does not estimate unreadable snapshots or positions with inconsistent risk inputs" $ do
      selectLiquidationSimulationCandidates
        (Just invalidRiskInputs)
        [ ("closed" :: Text, Right closedSnapshot)
        , ("open", Right openHealthySnapshot)
        , ("snapshot-failed", Left "Multicall subcall failed")
        ]
        `shouldBe` []

    it "uses only the affirmative stored-risk signal when fresh risk inputs are unavailable" $ do
      selectLiquidationSimulationCandidates
        Nothing
        [ ("closed" :: Text, Right closedSnapshot)
        , ("healthy", Right openHealthySnapshot)
        , ("risky", Right openRiskySnapshot)
        , ("snapshot-failed", Left "Multicall subcall failed")
        ]
        `shouldBe` ["risky"]

  describe "freshLiquidationRiskInputsFromCache" $ do
    it "correlates the full publish-time vector and reconstructs basket price and confidence" $ do
      freshLiquidationRiskInputsFromCache
        payload
        basketSnapshot
        200_000_000
        500
        15_000
        `shouldBe` Right
          FreshLiquidationRiskInputs
            { flriNeutralPrice = 100_000_000
            , flriBasketConfidence = 1_000
            , flriCapPrice = 200_000_000
            , flriRequiredMarginBps = 500
            , flriAdverseConfidenceMultiplierBps = 15_000
            , flriRiskBufferBps = 5
            }

    it "rejects mismatched component counts, publish times, and reconstructed prices" $ do
      freshLiquidationRiskInputsFromCache
        payload {puprPublishTimes = toJSON ([101] :: [Integer]), puprMaxPublishTime = 101}
        basketSnapshot
        200_000_000
        500
        15_000
        `shouldSatisfy` isLeft
      freshLiquidationRiskInputsFromCache
        payload
        basketSnapshot {bsrComponents = toJSON mismatchedPublishTimeComponents}
        200_000_000
        500
        15_000
        `shouldSatisfy` isLeft
      freshLiquidationRiskInputsFromCache
        payload
        basketSnapshot {bsrBasketPrice = 99_999_999}
        200_000_000
        500
        15_000
        `shouldSatisfy` isLeft

    it "rejects a non-positive raw component price instead of understating confidence" $ do
      freshLiquidationRiskInputsFromCache
        payload
        basketSnapshot {bsrComponents = toJSON nonPositiveRawPriceComponents}
        200_000_000
        500
        15_000
        `shouldSatisfy` isLeft

  describe "Pyth stored-price merge" $ do
    it "builds one ordered getPriceUnsafe subcall per cached component" $ do
      case decodeCachedLiquidationComponents payload basketSnapshot of
        Left err -> expectationFailure $ "expected valid cached components: " <> show err
        Right components ->
          case pythStoredPriceCalls pythContract components of
            Left err -> expectationFailure $ "expected valid Pyth calls: " <> show err
            Right calls -> do
              map Multicall.callTarget calls `shouldBe` replicate 2 pythContract
              map Multicall.callAllowFailure calls `shouldBe` [True, True]
              map Multicall.callCalldata calls
                `shouldBe`
                  [ encodeCall "getPriceUnsafe(bytes32)" [feedA]
                  , encodeCall "getPriceUnsafe(bytes32)" [feedB]
                  ]

    it "strictly decodes the four-word Pyth Price struct" $ do
      let encoded =
            signedWord 123_000_000
              <> encodeUint256 456
              <> signedWord (-8)
              <> encodeUint256 789
      decodePythStoredPriceResults 1 [Multicall.CallResult True encoded]
        `shouldBe`
          Right
            [ PythStoredPrice
                { pspPrice = 123_000_000
                , pspConfidence = 456
                , pspExponent = -8
                , pspPublishTime = 789
                }
            ]
      decodePythStoredPriceResults 1 [Multicall.CallResult True BS.empty]
        `shouldSatisfy` isLeft

    it "reproduces Pyth update semantics component by component" $ do
      case decodeCachedLiquidationComponents payload basketSnapshot of
        Left err -> expectationFailure $ "expected valid cached components: " <> show err
        Right components ->
          case
              mergeLiquidationBasketComponents
                components
                [ PythStoredPrice 300_000_000 3_000 (-8) 103
                , PythStoredPrice 90_000_000 900 (-8) 100
                ]
            of
            Left err -> expectationFailure $ "expected a merged basket: " <> show err
            Right merged -> do
              -- Stored data wins for the first feed because it is newer; the
              -- submitted cached update wins for the second feed.
              map lbcPrice merged `shouldBe` [300_000_000, 100_000_000]
              map lbcPublishTime merged `shouldBe` [103, 101]

    it "matches on-chain freshness, confidence, and publish-order policy" $ do
      case decodeCachedLiquidationComponents payload basketSnapshot of
        Left err -> expectationFailure $ "expected valid cached components: " <> show err
        Right components -> do
          validateMergedLiquidationBasket exactRiskGlobals components `shouldBe` Right ()
          validateMergedLiquidationBasket
            exactRiskGlobals {lrgBlockTimestamp = 101}
            components
            `shouldSatisfy` isLeft
          validateMergedLiquidationBasket
            exactRiskGlobals {lrgBlockTimestamp = 113}
            components
            `shouldSatisfy` isLeft
          validateMergedLiquidationBasket
            exactRiskGlobals {lrgLastMarkTime = 102}
            components
            `shouldSatisfy` isLeft
          let wideConfidence =
                case components of
                  first : rest -> first {lbcConfidence = 1_000_000} : rest
                  [] -> []
          validateMergedLiquidationBasket exactRiskGlobals wideConfidence
            `shouldSatisfy` isLeft

  describe "decodeCachedPythPayload" $ do
    it "decodes the latest cached publish times and update bytes" $ do
      decodeCachedPythPayload payload
        `shouldBe` Right ([101, 102], [BS.pack [0x01, 0x02], BS.pack [0xff]])

    it "rejects invalid update hex" $ do
      decodeCachedPythPayload payload {puprUpdateData = toJSON (["0xzz"] :: [String])}
        `shouldSatisfy` isLeft

  describe "isLiquidationReceiptFor" $ do
    it "accepts a successful matching PositionLiquidated event" $ do
      isLiquidationReceiptFor cfdEngine account (receipt True cfdEngine account) `shouldBe` True

    it "rejects failed receipts, another account, and another emitter" $ do
      isLiquidationReceiptFor cfdEngine account (receipt False cfdEngine account) `shouldBe` False
      isLiquidationReceiptFor cfdEngine account (receipt True cfdEngine otherAccount) `shouldBe` False
      isLiquidationReceiptFor cfdEngine account (receipt True otherEngine account) `shouldBe` False

  describe "validateLiquidationBatchReceipt" $ do
    it "accepts a complete ordered batch with isolated per-account outcomes" $ do
      let batchReceipt =
            receiptWithLogs
              True
              [ liquidationBatchItemLog 0 account LiquidationBatchLiquidated 25 BS.empty
              , liquidationBatchItemLog 1 otherAccount LiquidationBatchFailed 0 (BS.pack [0xde, 0xad, 0xbe, 0xef])
              ]
      case validateLiquidationBatchReceipt orderRouter [account, otherAccount] batchReceipt of
        Right progress -> do
          lbpNextIndex progress `shouldBe` 2
          map lbiResult (lbpItems progress)
            `shouldBe` [LiquidationBatchLiquidated, LiquidationBatchFailed]
        Left err -> expectationFailure $ "unexpected receipt validation error: " <> show err

    it "accepts a gas-bounded attempted prefix and leaves the suffix retryable" $ do
      let batchReceipt =
            receiptWithLogs
              True
              [ liquidationBatchItemLog 0 account LiquidationBatchSkippedSolvent 0 BS.empty
              , liquidationBatchStoppedLog 1
              ]
      case validateLiquidationBatchReceipt orderRouter [account, otherAccount] batchReceipt of
        Right progress -> do
          lbpNextIndex progress `shouldBe` 1
          map lbiAccount (lbpItems progress) `shouldBe` [account]
        Left err -> expectationFailure $ "unexpected receipt validation error: " <> show err

    it "reconciles persisted batches independently of database row order" $ do
      let batchReceipt =
            receiptWithLogs
              True
              [ liquidationBatchItemLog 0 account LiquidationBatchSkippedSolvent 0 BS.empty
              , liquidationBatchItemLog 1 otherAccount LiquidationBatchSkippedNoPosition 0 BS.empty
              ]
      case validateLiquidationBatchReceipt orderRouter [otherAccount, account] batchReceipt of
        Right progress -> lbpNextIndex progress `shouldBe` 2
        Left err -> expectationFailure $ "unexpected receipt validation error: " <> show err

    it "rejects gaps, foreign accounts, and inconsistent stop indices" $ do
      validateLiquidationBatchReceipt
        orderRouter
        [account, otherAccount]
        (receiptWithLogs True [liquidationBatchItemLog 1 otherAccount LiquidationBatchSkippedSolvent 0 BS.empty])
        `shouldSatisfy` isLeft
      validateLiquidationBatchReceipt
        orderRouter
        [account, otherAccount]
        ( receiptWithLogs
            True
            [liquidationBatchItemLog 0 foreignAccount LiquidationBatchSkippedSolvent 0 BS.empty]
        )
        `shouldSatisfy` isLeft
      validateLiquidationBatchReceipt
        orderRouter
        [account, otherAccount]
        ( receiptWithLogs
            True
            [ liquidationBatchItemLog 0 account LiquidationBatchSkippedSolvent 0 BS.empty
            , liquidationBatchStoppedLog 2
            ]
        )
        `shouldSatisfy` isLeft

  describe "isExpectedLiquidationSimulationRevert" $ do
    it "accepts the exact solvent and no-position engine reverts" $ do
      isExpectedLiquidationSimulationRevert
        (RpcNodeError 3 "execution reverted" (Just "0x451cebb2"))
        `shouldBe` True
      isExpectedLiquidationSimulationRevert
        (RpcNodeError 3 "execution reverted" (Just "0x4565ea0c"))
        `shouldBe` True
      isExpectedLiquidationSimulationRevert
        (RpcNodeError 3 "execution reverted: 0x451CEBB2" Nothing)
        `shouldBe` True

    it "rejects transport, oracle, and unstructured node errors" $ do
      isExpectedLiquidationSimulationRevert (RpcHttpError "timeout") `shouldBe` False
      isExpectedLiquidationSimulationRevert
        (RpcNodeError 3 "execution reverted" (Just "0xf4a25e0f"))
        `shouldBe` False
      isExpectedLiquidationSimulationRevert
        (RpcNodeError (-32000) "insufficient funds" Nothing)
        `shouldBe` False

  describe "payloadGlobalSimulationRevertSelector" $ do
    it "classifies the observed invalid-VAA and stale-price payload failures" $ do
      payloadGlobalSimulationRevertSelector
        (RpcNodeError 3 "execution reverted" (Just "0x2ACBE915"))
        `shouldBe` Just "0x2acbe915"
      payloadGlobalSimulationRevertSelector
        (RpcNodeError 3 "execution reverted" (Just "nested data: 0xf4a25e0f00"))
        `shouldBe` Just "0xf4a25e0f"
      payloadGlobalSimulationRevertSelector
        (RpcNodeError 3 "execution reverted: 0x2ACBE915" Nothing)
        `shouldBe` Just "0x2acbe915"

    it "does not classify transport, fee, or unrelated simulation failures" $ do
      payloadGlobalSimulationRevertSelector (RpcHttpError "timeout") `shouldBe` Nothing
      payloadGlobalSimulationRevertSelector
        (RpcNodeError (-32000) "insufficient funds for gas * price + value" Nothing)
        `shouldBe` Nothing
      payloadGlobalSimulationRevertSelector
        (RpcNodeError 3 "execution reverted" (Just "0x451cebb2"))
        `shouldBe` Nothing

  describe "liquidationPayloadFingerprint" $ do
    it "is stable across equivalent address formatting" $ do
      liquidationPayloadFingerprint "  0xABC  " "0xDEF" updateData
        `shouldBe` liquidationPayloadFingerprint "0xabc" "0xdef" updateData

    it "changes with either contract domain or update byte boundaries" $ do
      let fingerprint = liquidationPayloadFingerprint "0xabc" "0xdef" updateData
      fingerprint `shouldNotBe` liquidationPayloadFingerprint "0xabd" "0xdef" updateData
      fingerprint `shouldNotBe` liquidationPayloadFingerprint "0xabc" "0xdee" updateData
      fingerprint
        `shouldNotBe` liquidationPayloadFingerprint
          "0xabc"
          "0xdef"
          [BS.pack [0x01, 0x02], BS.pack [0x03]]

  describe "liquidationPayloadCircuitDecision" $ do
    it "processes without a rejection and suppresses only the same rejected key" $ do
      liquidationPayloadCircuitDecision Nothing "0xaaa"
        `shouldBe` ProcessLiquidationPayload
      liquidationPayloadCircuitDecision (Just "0xAaA") "0xaaa"
        `shouldBe` SuppressRejectedLiquidationPayload

    it "clears the circuit and resumes immediately for a new key" $ do
      liquidationPayloadCircuitDecision (Just "0xaaa") "0xbbb"
        `shouldBe` ClearRejectedLiquidationPayload

  describe "liquidationSignerCircuitDecision" $ do
    it "allows one freshly priced attempt when the persisted cooldown is due" $ do
      liquidationSignerCircuitDecision Nothing `shouldBe` SignerTransactionReady
      liquidationSignerCircuitDecision (Just True) `shouldBe` RecheckSignerTransaction

    it "suppresses new signer transactions during the persistent cooldown" $ do
      liquidationSignerCircuitDecision (Just False) `shouldBe` SuppressSignerTransaction

    it "keeps the persisted raw transaction live while replacement is suppressed" $ do
      liquidationPendingSignerAction False True `shouldBe` RebroadcastPendingSignerTransaction
      liquidationPendingSignerAction False False `shouldBe` WaitForPendingSignerTransaction
      liquidationPendingSignerAction True False `shouldBe` ReplacePendingSignerTransaction

  describe "isInsufficientFundsRpcError" $ do
    it "classifies deterministic node funding failures from message or data" $ do
      isInsufficientFundsRpcError
        (RpcNodeError (-32000) "insufficient funds for gas * price + value" Nothing)
        `shouldBe` True
      isInsufficientFundsRpcError
        (RpcNodeError 3 "transaction rejected" (Just "insufficient balance for transfer"))
        `shouldBe` True

    it "does not turn transport or unrelated node failures into a signer circuit" $ do
      isInsufficientFundsRpcError (RpcHttpError "timeout") `shouldBe` False
      isInsufficientFundsRpcError
        (RpcNodeError 3 "execution reverted" (Just "0x2acbe915"))
        `shouldBe` False

  describe "liquidationIndexRange" $ do
    it "starts at the configured deployment block and respects confirmations" $ do
      liquidationIndexRange 100 1 5_000 12 0 200 `shouldBe` Just (100, 199)

    it "rescans a bounded overlap after catching up" $ do
      liquidationIndexRange 100 1 5_000 12 150 155 `shouldBe` Just (139, 154)

    it "still advances when the configured batch size is one" $ do
      liquidationIndexRange 100 1 1 12 150 155 `shouldBe` Just (151, 151)

    it "waits when the configured start has not reached confirmation depth" $ do
      liquidationIndexRange 200 1 5_000 12 0 150 `shouldBe` Nothing

  describe "sameNonceReplacementFees" $ do
    it "bumps both prior fee fields by at least 12.5 percent" $ do
      sameNonceReplacementFees 2_500 50 1 10 100 `shouldBe` (12, 113)

    it "uses current buffered network fees when they are higher" $ do
      sameNonceReplacementFees 2_500 200 5 10 100 `shouldBe` (12, 250)

  describe "checkLiveSignerBalance" $ do
    it "does not evaluate the balance request in dry-run mode" $ do
      called <- newIORef False
      result <-
        checkLiveSignerBalance True $ do
          writeIORef called True
          pure $ Right 1
      result `shouldBe` Right Nothing
      readIORef called `shouldReturn` False

    it "fails closed for an RPC error or a zero balance" $ do
      rpcFailure <- checkLiveSignerBalance False $ pure $ Left $ RpcHttpError "timeout"
      zeroBalance <- checkLiveSignerBalance False $ pure $ Right 0
      rpcFailure `shouldSatisfy` isLeft
      zeroBalance `shouldSatisfy` isLeft

    it "returns a positive live signer balance" $ do
      checkLiveSignerBalance False (pure $ Right 123) `shouldReturn` Right (Just 123)

  describe "transaction affordability" $ do
    it "includes value and maximum EIP-1559 gas cost" $ do
      transactionMaximumCost maximumCostTx `shouldBe` 2_100_007

    it "accepts the exact maximum cost and rejects one wei less" $ do
      canAffordTransaction 2_100_007 maximumCostTx `shouldBe` True
      canAffordTransaction 2_100_006 maximumCostTx `shouldBe` False

payload :: PythUpdatePayloadRow
payload =
  PythUpdatePayloadRow
    { puprMinPublishTime = 101
    , puprMaxPublishTime = 102
    , puprPublishTimes = toJSON ([101, 102] :: [Integer])
    , puprUpdateData = toJSON (["0x0102", "0xff"] :: [String])
    , puprFetchedAt = 103
    , puprSource = "backend_hermes_latest_v2"
    }

basketSnapshot :: BasketSnapshotRow
basketSnapshot =
  BasketSnapshotRow
    { bsrTimestamp = 103
    , bsrIntervalSeconds = 30
    , bsrBasketPrice = 100_000_000
    -- Deliberately reverse publish-time order: cache correlation must compare
    -- the complete vector without relying on component order.
    , bsrComponents = toJSON basketComponents
    }

basketComponents :: [Value]
basketComponents =
  [ basketComponent 200_000_000 200_000_000 2_000 102 5_000 200_000_000
  , basketComponent 100_000_000 100_000_000 1_000 101 5_000 100_000_000
  ]

mismatchedPublishTimeComponents :: [Value]
mismatchedPublishTimeComponents =
  [ basketComponent 200_000_000 200_000_000 2_000 103 5_000 200_000_000
  , basketComponent 100_000_000 100_000_000 1_000 101 5_000 100_000_000
  ]

nonPositiveRawPriceComponents :: [Value]
nonPositiveRawPriceComponents =
  [ basketComponent 200_000_000 (-200_000_000) 2_000 102 5_000 200_000_000
  , basketComponent 100_000_000 100_000_000 1_000 101 5_000 100_000_000
  ]

basketComponent
  :: Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Value
basketComponent price rawPrice confidence publishTime weightBps basePrice =
  object
    [ "feedId" .= if price == 200_000_000 then feedAText else feedBText
    , "price" .= price
    , "rawPrice" .= rawPrice
    , "confidence" .= confidence
    , "exponent" .= (-8 :: Int)
    , "publishTime" .= publishTime
    , "inverted" .= False
    , "weightBps" .= weightBps
    , "basePrice" .= basePrice
    ]

feedAText :: Text
feedAText = "0x1111111111111111111111111111111111111111111111111111111111111111"

feedBText :: Text
feedBText = "0x2222222222222222222222222222222222222222222222222222222222222222"

feedA :: BS.ByteString
feedA = BS.replicate 32 0x11

feedB :: BS.ByteString
feedB = BS.replicate 32 0x22

account :: Text
account = "0x1111111111111111111111111111111111111111"

otherAccount :: Text
otherAccount = "0x2222222222222222222222222222222222222222"

foreignAccount :: Text
foreignAccount = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

accountLens :: Text
accountLens = "0x9999999999999999999999999999999999999999"

pythContract :: Text
pythContract = "0x8888888888888888888888888888888888888888"

cfdEngine :: Text
cfdEngine = "0x3333333333333333333333333333333333333333"

orderRouter :: Text
orderRouter = "0x7777777777777777777777777777777777777777"

otherEngine :: Text
otherEngine = "0x4444444444444444444444444444444444444444"

configuredCfdEngine :: Text
configuredCfdEngine = "0x5555555555555555555555555555555555555555"

configuredAccountLens :: Text
configuredAccountLens = "0x6666666666666666666666666666666666666666"

retiredCfdEngine :: Text
retiredCfdEngine = "0xA1Ebfb8aD9C90367eA30A29592419d447E3f8224"

testConfig :: Config
testConfig =
  Config
    { cfgRpcUrl = "https://eth-sepolia.example"
    , cfgChainId = 11155111
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Nothing
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = "https://benchmarks.pyth.network"
    , cfgPythHistoryUrl = "https://pyth.dourolabs.app/v1"
    , cfgPythHermesUrl = "https://hermes.pyth.network"
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 7
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgPerpsCandleWriteMode = PerpsCandleWritesOff
    , cfgPerpsCandleReadMode = PerpsCandleReadsLegacy
    , cfgPerpsCandleReadIntervals = []
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = "https://arb-sepolia.example"
    , cfgPerpsChainId = 421614
    , cfgPerpsUsdc = "0x1111111111111111111111111111111111111111"
    , cfgPerpsOrderRouter = "0x2222222222222222222222222222222222222222"
    , cfgPerpsOrderLifecycleBook = Nothing
    , cfgPerpsCfdEngine = configuredCfdEngine
    , cfgPerpsCfdEngineLens = "0x7777777777777777777777777777777777777777"
    , cfgPerpsCfdEngineSettlementSidecar = "0x8888888888888888888888888888888888888888"
    , cfgPerpsMarginClearinghouse = "0x3333333333333333333333333333333333333333"
    , cfgPerpsPletherOracle = "0x4444444444444444444444444444444444444444"
    , cfgPerpsAccountLens = configuredAccountLens
    , cfgPerpsHousePool = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgPerpsSettlementMonitorLens = "0xd251AC0BD90780c48F31F575152808315200664E"
    , cfgPerpsIndexerStartBlock = 302257125
    , cfgVaultHistoryHousePoolAddress = "0x0000000000000000000000000000000000000001"
    , cfgVaultHistorySeniorVaultAddress = "0x0000000000000000000000000000000000000002"
    , cfgVaultHistoryJuniorVaultAddress = "0x0000000000000000000000000000000000000003"
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryRpcUrl = "https://archive.example"
    , cfgVaultHistoryConfirmations = 12
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = liquidationReleaseManifest
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    , cfgLpSettlementEnabled = False
    , cfgLpSettlementPollSeconds = 15
    }

liquidationReleaseManifest :: CompetitionReleaseManifest
liquidationReleaseManifest =
  CompetitionReleaseManifest
    { crmReleaseId = "liquidation-worker-test"
    , crmChainId = 421614
    , crmUsdc = "0x1111111111111111111111111111111111111111"
    , crmOrderRouter = "0x2222222222222222222222222222222222222222"
    , crmMarginClearinghouse = "0x3333333333333333333333333333333333333333"
    , crmAccountLens = configuredAccountLens
    , crmCfdEngine = configuredCfdEngine
    , crmCfdEngineLens = "0x7777777777777777777777777777777777777777"
    , crmSettlementSidecar = "0x8888888888888888888888888888888888888888"
    , crmPletherOracle = "0x4444444444444444444444444444444444444444"
    , crmIndexerStartBlock = 288439939
    }

withUnsetEnv :: String -> IO a -> IO a
withUnsetEnv name action =
  bracket
    (do
        previous <- lookupEnv name
        unsetEnv name
        pure previous
    )
    (maybe (unsetEnv name) (setEnv name))
    (const action)

withEnv :: String -> String -> IO a -> IO a
withEnv name value action =
  bracket
    (do
        previous <- lookupEnv name
        setEnv name value
        pure previous
    )
    (maybe (unsetEnv name) (setEnv name))
    (const action)

receipt :: Bool -> Text -> Text -> TxReceipt
receipt succeeded emitter eventAccount =
  TxReceipt
    { receiptTxHash = "0xabc"
    , receiptBlockNumber = 123
    , receiptSucceeded = succeeded
    , receiptLogs =
        [ RpcLog
            { rpcLogTxHash = "0xabc"
            , rpcLogBlockNumber = 123
            , rpcLogAddress = emitter
            , rpcLogTopics = [positionLiquidatedTopic, encodeAddress eventAccount]
            , rpcLogData =
                encodeUint256 0
                  <> encodeUint256 1
                  <> encodeUint256 100_000_000
                  <> encodeUint256 5_000_000
            }
        ]
    }

receiptWithLogs :: Bool -> [RpcLog] -> TxReceipt
receiptWithLogs succeeded logs =
  TxReceipt
    { receiptTxHash = "0xbatch"
    , receiptBlockNumber = 124
    , receiptSucceeded = succeeded
    , receiptLogs = logs
    }

liquidationBatchItemLog
  :: Integer
  -> Text
  -> LiquidationBatchResult
  -> Integer
  -> BS.ByteString
  -> RpcLog
liquidationBatchItemLog index eventAccount result bounty selector =
  RpcLog
    { rpcLogTxHash = "0xbatch"
    , rpcLogBlockNumber = 124
    , rpcLogAddress = orderRouter
    , rpcLogTopics =
        [ liquidationBatchItemTopic
        , encodeUint256 index
        , encodeAddress eventAccount
        ]
    , rpcLogData =
        encodeUint256 (liquidationBatchResultCode result)
          <> encodeUint256 bounty
          <> BS.take 4 selector
          <> BS.replicate (28 + max 0 (4 - BS.length selector)) 0
    }

liquidationBatchStoppedLog :: Integer -> RpcLog
liquidationBatchStoppedLog nextIndex =
  RpcLog
    { rpcLogTxHash = "0xbatch"
    , rpcLogBlockNumber = 124
    , rpcLogAddress = orderRouter
    , rpcLogTopics = [liquidationBatchStoppedTopic, encodeUint256 nextIndex]
    , rpcLogData = BS.empty
    }

liquidationBatchResultCode :: LiquidationBatchResult -> Integer
liquidationBatchResultCode result =
  case result of
    LiquidationBatchLiquidated -> 0
    LiquidationBatchSkippedNoPosition -> 1
    LiquidationBatchSkippedSolvent -> 2
    LiquidationBatchFailed -> 3

isLeft :: Either a b -> Bool
isLeft value =
  case value of
    Left _ -> True
    Right _ -> False

maximumCostTx :: Tx1559
maximumCostTx =
  Tx1559
    { txChainId = 1
    , txNonce = 0
    , txMaxPriorityFeePerGas = 1
    , txMaxFeePerGas = 100
    , txGasLimit = 21_000
    , txTo = account
    , txValue = 7
    , txData = ""
    }

updateData :: [BS.ByteString]
updateData = [BS.pack [0x01], BS.pack [0x02, 0x03]]

closedSnapshot :: AccountLedgerSnapshot
closedSnapshot =
  emptyAccountSnapshot
    { alsHasPosition = False
    , alsSize = 0
    , alsLiquidatable = False
    }

openHealthySnapshot :: AccountLedgerSnapshot
openHealthySnapshot =
  emptyAccountSnapshot
    { alsHasPosition = True
    , alsSide = 0
    , alsSize = 100_000_000_000_000_000_000
    , alsMargin = 10_000_000
    , alsEntryPrice = 100_000_000
    , alsAccountEquityUsdc = 10_000_000
    , alsNetEquityUsdc = 10_000_000
    , alsLiquidatable = False
    }

openRiskySnapshot :: AccountLedgerSnapshot
openRiskySnapshot =
  openHealthySnapshot
    { alsAccountEquityUsdc = 500_000
    , alsNetEquityUsdc = 500_000
    , alsLiquidatable = True
    }

neutralRiskInputs :: FreshLiquidationRiskInputs
neutralRiskInputs =
  FreshLiquidationRiskInputs
    { flriNeutralPrice = 100_000_000
    , flriCapPrice = 200_000_000
    , flriRequiredMarginBps = 100
    , flriBasketConfidence = 0
    , flriAdverseConfidenceMultiplierBps = 0
    , flriRiskBufferBps = 0
    }

adverseRiskInputs :: FreshLiquidationRiskInputs
adverseRiskInputs =
  neutralRiskInputs
    { flriBasketConfidence = 1_000_000
    , flriAdverseConfidenceMultiplierBps = 10_000
    }

invalidRiskInputs :: FreshLiquidationRiskInputs
invalidRiskInputs = neutralRiskInputs {flriCapPrice = 0}

exactRiskGlobals :: LiquidationRiskGlobals
exactRiskGlobals =
  LiquidationRiskGlobals
    { lrgCapPrice = 200_000_000
    , lrgRequiredMarginBps = 100
    , lrgAdverseConfidenceMultiplierBps = 2_000
    , lrgPythContract = pythContract
    , lrgMaxStaleness = 10
    , lrgMaxConfidenceRatioBps = 10
    , lrgBlockTimestamp = 110
    , lrgLastMarkTime = 100
    }

emptyAccountSnapshot :: AccountLedgerSnapshot
emptyAccountSnapshot =
  AccountLedgerSnapshot
    { alsSettlementBalanceUsdc = 0
    , alsFreeSettlementUsdc = 0
    , alsActivePositionMarginUsdc = 0
    , alsOtherLockedMarginUsdc = 0
    , alsPositionMarginBucketUsdc = 0
    , alsCommittedOrderMarginBucketUsdc = 0
    , alsReservedSettlementBucketUsdc = 0
    , alsExecutionBountyReserveUsdc = 0
    , alsCommittedMarginUsdc = 0
    , alsTraderClaimBalanceUsdc = 0
    , alsPendingOrderCount = 0
    , alsCloseReachableUsdc = 0
    , alsTerminalReachableUsdc = 0
    , alsAccountEquityUsdc = 0
    , alsFreeBuyingPowerUsdc = 0
    , alsHasPosition = False
    , alsSide = 0
    , alsSize = 0
    , alsMargin = 0
    , alsEntryPrice = 0
    , alsUnrealizedPnlUsdc = 0
    , alsNetEquityUsdc = 0
    , alsLiquidatable = False
    }

encodedAccountSnapshot :: AccountLedgerSnapshot -> BS.ByteString
encodedAccountSnapshot AccountLedgerSnapshot {..} =
  mconcat
    [ encodeUint256 alsSettlementBalanceUsdc
    , encodeUint256 alsFreeSettlementUsdc
    , encodeUint256 alsActivePositionMarginUsdc
    , encodeUint256 alsOtherLockedMarginUsdc
    , encodeUint256 alsPositionMarginBucketUsdc
    , encodeUint256 alsCommittedOrderMarginBucketUsdc
    , encodeUint256 alsReservedSettlementBucketUsdc
    , encodeUint256 alsExecutionBountyReserveUsdc
    , encodeUint256 alsCommittedMarginUsdc
    , encodeUint256 alsTraderClaimBalanceUsdc
    , encodeUint256 alsPendingOrderCount
    , encodeUint256 alsCloseReachableUsdc
    , encodeUint256 alsTerminalReachableUsdc
    , encodeUint256 alsAccountEquityUsdc
    , encodeUint256 alsFreeBuyingPowerUsdc
    , encodeUint256 $ if alsHasPosition then 1 else 0
    , encodeUint256 alsSide
    , encodeUint256 alsSize
    , encodeUint256 alsMargin
    , encodeUint256 alsEntryPrice
    , signedWord alsUnrealizedPnlUsdc
    , signedWord alsNetEquityUsdc
    , encodeUint256 $ if alsLiquidatable then 1 else 0
    ]

signedWord :: Integer -> BS.ByteString
signedWord value
  | value >= 0 = encodeUint256 value
  | otherwise = encodeUint256 $ 2 ^ (256 :: Integer) + value
