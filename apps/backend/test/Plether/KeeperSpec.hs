module Plether.KeeperSpec (spec) where

import qualified Data.ByteString as BS
import Plether.Database.Schema
  ( PerpsKeeperOrderRow (..)
  , isAdmittedPythPayloadSource
  , isHistoricalRevealPayloadSource
  , promotePythPayloadSource
  )
import Plether.Keeper
  ( FreshPendingOrder (..)
  , LifecycleRefreshAction (..)
  , LpSettlementDecision (..)
  , V2PreflightAction (..)
  , assessBatchOrderPreflight
  , assessLifecycleRefresh
  , assessLpSettlementStatus
  , assessSingleOrderPreflight
  , isFrozenClosePayloadReady
  , isLpSettlementObservationSafe
  , isOrderPastValidUntil
  , isOrderRevealReady
  , isSameBlockMevGuardError
  , nextV2GasLimit
  , selectBatchCandidates
  , validateAtomicSettlementPayload
  )
import qualified Plether.Ethereum.Contracts.Perps as Perps
import Plether.Ethereum.Contracts.SettlementMonitor
  ( ExecutionPath (..)
  , SettlementObservation (..)
  , SettlementStatus (..)
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "LP epoch settlement gating" $ do
    it "distinguishes no-work, held, dependency-unknown, and executable paths" $ do
      assessLpSettlementStatus (settlementStatus CachedMark) `shouldBe` LpSettlementReady CachedMark
      assessLpSettlementStatus (settlementStatus AtomicOracleRefresh)
        `shouldBe` LpSettlementReady AtomicOracleRefresh
      assessLpSettlementStatus ((settlementStatus CachedMark) {ssHasMaturedWork = False})
        `shouldBe` LpSettlementNoMaturedWork
      assessLpSettlementStatus ((settlementStatus CachedMark) {ssLpEpochSettlementPaused = True})
        `shouldBe` LpSettlementHeld
      assessLpSettlementStatus ((settlementStatus CachedMark) {ssDependencyFailureMask = 1})
        `shouldBe` LpSettlementDependenciesUnknown
      assessLpSettlementStatus ((settlementStatus CachedMark) {ssOperationalBlockerMask = 1})
        `shouldBe` LpSettlementOperationallyBlocked

    it "requires a complete, healthy, dependency-free pinned observation" $ do
      isLpSettlementObservationSafe (settlementObservation CachedMark) `shouldBe` True
      isLpSettlementObservationSafe
        ((settlementObservation CachedMark) {soObservationComplete = False})
        `shouldBe` False
      isLpSettlementObservationSafe
        ((settlementObservation CachedMark) {soCriticalFaultMask = 1})
        `shouldBe` False
      isLpSettlementObservationSafe
        ((settlementObservation CachedMark) {soHealthDependencyFailureMask = 1})
        `shouldBe` False

    it "accepts only a fresh exact six-feed atomic payload" $ do
      let sixUpdates = replicate 6 $ BS.singleton 1
      validateAtomicSettlementPayload 100 (replicate 6 100) sixUpdates
        `shouldBe` Right ()
      validateAtomicSettlementPayload 100 (replicate 6 99) sixUpdates
        `shouldBe` Left "the latest Pyth payload predates the minimum atomic publish time"
      validateAtomicSettlementPayload 100 (replicate 5 100) (replicate 5 $ BS.singleton 1)
        `shouldBe` Left "the latest Pyth payload does not contain exactly six feeds"

  describe "isOrderPastValidUntil" $ do
    it "does not expire at the immutable deadline boundary" $ do
      isOrderPastValidUntil 110 110 `shouldBe` False

    it "expires after the immutable deadline" $ do
      isOrderPastValidUntil 111 110 `shouldBe` True

    it "does not derive expiry from a mutable max-order-age value" $ do
      isOrderPastValidUntil 111 150 `shouldBe` False

  describe "lifecycle queue-head refresh" $ do
    it "reconciles an already executed or failed queue head instead of reading its cleared policy" $ do
      assessLifecycleRefresh 2 `shouldBe` Right ReconcileTerminalLifecycle
      assessLifecycleRefresh 3 `shouldBe` Right ReconcileTerminalLifecycle

    it "reads immutable pending policy only for a pending lifecycle order" $ do
      assessLifecycleRefresh 1 `shouldBe` Right RefreshPendingLifecycle

    it "rejects a database row that has no lifecycle intent" $ do
      assessLifecycleRefresh 0
        `shouldBe` Left "lifecycle book reports that the indexed order is unused"

  describe "nextV2GasLimit" $ do
    it "doubles the misleading estimate until V2 execution has a usable envelope" $ do
      let estimatedWithBuffer = 695_529
      nextV2GasLimit estimatedWithBuffer 30_000_000 `shouldBe` Just 1_391_058
      nextV2GasLimit 1_391_058 30_000_000 `shouldBe` Just 2_782_116

    it "caps escalation and stops once the cap is reached" $ do
      nextV2GasLimit 20_000_000 30_000_000 `shouldBe` Just 30_000_000
      nextV2GasLimit 30_000_000 30_000_000 `shouldBe` Nothing

  describe "typed V2 execution preflight" $ do
    it "increases gas instead of broadcasting a successful InsufficientGas no-op" $ do
      assessSingleOrderPreflight 1 (executionResult 1 1 5)
        `shouldBe` V2PreflightIncreaseGas

    it "defers other pending outcomes without broadcasting" $ do
      assessSingleOrderPreflight 1 (executionResult 1 1 4)
        `shouldBe` V2PreflightDefer "order remains pending with reason 4"

    it "authorizes broadcast only for a terminal single-order result" $ do
      assessSingleOrderPreflight 1 (executionResult 1 3 0)
        `shouldBe` V2PreflightSubmit
      assessSingleOrderPreflight 1 (executionResult 2 3 0)
        `shouldBe` V2PreflightReject "executeOrder preflight returned a different order ID"

    it "requires gas escalation or terminal batch progress" $ do
      assessBatchOrderPreflight (Perps.OrderBatchResult 2 0 5)
        `shouldBe` V2PreflightIncreaseGas
      assessBatchOrderPreflight (Perps.OrderBatchResult 2 1 0)
        `shouldBe` V2PreflightSubmit
      assessBatchOrderPreflight (Perps.OrderBatchResult 2 0 4)
        `shouldBe` V2PreflightDefer "batch made no terminal progress and stopped with reason 4"

  describe "isOrderRevealReady" $ do
    it "accepts publish times starting at the first post-commit tick" $ do
      isOrderRevealReady 15 [101, 102, 103, 104, 105, 106] (order 1 100)
        `shouldBe` True

    it "rejects publish times before the order reveal window" $ do
      isOrderRevealReady 15 [101, 102, 103, 104, 105, 106] (order 1 101)
        `shouldBe` False

    it "rejects later in-window payloads because Pyth unique parsing expects the first tick" $ do
      isOrderRevealReady 15 [103, 103, 103, 103, 103, 103] (order 1 100)
        `shouldBe` False

  describe "isFrozenClosePayloadReady" $ do
    it "accepts a latest payload inside frozen close staleness policy" $ do
      isFrozenClosePayloadReady
        1_781_988_116
        259_200
        60
        [1_781_902_803, 1_781_902_803, 1_781_902_803, 1_781_902_803, 1_781_902_803, 1_781_902_803]
        `shouldBe` True

    it "rejects a payload older than the frozen close staleness policy" $ do
      isFrozenClosePayloadReady 1_000 60 60 [939, 939, 939]
        `shouldBe` False

    it "rejects a payload whose component publish times diverge too far" $ do
      isFrozenClosePayloadReady 1_100 200 60 [1_000, 1_061]
        `shouldBe` False

    it "rejects a future payload" $ do
      isFrozenClosePayloadReady 1_000 200 60 [1_001]
        `shouldBe` False

  describe "isHistoricalRevealPayloadSource" $ do
    it "accepts only on-chain-admitted historical reveal payload sources" $ do
      isHistoricalRevealPayloadSource "backend_hermes_historical_v2" `shouldBe` True
      isHistoricalRevealPayloadSource "backend_hermes_reveal_v2" `shouldBe` True

    it "rejects pre-admission legacy rows and latest-loop payload sources" $ do
      isHistoricalRevealPayloadSource "backend_hermes_historical" `shouldBe` False
      isHistoricalRevealPayloadSource "backend_hermes_reveal_backfill" `shouldBe` False
      isHistoricalRevealPayloadSource "backend_hermes_latest" `shouldBe` False
      isHistoricalRevealPayloadSource "backend_hermes_latest_v2" `shouldBe` False
      isHistoricalRevealPayloadSource "backend_hermes" `shouldBe` False

  describe "Pyth payload source admission" $ do
    it "versions a source only after the caller has completed on-chain admission" $ do
      promotePythPayloadSource "backend_hermes_latest"
        `shouldBe` Just "backend_hermes_latest_v2"
      promotePythPayloadSource "backend_hermes_historical"
        `shouldBe` Just "backend_hermes_historical_v2"
      promotePythPayloadSource "backend_hermes_reveal_backfill"
        `shouldBe` Just "backend_hermes_reveal_v2"

    it "does not admit unknown or pre-deployment source labels" $ do
      promotePythPayloadSource "backend_hermes" `shouldBe` Nothing
      isAdmittedPythPayloadSource "backend_hermes_latest" `shouldBe` False
      isAdmittedPythPayloadSource "backend_hermes_historical" `shouldBe` False
      isAdmittedPythPayloadSource "backend_hermes_latest_v2" `shouldBe` True

  describe "isSameBlockMevGuardError" $ do
    it "detects router same-block MEV guard reverts by selector" $ do
      isSameBlockMevGuardError "RPC node error 3: execution reverted; data: 0x7abb32d5"
        `shouldBe` True

    it "does not classify unrelated execution reverts as next-block retryable" $ do
      isSameBlockMevGuardError "RPC node error 3: execution reverted; data: 0x1dc4770a"
        `shouldBe` False

  describe "selectBatchCandidates" $ do
    it "takes contiguous ready orders sharing the same payload" $ do
      let selected =
            selectBatchCandidates
              110
              11
              15
              [101, 102, 103, 104, 105, 106]
              20
              [fresh 150 $ order 1 100, fresh 150 $ order 2 100, fresh 150 $ order 3 101]
      map pkorOrderId selected `shouldBe` [1, 2]

    it "includes expired terminal orders in a contiguous batch" $ do
      let selected =
            selectBatchCandidates
              120
              11
              15
              [101, 102, 103, 104, 105, 106]
              20
              [fresh 110 $ order 1 100, fresh 109 $ order 2 99]
      map pkorOrderId selected `shouldBe` [1, 2]

    it "stops at the same-block guard" $ do
      let selected =
            selectBatchCandidates
              110
              10
              15
              [101, 102, 103, 104, 105, 106]
              20
              [fresh 150 $ order 1 100]
      map pkorOrderId selected `shouldBe` []

    it "honors the max batch size" $ do
      let selected =
            selectBatchCandidates
              110
              11
              15
              [101, 102, 103, 104, 105, 106]
              1
              [fresh 150 $ order 1 100, fresh 150 $ order 2 99]
      map pkorOrderId selected `shouldBe` [1]

order :: Integer -> Integer -> PerpsKeeperOrderRow
order orderId commitTime =
  PerpsKeeperOrderRow
    { pkorOrderId = orderId
    , pkorOrderRouter = "0x2222222222222222222222222222222222222222"
    , pkorAccount = "0x1111111111111111111111111111111111111111"
    , pkorSide = 0
    , pkorCommitBlock = 10
    , pkorCommitTime = commitTime
    , pkorCommitTxHash = "0xcommit"
    , pkorStatus = "pending"
    , pkorAttemptCount = 0
    , pkorLastError = Nothing
    }

fresh :: Integer -> PerpsKeeperOrderRow -> FreshPendingOrder
fresh validUntil pendingOrder =
  FreshPendingOrder
    { fpoOrder = pendingOrder
    , fpoIsClose = False
    , fpoValidUntil = validUntil
    }

executionResult :: Integer -> Integer -> Integer -> Perps.OrderExecutionResult
executionResult orderId lifecycleStatus pendingReason =
  Perps.OrderExecutionResult
    { Perps.oerOrderId = orderId
    , Perps.oerLifecycleStatus = lifecycleStatus
    , Perps.oerTerminalReason = 0
    , Perps.oerPendingReason = pendingReason
    , Perps.oerReceiptHash = BS.replicate 32 0
    }

settlementStatus :: ExecutionPath -> SettlementStatus
settlementStatus path =
  SettlementStatus
    { ssObservedBlock = 302_300_000
    , ssCurrentEpoch = 500_000
    , ssMinimumAtomicPublishTime = 1_800_000_000
    , ssRequiredExecutionPath = path
    , ssCachedMarkPrice = 100_000_000
    , ssCachedMarkTime = 1_799_999_999
    , ssOperationalBlockerMask = 0
    , ssWarningMask = 0
    , ssExecutionPathDependencyMask = 0
    , ssDependencyFailureMask = 0
    , ssHasMaturedWork = True
    , ssLpEpochSettlementPaused = False
    }

settlementObservation :: ExecutionPath -> SettlementObservation
settlementObservation path =
  SettlementObservation
    { soSchemaVersion = 1
    , soStatus = settlementStatus path
    , soHealthState = 1
    , soCriticalFaultMask = 0
    , soHealthDependencyFailureMask = 0
    , soObservationDigest = "0x1234"
    , soObservationComplete = True
    }
