module Plether.KeeperSpec (spec) where

import Control.Monad (filterM)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Plether.Config (LpSettlementMode (..))
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
  , isLpSettlementObservationConsistent
  , isLpSettlementObservationSafe
  , isOrderPastValidUntil
  , isOrderRevealReady
  , isSameBlockMevGuardError
  , nextV2GasLimit
  , selectBatchCandidates
  , validateAtomicSettlementPayload
  , validateLpSettlementCost
  )
import qualified Plether.Ethereum.Contracts.Perps as Perps
import Plether.Ethereum.Contracts.SettlementMonitor
  ( ExecutionPath (..)
  , SettlementObservation (..)
  , SettlementStatus (..)
  , supportedObservationSchemaVersion
  )
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  keeperSource <- runIO loadKeeperSource
  let normalizedKeeperSource = T.unwords $ T.words keeperSource

  describe "LP epoch settlement call graph" $ do
    it "wires cached settlement only to HousePool with zero value" $ do
      normalizedKeeperSource
        `shouldSatisfy` T.isInfixOf
          "( cfgPerpsHousePool cfg , 0 , Perps.settleLpEpochPoolCall"

    it "wires atomic settlement only to Router with the exact quoted Pyth fee" $ do
      normalizedKeeperSource
        `shouldSatisfy` T.isInfixOf
          "( cfgPerpsOrderRouter cfg , exactFee , Perps.settleLpEpochRouterCall updateData )"

    it "does not invoke any user claim or refund path" $ do
      let foldedSource = T.toCaseFold keeperSource
      foldedSource `shouldNotSatisfy` T.isInfixOf "claim"
      foldedSource `shouldNotSatisfy` T.isInfixOf "refund"

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
      assessLpSettlementStatus ((settlementStatus CachedMark) {ssExecutionPathDependencyMask = 1})
        `shouldBe` LpSettlementDependenciesUnknown
      assessLpSettlementStatus ((settlementStatus CachedMark) {ssOperationalBlockerMask = 1})
        `shouldBe` LpSettlementOperationallyBlocked
      assessLpSettlementStatus (settlementStatus UnknownPath)
        `shouldBe` LpSettlementDependenciesUnknown
      assessLpSettlementStatus (settlementStatus NoMaturedWork)
        `shouldBe` LpSettlementNoMaturedWork

    it "requires a complete, healthy, dependency-free pinned observation" $ do
      isLpSettlementObservationSafe (settlementObservation CachedMark) `shouldBe` True
      isLpSettlementObservationSafe (settlementObservation AtomicOracleRefresh) `shouldBe` True
      isLpSettlementObservationSafe
        ((settlementObservation CachedMark) {soSchemaVersion = 2})
        `shouldBe` False
      isLpSettlementObservationSafe
        ((settlementObservation CachedMark) {soObservationComplete = False})
        `shouldBe` False
      isLpSettlementObservationSafe
        ((settlementObservation CachedMark) {soHealthState = 0})
        `shouldBe` False
      isLpSettlementObservationSafe
        ((settlementObservation CachedMark) {soCriticalFaultMask = 1})
        `shouldBe` False
      isLpSettlementObservationSafe
        ((settlementObservation CachedMark) {soHealthDependencyFailureMask = 1})
        `shouldBe` False
      isLpSettlementObservationSafe
        (withSettlementStatus (\status -> status {ssDependencyFailureMask = 1}) $ settlementObservation CachedMark)
        `shouldBe` False
      isLpSettlementObservationSafe
        (withSettlementStatus (\status -> status {ssExecutionPathDependencyMask = 1}) $ settlementObservation CachedMark)
        `shouldBe` False
      isLpSettlementObservationSafe
        (withSettlementStatus (\status -> status {ssOperationalBlockerMask = 1}) $ settlementObservation CachedMark)
        `shouldBe` False
      isLpSettlementObservationSafe
        (withSettlementStatus (\status -> status {ssLpEpochSettlementPaused = True}) $ settlementObservation CachedMark)
        `shouldBe` False
      isLpSettlementObservationSafe
        (withSettlementStatus (\status -> status {ssHasMaturedWork = False}) $ settlementObservation CachedMark)
        `shouldBe` False
      isLpSettlementObservationSafe (settlementObservation UnknownPath) `shouldBe` False
      isLpSettlementObservationSafe (settlementObservation NoMaturedWork) `shouldBe` False

    it "keeps a frozen-oracle warning-only cached observation executable" $ do
      let frozenOracleObservation =
            withSettlementStatus
              (\status -> status {ssWarningMask = 1})
              (settlementObservation CachedMark)
      assessLpSettlementStatus (soStatus frozenOracleObservation)
        `shouldBe` LpSettlementReady CachedMark
      isLpSettlementObservationSafe frozenOracleObservation `shouldBe` True

    it "pins the observation to the exact epoch, cutoff, and confirmed block" $ do
      let observation = settlementObservation CachedMark
      isLpSettlementObservationConsistent 500_000 302_300_000 observation
        `shouldBe` True
      isLpSettlementObservationConsistent
        500_000
        302_300_000
        (withSettlementStatus (\status -> status {ssCurrentEpoch = 500_001}) observation)
        `shouldBe` False
      isLpSettlementObservationConsistent
        500_000
        302_300_000
        (withSettlementStatus (\status -> status {ssSettlementCutoffEpoch = 499_999}) observation)
        `shouldBe` False
      isLpSettlementObservationConsistent
        500_000
        302_300_000
        (withSettlementStatus (\status -> status {ssObservedBlock = 302_299_999}) observation)
        `shouldBe` False

    it "accepts only a fresh exact six-feed atomic payload" $ do
      let combinedUpdate = [BS.singleton 1]
      validateAtomicSettlementPayload 100 (replicate 6 100) combinedUpdate
        `shouldBe` Right ()
      validateAtomicSettlementPayload 100 (replicate 6 99) combinedUpdate
        `shouldBe` Left "the latest Pyth payload predates the minimum atomic publish time"
      validateAtomicSettlementPayload 100 (replicate 5 100) combinedUpdate
        `shouldBe` Left "the latest admitted Pyth payload does not contain exactly six feed publish times"
      validateAtomicSettlementPayload 100 (replicate 6 100) []
        `shouldBe` Left "the latest admitted Pyth payload does not contain non-empty binary update data"
      validateAtomicSettlementPayload 100 (replicate 6 100) [BS.empty]
        `shouldBe` Left "the latest admitted Pyth payload does not contain non-empty binary update data"

    it "applies signer-balance and configured transaction-cost limits in both active modes" $ do
      validateLpSettlementCost LpSettlementObserve 0 1_000 1_000 `shouldBe` Right ()
      validateLpSettlementCost LpSettlementObserve 999 1_000 1_000
        `shouldSatisfy` isLeft
      validateLpSettlementCost LpSettlementExecute 0 1_000 1_000
        `shouldSatisfy` isLeft
      validateLpSettlementCost LpSettlementExecute 1_000 999 1_000
        `shouldSatisfy` isLeft
      validateLpSettlementCost LpSettlementExecute 1_000 1_000 1_000
        `shouldBe` Right ()

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
    , ssSettlementCutoffEpoch = 500_000
    , ssMinimumAtomicPublishTime = 1_800_000_000
    , ssSeniorMaturedDepositHeadEpoch = Nothing
    , ssSeniorMaturedDepositHeadAssets = 0
    , ssSeniorMaturedRedeemHeadEpoch = Just 499_999
    , ssSeniorMaturedRedeemHeadShares = 1
    , ssJuniorMaturedDepositHeadEpoch = Nothing
    , ssJuniorMaturedDepositHeadAssets = 0
    , ssJuniorMaturedRedeemHeadEpoch = Nothing
    , ssJuniorMaturedRedeemHeadShares = 0
    , ssOldestMaturedHead = Just 499_999
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
    { soSchemaVersion = supportedObservationSchemaVersion
    , soStatus = settlementStatus path
    , soHealthState = 1
    , soCriticalFaultMask = 0
    , soHealthDependencyFailureMask = 0
    , soObservationDigest = "0x1234"
    , soObservationComplete = True
    }

withSettlementStatus
  :: (SettlementStatus -> SettlementStatus)
  -> SettlementObservation
  -> SettlementObservation
withSettlementStatus update observation =
  observation {soStatus = update $ soStatus observation}

loadKeeperSource :: IO T.Text
loadKeeperSource = do
  let candidates =
        [ "src/Plether/Keeper.hs"
        , "apps/backend/src/Plether/Keeper.hs"
        ]
  existing <- filterM doesFileExist candidates
  case existing of
    path : _ -> TIO.readFile path
    [] -> fail "could not locate Plether/Keeper.hs for call-graph invariant tests"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
