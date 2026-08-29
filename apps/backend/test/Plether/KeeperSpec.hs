module Plether.KeeperSpec (spec) where

import qualified Data.ByteString as BS
import Plether.Database.Schema
  ( PerpsKeeperOrderRow (..)
  , isAdmittedPythPayloadSource
  , isHistoricalRevealPayloadSource
  , promotePythPayloadSource
  )
import Plether.Keeper
  ( LpSettlementDecision (..)
  , assessLpSettlementStatus
  , isFrozenClosePayloadReady
  , isLpSettlementObservationSafe
  , isOrderExpired
  , isOrderRevealReady
  , isSameBlockMevGuardError
  , selectBatchCandidates
  , validateAtomicSettlementPayload
  )
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

  describe "isOrderExpired" $ do
    it "does not expire at the exact max age boundary" $ do
      isOrderExpired 110 10 (order 1 100) `shouldBe` False

    it "expires after max age" $ do
      isOrderExpired 111 10 (order 1 100) `shouldBe` True

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
              50
              15
              [101, 102, 103, 104, 105, 106]
              20
              [order 1 100, order 2 100, order 3 101]
      map pkorOrderId selected `shouldBe` [1, 2]

    it "includes expired terminal orders in a contiguous batch" $ do
      let selected =
            selectBatchCandidates
              120
              11
              10
              15
              [101, 102, 103, 104, 105, 106]
              20
              [order 1 100, order 2 99]
      map pkorOrderId selected `shouldBe` [1, 2]

    it "stops at the same-block guard" $ do
      let selected =
            selectBatchCandidates
              110
              10
              50
              15
              [101, 102, 103, 104, 105, 106]
              20
              [order 1 100]
      map pkorOrderId selected `shouldBe` []

    it "honors the max batch size" $ do
      let selected =
            selectBatchCandidates
              110
              11
              50
              15
              [101, 102, 103, 104, 105, 106]
              1
              [order 1 100, order 2 99]
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
