module Plether.Vaults.PerformanceIndexerSpec (spec) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Plether.Database.VaultPerformance (VaultPerformanceSnapshotRow (..))
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Rpc (RpcBlock (..))
import Plether.Vaults.PerformanceIndexer
  ( SnapshotReconciliationDecision (..)
  , VaultPerformanceCycleResult (..)
  , VaultPerformanceLoopDecision (..)
  , decideSnapshotReconciliation
  , decideVaultPerformanceLoop
  , findLastBlockAtOrBeforeTimestamp
  , snapshotNeedsRepair
  , validateSampledBlockIdentity
  , vaultEpochBoundaries
  , vaultHistoryPointCount
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "vaultEpochBoundaries" $ do
    it "returns exactly 169 consecutive UTC hours for a mature deployment" $ do
      let boundaries = vaultEpochBoundaries 0 (200 * 3_600 + 42)
      length boundaries `shouldBe` vaultHistoryPointCount
      zipWith (-) (tail boundaries) boundaries `shouldBe` replicate 168 3_600
      last boundaries `shouldBe` 200 * 3_600

    it "does not request a boundary before a mid-hour deployment" $
      vaultEpochBoundaries 3_601 10_800 `shouldBe` [7_200, 10_800]

  describe "findLastBlockAtOrBeforeTimestamp" $ do
    it "includes a block exactly on the boundary" $ do
      result <- findLastBlockAtOrBeforeTimestamp fetchBlock 0 10 50
      result `shouldBe` Right (Just $ block 5)

    it "returns the immediately preceding block between timestamps" $ do
      result <- findLastBlockAtOrBeforeTimestamp fetchBlock 0 10 59
      result `shouldBe` Right (Just $ block 5)

    it "returns nothing when the configured lower block is newer than the boundary" $ do
      result <- findLastBlockAtOrBeforeTimestamp fetchBlock 6 10 50
      result `shouldBe` Right Nothing

    it "uses timestamp interpolation instead of scanning a wide block range" $ do
      readCount <- newIORef (0 :: Int)
      let countingFetch number = do
            modifyIORef' readCount (+ 1)
            pure (Right (block number) :: Either String RpcBlock)
      result <-
        findLastBlockAtOrBeforeTimestamp
          countingFetch
          0
          1_000_000
          5_000_000
      result `shouldBe` Right (Just $ block 500_000)
      readIORef readCount `shouldReturn` 4

  describe "snapshotNeedsRepair" $ do
    it "accepts the same canonical block identity" $
      snapshotNeedsRepair sampleRow (block 5) `shouldBe` False

    it "repairs hash, timestamp, or boundary violations" $ do
      snapshotNeedsRepair sampleRow ((block 5) {rpcBlockHash = "0xother"}) `shouldBe` True
      snapshotNeedsRepair sampleRow ((block 5) {rpcBlockTimestamp = 49}) `shouldBe` True
      snapshotNeedsRepair sampleRow ((block 5) {rpcBlockTimestamp = 61}) `shouldBe` True

    it "resamples legacy checkpoints without observed freshness" $
      snapshotNeedsRepair (sampleRow {vpsMarkFresh = Nothing}) (block 5) `shouldBe` True

  describe "snapshot reconciliation control" $ do
    it "keeps a canonical checkpoint idempotently without another upsert" $ do
      decideSnapshotReconciliation (Just sampleRow) (Just $ block 5)
        `shouldBe` KeepCanonicalSnapshot (block 5)
      decideSnapshotReconciliation (Just sampleRow) (Just $ block 5)
        `shouldBe` KeepCanonicalSnapshot (block 5)

    it "resamples a missing checkpoint, failed block read, or canonical replacement" $ do
      decideSnapshotReconciliation Nothing Nothing `shouldBe` ResampleSnapshot
      decideSnapshotReconciliation (Just sampleRow) Nothing `shouldBe` ResampleSnapshot
      decideSnapshotReconciliation
        (Just sampleRow)
        (Just $ (block 5) {rpcBlockHash = "0xreplacement"})
        `shouldBe` ResampleSnapshot

  describe "post-Multicall block verification" $ do
    it "accepts the unchanged exact block" $
      validateSampledBlockIdentity (block 5) (block 5)
        `shouldBe` Right (block 5)

    it "rejects a hash replacement before persistence" $
      validateSampledBlockIdentity
        (block 5)
        ((block 5) {rpcBlockHash = "0xreplacement"})
        `shouldSatisfy` isLeft

  describe "indexer retry control" $ do
    it "does not advance after an archive RPC failure, allowing the same epoch to retry" $
      decideVaultPerformanceLoop
        (Left "archive RPC unavailable" :: Either String VaultPerformanceCycleResult)
        `shouldBe` RetryVaultPerformanceCycle

    it "does not advance when another replica owns the advisory lock" $
      decideVaultPerformanceLoop
        (Right VaultPerformanceCycleLeaderBusy :: Either String VaultPerformanceCycleResult)
        `shouldBe` RetryVaultPerformanceCycle

    it "records only a fully reconciled boundary" $
      decideVaultPerformanceLoop
        (Right (VaultPerformanceCycleCompleted 10_800) :: Either String VaultPerformanceCycleResult)
        `shouldBe` RecordCompletedVaultBoundary 10_800

fetchBlock :: Integer -> IO (Either String RpcBlock)
fetchBlock = pure . Right . block

block :: Integer -> RpcBlock
block number =
  RpcBlock
    { rpcBlockNumber = number
    , rpcBlockL1Number = Nothing
    , rpcBlockHash = if number == 5 then canonicalHash else "0xhash"
    , rpcBlockTimestamp = number * 10
    }

sampleRow :: VaultPerformanceSnapshotRow
sampleRow =
  VaultPerformanceSnapshotRow
    { vpsChainId = 421_614
    , vpsHousePoolAddress = address 1
    , vpsSeniorVaultAddress = address 2
    , vpsJuniorVaultAddress = address 3
    , vpsEpochTimestamp = 60
    , vpsBlockNumber = 5
    , vpsBlockHash = canonicalHash
    , vpsBlockTimestamp = 50
    , vpsMarkFresh = Just True
    , vpsSeniorTotalAssets = 100
    , vpsSeniorTotalSupply = 100
    , vpsSeniorSharePriceWad = 10 ^ (18 :: Integer)
    , vpsJuniorTotalAssets = 100
    , vpsJuniorTotalSupply = 100
    , vpsJuniorSharePriceWad = 10 ^ (18 :: Integer)
    }

canonicalHash :: Text
canonicalHash = "0xcanonical"

address :: Integer -> Text
address suffix = "0x" <> T.replicate 39 "0" <> T.pack (show suffix)

isLeft :: Either a b -> Bool
isLeft value = case value of
  Left _ -> True
  Right _ -> False
