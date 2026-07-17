module Plether.Insights.SnapshotWorkerSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Plether.Ethereum.Contracts.CfdEngineAccountLens (AccountLedgerSnapshot (..))
import Plether.Ethereum.Rpc (RpcBlock (..))
import Plether.Insights.SnapshotWorker
  ( findLastBlockBeforeTimestamp
  , snapshotToJson
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "findLastBlockBeforeTimestamp" $ do
    it "returns the block immediately before an exact timestamp boundary" $ do
      result <- findLastBlockBeforeTimestamp fetchBlock 9 50
      result `shouldBe` Right (Just $ block 4)

    it "returns the safe upper block when it remains before the boundary" $ do
      result <- findLastBlockBeforeTimestamp fetchBlock 9 200
      result `shouldBe` Right (Just $ block 9)

    it "returns no block when the genesis block is already at the boundary" $ do
      result <- findLastBlockBeforeTimestamp fetchBlock 9 0
      result `shouldBe` Right Nothing

  describe "snapshotToJson" $ do
    it "preserves signed monetary fields as lossless decimal strings" $ do
      case snapshotToJson sampleSnapshot of
        Object fields -> do
          KeyMap.lookup (Key.fromString "unrealizedPnlUsdc") fields
            `shouldBe` Just (String "-25")
          KeyMap.lookup (Key.fromString "netEquityUsdc") fields
            `shouldBe` Just (String "975")
        _ -> expectationFailure "expected a JSON object"

fetchBlock :: Integer -> IO (Either String RpcBlock)
fetchBlock = pure . Right . block

block :: Integer -> RpcBlock
block number =
  RpcBlock
    { rpcBlockNumber = number
    , rpcBlockHash = "0xhash"
    , rpcBlockTimestamp = number * 10
    }

sampleSnapshot :: AccountLedgerSnapshot
sampleSnapshot =
  AccountLedgerSnapshot
    { alsSettlementBalanceUsdc = 1_000
    , alsFreeSettlementUsdc = 900
    , alsActivePositionMarginUsdc = 100
    , alsOtherLockedMarginUsdc = 0
    , alsPositionMarginBucketUsdc = 100
    , alsCommittedOrderMarginBucketUsdc = 0
    , alsReservedSettlementBucketUsdc = 0
    , alsExecutionBountyReserveUsdc = 0
    , alsCommittedMarginUsdc = 0
    , alsTraderClaimBalanceUsdc = 10
    , alsPendingOrderCount = 0
    , alsCloseReachableUsdc = 900
    , alsTerminalReachableUsdc = 900
    , alsAccountEquityUsdc = 975
    , alsFreeBuyingPowerUsdc = 875
    , alsHasPosition = True
    , alsSide = 0
    , alsSize = 1
    , alsMargin = 100
    , alsEntryPrice = 100
    , alsUnrealizedPnlUsdc = -25
    , alsNetEquityUsdc = 975
    , alsLiquidatable = False
    }
