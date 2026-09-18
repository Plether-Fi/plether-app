module Plether.Insights.SnapshotWorkerSpec (spec) where

import Data.Aeson (Value (..))
import Data.Time (UTCTime, addUTCTime)
import Plether.Insights.SnapshotObservability
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Plether.Ethereum.Abi (encodeUint256)
import Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( AccountLedgerSnapshot (..)
  , getAccountLedgerSnapshotCall
  )
import qualified Plether.Ethereum.Multicall as Multicall
import Plether.Ethereum.Rpc (RpcBlock (..))
import Plether.Insights.Competition
  ( EquitySnapshot (..), ScoreInput (..), ScoreBreakdown (..), calculateScore, economicAccountValue )
import Plether.Insights.SnapshotWorker
  ( accountSnapshotMulticallCalls
  , chunkInOrder
  , decodeSnapshotResults
  , defaultSnapshotMulticallSize
  , findLastBlockBeforeTimestamp
  , maxSnapshotMulticallSize
  , parseSnapshotMulticallSize
  , snapshotToJson
  , ledgerToEquity
  , inferPendingCarry
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "snapshot progress" $ do
    let started = read "2026-09-18 00:00:00 UTC" :: UTCTime
        at n = addUTCTime n started
        step n publication = advanceSnapshotProgress 60 started (at n) publication
    it "counts skipped cycles, warns on a sustained streak, and recovers only on a new committed publication" $ do
      let first = step 1 (Just $ Just $ at 0) initialSnapshotProgress
          skipped = step 30 (Just $ Just $ at 0) first
          twice = step 60 (Just $ Just $ at 0) skipped
          stalled = step 90 (Just $ Just $ at 0) twice
          recovered = step 100 (Just $ Just $ at 99) stalled
      spUnsuccessfulCycles skipped `shouldBe` 1
      spStalled twice `shouldBe` False
      spStalled stalled `shouldBe` True
      spUnsuccessfulCycles recovered `shouldBe` 0
      spStalled recovered `shouldBe` False
    it "alerts on freshness age and never invents an initial successful publication" $ do
      let pending = step 121 (Just Nothing) initialSnapshotProgress
          stale = step 121 (Just $ Just started) initialSnapshotProgress
      spStalled pending `shouldBe` True
      spLastPublication pending `shouldBe` Nothing
      spStalled stale `shouldBe` True
      step 1000 Nothing pending `shouldBe` initialSnapshotProgress

  describe "full account valuation" $ do
    it "reconciles the reported wallet at Arbitrum Sepolia block 308781536" $ do
      -- The old position-only value was 73,603.094480, omitting 30,425.778922
      -- of free settlement and liquidation reserve from a 100,000 deposit.
      let ledger = sampleSnapshot
            { alsSettlementBalanceUsdc = 99_155_238_343
            , alsLiquidationReachableSettlementUsdc = 99_155_238_343
            , alsFreeSettlementUsdc = 29_051_217_243
            , alsActivePositionMarginUsdc = 68_729_459_421
            , alsOtherLockedMarginUsdc = 1_374_561_679
            , alsTraderClaimBalanceUsdc = 0
            , alsUnrealizedPnlUsdc = 4_877_449_920
            , alsNetEquityUsdc = 73_603_094_480
            }
          carry = 3_814_861
          equity = ledgerToEquity ledger carry
          score = calculateScore $ ScoreInput
            (EquitySnapshot False 0 0 0) equity 100_000_000_000 0 0
      inferPendingCarry ledger `shouldBe` Just carry
      economicAccountValue equity `shouldBe` 104_028_873_402
      sbFinalPnlUsdc score `shouldBe` 4_028_873_402
      sbFinalPnlUsdc score - alsUnrealizedPnlUsdc ledger `shouldBe` -848_576_518

    it "counts claims once and retains order and liquidation reserves" $ do
      let ledger = sampleSnapshot
            { alsSettlementBalanceUsdc = 1_000
            , alsLiquidationReachableSettlementUsdc = 980
            , alsExecutionBountyReserveUsdc = 20
            , alsFreeSettlementUsdc = 650
            , alsActivePositionMarginUsdc = 100
            , alsOtherLockedMarginUsdc = 250
            , alsTraderClaimBalanceUsdc = 40
            , alsUnrealizedPnlUsdc = -25
            , alsNetEquityUsdc = 105 -- 100 pledge - 10 carry + 40 claims - 25 PnL
            }
      inferPendingCarry ledger `shouldBe` Just 10
      economicAccountValue (ledgerToEquity ledger 10) `shouldBe` 985

    it "requires the full carry charge once the projected pledge is exhausted" $ do
      let ledger = sampleSnapshot
            { alsLiquidationReachableSettlementUsdc = 1_000
            , alsActivePositionMarginUsdc = 100
            , alsTraderClaimBalanceUsdc = 40
            , alsUnrealizedPnlUsdc = -25
            , alsNetEquityUsdc = 15 -- zero pledge + claims + price PnL
            }
      inferPendingCarry ledger `shouldBe` Nothing
      -- Includes the extra 50 consumed from free settlement.
      economicAccountValue (ledgerToEquity ledger 150) `shouldBe` 865
      -- Uncovered carry remains a liability; floor only the full account value.
      economicAccountValue (ledgerToEquity ledger 1_100) `shouldBe` 0

    it "does not invent carry from inconsistent risk fields" $
      inferPendingCarry sampleSnapshot `shouldBe` Nothing

    it "uses reachable settlement and claims for a flat account" $ do
      let ledger = sampleSnapshot { alsHasPosition = False }
      inferPendingCarry ledger `shouldBe` Just 0
      economicAccountValue (ledgerToEquity ledger 0) `shouldBe` 910

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
          KeyMap.lookup (Key.fromString "liquidationReachableSettlementUsdc") fields
            `shouldBe` Just (String "900")
          KeyMap.lookup (Key.fromString "terminalPriceCollectibleCapUsdc") fields
            `shouldBe` Just (String "900")
        _ -> expectationFailure "expected a JSON object"

  describe "snapshot Multicall configuration" $ do
    it "defaults to conservative chunks of ten" $
      parseSnapshotMulticallSize Nothing
        `shouldBe` Right defaultSnapshotMulticallSize

    it "accepts zero as the direct-call rollback switch" $
      parseSnapshotMulticallSize (Just "0")
        `shouldBe` Right 0

    it "rejects negative, oversized, and malformed values" $ do
      parseSnapshotMulticallSize (Just "-1")
        `shouldSatisfy` isLeft
      parseSnapshotMulticallSize (Just $ show $ maxSnapshotMulticallSize + 1)
        `shouldSatisfy` isLeft
      parseSnapshotMulticallSize (Just "ten")
        `shouldSatisfy` isLeft

    it "chunks sequentially without changing account order" $
      chunkInOrder 3 ([1 .. 8] :: [Int])
        `shouldBe` Right [[1, 2, 3], [4, 5, 6], [7, 8]]

    it "builds allow-failure account-lens calls in wallet order" $ do
      let wallets =
            [ "0x0000000000000000000000000000000000000001"
            , "0x0000000000000000000000000000000000000002"
            ]
          calls =
            accountSnapshotMulticallCalls
              "0x0000000000000000000000000000000000000003"
              wallets
      map Multicall.callAllowFailure calls `shouldBe` [True, True]
      map Multicall.callCalldata calls
        `shouldBe` map getAccountLedgerSnapshotCall wallets

  describe "decodeSnapshotResults" $ do
    it "preserves successful subcall order" $ do
      let results =
            [ Multicall.CallResult True (encodedSnapshot 11)
            , Multicall.CallResult True (encodedSnapshot 22)
            ]
      fmap (map alsSettlementBalanceUsdc) (decodeSnapshotResults 2 results)
        `shouldBe` Right [11, 22]

    it "rejects a failed allow-failure subcall" $
      decodeSnapshotResults
        1
        [Multicall.CallResult False BS.empty]
        `shouldBe` Left "Multicall account snapshot subcall 0 failed"

    it "rejects missing or extra subcall results" $ do
      decodeSnapshotResults 2 [Multicall.CallResult True $ encodedSnapshot 1]
        `shouldSatisfy` isLeft
      decodeSnapshotResults
        1
        [ Multicall.CallResult True $ encodedSnapshot 1
        , Multicall.CallResult True $ encodedSnapshot 2
        ]
        `shouldSatisfy` isLeft

    it "rejects malformed account snapshot bytes" $
      decodeSnapshotResults 1 [Multicall.CallResult True BS.empty]
        `shouldBe` Left
          "Multicall account snapshot subcall 0 returned malformed data: Expected 768 bytes for AccountLedgerSnapshot, received 0"

fetchBlock :: Integer -> IO (Either String RpcBlock)
fetchBlock = pure . Right . block

block :: Integer -> RpcBlock
block number =
  RpcBlock
    { rpcBlockNumber = number
    , rpcBlockL1Number = Nothing
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
    , alsLiquidationReachableSettlementUsdc = 900
    , alsTerminalPriceCollectibleCapUsdc = 900
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

encodedSnapshot :: Integer -> BS.ByteString
encodedSnapshot settlementBalance =
  encodeUint256 settlementBalance
    <> BS.replicate (23 * 32) 0

isLeft :: Either a b -> Bool
isLeft value = case value of
  Left _ -> True
  Right _ -> False
