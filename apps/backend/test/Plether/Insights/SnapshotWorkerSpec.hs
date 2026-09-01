module Plether.Insights.SnapshotWorkerSpec (spec) where

import Data.Aeson (Value (..))
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
import Plether.Insights.SnapshotWorker
  ( accountSnapshotMulticallCalls
  , chunkInOrder
  , decodeSnapshotResults
  , defaultSnapshotMulticallSize
  , findLastBlockBeforeTimestamp
  , maxSnapshotMulticallSize
  , parseSnapshotMulticallSize
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
