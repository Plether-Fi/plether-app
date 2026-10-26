module Plether.Ethereum.Contracts.CfdEngineAccountLensSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Plether.Ethereum.Abi (encodeAddress, encodeUint256)
import Plether.Ethereum.Client (BlockTag (..), RpcError (..), renderBlockTag)
import Plether.Ethereum.Contracts.CfdEngineAccountLens
import Test.Hspec

spec :: Spec
spec = do
  describe "pending carry preview" $ do
    it "reads pending carry even when the zero-size diagnostic is not a valid open" $ do
      let response = mconcat $ map encodeUint256 $
            [0, 1] <> replicate 9 0 <> [3_814_861] <> replicate 12 0
      decodePendingCarryPreview response `shouldBe` Right 3_814_861

    it "rejects truncated and incompatible preview layouts" $ do
      decodePendingCarryPreview (BS.replicate (23 * 32) 0)
        `shouldBe` Left "Expected 768 bytes for V2 open carry preview"
      decodePendingCarryPreview (BS.replicate (25 * 32) 0)
        `shouldBe` Left "Expected 768 bytes for V2 open carry preview"

    it "encodes a read-only zero-size open diagnostic" $ do
      let account = "0x1111111111111111111111111111111111111111"
      BS.drop 4 (pendingCarryPreviewCall account)
        `shouldBe` encodeAddress account <> BS.replicate (5 * 32) 0

  describe "getAccountLedgerSnapshotCall" $ do
    it "encodes the account-lens selector and address" $ do
      expectedSelector <- parseHex "f4bb62c1"
      let account = "0x1111111111111111111111111111111111111111"
          call = getAccountLedgerSnapshotCall account
      BS.take 4 call `shouldBe` expectedSelector
      BS.drop 4 call `shouldBe` encodeAddress account

  describe "decodeAccountLedgerSnapshot" $ do
    it "decodes every tuple field, including signed P&L and equity" $ do
      decodeAccountLedgerSnapshot encodedSnapshot
        `shouldBe` Right
          AccountLedgerSnapshot
            { alsSettlementBalanceAssets = 101
            , alsFreeSettlementAssets = 102
            , alsActivePositionMarginAssets = 103
            , alsOtherLockedMarginAssets = 104
            , alsPositionMarginBucketAssets = 105
            , alsCommittedOrderMarginBucketAssets = 106
            , alsReservedSettlementBucketAssets = 107
            , alsExecutionBountyReserveAssets = 108
            , alsCommittedMarginAssets = 109
            , alsTraderClaimBalanceAssets = 110
            , alsPendingOrderCount = 111
            , alsCloseReachableAssets = 112
            , alsLiquidationReachableSettlementAssets = 113
            , alsTerminalPriceCollectibleCapAssets = 114
            , alsAccountEquityAssets = 115
            , alsFreeBuyingPowerAssets = 116
            , alsHasPosition = True
            , alsSide = 2
            , alsSize = 118
            , alsMargin = 119
            , alsEntryPrice = 120
            , alsUnrealizedPnlAssets = -321
            , alsNetEquityAssets = -654
            , alsLiquidatable = False
            }

    it "rejects truncated RPC output instead of fabricating zero fields" $ do
      decodeAccountLedgerSnapshot (BS.take (BS.length encodedSnapshot - 1) encodedSnapshot)
        `shouldBe` Left "Expected 768 bytes for AccountLedgerSnapshot, received 767"

    it "rejects the legacy 23-word snapshot shape" $ do
      decodeAccountLedgerSnapshot (BS.take (23 * 32) encodedSnapshot)
        `shouldBe` Left "Expected 768 bytes for AccountLedgerSnapshot, received 736"

  describe "renderBlockTag" $ do
    it "renders an exact block number as a canonical JSON-RPC quantity" $ do
      renderBlockTag (BlockNumber 273137426) `shouldBe` Right "0x1047bf12"

    it "renders named Ethereum block tags" $ do
      map renderBlockTag [Latest, Earliest, Pending, Safe, Finalized]
        `shouldBe` map Right ["latest", "earliest", "pending", "safe", "finalized"]

    it "rejects negative block numbers before making an RPC request" $ do
      renderBlockTag (BlockNumber (-1))
        `shouldBe` Left (RpcJsonError "Block number cannot be negative")

encodedSnapshot :: BS.ByteString
encodedSnapshot =
  mconcat $
    map encodeUint256 [101 .. 116]
      <> [ encodeUint256 1
         , encodeUint256 2
         , encodeUint256 118
         , encodeUint256 119
         , encodeUint256 120
         , signedWord (-321)
         , signedWord (-654)
         , encodeUint256 0
         ]

signedWord :: Integer -> BS.ByteString
signedWord value
  | value >= 0 = encodeUint256 value
  | otherwise = encodeUint256 $ 2 ^ (256 :: Integer) + value

parseHex :: BS.ByteString -> IO BS.ByteString
parseHex value =
  case B16.decode value of
    Right bytes -> pure bytes
    Left err -> fail $ "invalid calldata hex fixture: " <> err
