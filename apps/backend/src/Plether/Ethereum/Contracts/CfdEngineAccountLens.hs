module Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( AccountLedgerSnapshot (..)
  , getAccountLedgerSnapshot
  , getAccountLedgerSnapshotAtBlock
  , getAccountLedgerSnapshotCall
  , decodeAccountLedgerSnapshot
  , getPendingCarryAtBlock
  , pendingCarryPreviewCall
  , decodePendingCarryPreview
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Abi
  ( decodeBool
  , decodeInt256
  , decodeUint256
  , encodeAddress
  , encodeCall
  , encodeUint256
  )
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , RpcError (..)
  , ethCall
  , ethCallAtBlock
  )

-- | Complete account state returned by
-- @CfdEngineAccountLens.getAccountLedgerSnapshot(address)@.
--
-- All monetary values use the contracts' native settlement asset precision. The two
-- signed fields are decoded as two's-complement @int256@ values.
data AccountLedgerSnapshot = AccountLedgerSnapshot
  { alsSettlementBalanceAssets :: Integer
  , alsFreeSettlementAssets :: Integer
  , alsActivePositionMarginAssets :: Integer
  , alsOtherLockedMarginAssets :: Integer
  , alsPositionMarginBucketAssets :: Integer
  , alsCommittedOrderMarginBucketAssets :: Integer
  , alsReservedSettlementBucketAssets :: Integer
  , alsExecutionBountyReserveAssets :: Integer
  , alsCommittedMarginAssets :: Integer
  , alsTraderClaimBalanceAssets :: Integer
  , alsPendingOrderCount :: Integer
  , alsCloseReachableAssets :: Integer
  , alsLiquidationReachableSettlementAssets :: Integer
  , alsTerminalPriceCollectibleCapAssets :: Integer
  , alsAccountEquityAssets :: Integer
  , alsFreeBuyingPowerAssets :: Integer
  , alsHasPosition :: Bool
  , alsSide :: Integer
  , alsSize :: Integer
  , alsMargin :: Integer
  , alsEntryPrice :: Integer
  , alsUnrealizedPnlAssets :: Integer
  , alsNetEquityAssets :: Integer
  , alsLiquidatable :: Bool
  }
  deriving stock (Show, Eq)

getAccountLedgerSnapshot
  :: EthClient
  -> Text
  -> Text
  -> IO (Either RpcError AccountLedgerSnapshot)
getAccountLedgerSnapshot client accountLens account = do
  result <- ethCall client $ CallParams accountLens $ getAccountLedgerSnapshotCall account
  pure $ decodeRpcResult result

getAccountLedgerSnapshotAtBlock
  :: EthClient
  -> Text
  -> Text
  -> Integer
  -> IO (Either RpcError AccountLedgerSnapshot)
getAccountLedgerSnapshotAtBlock client accountLens account blockNumber = do
  result <-
    ethCallAtBlock
      client
      (CallParams accountLens $ getAccountLedgerSnapshotCall account)
      blockNumber
  pure $ decodeRpcResult result

getAccountLedgerSnapshotCall :: Text -> ByteString
getAccountLedgerSnapshotCall account =
  encodeCall "getAccountLedgerSnapshot(address)" [encodeAddress account]

-- | The V2 open planner populates pending carry before rejecting a zero-size
-- order. This read-only diagnostic exposes the full charge even when carry
-- has exhausted the pledge and the account lens's price-risk equity cannot
-- reveal how much additional carry consumes free settlement.
getPendingCarryAtBlock :: EthClient -> Text -> Text -> Integer -> IO (Either RpcError Integer)
getPendingCarryAtBlock client engineLens account blockNumber = do
  result <- ethCallAtBlock client (CallParams engineLens $ pendingCarryPreviewCall account) blockNumber
  pure $ case result of
    Left err -> Left err
    Right bytes -> either (Left . RpcJsonError) Right $ decodePendingCarryPreview bytes

pendingCarryPreviewCall :: Text -> ByteString
pendingCarryPreviewCall account =
  encodeCall "previewOpen(address,uint8,uint256,uint256,uint256,uint64)"
    [encodeAddress account, encodeUint256 0, encodeUint256 0, encodeUint256 0, encodeUint256 0, encodeUint256 0]

decodePendingCarryPreview :: ByteString -> Either Text Integer
decodePendingCarryPreview bytes
  | BS.length bytes /= 24 * abiWordLength = Left "Expected 768 bytes for V2 open carry preview"
  | otherwise = Right $ decodeUint256 $ wordAt bytes 11

decodeAccountLedgerSnapshot :: ByteString -> Either Text AccountLedgerSnapshot
decodeAccountLedgerSnapshot bytes
  | BS.length bytes /= encodedSnapshotLength =
      Left $
        "Expected "
          <> T.pack (show encodedSnapshotLength)
          <> " bytes for AccountLedgerSnapshot, received "
          <> T.pack (show $ BS.length bytes)
  | otherwise =
      Right $
        AccountLedgerSnapshot
          { alsSettlementBalanceAssets = uintAt 0
          , alsFreeSettlementAssets = uintAt 1
          , alsActivePositionMarginAssets = uintAt 2
          , alsOtherLockedMarginAssets = uintAt 3
          , alsPositionMarginBucketAssets = uintAt 4
          , alsCommittedOrderMarginBucketAssets = uintAt 5
          , alsReservedSettlementBucketAssets = uintAt 6
          , alsExecutionBountyReserveAssets = uintAt 7
          , alsCommittedMarginAssets = uintAt 8
          , alsTraderClaimBalanceAssets = uintAt 9
          , alsPendingOrderCount = uintAt 10
          , alsCloseReachableAssets = uintAt 11
          , alsLiquidationReachableSettlementAssets = uintAt 12
          , alsTerminalPriceCollectibleCapAssets = uintAt 13
          , alsAccountEquityAssets = uintAt 14
          , alsFreeBuyingPowerAssets = uintAt 15
          , alsHasPosition = boolAt 16
          , alsSide = uintAt 17
          , alsSize = uintAt 18
          , alsMargin = uintAt 19
          , alsEntryPrice = uintAt 20
          , alsUnrealizedPnlAssets = intAt 21
          , alsNetEquityAssets = intAt 22
          , alsLiquidatable = boolAt 23
          }
  where
    uintAt = decodeUint256 . wordAt bytes
    intAt = decodeInt256 . wordAt bytes
    boolAt = decodeBool . wordAt bytes

encodedSnapshotLength :: Int
encodedSnapshotLength = snapshotWordCount * abiWordLength

snapshotWordCount :: Int
snapshotWordCount = 24

abiWordLength :: Int
abiWordLength = 32

wordAt :: ByteString -> Int -> ByteString
wordAt bytes index = BS.take abiWordLength $ BS.drop (index * abiWordLength) bytes

decodeRpcResult
  :: Either RpcError ByteString
  -> Either RpcError AccountLedgerSnapshot
decodeRpcResult = \case
  Left err -> Left err
  Right bytes ->
    case decodeAccountLedgerSnapshot bytes of
      Left err -> Left $ RpcJsonError err
      Right snapshot -> Right snapshot
