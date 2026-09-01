module Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( AccountLedgerSnapshot (..)
  , getAccountLedgerSnapshot
  , getAccountLedgerSnapshotAtBlock
  , getAccountLedgerSnapshotCall
  , decodeAccountLedgerSnapshot
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
-- All monetary values use the contracts' native USDC precision. The two
-- signed fields are decoded as two's-complement @int256@ values.
data AccountLedgerSnapshot = AccountLedgerSnapshot
  { alsSettlementBalanceUsdc :: Integer
  , alsFreeSettlementUsdc :: Integer
  , alsActivePositionMarginUsdc :: Integer
  , alsOtherLockedMarginUsdc :: Integer
  , alsPositionMarginBucketUsdc :: Integer
  , alsCommittedOrderMarginBucketUsdc :: Integer
  , alsReservedSettlementBucketUsdc :: Integer
  , alsExecutionBountyReserveUsdc :: Integer
  , alsCommittedMarginUsdc :: Integer
  , alsTraderClaimBalanceUsdc :: Integer
  , alsPendingOrderCount :: Integer
  , alsCloseReachableUsdc :: Integer
  , alsLiquidationReachableSettlementUsdc :: Integer
  , alsTerminalPriceCollectibleCapUsdc :: Integer
  , alsAccountEquityUsdc :: Integer
  , alsFreeBuyingPowerUsdc :: Integer
  , alsHasPosition :: Bool
  , alsSide :: Integer
  , alsSize :: Integer
  , alsMargin :: Integer
  , alsEntryPrice :: Integer
  , alsUnrealizedPnlUsdc :: Integer
  , alsNetEquityUsdc :: Integer
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
          { alsSettlementBalanceUsdc = uintAt 0
          , alsFreeSettlementUsdc = uintAt 1
          , alsActivePositionMarginUsdc = uintAt 2
          , alsOtherLockedMarginUsdc = uintAt 3
          , alsPositionMarginBucketUsdc = uintAt 4
          , alsCommittedOrderMarginBucketUsdc = uintAt 5
          , alsReservedSettlementBucketUsdc = uintAt 6
          , alsExecutionBountyReserveUsdc = uintAt 7
          , alsCommittedMarginUsdc = uintAt 8
          , alsTraderClaimBalanceUsdc = uintAt 9
          , alsPendingOrderCount = uintAt 10
          , alsCloseReachableUsdc = uintAt 11
          , alsLiquidationReachableSettlementUsdc = uintAt 12
          , alsTerminalPriceCollectibleCapUsdc = uintAt 13
          , alsAccountEquityUsdc = uintAt 14
          , alsFreeBuyingPowerUsdc = uintAt 15
          , alsHasPosition = boolAt 16
          , alsSide = uintAt 17
          , alsSize = uintAt 18
          , alsMargin = uintAt 19
          , alsEntryPrice = uintAt 20
          , alsUnrealizedPnlUsdc = intAt 21
          , alsNetEquityUsdc = intAt 22
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
