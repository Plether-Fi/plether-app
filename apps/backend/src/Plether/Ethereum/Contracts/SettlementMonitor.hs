module Plether.Ethereum.Contracts.SettlementMonitor
  ( ExecutionPath (..)
  , SettlementStatus (..)
  , SettlementObservation (..)
  , verifyBindings
  , getCurrentEpoch
  , getSettlementStatus
  , getSettlementObservationAtBlock
  , decodeSettlementStatus
  , decodeSettlementObservation
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi
  ( decodeAddress
  , decodeBool
  , decodeUint256
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

data ExecutionPath
  = UnknownPath
  | NoMaturedWork
  | CachedMark
  | AtomicOracleRefresh
  deriving stock (Show, Eq)

data SettlementStatus = SettlementStatus
  { ssObservedBlock :: Integer
  , ssCurrentEpoch :: Integer
  , ssMinimumAtomicPublishTime :: Integer
  , ssRequiredExecutionPath :: ExecutionPath
  , ssCachedMarkPrice :: Integer
  , ssCachedMarkTime :: Integer
  , ssOperationalBlockerMask :: Integer
  , ssWarningMask :: Integer
  , ssExecutionPathDependencyMask :: Integer
  , ssDependencyFailureMask :: Integer
  , ssHasMaturedWork :: Bool
  , ssLpEpochSettlementPaused :: Bool
  }
  deriving stock (Show, Eq)

data SettlementObservation = SettlementObservation
  { soSchemaVersion :: Integer
  , soStatus :: SettlementStatus
  , soHealthState :: Integer
  , soCriticalFaultMask :: Integer
  , soHealthDependencyFailureMask :: Integer
  , soObservationDigest :: Text
  , soObservationComplete :: Bool
  }
  deriving stock (Show, Eq)

verifyBindings
  :: EthClient
  -> Text
  -> Text
  -> Text
  -> IO (Either Text ())
verifyBindings client monitor expectedRouter expectedHousePool = do
  routerResult <- readAddress client monitor "ROUTER()"
  poolResult <- readAddress client monitor "HOUSE_POOL()"
  pure $ do
    router <- firstRpc routerResult
    pool <- firstRpc poolResult
    if normalize router /= normalize expectedRouter
      then Left $ "Settlement Monitor ROUTER binding mismatch: expected " <> expectedRouter <> ", observed " <> router
      else
        if normalize pool /= normalize expectedHousePool
          then Left $ "Settlement Monitor HOUSE_POOL binding mismatch: expected " <> expectedHousePool <> ", observed " <> pool
          else Right ()

getCurrentEpoch :: EthClient -> Text -> IO (Either RpcError Integer)
getCurrentEpoch client housePool = do
  result <- ethCall client $ CallParams housePool $ encodeCall "currentLpEpoch()" []
  pure $ decodeSingleWord "currentLpEpoch()" result

getSettlementStatus
  :: EthClient
  -> Text
  -> Integer
  -> IO (Either RpcError SettlementStatus)
getSettlementStatus client monitor epoch = do
  result <-
    ethCall client $
      CallParams monitor $
        encodeCall "getSettlementStatus(uint256)" [encodeUint256 epoch]
  pure $ result >>= decodeSettlementStatus

getSettlementObservationAtBlock
  :: EthClient
  -> Text
  -> Integer
  -> Integer
  -> IO (Either RpcError SettlementObservation)
getSettlementObservationAtBlock client monitor epoch blockNumber = do
  result <-
    ethCallAtBlock
      client
      ( CallParams monitor $
          encodeCall "getSettlementObservation(uint256)" [encodeUint256 epoch]
      )
      blockNumber
  pure $ result >>= decodeSettlementObservation

decodeSettlementStatus :: ByteString -> Either RpcError SettlementStatus
decodeSettlementStatus bytes = decodeStatusAt 0 bytes

decodeSettlementObservation :: ByteString -> Either RpcError SettlementObservation
decodeSettlementObservation bytes
  | BS.length bytes < observationWordCount * 32 =
      Left $ RpcJsonError "getSettlementObservation(uint256) returned fewer than 194 ABI words"
  | otherwise = do
      status <- decodeStatusAt 1 bytes
      pure
        SettlementObservation
          { soSchemaVersion = wordAt 0 bytes
          , soStatus = status
          , soHealthState = wordAt 159 bytes
          , soCriticalFaultMask = wordAt 160 bytes
          , soHealthDependencyFailureMask = wordAt 161 bytes
          , soObservationDigest = bytes32Hex $ wordBytesAt 191 bytes
          , soObservationComplete = decodeBool $ wordBytesAt 193 bytes
          }

decodeStatusAt :: Int -> ByteString -> Either RpcError SettlementStatus
decodeStatusAt base bytes
  | BS.length bytes < (base + settlementStatusWordCount) * 32 =
      Left $ RpcJsonError "getSettlementStatus(uint256) returned fewer than 109 ABI words"
  | otherwise =
      pure
        SettlementStatus
          { ssObservedBlock = wordAt (base + 1) bytes
          , ssCurrentEpoch = wordAt (base + 4) bytes
          , ssMinimumAtomicPublishTime = wordAt (base + 11) bytes
          , ssRequiredExecutionPath = decodeExecutionPath $ wordAt (base + 86) bytes
          , ssCachedMarkPrice = wordAt (base + 87) bytes
          , ssCachedMarkTime = wordAt (base + 88) bytes
          , ssOperationalBlockerMask = wordAt (base + 93) bytes
          , ssWarningMask = wordAt (base + 94) bytes
          , ssExecutionPathDependencyMask = wordAt (base + 97) bytes
          , ssDependencyFailureMask = wordAt (base + 98) bytes
          , ssHasMaturedWork = decodeBool $ wordBytesAt (base + 99) bytes
          , ssLpEpochSettlementPaused = decodeBool $ wordBytesAt (base + 108) bytes
          }

decodeExecutionPath :: Integer -> ExecutionPath
decodeExecutionPath = \case
  1 -> NoMaturedWork
  2 -> CachedMark
  3 -> AtomicOracleRefresh
  _ -> UnknownPath

readAddress :: EthClient -> Text -> Text -> IO (Either RpcError Text)
readAddress client target signature = do
  result <- ethCall client $ CallParams target $ encodeCall signature []
  pure $ do
    bytes <- result
    if BS.length bytes < 32
      then Left $ RpcJsonError $ signature <> " returned less than one ABI word"
      else Right $ decodeAddress $ wordBytesAt 0 bytes

decodeSingleWord :: Text -> Either RpcError ByteString -> Either RpcError Integer
decodeSingleWord label result = do
  bytes <- result
  if BS.length bytes < 32
    then Left $ RpcJsonError $ label <> " returned less than one ABI word"
    else Right $ decodeUint256 $ wordBytesAt 0 bytes

firstRpc :: Either RpcError a -> Either Text a
firstRpc = either (Left . T.pack . show) Right

normalize :: Text -> Text
normalize = T.toLower . T.strip

bytes32Hex :: ByteString -> Text
bytes32Hex = ("0x" <>) . TE.decodeUtf8 . B16.encode

wordAt :: Int -> ByteString -> Integer
wordAt index = decodeUint256 . wordBytesAt index

wordBytesAt :: Int -> ByteString -> ByteString
wordBytesAt index = BS.take 32 . BS.drop (index * 32)

settlementStatusWordCount :: Int
settlementStatusWordCount = 109

observationWordCount :: Int
observationWordCount = 194
