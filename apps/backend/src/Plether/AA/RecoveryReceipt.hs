module Plether.AA.RecoveryReceipt (needsReceiptFallback, recoverReceipt, reconstructReceipt) where

import Control.Concurrent.Async (concurrently)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Numeric (readHex)
import Plether.AA.Paymaster (parsePackedUserOperation, PackedUserOperation (..), canonicalQuantity)
import Plether.AA.Reconciler (UserOperationEvent (..), parseUserOperationEvent)
import Plether.Database.AaSponsorship (ReceiptLocator (..))
import Plether.Ethereum.Abi (keccak256)
import Plether.Ethereum.Client (EthClient, rpcCall)

entryPoint :: Text
entryPoint = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"
topic :: Text -> Text
topic = ("0x" <>) . TE.decodeUtf8 . B16.encode . keccak256 . TE.encodeUtf8
opTopic, beforeTopic :: Text
opTopic = topic "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)"
beforeTopic = topic "BeforeExecution()"

fieldText :: Text -> Value -> Either Text Text
fieldText key (Object fields) = case KM.lookup (K.fromText key) fields of
  Just (String value) -> Right $ T.toLower value
  _ -> Left "RECOVERY_MALFORMED_EVIDENCE"
fieldText _ _ = Left "RECOVERY_MALFORMED_EVIDENCE"
quantity :: Text -> Value -> Either Text Integer
quantity key value = do
  raw <- fieldText key value
  case T.stripPrefix "0x" raw of
    Just digits | not (T.null digits) -> case readHex $ T.unpack digits of
      [(number,"")] | number >= 0, number < 2^(256 :: Int) -> Right number
      _ -> Left "RECOVERY_MALFORMED_EVIDENCE"
    _ -> Left "RECOVERY_MALFORMED_EVIDENCE"
ensure :: Bool -> Either Text ()
ensure condition = unless condition $ Left "RECOVERY_EVIDENCE_MISMATCH"

needsReceiptFallback :: Either err (Value, metadata) -> Bool
needsReceiptFallback (Left _) = True
needsReceiptFallback (Right (Object fields, _)) =
  KM.lookup "result" fields == Just Null || KM.member "error" fields
needsReceiptFallback _ = False

-- Uses finalized database evidence only as a locator. Every request verifies
-- the receipt and both canonical boundaries anew. No locks or ledger writes.
recoverReceipt :: EthClient -> Integer -> Text -> Text -> ReceiptLocator -> IO (Either Text Value)
recoverReceipt client chainId paymaster hash locator = runExceptT $ do
  let fetch method params = ExceptT $ do
        result <- rpcCall client method $ toJSON params
        pure $ either (const $ Left "RECOVERY_PROVIDER_UNAVAILABLE") Right result
      check = ExceptT . pure
  (chain,safe) <- ExceptT $ do
    (a,b) <- concurrently (rpcCall client "eth_chainId" $ toJSON ([] :: [Value]))
      (rpcCall client "eth_getBlockByNumber" $ toJSON [String "safe",Bool False])
    pure $ either (const $ Left "RECOVERY_PROVIDER_UNAVAILABLE") Right $ (,) <$> a <*> b
  check $ ensure $ chain == String (canonicalQuantity chainId)
  number <- check $ quantity "number" safe
  timestamp <- check $ quantity "timestamp" safe
  safeHash <- check $ fieldText "hash" safe
  check $ ensure $ rlBlockNumber locator <= number
  receipt <- fetch "eth_getTransactionReceipt" [String $ rlTransactionHash locator]
  reconstructed <- check $ reconstructReceipt paymaster hash locator receipt
  (header,safeAgain) <- liftIO $ concurrently
    (rpcCall client "eth_getBlockByNumber" $ toJSON [String $ canonicalQuantity $ rlBlockNumber locator,Bool False])
    (rpcCall client "eth_getBlockByNumber" $ toJSON [String $ canonicalQuantity number,Bool False])
  canonical <- check $ either (const $ Left "RECOVERY_PROVIDER_UNAVAILABLE") Right header
  boundary <- check $ either (const $ Left "RECOVERY_PROVIDER_UNAVAILABLE") Right safeAgain
  blockHash <- check $ fieldText "hash" canonical
  blockNumber <- check $ quantity "number" canonical
  boundaryHash <- check $ fieldText "hash" boundary
  boundaryNumber <- check $ quantity "number" boundary
  currentSafe <- fetch "eth_getBlockByNumber" [String "safe",Bool False]
  currentNumber <- check $ quantity "number" currentSafe
  currentTime <- check $ quantity "timestamp" currentSafe
  currentHash <- check $ fieldText "hash" currentSafe
  check $ ensure $ blockHash == T.toLower (rlBlockHash locator) && blockNumber == rlBlockNumber locator
    && boundaryHash == safeHash && boundaryNumber == number && currentNumber >= number && currentTime >= timestamp
    && (currentNumber /= number || currentHash == safeHash)
  pure reconstructed

reconstructReceipt :: Text -> Text -> ReceiptLocator -> Value -> Either Text Value
reconstructReceipt paymaster expectedHash locator receipt = do
  tx <- fieldText "transactionHash" receipt
  number <- quantity "blockNumber" receipt
  blockHash <- fieldText "blockHash" receipt
  status <- quantity "status" receipt
  ensure $ tx == T.toLower (rlTransactionHash locator) && number == rlBlockNumber locator
    && blockHash == T.toLower (rlBlockHash locator) && status == 1
  stored <- parseUserOperationEvent paymaster number number $ rlEvent locator
  operation <- case rlOperation locator of
    Object fields -> parsePackedUserOperation fields
    _ -> Left "RECOVERY_MALFORMED_EVIDENCE"
  ensure $ puoSender operation == T.toLower (rlSender locator) && puoNonce operation == rlNonce locator
    && maybe True ((== T.toLower paymaster) . T.toLower) (puoPaymaster operation)
  logs <- case receipt of
    Object fields | Just (Array values) <- KM.lookup "logs" fields -> Right $ V.toList values
    _ -> Left "RECOVERY_MALFORMED_EVIDENCE"
  indices <- traverse (quantity "logIndex") logs
  ensure $ and $ zipWith (<) indices $ drop 1 indices
  let topics value = case value of
        Object fields | Just (Array values) <- KM.lookup "topics" fields -> V.toList values
        _ -> []
      emitted value = fieldText "address" value == Right entryPoint
      isEvent value = emitted value && take 1 (topics value) == [String opTopic]
      matching value = isEvent value && take 2 (topics value) == [String opTopic,String $ T.toLower expectedHash]
  event <- case filter matching logs of
    [value] -> parseUserOperationEvent paymaster number number value
    _ -> Left "RECOVERY_EVENT_MISSING_OR_AMBIGUOUS"
  ensure $ uoeHash event == T.toLower expectedHash && uoeSender event == T.toLower (rlSender locator)
    && uoeNonce event == rlNonce locator && uoeTransactionHash event == tx && uoeBlockHash event == blockHash
    && uoeSuccess event == rlSuccess locator && uoeActualGasCost event == rlGasCost locator
    && stored {uoeRaw = Null} == event {uoeRaw = Null}
  let preceding = [index | (value,index) <- zip logs indices, index < uoeLogIndex event,
        isEvent value || (emitted value && topics value == [String beforeTopic])]
  start <- case reverse preceding of
    first : _ -> Right first
    [] -> Left "RECOVERY_BUNDLE_BOUNDARY_MISSING"
  let operationLogs = [value | (value,index) <- zip logs indices, index > start, index <= uoeLogIndex event]
  mapM_ (\value -> do
    logTx <- fieldText "transactionHash" value
    logBlock <- fieldText "blockHash" value
    logNumber <- quantity "blockNumber" value
    ensure $ logTx == tx && logBlock == blockHash && logNumber == number
    case value of
      Object fields -> ensure $ KM.lookup "removed" fields `elem` [Nothing,Just (Bool False)]
      _ -> Left "RECOVERY_MALFORMED_EVIDENCE") operationLogs
  pure $ object ["userOpHash" .= uoeHash event,"sender" .= uoeSender event,"entryPoint" .= entryPoint,
    "nonce" .= canonicalQuantity (uoeNonce event),"paymaster" .= T.toLower paymaster,
    "actualGasCost" .= canonicalQuantity (uoeActualGasCost event),
    "actualGasUsed" .= canonicalQuantity (uoeActualGasUsed event),"success" .= uoeSuccess event,
    "logs" .= operationLogs,"receipt" .= receipt]
