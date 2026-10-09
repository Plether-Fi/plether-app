-- | Canonical Across relay and margin-credit evidence. Shared handler events
-- alone cannot identify an intent: every outcome is scoped to a source relay
-- and the corresponding destination SpokePool fill/callback log interval.
module Plether.Perps.Funding.Chain
  ( verifyFundingDeployment, verifyFundingDeploymentAt, sourceDepositProof, depositProof
  , fundsDepositedTopic, filledRelayTopic, depositForTopic, originSpokePool
  , encodeHex, decodeHex, requireRpc
  ) where

import Control.Monad (filterM, unless)
import Control.Monad.Trans.Except
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.List (nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi
import Plether.Ethereum.Client
import Plether.Ethereum.Rpc
import Plether.Perps.Funding.Types
import Plether.Utils.Hex (intToHex)

encodeHex :: ByteString -> Text
encodeHex = ("0x" <>) . TE.decodeUtf8 . B16.encode
decodeHex :: Text -> Either Text ByteString
decodeHex raw
  | not ("0x" `T.isPrefixOf` raw) = Left "Invalid hex data"
  | otherwise = either (const $ Left "Invalid hex data") Right $ B16.decode $ TE.encodeUtf8 $ T.drop 2 raw
requireRpc :: IO (Either RpcError a) -> ExceptT Text IO a
requireRpc action = ExceptT $ either (const $ Left "DESTINATION_RPC_UNAVAILABLE") Right <$> action

originSpokePool, originUsdc, eventEmitter :: Text
originSpokePool = "0x5c7bcd6e7de5423a257d81b442095a1a6ced35c5"
originUsdc = "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48"
eventEmitter = "0xbf75133b48b0a42ab9374027902e83c5e2949034"

verifyFundingDeployment :: EthClient -> FundingDeployment -> IO (Either Text ())
verifyFundingDeployment client deployment = runExceptT $ do
  block <- requireRpc $ ethLatestBlock client
  ExceptT $ verifyFundingDeploymentAt client deployment block

-- Code, proxy implementation and settlement binding are checked at one block.
-- A proxy runtime hash alone would not attest the fill/callback ordering.
verifyFundingDeploymentAt :: EthClient -> FundingDeployment -> RpcBlock -> IO (Either Text ())
verifyFundingDeploymentAt client deployment block = runExceptT $ do
  chain <- requireRpc $ ethChainId client
  unless (chain == fdChainId deployment) $ throwE "FUNDING_DEPLOYMENT_MISMATCH"
  let blockTag = String $ "0x" <> intToHex (rpcBlockNumber block)
      dataResult action = requireRpc action >>= \case
        String raw -> either throwE pure $ decodeHex raw
        _ -> throwE "FUNDING_DEPLOYMENT_MISMATCH"
      code address = dataResult $ rpcCall client "eth_getCode" $ toJSONValues [String address,blockTag]
      pins = [(fdClearinghouse deployment,fdClearinghouseCodeHash deployment)
             ,(fdDestinationSpokePool deployment,fdDestinationSpokePoolCodeHash deployment)
             ,(fdDestinationSpokePoolImplementation deployment,fdDestinationSpokePoolImplementationCodeHash deployment)
             ,(fdMulticallHandler deployment,fdMulticallHandlerCodeHash deployment)
             ,(eventEmitter,"0x833b49ceddf001f197603e23297ddc21147b7d9e61adbe812e1167b3a7fe2fe5")]
  mapM_ (\(address,expected) -> do
    bytes <- code address
    unless (not (BS.null bytes) && encodeHex (keccak256 bytes) == expected) $ throwE "FUNDING_DEPLOYMENT_MISMATCH") pins
  implementation <- dataResult $ rpcCall client "eth_getStorageAt" $ toJSONValues
    [String $ fdDestinationSpokePool deployment
    ,String "0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc",blockTag]
  unless (implementation == encodeAddress (fdDestinationSpokePoolImplementation deployment)) $ throwE "ACROSS_IMPLEMENTATION_MISMATCH"
  settlement <- requireRpc $ ethCallAtBlock client (CallParams (fdClearinghouse deployment) $ encodeCall "settlementAsset()" []) (rpcBlockNumber block)
  unless (settlement == encodeAddress (fdToken deployment)) $ throwE "FUNDING_DEPLOYMENT_MISMATCH"
  canonical <- requireRpc $ ethGetBlockByNumber client $ rpcBlockNumber block
  unless (rpcBlockHash canonical == rpcBlockHash block) $ throwE "FUNDING_DEPLOYMENT_REORGED"

-- Avoid exposing Vector construction to the proof code below.
toJSONValues :: [Value] -> Value
toJSONValues = Data.Aeson.toJSON

fundsDepositedTopic, filledRelayTopic, depositForTopic :: ByteString
fundsDepositedTopic = keccak256 "FundsDeposited(bytes32,bytes32,uint256,uint256,uint256,uint256,uint32,uint32,uint32,bytes32,bytes32,bytes32,bytes)"
filledRelayTopic = keccak256 "FilledRelay(bytes32,bytes32,uint256,uint256,uint256,uint256,uint256,uint32,uint32,bytes32,bytes32,bytes32,bytes32,bytes32,(bytes32,bytes32,uint256,uint8))"
depositForTopic = keccak256 "DepositFor(address,address,uint256)"

-- The source transaction is independently matched to the exact reviewed plan
-- before calling this parser. The event determines proportional-swap amounts.
sourceDepositProof :: FundingDeployment -> Value -> Integer -> RpcBlock -> TxReceipt -> Either Text (Maybe Value)
sourceDepositProof deployment intent headBlock canonical receipt = do
  canonicalReceipt 2 headBlock canonical receipt >>= \ready -> if not ready then Right Nothing else do
    owner <- required "ownerAddress" intent
    message <- required "destinationMessage" intent >>= decodeHex
    minimumAmount <- numeric "minimumAmount" intent
    let matches entry = scoped receipt originSpokePool entry && case rpcLogTopics entry of
          [topic,destination,_,depositor] -> topic == fundsDepositedTopic
            && destination == encodeUint256 (fdChainId deployment) && depositor == encodeAddress owner
          _ -> False
    candidates <- mapM (parseDeposit owner message minimumAmount) $ filter matches $ receiptLogs receipt
    case [relay | Just relay <- candidates] of
      [] -> Right Nothing
      [relay] -> Right $ Just relay
      _ -> Left "AMBIGUOUS_SOURCE_DEPOSIT"
  where
    parseDeposit owner message minimumAmount entry = do
      let bytes = rpcLogData entry
      unless (BS.length bytes >= 352) $ Left "INVALID_SOURCE_DEPOSIT"
      msg <- dynamicBytes 10 9 bytes
      if msg /= message then Right Nothing else do
        unless (word 0 bytes == encodeAddress originUsdc && word 1 bytes == encodeAddress (fdToken deployment)
          && word 7 bytes == encodeAddress (fdMulticallHandler deployment)
          && uint 2 bytes > 0 && uint 3 bytes >= minimumAmount
          && all (< 2^(32 :: Int)) [uint 4 bytes,uint 5 bytes,uint 6 bytes]
          && BS.take 12 (word 8 bytes) == BS.replicate 12 0) $ Left "SOURCE_RELAY_BINDING_MISMATCH"
        let depositId = decodeUint256 $ rpcLogTopics entry !! 2
            inputAmount = uint 2 bytes
            outputAmount = uint 3 bytes
            fillDeadline = uint 5 bytes
            exclusivity = uint 6 bytes
            exclusive = decodeAddress $ word 8 bytes
            -- abi.encode(V3RelayData, destinationChainId), including tuple offset.
            encodedRelay = BS.concat
              [encodeUint256 64,encodeUint256 $ fdChainId deployment
              ,encodeAddress owner,encodeAddress $ fdMulticallHandler deployment,encodeAddress exclusive
              ,encodeAddress originUsdc,encodeAddress $ fdToken deployment
              ,encodeUint256 inputAmount,encodeUint256 outputAmount,encodeUint256 1,encodeUint256 depositId
              ,encodeUint256 fillDeadline,encodeUint256 exclusivity,encodeUint256 384,encodeDynamic message]
        Right $ Just $ object
          ["originChainId" .= (1 :: Integer),"sourceSpokePool" .= originSpokePool,"depositId" .= show depositId
          ,"relayHash" .= encodeHex (keccak256 encodedRelay),"sourceTxHash" .= receiptTxHash receipt
          ,"sourceBlockNumber" .= show (receiptBlockNumber receipt),"sourceBlockHash" .= receiptBlockHash receipt
          ,"sourceLogIndex" .= show (rpcLogIndex entry),"inputToken" .= originUsdc,"outputToken" .= fdToken deployment
          ,"inputAmount" .= show inputAmount,"outputAmount" .= show outputAmount,"fillDeadline" .= show fillDeadline
          ,"exclusivityDeadline" .= show exclusivity,"exclusiveRelayer" .= exclusive,"depositor" .= owner
          ,"recipient" .= fdMulticallHandler deployment,"messageHash" .= encodeHex (keccak256 message)]

-- A matching FilledRelay starts the exact callback interval. It is emitted
-- before token delivery/callback; the next pool fill ends the interval. The
-- fixed recipe's marker (success) or CallsFailed+drain (fallback) ends proof.
depositProof :: FundingDeployment -> Value -> Value -> Integer -> RpcBlock -> TxReceipt -> Either Text (Maybe Value)
depositProof deployment intent relay headBlock canonical receipt = do
  canonicalReceipt (fdConfirmations deployment) headBlock canonical receipt >>= \ready -> if not ready then Right Nothing else do
    account <- required "beneficiary" intent
    quoteId <- required "quoteId" intent >>= decodeHex
    message <- required "destinationMessage" intent >>= decodeHex
    relayId <- numeric "depositId" relay
    origin <- maybe (Left "INVALID_SOURCE_RELAY") Right $ fieldInteger "originChainId" relay
    let allLogs = sortOn rpcLogIndex $ receiptLogs receipt
        fills = filter (\entry -> scoped receipt (fdDestinationSpokePool deployment) entry
          && case rpcLogTopics entry of [topic,_,_,_] -> topic == filledRelayTopic; _ -> False) allLogs
        matches entry = take 3 (rpcLogTopics entry) == [filledRelayTopic,encodeUint256 origin,encodeUint256 relayId]
    matching <- filterM (matchesOriginalRelay relay) $ filter matches fills
    case matching of
      [] -> Right Nothing
      [fill] -> do
        verifyExecution relay fill
        let nextIndex = minimum $ (1 + maximum (map rpcLogIndex allLogs)) : [rpcLogIndex entry | entry <- fills,rpcLogIndex entry > rpcLogIndex fill]
            interval = filter (\entry -> rpcLogIndex entry > rpcLogIndex fill && rpcLogIndex entry < nextIndex) allLogs
            marker entry = scoped receipt eventEmitter entry
              && rpcLogTopics entry == [keccak256 "MetadataEmitted(bytes)"]
              && rpcLogData entry == encodeUint256 32 <> encodeDynamic quoteId
            failure entry = scoped receipt (fdMulticallHandler deployment) entry
              && rpcLogTopics entry == [keccak256 "CallsFailed((address,bytes,uint256)[],address)",encodeAddress account]
              && rpcLogData entry == callsFailedData message
            terminals = filter (\entry -> marker entry || failure entry) interval
            base = [("fillTxHash",String $ receiptTxHash receipt),("fillBlockNumber",String $ T.pack $ show $ receiptBlockNumber receipt)
                   ,("fillBlockHash",String $ receiptBlockHash receipt),("fillLogIndex",String $ T.pack $ show $ rpcLogIndex fill)]
        case terminals of
          terminal:_ | failure terminal -> fallbackProof account relay base terminal interval
          terminal:_ -> creditProof account base terminal interval
          [] -> Left "DESTINATION_ACTION_EVIDENCE_MISSING"
      _ -> Left "DUPLICATE_RELAY_FILL"
  where
    -- A source reorg can reuse a deposit ID with different relay contents.
    -- That historical fill is unrelated, not evidence of this intent failing.
    matchesOriginalRelay source entry = do
      let bytes = rpcLogData entry
      unless (BS.length bytes == 480) $ Left "INVALID_FILL_EVENT"
      input <- required "inputToken" source
      output <- required "outputToken" source
      depositor <- required "depositor" source
      recipient <- required "recipient" source
      exclusive <- required "exclusiveRelayer" source
      messageHash <- required "messageHash" source >>= decodeHex
      amounts <- mapM (`numeric` source) ["inputAmount","outputAmount","fillDeadline","exclusivityDeadline"]
      case amounts of
        [inputAmount,outputAmount,deadline,exclusivity] -> pure
          (word 0 bytes == encodeAddress input && word 1 bytes == encodeAddress output
           && uint 2 bytes == inputAmount && uint 3 bytes == outputAmount
           && uint 5 bytes == deadline && uint 6 bytes == exclusivity && word 7 bytes == encodeAddress exclusive
           && word 8 bytes == encodeAddress depositor && word 9 bytes == encodeAddress recipient && word 10 bytes == messageHash)
        _ -> Left "INVALID_SOURCE_RELAY"
    verifyExecution source entry = do
      recipient <- required "recipient" source
      messageHash <- required "messageHash" source >>= decodeHex
      outputAmount <- numeric "outputAmount" source
      let bytes = rpcLogData entry
      unless (word 11 bytes == encodeAddress recipient && word 12 bytes == messageHash
        && uint 13 bytes == outputAmount && uint 14 bytes <= 2) $ Left "DESTINATION_RELAY_BINDING_MISMATCH"
    creditProof account base terminal interval = do
      let before = filter ((< rpcLogIndex terminal) . rpcLogIndex) interval
          credits = filter (\entry -> scoped receipt (fdClearinghouse deployment) entry
            && rpcLogTopics entry == [depositForTopic,encodeAddress $ fdMulticallHandler deployment,encodeAddress account]
            && BS.length (rpcLogData entry) == 32) before
      case credits of
        [metadata] -> do
          let amount = decodeUint256 $ rpcLogData metadata
              canonicalDeposit entry = scoped receipt (fdClearinghouse deployment) entry
                && rpcLogIndex entry + 1 == rpcLogIndex metadata
                && rpcLogTopics entry == [keccak256 "Deposit(address,address,uint256)",encodeAddress account,encodeAddress $ fdToken deployment]
                && rpcLogData entry == encodeUint256 amount
          minimumAmount <- numeric "minimumAmount" intent
          outputAmount <- numeric "outputAmount" relay
          unless (amount >= minimumAmount && amount >= outputAmount && any canonicalDeposit before
            && any (transfer (fdMulticallHandler deployment) (fdClearinghouse deployment) amount) before) $ Left "DEPOSIT_EVENT_PAIR_MISSING"
          Right $ Just $ setFields (base <>
            [("status",String "confirmed"),("depositTxHash",String $ receiptTxHash receipt)
            ,("depositBlockNumber",String $ T.pack $ show $ receiptBlockNumber receipt),("depositBlockHash",String $ receiptBlockHash receipt)
            ,("depositLogIndices",Data.Aeson.toJSON [show $ rpcLogIndex metadata - 1,show $ rpcLogIndex metadata])
            ,("creditedAmount",String $ T.pack $ show amount)]) $ object []
        _ -> Left "AMBIGUOUS_MARGIN_CREDIT"
    fallbackProof account source base failed interval = do
      outputAmount <- numeric "outputAmount" source
      let after = filter ((> rpcLogIndex failed) . rpcLogIndex) interval
          drains = filter (\entry -> scoped receipt (fdMulticallHandler deployment) entry
            && take 3 (rpcLogTopics entry) == [keccak256 "DrainedTokens(address,address,uint256)",encodeAddress account,encodeAddress $ fdToken deployment]
            && length (rpcLogTopics entry) == 4
            && decodeUint256 (rpcLogTopics entry !! 3) >= outputAmount
            && BS.null (rpcLogData entry)) after
      case drains of
        drained:_ -> do
          -- The handler drains its entire balance, including unsolicited dust.
          -- Attribute only an actual matching token transfer, never the quote.
          let amount = decodeUint256 $ rpcLogTopics drained !! 3
              transfers = filter (\entry -> rpcLogIndex entry < rpcLogIndex drained
                && transfer (fdMulticallHandler deployment) account amount entry) after
          unless (length transfers == 1) $ Left "FALLBACK_TRANSFER_EVIDENCE_MISSING"
          Right $ Just $ setFields (base <>
            [("status",String "needs-deposit"),("fallbackTxHash",String $ receiptTxHash receipt)
            ,("fallbackBlockNumber",String $ T.pack $ show $ receiptBlockNumber receipt),("fallbackBlockHash",String $ receiptBlockHash receipt)
            ,("fallbackAmount",String $ T.pack $ show amount)
            ,("fallbackLogIndices",Data.Aeson.toJSON $ map (show . rpcLogIndex) (transfers <> [drained]))
            ,("creditedAmount",String "0")]) $ object []
        [] -> Left "FALLBACK_TRANSFER_EVIDENCE_MISSING"
    transfer from to amount entry = scoped receipt (fdToken deployment) entry
      && rpcLogTopics entry == [keccak256 "Transfer(address,address,uint256)",encodeAddress from,encodeAddress to]
      && rpcLogData entry == encodeUint256 amount

-- Instructions encodes one dynamic tuple. CallsFailed encodes that tuple's
-- dynamic calls array as the sole nonindexed argument.
callsFailedData :: ByteString -> ByteString
callsFailedData message = encodeUint256 32 <> BS.drop 96 message

canonicalReceipt :: Integer -> Integer -> RpcBlock -> TxReceipt -> Either Text Bool
canonicalReceipt confirmations headBlock canonical receipt = do
  unless (rpcBlockNumber canonical == receiptBlockNumber receipt && rpcBlockHash canonical == receiptBlockHash receipt) $ Left "DEPOSIT_REORGED"
  let indices = map rpcLogIndex $ receiptLogs receipt
  unless (length indices == length (nub indices)) $ Left "DUPLICATE_RECEIPT_LOG_INDEX"
  pure $ receiptSucceeded receipt && headBlock - receiptBlockNumber receipt + 1 >= confirmations
scoped :: TxReceipt -> Text -> RpcLog -> Bool
scoped receipt address entry = T.toLower (rpcLogAddress entry) == address
  && rpcLogTxHash entry == receiptTxHash receipt && rpcLogBlockNumber entry == receiptBlockNumber receipt
  && rpcLogBlockHash entry == receiptBlockHash receipt && rpcLogTransactionIndex entry == receiptTransactionIndex receipt
required :: Text -> Value -> Either Text Text
required key = maybe (Left "INVALID_FUNDING_PROOF") Right . fieldText key
numeric :: Text -> Value -> Either Text Integer
numeric key value = required key value >>= \raw -> case reads (T.unpack raw) of
  [(n,"")] | n >= 0 && n < 2^(256 :: Int) -> Right n
  _ -> Left "INVALID_FUNDING_PROOF"
word :: Int -> ByteString -> ByteString
word index = BS.take 32 . BS.drop (index*32)
uint :: Int -> ByteString -> Integer
uint index = decodeUint256 . word index
encodeDynamic :: ByteString -> ByteString
encodeDynamic bytes = encodeUint256 (fromIntegral $ BS.length bytes) <> bytes <> BS.replicate ((32 - BS.length bytes `mod` 32) `mod` 32) 0
dynamicBytes :: Int -> Int -> ByteString -> Either Text ByteString
dynamicBytes headWords offsetWord bytes = do
  unless (uint offsetWord bytes == fromIntegral (headWords*32)) $ Left "NONCANONICAL_EVENT_DATA"
  let size = uint headWords bytes
  unless (size <= fromIntegral (BS.length bytes)) $ Left "TRUNCATED_EVENT_DATA"
  let result = BS.take (fromIntegral size) $ BS.drop ((headWords+1)*32) bytes
  unless (BS.length bytes == headWords*32 + BS.length (encodeDynamic result)
    && BS.drop (headWords*32) bytes == encodeDynamic result) $ Left "NONCANONICAL_EVENT_DATA"
  Right result
