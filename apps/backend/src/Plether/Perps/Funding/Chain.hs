module Plether.Perps.Funding.Chain
  ( verifyAcrossDestinations, verifyFundingDeployment, predictReceiver, verifyReceiver, depositProof
  , depositForTopic, encodeHex, decodeHex, requireRpc
  ) where

import Control.Monad (unless)
import Control.Monad.Trans.Except
import Data.Aeson (Value, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.List (find, nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi
import Plether.Ethereum.Client
import Plether.Ethereum.Rpc
import Plether.Perps.Funding.Types

encodeHex :: ByteString -> Text
encodeHex = ("0x" <>) . TE.decodeUtf8 . B16.encode
decodeHex :: Text -> Either Text ByteString
decodeHex = either (const $ Left "Invalid hex data") Right . B16.decode . TE.encodeUtf8 . T.drop 2
requireRpc :: IO (Either RpcError a) -> ExceptT Text IO a
requireRpc action = ExceptT $ either (const $ Left "DESTINATION_RPC_UNAVAILABLE") Right <$> action

verifyFundingDeployment :: EthClient -> FundingDeployment -> IO (Either Text ())
verifyFundingDeployment client deployment = runExceptT $ do
  chain <- requireRpc $ ethChainId client
  code <- requireRpc $ ethGetCode client (fdFactory deployment)
  chCode <- requireRpc $ ethGetCode client (fdClearinghouse deployment)
  ch <- getter (fdFactory deployment) "clearinghouse()"
  token <- getter (fdFactory deployment) "usdc()"
  settlement <- getter (fdClearinghouse deployment) "settlementAsset()"
  unless (chain == fdChainId deployment && not (BS.null code)
    && encodeHex (keccak256 code) == fdFactoryCodeHash deployment
    && not (BS.null chCode) && encodeHex (keccak256 chCode) == fdClearinghouseCodeHash deployment
    && ch == fdClearinghouse deployment && token == fdToken deployment && settlement == fdToken deployment) $
      throwE "FUNDING_DEPLOYMENT_MISMATCH"
  where
    getter address sig = do
      bytes <- requireRpc $ ethCall client (CallParams address $ encodeCall sig [])
      unless (BS.length bytes == 32) $ throwE "FUNDING_DEPLOYMENT_MISMATCH"
      pure $ decodeAddress bytes

predictReceiver :: EthClient -> FundingDeployment -> Text -> Text -> IO (Either Text Text)
predictReceiver client deployment account salt = runExceptT $ do
  saltBytes <- either throwE pure $ decodeHex salt
  bytes <- requireRpc $ ethCall client $ CallParams (fdFactory deployment) $
    encodeCall "predictReceiver(address,bytes32)" [encodeAddress account,encodeBytes32 saltBytes]
  unless (BS.length bytes == 32) $ throwE "INVALID_RECEIVER_PREDICTION"
  either throwE pure $ validateAddress $ decodeAddress bytes

verifyReceiver :: EthClient -> FundingDeployment -> Value -> IO (Either Text Bool)
verifyReceiver client deployment intent = runExceptT $ do
  account <- required "beneficiary"
  salt <- required "intentSalt"
  receiver <- required "receiver"
  predicted <- ExceptT $ predictReceiver client deployment account salt
  unless (predicted == receiver) $ throwE "RECEIVER_BINDING_MISMATCH"
  code <- requireRpc $ ethGetCode client receiver
  if BS.null code then pure False else do
    values <- mapM (\sig -> requireRpc $ ethCall client $ CallParams receiver $ encodeCall sig [])
      ["beneficiary()","clearinghouse()","usdc()"]
    unless (all ((== 32) . BS.length) values && map decodeAddress values == [account,fdClearinghouse deployment,fdToken deployment]) $
      throwE "RECEIVER_BINDING_MISMATCH"
    pure True
  where required key = maybe (throwE "INVALID_STORED_INTENT") pure $ fieldText key intent

depositForTopic :: ByteString
depositForTopic = keccak256 "DepositFor(address,address,uint256)"

-- | A provider success/webhook or an unrelated beneficiary deposit is not
-- evidence. Require the release clearinghouse's ordered canonical event pair,
-- exact receiver payer, beneficiary, token, amount and receipt/block identities.
depositProof :: FundingDeployment -> Value -> Integer -> RpcBlock -> TxReceipt -> Either Text (Maybe Value)
depositProof deployment intent headBlock canonical receipt = do
  account <- maybe (Left "INVALID_STORED_INTENT") Right $ fieldText "beneficiary" intent
  receiver <- maybe (Left "INVALID_STORED_INTENT") Right $ fieldText "receiver" intent
  unless (rpcBlockNumber canonical == receiptBlockNumber receipt && rpcBlockHash canonical == receiptBlockHash receipt) $
    Left "DEPOSIT_REORGED"
  if not (receiptSucceeded receipt) || headBlock - receiptBlockNumber receipt + 1 < fdConfirmations deployment
    then Right Nothing
    else do
      let scoped logEntry = T.toLower (rpcLogAddress logEntry) == fdClearinghouse deployment
            && rpcLogTxHash logEntry == receiptTxHash receipt
            && rpcLogBlockNumber logEntry == receiptBlockNumber receipt
            && rpcLogBlockHash logEntry == receiptBlockHash receipt
          canonicalDeposit logEntry amount = scoped logEntry
            && rpcLogTopics logEntry == [keccak256 "Deposit(address,address,uint256)",encodeAddress account,encodeAddress (fdToken deployment)]
            && BS.length (rpcLogData logEntry) == 32 && decodeUint256 (rpcLogData logEntry) == amount
          proofFor logEntry = scoped logEntry
            && rpcLogTopics logEntry == [depositForTopic,encodeAddress receiver,encodeAddress account]
            && BS.length (rpcLogData logEntry) == 32 && decodeUint256 (rpcLogData logEntry) > 0
          matches = filter proofFor $ receiptLogs receipt
      let indices = map rpcLogIndex $ receiptLogs receipt
      unless (length indices == length (nub indices)) $ Left "DUPLICATE_RECEIPT_LOG_INDEX"
      if null matches then Right Nothing else do
        mapM_ (\metadata -> case find (\entry -> rpcLogIndex entry + 1 == rpcLogIndex metadata
                    && canonicalDeposit entry (decodeUint256 $ rpcLogData metadata)) (receiptLogs receipt) of
                  Nothing -> Left "DEPOSIT_EVENT_PAIR_MISSING"
                  Just _ -> Right ()) matches
        Right $ Just $ object
          ["depositTxHash" .= receiptTxHash receipt,"depositBlockNumber" .= show (receiptBlockNumber receipt)
          ,"depositBlockHash" .= receiptBlockHash receipt,"depositLogIndices" .= map (show . rpcLogIndex) matches
          ,"creditedAmount" .= show (sum $ map (decodeUint256 . rpcLogData) matches)]

-- Reviewed Arbitrum Across destination handlers. Provider calldata is also
-- structurally validated; a matching address alone is insufficient evidence.
verifyAcrossDestinations :: EthClient -> IO (Either Text ())
verifyAcrossDestinations client = runExceptT $ mapM_ (\(address,expected) -> do
  code <- requireRpc $ ethGetCode client address
  unless (not (BS.null code) && encodeHex (keccak256 code) == expected) $ throwE "ACROSS_DESTINATION_CODE_MISMATCH")
  [("0xbf75133b48b0a42ab9374027902e83c5e2949034","0x833b49ceddf001f197603e23297ddc21147b7d9e61adbe812e1167b3a7fe2fe5")
  ,("0x0f7ae28de1c8532170ad4ee566b5801485c13a0e","0x2a70f9d1b1c80cc0430bbffe16283cf067d915bee05f9812715cd94ad082b76a")]
