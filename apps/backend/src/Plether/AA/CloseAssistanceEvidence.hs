module Plether.AA.CloseAssistanceEvidence (verifyCloseAssistanceReceipt) where

import Control.Monad (unless)
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Plether.Database.CloseAssistance (CloseAssistanceReservation (..))
import Plether.Ethereum.Abi (keccak256)
import Plether.Ethereum.Client (parseRpcQuantity)
import qualified Plether.Perps.Manifest as Manifest

data ReceiptLog = ReceiptLog { rlAddress :: Text, rlTopics :: [Text], rlData :: BS.ByteString, rlIndex :: Integer }

-- | A receipt can contain many UserOperations. Only logs between the previous EntryPoint operation event and
-- this operation's event may provide funding evidence. A same-amount deposit elsewhere is never sufficient.
verifyCloseAssistanceReceipt
  :: CloseAssistanceReservation -> Text -> Text -> Integer -> Text -> Integer -> Value
  -> Either Text (Integer, Integer)
verifyCloseAssistanceReceipt grant operationHash tx blockNumber blockHash eventIndex (Object receipt) = do
  unless (KM.lookup "transactionHash" receipt == Just (String tx)
    && KM.lookup "blockHash" receipt == Just (String blockHash)) $ Left "Assistance receipt identity mismatch"
  receiptBlock <- number =<< required "blockNumber" receipt
  status <- number =<< required "status" receipt
  unless (receiptBlock == blockNumber && status == 1) $ Left "Assistance receipt block or status mismatch"
  values <- case KM.lookup "logs" receipt of Just (Array xs) -> Right $ V.toList xs; _ -> Left "Missing receipt logs"
  logs <- traverse parseLog values
  let operationEvents = filter isOperationEvent logs
      matching = filter (\l -> rlIndex l == eventIndex && take 2 (rlTopics l) == [operationTopic, T.toLower operationHash]) operationEvents
      previous = maximum $ (-1) : [rlIndex l | l <- operationEvents, rlIndex l < eventIndex]
      scoped = filter (\l -> rlIndex l > previous && rlIndex l < eventIndex) logs
      accountTopic = addressTopic $ carAccount grant
      token = T.toLower Manifest.mockUsdcAddress
      house = T.toLower Manifest.marginClearinghouseAddress
      mints = filter (\l -> rlAddress l == token && rlTopics l == [topic "Transfer(address,address,uint256)", zeroTopic, accountTopic]
        && word (rlData l) == Just (carAmountUsdc grant)) scoped
      deposits = filter (\l -> rlAddress l == house && rlTopics l == [topic "Deposit(address,address,uint256)", accountTopic, addressTopic token]
        && word (rlData l) == Just (carAmountUsdc grant)) scoped
      intents = filter (\l -> rlAddress l == T.toLower Manifest.orderLifecycleBookAddress && length (rlTopics l) == 4
        && take 1 (rlTopics l) == [intentTopic] && drop 2 (rlTopics l) == [accountTopic,T.toLower $ carClientOrderId grant]
        && BS.length (rlData l) == 20 * 32 && hex (keccak256 $ BS.drop 64 $ rlData l) == T.toLower (carRequestHash grant)) scoped
  unless (length matching == 1) $ Left "Assistance UserOperation boundary is missing or ambiguous"
  case (mints,deposits,intents) of
    ([mint],[deposit],[intent]) -> do
      idBytes <- maybe (Left "Missing order id") unhex $ at 1 $ rlTopics intent
      orderId <- maybe (Left "Invalid order id") Right $ word idBytes
      let commits = filter (\l -> rlAddress l == T.toLower (carRouter grant)
            && rlTopics l == [topic "OrderCommitted(uint64,address,uint8)",hex idBytes,accountTopic]) scoped
      unless (orderId > 0 && rlIndex mint < rlIndex deposit && rlIndex deposit < rlIndex intent
        && length commits == 1 && all ((> rlIndex intent) . rlIndex) commits) $
          Left "Assistance funding and commitment evidence is inconsistent"
      pure (rlIndex deposit,orderId)
    _ -> Left "Assistance requires exactly one matching mint, deposit and newly registered intent"
 where
  required key object = maybe (Left "Missing receipt field") Right $ KM.lookup key object
verifyCloseAssistanceReceipt _ _ _ _ _ _ _ = Left "Invalid assistance receipt"

parseLog :: Value -> Either Text ReceiptLog
parseLog (Object value) = do
  address <- case KM.lookup "address" value of Just (String s) -> Right $ T.toLower s; _ -> Left "Missing log address"
  topics <- case KM.lookup "topics" value of
    Just (Array xs) -> traverse (\v -> case v of String s -> Right $ T.toLower s; _ -> Left "Invalid topic") $ V.toList xs
    _ -> Left "Missing topics"
  bytes <- case KM.lookup "data" value of Just (String s) -> unhex s; _ -> Left "Missing log data"
  index <- maybe (Left "Missing log index") number $ KM.lookup "logIndex" value
  pure $ ReceiptLog address topics bytes index
parseLog _ = Left "Invalid receipt log"

isOperationEvent :: ReceiptLog -> Bool
isOperationEvent l = rlAddress l == "0x4337084d9e255ff0702461cf8895ce9e3b5ff108" && take 1 (rlTopics l) == [operationTopic]

operationTopic, intentTopic, zeroTopic :: Text
operationTopic = topic "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)"
intentTopic = topic "IntentRegistered(uint64,address,bytes32,bytes32,uint256,(bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)))"
zeroTopic = "0x" <> T.replicate 64 "0"

topic :: Text -> Text
topic = hex . keccak256 . TE.encodeUtf8
hex :: BS.ByteString -> Text
hex = ("0x" <>) . TE.decodeUtf8 . B16.encode
unhex :: Text -> Either Text BS.ByteString
unhex s = either (const $ Left "Invalid hex") Right $ B16.decode $ TE.encodeUtf8 $ T.drop 2 s
addressTopic :: Text -> Text
addressTopic s = "0x" <> T.replicate 24 "0" <> T.drop 2 (T.toLower s)
word :: BS.ByteString -> Maybe Integer
word bytes | BS.length bytes == 32 = Just $ BS.foldl' (\a b -> a * 256 + fromIntegral b) 0 bytes
           | otherwise = Nothing
number :: Value -> Either Text Integer
number (String s) = either (const $ Left "Invalid quantity") Right $ parseRpcQuantity "receipt" s
number _ = Left "Invalid quantity"
at :: Int -> [a] -> Maybe a
at n xs = case drop n xs of x:_ -> Just x; _ -> Nothing
