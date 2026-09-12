-- Advisory, fixed-block observations. No state changes and no authorization cache.
module Plether.AA.OracleReadiness (oracleReadiness, readOracleReadiness, classifyOracle, classifyProtectionOracle, decodePolicy) where

import Control.Concurrent.Async (Concurrently(..), runConcurrently)
import Data.Aeson (fromJSON, Result(..))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.Config (Config(..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema (getLatestPythUpdatePayload, PythUpdatePayloadRow(..))
import Plether.Ethereum.Abi (encodeCall, encodeBool, encodeUint256, decodeUint256)
import Plether.Ethereum.Client (EthClient, CallParams(..), ethCallAtBlock)
import Plether.Ethereum.Contracts.Perps (OrderExecutionPolicy(..))
import Plether.Ethereum.Rpc (ethLatestBlock, ethGetBlockByNumber, RpcBlock(..))
import Plether.Perps.Release (perpsV2PublicLens)

decodePolicy :: BS.ByteString -> Maybe OrderExecutionPolicy
decodePolicy bytes
  | BS.length bytes /= 192 || any (`notElem` [0,1]) [w 0,w 1,w 2,w 4,w 5] = Nothing
  | otherwise = Just $ OrderExecutionPolicy (w 0 == 1) (w 1 == 1) (w 2 == 1) (w 3) (w 4 == 1) (w 5 == 1)
 where w n = decodeUint256 $ BS.take 32 $ BS.drop (n * 32) bytes

-- Core v1.2.3 close policy is MarkRefresh, not the open policy. A valid
-- current basket and ingestion evidence are advisory only: a future live/FAD
-- commit still needs its unique post-commit payload and order-level assessment.
classifyOracle :: Bool -> Bool -> Integer -> Integer -> Maybe OrderExecutionPolicy -> Bool -> [Integer] -> (Text,Text)
classifyOracle isClose tradingActive now divergence policy basketValid times = case policy of
  Nothing -> unknown
  Just p
    | not isClose && (not tradingActive || oepCloseOnly p || oepOracleFrozen p || oepIsFadWindow p) -> ("blocked","OPEN_EXECUTION_UNAVAILABLE")
    | oepCloseOnly p || oepRequireStoredMark p || oepAllowAnyStoredMark p -> unknown
    | oepMaxStaleness p <= 0 || divergence <= 0 || not basketValid -> unknown
    | length times /= 6 || any (\t -> t <= 0 || t > now || now-t > oepMaxStaleness p) times -> unknown
    | maximum times - minimum times > divergence -> unknown
    | otherwise -> ("ready","READY")
 where unknown = ("unknown","ORACLE_UNAVAILABLE")

-- Armed triggers need live feeds; latched retries and cancellation have distinct
-- rules. A shared protection check must not block those safe actions.
classifyProtectionOracle :: Integer -> [Integer] -> Maybe OrderExecutionPolicy -> (Text,Text) -> (Text,Text)
classifyProtectionOracle now times (Just policy) close
  | oepOracleFrozen policy = ("unknown","PROTECTION_TRIGGER_UNAVAILABLE")
  | length times /= 6 || any (\t -> t <= 0 || t > now || now-t > 15) times = ("unknown","ORACLE_UNAVAILABLE")
  | otherwise = close
classifyProtectionOracle _ _ Nothing _ = ("unknown","ORACLE_UNAVAILABLE")

oracleReadiness :: Config -> DbPool -> EthClient -> IO ((Text,Text),(Text,Text),(Text,Text))
oracleReadiness cfg pool client = readOracleReadiness (cfgPerpsPletherOracle cfg) client (withDb pool getLatestPythUpdatePayload)

readOracleReadiness :: Text -> EthClient -> IO (Maybe PythUpdatePayloadRow) -> IO ((Text,Text),(Text,Text),(Text,Text))
readOracleReadiness oracle client loadPayload = do
  header <- ethLatestBlock client
  case header of
    Left _ -> pure unavailable
    Right block -> do
      let call target method args = ethCallAtBlock client (CallParams target $ encodeCall method args) (rpcBlockNumber block)
      (openPolicy,closePolicy,openPrice,closePrice,divergence,status,payload) <- runConcurrently $
        (,,,,,,) <$> Concurrently (call oracle "getOrderExecutionPolicy(bool)" [encodeBool False])
          <*> Concurrently (call oracle "getOrderExecutionPolicy(bool)" [encodeBool True])
          <*> Concurrently (call oracle "getLatestPrice(uint8)" [encodeUint256 0])
          <*> Concurrently (call oracle "getLatestPrice(uint8)" [encodeUint256 1])
          <*> Concurrently (call oracle "orderExecutionStalenessLimit()" [])
          <*> Concurrently (call perpsV2PublicLens "getProtocolStatus()" [])
          <*> Concurrently loadPayload
      canonical <- ethGetBlockByNumber client (rpcBlockNumber block)
      now <- floor <$> getPOSIXTime
      let uint result = either (const Nothing) (\b -> if BS.length b == 32 then Just $ decodeUint256 b else Nothing) result
          policy = either (const Nothing) decodePolicy
          basket result = case result of
            Right b -> BS.length b == 256 && decodeUint256 (BS.take 32 b) > 0
            _ -> False
          active = case status of
            Right b | BS.length b == 256 -> let n = decodeUint256 $ BS.take 32 $ BS.drop 160 b in if n `elem` [0,1] then Just (n == 1) else Nothing
            _ -> Nothing
          times = case payload of
            Just p -> case (fromJSON (puprPublishTimes p), fromJSON (puprUpdateData p)) of
              (Success ts, Success (updates :: [Text])) | not (null updates) && all validHex updates -> ts
              _ -> []
            _ -> []
      pure $ case (canonical,uint divergence) of
        (Right verified,Just d)
          | verified == block && rpcBlockTimestamp block <= now+2 && now-rpcBlockTimestamp block <= 15 ->
            let close = classifyOracle True True (rpcBlockTimestamp block) d (policy closePolicy) (basket closePrice) times
            in (maybe ("unknown","ORACLE_UNAVAILABLE") (\a -> classifyOracle False a (rpcBlockTimestamp block) d (policy openPolicy) (basket openPrice) times) active,
                close, classifyProtectionOracle (rpcBlockTimestamp block) times (policy closePolicy) close)
        _ -> unavailable
 where
  unavailable = (("unknown","ORACLE_UNAVAILABLE"),("unknown","ORACLE_UNAVAILABLE"),("unknown","ORACLE_UNAVAILABLE"))
  validHex t = T.isPrefixOf "0x" t && T.length t > 2 && even (T.length t) && T.all (`elem` ("0123456789abcdefABCDEF" :: String)) (T.drop 2 t)
