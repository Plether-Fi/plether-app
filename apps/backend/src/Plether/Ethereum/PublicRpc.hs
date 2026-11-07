module Plether.Ethereum.PublicRpc
  ( PublicRpcCall (..)
  , parsePublicRpc
  , handlePublicRpc
  ) where

import Control.Monad (replicateM, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isHexDigit)
import Data.Foldable (toList)
import Data.List (nub)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as V
import Network.HTTP.Types.Status (status400, status403, status413, status429, status503)
import qualified Plether.AA.Pimlico as Proxy
import Plether.AA.ClientKey (pseudonymousClientKey)
import Plether.Ethereum.Client (EthClient, RpcError (..), rpcCall)
import Plether.Utils.Hex (hexToInteger)
import System.Timeout (timeout)
import Web.Scotty (ActionM, header, json, request, setHeader, status)

data PublicRpcCall = PublicRpcCall
  { prcId :: Value
  , prcMethod :: Text
  , prcParams :: Value
  } deriving stock (Eq, Show)

-- Browser requests are deliberately narrower than the authenticated internal
-- router: no debug/admin methods, state overrides, notifications or open scans.
parsePublicRpc :: Value -> Either Text [PublicRpcCall]
parsePublicRpc input = do
  let values = case input of Array xs -> toList xs; value -> [value]
  unless (not (null values) && length values <= 20) $ Left "Invalid batch size"
  calls <- traverse parseCall values
  unless (length (nub $ map prcId calls) == length calls) $ Left "Duplicate request IDs"
  pure calls
 where
  parseCall (Object fields) = do
    unless (all (`elem` ["jsonrpc", "id", "method", "params"]) $ KM.keys fields) $ Left "Unknown request field"
    unless (KM.lookup "jsonrpc" fields == Just (String "2.0")) $ Left "Invalid JSON-RPC version"
    identifier <- maybe (Left "Request ID required") Right $ KM.lookup "id" fields
    unless (validId identifier) $ Left "Invalid request ID"
    method <- case KM.lookup "method" fields of Just (String value) -> Right value; _ -> Left "Method required"
    unless (method `elem` publicMethods) $ Left "Method not available on public RPC"
    params <- case KM.lookup "params" fields of Just (Array xs) -> Right xs; _ -> Left "Array params required"
    unless (V.length params <= 4) $ Left "Too many params"
    bounded <- boundParams method params
    pure $ PublicRpcCall identifier method (Array bounded)
  parseCall _ = Left "Request must be an object"
  validId (String value) = not (T.null value) && T.length value <= 128
  validId (Number value) = case floatingOrInteger value :: Either Double Integer of
    Right integer -> abs integer <= 9007199254740991
    Left _ -> False
  validId _ = False

publicMethods :: [Text]
publicMethods =
  [ "eth_chainId", "net_version", "web3_clientVersion", "eth_syncing"
  , "eth_blockNumber", "eth_getBlockByNumber", "eth_getBlockByHash"
  , "eth_getTransactionByHash", "eth_getTransactionReceipt", "eth_getTransactionCount"
  , "eth_getBalance", "eth_getCode", "eth_getStorageAt", "eth_call", "eth_estimateGas"
  , "eth_gasPrice", "eth_feeHistory", "eth_maxPriorityFeePerGas", "eth_getLogs"
  , "eth_sendRawTransaction"
  ]

quantity :: Value -> Maybe Integer
quantity (String value)
  | Just digits <- T.stripPrefix "0x" value
  , not (T.null digits), T.length digits <= 16, T.all isHexDigit digits = Just $ hexToInteger digits
quantity _ = Nothing

boundParams :: Text -> V.Vector Value -> Either Text (V.Vector Value)
boundParams method params
  | method `elem` ["eth_call", "eth_estimateGas"] = do
      unless (V.length params >= 1 && V.length params <= 2) $ Left "State overrides are not public"
      transaction <- case params V.!? 0 of Just (Object fields) -> Right fields; _ -> Left "Transaction required"
      case KM.lookup "gas" transaction of
        Nothing -> pure ()
        Just gas -> unless (maybe False (<= 2000000) $ quantity gas) $ Left "Call gas exceeds public limit"
      pure $ params V.// [(0, Object $ KM.insert "gas" (maybe (String "0x1e8480") id $ KM.lookup "gas" transaction) transaction)]
  | method == "eth_getLogs" = do
      fields <- case toList params of [Object value] -> Right value; _ -> Left "One log filter required"
      unless (KM.member "address" fields) $ Left "Log address required"
      case KM.lookup "blockHash" fields of
        Just (String hash) | T.length hash == 66 -> pure params
        _ -> do
          from <- maybe (Left "Explicit log range required") Right $ KM.lookup "fromBlock" fields >>= quantity
          to <- maybe (Left "Explicit log range required") Right $ KM.lookup "toBlock" fields >>= quantity
          unless (to >= from && to - from < 1000) $ Left "Public log range exceeds 1000 blocks"
          pure params
  | method == "eth_feeHistory" = do
      count <- maybe (Left "Fee history count required") Right $ params V.!? 0 >>= quantity
      unless (count > 0 && count <= 100) $ Left "Fee history exceeds 100 blocks"
      pure params
  | otherwise = pure params

handlePublicRpc :: Integer -> Maybe Text -> EthClient -> Proxy.PimlicoProxyState -> ActionM ()
handlePublicRpc chainId expected client limits = do
  setHeader "Cache-Control" "no-store"
  supplied <- fmap TL.toStrict <$> header "X-Plether-AA-Proxy-Token"
  ip <- fmap TL.toStrict <$> header "CF-Connecting-IP"
  let authenticated = maybe False (\secret -> maybe False (Proxy.constantTimeTextEq secret) supplied) expected
  if chainId /= 421614 || not authenticated
    then status status403 >> json (rpcFailure Null (-32001) "Forbidden")
    else case (expected, ip >>= Proxy.validateClientIp) of
      (Just secret, Just trustedIp) -> do
        now <- liftIO getCurrentTime
        admitted <- liftIO $ Proxy.checkRateLimit limits now ("public-rpc-body:" <> pseudonymousClientKey secret trustedIp) 1200
        if not admitted then limited else do
          req <- request
          body <- liftIO $ Proxy.readBoundedRequestBody 65536 req
          case body of
            Left _ -> status status413 >> json (rpcFailure Null (-32600) "Request too large")
            Right bytes -> case eitherDecode bytes of
              Left _ -> invalid
              Right value -> case parsePublicRpc value of
                Left _ -> invalid
                Right calls -> do
                  allowed <- liftIO $ and <$> replicateM (length calls) (Proxy.checkRateLimit limits now ("public-rpc-call:" <> pseudonymousClientKey secret trustedIp) 600)
                  global <- liftIO $ and <$> replicateM (length calls) (Proxy.checkRateLimit limits now "public-rpc-global" 12000)
                  if not (allowed && global) then limited else do
                    result <- liftIO $ timeout 15000000 $ traverse relay calls
                    case result of
                      Nothing -> status status503 >> json (rpcFailure Null (-32002) "RPC unavailable; reconcile transaction hashes before retrying")
                      Just replies -> json $ case value of Array _ -> Array $ V.fromList replies; _ -> head replies
      _ -> invalid
 where
  invalid = status status400 >> json (rpcFailure Null (-32600) "Invalid public RPC request")
  limited = status status429 >> setHeader "Retry-After" "60" >> json (rpcFailure Null (-32005) "RPC rate limit exceeded")
  relay PublicRpcCall {..} = do
    result <- rpcCall client prcMethod prcParams
    pure $ case result of
      Right value -> object ["jsonrpc" .= ("2.0" :: Text), "id" .= prcId, "result" .= value]
      Left (RpcNodeError code message details) -> object
        ["jsonrpc" .= ("2.0" :: Text), "id" .= prcId, "error" .= object
          (["code" .= code, "message" .= T.take 512 message] <> maybe [] (\value -> ["data" .= value]) details)]
      Left _ -> rpcFailure prcId (-32002) "RPC unavailable; reconcile transaction hashes before retrying"

rpcFailure :: Value -> Int -> Text -> Value
rpcFailure identifier code message = object
  ["jsonrpc" .= ("2.0" :: Text), "id" .= identifier, "error" .= object ["code" .= code, "message" .= message]]
