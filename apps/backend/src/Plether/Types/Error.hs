module Plether.Types.Error
  ( ApiError (..)
  , ApiErrorCode (..)
  , mkError
  , invalidAddress
  , invalidAmount
  , invalidSide
  , rpcError
  , rpcErrorToApiError
  , rateLimited
  , rateLimitedWithDetails
  , internalError
  , networkError
  ) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)
import Plether.Ethereum.Client (RpcError (RpcHttpError, RpcJsonError, RpcNodeError))

data ApiErrorCode
  = InvalidAddress
  | InvalidAmount
  | InvalidSide
  | RpcError
  | RateLimited
  | InternalError
  | NetworkError
  deriving stock (Show, Eq, Generic)

instance ToJSON ApiErrorCode where
  toJSON = \case
    InvalidAddress -> "INVALID_ADDRESS"
    InvalidAmount -> "INVALID_AMOUNT"
    InvalidSide -> "INVALID_SIDE"
    RpcError -> "RPC_ERROR"
    RateLimited -> "RATE_LIMITED"
    InternalError -> "INTERNAL_ERROR"
    NetworkError -> "NETWORK_ERROR"

data ApiError = ApiError
  { errCode :: ApiErrorCode
  , errMessage :: Text
  , errDetails :: Maybe Value
  }
  deriving stock (Show, Generic)

instance ToJSON ApiError where
  toJSON ApiError {..} =
    object
      [ "error"
          .= object
            ( [ "code" .= errCode
              , "message" .= errMessage
              ]
                ++ maybe [] (\d -> ["details" .= d]) errDetails
            )
      ]

mkError :: ApiErrorCode -> Text -> ApiError
mkError code msg = ApiError code msg Nothing

invalidAddress :: Text -> ApiError
invalidAddress addr = mkError InvalidAddress $ "Invalid Ethereum address: " <> addr

invalidAmount :: Text -> ApiError
invalidAmount reason = mkError InvalidAmount $ "Invalid amount: " <> reason

invalidSide :: Text -> ApiError
invalidSide side = mkError InvalidSide $ "Invalid side, expected 'bear' or 'bull': " <> side

rpcError :: Text -> ApiError
rpcError msg = mkError RpcError $ "RPC error: " <> msg

rateLimited :: ApiError
rateLimited = mkError RateLimited "Rate limit exceeded"

rateLimitedWithDetails :: Maybe BS8.ByteString -> ApiError
rateLimitedWithDetails retryAfter =
  ApiError
    { errCode = RateLimited
    , errMessage = "Hermes rate limit exceeded"
    , errDetails =
        Just $
          object
            [ "retryAfter" .= fmap BS8.unpack retryAfter
            ]
    }

internalError :: Text -> ApiError
internalError msg = mkError InternalError $ "Internal error: " <> msg

networkError :: Text -> ApiError
networkError msg = mkError NetworkError $ "Network error: " <> msg

rpcErrorToApiError :: RpcError -> ApiError
rpcErrorToApiError = \case
  RpcHttpError _ -> networkError "Blockchain node unreachable"
  RpcJsonError _ -> internalError "Unexpected response from blockchain node"
  RpcNodeError _ msg -> rpcError $ sanitizeNodeError msg

sanitizeNodeError :: Text -> Text
sanitizeNodeError msg
  | T.length msg > 200 = T.take 200 msg <> "..."
  | otherwise = msg
