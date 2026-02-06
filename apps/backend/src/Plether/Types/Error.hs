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
  , internalError
  , networkError
  ) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
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

internalError :: Text -> ApiError
internalError msg = mkError InternalError $ "Internal error: " <> msg

networkError :: Text -> ApiError
networkError msg = mkError NetworkError $ "Network error: " <> msg

rpcErrorToApiError :: RpcError -> ApiError
rpcErrorToApiError = \case
  RpcHttpError msg -> rpcError msg
  RpcJsonError msg -> rpcError msg
  RpcNodeError _ msg -> rpcError msg
