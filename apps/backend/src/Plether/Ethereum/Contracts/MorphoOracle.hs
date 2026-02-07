module Plether.Ethereum.Contracts.MorphoOracle
  ( price
  , priceCall
  , decodePrice
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256, encodeCall)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)

priceCall :: ByteString
priceCall = encodeCall "price()" []

decodePrice :: ByteString -> Integer
decodePrice = decodeUint256

price :: EthClient -> Text -> IO (Either RpcError Integer)
price client oracle = do
  result <- ethCall client (CallParams oracle priceCall)
  pure $ fmap decodePrice result
