module Plether.Ethereum.Contracts.MorphoIrm
  ( borrowRateView
  , borrowRateViewCall
  , decodeBorrowRateView
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall, encodeUint256)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)
import Plether.Ethereum.Contracts.Morpho (Market (..), MarketParams (..))

borrowRateViewCall :: MarketParams -> Market -> ByteString
borrowRateViewCall mp mkt =
  encodeCall
    "borrowRateView((address,address,address,address,uint256),(uint128,uint128,uint128,uint128,uint128,uint128))"
    [ encodeAddress (mpLoanToken mp)
    , encodeAddress (mpCollateralToken mp)
    , encodeAddress (mpOracle mp)
    , encodeAddress (mpIrm mp)
    , encodeUint256 (mpLltv mp)
    , encodeUint256 (mktTotalSupplyAssets mkt)
    , encodeUint256 (mktTotalSupplyShares mkt)
    , encodeUint256 (mktTotalBorrowAssets mkt)
    , encodeUint256 (mktTotalBorrowShares mkt)
    , encodeUint256 (mktLastUpdate mkt)
    , encodeUint256 (mktFee mkt)
    ]

decodeBorrowRateView :: ByteString -> Integer
decodeBorrowRateView = decodeUint256

borrowRateView :: EthClient -> Text -> MarketParams -> Market -> IO (Either RpcError Integer)
borrowRateView client irm mp mkt = do
  result <- ethCall client (CallParams irm (borrowRateViewCall mp mkt))
  pure $ fmap decodeBorrowRateView result
