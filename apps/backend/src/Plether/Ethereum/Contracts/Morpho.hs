module Plether.Ethereum.Contracts.Morpho
  ( position
  , market
  , isAuthorized
  , idToMarketParams
  , positionCall
  , marketCall
  , isAuthorizedCall
  , idToMarketParamsCall
  , decodePosition
  , decodeMarket
  , decodeIsAuthorized
  , decodeIdToMarketParams
  , Position (..)
  , Market (..)
  , MarketParams (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeAddress, decodeBool, decodeUint256, encodeAddress, encodeBytes32, encodeCall)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)

data Position = Position
  { posSupplyShares :: Integer
  , posBorrowShares :: Integer
  , posCollateral :: Integer
  }
  deriving stock (Show)

data Market = Market
  { mktTotalSupplyAssets :: Integer
  , mktTotalSupplyShares :: Integer
  , mktTotalBorrowAssets :: Integer
  , mktTotalBorrowShares :: Integer
  , mktLastUpdate :: Integer
  , mktFee :: Integer
  }
  deriving stock (Show)

positionCall :: ByteString -> Text -> ByteString
positionCall marketId user =
  encodeCall "position(bytes32,address)" [encodeBytes32 marketId, encodeAddress user]

marketCall :: ByteString -> ByteString
marketCall marketId =
  encodeCall "market(bytes32)" [encodeBytes32 marketId]

decodePosition :: ByteString -> Position
decodePosition bs =
  Position
    { posSupplyShares = decodeUint256 (BS.take 32 bs)
    , posBorrowShares = decodeUint256 (BS.take 32 $ BS.drop 32 bs)
    , posCollateral = decodeUint256 (BS.take 32 $ BS.drop 64 bs)
    }

decodeMarket :: ByteString -> Market
decodeMarket bs =
  Market
    { mktTotalSupplyAssets = decodeUint256 (BS.take 32 bs)
    , mktTotalSupplyShares = decodeUint256 (BS.take 32 $ BS.drop 32 bs)
    , mktTotalBorrowAssets = decodeUint256 (BS.take 32 $ BS.drop 64 bs)
    , mktTotalBorrowShares = decodeUint256 (BS.take 32 $ BS.drop 96 bs)
    , mktLastUpdate = decodeUint256 (BS.take 32 $ BS.drop 128 bs)
    , mktFee = decodeUint256 (BS.take 32 $ BS.drop 160 bs)
    }

position :: EthClient -> Text -> ByteString -> Text -> IO (Either RpcError Position)
position client morpho marketId user = do
  result <- ethCall client (CallParams morpho (positionCall marketId user))
  pure $ fmap decodePosition result

market :: EthClient -> Text -> ByteString -> IO (Either RpcError Market)
market client morpho marketId = do
  result <- ethCall client (CallParams morpho (marketCall marketId))
  pure $ fmap decodeMarket result

data MarketParams = MarketParams
  { mpLoanToken :: Text
  , mpCollateralToken :: Text
  , mpOracle :: Text
  , mpIrm :: Text
  , mpLltv :: Integer
  }
  deriving stock (Show)

isAuthorizedCall :: Text -> Text -> ByteString
isAuthorizedCall owner spender =
  encodeCall "isAuthorized(address,address)" [encodeAddress owner, encodeAddress spender]

decodeIsAuthorized :: ByteString -> Bool
decodeIsAuthorized = decodeBool

isAuthorized :: EthClient -> Text -> Text -> Text -> IO (Either RpcError Bool)
isAuthorized client morpho owner spender = do
  result <- ethCall client (CallParams morpho (isAuthorizedCall owner spender))
  pure $ fmap decodeIsAuthorized result

idToMarketParamsCall :: ByteString -> ByteString
idToMarketParamsCall marketId =
  encodeCall "idToMarketParams(bytes32)" [encodeBytes32 marketId]

decodeIdToMarketParams :: ByteString -> MarketParams
decodeIdToMarketParams bs =
  MarketParams
    { mpLoanToken = decodeAddress (BS.take 32 bs)
    , mpCollateralToken = decodeAddress (BS.take 32 $ BS.drop 32 bs)
    , mpOracle = decodeAddress (BS.take 32 $ BS.drop 64 bs)
    , mpIrm = decodeAddress (BS.take 32 $ BS.drop 96 bs)
    , mpLltv = decodeUint256 (BS.take 32 $ BS.drop 128 bs)
    }

idToMarketParams :: EthClient -> Text -> ByteString -> IO (Either RpcError MarketParams)
idToMarketParams client morpho marketId = do
  result <- ethCall client (CallParams morpho (idToMarketParamsCall marketId))
  pure $ fmap decodeIdToMarketParams result
