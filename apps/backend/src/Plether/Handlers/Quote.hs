module Plether.Handlers.Quote
  ( getMintQuote
  , getBurnQuote
  , getZapQuote
  , getTradeQuote
  , getLeverageQuote
  ) where

import Data.Text (Text)
import Plether.Config (Addresses (..), Config (..))
import Plether.Ethereum.Client (EthClient, ethBlockNumber)
import qualified Plether.Ethereum.Contracts.BasketOracle as Oracle
import qualified Plether.Ethereum.Contracts.CurvePool as Curve
import qualified Plether.Ethereum.Contracts.LeverageRouter as Leverage
import qualified Plether.Ethereum.Contracts.SyntheticSplitter as Splitter
import Plether.Types
import Plether.Utils.Numeric (wad)

getMintQuote :: EthClient -> Config -> Integer -> IO (Either ApiError (ApiResponse MintQuote))
getMintQuote client cfg amount = do
  let addrs = cfgAddresses cfg

  eBlockNum <- ethBlockNumber client
  eCap <- Splitter.getCap client (addrSyntheticSplitter addrs)
  eOracle <- Oracle.latestRoundData client (addrBasketOracle addrs)

  case (eBlockNum, eCap, eOracle) of
    (Right blockNum, Right cap, Right oracle) -> do
      let oraclePrice = Oracle.rdAnswer oracle
          usdcRequired = (amount * (oraclePrice + (cap - oraclePrice))) `div` (10 ^ (12 :: Integer))
          pricePerToken = (usdcRequired * wad) `div` amount

          quote =
            MintQuote
              { mintUsdcIn = usdcRequired
              , mintBearOut = amount
              , mintBullOut = amount
              , mintPricePerToken = pricePerToken
              }

      pure $ Right $ mkResponse blockNum (cfgChainId cfg) quote
    (Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, Left err) -> pure $ Left $ rpcErrorToApiError err

getBurnQuote :: EthClient -> Config -> Integer -> IO (Either ApiError (ApiResponse BurnQuote))
getBurnQuote client cfg amount = do
  let addrs = cfgAddresses cfg

  eBlockNum <- ethBlockNumber client
  ePreview <- Splitter.previewBurn client (addrSyntheticSplitter addrs) amount

  case (eBlockNum, ePreview) of
    (Right blockNum, Right preview) -> do
      let quote =
            BurnQuote
              { burnPairIn = amount
              , burnUsdcOut = Splitter.pbUsdcRefund preview
              , burnBearIn = amount
              , burnBullIn = amount
              }

      pure $ Right $ mkResponse blockNum (cfgChainId cfg) quote
    (Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err) -> pure $ Left $ rpcErrorToApiError err

getZapQuote :: EthClient -> Config -> Text -> Integer -> IO (Either ApiError (ApiResponse ZapQuote))
getZapQuote client cfg direction amount = do
  let addrs = cfgAddresses cfg

  eBlockNum <- ethBlockNumber client

  case eBlockNum of
    Right blockNum -> do
      let (dir, inToken, outToken, route) = case direction of
            "buy" ->
              ( Buy
              , "usdc"
              , "bull"
              , ["USDC", "plDXY-BEAR (flash)", "Curve", "Splitter", "plDXY-BULL"]
              )
            _ ->
              ( Sell
              , "bull"
              , "usdc"
              , ["plDXY-BULL", "Splitter", "Curve", "USDC"]
              )

          quote =
            ZapQuote
              { zapDirection = dir
              , zapInput =
                  ZapInput
                    { zapInToken = inToken
                    , zapInAmount = amount
                    }
              , zapOutput =
                  ZapOutput
                    { zapOutToken = outToken
                    , zapOutAmount = amount
                    , zapOutMinAmount = (amount * 99) `div` 100
                    }
              , zapPriceImpact = 0
              , zapRoute = route
              }

      pure $ Right $ mkResponse blockNum (cfgChainId cfg) quote
    Left err -> pure $ Left $ rpcErrorToApiError err

getTradeQuote :: EthClient -> Config -> Text -> Integer -> IO (Either ApiError (ApiResponse TradeQuote))
getTradeQuote client cfg from amount = do
  let addrs = cfgAddresses cfg

  eBlockNum <- ethBlockNumber client

  let (i, j, fromToken, toToken) = case from of
        "usdc" -> (0, 1, FromUsdc, FromBear)
        _ -> (1, 0, FromBear, FromUsdc)

  eDy <- Curve.getDy client (addrCurvePool addrs) i j amount

  case (eBlockNum, eDy) of
    (Right blockNum, Right dy) -> do
      let minOut = (dy * 99) `div` 100
          spotPrice =
            if amount > 0
              then (dy * wad) `div` amount
              else 0
          priceImpact = 0
          fee = (amount * 4) `div` 10000

          quote =
            TradeQuote
              { tradeFrom = fromToken
              , tradeTo = toToken
              , tradeAmountIn = amount
              , tradeAmountOut = dy
              , tradeMinAmountOut = minOut
              , tradeSpotPrice = spotPrice
              , tradePriceImpact = priceImpact
              , tradeFee = fee
              }

      pure $ Right $ mkResponse blockNum (cfgChainId cfg) quote
    (Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err) -> pure $ Left $ rpcErrorToApiError err

getLeverageQuote :: EthClient -> Config -> Text -> Integer -> Integer -> IO (Either ApiError (ApiResponse LeverageQuote))
getLeverageQuote client cfg side principal leverage = do
  let addrs = cfgAddresses cfg

  eBlockNum <- ethBlockNumber client

  let router = case side of
        "bear" -> addrLeverageRouter addrs
        _ -> addrBullLeverageRouter addrs

      sideVal = case side of
        "bear" -> Bear
        _ -> Bull

  ePreview <- Leverage.previewOpenLeverage client router principal leverage

  case (eBlockNum, ePreview) of
    (Right blockNum, Right preview) -> do
      let positionSize = Leverage.polExpectedTokens preview
          debt = Leverage.polExpectedDebt preview
          healthFactor =
            if debt > 0
              then (positionSize * 86 * (10 ^ (16 :: Integer))) `div` debt
              else maxHealthFactor

          quote =
            LeverageQuote
              { levqSide = sideVal
              , levqPrincipal = principal
              , levqLeverage = leverage
              , levqPositionSize = positionSize
              , levqPositionSizeUsd = positionSize
              , levqDebt = debt
              , levqHealthFactor = healthFactor
              , levqLiquidationPrice = 0
              , levqPriceImpact = 0
              , levqBorrowRate = 0
              }

      pure $ Right $ mkResponse blockNum (cfgChainId cfg) quote
    (Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err) -> pure $ Left $ rpcErrorToApiError err
  where
    maxHealthFactor = 10 ^ (20 :: Integer)

