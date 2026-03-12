module Plether.Handlers.Protocol
  ( getProtocolStatus
  , getProtocolConfig
  ) where

import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, try)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.Cache
  ( AppCache (..)
  , CacheEntry (..)
  , getCached
  , setCached
  )
import Data.Text (Text, pack)
import Plether.Config (Addresses (..), Config (..), currentAddresses)
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema (insertPriceSnapshot, getPriceAt, insertStakingSnapshot, getStakingRateForApy)
import Plether.Ethereum.Client (EthClient, ethBlockNumber)
import qualified Plether.Ethereum.Contracts.BasketOracle as Oracle
import qualified Plether.Ethereum.Contracts.Morpho as Morpho
import qualified Plether.Ethereum.Contracts.MorphoIrm as MorphoIrm
import qualified Plether.Ethereum.Contracts.StakedToken as Staked
import qualified Plether.Ethereum.Contracts.SyntheticSplitter as Splitter
import Plether.Types
import Plether.Utils.Hex (hexToByteString)
import Plether.Utils.Numeric (wad)

getProtocolStatus :: AppCache -> EthClient -> Config -> Maybe DbPool -> IO (Either ApiError (ApiResponse ProtocolStatus))
getProtocolStatus cache client cfg mPool = do
  eBlockNum <- ethBlockNumber client
  case eBlockNum of
    Left err -> pure $ Left $ rpcErrorToApiError err
    Right blockNum -> do
      mCached <- atomically $ getCached (cacheProtocolStatus cache) blockNum
      case mCached of
        Just entry ->
          pure $ Right $ mkCachedResponse blockNum (cfgChainId cfg) (ceCachedAt entry) False (ceValue entry)
        Nothing ->
          fetchAndCacheProtocolStatus cache client cfg mPool blockNum

fetchAndCacheProtocolStatus :: AppCache -> EthClient -> Config -> Maybe DbPool -> Integer -> IO (Either ApiError (ApiResponse ProtocolStatus))
fetchAndCacheProtocolStatus cache client cfg mPool blockNum = do
  let addrs = currentAddresses (cfgDeployments cfg)

  eStatus <- Splitter.currentStatus client (addrSyntheticSplitter addrs)
  eCap <- Splitter.getCap client (addrSyntheticSplitter addrs)
  eOracle <- Oracle.latestRoundData client (addrBasketOracle addrs)

  eBearAssets <- Staked.totalAssets client (addrStakingBear addrs)
  eBearShares <- Staked.totalSupply client (addrStakingBear addrs)
  eBullAssets <- Staked.totalAssets client (addrStakingBull addrs)
  eBullShares <- Staked.totalSupply client (addrStakingBull addrs)

  timestamp <- getPOSIXTime

  case (eStatus, eCap, eOracle, eBearAssets, eBearShares, eBullAssets, eBullShares) of
    (Right status, Right cap, Right oracle, Right bearAssets, Right bearShares, Right bullAssets, Right bullShares) -> do
      let oraclePrice = Oracle.rdAnswer oracle
          bearPrice = oraclePrice
          bullPrice = cap - oraclePrice

          bearExchangeRate =
            if bearShares > 0
              then (bearAssets * wad) `div` bearShares
              else wad

          bullExchangeRate =
            if bullShares > 0
              then (bullAssets * wad) `div` bullShares
              else wad

          protocolState = case Splitter.statusValue status of
            0 -> Active
            1 -> Paused
            _ -> Settled

      apyInfo <- getApyInfo client cfg
      let apy = case apyInfo of
            Right a -> a
            Left _ -> ApyInfo { apyBear = ApyStats 0 0 0, apyBull = ApyStats 0 0 0 }

      let nowUnix = round timestamp :: Integer
      change24h <- case mPool of
        Just pool -> computePriceChange pool blockNum nowUnix bearPrice bullPrice oraclePrice
        Nothing -> pure Nothing

      stakingApy <- case mPool of
        Just pool -> computeStakingApy pool blockNum nowUnix bearExchangeRate bullExchangeRate
        Nothing -> pure $ StakingApy Nothing Nothing

      let protoStatus =
            ProtocolStatus
              { statusPrices =
                  Prices
                    { priceBear = bearPrice
                    , priceBull = bullPrice
                    , priceCap = cap
                    , priceChange24h = change24h
                    }
              , statusState = protocolState
              , statusOracle =
                  OracleInfo
                    { oraclePrice = oraclePrice
                    , oracleUpdatedAt = fromIntegral (Oracle.rdUpdatedAt oracle)
                    , oracleDecimals = 8
                    }
              , statusStaking =
                  StakingInfo
                    { stakingBear =
                        StakingStats
                          { stakingTotalAssets = bearAssets
                          , stakingTotalShares = bearShares
                          , stakingExchangeRate = bearExchangeRate
                          }
                    , stakingBull =
                        StakingStats
                          { stakingTotalAssets = bullAssets
                          , stakingTotalShares = bullShares
                          , stakingExchangeRate = bullExchangeRate
                          }
                    , stakingApy7d = stakingApy
                    }
              , statusApy = apy
              , statusTimestamp = timestamp
              }

      atomically $ setCached (cacheProtocolStatus cache) protoStatus blockNum timestamp
      pure $ Right $ mkResponse blockNum (cfgChainId cfg) protoStatus
    (Left err, _, _, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err, _, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, Left err, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, Left err, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, _, Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, _, _, Left err) -> pure $ Left $ rpcErrorToApiError err

computePriceChange :: DbPool -> Integer -> Integer -> Integer -> Integer -> Integer -> IO (Maybe PriceChange)
computePriceChange pool blockNum nowUnix bearPrice bullPrice oraclePrice' = do
  result <- try @SomeException $ withDb pool $ \conn -> do
    insertPriceSnapshot conn blockNum nowUnix oraclePrice'
    let oneDayAgo = nowUnix - 86400
    getPriceAt conn oneDayAgo
  case result of
    Right (Just oldOracle) | oldOracle > 0 ->
      let cap = bearPrice + bullPrice
          oldBull = cap - oldOracle
          bearChange = (fromIntegral bearPrice - fromIntegral oldOracle) / fromIntegral oldOracle :: Double
          bullChange = if oldBull > 0
            then (fromIntegral bullPrice - fromIntegral oldBull) / fromIntegral oldBull :: Double
            else 0
      in pure $ Just PriceChange { changeBear = bearChange, changeBull = bullChange }
    _ -> pure Nothing

computeStakingApy :: DbPool -> Integer -> Integer -> Integer -> Integer -> IO StakingApy
computeStakingApy pool blockNum nowUnix bearRate bullRate = do
  result <- try @SomeException $ withDb pool $ \conn -> do
    insertStakingSnapshot conn blockNum nowUnix bearRate bullRate
    let sevenDaysAgo = nowUnix - 7 * 86400
    getStakingRateForApy conn sevenDaysAgo
  case result of
    Right (Just (oldTs, oldBear, oldBull)) ->
      let elapsed = nowUnix - oldTs
          minElapsed = 86400
      in if elapsed < minElapsed
        then pure $ StakingApy Nothing Nothing
        else
          let days = fromIntegral elapsed / 86400 :: Double
              annualize cur old =
                if old > 0
                  then Just $ (fromIntegral cur / fromIntegral old :: Double) ** (365 / days) - 1
                  else Nothing
          in pure $ StakingApy (annualize bearRate oldBear) (annualize bullRate oldBull)
    _ -> pure $ StakingApy Nothing Nothing

getApyInfo :: EthClient -> Config -> IO (Either ApiError ApyInfo)
getApyInfo client cfg = do
  let addrs = currentAddresses (cfgDeployments cfg)
      morphoAddr = addrMorpho addrs
  eBearApy <- getMarketApy client addrs morphoAddr (addrMorphoMarketBear addrs)
  eBullApy <- getMarketApy client addrs morphoAddr (addrMorphoMarketBull addrs)
  case (eBearApy, eBullApy) of
    (Right bearApy, Right bullApy) ->
      pure $ Right $ ApyInfo { apyBear = bearApy, apyBull = bullApy }
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left err

getMarketApy :: EthClient -> Addresses -> Text -> Text -> IO (Either ApiError ApyStats)
getMarketApy client addrs morphoAddr marketIdHex = do
  let marketIdBs = hexToByteString marketIdHex
  eMarket <- Morpho.market client morphoAddr marketIdBs
  eMarketParams <- Morpho.idToMarketParams client morphoAddr marketIdBs
  case (eMarket, eMarketParams) of
    (Right mkt, Right mp) -> do
      let irmAddr = Morpho.mpIrm mp
      eBorrowRate <- MorphoIrm.borrowRateView client irmAddr mp mkt
      case eBorrowRate of
        Right borrowRate -> do
          let secondsPerYear = 31536000 :: Integer
              -- borrowRate is per-second rate scaled by 1e18
              -- borrowApy = borrowRate * secondsPerYear / 1e18
              borrowApy = fromIntegral (borrowRate * secondsPerYear) / (1e18 :: Double)

              totalSupply = Morpho.mktTotalSupplyAssets mkt
              totalBorrow = Morpho.mktTotalBorrowAssets mkt
              fee = Morpho.mktFee mkt

              utilization =
                if totalSupply > 0
                  then fromIntegral totalBorrow / fromIntegral totalSupply :: Double
                  else 0

              -- supplyApy = borrowApy * utilization * (1 - fee/1e18)
              feeRate = fromIntegral fee / (1e18 :: Double)
              supplyApy = borrowApy * utilization * (1 - feeRate)

          pure $ Right $ ApyStats
            { apySupply = supplyApy
            , apyBorrow = borrowApy
            , apyUtilization = utilization
            }
        Left err -> pure $ Left $ rpcErrorToApiError err
    (Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err) -> pure $ Left $ rpcErrorToApiError err

getProtocolConfig :: EthClient -> Config -> IO (Either ApiError (ApiResponse ProtocolConfig))
getProtocolConfig client cfg = do
  let addrs = currentAddresses (cfgDeployments cfg)
      morphoAddr = addrMorpho addrs
      bearMarketBs = hexToByteString (addrMorphoMarketBear addrs)
      bullMarketBs = hexToByteString (addrMorphoMarketBull addrs)

  eBearParams <- Morpho.idToMarketParams client morphoAddr bearMarketBs
  eBullParams <- Morpho.idToMarketParams client morphoAddr bullMarketBs
  eBlockNum <- ethBlockNumber client

  case (eBearParams, eBullParams, eBlockNum) of
    (Right bearParams, Right bullParams, Right blockNum) -> do
      let config =
            ProtocolConfig
              { configContracts =
                  Contracts
                    { contractUsdc = addrUsdc addrs
                    , contractDxyBear = addrDxyBear addrs
                    , contractDxyBull = addrDxyBull addrs
                    , contractSdxyBear = addrSdxyBear addrs
                    , contractSdxyBull = addrSdxyBull addrs
                    , contractSyntheticSplitter = addrSyntheticSplitter addrs
                    , contractCurvePool = addrCurvePool addrs
                    , contractZapRouter = addrZapRouter addrs
                    , contractLeverageRouter = addrLeverageRouter addrs
                    , contractBullLeverageRouter = addrBullLeverageRouter addrs
                    , contractBasketOracle = addrBasketOracle addrs
                    , contractMorpho = morphoAddr
                    , contractMorphoBearMarket = addrMorphoMarketBear addrs
                    , contractMorphoBullMarket = addrMorphoMarketBull addrs
                    }
              , configDecimals =
                  Decimals
                    { decUsdc = 6
                    , decPlDxyBear = 18
                    , decPlDxyBull = 18
                    , decOraclePrice = 8
                    , decMorphoShares = 18
                    }
              , configConstants =
                  Constants
                    { constMaxSlippage = 0.01
                    , constMinLeverage = 1.1
                    , constMaxLeverage = 10.0
                    , constLiquidationLtv = 0.86
                    }
              , configMarkets =
                  MarketConfig
                    { marketBearId = addrMorphoMarketBear addrs
                    , marketBullId = addrMorphoMarketBull addrs
                    , marketBearLltv = pack $ show (Morpho.mpLltv bearParams)
                    , marketBullLltv = pack $ show (Morpho.mpLltv bullParams)
                    }
              , configChainId = cfgChainId cfg
              }
      pure $ Right $ mkResponse blockNum (cfgChainId cfg) config
    (Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, Left err) -> pure $ Left $ rpcErrorToApiError err
