module Plether.Handlers.Protocol
  ( getProtocolStatus
  , getProtocolConfig
  ) where

import Control.Concurrent.STM (atomically)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.Cache
  ( AppCache (..)
  , CacheEntry (..)
  , getCached
  , setCached
  )
import Plether.Config (Addresses (..), Config (..))
import Plether.Ethereum.Client (EthClient, ethBlockNumber)
import qualified Plether.Ethereum.Contracts.BasketOracle as Oracle
import qualified Plether.Ethereum.Contracts.StakedToken as Staked
import qualified Plether.Ethereum.Contracts.SyntheticSplitter as Splitter
import Plether.Types
import Plether.Utils.Numeric (wad)

getProtocolStatus :: AppCache -> EthClient -> Config -> IO (Either ApiError (ApiResponse ProtocolStatus))
getProtocolStatus cache client cfg = do
  eBlockNum <- ethBlockNumber client
  case eBlockNum of
    Left err -> pure $ Left $ rpcErrorToApiError err
    Right blockNum -> do
      mCached <- atomically $ getCached (cacheProtocolStatus cache) blockNum
      case mCached of
        Just entry ->
          pure $ Right $ mkCachedResponse blockNum (cfgChainId cfg) (ceCachedAt entry) False (ceValue entry)
        Nothing ->
          fetchAndCacheProtocolStatus cache client cfg blockNum

fetchAndCacheProtocolStatus :: AppCache -> EthClient -> Config -> Integer -> IO (Either ApiError (ApiResponse ProtocolStatus))
fetchAndCacheProtocolStatus cache client cfg blockNum = do
  let addrs = cfgAddresses cfg

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

          protoStatus =
            ProtocolStatus
              { statusPrices =
                  Prices
                    { priceBear = bearPrice
                    , priceBull = bullPrice
                    , priceCap = cap
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
                    }
              , statusApy =
                  ApyInfo
                    { apyBear = ApyStats 0 0 0
                    , apyBull = ApyStats 0 0 0
                    }
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

getProtocolConfig :: Config -> IO (ApiResponse ProtocolConfig)
getProtocolConfig cfg = do
  let addrs = cfgAddresses cfg
      config =
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
                , contractMorpho = "0xBBBBBbbBBb9cC5e90e3b3Af64bdAF62C37EEFFCb"
                , contractMorphoBearMarket = ""
                , contractMorphoBullMarket = ""
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
          , configChainId = cfgChainId cfg
          }
  pure $ mkResponse 0 (cfgChainId cfg) config

