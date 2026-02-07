module Plether.Handlers.User
  ( getUserDashboard
  , getUserBalances
  , getUserPositions
  , getUserAllowances
  ) where

import Control.Concurrent.STM (atomically)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.Cache
  ( AppCache (..)
  , CacheEntry (..)
  , evictStale
  , getCachedFor
  , setCachedFor
  )
import Plether.Config (Addresses (..), Config (..))
import Plether.Ethereum.Client (EthClient, ethBlockNumber)
import qualified Plether.Ethereum.Contracts.BasketOracle as Oracle
import qualified Plether.Ethereum.Contracts.ERC20 as ERC20
import qualified Plether.Ethereum.Contracts.LeverageRouter as LevRouter
import qualified Plether.Ethereum.Contracts.Morpho as Morpho
import qualified Plether.Ethereum.Contracts.MorphoOracle as MorphoOracle
import qualified Plether.Ethereum.Contracts.StakedToken as Staked
import qualified Plether.Ethereum.Contracts.SyntheticSplitter as Splitter
import Plether.Types
import Plether.Utils.Hex (hexToByteString)

getUserDashboard :: AppCache -> EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse UserDashboard))
getUserDashboard cache client cfg userAddr = do
  eBlockNum <- ethBlockNumber client
  case eBlockNum of
    Left err -> pure $ Left $ rpcErrorToApiError err
    Right blockNum -> do
      mCached <- atomically $ getCachedFor (cacheUserDashboards cache) userAddr blockNum
      case mCached of
        Just entry ->
          pure $ Right $ mkCachedResponse blockNum (cfgChainId cfg) (ceCachedAt entry) False (ceValue entry)
        Nothing ->
          fetchAndCacheDashboard cache client cfg userAddr blockNum

fetchAndCacheDashboard :: AppCache -> EthClient -> Config -> Text -> Integer -> IO (Either ApiError (ApiResponse UserDashboard))
fetchAndCacheDashboard cache client cfg userAddr blockNum = do
  eBalances <- getUserBalancesRaw client cfg userAddr
  case eBalances of
    Left err -> pure $ Left err
    Right balances -> do
      eLeverage <- getLeveragePositions client cfg userAddr
      case eLeverage of
        Left err -> pure $ Left err
        Right leverage -> do
          eLending <- getLendingPositions client cfg userAddr
          case eLending of
            Left err -> pure $ Left err
            Right lending -> do
              eAllowances <- getAllowancesRaw client cfg userAddr
              case eAllowances of
                Left err -> pure $ Left err
                Right allowances -> do
                  eAuth <- getMorphoAuthorization client cfg userAddr
                  case eAuth of
                    Left err -> pure $ Left err
                    Right authorization -> do
                      let dashboard =
                            UserDashboard
                              { dashBalances = balances
                              , dashLeverage = leverage
                              , dashLending = lending
                              , dashAllowances = allowances
                              , dashAuthorization = authorization
                              }
                      timestamp <- getPOSIXTime
                      atomically $ do
                        setCachedFor (cacheUserDashboards cache) userAddr dashboard blockNum timestamp
                        evictStale blockNum (cacheUserDashboards cache)
                      pure $ Right $ mkResponse blockNum (cfgChainId cfg) dashboard

getUserBalances :: EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse UserBalances))
getUserBalances client cfg userAddr = do
  eBalances <- getUserBalancesRaw client cfg userAddr
  eBlockNum <- ethBlockNumber client

  case (eBalances, eBlockNum) of
    (Right balances, Right blockNum) ->
      pure $ Right $ mkResponse blockNum (cfgChainId cfg) balances
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left $ rpcErrorToApiError err

getUserBalancesRaw :: EthClient -> Config -> Text -> IO (Either ApiError UserBalances)
getUserBalancesRaw client cfg userAddr = do
  let addrs = cfgAddresses cfg

  eUsdc <- ERC20.balanceOf client (addrUsdc addrs) userAddr
  eBear <- ERC20.balanceOf client (addrDxyBear addrs) userAddr
  eBull <- ERC20.balanceOf client (addrDxyBull addrs) userAddr
  eStakedBear <- Staked.balanceOf client (addrStakingBear addrs) userAddr
  eStakedBull <- Staked.balanceOf client (addrStakingBull addrs) userAddr

  case (eUsdc, eBear, eBull, eStakedBear, eStakedBull) of
    (Right usdc, Right bear, Right bull, Right stakedBear, Right stakedBull) -> do
      eStakedBearAssets <-
        if stakedBear > 0
          then Staked.convertToAssets client (addrStakingBear addrs) stakedBear
          else pure (Right 0)
      eStakedBullAssets <-
        if stakedBull > 0
          then Staked.convertToAssets client (addrStakingBull addrs) stakedBull
          else pure (Right 0)

      case (eStakedBearAssets, eStakedBullAssets) of
        (Right stakedBearAssets, Right stakedBullAssets) ->
          pure $
            Right $
              UserBalances
                { balUsdc = usdc
                , balBear = bear
                , balBull = bull
                , balStakedBear = stakedBear
                , balStakedBull = stakedBull
                , balStakedBearAssets = stakedBearAssets
                , balStakedBullAssets = stakedBullAssets
                }
        (Left err, _) -> pure $ Left $ rpcErrorToApiError err
        (_, Left err) -> pure $ Left $ rpcErrorToApiError err
    (Left err, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, Left err) -> pure $ Left $ rpcErrorToApiError err

getUserPositions :: EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse UserPositions))
getUserPositions client cfg userAddr = do
  eBlockNum <- ethBlockNumber client
  case eBlockNum of
    Left err -> pure $ Left $ rpcErrorToApiError err
    Right blockNum -> do
      eLeverage <- getLeveragePositions client cfg userAddr
      case eLeverage of
        Left err -> pure $ Left err
        Right leverage -> do
          eLending <- getLendingPositions client cfg userAddr
          case eLending of
            Left err -> pure $ Left err
            Right lending -> do
              let positions = UserPositions { posLeverage = leverage, posLending = lending }
              pure $ Right $ mkResponse blockNum (cfgChainId cfg) positions

getUserAllowances :: AppCache -> EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse UserAllowances))
getUserAllowances cache client cfg userAddr = do
  eBlockNum <- ethBlockNumber client
  case eBlockNum of
    Left err -> pure $ Left $ rpcErrorToApiError err
    Right blockNum -> do
      mCached <- atomically $ getCachedFor (cacheUserAllowances cache) userAddr blockNum
      case mCached of
        Just entry ->
          pure $ Right $ mkCachedResponse blockNum (cfgChainId cfg) (ceCachedAt entry) False (ceValue entry)
        Nothing ->
          fetchAndCacheAllowances cache client cfg userAddr blockNum

fetchAndCacheAllowances :: AppCache -> EthClient -> Config -> Text -> Integer -> IO (Either ApiError (ApiResponse UserAllowances))
fetchAndCacheAllowances cache client cfg userAddr blockNum = do
  eAllowances <- getAllowancesRaw client cfg userAddr
  case eAllowances of
    Left err -> pure $ Left err
    Right allowances -> do
      timestamp <- getPOSIXTime
      atomically $ do
        setCachedFor (cacheUserAllowances cache) userAddr allowances blockNum timestamp
        evictStale blockNum (cacheUserAllowances cache)
      pure $ Right $ mkResponse blockNum (cfgChainId cfg) allowances

getAllowancesRaw :: EthClient -> Config -> Text -> IO (Either ApiError UserAllowances)
getAllowancesRaw client cfg userAddr = do
  let addrs = cfgAddresses cfg

  eUsdcSplitter <- ERC20.allowance client (addrUsdc addrs) userAddr (addrSyntheticSplitter addrs)
  eUsdcZap <- ERC20.allowance client (addrUsdc addrs) userAddr (addrZapRouter addrs)
  eUsdcCurve <- ERC20.allowance client (addrUsdc addrs) userAddr (addrCurvePool addrs)
  eUsdcLevRouter <- ERC20.allowance client (addrUsdc addrs) userAddr (addrLeverageRouter addrs)
  eUsdcBullLevRouter <- ERC20.allowance client (addrUsdc addrs) userAddr (addrBullLeverageRouter addrs)

  eBearSplitter <- ERC20.allowance client (addrDxyBear addrs) userAddr (addrSyntheticSplitter addrs)
  eBearStaking <- ERC20.allowance client (addrDxyBear addrs) userAddr (addrStakingBear addrs)
  eBearLeverage <- ERC20.allowance client (addrDxyBear addrs) userAddr (addrLeverageRouter addrs)
  eBearCurve <- ERC20.allowance client (addrDxyBear addrs) userAddr (addrCurvePool addrs)

  eBullSplitter <- ERC20.allowance client (addrDxyBull addrs) userAddr (addrSyntheticSplitter addrs)
  eBullStaking <- ERC20.allowance client (addrDxyBull addrs) userAddr (addrStakingBull addrs)
  eBullLeverage <- ERC20.allowance client (addrDxyBull addrs) userAddr (addrBullLeverageRouter addrs)
  eBullZap <- ERC20.allowance client (addrDxyBull addrs) userAddr (addrZapRouter addrs)

  -- Morpho USDC allowances need the morpho address
  eUsdcMorpho <- ERC20.allowance client (addrUsdc addrs) userAddr (addrMorpho addrs)

  case ( eUsdcSplitter, eUsdcZap, eUsdcCurve, eUsdcLevRouter, eUsdcBullLevRouter
       , eBearSplitter, eBearStaking, eBearLeverage, eBearCurve
       , eBullSplitter, eBullStaking, eBullLeverage, eBullZap
       , eUsdcMorpho
       ) of
    ( Right usdcSplitter, Right usdcZap, Right usdcCurve, Right usdcLevRouter, Right usdcBullLevRouter
      , Right bearSplitter, Right bearStaking, Right bearLeverage, Right bearCurve
      , Right bullSplitter, Right bullStaking, Right bullLeverage, Right bullZap
      , Right usdcMorpho
      ) ->
        pure $
          Right $
            UserAllowances
              { allowUsdc =
                  UsdcAllowances
                    { usdcAllowSplitter = usdcSplitter
                    , usdcAllowZap = usdcZap
                    , usdcAllowMorphoBear = usdcMorpho
                    , usdcAllowMorphoBull = usdcMorpho
                    , usdcAllowCurvePool = usdcCurve
                    , usdcAllowLeverageRouter = usdcLevRouter
                    , usdcAllowBullLeverageRouter = usdcBullLevRouter
                    }
              , allowBear =
                  BearAllowances
                    { bearAllowSplitter = bearSplitter
                    , bearAllowStaking = bearStaking
                    , bearAllowLeverageRouter = bearLeverage
                    , bearAllowCurvePool = bearCurve
                    }
              , allowBull =
                  BullAllowances
                    { bullAllowSplitter = bullSplitter
                    , bullAllowStaking = bullStaking
                    , bullAllowLeverageRouter = bullLeverage
                    , bullAllowZapRouter = bullZap
                    }
              }
    _ -> pure $ Left $ internalError "Failed to fetch allowances"

-- Leverage positions
getLeveragePositions :: EthClient -> Config -> Text -> IO (Either ApiError LeveragePositions)
getLeveragePositions client cfg userAddr = do
  let addrs = cfgAddresses cfg
  eBear <- getLeveragePosition client addrs "BEAR" userAddr
  eBull <- getLeveragePosition client addrs "BULL" userAddr
  case (eBear, eBull) of
    (Right bear, Right bull) ->
      pure $ Right $ LeveragePositions { levPosBear = bear, levPosBull = bull }
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left err

getLeveragePosition :: EthClient -> Addresses -> Text -> Text -> IO (Either ApiError (Maybe LeveragePosition))
getLeveragePosition client addrs side userAddr = do
  let routerAddr = if side == "BEAR" then addrLeverageRouter addrs else addrBullLeverageRouter addrs
      morphoAddr = addrMorpho addrs
      marketIdHex = if side == "BEAR" then addrMorphoMarketBear addrs else addrMorphoMarketBull addrs
      marketIdBs = hexToByteString marketIdHex

  eCollateral <- LevRouter.getCollateral client routerAddr userAddr
  eDebt <- LevRouter.getActualDebt client routerAddr userAddr
  eOracle <- Oracle.latestRoundData client (addrBasketOracle addrs)
  eCap <- Splitter.getCap client (addrSyntheticSplitter addrs)
  eMarketParams <- Morpho.idToMarketParams client morphoAddr marketIdBs

  case (eCollateral, eDebt, eOracle, eCap, eMarketParams) of
    (Right collateral, Right debt, Right oracle, Right cap, Right mp)
      | collateral == 0 -> pure $ Right Nothing
      | otherwise -> do
          let oraclePrice = Oracle.rdAnswer oracle
              tokenPrice =
                if side == "BEAR"
                  then oraclePrice
                  else if cap > oraclePrice then cap - oraclePrice else 0
              lltv = Morpho.mpLltv mp
              -- collateral(21 dec) * tokenPrice(8 dec) / 10^23 = USDC(6 dec)
              collateralUsd = (collateral * tokenPrice) `div` (10 ^ (23 :: Integer))
              netValue = if collateralUsd > debt then collateralUsd - debt else 0
              -- leverage = collateralUsd * 100 / equity (result is leverage * 100)
              leverage =
                if netValue > 0
                  then (collateralUsd * 100) `div` netValue
                  else 0
              -- healthFactor = collateralUsd * lltv * 100 / (debt * 10^18)
              healthFactor =
                if debt > 0 && lltv > 0
                  then (collateralUsd * lltv * 100) `div` (debt * 10 ^ (18 :: Integer))
                  else 0
              -- liquidationPrice = debt * 10^41 / (collateral * lltv)
              liquidationPriceRaw =
                if collateral > 0 && lltv > 0
                  then (debt * 10 ^ (41 :: Integer)) `div` (collateral * lltv)
                  else 0
              -- Convert to 6 dec display; for BULL, flip the price
              liquidationPrice =
                if side == "BEAR"
                  then liquidationPriceRaw `div` 100
                  else
                    if cap > liquidationPriceRaw
                      then (cap - liquidationPriceRaw) `div` 100
                      else 0
          pure $
            Right $
              Just $
                LeveragePosition
                  { levCollateral = collateral
                  , levCollateralUsd = collateralUsd
                  , levDebt = debt
                  , levHealthFactor = healthFactor
                  , levLiquidationPrice = liquidationPrice
                  , levLeverage = leverage
                  , levNetValue = netValue
                  }
    (Left err, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, Left err) -> pure $ Left $ rpcErrorToApiError err

-- Lending positions
getLendingPositions :: EthClient -> Config -> Text -> IO (Either ApiError LendingPositions)
getLendingPositions client cfg userAddr = do
  let addrs = cfgAddresses cfg
  eBear <- getLendingPosition client addrs "BEAR" userAddr
  eBull <- getLendingPosition client addrs "BULL" userAddr
  case (eBear, eBull) of
    (Right bear, Right bull) ->
      pure $ Right $ LendingPositions { lendPosBear = bear, lendPosBull = bull }
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left err

getLendingPosition :: EthClient -> Addresses -> Text -> Text -> IO (Either ApiError (Maybe LendingPosition))
getLendingPosition client addrs side userAddr = do
  let morphoAddr = addrMorpho addrs
      marketIdHex = if side == "BEAR" then addrMorphoMarketBear addrs else addrMorphoMarketBull addrs
      marketIdBs = hexToByteString marketIdHex
      oracleAddr = if side == "BEAR" then addrMorphoOracleBear addrs else addrMorphoOracleBull addrs

  ePosition <- Morpho.position client morphoAddr marketIdBs userAddr
  eMarket <- Morpho.market client morphoAddr marketIdBs
  eOraclePrice <- MorphoOracle.price client oracleAddr
  eMarketParams <- Morpho.idToMarketParams client morphoAddr marketIdBs

  case (ePosition, eMarket, eOraclePrice, eMarketParams) of
    (Right pos, Right mkt, Right oraclePrice, Right mp)
      | Morpho.posSupplyShares pos == 0 && Morpho.posBorrowShares pos == 0 && Morpho.posCollateral pos == 0 ->
          pure $ Right Nothing
      | otherwise -> do
          let supplyShares = Morpho.posSupplyShares pos
              borrowShares = Morpho.posBorrowShares pos
              collateral = Morpho.posCollateral pos
              totalSupplyAssets = Morpho.mktTotalSupplyAssets mkt
              totalSupplyShares = Morpho.mktTotalSupplyShares mkt
              totalBorrowAssets = Morpho.mktTotalBorrowAssets mkt
              totalBorrowShares = Morpho.mktTotalBorrowShares mkt
              lltv = Morpho.mpLltv mp

              suppliedAssets =
                if totalSupplyShares > 0
                  then (supplyShares * totalSupplyAssets) `div` totalSupplyShares
                  else 0
              borrowedAssets =
                if totalBorrowShares > 0
                  then (borrowShares * totalBorrowAssets) `div` totalBorrowShares
                  else 0
              -- collateralUsd: collateral(21 dec) * oraclePrice(1e24 scale) / 10^39 = USDC(6 dec)
              collateralUsd =
                if oraclePrice > 0
                  then (collateral * oraclePrice) `div` (10 ^ (39 :: Integer))
                  else 0
              -- maxBorrow: collateral(21 dec) * oraclePrice(1e24 scale) * lltv(18 dec) / 10^57 = USDC(6 dec)
              maxBorrow =
                if lltv > 0 && oraclePrice > 0
                  then (collateral * oraclePrice * lltv) `div` (10 ^ (57 :: Integer))
                  else 0
              availableToBorrow =
                if maxBorrow > borrowedAssets then maxBorrow - borrowedAssets else 0
              -- healthFactor = maxBorrow * 1e18 / borrowedAssets
              healthFactor =
                if borrowedAssets > 0
                  then (maxBorrow * 10 ^ (18 :: Integer)) `div` borrowedAssets
                  else 0

          pure $
            Right $
              Just $
                LendingPosition
                  { lendSupplied = suppliedAssets
                  , lendSuppliedShares = supplyShares
                  , lendBorrowed = borrowedAssets
                  , lendBorrowedShares = borrowShares
                  , lendAvailableToBorrow = availableToBorrow
                  , lendCollateral = collateralUsd
                  , lendHealthFactor = healthFactor
                  }
    (Left err, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, Left err) -> pure $ Left $ rpcErrorToApiError err

-- Morpho authorization
getMorphoAuthorization :: EthClient -> Config -> Text -> IO (Either ApiError MorphoAuthorization)
getMorphoAuthorization client cfg userAddr = do
  let addrs = cfgAddresses cfg
      morphoAddr = addrMorpho addrs
  eBear <- Morpho.isAuthorized client morphoAddr userAddr (addrLeverageRouter addrs)
  eBull <- Morpho.isAuthorized client morphoAddr userAddr (addrBullLeverageRouter addrs)
  case (eBear, eBull) of
    (Right bear, Right bull) ->
      pure $ Right $ MorphoAuthorization { authBearLeverageRouter = bear, authBullLeverageRouter = bull }
    (Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err) -> pure $ Left $ rpcErrorToApiError err
