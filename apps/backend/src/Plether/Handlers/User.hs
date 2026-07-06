module Plether.Handlers.User
  ( getUserDashboard
  , getUserBalances
  , getUserPositions
  , getUserAllowances
  ) where

import Control.Concurrent.STM (atomically)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.Cache
  ( AppCache (..)
  , CacheEntry (..)
  , evictStale
  , getCachedFor
  , setCachedFor
  )
import Plether.Config (Addresses (..), Config (..), currentAddresses)
import Plether.Ethereum.Client (EthClient, ethBlockNumber)
import qualified Plether.Ethereum.Contracts.BasketOracle as Oracle
import qualified Plether.Ethereum.Contracts.ERC20 as ERC20
import qualified Plether.Ethereum.Contracts.LeverageRouter as LevRouter
import qualified Plether.Ethereum.Contracts.Morpho as Morpho
import qualified Plether.Ethereum.Contracts.MorphoOracle as MorphoOracle
import qualified Plether.Ethereum.Contracts.StakedToken as Staked
import qualified Plether.Ethereum.Contracts.SyntheticSplitter as Splitter
import qualified Plether.Ethereum.Multicall as Multicall
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
      leverage <- either (const emptyLeverage) id <$> getLeveragePositions client cfg userAddr
      lending <- either (const emptyLending) id <$> getLendingPositions client cfg userAddr
      allowances <- either (const emptyAllowances) id <$> getAllowancesRaw client cfg userAddr
      authorization <- either (const emptyAuth) id <$> getMorphoAuthorization client cfg userAddr
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
  let addrs = currentAddresses (cfgDeployments cfg)

  eResults <-
    multicallBytes
      client
      [ (addrUsdc addrs, ERC20.balanceOfCall userAddr)
      , (addrDxyBear addrs, ERC20.balanceOfCall userAddr)
      , (addrDxyBull addrs, ERC20.balanceOfCall userAddr)
      , (addrStakingBear addrs, Staked.balanceOfCall userAddr)
      , (addrStakingBull addrs, Staked.balanceOfCall userAddr)
      ]

  case eResults of
    Right [usdcBs, bearBs, bullBs, stakedBearBs, stakedBullBs] -> do
      let usdc = ERC20.decodeBalanceOf usdcBs
          bear = ERC20.decodeBalanceOf bearBs
          bull = ERC20.decodeBalanceOf bullBs
          stakedBear = Staked.decodeBalanceOf stakedBearBs
          stakedBull = Staked.decodeBalanceOf stakedBullBs
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
    Left err -> pure $ Left err
    _ -> pure $ Left $ internalError "Failed to fetch balances"

getUserPositions :: EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse UserPositions))
getUserPositions client cfg userAddr = do
  eBlockNum <- ethBlockNumber client
  case eBlockNum of
    Left err -> pure $ Left $ rpcErrorToApiError err
    Right blockNum -> do
      leverage <- either (const emptyLeverage) id <$> getLeveragePositions client cfg userAddr
      lending <- either (const emptyLending) id <$> getLendingPositions client cfg userAddr
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
  let addrs = currentAddresses (cfgDeployments cfg)

  eResults <-
    multicallBytes
      client
      [ (addrUsdc addrs, ERC20.allowanceCall userAddr (addrSyntheticSplitter addrs))
      , (addrUsdc addrs, ERC20.allowanceCall userAddr (addrZapRouter addrs))
      , (addrUsdc addrs, ERC20.allowanceCall userAddr (addrCurvePool addrs))
      , (addrUsdc addrs, ERC20.allowanceCall userAddr (addrLeverageRouter addrs))
      , (addrUsdc addrs, ERC20.allowanceCall userAddr (addrBullLeverageRouter addrs))
      , (addrDxyBear addrs, ERC20.allowanceCall userAddr (addrSyntheticSplitter addrs))
      , (addrDxyBear addrs, ERC20.allowanceCall userAddr (addrStakingBear addrs))
      , (addrDxyBear addrs, ERC20.allowanceCall userAddr (addrLeverageRouter addrs))
      , (addrDxyBear addrs, ERC20.allowanceCall userAddr (addrCurvePool addrs))
      , (addrDxyBull addrs, ERC20.allowanceCall userAddr (addrSyntheticSplitter addrs))
      , (addrDxyBull addrs, ERC20.allowanceCall userAddr (addrStakingBull addrs))
      , (addrDxyBull addrs, ERC20.allowanceCall userAddr (addrBullLeverageRouter addrs))
      , (addrDxyBull addrs, ERC20.allowanceCall userAddr (addrZapRouter addrs))
      , (addrUsdc addrs, ERC20.allowanceCall userAddr (addrMorpho addrs))
      ]

  case eResults of
    Right
      [ usdcSplitterBs
        , usdcZapBs
        , usdcCurveBs
        , usdcLevRouterBs
        , usdcBullLevRouterBs
        , bearSplitterBs
        , bearStakingBs
        , bearLeverageBs
        , bearCurveBs
        , bullSplitterBs
        , bullStakingBs
        , bullLeverageBs
        , bullZapBs
        , usdcMorphoBs
        ] -> do
        let usdcSplitter = ERC20.decodeAllowance usdcSplitterBs
            usdcZap = ERC20.decodeAllowance usdcZapBs
            usdcCurve = ERC20.decodeAllowance usdcCurveBs
            usdcLevRouter = ERC20.decodeAllowance usdcLevRouterBs
            usdcBullLevRouter = ERC20.decodeAllowance usdcBullLevRouterBs
            bearSplitter = ERC20.decodeAllowance bearSplitterBs
            bearStaking = ERC20.decodeAllowance bearStakingBs
            bearLeverage = ERC20.decodeAllowance bearLeverageBs
            bearCurve = ERC20.decodeAllowance bearCurveBs
            bullSplitter = ERC20.decodeAllowance bullSplitterBs
            bullStaking = ERC20.decodeAllowance bullStakingBs
            bullLeverage = ERC20.decodeAllowance bullLeverageBs
            bullZap = ERC20.decodeAllowance bullZapBs
            usdcMorpho = ERC20.decodeAllowance usdcMorphoBs
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
    Left err -> pure $ Left err
    _ -> pure $ Left $ internalError "Failed to fetch allowances"

-- Leverage positions
getLeveragePositions :: EthClient -> Config -> Text -> IO (Either ApiError LeveragePositions)
getLeveragePositions client cfg userAddr = do
  let addrs = currentAddresses (cfgDeployments cfg)
      morphoAddr = addrMorpho addrs
      bearMarketIdBs = hexToByteString (addrMorphoMarketBear addrs)
      bullMarketIdBs = hexToByteString (addrMorphoMarketBull addrs)

  eResults <-
    multicallBytes
      client
      [ (addrLeverageRouter addrs, LevRouter.getCollateralCall userAddr)
      , (addrLeverageRouter addrs, LevRouter.getActualDebtCall userAddr)
      , (addrBasketOracle addrs, Oracle.latestRoundDataCall)
      , (addrSyntheticSplitter addrs, Splitter.capCall)
      , (morphoAddr, Morpho.idToMarketParamsCall bearMarketIdBs)
      , (addrBullLeverageRouter addrs, LevRouter.getCollateralCall userAddr)
      , (addrBullLeverageRouter addrs, LevRouter.getActualDebtCall userAddr)
      , (addrBasketOracle addrs, Oracle.latestRoundDataCall)
      , (addrSyntheticSplitter addrs, Splitter.capCall)
      , (morphoAddr, Morpho.idToMarketParamsCall bullMarketIdBs)
      ]

  case eResults of
    Right
      [ bearCollateralBs
        , bearDebtBs
        , bearOracleBs
        , bearCapBs
        , bearMarketParamsBs
        , bullCollateralBs
        , bullDebtBs
        , bullOracleBs
        , bullCapBs
        , bullMarketParamsBs
        ] ->
        pure $
          Right $
            LeveragePositions
              { levPosBear =
                  buildLeveragePosition
                    "BEAR"
                    (LevRouter.decodeCollateral bearCollateralBs)
                    (LevRouter.decodeActualDebt bearDebtBs)
                    (Oracle.decodeLatestRoundData bearOracleBs)
                    (Splitter.decodeCap bearCapBs)
                    (Morpho.decodeIdToMarketParams bearMarketParamsBs)
              , levPosBull =
                  buildLeveragePosition
                    "BULL"
                    (LevRouter.decodeCollateral bullCollateralBs)
                    (LevRouter.decodeActualDebt bullDebtBs)
                    (Oracle.decodeLatestRoundData bullOracleBs)
                    (Splitter.decodeCap bullCapBs)
                    (Morpho.decodeIdToMarketParams bullMarketParamsBs)
              }
    Left err -> pure $ Left err
    _ -> pure $ Left $ internalError "Failed to fetch leverage positions"

buildLeveragePosition :: Text -> Integer -> Integer -> Oracle.RoundData -> Integer -> Morpho.MarketParams -> Maybe LeveragePosition
buildLeveragePosition side collateral debt oracle cap mp
  | collateral == 0 = Nothing
  | otherwise =
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
  where
    oraclePrice = Oracle.rdAnswer oracle
    tokenPrice =
      if side == "BEAR"
        then oraclePrice
        else if cap > oraclePrice then cap - oraclePrice else 0
    lltv = Morpho.mpLltv mp
    collateralUsd = (collateral * tokenPrice) `div` (10 ^ (23 :: Integer))
    netValue = if collateralUsd > debt then collateralUsd - debt else 0
    leverage =
      if netValue > 0
        then (collateralUsd * 100) `div` netValue
        else 0
    healthFactor =
      if debt > 0 && lltv > 0
        then (collateralUsd * lltv * 100) `div` (debt * 10 ^ (18 :: Integer))
        else 0
    liquidationPriceRaw =
      if collateral > 0 && lltv > 0
        then (debt * 10 ^ (41 :: Integer)) `div` (collateral * lltv)
        else 0
    liquidationPrice =
      if side == "BEAR"
        then liquidationPriceRaw `div` 100
        else
          if cap > liquidationPriceRaw
            then (cap - liquidationPriceRaw) `div` 100
            else 0

-- Lending positions
getLendingPositions :: EthClient -> Config -> Text -> IO (Either ApiError LendingPositions)
getLendingPositions client cfg userAddr = do
  let addrs = currentAddresses (cfgDeployments cfg)
      morphoAddr = addrMorpho addrs
      bearMarketIdBs = hexToByteString (addrMorphoMarketBear addrs)
      bullMarketIdBs = hexToByteString (addrMorphoMarketBull addrs)

  eResults <-
    multicallBytes
      client
      [ (morphoAddr, Morpho.positionCall bearMarketIdBs userAddr)
      , (morphoAddr, Morpho.marketCall bearMarketIdBs)
      , (addrMorphoOracleBear addrs, MorphoOracle.priceCall)
      , (morphoAddr, Morpho.idToMarketParamsCall bearMarketIdBs)
      , (morphoAddr, Morpho.positionCall bullMarketIdBs userAddr)
      , (morphoAddr, Morpho.marketCall bullMarketIdBs)
      , (addrMorphoOracleBull addrs, MorphoOracle.priceCall)
      , (morphoAddr, Morpho.idToMarketParamsCall bullMarketIdBs)
      ]

  case eResults of
    Right
      [ bearPositionBs
        , bearMarketBs
        , bearOraclePriceBs
        , bearMarketParamsBs
        , bullPositionBs
        , bullMarketBs
        , bullOraclePriceBs
        , bullMarketParamsBs
        ] ->
        pure $
          Right $
            LendingPositions
              { lendPosBear =
                  buildLendingPosition
                    (Morpho.decodePosition bearPositionBs)
                    (Morpho.decodeMarket bearMarketBs)
                    (MorphoOracle.decodePrice bearOraclePriceBs)
                    (Morpho.decodeIdToMarketParams bearMarketParamsBs)
              , lendPosBull =
                  buildLendingPosition
                    (Morpho.decodePosition bullPositionBs)
                    (Morpho.decodeMarket bullMarketBs)
                    (MorphoOracle.decodePrice bullOraclePriceBs)
                    (Morpho.decodeIdToMarketParams bullMarketParamsBs)
              }
    Left err -> pure $ Left err
    _ -> pure $ Left $ internalError "Failed to fetch lending positions"

buildLendingPosition :: Morpho.Position -> Morpho.Market -> Integer -> Morpho.MarketParams -> Maybe LendingPosition
buildLendingPosition pos mkt oraclePrice mp
  | Morpho.posSupplyShares pos == 0 && Morpho.posBorrowShares pos == 0 && Morpho.posCollateral pos == 0 = Nothing
  | otherwise =
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
  where
    supplyShares = Morpho.posSupplyShares pos
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
    collateralUsd =
      if oraclePrice > 0
        then (collateral * oraclePrice) `div` (10 ^ (39 :: Integer))
        else 0
    maxBorrow =
      if lltv > 0 && oraclePrice > 0
        then (collateral * oraclePrice * lltv) `div` (10 ^ (57 :: Integer))
        else 0
    availableToBorrow =
      if maxBorrow > borrowedAssets then maxBorrow - borrowedAssets else 0
    healthFactor =
      if borrowedAssets > 0
        then (maxBorrow * 10 ^ (18 :: Integer)) `div` borrowedAssets
        else 0

-- Morpho authorization
getMorphoAuthorization :: EthClient -> Config -> Text -> IO (Either ApiError MorphoAuthorization)
getMorphoAuthorization client cfg userAddr = do
  let addrs = currentAddresses (cfgDeployments cfg)
      morphoAddr = addrMorpho addrs
  eResults <-
    multicallBytes
      client
      [ (morphoAddr, Morpho.isAuthorizedCall userAddr (addrLeverageRouter addrs))
      , (morphoAddr, Morpho.isAuthorizedCall userAddr (addrBullLeverageRouter addrs))
      ]
  pure $ case eResults of
    Right [bearBs, bullBs] ->
      Right $
        MorphoAuthorization
          { authBearLeverageRouter = Morpho.decodeIsAuthorized bearBs
          , authBullLeverageRouter = Morpho.decodeIsAuthorized bullBs
          }
    Right _ -> Left $ internalError "Failed to fetch Morpho authorization"
    Left err -> Left err

emptyLeverage :: LeveragePositions
emptyLeverage = LeveragePositions { levPosBear = Nothing, levPosBull = Nothing }

emptyLending :: LendingPositions
emptyLending = LendingPositions { lendPosBear = Nothing, lendPosBull = Nothing }

emptyAllowances :: UserAllowances
emptyAllowances =
  UserAllowances
    { allowUsdc = UsdcAllowances 0 0 0 0 0 0 0
    , allowBear = BearAllowances 0 0 0 0
    , allowBull = BullAllowances 0 0 0 0
    }

emptyAuth :: MorphoAuthorization
emptyAuth = MorphoAuthorization { authBearLeverageRouter = False, authBullLeverageRouter = False }

multicallBytes :: EthClient -> [(Text, ByteString)] -> IO (Either ApiError [ByteString])
multicallBytes client calls = do
  eResults <-
    Multicall.multicall
      client
      [ Multicall.Call
          { Multicall.callTarget = target
          , Multicall.callAllowFailure = False
          , Multicall.callCalldata = calldata
          }
      | (target, calldata) <- calls
      ]
  pure $ case eResults of
    Left err -> Left $ rpcErrorToApiError err
    Right results
      | length results /= length calls -> Left $ internalError "Multicall returned an unexpected number of results"
      | any (not . Multicall.resultSuccess) results -> Left $ internalError "Multicall subcall failed"
      | otherwise -> Right $ map Multicall.resultData results
