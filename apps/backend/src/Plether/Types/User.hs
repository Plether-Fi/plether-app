module Plether.Types.User
  ( UserDashboard (..)
  , UserBalances (..)
  , LeveragePosition (..)
  , LendingPosition (..)
  , UserAllowances (..)
  , UsdcAllowances (..)
  , BearAllowances (..)
  , BullAllowances (..)
  , UserPositions (..)
  , LeveragePositions (..)
  , LendingPositions (..)
  , MorphoAuthorization (..)
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import GHC.Generics (Generic)

data UserBalances = UserBalances
  { balUsdc :: Integer
  , balBear :: Integer
  , balBull :: Integer
  , balStakedBear :: Integer
  , balStakedBull :: Integer
  , balStakedBearAssets :: Integer
  , balStakedBullAssets :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON UserBalances where
  toJSON UserBalances {..} =
    object
      [ "usdc" .= show balUsdc
      , "bear" .= show balBear
      , "bull" .= show balBull
      , "stakedBear" .= show balStakedBear
      , "stakedBull" .= show balStakedBull
      , "stakedBearAssets" .= show balStakedBearAssets
      , "stakedBullAssets" .= show balStakedBullAssets
      ]

data LeveragePosition = LeveragePosition
  { levCollateral :: Integer
  , levCollateralUsd :: Integer
  , levDebt :: Integer
  , levHealthFactor :: Integer
  , levLiquidationPrice :: Integer
  , levLeverage :: Integer
  , levNetValue :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON LeveragePosition where
  toJSON LeveragePosition {..} =
    object
      [ "collateral" .= show levCollateral
      , "collateralUsd" .= show levCollateralUsd
      , "debt" .= show levDebt
      , "healthFactor" .= show levHealthFactor
      , "liquidationPrice" .= show levLiquidationPrice
      , "leverage" .= show levLeverage
      , "netValue" .= show levNetValue
      ]

data LendingPosition = LendingPosition
  { lendSupplied :: Integer
  , lendSuppliedShares :: Integer
  , lendBorrowed :: Integer
  , lendBorrowedShares :: Integer
  , lendAvailableToBorrow :: Integer
  , lendCollateral :: Integer
  , lendHealthFactor :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON LendingPosition where
  toJSON LendingPosition {..} =
    object
      [ "supplied" .= show lendSupplied
      , "suppliedShares" .= show lendSuppliedShares
      , "borrowed" .= show lendBorrowed
      , "borrowedShares" .= show lendBorrowedShares
      , "availableToBorrow" .= show lendAvailableToBorrow
      , "collateral" .= show lendCollateral
      , "healthFactor" .= show lendHealthFactor
      ]

data LeveragePositions = LeveragePositions
  { levPosBear :: Maybe LeveragePosition
  , levPosBull :: Maybe LeveragePosition
  }
  deriving stock (Show, Generic)

instance ToJSON LeveragePositions where
  toJSON LeveragePositions {..} =
    object
      [ "bear" .= levPosBear
      , "bull" .= levPosBull
      ]

data LendingPositions = LendingPositions
  { lendPosBear :: Maybe LendingPosition
  , lendPosBull :: Maybe LendingPosition
  }
  deriving stock (Show, Generic)

instance ToJSON LendingPositions where
  toJSON LendingPositions {..} =
    object
      [ "bear" .= lendPosBear
      , "bull" .= lendPosBull
      ]

data MorphoAuthorization = MorphoAuthorization
  { authBearLeverageRouter :: Bool
  , authBullLeverageRouter :: Bool
  }
  deriving stock (Show, Generic)

instance ToJSON MorphoAuthorization where
  toJSON MorphoAuthorization {..} =
    object
      [ "bearLeverageRouter" .= authBearLeverageRouter
      , "bullLeverageRouter" .= authBullLeverageRouter
      ]

data UserDashboard = UserDashboard
  { dashBalances :: UserBalances
  , dashLeverage :: LeveragePositions
  , dashLending :: LendingPositions
  , dashAllowances :: UserAllowances
  , dashAuthorization :: MorphoAuthorization
  }
  deriving stock (Show, Generic)

instance ToJSON UserDashboard where
  toJSON UserDashboard {..} =
    object
      [ "balances" .= dashBalances
      , "leverage" .= dashLeverage
      , "lending" .= dashLending
      , "allowances" .= dashAllowances
      , "authorization" .= dashAuthorization
      ]

data UsdcAllowances = UsdcAllowances
  { usdcAllowSplitter :: Integer
  , usdcAllowZap :: Integer
  , usdcAllowMorphoBear :: Integer
  , usdcAllowMorphoBull :: Integer
  , usdcAllowCurvePool :: Integer
  , usdcAllowLeverageRouter :: Integer
  , usdcAllowBullLeverageRouter :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON UsdcAllowances where
  toJSON UsdcAllowances {..} =
    object
      [ "splitter" .= show usdcAllowSplitter
      , "zap" .= show usdcAllowZap
      , "morphoBear" .= show usdcAllowMorphoBear
      , "morphoBull" .= show usdcAllowMorphoBull
      , "curvePool" .= show usdcAllowCurvePool
      , "leverageRouter" .= show usdcAllowLeverageRouter
      , "bullLeverageRouter" .= show usdcAllowBullLeverageRouter
      ]

data BearAllowances = BearAllowances
  { bearAllowSplitter :: Integer
  , bearAllowStaking :: Integer
  , bearAllowLeverageRouter :: Integer
  , bearAllowCurvePool :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON BearAllowances where
  toJSON BearAllowances {..} =
    object
      [ "splitter" .= show bearAllowSplitter
      , "staking" .= show bearAllowStaking
      , "leverageRouter" .= show bearAllowLeverageRouter
      , "curvePool" .= show bearAllowCurvePool
      ]

data BullAllowances = BullAllowances
  { bullAllowSplitter :: Integer
  , bullAllowStaking :: Integer
  , bullAllowLeverageRouter :: Integer
  , bullAllowZapRouter :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON BullAllowances where
  toJSON BullAllowances {..} =
    object
      [ "splitter" .= show bullAllowSplitter
      , "staking" .= show bullAllowStaking
      , "leverageRouter" .= show bullAllowLeverageRouter
      , "zapRouter" .= show bullAllowZapRouter
      ]

data UserAllowances = UserAllowances
  { allowUsdc :: UsdcAllowances
  , allowBear :: BearAllowances
  , allowBull :: BullAllowances
  }
  deriving stock (Show, Generic)

instance ToJSON UserAllowances where
  toJSON UserAllowances {..} =
    object
      [ "usdc" .= allowUsdc
      , "bear" .= allowBear
      , "bull" .= allowBull
      ]

data UserPositions = UserPositions
  { posLeverage :: LeveragePositions
  , posLending :: LendingPositions
  }
  deriving stock (Show, Generic)

instance ToJSON UserPositions where
  toJSON UserPositions {..} =
    object
      [ "leverage" .= posLeverage
      , "lending" .= posLending
      ]
