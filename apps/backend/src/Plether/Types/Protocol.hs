module Plether.Types.Protocol
  ( ProtocolStatus (..)
  , ProtocolState (..)
  , Prices (..)
  , OracleInfo (..)
  , StakingInfo (..)
  , StakingStats (..)
  , ApyInfo (..)
  , ApyStats (..)
  , ProtocolConfig (..)
  , Contracts (..)
  , Decimals (..)
  , Constants (..)
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

data ProtocolState = Active | Paused | Settled
  deriving stock (Show, Eq, Generic)

instance ToJSON ProtocolState where
  toJSON = \case
    Active -> "ACTIVE"
    Paused -> "PAUSED"
    Settled -> "SETTLED"

data Prices = Prices
  { priceBear :: Integer
  , priceBull :: Integer
  , priceCap :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON Prices where
  toJSON Prices {..} =
    object
      [ "bear" .= show priceBear
      , "bull" .= show priceBull
      , "cap" .= show priceCap
      ]

data OracleInfo = OracleInfo
  { oraclePrice :: Integer
  , oracleUpdatedAt :: POSIXTime
  , oracleDecimals :: Int
  }
  deriving stock (Show, Generic)

instance ToJSON OracleInfo where
  toJSON OracleInfo {..} =
    object
      [ "price" .= show oraclePrice
      , "updatedAt" .= (round oracleUpdatedAt :: Integer)
      , "decimals" .= oracleDecimals
      ]

data StakingStats = StakingStats
  { stakingTotalAssets :: Integer
  , stakingTotalShares :: Integer
  , stakingExchangeRate :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON StakingStats where
  toJSON StakingStats {..} =
    object
      [ "totalAssets" .= show stakingTotalAssets
      , "totalShares" .= show stakingTotalShares
      , "exchangeRate" .= show stakingExchangeRate
      ]

data StakingInfo = StakingInfo
  { stakingBear :: StakingStats
  , stakingBull :: StakingStats
  }
  deriving stock (Show, Generic)

instance ToJSON StakingInfo where
  toJSON StakingInfo {..} =
    object
      [ "bear" .= stakingBear
      , "bull" .= stakingBull
      ]

data ApyStats = ApyStats
  { apySupply :: Double
  , apyBorrow :: Double
  , apyUtilization :: Double
  }
  deriving stock (Show, Generic)

instance ToJSON ApyStats where
  toJSON ApyStats {..} =
    object
      [ "supply" .= apySupply
      , "borrow" .= apyBorrow
      , "utilization" .= apyUtilization
      ]

data ApyInfo = ApyInfo
  { apyBear :: ApyStats
  , apyBull :: ApyStats
  }
  deriving stock (Show, Generic)

instance ToJSON ApyInfo where
  toJSON ApyInfo {..} =
    object
      [ "bear" .= apyBear
      , "bull" .= apyBull
      ]

data ProtocolStatus = ProtocolStatus
  { statusPrices :: Prices
  , statusState :: ProtocolState
  , statusOracle :: OracleInfo
  , statusStaking :: StakingInfo
  , statusApy :: ApyInfo
  , statusTimestamp :: POSIXTime
  }
  deriving stock (Show, Generic)

instance ToJSON ProtocolStatus where
  toJSON ProtocolStatus {..} =
    object
      [ "prices" .= statusPrices
      , "status" .= statusState
      , "oracle" .= statusOracle
      , "staking" .= statusStaking
      , "apy" .= statusApy
      , "timestamp" .= (round statusTimestamp :: Integer)
      ]

data Contracts = Contracts
  { contractUsdc :: Text
  , contractDxyBear :: Text
  , contractDxyBull :: Text
  , contractSdxyBear :: Text
  , contractSdxyBull :: Text
  , contractSyntheticSplitter :: Text
  , contractCurvePool :: Text
  , contractZapRouter :: Text
  , contractLeverageRouter :: Text
  , contractBullLeverageRouter :: Text
  , contractBasketOracle :: Text
  , contractMorpho :: Text
  , contractMorphoBearMarket :: Text
  , contractMorphoBullMarket :: Text
  }
  deriving stock (Show, Generic)

instance ToJSON Contracts where
  toJSON Contracts {..} =
    object
      [ "usdc" .= contractUsdc
      , "dxyBear" .= contractDxyBear
      , "dxyBull" .= contractDxyBull
      , "sdxyBear" .= contractSdxyBear
      , "sdxyBull" .= contractSdxyBull
      , "syntheticSplitter" .= contractSyntheticSplitter
      , "curvePool" .= contractCurvePool
      , "zapRouter" .= contractZapRouter
      , "leverageRouter" .= contractLeverageRouter
      , "bullLeverageRouter" .= contractBullLeverageRouter
      , "basketOracle" .= contractBasketOracle
      , "morpho" .= contractMorpho
      , "morphoBearMarket" .= contractMorphoBearMarket
      , "morphoBullMarket" .= contractMorphoBullMarket
      ]

data Decimals = Decimals
  { decUsdc :: Int
  , decPlDxyBear :: Int
  , decPlDxyBull :: Int
  , decOraclePrice :: Int
  , decMorphoShares :: Int
  }
  deriving stock (Show, Generic)

instance ToJSON Decimals where
  toJSON Decimals {..} =
    object
      [ "usdc" .= decUsdc
      , "plDxyBear" .= decPlDxyBear
      , "plDxyBull" .= decPlDxyBull
      , "oraclePrice" .= decOraclePrice
      , "morphoShares" .= decMorphoShares
      ]

data Constants = Constants
  { constMaxSlippage :: Double
  , constMinLeverage :: Double
  , constMaxLeverage :: Double
  , constLiquidationLtv :: Double
  }
  deriving stock (Show, Generic)

instance ToJSON Constants where
  toJSON Constants {..} =
    object
      [ "maxSlippage" .= constMaxSlippage
      , "minLeverage" .= constMinLeverage
      , "maxLeverage" .= constMaxLeverage
      , "liquidationLtv" .= constLiquidationLtv
      ]

data ProtocolConfig = ProtocolConfig
  { configContracts :: Contracts
  , configDecimals :: Decimals
  , configConstants :: Constants
  , configChainId :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON ProtocolConfig where
  toJSON ProtocolConfig {..} =
    object
      [ "contracts" .= configContracts
      , "decimals" .= configDecimals
      , "constants" .= configConstants
      , "chainId" .= configChainId
      ]
