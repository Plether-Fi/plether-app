module Plether.Types.VaultPerformance
  ( VaultPerformanceDeployment (..)
  , VaultPerformanceCoverage (..)
  , VaultPerformancePoint (..)
  , VaultPerformanceTranche (..)
  , VaultPerformanceHistory (..)
  , vaultPerformanceRange
  , vaultPerformanceIntervalSeconds
  , vaultPerformancePointCount
  , isCanonicalVaultPerformanceRequest
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

vaultPerformanceRange :: Text
vaultPerformanceRange = "7d"

vaultPerformanceIntervalSeconds :: Integer
vaultPerformanceIntervalSeconds = 3_600

-- Seven complete days have 168 intervals and therefore 169 boundary samples.
vaultPerformancePointCount :: Int
vaultPerformancePointCount = 169

-- | The endpoint has one cacheable public shape. Rejecting extra, duplicate,
-- or alternative spellings prevents semantically identical cache-key variants.
isCanonicalVaultPerformanceRequest :: [Text] -> Maybe Text -> Maybe Text -> Bool
isCanonicalVaultPerformanceRequest queryKeys requestedRange requestedInterval =
  length queryKeys == 2
    && count "range" == 1
    && count "interval" == 1
    && requestedRange == Just vaultPerformanceRange
    && requestedInterval == Just "3600"
 where
  count key = length $ filter (== key) queryKeys

data VaultPerformanceDeployment = VaultPerformanceDeployment
  { vpdChainId :: Integer
  , vpdHousePool :: Text
  , vpdSeniorVault :: Text
  , vpdJuniorVault :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON VaultPerformanceDeployment where
  toJSON VaultPerformanceDeployment {..} =
    object
      [ "chainId" .= vpdChainId
      , "housePool" .= vpdHousePool
      , "seniorVault" .= vpdSeniorVault
      , "juniorVault" .= vpdJuniorVault
      ]

-- Public coverage bounds are the actual sampled block timestamps used in the
-- APY elapsed-time calculation. Hourly epoch keys remain an internal storage
-- and strict-continuity concern.
data VaultPerformanceCoverage = VaultPerformanceCoverage
  { vpcStart :: Maybe Integer
  , vpcEnd :: Maybe Integer
  , vpcComplete :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON VaultPerformanceCoverage where
  toJSON VaultPerformanceCoverage {..} =
    object
      [ "start" .= vpcStart
      , "end" .= vpcEnd
      , "complete" .= vpcComplete
      ]

-- | All integer-valued EVM quantities are kept lossless in Haskell and encoded
-- as decimal strings below. 'vppSharePrice' is a 1e18-scaled USDC/share value.
data VaultPerformancePoint = VaultPerformancePoint
  { vppTimestamp :: Integer
  , vppBlockNumber :: Integer
  , vppMarkFresh :: Bool
  , vppSharePrice :: Integer
  , vppTotalAssets :: Integer
  , vppTotalSupply :: Integer
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON VaultPerformancePoint where
  toJSON VaultPerformancePoint {..} =
    object
      [ "timestamp" .= vppTimestamp
      , "blockNumber" .= show vppBlockNumber
      , "markFresh" .= vppMarkFresh
      , "sharePrice" .= show vppSharePrice
      , "totalAssets" .= show vppTotalAssets
      , "totalSupply" .= show vppTotalSupply
      ]

data VaultPerformanceTranche = VaultPerformanceTranche
  { vptApy7d :: Maybe Double
  , vptReturn7d :: Maybe Double
  , vptPoints :: [VaultPerformancePoint]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON VaultPerformanceTranche where
  toJSON VaultPerformanceTranche {..} =
    object
      [ "apy7d" .= vptApy7d
      , "return7d" .= vptReturn7d
      , "points" .= vptPoints
      ]

data VaultPerformanceHistory = VaultPerformanceHistory
  { vphRange :: Text
  , vphIntervalSeconds :: Integer
  , vphDeployment :: VaultPerformanceDeployment
  , vphCoverage :: VaultPerformanceCoverage
  , vphSenior :: VaultPerformanceTranche
  , vphJunior :: VaultPerformanceTranche
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON VaultPerformanceHistory where
  toJSON VaultPerformanceHistory {..} =
    object
      [ "range" .= vphRange
      , "intervalSeconds" .= vphIntervalSeconds
      , "deployment" .= vphDeployment
      , "coverage" .= vphCoverage
      , "senior" .= vphSenior
      , "junior" .= vphJunior
      ]
