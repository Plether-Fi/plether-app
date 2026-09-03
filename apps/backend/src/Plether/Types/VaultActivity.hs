module Plether.Types.VaultActivity
  ( VaultActivityDeploymentIdentity (..)
  , VaultActivityCoverage (..)
  , VaultDepositAttributionCoverage (..)
  , VaultActivityHolder (..)
  , VaultActivityItem (..)
  , VaultActivityTrancheData (..)
  , VaultActivityResponse (..)
  , VaultRequestIdsResponse (..)
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)

data VaultActivityDeploymentIdentity = VaultActivityDeploymentIdentity
  { vaidChainId :: Integer
  , vaidHousePool :: Text
  , vaidSeniorVault :: Text
  , vaidJuniorVault :: Text
  , vaidDeploymentBlock :: Integer
  }
  deriving stock (Eq, Show)

instance ToJSON VaultActivityDeploymentIdentity where
  toJSON VaultActivityDeploymentIdentity {..} =
    object
      [ "chainId" .= vaidChainId
      , "housePool" .= vaidHousePool
      , "seniorVault" .= vaidSeniorVault
      , "juniorVault" .= vaidJuniorVault
      , "deploymentBlock" .= vaidDeploymentBlock
      ]

data VaultActivityCoverage = VaultActivityCoverage
  { vacConfirmedThroughBlock :: Integer
  , vacConfirmedThroughHash :: Maybe Text
  , vacObservedSafeHeadBlock :: Integer
  , vacObservedSafeHeadHash :: Maybe Text
  , vacComplete :: Bool
  , vacStale :: Bool
  , vacLagBlocks :: Integer
  , vacLagSeconds :: Integer
  , vacLastSuccessfulPoll :: Integer
  , vacDepositShareAttribution :: VaultDepositAttributionCoverage
  }
  deriving stock (Eq, Show)

instance ToJSON VaultActivityCoverage where
  toJSON VaultActivityCoverage {..} =
    object
      [ "confirmedThroughBlock" .= vacConfirmedThroughBlock
      , "confirmedThroughHash" .= vacConfirmedThroughHash
      , "observedSafeHeadBlock" .= vacObservedSafeHeadBlock
      , "observedSafeHeadHash" .= vacObservedSafeHeadHash
      , "complete" .= vacComplete
      , "stale" .= vacStale
      , "lagBlocks" .= vacLagBlocks
      , "lagSeconds" .= vacLagSeconds
      , "lastSuccessfulPoll" .= vacLastSuccessfulPoll
      , "depositShareAttribution" .= vacDepositShareAttribution
      ]

data VaultDepositAttributionCoverage = VaultDepositAttributionCoverage
  { vdacConfirmedThroughBlock :: Integer
  , vdacConfirmedThroughHash :: Maybe Text
  , vdacComplete :: Bool
  , vdacLastSuccessfulPoll :: Integer
  }
  deriving stock (Eq, Show)

instance ToJSON VaultDepositAttributionCoverage where
  toJSON VaultDepositAttributionCoverage {..} =
    object
      [ "confirmedThroughBlock" .= vdacConfirmedThroughBlock
      , "confirmedThroughHash" .= vdacConfirmedThroughHash
      , "complete" .= vdacComplete
      , "lastSuccessfulPoll" .= vdacLastSuccessfulPoll
      ]

data VaultActivityHolder = VaultActivityHolder
  { vahAddress :: Text
  , vahShareBalance :: Integer
  , vahUnclaimedDepositShares :: Integer
  , vahTotalAttributedShares :: Integer
  }
  deriving stock (Eq, Show)

instance ToJSON VaultActivityHolder where
  toJSON VaultActivityHolder {..} =
    object
      [ "address" .= vahAddress
      , "shareBalance" .= show vahShareBalance
      , "unclaimedDepositShares" .= show vahUnclaimedDepositShares
      , "totalAttributedShares" .= show vahTotalAttributedShares
      ]

data VaultActivityItem = VaultActivityItem
  { vaiEventId :: Text
  , vaiTranche :: Text
  , vaiKind :: Text
  , vaiAccount :: Text
  , vaiRequestId :: Integer
  , vaiRawAssets :: Maybe Integer
  , vaiRawShares :: Maybe Integer
  , vaiTimestamp :: Integer
  , vaiBlockNumber :: Integer
  , vaiTransactionIndex :: Integer
  , vaiLogIndex :: Integer
  , vaiTransactionHash :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON VaultActivityItem where
  toJSON VaultActivityItem {..} =
    object
      [ "id" .= vaiEventId
      , "tranche" .= vaiTranche
      , "kind" .= vaiKind
      , "account" .= vaiAccount
      , "requestId" .= show vaiRequestId
      , "rawAssets" .= fmap show vaiRawAssets
      , "rawShares" .= fmap show vaiRawShares
      , "timestamp" .= vaiTimestamp
      , "blockNumber" .= vaiBlockNumber
      , "transactionIndex" .= vaiTransactionIndex
      , "logIndex" .= vaiLogIndex
      , "transactionHash" .= vaiTransactionHash
      ]

data VaultActivityTrancheData = VaultActivityTrancheData
  { vatHolders :: [VaultActivityHolder]
  , vatHolderCount :: Integer
  , vatHoldersTruncated :: Bool
  , vatTotalAttributedShares :: Integer
  , vatActivity :: [VaultActivityItem]
  , vatActivityCount :: Integer
  , vatActivityTruncated :: Bool
  }
  deriving stock (Eq, Show)

instance ToJSON VaultActivityTrancheData where
  toJSON VaultActivityTrancheData {..} =
    object
      [ "holders" .= vatHolders
      , "holderCount" .= vatHolderCount
      , "holdersTruncated" .= vatHoldersTruncated
      , "totalAttributedShares" .= show vatTotalAttributedShares
      , "activity" .= vatActivity
      , "activityCount" .= vatActivityCount
      , "activityTruncated" .= vatActivityTruncated
      ]

data VaultActivityResponse = VaultActivityResponse
  { varDeployment :: VaultActivityDeploymentIdentity
  , varCoverage :: VaultActivityCoverage
  , varSenior :: VaultActivityTrancheData
  , varJunior :: VaultActivityTrancheData
  }
  deriving stock (Eq, Show)

instance ToJSON VaultActivityResponse where
  toJSON VaultActivityResponse {..} =
    object
      [ "deployment" .= varDeployment
      , "coverage" .= varCoverage
      , "senior" .= varSenior
      , "junior" .= varJunior
      ]

data VaultRequestIdsResponse = VaultRequestIdsResponse
  { vrirTranche :: Text
  , vrirAccount :: Text
  , vrirRequestIds :: [Integer]
  , vrirNextCursor :: Maybe Integer
  , vrirConfirmedThroughBlock :: Integer
  , vrirStale :: Bool
  }
  deriving stock (Eq, Show)

instance ToJSON VaultRequestIdsResponse where
  toJSON VaultRequestIdsResponse {..} =
    object
      [ "tranche" .= vrirTranche
      , "account" .= vrirAccount
      , "requestIds" .= map show vrirRequestIds
      , "nextCursor" .= fmap show vrirNextCursor
      , "confirmedThroughBlock" .= vrirConfirmedThroughBlock
      , "stale" .= vrirStale
      ]
