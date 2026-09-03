module Plether.Types.VaultActivity
  ( VaultActivityDeploymentIdentity (..)
  , VaultActivityCoverage (..)
  , VaultShareAttributionCoverage (..)
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
  , vacShareAttribution :: VaultShareAttributionCoverage
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
      , "shareAttribution" .= vacShareAttribution
      ]

data VaultShareAttributionCoverage = VaultShareAttributionCoverage
  { vsacConfirmedThroughBlock :: Integer
  , vsacConfirmedThroughHash :: Maybe Text
  , vsacComplete :: Bool
  , vsacLastSuccessfulPoll :: Integer
  }
  deriving stock (Eq, Show)

instance ToJSON VaultShareAttributionCoverage where
  toJSON VaultShareAttributionCoverage {..} =
    object
      [ "confirmedThroughBlock" .= vsacConfirmedThroughBlock
      , "confirmedThroughHash" .= vsacConfirmedThroughHash
      , "complete" .= vsacComplete
      , "lastSuccessfulPoll" .= vsacLastSuccessfulPoll
      ]

data VaultActivityHolder = VaultActivityHolder
  { vahAddress :: Text
  , vahShareBalance :: Integer
  , vahUnclaimedDepositShares :: Integer
  , vahWithdrawalEscrowShares :: Integer
  , vahTotalAttributedShares :: Integer
  }
  deriving stock (Eq, Show)

instance ToJSON VaultActivityHolder where
  toJSON VaultActivityHolder {..} =
    object
      [ "address" .= vahAddress
      , "shareBalance" .= show vahShareBalance
      , "unclaimedDepositShares" .= show vahUnclaimedDepositShares
      , "withdrawalEscrowShares" .= show vahWithdrawalEscrowShares
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
