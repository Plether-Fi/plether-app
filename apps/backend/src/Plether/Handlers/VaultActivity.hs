module Plether.Handlers.VaultActivity
  ( getVaultActivity
  , getVaultAccountRequestIds
  ) where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadOnly)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.Database (DbPool, withDb)
import Plether.Database.VaultActivity
  ( VaultActivityDeployment (..)
  , VaultActivityIndexerStateRow (..)
  , VaultHolderRow (..)
  , VaultRequestRow (..)
  , countVaultHolders
  , countVaultRequests
  , getVaultActivityIndexerState
  , getVaultHolders
  , getVaultRequestIds
  , getVaultRequests
  )
import Plether.Types (ApiResponse, mkResponse)
import Plether.Types.VaultActivity
  ( VaultActivityCoverage (..)
  , VaultActivityDeploymentIdentity (..)
  , VaultActivityHolder (..)
  , VaultActivityItem (..)
  , VaultActivityResponse (..)
  , VaultActivityTrancheData (..)
  , VaultRequestIdsResponse (..)
  )

activityLimit :: Int
activityLimit = 250

-- | A missing result means the deployment has never completed its canonical
-- backfill and must remain unavailable. Once published, incomplete current
-- coverage is served with explicit stale metadata.
getVaultActivity
  :: DbPool
  -> VaultActivityDeployment
  -> IO (Maybe (ApiResponse VaultActivityResponse))
getVaultActivity pool deployment = do
  now <- floor <$> getPOSIXTime
  withDb pool $ \conn -> withVaultActivityReadSnapshot conn $ do
    mState <- getVaultActivityIndexerState conn deployment
    case mState of
      Just state | isPublishable state -> do
        senior <- trancheData conn "senior" $ vadSeniorVault deployment
        junior <- trancheData conn "junior" $ vadJuniorVault deployment
        let coverage = coverageAt now state
        pure $
          Just $
            mkResponse
              (vaisLastIndexedBlock state)
              (vadChainId deployment)
              VaultActivityResponse
                { varDeployment = deploymentIdentity deployment
                , varCoverage = coverage
                , varSenior = senior
                , varJunior = junior
                }
      _ -> pure Nothing
 where
  trancheData conn tranche vault = do
    holders <- getVaultHolders conn deployment vault activityLimit
    holderCount <- countVaultHolders conn deployment vault
    requests <- getVaultRequests conn deployment vault activityLimit
    requestCount <- countVaultRequests conn deployment vault
    pure $
      VaultActivityTrancheData
        { vatHolders = map holderRow holders
        , vatHolderCount = fromIntegral holderCount
        , vatHoldersTruncated = holderCount > fromIntegral activityLimit
        , vatActivity = map (activityRow tranche) requests
        , vatActivityCount = fromIntegral requestCount
        , vatActivityTruncated = requestCount > fromIntegral activityLimit
        }

getVaultAccountRequestIds
  :: DbPool
  -> VaultActivityDeployment
  -> Text
  -> Text
  -> Int
  -> Maybe Integer
  -> IO (Maybe (ApiResponse VaultRequestIdsResponse))
getVaultAccountRequestIds pool deployment tranche account requestedLimit cursor = do
  now <- floor <$> getPOSIXTime
  withDb pool $ \conn -> withVaultActivityReadSnapshot conn $ do
    mState <- getVaultActivityIndexerState conn deployment
    case mState of
      Just state | isPublishable state -> do
        let vault = if tranche == "senior" then vadSeniorVault deployment else vadJuniorVault deployment
            limit = max 1 $ min 250 requestedLimit
        found <- getVaultRequestIds conn deployment vault account (limit + 1) cursor
        let page = take limit found
            nextCursor = if length found > limit then lastMaybe page else Nothing
            coverage = coverageAt now state
        pure $
          Just $
            mkResponse
              (vaisLastIndexedBlock state)
              (vadChainId deployment)
              VaultRequestIdsResponse
                { vrirTranche = tranche
                , vrirAccount = T.toLower account
                , vrirRequestIds = page
                , vrirNextCursor = nextCursor
                , vrirConfirmedThroughBlock = vaisLastIndexedBlock state
                , vrirStale = vacStale coverage
                }
      _ -> pure Nothing

deploymentIdentity :: VaultActivityDeployment -> VaultActivityDeploymentIdentity
deploymentIdentity VaultActivityDeployment {..} =
  VaultActivityDeploymentIdentity
    { vaidChainId = vadChainId
    , vaidHousePool = vadHousePool
    , vaidSeniorVault = vadSeniorVault
    , vaidJuniorVault = vadJuniorVault
    , vaidDeploymentBlock = vadDeploymentBlock
    }

coverageAt :: Integer -> VaultActivityIndexerStateRow -> VaultActivityCoverage
coverageAt now state =
  VaultActivityCoverage
    { vacConfirmedThroughBlock = vaisLastIndexedBlock state
    , vacConfirmedThroughHash = vaisLastIndexedBlockHash state
    , vacObservedSafeHeadBlock = vaisSafeHeadBlock state
    , vacObservedSafeHeadHash = vaisSafeHeadBlockHash state
    , vacComplete = complete
    , vacStale = not complete || lagSeconds > 120 || pollAge > 180
    , vacLagBlocks = lagBlocks
    , vacLagSeconds = lagSeconds
    , vacLastSuccessfulPoll = vaisLastSuccessTimestamp state
    }
 where
  complete = vaisLastIndexedBlock state >= vaisSafeHeadBlock state
  lagBlocks = max 0 $ vaisSafeHeadBlock state - vaisLastIndexedBlock state
  lagSeconds = max 0 $ vaisSafeHeadTimestamp state - vaisLastIndexedBlockTimestamp state
  pollAge = max 0 $ now - vaisLastSuccessTimestamp state

holderRow :: VaultHolderRow -> VaultActivityHolder
holderRow VaultHolderRow {..} = VaultActivityHolder vhrAddress vhrShareBalance

activityRow :: Text -> VaultRequestRow -> VaultActivityItem
activityRow tranche VaultRequestRow {..} =
  VaultActivityItem
    { vaiEventId = vrrTxHash <> "-" <> T.pack (show vrrLogIndex)
    , vaiTranche = tranche
    , vaiKind = kind
    , vaiAccount = vrrOwner
    , vaiRequestId = vrrRequestId
    , vaiRawAssets = if kind == "deposit" then Just vrrRawAmount else Nothing
    , vaiRawShares = if kind == "withdraw" then Just vrrRawAmount else Nothing
    , vaiTimestamp = vrrTimestamp
    , vaiBlockNumber = vrrBlockNumber
    , vaiTransactionIndex = vrrTxIndex
    , vaiLogIndex = vrrLogIndex
    , vaiTransactionHash = vrrTxHash
    }
 where
  kind = if vrrEventName == "RedeemRequest" then "withdraw" else "deposit"

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe values = Just $ last values

isPublishable :: VaultActivityIndexerStateRow -> Bool
isPublishable state =
  vaisBackfillComplete state
    && isJust (vaisLastIndexedBlockHash state)
    && isJust (vaisSafeHeadBlockHash state)

withVaultActivityReadSnapshot :: Connection -> IO value -> IO value
withVaultActivityReadSnapshot =
  withTransactionMode $
    TransactionMode
      { isolationLevel = RepeatableRead
      , readWriteMode = ReadOnly
      }
