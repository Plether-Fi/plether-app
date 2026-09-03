module Plether.Handlers.VaultActivity
  ( getVaultActivity
  , getVaultAccountRequestIds
  , vaultCoverageIsStale
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
  , VaultAttributedHolderRow (..)
  , VaultDepositAttributionStateRow (..)
  , VaultRequestRow (..)
  , countVaultRequestsThrough
  , getVaultActivityIndexerState
  , getVaultAttributedHolderSummary
  , getVaultAttributedHolders
  , getVaultDepositAttributionState
  , getVaultRequestIds
  , getVaultRequestsThrough
  )
import Plether.Types (ApiResponse, mkResponse)
import Plether.Types.VaultActivity
  ( VaultActivityCoverage (..)
  , VaultActivityDeploymentIdentity (..)
  , VaultActivityHolder (..)
  , VaultActivityItem (..)
  , VaultActivityResponse (..)
  , VaultActivityTrancheData (..)
  , VaultShareAttributionCoverage (..)
  , VaultRequestIdsResponse (..)
  )

activityLimit :: Int
activityLimit = 250

-- | A missing result means the deployment has never completed its canonical
-- and request-share-attribution backfills and must remain unavailable. Once
-- published, incomplete current coverage is served with stale metadata.
getVaultActivity
  :: DbPool
  -> VaultActivityDeployment
  -> IO (Maybe (ApiResponse VaultActivityResponse))
getVaultActivity pool deployment = do
  now <- floor <$> getPOSIXTime
  withDb pool $ \conn -> withVaultActivityReadSnapshot conn $ do
    mState <- getVaultActivityIndexerState conn deployment
    mAttributionState <- getVaultDepositAttributionState conn deployment
    case (mState, mAttributionState) of
      (Just state, Just attributionState)
        | isPublishable state && isAttributionPublishable attributionState -> do
            let coverage = coverageAt now state attributionState
                confirmedBlock = vacConfirmedThroughBlock coverage
            senior <- trancheData conn confirmedBlock "senior" $ vadSeniorVault deployment
            junior <- trancheData conn confirmedBlock "junior" $ vadJuniorVault deployment
            pure $
              Just $
                mkResponse
                  (vacConfirmedThroughBlock coverage)
                  (vadChainId deployment)
                  VaultActivityResponse
                    { varDeployment = deploymentIdentity deployment
                    , varCoverage = coverage
                    , varSenior = senior
                    , varJunior = junior
                    }
      _ -> pure Nothing
 where
  trancheData conn confirmedBlock tranche vault = do
    holders <- getVaultAttributedHolders conn deployment vault activityLimit
    (holderCount, totalAttributedShares) <- getVaultAttributedHolderSummary conn deployment vault
    requests <- getVaultRequestsThrough conn deployment vault confirmedBlock activityLimit
    requestCount <- countVaultRequestsThrough conn deployment vault confirmedBlock
    pure $
      VaultActivityTrancheData
        { vatHolders = map holderRow holders
        , vatHolderCount = fromIntegral holderCount
        , vatHoldersTruncated = holderCount > fromIntegral activityLimit
        , vatTotalAttributedShares = totalAttributedShares
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
            stale = activityStateStale now state
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
                , vrirStale = stale
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

coverageAt
  :: Integer
  -> VaultActivityIndexerStateRow
  -> VaultDepositAttributionStateRow
  -> VaultActivityCoverage
coverageAt now state attributionState =
  VaultActivityCoverage
    { vacConfirmedThroughBlock = confirmedBlock
    , vacConfirmedThroughHash = confirmedHash
    , vacObservedSafeHeadBlock = vaisSafeHeadBlock state
    , vacObservedSafeHeadHash = vaisSafeHeadBlockHash state
    , vacComplete = complete
    , vacStale = vaultCoverageIsStale lagSeconds pollAge
    , vacLagBlocks = lagBlocks
    , vacLagSeconds = lagSeconds
    , vacLastSuccessfulPoll = min (vaisLastSuccessTimestamp state) (vdasLastSuccessTimestamp attributionState)
    , vacShareAttribution =
        VaultShareAttributionCoverage
          { vsacConfirmedThroughBlock = vdasConfirmedThroughBlock attributionState
          , vsacConfirmedThroughHash = vdasConfirmedThroughBlockHash attributionState
          , vsacComplete = attributionComplete
          , vsacLastSuccessfulPoll = vdasLastSuccessTimestamp attributionState
          }
    }
 where
  confirmedBlock = min (vaisLastIndexedBlock state) (vdasConfirmedThroughBlock attributionState)
  (confirmedHash, confirmedTimestamp)
    | vdasConfirmedThroughBlock attributionState <= vaisLastIndexedBlock state =
        (vdasConfirmedThroughBlockHash attributionState, vdasConfirmedThroughBlockTimestamp attributionState)
    | otherwise = (vaisLastIndexedBlockHash state, vaisLastIndexedBlockTimestamp state)
  attributionComplete = vdasConfirmedThroughBlock attributionState >= vaisSafeHeadBlock state
  complete = vaisLastIndexedBlock state >= vaisSafeHeadBlock state && attributionComplete
  lagBlocks = max 0 $ vaisSafeHeadBlock state - confirmedBlock
  lagSeconds = max 0 $ vaisSafeHeadTimestamp state - confirmedTimestamp
  pollAge = max 0 $ now - min (vaisLastSuccessTimestamp state) (vdasLastSuccessTimestamp attributionState)

-- Brief disagreement between the independently polled activity and attribution
-- cursors is expected. Warn only when confirmed data or the workers are
-- meaningfully late; initial backfills remain gated by the publishable checks.
vaultCoverageIsStale :: Integer -> Integer -> Bool
vaultCoverageIsStale lagSeconds pollAge =
  lagSeconds > 120 || pollAge > 180

holderRow :: VaultAttributedHolderRow -> VaultActivityHolder
holderRow VaultAttributedHolderRow {..} =
  VaultActivityHolder
    vahrAddress
    vahrShareBalance
    vahrUnclaimedDepositShares
    vahrWithdrawalEscrowShares
    vahrTotalAttributedShares

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

isAttributionPublishable :: VaultDepositAttributionStateRow -> Bool
isAttributionPublishable state =
  vdasBackfillComplete state
    && isJust (vdasConfirmedThroughBlockHash state)

activityStateStale :: Integer -> VaultActivityIndexerStateRow -> Bool
activityStateStale now state =
  vaisLastIndexedBlock state < vaisSafeHeadBlock state
    || max 0 (vaisSafeHeadTimestamp state - vaisLastIndexedBlockTimestamp state) > 120
    || max 0 (now - vaisLastSuccessTimestamp state) > 180

withVaultActivityReadSnapshot :: Connection -> IO value -> IO value
withVaultActivityReadSnapshot =
  withTransactionMode $
    TransactionMode
      { isolationLevel = RepeatableRead
      , readWriteMode = ReadOnly
      }
