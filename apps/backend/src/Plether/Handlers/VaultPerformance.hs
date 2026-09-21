module Plether.Handlers.VaultPerformance
  ( getVaultPerformanceHistory
  , buildVaultPerformanceHistoryAt
  , buildVaultAssetHistoryAt
  , carryForwardStaleSnapshots
  , computeVaultPerformance
  , hasCompleteVaultPerformanceCoverageAt
  , latestEligibleVaultPerformanceEpoch
  , vaultPerformancePublicationGraceSeconds
  ) where

import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.Database (DbPool, withDb)
import Plether.Database.VaultPerformance
  ( VaultPerformanceSnapshotRow (..)
  , getVaultPerformanceSnapshots
  )
import Plether.Types
  ( ApiError
  , ApiResponse
  , mkResponse
  )
import Plether.Types.VaultPerformance
  ( VaultPerformanceCoverage (..)
  , VaultPerformanceDeployment (..)
  , VaultPerformanceHistory (..)
  , VaultPerformancePoint (..)
  , VaultPerformanceTranche (..)
  , vaultPerformanceIntervalSeconds
  , vaultPerformancePointCount
  , vaultPerformanceRange
  )

secondsPerYear :: Integer
secondsPerYear = 365 * 24 * 60 * 60

-- Allow the sampler to resolve and publish the canonical block at a freshly
-- crossed hour without making complete history disappear from the UI.
vaultPerformancePublicationGraceSeconds :: Integer
vaultPerformancePublicationGraceSeconds = 120

getVaultPerformanceHistory
  :: DbPool
  -> VaultPerformanceDeployment
  -> Text
  -> IO (Either ApiError (ApiResponse VaultPerformanceHistory))
getVaultPerformanceHistory pool deployment@VaultPerformanceDeployment {..} requestedRange = do
  rows <- withDb pool $ \conn ->
    getVaultPerformanceSnapshots
      conn
      vpdChainId
      vpdHousePool
      vpdSeniorVault
      vpdJuniorVault
      (if requestedRange == "30d" then 721 * 2 else vaultPerformancePointCount * 2)
  now <- floor <$> getPOSIXTime
  let history = (if requestedRange == "30d" then buildVaultAssetHistoryAt else buildVaultPerformanceHistoryAt) now deployment rows
      responseBlock =
        maybe 0 vpsBlockNumber $
          lastMaybe $
            deploymentRowsAt now deployment rows
  pure $ Right $ mkResponse responseBlock vpdChainId history

-- Asset history has its own thirty-day coverage; APY retains its seven-day meaning.
buildVaultAssetHistoryAt
  :: Integer
  -> VaultPerformanceDeployment
  -> [VaultPerformanceSnapshotRow]
  -> VaultPerformanceHistory
buildVaultAssetHistoryAt now deployment rows =
  performance
    { vphRange = "30d"
    , vphCoverage = VaultPerformanceCoverage
        (vpsBlockTimestamp <$> firstMaybe selected)
        (vpsBlockTimestamp <$> lastMaybe selected)
        complete
    , vphSenior = (vphSenior performance) {vptPoints = map seniorPoint selected}
    , vphJunior = (vphJunior performance) {vptPoints = map juniorPoint selected}
    }
 where
  performance = buildVaultPerformanceHistoryAt now deployment rows
  end = latestEligibleVaultPerformanceEpoch now
  selected = filter ((>= end - 30 * 24 * 3600) . vpsEpochTimestamp)
    $ carryForwardStaleSnapshots $ sortOn vpsEpochTimestamp
    $ filter (\row -> matchesDeployment deployment row && vpsEpochTimestamp row <= end) rows
  complete = length selected == 721
    && maybe False ((== end) . vpsEpochTimestamp) (lastMaybe selected)
    && all (\(a, b) -> vpsEpochTimestamp b - vpsEpochTimestamp a == 3600
      && vpsBlockTimestamp b > vpsBlockTimestamp a) (zip selected $ drop 1 selected)
    && all (\row -> vpsMarkFresh row == Just True
      && isJust (vpsSeniorLockedAssets row) && isJust (vpsJuniorLockedAssets row)) selected

buildVaultPerformanceHistoryAt
  :: Integer
  -> VaultPerformanceDeployment
  -> [VaultPerformanceSnapshotRow]
  -> VaultPerformanceHistory
buildVaultPerformanceHistoryAt now deployment rows =
  VaultPerformanceHistory
    { vphRange = vaultPerformanceRange
    , vphIntervalSeconds = vaultPerformanceIntervalSeconds
    , vphDeployment = deployment
    , vphCoverage =
        VaultPerformanceCoverage
          { vpcStart = vpsBlockTimestamp <$> firstMaybe selectedRows
          , vpcEnd = vpsBlockTimestamp <$> lastMaybe selectedRows
          , vpcComplete = coverageComplete
          }
    , vphSenior = tranche seniorPoint publishedSeniorMetrics
    , vphJunior = tranche juniorPoint publishedJuniorMetrics
    }
 where
  selectedRows = deploymentRowsAt now deployment rows
  structuralCoverage =
    hasCompleteVaultPerformanceCoverageAt now deployment selectedRows
  seniorMetrics = metricsFor structuralCoverage vpsSeniorSharePriceWad selectedRows
  juniorMetrics = metricsFor structuralCoverage vpsJuniorSharePriceWad selectedRows
  coverageComplete = structuralCoverage && isJust seniorMetrics && isJust juniorMetrics
  publishedSeniorMetrics = if coverageComplete then seniorMetrics else Nothing
  publishedJuniorMetrics = if coverageComplete then juniorMetrics else Nothing

  tranche toPoint metrics =
    VaultPerformanceTranche
      { vptApy7d = snd <$> metrics
      , vptReturn7d = fst <$> metrics
      , vptPoints = map toPoint selectedRows
      }

-- | Keep the current deployment isolated even if a caller accidentally passes
-- rows for another release, and cap the response to the latest seven-day
-- window. The database query applies the same identity predicate; retaining it
-- here makes series-splicing impossible at the response boundary as well.
deploymentRowsAt
  :: Integer
  -> VaultPerformanceDeployment
  -> [VaultPerformanceSnapshotRow]
  -> [VaultPerformanceSnapshotRow]
deploymentRowsAt now deployment =
  keepLatest vaultPerformancePointCount
    . carryForwardStaleSnapshots
    . sortOn vpsEpochTimestamp
    . filter ((<= latestEligibleVaultPerformanceEpoch now) . vpsEpochTimestamp)
    . filter (matchesDeployment deployment)

-- | Stale HousePool previews intentionally omit mark-dependent PnL. Preserve
-- their hourly timestamp and freshness status, but publish the last coherent
-- fresh valuation instead of turning that safety fallback into a chart move.
-- Leading stale rows without a known fresh predecessor are omitted.
carryForwardStaleSnapshots
  :: [VaultPerformanceSnapshotRow]
  -> [VaultPerformanceSnapshotRow]
carryForwardStaleSnapshots = go Nothing
 where
  go _ [] = []
  go previousFresh (row : rows) =
    case vpsMarkFresh row of
      Just True -> row : go (Just row) rows
      Just False ->
        case previousFresh of
          Just fresh -> carryValuation fresh row : go previousFresh rows
          Nothing -> go Nothing rows
      Nothing -> go previousFresh rows

  carryValuation fresh stale =
    stale
      { vpsSeniorTotalAssets = vpsSeniorTotalAssets fresh
      , vpsSeniorTotalSupply = vpsSeniorTotalSupply fresh
      , vpsSeniorSharePriceWad = vpsSeniorSharePriceWad fresh
      , vpsJuniorTotalAssets = vpsJuniorTotalAssets fresh
      , vpsJuniorTotalSupply = vpsJuniorTotalSupply fresh
      , vpsJuniorSharePriceWad = vpsJuniorSharePriceWad fresh
      }

matchesDeployment :: VaultPerformanceDeployment -> VaultPerformanceSnapshotRow -> Bool
matchesDeployment VaultPerformanceDeployment {..} VaultPerformanceSnapshotRow {..} =
  vpsChainId == vpdChainId
    && sameAddress vpsHousePoolAddress vpdHousePool
    && sameAddress vpsSeniorVaultAddress vpdSeniorVault
    && sameAddress vpsJuniorVaultAddress vpdJuniorVault

sameAddress :: Text -> Text -> Bool
sameAddress left right = T.toLower left == T.toLower right

keepLatest :: Int -> [a] -> [a]
keepLatest count values = drop (max 0 $ length values - count) values

hasCompleteVaultPerformanceCoverageAt
  :: Integer
  -> VaultPerformanceDeployment
  -> [VaultPerformanceSnapshotRow]
  -> Bool
hasCompleteVaultPerformanceCoverageAt now deployment rows =
  length rows == vaultPerformancePointCount
    && all (matchesDeployment deployment) rows
    && all validSnapshot rows
    && all consecutive (zip rows $ drop 1 rows)
    && maybe False ((> 0) . vpsSeniorSharePriceWad) (firstMaybe rows)
    && maybe False ((> 0) . vpsJuniorSharePriceWad) (firstMaybe rows)
    && maybe False
      ((== latestEligibleVaultPerformanceEpoch now) . vpsEpochTimestamp)
      (lastMaybe rows)
 where
  validSnapshot VaultPerformanceSnapshotRow {..} =
    vpsEpochTimestamp >= 0
      && vpsEpochTimestamp `mod` vaultPerformanceIntervalSeconds == 0
      && vpsBlockTimestamp >= 0
      && vpsBlockTimestamp <= vpsEpochTimestamp
      && vpsBlockNumber >= 0
      && not (T.null vpsBlockHash)
      && isJust vpsMarkFresh
      && vpsSeniorTotalAssets >= 0
      && vpsSeniorTotalSupply > 0
      && vpsSeniorSharePriceWad >= 0
      && vpsJuniorTotalAssets >= 0
      && vpsJuniorTotalSupply > 0
      && vpsJuniorSharePriceWad >= 0

  consecutive (previous, current) =
    vpsEpochTimestamp current - vpsEpochTimestamp previous
      == vaultPerformanceIntervalSeconds
      && vpsBlockTimestamp current > vpsBlockTimestamp previous
      && vpsBlockNumber current >= vpsBlockNumber previous

latestEligibleVaultPerformanceEpoch :: Integer -> Integer
latestEligibleVaultPerformanceEpoch now
  | now <= vaultPerformancePublicationGraceSeconds = 0
  | otherwise =
      ((now - vaultPerformancePublicationGraceSeconds) `div` vaultPerformanceIntervalSeconds)
        * vaultPerformanceIntervalSeconds

metricsFor
  :: Bool
  -> (VaultPerformanceSnapshotRow -> Integer)
  -> [VaultPerformanceSnapshotRow]
  -> Maybe (Double, Double)
metricsFor False _ _ = Nothing
metricsFor True price rows = do
  firstRow <- firstMaybe rows
  lastRow <- lastMaybe rows
  computeVaultPerformance
    (price firstRow)
    (price lastRow)
    (vpsBlockTimestamp lastRow - vpsBlockTimestamp firstRow)

-- | Return the realized period return and its compounded annualization. Prices
-- share the same fixed-point scale, so it cancels from the ratio. A zero ending
-- price is a valid total loss; a zero starting price cannot define a return.
computeVaultPerformance
  :: Integer
  -> Integer
  -> Integer
  -> Maybe (Double, Double)
computeVaultPerformance startPrice endPrice elapsedSeconds
  | startPrice <= 0 = Nothing
  | endPrice < 0 = Nothing
  | elapsedSeconds <= 0 = Nothing
  | endPrice == 0 = Just (-1, -1)
  | otherwise =
      let ratio = fromRational (endPrice % startPrice) :: Double
          periodReturn = ratio - 1
          annualizationExponent = fromRational (secondsPerYear % elapsedSeconds) :: Double
          annualizedReturn = exp (log ratio * annualizationExponent) - 1
       in if all isFinite [periodReturn, annualizedReturn]
            then Just (normalizeZero periodReturn, normalizeZero annualizedReturn)
            else Nothing
 where
  isFinite value = not $ isNaN value || isInfinite value
  normalizeZero value
    | abs value < 1e-15 = 0
    | otherwise = value

seniorPoint :: VaultPerformanceSnapshotRow -> VaultPerformancePoint
seniorPoint VaultPerformanceSnapshotRow {..} =
  VaultPerformancePoint
    { vppTimestamp = vpsBlockTimestamp
    , vppBlockNumber = vpsBlockNumber
    , vppMarkFresh = vpsMarkFresh == Just True
    , vppSharePrice = vpsSeniorSharePriceWad
    , vppTotalAssets = vpsSeniorTotalAssets
    , vppLockedAssets = if vpsMarkFresh == Just True then vpsSeniorLockedAssets else Nothing
    , vppTotalSupply = vpsSeniorTotalSupply
    }

juniorPoint :: VaultPerformanceSnapshotRow -> VaultPerformancePoint
juniorPoint VaultPerformanceSnapshotRow {..} =
  VaultPerformancePoint
    { vppTimestamp = vpsBlockTimestamp
    , vppBlockNumber = vpsBlockNumber
    , vppMarkFresh = vpsMarkFresh == Just True
    , vppSharePrice = vpsJuniorSharePriceWad
    , vppTotalAssets = vpsJuniorTotalAssets
    , vppLockedAssets = if vpsMarkFresh == Just True then vpsJuniorLockedAssets else Nothing
    , vppTotalSupply = vpsJuniorTotalSupply
    }

firstMaybe :: [a] -> Maybe a
firstMaybe [] = Nothing
firstMaybe (value : _) = Just value

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe values = Just $ last values
