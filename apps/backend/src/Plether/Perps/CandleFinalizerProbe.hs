module Plether.Perps.CandleFinalizerProbe
  ( FinalizerProbeEnvironment (..)
  , FinalizerProbePlan (..)
  , finalizerProbeRecovered
  , planFinalizerProbe
  , validateFinalizerDatabaseUrl
  , validateFinalizerLibpqEnvironment
  , validateFinalizerProbeEnvironment
  , validateFinalizerProbePrestate
  ) where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Database.Candles
  ( RollupCoverage (..)
  , RollupKind (..)
  , defaultBasketSeriesId
  )

data FinalizerProbeEnvironment = FinalizerProbeEnvironment
  { fpeDeploymentEnvironment :: Text
  , fpeChainId :: Integer
  , fpeWriteMode :: Text
  , fpeReadMode :: Text
  , fpeReadIntervals :: Text
  , fpeStrictCoverage :: Text
  , fpeLatenessSeconds :: Integer
  , fpeFinalizationGraceSeconds :: Integer
  }
  deriving stock (Eq, Show)

data FinalizerProbePlan = FinalizerProbePlan
  { fppBoundary :: Integer
  , fppAcquireAt :: Integer
  , fppGraceExpiresAt :: Integer
  , fppReleaseAt :: Integer
  , fppRecoveryDeadline :: Integer
  }
  deriving stock (Eq, Show)

hourlyInterval :: Integer
hourlyInterval = 3_600

probeLatenessSeconds :: Integer
probeLatenessSeconds = 120

probeGraceSeconds :: Integer
probeGraceSeconds = 15

probeAcquireLeadSeconds :: Integer
probeAcquireLeadSeconds = 15

probeReleaseBufferSeconds :: Integer
probeReleaseBufferSeconds = 15

probeRecoverySeconds :: Integer
probeRecoverySeconds = 15

maximumLaunchLeadSeconds :: Integer
maximumLaunchLeadSeconds = 1_800

maximumRuntimeSeconds :: Int
maximumRuntimeSeconds = 2_100

validateFinalizerDatabaseUrl :: Text -> Text -> Either Text ()
validateFinalizerDatabaseUrl expectedHost databaseUrl = do
  unless (validExpectedHost normalizedExpectedHost) $
    Left "Finalizer probe expected database host is not a Sepolia RDS endpoint"
  uriBody <-
    maybe
      (Left "Finalizer probe DATABASE_URL must use the canonical postgresql URI")
      Right
      (T.stripPrefix "postgresql://" $ T.strip databaseUrl)
  unless (not $ T.any (`elem` ['?', '#']) uriBody) $
    Left "Finalizer probe DATABASE_URL must not contain URI overrides"
  let (authority, databasePath) = T.breakOn "/" uriBody
  unless (databasePath == "/plether") $
    Left "Finalizer probe DATABASE_URL must select the plether database exactly"
  hostPort <-
    case reverse $ T.splitOn "@" authority of
      candidate : _ : _ -> Right candidate
      _ -> Left "Finalizer probe DATABASE_URL must contain canonical credentials and host"
  actualHost <-
    case T.splitOn ":" hostPort of
      [host, "5432"] | not (T.null host) -> Right $ T.toLower host
      _ -> Left "Finalizer probe DATABASE_URL must use the canonical RDS host and port"
  unless (actualHost == normalizedExpectedHost) $
    Left "Finalizer probe DATABASE_URL does not target the validated Sepolia RDS endpoint"
 where
  normalizedExpectedHost = T.toLower $ T.strip expectedHost
  validExpectedHost host =
    "plether-sepolia." `T.isPrefixOf` host
      && ".rds.amazonaws.com" `T.isSuffixOf` host
      && T.all (\character -> character == '.' || character == '-' || isAsciiLower character || isDigit character) host
  isAsciiLower character = character >= 'a' && character <= 'z'
  isDigit character = character >= '0' && character <= '9'

validateFinalizerLibpqEnvironment :: [Text] -> Either Text ()
validateFinalizerLibpqEnvironment variableNames =
  unless (all (not . T.isPrefixOf "PG") variableNames) $
    Left "Finalizer probe forbids ambient libpq PG* connection variables"

planFinalizerProbe :: Integer -> Integer -> Int -> Either Text FinalizerProbePlan
planFinalizerProbe databaseTimestamp boundary maxRuntimeSeconds
  | boundary <= hourlyInterval || boundary `mod` hourlyInterval /= 0 =
      Left "Finalizer probe boundary must be a positive aligned UTC hour"
  | launchLead < 5 =
      Left "Finalizer probe started too late to acquire the writer lock safely"
  | launchLead > maximumLaunchLeadSeconds =
      Left "Finalizer probe boundary is more than 30 minutes from lock acquisition"
  | maxRuntimeSeconds > maximumRuntimeSeconds =
      Left "Finalizer probe maximum runtime cannot exceed 2100 seconds"
  | fromIntegral maxRuntimeSeconds < requiredRuntime =
      Left "Finalizer probe maximum runtime does not cover its recovery deadline"
  | otherwise = Right plan
 where
  acquireAt = boundary + probeLatenessSeconds - probeAcquireLeadSeconds
  graceExpiresAt = boundary + probeLatenessSeconds + probeGraceSeconds
  releaseAt = graceExpiresAt + probeReleaseBufferSeconds
  recoveryDeadline = releaseAt + probeRecoverySeconds
  launchLead = acquireAt - databaseTimestamp
  requiredRuntime = recoveryDeadline - databaseTimestamp + probeGraceSeconds
  plan =
    FinalizerProbePlan
      { fppBoundary = boundary
      , fppAcquireAt = acquireAt
      , fppGraceExpiresAt = graceExpiresAt
      , fppReleaseAt = releaseAt
      , fppRecoveryDeadline = recoveryDeadline
      }

validateFinalizerProbeEnvironment :: FinalizerProbeEnvironment -> Either Text ()
validateFinalizerProbeEnvironment FinalizerProbeEnvironment {..}
  | normalized fpeDeploymentEnvironment /= "sepolia" =
      Left "Finalizer probe is restricted to DEPLOYMENT_ENVIRONMENT=sepolia"
  | fpeChainId /= 421_614 =
      Left "Finalizer probe is restricted to PERPS_CHAIN_ID=421614"
  | normalized fpeWriteMode /= "dual" =
      Left "Finalizer probe requires PERPS_CANDLE_WRITE_MODE=dual"
  | normalized fpeReadMode /= "rollup" =
      Left "Finalizer probe requires PERPS_CANDLE_READ_MODE=rollup"
  | T.strip fpeReadIntervals /= "3600" =
      Left "Finalizer probe requires PERPS_CANDLE_READ_INTERVALS=3600 exactly"
  | normalized fpeStrictCoverage /= "true" =
      Left "Finalizer probe requires PERPS_CANDLE_STRICT_COVERAGE=true"
  | fpeLatenessSeconds /= probeLatenessSeconds =
      Left "Finalizer probe requires PERPS_CANDLE_LATENESS_SECONDS=120"
  | fpeFinalizationGraceSeconds /= probeGraceSeconds =
      Left "Finalizer probe requires PERPS_CANDLE_FINALIZATION_GRACE_SECONDS=15"
  | otherwise = Right ()
 where
  normalized = T.toLower . T.strip

validateFinalizerProbePrestate
  :: Integer
  -> Integer
  -> Text
  -> RollupCoverage
  -> RollupCoverage
  -> Either Text ()
validateFinalizerProbePrestate boundary chainId releaseRouter price volume = do
  validateIdentity PriceRollup (Just defaultBasketSeriesId) Nothing Nothing price
  validateIdentity VolumeRollup Nothing (Just chainId) (Just normalizedRouter) volume
  validateCoverage "price" price
  validateCoverage "volume" volume
  unless (rcFinalizedThrough price == Just expectedPriceFinalized) $
    Left "price hourly finalized watermark is not the expected pre-boundary value"
  unless (maybe False (>= boundary) $ rcFinalizedThrough volume) $
    Left "volume hourly finalized watermark has not reached the probe boundary"
 where
  normalizedRouter = T.toLower $ T.strip releaseRouter
  expectedPriceFinalized = boundary - hourlyInterval

  validateCoverage label coverage
    | not $ rcComplete coverage = Left $ label <> " hourly coverage is incomplete"
    | rcDerivationVersion coverage /= "v1" = Left $ label <> " hourly derivation is not v1"
    | rcLastError coverage /= Nothing = Left $ label <> " hourly coverage has a durable error"
    | rcMaintenanceFrom coverage /= Nothing || rcMaintenanceTo coverage /= Nothing =
        Left $ label <> " hourly coverage is under maintenance"
    | maybe True (> expectedPriceFinalized) (rcCoverageStart coverage) =
        Left $ label <> " hourly coverage does not include the closing bucket"
    | maybe True (< boundary) (rcCoverageEnd coverage) =
        Left $ label <> " hourly coverage has not reached the probe boundary"
    | rcGeneration coverage <= 0 = Left $ label <> " hourly generation is invalid"
    | otherwise = Right ()

finalizerProbeRecovered
  :: Integer
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> RollupCoverage
  -> RollupCoverage
  -> Bool
finalizerProbeRecovered boundary chainId releaseRouter priceGeneration volumeGeneration price volume =
  recoveredIdentity PriceRollup (Just defaultBasketSeriesId) Nothing Nothing price
    && recoveredIdentity VolumeRollup Nothing (Just chainId) (Just normalizedRouter) volume
    && recovered priceGeneration price
    && recovered volumeGeneration volume
 where
  normalizedRouter = T.toLower $ T.strip releaseRouter
  recovered generation coverage =
    rcComplete coverage
      && rcDerivationVersion coverage == "v1"
      && rcLastError coverage == Nothing
      && rcMaintenanceFrom coverage == Nothing
      && rcMaintenanceTo coverage == Nothing
      && maybe False (<= boundary - hourlyInterval) (rcCoverageStart coverage)
      && maybe False (>= boundary) (rcCoverageEnd coverage)
      && maybe False (>= boundary) (rcFinalizedThrough coverage)
      && rcGeneration coverage == generation

validateIdentity
  :: RollupKind
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Text
  -> RollupCoverage
  -> Either Text ()
validateIdentity kind seriesId chainId releaseRouter coverage
  | recoveredIdentity kind seriesId chainId releaseRouter coverage = Right ()
  | otherwise = Left "Finalizer probe coverage identity does not match the protected dataset"

recoveredIdentity
  :: RollupKind
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Text
  -> RollupCoverage
  -> Bool
recoveredIdentity kind seriesId chainId releaseRouter coverage =
  rcKind coverage == kind
    && rcSeriesId coverage == seriesId
    && rcChainId coverage == chainId
    && rcReleaseRouter coverage == releaseRouter
    && rcIntervalSeconds coverage == hourlyInterval
