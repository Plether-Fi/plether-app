module Plether.Perps.CandleFinalizerProbeSpec (spec) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.Text (Text)
import Plether.Database.Candles
  ( RollupCoverage (..)
  , RollupKind (..)
  , defaultBasketSeriesId
  )
import Plether.Perps.CandleFinalizerProbe
  ( FinalizerProbeEnvironment (..)
  , FinalizerProbePlan (..)
  , finalizerProbeRecovered
  , planFinalizerProbe
  , validateFinalizerDatabaseUrl
  , validateFinalizerLibpqEnvironment
  , validateFinalizerProbeEnvironment
  , validateFinalizerProbePrestate
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "validateFinalizerDatabaseUrl" $ do
    it "accepts only the canonical URL for the validated Sepolia RDS host" $ do
      validateFinalizerDatabaseUrl sepoliaDatabaseHost sepoliaDatabaseUrl
        `shouldBe` Right ()
      forM_
        [ "postgresql://plether:secret@plether-mainnet.abc.ap-southeast-1.rds.amazonaws.com:5432/plether"
        , "postgresql://plether:secret@plether-sepolia.abc.ap-southeast-1.rds.amazonaws.com:5432/other"
        , "postgresql://plether:secret@plether-sepolia.abc.ap-southeast-1.rds.amazonaws.com:5432/plether?hostaddr=10.0.0.1"
        , "postgresql://plether:secret@plether-sepolia.abc.ap-southeast-1.rds.amazonaws.com/plether"
        , canonicalDatabasePrefix <> "?sslmode=require&sslrootcert=%2Fetc%2Fssl%2Fcerts%2Faws-rds-global-bundle.pem"
        , canonicalDatabasePrefix <> "?sslmode=verify-full"
        , canonicalDatabasePrefix <> "?sslmode=verify-full&sslrootcert=%2Fwrong.pem"
        , canonicalDatabasePrefix <> "?sslrootcert=%2Fetc%2Fssl%2Fcerts%2Faws-rds-global-bundle.pem&sslmode=verify-full"
        , sepoliaDatabaseUrl <> "&sslmode=verify-full"
        , sepoliaDatabaseUrl <> "#fragment"
        ] $ \databaseUrl ->
          validateFinalizerDatabaseUrl sepoliaDatabaseHost databaseUrl
            `shouldSatisfy` isLeft

    it "rejects an expected host that is not the named Sepolia RDS endpoint" $
      validateFinalizerDatabaseUrl
        "plether-mainnet.abc.ap-southeast-1.rds.amazonaws.com"
        sepoliaDatabaseUrl
        `shouldSatisfy` isLeft

  describe "validateFinalizerLibpqEnvironment" $ do
    it "accepts ordinary process variables but rejects every ambient PG override" $ do
      validateFinalizerLibpqEnvironment
        ["DATABASE_URL", "DEPLOYMENT_ENVIRONMENT", "EXPECTED_DATABASE_HOST"]
        `shouldBe` Right ()
      forM_ ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE", "PGOPTIONS"] $ \name ->
        validateFinalizerLibpqEnvironment ["DATABASE_URL", name]
          `shouldSatisfy` isLeft

  describe "planFinalizerProbe" $ do
    it "derives the fixed hourly lock, grace, release, and recovery boundaries" $ do
      let acquireAt = boundary + 105
      planFinalizerProbe (acquireAt - 300) boundary 2_100
        `shouldBe` Right
          FinalizerProbePlan
            { fppBoundary = boundary
            , fppAcquireAt = boundary + 105
            , fppGraceExpiresAt = boundary + 135
            , fppReleaseAt = boundary + 150
            , fppRecoveryDeadline = boundary + 165
            }

    it "rejects unaligned, late, and excessively early launches" $ do
      let acquireAt = boundary + 105
      planFinalizerProbe (acquireAt - 300) (boundary + 1) 2_100
        `shouldSatisfy` isLeft
      planFinalizerProbe (acquireAt - 4) boundary 2_100
        `shouldSatisfy` isLeft
      planFinalizerProbe (acquireAt - 1_801) boundary 2_100
        `shouldSatisfy` isLeft

    it "requires a bounded runtime covering recovery plus cleanup headroom" $ do
      let acquireAt = boundary + 105
          databaseTimestamp = acquireAt - 5
      planFinalizerProbe databaseTimestamp boundary 79 `shouldSatisfy` isLeft
      planFinalizerProbe databaseTimestamp boundary 80 `shouldSatisfy` either (const False) (const True)
      planFinalizerProbe (acquireAt - 300) boundary 2_101 `shouldSatisfy` isLeft

  describe "validateFinalizerProbeEnvironment" $ do
    it "accepts only the exact supervised Sepolia hourly canary configuration" $
      validateFinalizerProbeEnvironment validEnvironment `shouldBe` Right ()

    it "rejects every safety-critical environment mismatch" $
      forM_
        [ validEnvironment {fpeDeploymentEnvironment = "mainnet"}
        , validEnvironment {fpeChainId = 1}
        , validEnvironment {fpeWriteMode = "off"}
        , validEnvironment {fpeReadMode = "legacy"}
        , validEnvironment {fpeReadIntervals = "3600,86400"}
        , validEnvironment {fpeStrictCoverage = "false"}
        , validEnvironment {fpeLatenessSeconds = 121}
        , validEnvironment {fpeFinalizationGraceSeconds = 14}
        ] $ \environment ->
          validateFinalizerProbeEnvironment environment `shouldSatisfy` isLeft

  describe "validateFinalizerProbePrestate" $ do
    it "accepts complete error-free hourly coverage immediately before finalization" $
      validateFinalizerProbePrestate boundary testChainId testRouter pricePrestate volumePrestate
        `shouldBe` Right ()

    it "rejects incomplete, erroneous, maintained, stale, or wrong-identity coverage" $ do
      forM_
        [ pricePrestate {rcComplete = False}
        , pricePrestate {rcLastError = Just "price_watermark_gap"}
        , pricePrestate {rcMaintenanceFrom = Just $ boundary - 86_400}
        , pricePrestate {rcCoverageEnd = Just $ boundary - 1}
        , pricePrestate {rcFinalizedThrough = Just boundary}
        , pricePrestate {rcGeneration = 0}
        , pricePrestate {rcSeriesId = Just "unexpected-series"}
        ] $ \invalidPrice ->
          validateFinalizerProbePrestate boundary testChainId testRouter invalidPrice volumePrestate
            `shouldSatisfy` isLeft
      forM_
        [ volumePrestate {rcComplete = False}
        , volumePrestate {rcLastError = Just "chain_reorg"}
        , volumePrestate {rcMaintenanceTo = Just boundary}
        , volumePrestate {rcCoverageEnd = Nothing}
        , volumePrestate {rcFinalizedThrough = Just $ boundary - 3_600}
        , volumePrestate {rcChainId = Just 1}
        ] $ \invalidVolume ->
          validateFinalizerProbePrestate boundary testChainId testRouter pricePrestate invalidVolume
            `shouldSatisfy` isLeft

  describe "finalizerProbeRecovered" $ do
    it "requires price recovery and healthy volume without changing either generation" $ do
      finalizerProbeRecovered
        boundary testChainId testRouter 11 12 pricePrestate volumePrestate
        `shouldBe` False
      finalizerProbeRecovered
        boundary testChainId testRouter 11 12 priceRecovered volumeRecovered
        `shouldBe` True
      finalizerProbeRecovered
        boundary testChainId testRouter 11 12
        priceRecovered {rcGeneration = 13}
        volumeRecovered
        `shouldBe` False
      finalizerProbeRecovered
        boundary testChainId testRouter 11 12
        priceRecovered
        volumeRecovered {rcLastError = Just "chain_reorg"}
        `shouldBe` False
      finalizerProbeRecovered
        boundary testChainId testRouter 11 12
        priceRecovered {rcCoverageStart = Just boundary}
        volumeRecovered
        `shouldBe` False

validEnvironment :: FinalizerProbeEnvironment
validEnvironment =
  FinalizerProbeEnvironment
    { fpeDeploymentEnvironment = "sepolia"
    , fpeChainId = testChainId
    , fpeWriteMode = "dual"
    , fpeReadMode = "rollup"
    , fpeReadIntervals = "3600"
    , fpeStrictCoverage = "true"
    , fpeLatenessSeconds = 120
    , fpeFinalizationGraceSeconds = 15
    }

boundary :: Integer
boundary = 3_600_000

testChainId :: Integer
testChainId = 421_614

testRouter :: Text
testRouter = "0xabc"

sepoliaDatabaseHost :: Text
sepoliaDatabaseHost = "plether-sepolia.abc.ap-southeast-1.rds.amazonaws.com"

sepoliaDatabaseUrl :: Text
sepoliaDatabaseUrl =
  canonicalDatabasePrefix
    <> "?sslmode=verify-full&sslrootcert=%2Fetc%2Fssl%2Fcerts%2Faws-rds-global-bundle.pem"

canonicalDatabasePrefix :: Text
canonicalDatabasePrefix =
  "postgresql://plether:secret@plether-sepolia.abc.ap-southeast-1.rds.amazonaws.com:5432/plether"

pricePrestate :: RollupCoverage
pricePrestate =
  RollupCoverage
    { rcKind = PriceRollup
    , rcSeriesId = Just defaultBasketSeriesId
    , rcChainId = Nothing
    , rcReleaseRouter = Nothing
    , rcIntervalSeconds = 3_600
    , rcCoverageStart = Just $ boundary - 86_400
    , rcCoverageEnd = Just boundary
    , rcFinalizedThrough = Just $ boundary - 3_600
    , rcGeneration = 11
    , rcComplete = True
    , rcDerivationVersion = "v1"
    , rcLastError = Nothing
    , rcMaintenanceFrom = Nothing
    , rcMaintenanceTo = Nothing
    }

volumePrestate :: RollupCoverage
volumePrestate =
  pricePrestate
    { rcKind = VolumeRollup
    , rcSeriesId = Nothing
    , rcChainId = Just testChainId
    , rcReleaseRouter = Just "0xabc"
    , rcFinalizedThrough = Just boundary
    , rcGeneration = 12
    }

priceRecovered :: RollupCoverage
priceRecovered =
  pricePrestate
    { rcCoverageEnd = Just boundary
    , rcFinalizedThrough = Just boundary
    }

volumeRecovered :: RollupCoverage
volumeRecovered =
  volumePrestate
    { rcCoverageEnd = Just boundary
    , rcFinalizedThrough = Just boundary
    }
