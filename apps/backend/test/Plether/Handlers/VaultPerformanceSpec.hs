module Plether.Handlers.VaultPerformanceSpec (spec) where

import Data.Aeson (encode, object, toJSON, (.=))
import Plether.Database.VaultPerformance (VaultPerformanceSnapshotRow (..))
import Plether.Handlers.VaultPerformance
  ( buildVaultPerformanceHistoryAt
  , computeVaultPerformance
  , hasCompleteVaultPerformanceCoverageAt
  , latestEligibleVaultPerformanceEpoch
  , vaultPerformancePublicationGraceSeconds
  )
import Plether.Types.VaultPerformance
  ( VaultPerformanceCoverage (..)
  , VaultPerformanceDeployment (..)
  , VaultPerformanceHistory (..)
  , VaultPerformancePoint (..)
  , VaultPerformanceTranche (..)
  , isCanonicalVaultPerformanceRequest
  , vaultPerformanceIntervalSeconds
  , vaultPerformancePointCount
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "vault performance request shape" $ do
    it "accepts only the canonical cacheable query" $ do
      isCanonicalVaultPerformanceRequest ["range", "interval"] (Just "7d") (Just "3600")
        `shouldBe` True
      isCanonicalVaultPerformanceRequest ["interval", "range"] (Just "7d") (Just "3600")
        `shouldBe` True

    it "rejects missing, duplicate, extra, or alternative query values" $ do
      isCanonicalVaultPerformanceRequest ["range"] (Just "7d") Nothing `shouldBe` False
      isCanonicalVaultPerformanceRequest ["range", "range", "interval"] (Just "7d") (Just "3600")
        `shouldBe` False
      isCanonicalVaultPerformanceRequest ["range", "interval", "cursor"] (Just "7d") (Just "3600")
        `shouldBe` False
      isCanonicalVaultPerformanceRequest ["range", "interval"] (Just "7D") (Just "3600")
        `shouldBe` False
      isCanonicalVaultPerformanceRequest ["range", "interval"] (Just "7d") (Just "03600")
        `shouldBe` False

  describe "computeVaultPerformance" $ do
    it "annualizes a positive realized return using actual elapsed seconds" $ do
      let elapsed = 7 * 24 * 60 * 60
          expectedApy = (1.01 :: Double) ** (365 / 7) - 1
      case computeVaultPerformance 10_000 10_100 elapsed of
        Just (periodReturn, apy) -> do
          periodReturn `shouldApprox` 0.01
          apy `shouldApprox` expectedApy
        Nothing -> expectationFailure "expected positive performance"

    it "uses the sampled block elapsed time rather than assuming seven days" $ do
      let elapsed = 7 * 24 * 60 * 60 - 120
          expectedApy = (1.02 :: Double) ** (31_536_000 / fromIntegral elapsed) - 1
      case computeVaultPerformance 10_000 10_200 elapsed of
        Just (_, apy) -> apy `shouldApprox` expectedApy
        Nothing -> expectationFailure "expected performance for a positive elapsed time"

    it "supports negative and flat returns" $ do
      case computeVaultPerformance 10_000 9_000 (7 * 24 * 60 * 60) of
        Just (periodReturn, apy) -> do
          periodReturn `shouldApprox` (-0.1)
          apy `shouldSatisfy` (< (-0.99))
        Nothing -> expectationFailure "expected negative performance"
      computeVaultPerformance 10_000 10_000 (7 * 24 * 60 * 60)
        `shouldBe` Just (0, 0)

    it "represents a total loss as minus one for both metrics" $ do
      computeVaultPerformance 10_000 0 (7 * 24 * 60 * 60)
        `shouldBe` Just (-1, -1)

    it "rejects an undefined starting price or elapsed time" $ do
      computeVaultPerformance 0 10_000 (7 * 24 * 60 * 60) `shouldBe` Nothing
      computeVaultPerformance 10_000 10_000 0 `shouldBe` Nothing

  describe "strict vault performance coverage" $ do
    it "requires all 169 consecutive hourly checkpoints" $ do
      hasCompleteVaultPerformanceCoverageAt freshNow deployment completeRows `shouldBe` True
      hasCompleteVaultPerformanceCoverageAt freshNow deployment (drop 1 completeRows) `shouldBe` False
      hasCompleteVaultPerformanceCoverageAt freshNow deployment (removeAt 80 completeRows) `shouldBe` False

    it "requires the final checkpoint to match the latest eligible epoch" $ do
      latestEligibleVaultPerformanceEpoch freshNow `shouldBe` vpsEpochTimestamp (last completeRows)
      hasCompleteVaultPerformanceCoverageAt
        (freshNow + vaultPerformanceIntervalSeconds)
        deployment
        completeRows
        `shouldBe` False
      let history =
            buildVaultPerformanceHistoryAt
              (freshNow + vaultPerformanceIntervalSeconds)
              deployment
              completeRows
      vpcComplete (vphCoverage history) `shouldBe` False
      vptApy7d (vphSenior history) `shouldBe` Nothing

    it "keeps the prior epoch eligible for a two-minute publication grace" $ do
      let nextBoundary = vpsEpochTimestamp (last completeRows) + vaultPerformanceIntervalSeconds
      latestEligibleVaultPerformanceEpoch nextBoundary
        `shouldBe` vpsEpochTimestamp (last completeRows)
      latestEligibleVaultPerformanceEpoch (nextBoundary + vaultPerformancePublicationGraceSeconds - 1)
        `shouldBe` vpsEpochTimestamp (last completeRows)
      latestEligibleVaultPerformanceEpoch (nextBoundary + vaultPerformancePublicationGraceSeconds)
        `shouldBe` nextBoundary
      hasCompleteVaultPerformanceCoverageAt nextBoundary deployment completeRows `shouldBe` True
      hasCompleteVaultPerformanceCoverageAt
        (nextBoundary + vaultPerformancePublicationGraceSeconds - 1)
        deployment
        completeRows
        `shouldBe` True
      hasCompleteVaultPerformanceCoverageAt
        (nextBoundary + vaultPerformancePublicationGraceSeconds)
        deployment
        completeRows
        `shouldBe` False

    it "does not expose a newly published epoch before its grace expires" $ do
      let nextRow = snapshot vaultPerformancePointCount
          rowsWithNextEpoch = completeRows <> [nextRow]
          nextBoundary = vpsEpochTimestamp nextRow
          duringGrace =
            buildVaultPerformanceHistoryAt
              (nextBoundary + vaultPerformancePublicationGraceSeconds - 1)
              deployment
              rowsWithNextEpoch
          afterGrace =
            buildVaultPerformanceHistoryAt
              (nextBoundary + vaultPerformancePublicationGraceSeconds)
              deployment
              rowsWithNextEpoch
      vpcComplete (vphCoverage duringGrace) `shouldBe` True
      map vppTimestamp (vptPoints $ vphSenior duringGrace)
        `shouldBe` map vpsBlockTimestamp completeRows
      vpcComplete (vphCoverage afterGrace) `shouldBe` True
      map vppTimestamp (vptPoints $ vphSenior afterGrace)
        `shouldBe` map vpsBlockTimestamp (drop 1 rowsWithNextEpoch)

    it "requires valid nonzero supplies for both tranches" $ do
      let invalid = updateAt 42 (\row -> row {vpsJuniorTotalSupply = 0}) completeRows
      hasCompleteVaultPerformanceCoverageAt freshNow deployment invalid `shouldBe` False

    it "allows a zero ending price but rejects a zero starting price" $ do
      let totalLoss = updateAt 168 (\row -> row {vpsJuniorSharePriceWad = 0}) completeRows
          undefinedStart = updateAt 0 (\row -> row {vpsJuniorSharePriceWad = 0}) completeRows
      hasCompleteVaultPerformanceCoverageAt freshNow deployment totalLoss `shouldBe` True
      hasCompleteVaultPerformanceCoverageAt freshNow deployment undefinedStart `shouldBe` False

    it "does not splice checkpoints from another deployment" $ do
      let oldRows =
            map
              (\row -> row {vpsHousePoolAddress = "0xold-house-pool"})
              (take 84 completeRows)
          currentRows = drop 84 completeRows
          history = buildVaultPerformanceHistoryAt freshNow deployment (oldRows <> currentRows)
      vpcComplete (vphCoverage history) `shouldBe` False
      length (vptPoints $ vphSenior history) `shouldBe` 85

  describe "vault performance response" $ do
    it "sorts points chronologically and publishes metrics only when complete" $ do
      let history = buildVaultPerformanceHistoryAt freshNow deployment (reverse completeRows)
          timestamps = map vppTimestamp $ vptPoints $ vphSenior history
      vpcComplete (vphCoverage history) `shouldBe` True
      timestamps `shouldBe` map vpsBlockTimestamp completeRows
      vptReturn7d (vphSenior history) `shouldSatisfy` maybe False (> 0)
      vptApy7d (vphJunior history) `shouldSatisfy` maybe False (> 0)

    it "returns available points but null metrics for incomplete coverage" $ do
      let history = buildVaultPerformanceHistoryAt freshNow deployment $ take 12 completeRows
      vpcComplete (vphCoverage history) `shouldBe` False
      vptApy7d (vphSenior history) `shouldBe` Nothing
      vptReturn7d (vphJunior history) `shouldBe` Nothing
      length (vptPoints $ vphSenior history) `shouldBe` 12

    it "publishes neither tranche's metrics when either result is not representable" $ do
      let overflowing =
            updateAt
              168
              (\row -> row {vpsSeniorSharePriceWad = 10 ^ (70 :: Integer)})
              completeRows
          history = buildVaultPerformanceHistoryAt freshNow deployment overflowing
      vpcComplete (vphCoverage history) `shouldBe` False
      vptApy7d (vphSenior history) `shouldBe` Nothing
      vptReturn7d (vphSenior history) `shouldBe` Nothing
      vptApy7d (vphJunior history) `shouldBe` Nothing
      vptReturn7d (vphJunior history) `shouldBe` Nothing

    it "encodes EVM integer quantities and prices as decimal strings" $ do
      encode samplePoint
        `shouldBe`
          encode
            ( object
                [ "timestamp" .= (1_800_000_000 :: Integer)
                , "blockNumber" .= ("12345678901234567890" :: String)
                , "sharePrice" .= ("1007500000000000000" :: String)
                , "totalAssets" .= ("402670000000000" :: String)
                , "totalSupply" .= ("399673000000000000" :: String)
                ]
            )

    it "uses the exact deployment, coverage, and tranche response contract" $ do
      let history = buildVaultPerformanceHistoryAt freshNow deployment completeRows
      toJSON history
        `shouldBe`
          object
            [ "range" .= ("7d" :: String)
            , "intervalSeconds" .= (3_600 :: Integer)
            , "deployment" .=
                object
                  [ "chainId" .= vpdChainId deployment
                  , "housePool" .= vpdHousePool deployment
                  , "seniorVault" .= vpdSeniorVault deployment
                  , "juniorVault" .= vpdJuniorVault deployment
                  ]
            , "coverage" .= vphCoverage history
            , "senior" .= vphSenior history
            , "junior" .= vphJunior history
            ]

deployment :: VaultPerformanceDeployment
deployment =
  VaultPerformanceDeployment
    { vpdChainId = 421_614
    , vpdHousePool = "0xhouse"
    , vpdSeniorVault = "0xsenior"
    , vpdJuniorVault = "0xjunior"
    }

completeRows :: [VaultPerformanceSnapshotRow]
completeRows = map snapshot [0 .. vaultPerformancePointCount - 1]

snapshot :: Int -> VaultPerformanceSnapshotRow
snapshot index =
  VaultPerformanceSnapshotRow
    { vpsChainId = vpdChainId deployment
    , vpsHousePoolAddress = vpdHousePool deployment
    , vpsSeniorVaultAddress = vpdSeniorVault deployment
    , vpsJuniorVaultAddress = vpdJuniorVault deployment
    , vpsEpochTimestamp = baseEpoch + fromIntegral index * vaultPerformanceIntervalSeconds
    , vpsBlockNumber = 10_000 + fromIntegral index * 1_000
    , vpsBlockHash = "0xblock"
    , vpsBlockTimestamp = baseEpoch + fromIntegral index * vaultPerformanceIntervalSeconds - 12
    , vpsSeniorTotalAssets = 400_000_000_000_000 + fromIntegral index * 1_000_000
    , vpsSeniorTotalSupply = 397_000_000_000_000
    , vpsSeniorSharePriceWad = 1_000_000_000_000_000_000 + fromIntegral index * 1_000_000_000_000
    , vpsJuniorTotalAssets = 100_000_000_000_000 + fromIntegral index * 2_000_000
    , vpsJuniorTotalSupply = 155_000_000_000_000
    , vpsJuniorSharePriceWad = 640_000_000_000_000_000 + fromIntegral index * 2_000_000_000_000
    }

baseEpoch :: Integer
baseEpoch = 1_800_003_600

freshNow :: Integer
freshNow = vpsEpochTimestamp (last completeRows) + 5 * 60

samplePoint :: VaultPerformancePoint
samplePoint =
  VaultPerformancePoint
    { vppTimestamp = 1_800_000_000
    , vppBlockNumber = 12_345_678_901_234_567_890
    , vppSharePrice = 1_007_500_000_000_000_000
    , vppTotalAssets = 402_670_000_000_000
    , vppTotalSupply = 399_673_000_000_000_000
    }

removeAt :: Int -> [a] -> [a]
removeAt index values = take index values <> drop (index + 1) values

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt index update values =
  case splitAt index values of
    (prefix, value : suffix) -> prefix <> (update value : suffix)
    _ -> values

shouldApprox :: Double -> Double -> Expectation
actual `shouldApprox` expected = abs (actual - expected) `shouldSatisfy` (< 1e-10)
