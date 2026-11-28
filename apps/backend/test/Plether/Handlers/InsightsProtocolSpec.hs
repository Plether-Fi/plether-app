module Plether.Handlers.InsightsProtocolSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi (encodeUint256, keccak256)
import Plether.Handlers.InsightsProtocol
  ( ContractCodeCheck (..)
  , LiveEngineRiskParams (..)
  , LiveProtocolStatus (..)
  , ManifestContractSpec (..)
  , ProtocolTvlSummary (..)
  , classifyParameterDrift
  , computeProtocolTvl
  , computeTvlShareBps
  , decodeEngineRiskParams
  , decodeProtocolStatus
  , decodeSingleBool
  , decodeSingleUint
  , extractManifestContracts
  , extractManifestEconomics
  , phaseLabel
  , verifyRuntimeCodeHash
  )
import qualified Plether.Perps.Manifest as Manifest
import Test.Hspec

spec :: Spec
spec = describe "Plether.Handlers.InsightsProtocol" $ do
  it "decodes PerpsPublicLens.getProtocolStatus() 8-word tuple accurately" $ do
    let payload =
          BS.concat
            [ encodeUint256 1 -- active phase
            , encodeUint256 94743619 -- lastMarkPrice
            , encodeUint256 1795710000 -- lastMarkTime
            , encodeUint256 0 -- oracleFrozen = False
            , encodeUint256 0 -- fadWindow = False
            , encodeUint256 1 -- tradingActive = True
            , encodeUint256 1 -- withdrawalLive = True
            , encodeUint256 0 -- lpEpochSettlementPaused = False
            ]
    decodeProtocolStatus payload
      `shouldBe` Just
        LiveProtocolStatus
          { lpsPhase = 1
          , lpsLastMarkPrice = 94743619
          , lpsLastMarkTime = 1795710000
          , lpsOracleFrozen = False
          , lpsFadWindow = False
          , lpsTradingActive = True
          , lpsWithdrawalLive = True
          , lpsLpEpochSettlementPaused = False
          }
    decodeProtocolStatus (BS.replicate 64 0) `shouldBe` Nothing
    phaseLabel 1 `shouldBe` "active"
    phaseLabel 3 `shouldBe` "paused"

  it "decodes CfdEngine.riskParams() 10-word tuple accurately" $ do
    let payload =
          BS.concat $
            map
              encodeUint256
              [ 10000000000000000 -- vpiFactor
              , 400000000000000000 -- maxSkewRatio
              , 10 -- maintMarginBps
              , 20 -- initMarginBps
              , 300 -- fadMarginBps
              , 500 -- baseCarryBps
              , 1000000 -- minBountyAssets
              , 10 -- bountyBps
              , 5000 -- keeperShareBps
              , 0 -- protocolShareBps
              ]
    decodeEngineRiskParams payload
      `shouldBe` Just
        LiveEngineRiskParams
          { lerVpiFactorWad = 10000000000000000
          , lerMaxSkewRatioWad = 400000000000000000
          , lerMaintenanceMarginBps = 10
          , lerInitialMarginBps = 20
          , lerFadMarginBps = 300
          , lerBaseCarryBps = 500
          , lerMinimumLiquidationChargeAssets = 1000000
          , lerLiquidationChargeBps = 10
          , lerKeeperShareBps = 5000
          , lerProtocolShareBps = 0
          }
    decodeEngineRiskParams (BS.replicate 32 0) `shouldBe` Nothing

  it "classifies manifest vs live parameter drift without faking matches" $ do
    classifyParameterDrift (Just "20") (Just "20") `shouldBe` "match"
    classifyParameterDrift (Just "20") (Just "25") `shouldBe` "drifted"
    classifyParameterDrift Nothing (Just "3600") `shouldBe` "live_only"
    classifyParameterDrift (Just "20") Nothing `shouldBe` "unavailable"
    decodeSingleUint (encodeUint256 42) `shouldBe` Just 42
    decodeSingleBool (encodeUint256 1) `shouldBe` Just True
    decodeSingleBool (encodeUint256 2) `shouldBe` Nothing

  it "verifies runtime bytecode hashes against eth_getCode payloads" $ do
    let rawCode = BS.pack [0x60, 0x80, 0x60, 0x40]
        encodedHex = "0x" <> TE.decodeUtf8 (B16.encode rawCode)
        expectedHash = "0x" <> TE.decodeUtf8 (B16.encode (keccak256 rawCode))
    verifyRuntimeCodeHash expectedHash (String encodedHex)
      `shouldBe` ContractCodeCheck (Just expectedHash) (Just True)
    verifyRuntimeCodeHash ("0x" <> TE.decodeUtf8 (B16.encode (BS.replicate 32 0xaa))) (String encodedHex)
      `shouldBe` ContractCodeCheck (Just expectedHash) (Just False)
    verifyRuntimeCodeHash expectedHash (String "0x")
      `shouldBe` ContractCodeCheck (Just "0x") (Just False)
    verifyRuntimeCodeHash expectedHash Null
      `shouldBe` ContractCodeCheck Nothing Nothing

  it "extracts all 31 contracts and economics from the embedded release manifest" $ do
    let contracts = extractManifestContracts Manifest.manifestValue
        economics = extractManifestEconomics Manifest.manifestValue
    length contracts `shouldSatisfy` (>= 31)
    any (\c -> mcsKey c == "cfdEngine") contracts `shouldBe` True
    any (\c -> mcsKey c == "cfdOrderAdmissionValidator") contracts `shouldBe` True
    any (\c -> mcsKey c == "emergencyPauseCoordinator") contracts `shouldBe` True
    Map.lookup "initialMarginBps" economics `shouldBe` Just "30"
    Map.lookup "executionFeeBps" economics `shouldBe` Just "4"
    extractManifestContracts (object ["contracts" .= object []]) `shouldBe` []

  it "aggregates disjoint protocol contract and Kernel v4 trading account TVL without double counting" $ do
    let hp = ManifestContractSpec "housePool" "0x0000000000000000000000000000000000000001" "0x01"
        sr = ManifestContractSpec "seniorVault" "0x0000000000000000000000000000000000000002" "0x02"
        jr = ManifestContractSpec "juniorVault" "0x0000000000000000000000000000000000000003" "0x03"
        ch = ManifestContractSpec "marginClearinghouse" "0x0000000000000000000000000000000000000004" "0x04"
        eng = ManifestContractSpec "cfdEngine" "0x0000000000000000000000000000000000000005" "0x05"
        kernelA = "0x1111111111111111111111111111111111111111"
        kernelB = "0x2222222222222222222222222222222222222222"
        -- Duplicate kernelA (mixed case) and accidental overlap with housePool must be ignored
        summary =
          computeProtocolTvl
            [ (hp, Just 500_000_000_000)
            , (sr, Just 100_000_000_000)
            , (jr, Just 50_000_000_000)
            , (ch, Just 200_000_000_000)
            , (eng, Just 5_000_000_000)
            ]
            True
            [ (kernelA, Just 100_000_000_000)
            , ("0x1111111111111111111111111111111111111111", Just 999_000_000_000)
            , (kernelB, Just 45_000_000_000)
            , ("0x0000000000000000000000000000000000000001", Just 500_000_000_000)
            ]
    ptsLpVaultsTvlAssets summary `shouldBe` 650_000_000_000
    ptsMarginClearinghouseAssets summary `shouldBe` 200_000_000_000
    ptsOtherContractsAssets summary `shouldBe` 5_000_000_000
    ptsProtocolContractsTvlAssets summary `shouldBe` 855_000_000_000
    ptsKernelTradingAccountsAssets summary `shouldBe` 145_000_000_000
    ptsTotalTraderMarginAssets summary `shouldBe` 345_000_000_000
    ptsTotalTvlAssets summary `shouldBe` 1_000_000_000_000
    ptsKernelAccountsTracked summary `shouldBe` 2
    ptsKernelAccountsFunded summary `shouldBe` 2
    ptsIsComplete summary `shouldBe` True
    computeTvlShareBps 650_000_000_000 1_000_000_000_000 `shouldBe` Just 6500
    computeTvlShareBps 145_000_000_000 1_000_000_000_000 `shouldBe` Just 1450
    computeTvlShareBps 100 0 `shouldBe` Nothing

  it "marks TVL incomplete when any balance read or Kernel account discovery fails" $ do
    let hp = ManifestContractSpec "housePool" "0x0000000000000000000000000000000000000001" "0x01"
        partial = computeProtocolTvl [(hp, Just 100_000_000)] True [("0x1111111111111111111111111111111111111111", Nothing)]
        noDb = computeProtocolTvl [(hp, Just 100_000_000)] False []
    ptsIsComplete partial `shouldBe` False
    ptsKernelAccountsFailedReads partial `shouldBe` 1
    ptsIsComplete noDb `shouldBe` False
    ptsKernelAccountsAvailable noDb `shouldBe` False
