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
  , classifyParameterDrift
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

  it "extracts all 30 contracts and economics from the embedded release manifest" $ do
    let contracts = extractManifestContracts Manifest.manifestValue
        economics = extractManifestEconomics Manifest.manifestValue
    length contracts `shouldSatisfy` (>= 30)
    any (\c -> mcsKey c == "cfdEngine") contracts `shouldBe` True
    any (\c -> mcsKey c == "emergencyPauseCoordinator") contracts `shouldBe` True
    Map.lookup "initialMarginBps" economics `shouldBe` Just "20"
    Map.lookup "executionFeeBps" economics `shouldBe` Just "4"
    extractManifestContracts (object ["contracts" .= object []]) `shouldBe` []
