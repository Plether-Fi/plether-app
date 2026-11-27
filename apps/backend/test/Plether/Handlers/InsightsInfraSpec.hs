module Plether.Handlers.InsightsInfraSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Plether.Handlers.InsightsInfra
  ( classifyActionReadiness
  , classifyOverallInfra
  , classifyServiceHealth
  , parseEthSyncing
  , parsePeerCount
  )
import Plether.Infra.AwsHealth (AwsEcsServiceStatus (..))
import Test.Hspec

spec :: Spec
spec = describe "Plether.Handlers.InsightsInfra" $ do
  it "classifies ECS + heartbeat + funding service health fail-closed" $ do
    let ecsOk = Just (AwsEcsServiceStatus "keeper" "ACTIVE" 1 1 0 (Just "COMPLETED"))
        ecsDown = Just (AwsEcsServiceStatus "keeper" "ACTIVE" 1 0 1 Nothing)
        ecsHalf = Just (AwsEcsServiceStatus "api" "ACTIVE" 2 1 1 Nothing)
        ecsZero = Just (AwsEcsServiceStatus "alto" "ACTIVE" 0 0 0 Nothing)
    classifyServiceHealth ecsOk (Just 12) 120 (Just "ready") `shouldBe` "healthy"
    classifyServiceHealth ecsDown (Just 12) 120 (Just "ready") `shouldBe` "down"
    classifyServiceHealth ecsOk (Just 12) 120 (Just "blocked") `shouldBe` "down"
    classifyServiceHealth ecsHalf (Just 12) 120 (Just "ready") `shouldBe` "degraded"
    classifyServiceHealth ecsOk (Just 240) 120 (Just "ready") `shouldBe` "degraded"
    classifyServiceHealth ecsZero Nothing 120 Nothing `shouldBe` "scaled_zero"
    classifyServiceHealth Nothing Nothing 120 Nothing `shouldBe` "unknown"

  it "classifies overall infra health across healthy, degraded, and down states" $ do
    classifyOverallInfra True True True True 9 9 `shouldBe` "healthy"
    classifyOverallInfra True True True True 7 9 `shouldBe` "degraded"
    classifyOverallInfra True True True False 9 9 `shouldBe` "degraded"
    classifyOverallInfra True False True True 9 9 `shouldBe` "down"
    classifyOverallInfra True True False True 9 9 `shouldBe` "down"

  it "classifies user action readiness deterministically" $ do
    classifyActionReadiness "Open position" [("ready", "READY"), ("ready", "READY")]
      `shouldBe` object ["label" .= ("Open position" :: String), "status" .= ("ready" :: String), "reason" .= ("READY" :: String)]
    classifyActionReadiness "Open position" [("ready", "READY"), ("blocked", "KEEPER_STALE")]
      `shouldBe` object ["label" .= ("Open position" :: String), "status" .= ("blocked" :: String), "reason" .= ("KEEPER_STALE" :: String)]

  it "parses eth_syncing and net_peerCount RPC responses cleanly" $ do
    parseEthSyncing (Bool False) `shouldBe` Just False
    parseEthSyncing (Bool True) `shouldBe` Just True
    parseEthSyncing (object ["currentBlock" .= ("0x10" :: String)]) `shouldBe` Just True
    parseEthSyncing Null `shouldBe` Nothing
    parsePeerCount (String "0x1a") `shouldBe` Just 26
    parsePeerCount (String "0x0") `shouldBe` Just 0
    parsePeerCount Null `shouldBe` Nothing
