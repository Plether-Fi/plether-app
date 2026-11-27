module Plether.Handlers.InsightsInfraSpec (spec) where

import Control.Concurrent.MVar (newMVar)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Plether.Handlers.InsightsInfra
  ( classifyActionReadiness
  , classifyEventDrivenServiceHealth
  , classifyOverallInfra
  , classifyServiceHealth
  , parseEthSyncing
  , parsePeerCount
  , pickFresherHeartbeat
  , withTtlCache
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

  it "classifies idle event-driven workers as healthy unless faulted, blocked, or ECS down" $ do
    let ecsOk = Just (AwsEcsServiceStatus "keeper" "ACTIVE" 1 1 0 (Just "COMPLETED"))
        ecsDown = Just (AwsEcsServiceStatus "keeper" "ACTIVE" 1 0 1 Nothing)
    classifyEventDrivenServiceHealth Nothing (Just 172980) 600 (Just "ready") False `shouldBe` "healthy"
    classifyEventDrivenServiceHealth ecsOk (Just 172980) 600 (Just "ready") False `shouldBe` "healthy"
    classifyEventDrivenServiceHealth ecsOk (Just 172980) 600 (Just "ready") True `shouldBe` "degraded"
    classifyEventDrivenServiceHealth ecsOk (Just 172980) 600 (Just "blocked") False `shouldBe` "down"
    classifyEventDrivenServiceHealth ecsDown (Just 10) 600 (Just "ready") False `shouldBe` "down"
    classifyEventDrivenServiceHealth Nothing Nothing 600 Nothing False `shouldBe` "unknown"

  it "picks the fresher heartbeat between Divisions live snapshots and legacy snapshot batches" $ do
    let older = posixSecondsToUTCTime 1700000000
        newer = posixSecondsToUTCTime 1795000000
    pickFresherHeartbeat (Just 500, Just newer) (Just 100, Just older) `shouldBe` (Just 500, Just newer)
    pickFresherHeartbeat (Nothing, Nothing) (Just 100, Just older) `shouldBe` (Just 100, Just older)
    pickFresherHeartbeat (Just 500, Just newer) (Nothing, Nothing) `shouldBe` (Just 500, Just newer)

  it "caches successful responses inside the TTL window and does not cache errors" $ do
    cacheVar <- newMVar Nothing
    calls <- newIORef (0 :: Int)
    let okAction :: IO (Either String Int)
        okAction = do
          n <- atomicModifyIORef' calls (\c -> (c + 1, c + 1))
          pure (Right n)
        errAction :: IO (Either String Int)
        errAction = do
          _ <- atomicModifyIORef' calls (\c -> (c + 1, c + 1))
          pure (Left "boom")
    rErr <- withTtlCache cacheVar 10000000000 errAction
    rErr `shouldBe` Left "boom"
    r1 <- withTtlCache cacheVar 10000000000 okAction
    r2 <- withTtlCache cacheVar 10000000000 okAction
    r1 `shouldBe` Right 2
    r2 `shouldBe` Right 2
    readIORef calls `shouldReturn` 2
