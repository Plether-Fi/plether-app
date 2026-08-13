module Plether.RequestLoggingSpec (spec) where

import Plether.RequestLogging
  ( RequestClass (..)
  , afterResponseHandoff
  , classifyNormalizedRoute
  , normalizeRouteSegments
  , shouldEmitForegroundSample
  , shouldEmitSlowWarning
  )
import Data.IORef (modifyIORef', newIORef, readIORef)
import Test.Hspec

spec :: Spec
spec = describe "Plether.RequestLogging" $ do
  describe "request classification" $ do
    it "classifies the normalized order wait route as an intentional long poll" $ do
      let route = normalizeRouteSegments ["api", "perps", "orders", "12345", "wait"]
      route `shouldBe` "/api/perps/orders/:id/wait"
      classifyNormalizedRoute route `shouldBe` LongPoll

    it "classifies only the health endpoint as a health check" $ do
      classifyNormalizedRoute (normalizeRouteSegments ["api", "health"])
        `shouldBe` HealthCheck
      classifyNormalizedRoute (normalizeRouteSegments ["api", "protocol", "status"])
        `shouldBe` Foreground

    it "keeps other order routes in the foreground class" $ do
      classifyNormalizedRoute
        (normalizeRouteSegments ["api", "perps", "orders", "12345", "reveal-payload"])
        `shouldBe` Foreground

    it "normalizes dynamic competition slugs and wallet addresses" $ do
      normalizeRouteSegments
        [ "api"
        , "insights"
        , "v1"
        , "competitions"
        , "august-2026"
        , "wallets"
        , "0x1111111111111111111111111111111111111111"
        ]
        `shouldBe` "/api/insights/v1/competitions/:slug/wallets/:address"

    it "collapses unknown paths instead of logging attacker-controlled routes" $ do
      normalizeRouteSegments ["api", "invented", "high-cardinality-value"]
        `shouldBe` "/:unmatched"

  describe "foreground latency sampling" $ do
    it "records completion only after the response handoff returns" $ do
      ordering <- newIORef ([] :: [String])
      result <-
        afterResponseHandoff
          (modifyIORef' ordering (<> ["handoff"]) >> pure ("received" :: String))
          (modifyIORef' ordering (<> ["completion"]))
      result `shouldBe` "received"
      readIORef ordering `shouldReturn` ["handoff", "completion"]

    it "samples only foreground requests" $ do
      shouldEmitForegroundSample Foreground `shouldBe` True
      shouldEmitForegroundSample LongPoll `shouldBe` False
      shouldEmitForegroundSample HealthCheck `shouldBe` False

    it "keeps basket history and account order reads in the foreground class" $ do
      classifyNormalizedRoute (normalizeRouteSegments ["api", "perps", "basket", "history"])
        `shouldBe` Foreground
      classifyNormalizedRoute
        ( normalizeRouteSegments
            [ "api"
            , "perps"
            , "accounts"
            , "0x1111111111111111111111111111111111111111"
            , "orders"
            ]
        )
        `shouldBe` Foreground

    it "does not let expected long polls or health checks consume slow-warning slots" $ do
      shouldEmitSlowWarning Foreground 200 2_000 `shouldBe` True
      shouldEmitSlowWarning LongPoll 200 60_000 `shouldBe` False
      shouldEmitSlowWarning HealthCheck 200 60_000 `shouldBe` False

    it "keeps server failures on the dedicated error path" $ do
      shouldEmitSlowWarning Foreground 500 60_000 `shouldBe` False
