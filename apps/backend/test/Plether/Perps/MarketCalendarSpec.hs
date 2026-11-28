module Plether.Perps.MarketCalendarSpec (spec) where

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Plether.Perps.MarketCalendar
  ( MarketCalendarSnapshot (..)
  , MarketClosure (..)
  , MarketRegime (..)
  , UpcomingMarketWindow (..)
  , buildMarketCalendarSnapshot
  , evaluateMarketStatus
  , isNewYorkDst
  , newYorkDstMarketBoundary
  , newYorkMarketBoundary
  , newYorkStandardMarketBoundary
  )
import Test.Hspec

utc :: Integer -> Int -> Int -> Int -> Int -> Int -> Integer
utc y m d hh mm ss =
  floor $
    utcTimeToPOSIXSeconds $
      UTCTime (fromGregorian y m d) (secondsToDiffTime (fromIntegral (hh * 3600 + mm * 60 + ss)))

spec :: Spec
spec = describe "Plether.Perps.MarketCalendar" $ do
  it "applies post-2007 US DST transitions accurately (spring forward & fall back)" $ do
    -- 2026: 2nd Sunday of March is 2026-03-08 (DST starts 07:00 UTC)
    isNewYorkDst (utc 2026 3 8 6 59 59) `shouldBe` False
    isNewYorkDst (utc 2026 3 8 7 0 0) `shouldBe` True
    newYorkMarketBoundary (utc 2026 3 6 21 0 0) `shouldBe` newYorkStandardMarketBoundary -- Friday before DST: 22:00 UTC
    newYorkMarketBoundary (utc 2026 3 8 21 0 0) `shouldBe` newYorkDstMarketBoundary -- Sunday after spring forward: 21:00 UTC

    -- 2026: 1st Sunday of November is 2026-11-01 (DST ends 06:00 UTC)
    isNewYorkDst (utc 2026 11 1 5 59 59) `shouldBe` True
    isNewYorkDst (utc 2026 11 1 6 0 0) `shouldBe` False
    newYorkMarketBoundary (utc 2026 10 30 21 0 0) `shouldBe` newYorkDstMarketBoundary -- Friday before fall back: 21:00 UTC
    newYorkMarketBoundary (utc 2026 11 1 21 0 0) `shouldBe` newYorkStandardMarketBoundary -- Sunday after fall back: 22:00 UTC

  it "classifies recurring Friday FAD lead (10m), weekend freeze, and Sunday FAD lag (5m)" $ do
    -- Standard time weekend: Fri 2026-11-27 (boundary 22:00 UTC) -> Sun 2026-11-29 (boundary 22:00 UTC)
    evaluateMarketStatus (utc 2026 11 27 21 49 59) [] 0 `shouldBe` (False, False)
    evaluateMarketStatus (utc 2026 11 27 21 50 0) [] 0 `shouldBe` (True, False)
    evaluateMarketStatus (utc 2026 11 27 21 59 59) [] 0 `shouldBe` (True, False)
    evaluateMarketStatus (utc 2026 11 27 22 0 0) [] 0 `shouldBe` (True, True)
    evaluateMarketStatus (utc 2026 11 28 12 0 0) [] 0 `shouldBe` (True, True)
    evaluateMarketStatus (utc 2026 11 29 21 59 59) [] 0 `shouldBe` (True, True)
    evaluateMarketStatus (utc 2026 11 29 22 0 0) [] 0 `shouldBe` (True, False)
    evaluateMarketStatus (utc 2026 11 29 22 4 59) [] 0 `shouldBe` (True, False)
    evaluateMarketStatus (utc 2026 11 29 22 5 0) [] 0 `shouldBe` (False, False)

  it "applies governance MarketClosure fadRunwaySeconds and 5-minute post-closure FAD lag" $ do
    -- Mid-week holiday closure: Wed 2026-12-23 18:00 UTC -> Thu 2026-12-24 18:00 UTC with 3600s runway
    let closure = MarketClosure (utc 2026 12 23 18 0 0) (utc 2026 12 24 18 0 0)
    evaluateMarketStatus (utc 2026 12 23 16 59 59) [closure] 3600 `shouldBe` (False, False)
    evaluateMarketStatus (utc 2026 12 23 17 0 0) [closure] 3600 `shouldBe` (True, False)
    evaluateMarketStatus (utc 2026 12 23 18 0 0) [closure] 3600 `shouldBe` (True, True)
    evaluateMarketStatus (utc 2026 12 24 18 0 0) [closure] 3600 `shouldBe` (True, False)
    evaluateMarketStatus (utc 2026 12 24 18 4 59) [closure] 3600 `shouldBe` (True, False)
    evaluateMarketStatus (utc 2026 12 24 18 5 0) [closure] 3600 `shouldBe` (False, False)

  it "computes upcoming windows, countdowns, and active regimes cleanly" $ do
    -- Thursday 2026-11-26 21:50 UTC -> exactly 24h before Fri 21:50 UTC FAD start
    let now = utc 2026 11 26 21 50 0
        snap = buildMarketCalendarSnapshot now [] 3600 Nothing 14
    mcsRegime snap `shouldBe` RegimeLive
    mcsFadWindow snap `shouldBe` False
    mcsOracleFrozen snap `shouldBe` False
    mcsNextFadStartsAt snap `shouldBe` Just (utc 2026 11 27 21 50 0)
    mcsNextFreezeStartsAt snap `shouldBe` Just (utc 2026 11 27 22 0 0)
    mcsSecondsUntilNextFad snap `shouldBe` Just 86400
    mcsSecondsUntilNextFreeze snap `shouldBe` Just (86400 + 600)
    length (mcsUpcomingWindows snap) `shouldSatisfy` (>= 2)
    umwKind (head (mcsUpcomingWindows snap)) `shouldBe` "weekend"

    -- During DST transition weekend (Fri 2026-03-06 22:00 UTC -> Sun 2026-03-08 21:00 UTC)
    let springSnap = buildMarketCalendarSnapshot (utc 2026 3 5 12 0 0) [] 0 Nothing 7
        firstWin = head (mcsUpcomingWindows springSnap)
    umwFreezeStartsAt firstWin `shouldBe` utc 2026 3 6 22 0 0
    umwFreezeEndsAt firstWin `shouldBe` utc 2026 3 8 21 0 0
