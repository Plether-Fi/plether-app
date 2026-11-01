module Plether.Perps.ClosedPriceGap
  ( validateClosedPriceGapEvidence
  , withinWeeklyFxClosure
  ) where

import Control.Monad (unless, when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..), utctDay, utctDayTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Plether.Pyth.Basket (basketComponents)

-- | Validate the negative evidence used by the one-shot Sepolia recovery.
-- The authenticated history endpoint must report no component observations in
-- the missing range, while a fresh signed latest payload must still resolve to
-- the observation that predates it. The operator-supplied upper bound prevents
-- an approval-delayed workflow from crossing into the live FX session.
validateClosedPriceGapEvidence
  :: Integer -- ^ published minute coverage end expected in PostgreSQL
  -> Integer -- ^ latest payload fetch time; recovery advances through here
  -> Integer -- ^ exclusive operator-approved recovery deadline
  -> Integer -- ^ newest signed component publish time
  -> [[Integer]] -- ^ authenticated history timestamps, one list per feed
  -> Either Text ()
validateClosedPriceGapEvidence coverageEnd checkedThrough recoverBefore latestPublishTime history = do
  unless (coverageEnd >= 0 && coverageEnd `mod` 60 == 0) $
    Left "Expected price coverage end must align to a UTC minute"
  unless (checkedThrough > coverageEnd) $
    Left "Closed price-gap recovery did not extend beyond published coverage"
  unless (recoverBefore > checkedThrough) $
    Left "Closed price-gap recovery reached or crossed its approved deadline"
  unless (length history == length basketComponents) $
    Left "Closed price-gap history evidence did not cover all six basket feeds"
  unless (latestPublishTime < coverageEnd) $
    Left "Latest signed Pyth state does not predate the missing coverage range"
  unless (withinWeeklyFxClosure coverageEnd recoverBefore) $
    Left "Closed price-gap recovery bounds are not wholly inside one weekly FX closure"
  let observedInGap =
        [ timestamp
        | timestamps <- history
        , timestamp <- timestamps
        , timestamp >= coverageEnd
        , timestamp < checkedThrough
        ]
  when (not $ null observedInGap) $
    Left $
      "Pyth history contains "
        <> T.pack (show $ length observedInGap)
        <> " component update(s) inside the proposed recovery range"

-- | Conservative weekly oracle-frozen interval documented by the protocol:
-- Friday 17:00 New York through Sunday 17:00 New York, matching core
-- MarketCalendarLib, including weekends where the UTC offsets differ. Excludes the
-- live-oracle close-only shoulders on both sides.
withinWeeklyFxClosure :: Integer -> Integer -> Bool
withinWeeklyFxClosure rangeStart rangeEnd =
  rangeStart >= closureStart
    && rangeEnd <= closureEnd
    && rangeEnd > rangeStart
 where
  startUtc = posixSecondsToUTCTime $ fromInteger rangeStart
  (_, _, weekday) = toWeekDate $ utctDay startUtc
  secondsToday = floor $ utctDayTime startUtc
  secondsFromMonday = fromIntegral (weekday - 1) * secondsPerDay + secondsToday
  mondayStart = rangeStart - secondsFromMonday
  friday = mondayStart + 4 * secondsPerDay
  sunday = mondayStart + 6 * secondsPerDay
  closureStart = friday + newYorkMarketBoundary friday
  closureEnd = sunday + newYorkMarketBoundary sunday

secondsPerDay :: Integer
secondsPerDay = 86_400

-- Evaluate each endpoint independently using the protocol's post-2007 US DST
-- rules. Noon UTC is after either Sunday transition and before FX reopening.
newYorkMarketBoundary :: Integer -> Integer
newYorkMarketBoundary dayStart =
  if midday >= dstStart && midday < dstEnd then 21 * 3_600 else 22 * 3_600
 where
  midday = dayStart + 12 * 3_600
  (year, _, _) = toGregorian $ utctDay $ posixSecondsToUTCTime $ fromInteger midday
  firstSunday month =
    let (_, _, weekday) = toWeekDate $ fromGregorian year month 1
     in 1 + (7 - weekday) `mod` 7
  transition month day hour =
    floor $ utcTimeToPOSIXSeconds $ UTCTime (fromGregorian year month day) (fromIntegral $ hour * 3_600)
  dstStart = transition 3 (firstSunday 3 + 7) (7 :: Integer)
  dstEnd = transition 11 (firstSunday 11) (6 :: Integer)
