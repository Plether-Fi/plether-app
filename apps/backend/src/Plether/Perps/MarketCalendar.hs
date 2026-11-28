module Plether.Perps.MarketCalendar
  ( MarketClosure (..)
  , MarketRegime (..)
  , UpcomingMarketWindow (..)
  , MarketCalendarSnapshot (..)
  , secondsPerDay
  , newYorkDstMarketBoundary
  , newYorkStandardMarketBoundary
  , fadLeadSeconds
  , fadLagSeconds
  , isNewYorkDst
  , newYorkMarketBoundary
  , evaluateMarketStatus
  , buildUpcomingMarketWindows
  , buildMarketCalendarSnapshot
  , marketRegimeText
  ) where

import Data.List (find, sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

secondsPerDay :: Integer
secondsPerDay = 86400

-- | 17:00 New York while EDT (UTC-4) is active: 21:00 UTC.
newYorkDstMarketBoundary :: Integer
newYorkDstMarketBoundary = 21 * 3600

-- | 17:00 New York while EST (UTC-5) is active: 22:00 UTC.
newYorkStandardMarketBoundary :: Integer
newYorkStandardMarketBoundary = 22 * 3600

-- | FAD starts 10 minutes before the Friday 17:00 New York FX close.
fadLeadSeconds :: Integer
fadLeadSeconds = 10 * 60

-- | FAD stays active 5 minutes after the Sunday 17:00 New York FX open (or closure end).
fadLagSeconds :: Integer
fadLagSeconds = 5 * 60

-- | 02:00 EST -> 07:00 UTC on the 2nd Sunday of March.
newYorkDstStartUtc :: Integer
newYorkDstStartUtc = 7 * 3600

-- | 02:00 EDT -> 06:00 UTC on the 1st Sunday of November.
newYorkDstEndUtc :: Integer
newYorkDstEndUtc = 6 * 3600

data MarketClosure = MarketClosure
  { mcStartTime :: Integer
  , mcEndTime :: Integer
  }
  deriving stock (Eq, Show)

data MarketRegime
  = RegimeLive
  | RegimeFadRunway
  | RegimeFrozen
  | RegimeScheduledClosure
  deriving stock (Eq, Show)

marketRegimeText :: MarketRegime -> Text
marketRegimeText = \case
  RegimeLive -> "live"
  RegimeFadRunway -> "fad_runway"
  RegimeFrozen -> "frozen"
  RegimeScheduledClosure -> "scheduled_closure"

data UpcomingMarketWindow = UpcomingMarketWindow
  { umwKind :: Text -- "weekend" | "scheduled_closure"
  , umwLabel :: Text
  , umwFadStartsAt :: Integer
  , umwFreezeStartsAt :: Integer
  , umwFreezeEndsAt :: Integer
  , umwFadEndsAt :: Integer
  }
  deriving stock (Eq, Show)

data MarketCalendarSnapshot = MarketCalendarSnapshot
  { mcsRegime :: MarketRegime
  , mcsFadWindow :: Bool
  , mcsOracleFrozen :: Bool
  , mcsIsNewYorkDst :: Bool
  , mcsNextFadStartsAt :: Maybe Integer
  , mcsNextFreezeStartsAt :: Maybe Integer
  , mcsNextReopenAt :: Maybe Integer
  , mcsCurrentFadEndsAt :: Maybe Integer
  , mcsSecondsUntilNextFad :: Maybe Integer
  , mcsSecondsUntilNextFreeze :: Maybe Integer
  , mcsFadRunwaySeconds :: Integer
  , mcsScheduledClosures :: [MarketClosure]
  , mcsUpcomingWindows :: [UpcomingMarketWindow]
  }
  deriving stock (Eq, Show)

-- | Howard Hinnant civil-from-days algorithm matching MarketCalendarLib._monthAndDay.
-- Converts Unix day number (days since 1970-01-01) to (month [1..12], dayOfMonth [1..31]).
monthAndDay :: Integer -> (Integer, Integer)
monthAndDay daysSinceEpoch =
  let z = daysSinceEpoch + 719468
      era = (if z >= 0 then z else z - 146096) `div` 146097
      doe = z - era * 146097
      yoe = (doe - doe `div` 1460 + doe `div` 36524 - doe `div` 146096) `div` 365
      doy = doe - (365 * yoe + yoe `div` 4 - yoe `div` 100)
      mp = (5 * doy + 2) `div` 153
      dayOfMonth = doy - (153 * mp + 2) `div` 5 + 1
      month = if mp < 10 then mp + 3 else mp - 9
   in (month, dayOfMonth)

-- | Sunday-based UTC weekday (Sunday=0 .. Friday=5, Saturday=6) and second-of-day [0..86399].
dayAndSecond :: Integer -> (Integer, Integer, Integer)
dayAndSecond ts =
  let (daysSinceEpoch, secOfDay) = ts `divMod` secondsPerDay
      dayOfWeek = (daysSinceEpoch + 4) `mod` 7
   in (daysSinceEpoch, dayOfWeek, secOfDay)

-- | Post-2007 US daylight saving time check for America/New_York, matching
-- MarketCalendarLib._isNewYorkDaylightSavingTime.
isNewYorkDst :: Integer -> Bool
isNewYorkDst ts =
  let (daysSinceEpoch, dayOfWeek, secOfDay) = dayAndSecond ts
      (month, dayOfMonth) = monthAndDay daysSinceEpoch
      lastSunday = dayOfMonth - dayOfWeek
   in case month of
        m | m < 3 || m > 11 -> False
        m | m > 3 && m < 11 -> True
        3 ->
          if lastSunday < 8
            then False
            else
              if dayOfWeek == 0 && dayOfMonth <= 14
                then secOfDay >= newYorkDstStartUtc
                else True
        11 ->
          if lastSunday <= 0
            then True
            else
              if dayOfWeek == 0 && dayOfMonth <= 7
                then secOfDay < newYorkDstEndUtc
                else False
        _ -> False

-- | UTC second-of-day for the 17:00 New York FX boundary at a Unix timestamp.
newYorkMarketBoundary :: Integer -> Integer
newYorkMarketBoundary ts =
  if isNewYorkDst ts
    then newYorkDstMarketBoundary
    else newYorkStandardMarketBoundary

-- | Classify a Unix timestamp against recurring weekend rules and governance
-- MarketClosure intervals, returning @(fadWindow, oracleFrozen)@.
evaluateMarketStatus :: Integer -> [MarketClosure] -> Integer -> (Bool, Bool)
evaluateMarketStatus ts closures fadRunwaySecs =
  let (_, dayOfWeek, secOfDay) = dayAndSecond ts
      boundary = newYorkMarketBoundary ts
      recurringFrozen =
        (dayOfWeek == 5 && secOfDay >= boundary)
          || dayOfWeek == 6
          || (dayOfWeek == 0 && secOfDay < boundary)
      recurringFad =
        recurringFrozen
          || (dayOfWeek == 5 && secOfDay >= boundary - fadLeadSeconds)
          || (dayOfWeek == 0 && secOfDay < boundary + fadLagSeconds)
      runway = max 0 fadRunwaySecs
      inClosureFreeze c = ts >= mcStartTime c && ts < mcEndTime c
      inClosureFad c =
        let fadStart = max 0 (mcStartTime c - runway)
            fadEnd = mcEndTime c + fadLagSeconds
         in ts >= fadStart && ts < fadEnd
      closureFrozen = any inClosureFreeze closures
      closureFad = any inClosureFad closures
   in (recurringFad || closureFad, recurringFrozen || closureFrozen)

-- | Generate all weekend FX freeze windows and scheduled governance closures
-- overlapping @[now - 3 days, now + horizonDays]@, sorted chronologically.
buildUpcomingMarketWindows
  :: Integer
  -> [MarketClosure]
  -> Integer
  -> Integer
  -> [UpcomingMarketWindow]
buildUpcomingMarketWindows now closures fadRunwaySecs horizonDays =
  let horizonEnd = now + max 1 horizonDays * secondsPerDay
      lookbackStart = now - 3 * secondsPerDay
      startDay = lookbackStart `div` secondsPerDay
      endDay = (horizonEnd `div` secondsPerDay) + 3
      fridays = [d | d <- [startDay .. endDay], ((d + 4) `mod` 7) == 5]
      mkWeekend friDay =
        let friSample = friDay * secondsPerDay + newYorkDstMarketBoundary
            friBoundary = friDay * secondsPerDay + newYorkMarketBoundary friSample
            sunDay = friDay + 2
            sunSample = sunDay * secondsPerDay + newYorkDstMarketBoundary
            sunBoundary = sunDay * secondsPerDay + newYorkMarketBoundary sunSample
         in UpcomingMarketWindow
              { umwKind = "weekend"
              , umwLabel = "Weekly FX Weekend Freeze"
              , umwFadStartsAt = friBoundary - fadLeadSeconds
              , umwFreezeStartsAt = friBoundary
              , umwFreezeEndsAt = sunBoundary
              , umwFadEndsAt = sunBoundary + fadLagSeconds
              }
      runway = max 0 fadRunwaySecs
      mkClosure c =
        UpcomingMarketWindow
          { umwKind = "scheduled_closure"
          , umwLabel = "Governance Scheduled Closure"
          , umwFadStartsAt = max 0 (mcStartTime c - runway)
          , umwFreezeStartsAt = mcStartTime c
          , umwFreezeEndsAt = mcEndTime c
          , umwFadEndsAt = mcEndTime c + fadLagSeconds
          }
      allWindows =
        map mkWeekend fridays
          <> [mkClosure c | c <- closures, mcEndTime c > mcStartTime c]
      relevant =
        filter
          (\w -> umwFadEndsAt w > now && umwFadStartsAt w <= horizonEnd)
          allWindows
   in sortOn umwFadStartsAt relevant

-- | Build the complete calendar snapshot for a timestamp, reconciling with
-- optional live on-chain @(fadWindow, oracleFrozen)@ flags from PerpsPublicLens.
buildMarketCalendarSnapshot
  :: Integer
  -> [MarketClosure]
  -> Integer
  -> Maybe (Bool, Bool)
  -> Integer
  -> MarketCalendarSnapshot
buildMarketCalendarSnapshot now closures fadRunwaySecs mOnChainFlags horizonDays =
  let (calFad, calFrozen) = evaluateMarketStatus now closures fadRunwaySecs
      (fadActive, oracleFrozen) = case mOnChainFlags of
        Just (onChainFad, onChainFrozen) -> (onChainFad, onChainFrozen)
        Nothing -> (calFad, calFrozen)
      windows = buildUpcomingMarketWindows now closures fadRunwaySecs horizonDays
      activeWindow = find (\w -> now >= umwFadStartsAt w && now < umwFadEndsAt w) windows
      activeFreezeWindow = find (\w -> now >= umwFreezeStartsAt w && now < umwFreezeEndsAt w) windows
      nextUpcomingFad = find (\w -> umwFadStartsAt w > now) windows
      nextUpcomingFreeze = find (\w -> umwFreezeStartsAt w > now) windows
      regime
        | Just w <- activeFreezeWindow, umwKind w == "scheduled_closure" = RegimeScheduledClosure
        | oracleFrozen = RegimeFrozen
        | fadActive = RegimeFadRunway
        | otherwise = RegimeLive
      nextFadStartsAt =
        if fadActive
          then (umwFadStartsAt <$> activeWindow) `orElse` (umwFadStartsAt <$> nextUpcomingFad)
          else umwFadStartsAt <$> nextUpcomingFad
      nextFreezeStartsAt =
        if oracleFrozen
          then (umwFreezeStartsAt <$> activeFreezeWindow) `orElse` (umwFreezeStartsAt <$> nextUpcomingFreeze)
          else umwFreezeStartsAt <$> nextUpcomingFreeze
      nextReopenAt =
        case activeFreezeWindow of
          Just w -> Just (umwFreezeEndsAt w)
          Nothing -> umwFreezeEndsAt <$> listToMaybe windows
      currentFadEndsAt =
        if fadActive
          then umwFadEndsAt <$> activeWindow
          else Nothing
      secsUntilFad =
        if fadActive
          then Just 0
          else max 0 . subtract now <$> nextFadStartsAt
      secsUntilFreeze =
        if oracleFrozen
          then Just 0
          else max 0 . subtract now <$> nextFreezeStartsAt
      activeClosures = filter (\c -> mcEndTime c + fadLagSeconds > now) closures
   in MarketCalendarSnapshot
        { mcsRegime = regime
        , mcsFadWindow = fadActive
        , mcsOracleFrozen = oracleFrozen
        , mcsIsNewYorkDst = isNewYorkDst now
        , mcsNextFadStartsAt = nextFadStartsAt
        , mcsNextFreezeStartsAt = nextFreezeStartsAt
        , mcsNextReopenAt = nextReopenAt
        , mcsCurrentFadEndsAt = currentFadEndsAt
        , mcsSecondsUntilNextFad = secsUntilFad
        , mcsSecondsUntilNextFreeze = secsUntilFreeze
        , mcsFadRunwaySeconds = max 0 fadRunwaySecs
        , mcsScheduledClosures = activeClosures
        , mcsUpcomingWindows = windows
        }
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y
