module Plether.Handlers.Perps
  ( getBasketHistory
  ) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema (BasketSnapshotRow (..), getBasketSnapshots)
import Plether.Types

getBasketHistory
  :: DbPool
  -> Config
  -> BasketHistoryParams
  -> IO (Either ApiError (ApiResponse BasketHistory))
getBasketHistory pool cfg params = do
  now <- getPOSIXTime
  let nowUnix = round now
      fromUnix = nowUnix - basketRangeSeconds (bhpRange params)
      interval = max 60 (bhpIntervalSeconds params)
      maxPoints = fromIntegral ((basketRangeSeconds (bhpRange params) `div` interval) + 4)

  rows <- withDb pool $ \conn ->
    getBasketSnapshots conn fromUnix nowUnix maxPoints

  let points = map rowToPoint rows
      latest = case reverse rows of
        row : _ -> Just (bsrBasketPrice row)
        [] -> Nothing
      changePct = computeChange rows
      history =
        BasketHistory
          { bhRange = bhpRange params
          , bhIntervalSeconds = interval
          , bhSource = "pyth_benchmarks"
          , bhGeneratedAt = now
          , bhLatestPrice = latest
          , bhChangePct = changePct
          , bhPoints = points
          }

  pure $ Right $ mkResponse 0 (cfgChainId cfg) history

rowToPoint :: BasketSnapshotRow -> BasketHistoryPoint
rowToPoint BasketSnapshotRow {..} =
  BasketHistoryPoint
    { bhpTimestamp = bsrTimestamp
    , bhpBasketPrice = bsrBasketPrice
    , bhpComponents = bsrComponents
    }

computeChange :: [BasketSnapshotRow] -> Maybe Double
computeChange rows =
  case (rows, reverse rows) of
    (first : _, lastRow : _) | bsrBasketPrice first > 0 ->
      Just $
        (fromIntegral (bsrBasketPrice lastRow - bsrBasketPrice first) / fromIntegral (bsrBasketPrice first) :: Double)
    _ -> Nothing
