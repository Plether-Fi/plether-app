module Plether.Pyth.RevealPayload
  ( maxComponentPublishTimeDivergence
  , validatePublishTimes
  , validateRevealWindow
  ) where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T

maxComponentPublishTimeDivergence :: Integer
maxComponentPublishTimeDivergence = 5

validatePublishTimes :: [Integer] -> Either Text (Integer, Integer)
validatePublishTimes [] = Left "Hermes update did not include feed publish times"
validatePublishTimes publishTimes =
  let (minimumTs, maximumTs) =
        foldl'
          (\(lo, hi) value -> (min lo value, max hi value))
          (head publishTimes, head publishTimes)
          publishTimes
   in if maximumTs - minimumTs > maxComponentPublishTimeDivergence
        then
          Left $
            "component publish times diverged by "
              <> T.pack (show (maximumTs - minimumTs))
              <> "s, over "
              <> T.pack (show maxComponentPublishTimeDivergence)
              <> "s policy"
        else Right (minimumTs, maximumTs)

validateRevealWindow
  :: Integer -- commit time
  -> Integer -- order settlement window
  -> [Integer]
  -> Either Text (Integer, Integer)
validateRevealWindow commitTime settlementWindow publishTimes = do
  let minAllowed = commitTime + 1
      maxAllowed = commitTime + settlementWindow
  (minimumTs, maximumTs) <- validatePublishTimes publishTimes
  if minimumTs < minAllowed
    then Left "payload publish time is before the reveal window"
    else if minimumTs > minAllowed
      then Left "payload publish time is after the first reveal tick"
      else if maximumTs > maxAllowed
        then Left "payload publish time is after the reveal window"
        else Right (minimumTs, maximumTs)
