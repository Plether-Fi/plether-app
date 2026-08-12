module Plether.Pyth.RevealPayload
  ( PythPayloadAdmission (..)
  , classifyPythPayloadAdmission
  , maxComponentPublishTimeDivergence
  , validatePublishTimes
  , validateLatestPublishTimes
  , validateRevealWindow
  ) where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T

data PythPayloadAdmission
  = AdmitLatestPayload
  | AdmitHistoricalPayload Integer Integer
  deriving stock (Eq, Show)

-- Route and source are independent trust inputs. Requiring them to agree keeps
-- a latest-labelled payload from taking the non-unique parser on a historical
-- reveal route (and vice versa).
classifyPythPayloadAdmission
  :: Maybe (Integer, Integer)
  -> Text
  -> Either Text PythPayloadAdmission
classifyPythPayloadAdmission Nothing "backend_hermes_latest" =
  Right AdmitLatestPayload
classifyPythPayloadAdmission (Just (minimumTs, maximumTs)) source
  | source `elem` ["backend_hermes_historical", "backend_hermes_reveal_backfill"] =
      Right $ AdmitHistoricalPayload minimumTs maximumTs
classifyPythPayloadAdmission Nothing source =
  Left $ "historical Pyth source requires on-chain order reveal bounds: " <> source
classifyPythPayloadAdmission (Just _) source =
  Left $ "latest Pyth source is not valid on a historical reveal route: " <> source

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

validateLatestPublishTimes
  :: Integer -- fetch time
  -> Integer -- maximum permitted age
  -> [Integer]
  -> Either Text (Integer, Integer)
validateLatestPublishTimes fetchedAt maxAge publishTimes = do
  (minimumTs, maximumTs) <- validatePublishTimes publishTimes
  let age = fetchedAt - minimumTs
      futureSkew = maximumTs - fetchedAt
  if age > max 0 maxAge
    then
      Left $
        "latest payload is "
          <> T.pack (show age)
          <> "s old, over "
          <> T.pack (show (max 0 maxAge))
          <> "s policy"
    else if futureSkew > maxComponentPublishTimeDivergence
      then
        Left $
          "latest payload publish time is "
            <> T.pack (show futureSkew)
            <> "s in the future"
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
