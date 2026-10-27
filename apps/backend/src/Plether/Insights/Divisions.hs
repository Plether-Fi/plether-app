-- Persistent weekly badges. This is the backend counterpart of the Insights
-- publication engine; shared golden fixtures keep their rules identical.
module Plether.Insights.Divisions
  ( LiveScore (..), rankScores, publishDivisionWeek, firstWeekStart, weekSeconds
  , publicationTime, divisionTimestamp
  ) where

import Control.Monad (unless)
import Data.Aeson (Value (..), object, (.=), (.:), withObject)
import Data.Aeson.Types (Parser, parseEither)
import Data.Char (isHexDigit)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, parseTimeM, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Plether.Ethereum.Rpc (RpcBlock (..))
import Text.Read (readMaybe)

firstWeekStart, weekSeconds :: Integer
firstWeekStart = 1791752400 -- Sunday 2026-10-11 21:00 UTC
weekSeconds = 7 * 86400

publicationTime :: Integer -> Integer
publicationTime start = start + weekSeconds + 3 * 3600

divisionTimestamp :: Integer -> Text
divisionTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.000Z" . posixSecondsToUTCTime . fromInteger

data LiveScore = LiveScore
  { lsAddress :: Text, lsAdjustedEquity :: Integer, lsPnl :: Integer, lsActiveDays :: Int
  } deriving stock (Show, Eq)

rankScores :: [LiveScore] -> [(LiveScore, Maybe Int)]
rankScores scores = zip sorted $ go 1 Nothing sorted
  where
    ratio a b = compare (lsPnl b * lsAdjustedEquity a) (lsPnl a * lsAdjustedEquity b)
    compareScore a b
      | lsAdjustedEquity a <= 0 && lsAdjustedEquity b <= 0 = comparing lsAddress a b
      | lsAdjustedEquity a <= 0 = GT
      | lsAdjustedEquity b <= 0 = LT
      | otherwise = ratio a b <> comparing lsAddress a b
    sorted = sortBy compareScore scores
    go _ _ [] = []
    go index prior (score:rest) =
      let rank = if lsAdjustedEquity score <= 0 then Nothing else case prior of
            Just (previous, previousRank) | ratio previous score == EQ -> previousRank
            _ -> Just index
       in rank : go (index + 1) (Just (score, rank)) rest

tiers :: [Text]
tiers = ["Iron", "Bronze", "Silver", "Gold", "Platinum", "Diamond"]

-- Parse only the immutable state needed to advance, and reject corrupt state.
previousState :: Value -> Parser (Integer, Integer, Map.Map Text (Int, Int))
previousState = withObject "publication" $ \o -> do
  version <- o .: "version"
  unless (version == (1 :: Int)) $ fail "Invalid publication version"
  startText <- o .: "week" >>= withObject "week" (.: "startsAt")
  start <- maybe (fail "Invalid previous week") (pure . floor . utcTimeToPOSIXSeconds)
    (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" startText :: Maybe UTCTime)
  closing <- o .: "source" >>= withObject "source" (.: "closingBlock")
    >>= withObject "block" (.: "number")
  block <- maybe (fail "Invalid previous block") pure (readMaybe closing)
  rows <- o .: "standings" >>= traverse (withObject "standing" $ \r -> do
    address <- r .: "address"
    tier <- r .: "tier"
    index <- maybe (fail "Invalid previous tier") pure $ lookup tier (zip tiers [0..])
    streak <- r .: "nonQualifyingWeeks"
    unless (streak >= 0 && streak <= 1) $ fail "Invalid failure streak"
    pure (address, (index, streak)))
  unless (Map.size (Map.fromList rows) == length rows) $ fail "Duplicate previous wallet"
  pure (start, block, Map.fromList rows)

publishDivisionWeek :: Integer -> Integer -> RpcBlock -> RpcBlock -> Maybe Value -> [LiveScore] -> Either String Value
publishDivisionWeek start now baseline closing previous scores = do
  (previousStart, previousBlock, prior) <- maybe (Right (firstWeekStart - weekSeconds, -1, Map.empty))
    (parseEither previousState) previous
  unless (start == previousStart + weekSeconds && start >= firstWeekStart && (start - firstWeekStart) `mod` weekSeconds == 0) $
    Left "Publish every week in order"
  unless (now >= publicationTime start) $ Left "Final badges cannot be published before Monday"
  unless (rpcBlockNumber closing > rpcBlockNumber baseline && rpcBlockNumber baseline > previousBlock
      && rpcBlockTimestamp baseline < start && rpcBlockTimestamp closing >= start
      && rpcBlockTimestamp closing < start + 5 * 86400
      && all validBlock [baseline, closing]) $ Left "Invalid canonical boundary blocks"
  let addresses = Set.fromList (map lsAddress scores)
  unless (Set.size addresses == length scores && all validScore scores) $ Left "Invalid or duplicate wallet score"
  unless (Map.keysSet prior `Set.isSubsetOf` addresses) $ Left "Missing weekly snapshot; missing data is not inactivity"
  let ranked = rankScores scores
      profitable = length [() | (s, Just _) <- ranked, lsPnl s > 0]
      target rank = case [index | (index, percent) <- [(5,5),(4,10),(3,20),(2,50)], rank <= (profitable * percent + 99) `div` 100] of
        index:_ -> index
        [] -> 1
      advance (score, rank) =
        let address = lsAddress score
            (old, streak) = Map.findWithDefault (0,0) address prior
            qualifies = lsAdjustedEquity score > 0 && lsPnl score > 0 && lsActiveDays score >= 2
            next | qualifies = maybe old (\r -> min (old + 1) (max old (target r))) rank
                 | streak == 1 = max 0 (old - 1)
                 | otherwise = old
            nextStreak = if qualifies || streak == 1 then 0 else 1 :: Int
            movement | next > old = "promoted" :: Text
                     | next < old = "demoted"
                     | otherwise = "unchanged"
            row = object
              [ "address" .= address, "displayName" .= Null, "tier" .= (tiers !! next)
              , "previousTier" .= (tiers !! old), "nonQualifyingWeeks" .= nextStreak
              , "weeklyPnlUsdc" .= show (lsPnl score), "adjustedEquityUsdc" .= show (lsAdjustedEquity score)
              , "activeDays" .= lsActiveDays score, "qualifying" .= qualifies, "rank" .= rank, "movement" .= movement
              ]
         in (address, old, next, row)
      advanced = map advance ranked
      boundary block = object ["number" .= show (rpcBlockNumber block), "hash" .= rpcBlockHash block]
  pure $ object
    [ "version" .= (1 :: Int)
    , "source" .= object ["baselineBlock" .= boundary baseline, "closingBlock" .= boundary closing]
    , "week" .= object ["startsAt" .= divisionTimestamp start, "closesAt" .= divisionTimestamp (start + 5 * 86400), "publishesAt" .= divisionTimestamp (publicationTime start)]
    , "publishedAt" .= divisionTimestamp now
    , "standings" .= [row | (_,_,_,row) <- advanced]
    , "announcement" .= object
        [ "tradersOfTheWeek" .= take 3 [lsAddress score | (score, Just _) <- ranked, lsPnl score > 0]
        , "promotions" .= [object ["address" .= address, "tier" .= (tiers !! next)] | (address,old,next,_) <- advanced, next > old, next >= 3]
        , "diamondRoster" .= [address | (address,_,next,_) <- advanced, next == 5]
        ]
    ]
  where
    validHex n t = T.length t == n + 2 && T.take 2 t == "0x" && T.all isHexDigit (T.drop 2 t)
    validBlock block = rpcBlockNumber block >= 0 && validHex 64 (rpcBlockHash block)
    validScore score = validHex 40 (lsAddress score) && T.toLower (lsAddress score) == lsAddress score
      && lsActiveDays score >= 0 && lsActiveDays score <= 5
