module Plether.Perps.IndexerOptions
  ( PerpsIndexerInvocation (..)
  , ReplayOptions (..)
  , parsePerpsIndexerInvocation
  , validateReplayOptions
  , maximumReplayBlockSpan
  , minimumReplayStatementTimeoutMs
  , maximumReplayStatementTimeoutMs
  , minimumReplayLockTimeoutMs
  , maximumReplayLockTimeoutMs
  , minimumReplayRuntimeSeconds
  , maximumReplayRuntimeSeconds
  ) where

import Data.Char (ord)
import Data.Int (Int64)
import Data.List (isPrefixOf)

-- | The operational mode selected by the replay-specific command-line parser.
-- Ordinary indexer arguments remain the responsibility of the existing worker
-- parser; this type only separates them from the fail-closed replay command.
data PerpsIndexerInvocation
  = PerpsIndexerLoop
  | PerpsIndexerReplay ReplayOptions
  deriving stock (Show, Eq)

-- | Every replay limit is mandatory. Keeping the limits in the parsed value
-- prevents the caller from silently supplying operational defaults.
data ReplayOptions = ReplayOptions
  { roFromBlock :: Integer
  , roToBlock :: Integer
  , roStatementTimeoutMs :: Int
  , roLockTimeoutMs :: Int
  , roMaxRuntimeSeconds :: Int
  }
  deriving stock (Show, Eq)

maximumReplayBlockSpan :: Integer
maximumReplayBlockSpan = 5_000

minimumReplayStatementTimeoutMs :: Int
minimumReplayStatementTimeoutMs = 1_000

maximumReplayStatementTimeoutMs :: Int
maximumReplayStatementTimeoutMs = 1_800_000

minimumReplayLockTimeoutMs :: Int
minimumReplayLockTimeoutMs = 100

maximumReplayLockTimeoutMs :: Int
maximumReplayLockTimeoutMs = 60_000

minimumReplayRuntimeSeconds :: Int
minimumReplayRuntimeSeconds = 60

maximumReplayRuntimeSeconds :: Int
maximumReplayRuntimeSeconds = 21_600

maximumReplayBlockNumber :: Integer
maximumReplayBlockNumber = toInteger (maxBound :: Int64)

-- | Select replay mode only for the exact @--replay@ command. Normal loop and
-- @--once@ invocations pass through unchanged, while the removed legacy
-- backfill mode always fails instead of falling back to the ordinary loop.
parsePerpsIndexerInvocation :: [String] -> Either String PerpsIndexerInvocation
parsePerpsIndexerInvocation args
  | any isLegacyBackfillToken args =
      Left "Legacy --backfill mode is disabled; use the bounded --replay command"
  | "--replay" `elem` args = parseReplayCommand args
  | any ("--replay" `isPrefixOf`) args =
      Left "Malformed replay command; use the exact --replay token"
  | otherwise = Right PerpsIndexerLoop
 where
  isLegacyBackfillToken token = token == "--backfill" || "--backfill=" `isPrefixOf` token

parseReplayCommand :: [String] -> Either String PerpsIndexerInvocation
parseReplayCommand ("--replay" : args) = do
  partial <- parseReplayFlags emptyPartialReplayOptions args
  options <- completeReplayOptions partial
  PerpsIndexerReplay <$> validateReplayOptions options
parseReplayCommand _ = Left "--replay must be the first command-line argument"

data PartialReplayOptions = PartialReplayOptions
  { proFromBlock :: Maybe Integer
  , proToBlock :: Maybe Integer
  , proStatementTimeoutMs :: Maybe Int
  , proLockTimeoutMs :: Maybe Int
  , proMaxRuntimeSeconds :: Maybe Int
  }

emptyPartialReplayOptions :: PartialReplayOptions
emptyPartialReplayOptions =
  PartialReplayOptions
    { proFromBlock = Nothing
    , proToBlock = Nothing
    , proStatementTimeoutMs = Nothing
    , proLockTimeoutMs = Nothing
    , proMaxRuntimeSeconds = Nothing
    }

parseReplayFlags :: PartialReplayOptions -> [String] -> Either String PartialReplayOptions
parseReplayFlags options = \case
  [] -> Right options
  "--from-block" : rest -> do
    (raw, remaining) <- requireOptionValue "--from-block" rest
    ensureAbsent "--from-block" $ proFromBlock options
    value <- parseBoundedDecimal "--from-block" 0 maximumReplayBlockNumber raw
    parseReplayFlags options {proFromBlock = Just value} remaining
  "--to-block" : rest -> do
    (raw, remaining) <- requireOptionValue "--to-block" rest
    ensureAbsent "--to-block" $ proToBlock options
    value <- parseBoundedDecimal "--to-block" 0 maximumReplayBlockNumber raw
    parseReplayFlags options {proToBlock = Just value} remaining
  "--statement-timeout-ms" : rest -> do
    (raw, remaining) <- requireOptionValue "--statement-timeout-ms" rest
    ensureAbsent "--statement-timeout-ms" $ proStatementTimeoutMs options
    value <- parseBoundedInt "--statement-timeout-ms" minimumReplayStatementTimeoutMs maximumReplayStatementTimeoutMs raw
    parseReplayFlags options {proStatementTimeoutMs = Just value} remaining
  "--lock-timeout-ms" : rest -> do
    (raw, remaining) <- requireOptionValue "--lock-timeout-ms" rest
    ensureAbsent "--lock-timeout-ms" $ proLockTimeoutMs options
    value <- parseBoundedInt "--lock-timeout-ms" minimumReplayLockTimeoutMs maximumReplayLockTimeoutMs raw
    parseReplayFlags options {proLockTimeoutMs = Just value} remaining
  "--max-runtime-seconds" : rest -> do
    (raw, remaining) <- requireOptionValue "--max-runtime-seconds" rest
    ensureAbsent "--max-runtime-seconds" $ proMaxRuntimeSeconds options
    value <- parseBoundedInt "--max-runtime-seconds" minimumReplayRuntimeSeconds maximumReplayRuntimeSeconds raw
    parseReplayFlags options {proMaxRuntimeSeconds = Just value} remaining
  "--replay" : _ -> Left "Duplicate command: --replay"
  option : _ -> Left $ "Unknown replay option: " <> option

requireOptionValue :: String -> [String] -> Either String (String, [String])
requireOptionValue option = \case
  [] -> Left $ "Missing value for replay option: " <> option
  value : rest -> Right (value, rest)

ensureAbsent :: String -> Maybe a -> Either String ()
ensureAbsent option = \case
  Nothing -> Right ()
  Just _ -> Left $ "Duplicate replay option: " <> option

completeReplayOptions :: PartialReplayOptions -> Either String ReplayOptions
completeReplayOptions partial =
  ReplayOptions
    <$> requireParsedOption "--from-block" (proFromBlock partial)
    <*> requireParsedOption "--to-block" (proToBlock partial)
    <*> requireParsedOption "--statement-timeout-ms" (proStatementTimeoutMs partial)
    <*> requireParsedOption "--lock-timeout-ms" (proLockTimeoutMs partial)
    <*> requireParsedOption "--max-runtime-seconds" (proMaxRuntimeSeconds partial)

requireParsedOption :: String -> Maybe a -> Either String a
requireParsedOption option = \case
  Nothing -> Left $ "Missing required replay option: " <> option
  Just value -> Right value

-- | Revalidate a replay value at the execution boundary. This is exported so
-- callers do not have to duplicate parser-only invariants when constructing a
-- value programmatically.
validateReplayOptions :: ReplayOptions -> Either String ReplayOptions
validateReplayOptions options@ReplayOptions {..}
  | roFromBlock < 0 || roFromBlock > maximumReplayBlockNumber =
      Left "--from-block is outside the supported block-number range"
  | roToBlock < 0 || roToBlock > maximumReplayBlockNumber =
      Left "--to-block is outside the supported block-number range"
  | roFromBlock > roToBlock = Left "--from-block must not be greater than --to-block"
  | roToBlock - roFromBlock + 1 > maximumReplayBlockSpan =
      Left $ "Replay range cannot exceed " <> show maximumReplayBlockSpan <> " blocks inclusive"
  | roStatementTimeoutMs < minimumReplayStatementTimeoutMs
      || roStatementTimeoutMs > maximumReplayStatementTimeoutMs =
      Left "--statement-timeout-ms is outside the supported range"
  | roLockTimeoutMs < minimumReplayLockTimeoutMs
      || roLockTimeoutMs > maximumReplayLockTimeoutMs =
      Left "--lock-timeout-ms is outside the supported range"
  | roMaxRuntimeSeconds < minimumReplayRuntimeSeconds
      || roMaxRuntimeSeconds > maximumReplayRuntimeSeconds =
      Left "--max-runtime-seconds is outside the supported range"
  | otherwise = Right options

parseBoundedInt :: String -> Int -> Int -> String -> Either String Int
parseBoundedInt option lower upper raw = do
  value <- parseBoundedDecimal option (toInteger lower) (toInteger upper) raw
  pure $ fromInteger value

parseBoundedDecimal :: String -> Integer -> Integer -> String -> Either String Integer
parseBoundedDecimal option lower upper raw
  | null raw || not (all isAsciiDigit raw) = invalid
  | otherwise =
      case boundedDecimalValue upper raw of
        Just value
          | value >= lower && value <= upper -> Right value
        _ -> invalid
 where
  invalid =
    Left $
      option
        <> " must be an unsigned decimal integer from "
        <> show lower
        <> " through "
        <> show upper

isAsciiDigit :: Char -> Bool
isAsciiDigit character = character >= '0' && character <= '9'

boundedDecimalValue :: Integer -> String -> Maybe Integer
boundedDecimalValue upper = appendDigit 0
 where
  appendDigit value = \case
    [] -> Just value
    character : rest
      | value > (upper - digit) `div` 10 -> Nothing
      | otherwise -> appendDigit (value * 10 + digit) rest
     where
      digit = toInteger (ord character - ord '0')
