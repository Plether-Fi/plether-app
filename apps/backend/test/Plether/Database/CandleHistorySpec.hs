module Plether.Database.CandleHistorySpec (spec) where

import qualified Data.ByteString.Char8 as BS8
import Data.Char (isAlphaNum, isSpace)
import Data.Either (isLeft)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple.Types (Query (..))
import Plether.Database.CandleHistory
  ( CandleHistoryIngestionProgress (..)
  , CandleHistorySelection (..)
  , MarketReleaseEpoch (..)
  , candleHistorySchemaStatements
  , defaultCandleMarketId
  , effectiveHistoryStart
  , releaseEpochAtBlock
  , validateCandleHistoryIngestionCompletion
  , validateCandleHistorySelection
  , validateMarketReleaseEpoch
  , validateMarketReleaseEpochSequence
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "operator-selected candle history" $ do
    it "uses one explicit logical-market identity independent of releases" $
      defaultCandleMarketId `shouldBe` "dxy-perps-v1"

    it "accepts an arbitrary second and aligns each interval independently" $ do
      validateCandleHistorySelection selection `shouldBe` Right ()
      effectiveHistoryStart 60 120 `shouldBe` Right 120
      effectiveHistoryStart 60 121 `shouldBe` Right 180
      effectiveHistoryStart 3_600 3_601 `shouldBe` Right 7_200

    it "rejects invalid time domains and unauditable selections" $ do
      effectiveHistoryStart 0 120 `shouldSatisfy` isLeft
      effectiveHistoryStart 60 (-1) `shouldSatisfy` isLeft
      validateCandleHistorySelection selection {chsMarketId = "DXY"}
        `shouldSatisfy` isLeft
      validateCandleHistorySelection selection {chsRevision = 0}
        `shouldSatisfy` isLeft
      validateCandleHistorySelection selection {chsRequestedBy = "  "}
        `shouldSatisfy` isLeft
      validateCandleHistorySelection selection {chsRequestReference = ""}
        `shouldSatisfy` isLeft

    it "requires exact canonical minute-grid progress before publication" $ do
      validateCandleHistoryIngestionCompletion selection completedProgress
        `shouldBe` Right ()
      validateCandleHistoryIngestionCompletion
        selection
        completedProgress {chipTargetRevision = 2}
        `shouldSatisfy` isLeft
      validateCandleHistoryIngestionCompletion
        selection
        completedProgress {chipSampleIntervalSeconds = 180}
        `shouldSatisfy` isLeft
      validateCandleHistoryIngestionCompletion
        selection
        completedProgress {chipComplete = False, chipNextTimestamp = 1_700_000_060}
        `shouldSatisfy` isLeft
      validateCandleHistoryIngestionCompletion
        selection
        completedProgress {chipLastError = Just "partial component window"}
        `shouldSatisfy` isLeft

  describe "immutable logical-market releases" $ do
    it "accepts contiguous releases in activation order" $
      validateMarketReleaseEpochSequence releases `shouldBe` Right ()

    it "assigns a transition block to the successor without overlap" $ do
      fmap mreReleaseRouter <$> releaseEpochAtBlock releases 99
        `shouldBe` Right Nothing
      fmap mreReleaseRouter <$> releaseEpochAtBlock releases 199
        `shouldBe` Right (Just firstRouter)
      fmap mreReleaseRouter <$> releaseEpochAtBlock releases 200
        `shouldBe` Right (Just secondRouter)

    it "rejects revision gaps, invalid genesis, and noncanonical ordering" $ do
      validateMarketReleaseEpochSequence
        [firstRelease, secondRelease {mreRevision = 3}]
        `shouldSatisfy` isLeft
      validateMarketReleaseEpochSequence
        [firstRelease {mreIsMarketGenesis = False}]
        `shouldSatisfy` isLeft
      validateMarketReleaseEpochSequence
        [firstRelease, secondRelease {mreIsMarketGenesis = True}]
        `shouldSatisfy` isLeft
      validateMarketReleaseEpochSequence
        [firstRelease, secondRelease {mreActivationBlock = 100}]
        `shouldSatisfy` isLeft
      validateMarketReleaseEpochSequence
        [firstRelease, secondRelease {mreActivationTimestamp = 999}]
        `shouldSatisfy` isLeft

    it "rejects mixed markets, chains, routers, and malformed evidence" $ do
      validateMarketReleaseEpoch firstRelease `shouldBe` Right ()
      validateMarketReleaseEpochSequence
        [firstRelease, secondRelease {mreMarketId = "other-market"}]
        `shouldSatisfy` isLeft
      validateMarketReleaseEpochSequence
        [firstRelease, secondRelease {mreChainId = 1}]
        `shouldSatisfy` isLeft
      validateMarketReleaseEpochSequence
        [firstRelease, secondRelease {mreReleaseRouter = firstRouter}]
        `shouldSatisfy` isLeft
      validateMarketReleaseEpoch firstRelease {mreReleaseRouter = T.toUpper firstRouter}
        `shouldSatisfy` isLeft
      validateMarketReleaseEpoch firstRelease {mreRevision = 2}
        `shouldSatisfy` isLeft
      validateMarketReleaseEpoch firstRelease {mreActivationBlock = 89}
        `shouldSatisfy` isLeft
      validateMarketReleaseEpoch firstRelease {mreCfdEngine = "0x1"}
        `shouldSatisfy` isLeft
      validateMarketReleaseEpoch firstRelease {mreDeploymentBlockHash = blockHash '0'}
        `shouldSatisfy` isLeft
      validateMarketReleaseEpoch firstRelease {mreApprovalReference = " "}
        `shouldSatisfy` isLeft

  describe "history schema" $ do
    it "contains immutable targets, resumable ingestion proof, and release metadata" $ do
      schemaContains "CREATE TABLE IF NOT EXISTS perps_candle_markets"
      schemaContains "CREATE TABLE IF NOT EXISTS perps_candle_history_targets"
      schemaContains "CREATE TABLE IF NOT EXISTS perps_candle_history_ingestions"
      schemaContains "CREATE TABLE IF NOT EXISTS perps_candle_history_ingestion_windows"
      schemaContains "CREATE TABLE IF NOT EXISTS perps_market_release_epochs"
      schemaContains "candle market identity is immutable"
      schemaContains "candle history targets are immutable; append a revision"
      schemaContains "candle history target must append the next revision"
      schemaContains "candle history publication is immutable"
      schemaContains "published_generation"
      schemaContains "market release epochs are immutable; append a successor epoch"
      schemaContains "market release must append the next revision"
      schemaContains "UNIQUE (market_id, activation_block)"
      schemaContains "UNIQUE (chain_id, release_router)"
      schemaDoesNotContain "perps_market_release_progress"
      schemaDoesNotContain "release_head_activation_block"
      schemaDoesNotContain "effective_end_block"
      schemaDoesNotContain "addresses_configuration"

    it "keeps the static schema aligned with the runtime migration" $ do
      schema <- readFile "schema.sql"
      staticFoundation <-
        case markedSqlSection schema of
          Just section -> pure section
          Nothing -> expectationFailure "Static candle-history schema markers are missing" >> pure ""
      let runtimeFoundation =
            unlines
              [ BS8.unpack (fromQuery statement) <> ";"
              | statement <- candleHistorySchemaStatements
              ]
      case firstSqlDifference (sqlTokensNormalized staticFoundation) (sqlTokensNormalized runtimeFoundation) of
        Nothing -> pure ()
        Just mismatch -> expectationFailure mismatch

    it "installs the generation monotonicity guard in the static bootstrap" $ do
      schema <- readFile "schema.sql"
      schema `shouldContain` "protect_perps_rollup_generation_monotonic"
      schema `shouldContain` "perps rollup usability regression requires a new generation"

selection :: CandleHistorySelection
selection =
  CandleHistorySelection
    { chsMarketId = marketId
    , chsRevision = 1
    , chsRequestedStartTimestamp = 1_700_000_001
    , chsRequestedBy = "operator@example.com"
    , chsRequestReference = "change-42"
    }

completedProgress :: CandleHistoryIngestionProgress
completedProgress =
  CandleHistoryIngestionProgress
    { chipMarketId = marketId
    , chipTargetRevision = 1
    , chipStartTimestamp = 1_700_000_040
    , chipEndTimestampExclusive = 1_700_086_440
    , chipNextTimestamp = 1_700_086_440
    , chipSampleIntervalSeconds = 60
    , chipComplete = True
    , chipLastError = Nothing
    , chipPublishedGeneration = Nothing
    }

releases :: [MarketReleaseEpoch]
releases = [firstRelease, secondRelease]

firstRelease :: MarketReleaseEpoch
firstRelease = release 1 firstRouter 90 100 1_000 True

secondRelease :: MarketReleaseEpoch
secondRelease = release 2 secondRouter 190 200 2_000 False

release
  :: Integer
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Bool
  -> MarketReleaseEpoch
release revision router deploymentBlock activationBlock activationTimestamp isGenesis =
  MarketReleaseEpoch
    { mreMarketId = marketId
    , mreRevision = revision
    , mreChainId = 421_614
    , mreReleaseRouter = router
    , mreCfdEngine = "0x" <> T.replicate 40 "3"
    , mreMarginClearinghouse = "0x" <> T.replicate 40 "4"
    , mreDeploymentBlock = deploymentBlock
    , mreDeploymentBlockHash = blockHash 'a'
    , mreDeploymentTransactionHash = blockHash 'b'
    , mreActivationBlock = activationBlock
    , mreActivationTimestamp = activationTimestamp
    , mreActivationBlockHash = blockHash 'c'
    , mreApprovalReference = "release-approval"
    , mreIsMarketGenesis = isGenesis
    }

marketId :: Text
marketId = defaultCandleMarketId

firstRouter :: Text
firstRouter = "0x" <> T.replicate 40 "1"

secondRouter :: Text
secondRouter = "0x" <> T.replicate 40 "2"

blockHash :: Char -> Text
blockHash digit = "0x" <> T.replicate 64 (T.singleton digit)

schemaContains :: String -> Expectation
schemaContains fragment =
  map show candleHistorySchemaStatements
    `shouldSatisfy` any (isInfixOf fragment)

schemaDoesNotContain :: String -> Expectation
schemaDoesNotContain fragment =
  map show candleHistorySchemaStatements
    `shouldSatisfy` all (not . isInfixOf fragment)

markedSqlSection :: String -> Maybe String
markedSqlSection schema =
  case dropWhile (/= beginMarker) $ lines schema of
    [] -> Nothing
    (_ : afterBegin) ->
      case break (== endMarker) afterBegin of
        (_, []) -> Nothing
        (section, _ : _) -> Just $ unlines section
 where
  beginMarker = "-- BEGIN PERPS CANDLE HISTORY FOUNDATION"
  endMarker = "-- END PERPS CANDLE HISTORY FOUNDATION"

sqlTokensNormalized :: String -> [String]
sqlTokensNormalized =
  sqlTokens
    . unlines
    . filter (not . isPrefixOf "--" . dropWhile isSpace)
    . lines

sqlTokens :: String -> [String]
sqlTokens [] = []
sqlTokens (character : remaining)
  | isSpace character = sqlTokens remaining
  | character == '\'' =
      let (quoted, rest) = consumeSqlQuote remaining
       in ('\'' : quoted) : sqlTokens rest
  | isSqlWordCharacter character =
      let (wordTail, rest) = span isSqlWordCharacter remaining
       in (character : wordTail) : sqlTokens rest
  | otherwise = [character] : sqlTokens remaining
 where
  isSqlWordCharacter value = isAlphaNum value || value == '_'

consumeSqlQuote :: String -> (String, String)
consumeSqlQuote [] = ([], [])
consumeSqlQuote ('\'' : '\'' : remaining) =
  let (quoted, rest) = consumeSqlQuote remaining
   in ('\'' : '\'' : quoted, rest)
consumeSqlQuote ('\'' : remaining) = ("'", remaining)
consumeSqlQuote (character : remaining) =
  let (quoted, rest) = consumeSqlQuote remaining
   in (character : quoted, rest)

firstSqlDifference :: [String] -> [String] -> Maybe String
firstSqlDifference = go 1
 where
  go :: Int -> [String] -> [String] -> Maybe String
  go _ [] [] = Nothing
  go position leftTokens rightTokens =
    case (leftTokens, rightTokens) of
      (left : leftRest, right : rightRest)
        | left == right -> go (position + 1) leftRest rightRest
      _ ->
        Just $
          "Static/runtime candle-history SQL diverged at token "
            <> show position
            <> "; static="
            <> show (take 8 leftTokens)
            <> "; runtime="
            <> show (take 8 rightTokens)
