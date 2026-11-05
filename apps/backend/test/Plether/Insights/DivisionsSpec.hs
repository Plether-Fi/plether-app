module Plether.Insights.DivisionsSpec (spec) where

import Control.Monad (foldM)
import Data.Aeson (Value, eitherDecodeFileStrict', (.:), withObject)
import Data.Aeson.Types (parseEither, Parser)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM, UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Plether.Ethereum.Rpc (RpcBlock (..))
import Plether.Insights.Divisions
import Test.Hspec

spec :: Spec
spec = describe "Divisions publication" $ do
  it "fixes one contract per week and switches only at the scheduled boundary" $ do
    let cutover = 1793566800
        old = DivisionRelease "0x9b9ddc783780a14e4930dbbcd94d6356a82725e8" "v1.3.1" "USDC"
        new = DivisionRelease "0x918663d6da8cb33b325a04c4a5e8c51d42f3b444" "v2.0.0" "USDG"
    divisionReleaseForWeek (cutover - weekSeconds) `shouldBe` Just old
    divisionReleaseForWeek cutover `shouldBe` Just new
    divisionReleaseForWeek (cutover + weekSeconds) `shouldBe` Just new
    divisionReleaseForWeek (cutover - 1) `shouldBe` Nothing
    divisionReleaseForWeek (firstWeekStart - weekSeconds) `shouldBe` Nothing
    divisionWeekStart (cutover - 1) `shouldBe` cutover - weekSeconds
    divisionWeekStart cutover `shouldBe` cutover
  it "carries prior wallets into the next release and rejects a missing snapshot" $ do
    let publish n prior scores = publishDivisionWeek (firstWeekStart + n * weekSeconds)
          (publicationTime (firstWeekStart + n * weekSeconds)) (baseline n) (closing n) prior scores
    case foldM (\prior n -> Just <$> publish n prior [score 1 100 10 2]) Nothing [0,1,2] of
      Left err -> expectationFailure err
      Right Nothing -> expectationFailure "missing prior publication"
      Right (Just prior) -> do
        publishedWallets prior `shouldBe` Right [address 1]
        publish 3 (Just prior) [] `shouldSatisfy` isLeft
        case publish 3 (Just prior) [score 1 0 0 0,score 2 100 10 2] of
          Left err -> expectationFailure err
          Right next -> publishedWallets next `shouldBe` Right [address 1,address 2]

  it "matches the frontend engine across empty weeks, ties, one-day traders, Diamond promotions and two-week demotions" $ do
    decoded <- eitherDecodeFileStrict' "test/fixtures/divisions-publications.json"
    case decoded of
      Left err -> expectationFailure err
      Right fixtures -> do
        _ <- foldM check Nothing (fixtures :: [Value])
        pure ()
  it "refuses premature publication, skipped weeks, duplicate wallets and missing prior wallets" $ do
    let start = firstWeekStart
        a = score 1 100 10 2
        publish index now prior scores = publishDivisionWeek (start + index * weekSeconds) now
          (baseline index) (closing index) prior scores
        now = publicationTime start
    publish 0 (now - 1) Nothing [a] `shouldSatisfy` isLeft
    publish 1 (now + weekSeconds) Nothing [a] `shouldSatisfy` isLeft
    publish 0 now Nothing [a,a] `shouldSatisfy` isLeft
    case publish 0 now Nothing [a] of
      Left err -> expectationFailure err
      Right prior -> do
        publish 1 (now + weekSeconds) (Just prior) [] `shouldSatisfy` isLeft
        publish 2 (now + 2 * weekSeconds) (Just prior) [a] `shouldSatisfy` isLeft
  it "ranks exact ratios even when displayed returns would round to the same value" $ do
    map (lsAddress . fst) (rankScores [score 1 100000000 10000000 2, score 2 100000000 10000001 2])
      `shouldBe` [address 2, address 1]
  it "keeps nonpositive denominators unranked" $ do
    map snd (rankScores [score 1 0 10 2, score 2 (-100) 10 2]) `shouldBe` [Nothing, Nothing]
  it "requires valid finalized boundary identities" $ do
    publishDivisionWeek firstWeekStart (publicationTime firstWeekStart)
      (baseline 0) ((closing 0) { rpcBlockHash = "bad" }) Nothing [] `shouldSatisfy` isLeft

address :: Int -> Text
address n = "0x" <> T.justifyRight 40 '0' (T.pack $ show n)
score :: Int -> Integer -> Integer -> Int -> LiveScore
score n = LiveScore (address n)
baseline, closing :: Integer -> RpcBlock
baseline n = RpcBlock (100 + n * 100) Nothing ("0x" <> T.replicate 64 "a") (firstWeekStart + n * weekSeconds - 1)
closing n = RpcBlock (199 + n * 100) Nothing ("0x" <> T.replicate 64 "b") (firstWeekStart + n * weekSeconds + 5 * 86400 - 1)

check :: Maybe Value -> Value -> IO (Maybe Value)
check previous fixture = case parseEither parseFixture fixture of
  Left err -> expectationFailure err >> pure previous
  Right (start, now, opening, end, scores, expected) -> do
    publishDivisionWeek start now opening end previous scores `shouldBe` Right expected
    pure $ Just expected

parseFixture :: Value -> Parser (Integer, Integer, RpcBlock, RpcBlock, [LiveScore], Value)
parseFixture = withObject "fixture" $ \f -> do
  expected <- f .: "expected"
  (start, now, opening, end, scores) <- f .: "input" >>= withObject "input" (\i -> do
    start <- i .: "startsAt" >>= timestamp
    now <- i .: "publishedAt" >>= timestamp
    opening <- i .: "baselineBlock" >>= block (start - 1)
    end <- i .: "closingBlock" >>= block (start + 5 * 86400 - 1)
    scores <- i .: "accounts" >>= traverse (withObject "account" $ \a -> do
      wallet <- a .: "address"
      initial <- read <$> a .: "startingEquityAssets"
      final <- read <$> a .: "closingEquityAssets"
      deposits <- read <$> a .: "depositsAssets"
      withdrawals <- read <$> a .: "withdrawalsAssets"
      trades <- a .: "executedTradeTimes" :: Parser [Text]
      let adjusted = initial + deposits - withdrawals
      pure $ LiveScore wallet adjusted (final - adjusted) (length trades))
    pure (start, now, opening, end, scores))
  pure (start, now, opening, end, scores, expected)
  where
    timestamp t = maybe (fail "Invalid fixture date") (pure . floor . utcTimeToPOSIXSeconds)
      (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" t :: Maybe UTCTime)
    block time = withObject "block" $ \b -> RpcBlock <$> (read <$> b .: "number") <*> pure Nothing <*> b .: "hash" <*> pure time
