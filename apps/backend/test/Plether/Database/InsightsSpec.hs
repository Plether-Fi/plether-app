module Plether.Database.InsightsSpec (spec) where

import Data.List (isInfixOf)
import Database.PostgreSQL.Simple (Query)
import Plether.Database.Insights
  ( leaderboardQuerySql
  , leaderboardSearchPattern
  , walletActivityQuerySql
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "leaderboardSearchPattern" $ do
    it "builds a case-insensitive substring pattern" $
      leaderboardSearchPattern "Alice" `shouldBe` "%Alice%"

    it "escapes LIKE wildcards and the escape character" $
      leaderboardSearchPattern "team_100%!" `shouldBe` "%team!_100!%!!%"

  describe "competition result SQL" $ do
    it "ranks prizes only among reviewed, mechanically qualified participants" $ do
      queryContains leaderboardQuerySql "eligibility_status = 'eligible'"
      queryContains leaderboardQuerySql "active_days >= competition_minimum_active_days"
      queryContains leaderboardQuerySql "pc.prize_place <= 3"

    it "uses one published snapshot batch as the event projection upper bound" $ do
      queryContains leaderboardQuerySql "a.block_number <= cb.block_number"
      queryContains walletActivityQuerySql "a.block_number <= b.block_number"

    it "accepts legacy clearinghouse flows whose indexed JSON predates asset metadata" $ do
      queryContains leaderboardQuerySql "a.activity_type IN ('Deposit', 'Withdraw')"
      queryContains leaderboardQuerySql "OR NOT jsonb_exists(a.data, 'asset')"

queryContains :: Query -> String -> Expectation
queryContains sql fragment =
  show sql `shouldSatisfy` isInfixOf fragment
