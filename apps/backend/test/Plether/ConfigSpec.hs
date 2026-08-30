module Plether.ConfigSpec (spec) where

import Plether.Config
  ( FaucetGuardConfig (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  , parsePerpsCandleReadIntervals
  , parsePerpsCandleReadMode
  , parsePerpsCandleWriteMode
  , perpsCandleRollupReadEnabled
  , validateInsightsCompetitionActivation
  , validateFaucetGuardConfig
  , validatePerpsCandleModeCombination
  )
import Plether.Insights.Competition
  ( crSlug
  , july2026CompetitionSlug
  , september2026CompetitionSlug
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "Insights competition activation" $ do
    it "keeps the historical competition as the no-release-change default" $ do
      fmap crSlug
        (validateInsightsCompetitionActivation july2026CompetitionSlug Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
        `shouldBe` Right july2026CompetitionSlug

    it "allows September registration before a release is bound" $ do
      fmap crSlug
        (validateInsightsCompetitionActivation september2026CompetitionSlug Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
        `shouldBe` Right september2026CompetitionSlug

    it "rejects the September slug when any address inherits the July release" $ do
      validateInsightsCompetitionActivation
        september2026CompetitionSlug
        (Just "testnet-trading-2026-09")
        Nothing
        (Just "0x1111111111111111111111111111111111111111")
        (Just "0x2222222222222222222222222222222222222222")
        (Just "0x7777777777777777777777777777777777777777")
        (Just "0x8888888888888888888888888888888888888888")
        (Just "0x3333333333333333333333333333333333333333")
        (Just "0x4444444444444444444444444444444444444444")
        (Just "0x5555555555555555555555555555555555555555")
        (Just "300000000")
        `shouldSatisfy` isLeft
      validateInsightsCompetitionActivation
        september2026CompetitionSlug
        (Just "testnet-trading-2026-09")
        (Just "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3")
        (Just "0x1111111111111111111111111111111111111111")
        (Just "0x2222222222222222222222222222222222222222")
        (Just "0x7777777777777777777777777777777777777777")
        (Just "0x8888888888888888888888888888888888888888")
        (Just "0x3333333333333333333333333333333333333333")
        (Just "0x4444444444444444444444444444444444444444")
        (Just "0x5555555555555555555555555555555555555555")
        (Just "300000000")
        `shouldSatisfy` isLeft

    it "rejects a July address reused in a different September role" $ do
      validateInsightsCompetitionActivation
        september2026CompetitionSlug
        (Just "testnet-trading-2026-09")
        (Just "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E")
        (Just "0x1111111111111111111111111111111111111111")
        (Just "0x2222222222222222222222222222222222222222")
        (Just "0x7777777777777777777777777777777777777777")
        (Just "0x8888888888888888888888888888888888888888")
        (Just "0x3333333333333333333333333333333333333333")
        (Just "0x4444444444444444444444444444444444444444")
        (Just "0x5555555555555555555555555555555555555555")
        (Just "300000000")
        `shouldSatisfy` isLeft

    it "requires distinct addresses for all September release roles" $ do
      validateInsightsCompetitionActivation
        september2026CompetitionSlug
        (Just "testnet-trading-2026-09")
        (Just "0x1111111111111111111111111111111111111111")
        (Just "0x1111111111111111111111111111111111111111")
        (Just "0x2222222222222222222222222222222222222222")
        (Just "0x7777777777777777777777777777777777777777")
        (Just "0x8888888888888888888888888888888888888888")
        (Just "0x3333333333333333333333333333333333333333")
        (Just "0x4444444444444444444444444444444444444444")
        (Just "0x5555555555555555555555555555555555555555")
        (Just "300000000")
        `shouldSatisfy` isLeft

    it "accepts the September slug only with an explicit release ID, address bundle, and indexer start" $ do
      fmap crSlug
        ( validateInsightsCompetitionActivation
            september2026CompetitionSlug
            (Just "testnet-trading-2026-09")
            (Just "0x1111111111111111111111111111111111111111")
            (Just "0x2222222222222222222222222222222222222222")
            (Just "0x3333333333333333333333333333333333333333")
            (Just "0x7777777777777777777777777777777777777777")
            (Just "0x8888888888888888888888888888888888888888")
            (Just "0x4444444444444444444444444444444444444444")
            (Just "0x5555555555555555555555555555555555555555")
            (Just "0x6666666666666666666666666666666666666666")
            (Just "300000000")
        )
        `shouldBe` Right september2026CompetitionSlug

    it "rejects the zero address anywhere in the September release bundle" $ do
      validateInsightsCompetitionActivation
        september2026CompetitionSlug
        (Just "testnet-trading-2026-09")
        (Just "0x0000000000000000000000000000000000000000")
        (Just "0x2222222222222222222222222222222222222222")
        (Just "0x3333333333333333333333333333333333333333")
        (Just "0x7777777777777777777777777777777777777777")
        (Just "0x8888888888888888888888888888888888888888")
        (Just "0x4444444444444444444444444444444444444444")
        (Just "0x5555555555555555555555555555555555555555")
        (Just "0x6666666666666666666666666666666666666666")
        (Just "300000000")
        `shouldSatisfy` isLeft

    it "rejects a zero indexer start for the September release" $ do
      validateInsightsCompetitionActivation
        september2026CompetitionSlug
        (Just "testnet-trading-2026-09")
        (Just "0x1111111111111111111111111111111111111111")
        (Just "0x2222222222222222222222222222222222222222")
        (Just "0x3333333333333333333333333333333333333333")
        (Just "0x7777777777777777777777777777777777777777")
        (Just "0x8888888888888888888888888888888888888888")
        (Just "0x4444444444444444444444444444444444444444")
        (Just "0x5555555555555555555555555555555555555555")
        (Just "0x6666666666666666666666666666666666666666")
        (Just "0")
        `shouldSatisfy` isLeft

    it "rejects unversioned or unknown competition slugs" $ do
      validateInsightsCompetitionActivation "testnet-trading" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        `shouldSatisfy` isLeft

  describe "perps candle feature configuration" $ do
    it "accepts only explicit safe write modes" $ do
      parsePerpsCandleWriteMode "off" `shouldBe` Right PerpsCandleWritesOff
      parsePerpsCandleWriteMode "DUAL" `shouldBe` Right PerpsCandleWritesDual
      parsePerpsCandleWriteMode "on" `shouldSatisfy` isLeft

    it "accepts legacy, shadow, and rollup read modes" $ do
      parsePerpsCandleReadMode "legacy" `shouldBe` Right PerpsCandleReadsLegacy
      parsePerpsCandleReadMode " shadow " `shouldBe` Right PerpsCandleReadsShadow
      parsePerpsCandleReadMode "rollup" `shouldBe` Right PerpsCandleReadsRollup
      parsePerpsCandleReadMode "enabled" `shouldSatisfy` isLeft

    it "validates every rollout interval against the canonical set" $ do
      parsePerpsCandleReadIntervals "60, 300 3600,86400"
        `shouldBe` Right [60, 300, 3600, 86_400]
      parsePerpsCandleReadIntervals "" `shouldBe` Right []
      parsePerpsCandleReadIntervals "120" `shouldSatisfy` isLeft

    it "exposes only allowlisted intervals in explicit rollup mode" $ do
      perpsCandleRollupReadEnabled PerpsCandleReadsRollup True [60, 3600] 3600
        `shouldBe` True
      perpsCandleRollupReadEnabled PerpsCandleReadsRollup True [] 3600
        `shouldBe` False
      perpsCandleRollupReadEnabled PerpsCandleReadsLegacy True [3600] 3600
        `shouldBe` False
      perpsCandleRollupReadEnabled PerpsCandleReadsShadow True [3600] 3600
        `shouldBe` False
      perpsCandleRollupReadEnabled PerpsCandleReadsRollup True [120] 120
        `shouldBe` False
      perpsCandleRollupReadEnabled PerpsCandleReadsRollup False [3600] 3600
        `shouldBe` False

    it "requires dual writes before any rollup interval is allowlisted" $ do
      validatePerpsCandleModeCombination
        PerpsCandleWritesOff
        PerpsCandleReadsRollup
        [3600]
        True
        `shouldSatisfy` isLeft
      validatePerpsCandleModeCombination
        PerpsCandleWritesDual
        PerpsCandleReadsRollup
        [3600]
        True
        `shouldBe` Right ()
      validatePerpsCandleModeCombination
        PerpsCandleWritesOff
        PerpsCandleReadsRollup
        []
        True
        `shouldBe` Right ()
      validatePerpsCandleModeCombination
        PerpsCandleWritesOff
        PerpsCandleReadsLegacy
        [3600]
        True
        `shouldSatisfy` isLeft

    it "requires strict coverage whenever rollup read mode is selected" $ do
      validatePerpsCandleModeCombination
        PerpsCandleWritesDual
        PerpsCandleReadsRollup
        [3600]
        False
        `shouldSatisfy` isLeft
      validatePerpsCandleModeCombination
        PerpsCandleWritesOff
        PerpsCandleReadsLegacy
        []
        False
        `shouldBe` Right ()

  describe "faucet guard configuration" $ do
    it "defaults to the selected moderate hourly quotas" $
      validateFaucetGuardConfig
        (Just "configured-private-key")
        (Just faucetToken)
        "20"
        "200"
        `shouldBe` Right
          ( Just
              FaucetGuardConfig
                { fgcProxyOriginToken = "0123456789abcdef0123456789abcdef"
                , fgcClientRequestsPerHour = 20
                , fgcGlobalRequestsPerHour = 200
                }
          )

    it "fails startup when the faucet signer has no proxy token" $
      validateFaucetGuardConfig
        (Just "configured-private-key")
        Nothing
        "20"
        "200"
        `shouldSatisfy` isLeft

    it "rejects short tokens and invalid or inverted quota values" $ do
      validateFaucetGuardConfig Nothing (Just "too-short") "20" "200"
        `shouldSatisfy` isLeft
      validateFaucetGuardConfig Nothing (Just faucetToken) "0" "200"
        `shouldSatisfy` isLeft
      validateFaucetGuardConfig Nothing (Just faucetToken) "201" "200"
        `shouldSatisfy` isLeft
  where
    faucetToken = "0123456789abcdef0123456789abcdef"
    isLeft (Left _) = True
    isLeft (Right _) = False
