module Plether.ConfigSpec (spec) where

import Control.Exception (bracket)
import qualified Data.Text as T
import Plether.Config
  ( Config (..)
  , FaucetGuardConfig (..)
  , LpSettlementMode (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  , parseLpSettlementLimits
  , parseLpSettlementMode
  , parsePerpsCandleReadIntervals
  , parsePerpsCandleReadMode
  , parsePerpsCandleWriteMode
  , perpsCandleRollupReadEnabled
  , loadConfig
  , resolveLpSettlementMode
  , validateLpSettlementChainId
  , validateLpSettlementPrivateKeyConfig
  , validateInsightsCompetitionActivation
  , validateKeeperPollSeconds
  , validateFaucetGuardConfig
  , validatePerpsCandleModeCombination
  )
import Plether.Keeper (lpSettlementRequiredBalance)
import Plether.Insights.Competition
  ( crSlug
  , july2026CompetitionSlug
  , september2026CompetitionSlug
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = do
  describe "keeper polling configuration" $ do
    it "accepts an idle cadence at least as long as the active cadence" $ do
      validateKeeperPollSeconds "1" "5" `shouldBe` Right (1, 5)
      validateKeeperPollSeconds "5" "1" `shouldSatisfy` isLeft
      validateKeeperPollSeconds "0" "5" `shouldSatisfy` isLeft

  describe "LP settlement configuration" $ do
    it "defaults to off and accepts only explicit rollout modes" $ do
      resolveLpSettlementMode Nothing Nothing `shouldBe` Right LpSettlementOff
      parseLpSettlementMode " OFF " `shouldBe` Right LpSettlementOff
      parseLpSettlementMode "Observe" `shouldBe` Right LpSettlementObserve
      parseLpSettlementMode "execute" `shouldBe` Right LpSettlementExecute
      parseLpSettlementMode "enabled" `shouldSatisfy` isLeft

    it "maps the legacy false flag to off but never treats legacy true as execute" $ do
      resolveLpSettlementMode Nothing (Just "false") `shouldBe` Right LpSettlementOff
      resolveLpSettlementMode (Just "off") (Just "0") `shouldBe` Right LpSettlementOff
      resolveLpSettlementMode Nothing (Just "true") `shouldSatisfy` isLeft
      resolveLpSettlementMode (Just "execute") (Just "false") `shouldSatisfy` isLeft
      resolveLpSettlementMode (Just "observe") (Just "not-a-bool") `shouldSatisfy` isLeft

    it "keeps active settlement scoped to the reviewed Arbitrum Sepolia deployment" $ do
      validateLpSettlementChainId LpSettlementOff "1" `shouldBe` Right ()
      validateLpSettlementChainId LpSettlementObserve "421614" `shouldBe` Right ()
      validateLpSettlementChainId LpSettlementExecute "1" `shouldSatisfy` isLeft
      validateLpSettlementChainId LpSettlementObserve "not-a-chain" `shouldSatisfy` isLeft

    it "strictly bounds drain and replacement controls" $ do
      parseLpSettlementLimits LpSettlementObserve "4" "60" "3" "0"
        `shouldBe` Right (4, 60, 3, 0)
      parseLpSettlementLimits LpSettlementObserve "0" "60" "3" "0"
        `shouldSatisfy` isLeft
      parseLpSettlementLimits LpSettlementObserve "5" "60" "3" "0"
        `shouldSatisfy` isLeft
      parseLpSettlementLimits LpSettlementObserve "17" "60" "3" "0"
        `shouldSatisfy` isLeft
      parseLpSettlementLimits LpSettlementObserve "4" "59" "3" "0"
        `shouldSatisfy` isLeft
      parseLpSettlementLimits LpSettlementObserve "4" "29" "3" "0"
        `shouldSatisfy` isLeft
      parseLpSettlementLimits LpSettlementObserve "4" "3601" "3" "0"
        `shouldSatisfy` isLeft
      parseLpSettlementLimits LpSettlementObserve "4" "60" "21" "0"
        `shouldSatisfy` isLeft
      parseLpSettlementLimits LpSettlementObserve "4" "60" "4" "0"
        `shouldSatisfy` isLeft
      parseLpSettlementLimits LpSettlementObserve "04" "60" "3" "0"
        `shouldSatisfy` isLeft
      parseLpSettlementLimits LpSettlementObserve "4" "60" "-1" "0"
        `shouldSatisfy` isLeft

    it "requires a positive transaction-cost ceiling only in execute mode" $ do
      parseLpSettlementLimits LpSettlementOff "4" "60" "3" "0"
        `shouldBe` Right (4, 60, 3, 0)
      parseLpSettlementLimits LpSettlementObserve "4" "60" "3" "0"
        `shouldBe` Right (4, 60, 3, 0)
      parseLpSettlementLimits LpSettlementExecute "4" "60" "3" "0"
        `shouldSatisfy` isLeft
      parseLpSettlementLimits LpSettlementExecute "4" "60" "3" "1000000000000000"
        `shouldBe` Right (4, 60, 3, 1_000_000_000_000_000)
      parseLpSettlementLimits LpSettlementObserve "4" "60" "3" "-1"
        `shouldSatisfy` isLeft

    it "requires a structurally valid dedicated signer in every active mode" $ do
      let privateKey = T.replicate 64 "1"
      validateLpSettlementPrivateKeyConfig LpSettlementOff Nothing Nothing
        `shouldBe` Right Nothing
      validateLpSettlementPrivateKeyConfig LpSettlementObserve (Just privateKey) Nothing
        `shouldBe` Right (Just privateKey)
      validateLpSettlementPrivateKeyConfig LpSettlementExecute Nothing Nothing
        `shouldSatisfy` isLeft
      validateLpSettlementPrivateKeyConfig LpSettlementOff (Just "not-a-private-key") Nothing
        `shouldSatisfy` isLeft
      validateLpSettlementPrivateKeyConfig
        LpSettlementObserve
        (Just $ "0x" <> privateKey)
        (Just $ T.toUpper privateKey)
        `shouldSatisfy` isLeft

    it "redacts every configured secret from Config's Show instance" $ do
      let rpcSecret = "rpc-show-secret"
          rpcAuthSecret = "rpc-auth-show-secret"
          databaseSecret = "database-show-secret"
          pythSecret = "pyth-show-secret"
          faucetSecret = "faucet-show-secret"
          faucetProxySecret = "abcdef0123456789abcdef0123456789"
          keeperSecret = replicate 64 '1'
          lpSettlementSecret = replicate 64 '2'
      withEnvironmentVariables
        [ ("RPC_URL", Just $ "https://rpc.example/" <> rpcSecret)
        , ("RPC_AUTH_TOKEN", Just rpcAuthSecret)
        , ("PERPS_RPC_URL", Just $ "https://perps-rpc.example/" <> rpcSecret)
        , ("PERPS_RPC_AUTH_TOKEN", Just $ rpcAuthSecret <> "-perps")
        , ("CHAIN_ID", Just "11155111")
        , ("PERPS_CHAIN_ID", Just "421614")
        , ("DATABASE_URL", Just $ "postgresql://user:" <> databaseSecret <> "@database.example/db")
        , ("PYTH_API_KEY", Just pythSecret)
        , ("FAUCET_PRIVATE_KEY", Just faucetSecret)
        , ("FAUCET_PROXY_ORIGIN_TOKEN", Just faucetProxySecret)
        , ("KEEPER_PRIVATE_KEY", Just keeperSecret)
        , ("LP_SETTLEMENT_PRIVATE_KEY", Just lpSettlementSecret)
        , ("LP_SETTLEMENT_MODE", Just "off")
        , ("LP_SETTLEMENT_ENABLED", Nothing)
        , ("LP_SETTLEMENT_POLL_SECONDS", Just "15")
        , ("LP_SETTLEMENT_MAX_DRAIN_TRANSACTIONS", Just "4")
        , ("LP_SETTLEMENT_PENDING_REPLACEMENT_SECONDS", Just "60")
        , ("LP_SETTLEMENT_MAX_REPLACEMENTS", Just "3")
        , ("LP_SETTLEMENT_MAX_TX_COST_WEI", Just "0")
        , ("AA_PROXY_ORIGIN_TOKEN", Nothing)
        , ("PIMLICO_API_KEY", Nothing)
        , ("PIMLICO_SPONSORSHIP_POLICY_ID", Nothing)
        , ("AA_SPONSORSHIP_ENABLED", Just "false")
        , ("INSIGHTS_REGISTRATION_ENABLED", Just "false")
        , ("INSIGHTS_REGISTRATION_PROVISIONED", Just "false")
        , ("INSIGHTS_ACTIVE_COMPETITION_SLUG", Nothing)
        , ("INSIGHTS_COMPETITION_RELEASE_ID", Nothing)
        ]
        $ do
          result <- loadConfig
          case result of
            Left err -> expectationFailure $ "loadConfig failed: " <> err
            Right cfg -> do
              let rendered = show cfg
              mapM_
                (rendered `shouldNotContain`)
                [ rpcSecret
                , rpcAuthSecret
                , databaseSecret
                , pythSecret
                , faucetSecret
                , faucetProxySecret
                , keeperSecret
                , lpSettlementSecret
                ]
              rendered `shouldContain` "cfgLpSettlementMode = LpSettlementOff"
              lpSettlementRequiredBalance
                ( cfg
                    { cfgLpSettlementMaxDrainTransactions = 1
                    , cfgLpSettlementMaxTxCostWei = 125
                    }
                )
                `shouldBe` 1_000
              lpSettlementRequiredBalance
                ( cfg
                    { cfgLpSettlementMaxDrainTransactions = 4
                    , cfgLpSettlementMaxTxCostWei = 125
                    }
                )
                `shouldBe` 1_000

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

withEnvironmentVariables :: [(String, Maybe String)] -> IO a -> IO a
withEnvironmentVariables [] action = action
withEnvironmentVariables ((name, configuredValue) : rest) action =
  bracket
    (do
      previousValue <- lookupEnv name
      apply configuredValue
      pure previousValue
    )
    apply
    (\_ -> withEnvironmentVariables rest action)
  where
    apply value =
      case value of
        Just configured -> setEnv name configured
        Nothing -> unsetEnv name
