module Plether.ConfigSpec (spec) where

import Control.Exception (bracket)
import qualified Data.Text as T
import Plether.Config
  ( Config (..)
  , FaucetGuardConfig (..)
  , LpSettlementMode (..)
  , NativeAaSafetyInput (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  , parseLpSettlementLimits
  , parseLpSettlementMode
  , parsePerpsCandleReadIntervals
  , parsePerpsCandleReadMode
  , parsePerpsCandleWriteMode
  , perpsCandleRollupReadEnabled
  , loadConfig
  , normalizeExternalSecurityRpcUrl
  , resolveLpSettlementMode
  , validAaDeploymentAddresses
  , validAaOriginSecret
  , validateFaucetGuardConfig
  , validateInsightsCompetitionActivation
  , validateKeeperPollSeconds
  , validateLpSettlementChainId
  , validateLpSettlementPrivateKeyConfig
  , validateNativeAaPresence
  , validateNativeAaSafety
  , validatePerpsCandleModeCombination
  )
import Plether.Insights.Competition
  ( crSlug
  , july2026CompetitionSlug
  , september2026CompetitionSlug
  )
import Plether.Keeper (lpSettlementRequiredBalance)
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

  describe "native AA configuration safety" $ do
    it "accepts only generated-looking 64-character lowercase hex origin tokens" $ do
      validAaOriginSecret validOriginToken `shouldBe` True
      validAaOriginSecret (T.toUpper validOriginToken) `shouldBe` False
      validAaOriginSecret (T.take 63 validOriginToken) `shouldBe` False
      validAaOriginSecret (validOriginToken <> "0") `shouldBe` False

    it "rejects known origin-token placeholders" $ do
      mapM_ (\candidate -> validAaOriginSecret candidate `shouldBe` False)
        [ T.replicate 64 "0"
        , T.replicate 64 "f"
        , T.concat $ replicate 4 "0123456789abcdef"
        , T.concat $ replicate 8 "deadbeef"
        ]

    it "rejects an invalid origin token even when both AA providers are disabled" $ do
      validateNativeAaPresence False False False (Just $ replicate 64 '0') (replicate 8 Nothing)
        `shouldSatisfy` isLeft
      validateNativeAaPresence False False False (Just "short") (replicate 8 Nothing)
        `shouldSatisfy` isLeft
      validateNativeAaPresence False False False (Just $ T.unpack validOriginToken) (replicate 8 Nothing)
        `shouldBe` Right False

    it "distinguishes an absent native configuration from any partial configuration" $ do
      let completeNativeFields = replicate 8 $ Just "configured"
      validateNativeAaPresence False False False Nothing (replicate 8 Nothing)
        `shouldBe` Right False
      validateNativeAaPresence False False False (Just $ T.unpack validOriginToken) (replicate 8 Nothing)
        `shouldBe` Right False
      validateNativeAaPresence False False False Nothing completeNativeFields
        `shouldSatisfy` isLeft
      mapM_
        (\missingIndex ->
          validateNativeAaPresence
            False
            False
            False
            (Just $ T.unpack validOriginToken)
            ( take missingIndex completeNativeFields
                <> [Nothing]
                <> drop (missingIndex + 1) completeNativeFields
            )
            `shouldSatisfy` isLeft
        )
        [0 .. 7]
      validateNativeAaPresence True True False (Just $ T.unpack validOriginToken) (replicate 8 Nothing)
        `shouldSatisfy` isLeft
      validateNativeAaPresence False True False (Just $ T.unpack validOriginToken) (replicate 8 Nothing)
        `shouldSatisfy` isLeft
      validateNativeAaPresence False False False (Just $ T.unpack validOriginToken) completeNativeFields
        `shouldBe` Right True

    it "rejects the global rollout flag unconditionally and requires a canary when enabled" $ do
      validateNativeAaPresence False False True Nothing (replicate 8 Nothing)
        `shouldBe` Left globalRolloutUnsupported
      validateNativeAaSafety
        (validNativeSafety {nasiGlobalRolloutEnabled = True})
        `shouldBe` Left globalRolloutUnsupported
      validateNativeAaSafety
        (validNativeSafety {nasiCanaryOwners = []})
        `shouldSatisfy` isLeft
      validateNativeAaSafety validNativeSafety `shouldBe` Right ()

    it "caps paymaster authorization validity at 570 seconds" $ do
      validateNativeAaSafety
        (validNativeSafety {nasiValiditySeconds = 570})
        `shouldBe` Right ()
      validateNativeAaSafety
        (validNativeSafety {nasiValiditySeconds = 571})
        `shouldSatisfy` isLeft
      validateNativeAaSafety
        (validNativeSafety {nasiValiditySeconds = 0})
        `shouldSatisfy` isLeft

    it "pins the reviewed policy and account runtime and rejects a zero paymaster runtime hash" $ do
      validateNativeAaSafety validNativeSafety `shouldBe` Right ()
      validateNativeAaSafety
        (validNativeSafety {nasiPolicyId = "0x" <> T.replicate 64 "1"})
        `shouldSatisfy` isLeft
      validateNativeAaSafety
        (validNativeSafety {nasiAccountCodeHash = "0x" <> T.replicate 64 "2"})
        `shouldSatisfy` isLeft
      validateNativeAaSafety
        (validNativeSafety {nasiPaymasterCodeHash = "0x" <> T.replicate 64 "0"})
        `shouldSatisfy` isLeft

    it "requires per-operation cost to fit both account and client caps before the global cap" $ do
      validateNativeAaSafety
        (validNativeSafety {nasiMaxCostWei = 21})
        `shouldSatisfy` isLeft
      validateNativeAaSafety
        (validNativeSafety {nasiClientOutstandingWei = 9})
        `shouldSatisfy` isLeft
      validateNativeAaSafety
        (validNativeSafety {nasiAccountOutstandingWei = 101})
        `shouldSatisfy` isLeft
      validateNativeAaSafety
        (validNativeSafety {nasiClientOutstandingWei = 101})
        `shouldSatisfy` isLeft
      validateNativeAaSafety validNativeSafety `shouldBe` Right ()

    it "orders account-hourly, global-hourly, and global-daily spend budgets" $ do
      validateNativeAaSafety
        (validNativeSafety {nasiAccountHourlyWei = 201})
        `shouldSatisfy` isLeft
      validateNativeAaSafety
        (validNativeSafety {nasiGlobalDailyWei = 199})
        `shouldSatisfy` isLeft
      validateNativeAaSafety validNativeSafety `shouldBe` Right ()

    it "requires submission whenever sponsorship is enabled" $ do
      validateNativeAaSafety
        (validNativeSafety {nasiSubmissionEnabled = False})
        `shouldSatisfy` isLeft
      validateNativeAaSafety
        ( validNativeSafety
            { nasiSponsorshipEnabled = False
            , nasiSubmissionEnabled = False
            , nasiCanaryOwners = []
            }
        )
        `shouldBe` Right ()

    it "pins native action targets to the reviewed Arbitrum Sepolia deployment" $ do
      validAaDeploymentAddresses
        "0xabee441b564dc084857468fa244aee0a444b07df"
        "0x2b9790ad11ce5fb1b91ac3415b08cd1ec7d0ce0b"
        "0x2cedc3f0059f0e9c1099be96974f459e58c428d6"
        "0x91c85540a1f64c9aec2c801fcc927f037d619f17"
        `shouldBe` True
      validAaDeploymentAddresses
        "0x1111111111111111111111111111111111111111"
        "0x2b9790ad11ce5fb1b91ac3415b08cd1ec7d0ce0b"
        "0x2cedc3f0059f0e9c1099be96974f459e58c428d6"
        "0x91c85540a1f64c9aec2c801fcc927f037d619f17"
        `shouldBe` False

    it "normalizes only HTTPS/default-443 external security RPC endpoints" $ do
      normalizeExternalSecurityRpcUrl "https://RPC.Example.com/tenant"
        `shouldBe` Just "https://rpc.example.com/tenant"
      normalizeExternalSecurityRpcUrl "https://rpc.example.com:443/"
        `shouldBe` Just "https://rpc.example.com"
      normalizeExternalSecurityRpcUrl "http://rpc.example.com" `shouldBe` Nothing
      normalizeExternalSecurityRpcUrl "https://user@rpc.example.com" `shouldBe` Nothing
      normalizeExternalSecurityRpcUrl "https://rpc.example.com:8443" `shouldBe` Nothing
      normalizeExternalSecurityRpcUrl "https://rpc.example.com/?token=x" `shouldBe` Nothing

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

faucetToken :: String
faucetToken = "0123456789abcdef0123456789abcdef"

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

validOriginToken :: T.Text
validOriginToken = T.replicate 63 "a" <> "b"

globalRolloutUnsupported :: String
globalRolloutUnsupported =
  "AA_NATIVE_GLOBAL_ROLLOUT_ENABLED=true is not supported; native sponsorship must remain canary-scoped"

validNativeSafety :: NativeAaSafetyInput
validNativeSafety =
  NativeAaSafetyInput
    { nasiOriginToken = validOriginToken
    , nasiSponsorshipEnabled = True
    , nasiSubmissionEnabled = True
    , nasiGlobalRolloutEnabled = False
    , nasiCanaryOwners = ["0x1111111111111111111111111111111111111111"]
    , nasiValiditySeconds = 570
    , nasiPaymasterCodeHash = "0x" <> T.replicate 64 "3"
    , nasiPolicyId = "0x8dd77324b94da492342191f762a32cdf99e828a7f24d77c8ed5ace90cf4f5ae3"
    , nasiAccountCodeHash = "0x41ee894da413cc99e8dec0a1784470eceb736845ad1591e06ff0ecdf0aca26c9"
    , nasiMaxCostWei = 10
    , nasiAccountOutstandingWei = 20
    , nasiClientOutstandingWei = 20
    , nasiGlobalOutstandingWei = 100
    , nasiAccountHourlyWei = 30
    , nasiGlobalHourlyWei = 200
    , nasiGlobalDailyWei = 250
    }
