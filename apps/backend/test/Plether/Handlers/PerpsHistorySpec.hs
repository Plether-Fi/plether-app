module Plether.Handlers.PerpsHistorySpec (spec) where

import Plether.Config (Config (..))
import Plether.Handlers.PerpsHistory (perpsHistoryRouter, perpsMarketStatsChainId)
import Test.Hspec

spec :: Spec
spec = do
  describe "perpsMarketStatsChainId" $
    it "uses the Perps chain for market activity stats" $ do
      perpsMarketStatsChainId testConfig `shouldBe` 421614

  describe "perpsHistoryRouter" $ do
    it "defaults to the configured current router" $
      perpsHistoryRouter testConfig Nothing
        `shouldBe` "0x04e3103752f623fbcdcd01f588590af4c53e4c1e"

    it "normalizes an explicit release router" $
      perpsHistoryRouter testConfig (Just "  0x485703D16FE36369C134DEe2A61C057733E7830F  ")
        `shouldBe` "0x485703d16fe36369c134dee2a61c057733e7830f"

testConfig :: Config
testConfig =
  Config
    { cfgRpcUrl = "https://eth-sepolia.example"
    , cfgChainId = 11155111
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Nothing
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = "https://benchmarks.pyth.network"
    , cfgPythHermesUrl = "https://hermes.pyth.network"
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 7
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgPerpsRpcUrl = "https://arb-sepolia.example"
    , cfgPerpsChainId = 421614
    , cfgPerpsUsdc = "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3"
    , cfgPerpsOrderRouter = "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E"
    , cfgPerpsCfdEngine = "0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a"
    , cfgPerpsMarginClearinghouse = "0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e"
    , cfgPerpsPletherOracle = "0x0000000000000000000000000000000000000000"
    , cfgPerpsAccountLens = "0xb46f7ECAE1E7D3BC8ebC7FB1cda20d2d9a83cC29"
    , cfgPerpsIndexerStartBlock = 0
    , cfgAaConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    }
