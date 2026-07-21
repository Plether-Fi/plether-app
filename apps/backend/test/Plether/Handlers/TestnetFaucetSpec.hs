module Plether.Handlers.TestnetFaucetSpec (spec) where

import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Config (Config (..))
import Plether.Handlers.TestnetFaucet
  ( faucetMintCall
  , testnetFaucetAmount
  , testnetFaucetEnabled
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "testnetFaucetEnabled" $ do
    it "is enabled only for Arbitrum Sepolia perps" $ do
      testnetFaucetEnabled (testConfig 11155111 421614) `shouldBe` True
      testnetFaucetEnabled (testConfig 11155111 11155111) `shouldBe` False
      testnetFaucetEnabled (testConfig 1 421614) `shouldBe` True

  describe "testnetFaucetAmount" $
    it "is 100,000 USDC with 6 decimals" $
      testnetFaucetAmount `shouldBe` 100_000_000_000

  describe "faucetMintCall" $
    it "encodes mint(address,uint256) for the faucet amount" $ do
      let calldata =
            TE.decodeUtf8 $
              B16.encode $
                faucetMintCall "0x1111111111111111111111111111111111111111"
      T.take 8 calldata `shouldBe` "40c10f19"
      T.unpack calldata `shouldContain` "0000000000000000000000001111111111111111111111111111111111111111"
      T.unpack calldata `shouldContain` "000000000000000000000000000000000000000000000000000000174876e800"

testConfig :: Integer -> Integer -> Config
testConfig chainId perpsChainId =
  Config
    { cfgRpcUrl = "https://eth-sepolia.example"
    , cfgChainId = chainId
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
    , cfgPerpsChainId = perpsChainId
    , cfgPerpsUsdc = "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3"
    , cfgPerpsOrderRouter = "0x0000000000000000000000000000000000000000"
    , cfgPerpsCfdEngine = "0x0000000000000000000000000000000000000000"
    , cfgPerpsMarginClearinghouse = "0x0000000000000000000000000000000000000000"
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
