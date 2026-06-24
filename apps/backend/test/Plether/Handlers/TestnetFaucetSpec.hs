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
    it "is enabled only for Sepolia" $ do
      testnetFaucetEnabled (testConfig 11155111) `shouldBe` True
      testnetFaucetEnabled (testConfig 1) `shouldBe` False
      testnetFaucetEnabled (testConfig 421614) `shouldBe` False

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

testConfig :: Integer -> Config
testConfig chainId =
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
    , cfgPythIngestionEnabled = False
    , cfgPerpsRpcUrl = "https://arb-sepolia.example"
    , cfgPerpsChainId = 421614
    , cfgPerpsOrderRouter = "0x0000000000000000000000000000000000000000"
    , cfgPerpsPletherOracle = "0x0000000000000000000000000000000000000000"
    , cfgPerpsIndexerStartBlock = 0
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    }
