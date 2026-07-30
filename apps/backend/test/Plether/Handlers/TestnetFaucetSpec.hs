module Plether.Handlers.TestnetFaucetSpec (spec) where

import qualified Data.ByteString.Base16 as B16
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Query)
import Plether.Config (Config (..))
import Plether.Database.Schema
  ( TestnetFaucetClaimRow (..)
  , beginTestnetFaucetClaimSql
  , markTestnetFaucetClaimSubmittedSql
  )
import Plether.Handlers.TestnetFaucet
  ( FaucetClaimDisposition (..)
  , classifyTestnetFaucetClaim
  , faucetMintCall
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

  describe "faucet claim recovery state machine" $ do
    it "starts absent, failed, and preparing claims" $ do
      classifyTestnetFaucetClaim Nothing `shouldBe` FaucetBeginOrWait
      classifyTestnetFaucetClaim (Just $ claimRow "failed" Nothing Nothing)
        `shouldBe` FaucetBeginOrWait
      classifyTestnetFaucetClaim (Just $ claimRow "preparing" Nothing Nothing)
        `shouldBe` FaucetBeginOrWait

    it "reconciles legacy pending rows against their on-chain balance" $
      classifyTestnetFaucetClaim (Just $ claimRow "pending" Nothing Nothing)
        `shouldBe` FaucetReconcileLegacy

    it "resumes only submitted rows with a durable hash and raw transaction" $ do
      classifyTestnetFaucetClaim (Just $ claimRow "submitted" (Just txHash) (Just "0x02abcd"))
        `shouldBe` FaucetResumeSubmitted
      classifyTestnetFaucetClaim (Just $ claimRow "submitted" (Just txHash) Nothing)
        `shouldBe` FaucetInvalidState
      classifyTestnetFaucetClaim (Just $ claimRow "submitted" Nothing (Just "0x02abcd"))
        `shouldBe` FaucetInvalidState

    it "returns completed claims without another submission" $
      classifyTestnetFaucetClaim (Just $ claimRow "success" (Just txHash) Nothing)
        `shouldBe` FaucetAlreadyClaimed

  describe "faucet claim persistence SQL" $ do
    it "uses a recoverable preparing lease before any transaction can be broadcast" $ do
      queryContains beginTestnetFaucetClaimSql "'preparing'"
      queryContains beginTestnetFaucetClaimSql "status = 'failed'"
      queryContains beginTestnetFaucetClaimSql "INTERVAL '5 minutes'"
      queryContains beginTestnetFaucetClaimSql "raw_tx = NULL"

    it "atomically persists the signed transaction before marking it submitted" $ do
      queryContains markTestnetFaucetClaimSubmittedSql "tx_hash = ?"
      queryContains markTestnetFaucetClaimSubmittedSql "raw_tx = ?"
      queryContains markTestnetFaucetClaimSubmittedSql "status = 'submitted'"
      queryContains markTestnetFaucetClaimSubmittedSql "status = 'preparing'"

    it "keeps the static schema aligned with durable transaction recovery" $ do
      schema <- readFile "schema.sql"
      schema `shouldSatisfy` isInfixOf "raw_tx TEXT"

claimRow :: T.Text -> Maybe T.Text -> Maybe T.Text -> TestnetFaucetClaimRow
claimRow status hash rawTx =
  TestnetFaucetClaimRow
    { tfcAddress = "0x1111111111111111111111111111111111111111"
    , tfcAmount = testnetFaucetAmount
    , tfcTokenAddress = "0x2222222222222222222222222222222222222222"
    , tfcTxHash = hash
    , tfcRawTx = rawTx
    , tfcStatus = status
    , tfcError = Nothing
    }

txHash :: T.Text
txHash = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

queryContains :: Query -> String -> Expectation
queryContains sql fragment =
  show sql `shouldSatisfy` isInfixOf fragment

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
    , cfgProtocolExplorerEnabled = True
    , cfgPerpsRpcUrl = "https://arb-sepolia.example"
    , cfgPerpsChainId = perpsChainId
    , cfgPerpsUsdc = "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3"
    , cfgPerpsOrderRouter = "0x0000000000000000000000000000000000000000"
    , cfgPerpsCfdEngine = "0x0000000000000000000000000000000000000000"
    , cfgPerpsMarginClearinghouse = "0x0000000000000000000000000000000000000000"
    , cfgPerpsPletherOracle = "0x0000000000000000000000000000000000000000"
    , cfgPerpsAccountLens = "0xC4C886A6F1D7CB22C833AC1b29f29Da43AfbcCd1"
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
