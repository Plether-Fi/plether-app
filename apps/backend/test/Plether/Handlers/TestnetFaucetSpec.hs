module Plether.Handlers.TestnetFaucetSpec (spec) where

import qualified Data.ByteString.Base16 as B16
import Data.Aeson (Value (String), toJSON)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Query)
import Plether.Config
  ( Config (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  )
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , july2026Competition
  )
import Plether.Database.Schema
  ( TestnetFaucetClaimRow (..)
  , beginTestnetFaucetClaimSql
  , markTestnetFaucetClaimRevertedSql
  , markTestnetFaucetClaimSubmittedSql
  , markTestnetFaucetClaimSuccessSql
  )
import Plether.Handlers.TestnetFaucet
  ( FaucetBroadcastErrorDisposition (..)
  , FaucetClaimDisposition (..)
  , FaucetResponseStatus (..)
  , TestnetFaucetResponse (..)
  , classifyFaucetBroadcastErrorText
  , classifyTestnetFaucetClaim
  , faucetMintCall
  , faucetRouteTimeoutMicros
  , gateSubmittedFaucetResponse
  , receiptMatchesPersistedTransaction
  , testnetFaucetAmount
  , testnetFaucetEnabled
  )
import Plether.Ethereum.Rpc (TxReceipt (..))
import Plether.Types (ApiError (..), ApiErrorCode (..), ApiResponse (..), mkResponse)
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

  describe "faucet response status JSON" $
    it "distinguishes pending confirmation from terminal funding states" $
      map
        toJSON
        [ FaucetResponseSubmitted
        , FaucetResponseMinted
        , FaucetResponseAlreadyClaimed
        , FaucetResponseAlreadyFunded
        ]
        `shouldBe` map String ["submitted", "minted", "already_claimed", "already_funded"]

  describe "faucet route deadline" $
    it "returns before the load balancer's 75-second idle timeout" $ do
      faucetRouteTimeoutMicros `shouldBe` 60_000_000
      faucetRouteTimeoutMicros `shouldSatisfy` (< 75_000_000)

  describe "faucet submitted-response compatibility" $ do
    let response status =
          Right $
            mkResponse
              0
              421614
              TestnetFaucetResponse
                { tfrAddress = "0x1111111111111111111111111111111111111111"
                , tfrAmount = testnetFaucetAmount
                , tfrToken = "0x2222222222222222222222222222222222222222"
                , tfrTxHash = Just txHash
                , tfrStatus = status
                }

    it "returns submitted only to clients that opt into the pending state" $ do
      case gateSubmittedFaucetResponse True $ response FaucetResponseSubmitted of
        Right ApiResponse {respData = TestnetFaucetResponse {tfrStatus}} ->
          tfrStatus `shouldBe` FaucetResponseSubmitted
        _ -> expectationFailure "async client did not receive the durable submitted state"

      case gateSubmittedFaucetResponse False $ response FaucetResponseSubmitted of
        Left ApiError {errCode} -> errCode `shouldBe` RateLimited
        _ -> expectationFailure "legacy client could mistake a submitted transaction for minted funds"

    it "leaves terminal responses unchanged for every client" $
      case gateSubmittedFaucetResponse False $ response FaucetResponseMinted of
        Right ApiResponse {respData = TestnetFaucetResponse {tfrStatus}} ->
          tfrStatus `shouldBe` FaucetResponseMinted
        _ -> expectationFailure "terminal faucet response was not preserved"

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

  describe "faucet broadcast recovery" $ do
    it "keeps known persisted transactions pending for an exact receipt" $ do
      classifyFaucetBroadcastErrorText "already known"
        `shouldBe` FaucetAlreadyKnown
      classifyFaucetBroadcastErrorText "Known transaction"
        `shouldBe` FaucetAlreadyKnown
      classifyFaucetBroadcastErrorText "already imported"
        `shouldBe` FaucetAlreadyKnown

    it "does not treat a consumed nonce as proof that the exact mint succeeded" $
      classifyFaucetBroadcastErrorText "replacement transaction underpriced: nonce too low"
        `shouldBe` FaucetNonceAlreadyConsumed

    it "keeps unrelated broadcast failures as real RPC errors" $
      classifyFaucetBroadcastErrorText "insufficient funds for gas"
        `shouldBe` FaucetBroadcastRejected

    it "does not mistake unknown transactions for known transactions" $ do
      classifyFaucetBroadcastErrorText "unknown transaction"
        `shouldBe` FaucetBroadcastRejected
      classifyFaucetBroadcastErrorText "transaction not known"
        `shouldBe` FaucetBroadcastRejected

    it "accepts only the exact persisted transaction receipt" $ do
      let matchingReceipt =
            TxReceipt
              { receiptTxHash = T.toUpper txHash
              , receiptBlockNumber = 123
              , receiptSucceeded = True
              , receiptLogs = []
              }
          mismatchedReceipt = matchingReceipt {receiptTxHash = "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"}
      receiptMatchesPersistedTransaction txHash matchingReceipt `shouldBe` True
      receiptMatchesPersistedTransaction txHash mismatchedReceipt `shouldBe` False
      receiptMatchesPersistedTransaction txHash (matchingReceipt {receiptTxHash = ""})
        `shouldBe` False

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

    it "finalizes only the exact durable submitted transaction" $ do
      queryContains markTestnetFaucetClaimSuccessSql "tx_hash = ?"
      queryContains markTestnetFaucetClaimSuccessSql "mint_block_number = ?"
      queryContains markTestnetFaucetClaimSuccessSql "status IN ('submitted', 'success')"
      queryContains markTestnetFaucetClaimRevertedSql "tx_hash = ?"
      queryContains markTestnetFaucetClaimRevertedSql "status = 'submitted'"

    it "keeps the static schema aligned with durable transaction recovery" $ do
      schema <- readFile "schema.sql"
      schema `shouldSatisfy` isInfixOf "raw_tx TEXT"
      schema `shouldSatisfy` isInfixOf "mint_block_number BIGINT"

claimRow :: T.Text -> Maybe T.Text -> Maybe T.Text -> TestnetFaucetClaimRow
claimRow status hash rawTx =
  TestnetFaucetClaimRow
    { tfcAddress = "0x1111111111111111111111111111111111111111"
    , tfcAmount = testnetFaucetAmount
    , tfcTokenAddress = "0x2222222222222222222222222222222222222222"
    , tfcTxHash = hash
    , tfcRawTx = rawTx
    , tfcMintBlockNumber = Nothing
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
    , cfgPerpsCandleWriteMode = PerpsCandleWritesOff
    , cfgPerpsCandleReadMode = PerpsCandleReadsLegacy
    , cfgPerpsCandleReadIntervals = []
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = "https://arb-sepolia.example"
    , cfgPerpsChainId = perpsChainId
    , cfgPerpsUsdc = "0x1647e41f49ED6D688936092B5a291c4B28106343"
    , cfgPerpsOrderRouter = "0x0000000000000000000000000000000000000000"
    , cfgPerpsCfdEngine = "0x0000000000000000000000000000000000000000"
    , cfgPerpsCfdEngineLens = "0x0000000000000000000000000000000000000000"
    , cfgPerpsCfdEngineSettlementSidecar = "0x0000000000000000000000000000000000000000"
    , cfgPerpsMarginClearinghouse = "0x0000000000000000000000000000000000000000"
    , cfgPerpsPletherOracle = "0x0000000000000000000000000000000000000000"
    , cfgPerpsAccountLens = "0x429DA61a7a616DeDD84d2a51eB6Dc1bD72427dC1"
    , cfgPerpsPublicLens = "0x0000000000000000000000000000000000000000"
    , cfgPerpsHousePool = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgPerpsSeniorVault = "0x0000000000000000000000000000000000000000"
    , cfgPerpsJuniorVault = "0x0000000000000000000000000000000000000000"
    , cfgPerpsOrderRouterAdmin = "0x0000000000000000000000000000000000000000"
    , cfgPerpsCfdEngineAdmin = "0x0000000000000000000000000000000000000000"
    , cfgPerpsSettlementMonitorLens = "0xd251AC0BD90780c48F31F575152808315200664E"
    , cfgPerpsIndexerStartBlock = 0
    , cfgVaultHistoryHousePoolAddress = "0x0000000000000000000000000000000000000001"
    , cfgVaultHistorySeniorVaultAddress = "0x0000000000000000000000000000000000000002"
    , cfgVaultHistoryJuniorVaultAddress = "0x0000000000000000000000000000000000000003"
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryRpcUrl = "https://archive.example"
    , cfgVaultHistoryConfirmations = 12
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = faucetReleaseManifest perpsChainId
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    , cfgLpSettlementEnabled = False
    , cfgLpSettlementPollSeconds = 15
    }

faucetReleaseManifest :: Integer -> CompetitionReleaseManifest
faucetReleaseManifest chainId =
  CompetitionReleaseManifest
    { crmReleaseId = "testnet-faucet-test"
    , crmChainId = chainId
    , crmUsdc = "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3"
    , crmOrderRouter = zeroAddress
    , crmMarginClearinghouse = zeroAddress
    , crmAccountLens = "0xC4C886A6F1D7CB22C833AC1b29f29Da43AfbcCd1"
    , crmCfdEngine = zeroAddress
    , crmCfdEngineLens = zeroAddress
    , crmSettlementSidecar = zeroAddress
    , crmPletherOracle = zeroAddress
    , crmIndexerStartBlock = 0
    }
  where
    zeroAddress = "0x0000000000000000000000000000000000000000"
