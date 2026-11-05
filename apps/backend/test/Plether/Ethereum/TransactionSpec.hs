module Plether.Ethereum.TransactionSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Aeson (toJSON)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text.Encoding as TE
import Plether.Config (Config (..), LpSettlementMode (..), PerpsCandleReadMode (..), PerpsCandleWriteMode (..))
import Plether.Database.Schema (PythUpdatePayloadRow (..), LpSettlementTransactionRow (..))
import Plether.Ethereum.Client (RpcError (..))
import Plether.Ethereum.Contracts.SettlementMonitor (ExecutionPath (..), SettlementStatus (..))
import Plether.Insights.Competition (CompetitionReleaseManifest (..), july2026Competition)
import Plether.Keeper (buildLpSettlementTransactionWith, buildLpSettlementReplacement)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Contracts.Perps
  ( settleLpEpochPoolCall
  , settleLpEpochRouterCall
  )
import Plether.Ethereum.Transaction
  ( SignedTransaction (..)
  , Tx1559 (..)
  , deriveAddress
  , rawTransactionHash
  , decodeSignedTransaction
  , sameNonceReplacementFees
  , signTransaction
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeSignedTransaction" $ do
    it "recovers the signed nonce, exact intent and sender from the canonical vector" $ do
      raw <- decodeHex rawTransactionVector
      decodeSignedTransaction raw `shouldReturn` Right (sampleTx, "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf")
    it "rejects malformed, truncated, trailing and unsupported transaction encodings" $ do
      raw <- decodeHex rawTransactionVector
      mapM_ (\bytes -> decodeSignedTransaction bytes >>= (`shouldSatisfy` either (const True) (const False)))
        [BS.empty, BS.drop 1 raw, BS.take (BS.length raw - 1) raw, raw <> BS.singleton 0,
         BS.pack [2, 0xf8, 0xff], BS.pack [2, 0xc0], BS.replicate 1_048_577 2]
  describe "deriveAddress" $ do
    it "derives the expected address for private key 1" $ do
      deriveAddress privateKeyOne
        `shouldReturn` Right "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"

  describe "signTransaction" $ do
    it "signs a typed EIP-1559 transaction and reports the sender" $ do
      result <- signTransaction privateKeyOne sampleTx
      case result of
        Left err -> expectationFailure $ "signing failed: " <> show err
        Right signed -> do
          signedFrom signed `shouldBe` "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"
          BS.head (signedRawTransaction signed) `shouldBe` 0x02
          BS.length (signedRawTransaction signed) `shouldSatisfy` (> 1)
          T.length (signedTransactionHash signed) `shouldBe` 66

    it "is deterministic and matches the canonical raw transaction vector" $ do
      first <- signTransaction privateKeyOne sampleTx
      second <- signTransaction privateKeyOne sampleTx
      first `shouldBe` second
      case first of
        Left err -> expectationFailure $ "signing failed: " <> show err
        Right signed -> do
          expectedRaw <- decodeHex rawTransactionVector
          signedRawTransaction signed `shouldBe` expectedRaw
          signedTransactionHash signed `shouldBe` transactionHashVector
          rawTransactionHash expectedRaw `shouldBe` transactionHashVector

  describe "sameNonceReplacementFees" $ do
    it "beats both old fee fields by at least 12.5 percent" $
      sameNonceReplacementFees 2_500 50 1 10 100 `shouldBe` (12, 113)

    it "uses a higher buffered network quote" $
      sameNonceReplacementFees 2_500 200 5 10 100 `shouldBe` (12, 250)

    it "builds a replacement from the durable intent while changing only the fees" $ do
      let (priority, maxFee) = sameNonceReplacementFees 2_500 2_500_000 500_000 1_000_000 2_000_000
          replacement = buildLpSettlementReplacement persistedIntent priority maxFee
      replacement `shouldBe` Tx1559
        { txChainId = 421614, txNonce = 17
        , txMaxPriorityFeePerGas = 1_125_000, txMaxFeePerGas = 3_125_000
        , txGasLimit = 21_000, txTo = "0x2222222222222222222222222222222222222222"
        , txValue = 987_654, txData = settleLpEpochRouterCall sixFeedData
        }
      maximumCost replacement `shouldBe` 65_625_987_654

  describe "production LP settlement transaction builder" $ do
    it "selects HousePool with zero value and does not fetch or quote a payload" $ do
      buildLpSettlementTransactionWith settlementConfig
        (\_ -> expectationFailure "cached path fetched a Pyth payload" >> pure Nothing)
        (\_ -> expectationFailure "cached path quoted a Pyth fee" >> pure (Right 1))
        (settlementStatus CachedMark)
        `shouldReturn` Right
          (cfgPerpsHousePool settlementConfig, 0, settleLpEpochPoolCall 123_456_789 1_700_000_000)

    it "loads at the required publish bound and quotes the exact admitted payload for Router" $ do
      effects <- newIORef ([] :: [Text])
      let load bound = do
            bound `shouldBe` 100
            modifyIORef' effects (<> ["load"])
            pure $ Just cachedPayload
          quote payload = do
            payload `shouldBe` sixFeedData
            modifyIORef' effects (<> ["quote"])
            pure $ Right 987_654
      buildLpSettlementTransactionWith settlementConfig load quote (settlementStatus AtomicOracleRefresh)
        `shouldReturn` Right
          (cfgPerpsOrderRouter settlementConfig,987_654,settleLpEpochRouterCall sixFeedData)
      readIORef effects `shouldReturn` ["load","quote"]

    it "does not quote when the cache is empty or its publish times fail admission" $ do
      let noQuote _ = expectationFailure "invalid payload reached fee quote" >> pure (Right 1)
      buildLpSettlementTransactionWith settlementConfig (\_ -> pure Nothing) noQuote
        (settlementStatus AtomicOracleRefresh)
        `shouldReturn` Left "no admitted latest six-feed Pyth payload is cached"
      result <- buildLpSettlementTransactionWith settlementConfig
        (\_ -> pure $ Just cachedPayload {puprPublishTimes = toJSON (replicate 6 (99 :: Integer))})
        noQuote (settlementStatus AtomicOracleRefresh)
      result `shouldBe` Left "the latest Pyth payload predates the minimum atomic publish time"

    it "propagates a fee quote failure instead of building a payable transaction" $
      buildLpSettlementTransactionWith settlementConfig (\_ -> pure $ Just cachedPayload)
        (\_ -> pure $ Left $ RpcNodeError (-32000) "fee unavailable" Nothing)
        (settlementStatus AtomicOracleRefresh)
        `shouldReturn` Left "RPC node error -32000: fee unavailable"

privateKeyOne :: Text
privateKeyOne = "0x0000000000000000000000000000000000000000000000000000000000000001"

sampleTx :: Tx1559
sampleTx =
  Tx1559
    { txChainId = 421614
    , txNonce = 0
    , txMaxPriorityFeePerGas = 1_000_000
    , txMaxFeePerGas = 2_000_000
    , txGasLimit = 21_000
    , txTo = "0x0000000000000000000000000000000000000001"
    , txValue = 0
    , txData = ""
    }

rawTransactionVector :: BS.ByteString
rawTransactionVector =
  "02f86b83066eee80830f4240831e84808252089400000000000000000000000000000000000000018080c080a048aeac63d786d61a53995aa7ce8ea6e2c58d542eb6ffda4d2f2919e880f42c75a05942cfbde0f5579eb1302acc4808798f1d6c2ba35aeb630c04d16d869ed4ca05"

transactionHashVector :: Text
transactionHashVector = "0xc7c053de99614c47bee680ab0a4d5a6846e2aeed5ece72583af66e00973b6f55"

maximumCost :: Tx1559 -> Integer
maximumCost tx = txValue tx + txGasLimit tx * txMaxFeePerGas tx

decodeHex :: BS.ByteString -> IO BS.ByteString
decodeHex encoded =
  case B16.decode encoded of
    Right bytes -> pure bytes
    Left err -> expectationFailure ("invalid transaction fixture: " <> err) >> fail "unreachable"

settlementConfig :: Config
settlementConfig =
  Config
    { cfgRpcUrl = ""
    , cfgRpcAuthToken = Nothing
    , cfgChainId = 11155111
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Nothing
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = ""
    , cfgPythHistoryUrl = ""
    , cfgPythHermesUrl = ""
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 7
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgPerpsCandleWriteMode = PerpsCandleWritesDual
    , cfgPerpsCandleReadMode = PerpsCandleReadsRollup
    , cfgPerpsCandleReadIntervals = [60, 3600]
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = ""
    , cfgPerpsRpcAuthToken = Nothing
    , cfgPerpsChainId = 421614
    , cfgPerpsSettlementToken = ""
    , cfgPerpsOrderRouter = "0x2222222222222222222222222222222222222222"
    , cfgPerpsOrderLifecycleBook = Nothing
    , cfgPerpsCfdEngine = ""
    , cfgPerpsCfdEngineLens = ""
    , cfgPerpsCfdEngineSettlementSidecar = ""
    , cfgPerpsMarginClearinghouse = ""
    , cfgPerpsPletherOracle = ""
    , cfgPerpsAccountLens = ""
    , cfgPerpsHousePool = "0x1111111111111111111111111111111111111111"
    , cfgPerpsSettlementMonitorLens = "0xb80b72b6517d53ed6387c9873ea4b61d0c97ade8"
    , cfgPerpsIndexerStartBlock = 0
    , cfgVaultHistoryHousePoolAddress = "0x0000000000000000000000000000000000000001"
    , cfgVaultHistorySeniorVaultAddress = "0x0000000000000000000000000000000000000002"
    , cfgVaultHistoryJuniorVaultAddress = "0x0000000000000000000000000000000000000003"
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryConfirmations = 12
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = settlementReleaseManifest
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Nothing
    , cfgFaucetGuardConfig = Nothing
    , cfgNativeAaConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgTestnetMarginFundingEnabled = False
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperIdlePollSeconds = 5
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    , cfgLpSettlementMode = LpSettlementOff
    , cfgLpSettlementPrivateKey = Nothing
    , cfgLpSettlementSeniorVault = "0xee755754319be980dd0f7df77107720485fe9d20"
    , cfgLpSettlementJuniorVault = "0x1d4beed4d594cadfe4c3c5bdc8174b0fbfc4aaf0"
    , cfgLpSettlementPollSeconds = 15
    , cfgLpSettlementMaxDrainTransactions = 4
    , cfgLpSettlementPendingReplacementSeconds = 60
    , cfgLpSettlementMaxReplacements = 3
    , cfgLpSettlementMaxTxCostWei = 0
    }

settlementReleaseManifest :: CompetitionReleaseManifest
settlementReleaseManifest =
  CompetitionReleaseManifest
    { crmReleaseId = "perps-rollup-test"
    , crmChainId = 421614
    , crmSettlementToken = ""
    , crmOrderRouter = "0xrouter"
    , crmMarginClearinghouse = ""
    , crmAccountLens = ""
    , crmCfdEngine = ""
    , crmCfdEngineLens = ""
    , crmSettlementSidecar = ""
    , crmPletherOracle = ""
    , crmIndexerStartBlock = 0
    }

settlementStatus :: ExecutionPath -> SettlementStatus
settlementStatus path =
  SettlementStatus
    { ssObservedBlock = 302_300_000
    , ssCurrentEpoch = 500_000
    , ssSettlementCutoffEpoch = 500_000
    , ssMinimumAtomicPublishTime = 100
    , ssSeniorMaturedDepositHeadEpoch = Nothing
    , ssSeniorMaturedDepositHeadAssets = 0
    , ssSeniorMaturedRedeemHeadEpoch = Just 499_999
    , ssSeniorMaturedRedeemHeadShares = 1
    , ssJuniorMaturedDepositHeadEpoch = Nothing
    , ssJuniorMaturedDepositHeadAssets = 0
    , ssJuniorMaturedRedeemHeadEpoch = Nothing
    , ssJuniorMaturedRedeemHeadShares = 0
    , ssOldestMaturedHead = Just 499_999
    , ssRequiredExecutionPath = path
    , ssCachedMarkPrice = 123_456_789
    , ssCachedMarkTime = 1_700_000_000
    , ssOperationalBlockerMask = 0
    , ssWarningMask = 0
    , ssExecutionPathDependencyMask = 0
    , ssDependencyFailureMask = 0
    , ssHasMaturedWork = True
    , ssLpEpochSettlementPaused = False
    }

sixFeedData :: [BS.ByteString]
sixFeedData = map BS.singleton [1..6]

cachedPayload :: PythUpdatePayloadRow
cachedPayload = PythUpdatePayloadRow 100 105 (toJSON ([100..105] :: [Integer]))
  (toJSON $ map (TE.decodeUtf8 . B16.encode) sixFeedData) 106 "latest"

persistedIntent :: LpSettlementTransactionRow
persistedIntent = LpSettlementTransactionRow
  { lstrId = 1, lstrChainId = 421614, lstrMonitorAddress = "monitor", lstrObservationDigest = "digest"
  , lstrEpoch = 500000, lstrReplacementCount = 0, lstrReplacesAttemptId = Nothing
  , lstrSignerAddress = "signer", lstrNonce = 17
  , lstrTargetAddress = "0x2222222222222222222222222222222222222222"
  , lstrValue = 987_654, lstrCalldata = settleLpEpochRouterCall sixFeedData
  , lstrGasLimit = 21_000, lstrMaxPriorityFeePerGas = 1_000_000, lstrMaxFeePerGas = 2_000_000
  , lstrSignedRawTransaction = "raw", lstrSignedTransactionHash = "hash", lstrStatus = "pending"
  , lstrLastError = Nothing, lstrReceiptTransactionHash = Nothing, lstrReceiptBlockNumber = Nothing
  , lstrReceiptBlockHash = Nothing, lstrReceiptSucceeded = Nothing, lstrConfirmedAt = Nothing
  , lstrConfirmationDepth = Nothing, lstrSettlementEventLogIndex = Nothing, lstrCutoffEpoch = Nothing
  , lstrSeniorRedeemAssets = Nothing, lstrJuniorRedeemAssets = Nothing
  , lstrJuniorDepositAssets = Nothing, lstrSeniorDepositAssets = Nothing
  , lstrSeniorBacklog = Nothing, lstrJuniorBacklog = Nothing, lstrEntriesDeferred = Nothing
  , lstrCreatedAt = read "2026-11-01 00:00:00 UTC", lstrUpdatedAt = read "2026-11-01 00:00:00 UTC"
  }
