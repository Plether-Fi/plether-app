module Plether.LiquidationWorkerSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (toJSON)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Plether.Config (Config (..))
import Plether.Database.Schema (PythUpdatePayloadRow (..))
import Plether.Ethereum.Abi (encodeAddress, encodeUint256)
import Plether.Ethereum.Client (RpcError (..))
import Plether.Ethereum.Contracts.Perps (positionLiquidatedTopic)
import Plether.Ethereum.Rpc (RpcLog (..), TxReceipt (..))
import Plether.LiquidationWorker
  ( LiquidationWorkerConfig (..)
  , decodeCachedPythPayload
  , isExpectedLiquidationSimulationRevert
  , isLiquidationReceiptFor
  , liquidationIndexRange
  , loadLiquidationWorkerConfig
  , sameNonceReplacementFees
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = do
  describe "loadLiquidationWorkerConfig" $
    it "uses the CFD engine already resolved by shared backend configuration" $ do
      withUnsetEnv "PERPS_CFD_ENGINE" $ do
        workerCfg <- loadLiquidationWorkerConfig testConfig "private-key"
        lwcCfdEngine workerCfg `shouldBe` configuredCfdEngine
        lwcCfdEngine workerCfg `shouldNotBe` retiredCfdEngine

  describe "decodeCachedPythPayload" $ do
    it "decodes the latest cached publish times and update bytes" $ do
      decodeCachedPythPayload payload
        `shouldBe` Right ([101, 102], [BS.pack [0x01, 0x02], BS.pack [0xff]])

    it "rejects invalid update hex" $ do
      decodeCachedPythPayload payload {puprUpdateData = toJSON (["0xzz"] :: [String])}
        `shouldSatisfy` isLeft

  describe "isLiquidationReceiptFor" $ do
    it "accepts a successful matching PositionLiquidated event" $ do
      isLiquidationReceiptFor cfdEngine account (receipt True cfdEngine account) `shouldBe` True

    it "rejects failed receipts, another account, and another emitter" $ do
      isLiquidationReceiptFor cfdEngine account (receipt False cfdEngine account) `shouldBe` False
      isLiquidationReceiptFor cfdEngine account (receipt True cfdEngine otherAccount) `shouldBe` False
      isLiquidationReceiptFor cfdEngine account (receipt True otherEngine account) `shouldBe` False

  describe "isExpectedLiquidationSimulationRevert" $ do
    it "accepts the exact solvent and no-position engine reverts" $ do
      isExpectedLiquidationSimulationRevert
        (RpcNodeError 3 "execution reverted" (Just "0x451cebb2"))
        `shouldBe` True
      isExpectedLiquidationSimulationRevert
        (RpcNodeError 3 "execution reverted" (Just "0x4565ea0c"))
        `shouldBe` True

    it "rejects transport, oracle, and unstructured node errors" $ do
      isExpectedLiquidationSimulationRevert (RpcHttpError "timeout") `shouldBe` False
      isExpectedLiquidationSimulationRevert
        (RpcNodeError 3 "execution reverted" (Just "0xf4a25e0f"))
        `shouldBe` False
      isExpectedLiquidationSimulationRevert
        (RpcNodeError (-32000) "insufficient funds" Nothing)
        `shouldBe` False

  describe "liquidationIndexRange" $ do
    it "starts at the configured deployment block and respects confirmations" $ do
      liquidationIndexRange 100 1 5_000 12 0 200 `shouldBe` Just (100, 199)

    it "rescans a bounded overlap after catching up" $ do
      liquidationIndexRange 100 1 5_000 12 150 155 `shouldBe` Just (139, 154)

    it "still advances when the configured batch size is one" $ do
      liquidationIndexRange 100 1 1 12 150 155 `shouldBe` Just (151, 151)

    it "waits when the configured start has not reached confirmation depth" $ do
      liquidationIndexRange 200 1 5_000 12 0 150 `shouldBe` Nothing

  describe "sameNonceReplacementFees" $ do
    it "bumps both prior fee fields by at least 12.5 percent" $ do
      sameNonceReplacementFees 2_500 50 1 10 100 `shouldBe` (12, 113)

    it "uses current buffered network fees when they are higher" $ do
      sameNonceReplacementFees 2_500 200 5 10 100 `shouldBe` (12, 250)

payload :: PythUpdatePayloadRow
payload =
  PythUpdatePayloadRow
    { puprMinPublishTime = 101
    , puprMaxPublishTime = 102
    , puprPublishTimes = toJSON ([101, 102] :: [Integer])
    , puprUpdateData = toJSON (["0x0102", "0xff"] :: [String])
    , puprFetchedAt = 103
    , puprSource = "backend_hermes_latest"
    }

account :: Text
account = "0x1111111111111111111111111111111111111111"

otherAccount :: Text
otherAccount = "0x2222222222222222222222222222222222222222"

cfdEngine :: Text
cfdEngine = "0x3333333333333333333333333333333333333333"

otherEngine :: Text
otherEngine = "0x4444444444444444444444444444444444444444"

configuredCfdEngine :: Text
configuredCfdEngine = "0x5555555555555555555555555555555555555555"

retiredCfdEngine :: Text
retiredCfdEngine = "0xA1Ebfb8aD9C90367eA30A29592419d447E3f8224"

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
    , cfgPythIngestionEnabled = False
    , cfgPerpsRpcUrl = "https://arb-sepolia.example"
    , cfgPerpsChainId = 421614
    , cfgPerpsUsdc = "0x1111111111111111111111111111111111111111"
    , cfgPerpsOrderRouter = "0x2222222222222222222222222222222222222222"
    , cfgPerpsCfdEngine = configuredCfdEngine
    , cfgPerpsMarginClearinghouse = "0x3333333333333333333333333333333333333333"
    , cfgPerpsPletherOracle = "0x4444444444444444444444444444444444444444"
    , cfgPerpsIndexerStartBlock = 288439939
    , cfgAaConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    }

withUnsetEnv :: String -> IO a -> IO a
withUnsetEnv name action =
  bracket
    (do
        previous <- lookupEnv name
        unsetEnv name
        pure previous
    )
    (maybe (unsetEnv name) (setEnv name))
    (const action)

receipt :: Bool -> Text -> Text -> TxReceipt
receipt succeeded emitter eventAccount =
  TxReceipt
    { receiptTxHash = "0xabc"
    , receiptBlockNumber = 123
    , receiptSucceeded = succeeded
    , receiptLogs =
        [ RpcLog
            { rpcLogTxHash = "0xabc"
            , rpcLogBlockNumber = 123
            , rpcLogAddress = emitter
            , rpcLogTopics = [positionLiquidatedTopic, encodeAddress eventAccount]
            , rpcLogData =
                encodeUint256 0
                  <> encodeUint256 1
                  <> encodeUint256 100_000_000
                  <> encodeUint256 5_000_000
            }
        ]
    }

isLeft :: Either a b -> Bool
isLeft value =
  case value of
    Left _ -> True
    Right _ -> False
