module Plether.LiquidationWorkerSpec (spec) where

import Data.Aeson (toJSON)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Plether.Database.Schema (PythUpdatePayloadRow (..))
import Plether.Ethereum.Abi (encodeAddress, encodeUint256)
import Plether.Ethereum.Client (RpcError (..))
import Plether.Ethereum.Contracts.Perps (positionLiquidatedTopic)
import Plether.Ethereum.Rpc (RpcLog (..), TxReceipt (..))
import Plether.Ethereum.Transaction (Tx1559 (..))
import Plether.LiquidationWorker
  ( LiquidationPayloadCircuitDecision (..)
  , LiquidationPendingSignerAction (..)
  , LiquidationSignerCircuitDecision (..)
  , canAffordTransaction
  , checkLiveSignerBalance
  , decodeCachedPythPayload
  , isExpectedLiquidationSimulationRevert
  , isInsufficientFundsRpcError
  , isLiquidationReceiptFor
  , liquidationIndexRange
  , liquidationPayloadCircuitDecision
  , liquidationPayloadFingerprint
  , liquidationPendingSignerAction
  , liquidationSignerCircuitDecision
  , payloadGlobalSimulationRevertSelector
  , sameNonceReplacementFees
  , transactionMaximumCost
  )
import Test.Hspec

spec :: Spec
spec = do
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
      isExpectedLiquidationSimulationRevert
        (RpcNodeError 3 "execution reverted: 0x451CEBB2" Nothing)
        `shouldBe` True

    it "rejects transport, oracle, and unstructured node errors" $ do
      isExpectedLiquidationSimulationRevert (RpcHttpError "timeout") `shouldBe` False
      isExpectedLiquidationSimulationRevert
        (RpcNodeError 3 "execution reverted" (Just "0xf4a25e0f"))
        `shouldBe` False
      isExpectedLiquidationSimulationRevert
        (RpcNodeError (-32000) "insufficient funds" Nothing)
        `shouldBe` False

  describe "payloadGlobalSimulationRevertSelector" $ do
    it "classifies the observed invalid-VAA and stale-price payload failures" $ do
      payloadGlobalSimulationRevertSelector
        (RpcNodeError 3 "execution reverted" (Just "0x2ACBE915"))
        `shouldBe` Just "0x2acbe915"
      payloadGlobalSimulationRevertSelector
        (RpcNodeError 3 "execution reverted" (Just "nested data: 0xf4a25e0f00"))
        `shouldBe` Just "0xf4a25e0f"
      payloadGlobalSimulationRevertSelector
        (RpcNodeError 3 "execution reverted: 0x2ACBE915" Nothing)
        `shouldBe` Just "0x2acbe915"

    it "does not classify transport, fee, or unrelated simulation failures" $ do
      payloadGlobalSimulationRevertSelector (RpcHttpError "timeout") `shouldBe` Nothing
      payloadGlobalSimulationRevertSelector
        (RpcNodeError (-32000) "insufficient funds for gas * price + value" Nothing)
        `shouldBe` Nothing
      payloadGlobalSimulationRevertSelector
        (RpcNodeError 3 "execution reverted" (Just "0x451cebb2"))
        `shouldBe` Nothing

  describe "liquidationPayloadFingerprint" $ do
    it "is stable across equivalent address formatting" $ do
      liquidationPayloadFingerprint "  0xABC  " "0xDEF" updateData
        `shouldBe` liquidationPayloadFingerprint "0xabc" "0xdef" updateData

    it "changes with either contract domain or update byte boundaries" $ do
      let fingerprint = liquidationPayloadFingerprint "0xabc" "0xdef" updateData
      fingerprint `shouldNotBe` liquidationPayloadFingerprint "0xabd" "0xdef" updateData
      fingerprint `shouldNotBe` liquidationPayloadFingerprint "0xabc" "0xdee" updateData
      fingerprint
        `shouldNotBe` liquidationPayloadFingerprint
          "0xabc"
          "0xdef"
          [BS.pack [0x01, 0x02], BS.pack [0x03]]

  describe "liquidationPayloadCircuitDecision" $ do
    it "processes without a rejection and suppresses only the same rejected key" $ do
      liquidationPayloadCircuitDecision Nothing "0xaaa"
        `shouldBe` ProcessLiquidationPayload
      liquidationPayloadCircuitDecision (Just "0xAaA") "0xaaa"
        `shouldBe` SuppressRejectedLiquidationPayload

    it "clears the circuit and resumes immediately for a new key" $ do
      liquidationPayloadCircuitDecision (Just "0xaaa") "0xbbb"
        `shouldBe` ClearRejectedLiquidationPayload

  describe "liquidationSignerCircuitDecision" $ do
    it "allows one freshly priced attempt when the persisted cooldown is due" $ do
      liquidationSignerCircuitDecision Nothing `shouldBe` SignerTransactionReady
      liquidationSignerCircuitDecision (Just True) `shouldBe` RecheckSignerTransaction

    it "suppresses new signer transactions during the persistent cooldown" $ do
      liquidationSignerCircuitDecision (Just False) `shouldBe` SuppressSignerTransaction

    it "keeps the persisted raw transaction live while replacement is suppressed" $ do
      liquidationPendingSignerAction False True `shouldBe` RebroadcastPendingSignerTransaction
      liquidationPendingSignerAction False False `shouldBe` WaitForPendingSignerTransaction
      liquidationPendingSignerAction True False `shouldBe` ReplacePendingSignerTransaction

  describe "isInsufficientFundsRpcError" $ do
    it "classifies deterministic node funding failures from message or data" $ do
      isInsufficientFundsRpcError
        (RpcNodeError (-32000) "insufficient funds for gas * price + value" Nothing)
        `shouldBe` True
      isInsufficientFundsRpcError
        (RpcNodeError 3 "transaction rejected" (Just "insufficient balance for transfer"))
        `shouldBe` True

    it "does not turn transport or unrelated node failures into a signer circuit" $ do
      isInsufficientFundsRpcError (RpcHttpError "timeout") `shouldBe` False
      isInsufficientFundsRpcError
        (RpcNodeError 3 "execution reverted" (Just "0x2acbe915"))
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

  describe "checkLiveSignerBalance" $ do
    it "does not evaluate the balance request in dry-run mode" $ do
      called <- newIORef False
      result <-
        checkLiveSignerBalance True $ do
          writeIORef called True
          pure $ Right 1
      result `shouldBe` Right Nothing
      readIORef called `shouldReturn` False

    it "fails closed for an RPC error or a zero balance" $ do
      rpcFailure <- checkLiveSignerBalance False $ pure $ Left $ RpcHttpError "timeout"
      zeroBalance <- checkLiveSignerBalance False $ pure $ Right 0
      rpcFailure `shouldSatisfy` isLeft
      zeroBalance `shouldSatisfy` isLeft

    it "returns a positive live signer balance" $ do
      checkLiveSignerBalance False (pure $ Right 123) `shouldReturn` Right (Just 123)

  describe "transaction affordability" $ do
    it "includes value and maximum EIP-1559 gas cost" $ do
      transactionMaximumCost maximumCostTx `shouldBe` 2_100_007

    it "accepts the exact maximum cost and rejects one wei less" $ do
      canAffordTransaction 2_100_007 maximumCostTx `shouldBe` True
      canAffordTransaction 2_100_006 maximumCostTx `shouldBe` False

payload :: PythUpdatePayloadRow
payload =
  PythUpdatePayloadRow
    { puprMinPublishTime = 101
    , puprMaxPublishTime = 102
    , puprPublishTimes = toJSON ([101, 102] :: [Integer])
    , puprUpdateData = toJSON (["0x0102", "0xff"] :: [String])
    , puprFetchedAt = 103
    , puprSource = "backend_hermes_latest_v2"
    }

account :: Text
account = "0x1111111111111111111111111111111111111111"

otherAccount :: Text
otherAccount = "0x2222222222222222222222222222222222222222"

cfdEngine :: Text
cfdEngine = "0x3333333333333333333333333333333333333333"

otherEngine :: Text
otherEngine = "0x4444444444444444444444444444444444444444"

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

maximumCostTx :: Tx1559
maximumCostTx =
  Tx1559
    { txChainId = 1
    , txNonce = 0
    , txMaxPriorityFeePerGas = 1
    , txMaxFeePerGas = 100
    , txGasLimit = 21_000
    , txTo = account
    , txValue = 7
    , txData = ""
    }

updateData :: [BS.ByteString]
updateData = [BS.pack [0x01], BS.pack [0x02, 0x03]]
