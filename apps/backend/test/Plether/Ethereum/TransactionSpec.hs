module Plether.Ethereum.TransactionSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
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
  , sameNonceReplacementFees
  , signTransaction
  )
import Test.Hspec

spec :: Spec
spec = do
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

    it "keeps the nonce and settlement semantics fixed while increasing the bounded cost" $ do
      let exactPythFee = 987_654
          updateData = map BS.singleton [1 .. 6]
          original =
            sampleTx
              { txNonce = 17
              , txMaxPriorityFeePerGas = 1_000_000
              , txMaxFeePerGas = 2_000_000
              , txGasLimit = 21_000
              , txTo = "0x2222222222222222222222222222222222222222"
              , txValue = exactPythFee
              , txData = settleLpEpochRouterCall updateData
              }
          (replacementPriorityFee, replacementMaxFee) =
            sameNonceReplacementFees
              2_500
              2_500_000
              500_000
              (txMaxPriorityFeePerGas original)
              (txMaxFeePerGas original)
          replacement =
            original
              { txMaxPriorityFeePerGas = replacementPriorityFee
              , txMaxFeePerGas = replacementMaxFee
              }
      txNonce replacement `shouldBe` txNonce original
      txTo replacement `shouldBe` txTo original
      txValue replacement `shouldBe` exactPythFee
      txData replacement `shouldBe` txData original
      replacementPriorityFee `shouldBe` 1_125_000
      replacementMaxFee `shouldBe` 3_125_000
      maximumCost replacement `shouldBe` 65_625_987_654
      maximumCost replacement `shouldSatisfy` (> maximumCost original)

  describe "LP settlement transaction value" $ do
    it "uses zero value for cached HousePool settlement" $ do
      let cached =
            sampleTx
              { txTo = "0x1111111111111111111111111111111111111111"
              , txValue = 0
              , txData = settleLpEpochPoolCall 123_456_789 1_700_000_000
              }
      txValue cached `shouldBe` 0
      txData cached
        `shouldBe` settleLpEpochPoolCall 123_456_789 1_700_000_000

    it "uses the exact quoted Pyth fee for a six-feed atomic Router settlement" $ do
      let exactPythFee = 987_654
          updateData = map BS.singleton [1 .. 6]
          atomic =
            sampleTx
              { txTo = "0x2222222222222222222222222222222222222222"
              , txValue = exactPythFee
              , txData = settleLpEpochRouterCall updateData
              }
      length updateData `shouldBe` 6
      txValue atomic `shouldBe` exactPythFee
      txData atomic `shouldBe` settleLpEpochRouterCall updateData

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
