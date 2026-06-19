module Plether.Ethereum.TransactionSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Transaction
  ( SignedTransaction (..)
  , Tx1559 (..)
  , deriveAddress
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
