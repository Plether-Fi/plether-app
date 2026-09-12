module Plether.AA.ExecutionDiagnosticsSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Plether.AA.ExecutionDiagnostics (executionOutOfGas, gasUtilizationBps)
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi (encodeUint256)
import Test.Hspec

spec :: Spec
spec = describe "verified UserOperation out-of-gas diagnostics" $ do
  it "measures total receipt gas against total allowance, not just execution gas" $ do
    let op = object ["sender" .= sender,"nonce" .= ("0x0" :: Text),"callData" .= calldata,
          "callGasLimit" .= ("0x64" :: Text),"verificationGasLimit" .= ("0x64" :: Text),
          "preVerificationGas" .= ("0x64" :: Text),"maxFeePerGas" .= ("0x1" :: Text),"maxPriorityFeePerGas" .= ("0x0" :: Text)]
        event = object ["data" .= ("0x" <> TE.decodeUtf8 (B16.encode $ mconcat $ map encodeUint256 [0,0,100,150]))]
    gasUtilizationBps op event `shouldBe` Just 5000
    gasUtilizationBps op (object ["data" .= ("0x00" :: Text)]) `shouldBe` Nothing
  it "recognizes the incident's propagated nested OOG" $
    executionOutOfGas sender calldata (root [failed [oog]]) `shouldBe` True
  it "does not infer OOG from empty reverts or successful execution" $ do
    executionOutOfGas sender calldata (root [failed []]) `shouldBe` False
    executionOutOfGas sender calldata (root [node ep sender calldata Nothing [oog]]) `shouldBe` False
  it "ignores caught OOG underneath a successful intermediate call" $
    executionOutOfGas sender calldata (root [failed [node sender other "0xab" Nothing [oog]]]) `shouldBe` False
  it "rejects ambiguous duplicate operations and mismatched sender/calldata" $ do
    executionOutOfGas sender calldata (root [failed [oog],failed [oog]]) `shouldBe` False
    executionOutOfGas other calldata (root [failed [oog]]) `shouldBe` False
    executionOutOfGas sender "0xdead" (root [failed [oog]]) `shouldBe` False
  it "rejects malformed traces rather than guessing a historical cause" $ do
    executionOutOfGas sender calldata (object ["calls" .= ("secret" :: Text)]) `shouldBe` False
    executionOutOfGas sender calldata (node other ep "0xab" (Just "reverted") [failed [oog]]) `shouldBe` False
 where
  ep = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"
  sender = "0x1111111111111111111111111111111111111111"
  other = "0x2222222222222222222222222222222222222222"
  calldata = "0x34fcd5be0000"
  root = node other ep "0x765e827f" Nothing
  failed = node ep sender calldata (Just "execution reverted")
  oog = node sender other "0x93e21b0a" (Just "out of gas: not enough gas for reentrancy sentry") []

node :: Text -> Text -> Text -> Maybe Text -> [Value] -> Value
node from to input err children = object $
  ["from" .= from,"to" .= to,"input" .= input,"calls" .= children]
    ++ maybe [] (\e -> ["error" .= e]) err
