module Plether.AA.PreparationRecoverySpec (spec) where

import Data.Aeson (Value(..), object, (.=))
import Data.Either (isLeft)
import qualified Data.Text as T
import Test.Hspec
import Plether.Insights.Registration.Wallet (recoverPersonalSignAddress)
import Plether.AA.PreparationRecovery
import Plether.Database.AaPreparationRecovery (Scope(..))

spec :: Spec
spec = describe "owner-scoped preparation recovery protocol" $ do
  let sender = "0x" <> T.replicate 40 "1"
      paymaster = "0x" <> T.replicate 40 "2"
      identifier = "0x" <> T.replicate 64 "3"
      scope = Scope 421614 paymaster sender identifier
      locator chain = object ["version" .= (1::Int), "chainId" .= (chain::T.Text), "sender" .= sender, "preparationId" .= identifier]
  it "requires an explicit Sepolia loopback origin while keeping the hosted origin" $ do
    let hosted = Just "https://app.sepolia.plether.com"
        local = "http://127.0.0.1:5192"
    parseRecoveryOrigins 421614 hosted "" `shouldBe` Right ["https://app.sepolia.plether.com"]
    parseRecoveryOrigins 421614 hosted local `shouldBe` Right ["https://app.sepolia.plether.com", local]
    parseRecoveryOrigins 1 hosted local `shouldSatisfy` isLeft
    parseRecoveryOrigins 42161 hosted local `shouldSatisfy` isLeft
    parseRecoveryOrigins 421614 Nothing local `shouldSatisfy` isLeft
    parseRecoveryOrigins 421614 Nothing "" `shouldBe` Right []
    parseRecoveryOrigins 421614 (Just local) "" `shouldSatisfy` isLeft
  it "accepts distinct exact local origins and rejects wildcard, path and authority tricks" $ do
    let hosted = Just "https://app.sepolia.plether.com"
    parseRecoveryOrigins 421614 hosted "http://localhost:5192,http://[::1]:5192"
      `shouldBe` Right ["https://app.sepolia.plether.com", "http://localhost:5192", "http://[::1]:5192"]
    mapM_ (\origin -> parseRecoveryOrigins 421614 hosted origin `shouldSatisfy` isLeft)
      [ "http://127.0.0.1.evil.example:5192", "http://0.0.0.0:5192", "http://example.com"
      , "http://127.0.0.1:5192/", "http://127.0.0.1:5192/path", "http://127.0.0.1:5192?x=1"
      , "http://127.0.0.1:5192#fragment", "http://owner@127.0.0.1:5192", "http://*.localhost:5192"
      , "http://127.0.0.1:0", "http://127.0.0.1:65536", "http://127.0.0.1:05192"
      , "http://127.0.0.1:5192,http://127.0.0.1:5192", "http://127.0.0.1:5192,"
      , "https://app.sepolia.plether.com", "https://example.com/path" ]
  it "binds a local recovery signature to its exact configured origin and port" $ do
    let message = renderChallenge "http://127.0.0.1:5192" scope sender "nonce" 123
    message `shouldSatisfy` T.isInfixOf "Origin: http://127.0.0.1:5192"
    message `shouldNotBe` renderChallenge "https://app.sepolia.plether.com" scope sender "nonce" 123
    message `shouldNotBe` renderChallenge "http://127.0.0.1:5173" scope sender "nonce" 123
  it "parses only the versioned Sepolia locator" $ do
    fmap fst (parseRecoveryRequest paymaster [] [locator "0x66eee"]) `shouldBe` Right scope
    parseRecoveryRequest paymaster [] [locator "0x1"] `shouldSatisfy` isLeft
    parseRecoveryRequest paymaster [] [Null] `shouldSatisfy` isLeft
  it "binds the wallet message to purpose, deployment and attempt" $ do
    let message = renderChallenge "https://testnet.plether.com" scope sender "nonce" 123
    message `shouldSatisfy` T.isInfixOf "does not authorize a blockchain transaction"
    message `shouldSatisfy` T.isInfixOf identifier
    message `shouldNotBe` renderChallenge "https://another.example" scope sender "nonce" 123
    message `shouldNotBe` renderChallenge "https://testnet.plether.com" (scope { scopePaymaster = sender }) sender "nonce" 123
    message `shouldNotBe` renderChallenge "https://testnet.plether.com" scope sender "other" 123
  it "issues opaque tokens and stores a separate digest" $ do
    first <- randomToken
    second <- randomToken
    T.length first `shouldBe` 64
    first `shouldNotBe` second
    tokenHash first `shouldNotBe` first
  it "verifies a viem personal-sign vector and rejects altered recovery scope" $ do
    let owner = "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"
        signature = "0x9d4986f005394511c1f794c1933b66a8fd2018c7a65df80f41c92023ed6bc186684738bdee4e0d56fd248eb0c4e3c1253237a5d3536906b13abdb80f3659020f1b"
        message = renderChallenge "https://testnet.plether.com" scope owner "nonce" 123
    recoverPersonalSignAddress message signature `shouldReturn` Right owner
    let altered = [renderChallenge "https://other.example" scope owner "nonce" 123,
          renderChallenge "https://testnet.plether.com" (scope { scopeId = "0x" <> T.replicate 64 "4" }) owner "nonce" 123,
          renderChallenge "https://testnet.plether.com" (scope { scopeSender = paymaster }) owner "nonce" 123,
          renderChallenge "https://testnet.plether.com" (scope { scopeChain = 1 }) owner "nonce" 123,
          renderChallenge "https://testnet.plether.com" (scope { scopePaymaster = sender }) owner "nonce" 123]
    mapM_ (\value -> recoverPersonalSignAddress value signature >>= (`shouldNotBe` Right owner)) altered
  it "allows verified missing attempts to retry while preserving original historical namespaces" $ do
    selectPreparationClient "new-ip" True [] `shouldBe` ClientProofRequired "new-ip"
    selectPreparationClient "new-ip" False [] `shouldBe` ClientAllowed "new-ip"
    selectPreparationClient "new-ip" True [("old-ip",Nothing,True)] `shouldBe` ClientProofRequired "old-ip"
    selectPreparationClient "new-ip" False [("old-ip",Nothing,True)] `shouldBe` ClientProofRequired "old-ip"
    selectPreparationClient "new-ip" True [("old-ip",Nothing,False)] `shouldBe` ClientAmbiguous
    selectPreparationClient "new-ip" True [("old-ip",Nothing,True),("other-ip",Nothing,True)] `shouldBe` ClientAmbiguous
