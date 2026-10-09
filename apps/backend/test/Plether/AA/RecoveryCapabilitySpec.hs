module Plether.AA.RecoveryCapabilitySpec (spec) where

import qualified Data.Text as T
import Plether.AA.RecoveryCapability
import qualified Plether.Perps.Manifest as Manifest
import Test.Hspec

spec :: Spec
spec = describe "operation-scoped recovery credentials" $ do
  let op = "0x" <> T.replicate 64 "a"
      client = "0x" <> T.replicate 64 "b"
      token = issue "secret" "paymaster" 1000 op client
  it "preserves the Sepolia credential bytes" $ do
    let payload = T.intercalate "." ["v1", op, client, "605800"]
    issueForChain 421614 "secret" "paymaster" 1000 op client
      `shouldBe` payload <> ".10382a1f8186459254d160572124b217b7c44eb88055ca46b1c1c9f2900947e5"
  it "rejects credentials from another chain even when all payload and deployment fields match" $ do
    let sepolia = issueForChain 421614 "secret" "paymaster" 1000 op client
        mainnet = issueForChain 42161 "secret" "paymaster" 1000 op client
    mainnet `shouldNotBe` sepolia
    verifyForChain 421614 "secret" "paymaster" 1001 op sepolia `shouldBe` Just client
    verifyForChain 42161 "secret" "paymaster" 1001 op mainnet `shouldBe` Just client
    verifyForChain 421614 "secret" "paymaster" 1001 op mainnet `shouldBe` Nothing
    verifyForChain 42161 "secret" "paymaster" 1001 op sepolia `shouldBe` Nothing
  it "uses the compiled release chain for production issuance and verification" $ do
    let otherChain = if Manifest.releaseChainId == 42161 then 421614 else 42161
    token `shouldBe` issueForChain Manifest.releaseChainId "secret" "paymaster" 1000 op client
    verifyForChain Manifest.releaseChainId "secret" "paymaster" 1001 op token `shouldBe` Just client
    verifyForChain otherChain "secret" "paymaster" 1001 op token `shouldBe` Nothing
    verify "secret" "paymaster" 1001 op (issueForChain otherChain "secret" "paymaster" 1000 op client)
      `shouldBe` Nothing
  it "recovers the original pseudonym independently of the current IP" $
    verify "secret" "paymaster" 1001 op token `shouldBe` Just client
  it "rejects another operation, secret or deployment" $ do
    verify "secret" "paymaster" 1001 ("0x" <> T.replicate 64 "c") token `shouldBe` Nothing
    verify "rotated" "paymaster" 1001 op token `shouldBe` Nothing
    verify "secret" "other" 1001 op token `shouldBe` Nothing
  it "expires at the boundary and rejects future-issued or tampered tokens" $ do
    verify "secret" "paymaster" 605799 op token `shouldBe` Just client
    verify "secret" "paymaster" 605800 op token `shouldBe` Nothing
    verify "secret" "paymaster" 999 op token `shouldBe` Nothing
    mapM_ (\bad -> verify "secret" "paymaster" 1001 op bad `shouldBe` Nothing)
      ["", token <> "extra", T.replace "605800" "605801" token, T.replace client op token]
