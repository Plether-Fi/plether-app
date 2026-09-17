module Plether.AA.RecoveryCapabilitySpec (spec) where

import qualified Data.Text as T
import Plether.AA.RecoveryCapability
import Test.Hspec

spec :: Spec
spec = describe "operation-scoped recovery credentials" $ do
  let op = "0x" <> T.replicate 64 "a"
      client = "0x" <> T.replicate 64 "b"
      token = issue "secret" "paymaster" 1000 op client
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
