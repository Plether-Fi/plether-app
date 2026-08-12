module Plether.AA.SimpleAccountSpec (spec) where

import Plether.AA.SimpleAccount (deriveTradingAccountAddress)
import Test.Hspec

spec :: Spec
spec = do
  describe "deriveTradingAccountAddress" $ do
    it "derives canonical v0.8 SimpleAccount addresses locally" $ do
      deriveTradingAccountAddress "0x609e9e87fDaF435CF27C684f58999B3d7b669b0B"
        `shouldBe` Right "0x24d6ea058f84834633432d8a57892a49b46f5236"
      deriveTradingAccountAddress "0x5D3A31cEa19fd8F3E9da88cd3b0E5b95D30a8895"
        `shouldBe` Right "0xcc890fcb535070ef8f3292c7b8b0468ea66a920f"
      deriveTradingAccountAddress "0x69cf2f5138244852a3cd58eb8a5096f2154991ca"
        `shouldBe` Right "0x5e3c355b504dac2d9dfae24ce00da0d99df354f0"

    it "rejects malformed owner wallets" $
      deriveTradingAccountAddress "not-an-address"
        `shouldBe` Left "OWNER_WALLET must be a valid Ethereum address"
