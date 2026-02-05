module Plether.Indexer.ContractsSpec (spec) where

import qualified Data.ByteString as BS
import Test.Hspec

import Plether.Indexer.Contracts

spec :: Spec
spec = do
  describe "event topic signatures" $ do
    it "Mint topic is keccak256 of signature" $
      esTopic mintEvent `shouldBe` keccak256Text "Mint(address,uint256,uint256)"

    it "Burn topic is keccak256 of signature" $
      esTopic burnEvent `shouldBe` keccak256Text "Burn(address,uint256,uint256)"

    it "TokenExchange topic is keccak256 of signature" $
      esTopic tokenExchangeEvent `shouldBe`
        keccak256Text "TokenExchange(address,uint256,uint256,uint256,uint256)"

    it "ZapMint topic is keccak256 of signature" $
      esTopic zapMintEvent `shouldBe` keccak256Text "ZapMint(address,uint256,uint256)"

    it "ZapBurn topic is keccak256 of signature" $
      esTopic zapBurnEvent `shouldBe` keccak256Text "ZapBurn(address,uint256,uint256)"

    it "Deposit (staking) topic is keccak256 of signature" $
      esTopic stakingDepositEvent `shouldBe`
        keccak256Text "Deposit(address,address,uint256,uint256)"

    it "Withdraw (staking) topic is keccak256 of signature" $
      esTopic stakingWithdrawEvent `shouldBe`
        keccak256Text "Withdraw(address,address,address,uint256,uint256)"

    it "PositionOpened topic is keccak256 of signature" $
      esTopic positionOpenedEvent `shouldBe`
        keccak256Text "PositionOpened(address,uint256,uint256,uint256,uint256)"

    it "PositionClosed topic is keccak256 of signature" $
      esTopic positionClosedEvent `shouldBe`
        keccak256Text "PositionClosed(address,uint256,uint256)"

  describe "event signature topics are 32 bytes" $ do
    it "mintEvent topic is 32 bytes" $
      BS.length (esTopic mintEvent) `shouldBe` 32

    it "burnEvent topic is 32 bytes" $
      BS.length (esTopic burnEvent) `shouldBe` 32

    it "tokenExchangeEvent topic is 32 bytes" $
      BS.length (esTopic tokenExchangeEvent) `shouldBe` 32

    it "zapMintEvent topic is 32 bytes" $
      BS.length (esTopic zapMintEvent) `shouldBe` 32

    it "zapBurnEvent topic is 32 bytes" $
      BS.length (esTopic zapBurnEvent) `shouldBe` 32

    it "stakingDepositEvent topic is 32 bytes" $
      BS.length (esTopic stakingDepositEvent) `shouldBe` 32

    it "stakingWithdrawEvent topic is 32 bytes" $
      BS.length (esTopic stakingWithdrawEvent) `shouldBe` 32

    it "positionOpenedEvent topic is 32 bytes" $
      BS.length (esTopic positionOpenedEvent) `shouldBe` 32

    it "positionClosedEvent topic is 32 bytes" $
      BS.length (esTopic positionClosedEvent) `shouldBe` 32

  describe "event metadata" $ do
    it "mintEvent has correct name and txType" $ do
      esName mintEvent `shouldBe` "Mint"
      esTxType mintEvent `shouldBe` "mint"
      esSide mintEvent `shouldBe` Nothing

    it "burnEvent has correct name and txType" $ do
      esName burnEvent `shouldBe` "Burn"
      esTxType burnEvent `shouldBe` "burn"
      esSide burnEvent `shouldBe` Nothing

    it "tokenExchangeEvent has correct name and txType" $ do
      esName tokenExchangeEvent `shouldBe` "TokenExchange"
      esTxType tokenExchangeEvent `shouldBe` "swap"
      esSide tokenExchangeEvent `shouldBe` Nothing

    it "zapMintEvent has correct side" $ do
      esName zapMintEvent `shouldBe` "ZapMint"
      esTxType zapMintEvent `shouldBe` "zap_buy"
      esSide zapMintEvent `shouldBe` Just "bull"

    it "zapBurnEvent has correct side" $ do
      esName zapBurnEvent `shouldBe` "ZapBurn"
      esTxType zapBurnEvent `shouldBe` "zap_sell"
      esSide zapBurnEvent `shouldBe` Just "bull"

    it "stakingDepositEvent has correct metadata" $ do
      esName stakingDepositEvent `shouldBe` "Deposit"
      esTxType stakingDepositEvent `shouldBe` "stake"

    it "stakingWithdrawEvent has correct metadata" $ do
      esName stakingWithdrawEvent `shouldBe` "Withdraw"
      esTxType stakingWithdrawEvent `shouldBe` "unstake"

    it "positionOpenedEvent has correct metadata" $ do
      esName positionOpenedEvent `shouldBe` "PositionOpened"
      esTxType positionOpenedEvent `shouldBe` "leverage_open"

    it "positionClosedEvent has correct metadata" $ do
      esName positionClosedEvent `shouldBe` "PositionClosed"
      esTxType positionClosedEvent `shouldBe` "leverage_close"

  describe "allEventSignatures" $ do
    it "contains all 9 events" $
      length allEventSignatures `shouldBe` 9

    it "all events have unique topics" $
      let topics = map esTopic allEventSignatures
      in length topics `shouldBe` length (removeDuplicates topics)

    it "all events have unique names" $
      let names = map esName allEventSignatures
      in length names `shouldBe` length (removeDuplicates names)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
