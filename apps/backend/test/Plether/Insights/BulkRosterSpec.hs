module Plether.Insights.BulkRosterSpec (spec) where

import Data.Text qualified as T
import Plether.Insights.BulkRoster
  ( BulkParticipantEntry (..)
  , parseBulkParticipantEntries
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "parseBulkParticipantEntries" $ do
    it "derives canonical Trading Accounts without retaining owner wallets" $ do
      parseBulkParticipantEntries
        1
        "@trader_01\t550e8400-e29b-41d4-a716-446655440000\t0x609e9e87fDaF435CF27C684f58999B3d7b669b0B"
        `shouldBe` Right
          [ BulkParticipantEntry
              { bpeAlias = "@trader_01"
              , bpeTraderReference = "550e8400-e29b-41d4-a716-446655440000"
              , bpeTradingAccount = "0x24d6ea058f84834633432d8a57892a49b46f5236"
              }
          ]

    it "rejects duplicate aliases case-insensitively" $ do
      parseBulkParticipantEntries 2 (T.unlines
        [ "@Trader\t550e8400-e29b-41d4-a716-446655440000\t0x609e9e87fDaF435CF27C684f58999B3d7b669b0B"
        , "@trader\t550e8400-e29b-41d4-a716-446655440001\t0x5D3A31cEa19fd8F3E9da88cd3b0E5b95D30a8895"
        ])
        `shouldBe` Left "Bulk participant roster contains a duplicate alias"

    it "rejects duplicate opaque references" $ do
      parseBulkParticipantEntries 2 (T.unlines
        [ "@first\t550e8400-e29b-41d4-a716-446655440000\t0x609e9e87fDaF435CF27C684f58999B3d7b669b0B"
        , "@second\t550e8400-e29b-41d4-a716-446655440000\t0x5D3A31cEa19fd8F3E9da88cd3b0E5b95D30a8895"
        ])
        `shouldBe` Left "Bulk participant roster contains a duplicate opaque trader reference"

    it "rejects duplicate derived Trading Accounts" $ do
      parseBulkParticipantEntries 2 (T.unlines
        [ "@first\t550e8400-e29b-41d4-a716-446655440000\t0x609e9e87fDaF435CF27C684f58999B3d7b669b0B"
        , "@second\t550e8400-e29b-41d4-a716-446655440001\t0x609e9e87fDaF435CF27C684f58999B3d7b669b0B"
        ])
        `shouldBe` Left "Bulk participant roster contains a duplicate Trading Account destination"

    it "rejects malformed aliases, references, wallets, and counts" $ do
      parseBulkParticipantEntries
        1
        "alias\t550e8400-e29b-41d4-a716-446655440000\t0x609e9e87fDaF435CF27C684f58999B3d7b669b0B"
        `shouldBe` Left "Bulk participant roster contains an invalid @alias"
      parseBulkParticipantEntries
        1
        "@alias\tnot-opaque\t0x609e9e87fDaF435CF27C684f58999B3d7b669b0B"
        `shouldBe` Left "Bulk participant roster references must be opaque UUIDv4 values"
      parseBulkParticipantEntries
        1
        "@alias\t550e8400-e29b-41d4-a716-446655440000\tnot-an-address"
        `shouldBe` Left "Bulk participant roster contains an invalid OWNER_WALLET"
      parseBulkParticipantEntries 2 "@alias\t550e8400-e29b-41d4-a716-446655440000\t0x609e9e87fDaF435CF27C684f58999B3d7b669b0B"
        `shouldBe` Left "Bulk participant entry count does not match EXPECTED_INPUT_COUNT"
