{-# LANGUAGE LambdaCase #-}

module Plether.Vaults.ActivityIndexerSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Database.VaultActivity
  ( VaultActivityDeployment (..)
  , VaultDepositRequestKey (..)
  , VaultDepositRequestStateRow (..)
  , VaultRequestRow (..)
  )
import Plether.Ethereum.Abi (encodeAddress, encodeBool, encodeCall, encodeUint256)
import Plether.Ethereum.Rpc (RpcLog (..))
import Plether.Ethereum.Client (RpcError (..))
import Plether.Vaults.ActivityIndexer
  ( ParsedVaultEvent (..)
  , VaultTransfer (..)
  , depositRequestTopic
  , legacyDepositRequestedTopic
  , isProviderLogRangeLimit
  , parseVaultLog
  , redeemRequestTopic
  , transferTopic
  )
import Plether.Vaults.DepositAttributionIndexer
  ( decodeLpRequestState
  , lpRequestStateCall
  )
import Test.Hspec

spec :: Spec
spec = describe "vault activity ABI decoding" $ do
  it "strictly decodes ERC-20 mint, burn, transfer, and same-address movements" $ do
    let cases =
          [ (zeroAddress, ownerAddress)
          , (ownerAddress, zeroAddress)
          , (ownerAddress, secondAddress)
          , (ownerAddress, ownerAddress)
          ]
    mapM_
      (\(fromAddress, toAddress) ->
        parseVaultLog deployment 1_800_000_000
          (baseLog transferTopic [addressTopic fromAddress, addressTopic toAddress] [word 42])
          `shouldSatisfy` \case
            Right (ParsedVaultTransfer transfer) ->
              vtFromAddress transfer == fromAddress
                && vtToAddress transfer == toAddress
                && vtAmount transfer == 42
            _ -> False)
      cases

  it "decodes the authoritative DepositRequest and RedeemRequest data word" $ do
    let indexed = [addressTopic ownerAddress, addressTopic secondAddress, word 77]
        deposit = baseLog depositRequestTopic indexed [addressWord unknownAddress, word 456]
        redeem = baseLog redeemRequestTopic indexed [addressWord unknownAddress, word 789]
    parseVaultLog deployment 1_800_000_000 deposit
      `shouldSatisfy` isRequest "DepositRequest" ownerAddress secondAddress 77 456
    parseVaultLog deployment 1_800_000_000 redeem
      `shouldSatisfy` isRequest "RedeemRequest" ownerAddress secondAddress 77 789

  it "retains legacy DepositRequested for wallet request discovery" $ do
    let entry =
          baseLog
            legacyDepositRequestedTopic
            [addressTopic secondAddress, addressTopic ownerAddress, word 88]
            [word 999]
    parseVaultLog deployment 1_800_000_000 entry
      `shouldSatisfy` isRequest "DepositRequested" secondAddress ownerAddress 88 999

  it "rejects unknown vaults, unknown events, malformed topics, and malformed data" $ do
    let valid = baseLog transferTopic [addressTopic ownerAddress, addressTopic secondAddress] [word 1]
    parseVaultLog deployment 1_800_000_000 valid {rpcLogAddress = unknownAddress}
      `shouldSatisfy` isLeft
    parseVaultLog deployment 1_800_000_000 valid {rpcLogTopics = word 999 : tail (rpcLogTopics valid)}
      `shouldSatisfy` isLeft
    parseVaultLog deployment 1_800_000_000 valid {rpcLogTopics = [transferTopic, addressTopic ownerAddress]}
      `shouldSatisfy` isLeft
    parseVaultLog deployment 1_800_000_000 valid {rpcLogData = BS.replicate 31 0}
      `shouldSatisfy` isLeft
    parseVaultLog deployment 1_800_000_000 valid {rpcLogTopics = [transferTopic, BS.replicate 32 255, addressTopic secondAddress]}
      `shouldSatisfy` isLeft
    let malformedRequest = baseLog depositRequestTopic
          [addressTopic ownerAddress, addressTopic secondAddress, word 1]
          [BS.replicate 32 255, word 1]
    parseVaultLog deployment 1_800_000_000 malformedRequest `shouldSatisfy` isLeft

  it "splits only recognized provider log-range limit failures" $ do
    isProviderLogRangeLimit (RpcNodeError (-32005) "provider range limit" Nothing)
      `shouldBe` True
    isProviderLogRangeLimit (RpcNodeError (-32602) "block range exceeded" Nothing)
      `shouldBe` True
    isProviderLogRangeLimit (RpcNodeError (-32000) "authentication failed" Nothing)
      `shouldBe` False
    isProviderLogRangeLimit (RpcHttpError "StatusCodeException statusCode = 413")
      `shouldBe` True

  it "encodes and strictly decodes pinned Public Lens deposit attribution" $ do
    let key = VaultDepositRequestKey seniorVault ownerAddress 77
        call =
          encodeCall
            "getLpRequestState(bool,uint256,address)"
            [encodeBool True, encodeUint256 77, encodeAddress ownerAddress]
        response = BS.concat
          [ encodeAddress seniorVault
          , word 77
          , encodeAddress ownerAddress
          , word 10
          , word 11
          , word 12
          , word 13
          , word 14
          , word 15
          , word 16
          , word 17
          , word 18
          , word 19
          , encodeBool False
          ]
        expected =
          VaultDepositRequestStateRow
            { vdrsKey = key
            , vdrsPendingDepositAssets = 10
            , vdrsClaimableDepositAssets = 12
            , vdrsClaimableDepositShares = 13
            , vdrsRefundableDepositAssets = 18
            , vdrsActive = True
            , vdrsObservedBlock = 123
            , vdrsObservedBlockHash = blockHash
            }
    lpRequestStateCall deployment key `shouldBe` Right call
    decodeLpRequestState key 123 blockHash response `shouldBe` Right expected
    decodeLpRequestState key 123 blockHash (BS.take (BS.length response - 1) response)
      `shouldSatisfy` isLeft
    decodeLpRequestState key 123 blockHash (replaceWord 1 78 response)
      `shouldSatisfy` isLeft
    decodeLpRequestState key 123 blockHash (replaceWord 13 2 response)
      `shouldSatisfy` isLeft

  it "keeps pending and refundable deposits out of attributed shares" $ do
    let key = VaultDepositRequestKey juniorVault ownerAddress 88
        response pendingAssets claimable refundable = BS.concat
          [ encodeAddress juniorVault
          , word 88
          , encodeAddress ownerAddress
          , word pendingAssets
          , word 0
          , word 0
          , word claimable
          , word 0
          , word 0
          , word 0
          , word 0
          , word refundable
          , word 0
          , encodeBool False
          ]
    fmap vdrsActive (decodeLpRequestState key 123 blockHash $ response 50 0 0)
      `shouldBe` Right True
    fmap vdrsActive (decodeLpRequestState key 123 blockHash $ response 0 0 50)
      `shouldBe` Right False
    fmap vdrsClaimableDepositShares (decodeLpRequestState key 123 blockHash $ response 50 0 0)
      `shouldBe` Right 0

  it "keeps runtime and static canonical vault schemas aligned" $ do
    runtime <- readFile "src/Plether/Database/VaultActivity.hs"
    static <- readFile "schema.sql"
    mapM_
      (\table -> do
        runtime `shouldContain` ("CREATE TABLE IF NOT EXISTS " <> table)
        static `shouldContain` ("CREATE TABLE IF NOT EXISTS " <> table)
      )
      [ "vault_activity_indexer_state"
      , "vault_deposit_attribution_state"
      , "vault_deposit_request_states"
      , "vault_attributed_holder_balances"
      , "vault_canonical_logs"
      , "vault_share_transfers"
      , "vault_holder_balances"
      , "vault_request_events"
      ]
    runtime `shouldContain` "PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, tx_hash, log_index)"
    static `shouldContain` "PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, tx_hash, log_index)"

isRequest :: Text -> Text -> Text -> Integer -> Integer -> Either a ParsedVaultEvent -> Bool
isRequest eventName controller owner requestId amount = \case
  Right (ParsedVaultRequest row) ->
    vrrEventName row == eventName
      && vrrController row == controller
      && vrrOwner row == owner
      && vrrRequestId row == requestId
      && vrrRawAmount row == amount
  _ -> False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

replaceWord :: Int -> Integer -> BS.ByteString -> BS.ByteString
replaceWord index value bytes =
  BS.take (index * 32) bytes
    <> word value
    <> BS.drop ((index + 1) * 32) bytes

baseLog :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString] -> RpcLog
baseLog topic indexed values =
  RpcLog
    { rpcLogTxHash = "0x" <> T.replicate 64 "1"
    , rpcLogBlockNumber = 302_257_125
    , rpcLogBlockHash = "0x" <> T.replicate 64 "2"
    , rpcLogTransactionIndex = 3
    , rpcLogIndex = 4
    , rpcLogAddress = seniorVault
    , rpcLogTopics = topic : indexed
    , rpcLogData = BS.concat values
    }

addressTopic :: Text -> BS.ByteString
addressTopic address = BS.replicate 12 0 <> decodeHex (T.drop 2 address)

addressWord :: Text -> BS.ByteString
addressWord = addressTopic

word :: Integer -> BS.ByteString
word value = BS.replicate (32 - BS.length raw) 0 <> raw
 where
  raw = reverseBytes value

reverseBytes :: Integer -> BS.ByteString
reverseBytes 0 = BS.singleton 0
reverseBytes value = BS.reverse $ BS.unfoldr step value
 where
  step 0 = Nothing
  step remaining = Just (fromIntegral $ remaining `mod` 256, remaining `div` 256)

decodeHex :: Text -> BS.ByteString
decodeHex value = either (const BS.empty) id $ B16.decode $ TE.encodeUtf8 value

deployment :: VaultActivityDeployment
deployment = VaultActivityDeployment 421_614 housePool seniorVault juniorVault 302_257_125

housePool, seniorVault, juniorVault, ownerAddress, secondAddress, unknownAddress, zeroAddress :: Text
housePool = "0x1111111111111111111111111111111111111111"
seniorVault = "0x2222222222222222222222222222222222222222"
juniorVault = "0x3333333333333333333333333333333333333333"
ownerAddress = "0x4444444444444444444444444444444444444444"
secondAddress = "0x5555555555555555555555555555555555555555"
unknownAddress = "0x6666666666666666666666666666666666666666"
zeroAddress = "0x0000000000000000000000000000000000000000"

blockHash :: Text
blockHash = "0x" <> T.replicate 64 "a"
