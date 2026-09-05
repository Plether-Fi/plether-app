{-# LANGUAGE LambdaCase #-}

module Plether.AA.ReconcilerSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.AA.Reconciler
  ( BlockHeader (..)
  , UserOperationEvent (..)
  , agreeUserOperationLogs
  , boundariesRemainCanonical
  , parseUserOperationEvent
  , validateSafeHeadFreshness
  , validateTargetTimestamp
  , validateDeploymentAnchor
  )
import Plether.Ethereum.Abi (encodeUint256, keccak256)
import Test.Hspec

spec :: Spec
spec =
  describe "safe UserOperationEvent validation" $ do
    it "accepts the exact EntryPoint/paymaster/range shape" $ do
      event <- expectRight $ parseUserOperationEvent paymaster 90 110 validEvent
      uoeHash event `shouldBe` operationHash
      uoeSender event `shouldBe` sender
      uoeNonce event `shouldBe` 7
      uoeBlockNumber event `shouldBe` 100
      uoeLogIndex event `shouldBe` 2
      uoeSuccess event `shouldBe` True
      uoeActualGasCost event `shouldBe` 1000
      uoeActualGasUsed event `shouldBe` 900

    it "rejects a forged emitter, paymaster topic, removed log, or range escape" $ do
      parseUserOperationEvent paymaster 90 110 (setText "address" paymaster validEvent)
        `shouldSatisfy` isLeft
      parseUserOperationEvent otherPaymaster 90 110 validEvent
        `shouldSatisfy` isLeft
      parseUserOperationEvent paymaster 101 110 validEvent
        `shouldSatisfy` isLeft
      parseUserOperationEvent paymaster 90 110 (setBool "removed" True validEvent)
        `shouldSatisfy` isLeft

    it "requires identical canonical provider log sets and rejects duplicates" $ do
      event <- expectRight $ parseUserOperationEvent paymaster 90 110 validEvent
      agreeUserOperationLogs [event] [event] `shouldBe` Right [event]
      agreeUserOperationLogs [event, event] [event, event]
        `shouldSatisfy` isLeft
      agreeUserOperationLogs [event] [event {uoeActualGasUsed = 901}]
        `shouldSatisfy` isLeft
      agreeUserOperationLogs [event] [] `shouldSatisfy` isLeft

    it "rejects regressing and implausibly future safe timestamps" $ do
      let cursor = BlockHeader 100 blockHash 1_000
      validateTargetTimestamp 1_100 cursor (BlockHeader 101 otherBlockHash 1_050)
        `shouldBe` Right ()
      validateTargetTimestamp 1_100 cursor (BlockHeader 101 otherBlockHash 999)
        `shouldSatisfy` isLeft
      validateTargetTimestamp 1_100 cursor (BlockHeader 101 otherBlockHash 1_161)
        `shouldSatisfy` isLeft
      validateSafeHeadFreshness 100 2_000 (BlockHeader 101 otherBlockHash 1_050)
        `shouldSatisfy` isLeft
      validateSafeHeadFreshness 600 2_000 (BlockHeader 101 otherBlockHash 2_061)
        `shouldSatisfy` isLeft

    it "allows historical catch-up targets when the agreed safe boundary is fresh" $ do
      let oldCursor = BlockHeader 100 blockHash 1_000
          oldTarget = BlockHeader 200 otherBlockHash 1_100
          freshSafe = BlockHeader 10_000 operationHash 10_000
      validateSafeHeadFreshness 600 10_100 freshSafe `shouldBe` Right ()
      validateTargetTimestamp 10_100 oldCursor oldTarget `shouldBe` Right ()

    it "requires both cursor and target headers to remain exact before settlement" $ do
      let cursor = BlockHeader 100 blockHash 1_000
          target = BlockHeader 110 otherBlockHash 1_100
      boundariesRemainCanonical cursor target cursor target `shouldBe` Right ()
      boundariesRemainCanonical cursor target (cursor {bhHash = operationHash}) target
        `shouldSatisfy` isLeft
      boundariesRemainCanonical cursor target cursor (target {bhHash = operationHash})
        `shouldSatisfy` isLeft

    it "pins cursor bootstrap to an exact deployment block and no-code to code transition" $ do
      let runtime = "reviewed-paymaster-runtime"
          deployment = BlockHeader 100 blockHash 1_000
          runtimeHash = hex $ keccak256 runtime
      validateDeploymentAnchor blockHash runtimeHash deployment mempty runtime
        `shouldBe` Right ()
      validateDeploymentAnchor otherBlockHash runtimeHash deployment mempty runtime
        `shouldSatisfy` isLeft
      validateDeploymentAnchor blockHash runtimeHash deployment "already-deployed" runtime
        `shouldSatisfy` isLeft
      validateDeploymentAnchor blockHash runtimeHash deployment mempty "wrong-runtime"
        `shouldSatisfy` isLeft

validEvent :: Value
validEvent =
  object
    [ "address" .= entryPoint
    , "topics"
        .= [ eventTopic
           , operationHash
           , addressTopic sender
           , addressTopic paymaster
           ]
    , "data" .= hex (encodeUint256 7 <> encodeUint256 1 <> encodeUint256 1000 <> encodeUint256 900)
    , "transactionHash" .= transactionHash
    , "blockNumber" .= ("0x64" :: Text)
    , "blockHash" .= blockHash
    , "logIndex" .= ("0x2" :: Text)
    , "removed" .= False
    ]

setText :: Text -> Text -> Value -> Value
setText key value (Object objectValue) = Object $ KM.insert (fromText key) (String value) objectValue
setText _ _ value = value

setBool :: Text -> Bool -> Value -> Value
setBool key value (Object objectValue) = Object $ KM.insert (fromText key) (Bool value) objectValue
setBool _ _ value = value

fromText :: Text -> Key.Key
fromText = Key.fromText

addressTopic :: Text -> Text
addressTopic address = "0x" <> T.replicate 24 "0" <> T.drop 2 address

hex :: ByteString -> Text
hex bytes = "0x" <> TE.decodeUtf8 (B16.encode bytes)

eventTopic :: Text
eventTopic =
  hex $ keccak256 "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)"

entryPoint, paymaster, otherPaymaster, sender :: Text
entryPoint = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"
paymaster = "0x1111111111111111111111111111111111111111"
otherPaymaster = "0x2222222222222222222222222222222222222222"
sender = "0x3333333333333333333333333333333333333333"

operationHash, transactionHash, blockHash :: Text
operationHash = "0x" <> T.replicate 64 "4"
transactionHash = "0x" <> T.replicate 64 "5"
blockHash = "0x" <> T.replicate 64 "6"

otherBlockHash :: Text
otherBlockHash = "0x" <> T.replicate 64 "7"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

expectRight :: Show err => Either err value -> IO value
expectRight = \case
  Left err -> do
    expectationFailure $ show err
    error "unreachable"
  Right value -> pure value
