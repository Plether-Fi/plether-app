{-# LANGUAGE LambdaCase #-}

module Plether.AA.ReconcilerSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.AA.Gateway (SecurityBlockHeader (..), validateSecurityHeaderTime)
import Plether.AA.KernelReconciler (reconstructKernelReceipt)
import Plether.AA.Reconciler
  ( BlockHeader (..)
  , AaReconcilerConfig (..)
  , UserOperationEvent (..)
  , agreeUserOperationLogs
  , boundariesRemainCanonical
  , targetReachesSafeBoundary
  , parseUserOperationEvent
  , parseUserOperationEventAt
  , validateSafeHeadFreshness
  , validateTargetTimestamp
  , validateDeploymentAnchor
  , loadAaReconcilerConfig
  )
import Plether.Database.KernelOperation (KernelInclusion (..))
import Plether.Ethereum.Abi (encodeUint256, keccak256)
import Test.Hspec
import System.Environment (lookupEnv, setEnv, unsetEnv)

spec :: Spec
spec =
  describe "safe UserOperationEvent validation" $ do
    it "refuses a startup batch larger than the shared gateway contract" $ do
      let required =
            [("PERPS_CHAIN_ID", "421614"), ("AA_PAYMASTER_ADDRESS", T.unpack paymaster)
            ,("AA_PAYMASTER_CODE_HASH", T.unpack blockHash), ("AA_RECONCILER_START_BLOCK", "1")
            ,("AA_RECONCILER_START_BLOCK_HASH", T.unpack blockHash), ("AA_RECONCILER_POLL_SECONDS", "2")
            ,("AA_RECONCILER_MAX_SAFE_LAG_SECONDS", "1800"), ("AA_PAYMASTER_MIN_DEPOSIT_WEI", "1")
            ,("AA_RECONCILER_FAILURE_PAUSE_SECONDS", "30")]
      withEnvironment required $ do
        withEnvironment [("AA_RECONCILER_BATCH_BLOCKS", "5000")] $ do
          result <- loadAaReconcilerConfig
          fmap arcBatchBlocks result `shouldBe` Right 5000
        withEnvironment [("AA_RECONCILER_BATCH_BLOCKS", "10000")] $
          loadAaReconcilerConfig `shouldReturn` Left "AA_RECONCILER_BATCH_BLOCKS must be between 1 and 5000"

    it "publishes health only at the exact reverified safe tip" $ do
      let target = BlockHeader 101 blockHash 1000
      targetReachesSafeBoundary target target `shouldBe` Right True
      targetReachesSafeBoundary target (BlockHeader 102 blockHash 1001) `shouldBe` Right False
      targetReachesSafeBoundary target (BlockHeader 100 blockHash 999) `shouldSatisfy` isLeft
      targetReachesSafeBoundary target (BlockHeader 101 operationHash 1000) `shouldSatisfy` isLeft
      targetReachesSafeBoundary target (BlockHeader 101 blockHash 1001) `shouldSatisfy` isLeft

    it "applies matching 10/30 minute gateway and reconciler boundaries" $ do
      let check limit age = do
            let timestamp = 10000 - age
                expected = age <= limit && age >= (-60)
            (validateSafeHeadFreshness limit 10000 (BlockHeader 101 blockHash timestamp) == Right ())
              `shouldBe` expected
            (validateSecurityHeaderTime limit 10000 (SecurityBlockHeader 101 blockHash timestamp 1) == Right ())
              `shouldBe` expected
      mapM_ (\limit -> mapM_ (check limit) [-61, -60, 0, 600, 601, 620, 656, 1800, 1801]) [600, 1800]

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

    it "applies the account profile's EntryPoint and payer bindings" $ do
      let kernelLog = setText "address" kernelEntryPoint validEvent
      event <- expectRight $ parseUserOperationEventAt kernelEntryPoint paymaster 100 100 kernelLog
      uoeHash event `shouldBe` operationHash
      parseUserOperationEvent paymaster 100 100 kernelLog `shouldSatisfy` isLeft
      parseUserOperationEventAt kernelEntryPoint otherPaymaster 100 100 kernelLog `shouldSatisfy` isLeft

    it "reconstructs an included Kernel operation revert with its actual ETH cost" $ do
      let unsponsored = kernelEvent zeroAddress False
          receipt = kernelReceipt [executionBoundary, unsponsored]
      (included, result) <- expectRight $
        reconstructKernelReceipt kernelEntryPoint sender 7 zeroAddress operationHash transactionHash receipt
      kiPaymaster included `shouldBe` Nothing
      kiExecutionSuccess included `shouldBe` False
      kiActualGasCost included `shouldBe` 1000
      case result of
        Object value -> KM.lookup "logs" value `shouldBe` Just (toJSON [unsponsored])
        _ -> expectationFailure "expected a reconstructed receipt object"

    it "binds a Kernel receipt to the signed operation and bundle execution boundary" $ do
      let sponsored = kernelEvent paymaster True
          prove = reconstructKernelReceipt kernelEntryPoint sender 7 paymaster operationHash transactionHash
      case prove $ kernelReceipt [executionBoundary, sponsored] of
        Right (included, _) -> kiPaymaster included `shouldBe` Just paymaster
        Left reason -> expectationFailure $ T.unpack reason
      prove (kernelReceipt [sponsored]) `shouldSatisfy` isLeft
      prove (kernelReceipt [executionBoundary, kernelEvent zeroAddress True]) `shouldSatisfy` isLeft
      prove (kernelReceipt [executionBoundary, setText "blockHash" otherBlockHash sponsored]) `shouldSatisfy` isLeft
      prove (kernelReceipt [executionBoundary, sponsored, setText "logIndex" "0x3" sponsored]) `shouldSatisfy` isLeft

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
setText key value = setValue key (String value)

setBool :: Text -> Bool -> Value -> Value
setBool key value = setValue key (Bool value)

setValue :: Text -> Value -> Value -> Value
setValue key value (Object objectValue) = Object $ KM.insert (fromText key) value objectValue
setValue _ _ value = value

kernelEvent :: Text -> Bool -> Value
kernelEvent payer success =
  setText "address" kernelEntryPoint $
    setValue "topics" (toJSON [eventTopic, operationHash, addressTopic sender, addressTopic payer]) $
      setText "data" (hex $ encodeUint256 7 <> encodeUint256 (if success then 1 else 0) <> encodeUint256 1000 <> encodeUint256 900) validEvent

executionBoundary :: Value
executionBoundary = setText "logIndex" "0x1" $ setText "data" "0x" $
  setValue "topics" (toJSON [hex $ keccak256 "BeforeExecution()"]) $ setText "address" kernelEntryPoint validEvent

kernelReceipt :: [Value] -> Value
kernelReceipt logs = object
  [ "transactionHash" .= transactionHash
  , "blockNumber" .= ("0x64" :: Text)
  , "blockHash" .= blockHash
  , "status" .= ("0x1" :: Text)
  , "logs" .= logs
  ]

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

kernelEntryPoint, zeroAddress :: Text
kernelEntryPoint = "0x433709009b8330fda32311df1c2afa402ed8d009"
zeroAddress = "0x" <> T.replicate 40 "0"

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

withEnvironment :: [(String, String)] -> IO a -> IO a
withEnvironment [] action = action
withEnvironment ((name, value) : rest) action =
  bracket (lookupEnv name <* setEnv name value)
    (maybe (unsetEnv name) (setEnv name))
    (const $ withEnvironment rest action)
