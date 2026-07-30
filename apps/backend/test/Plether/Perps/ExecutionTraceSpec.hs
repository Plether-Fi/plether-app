module Plether.Perps.ExecutionTraceSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Plether.Ethereum.Abi
  ( encodeAddress
  , encodeInt256
  , encodeUint256
  , selector
  )
import Plether.Perps.ExecutionTrace
import Test.Hspec

spec :: Spec
spec = do
  describe "execution selectors" $ do
    it "exports the deployed Engine and SettlementSidecar selectors" $ do
      processOrderTypedSelector
        `shouldBe` selector
          "processOrderTyped((address,uint256,uint256,uint256,uint64,uint64,uint64,uint8,bool),uint256,uint256,uint64)"
      executeOpenSelector `shouldBe` BS.pack [0x41, 0xe3, 0x9b, 0xc7]
      executeCloseSelector `shouldBe` BS.pack [0x4e, 0x8e, 0x4f, 0xbc]
      executeOracleSelector `shouldBe` BS.pack [0x08, 0x83, 0x5e, 0x57]
      executeFrozenOracleSelector `shouldBe` BS.pack [0xb3, 0x69, 0xf2, 0xae]
      BS.length (oracleOutput defaultOracle) `shouldBe` 9 * 32

  describe "decodeTradeExecutionEvidence" $ do
    it "decodes the exact positive close VPI and zero spread from order 9202" $ do
      decode fixture9202
        `shouldBe` Right
          ( Map.singleton
              9202
              (evidenceFor oracle9202 TradeClose 182_822_887 $ Just 0)
          )

    it "decodes a signed open VPI" $ do
      let trace =
            blockscoutRoot
              [processFrame TradeOpen 41 $ openSettlement (-123_456_789)]
      decode trace
        `shouldBe` Right
          ( Map.singleton
              41
              (evidenceFor defaultOracle TradeOpen (-123_456_789) Nothing)
          )

    it "returns the actual nonzero frozen-close spread" $ do
      let trace =
            blockscoutRoot
              [processFrame TradeClose 42 $ closeSettlement (-17) 3_940_366]
      decode trace
        `shouldBe` Right
          ( Map.singleton
              42
              ( evidenceFor
                  defaultOracle
                  TradeClose
                  (-17)
                  (Just 3_940_366)
              )
          )

    it "preserves both VPI signs and frozen spread from live Sepolia traces" $ do
      let trace =
            blockscoutRoot
              [ processFrameWithOracleCalls
                  oracle9218
                  TradeOpen
                  9218
                  [settlementFrameWithCalldata TradeOpen liveOpen9218]
              , processFrameWithOracleCalls
                  oracle9222
                  TradeClose
                  9222
                  [settlementFrameWithCalldata TradeClose liveClose9222]
              , processFrameWithOracleCalls
                  oracle7051
                  TradeClose
                  7051
                  [settlementFrameWithCalldata TradeClose liveFrozenClose7051]
              ]
      decode trace
        `shouldBe` Right
          ( Map.fromList
              [ ( 7051
                , evidenceFor
                    oracle7051
                    TradeClose
                    39_254_431
                    (Just 70_428_103)
                )
              , ( 9218
                , evidenceFor oracle9218 TradeOpen 93_368_631 Nothing
                )
              , ( 9222
                , evidenceFor oracle9222 TradeClose (-15_790_673) (Just 0)
                )
              ]
          )

    it "supports the frozen-oracle selector and preserves its exact snapshot" $ do
      let frozenOracle =
            OracleFixture
              { ofSelector = executeFrozenOracleSelector
              , ofExecutionPrice = 97_000_123
              , ofMarkPrice = 97_000_456
              , ofPublishTime = 1_800_000_007
              , ofFrozen = True
              , ofFad = False
              }
          trace =
            processFrameWithOracleCalls
              frozenOracle
              TradeClose
              43
              [closeSettlement 99 0]
      decode trace
        `shouldBe` Right
          ( Map.singleton
              43
              (evidenceFor frozenOracle TradeClose 99 $ Just 0)
          )

    it "requires an authenticated, preceding oracle response matching the process input" $ do
      let differentExecution =
            defaultOracle
              { ofExecutionPrice = ofExecutionPrice defaultOracle + 1
              }
          processForDifferentExecution =
            rawProcessFrameWithCalls
              differentExecution
              TradeOpen
              44
              [openSettlement 1]
          wrongOracleTarget =
            setObjectField
              "to"
              (String helperContract)
              (oracleFrame differentExecution)
          mismatched =
            blockscoutRoot
              [oracleFrame defaultOracle, processForDifferentExecution]
          unauthenticated =
            blockscoutRoot
              [wrongOracleTarget, processForDifferentExecution]
          oracleAfterProcess =
            blockscoutRoot
              [processForDifferentExecution, oracleFrame differentExecution]
      decode mismatched `shouldSatisfy` isLeft
      decode unauthenticated `shouldSatisfy` isLeft
      decode oracleAfterProcess `shouldSatisfy` isLeft

    it "consumes matching oracle occurrences for reverted process attempts" $ do
      let revertedProcess =
            addError
              "execution reverted"
              ( rawProcessFrameWithCalls
                  defaultOracle
                  TradeOpen
                  45
                  [openSettlement 1]
              )
          successfulProcess =
            rawProcessFrameWithCalls
              defaultOracle
              TradeOpen
              46
              [openSettlement 2]
          missingSecondOracle =
            blockscoutRoot
              [oracleFrame defaultOracle, revertedProcess, successfulProcess]
          paired =
            blockscoutRoot
              [ oracleFrame defaultOracle
              , revertedProcess
              , oracleFrame defaultOracle
              , successfulProcess
              ]
      decode missingSecondOracle `shouldSatisfy` isLeft
      decode paired
        `shouldBe` Right
          ( Map.singleton
              46
              (evidenceFor defaultOracle TradeOpen 2 Nothing)
          )

    it "rejects malformed or noncanonical oracle output" $ do
      let process =
            rawProcessFrameWithCalls
              defaultOracle
              TradeOpen
              47
              [openSettlement 1]
          truncatedOracle =
            setObjectField
              "output"
              (String $ hexText $ BS.init $ oracleOutput defaultOracle)
              (oracleFrame defaultOracle)
          invalidResult =
            setObjectField
              "output"
              ( String $
                  hexText $
                    replaceOutputWord
                      0
                      (encodeUint256 0)
                      (oracleOutput defaultOracle)
              )
              (oracleFrame defaultOracle)
          invalidFrozen =
            setObjectField
              "output"
              ( String $
                  hexText $
                    replaceOutputWord
                      7
                      (encodeUint256 2)
                      (oracleOutput defaultOracle)
              )
              (oracleFrame defaultOracle)
          invalidFad =
            setObjectField
              "output"
              ( String $
                  hexText $
                    replaceOutputWord
                      8
                      (encodeUint256 2)
                      (oracleOutput defaultOracle)
              )
              (oracleFrame defaultOracle)
      decode (blockscoutRoot [truncatedOracle, process])
        `shouldSatisfy` isLeft
      decode (blockscoutRoot [invalidResult, process])
        `shouldSatisfy` isLeft
      decode (blockscoutRoot [invalidFrozen, process])
        `shouldSatisfy` isLeft
      decode (blockscoutRoot [invalidFad, process])
        `shouldSatisfy` isLeft

    it "recursively traverses callTracer and Blockscout root objects" $ do
      let wrapper =
            callFrame
              externalAccount
              orderRouter
              "CALL"
              BS.empty
              [processFrame TradeOpen 7 $ openSettlement 11]
          trace =
            object
              [ "beforeEVMTransfers" .= ([] :: [Value])
              , "calls" .= [wrapper]
              ]
      decode trace
        `shouldBe` Right
          ( Map.singleton
              7
              (evidenceFor defaultOracle TradeOpen 11 Nothing)
          )
      decode (processFrame TradeClose 8 $ closeSettlement 22 0)
        `shouldBe` Right
          ( Map.singleton
              8
              (evidenceFor defaultOracle TradeClose 22 $ Just 0)
          )

    it "matches checksummed trace addresses case-insensitively" $ do
      decodeTradeExecutionEvidence
        (Text.toUpper orderRouter)
        (Text.toUpper engine)
        (Text.toUpper settlementSidecar)
        (Text.toUpper pletherOracle)
        (processFrame TradeOpen 9 $ openSettlement 33)
        `shouldBe` Right
          ( Map.singleton
              9
              (evidenceFor defaultOracle TradeOpen 33 Nothing)
          )

    it "ignores selector collisions outside the configured call path" $ do
      let spoofedProcess =
            callFrame
              externalAccount
              engine
              "CALL"
              (processCalldata TradeOpen 10)
              [openSettlement 44]
          trace = blockscoutRoot [spoofedProcess]
      decode trace `shouldBe` Right Map.empty

    it "ignores reverted process calls and their successful descendants" $ do
      let reverted =
            addError
              "execution reverted"
              (processFrame TradeOpen 11 $ openSettlement 55)
      decode (blockscoutRoot [reverted]) `shouldBe` Right Map.empty

    it "requires the process calldata to have canonical complete ABI words" $ do
      let tooShort =
            callFrame
              orderRouter
              engine
              "CALL"
              (processOrderTypedSelector <> mconcat (replicate 11 $ encodeUint256 0))
              [openSettlement 1]
          trailingByte =
            callFrame
              orderRouter
              engine
              "CALL"
              (processCalldata TradeOpen 12 <> BS.singleton 0)
              [openSettlement 1]
      decode tooShort `shouldSatisfy` isLeft
      decode trailingByte `shouldSatisfy` isLeft

    it "requires the order ID word to be a canonical uint64" $ do
      let oversized = 2 ^ (64 :: Integer)
          trace =
            callFrame
              orderRouter
              engine
              "CALL"
              (processCalldata TradeOpen oversized)
              [openSettlement 1]
      decode trace `shouldSatisfy` isLeft

    it "requires canonical isClose and agreement with the settlement selector" $ do
      let noncanonicalIsClose =
            callFrame
              orderRouter
              engine
              "CALL"
              ( replaceArgumentWord
                  8
                  (encodeUint256 2)
                  (processCalldata TradeOpen 13)
              )
              [openSettlement 1]
          closeWithOpenSettlement =
            processFrameWithCalls TradeClose 13 [openSettlement 1]
          openWithCloseSettlement =
            processFrameWithCalls TradeOpen 13 [closeSettlement 1 0]
      decode noncanonicalIsClose `shouldSatisfy` isLeft
      decode closeWithOpenSettlement `shouldSatisfy` isLeft
      decode openWithCloseSettlement `shouldSatisfy` isLeft

    it "requires exactly one successful direct settlement call" $ do
      let noSettlement = processFrameWithCalls TradeOpen 13 []
          nestedOnly =
            processFrameWithCalls TradeOpen
              13
              [ callFrame
                  engine
                  helperContract
                  "CALL"
                  BS.empty
                  [openSettlement 1]
              ]
          duplicate =
            processFrameWithCalls TradeOpen
              13
              [openSettlement 1, closeSettlement 2 0]
          revertedOnly =
            processFrameWithCalls
              TradeOpen
              13
              [addError "reverted" $ openSettlement 1]
      decode noSettlement `shouldSatisfy` isLeft
      decode nestedOnly `shouldSatisfy` isLeft
      decode duplicate `shouldSatisfy` isLeft
      decode revertedOnly `shouldSatisfy` isLeft

    it "requires direct execution frames to be CALLs" $ do
      let wrongProcessType =
            callFrame
              orderRouter
              engine
              "DELEGATECALL"
              (processCalldata TradeOpen 14)
              [openSettlement 1]
          wrongSettlementType =
            processFrameWithCalls TradeOpen
              14
              [ callFrame
                  engine
                  settlementSidecar
                  "DELEGATECALL"
                  (settlementCalldata TradeOpen 1 0)
                  []
              ]
      decode wrongProcessType `shouldSatisfy` isLeft
      decode wrongSettlementType `shouldSatisfy` isLeft

    it "authenticates the settlement host, valid flag, and revert code" $ do
      let wrongHost =
            processFrameWithCalls TradeOpen
              15
              [settlementFrame TradeOpen helperContract 1 0 1 0]
          noncanonicalHost =
            processFrameWithCalls TradeOpen
              15
              [ settlementFrameWithCalldata
                  TradeOpen
                  ( replaceArgumentWord
                      0
                      (BS.singleton 1 <> BS.drop 1 (encodeAddress engine))
                      (settlementCalldata TradeOpen 1 0)
                  )
              ]
          invalidDelta =
            processFrameWithCalls TradeOpen
              15
              [settlementFrame TradeOpen engine 1 0 0 0]
          nonzeroRevert =
            processFrameWithCalls TradeOpen
              15
              [settlementFrame TradeOpen engine 1 0 1 7]
      decode wrongHost `shouldSatisfy` isLeft
      decode noncanonicalHost `shouldSatisfy` isLeft
      decode invalidDelta `shouldSatisfy` isLeft
      decode nonzeroRevert `shouldSatisfy` isLeft

    it "requires the exact deployed settlement calldata lengths" $ do
      let truncated =
            processFrameWithCalls TradeClose
              16
              [ settlementFrameWithCalldata
                  TradeClose
                  (executeCloseSelector <> mconcat (replicate 11 $ encodeUint256 0))
              ]
          trailingByte =
            processFrameWithCalls TradeOpen
              16
              [ settlementFrameWithCalldata
                  TradeOpen
                  (settlementCalldata TradeOpen 1 0 <> BS.singleton 0)
              ]
          extraWord =
            processFrameWithCalls TradeOpen
              16
              [ settlementFrameWithCalldata
                  TradeOpen
                  (settlementCalldata TradeOpen 1 0 <> encodeUint256 0)
              ]
      decode truncated `shouldSatisfy` isLeft
      decode trailingByte `shouldSatisfy` isLeft
      decode extraWord `shouldSatisfy` isLeft

    it "rejects duplicate order evidence even when both executions agree" $ do
      let trace =
            blockscoutRoot
              [ processFrame TradeOpen 17 $ openSettlement 1
              , processFrame TradeOpen 17 $ openSettlement 1
              ]
      decode trace `shouldSatisfy` isLeft

    it "rejects malformed trace nodes instead of silently trusting them" $ do
      decode (object ["calls" .= String "not-an-array"])
        `shouldSatisfy` isLeft
      decode (object ["calls" .= [String "not-an-object"]])
        `shouldSatisfy` isLeft
      decode
        ( object
            [ "calls"
                .= [ object
                       [ "from" .= orderRouter
                       , "to" .= engine
                       , "type" .= String "CALL"
                       , "input" .= String "0xzz"
                       ]
                   ]
            ]
        )
        `shouldSatisfy` isLeft

decode :: Value -> Either Text (Map.Map Integer TradeExecutionEvidence)
decode =
  decodeTradeExecutionEvidence
    orderRouter
    engine
    settlementSidecar
    pletherOracle

evidenceFor
  :: OracleFixture
  -> TradeExecutionKind
  -> Integer
  -> Maybe Integer
  -> TradeExecutionEvidence
evidenceFor oracleFixture kind vpi frozenCloseSpread =
  TradeExecutionEvidence
    { teeKind = kind
    , teeVpiUsdc = vpi
    , teeFrozenCloseSpreadUsdc = frozenCloseSpread
    , teeExecutionOraclePrice = ofMarkPrice oracleFixture
    , teeOraclePublishTime = ofPublishTime oracleFixture
    , teeOracleFrozen = ofFrozen oracleFixture
    }

fixture9202 :: Value
fixture9202 =
  blockscoutRoot
    [ processFrameWithOracleCalls
        oracle9202
        TradeClose
        9202
        [settlementFrameWithCalldata TradeClose liveClose9202]
    ]

blockscoutRoot :: [Value] -> Value
blockscoutRoot calls =
  object
    [ "afterEVMTransfers" .= ([] :: [Value])
    , "calls" .= calls
    ]

data OracleFixture = OracleFixture
  { ofSelector :: BS.ByteString
  , ofExecutionPrice :: Integer
  , ofMarkPrice :: Integer
  , ofPublishTime :: Integer
  , ofFrozen :: Bool
  , ofFad :: Bool
  }

defaultOracle, oracle9202, oracle9218, oracle9222, oracle7051 :: OracleFixture
defaultOracle =
  OracleFixture
    { ofSelector = executeOracleSelector
    , ofExecutionPrice = 98_000_000
    , ofMarkPrice = 98_100_000
    , ofPublishTime = 1_700_000_000
    , ofFrozen = False
    , ofFad = False
    }
oracle9202 =
  OracleFixture
    { ofSelector = executeOracleSelector
    , ofExecutionPrice = 98_391_251
    , ofMarkPrice = 98_391_482
    , ofPublishTime = 1_785_437_834
    , ofFrozen = False
    , ofFad = False
    }
oracle9218 =
  OracleFixture
    { ofSelector = executeOracleSelector
    , ofExecutionPrice = 98_331_712
    , ofMarkPrice = 98_331_978
    , ofPublishTime = 1_785_441_366
    , ofFrozen = False
    , ofFad = False
    }
oracle9222 =
  OracleFixture
    { ofSelector = executeOracleSelector
    , ofExecutionPrice = 98_305_654
    , ofMarkPrice = 98_305_342
    , ofPublishTime = 1_785_444_692
    , ofFrozen = False
    , ofFad = False
    }
oracle7051 =
  OracleFixture
    { ofSelector = executeOracleSelector
    , ofExecutionPrice = 96_869_818
    , ofMarkPrice = 96_869_818
    , ofPublishTime = 1_784_926_799
    , ofFrozen = True
    , ofFad = True
    }

oracleFrame :: OracleFixture -> Value
oracleFrame oracleFixture =
  object
    [ "from" .= orderRouter
    , "to" .= pletherOracle
    , "type" .= String "CALL"
    , "input" .= hexText (ofSelector oracleFixture)
    , "output" .= hexText (oracleOutput oracleFixture)
    , "calls" .= ([] :: [Value])
    ]

oracleOutput :: OracleFixture -> BS.ByteString
oracleOutput oracleFixture =
  mconcat
    [ encodeUint256 1
    , encodeUint256 $ ofExecutionPrice oracleFixture
    , encodeUint256 $ ofMarkPrice oracleFixture
    , encodeUint256 $ ofPublishTime oracleFixture
    , encodeUint256 0
    , encodeUint256 60
    , encodeUint256 0
    , encodeUint256 $ if ofFrozen oracleFixture then 1 else 0
    , encodeUint256 $ if ofFad oracleFixture then 1 else 0
    ]

processFrame :: TradeExecutionKind -> Integer -> Value -> Value
processFrame kind orderId settlement =
  processFrameWithCalls kind orderId [settlement]

processFrameWithCalls :: TradeExecutionKind -> Integer -> [Value] -> Value
processFrameWithCalls kind orderId =
  processFrameWithOracleCalls defaultOracle kind orderId

processFrameWithOracleCalls
  :: OracleFixture
  -> TradeExecutionKind
  -> Integer
  -> [Value]
  -> Value
processFrameWithOracleCalls oracleFixture kind orderId settlementCalls =
  object
    [ "calls"
        .= [ oracleFrame oracleFixture
           , rawProcessFrameWithCalls
               oracleFixture
               kind
               orderId
               settlementCalls
           ]
    ]

rawProcessFrameWithCalls
  :: OracleFixture
  -> TradeExecutionKind
  -> Integer
  -> [Value]
  -> Value
rawProcessFrameWithCalls oracleFixture kind orderId =
  callFrame
    orderRouter
    engine
    "CALL"
    (processCalldataFor oracleFixture kind orderId)

processCalldata :: TradeExecutionKind -> Integer -> BS.ByteString
processCalldata kind orderId =
  processCalldataFor defaultOracle kind orderId

processCalldataFor
  :: OracleFixture
  -> TradeExecutionKind
  -> Integer
  -> BS.ByteString
processCalldataFor oracleFixture kind orderId =
  processOrderTypedSelector
    <> mconcat
      ( replaceAt
          11
          (encodeUint256 $ ofPublishTime oracleFixture)
          ( replaceAt
              9
              (encodeUint256 $ ofExecutionPrice oracleFixture)
              ( replaceAt
                  8
                  ( encodeUint256 $
                      case kind of
                        TradeOpen -> 0
                        TradeClose -> 1
                  )
                  ( replaceAt
                      6
                      (encodeUint256 orderId)
                      (replicate 12 $ encodeUint256 0)
                  )
              )
          )
      )

openSettlement :: Integer -> Value
openSettlement vpi =
  settlementFrame TradeOpen engine vpi 0 1 0

closeSettlement :: Integer -> Integer -> Value
closeSettlement vpi frozenCloseSpread =
  settlementFrame TradeClose engine vpi frozenCloseSpread 1 0

settlementFrame
  :: TradeExecutionKind
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Value
settlementFrame kind host vpi frozenCloseSpread validDelta revertCode =
  settlementFrameWithCalldata
    kind
    ( settlementCalldataWithStatus
        kind
        host
        vpi
        frozenCloseSpread
        validDelta
        revertCode
    )

settlementFrameWithCalldata :: TradeExecutionKind -> BS.ByteString -> Value
settlementFrameWithCalldata _kind calldata =
  callFrame engine settlementSidecar "CALL" calldata []

settlementCalldata
  :: TradeExecutionKind
  -> Integer
  -> Integer
  -> BS.ByteString
settlementCalldata kind vpi frozenCloseSpread =
  settlementCalldataWithStatus
    kind
    engine
    vpi
    frozenCloseSpread
    1
    0

settlementCalldataWithStatus
  :: TradeExecutionKind
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> BS.ByteString
settlementCalldataWithStatus kind host vpi frozenCloseSpread validDelta revertCode =
  executionSelector
    <> mconcat
      ( replaceAt
          11
          (encodeUint256 frozenCloseSpread)
          ( replaceAt
              9
              (encodeInt256 vpi)
              ( replaceAt
                  2
                  (encodeUint256 revertCode)
                  ( replaceAt
                      1
                      (encodeUint256 validDelta)
                      ( replaceAt
                          0
                          (encodeAddress host)
                          (replicate executionWordCount $ encodeUint256 0)
                      )
                  )
              )
          )
      )
  where
    executionSelector =
      case kind of
        TradeOpen -> executeOpenSelector
        TradeClose -> executeCloseSelector
    executionWordCount =
      case kind of
        TradeOpen -> 45
        TradeClose -> 67

-- These leading delta words come directly from the named Arbitrum Sepolia
-- transaction traces. Only zero-filled trailing fields, which this decoder
-- deliberately does not inspect, are elided from the fixture source.
liveOpen9218, liveClose9222, liveFrozenClose7051, liveClose9202 :: BS.ByteString
liveOpen9218 =
  liveSettlementCalldata
    TradeOpen
    [ "0000000000000000000000000000000000000000000000000000000b427c706d"
    , "00000000000000000000000000000000000000b6c3aa468cd4421e8eb30e9da6"
    , "0000000000000000000000000000000000000000000000000000000005db3642"
    , "0000000000000000000000000000000000000000000029a18782bc3e435f6ddd"
    , "00000000000000000000000000000000000000f3cda748bec3fe2a0bccd5f0fa"
    , "00000000000000000000000000000000000000000000000000013c4f43f0de24"
    , "000000000000000000000000000000000000000000000000000000000590b137"
    , "0000000000000000000000000000000000000000000000000000000b427c706d"
    , "0000000000000000000000000000000000000000000000000000000001272aa4"
    ]
liveClose9222 =
  liveSettlementCalldata
    TradeClose
    [ "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffd7b95202"
    , "0000000000000000000000000000000000000000000000000000000116146f53"
    , "0000000000000000000000000000000000000000000000000000000000000000"
    , "0000000000000000000000000000000000000000000000000000000000000000"
    , "0000000000000000000000000000000000000000000000000000000af19154db"
    , "0000000000000000000000000000000000000000000000000000000000f0f251"
    , "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0f0daf"
    , "000000000000000000000000000000000000000000000000000000000123013a"
    , "0000000000000000000000000000000000000000000000000000000000000000"
    ]
liveFrozenClose7051 =
  liveSettlementCalldata
    TradeClose
    [ "00000000000000000000000000000000000000000000000000000000003f976b"
    , "000000000000000000000000000000000000000000000000000000002ab33386"
    , "0000000000000000000000000000000000000000000000000000000000000000"
    , "0000000000000000000000000000000000000000000000000000000000000000"
    , "000000000000000000000000000000000000000000000000000000037e134339"
    , "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffeee3846"
    , "000000000000000000000000000000000000000000000000000000000256f99f"
    , "000000000000000000000000000000000000000000000000000000000055f8c8"
    , "000000000000000000000000000000000000000000000000000000000432a5c7"
    ]
liveClose9202 =
  liveSettlementCalldata
    TradeClose
    [ "000000000000000000000000000000000000000000000000000000000640c8fe"
    , "0000000000000000000000000000000000000000000000000000000247ea119f"
    , "0000000000000000000000000000000000000000000000000000000000000000"
    , "0000000000000000000000000000000000000000000000000000000000000000"
    , "00000000000000000000000000000000000000000000000000000017482b8ec6"
    , "fffffffffffffffffffffffffffffffffffffffffffffffffffffffff5aa62fa"
    , "000000000000000000000000000000000000000000000000000000000ae5a7e7"
    , "00000000000000000000000000000000000000000000000000000000024e600e"
    , "0000000000000000000000000000000000000000000000000000000000000000"
    ]

liveSettlementCalldata :: TradeExecutionKind -> [Text] -> BS.ByteString
liveSettlementCalldata kind deltaWords =
  executionSelector
    <> encodeAddress engine
    <> encodeUint256 1
    <> encodeUint256 0
    <> mconcat (map fixtureWord deltaWords)
    <> mconcat
      (replicate (executionWordCount - 3 - length deltaWords) $ encodeUint256 0)
  where
    executionSelector =
      case kind of
        TradeOpen -> executeOpenSelector
        TradeClose -> executeCloseSelector
    executionWordCount =
      case kind of
        TradeOpen -> 45
        TradeClose -> 67

fixtureWord :: Text -> BS.ByteString
fixtureWord =
  either (const BS.empty) id
    . Base16.decode
    . Text.encodeUtf8

callFrame
  :: Text
  -> Text
  -> Text
  -> BS.ByteString
  -> [Value]
  -> Value
callFrame fromAddress toAddress callType calldata calls =
  object
    [ "from" .= fromAddress
    , "to" .= toAddress
    , "type" .= callType
    , "input" .= hexText calldata
    , "calls" .= calls
    ]

addError :: Text -> Value -> Value
addError message original =
  case original of
    Object value -> Object $ KeyMap.insert "error" (String message) value
    value -> value

setObjectField :: Text -> Value -> Value -> Value
setObjectField field replacement original =
  case original of
    Object value -> Object $ KeyMap.insert (Key.fromText field) replacement value
    value -> value

replaceArgumentWord
  :: Int
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
replaceArgumentWord wordIndex replacement calldata =
  let offset = 4 + wordIndex * 32
   in BS.take offset calldata
        <> replacement
        <> BS.drop (offset + 32) calldata

replaceOutputWord
  :: Int
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
replaceOutputWord wordIndex replacement output =
  let offset = wordIndex * 32
   in BS.take offset output
        <> replacement
        <> BS.drop (offset + 32) output

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index replacement values =
  take index values <> [replacement] <> drop (index + 1) values

hexText :: BS.ByteString -> Text
hexText bytes =
  "0x" <> Text.decodeUtf8 (Base16.encode bytes)

orderRouter, engine, settlementSidecar, pletherOracle, externalAccount, helperContract :: Text
orderRouter = "0x04e3103752f623fbcdcd01f588590af4c53e4c1e"
engine = "0x6a25ea1015b5f032d8a2d95d57aefcb99219bf0a"
settlementSidecar = "0x0b652c4d4610234e221403076c116292f935b424"
pletherOracle = "0xadfed3bf768d810309b97b4df9f9e77eaa3a401c"
externalAccount = "0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b"
helperContract = "0x7b42000000000000000000000000000000000000"
