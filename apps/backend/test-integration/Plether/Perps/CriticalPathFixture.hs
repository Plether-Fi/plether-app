module Plether.Perps.CriticalPathFixture
  ( CanonicalBranch (..)
  , EvidenceFixture (..)
  , ScriptedChain
  , withScriptedChain
  , fixtureRpcUrl
  , fixtureTraceApiUrl
  , setCanonicalBranch
  , setTraceAvailable
  , setTraceEvidence
  , getRawTraceRequestCount
  , getUnexpectedRequests
  , assertNoUnexpectedRequests
  , testIndexerConfig
  , testAddresses
  , testChainId
  , testOrderId
  , testAccount
  , testRouter
  , testEngine
  , testSidecar
  , testOracle
  , testLens
  , testClearinghouse
  , testKeeper
  , commitBlockNumber
  , terminalBlockNumber
  , commitTxHashA
  , terminalTxHashA
  , commitBlockHashA
  , terminalBlockHashA
  , committedOnlyCommitTxHash
  , commitTxHashB
  , terminalTxHashB
  , commitBlockHashB
  , terminalBlockHashB
  , evidenceA
  , evidenceB
  , conflictingEvidence
  ) where

import Control.Monad (unless)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (toList)
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Types
  ( ResponseHeaders
  , methodGet
  , methodPost
  , status200
  , status404
  , status503
  )
import Network.Wai
  ( Application
  , Request
  , Response
  , pathInfo
  , requestMethod
  , responseLBS
  , strictRequestBody
  )
import Network.Wai.Handler.Warp (testWithApplication)
import Numeric (showHex)
import Plether.Config (PerpsCandleWriteMode (PerpsCandleWritesOff))
import Plether.Ethereum.Abi
  ( encodeAddress
  , encodeCall
  , encodeInt256
  , encodeUint256
  )
import Plether.Ethereum.Contracts.Perps (executeOrderCall)
import Plether.Indexer.Contracts (keccak256Text)
import Plether.Perps.ExecutionTrace
  ( executeCloseSelector
  , executeOracleSelector
  , processOrderTypedSelector
  )
import Plether.Perps.HistoryIndexer
  ( PerpsAddresses (..)
  , PerpsIndexerConfig (..)
  , PerpsIndexerMode (PerpsIndexerOnce)
  , perpsEventTopics
  , perpsIndexerName
  )

-- | Each constructor is a complete canonical view at the same chain height.
-- Switching views changes the cursor block hash, exercising the production
-- indexer's reorg rebuild path without timing or network dependencies.
data CanonicalBranch
  = TerminalA
  | CommittedOnly
  | Empty
  | TerminalB
  deriving (Show, Eq)

-- | Exact values returned independently by the activity preview and by the
-- authenticated execution trace. Keeping both in one fixture makes it easy for
-- the integration test to prove that finalized economics are trace-derived.
data EvidenceFixture = EvidenceFixture
  { efExecutionPrice :: Integer
  , efOraclePrice :: Integer
  , efOraclePublishTime :: Integer
  , efVpiUsdc :: Integer
  , efFrozenCloseSpreadUsdc :: Integer
  , efOracleFrozen :: Bool
  , efActivityVpiUsdc :: Integer
  , efActivityExecutionFeeUsdc :: Integer
  }
  deriving (Show, Eq)

data FixtureState = FixtureState
  { fsBranch :: CanonicalBranch
  , fsTraceAvailable :: Bool
  , fsTraceEvidence :: EvidenceFixture
  }

data ScriptedChain = ScriptedChain
  { scBaseUrl :: Text
  , scState :: IORef FixtureState
  , scRawTraceRequestCount :: IORef Int
  , scUnexpectedRequests :: IORef [Text]
  }

withScriptedChain :: (ScriptedChain -> IO a) -> IO a
withScriptedChain action = do
  stateRef <-
    newIORef
      FixtureState
        { fsBranch = TerminalA
        , fsTraceAvailable = False
        , fsTraceEvidence = evidenceA
        }
  rawTraceCountRef <- newIORef 0
  unexpectedRef <- newIORef []
  testWithApplication
    (pure $ fixtureApplication stateRef rawTraceCountRef unexpectedRef)
    $ \port ->
      let chain =
            ScriptedChain
              { scBaseUrl =
                  "http://127.0.0.1:" <> Text.pack (show port)
              , scState = stateRef
              , scRawTraceRequestCount = rawTraceCountRef
              , scUnexpectedRequests = unexpectedRef
              }
       in action chain

fixtureRpcUrl :: ScriptedChain -> Text
fixtureRpcUrl chain = scBaseUrl chain <> "/rpc"

fixtureTraceApiUrl :: ScriptedChain -> Text
fixtureTraceApiUrl chain = scBaseUrl chain <> "/api/v2"

setCanonicalBranch :: ScriptedChain -> CanonicalBranch -> IO ()
setCanonicalBranch chain branch =
  atomicModifyIORef' (scState chain) $ \state ->
    ( state
        { fsBranch = branch
        , fsTraceEvidence =
            case branch of
              TerminalA -> evidenceA
              TerminalB -> evidenceB
              _ -> fsTraceEvidence state
        }
    , ()
    )

setTraceAvailable :: ScriptedChain -> Bool -> IO ()
setTraceAvailable chain available =
  atomicModifyIORef' (scState chain) $ \state ->
    (state {fsTraceAvailable = available}, ())

setTraceEvidence :: ScriptedChain -> EvidenceFixture -> IO ()
setTraceEvidence chain evidence =
  atomicModifyIORef' (scState chain) $ \state ->
    (state {fsTraceEvidence = evidence}, ())

getRawTraceRequestCount :: ScriptedChain -> IO Int
getRawTraceRequestCount = readIORef . scRawTraceRequestCount

getUnexpectedRequests :: ScriptedChain -> IO [Text]
getUnexpectedRequests chain =
  reverse <$> readIORef (scUnexpectedRequests chain)

assertNoUnexpectedRequests :: ScriptedChain -> IO ()
assertNoUnexpectedRequests chain = do
  unexpected <- getUnexpectedRequests chain
  unless (null unexpected) $
    fail $
      "Scripted chain received unexpected requests:\n"
        <> unlines (map Text.unpack unexpected)

testIndexerConfig :: ScriptedChain -> PerpsIndexerConfig
testIndexerConfig chain =
  PerpsIndexerConfig
    { picRpcUrls = [fixtureRpcUrl chain]
    , picTraceApiUrl = Just $ fixtureTraceApiUrl chain
    , picChainId = testChainId
    , picAddresses = testAddresses
    , picStartBlock = commitBlockNumber
    , picConfirmations = 0
    , picBatchSize = 10
    , picPollIntervalMicros = 1_000_000
    , picIndexerName = perpsIndexerName
    , picMode = PerpsIndexerOnce
    , picCandleWriteMode = PerpsCandleWritesOff
    , picCandleLatenessSeconds = 120
    , picDeploymentEnvironment = Nothing
    }

fixtureApplication
  :: IORef FixtureState
  -> IORef Int
  -> IORef [Text]
  -> Application
fixtureApplication stateRef rawTraceCountRef unexpectedRef request respond
  | requestMethod request == methodPost
      && pathInfo request == ["rpc"] = do
      body <- strictRequestBody request
      response <- handleRpc stateRef unexpectedRef body
      respond $
        responseLBS
          status200
          jsonHeaders
          (Aeson.encode response)
  | requestMethod request == methodGet
      && take 2 (pathInfo request) == ["api", "v2"] =
      handleTrace
        stateRef
        rawTraceCountRef
        unexpectedRef
        request
        respond
  | otherwise = do
      recordUnexpected
        unexpectedRef
        ( "unexpected HTTP request "
            <> decodeMethod request
            <> " /"
            <> Text.intercalate "/" (pathInfo request)
        )
      respond $
        responseLBS
          status404
          jsonHeaders
          (Aeson.encode $ object ["error" .= ("not found" :: Text)])

handleRpc
  :: IORef FixtureState
  -> IORef [Text]
  -> Lazy.ByteString
  -> IO Value
handleRpc stateRef unexpectedRef body =
  case Aeson.eitherDecode body of
    Left err -> do
      recordUnexpected
        unexpectedRef
        ("invalid JSON-RPC request: " <> Text.pack err)
      pure $ rpcError Null (-32700) "parse error"
    Right (Object requestObject) -> do
      let requestId =
            fromMaybe Null $
              KeyMap.lookup (Key.fromText "id") requestObject
          params =
            fromMaybe Null $
              KeyMap.lookup (Key.fromText "params") requestObject
      case KeyMap.lookup (Key.fromText "method") requestObject of
        Just (String methodName) -> do
          state <- readIORef stateRef
          dispatchResult <-
            dispatchRpc unexpectedRef state methodName params
          pure $
            case dispatchResult of
              RpcSuccess result -> rpcSuccess requestId result
              RpcFailure code message -> rpcError requestId code message
        _ -> do
          recordUnexpected unexpectedRef "JSON-RPC request has no string method"
          pure $ rpcError requestId (-32600) "invalid request"
    Right _ -> do
      recordUnexpected unexpectedRef "JSON-RPC request is not an object"
      pure $ rpcError Null (-32600) "invalid request"

data RpcDispatchResult
  = RpcSuccess Value
  | RpcFailure Int Text

dispatchRpc
  :: IORef [Text]
  -> FixtureState
  -> Text
  -> Value
  -> IO RpcDispatchResult
dispatchRpc unexpectedRef state methodName params =
  case methodName of
    "eth_blockNumber" ->
      checkedParams unexpectedRef methodName emptyParams params $
        String "0x65"
    "eth_getLogs" ->
      checkedParams unexpectedRef methodName expectedLogsParams params $
        Aeson.toJSON (branchLogs state)
    "eth_getBlockByNumber" ->
      case arrayItems params of
        [String blockNumber, Bool False]
          | blockNumber == "0x64" ->
              pure $ RpcSuccess $ blockValue state commitBlockNumber
          | blockNumber == "0x65" ->
              pure $ RpcSuccess $ blockValue state terminalBlockNumber
        _ -> unexpectedRpc unexpectedRef methodName params
    "eth_getTransactionByHash" ->
      case arrayItems params of
        [String txHash] ->
          case transactionValue state txHash of
            Just transaction -> pure $ RpcSuccess transaction
            Nothing -> unexpectedRpc unexpectedRef methodName params
        _ -> unexpectedRpc unexpectedRef methodName params
    "eth_call" ->
      checkedParams
        unexpectedRef
        methodName
        (expectedClosePreviewParams state)
        params
        (String $ hexText $ closePreviewResult $ fsTraceEvidence state)
    "debug_traceTransaction" ->
      case arrayItems params of
        [String txHash, Object options]
          | isCanonicalTerminalHash state txHash
              && KeyMap.lookup (Key.fromText "tracer") options
                == Just (String "callTracer")
              && KeyMap.lookup (Key.fromText "timeout") options
                == Just (String "20s") ->
              pure $
                RpcFailure
                  (-32601)
                  "debug_traceTransaction intentionally unavailable"
        _ -> unexpectedRpc unexpectedRef methodName params
    _ -> unexpectedRpc unexpectedRef methodName params

checkedParams
  :: IORef [Text]
  -> Text
  -> Value
  -> Value
  -> Value
  -> IO RpcDispatchResult
checkedParams unexpectedRef methodName expected actual result
  | actual == expected = pure $ RpcSuccess result
  | otherwise = unexpectedRpc unexpectedRef methodName actual

unexpectedRpc
  :: IORef [Text]
  -> Text
  -> Value
  -> IO RpcDispatchResult
unexpectedRpc unexpectedRef methodName params = do
  recordUnexpected
    unexpectedRef
    ( "unexpected JSON-RPC call "
        <> methodName
        <> " with params "
        <> Text.pack (show params)
    )
  pure $ RpcFailure (-32602) "unexpected fixture request"

handleTrace
  :: IORef FixtureState
  -> IORef Int
  -> IORef [Text]
  -> Request
  -> (Response -> IO a)
  -> IO a
handleTrace stateRef rawTraceCountRef unexpectedRef request respond = do
  _ <-
    atomicModifyIORef' rawTraceCountRef $ \count ->
      (count + 1, count + 1)
  state <- readIORef stateRef
  case pathInfo request of
    ["api", "v2", "transactions", txHash, "raw-trace"]
      | isCanonicalTerminalHash state txHash ->
          if fsTraceAvailable state
            then
              respond $
                responseLBS
                  status200
                  jsonHeaders
                  (Aeson.encode $ executionTrace state)
            else
              respond $
                responseLBS
                  status503
                  jsonHeaders
                  ( Aeson.encode $
                      object ["error" .= ("trace not indexed yet" :: Text)]
                  )
      | otherwise -> do
          recordUnexpected
            unexpectedRef
            ("raw trace requested for noncanonical transaction " <> txHash)
          respond $
            responseLBS
              status404
              jsonHeaders
              (Aeson.encode $ object ["error" .= ("not found" :: Text)])
    _ -> do
      recordUnexpected
        unexpectedRef
        ( "unexpected trace API path /"
            <> Text.intercalate "/" (pathInfo request)
        )
      respond $
        responseLBS
          status404
          jsonHeaders
          (Aeson.encode $ object ["error" .= ("not found" :: Text)])

branchLogs :: FixtureState -> [Value]
branchLogs state =
  case fsBranch state of
    TerminalA -> terminalLogs branchA (fsTraceEvidence state)
    CommittedOnly -> [committedLog branchCommitted]
    Empty -> []
    TerminalB -> terminalLogs branchB (fsTraceEvidence state)

terminalLogs :: BranchFixture -> EvidenceFixture -> [Value]
terminalLogs branch evidence =
  [ committedLog branch
  , rpcLog
      testEngine
      [positionClosedTopic, hexText $ encodeAddress testAccount]
      ( mconcat
          [ encodeUint256 1
          , encodeUint256 100_000_000
          , encodeUint256 $ efExecutionPrice evidence
          , encodeInt256 (-75_000_000)
          ]
      )
      (bfTerminalTxHash branch)
      terminalBlockNumber
      (bfTerminalBlockHash branch)
      0
      0
  , rpcLog
      testRouter
      [orderExecutedTopic, hexText $ encodeUint256 testOrderId]
      (encodeUint256 $ efExecutionPrice evidence)
      (bfTerminalTxHash branch)
      terminalBlockNumber
      (bfTerminalBlockHash branch)
      0
      1
  ]

committedLog :: BranchFixture -> Value
committedLog branch =
  rpcLog
    testRouter
    [ orderCommittedTopic
    , hexText $ encodeUint256 testOrderId
    , hexText $ encodeAddress testAccount
    ]
    (encodeUint256 1)
    (bfCommitTxHash branch)
    commitBlockNumber
    (bfCommitBlockHash branch)
    0
    0

rpcLog
  :: Text
  -> [Text]
  -> BS.ByteString
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> Value
rpcLog address topics eventData txHash blockNumber blockHash txIndex logIndex =
  object
    [ "address" .= address
    , "topics" .= topics
    , "data" .= hexText eventData
    , "transactionHash" .= txHash
    , "blockNumber" .= quantityHex blockNumber
    , "blockHash" .= blockHash
    , "transactionIndex" .= quantityHex txIndex
    , "logIndex" .= quantityHex logIndex
    ]

blockValue :: FixtureState -> Integer -> Value
blockValue state blockNumber =
  object
    [ "number" .= quantityHex blockNumber
    , "hash" .= branchBlockHash (fsBranch state) blockNumber
    , "timestamp"
        .= quantityHex
          ( if blockNumber == commitBlockNumber
              then commitTimestamp
              else terminalTimestamp
          )
    ]

transactionValue :: FixtureState -> Text -> Maybe Value
transactionValue state requestedHash
  | requestedHash == bfCommitTxHash branch =
      Just $
        object
          [ "hash" .= bfCommitTxHash branch
          , "from" .= testAccount
          , "to" .= testRouter
          , "blockHash" .= bfCommitBlockHash branch
          , "input" .= ("0x" :: Text)
          ]
  | branchHasTerminal (fsBranch state)
      && requestedHash == bfTerminalTxHash branch =
      Just $
        object
          [ "hash" .= bfTerminalTxHash branch
          , "from" .= testKeeper
          , "to" .= testRouter
          , "blockHash" .= bfTerminalBlockHash branch
          , "input" .= hexText executionTransactionInput
          ]
  | otherwise = Nothing
  where
    branch = branchFixture $ fsBranch state

executionTrace :: FixtureState -> Value
executionTrace state =
  object
    [ "from" .= testKeeper
    , "to" .= testRouter
    , "type" .= ("CALL" :: Text)
    , "input" .= hexText executionTransactionInput
    , "calls"
        .= [oracleFrame evidence, processFrame evidence]
    ]
  where
    evidence = fsTraceEvidence state

oracleFrame :: EvidenceFixture -> Value
oracleFrame evidence =
  object
    [ "from" .= testRouter
    , "to" .= testOracle
    , "type" .= ("CALL" :: Text)
    , "input" .= hexText executeOracleSelector
    , "output"
        .= hexText
          ( mconcat
              [ encodeUint256 1
              , encodeUint256 $ efExecutionPrice evidence
              , encodeUint256 $ efOraclePrice evidence
              , encodeUint256 $ efOraclePublishTime evidence
              , encodeUint256 0
              , encodeUint256 60
              , encodeUint256 0
              , encodeUint256 $ if efOracleFrozen evidence then 1 else 0
              , encodeUint256 0
              ]
          )
    , "calls" .= ([] :: [Value])
    ]

processFrame :: EvidenceFixture -> Value
processFrame evidence =
  object
    [ "from" .= testRouter
    , "to" .= testEngine
    , "type" .= ("CALL" :: Text)
    , "input" .= hexText (processCalldata evidence)
    , "calls" .= [settlementFrame evidence]
    ]

settlementFrame :: EvidenceFixture -> Value
settlementFrame evidence =
  object
    [ "from" .= testEngine
    , "to" .= testSidecar
    , "type" .= ("CALL" :: Text)
    , "input" .= hexText (settlementCalldata evidence)
    , "calls" .= ([] :: [Value])
    ]

processCalldata :: EvidenceFixture -> BS.ByteString
processCalldata evidence =
  processOrderTypedSelector
    <> mconcat
      ( replaceAt
          11
          (encodeUint256 $ efOraclePublishTime evidence)
          ( replaceAt
              9
              (encodeUint256 $ efExecutionPrice evidence)
              ( replaceAt
                  8
                  (encodeUint256 1)
                  ( replaceAt
                      6
                      (encodeUint256 testOrderId)
                      (replicate 12 $ encodeUint256 0)
                  )
              )
          )
      )

settlementCalldata :: EvidenceFixture -> BS.ByteString
settlementCalldata evidence =
  executeCloseSelector
    <> mconcat
      ( replaceAt
          11
          (encodeUint256 $ efFrozenCloseSpreadUsdc evidence)
          ( replaceAt
              9
              (encodeInt256 $ efVpiUsdc evidence)
              ( replaceAt
                  2
                  (encodeUint256 0)
                  ( replaceAt
                      1
                      (encodeUint256 1)
                      ( replaceAt
                          0
                          (encodeAddress testEngine)
                          (replicate 67 $ encodeUint256 0)
                      )
                  )
              )
          )
      )

closePreviewResult :: EvidenceFixture -> BS.ByteString
closePreviewResult evidence =
  mconcat $
    replaceAt
      7
      (encodeUint256 $ efActivityExecutionFeeUsdc evidence)
      ( replaceAt
          5
          (encodeInt256 $ efActivityVpiUsdc evidence)
          (replicate 8 $ encodeUint256 0)
      )

expectedClosePreviewParams :: FixtureState -> Value
expectedClosePreviewParams state =
  Aeson.toJSON
    [ object
        [ "to" .= testLens
        , "data"
            .= hexText
              ( encodeCall
                  "previewClose(address,uint256,uint256)"
                  [ encodeAddress testAccount
                  , encodeUint256 100_000_000
                  , encodeUint256 $
                      efExecutionPrice (fsTraceEvidence state)
                  ]
              )
        ]
    , String "0x64"
    ]

expectedLogsParams :: Value
expectedLogsParams =
  Aeson.toJSON
    [ object
        [ "address"
            .= [testRouter, testEngine, testClearinghouse]
        , "topics"
            .= [map (String . hexText) perpsEventTopics]
        , "fromBlock" .= ("0x64" :: Text)
        , "toBlock" .= ("0x65" :: Text)
        ]
    ]

emptyParams :: Value
emptyParams = Aeson.toJSON ([] :: [Value])

rpcSuccess :: Value -> Value -> Value
rpcSuccess requestId result =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= requestId
    , "result" .= result
    ]

rpcError :: Value -> Int -> Text -> Value
rpcError requestId code message =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= requestId
    , "error"
        .= object
          [ "code" .= code
          , "message" .= message
          ]
    ]

recordUnexpected :: IORef [Text] -> Text -> IO ()
recordUnexpected ref message =
  atomicModifyIORef' ref $ \messages ->
    (message : messages, ())

decodeMethod :: Request -> Text
decodeMethod =
  Text.decodeUtf8With
    (\_ _ -> Just '\xfffd')
    . requestMethod

jsonHeaders :: ResponseHeaders
jsonHeaders = [("Content-Type", "application/json")]

arrayItems :: Value -> [Value]
arrayItems = \case
  Array values -> toList values
  _ -> []

isCanonicalTerminalHash :: FixtureState -> Text -> Bool
isCanonicalTerminalHash state txHash =
  branchHasTerminal (fsBranch state)
    && Text.toLower txHash
      == Text.toLower (bfTerminalTxHash $ branchFixture $ fsBranch state)

branchHasTerminal :: CanonicalBranch -> Bool
branchHasTerminal TerminalA = True
branchHasTerminal TerminalB = True
branchHasTerminal _ = False

branchBlockHash :: CanonicalBranch -> Integer -> Text
branchBlockHash branch blockNumber
  | blockNumber == commitBlockNumber = bfCommitBlockHash fixture
  | otherwise = bfTerminalBlockHash fixture
  where
    fixture = branchFixture branch

data BranchFixture = BranchFixture
  { bfCommitTxHash :: Text
  , bfTerminalTxHash :: Text
  , bfCommitBlockHash :: Text
  , bfTerminalBlockHash :: Text
  }

branchFixture :: CanonicalBranch -> BranchFixture
branchFixture = \case
  TerminalA -> branchA
  CommittedOnly -> branchCommitted
  Empty -> branchEmpty
  TerminalB -> branchB

branchA, branchCommitted, branchEmpty, branchB :: BranchFixture
branchA =
  BranchFixture
    { bfCommitTxHash = commitTxHashA
    , bfTerminalTxHash = terminalTxHashA
    , bfCommitBlockHash = commitBlockHashA
    , bfTerminalBlockHash = terminalBlockHashA
    }
branchCommitted =
  BranchFixture
    { bfCommitTxHash = committedOnlyCommitTxHash
    , bfTerminalTxHash = fixedHash 0xc002
    , bfCommitBlockHash = fixedHash 0xc100
    , bfTerminalBlockHash = fixedHash 0xc101
    }
branchEmpty =
  BranchFixture
    { bfCommitTxHash = fixedHash 0xe001
    , bfTerminalTxHash = fixedHash 0xe002
    , bfCommitBlockHash = fixedHash 0xe100
    , bfTerminalBlockHash = fixedHash 0xe101
    }
branchB =
  BranchFixture
    { bfCommitTxHash = commitTxHashB
    , bfTerminalTxHash = terminalTxHashB
    , bfCommitBlockHash = commitBlockHashB
    , bfTerminalBlockHash = terminalBlockHashB
    }

testAddresses :: PerpsAddresses
testAddresses =
  PerpsAddresses
    { paOrderRouter = testRouter
    , paCfdEngine = testEngine
    , paCfdEngineLens = testLens
    , paCfdEngineSettlementSidecar = testSidecar
    , paMarginClearinghouse = testClearinghouse
    , paPletherOracle = testOracle
    }

testChainId, testOrderId :: Integer
testChainId = 987_654_321
testOrderId = 42

testAccount, testRouter, testEngine, testSidecar, testOracle, testLens, testClearinghouse, testKeeper :: Text
testAccount = fixedAddress 0xa1
testRouter = fixedAddress 0xb1
testEngine = fixedAddress 0xb2
testSidecar = fixedAddress 0xb3
testOracle = fixedAddress 0xb4
testLens = fixedAddress 0xb5
testClearinghouse = fixedAddress 0xb6
testKeeper = fixedAddress 0xc1

commitBlockNumber, terminalBlockNumber, commitTimestamp, terminalTimestamp :: Integer
commitBlockNumber = 100
terminalBlockNumber = 101
commitTimestamp = 1_785_437_800
terminalTimestamp = 1_785_437_841

commitTxHashA, terminalTxHashA, commitBlockHashA, terminalBlockHashA :: Text
commitTxHashA = fixedHash 0xa001
terminalTxHashA = fixedHash 0xa002
commitBlockHashA = fixedHash 0xa100
terminalBlockHashA = fixedHash 0xa101

committedOnlyCommitTxHash :: Text
committedOnlyCommitTxHash = fixedHash 0xc001

commitTxHashB, terminalTxHashB, commitBlockHashB, terminalBlockHashB :: Text
commitTxHashB = fixedHash 0xb001
terminalTxHashB = fixedHash 0xb002
commitBlockHashB = fixedHash 0xb100
terminalBlockHashB = fixedHash 0xb101

evidenceA, evidenceB, conflictingEvidence :: EvidenceFixture
evidenceA =
  EvidenceFixture
    { efExecutionPrice = 98_391_251
    , efOraclePrice = 98_391_482
    , efOraclePublishTime = 1_785_437_834
    , efVpiUsdc = 182_822_887
    , efFrozenCloseSpreadUsdc = 0
    , efOracleFrozen = False
    , efActivityVpiUsdc = -444_000_000
    , efActivityExecutionFeeUsdc = 1_748_645_480
    }
evidenceB =
  EvidenceFixture
    { efExecutionPrice = 98_400_000
    , efOraclePrice = 98_399_000
    , efOraclePublishTime = 1_785_437_934
    , efVpiUsdc = -123_456_789
    , efFrozenCloseSpreadUsdc = 3_940_366
    , efOracleFrozen = True
    , efActivityVpiUsdc = 777_000_000
    , efActivityExecutionFeeUsdc = 1_900_000_000
    }
conflictingEvidence =
  evidenceA
    { efOraclePrice = 77_777_777
    , efVpiUsdc = 9_999_999_999
    , efFrozenCloseSpreadUsdc = 8_888_888
    , efOracleFrozen = True
    }

executionTransactionInput :: BS.ByteString
executionTransactionInput = executeOrderCall testOrderId []

orderCommittedTopic, orderExecutedTopic, positionClosedTopic :: Text
orderCommittedTopic =
  hexText $ keccak256Text "OrderCommitted(uint64,address,uint8)"
orderExecutedTopic =
  hexText $ keccak256Text "OrderExecuted(uint64,uint256)"
positionClosedTopic =
  hexText $ keccak256Text "PositionClosed(address,uint8,uint256,uint256,int256)"

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index replacement values =
  take index values <> [replacement] <> drop (index + 1) values

fixedAddress :: Integer -> Text
fixedAddress value = fixedHex 20 value

fixedHash :: Integer -> Text
fixedHash value = fixedHex 32 value

fixedHex :: Int -> Integer -> Text
fixedHex byteCount value =
  "0x"
    <> Text.justifyRight
      (byteCount * 2)
      '0'
      (Text.pack $ showHex value "")

quantityHex :: Integer -> Text
quantityHex value = "0x" <> Text.pack (showHex value "")

hexText :: BS.ByteString -> Text
hexText bytes =
  "0x" <> Text.decodeUtf8 (Base16.encode bytes)
