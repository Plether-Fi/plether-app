{-# LANGUAGE OverloadedStrings #-}

module Plether.Perps.Funding.WorkerSpec (fundingWorkerSpec) where

import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Aeson (Value (..), eitherDecode, encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, modifyIORef')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Only (..), execute_, query_)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200)
import Network.Wai (Application, pathInfo, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Database (DbPool, destroyDbPool, newDbPool, withDb)
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256, keccak256)
import Plether.Ethereum.Client (EthClient, RpcClientOptions (..), newClientWithManager, parseRpcQuantity)
import Plether.Ethereum.Rpc (RpcLog (..), TxReceipt (..))
import Plether.Perps.Funding.Across (buildAcrossDestinationMessage)
import Plether.Perps.Funding.Chain
import Plether.Perps.Funding.Store
import Plether.Perps.Funding.Types
import Plether.Perps.Funding.Worker (reconcileFundingOnce)
import Plether.Utils.Hex (intToHex)
import Test.Hspec hiding (pending)

fundingWorkerSpec :: Text -> Spec
fundingWorkerSpec databaseUrl = around (withFixture databaseUrl) $ describe "unsigned bridge funding observer recovery" $ do
  it "persists canonical credit and revalidates it after restart without duplicating it" $ \fixture -> do
    seed fixture first
    observe fixture `shouldReturn` Right ()
    assertField fixture first "status" "confirmed"
    assertField fixture first "creditedAmount" "1000000"
    assertField fixture first "depositTxHash" (destinationTx first)
    originalRelay <- withDb (db fixture) $ \conn -> getSourceRelay conn (intentId first)
    originalRelay `shouldSatisfy` maybe False (const True)
    -- Restart with a new DB pool and no discovery logs. Previously recorded
    -- proof must be checked by receipt, not counted again from a new scan.
    modifyIORef' (state fixture) $ \s -> s {destinationLogs=[]}
    scansBefore <- countCalls fixture "eth_getLogs"
    bracket (newDbPool $ scopedDatabaseUrl databaseUrl) destroyDbPool $ \restarted ->
      observe (fixture {db=restarted}) `shouldReturn` Right ()
    countCalls fixture "eth_getLogs" `shouldReturn` scansBefore
    assertField fixture first "creditedAmount" "1000000"
    withDb (db fixture) (\conn -> getSourceRelay conn $ intentId first) `shouldReturn` originalRelay
    ready fixture `shouldReturn` True

  it "persists beneficiary fallback as needs-deposit across restart" $ \fixture -> do
    seed fixture first
    modifyIORef' (state fixture) $ \s -> s {destinationReceipts=[(destinationTx first,fallbackReceipt first)]}
    observe fixture `shouldReturn` Right ()
    assertField fixture first "status" "needs-deposit"
    assertField fixture first "fallbackAmount" "1000000"
    assertField fixture first "creditedAmount" "0"
    assertAbsent fixture first "depositTxHash"
    bracket (newDbPool $ scopedDatabaseUrl databaseUrl) destroyDbPool $ \restarted ->
      observe (fixture {db=restarted}) `shouldReturn` Right ()
    assertField fixture first "status" "needs-deposit"
    ready fixture `shouldReturn` True

  it "waits for source finality before claiming the relay or inspecting destination fills" $ \fixture -> do
    seed fixture first
    modifyIORef' (state fixture) $ \s -> s {sourceHead=10}
    observe fixture `shouldReturn` Right ()
    assertField fixture first "sourceStatus" "pending"
    assertField fixture first "status" "bridging"
    withDb (db fixture) (\conn -> getSourceRelay conn $ intentId first) `shouldReturn` Nothing
    countCalls fixture "eth_getLogs" `shouldReturn` 0
    modifyIORef' (state fixture) $ \s -> s {sourceHead=11}
    observe fixture `shouldReturn` Right ()
    assertField fixture first "status" "confirmed"

  it "marks a reverted source transaction terminal only after canonical confirmation depth" $ \fixture -> do
    seed fixture first
    modifyIORef' (state fixture) $ \s -> s {sourceHead=10
      ,sourceReceipts=[(sourceTx first,(sourceReceipt first) {receiptSucceeded=False,receiptLogs=[]})]}
    observe fixture `shouldReturn` Right ()
    assertField fixture first "sourceStatus" "pending"
    assertValue fixture first "sourceTerminal" $ Bool False
    assertValue fixture first "sourceConfirmations" $ toJSON (1 :: Integer)
    withDb (db fixture) (\conn -> getSourceRelay conn $ intentId first) `shouldReturn` Nothing
    countCalls fixture "eth_getLogs" `shouldReturn` 0
    ready fixture `shouldReturn` True
    modifyIORef' (state fixture) $ \s -> s {sourceHead=11}
    observe fixture `shouldReturn` Right ()
    assertField fixture first "sourceStatus" "reverted"
    assertValue fixture first "sourceTerminal" $ Bool True
    assertValue fixture first "sourceConfirmations" $ toJSON (2 :: Integer)
    assertField fixture first "creditedAmount" "0"
    assertAbsent fixture first "depositTxHash"
    withDb (db fixture) (\conn -> getSourceRelay conn $ intentId first) `shouldReturn` Nothing
    countCalls fixture "eth_getLogs" `shouldReturn` 0
    ready fixture `shouldReturn` True

  it "retracts destination credit after a canonical reorg and recovers a replacement fill" $ \fixture -> do
    seed fixture first
    observe fixture `shouldReturn` Right ()
    relay <- withDb (db fixture) $ \conn -> getSourceRelay conn (intentId first)
    modifyIORef' (state fixture) $ \s -> s {destinationHash=otherHash,destinationReceipts=[],destinationLogs=[]}
    observe fixture `shouldReturn` Right ()
    assertField fixture first "status" "bridging"
    assertField fixture first "creditedAmount" "0"
    assertAbsent fixture first "depositTxHash"
    withDb (db fixture) (\conn -> getSourceRelay conn $ intentId first) `shouldReturn` relay
    -- A changed scan boundary rewinds discovery before this canonical fill.
    modifyIORef' (state fixture) $ \s -> s {destinationHash=destinationBlockHash
      ,destinationReceipts=[(destinationTx first,successReceipt first)],destinationLogs=[fillLog first]}
    observe fixture `shouldReturn` Right ()
    assertField fixture first "status" "confirmed"

  it "invalidates a positively orphaned source claim and clears destination proof" $ \fixture -> do
    seed fixture first
    observe fixture `shouldReturn` Right ()
    modifyIORef' (state fixture) $ \s -> s {sourceHash=otherHash,sourceReceipts=[]}
    observe fixture `shouldReturn` Right ()
    assertField fixture first "status" "bridging"
    assertField fixture first "creditedAmount" "0"
    assertAbsent fixture first "depositTxHash"
    assertAbsent fixture first "sourceRelay"
    withDb (db fixture) (\conn -> getSourceRelay conn $ intentId first) `shouldReturn` Nothing
    -- Orphaning preserves the record and permanent relay ownership for audit.
    withDb (db fixture) (\conn -> query_ conn "SELECT canonical,jsonb_array_length(orphaned_evidence) FROM perps_funding_source_relays" :: IO [(Bool,Int)])
      `shouldReturn` [(False,1)]
    modifyIORef' (state fixture) $ \s -> s {sourceHash=sourceBlockHash,sourceReceipts=[(sourceTx first,sourceReceipt first)]}
    observe fixture `shouldReturn` Right ()
    assertField fixture first "status" "confirmed"

  it "preserves a claimed source relay on RPC failure and resumes after restart" $ \fixture -> do
    seed fixture first
    observe fixture `shouldReturn` Right ()
    relay <- withDb (db fixture) $ \conn -> getSourceRelay conn (intentId first)
    modifyIORef' (state fixture) $ \s -> s {sourceUnavailable=True}
    outcome <- observe fixture
    outcome `shouldSatisfy` either (const True) (const False)
    ready fixture `shouldReturn` False
    withDb (db fixture) (\conn -> getSourceRelay conn $ intentId first) `shouldReturn` relay
    assertField fixture first "creditedAmount" "1000000"
    modifyIORef' (state fixture) $ \s -> s {sourceUnavailable=False}
    bracket (newDbPool $ scopedDatabaseUrl databaseUrl) destroyDbPool $ \restarted ->
      observe (fixture {db=restarted}) `shouldReturn` Right ()
    assertField fixture first "status" "confirmed"
    ready fixture `shouldReturn` True

  it "does not let a dropped source transaction prevent a later intent or readiness" $ \fixture -> do
    seed fixture first
    seed fixture second
    modifyIORef' (state fixture) $ \s -> s {sourceTransactions=filter ((/= sourceTx first) . fst) $ sourceTransactions s}
    observe fixture `shouldReturn` Right ()
    assertAbsent fixture first "sourceRelay"
    assertField fixture second "status" "confirmed"
    ready fixture `shouldReturn` True

  it "does not let a mismatched source transaction prevent a later intent or readiness" $ \fixture -> do
    seed fixture first
    seed fixture second
    modifyIORef' (state fixture) $ \s -> s {sourceTransactions=
      [(hash,if hash == sourceTx first then setFields [("from",String beneficiary)] tx else tx) | (hash,tx) <- sourceTransactions s]}
    observe fixture `shouldReturn` Right ()
    assertAbsent fixture first "sourceRelay"
    assertField fixture second "status" "confirmed"
    ready fixture `shouldReturn` True

  it "retains relay ownership when a confirmed source receipt disappears and rediscovers credit when it returns" $ \fixture -> do
    seed fixture first
    observe fixture `shouldReturn` Right ()
    relay <- withDb (db fixture) $ \conn -> getSourceRelay conn (intentId first)
    modifyIORef' (state fixture) $ \s -> s {sourceReceipts=[]}
    observe fixture `shouldReturn` Right ()
    assertField fixture first "status" "bridging"
    assertField fixture first "creditedAmount" "0"
    withDb (db fixture) (\conn -> getSourceRelay conn $ intentId first) `shouldReturn` relay
    modifyIORef' (state fixture) $ \s -> s {sourceReceipts=[(sourceTx first,sourceReceipt first)]}
    bracket (newDbPool $ scopedDatabaseUrl databaseUrl) destroyDbPool $ \restarted ->
      observe (fixture {db=restarted}) `shouldReturn` Right ()
    assertField fixture first "status" "confirmed"
    assertField fixture first "creditedAmount" "1000000"

  it "retries a discovered fill whose receipt was temporarily missing, including after restart" $ \fixture -> do
    seed fixture first
    modifyIORef' (state fixture) $ \s -> s {destinationReceipts=[]}
    observe fixture `shouldReturn` Right ()
    assertAbsent fixture first "depositTxHash"
    modifyIORef' (state fixture) $ \s -> s {destinationReceipts=[(destinationTx first,successReceipt first)]}
    bracket (newDbPool $ scopedDatabaseUrl databaseUrl) destroyDbPool $ \restarted ->
      observe (fixture {db=restarted}) `shouldReturn` Right ()
    assertField fixture first "status" "confirmed"

  it "ignores an orphaned relay's old destination fill when a canonical relay reuses its deposit ID" $ \fixture -> do
    seed fixture first
    observe fixture `shouldReturn` Right ()
    -- Re-inclusion after a source reorg can allocate the same deposit ID to
    -- changed relay facts. The old destination fill stays canonical on L2.
    let newTx = identifier 'e'
        newSourceLog = (sourceLog first) {rpcLogBlockHash=otherHash
          ,rpcLogData=replaceWord 5 (encodeUint256 1001) $ rpcLogData $ sourceLog first}
        newSourceReceipt = (sourceReceipt first) {receiptBlockHash=otherHash,receiptLogs=[newSourceLog]}
        newLogs = [entryLog {rpcLogTxHash=newTx,rpcLogData=if rpcLogIndex entryLog == 0
                    then replaceWord 5 (encodeUint256 1001) $ rpcLogData entryLog else rpcLogData entryLog}
                  | entryLog <- receiptLogs $ successReceipt first]
        newReceipt = (successReceipt first) {receiptTxHash=newTx,receiptLogs=newLogs}
    modifyIORef' (state fixture) $ \s -> s {sourceHash=otherHash,sourceReceipts=[(sourceTx first,newSourceReceipt)]
      ,destinationReceipts=destinationReceipts s <> [(newTx,newReceipt)],destinationLogs=destinationLogs s <> take 1 newLogs}
    observe fixture `shouldReturn` Right ()
    assertField fixture first "status" "confirmed"
    assertField fixture first "depositTxHash" newTx
    relay <- withDb (db fixture) $ \conn -> getSourceRelay conn (intentId first)
    (relay >>= fieldText "fillDeadline") `shouldBe` Just "1001"
    withDb (db fixture) (\conn -> query_ conn "SELECT canonical,count(*) FROM perps_funding_source_relays GROUP BY canonical ORDER BY canonical" :: IO [(Bool,Int)])
      `shouldReturn` [(False,1),(True,1)]

  it "skips an intent with a different pinned implementation hash even when release and addresses match" $ \fixture -> do
    withDb (db fixture) $ \conn -> do
      let otherQuote = setFields [("destinationSpokePoolImplementationCodeHash",String otherHash)] $ quote first
          initial = intentFromQuote (intentId first) otherQuote
      insertQuote conn (quoteId first) otherQuote expiry
      createIntent conn "observer-other-deployment" (intentId first) (quoteId first) initial `shouldReturn` Right initial
      updateIntent conn (intentId first) $ setFields [("status",String "bridging"),("sourceTxHash",String $ sourceTx first)] initial
    observe fixture `shouldReturn` Right ()
    countCalls fixture "eth_getTransactionByHash" `shouldReturn` 0
    assertField fixture first "status" "bridging"
    assertAbsent fixture first "sourceRelay"
    ready fixture `shouldReturn` True

  it "withdraws readiness on a changed destination implementation before observing any intent" $ \fixture -> do
    seed fixture first
    withDb (db fixture) $ \conn -> setWorkerReadiness conn (deploymentReadinessKey deployment) destinationChain True
    modifyIORef' (state fixture) $ \s -> s {changedImplementation=True}
    observe fixture `shouldReturn` Left "ACROSS_IMPLEMENTATION_MISMATCH"
    ready fixture `shouldReturn` False
    countCalls fixture "eth_getTransactionByHash" `shouldReturn` 0
    assertField fixture first "status" "bridging"

data Fixture = Fixture {db :: DbPool,destination :: EthClient,source :: EthClient,state :: IORef RpcState,calls :: IORef [(Text,Text,[Value])]}
data RpcState = RpcState
  { sourceTransactions :: [(Text,Value)],sourceReceipts :: [(Text,TxReceipt)],destinationReceipts :: [(Text,TxReceipt)]
  , destinationLogs :: [RpcLog],sourceHash :: Text,destinationHash :: Text,sourceHead :: Integer,destinationHead :: Integer
  , sourceUnavailable :: Bool,changedImplementation :: Bool }

withFixture :: Text -> (Fixture -> IO ()) -> IO ()
withFixture databaseUrl action = bracket (newDbPool $ scopedDatabaseUrl databaseUrl) destroyDbPool $ \pool -> do
  withDb pool $ \conn -> do
    names <- query_ conn "SELECT current_database()" :: IO [Only Text]
    case names of
      [Only name] | "_test" `T.isSuffixOf` name || "critical_path" `T.isInfixOf` name -> pure ()
      _ -> fail "Funding worker tests require a dedicated _test or critical_path database"
    void $ execute_ conn "DROP SCHEMA IF EXISTS perps_funding_worker_spec CASCADE"
    void $ execute_ conn "CREATE SCHEMA perps_funding_worker_spec"
    ensureFundingSchema conn
  let cleanup = withDb pool $ \conn -> void $ execute_ conn "DROP SCHEMA perps_funding_worker_spec CASCADE"
  (do
    rpc <- newIORef $ RpcState [] [] [] [] sourceBlockHash destinationBlockHash 11 11 False False
    observed <- newIORef []
    unexpectedCalls <- newIORef []
    testWithApplication (pure $ rpcApplication rpc observed unexpectedCalls) $ \port -> do
      manager <- newManager defaultManagerSettings
      requestId <- newIORef 1
      let client path = newClientWithManager manager requestId $ RpcClientOptions
            ("http://127.0.0.1:" <> T.pack (show port) <> path) Nothing "funding-observer-test"
      destinationClient <- client "/destination"
      sourceClient <- client "/source"
      let fixture = Fixture pool destinationClient sourceClient rpc observed
      action fixture
      readIORef unexpectedCalls `shouldReturn` []
      methods <- map (\(_,method,_) -> method) <$> readIORef observed
      -- Every scenario is a read-only observer: even gas/nonce preparation is
      -- forbidden, and an accidental write RPC also fails the strict mock.
      mapM_ (\method -> methods `shouldNotContain` [method])
        ["eth_sendRawTransaction","eth_sendTransaction","eth_signTransaction","eth_getTransactionCount","eth_estimateGas","eth_gasPrice","eth_getBalance"])
    `finally` cleanup

scopedDatabaseUrl :: Text -> Text
scopedDatabaseUrl url
  | "postgres://" `T.isPrefixOf` url || "postgresql://" `T.isPrefixOf` url =
      url <> (if "?" `T.isInfixOf` url then "&" else "?") <> "options=-c%20search_path%3Dperps_funding_worker_spec"
  | otherwise = url <> " options='-c search_path=perps_funding_worker_spec'"

observe :: Fixture -> IO (Either Text ())
observe fixture = reconcileFundingOnce (destination fixture) (source fixture) (db fixture) deployment
ready :: Fixture -> IO Bool
ready fixture = withDb (db fixture) $ \conn -> isWorkerReady conn (deploymentReadinessKey deployment) destinationChain
countCalls :: Fixture -> Text -> IO Int
countCalls fixture method = length . filter (\(_,name,_) -> name == method) <$> readIORef (calls fixture)
assertField :: Fixture -> FundingCase -> Text -> Text -> IO ()
assertField fixture funding key expected = do
  current <- withDb (db fixture) $ \conn -> getIntent conn $ intentId funding
  (current >>= fieldText key) `shouldBe` Just expected
assertValue :: Fixture -> FundingCase -> Text -> Value -> IO ()
assertValue fixture funding key expected = do
  current <- withDb (db fixture) $ \conn -> getIntent conn $ intentId funding
  let value = case current of Just (Object fields) -> KM.lookup (Key.fromText key) fields; _ -> Nothing
  value `shouldBe` Just expected
assertAbsent :: Fixture -> FundingCase -> Text -> IO ()
assertAbsent fixture funding key = do
  current <- withDb (db fixture) $ \conn -> getIntent conn $ intentId funding
  let value = case current of Just (Object fields) -> KM.lookup (Key.fromText key) fields; _ -> Nothing
  value `shouldSatisfy` \item -> item == Nothing || item == Just Null

rpcApplication :: IORef RpcState -> IORef [(Text,Text,[Value])] -> IORef [Value] -> Application
rpcApplication stateRef observed unexpectedRef request respond = do
  body <- strictRequestBody request
  let envelope = either (const Null) id $ eitherDecode body
      requestIdentifier = case envelope of Object fields -> fromMaybe Null $ KM.lookup "id" fields; _ -> Null
      method = fromMaybe "" $ fieldText "method" envelope
      params = case envelope of
        Object fields -> case KM.lookup "params" fields of
          Just (Array xs) -> toList xs
          _ -> []
        _ -> []
      path = T.intercalate "/" $ pathInfo request
      isSource = path == "source"
  atomicModifyIORef' observed $ \items -> (items <> [(path,method,params)],())
  current <- readIORef stateRef
  let answer = Right
      outage = Left $ object ["code" .= (-32000 :: Int),"message" .= ("temporary source RPC outage" :: Text)]
      headBlock = if isSource then sourceHead current else destinationHead current
      blockHash = if isSource then sourceHash current else destinationHash current
      unknown = do
        atomicModifyIORef' unexpectedRef $ \items -> (items <> [envelope],())
        pure $ Left $ object ["code" .= (-32601 :: Int),"message" .= ("Unexpected observer RPC" :: Text)]
  result <- if isSource && sourceUnavailable current then pure outage else case (path,method,params) of
    (_,"eth_chainId",[]) | path `elem` ["source","destination"] -> pure $ answer $ String $ quantity $ if isSource then 1 else destinationChain
    (_,"eth_blockNumber",[]) -> pure $ answer $ String $ quantity headBlock
    (_,"eth_getBlockByNumber",[String tag,Bool False]) -> pure $ answer $ object
      ["number" .= (if tag == "latest" then quantity headBlock else tag),"hash" .= blockHash,"timestamp" .= ("0x64" :: Text)]
    ("source","eth_getTransactionByHash",[String hash]) -> pure $ answer $ fromMaybe Null $ lookup hash $ sourceTransactions current
    (_,"eth_getTransactionReceipt",[String hash]) -> pure $ answer $ maybe Null receiptJSON $
      lookup hash $ if isSource then sourceReceipts current else destinationReceipts current
    ("destination","eth_getLogs",[Object filterFields]) -> case (KM.lookup "address" filterFields,KM.lookup "topics" filterFields,KM.lookup "fromBlock" filterFields,KM.lookup "toBlock" filterFields) of
      (Just (String target),Just topics,Just (String from),Just (String to))
        | target == spoke && topics == toJSON [[encodeHex filledRelayTopic]]
        , Right lower <- parseRpcQuantity "from" from,Right upper <- parseRpcQuantity "to" to ->
            pure $ answer $ toJSON $ map logJSON $ filter (\logEntry -> rpcLogBlockNumber logEntry >= lower && rpcLogBlockNumber logEntry <= upper) $ destinationLogs current
      _ -> unknown
    ("destination","eth_getCode",[String target,String _]) -> case lookup target runtimeCodes of
      Just bytes -> pure $ answer $ String $ encodeHex bytes
      Nothing -> unknown
    ("destination","eth_getStorageAt",[String target,String slot,String _])
      | target == spoke && slot == "0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc" ->
          pure $ answer $ String $ encodeHex $ encodeAddress $ if changedImplementation current then beneficiary else implementation
    ("destination","eth_call",[Object call,String _])
      | KM.lookup "to" call == Just (String clearinghouse)
      , KM.lookup "data" call == Just (String $ encodeHex $ encodeCall "settlementAsset()" []) ->
          pure $ answer $ String $ encodeHex $ encodeAddress token
    _ -> unknown
  respond $ responseLBS status200 [("Content-Type","application/json")] $ encode $ object
    (["jsonrpc" .= ("2.0" :: Text),"id" .= requestIdentifier] <> either (\err -> ["error" .= err]) (\value -> ["result" .= value]) result)

-- Each quote has a distinct marker and relay, even for the same beneficiary.
data FundingCase = FundingCase {quoteId :: Text,intentId :: Text,sourceTx :: Text,destinationTx :: Text,depositId :: Integer}
first,second :: FundingCase
first = FundingCase (identifier '1') (identifier '2') (identifier '3') (identifier '9') 7
second = FundingCase (identifier '4') (identifier '5') (identifier '6') (identifier 'c') 8
seed :: Fixture -> FundingCase -> IO ()
seed fixture funding = do
  withDb (db fixture) $ \conn -> do
    insertQuote conn (quoteId funding) (quote funding) expiry
    let initial = intentFromQuote (intentId funding) $ quote funding
    createIntent conn ("observer-test-" <> T.drop 2 (intentId funding)) (intentId funding) (quoteId funding) initial `shouldReturn` Right initial
    updateIntent conn (intentId funding) $ setFields [("status",String "bridging"),("sourceTxHash",String $ sourceTx funding)] initial
  modifyIORef' (state fixture) $ \s -> s
    { sourceTransactions=sourceTransactions s <> [(sourceTx funding,object ["hash" .= sourceTx funding,"from" .= owner,"to" .= originSpokePool,"input" .= plannedData funding,"value" .= ("0x0" :: Text)])]
    , sourceReceipts=sourceReceipts s <> [(sourceTx funding,sourceReceipt funding)]
    , destinationReceipts=destinationReceipts s <> [(destinationTx funding,successReceipt funding)]
    , destinationLogs=destinationLogs s <> [fillLog funding] }
quote :: FundingCase -> Value
quote funding = setFields
  [("quoteId",String $ quoteId funding),("expiresAt",toJSON expiry),("beneficiary",String beneficiary),("ownerAddress",String owner)
  ,("minimumAmount",String "1000000"),("estimatedAmount",String "1000000"),("sourceChainId",toJSON (1 :: Integer))
  ,("sourceToken",String sourceToken),("sourceAmount",String "1000000"),("provider",String "across")
  ,("destinationMessage",String $ encodeHex $ message funding),("destinationMessageHash",String $ encodeHex $ keccak256 $ message funding)
  ,("sourceTransactions",toJSON [SourceTransaction 1 "bridge" originSpokePool (plannedData funding) "0"])
  ,("observationFromBlock",toJSON (10 :: Integer))] $ toJSON deployment
plannedData :: FundingCase -> Text
plannedData funding = "0x12345678" <> T.drop 2 (quoteId funding)
message :: FundingCase -> BS.ByteString
message funding = either (error . T.unpack) id $ decodeHex $ buildAcrossDestinationMessage deployment (QuoteRequest beneficiary owner 1 sourceToken "1000000") (quoteId funding)
sourceReceipt,successReceipt,fallbackReceipt :: FundingCase -> TxReceipt
sourceReceipt funding = TxReceipt (sourceTx funding) 10 sourceBlockHash 0 True [sourceLog funding]
successReceipt funding = TxReceipt (destinationTx funding) 10 destinationBlockHash 0 True
  [fillLog funding,transfer funding 1 handler clearinghouse,canonical funding 2,credit funding 3,marker funding 4]
fallbackReceipt funding = TxReceipt (destinationTx funding) 10 destinationBlockHash 0 True
  [fillLog funding,entry funding 1 handler [keccak256 "CallsFailed((address,bytes,uint256)[],address)",encodeAddress beneficiary] $ encodeUint256 32 <> BS.drop 96 (message funding)
  ,transfer funding 2 handler beneficiary
  ,entry funding 3 handler [keccak256 "DrainedTokens(address,address,uint256)",encodeAddress beneficiary,encodeAddress token,encodeUint256 1000000] BS.empty]
entry :: FundingCase -> Integer -> Text -> [BS.ByteString] -> BS.ByteString -> RpcLog
entry funding index address topics bytes = RpcLog (destinationTx funding) 10 destinationBlockHash 0 index address topics bytes
sourceLog,fillLog :: FundingCase -> RpcLog
sourceLog funding = RpcLog (sourceTx funding) 10 sourceBlockHash 0 0 originSpokePool
  [fundsDepositedTopic,encodeUint256 destinationChain,encodeUint256 $ depositId funding,encodeAddress owner] $ BS.concat
  [encodeAddress sourceToken,encodeAddress token,encodeUint256 1000000,encodeUint256 1000000,encodeUint256 90
  ,encodeUint256 1000,encodeUint256 0,encodeAddress handler,BS.replicate 32 0,encodeUint256 320,dynamic $ message funding]
fillLog funding = entry funding 0 spoke [filledRelayTopic,encodeUint256 1,encodeUint256 $ depositId funding,encodeAddress owner] $ BS.concat
  [encodeAddress sourceToken,encodeAddress token,encodeUint256 1000000,encodeUint256 1000000,encodeUint256 1
  ,encodeUint256 1000,encodeUint256 0,BS.replicate 32 0,encodeAddress owner,encodeAddress handler,keccak256 $ message funding
  ,encodeAddress handler,keccak256 $ message funding,encodeUint256 1000000,encodeUint256 0]
transfer :: FundingCase -> Integer -> Text -> Text -> RpcLog
transfer funding index from to = entry funding index token [keccak256 "Transfer(address,address,uint256)",encodeAddress from,encodeAddress to] $ encodeUint256 1000000
canonical,credit,marker :: FundingCase -> Integer -> RpcLog
canonical funding index = entry funding index clearinghouse [keccak256 "Deposit(address,address,uint256)",encodeAddress beneficiary,encodeAddress token] $ encodeUint256 1000000
credit funding index = entry funding index clearinghouse [depositForTopic,encodeAddress handler,encodeAddress beneficiary] $ encodeUint256 1000000
marker funding index = entry funding index emitter [keccak256 "MetadataEmitted(bytes)"] $ encodeUint256 32 <> dynamic (either (error . T.unpack) id $ decodeHex $ quoteId funding)
dynamic :: BS.ByteString -> BS.ByteString
dynamic bytes = encodeUint256 (fromIntegral $ BS.length bytes) <> bytes <> BS.replicate ((32-BS.length bytes `mod` 32) `mod` 32) 0
receiptJSON :: TxReceipt -> Value
receiptJSON receipt = object ["transactionHash" .= receiptTxHash receipt,"blockNumber" .= quantity (receiptBlockNumber receipt)
  ,"blockHash" .= receiptBlockHash receipt,"transactionIndex" .= quantity (receiptTransactionIndex receipt)
  ,"status" .= (if receiptSucceeded receipt then "0x1" else "0x0" :: Text),"logs" .= map logJSON (receiptLogs receipt)]
logJSON :: RpcLog -> Value
logJSON entryLog = object ["transactionHash" .= rpcLogTxHash entryLog,"blockNumber" .= quantity (rpcLogBlockNumber entryLog)
  ,"blockHash" .= rpcLogBlockHash entryLog,"transactionIndex" .= quantity (rpcLogTransactionIndex entryLog),"logIndex" .= quantity (rpcLogIndex entryLog)
  ,"address" .= rpcLogAddress entryLog,"topics" .= map encodeHex (rpcLogTopics entryLog),"data" .= encodeHex (rpcLogData entryLog)]
replaceWord :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
replaceWord index replacement bytes = BS.take (index*32) bytes <> replacement <> BS.drop ((index+1)*32) bytes
quantity :: Integer -> Text
quantity number = "0x" <> intToHex number
identifier :: Char -> Text
identifier character = "0x" <> T.replicate 64 (T.singleton character)
beneficiary,owner,handler,token,clearinghouse,spoke,implementation,emitter,sourceToken,sourceBlockHash,destinationBlockHash,otherHash :: Text
beneficiary="0x1111111111111111111111111111111111111111"
owner="0x2222222222222222222222222222222222222222"
handler="0x0f7ae28de1c8532170ad4ee566b5801485c13a0e"
token="0xaf88d065e77c8cc2239327c5edb3a432268e5831"
clearinghouse="0x4444444444444444444444444444444444444444"
spoke="0x5555555555555555555555555555555555555555"
implementation="0x6666666666666666666666666666666666666666"
emitter="0xbf75133b48b0a42ab9374027902e83c5e2949034"
sourceToken="0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48"
sourceBlockHash=identifier 'a'
destinationBlockHash=identifier 'b'
otherHash=identifier 'f'
destinationChain,expiry :: Integer
destinationChain=42161
expiry=4102444800
deployment :: FundingDeployment
deployment=FundingDeployment destinationChain "observer-test-release" clearinghouse token spoke (hashCode spoke)
  implementation (hashCode implementation) handler (hashCode handler) 2 10 (hashCode clearinghouse)
  where hashCode address = encodeHex $ keccak256 $ fromMaybe (error "Missing runtime fixture") $ lookup address runtimeCodes
runtimeCodes :: [(Text,BS.ByteString)]
runtimeCodes = [(clearinghouse,BS.pack [0x60,1]),(spoke,BS.pack [0x60,2]),(implementation,BS.pack [0x60,3]),(handler,BS.pack [0x60,4]),(emitter,loggerRuntime)]
-- Public, verified Across metadata-emitter runtime on Arbitrum. Its fixed code
-- hash is a production trust pin; embedding bytes keeps the test offline.
loggerRuntime :: BS.ByteString
loggerRuntime = either (error . T.unpack) id $ decodeHex "0x60808060405260043610156011575f80fd5b5f3560e01c63d836083e146023575f80fd5b3460f45760207ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc36011260f4576004359067ffffffffffffffff80831160f4573660238401121560f457826004013590811160f457366024828501011160f457801560f457817fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe0601f8360409460247fc28009f405f9b451f5155492167b1ad5ab376d991bea880cb5049e924e5b823c986020875282602088015201868601375f85828601015201168101030190a1005b5f80fdfea2646970667358221220a2c7a0f09e981d67663b05d824037d0d54404d85e8c1debac93ab835ff1790aa64736f6c63430008170033"
