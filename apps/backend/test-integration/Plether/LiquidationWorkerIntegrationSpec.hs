module Plether.LiquidationWorkerIntegrationSpec (liquidationWorkerSpec) where

import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import Data.Aeson (Value(..), decode, encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (Key)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Foldable (toList)
import Data.IORef
import Data.List (sort)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Unique (hashUnique, newUnique)
import GHC.Clock (getMonotonicTimeNSec)
import Database.PostgreSQL.Simple
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Numeric (showHex)
import Plether.Database.Schema
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall, encodeUint256)
import Plether.Ethereum.Client (EthClient, newClient)
import qualified Plether.Ethereum.Contracts.Perps as Perps
import Plether.Ethereum.Transaction (Tx1559(..), decodeSignedTransaction, rawTransactionHash)
import Plether.LiquidationWorker
import System.Timeout (timeout)
import Test.Hspec

-- Real processCandidates owns DB membership, snapshot classification, payload
-- preparation, fee quotation, signing, persistence, broadcast and reconciliation.
-- This fixture replaces only the external RPC/chain and advances its clock/cache.
liquidationWorkerSpec :: Text -> Spec
liquidationWorkerSpec databaseUrl = describe "liquidation worker DB/RPC orchestration" $ do
  it "refreshes after a real future-price retry and requotes each submitted batch" $
    withFixture databaseUrl 2 RefreshPayload $ \conn fixture client -> do
      runWorker cfg conn client
      readIORef (fSnapshots fixture) `shouldReturn` accounts 2
      readIORef (fQuotes fixture) `shouldReturn`
        [(quantityText 103, BS.singleton 0x01), (quantityText 112, BS.singleton 0xaa), (quantityText 124, BS.singleton 0xbb)]
      readIORef (fOraclePreviews fixture) `shouldReturn`
        [ (quantityText 100, [], 0)
        , (quantityText 103, [BS.singleton 0x01], 7)
        , (quantityText 112, [BS.singleton 0xaa], 11)
        , (quantityText 124, [BS.singleton 0xbb], 19)
        ]
      estimates <- readIORef (fEstimates fixture)
      map (\(_,fee,_) -> fee) estimates `shouldBe` [11,19]
      map (\(batch,_,_) -> batch) estimates `shouldBe` [[account 1],[account 2]]
      map (\(batch,_,bytes) -> bytes == Perps.executeLiquidationBatchCall batch [BS.singleton (if batch == [account 1] then 0xaa else 0xbb)]) estimates
        `shouldBe` [True,True]
      readIORef (fRiskTimes fixture) `shouldReturn` [100,103]
      assertSignedIntents fixture
      remaining conn `shouldReturn` 0

  it "screens and submits using canonical accepted-basket prices when a signed payload is rejected" $
    withFixture databaseUrl 1 ReplayAcceptedBasket $ \conn fixture client -> do
      runWorker cfg conn client
      readIORef (fSnapshots fixture) `shouldReturn` accounts 1
      readIORef (fOraclePreviews fixture) `shouldReturn`
        [ (quantityText 110, [BS.singleton 0x01], 7)
        , (quantityText 110, [], 0)
        , (quantityText 110, [BS.singleton 0x01], 7)
        , (quantityText 110, [], 0)
        ]
      readIORef (fEstimates fixture) `shouldReturn`
        [(accounts 1, 0, Perps.executeLiquidationBatchCall (accounts 1) [])]
      readIORef (fBatchSimulations fixture) `shouldReturn`
        [(quantityText 110, Perps.executeLiquidationBatchCall (accounts 1) [])]
      assertSignedIntents fixture
      remaining conn `shouldReturn` 0

  it "recovers an out-of-order page at a newer block using the refreshed admitted payload" $
    withFixture databaseUrl 1 RetryOracleOrdering $ \conn fixture client -> do
      runWorker cfg conn client
      readIORef (fSnapshots fixture) `shouldReturn` accounts 1
      readIORef (fOraclePreviews fixture) `shouldReturn`
        [ (quantityText 110, [BS.singleton 0x01], 7)
        , (quantityText 110, [], 0)
        , (quantityText 112, [BS.singleton 0xaa], 11)
        , (quantityText 112, [BS.singleton 0xaa], 11)
        ]
      readIORef (fBatchSimulations fixture) `shouldReturn`
        [(quantityText 112, Perps.executeLiquidationBatchCall (accounts 1) [BS.singleton 0xaa])]
      (length <$> readIORef (fSent fixture)) `shouldReturn` 1
      assertSignedIntents fixture
      remaining conn `shouldReturn` 0

  it "keeps an exhausted page unknown and continues to the next page" $
    withFixture databaseUrl 2 ExhaustOracleOrdering $ \conn fixture client -> do
      runWorker (cfg {lwcScanBatchSize=1}) conn client
      readIORef (fSnapshots fixture) `shouldReturn` accounts 2
      readIORef (fOraclePreviews fixture) `shouldReturn`
        [ (quantityText 110, [BS.singleton 0x01], 7)
        , (quantityText 110, [], 0)
        , (quantityText 112, [BS.singleton 0xaa], 11)
        , (quantityText 112, [BS.singleton 0xaa], 11)
        ]
      readIORef (fBatchSimulations fixture) `shouldReturn`
        [(quantityText 112, Perps.executeLiquidationBatchCall [account 2] [BS.singleton 0xaa])]
      first <- query_ conn "SELECT account,risk_checked_at IS NULL,last_error FROM perps_liquidation_candidates"
        :: IO [(Text,Bool,Maybe Text)]
      first `shouldBe` [(account 1,True,Just "Fresh liquidation risk inputs were unavailable")]
      assertSignedIntents fixture
      pendingRows conn `shouldReturn` []

  it "withholds liquidation screening when the accepted basket exceeds the local age limit" $
    withFixture databaseUrl 1 ReplayStaleBasket $ \conn fixture client -> do
      runWorker cfg conn client
      readIORef (fSnapshots fixture) `shouldReturn` accounts 1
      readIORef (fOraclePreviews fixture) `shouldReturn`
        [(quantityText 110, [BS.singleton 0x01], 7), (quantityText 110, [], 0)]
      readIORef (fEstimates fixture) `shouldReturn` []
      readIORef (fBatchSimulations fixture) `shouldReturn` []
      readIORef (fSent fixture) `shouldReturn` []
      errors <- query_ conn "SELECT last_error FROM perps_liquidation_candidates" :: IO [Only (Maybe Text)]
      errors `shouldBe` [Only (Just "Fresh liquidation risk inputs were unavailable")]

  it "retains the exact persisted transaction on confirmation failure and recovers before another nonce" $
    withFixture databaseUrl 2 SignedPayload $ \conn fixture client -> do
      writeIORef (fFailConfirmation fixture) True
      runWorker cfg conn client
      first <- readIORef (fSent fixture)
      length first `shouldBe` 1
      stored <- pendingRows conn
      map (\(_,hash,raw,nonce) -> (hash,raw,nonce)) stored
        `shouldBe` [(rawTransactionHash $ head first, hex $ head first, 0)]
      readIORef (fSnapshots fixture) `shouldReturn` accounts 2
      -- Restart the real worker entry point with the same database. It must
      -- reconcile the persisted operation, without preparing a new transaction.
      writeIORef (fFailConfirmation fixture) False
      runWorker cfg conn client
      readIORef (fSent fixture) `shouldReturn` first
      pendingRows conn `shouldReturn` []
      remaining conn `shouldReturn` 1
      -- A subsequent sweep may now use the next nonce for the other account.
      runWorker cfg conn client
      sends <- readIORef (fSent fixture)
      length sends `shouldBe` 2
      assertSignedIntents fixture
      remaining conn `shouldReturn` 0

  it "loads all 8000 stored accounts and submits the 250 risks across every page" $
    withFixture databaseUrl 8000 SignedPayload $ \conn fixture client -> do
      runWorker (cfg {lwcExecutionBatchSize=20}) conn client
      seen <- readIORef (fSnapshots fixture)
      length seen `shouldBe` 8000
      sort seen `shouldBe` accounts 8000
      estimates <- readIORef (fEstimates fixture)
      length estimates `shouldBe` 16
      concatMap (\(batch,_,_) -> batch) estimates `shouldBe` [account n | n <- [32,64..8000]]
      assertSignedIntents fixture
      remaining conn `shouldReturn` 7750
      checked <- query_ conn "SELECT count(*) FROM perps_liquidation_candidates WHERE last_checked_at IS NOT NULL" :: IO [Only Int]
      checked `shouldBe` [Only 7750]
      pendingRows conn `shouldReturn` []

runWorker :: LiquidationWorkerConfig -> Connection -> EthClient -> IO ()
runWorker config conn client = do
  result <- timeout 120_000_000 $ processCandidates config conn client signer False
  result `shouldBe` Just ()

cfg :: LiquidationWorkerConfig
cfg = LiquidationWorkerConfig
  { lwcChainId=421614, lwcOrderRouter=router, lwcPletherOracle=oracle,
    lwcCfdEngine=engine, lwcAccountLens=lens, lwcPrivateKey=privateKey,
    lwcPollSeconds=5, lwcScanBatchSize=1000, lwcMulticallSize=100,
    lwcExecutionBatchSize=1, lwcIndexerStartBlock=0, lwcIndexerConfirmations=0,
    lwcIndexerBatchSize=5000, lwcIndexerOverlapBlocks=12,
    lwcPendingReplacementSeconds=60, lwcGasBufferBps=0, lwcFeeBufferBps=0,
    lwcFuturePublishMaxRetries=2, lwcFuturePublishRetryMaxSeconds=10,
    lwcMaxTransactionGas=25000000, lwcPythLatestMaxAgeSeconds=10 }

router, oracle, engine, lens, signer, privateKey :: Text
router = "0x7777777777777777777777777777777777777777"
oracle = "0x8888888888888888888888888888888888888888"
engine = "0x3333333333333333333333333333333333333333"
lens = "0x9999999999999999999999999999999999999999"
signer = "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"
privateKey = "0x" <> T.replicate 63 "0" <> "1"
account :: Int -> Text
account n = "0x" <> T.justifyRight 40 '0' (T.pack $ showHex n "")
accounts :: Int -> [Text]
accounts count = map account [1..count]

-- Test-only external effects and captured requests, never a risk/selection loop.
data FixtureScenario = SignedPayload | RefreshPayload | ReplayAcceptedBasket | ReplayStaleBasket
  | RetryOracleOrdering | ExhaustOracleOrdering
  deriving (Eq)

data FixtureRpcFailure = RpcUnavailable Text | RpcRevert BS.ByteString

data Fixture = Fixture
  { fClock :: IORef Integer, fScenario :: FixtureScenario, fCount :: Int,
    fSnapshots :: IORef [Text], fQuotes :: IORef [(Text, BS.ByteString)],
    fOraclePreviews :: IORef [(Text, [BS.ByteString], Integer)],
    fEstimates :: IORef [([Text],Integer,BS.ByteString)],
    fBatchSimulations :: IORef [(Text, BS.ByteString)], fSent :: IORef [BS.ByteString],
    fReceipts :: IORef [(Text,Value)], fRiskTimes :: IORef [Integer],
    fFailConfirmation :: IORef Bool, fDb :: Connection }

withFixture :: Text -> Int -> FixtureScenario -> (Connection -> Fixture -> EthClient -> IO ()) -> IO ()
withFixture databaseUrl count scenario action = bracket open closeAll $ \(conn,rpcConn,schema) -> do
  ensurePerpsLiquidationSchema conn
  ensureBasketSnapshotSchema conn
  void $ execute conn
    "INSERT INTO perps_liquidation_candidates(chain_id,cfd_engine,account,first_seen_block,last_seen_block) SELECT ?,?, '0x'||lpad(to_hex(n),40,'0'),n,n FROM generate_series(1,?) n"
    (421614::Integer,engine,count)
  seedPayload conn 102 0x01
  fixture <- Fixture <$> newIORef (if scenario == RefreshPayload then 100 else 110) <*> pure scenario <*> pure count
    <*> newIORef [] <*> newIORef []
    <*> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef []
    <*> newIORef [] <*> newIORef False <*> pure rpcConn
  testWithApplication (pure $ rpcApplication fixture) $ \port -> do
    client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
    action conn fixture client
 where
  open = do
    conn <- connectPostgreSQL $ TE.encodeUtf8 databaseUrl
    rpcConn <- connectPostgreSQL $ TE.encodeUtf8 databaseUrl
    unique <- hashUnique <$> newUnique
    stamp <- getMonotonicTimeNSec
    let schema = "ak_liquidation_" <> show stamp <> "_" <> show (abs unique)
    void $ execute_ conn $ fromString $ "CREATE SCHEMA " <> schema
    forM_ [conn,rpcConn] $ \c -> void $ execute_ c $ fromString $ "SET search_path TO " <> schema
    pure (conn,rpcConn,schema)
  closeAll (conn,rpcConn,schema) = do
    close rpcConn
    void $ execute_ conn $ fromString $ "DROP SCHEMA " <> schema <> " CASCADE"
    close conn

remaining :: Connection -> IO Int
remaining conn = do
  [Only count] <- query_ conn "SELECT count(*) FROM perps_liquidation_candidates"
  pure count
pendingRows :: Connection -> IO [(Text,Text,Text,Integer)]
pendingRows conn = query_ conn "SELECT account,pending_tx_hash,pending_raw_tx,pending_nonce FROM perps_liquidation_candidates WHERE pending_tx_hash IS NOT NULL ORDER BY account"

seedPayload :: Connection -> Integer -> Int -> IO ()
seedPayload conn timestamp byte = insertPairedPythUpdatePayload conn timestamp timestamp
  (toJSON [timestamp,timestamp]) (toJSON [hex $ BS.singleton $ fromIntegral byte]) timestamp
  "backend_hermes_latest_v2" 100000000 (toJSON
    [object ["feedId" .= hex (BS.replicate 32 feed), "price" .= (100000000::Integer),
      "rawPrice" .= (100000000::Integer), "confidence" .= (0::Integer), "exponent" .= (-8::Int),
      "publishTime" .= timestamp, "inverted" .= False, "weightBps" .= (5000::Integer),
      "basePrice" .= (100000000::Integer)] | feed <- [0x11,0x22]])

rpcApplication :: Fixture -> Application
rpcApplication fixture request respond = do
  body <- strictRequestBody request
  case decode body of
    Just (Object fields) -> do
      let method = textAt "method" fields
          params = case KM.lookup "params" fields of Just (Array xs) -> toList xs; _ -> []
      result <- dispatch fixture method params
      let reply = case result of
            Left (RpcUnavailable message) -> ["error" .= object ["code" .= (-32000::Int),"message" .= message]]
            Left (RpcRevert revertData) ->
              ["error" .= object ["code" .= (3::Int),"message" .= ("execution reverted"::Text),"data" .= hex revertData]]
            Right value -> ["result" .= value]
      respond $ responseLBS status200 [] $ encode $ object
        (["jsonrpc" .= ("2.0"::Text),"id" .= KM.lookup "id" fields] <> reply)
    _ -> expectationFailure "invalid RPC JSON" >> respond (responseLBS status200 [] "{}")

dispatch :: Fixture -> Text -> [Value] -> IO (Either FixtureRpcFailure Value)
dispatch fixture method params = do
  clock <- readIORef $ fClock fixture
  sent <- readIORef $ fSent fixture
  failConfirmation <- readIORef $ fFailConfirmation fixture
  case (method,params) of
    ("eth_blockNumber",_) | failConfirmation && not (null sent) -> pure $ Left $ RpcUnavailable "confirmation RPC unavailable"
                          | otherwise -> pure $ Right $ quantity clock
    ("eth_getTransactionCount",_) -> pure $ Right $ quantity $ fromIntegral $ length sent
    ("eth_gasPrice",_) -> pure $ Right $ quantity 100
    ("eth_maxPriorityFeePerGas",_) -> pure $ Right $ quantity 10
    ("eth_getBalance",_) -> pure $ Right $ quantity (10^(30::Int))
    ("eth_call",Object call:String block:_) -> do
      let bytes = unhex $ textAt "data" call
      if bytes == encodeCall "getAcceptedFeedPublishTimes()" []
        then do
          textAt "to" call `shouldBe` oracle
          block `shouldBe` quantityText 110
          let oldest = if fScenario fixture == ReplayStaleBasket then 99 else 110
          pure $ Right $ String $ hex $ words' [32, 2, oldest, 110]
        else if BS.take 4 bytes == BS.take 4 (encodeCall "aggregate3((address,bool,bytes)[])" [])
          then do
            values <- traverse (subcall fixture block) $ aggregateCalls bytes
            pure $ Right $ String $ hex $ encodeResults values
          else if BS.take 4 bytes == BS.take 4 (Perps.executeLiquidationBatchCall [] [])
            then do
              textAt "from" call `shouldBe` signer
              textAt "to" call `shouldBe` router
              modifyIORef' (fBatchSimulations fixture) (<> [(block, bytes)])
              pure $ Right $ String $ hex $ encodeUint256 $ fromIntegral $ length $ batchAccounts bytes
            else do
              let payloads = [] : [[BS.singleton b] | b <- [0x01,0xaa,0xbb]]
                  previewPayloads = filter ((bytes ==) . Perps.updateLiquidationBatchPriceCall signer) payloads
              case previewPayloads of
                [updates] -> previewOracle fixture call block updates
                _ ->
                  case [update | [update] <- payloads, bytes == Perps.getUpdateFeeCall [update]] of
                    [update] -> do
                      modifyIORef' (fQuotes fixture) (<> [(block, update)])
                      pure $ Right $ String $ hex $ encodeUint256 $ feeFor update
                    _ -> unexpected method params
    ("eth_estimateGas",Object call:_) -> do
      textAt "to" call `shouldBe` router
      let bytes = unhex $ textAt "data" call
          batch = batchAccounts bytes
          updates = case fScenario fixture of
            ReplayAcceptedBasket -> []
            ReplayStaleBasket -> []
            SignedPayload -> [BS.singleton 0x01]
            RefreshPayload -> [BS.singleton $ if clock <= 112 then 0xaa else 0xbb]
            RetryOracleOrdering -> [BS.singleton 0xaa]
            ExhaustOracleOrdering -> [BS.singleton 0xaa]
          fee = sum $ map feeFor updates
      textAt "value" call `shouldBe` quantityText fee
      bytes `shouldBe` Perps.executeLiquidationBatchCall batch updates
      modifyIORef' (fEstimates fixture) (<> [(batch,fee,bytes)])
      pure $ Right $ quantity 5000000
    ("eth_sendRawTransaction",[String raw]) -> do
      let bytes = unhex raw
          hash = rawTransactionHash bytes
      rows <- pendingRows $ fDb fixture
      -- The signed operation must already be durable when the network sees it.
      rows `shouldSatisfy` (not . null)
      forM_ rows $ \(_,storedHash,storedRaw,nonce) -> do
        storedHash `shouldBe` hash
        storedRaw `shouldBe` raw
        nonce `shouldBe` fromIntegral (length sent)
      estimates <- readIORef $ fEstimates fixture
      let (batch,_,_) = last estimates
      modifyIORef' (fSent fixture) (<> [bytes])
      modifyIORef' (fReceipts fixture) (<> [(hash,receiptValue hash clock batch)])
      pure $ Right $ String hash
    ("eth_getTransactionReceipt",[String hash]) -> do
      receipts <- readIORef $ fReceipts fixture
      when (fScenario fixture == RefreshPayload && length sent == 1 && clock == 112) $ do
        seedPayload (fDb fixture) 124 0xbb
        writeIORef (fClock fixture) 124
      pure $ Right $ maybe Null id $ lookup hash receipts
    _ -> unexpected method params
 where
  unexpected method' params' =
    expectationFailure ("unexpected RPC: " <> show (method',params')) >> pure (Left $ RpcUnavailable "unexpected RPC")

previewOracle :: Fixture -> KM.KeyMap Value -> Text -> [BS.ByteString] -> IO (Either FixtureRpcFailure Value)
previewOracle fixture call block updates = do
  let fee = sum $ map feeFor updates
  textAt "to" call `shouldBe` oracle
  textAt "from" call `shouldBe` router
  textAt "value" call `shouldBe` quantityText fee
  modifyIORef' (fOraclePreviews fixture) (<> [(block, updates, fee)])
  case fScenario fixture of
    RefreshPayload | block == quantityText 100 -> pure $ Left $ RpcUnavailable "accepted oracle basket unavailable"
    RetryOracleOrdering | block == quantityText 110 -> do
      when (null updates) $ do
        seedPayload (fDb fixture) 112 0xaa
        writeIORef (fClock fixture) 112
      pure $ Left $ RpcRevert outOfOrder
    ExhaustOracleOrdering | block == quantityText 110 -> pure $ Left $ RpcRevert outOfOrder
    scenario | scenario `elem` [ReplayAcceptedBasket, ReplayStaleBasket] ->
      if null updates
        then pure $ Right $ String $ hex $ words' [110000000, 90000000, 100000000, 110, 0]
        else pure $ Left $ RpcRevert outOfOrder
    _ -> do
      let publishTime = case updates of
            [update] | update == BS.singleton 0xaa -> 112
                     | update == BS.singleton 0xbb -> 124
            _ -> 102
      pure $ Right $ String $ hex $ words' [100000000, 100000000, 100000000, publishTime, fee]
  where
    outOfOrder = encodeCall "PletherOracle__PriceOutOfOrder(uint64,uint64)" [encodeUint256 102, encodeUint256 109]

subcall :: Fixture -> Text -> (Text,BS.ByteString) -> IO BS.ByteString
subcall fixture block (_,bytes)
  | starts "getAccountLedgerSnapshot(address)" = do
      let who = hex $ BS.drop 12 $ BS.take 32 $ BS.drop 4 bytes
          n = decodeUint256 $ BS.drop 4 bytes
          risky = fScenario fixture `notElem` [ReplayAcceptedBasket, ReplayStaleBasket]
            && (fCount fixture <= 2 || (n::Integer) `mod` 32 == 0)
      when (fScenario fixture == RetryOracleOrdering) $
        block `shouldBe` quantityText 112
      when (fScenario fixture == ExhaustOracleOrdering) $
        if who == account 1
          then do
            block `shouldBe` quantityText 110
            seedPayload (fDb fixture) 112 0xaa
            writeIORef (fClock fixture) 112
          else block `shouldBe` quantityText 112
      modifyIORef' (fSnapshots fixture) (<> [who])
      when (fScenario fixture == RefreshPayload) $ do
        seedPayload (fDb fixture) 112 0xaa
        writeIORef (fClock fixture) 112
      -- Stored flags are deliberately false. With accepted replay, equity is
      -- healthy at the mark and only the long-side quote makes it liquidatable.
      pure $ words' ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,equity risky,0,1,0,10^(20::Int),10000000,100000000,0,equity risky,0])
  | starts "positions(address)" = pure $ BS.replicate (7*32) 0
  | starts "getPriceUnsafe(bytes32)" = pure $ words' [100000000,0,2^(256::Int)-8,102]
  | starts "riskParams()" = pure $ words' [0,0,100,0,200,0,0,0,5000,0]
  | starts "CAP_PRICE()" = pure $ words' [200000000]
  | starts "adverseConfidenceMultiplierBps()" = pure $ words' [0]
  | starts "pyth()" = pure $ encodeAddress oracle
  | starts "isOracleFrozen()" = pure $ words' [0]
  | starts "liquidationStalenessLimit()" = pure $ words' [30]
  | starts "basketMaxConfidenceRatioBps()" = pure $ words' [10]
  | starts "fadMaxStaleness()" = pure $ words' [30]
  | starts "isFadWindow()" = pure $ words' [0]
  | starts "lastMarkTime()" = pure $ words' [100]
  | starts "getCurrentBlockGasLimit()" = pure $ words' [100000000]
  | starts "getCurrentBlockTimestamp()" = do
      now <- readIORef $ fClock fixture
      -- Only the risk observation has lastMarkTime; execution-policy calls use
      -- the updated clock after the snapshot/receipt boundary.
      when (now <= 103) $ modifyIORef' (fRiskTimes fixture) (<> [now])
      when (now == 100) $ writeIORef (fClock fixture) 103
      pure $ words' [now]
  | otherwise = expectationFailure ("unexpected Multicall subcall: " <> T.unpack (hex bytes)) >> pure BS.empty
 where
  starts signature = BS.take 4 bytes == BS.take 4 (encodeCall signature [])
  equity True = 500000
  equity False = 10000000

assertSignedIntents :: Fixture -> IO ()
assertSignedIntents fixture = do
  estimates <- readIORef $ fEstimates fixture
  sends <- readIORef $ fSent fixture
  length sends `shouldBe` length estimates
  forM_ (zip3 [0..] estimates sends) $ \(nonce,(_,fee,bytes),raw) -> do
    decoded <- decodeSignedTransaction raw
    decoded `shouldBe` Right (Tx1559 421614 nonce 10 100 5000000 router fee bytes, signer)

receiptValue :: Text -> Integer -> [Text] -> Value
receiptValue hash block batch = object
  ["transactionHash" .= hash,"blockNumber" .= quantityText block,"blockHash" .= blockHash,
   "transactionIndex" .= ("0x0"::Text),"status" .= ("0x1"::Text),"logs" .= concat
     [[entry (2*i) router [Perps.liquidationBatchItemTopic,encodeUint256 i,encodeAddress who] (words' [0,25,0]),
       entry (2*i+1) engine [Perps.positionLiquidatedTopic,encodeAddress who] (words' [0,1,100000000,25])]
       | (i,who) <- zip [0..] batch]]
 where
  blockHash = "0x" <> T.replicate 64 "a"
  entry index emitter topics bytes = object
    ["transactionHash" .= hash,"blockNumber" .= quantityText block,"blockHash" .= blockHash,
     "transactionIndex" .= ("0x0"::Text),"logIndex" .= quantityText index,
     "address" .= emitter,"topics" .= map hex topics,"data" .= hex bytes]

aggregateCalls :: BS.ByteString -> [(Text,BS.ByteString)]
aggregateCalls bytes =
  let payload = BS.drop 4 bytes
      count = fromIntegral $ word payload 32
      at i = 64 + fromIntegral (word payload (64+32*i))
  in [(hex $ BS.drop 12 $ BS.take 32 $ BS.drop (at i) payload,
       BS.take (fromIntegral $ word payload (at i+96)) $ BS.drop (at i+128) payload) | i <- [0..count-1]]
batchAccounts :: BS.ByteString -> [Text]
batchAccounts bytes =
  let payload = BS.drop 4 bytes; base = fromIntegral $ word payload 0; count = fromIntegral $ word payload base
  in [hex $ BS.drop 12 $ BS.take 32 $ BS.drop (base+32+32*i) payload | i <- [0..count-1]]
encodeResults :: [BS.ByteString] -> BS.ByteString
encodeResults results =
  let tuples = [words' [1,64,fromIntegral $ BS.length bytes] <> bytes <> BS.replicate ((32-BS.length bytes `mod` 32) `mod` 32) 0 | bytes <- results]
      offsets = init $ scanl (+) (fromIntegral $ 32*length tuples) (map (fromIntegral . BS.length) tuples)
  in words' [32,fromIntegral $ length tuples] <> words' offsets <> mconcat tuples
word :: BS.ByteString -> Int -> Integer
word bytes offset = decodeUint256 $ BS.take 32 $ BS.drop offset bytes
words' :: [Integer] -> BS.ByteString
words' = mconcat . map encodeUint256
feeFor :: BS.ByteString -> Integer
feeFor bytes | bytes == BS.singleton 0xaa = 11
             | bytes == BS.singleton 0xbb = 19
             | otherwise = 7
quantity :: Integer -> Value
quantity = String . quantityText
quantityText :: Integer -> Text
quantityText n = "0x" <> T.pack (showHex n "")
textAt :: Key -> KM.KeyMap Value -> Text
textAt key fields = case KM.lookup key fields of Just (String value) -> value; other -> error $ "missing text field: " <> show (key,other)
hex :: BS.ByteString -> Text
hex = ("0x" <>) . TE.decodeUtf8 . B16.encode
unhex :: Text -> BS.ByteString
unhex value = either error id $ B16.decode $ TE.encodeUtf8 $ T.drop 2 value
