{-# LANGUAGE OverloadedStrings #-}

module Plether.Perps.Funding.WorkerSpec (fundingWorkerSpec) where

import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Aeson (Value (..), eitherDecode, encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Only (..), execute_, query_)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.AA.Kms (PaymasterSigner (..))
import Plether.Database (DbPool, destroyDbPool, newDbPool, withDb)
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256, keccak256)
import Plether.Ethereum.Client (EthClient, RpcClientOptions (..), newClientWithManager)
import Plether.Ethereum.Transaction (SignedTransaction (..), Tx1559 (..), signTransaction)
import Plether.Perps.Funding.Chain (decodeHex, encodeHex)
import Plether.Perps.Funding.Store
import Plether.Perps.Funding.Types
import Plether.Perps.Funding.Worker (reconcileFundingOnce, verifyFundingSigner)
import Test.Hspec hiding (pending)

fundingWorkerSpec :: Text -> Spec
fundingWorkerSpec databaseUrl = around (withFixture databaseUrl) $ describe "bridge funding worker recovery" $ do
  it "observes a persisted signed transaction without broadcasting when no signer is configured" $ \(pool, client, rpc) -> do
    pending <- seedPending pool flushTransaction
    reconcileFundingOnce client pool deployment Nothing gasBudget `shouldReturn` Right ()
    readIORef (sentSnapshots rpc) `shouldReturn` []
    latest <- withDb pool $ \conn -> getIntent conn intentId
    fmap (fieldText "signedRawTransaction") latest `shouldBe` Just (fieldText "signedRawTransaction" pending)
    withDb pool (\conn -> isWorkerReady conn (deploymentReadinessKey deployment) destinationChain) `shouldReturn` False
    calls <- readIORef $ observedMethods rpc
    calls `shouldContain` ["eth_getTransactionReceipt"]

  it "rejects a corrupted persisted transaction hash before any broadcast" $ \(pool, client, rpc) -> do
    pending <- seedPending pool flushTransaction
    let corrupted = setFields [("transactionHash",String $ identifier 'f')] pending
    withDb pool $ \conn -> updateIntent conn intentId corrupted
    reconcileFundingOnce client pool deployment (Just readySigner) gasBudget
      `shouldReturn` Left "PERSISTED_TRANSACTION_HASH_MISMATCH"
    readIORef (sentSnapshots rpc) `shouldReturn` []
    withDb pool (\conn -> isWorkerReady conn (deploymentReadinessKey deployment) destinationChain) `shouldReturn` False
    latest <- withDb pool $ \conn -> getIntent conn intentId
    fmap (fieldText "signedRawTransaction") latest `shouldBe` Just (fieldText "signedRawTransaction" pending)

  it "rejects a validly signed transaction with the wrong protocol target" $ \(pool, client, rpc) -> do
    pending <- seedPending pool (flushTransaction {txTo = token})
    reconcileFundingOnce client pool deployment (Just readySigner) gasBudget
      `shouldReturn` Left "PERSISTED_TRANSACTION_BINDING_MISMATCH"
    readIORef (sentSnapshots rpc) `shouldReturn` []
    latest <- withDb pool $ \conn -> getIntent conn intentId
    fmap (fieldText "signedRawTransaction") latest `shouldBe` Just (fieldText "signedRawTransaction" pending)

  it "preserves uncertain broadcasts and only retries the same durable bytes after restart" $ \(pool, client, rpc) -> do
    verifyFundingSigner readySigner `shouldReturn` Right ()
    pending <- seedPending pool flushTransaction
    let raw = fieldText "signedRawTransaction" pending
    reconcileFundingOnce client pool deployment (Just readySigner) gasBudget `shouldReturn` Left "DESTINATION_BROADCAST_REJECTED"
    withDb pool (\conn -> isWorkerReady conn (deploymentReadinessKey deployment) destinationChain) `shouldReturn` False
    -- A separate pool represents a process restart. RPC deliberately rejects
    -- the send response without reporting a receipt; the outcome is unknown.
    bracket (newDbPool $ scopedDatabaseUrl databaseUrl) destroyDbPool $ \restarted ->
      reconcileFundingOnce client restarted deployment (Just readySigner) gasBudget `shouldReturn` Left "DESTINATION_BROADCAST_REJECTED"
    snapshots <- readIORef $ sentSnapshots rpc
    map (Just . fst) snapshots `shouldBe` [raw,raw]
    -- PostgreSQL is read at the exact send boundary: identity and raw bytes
    -- must already be durable before either network submission.
    map (fmap (fieldText "signedRawTransaction") . snd) snapshots `shouldBe` [Just raw,Just raw]
    map (fmap (fieldText "transactionHash") . snd) snapshots
      `shouldBe` replicate 2 (Just $ fieldText "transactionHash" pending)
    latest <- withDb pool $ \conn -> getIntent conn intentId
    fmap (fieldText "signedRawTransaction") latest `shouldBe` Just raw
    calls <- readIORef $ observedMethods rpc
    calls `shouldNotContain` ["eth_getTransactionCount"]
    calls `shouldNotContain` ["eth_estimateGas"]

  it "disables new quotes on nonce rejection and restores readiness only after the same transaction is accepted" $ \(pool, client, rpc) -> do
    pending <- seedPending pool flushTransaction
    let raw = fieldText "signedRawTransaction" pending
        expectedHash = maybe (error "missing test transaction hash") id $ fieldText "transactionHash" pending
    withDb pool $ \conn -> setWorkerReadiness conn (deploymentReadinessKey deployment) destinationChain True
    writeIORef (sendResponse rpc) $ Left "nonce too low"
    reconcileFundingOnce client pool deployment (Just readySigner) gasBudget
      `shouldReturn` Left "DESTINATION_BROADCAST_REJECTED"
    withDb pool (\conn -> isWorkerReady conn (deploymentReadinessKey deployment) destinationChain) `shouldReturn` False
    writeIORef (sendResponse rpc) $ Right expectedHash
    reconcileFundingOnce client pool deployment (Just readySigner) gasBudget `shouldReturn` Right ()
    withDb pool (\conn -> isWorkerReady conn (deploymentReadinessKey deployment) destinationChain) `shouldReturn` True
    snapshots <- readIORef $ sentSnapshots rpc
    map (Just . fst) snapshots `shouldBe` [raw,raw]
    calls <- readIORef $ observedMethods rpc
    calls `shouldNotContain` ["eth_getTransactionCount"]
    calls `shouldNotContain` ["eth_estimateGas"]

  it "rejects an RPC acknowledgement for another transaction without losing the durable bytes" $ \(pool, client, rpc) -> do
    pending <- seedPending pool flushTransaction
    writeIORef (sendResponse rpc) $ Right $ identifier 'f'
    reconcileFundingOnce client pool deployment (Just readySigner) gasBudget
      `shouldReturn` Left "DESTINATION_BROADCAST_HASH_MISMATCH"
    withDb pool (\conn -> isWorkerReady conn (deploymentReadinessKey deployment) destinationChain) `shouldReturn` False
    latest <- withDb pool $ \conn -> getIntent conn intentId
    fmap (fieldText "signedRawTransaction") latest `shouldBe` Just (fieldText "signedRawTransaction" pending)

  it "withdraws readiness when a configured signer cannot sign the attestation" $ \(pool, client, rpc) -> do
    withDb pool $ \conn -> setWorkerReadiness conn (deploymentReadinessKey deployment) destinationChain True
    let deniedSigner = PaymasterSigner signerAddress (const $ pure $ Left "KMS access denied")
    reconcileFundingOnce client pool deployment (Just deniedSigner) gasBudget
      `shouldReturn` Left "DESTINATION_SIGNER_UNAVAILABLE"
    withDb pool (\conn -> isWorkerReady conn (deploymentReadinessKey deployment) destinationChain) `shouldReturn` False
    readIORef (sentSnapshots rpc) `shouldReturn` []

-- Private schema and a real pool. URI/libpq connection strings both propagate
-- search_path to every connection, including the restarted pool.
withFixture :: Text -> ((DbPool, EthClient, RpcFixture) -> IO ()) -> IO ()
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
    rpc <- RpcFixture <$> newIORef [] <*> newIORef [] <*> newIORef []
      <*> newIORef (Left "uncertain broadcast: upstream temporarily unavailable")
    testWithApplication (pure $ rpcApplication pool rpc) $ \port -> do
      manager <- newManager defaultManagerSettings
      requestId <- newIORef 1
      client <- newClientWithManager manager requestId $ RpcClientOptions
        ("http://127.0.0.1:" <> T.pack (show port)) Nothing "funding-worker-test"
      action (pool, client, rpc)
      readIORef (unexpectedRequests rpc) `shouldReturn` []) `finally` cleanup

scopedDatabaseUrl :: Text -> Text
scopedDatabaseUrl url
  | "postgres://" `T.isPrefixOf` url || "postgresql://" `T.isPrefixOf` url =
      url <> (if "?" `T.isInfixOf` url then "&" else "?") <> "options=-c%20search_path%3Dperps_funding_worker_spec"
  | otherwise = url <> " options='-c search_path=perps_funding_worker_spec'"

data RpcFixture = RpcFixture
  { sentSnapshots :: IORef [(Text, Maybe Value)]
  , observedMethods :: IORef [Text]
  , unexpectedRequests :: IORef [Value]
  , sendResponse :: IORef (Either Text Text)
  }

rpcApplication :: DbPool -> RpcFixture -> Application
rpcApplication pool fixture request respond = do
  requestBody <- strictRequestBody request
  let envelope = either (const Null) id $ eitherDecode requestBody
      requestId = case envelope of
        Object fields -> maybe Null id $ KM.lookup "id" fields
        _ -> Null
      method = fieldText "method" envelope
      params = case envelope of
        Object fields -> case KM.lookup "params" fields of
          Just (Array xs) -> toList xs
          _ -> []
        _ -> []
  result <- case method of
    Just name -> do
      atomicModifyIORef' (observedMethods fixture) $ \items -> (items <> [name], ())
      case (name,params) of
        ("eth_chainId",[]) -> pure $ Right $ String "0xa4b1"
        ("eth_getCode",[String target,String _]) | target == factory -> pure $ Right $ String $ encodeHex factoryCode
        ("eth_getCode",[String target,String _]) | target == clearinghouse -> pure $ Right $ String $ encodeHex clearinghouseCode
        ("eth_getCode",[String target,String _]) | target == receiver -> pure $ Right $ String $ encodeHex factoryCode
        ("eth_call",[Object call,String _]) -> case (KM.lookup "to" call,KM.lookup "data" call) of
          (Just (String target),Just (String input)) -> case getterResult target input of
            Just resultValue -> pure $ Right $ String $ encodeHex resultValue
            Nothing -> unexpected envelope
          _ -> unexpected envelope
        ("eth_getBalance",[String target,String _]) | target == signerAddress -> pure $ Right $ String "0xde0b6b3a7640000"
        ("eth_blockNumber",[]) -> pure $ Right $ String "0xa"
        ("eth_getBlockByNumber",[String blockNumber,Bool False]) -> pure $ Right $ object
          ["number" .= blockNumber,"hash" .= identifier 'a',"timestamp" .= ("0x6553f100" :: Text)]
        ("eth_getLogs",[_]) -> pure $ Right $ toJSON ([] :: [Value])
        ("eth_getTransactionReceipt",[String _]) -> pure $ Right Null
        ("eth_sendRawTransaction",[String raw]) -> do
          snapshot <- withDb pool getActiveTransaction
          atomicModifyIORef' (sentSnapshots fixture) $ \items -> (items <> [(raw,snapshot)], ())
          outcome <- readIORef $ sendResponse fixture
          pure $ either (\message -> Left $ object ["code" .= (-32000 :: Int),"message" .= message]) (Right . String) outcome
        _ -> unexpected envelope
    Nothing -> unexpected envelope
  let response = object (["jsonrpc" .= ("2.0" :: Text),"id" .= requestId] <> either (\err -> ["error" .= err]) (\value -> ["result" .= value]) result)
  respond $ responseLBS status200 [("Content-Type","application/json")] $ encode response
  where
    unexpected value = do
      atomicModifyIORef' (unexpectedRequests fixture) $ \items -> (items <> [value], ())
      pure $ Left $ object ["code" .= (-32601 :: Int),"message" .= ("Unexpected funding test RPC" :: Text)]

getterResult :: Text -> Text -> Maybe BS.ByteString
getterResult target input
  | target == factory && input == calldata "clearinghouse()" = Just $ encodeAddress clearinghouse
  | target == factory && input == calldata "usdc()" = Just $ encodeAddress token
  | target == clearinghouse && input == calldata "settlementAsset()" = Just $ encodeAddress token
  | target == receiver && input == calldata "beneficiary()" = Just $ encodeAddress beneficiary
  | target == receiver && input == calldata "clearinghouse()" = Just $ encodeAddress clearinghouse
  | target == receiver && input == calldata "usdc()" = Just $ encodeAddress token
  | target == factory && T.isPrefixOf (calldata "predictReceiver(address,bytes32)") input = Just $ encodeAddress receiver
  | target == token && T.isPrefixOf (calldata "balanceOf(address)") input = Just $ encodeUint256 0
  | otherwise = Nothing
  where calldata signature = encodeHex $ encodeCall signature []

seedPending :: DbPool -> Tx1559 -> IO Value
seedPending pool transaction = do
  signed <- signTransaction testPrivateKey transaction >>= either (fail . T.unpack) pure
  withDb pool $ \conn -> do
    insertQuote conn quoteId quote expiry
    let initial = intentFromQuote intentId quote
    createIntent conn "worker-test-000001" intentId quoteId initial `shouldReturn` Right initial
    let pending = setFields
          [("status",String "depositing"),("signedRawTransaction",String $ encodeHex $ signedRawTransaction signed)
          ,("transactionHash",String $ signedTransactionHash signed),("transactionKind",String "flush")
          ,("sender",String $ signedFrom signed),("nonce",String "4")] initial
    updateIntent conn intentId pending
    pure pending

-- Public deterministic test key 1, never a deployment credential. This fixed
-- signature authenticates only the application's readiness challenge.
readySigner :: PaymasterSigner
readySigner = PaymasterSigner signerAddress $ \digest -> pure $
  if digest == keccak256 "Plether bridge funding executor readiness v1"
    then decodeHex "0xe0c50cbe73e5f76f32f8446b0881f1802d43c30f5c261175a9cfd36456d7de6703db049b09103f2820836503d12b48a3b533f603fa22f599e5cba464b0b0cef71c"
    else Left "Unexpected signing request; recovery must reuse the persisted transaction"

testPrivateKey, signerAddress, factory, clearinghouse, receiver, beneficiary, token, releaseId, quoteId, intentId :: Text
testPrivateKey = "0x" <> T.replicate 63 "0" <> "1"
signerAddress = "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"
factory = "0x1111111111111111111111111111111111111111"
clearinghouse = "0x2222222222222222222222222222222222222222"
receiver = "0x3333333333333333333333333333333333333333"
beneficiary = "0x4444444444444444444444444444444444444444"
token = "0x5555555555555555555555555555555555555555"
releaseId = "funding-worker-test-release"
quoteId = identifier '1'
intentId = identifier '2'

identifier :: Char -> Text
identifier character = "0x" <> T.replicate 64 (T.singleton character)

factoryCode, clearinghouseCode :: BS.ByteString
factoryCode = BS.pack [0x60,0x01,0x60,0x00]
clearinghouseCode = BS.pack [0x60,0x02,0x60,0x00]

destinationChain, expiry, gasBudget :: Integer
destinationChain = 42161
expiry = 4102444800
gasBudget = 1000000

deployment :: FundingDeployment
deployment = FundingDeployment destinationChain releaseId clearinghouse token factory (encodeHex $ keccak256 factoryCode) 3 0 (encodeHex $ keccak256 clearinghouseCode)

flushTransaction :: Tx1559
flushTransaction = Tx1559 destinationChain 4 1 2 21000 receiver 0 (encodeCall "flush()" [])

quote :: Value
quote = object
  ["quoteId" .= quoteId,"expiresAt" .= expiry,"destinationChainId" .= destinationChain
  ,"releaseId" .= releaseId,"clearinghouse" .= clearinghouse,"token" .= token,"receiverFactory" .= factory
  ,"receiver" .= receiver,"beneficiary" .= beneficiary,"ownerAddress" .= signerAddress,"intentSalt" .= quoteId
  ,"minimumAmount" .= ("1000000" :: Text),"estimatedAmount" .= ("1000000" :: Text)
  ,"sourceChainId" .= (1 :: Integer),"sourceToken" .= token,"sourceAmount" .= ("1000000" :: Text)
  ,"provider" .= ("test" :: Text),"sourceTransactions" .= ([] :: [Value])]
