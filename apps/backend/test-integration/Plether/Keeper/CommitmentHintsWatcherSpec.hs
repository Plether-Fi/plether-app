module Plether.Keeper.CommitmentHintsWatcherSpec (commitmentHintsWatcherSpec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad (void, forM_)
import Data.Aeson (Value(..), eitherDecode, eitherDecodeFileStrict', object, encode, (.=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Foldable (toList)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification (getNotification, Notification(..))
import Database.PostgreSQL.Simple.Types (Identifier(..))
import Network.HTTP.Types.Status (status200)
import Network.Wai (strictRequestBody, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Config (Config(..), NativeAaConfig(..), AaRpcMode(..))
import Plether.Database (newDbPool, destroyDbPool)
import Plether.Ethereum.Abi (encodeUint256, encodeAddress, keccak256)
import Plether.Ethereum.Client (newClient)
import qualified Plether.Ethereum.Contracts.Perps as Perps
import Plether.Keeper.CommitmentHints
import qualified Plether.Perps.Manifest as Manifest
import Plether.Perps.CriticalPathSpec (testConfig)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Timeout (timeout)
import Test.Hspec

-- This exercises the actual starter, submitted-journal recovery, Alto locator,
-- primary RPC decoding, canonical/pending-order validation, fenced publication
-- and committed PostgreSQL NOTIFY. No keeper order or financial state is made.
commitmentHintsWatcherSpec :: Text -> Spec
commitmentHintsWatcherSpec databaseUrl = describe "Running commitment hint watcher" $ do
  it "reads actual SDK single/batch ordinary/protected commitment encodings" $ do
    fixture <- sdkFixture
    forM_ (arrayAt "scenarios" fixture) $ \scenario ->
      expectedClientOrderId Manifest.orderRouterAddress (valueAt "operation" scenario)
        `shouldBe` Just (textAt "clientOrderId" scenario)
  forM_ [("active",False),("observe",False),("active",True)] $ \(mode,mismatched) ->
    it ("tracks an actual SDK batch from Alto to primary evidence; mode=" <> mode <> ", mismatched locator=" <> show mismatched) $
      withMode mode $ isolatedDatabase databaseUrl $ \conn scopedUrl -> do
        fixture <- sdkFixture
        let scenario = head [x | x <- arrayAt "scenarios" fixture, textAt "name" x == "ordinary-batch"]
            operation = valueAt "operation" scenario
            opHash = textAt "operationHash" scenario
            sender = textAt "sender" operation
            paymaster = textAt "paymaster" operation
            clientId = textAt "clientOrderId" scenario
            receipt = rawReceipt opHash sender paymaster clientId
            locator = object ["userOpHash" .= (if mismatched then otherHash else opHash),
              "sender" .= sender, "entryPoint" .= entryPoint, "paymaster" .= paymaster,
              "nonce" .= ("0x0"::Text), "actualGasUsed" .= ("0x64"::Text),
              "actualGasCost" .= ("0x64"::Text), "success" .= True, "logs" .= ([]::[Value]), "receipt" .= receipt]
        void $ execute conn "INSERT INTO aa_sponsorship_authorizations(digest,expected_user_operation_hash,sender,valid_until,state,operation) VALUES(?,?,?,extract(epoch FROM clock_timestamp())::bigint+120,'submitted',?)"
          (opHash,opHash,sender,operation)
        void $ execute conn "INSERT INTO aa_preparation_authorizations(chain_id,paymaster,sender,digest) VALUES(421614,?,?,?)" (paymaster,sender,opHash)
        void $ execute_ conn "LISTEN plether_perps_commitment_wakeup"
        calls <- newIORef ([]::[(Text,Value)])
        let application request respond = do
              bytes <- strictRequestBody request
              case eitherDecode bytes of
                Right (Object input) -> do
                  let method = maybe "" id $ KM.lookup "method" input >>= asText
                      params = maybe Null id $ KM.lookup "params" input
                  atomicModifyIORef' calls $ \xs -> ((method,params):xs,())
                  let result = case method of
                        "eth_getUserOperationReceipt" -> locator
                        "eth_chainId" -> String "0x66eee"
                        "eth_getTransactionReceipt" -> receipt
                        "eth_getBlockByNumber" -> case params of
                          Array ps | take 1 (toList ps) == [String "latest"] -> rawHeader 102 1001
                          _ -> rawHeader 100 1000
                        "eth_call" -> String $ hex $ BS.concat $ map encodeUint256 [20,0,0,100,1000000,100000000,1000,100,0,0,0]
                        _ -> Null
                  respond $ responseLBS status200 [] $ encode $ object ["jsonrpc" .= ("2.0"::Text), "id" .= KM.lookup "id" input, "result" .= result]
                _ -> respond $ responseLBS status200 [] "{}"
        testWithApplication (pure application) $ \port -> do
          let rpcUrl="http://127.0.0.1:"<>T.pack(show port)
              cfg=(testConfig scopedUrl rpcUrl){cfgPerpsChainId=421614,cfgPerpsOrderRouter=Manifest.orderRouterAddress,
                cfgPerpsOrderLifecycleBook=Just Manifest.orderLifecycleBookAddress,cfgNativeAaConfig=Just $ nativeConfig rpcUrl paymaster,
                cfgKeeperConfirmations=1}
          primary <- newClient rpcUrl
          bracket (newDbPool scopedUrl) destroyDbPool $ \pool ->
            bracket (startCommitmentHints cfg pool primary) (mapM_ stopCommitmentHints) $ \started -> case started of
              Nothing -> expectationFailure "The reviewed Sepolia watcher did not start"
              Just sink -> do
                -- Unknown browser/advisory hashes create no extra work. The
                -- valid operation is recovered from its existing journal.
                enqueueCommitmentHint (Just sink) otherHash
                enqueueCommitmentHint (Just sink) opHash
                located <- timeout 5_000_000 $ await $ any ((=="eth_getUserOperationReceipt").fst) <$> readIORef calls
                located `shouldBe` Just ()
                if mismatched then do
                  threadDelay 300_000
                  query_ conn "SELECT count(*)::bigint FROM perps_commitment_hints WHERE state='provisional'" `shouldReturn` [Only (0::Int64)]
                  seen <- readIORef calls
                  map fst seen `shouldNotContain` ["eth_getTransactionReceipt"]
                  timeout 100_000 (getNotification conn) `shouldReturn` Nothing
                else do
                  published <- timeout 5_000_000 $ await $ do
                    rows <- query_ conn "SELECT count(*)::bigint FROM perps_commitment_hints WHERE state='provisional'"
                    pure $ rows==[Only (1::Int64)]
                  published `shouldBe` Just ()
                  query_ conn "SELECT order_id,commit_event_block,commit_time FROM perps_commitment_hints"
                    `shouldReturn` [(20::Integer,100::Integer,1000::Integer)]
                  notification <- timeout (if mode=="active" then 2_000_000 else 100_000) $ getNotification conn
                  if mode=="active" then case notification of
                    Just value -> notificationData value `shouldBe` TE.encodeUtf8 Manifest.orderRouterAddress
                    Nothing -> expectationFailure "Verified active hint did not publish its committed wakeup"
                   else notification `shouldBe` Nothing
                  seen <- readIORef calls
                  seen `shouldSatisfy` any (\(method,params) -> method=="eth_call" && case params of
                    Array ps -> drop 1 (toList ps)==[String "0x64"]
                    _ -> False)
                query_ conn "SELECT count(*)::bigint FROM perps_keeper_orders" `shouldReturn` [Only (0::Int64)]
                query_ conn "SELECT state FROM aa_sponsorship_authorizations" `shouldReturn` [Only ("submitted"::Text)]
                query_ conn "SELECT count(*)::bigint FROM perps_commitment_hints" `shouldReturn` [Only (1::Int64)]
                seen <- readIORef calls
                map fst seen `shouldSatisfy` all (`elem` ["eth_getUserOperationReceipt","eth_chainId","eth_getTransactionReceipt","eth_getBlockByNumber","eth_call"])
                stopCommitmentHints sink
                stoppedCalls <- readIORef calls
                threadDelay 300_000
                readIORef calls `shouldReturn` stoppedCalls

await :: IO Bool -> IO ()
await predicate = do
  ready <- predicate
  if ready then pure () else threadDelay 20_000 >> await predicate

withMode :: String -> IO a -> IO a
withMode value action=bracket (lookupEnv "PERPS_COMMITMENT_WAKEUPS_MODE") restore $ \_ -> setEnv "PERPS_COMMITMENT_WAKEUPS_MODE" value >> action
 where restore=maybe (unsetEnv "PERPS_COMMITMENT_WAKEUPS_MODE") (setEnv "PERPS_COMMITMENT_WAKEUPS_MODE")

isolatedDatabase :: Text -> (Connection -> Text -> IO a) -> IO a
isolatedDatabase url action=bracket (connectPostgreSQL $ TE.encodeUtf8 url) close $ \conn -> do
  [Only pid] <- query_ conn "SELECT pg_backend_pid()" :: IO [Only Int]
  let name="commitment_watcher_test_"<>T.pack(show pid)
      schema=Identifier name
      scopedUrl | "postgresql://" `T.isPrefixOf` url || "postgres://" `T.isPrefixOf` url =
                    url<>(if "?" `T.isInfixOf` url then "&" else "?")<>"options=-c%20search_path%3D"<>name
                | otherwise=url<>" options='-c search_path="<>name<>"'"
  void $ execute conn "CREATE SCHEMA ?" (Only schema)
  finally (do
    void $ execute conn "SET search_path TO ?" (Only schema)
    void $ execute_ conn "CREATE TABLE aa_sponsorship_authorizations(digest TEXT PRIMARY KEY,expected_user_operation_hash TEXT UNIQUE,sender TEXT,valid_until BIGINT,state TEXT,submitted_at TIMESTAMPTZ DEFAULT clock_timestamp(),operation JSONB)"
    void $ execute_ conn "CREATE TABLE aa_preparation_authorizations(chain_id BIGINT,paymaster TEXT,sender TEXT,digest TEXT)"
    void $ execute_ conn "CREATE TABLE perps_keeper_orders(order_router TEXT,order_id BIGINT,account TEXT,status TEXT)"
    ensureCommitmentHintSchema conn
    action conn scopedUrl) (void $ execute conn "DROP SCHEMA ? CASCADE" $ Only schema)

sdkFixture :: IO Value
sdkFixture= either (fail . show) pure =<< eitherDecodeFileStrict' "test/fixtures/commitment-hint-sdk-v040.json"
valueAt :: Key -> Value -> Value
valueAt key (Object fields)=maybe Null id $ KM.lookup key fields
valueAt _ _=Null
textAt :: Key -> Value -> Text
textAt key value=case valueAt key value of String text->text; _->error "Invalid SDK fixture text"
arrayAt :: Key -> Value -> [Value]
arrayAt key value=case valueAt key value of Array values->toList values; _->error "Invalid SDK fixture array"
asText :: Value -> Maybe Text
asText (String text)=Just text
asText _=Nothing

entryPoint,transactionHash,blockHash,otherHash :: Text
entryPoint="0x4337084d9e255ff0702461cf8895ce9e3b5ff108"
transactionHash="0x"<>T.replicate 64 "8"
blockHash="0x"<>T.replicate 64 "9"
otherHash="0x"<>T.replicate 64 "7"
hex :: BS.ByteString -> Text
hex bytes="0x"<>TE.decodeUtf8(B16.encode bytes)
rawHeader :: Integer -> Integer -> Value
rawHeader number stamp=object ["number".=quantity number,"hash".=blockHash,"timestamp".=quantity stamp,"l1BlockNumber".=("0x32"::Text)]
quantity :: Integer -> Text
quantity value = "0x"<>T.dropWhile (=='0') (TE.decodeUtf8 $ B16.encode $ encodeUint256 value)
rawReceipt :: Text -> Text -> Text -> Text -> Value
rawReceipt opHash sender paymaster clientId=object ["transactionHash".=transactionHash,"blockHash".=blockHash,"blockNumber".=("0x64"::Text),
  "transactionIndex".=("0x0"::Text),"status".=("0x1"::Text),"logs".=[
    event 1 Manifest.orderLifecycleBookAddress [Perps.intentRegisteredTopic,encodeUint256 20,encodeAddress sender,unhex clientId] (BS.replicate 800 0),
    event 2 Manifest.orderRouterAddress [Perps.orderCommittedTopic,encodeUint256 20,encodeAddress sender] (encodeUint256 0),
    event 3 entryPoint [keccak256 "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)",unhex opHash,encodeAddress sender,encodeAddress paymaster]
      (encodeUint256 0<>encodeUint256 1<>encodeUint256 100<>encodeUint256 100)]]
 where
  event index address topics bytes=object ["address".=address,"topics".=map hex topics,"data".=hex bytes,"transactionHash".=transactionHash,
    "blockNumber".=("0x64"::Text),"blockHash".=blockHash,"transactionIndex".=("0x0"::Text),"logIndex".=quantity index,"removed".=False]
unhex :: Text -> BS.ByteString
unhex value=either error id $ B16.decode $ TE.encodeUtf8 $ T.drop 2 value

nativeConfig :: Text -> Text -> NativeAaConfig
nativeConfig rpc sponsor=NativeAaConfig
  { naaProxyOriginToken="test-only",naaAltoRpcUrl=rpc,naaSecurityRpcUrl=rpc,naaRpcMode=SingleProviderSepolia,
    naaMaxSafeLagSeconds=600,naaPaymasterAddress=sponsor,naaPaymasterCodeHash=otherHash,naaPolicyId=otherHash,
    naaSignerAddress="0x"<>T.replicate 40 "5",naaKmsKeyId="test-only",naaAccountCodeHash=otherHash,
    naaSponsorshipEnabled=True,naaPreparationEnabled=True,naaSubmissionEnabled=True,naaIpRateLimitPerMinute=120,
    naaFinalRateLimitPerMinute=6,naaAccountRateLimitPerMinute=30,naaMaxRequestBytes=262144,naaValiditySeconds=300,
    naaVerificationGasLimit=100000,naaPostOpGasLimit=40000,naaMaxCostWei=10000000000000000,
    naaAccountOutstandingWei=20000000000000000,naaClientOutstandingWei=20000000000000000,
    naaGlobalOutstandingWei=100000000000000000,naaAccountHourlyWei=30000000000000000,
    naaGlobalHourlyWei=100000000000000000,naaGlobalDailyWei=250000000000000000,
    naaCanaryOwners=[],naaGlobalRolloutEnabled=True }
