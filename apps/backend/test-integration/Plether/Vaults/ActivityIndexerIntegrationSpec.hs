module Plether.Vaults.ActivityIndexerIntegrationSpec
  ( vaultActivityIndexerIntegrationSpec
  ) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, finally, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as Lazy
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (methodPost, status200)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.VaultActivity
  ( VaultActivityDeployment (..)
  , VaultActivityIndexerStateRow (..)
  , VaultAttributedHolderRow (..)
  , VaultDepositRequestKey (..)
  , VaultHolderRow (..)
  , ensureVaultActivitySchema
  , getVaultActivityIndexerState
  , getVaultAttributedHolders
  , getVaultHolders
  , getVaultRequestIds
  , resetVaultActivityDeployment
  )
import Plether.Ethereum.Abi (encodeAddress, encodeBool, encodeCall, encodeUint256)
import Plether.Ethereum.Client (newClient)
import Plether.Utils.Hex (hexToInteger, intToHex)
import Plether.Vaults.ActivityIndexer
  ( VaultActivityCycleResult (..)
  , VaultActivityCycleStats (..)
  , VaultActivityIndexerConfig (..)
  , depositRequestTopic
  , runVaultActivityIndexerCycle
  , startVaultActivityIndexer
  , transferTopic
  , verifyVaultActivityBindings
  )
import Plether.Vaults.DepositAttributionIndexer
  ( VaultDepositAttributionCycleResult (..)
  , VaultDepositAttributionCycleStats (..)
  , lpRequestStateCall
  , runVaultDepositAttributionCycle
  )
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

data FixtureState = FixtureState
  { fsReplacementBranch :: Bool
  , fsHead :: Integer
  , fsMalformedAtThirteen :: Bool
  , fsLogRanges :: [(Text, Integer, Integer)]
  , fsUnexpected :: [Text]
  }

vaultActivityIndexerIntegrationSpec :: Text -> Spec
vaultActivityIndexerIntegrationSpec databaseUrl =
  describe "vault activity scripted-RPC integration" $ do
    it "splits ranges, rebuilds a reorg, and never advances past malformed logs" $
      withFixture $ \fixtureUrl stateRef ->
        withVaultIndexerDatabase databaseUrl $ \pool -> do
          client <- newClient fixtureUrl
          verifyVaultActivityBindings client indexerConfig

          first <- runVaultActivityIndexerCycle client pool indexerConfig
          assertCompleted first 12 2
          attributed <- runVaultDepositAttributionCycle client pool indexerConfig
          assertAttributionCompleted attributed 12 1
          withDb pool $ \conn -> do
            getVaultHolders conn deployment seniorVault 10
              `shouldReturnValue` [VaultHolderRow holderA 100]
            getVaultRequestIds conn deployment seniorVault holderA 10 Nothing
              `shouldReturnValue` [11]
            getVaultAttributedHolders conn deployment seniorVault 10
              `shouldReturnValue` [VaultAttributedHolderRow holderA 100 25 125]
          ranges <- fsLogRanges <$> readIORef stateRef
          ranges `shouldSatisfy` elem (seniorVault, 10, 12)
          ranges `shouldSatisfy` elem (seniorVault, 10, 11)
          ranges `shouldSatisfy` elem (seniorVault, 12, 12)

          restarted <- runVaultActivityIndexerCycle client pool indexerConfig
          assertCompleted restarted 12 2
          withDb pool $ \conn ->
            getVaultHolders conn deployment seniorVault 10
              `shouldReturnValue` [VaultHolderRow holderA 100]

          atomicModifyIORef' stateRef $ \state ->
            (state {fsReplacementBranch = True}, ())
          rebuilt <- runVaultActivityIndexerCycle client pool indexerConfig
          assertCompleted rebuilt 12 2
          withDb pool $ \conn -> do
            getVaultHolders conn deployment seniorVault 10
              `shouldReturnValue` [VaultHolderRow holderA 200]
            getVaultAttributedHolders conn deployment seniorVault 10
              `shouldReturnValue` []
          reattributed <- runVaultDepositAttributionCycle client pool indexerConfig
          assertAttributionCompleted reattributed 12 1
          withDb pool $ \conn ->
            getVaultAttributedHolders conn deployment seniorVault 10
              `shouldReturnValue` [VaultAttributedHolderRow holderA 200 25 225]

          atomicModifyIORef' stateRef $ \state ->
            (state {fsHead = 25, fsMalformedAtThirteen = True}, ())
          malformed <- try $ runVaultActivityIndexerCycle client pool indexerConfig
            :: IO (Either SomeException VaultActivityCycleResult)
          malformed `shouldSatisfy` isLeft
          withDb pool $ \conn -> do
            current <- getVaultActivityIndexerState conn deployment
            fmap vaisLastIndexedBlock current `shouldBe` Just 12
            getVaultHolders conn deployment seniorVault 10
              `shouldReturnValue` [VaultHolderRow holderA 200]

          atomicModifyIORef' stateRef $ \state ->
            (state {fsMalformedAtThirteen = False}, ())
          recovered <- runVaultActivityIndexerCycle client pool indexerConfig
          assertCompleted recovered 13 2
          unexpected <- fsUnexpected <$> readIORef stateRef
          unexpected `shouldBe` []

    it "continues independently while a sibling Perps loop is blocked" $
      withFixture $ \fixtureUrl _ ->
        withVaultIndexerDatabase databaseUrl $ \pool -> do
          client <- newClient fixtureUrl
          blocked <- newEmptyMVar
          siblingThread <- forkIO $ takeMVar blocked
          vaultThread <- forkIO $ startVaultActivityIndexer client pool indexerConfig
          let stop = killThread vaultThread >> killThread siblingThread
          completed <- (`finally` stop) $ timeout 5_000_000 $ waitForBackfill pool
          completed `shouldBe` Just True

waitForBackfill :: DbPool -> IO Bool
waitForBackfill pool = do
  state <- withDb pool $ \conn -> getVaultActivityIndexerState conn deployment
  case state of
    Just current | vaisBackfillComplete current -> pure True
    _ -> threadDelay 50_000 >> waitForBackfill pool

assertCompleted :: VaultActivityCycleResult -> Integer -> Int -> IO ()
assertCompleted (VaultActivityCycleCompleted VaultActivityCycleStats {..}) expectedBlock expectedEvents = do
  vacrIndexedThrough `shouldBe` expectedBlock
  vacrSafeHead `shouldBe` expectedBlock
  vacrBackfillComplete `shouldBe` True
  vacrEventCount `shouldBe` expectedEvents
assertCompleted result _ _ =
  expectationFailure $ "Expected a completed vault cycle, received " <> show result

assertAttributionCompleted
  :: VaultDepositAttributionCycleResult
  -> Integer
  -> Int
  -> IO ()
assertAttributionCompleted
  (VaultDepositAttributionCycleCompleted VaultDepositAttributionCycleStats {..})
  expectedBlock
  expectedRequests = do
    vdacrConfirmedThrough `shouldBe` expectedBlock
    vdacrBackfillComplete `shouldBe` True
    vdacrRequestsObserved `shouldBe` expectedRequests
assertAttributionCompleted result _ _ =
  expectationFailure $ "Expected a completed deposit attribution cycle, received " <> show result

withVaultIndexerDatabase :: Text -> (DbPool -> IO a) -> IO a
withVaultIndexerDatabase databaseUrl action = do
  pool <- newDbPool databaseUrl
  let cleanup = do
        withDb pool $ \conn -> resetVaultActivityDeployment conn deployment
        destroyAllResources pool
  (do
      withDb pool ensureVaultActivitySchema
      withDb pool $ \conn -> resetVaultActivityDeployment conn deployment
      action pool
    ) `finally` cleanup

withFixture :: (Text -> IORef FixtureState -> IO a) -> IO a
withFixture action = do
  stateRef <- newIORef initialState
  testWithApplication (pure $ fixtureApplication stateRef) $ \port ->
    action ("http://127.0.0.1:" <> T.pack (show port)) stateRef
 where
  initialState =
    FixtureState
      { fsReplacementBranch = False
      , fsHead = 24
      , fsMalformedAtThirteen = False
      , fsLogRanges = []
      , fsUnexpected = []
      }

fixtureApplication :: IORef FixtureState -> Application
fixtureApplication stateRef request respond
  | requestMethod request == methodPost && null (pathInfo request) = do
      body <- strictRequestBody request
      response <- dispatchRequest stateRef body
      respond $ responseLBS status200 [("Content-Type", "application/json")] $ Aeson.encode response
  | otherwise = do
      recordUnexpected stateRef "unexpected HTTP request"
      respond $ responseLBS status200 [("Content-Type", "application/json")] $
        Aeson.encode $ rpcError Null (-32600) "unexpected HTTP request"

dispatchRequest :: IORef FixtureState -> Lazy.ByteString -> IO Value
dispatchRequest stateRef body =
  case Aeson.eitherDecode body of
    Right (Object requestObject) -> do
      let requestId = maybe Null id $ KeyMap.lookup "id" requestObject
          params = maybe Null id $ KeyMap.lookup "params" requestObject
      case KeyMap.lookup "method" requestObject of
        Just (String methodName) -> dispatchRpc stateRef requestId methodName params
        _ -> pure $ rpcError requestId (-32600) "missing method"
    _ -> pure $ rpcError Null (-32700) "parse error"

dispatchRpc :: IORef FixtureState -> Value -> Text -> Value -> IO Value
dispatchRpc stateRef requestId methodName params = do
  state <- readIORef stateRef
  case methodName of
    "eth_chainId" -> pure $ rpcSuccess requestId $ String "0x66eee"
    "eth_getCode" -> pure $ rpcSuccess requestId $ String "0x01"
    "eth_call" ->
      case callTargetAndData params >>= bindingResult state of
        Just result -> pure $ rpcSuccess requestId $ String $ hexBytes result
        Nothing -> unexpected "unexpected eth_call"
    "eth_getBlockByNumber" ->
      case arrayItems params of
        String blockTag : _ ->
          case if blockTag == "latest" then Just (fsHead state) else parseQuantity blockTag of
            Just number -> pure $ rpcSuccess requestId $ blockValue state number
            Nothing -> unexpected "invalid block tag"
        _ -> unexpected "invalid block params"
    "eth_getLogs" ->
      case logRange params of
        Just (vault, fromBlock, toBlock) -> do
          atomicModifyIORef' stateRef $ \current ->
            (current {fsLogRanges = fsLogRanges current <> [(vault, fromBlock, toBlock)]}, ())
          if toBlock - fromBlock + 1 > 2
            then pure $ rpcError requestId (-32005) "provider range limit"
            else pure $ rpcSuccess requestId $ Aeson.toJSON $ logsFor state vault fromBlock toBlock
        Nothing -> unexpected "invalid log params"
    _ -> unexpected $ "unexpected RPC method " <> methodName
 where
  unexpected message = do
    recordUnexpected stateRef message
    pure $ rpcError requestId (-32601) message

bindingResult :: FixtureState -> (Text, Text) -> Maybe BS.ByteString
bindingResult _ (target, calldata)
  | target == housePool && calldata == callHex "seniorVault()" = Just $ encodeAddress seniorVault
  | target == housePool && calldata == callHex "juniorVault()" = Just $ encodeAddress juniorVault
  | target `elem` [seniorVault, juniorVault] && calldata == callHex "POOL()" = Just $ encodeAddress housePool
  | target `elem` [seniorVault, juniorVault] && calldata == callHex "asset()" = Just $ encodeAddress asset
  | target `elem` [seniorVault, juniorVault] && calldata == callHex "decimals()" = Just $ encodeUint256 9
  | target == publicLens && calldata == callHex "HOUSE_POOL()" = Just $ encodeAddress housePool
  | target == publicLens && calldata == requestStateCallHex = Just requestStateResult
  | otherwise = Nothing

requestStateCallHex :: Text
requestStateCallHex =
  case lpRequestStateCall deployment (VaultDepositRequestKey seniorVault holderA 11) of
    Right calldata -> hexBytes calldata
    Left _ -> ""

requestStateResult :: BS.ByteString
requestStateResult = BS.concat
  [ encodeAddress seniorVault
  , encodeUint256 11
  , encodeAddress holderA
  , encodeUint256 0
  , encodeUint256 0
  , encodeUint256 50
  , encodeUint256 25
  , encodeUint256 0
  , encodeUint256 0
  , encodeUint256 0
  , encodeUint256 0
  , encodeUint256 0
  , encodeUint256 0
  , encodeBool False
  ]

callTargetAndData :: Value -> Maybe (Text, Text)
callTargetAndData value =
  case arrayItems value of
    Object callObject : _ -> do
      String target <- KeyMap.lookup "to" callObject
      String calldata <- KeyMap.lookup "data" callObject
      pure (T.toLower target, T.toLower calldata)
    _ -> Nothing

logRange :: Value -> Maybe (Text, Integer, Integer)
logRange value =
  case arrayItems value of
    Object filterObject : _ -> do
      String vault <- KeyMap.lookup "address" filterObject
      String fromBlockText <- KeyMap.lookup "fromBlock" filterObject
      String toBlockText <- KeyMap.lookup "toBlock" filterObject
      fromBlock <- parseQuantity fromBlockText
      toBlock <- parseQuantity toBlockText
      pure (T.toLower vault, fromBlock, toBlock)
    _ -> Nothing

logsFor :: FixtureState -> Text -> Integer -> Integer -> [Value]
logsFor state vault fromBlock toBlock
  | vault /= seniorVault = []
  | otherwise =
      concat
        [ [transferLog state | includes 10]
        , [depositLog state | includes 11]
        , [malformedLog state | includes 13 && fsMalformedAtThirteen state]
        ]
 where
  includes blockNumber = fromBlock <= blockNumber && blockNumber <= toBlock

transferLog, depositLog, malformedLog :: FixtureState -> Value
transferLog state =
  logValue state 10 0 transferTopic
    [addressTopic zeroAddress, addressTopic holderA]
    (encodeUint256 $ if fsReplacementBranch state then 200 else 100)

depositLog state =
  logValue state 11 0 depositRequestTopic
    [addressTopic holderA, addressTopic holderA, encodeUint256 11]
    (encodeUint256 0 <> encodeUint256 55)

malformedLog state =
  logValue state 13 0 (BS.replicate 32 255) [] BS.empty

logValue :: FixtureState -> Integer -> Integer -> BS.ByteString -> [BS.ByteString] -> BS.ByteString -> Value
logValue state blockNumber logIndex topic indexed eventData =
  object
    [ "transactionHash" .= fixedHash 'c' blockNumber
    , "blockNumber" .= quantity blockNumber
    , "blockHash" .= canonicalHash state blockNumber
    , "transactionIndex" .= quantity 0
    , "logIndex" .= quantity logIndex
    , "address" .= seniorVault
    , "topics" .= map (String . hexBytes) (topic : indexed)
    , "data" .= hexBytes eventData
    ]

blockValue :: FixtureState -> Integer -> Value
blockValue state number =
  object
    [ "number" .= quantity number
    , "hash" .= canonicalHash state number
    , "timestamp" .= quantity (1_700_000_000 + number)
    ]

canonicalHash :: FixtureState -> Integer -> Text
canonicalHash state = fixedHash $ if fsReplacementBranch state then 'b' else 'a'

fixedHash :: Char -> Integer -> Text
fixedHash prefix value =
  let suffix = T.pack $ show value
   in "0x" <> T.replicate (64 - T.length suffix) (T.singleton prefix) <> suffix

addressTopic :: Text -> BS.ByteString
addressTopic address = BS.replicate 12 0 <> decodeHex (T.drop 2 address)

decodeHex :: Text -> BS.ByteString
decodeHex value = either (const BS.empty) id $ B16.decode $ TE.encodeUtf8 value

callHex :: Text -> Text
callHex = hexBytes . (`encodeCall` [])

hexBytes :: BS.ByteString -> Text
hexBytes = ("0x" <>) . TE.decodeUtf8 . B16.encode

quantity :: Integer -> Text
quantity = ("0x" <>) . intToHex

parseQuantity :: Text -> Maybe Integer
parseQuantity value
  | "0x" `T.isPrefixOf` value
      && not (T.null digits)
      && T.all (`elem` ("0123456789abcdefABCDEF" :: String)) digits =
      Just $ hexToInteger digits
  | otherwise = Nothing
 where
  digits = T.drop 2 value

arrayItems :: Value -> [Value]
arrayItems (Array values) = toList values
arrayItems _ = []

rpcSuccess :: Value -> Value -> Value
rpcSuccess requestId result =
  object ["jsonrpc" .= ("2.0" :: Text), "id" .= requestId, "result" .= result]

rpcError :: Value -> Int -> Text -> Value
rpcError requestId code message =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= requestId
    , "error" .= object ["code" .= code, "message" .= message]
    ]

recordUnexpected :: IORef FixtureState -> Text -> IO ()
recordUnexpected stateRef message =
  atomicModifyIORef' stateRef $ \state ->
    (state {fsUnexpected = fsUnexpected state <> [message]}, ())

shouldReturnValue :: (Eq a, Show a) => IO a -> a -> IO ()
shouldReturnValue action expected = action >>= (`shouldBe` expected)

indexerConfig :: VaultActivityIndexerConfig
indexerConfig =
  VaultActivityIndexerConfig
    { vaicDeployment = deployment
    , vaicAssetAddress = asset
    , vaicPublicLensAddress = publicLens
    , vaicConfirmations = 12
    , vaicBatchSize = 5_000
    , vaicPollIntervalMicros = 12_000_000
    }

deployment :: VaultActivityDeployment
deployment = VaultActivityDeployment 421_614 housePool seniorVault juniorVault 10

housePool, seniorVault, juniorVault, asset, publicLens, holderA, zeroAddress :: Text
housePool = "0x0000000000000000000000000000000000001100"
seniorVault = "0x0000000000000000000000000000000000001200"
juniorVault = "0x0000000000000000000000000000000000001300"
asset = "0x0000000000000000000000000000000000001400"
publicLens = "0x0000000000000000000000000000000000001450"
holderA = "0x0000000000000000000000000000000000001500"
zeroAddress = "0x0000000000000000000000000000000000000000"
