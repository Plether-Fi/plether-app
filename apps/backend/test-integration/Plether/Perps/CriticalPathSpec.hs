module Plether.Perps.CriticalPathSpec
  ( criticalPathSpec
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, void)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.IORef (newIORef)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.PostgreSQL.Simple
  ( Only (..)
  , execute
  , query
  , query_
  )
import Network.HTTP.Client
  ( Manager
  , defaultManagerSettings
  , newManager
  )
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application)
import Network.Wai.Test
  ( SResponse (..)
  , defaultRequest
  , request
  , runSession
  , setPath
  )
import Plether.AA.Pimlico (newPimlicoProxyState)
import Plether.Api (app)
import Plether.Cache (newAppCache)
import Plether.Config
  ( Config (..)
  , LpSettlementMode (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  )
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Schema
  ( deletePerpsHistoryFromBlock
  , ensurePerpsHistorySchema
  , ensurePerpsKeeperSchema
  , markPerpsKeeperOrderExecuted
  , upsertPerpsKeeperOrderCommitted
  )
import Plether.Ethereum.Client (RpcClientOptions (..), newClientWithManager)
import Plether.Handlers.TestnetFaucetGuard (newFaucetGuardState)
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , CompetitionRules (crSlug)
  , july2026Competition
  )
import Plether.Perps.CriticalPathFixture
import Plether.Perps.HistoryIndexer (runPerpsIndexer)
import Test.Hspec
  ( Expectation
  , Spec
  , describe
  , expectationFailure
  , it
  , shouldBe
  )
import Web.Scotty (scottyApp)

criticalPathSpec :: Text -> Spec
criticalPathSpec databaseUrl =
  describe "deterministic Perps critical path" $
    it "reconciles V2 lifecycle identity, canonical receipts, and reorgs" $
      withScriptedChain $ \chain ->
        withCriticalPathDatabase databaseUrl $ \pool -> do
          manager <- newManager defaultManagerSettings
          apiApplication <-
            makeApiApplication
              manager
              pool
              (testConfig databaseUrl $ fixtureRpcUrl chain)
              (fixtureRpcUrl chain)
          (runScenario manager pool apiApplication chain)
            `finally` assertNoUnexpectedRequests chain

runScenario
  :: Manager
  -> DbPool
  -> Application
  -> ScriptedChain
  -> IO ()
runScenario manager pool apiApplication chain = do
  seedFastKeeperTerminal pool
  setTraceAvailable chain True

  -- V1 keeper state is deliberately ignored. The V2 wait endpoint only
  -- returns lifecycle-backed indexed history with a client intent identity.
  fastWait <- getApiJson apiApplication waitPath
  assertPath fastWait ["data", "timedOut"] $ Bool True
  assertPath fastWait ["data", "order"] Null

  -- Finalization identity comes from canonical V2 lifecycle evidence, while
  -- exact execution economics are enriched through Alchemy callTracer output.
  runIndexer manager pool chain
  getDebugTraceRequestCount chain `shouldReturnValue` 1
  (terminalOrder, waitOrder) <-
    getOrderFromBothEndpoints apiApplication
  forM_ [terminalOrder, waitOrder] $ \order -> do
    assertCanonicalTerminal
      order
      commitTxHashA
      terminalTxHashA
      terminalBlockHashA
      receiptHashA
      evidenceA
    assertReceiptEconomics order evidenceA
    assertField order "activityVpiUsdc" $
      String $ decimal $ efActivityVpiUsdc evidenceA
  indexedOrders <- getApiJson apiApplication ordersPath
  assertPath indexedOrders ["data", "indexedThroughBlock"] $
    String $ decimal terminalBlockNumber

  -- Re-indexing is idempotent and cannot mutate the canonical receipt.
  runIndexer manager pool chain
  getDebugTraceRequestCount chain `shouldReturnValue` 1
  stableOrder <- getFirstOrder apiApplication
  stableOrder `shouldBe` terminalOrder

  -- Reorg A into a branch containing only a replacement commit. Canonical
  -- history must rewind to the replacement V2 intent and /wait must continue
  -- ignoring the stale V1 keeper terminal A.
  setCanonicalBranch chain CommittedOnly
  runIndexer manager pool chain
  committedOrder <- getFirstOrder apiApplication
  assertField committedOrder "terminalStatus" $ String "Committed"
  assertField committedOrder "commitTxHash" $
    String committedOnlyCommitTxHash
  assertField committedOrder "commitBlockNumber" $
    String $ decimal commitBlockNumber
  assertMissingFields
    committedOrder
    [ "terminalTxHash"
    , "terminalBlockNumber"
    , "terminalBlockHash"
    , "executionPrice"
    , "vpiUsdc"
    , "frozenCloseSpreadUsdc"
    , "executionEconomicsVersion"
    , "executionOraclePrice"
    , "executionOracleFrozen"
    , "oracleMinPublishTime"
    , "oracleMaxPublishTime"
    , "oracleDerivationVersion"
    , "activityType"
    , "activityVpiUsdc"
    ]
  committedWait <- getApiJson apiApplication waitPath
  assertPath committedWait ["data", "timedOut"] $ Bool True
  committedWaitOrder <- requirePath committedWait ["data", "order"]
  assertField committedWaitOrder "terminalStatus" $ String "Committed"
  assertMissingField committedWaitOrder "terminalTxHash"

  -- Reorg again so even the commit disappears. The old keeper terminal remains
  -- irrelevant when the V2 intent disappears.
  setCanonicalBranch chain Empty
  runIndexer manager pool chain
  emptyOrders <- getApiJson apiApplication ordersPath
  assertPath emptyOrders ["data", "orders"] $ Aeson.toJSON ([] :: [Value])
  emptyWait <- getApiJson apiApplication waitPath
  assertPath emptyWait ["data", "timedOut"] $ Bool True
  assertPath emptyWait ["data", "order"] Null

  -- Canonical replacement B is accepted and enriched independently.
  setCanonicalBranch chain TerminalB
  runIndexer manager pool chain
  getDebugTraceRequestCount chain `shouldReturnValue` 2
  (replacementOrder, replacementWaitOrder) <-
    getOrderFromBothEndpoints apiApplication
  forM_ [replacementOrder, replacementWaitOrder] $ \order -> do
    assertCanonicalTerminal
      order
      commitTxHashB
      terminalTxHashB
      terminalBlockHashB
      receiptHashB
      evidenceB
    assertReceiptEconomics order evidenceB

  oldTerminalEventCount <- countTerminalEvents pool terminalTxHashA
  oldTerminalEventCount `shouldBe` 0


runIndexer :: Manager -> DbPool -> ScriptedChain -> IO ()
runIndexer manager pool chain =
  runPerpsIndexer manager pool $ testIndexerConfig chain

makeApiApplication
  :: Manager
  -> DbPool
  -> Config
  -> Text
  -> IO Application
makeApiApplication manager pool config rpcUrl = do
  cache <- newAppCache
  proxyState <- newPimlicoProxyState
  faucetGuardState <- newFaucetGuardState
  mainRequestId <- newIORef 1
  perpsRequestId <- newIORef 1
  client <-
    newClientWithManager
      manager
      mainRequestId
      (RpcClientOptions rpcUrl Nothing "integration-api-core")
  perpsClient <-
    newClientWithManager
      manager
      perpsRequestId
      (RpcClientOptions rpcUrl Nothing "integration-api-perps")
  scottyApp $
    app cache client perpsClient config (Just pool) manager proxyState faucetGuardState

withCriticalPathDatabase :: Text -> (DbPool -> IO a) -> IO a
withCriticalPathDatabase databaseUrl action =
  bracket (newDbPool databaseUrl) destroyAllResources $ \pool -> do
    prepareDatabase pool
    action pool `finally` cleanupDatabase pool

prepareDatabase :: DbPool -> IO ()
prepareDatabase pool = do
  withDb pool $ \connection -> do
    databaseNames <-
      (query_ connection "SELECT current_database()" :: IO [Only Text])
    case databaseNames of
      [Only databaseName]
        | "critical_path" `Text.isInfixOf` Text.toLower databaseName ->
            pure ()
      [Only databaseName] ->
        fail $
          "Refusing to run the destructive integration setup against database "
            <> Text.unpack databaseName
            <> "; its name must contain critical_path"
      _ -> fail "PostgreSQL did not return exactly one current_database() row"
    ensurePerpsHistorySchema connection
    ensurePerpsKeeperSchema connection
  cleanupDatabase pool

cleanupDatabase :: DbPool -> IO ()
cleanupDatabase pool =
  withDb pool $ \connection -> do
    deletePerpsHistoryFromBlock
      connection
      testChainId
      testRouter
      0
    void $
      execute
        connection
        "DELETE FROM perps_indexer_state \
        \WHERE chain_id = ? AND release_router = ?"
        (testChainId, Text.toLower testRouter)
    void $
      execute
        connection
        "DELETE FROM perps_keeper_orders WHERE order_router = ?"
        (Only $ Text.toLower testRouter)
    void $
      execute
        connection
        "DELETE FROM perps_keeper_state WHERE order_router = ?"
        (Only $ Text.toLower testRouter)

seedFastKeeperTerminal :: DbPool -> IO ()
seedFastKeeperTerminal pool =
  withDb pool $ \connection -> do
    upsertPerpsKeeperOrderCommitted
      connection
      testRouter
      testOrderId
      testAccount
      1
      commitBlockNumber
      commitBlockNumber
      1_785_437_800
      commitTxHashA
    markPerpsKeeperOrderExecuted
      connection
      testRouter
      testOrderId
      terminalTxHashA
      terminalBlockNumber
      (efExecutionPrice evidenceA)

countTerminalEvents :: DbPool -> Text -> IO Integer
countTerminalEvents pool txHash =
  withDb pool $ \connection -> do
    rows <-
      ( query
          connection
          "SELECT COUNT(*) FROM perps_events \
          \WHERE chain_id = ? AND release_router = ? AND tx_hash = ?"
          (testChainId, Text.toLower testRouter, Text.toLower txHash)
          :: IO [Only Integer]
      )
    case rows of
      [Only count] -> pure count
      _ -> fail "Expected one COUNT(*) row for canonical terminal events"

getOrderFromBothEndpoints :: Application -> IO (Value, Value)
getOrderFromBothEndpoints apiApplication = do
  order <- getFirstOrder apiApplication
  waitResponse <- getApiJson apiApplication waitPath
  assertPath waitResponse ["data", "timedOut"] $ Bool False
  waitOrder <- requirePath waitResponse ["data", "order"]
  pure (order, waitOrder)

getFirstOrder :: Application -> IO Value
getFirstOrder apiApplication = do
  response <- getApiJson apiApplication ordersPath
  case lookupPath response ["data", "orders"] of
    Just (Array orders) ->
      case toList orders of
        [order] -> pure order
        values -> do
          expectationFailure $
            "Expected exactly one canonical order, got "
              <> show (length values)
          pure Null
    value -> do
      expectationFailure $
        "Expected data.orders array, got " <> show value
      pure Null

getApiJson :: Application -> Text -> IO Value
getApiJson apiApplication path = do
  response <-
    runSession
      (request $ setPath defaultRequest $ Text.encodeUtf8 path)
      apiApplication
  simpleStatus response `shouldBe` status200
  case Aeson.eitherDecode $ simpleBody response of
    Left err -> do
      expectationFailure $
        "API response was not valid JSON: " <> err
      pure Null
    Right value -> pure value

assertCanonicalTerminal
  :: Value
  -> Text
  -> Text
  -> Text
  -> Text
  -> EvidenceFixture
  -> Expectation
assertCanonicalTerminal order commitTxHash txHash blockHash receiptHash evidence = do
  assertField order "terminalStatus" $ String "Executed"
  assertField order "commitTxHash" $ String commitTxHash
  assertField order "terminalTxHash" $ String txHash
  assertField order "terminalBlockNumber" $
    String $ decimal terminalBlockNumber
  assertField order "terminalBlockHash" $ String blockHash
  assertField order "clientOrderId" $ String testClientOrderId
  assertField order "receiptHash" $ String receiptHash
  assertField order "terminalReason" $ String "Executed"
  assertField order "executionMode" $ String "Live"
  assertField order "executionPrice" $
    String $ decimal $ efExecutionPrice evidence

assertReceiptEconomics :: Value -> EvidenceFixture -> Expectation
assertReceiptEconomics order evidence = do
  assertPath order ["receiptEconomics", "vpiUsdc"] $
    String $ decimal $ efVpiUsdc evidence
  assertPath order ["receiptEconomics", "frozenSpreadUsdc"] $
    String $ decimal $ efFrozenCloseSpreadUsdc evidence
  assertField order "executionEconomicsVersion" $
    Aeson.toJSON (2 :: Int)

assertField :: Value -> Text -> Value -> Expectation
assertField value fieldName expected =
  lookupObjectField value fieldName `shouldBe` Just expected

assertMissingField :: Value -> Text -> Expectation
assertMissingField value fieldName =
  lookupObjectField value fieldName `shouldBe` Nothing

assertMissingFields :: Value -> [Text] -> Expectation
assertMissingFields value =
  mapM_ $ assertMissingField value

assertPath :: Value -> [Text] -> Value -> Expectation
assertPath value path expected =
  lookupPath value path `shouldBe` Just expected

requirePath :: Value -> [Text] -> IO Value
requirePath value path =
  case lookupPath value path of
    Just result -> pure result
    Nothing -> do
      expectationFailure $
        "Missing JSON path " <> show path <> " in " <> show value
      pure Null

lookupPath :: Value -> [Text] -> Maybe Value
lookupPath value [] = Just value
lookupPath value (fieldName : rest) =
  lookupObjectField value fieldName >>= (`lookupPath` rest)

lookupObjectField :: Value -> Text -> Maybe Value
lookupObjectField (Object objectValue) fieldName =
  KeyMap.lookup (Key.fromText fieldName) objectValue
lookupObjectField _ _ = Nothing

shouldReturnValue :: (Show a, Eq a) => IO a -> a -> IO ()
shouldReturnValue action expected =
  action >>= (`shouldBe` expected)

ordersPath :: Text
ordersPath =
  "/api/perps/accounts/"
    <> testAccount
    <> "/orders?router="
    <> testRouter
    <> "&limit=30"

waitPath :: Text
waitPath =
  "/api/perps/orders/"
    <> decimal testOrderId
    <> "/wait?router="
    <> testRouter
    <> "&account="
    <> testAccount
    <> "&timeoutSeconds=1"

decimal :: Integer -> Text
decimal = Text.pack . show

testConfig :: Text -> Text -> Config
testConfig databaseUrl rpcUrl =
  Config
    { cfgRpcUrl = rpcUrl
    , cfgRpcAuthToken = Nothing
    , cfgChainId = testChainId
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Just databaseUrl
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = "https://benchmarks.pyth.network"
    , cfgPythHistoryUrl = "https://pyth.dourolabs.app/v1"
    , cfgPythHermesUrl = "https://hermes.pyth.network"
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 1
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgPerpsCandleWriteMode = PerpsCandleWritesOff
    , cfgPerpsCandleReadMode = PerpsCandleReadsLegacy
    , cfgPerpsCandleReadIntervals = []
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = rpcUrl
    , cfgPerpsRpcAuthToken = Nothing
    , cfgPerpsChainId = testChainId
    , cfgPerpsUsdc = testClearinghouse
    , cfgPerpsOrderRouter = testRouter
    , cfgPerpsOrderLifecycleBook = Just testLifecycleBook
    , cfgPerpsCfdEngine = testEngine
    , cfgPerpsCfdEngineLens = testLens
    , cfgPerpsCfdEngineSettlementSidecar = testSidecar
    , cfgPerpsMarginClearinghouse = testClearinghouse
    , cfgPerpsPletherOracle = testOracle
    , cfgPerpsAccountLens = testLens
    , cfgPerpsHousePool = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgPerpsSettlementMonitorLens = "0xd251AC0BD90780c48F31F575152808315200664E"
    , cfgPerpsIndexerStartBlock = commitBlockNumber
    , cfgVaultHistoryHousePoolAddress = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgVaultHistorySeniorVaultAddress = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
    , cfgVaultHistoryJuniorVaultAddress = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryConfirmations = 0
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = testCompetitionReleaseManifest
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Nothing
    , cfgFaucetGuardConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperIdlePollSeconds = 5
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 0
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    , cfgLpSettlementMode = LpSettlementOff
    , cfgLpSettlementPrivateKey = Nothing
    , cfgLpSettlementSeniorVault = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
    , cfgLpSettlementJuniorVault = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
    , cfgLpSettlementPollSeconds = 15
    , cfgLpSettlementMaxDrainTransactions = 4
    , cfgLpSettlementPendingReplacementSeconds = 60
    , cfgLpSettlementMaxReplacements = 3
    , cfgLpSettlementMaxTxCostWei = 0
    }

testCompetitionReleaseManifest :: CompetitionReleaseManifest
testCompetitionReleaseManifest =
  CompetitionReleaseManifest
    { crmReleaseId = crSlug july2026Competition
    , crmChainId = testChainId
    , crmUsdc = testClearinghouse
    , crmOrderRouter = testRouter
    , crmMarginClearinghouse = testClearinghouse
    , crmAccountLens = testLens
    , crmCfdEngine = testEngine
    , crmCfdEngineLens = testLens
    , crmSettlementSidecar = testSidecar
    , crmPletherOracle = testOracle
    , crmIndexerStartBlock = commitBlockNumber
    }
