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
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  )
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Protocol
  ( deleteProtocolLedgerFromBlock
  , ensureProtocolSchema
  )
import Plether.Database.Schema
  ( deletePerpsHistoryFromBlock
  , ensurePerpsHistorySchema
  , ensurePerpsKeeperSchema
  , markPerpsKeeperOrderExecuted
  , updatePerpsOrderEconomicsEvidence
  , updatePerpsOrderOracleEvidence
  , upsertPerpsKeeperOrderCommitted
  )
import Plether.Ethereum.Client (EthClient (..))
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , CompetitionRules (crSlug)
  , july2026Competition
  )
import Plether.Perps.CriticalPathFixture
import Plether.Perps.HistoryIndexer (perpsIndexerName, runPerpsIndexer)
import Plether.Protocol.Release (ProtocolRelease (..))
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
    it "reconciles keeper, canonical history, exact evidence, and reorgs" $
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

  -- The keeper is intentionally faster than canonical history. This proves
  -- that /wait can surface inclusion immediately without manufacturing exact
  -- execution evidence or a canonical block hash.
  fastWait <- getApiJson apiApplication waitPath
  assertPath fastWait ["data", "timedOut"] $ Bool False
  fastOrder <- requirePath fastWait ["data", "order"]
  assertField fastOrder "terminalStatus" $ String "Executed"
  assertField fastOrder "commitTxHash" $ String commitTxHashA
  assertField fastOrder "terminalTxHash" $ String terminalTxHashA
  assertField fastOrder "executionPrice" $
    String $ decimal $ efExecutionPrice evidenceA
  assertMissingFields
    fastOrder
    [ "terminalBlockHash"
    , "vpiUsdc"
    , "executionOraclePrice"
    , "executionOracleFrozen"
    ]

  -- First pass: canonical terminal data is indexed, but both callTracer and
  -- explorer evidence are unavailable. Activity preview VPI must remain
  -- explicitly separate from the missing exact VPI.
  runIndexer manager pool chain
  getRawTraceRequestCount chain `shouldReturnValue` 1
  (terminalWithoutEvidence, waitWithoutEvidence) <-
    getOrderFromBothEndpoints apiApplication
  forM_ [terminalWithoutEvidence, waitWithoutEvidence] $ \order -> do
    assertCanonicalTerminal
      order
      commitTxHashA
      terminalTxHashA
      terminalBlockHashA
      evidenceA
    assertField order "activityVpiUsdc" $
      String $ decimal $ efActivityVpiUsdc evidenceA
    assertMissingFields
      order
      [ "vpiUsdc"
      , "frozenCloseSpreadUsdc"
      , "executionEconomicsVersion"
      , "executionOraclePrice"
      , "executionOracleFrozen"
      ]
  ordersWithoutEvidence <- getApiJson apiApplication ordersPath
  assertPath ordersWithoutEvidence ["data", "indexedThroughBlock"] $
    String $ decimal terminalBlockNumber

  -- A normal iteration inside the five-minute backoff must not refetch the
  -- trace. This catches a missing/failed attempt timestamp rather than merely
  -- proving that an explicitly aged row can retry.
  runIndexer manager pool chain
  getRawTraceRequestCount chain `shouldReturnValue` 1
  backoffOrder <- getFirstOrder apiApplication
  backoffOrder `shouldBe` terminalWithoutEvidence

  -- The production retry interval is five minutes. Age the attempt in the
  -- isolated database instead of sleeping, then enable only the Blockscout
  -- fallback. The real evidence worker must hydrate the same canonical row.
  ageEvidenceAttempt pool
  setTraceAvailable chain True
  runIndexer manager pool chain
  getRawTraceRequestCount chain `shouldReturnValue` 2
  (enrichedOrder, enrichedWaitOrder) <-
    getOrderFromBothEndpoints apiApplication
  forM_ [enrichedOrder, enrichedWaitOrder] $ \order ->
    assertExactEvidence order evidenceA

  -- Once derivation versions are complete, a conflicting provider response
  -- must not be fetched and cannot mutate the finalized values.
  setTraceEvidence chain conflictingEvidence
  runIndexer manager pool chain
  getRawTraceRequestCount chain `shouldReturnValue` 2
  stableOrder <- getFirstOrder apiApplication
  stableOrder `shouldBe` enrichedOrder

  -- Reorg A into a branch containing only a replacement commit. Canonical
  -- history must rewind to Committed and /wait must suppress keeper terminal A.
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

  -- Reorg again so even the commit disappears. Cursor coverage proves the old
  -- keeper terminal stale, therefore /wait returns null instead of resurrecting
  -- terminal A.
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
  getRawTraceRequestCount chain `shouldReturnValue` 3
  (replacementOrder, replacementWaitOrder) <-
    getOrderFromBothEndpoints apiApplication
  forM_ [replacementOrder, replacementWaitOrder] $ \order -> do
    assertCanonicalTerminal
      order
      commitTxHashB
      terminalTxHashB
      terminalBlockHashB
      evidenceB
    assertExactEvidence order evidenceB

  oldTerminalEventCount <- countTerminalEvents pool terminalTxHashA
  oldTerminalEventCount `shouldBe` 0

  -- Simulate a late evidence result from the orphaned A transaction. The
  -- production SQL guards on tx, block, and block hash; B must remain byte
  -- identical for both economics and oracle evidence.
  writeLateOrphanedEvidence pool
  replacementAfterLateEvidence <- getFirstOrder apiApplication
  replacementAfterLateEvidence `shouldBe` replacementOrder

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
  mainRequestId <- newIORef 1
  perpsRequestId <- newIORef 1
  let client =
        EthClient
          { clientManager = manager
          , clientRpcUrl = rpcUrl
          , clientRequestId = mainRequestId
          }
      perpsClient =
        EthClient
          { clientManager = manager
          , clientRpcUrl = rpcUrl
          , clientRequestId = perpsRequestId
          }
  scottyApp $
    app cache client perpsClient config (Just pool) manager proxyState

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
    ensureProtocolSchema connection criticalPathRelease
  cleanupDatabase pool

cleanupDatabase :: DbPool -> IO ()
cleanupDatabase pool =
  withDb pool $ \connection -> do
    deletePerpsHistoryFromBlock
      connection
      testChainId
      testRouter
      0
    deleteProtocolLedgerFromBlock connection criticalPathReleaseId 0
    void $
      execute
        connection
        "DELETE FROM protocol_indexed_blocks \
        \WHERE release_id = ? AND indexer_name = ?"
        (criticalPathReleaseId, perpsIndexerName)
    void $
      execute
        connection
        "DELETE FROM protocol_indexer_state \
        \WHERE release_id = ? AND indexer_name = ?"
        (criticalPathReleaseId, perpsIndexerName)
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

ageEvidenceAttempt :: DbPool -> IO ()
ageEvidenceAttempt pool =
  withDb pool $ \connection ->
    void $
      execute
        connection
        "UPDATE perps_orders \
        \SET execution_evidence_last_attempt_at = NOW() - INTERVAL '6 minutes' \
        \WHERE chain_id = ? AND order_router = ? AND order_id = ?"
        (testChainId, Text.toLower testRouter, testOrderId)

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

writeLateOrphanedEvidence :: DbPool -> IO ()
writeLateOrphanedEvidence pool =
  withDb pool $ \connection -> do
    updatePerpsOrderEconomicsEvidence
      connection
      testChainId
      testRouter
      testOrderId
      terminalTxHashA
      terminalBlockNumber
      terminalBlockHashA
      9_999_999_999
      (Just 8_888_888)
      99
    updatePerpsOrderOracleEvidence
      connection
      testChainId
      testRouter
      testOrderId
      terminalTxHashA
      terminalBlockNumber
      terminalBlockHashA
      (Just 77_777_777)
      (Just True)
      (Just 1)
      (Just 1)
      99

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
  -> EvidenceFixture
  -> Expectation
assertCanonicalTerminal order commitTxHash txHash blockHash evidence = do
  assertField order "terminalStatus" $ String "Executed"
  assertField order "commitTxHash" $ String commitTxHash
  assertField order "terminalTxHash" $ String txHash
  assertField order "terminalBlockNumber" $
    String $ decimal terminalBlockNumber
  assertField order "terminalBlockHash" $ String blockHash
  assertField order "executionPrice" $
    String $ decimal $ efExecutionPrice evidence

assertExactEvidence :: Value -> EvidenceFixture -> Expectation
assertExactEvidence order evidence = do
  assertField order "vpiUsdc" $
    String $ decimal $ efVpiUsdc evidence
  assertField order "frozenCloseSpreadUsdc" $
    String $ decimal $ efFrozenCloseSpreadUsdc evidence
  assertField order "executionEconomicsVersion" $
    Aeson.toJSON (1 :: Int)
  assertField order "executionOraclePrice" $
    String $ decimal $ efOraclePrice evidence
  assertField order "executionOracleFrozen" $
    Bool $ efOracleFrozen evidence
  assertField order "oracleMinPublishTime" $
    String $ decimal $ efOraclePublishTime evidence
  assertField order "oracleMaxPublishTime" $
    String $ decimal $ efOraclePublishTime evidence
  assertField order "oracleDerivationVersion" $
    Aeson.toJSON (2 :: Int)
  assertField order "activityVpiUsdc" $
    String $ decimal $ efActivityVpiUsdc evidence

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
    , cfgChainId = testChainId
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Just databaseUrl
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = "https://benchmarks.pyth.network"
    , cfgPythHermesUrl = "https://hermes.pyth.network"
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 1
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgProtocolExplorerEnabled = False
    , cfgPerpsCandleWriteMode = PerpsCandleWritesOff
    , cfgPerpsCandleReadMode = PerpsCandleReadsLegacy
    , cfgPerpsCandleReadIntervals = []
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = rpcUrl
    , cfgPerpsChainId = testChainId
    , cfgPerpsUsdc = testClearinghouse
    , cfgPerpsOrderRouter = testRouter
    , cfgPerpsCfdEngine = testEngine
    , cfgPerpsCfdEngineLens = testLens
    , cfgPerpsCfdEngineSettlementSidecar = testSidecar
    , cfgPerpsMarginClearinghouse = testClearinghouse
    , cfgPerpsPletherOracle = testOracle
    , cfgPerpsAccountLens = testLens
    , cfgPerpsPublicLens = testLens
    , cfgPerpsHousePool = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgPerpsSeniorVault = testEngine
    , cfgPerpsJuniorVault = testSidecar
    , cfgPerpsOrderRouterAdmin = testRouter
    , cfgPerpsCfdEngineAdmin = testEngine
    , cfgPerpsSettlementMonitorLens = "0xd251AC0BD90780c48F31F575152808315200664E"
    , cfgPerpsIndexerStartBlock = commitBlockNumber
    , cfgVaultHistoryHousePoolAddress = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgVaultHistorySeniorVaultAddress = testEngine
    , cfgVaultHistoryJuniorVaultAddress = testSidecar
    , cfgVaultHistoryDeploymentBlock = commitBlockNumber
    , cfgVaultHistoryRpcUrl = rpcUrl
    , cfgVaultHistoryConfirmations = 0
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = testCompetitionReleaseManifest
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 0
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    , cfgLpSettlementEnabled = False
    , cfgLpSettlementPollSeconds = 15
    }

criticalPathReleaseId :: Text
criticalPathReleaseId = "critical-path-integration"

criticalPathRelease :: ProtocolRelease
criticalPathRelease =
  ProtocolRelease
    { prId = criticalPathReleaseId
    , prName = "Deterministic critical-path integration"
    , prChainId = testChainId
    , prDeploymentBlock = commitBlockNumber
    , prCalculationVersion = "protocol-transparency-v1"
    , prUsdc = testUsdc
    , prOrderRouter = testRouter
    , prOrderRouterAdmin = testRouter
    , prCfdEngine = testEngine
    , prCfdEngineAdmin = testEngine
    , prMarginClearinghouse = testClearinghouse
    , prPublicLens = testLens
    , prAccountLens = testLens
    , prHousePool = testClearinghouse
    , prSeniorVault = testSidecar
    , prJuniorVault = testOracle
    , prPletherOracle = testOracle
    , prOperationalWallets = []
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
