module Plether.Database.Candles
  ( BasketObservationInput (..)
  , BasketDefinitionIdentity (..)
  , BasketCandleRow (..)
  , CandleCurrent (..)
  , CandlePage (..)
  , CandleRange (..)
  , CandleQuality (..)
  , RollupCoverage (..)
  , RollupKind (..)
  , MarketVolumeRollupSnapshot (..)
  , canonicalCandleIntervals
  , defaultBasketSeriesId
  , ensureCandleSchema
  , ensureCurrentBasketDefinition
  , upsertBasketObservation
  , recomputeBasketCandleHierarchy
  , recomputeMarketVolumeHierarchy
  , recomputeMarketVolumeHierarchyBatch
  , lockMarketVolumeDataset
  , advanceBasketPriceCoverage
  , advanceMarketVolumeCoverage
  , invalidateMarketVolumeFromBlock
  , getActiveBasketSeriesId
  , getActiveBasketDefinitionIdentity
  , getBasketCandlePage
  , getBasketCandleRange
  , getCurrentBasketCandle
  , backfillLegacyBasketSnapshots
  , backfillMarketVolume
  , countBasketCandles
  , countMarketVolumeRollups
  , getRollupCoverage
  , getMarketVolumeCoverageSnapshot
  , getMarketVolumeRollupSnapshot
  , upsertRollupCoverage
  , beginRollupMaintenance
  , bumpRollupDatasetGeneration
  , markRollupCoverageIncomplete
  ) where

import Control.Monad (forM, forM_, unless, when)
import Crypto.Hash (Digest, SHA256, hashlazy)
import Data.Aeson (Value, encode, object, (.=))
import Data.Maybe (catMaybes, listToMaybe)
import Data.Scientific (Scientific, base10Exponent, coefficient)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection
  , In (..)
  , Only (..)
  , Query
  , execute
  , execute_
  , query
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Plether.Pyth.Basket (BasketComponent (..), basketComponents, basketDisplayPriceCap)

data BasketObservationInput = BasketObservationInput
  { boiSeriesId :: Text
  , boiObservationId :: Text
  , boiPublishTime :: Integer
  , boiBasketPrice :: Integer
  , boiComponentPrices :: Value
  , boiSource :: Text
  , boiSourcePriority :: Int
  }
  deriving stock (Show)

data BasketDefinitionIdentity = BasketDefinitionIdentity
  { bdiSeriesId :: Text
  , bdiConfigurationHash :: Text
  , bdiDisplayPriceCap :: Integer
  , bdiEffectiveFrom :: Integer
  , bdiEffectiveTo :: Maybe Integer
  }
  deriving stock (Eq, Show)

data CandleQuality = CandleObserved | CandleLegacySampled | CandleMixed
  deriving stock (Eq, Show)

data BasketCandleRow = BasketCandleRow
  { bcrBucketStart :: Integer
  , bcrRawOpenPrice :: Integer
  , bcrRawHighPrice :: Integer
  , bcrRawLowPrice :: Integer
  , bcrRawClosePrice :: Integer
  , bcrSampleCount :: Int
  , bcrQuality :: CandleQuality
  , bcrRevision :: Integer
  , bcrPriceComplete :: Bool
  , bcrVolumeNumerator :: Maybe Integer
  , bcrTradeCount :: Maybe Integer
  , bcrVolumeComplete :: Bool
  }
  deriving stock (Eq, Show)

-- Metadata is present even when there is no observation in the active bucket.
data CandleCurrent = CandleCurrent
  { ccCandle :: Maybe BasketCandleRow
  , ccCoverageStart :: Maybe Integer
  , ccCoverageEnd :: Maybe Integer
  , ccFinalizedThrough :: Maybe Integer
  , ccDatasetGeneration :: Integer
  , ccCoverageComplete :: Bool
  }
  deriving stock (Eq, Show)

data CandlePage = CandlePage
  { cpCandles :: [BasketCandleRow]
  , cpPreviousCursor :: Maybe Integer
  , cpHasEarlier :: Bool
  , cpCoverageStart :: Maybe Integer
  , cpCoverageEnd :: Maybe Integer
  , cpFinalizedThrough :: Maybe Integer
  , cpDatasetGeneration :: Integer
  , cpCoverageComplete :: Bool
  }
  deriving stock (Eq, Show)

-- A bounded compatibility read shares the same combined price/volume metadata
-- as native pages, but returns one caller-supplied time range in one SQL query.
-- The handler validates every row and the maximum result count before exposing
-- this read through the legacy response shape.
data CandleRange = CandleRange
  { crCandles :: [BasketCandleRow]
  , crCoverageStart :: Maybe Integer
  , crCoverageEnd :: Maybe Integer
  , crFinalizedThrough :: Maybe Integer
  , crDatasetGeneration :: Integer
  , crCoverageComplete :: Bool
  }
  deriving stock (Eq, Show)

data RollupKind = PriceRollup | VolumeRollup
  deriving stock (Eq, Show)

data RollupCoverage = RollupCoverage
  { rcKind :: RollupKind
  , rcSeriesId :: Maybe Text
  , rcChainId :: Maybe Integer
  , rcReleaseRouter :: Maybe Text
  , rcIntervalSeconds :: Integer
  , rcCoverageStart :: Maybe Integer
  , rcCoverageEnd :: Maybe Integer
  , rcFinalizedThrough :: Maybe Integer
  , rcGeneration :: Integer
  , rcComplete :: Bool
  , rcDerivationVersion :: Text
  , rcLastError :: Maybe Text
  , rcMaintenanceFrom :: Maybe Integer
  , rcMaintenanceTo :: Maybe Integer
  }
  deriving stock (Eq, Show)

-- Semantic snapshot used by the bounded duplicate-ingestion gate. Timestamps
-- are intentionally absent; values, source bounds, revision, and finalized
-- state are all part of the identity that replay is forbidden to change.
data MarketVolumeRollupSnapshot = MarketVolumeRollupSnapshot
  { mvrsIntervalSeconds :: Integer
  , mvrsBucketStart :: Integer
  , mvrsVolumeNumerator :: Scientific
  , mvrsTradeCount :: Integer
  , mvrsFirstSourceBlock :: Integer
  , mvrsLastSourceBlock :: Integer
  , mvrsRevision :: Integer
  , mvrsFinalized :: Bool
  }
  deriving stock (Eq, Show)

instance FromRow MarketVolumeRollupSnapshot where
  fromRow =
    MarketVolumeRollupSnapshot
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

canonicalCandleIntervals :: [Integer]
canonicalCandleIntervals = [60, 180, 300, 900, 1800, 3600, 86_400]

defaultBasketSeriesId :: Text
defaultBasketSeriesId = "dxy-v1"

-- Additive DDL only. Potentially large history indexes are built concurrently;
-- historical rewrites belong to CandleAdmin.
ensureCandleSchema :: Connection -> IO ()
ensureCandleSchema conn = do
  forM_ candleTableSchemaStatements $ \statement -> do
    _ <- execute_ conn statement
    pure ()
  ensureCandleActivityIndexPrerequisites conn
  ensureCandleEventIndexPrerequisites conn
  ensureCandleActivityIndex conn
  ensureCandleEventIndex conn
  ensureCandleActivityReorgIndex conn
  ensureCandleEventReorgIndex conn

ensureCandleActivityIndex :: Connection -> IO ()
ensureCandleActivityIndex conn = do
  indexState <- getCandleActivityIndexState conn
  case indexState of
    Nothing -> do
      assertCandleActivityIndexNameAvailable conn
      buildCandleActivityIndex conn
    Just state
      | candleActivityIndexUsable state -> pure ()
      | candleActivityIndexCatalogValid state ->
          fail $ candleActivityIndexShapeConflictMessage state
      | candleActivityIndexTargetsHistory state -> do
          -- A failed CREATE INDEX CONCURRENTLY leaves a named, invalid catalog
          -- entry behind. IF NOT EXISTS would silently accept it forever, so
          -- remove only this exact invalid index, without blocking history
          -- writers, before retrying the build.
          _ <- execute_ conn candleActivityIndexDropStatement
          buildCandleActivityIndex conn
      | otherwise ->
          fail $ candleActivityIndexConflictMessage state

ensureCandleEventIndex :: Connection -> IO ()
ensureCandleEventIndex conn = do
  indexState <- getCandleEventIndexState conn
  case indexState of
    Nothing -> do
      assertCandleEventIndexNameAvailable conn
      buildCandleEventIndex conn
    Just state
      | candleEventIndexUsable state -> pure ()
      | candleActivityIndexCatalogValid state ->
          fail $ candleEventIndexShapeConflictMessage state
      | candleEventIndexTargetsHistory state -> do
          -- As above, retry only PostgreSQL's exact invalid index artifact and
          -- keep the drop concurrent so event ingestion remains available.
          _ <- execute_ conn candleEventIndexDropStatement
          buildCandleEventIndex conn
      | otherwise ->
          fail $ candleEventIndexConflictMessage state

ensureCandleActivityReorgIndex :: Connection -> IO ()
ensureCandleActivityReorgIndex conn = do
  indexState <- getCandleActivityReorgIndexState conn
  case indexState of
    Nothing -> do
      assertCandleActivityReorgIndexNameAvailable conn
      buildCandleActivityReorgIndex conn
    Just state
      | candleActivityReorgIndexUsable state -> pure ()
      | candleActivityIndexCatalogValid state ->
          fail $ candleActivityReorgIndexShapeConflictMessage state
      | candleActivityIndexTargetsHistory state -> do
          _ <- execute_ conn candleActivityReorgIndexDropStatement
          buildCandleActivityReorgIndex conn
      | otherwise ->
          fail $ candleActivityReorgIndexConflictMessage state

ensureCandleEventReorgIndex :: Connection -> IO ()
ensureCandleEventReorgIndex conn = do
  indexState <- getCandleEventReorgIndexState conn
  case indexState of
    Nothing -> do
      assertCandleEventReorgIndexNameAvailable conn
      buildCandleEventReorgIndex conn
    Just state
      | candleEventReorgIndexUsable state -> pure ()
      | candleActivityIndexCatalogValid state ->
          fail $ candleEventReorgIndexShapeConflictMessage state
      | candleEventIndexTargetsHistory state -> do
          _ <- execute_ conn candleEventReorgIndexDropStatement
          buildCandleEventReorgIndex conn
      | otherwise ->
          fail $ candleEventReorgIndexConflictMessage state

ensureCandleActivityIndexPrerequisites :: Connection -> IO ()
ensureCandleActivityIndexPrerequisites conn = do
  rows <-
    query conn
      "SELECT EXISTS (\
      \ SELECT 1 FROM pg_class table_relation \
      \ JOIN pg_namespace table_namespace ON table_namespace.oid = table_relation.relnamespace \
      \ WHERE table_namespace.nspname = current_schema() \
      \ AND table_relation.relname = 'perps_account_activity' \
      \ AND table_relation.relkind IN ('r','p')) \
      \AND (SELECT COUNT(*) FROM information_schema.columns \
      \ WHERE table_schema = current_schema() AND table_name = 'perps_account_activity' \
      \ AND column_name IN ('chain_id','release_router','timestamp','size_delta','price',\
      \                     'block_number','activity_type')) = 7"
      () :: IO [Only Bool]
  case rows of
    [Only True] -> pure ()
    _ ->
      fail $
        "Candle migration requires the Perps history table perps_account_activity "
          <> "with its current columns. Start the API or Perps indexer once to run "
          <> "ensurePerpsHistorySchema, then retry plether-candle-admin migrate."

ensureCandleEventIndexPrerequisites :: Connection -> IO ()
ensureCandleEventIndexPrerequisites conn = do
  rows <-
    query conn
      "SELECT EXISTS (\
      \ SELECT 1 FROM pg_class table_relation \
      \ JOIN pg_namespace table_namespace ON table_namespace.oid = table_relation.relnamespace \
      \ WHERE table_namespace.nspname = current_schema() \
      \ AND table_relation.relname = 'perps_events' \
      \ AND table_relation.relkind IN ('r','p')) \
      \AND (SELECT COUNT(*) FROM information_schema.columns \
      \ WHERE table_schema = current_schema() AND table_name = 'perps_events' \
      \ AND column_name IN ('chain_id','release_router','timestamp','block_number')) = 4"
      () :: IO [Only Bool]
  case rows of
    [Only True] -> pure ()
    _ ->
      fail $
        "Candle migration requires the Perps history table perps_events "
          <> "with its current columns. Start the API or Perps indexer once to run "
          <> "ensurePerpsHistorySchema, then retry plether-candle-admin migrate."

data CandleActivityIndexState = CandleActivityIndexState
  { caisValid :: Bool
  , caisReady :: Bool
  , caisLive :: Bool
  , caisTableName :: Text
  , caisKeyAttributeCount :: Int
  , caisAttributeCount :: Int
  , caisAttribute1 :: Text
  , caisAttribute2 :: Text
  , caisAttribute3 :: Text
  , caisAttribute4 :: Text
  , caisAttribute5 :: Text
  , caisAttribute6 :: Text
  , caisPredicate :: Maybe Text
  }
  deriving stock (Show)

instance FromRow CandleActivityIndexState where
  fromRow =
    CandleActivityIndexState
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

getCandleActivityIndexState :: Connection -> IO (Maybe CandleActivityIndexState)
getCandleActivityIndexState conn = do
  rows <-
    query conn
      "SELECT index_state.indisvalid, index_state.indisready, index_state.indislive, \
      \ target_relation.relname, index_state.indnkeyatts::integer, \
      \ index_state.indnatts::integer, \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 1, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 2, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 3, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 4, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 5, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 6, TRUE), ''), \
      \ pg_get_expr(index_state.indpred, index_state.indrelid, TRUE) \
      \FROM pg_class index_relation \
      \JOIN pg_namespace index_namespace ON index_namespace.oid = index_relation.relnamespace \
      \JOIN pg_index index_state ON index_state.indexrelid = index_relation.oid \
      \JOIN pg_class target_relation ON target_relation.oid = index_state.indrelid \
      \WHERE index_namespace.nspname = current_schema() \
      \AND index_relation.relname = 'idx_perps_account_activity_volume_rollup'"
      ()
  case rows of
    [] -> pure Nothing
    [state] -> pure $ Just state
    _ -> fail "Candle activity index lookup returned more than one relation"

getCandleEventIndexState :: Connection -> IO (Maybe CandleActivityIndexState)
getCandleEventIndexState conn = do
  rows <-
    query conn
      "SELECT index_state.indisvalid, index_state.indisready, index_state.indislive, \
      \ target_relation.relname, index_state.indnkeyatts::integer, \
      \ index_state.indnatts::integer, \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 1, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 2, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 3, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 4, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 5, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 6, TRUE), ''), \
      \ pg_get_expr(index_state.indpred, index_state.indrelid, TRUE) \
      \FROM pg_class index_relation \
      \JOIN pg_namespace index_namespace ON index_namespace.oid = index_relation.relnamespace \
      \JOIN pg_index index_state ON index_state.indexrelid = index_relation.oid \
      \JOIN pg_class target_relation ON target_relation.oid = index_state.indrelid \
      \WHERE index_namespace.nspname = current_schema() \
      \AND index_relation.relname = 'idx_perps_events_candle_bounds'"
      ()
  case rows of
    [] -> pure Nothing
    [state] -> pure $ Just state
    _ -> fail "Candle event index lookup returned more than one relation"

getCandleActivityReorgIndexState :: Connection -> IO (Maybe CandleActivityIndexState)
getCandleActivityReorgIndexState conn =
  getCandleNamedIndexState conn
    "idx_perps_account_activity_reorg_blocks"
    "Candle activity reorg index lookup returned more than one relation"

getCandleEventReorgIndexState :: Connection -> IO (Maybe CandleActivityIndexState)
getCandleEventReorgIndexState conn =
  getCandleNamedIndexState conn
    "idx_perps_events_reorg_blocks"
    "Candle event reorg index lookup returned more than one relation"

getCandleNamedIndexState
  :: Connection -> Text -> String -> IO (Maybe CandleActivityIndexState)
getCandleNamedIndexState conn indexName duplicateMessage = do
  rows <-
    query conn
      "SELECT index_state.indisvalid, index_state.indisready, index_state.indislive, \
      \ target_relation.relname, index_state.indnkeyatts::integer, \
      \ index_state.indnatts::integer, \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 1, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 2, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 3, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 4, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 5, TRUE), ''), \
      \ COALESCE(pg_get_indexdef(index_relation.oid, 6, TRUE), ''), \
      \ pg_get_expr(index_state.indpred, index_state.indrelid, TRUE) \
      \FROM pg_class index_relation \
      \JOIN pg_namespace index_namespace ON index_namespace.oid = index_relation.relnamespace \
      \JOIN pg_index index_state ON index_state.indexrelid = index_relation.oid \
      \JOIN pg_class target_relation ON target_relation.oid = index_state.indrelid \
      \WHERE index_namespace.nspname = current_schema() \
      \AND index_relation.relname = ?"
      (Only indexName)
  case rows of
    [] -> pure Nothing
    [state] -> pure $ Just state
    _ -> fail duplicateMessage

assertCandleActivityIndexNameAvailable :: Connection -> IO ()
assertCandleActivityIndexNameAvailable conn = do
  rows <-
    query conn
      "SELECT relation.relkind::text FROM pg_class relation \
      \JOIN pg_namespace namespace ON namespace.oid = relation.relnamespace \
      \WHERE namespace.nspname = current_schema() \
      \AND relation.relname = 'idx_perps_account_activity_volume_rollup'"
      () :: IO [Only Text]
  case rows of
    [] -> pure ()
    [Only relationKind] ->
      fail $
        "Relation idx_perps_account_activity_volume_rollup already exists in the current schema "
          <> "but is not a PostgreSQL index catalog entry (relkind="
          <> T.unpack relationKind
          <> "). Refusing to drop it."
    _ -> fail "Candle activity index name lookup returned more than one relation"

assertCandleEventIndexNameAvailable :: Connection -> IO ()
assertCandleEventIndexNameAvailable conn = do
  rows <-
    query conn
      "SELECT relation.relkind::text FROM pg_class relation \
      \JOIN pg_namespace namespace ON namespace.oid = relation.relnamespace \
      \WHERE namespace.nspname = current_schema() \
      \AND relation.relname = 'idx_perps_events_candle_bounds'"
      () :: IO [Only Text]
  case rows of
    [] -> pure ()
    [Only relationKind] ->
      fail $
        "Relation idx_perps_events_candle_bounds already exists in the current schema "
          <> "but is not a PostgreSQL index catalog entry (relkind="
          <> T.unpack relationKind
          <> "). Refusing to drop it."
    _ -> fail "Candle event index name lookup returned more than one relation"

assertCandleActivityReorgIndexNameAvailable :: Connection -> IO ()
assertCandleActivityReorgIndexNameAvailable conn =
  assertCandleNamedIndexNameAvailable
    conn
    "idx_perps_account_activity_reorg_blocks"
    "Candle activity reorg index"

assertCandleEventReorgIndexNameAvailable :: Connection -> IO ()
assertCandleEventReorgIndexNameAvailable conn =
  assertCandleNamedIndexNameAvailable
    conn
    "idx_perps_events_reorg_blocks"
    "Candle event reorg index"

assertCandleNamedIndexNameAvailable :: Connection -> Text -> String -> IO ()
assertCandleNamedIndexNameAvailable conn indexName label = do
  rows <-
    query conn
      "SELECT relation.relkind::text FROM pg_class relation \
      \JOIN pg_namespace namespace ON namespace.oid = relation.relnamespace \
      \WHERE namespace.nspname = current_schema() \
      \AND relation.relname = ?"
      (Only indexName) :: IO [Only Text]
  case rows of
    [] -> pure ()
    [Only relationKind] ->
      fail $
        "Relation "
          <> T.unpack indexName
          <> " already exists in the current schema but is not a PostgreSQL index "
          <> "catalog entry (relkind="
          <> T.unpack relationKind
          <> "). Refusing to drop it."
    _ -> fail $ label <> " name lookup returned more than one relation"

candleActivityIndexTargetsHistory :: CandleActivityIndexState -> Bool
candleActivityIndexTargetsHistory state =
  caisTableName state == "perps_account_activity"

candleEventIndexTargetsHistory :: CandleActivityIndexState -> Bool
candleEventIndexTargetsHistory state =
  caisTableName state == "perps_events"

candleActivityIndexUsable :: CandleActivityIndexState -> Bool
candleActivityIndexUsable state =
  candleActivityIndexTargetsHistory state
    && candleActivityIndexCatalogValid state
    && candleActivityIndexHasExpectedShape state

candleEventIndexUsable :: CandleActivityIndexState -> Bool
candleEventIndexUsable state =
  candleEventIndexTargetsHistory state
    && candleActivityIndexCatalogValid state
    && candleEventIndexHasExpectedShape state

candleActivityReorgIndexUsable :: CandleActivityIndexState -> Bool
candleActivityReorgIndexUsable state =
  candleActivityIndexTargetsHistory state
    && candleActivityIndexCatalogValid state
    && candleActivityReorgIndexHasExpectedShape state

candleEventReorgIndexUsable :: CandleActivityIndexState -> Bool
candleEventReorgIndexUsable state =
  candleEventIndexTargetsHistory state
    && candleActivityIndexCatalogValid state
    && candleEventReorgIndexHasExpectedShape state

candleActivityIndexCatalogValid :: CandleActivityIndexState -> Bool
candleActivityIndexCatalogValid state =
  caisValid state && caisReady state && caisLive state

candleActivityIndexHasExpectedShape :: CandleActivityIndexState -> Bool
candleActivityIndexHasExpectedShape state =
  caisKeyAttributeCount state == 3
    && caisAttributeCount state == 6
    && map normalizeIndexAttribute
      [ caisAttribute1 state
      , caisAttribute2 state
      , caisAttribute3 state
      , caisAttribute4 state
      , caisAttribute5 state
      , caisAttribute6 state
      ]
      == ["chain_id", "release_router", "timestamp", "size_delta", "price", "block_number"]
    && maybe False hasExpectedCandleActivityPredicate (caisPredicate state)

candleEventIndexHasExpectedShape :: CandleActivityIndexState -> Bool
candleEventIndexHasExpectedShape state =
  caisKeyAttributeCount state == 3
    && caisAttributeCount state == 3
    && map normalizeIndexAttribute
      [caisAttribute1 state, caisAttribute2 state, caisAttribute3 state]
      == ["chain_id", "release_router", "timestamp"]
    && caisPredicate state == Nothing

candleActivityReorgIndexHasExpectedShape :: CandleActivityIndexState -> Bool
candleActivityReorgIndexHasExpectedShape state =
  caisKeyAttributeCount state == 3
    && caisAttributeCount state == 4
    && map normalizeIndexAttribute
      [caisAttribute1 state, caisAttribute2 state, caisAttribute3 state, caisAttribute4 state]
      == ["chain_id", "release_router", "block_number", "timestamp"]
    && caisPredicate state == Nothing

candleEventReorgIndexHasExpectedShape :: CandleActivityIndexState -> Bool
candleEventReorgIndexHasExpectedShape state =
  caisKeyAttributeCount state == 3
    && caisAttributeCount state == 3
    && map normalizeIndexAttribute
      [caisAttribute1 state, caisAttribute2 state, caisAttribute3 state]
      == ["chain_id", "release_router", "block_number"]
    && caisPredicate state == Nothing

normalizeIndexAttribute :: Text -> Text
normalizeIndexAttribute = T.toLower . T.filter (`notElem` ['"', ' '])

hasExpectedCandleActivityPredicate :: Text -> Bool
hasExpectedCandleActivityPredicate predicate =
  T.toLower (T.filter (`notElem` ['"', ' ', '\n', '\r', '\t', '(', ')']) predicate)
    == "activity_type=anyarray['open'::text,'close'::text,'liquidated'::text]andsize_deltaisnotnullandpriceisnotnull"

candleActivityIndexConflictMessage :: CandleActivityIndexState -> String
candleActivityIndexConflictMessage state =
  "Relation idx_perps_account_activity_volume_rollup already exists in the current schema "
    <> "but does not target perps_account_activity. Refusing to drop an unrelated relation: "
    <> show state

candleActivityIndexShapeConflictMessage :: CandleActivityIndexState -> String
candleActivityIndexShapeConflictMessage state =
  "A valid relation named idx_perps_account_activity_volume_rollup exists, but it does not "
    <> "match the required candle volume-rollup index. Refusing to replace a valid index "
    <> "automatically: "
    <> show state

candleEventIndexConflictMessage :: CandleActivityIndexState -> String
candleEventIndexConflictMessage state =
  "Relation idx_perps_events_candle_bounds already exists in the current schema "
    <> "but does not target perps_events. Refusing to drop an unrelated relation: "
    <> show state

candleEventIndexShapeConflictMessage :: CandleActivityIndexState -> String
candleEventIndexShapeConflictMessage state =
  "A valid relation named idx_perps_events_candle_bounds exists, but it does not "
    <> "match the required candle source-bounds index. Refusing to replace a valid index "
    <> "automatically: "
    <> show state

candleActivityReorgIndexConflictMessage :: CandleActivityIndexState -> String
candleActivityReorgIndexConflictMessage state =
  "Relation idx_perps_account_activity_reorg_blocks already exists in the current schema "
    <> "but does not target perps_account_activity. Refusing to drop an unrelated relation: "
    <> show state

candleActivityReorgIndexShapeConflictMessage :: CandleActivityIndexState -> String
candleActivityReorgIndexShapeConflictMessage state =
  "A valid relation named idx_perps_account_activity_reorg_blocks exists, but it does not "
    <> "match the required candle reorg activity index. Refusing to replace a valid index "
    <> "automatically: "
    <> show state

candleEventReorgIndexConflictMessage :: CandleActivityIndexState -> String
candleEventReorgIndexConflictMessage state =
  "Relation idx_perps_events_reorg_blocks already exists in the current schema "
    <> "but does not target perps_events. Refusing to drop an unrelated relation: "
    <> show state

candleEventReorgIndexShapeConflictMessage :: CandleActivityIndexState -> String
candleEventReorgIndexShapeConflictMessage state =
  "A valid relation named idx_perps_events_reorg_blocks exists, but it does not "
    <> "match the required candle reorg event index. Refusing to replace a valid index "
    <> "automatically: "
    <> show state

buildCandleActivityIndex :: Connection -> IO ()
buildCandleActivityIndex conn = do
  -- PostgreSQL forbids CONCURRENTLY inside a transaction. Callers receive its
  -- explicit error if they wrap the migration in one.
  _ <- execute_ conn candleActivityIndexStatement
  verified <- getCandleActivityIndexState conn
  case verified of
    Just state
      | candleActivityIndexUsable state -> pure ()
      | otherwise ->
          fail $
            "Candle activity index build completed without a valid expected index: "
              <> show state
    Nothing -> fail "Candle activity index build completed but the index is absent"

buildCandleEventIndex :: Connection -> IO ()
buildCandleEventIndex conn = do
  _ <- execute_ conn candleEventIndexStatement
  verified <- getCandleEventIndexState conn
  case verified of
    Just state
      | candleEventIndexUsable state -> pure ()
      | otherwise ->
          fail $
            "Candle event index build completed without a valid expected index: "
              <> show state
    Nothing -> fail "Candle event index build completed but the index is absent"

buildCandleActivityReorgIndex :: Connection -> IO ()
buildCandleActivityReorgIndex conn = do
  _ <- execute_ conn candleActivityReorgIndexStatement
  verified <- getCandleActivityReorgIndexState conn
  case verified of
    Just state
      | candleActivityReorgIndexUsable state -> pure ()
      | otherwise ->
          fail $
            "Candle activity reorg index build completed without a valid expected index: "
              <> show state
    Nothing -> fail "Candle activity reorg index build completed but the index is absent"

buildCandleEventReorgIndex :: Connection -> IO ()
buildCandleEventReorgIndex conn = do
  _ <- execute_ conn candleEventReorgIndexStatement
  verified <- getCandleEventReorgIndexState conn
  case verified of
    Just state
      | candleEventReorgIndexUsable state -> pure ()
      | otherwise ->
          fail $
            "Candle event reorg index build completed without a valid expected index: "
              <> show state
    Nothing -> fail "Candle event reorg index build completed but the index is absent"

ensureCurrentBasketDefinition :: Connection -> Text -> IO ()
ensureCurrentBasketDefinition conn seriesId = do
  let configuration = basketDefinitionConfiguration
      configurationHash = hashConfiguration configuration
  _ <- execute conn
    "INSERT INTO perps_basket_definitions \
    \(series_id, definition_version, configuration_hash, configuration, effective_from, active) \
    \VALUES (?, 'v1', ?, ?, 0, TRUE) \
    \ON CONFLICT (series_id) DO NOTHING"
    (seriesId, configurationHash, encode configuration)
  rows <- query conn
    "SELECT 1::BIGINT FROM perps_basket_definitions \
    \WHERE series_id = ? AND definition_version = 'v1' \
    \AND configuration_hash = ? AND configuration = ?::jsonb \
    \AND effective_from = 0 AND effective_to IS NULL AND active"
    (seriesId, configurationHash, encode configuration) :: IO [Only Integer]
  case rows of
    [_] -> pure ()
    _ -> fail "Basket definition identity conflicts with the compiled immutable v1 configuration"

upsertBasketObservation :: Connection -> BasketObservationInput -> IO Bool
upsertBasketObservation conn BasketObservationInput {..} = do
  domainRows <- query conn
    "SELECT 1::BIGINT FROM perps_basket_definitions \
    \WHERE series_id = ? AND active AND effective_to IS NULL \
    \AND ? > 0 AND ? < (configuration ->> 'priceCap')::BIGINT"
    (boiSeriesId, boiBasketPrice, boiBasketPrice) :: IO [Only Integer]
  unless (length domainRows == 1) $
    fail "Basket observation is outside its active immutable definition's display domain"
  assertObservationPublishTime conn boiSeriesId boiObservationId boiPublishTime
  changed <- query conn
    "INSERT INTO perps_basket_observations \
    \(series_id, observation_id, publish_time, basket_price, component_prices, source, source_priority) \
    \VALUES (?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (series_id, observation_id) DO UPDATE SET \
    \ basket_price = EXCLUDED.basket_price, \
    \ component_prices = EXCLUDED.component_prices, source = EXCLUDED.source, \
    \ source_priority = EXCLUDED.source_priority, updated_at = NOW() \
    \WHERE perps_basket_observations.publish_time = EXCLUDED.publish_time \
    \AND perps_basket_observations.source_priority <= EXCLUDED.source_priority \
    \AND (perps_basket_observations.publish_time, perps_basket_observations.basket_price, \
    \     perps_basket_observations.component_prices, perps_basket_observations.source, \
    \     perps_basket_observations.source_priority) \
    \ IS DISTINCT FROM \
    \    (EXCLUDED.publish_time, EXCLUDED.basket_price, EXCLUDED.component_prices, \
    \     EXCLUDED.source, EXCLUDED.source_priority) \
    \RETURNING 1::BIGINT"
    ( boiSeriesId
    , boiObservationId
    , boiPublishTime
    , boiBasketPrice
    , encode boiComponentPrices
    , boiSource
    , boiSourcePriority
    ) :: IO [Only Integer]
  -- The preflight check gives callers an actionable error in the normal case;
  -- this post-conflict check closes the race where two first deliveries of the
  -- same identity arrive concurrently with different source times.
  unless (not $ null changed) $
    assertObservationPublishTime conn boiSeriesId boiObservationId boiPublishTime
  pure $ not $ null changed

assertObservationPublishTime :: Connection -> Text -> Text -> Integer -> IO ()
assertObservationPublishTime conn seriesId observationId publishTime = do
  existingTimes <- query conn
    "SELECT publish_time FROM perps_basket_observations \
    \WHERE series_id = ? AND observation_id = ?"
    (seriesId, observationId) :: IO [Only Integer]
  case existingTimes of
    [] -> pure ()
    [Only existingTime]
      | existingTime == publishTime -> pure ()
      | otherwise ->
          fail "Basket observation identity cannot move to a different publish time"
    _ -> fail "Basket observation identity lookup returned duplicate rows"

recomputeBasketCandleHierarchy :: Connection -> Text -> Integer -> Integer -> IO ()
recomputeBasketCandleHierarchy conn seriesId publishTime latenessSeconds = do
  lockDataset conn "price" seriesId 0
  lockBucket conn "price" seriesId 60 $ alignDown publishTime 60
  minuteChanged <- replacePriceMinute conn seriesId (alignDown publishTime 60) latenessSeconds
  forM_ (drop 1 canonicalCandleIntervals) $ \interval -> do
    -- Different minute writers can share a parent candle. Serialize each
    -- affected bucket, in ascending interval order, to avoid lost updates.
    lockBucket conn "price" seriesId interval $ alignDown publishTime interval
    replacePriceParent conn seriesId interval (alignDown publishTime interval) latenessSeconds
  whenChanged minuteChanged $
    bumpCorrectionGeneration conn PriceRollup (Just seriesId) Nothing Nothing publishTime

-- A successful latest-source poll is an explicit watermark: it proves there
-- were no missing source updates up to checkedThrough, including weekends. A
-- short polling gap can extend complete coverage, but a large unchecked gap is
-- never silently bridged; it disables reads until an admin repair republishes
-- the range.
advanceBasketPriceCoverage :: Connection -> Text -> Integer -> Integer -> IO ()
advanceBasketPriceCoverage conn seriesId checkedThrough latenessSeconds = do
  -- Watermarks are writes to the immutable series just as observations are.
  -- Validate the compiled definition here so stale/no-update polls cannot
  -- bypass the configuration-hash fail-closed check in the observation path.
  ensureCurrentBasketDefinition conn seriesId
  lockDataset conn "price" seriesId 0
  let maximumPollGap = max 300 (max 0 latenessSeconds * 2)
  coverageRows <- query conn
    "SELECT interval_seconds, coverage_end, complete \
    \FROM perps_rollup_coverage \
    \WHERE kind = 'price' AND series_id = ? AND chain_id = 0 AND release_router = '' \
    \FOR UPDATE"
    (Only seriesId) :: IO [(Integer, Maybe Integer, Bool)]
  let anchors =
        [ (coverageEnd, complete)
        | (60, coverageEnd, complete) <- coverageRows
        ]
  let healthy = case anchors of
        [(Just anchorEnd, True)] -> checkedThrough <= anchorEnd + maximumPollGap
        _ -> False
      -- Once every published interval is already disabled, later polls cannot
      -- prove the missing range. Preserve that actionable state and its dataset
      -- identity until an administrator republishes coverage. Check the whole
      -- dataset: a missing/incomplete minute anchor must not leave a complete
      -- coarser interval readable.
      alreadyIncomplete =
        not (null coverageRows)
          && all (\(_, _, complete) -> not complete) coverageRows
  if healthy
    then forM_ canonicalCandleIntervals $ \interval ->
      advanceExistingCoverage
        conn PriceRollup (Just seriesId) Nothing Nothing interval
        checkedThrough (alignDown checkedThrough interval) latenessSeconds
    else unless alreadyIncomplete $ do
      assertGenerationCapacity conn PriceRollup (Just seriesId) Nothing Nothing
      _ <- execute conn
        "UPDATE perps_rollup_coverage SET complete = FALSE, \
        \ generation = generation + 1, last_error = 'price_watermark_gap', \
        \ maintenance_from = NULL, maintenance_to = NULL, updated_at = NOW() \
        \WHERE kind = 'price' AND series_id = ? AND chain_id = 0 AND release_router = ''"
        (Only seriesId)
      pure ()

recomputeMarketVolumeHierarchy :: Connection -> Integer -> Text -> Integer -> Integer -> IO ()
recomputeMarketVolumeHierarchy conn chainId releaseRouter timestamp latenessSeconds =
  recomputeMarketVolumeHierarchyBatch conn chainId releaseRouter [timestamp] latenessSeconds

-- Rebuild a batch from canonical activity with bounded write amplification.
-- Dataset and bucket locks are acquired in deterministic interval/bucket order;
-- every minute and overlapping parent is recomputed once regardless of how
-- many trade events shared it. A finalized correction advances the dataset
-- generation once for the whole batch, never once per minute.
recomputeMarketVolumeHierarchyBatch
  :: Connection -> Integer -> Text -> [Integer] -> Integer -> IO ()
recomputeMarketVolumeHierarchyBatch conn chainId releaseRouter timestamps latenessSeconds = do
  let router = normalizeRouter releaseRouter
      minutes = Set.toAscList $ Set.fromList $ map (`alignDown` 60) timestamps
  unless (null minutes) $ do
    lockDataset conn "volume" router chainId
    changedMinutes <- fmap catMaybes $ forM minutes $ \minute -> do
      lockBucket conn "volume" router (chainId * 100_000 + 60) minute
      changed <- replaceVolumeMinute conn chainId router minute latenessSeconds
      pure $ if changed then Just minute else Nothing
    forM_ (drop 1 canonicalCandleIntervals) $ \interval ->
      forM_ (Set.toAscList $ Set.fromList $ map (`alignDown` interval) minutes) $ \parent -> do
        lockBucket conn "volume" router (chainId * 100_000 + interval) parent
        replaceVolumeParent conn chainId router interval parent latenessSeconds
    bumpCorrectionGenerationForTimestamps
      conn VolumeRollup Nothing (Just chainId) (Just router) changedMinutes

-- Bounded replay takes the indexer lock before this dataset lock. Exporting
-- only the exact volume-dataset lock keeps the operational lock order explicit
-- without exposing the generic advisory-lock namespace to callers.
lockMarketVolumeDataset :: Connection -> Integer -> Text -> IO ()
lockMarketVolumeDataset conn chainId releaseRouter =
  lockDataset conn "volume" (normalizeRouter releaseRouter) chainId

-- Call once after every successfully committed canonical indexer batch, even
-- when the batch contained no trades. This proves zero-volume ranges complete
-- without manufacturing rollup rows (reads represent them as zero).
advanceMarketVolumeCoverage :: Connection -> Integer -> Text -> Integer -> Integer -> IO ()
advanceMarketVolumeCoverage conn chainId releaseRouter indexedThrough latenessSeconds = do
  let router = normalizeRouter releaseRouter
  lockDataset conn "volume" router chainId
  forM_ canonicalCandleIntervals $ \interval -> do
    recoverReorgCoverage conn chainId router interval indexedThrough latenessSeconds
    advanceExistingCoverage
      conn VolumeRollup Nothing (Just chainId) (Just router) interval
      indexedThrough (alignDown indexedThrough interval) latenessSeconds

-- A reorg can remove the only trade in a bucket. Delete every rollup whose
-- source-block bounds cross the rewind, while preserving rows proven to come
-- solely from earlier blocks. The caller must delete the orphaned history and
-- recompute the returned minutes in the same transaction; that rebuilds every
-- affected parent from the retained canonical minute rows before replay starts.
invalidateMarketVolumeFromBlock :: Connection -> Integer -> Text -> Integer -> IO [Integer]
invalidateMarketVolumeFromBlock conn chainId releaseRouter rewindBlock = do
  let router = normalizeRouter releaseRouter
  lockDataset conn "volume" router chainId
  assertGenerationCapacity conn VolumeRollup Nothing (Just chainId) (Just router)
  affected <- query conn
    "WITH affected_minutes AS MATERIALIZED (\
    \ SELECT DISTINCT (timestamp / 60) * 60 AS minute \
    \ FROM perps_account_activity \
    \ WHERE chain_id = ? AND release_router = ? AND block_number >= ? \
    \ AND activity_type IN ('Open', 'Close', 'Liquidated') \
    \ AND size_delta IS NOT NULL AND price IS NOT NULL\
    \), deleted AS (\
    \ DELETE FROM perps_market_volume_rollups rollup USING affected_minutes affected \
    \ WHERE rollup.chain_id = ? AND rollup.release_router = ? \
    \ AND affected.minute >= rollup.bucket_start \
    \ AND affected.minute < rollup.bucket_start + rollup.interval_seconds \
    \ RETURNING 1\
    \), deletion_barrier AS (SELECT COUNT(*) FROM deleted) \
    \SELECT affected.minute FROM affected_minutes affected CROSS JOIN deletion_barrier \
    \ORDER BY affected.minute"
    (chainId, router, rewindBlock, chainId, router) :: IO [Only Integer]
  _ <- execute conn
    "WITH next_generation AS (\
    \ SELECT COALESCE(MAX(generation), 0) + 1 AS generation \
    \ FROM perps_rollup_coverage WHERE kind = 'volume' \
    \ AND series_id = '' AND chain_id = ? AND release_router = ?\
    \) UPDATE perps_rollup_coverage coverage SET complete = FALSE, \
    \ last_error = 'chain_reorg', generation = next_generation.generation, \
    \ maintenance_from = NULL, maintenance_to = NULL, \
    \ finalized_through = coverage.coverage_start, updated_at = NOW() \
    \FROM next_generation WHERE coverage.kind = 'volume' AND coverage.series_id = '' \
    \AND coverage.chain_id = ? AND coverage.release_router = ?"
    (chainId, router, chainId, router)
  pure [timestamp | Only timestamp <- affected]

getActiveBasketSeriesId :: Connection -> Integer -> IO (Maybe Text)
getActiveBasketSeriesId conn timestamp =
  fmap bdiSeriesId <$> getActiveBasketDefinitionIdentity conn timestamp

getActiveBasketDefinitionIdentity :: Connection -> Integer -> IO (Maybe BasketDefinitionIdentity)
getActiveBasketDefinitionIdentity conn timestamp = do
  rows <- query conn
    "SELECT series_id, configuration_hash, (configuration ->> 'priceCap')::BIGINT, \
    \effective_from, effective_to \
    \FROM perps_basket_definitions \
    \WHERE active AND effective_from <= ? \
    \AND (effective_to IS NULL OR effective_to > ?) \
    \ORDER BY effective_from DESC, series_id ASC LIMIT 1"
    (timestamp, timestamp) :: IO [BasketDefinitionIdentity]
  pure $ listToMaybe rows

instance FromRow BasketDefinitionIdentity where
  fromRow = BasketDefinitionIdentity <$> field <*> field <*> field <*> field <*> field

basketDefinitionConfiguration :: Value
basketDefinitionConfiguration =
  object
    [ "derivationVersion" .= ("v1" :: Text)
    , "priceCap" .= basketDisplayPriceCap
    , "decimals" .= (8 :: Int)
    , "components" .= map componentConfiguration basketComponents
    ]
 where
  componentConfiguration BasketComponent {..} =
    object
      [ "symbol" .= bcSymbol
      , "feedSymbol" .= bcFeedSymbol
      , "feedId" .= T.toLower bcFeedId
      , "weight" .= bcWeight
      , "basePrice" .= bcBasePrice
      , "inverted" .= bcInverted
      ]

hashConfiguration :: Value -> Text
hashConfiguration configuration =
  "sha256:" <> T.pack (show (hashlazy (encode configuration) :: Digest SHA256))

getBasketCandlePage
  :: Connection -> Text -> Integer -> Text -> Integer -> Integer -> IO CandlePage
getBasketCandlePage conn seriesId chainId releaseRouter interval cursor = do
  let pageSpan = interval * 500
      pageStart = cursor - pageSpan
      router = normalizeRouter releaseRouter
  metadata <- getCombinedMetadata conn seriesId chainId router interval
  case metadata of
    Nothing -> pure emptyPage
    Just CombinedMetadata {..} -> do
      let effectiveStart = max pageStart cmCoverageStart
          effectiveEnd = min cursor $ min cmCoverageEnd cmFinalizedThrough
      candles <-
        if not cmComplete || effectiveStart >= effectiveEnd
          then pure []
          else query conn candleRowsSql
            (chainId, router, seriesId, interval, effectiveStart, effectiveEnd)
      earlierRows <- query conn
        "SELECT bucket_start FROM perps_basket_candles \
        \WHERE series_id = ? AND interval_seconds = ? \
        \AND bucket_start >= ? AND bucket_start < ? \
        \ORDER BY bucket_start DESC LIMIT 1"
        (seriesId, interval, cmCoverageStart, effectiveStart) :: IO [Only Integer]
      let mEarlierBucket = fromOnly <$> listToMaybe earlierRows
          previousCursor =
            (\bucket -> (bucket `div` pageSpan + 1) * pageSpan) <$> mEarlierBucket
      pure
        CandlePage
          { cpCandles = candles
          , cpPreviousCursor = previousCursor
          , cpHasEarlier = maybe False (const True) mEarlierBucket
          , cpCoverageStart = Just cmCoverageStart
          , cpCoverageEnd = Just cmCoverageEnd
          , cpFinalizedThrough = Just cmFinalizedThrough
          , cpDatasetGeneration = cmGeneration
          , cpCoverageComplete = cmComplete
          }
 where
  emptyPage =
    CandlePage [] Nothing False Nothing Nothing Nothing 0 False

getBasketCandleRange
  :: Connection
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Int
  -> IO CandleRange
getBasketCandleRange conn seriesId chainId releaseRouter interval rangeStart rangeEnd maxRows = do
  let router = normalizeRouter releaseRouter
      boundedLimit = max 1 maxRows
  metadata <- getCombinedMetadata conn seriesId chainId router interval
  case metadata of
    Nothing -> pure emptyRange
    Just CombinedMetadata {..} -> do
      let effectiveStart = max rangeStart cmCoverageStart
          effectiveEnd = minimum [rangeEnd, cmCoverageEnd, cmFinalizedThrough]
      candles <-
        if not cmComplete || effectiveStart >= effectiveEnd
          then pure []
          else query conn candleRangeRowsSql
            (chainId, router, seriesId, interval, effectiveStart, effectiveEnd, boundedLimit)
      pure
        CandleRange
          { crCandles = candles
          , crCoverageStart = Just cmCoverageStart
          , crCoverageEnd = Just cmCoverageEnd
          , crFinalizedThrough = Just cmFinalizedThrough
          , crDatasetGeneration = cmGeneration
          , crCoverageComplete = cmComplete
          }
 where
  emptyRange =
    CandleRange [] Nothing Nothing Nothing 0 False

getCurrentBasketCandle
  :: Connection -> Text -> Integer -> Text -> Integer -> Integer -> IO CandleCurrent
getCurrentBasketCandle conn seriesId chainId releaseRouter interval now = do
  let bucketStart = alignDown now interval
      router = normalizeRouter releaseRouter
  metadata <- getCombinedMetadata conn seriesId chainId router interval
  rows <- query conn currentCandleRowSql
    (chainId, router, seriesId, interval, bucketStart) :: IO [BasketCandleRow]
  pure
    CandleCurrent
      { ccCandle = listToMaybe rows
      , ccCoverageStart = cmCoverageStart <$> metadata
      , ccCoverageEnd = cmCoverageEnd <$> metadata
      , ccFinalizedThrough = cmFinalizedThrough <$> metadata
      , ccDatasetGeneration = maybe 0 cmGeneration metadata
      , ccCoverageComplete = maybe False cmComplete metadata
      }

-- Range replacement is intentional: a repair must remove stale buckets whose
-- source rows disappeared, rather than only upserting rows that still exist.
backfillLegacyBasketSnapshots :: Connection -> Text -> Integer -> Integer -> IO Integer
backfillLegacyBasketSnapshots conn seriesId fromTimestamp toTimestamp = do
  validateBackfillRange fromTimestamp toTimestamp
  ensureCurrentBasketDefinition conn seriesId
  lockDataset conn "price" seriesId 0
  lockRange conn "price-backfill" seriesId fromTimestamp toTimestamp
  invalidSources <- query conn
    "SELECT COUNT(*)::BIGINT FROM (\
    \ SELECT o.basket_price FROM perps_basket_observations o \
    \ WHERE o.series_id = ? AND o.publish_time >= ? AND o.publish_time < ? \
    \ UNION ALL SELECT s.basket_price FROM perps_basket_snapshots s \
    \ WHERE s.timestamp >= ? AND s.timestamp < ?) source_values \
    \CROSS JOIN perps_basket_definitions d WHERE d.series_id = ? \
    \AND (source_values.basket_price <= 0 OR source_values.basket_price >= \
    \ (d.configuration ->> 'priceCap')::BIGINT)"
    (seriesId, fromTimestamp, toTimestamp, fromTimestamp, toTimestamp, seriesId)
    :: IO [Only Integer]
  unless (invalidSources == [Only 0]) $
    fail "Perps candle backfill encountered a source price outside the immutable display domain"
  _ <- execute conn
    "DELETE FROM perps_basket_candles WHERE series_id = ? \
    \AND ((interval_seconds = 60 AND bucket_start >= ? AND bucket_start < ?) \
    \ OR (interval_seconds <> 60 AND bucket_start < ? \
    \     AND bucket_start + interval_seconds > ?))"
    (seriesId, fromTimestamp, toTimestamp, toTimestamp, fromTimestamp)
  affected <- execute conn legacyMinuteBackfillSql
    (seriesId, fromTimestamp, toTimestamp)
  forM_ (drop 1 canonicalCandleIntervals) $ \interval -> do
    _ <- execute conn priceParentRangeSql
      (seriesId, interval, fromTimestamp, toTimestamp)
    pure ()
  pure $ fromIntegral affected

backfillMarketVolume :: Connection -> Integer -> Text -> Integer -> Integer -> IO Integer
backfillMarketVolume conn chainId releaseRouter fromTimestamp toTimestamp = do
  validateBackfillRange fromTimestamp toTimestamp
  let router = normalizeRouter releaseRouter
  lockDataset conn "volume" router chainId
  lockRange conn "volume-backfill" router fromTimestamp toTimestamp
  _ <- execute conn
    "DELETE FROM perps_market_volume_rollups WHERE chain_id = ? AND release_router = ? \
    \AND ((interval_seconds = 60 AND bucket_start >= ? AND bucket_start < ?) \
    \ OR (interval_seconds <> 60 AND bucket_start < ? \
    \     AND bucket_start + interval_seconds > ?))"
    (chainId, router, fromTimestamp, toTimestamp, toTimestamp, fromTimestamp)
  affected <- execute conn volumeMinuteBackfillSql
    (chainId, router, chainId, router, fromTimestamp, toTimestamp)
  forM_ (drop 1 canonicalCandleIntervals) $ \interval -> do
    _ <- execute conn volumeParentRangeSql
      (chainId, router, interval, fromTimestamp, toTimestamp)
    pure ()
  pure $ fromIntegral affected

countBasketCandles :: Connection -> Text -> Integer -> Integer -> Integer -> IO Integer
countBasketCandles conn seriesId interval fromTimestamp toTimestamp = do
  rows <- query conn
    "SELECT COUNT(*)::BIGINT FROM perps_basket_candles \
    \WHERE series_id = ? AND interval_seconds = ? \
    \AND bucket_start >= ? AND bucket_start < ?"
    (seriesId, interval, fromTimestamp, toTimestamp) :: IO [Only Integer]
  pure $ maybe 0 fromOnly $ listToMaybe rows

countMarketVolumeRollups :: Connection -> Integer -> Text -> Integer -> Integer -> Integer -> IO Integer
countMarketVolumeRollups conn chainId releaseRouter interval fromTimestamp toTimestamp = do
  rows <- query conn
    "SELECT COUNT(*)::BIGINT FROM perps_market_volume_rollups \
    \WHERE chain_id = ? AND release_router = ? AND interval_seconds = ? \
    \AND bucket_start >= ? AND bucket_start < ?"
    (chainId, normalizeRouter releaseRouter, interval, fromTimestamp, toTimestamp)
    :: IO [Only Integer]
  pure $ maybe 0 fromOnly $ listToMaybe rows

getRollupCoverage
  :: Connection -> RollupKind -> Maybe Text -> Maybe Integer -> Maybe Text -> Integer
  -> IO (Maybe RollupCoverage)
getRollupCoverage conn kind seriesId chainId releaseRouter interval = do
  rows <- query conn
    "SELECT coverage_start, coverage_end, finalized_through, generation, complete, \
    \derivation_version, last_error, maintenance_from, maintenance_to \
    \FROM perps_rollup_coverage \
    \WHERE kind = ? AND series_id = ? AND chain_id = ? AND release_router = ? \
    \AND interval_seconds = ?"
    ( rollupKindText kind
    , maybe "" id seriesId
    , maybe 0 id chainId
    , maybe "" normalizeRouter releaseRouter
    , interval
    ) :: IO [CoverageDbRow]
  pure $ coverageFromDb kind seriesId chainId (normalizeRouter <$> releaseRouter) interval <$> listToMaybe rows

-- Preserve seven ordered slots, including absence. Early rollout legitimately
-- has no published coverage rows; replay must prove it did not create or
-- mutate any of them.
getMarketVolumeCoverageSnapshot :: Connection -> Integer -> Text -> IO [Maybe RollupCoverage]
getMarketVolumeCoverageSnapshot conn chainId releaseRouter = do
  forM canonicalCandleIntervals $ \interval ->
    getRollupCoverage
      conn
      VolumeRollup
      Nothing
      (Just chainId)
      (Just releaseRouter)
      interval

-- Snapshot exactly the minute buckets touched by parsed volume activity and
-- every overlapping canonical parent. An absent bucket is represented by its
-- absence from this sorted list, so insertion/deletion is detected as well as
-- semantic row changes.
getMarketVolumeRollupSnapshot
  :: Connection -> Integer -> Text -> [Integer] -> IO [MarketVolumeRollupSnapshot]
getMarketVolumeRollupSnapshot conn chainId releaseRouter timestamps =
  fmap concat $
    forM canonicalCandleIntervals $ \interval -> do
      let buckets =
            Set.toAscList $
              Set.fromList $
                map (`alignDown` interval) timestamps
      if null buckets
        then pure []
        else
          query
            conn
            "SELECT interval_seconds, bucket_start, volume_numerator, trade_count, \
            \ first_source_block, last_source_block, revision, finalized \
            \FROM perps_market_volume_rollups \
            \WHERE chain_id = ? AND release_router = ? AND interval_seconds = ? \
            \AND bucket_start IN ? ORDER BY bucket_start"
            (chainId, normalizeRouter releaseRouter, interval, In buckets)

upsertRollupCoverage :: Connection -> RollupCoverage -> IO ()
upsertRollupCoverage conn RollupCoverage {..} = do
  unless (rcGeneration > 0 && rcGeneration < generationRadix) $
    fail "Perps candle dataset generation is outside the 26-bit range"
  validateMaintenanceBounds rcMaintenanceFrom rcMaintenanceTo
  validateMaintenanceState rcComplete rcLastError rcMaintenanceFrom rcMaintenanceTo
  _ <- execute conn
    "INSERT INTO perps_rollup_coverage \
    \(kind, series_id, chain_id, release_router, interval_seconds, coverage_start, coverage_end, \
    \ finalized_through, generation, complete, derivation_version, last_error, \
    \ maintenance_from, maintenance_to) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (kind, series_id, chain_id, release_router, interval_seconds) DO UPDATE SET \
    \ coverage_start = CASE WHEN EXCLUDED.generation >= perps_rollup_coverage.generation \
    \   THEN EXCLUDED.coverage_start ELSE perps_rollup_coverage.coverage_start END, \
    \ coverage_end = CASE WHEN EXCLUDED.generation >= perps_rollup_coverage.generation \
    \   THEN EXCLUDED.coverage_end ELSE perps_rollup_coverage.coverage_end END, \
    \ finalized_through = CASE WHEN EXCLUDED.generation >= perps_rollup_coverage.generation \
    \   THEN EXCLUDED.finalized_through ELSE perps_rollup_coverage.finalized_through END, \
    \ generation = GREATEST(perps_rollup_coverage.generation, EXCLUDED.generation), \
    \ complete = CASE WHEN EXCLUDED.generation >= perps_rollup_coverage.generation \
    \   THEN EXCLUDED.complete ELSE perps_rollup_coverage.complete END, \
    \ derivation_version = CASE WHEN EXCLUDED.generation >= perps_rollup_coverage.generation \
    \   THEN EXCLUDED.derivation_version ELSE perps_rollup_coverage.derivation_version END, \
    \ last_error = CASE WHEN EXCLUDED.generation >= perps_rollup_coverage.generation \
    \   THEN EXCLUDED.last_error ELSE perps_rollup_coverage.last_error END, \
    \ maintenance_from = CASE WHEN EXCLUDED.generation >= perps_rollup_coverage.generation \
    \   THEN EXCLUDED.maintenance_from ELSE perps_rollup_coverage.maintenance_from END, \
    \ maintenance_to = CASE WHEN EXCLUDED.generation >= perps_rollup_coverage.generation \
    \   THEN EXCLUDED.maintenance_to ELSE perps_rollup_coverage.maintenance_to END, \
    \ updated_at = NOW()"
    ( rollupKindText rcKind
    , maybe "" id rcSeriesId
    , maybe 0 id rcChainId
    , maybe "" normalizeRouter rcReleaseRouter
    , rcIntervalSeconds
    , rcCoverageStart
    , rcCoverageEnd
    , rcFinalizedThrough
    , rcGeneration
    , rcComplete
    , rcDerivationVersion
    , rcLastError
    , rcMaintenanceFrom
    , rcMaintenanceTo
    )
  pure ()

-- Atomically begin bounded maintenance for a complete canonical dataset. One
-- generation is allocated for every interval and the exact resumable domain is
-- persisted in the same statement as publication is disabled. No intermediate
-- marker-without-bounds state is observable, even outside a caller transaction.
beginRollupMaintenance
  :: Connection
  -> RollupKind
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Text
  -> Integer
  -> Integer
  -> IO Integer
beginRollupMaintenance conn kind seriesId chainId releaseRouter maintenanceFrom maintenanceTo = do
  validateMaintenanceBounds (Just maintenanceFrom) (Just maintenanceTo)
  let scope = maybe "" id seriesId <> maybe "" normalizeRouter releaseRouter
      discriminator = maybe 0 id chainId
      normalizedRouter = maybe "" normalizeRouter releaseRouter
  lockDataset conn (rollupKindText kind) scope discriminator
  rows <- query conn
    "WITH target AS MATERIALIZED (\
    \ SELECT interval_seconds, generation, complete, last_error, \
    \   maintenance_from, maintenance_to FROM perps_rollup_coverage \
    \ WHERE kind = ? AND series_id = ? AND chain_id = ? AND release_router = ? \
    \ FOR UPDATE), summary AS (\
    \ SELECT array_agg(interval_seconds ORDER BY interval_seconds) AS intervals, \
    \   MIN(generation) AS min_generation, MAX(generation) AS old_generation, \
    \   COALESCE(bool_and(complete AND last_error IS NULL \
    \     AND maintenance_from IS NULL AND maintenance_to IS NULL), FALSE) AS all_complete, \
    \   COALESCE(bool_and(NOT complete \
    \     AND last_error IS NOT DISTINCT FROM 'bounded_admin_repair' \
    \     AND maintenance_from IS NOT DISTINCT FROM ? \
    \     AND maintenance_to IS NOT DISTINCT FROM ?), FALSE) AS all_resuming \
    \ FROM target), eligible AS (\
    \ SELECT old_generation, all_complete AS starting_new, \
    \   intervals = ARRAY[60,180,300,900,1800,3600,86400]::BIGINT[] \
    \   AND min_generation = old_generation \
    \   AND (all_resuming OR (all_complete AND old_generation < 67108863)) AS ready \
    \ FROM summary \
    \), updated AS (\
    \ UPDATE perps_rollup_coverage coverage SET maintenance_from = ?, maintenance_to = ?, \
    \ complete = FALSE, last_error = 'bounded_admin_repair', \
    \ generation = eligible.old_generation + CASE WHEN eligible.starting_new THEN 1 ELSE 0 END, \
    \ updated_at = NOW() FROM eligible \
    \WHERE eligible.ready AND coverage.kind = ? AND coverage.series_id = ? \
    \ AND coverage.chain_id = ? AND coverage.release_router = ? \
    \ RETURNING coverage.generation) \
    \SELECT generation FROM updated"
    ( rollupKindText kind
    , maybe "" id seriesId
    , maybe 0 id chainId
    , normalizedRouter
    , maintenanceFrom
    , maintenanceTo
    , maintenanceFrom
    , maintenanceTo
    , rollupKindText kind
    , maybe "" id seriesId
    , maybe 0 id chainId
    , normalizedRouter
    ) :: IO [Only Integer]
  unless (length rows == length canonicalCandleIntervals) $
    fail "Perps candle maintenance bounds require every canonical coverage interval"
  case rows of
    Only generation : rest
      | all ((== generation) . fromOnly) rest -> pure generation
    _ -> fail "Perps candle maintenance requires one shared dataset generation"

validateMaintenanceBounds :: Maybe Integer -> Maybe Integer -> IO ()
validateMaintenanceBounds Nothing Nothing = pure ()
validateMaintenanceBounds (Just maintenanceFrom) (Just maintenanceTo)
  | maintenanceFrom >= 0
  , maintenanceTo > maintenanceFrom
  , maintenanceFrom `mod` 60 == 0
  , maintenanceTo `mod` 60 == 0 = pure ()
validateMaintenanceBounds _ _ =
  fail "Perps candle maintenance bounds must be paired, non-negative, increasing, and minute-aligned"

validateMaintenanceState
  :: Bool -> Maybe Text -> Maybe Integer -> Maybe Integer -> IO ()
validateMaintenanceState complete lastError maintenanceFrom maintenanceTo =
  case (lastError == Just "bounded_admin_repair", maintenanceFrom, maintenanceTo) of
    (False, Nothing, Nothing) -> pure ()
    (True, Just _, Just _) | not complete -> pure ()
    _ ->
      fail $ if complete
        then "Complete perps candle coverage cannot retain maintenance bounds"
        else "Bounded perps candle repair state requires explicit maintenance bounds"

markRollupCoverageIncomplete
  :: Connection -> RollupKind -> Maybe Text -> Maybe Integer -> Maybe Text -> Integer -> Text
  -> IO Integer
markRollupCoverageIncomplete conn kind seriesId chainId releaseRouter interval reason = do
  when (reason == "bounded_admin_repair") $
    fail "Use beginRollupMaintenance for bounded perps candle repairs"
  let scope = maybe "" id seriesId <> maybe "" normalizeRouter releaseRouter
      discriminator = maybe 0 id chainId
  lockDataset conn (rollupKindText kind) scope discriminator
  assertGenerationCapacity conn kind seriesId chainId releaseRouter
  rows <- query conn
    "INSERT INTO perps_rollup_coverage \
    \(kind, series_id, chain_id, release_router, interval_seconds, generation, complete, \
    \ derivation_version, last_error) \
    \VALUES (?, ?, ?, ?, ?, 1, FALSE, 'v1', ?) \
    \ON CONFLICT (kind, series_id, chain_id, release_router, interval_seconds) DO UPDATE SET \
    \ generation = perps_rollup_coverage.generation + 1, complete = FALSE, \
    \ last_error = EXCLUDED.last_error, maintenance_from = NULL, maintenance_to = NULL, \
    \ updated_at = NOW() \
    \RETURNING generation"
    ( rollupKindText kind
    , maybe "" id seriesId
    , maybe 0 id chainId
    , maybe "" normalizeRouter releaseRouter
    , interval
    , reason
    ) :: IO [Only Integer]
  pure $ maybe 0 fromOnly $ listToMaybe rows

-- Allocate one generation atomically across every interval after a verified
-- rebuild or repair. This prevents stale edge-cache generations being reused.
bumpRollupDatasetGeneration
  :: Connection -> RollupKind -> Maybe Text -> Maybe Integer -> Maybe Text -> IO Integer
bumpRollupDatasetGeneration conn kind seriesId chainId releaseRouter = do
  let scope = maybe "" id seriesId <> maybe "" normalizeRouter releaseRouter
      discriminator = maybe 0 id chainId
  lockDataset conn (rollupKindText kind) scope discriminator
  assertGenerationCapacity conn kind seriesId chainId releaseRouter
  rows <- query conn
    "WITH next AS (SELECT COALESCE(MAX(generation), 0) + 1 AS generation \
    \ FROM perps_rollup_coverage WHERE kind = ? AND series_id = ? \
    \ AND chain_id = ? AND release_router = ?), updated AS (\
    \ UPDATE perps_rollup_coverage c SET generation = n.generation, updated_at = NOW() \
    \ FROM next n WHERE c.kind = ? AND c.series_id = ? AND c.chain_id = ? \
    \ AND c.release_router = ? RETURNING n.generation) \
    \SELECT COALESCE(MAX(generation), 1)::bigint FROM updated"
    ( rollupKindText kind, maybe "" id seriesId, maybe 0 id chainId
    , maybe "" normalizeRouter releaseRouter, rollupKindText kind
    , maybe "" id seriesId, maybe 0 id chainId, maybe "" normalizeRouter releaseRouter
    ) :: IO [Only Integer]
  pure $ maybe 1 fromOnly $ listToMaybe rows

data CoverageDbRow = CoverageDbRow
  { cdrCoverageStart :: Maybe Integer
  , cdrCoverageEnd :: Maybe Integer
  , cdrFinalizedThrough :: Maybe Integer
  , cdrGeneration :: Integer
  , cdrComplete :: Bool
  , cdrDerivationVersion :: Text
  , cdrLastError :: Maybe Text
  , cdrMaintenanceFrom :: Maybe Integer
  , cdrMaintenanceTo :: Maybe Integer
  }

instance FromRow CoverageDbRow where
  fromRow =
    CoverageDbRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field

data CombinedMetadata = CombinedMetadata
  { cmCoverageStart :: Integer
  , cmCoverageEnd :: Integer
  , cmFinalizedThrough :: Integer
  , cmGeneration :: Integer
  , cmComplete :: Bool
  }

getCombinedMetadata
  :: Connection -> Text -> Integer -> Text -> Integer -> IO (Maybe CombinedMetadata)
getCombinedMetadata conn seriesId chainId releaseRouter interval = do
  rows <- query conn combinedMetadataSql
    (currentDerivationVersion, chainId, normalizeRouter releaseRouter, seriesId, interval)
  pure $ listToMaybe rows

instance FromRow CombinedMetadata where
  fromRow = CombinedMetadata <$> field <*> field <*> field <*> field <*> field

coverageFromDb
  :: RollupKind -> Maybe Text -> Maybe Integer -> Maybe Text -> Integer
  -> CoverageDbRow -> RollupCoverage
coverageFromDb kind seriesId chainId releaseRouter interval CoverageDbRow {..} =
  RollupCoverage
    { rcKind = kind
    , rcSeriesId = seriesId
    , rcChainId = chainId
    , rcReleaseRouter = releaseRouter
    , rcIntervalSeconds = interval
    , rcCoverageStart = cdrCoverageStart
    , rcCoverageEnd = cdrCoverageEnd
    , rcFinalizedThrough = cdrFinalizedThrough
    , rcGeneration = cdrGeneration
    , rcComplete = cdrComplete
    , rcDerivationVersion = cdrDerivationVersion
    , rcLastError = cdrLastError
    , rcMaintenanceFrom = cdrMaintenanceFrom
    , rcMaintenanceTo = cdrMaintenanceTo
    }

instance FromRow BasketCandleRow where
  fromRow = do
    bucketStart <- field
    openPrice <- field
    highPrice <- field
    lowPrice <- field
    closePrice <- field
    sampleCount <- field
    quality <- parseCandleQuality <$> field
    revision <- field
    priceComplete <- field
    volume <- fmap scientificToInteger <$> field
    trades <- field
    volumeComplete <- field
    pure $ BasketCandleRow bucketStart openPrice highPrice lowPrice closePrice sampleCount
      quality revision priceComplete volume trades volumeComplete

parseCandleQuality :: Text -> CandleQuality
parseCandleQuality = \case
  "observed" -> CandleObserved
  "legacy_sampled" -> CandleLegacySampled
  _ -> CandleMixed

scientificToInteger :: Scientific -> Integer
scientificToInteger value
  | decimalExponent >= 0 = coefficient value * 10 ^ decimalExponent
  | otherwise = coefficient value `div` (10 ^ negate decimalExponent)
 where
  decimalExponent = base10Exponent value

rollupKindText :: RollupKind -> Text
rollupKindText PriceRollup = "price"
rollupKindText VolumeRollup = "volume"

generationRadix :: Integer
generationRadix = 67_108_864

currentDerivationVersion :: Text
currentDerivationVersion = "v1"

normalizeRouter :: Text -> Text
normalizeRouter = T.toLower . T.strip

alignDown :: Integer -> Integer -> Integer
alignDown timestamp interval = timestamp - timestamp `mod` interval

validateBackfillRange :: Integer -> Integer -> IO ()
validateBackfillRange fromTimestamp toTimestamp =
  unless (fromTimestamp >= 0 && fromTimestamp < toTimestamp
    && fromTimestamp `mod` 60 == 0 && toTimestamp `mod` 60 == 0) $
    fail "Perps candle backfill range must be non-negative, increasing, and minute-aligned"

lockBucket :: Connection -> Text -> Text -> Integer -> Integer -> IO ()
lockBucket conn namespace scope discriminator bucket = do
  _ <- query conn
    "SELECT 1::BIGINT FROM (SELECT pg_advisory_xact_lock(hashtextextended(?, ?))) locked"
    (namespace <> ":" <> scope <> ":" <> T.pack (show discriminator), bucket)
    :: IO [Only Integer]
  pure ()

lockDataset :: Connection -> Text -> Text -> Integer -> IO ()
lockDataset conn namespace scope discriminator =
  lockBucket conn (namespace <> "-dataset") scope discriminator 0

whenChanged :: Bool -> IO () -> IO ()
whenChanged True action = action
whenChanged False _ = pure ()

bumpCorrectionGeneration
  :: Connection -> RollupKind -> Maybe Text -> Maybe Integer -> Maybe Text -> Integer -> IO ()
bumpCorrectionGeneration conn kind seriesId chainId releaseRouter sourceTimestamp =
  bumpCorrectionGenerationForTimestamps
    conn kind seriesId chainId releaseRouter [sourceTimestamp]

bumpCorrectionGenerationForTimestamps
  :: Connection -> RollupKind -> Maybe Text -> Maybe Integer -> Maybe Text -> [Integer] -> IO ()
bumpCorrectionGenerationForTimestamps conn kind seriesId chainId releaseRouter sourceTimestamps =
  unless (null sourceTimestamps) $ do
    finalizedRanges <- query conn
      "SELECT coverage_start, finalized_through FROM perps_rollup_coverage \
      \WHERE kind = ? AND series_id = ? AND chain_id = ? AND release_router = ? \
      \AND complete AND coverage_start IS NOT NULL AND finalized_through IS NOT NULL \
      \AND finalized_through > coverage_start"
      ( rollupKindText kind
      , maybe "" id seriesId
      , maybe 0 id chainId
      , maybe "" normalizeRouter releaseRouter
      ) :: IO [(Integer, Integer)]
    let impacted = any
          (\sourceTimestamp -> any
            (\(coverageStart, finalizedThrough) ->
              sourceTimestamp >= coverageStart && sourceTimestamp < finalizedThrough)
            finalizedRanges)
          sourceTimestamps
    -- A correction to any finalized interval changes the dataset identity for
    -- every interval. Allocate one shared generation so admin verification and
    -- edge-cache invalidation cannot observe a mixture of interval-local epochs
    -- while coarser parent buckets are still mutable.
    when impacted $ do
      _ <- bumpRollupDatasetGeneration conn kind seriesId chainId releaseRouter
      pure ()

assertGenerationCapacity
  :: Connection -> RollupKind -> Maybe Text -> Maybe Integer -> Maybe Text -> IO ()
assertGenerationCapacity conn kind seriesId chainId releaseRouter = do
  rows <- query conn
    "SELECT 1::BIGINT FROM perps_rollup_coverage \
    \WHERE kind = ? AND series_id = ? AND chain_id = ? AND release_router = ? \
    \AND generation >= 67108863 LIMIT 1"
    ( rollupKindText kind
    , maybe "" id seriesId
    , maybe 0 id chainId
    , maybe "" normalizeRouter releaseRouter
    ) :: IO [Only Integer]
  unless (null rows) $
    fail "Perps candle dataset generation exhausted; publish a new derivation version"

lockRange :: Connection -> Text -> Text -> Integer -> Integer -> IO ()
lockRange conn namespace scope fromTimestamp toTimestamp =
  lockBucket conn namespace scope fromTimestamp toTimestamp

-- Live writes extend only a complete coverage record published by the admin
-- backfill. A newly created or reorg-invalidated dataset cannot be promoted by
-- observing a single bucket. Append/finalization keeps the cache generation
-- stable; only a correction inside an already finalized range increments it.
advanceExistingCoverage
  :: Connection
  -> RollupKind
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Text
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
advanceExistingCoverage conn kind seriesId chainId releaseRouter interval sourceTimestamp candidateEnd latenessSeconds = do
  let lateness = max 0 latenessSeconds
  advanced <- query conn
    "WITH input AS (SELECT ?::text AS kind, ?::text AS series_id, \
    \ ?::bigint AS chain_id, ?::text AS release_router, ?::bigint AS interval_seconds, \
    \ ?::bigint AS source_timestamp, ?::bigint AS candidate_end, ?::bigint AS lateness), \
    \candidate AS (SELECT input.*, LEAST(\
    \ (source_timestamp / interval_seconds) * interval_seconds, \
    \ ((EXTRACT(EPOCH FROM NOW())::bigint - lateness) / interval_seconds) * interval_seconds\
    \) AS candidate_finalized FROM input), locked AS (\
    \ SELECT c.*, c.finalized_through AS old_finalized FROM perps_rollup_coverage c, input i \
    \ WHERE c.kind = i.kind AND c.series_id = i.series_id AND c.chain_id = i.chain_id \
    \ AND c.release_router = i.release_router AND c.interval_seconds = i.interval_seconds \
    \ AND c.complete FOR UPDATE), updated AS (\
    \UPDATE perps_rollup_coverage c SET \
    \ coverage_end = GREATEST(COALESCE(c.coverage_end, i.candidate_end), i.candidate_end), \
    \ finalized_through = GREATEST(COALESCE(c.finalized_through, c.coverage_start), \
    \   i.candidate_finalized), \
    \ updated_at = NOW() FROM candidate i, locked l \
    \WHERE c.kind = l.kind AND c.series_id = l.series_id AND c.chain_id = l.chain_id \
    \AND c.release_router = l.release_router AND c.interval_seconds = l.interval_seconds \
    \AND (c.coverage_end IS DISTINCT FROM \
    \       GREATEST(COALESCE(c.coverage_end, i.candidate_end), i.candidate_end) \
    \ OR c.finalized_through IS DISTINCT FROM \
    \       GREATEST(COALESCE(c.finalized_through, c.coverage_start), i.candidate_finalized)) \
    \RETURNING COALESCE(l.old_finalized, l.coverage_start), c.finalized_through) \
    \SELECT * FROM updated"
    ( rollupKindText kind
    , maybe "" id seriesId
    , maybe 0 id chainId
    , maybe "" normalizeRouter releaseRouter
    , interval
    , sourceTimestamp
    , candidateEnd
    , lateness
    ) :: IO [(Maybe Integer, Maybe Integer)]
  forM_ advanced $ \case
    (Just oldFinalized, Just newFinalized) | newFinalized > oldFinalized ->
      finalizeAdvancedRows conn kind seriesId chainId releaseRouter interval oldFinalized newFinalized
    _ -> pure ()

finalizeAdvancedRows
  :: Connection -> RollupKind -> Maybe Text -> Maybe Integer -> Maybe Text
  -> Integer -> Integer -> Integer -> IO ()
finalizeAdvancedRows conn kind seriesId chainId releaseRouter interval oldFinalized newFinalized =
  case kind of
    PriceRollup -> do
      _ <- execute conn
        "UPDATE perps_basket_candles SET finalized = TRUE, updated_at = NOW() \
        \WHERE series_id = ? AND interval_seconds = ? AND bucket_start >= ? \
        \AND bucket_start < ? AND NOT finalized"
        (maybe "" id seriesId, interval, oldFinalized, newFinalized)
      pure ()
    VolumeRollup -> do
      _ <- execute conn
        "UPDATE perps_market_volume_rollups SET finalized = TRUE, updated_at = NOW() \
        \WHERE chain_id = ? AND release_router = ? AND interval_seconds = ? \
        \AND bucket_start >= ? AND bucket_start < ? AND NOT finalized"
        ( maybe 0 id chainId, maybe "" normalizeRouter releaseRouter, interval
        , oldFinalized, newFinalized
        )
      pure ()

-- Reorg invalidation retains the previously proven terminal as the rebuild
-- target. Reads stay disabled until replay has reached that target; only then
-- can normal watermark advancement resume. This prevents a partially replayed
-- prefix from masquerading as the full canonical dataset.
recoverReorgCoverage :: Connection -> Integer -> Text -> Integer -> Integer -> Integer -> IO ()
recoverReorgCoverage conn chainId releaseRouter interval indexedThrough latenessSeconds = do
  let lateness = max 0 latenessSeconds
  _ <- execute conn
    "WITH input AS (SELECT LEAST(\
    \ (? / ?) * ?, \
    \ ((EXTRACT(EPOCH FROM NOW())::bigint - ?) / ?) * ?\
    \) AS replayed_through) \
    \UPDATE perps_rollup_coverage c SET \
    \ finalized_through = LEAST(c.coverage_end, GREATEST(c.coverage_start, i.replayed_through)), \
    \ complete = (i.replayed_through >= c.coverage_end), \
    \ last_error = CASE WHEN i.replayed_through >= c.coverage_end THEN NULL ELSE c.last_error END, \
    \ maintenance_from = CASE WHEN i.replayed_through >= c.coverage_end THEN NULL \
    \   ELSE c.maintenance_from END, \
    \ maintenance_to = CASE WHEN i.replayed_through >= c.coverage_end THEN NULL \
    \   ELSE c.maintenance_to END, \
    \ updated_at = NOW() FROM input i \
    \WHERE c.kind = 'volume' AND c.series_id = '' AND c.chain_id = ? \
    \AND c.release_router = ? AND c.interval_seconds = ? \
    \AND NOT c.complete AND c.last_error = 'chain_reorg' \
    \AND c.coverage_start IS NOT NULL AND c.coverage_end IS NOT NULL"
    ( indexedThrough
    , interval
    , interval
    , lateness
    , interval
    , interval
    , chainId
    , releaseRouter
    , interval
    )
  pure ()

replacePriceMinute :: Connection -> Text -> Integer -> Integer -> IO Bool
replacePriceMinute conn seriesId bucketStart latenessSeconds = do
  removed <- query conn
    "DELETE FROM perps_basket_candles c \
    \WHERE c.series_id = ? AND c.interval_seconds = 60 AND c.bucket_start = ? \
    \AND NOT EXISTS (SELECT 1 FROM perps_basket_observations o \
    \ WHERE o.series_id = c.series_id AND o.publish_time >= ? AND o.publish_time < ? + 60) \
    \RETURNING 1::BIGINT"
    (seriesId, bucketStart, bucketStart, bucketStart) :: IO [Only Integer]
  changed <- query conn priceMinuteUpsertSql
    ( seriesId, bucketStart, bucketStart + 60
    , seriesId, bucketStart, bucketStart, max 0 latenessSeconds
    ) :: IO [Only Integer]
  _ <- execute conn
    "UPDATE perps_basket_candles SET finalized = \
    \ (? + 60 <= EXTRACT(EPOCH FROM NOW())::bigint - ?), updated_at = NOW() \
    \WHERE series_id = ? AND interval_seconds = 60 AND bucket_start = ? \
    \AND finalized IS DISTINCT FROM \
    \ (? + 60 <= EXTRACT(EPOCH FROM NOW())::bigint - ?)"
    (bucketStart, max 0 latenessSeconds, seriesId, bucketStart, bucketStart, max 0 latenessSeconds)
  pure $ not (null removed) || not (null changed)

replacePriceParent :: Connection -> Text -> Integer -> Integer -> Integer -> IO ()
replacePriceParent conn seriesId interval bucketStart latenessSeconds = do
  _ <- execute conn
    "DELETE FROM perps_basket_candles c \
    \WHERE c.series_id = ? AND c.interval_seconds = ? AND c.bucket_start = ? \
    \AND NOT EXISTS (SELECT 1 FROM perps_basket_candles m \
    \ WHERE m.series_id = c.series_id AND m.interval_seconds = 60 \
    \ AND m.bucket_start >= ? AND m.bucket_start < ? + ?)"
    (seriesId, interval, bucketStart, bucketStart, bucketStart, interval)
  _ <- execute conn priceParentUpsertSql
    ( seriesId, interval, bucketStart, bucketStart, interval, max 0 latenessSeconds
    , seriesId, bucketStart, bucketStart + interval
    )
  _ <- execute conn
    "UPDATE perps_basket_candles SET finalized = \
    \ (? + ? <= EXTRACT(EPOCH FROM NOW())::bigint - ?), updated_at = NOW() \
    \WHERE series_id = ? AND interval_seconds = ? AND bucket_start = ? \
    \AND finalized IS DISTINCT FROM \
    \ (? + ? <= EXTRACT(EPOCH FROM NOW())::bigint - ?)"
    ( bucketStart, interval, max 0 latenessSeconds, seriesId, interval, bucketStart
    , bucketStart, interval, max 0 latenessSeconds
    )
  pure ()

replaceVolumeMinute :: Connection -> Integer -> Text -> Integer -> Integer -> IO Bool
replaceVolumeMinute conn chainId releaseRouter bucketStart latenessSeconds = do
  removed <- query conn
    "DELETE FROM perps_market_volume_rollups v \
    \WHERE v.chain_id = ? AND v.release_router = ? AND v.interval_seconds = 60 \
    \AND v.bucket_start = ? AND NOT EXISTS (SELECT 1 FROM perps_account_activity a \
    \ WHERE a.chain_id = v.chain_id AND a.release_router = v.release_router \
    \ AND a.timestamp >= ? AND a.timestamp < ? + 60 \
    \ AND a.activity_type IN ('Open', 'Close', 'Liquidated') \
    \ AND a.size_delta IS NOT NULL AND a.price IS NOT NULL) RETURNING 1::BIGINT"
    (chainId, releaseRouter, bucketStart, bucketStart, bucketStart) :: IO [Only Integer]
  changed <- query conn volumeMinuteUpsertSql
    ( chainId, releaseRouter, bucketStart, bucketStart, max 0 latenessSeconds
    , chainId, releaseRouter, bucketStart, bucketStart + 60
    ) :: IO [Only Integer]
  _ <- execute conn
    "UPDATE perps_market_volume_rollups SET finalized = \
    \ (? + 60 <= EXTRACT(EPOCH FROM NOW())::bigint - ?), updated_at = NOW() \
    \WHERE chain_id = ? AND release_router = ? AND interval_seconds = 60 \
    \AND bucket_start = ? AND finalized IS DISTINCT FROM \
    \ (? + 60 <= EXTRACT(EPOCH FROM NOW())::bigint - ?)"
    ( bucketStart, max 0 latenessSeconds, chainId, releaseRouter, bucketStart
    , bucketStart, max 0 latenessSeconds
    )
  pure $ not (null removed) || not (null changed)

replaceVolumeParent :: Connection -> Integer -> Text -> Integer -> Integer -> Integer -> IO ()
replaceVolumeParent conn chainId releaseRouter interval bucketStart latenessSeconds = do
  _ <- execute conn
    "DELETE FROM perps_market_volume_rollups v \
    \WHERE v.chain_id = ? AND v.release_router = ? AND v.interval_seconds = ? \
    \AND v.bucket_start = ? AND NOT EXISTS (SELECT 1 FROM perps_market_volume_rollups m \
    \ WHERE m.chain_id = v.chain_id AND m.release_router = v.release_router \
    \ AND m.interval_seconds = 60 AND m.bucket_start >= ? AND m.bucket_start < ? + ?)"
    (chainId, releaseRouter, interval, bucketStart, bucketStart, bucketStart, interval)
  _ <- execute conn volumeParentUpsertSql
    ( chainId, releaseRouter, interval, bucketStart
    , bucketStart, interval, max 0 latenessSeconds
    , chainId, releaseRouter, bucketStart, bucketStart + interval
    )
  _ <- execute conn
    "UPDATE perps_market_volume_rollups SET finalized = \
    \ (? + ? <= EXTRACT(EPOCH FROM NOW())::bigint - ?), updated_at = NOW() \
    \WHERE chain_id = ? AND release_router = ? AND interval_seconds = ? \
    \AND bucket_start = ? AND finalized IS DISTINCT FROM \
    \ (? + ? <= EXTRACT(EPOCH FROM NOW())::bigint - ?)"
    ( bucketStart, interval, max 0 latenessSeconds, chainId, releaseRouter, interval
    , bucketStart, bucketStart, interval, max 0 latenessSeconds
    )
  pure ()

candleRowsSql :: Query
candleRowsSql =
  "SELECT c.bucket_start, c.raw_open_price, c.raw_high_price, c.raw_low_price, \
  \c.raw_close_price, c.sample_count, c.quality, c.revision, c.finalized, \
  \COALESCE(v.volume_numerator, 0::numeric), COALESCE(v.trade_count, 0), \
  \COALESCE(v.finalized, TRUE) \
  \FROM perps_basket_candles c \
  \LEFT JOIN perps_market_volume_rollups v \
  \ ON v.chain_id = ? AND v.release_router = ? \
  \ AND v.interval_seconds = c.interval_seconds AND v.bucket_start = c.bucket_start \
  \WHERE c.series_id = ? AND c.interval_seconds = ? \
  \AND c.bucket_start >= ? AND c.bucket_start < ? \
  \ORDER BY c.bucket_start ASC LIMIT 500"

candleRangeRowsSql :: Query
candleRangeRowsSql =
  "SELECT c.bucket_start, c.raw_open_price, c.raw_high_price, c.raw_low_price, \
  \c.raw_close_price, c.sample_count, c.quality, c.revision, c.finalized, \
  \COALESCE(v.volume_numerator, 0::numeric), COALESCE(v.trade_count, 0), \
  \COALESCE(v.finalized, TRUE) \
  \FROM perps_basket_candles c \
  \LEFT JOIN perps_market_volume_rollups v \
  \ ON v.chain_id = ? AND v.release_router = ? \
  \ AND v.interval_seconds = c.interval_seconds AND v.bucket_start = c.bucket_start \
  \WHERE c.series_id = ? AND c.interval_seconds = ? \
  \AND c.bucket_start >= ? AND c.bucket_start < ? \
  \ORDER BY c.bucket_start ASC LIMIT ?"

combinedMetadataSql :: Query
combinedMetadataSql =
  "SELECT GREATEST(price.coverage_start, volume.coverage_start), \
  \LEAST(price.coverage_end, volume.coverage_end), \
  \LEAST(price.finalized_through, volume.finalized_through), \
  \price.generation * 67108864 + volume.generation, \
  \price.complete AND volume.complete \
  \ AND price.generation > 0 AND price.generation < 67108864 \
  \ AND volume.generation > 0 AND volume.generation < 67108864 \
  \ AND price.derivation_version = volume.derivation_version \
  \ AND price.derivation_version = ? \
  \FROM perps_rollup_coverage price \
  \JOIN perps_rollup_coverage volume \
  \ ON volume.kind = 'volume' AND volume.series_id = '' \
  \ AND volume.chain_id = ? AND volume.release_router = ? \
  \ AND volume.interval_seconds = price.interval_seconds \
  \WHERE price.kind = 'price' AND price.series_id = ? \
  \AND price.chain_id = 0 AND price.release_router = '' \
  \AND price.interval_seconds = ? \
  \AND price.coverage_start IS NOT NULL AND volume.coverage_start IS NOT NULL \
  \AND price.coverage_end IS NOT NULL AND volume.coverage_end IS NOT NULL \
  \AND price.finalized_through IS NOT NULL AND volume.finalized_through IS NOT NULL"

currentCandleRowSql :: Query
currentCandleRowSql =
  "SELECT c.bucket_start, c.raw_open_price, c.raw_high_price, c.raw_low_price, \
  \c.raw_close_price, c.sample_count, c.quality, c.revision, c.finalized, \
  \v.volume_numerator, v.trade_count, (v.bucket_start IS NOT NULL AND v.finalized) \
  \FROM perps_basket_candles c \
  \LEFT JOIN perps_market_volume_rollups v \
  \ ON v.chain_id = ? AND v.release_router = ? \
  \ AND v.interval_seconds = c.interval_seconds AND v.bucket_start = c.bucket_start \
  \WHERE c.series_id = ? AND c.interval_seconds = ? AND c.bucket_start = ? LIMIT 1"

priceMinuteUpsertSql :: Query
priceMinuteUpsertSql =
  "WITH prioritized AS (\
  \ SELECT observation_id, publish_time, basket_price, source, source_priority, \
  \   MAX(source_priority) OVER (PARTITION BY publish_time) AS max_source_priority \
  \ FROM perps_basket_observations \
  \ WHERE series_id = ? AND publish_time >= ? AND publish_time < ?\
  \), ranked AS (\
  \ SELECT observation_id, publish_time, basket_price, source, source_priority \
  \ FROM prioritized WHERE source_priority = max_source_priority\
  \) INSERT INTO perps_basket_candles \
  \(series_id, interval_seconds, bucket_start, raw_open_price, raw_high_price, \
  \ raw_low_price, raw_close_price, first_observation_time, last_observation_time, \
  \ sample_count, quality, revision, finalized) \
  \SELECT ?, 60, (? / 60) * 60, \
  \ (array_agg(basket_price ORDER BY publish_time ASC, source_priority DESC, observation_id ASC))[1], \
  \ MAX(basket_price), MIN(basket_price), \
  \ (array_agg(basket_price ORDER BY publish_time DESC, source_priority DESC, observation_id DESC))[1], \
  \ MIN(publish_time), MAX(publish_time), COUNT(*)::integer, \
  \ CASE WHEN bool_and(source = 'legacy_sampled') THEN 'legacy_sampled' \
  \      WHEN bool_or(source = 'legacy_sampled') THEN 'mixed' ELSE 'observed' END, \
  \ 1, ((? / 60) * 60 + 60 <= EXTRACT(EPOCH FROM NOW())::bigint - ?) \
  \FROM ranked \
  \HAVING COUNT(*) > 0 \
  \ON CONFLICT (series_id, interval_seconds, bucket_start) DO UPDATE SET \
  \ raw_open_price = EXCLUDED.raw_open_price, raw_high_price = EXCLUDED.raw_high_price, \
  \ raw_low_price = EXCLUDED.raw_low_price, raw_close_price = EXCLUDED.raw_close_price, \
  \ first_observation_time = EXCLUDED.first_observation_time, \
  \ last_observation_time = EXCLUDED.last_observation_time, sample_count = EXCLUDED.sample_count, \
  \ quality = EXCLUDED.quality, finalized = EXCLUDED.finalized, \
  \ revision = perps_basket_candles.revision + 1, updated_at = NOW() \
  \WHERE (perps_basket_candles.raw_open_price, perps_basket_candles.raw_high_price, \
  \ perps_basket_candles.raw_low_price, perps_basket_candles.raw_close_price, \
  \ perps_basket_candles.first_observation_time, perps_basket_candles.last_observation_time, \
  \ perps_basket_candles.sample_count, perps_basket_candles.quality) \
  \IS DISTINCT FROM (EXCLUDED.raw_open_price, EXCLUDED.raw_high_price, EXCLUDED.raw_low_price, \
  \ EXCLUDED.raw_close_price, EXCLUDED.first_observation_time, EXCLUDED.last_observation_time, \
  \ EXCLUDED.sample_count, EXCLUDED.quality) \
  \RETURNING 1::BIGINT"

priceParentUpsertSql :: Query
priceParentUpsertSql =
  "INSERT INTO perps_basket_candles \
  \(series_id, interval_seconds, bucket_start, raw_open_price, raw_high_price, \
  \ raw_low_price, raw_close_price, first_observation_time, last_observation_time, \
  \ sample_count, quality, revision, finalized) \
  \SELECT ?, ?, ?, (array_agg(raw_open_price ORDER BY bucket_start ASC))[1], \
  \ MAX(raw_high_price), MIN(raw_low_price), \
  \ (array_agg(raw_close_price ORDER BY bucket_start DESC))[1], \
  \ MIN(first_observation_time), MAX(last_observation_time), SUM(sample_count)::integer, \
  \ CASE WHEN bool_and(quality = 'legacy_sampled') THEN 'legacy_sampled' \
  \      WHEN bool_and(quality = 'observed') THEN 'observed' ELSE 'mixed' END, \
  \ 1, (? + ? <= EXTRACT(EPOCH FROM NOW())::bigint - ?) \
  \FROM perps_basket_candles WHERE series_id = ? AND interval_seconds = 60 \
  \AND bucket_start >= ? AND bucket_start < ? HAVING COUNT(*) > 0 \
  \ON CONFLICT (series_id, interval_seconds, bucket_start) DO UPDATE SET \
  \ raw_open_price = EXCLUDED.raw_open_price, raw_high_price = EXCLUDED.raw_high_price, \
  \ raw_low_price = EXCLUDED.raw_low_price, raw_close_price = EXCLUDED.raw_close_price, \
  \ first_observation_time = EXCLUDED.first_observation_time, \
  \ last_observation_time = EXCLUDED.last_observation_time, sample_count = EXCLUDED.sample_count, \
  \ quality = EXCLUDED.quality, finalized = EXCLUDED.finalized, \
  \ revision = perps_basket_candles.revision + 1, updated_at = NOW() \
  \WHERE (perps_basket_candles.raw_open_price, perps_basket_candles.raw_high_price, \
  \ perps_basket_candles.raw_low_price, perps_basket_candles.raw_close_price, \
  \ perps_basket_candles.first_observation_time, perps_basket_candles.last_observation_time, \
  \ perps_basket_candles.sample_count, perps_basket_candles.quality) \
  \IS DISTINCT FROM (EXCLUDED.raw_open_price, EXCLUDED.raw_high_price, EXCLUDED.raw_low_price, \
  \ EXCLUDED.raw_close_price, EXCLUDED.first_observation_time, EXCLUDED.last_observation_time, \
  \ EXCLUDED.sample_count, EXCLUDED.quality)"

volumeMinuteUpsertSql :: Query
volumeMinuteUpsertSql =
  "INSERT INTO perps_market_volume_rollups \
  \(chain_id, release_router, interval_seconds, bucket_start, volume_numerator, trade_count, \
  \ first_source_block, last_source_block, revision, finalized) \
  \SELECT ?, ?, 60, (? / 60) * 60, FLOOR(SUM(ABS(size_delta) * price)), COUNT(*)::bigint, \
  \ MIN(block_number), MAX(block_number), 1, \
  \ ((? / 60) * 60 + 60 <= EXTRACT(EPOCH FROM NOW())::bigint - ?) \
  \FROM perps_account_activity WHERE chain_id = ? AND release_router = ? \
  \AND timestamp >= ? AND timestamp < ? \
  \AND activity_type IN ('Open', 'Close', 'Liquidated') \
  \AND size_delta IS NOT NULL AND price IS NOT NULL HAVING COUNT(*) > 0 \
  \ON CONFLICT (chain_id, release_router, interval_seconds, bucket_start) DO UPDATE SET \
  \ volume_numerator = EXCLUDED.volume_numerator, trade_count = EXCLUDED.trade_count, \
  \ first_source_block = EXCLUDED.first_source_block, last_source_block = EXCLUDED.last_source_block, \
  \ finalized = EXCLUDED.finalized, revision = perps_market_volume_rollups.revision + 1, updated_at = NOW() \
  \WHERE (perps_market_volume_rollups.volume_numerator, perps_market_volume_rollups.trade_count, \
  \ perps_market_volume_rollups.first_source_block, perps_market_volume_rollups.last_source_block) \
  \ IS DISTINCT FROM \
  \ (EXCLUDED.volume_numerator, EXCLUDED.trade_count, EXCLUDED.first_source_block, \
  \ EXCLUDED.last_source_block) RETURNING 1::BIGINT"

volumeParentUpsertSql :: Query
volumeParentUpsertSql =
  "INSERT INTO perps_market_volume_rollups \
  \(chain_id, release_router, interval_seconds, bucket_start, volume_numerator, trade_count, \
  \ first_source_block, last_source_block, revision, finalized) \
  \SELECT ?, ?, ?, ?, SUM(volume_numerator), SUM(trade_count), \
  \ MIN(first_source_block), MAX(last_source_block), 1, \
  \ (? + ? <= EXTRACT(EPOCH FROM NOW())::bigint - ?) \
  \FROM perps_market_volume_rollups WHERE chain_id = ? AND release_router = ? \
  \AND interval_seconds = 60 AND bucket_start >= ? AND bucket_start < ? HAVING COUNT(*) > 0 \
  \ON CONFLICT (chain_id, release_router, interval_seconds, bucket_start) DO UPDATE SET \
  \ volume_numerator = EXCLUDED.volume_numerator, trade_count = EXCLUDED.trade_count, \
  \ first_source_block = EXCLUDED.first_source_block, last_source_block = EXCLUDED.last_source_block, \
  \ finalized = EXCLUDED.finalized, revision = perps_market_volume_rollups.revision + 1, updated_at = NOW() \
  \WHERE (perps_market_volume_rollups.volume_numerator, perps_market_volume_rollups.trade_count, \
  \ perps_market_volume_rollups.first_source_block, perps_market_volume_rollups.last_source_block) \
  \ IS DISTINCT FROM \
  \ (EXCLUDED.volume_numerator, EXCLUDED.trade_count, EXCLUDED.first_source_block, \
  \ EXCLUDED.last_source_block)"

legacyMinuteBackfillSql :: Query
legacyMinuteBackfillSql =
  "WITH input AS (SELECT ?::text AS series_id, ?::bigint AS range_start, \
  \ ?::bigint AS range_end), observed_prioritized AS (\
  \ SELECT o.observation_id, o.publish_time, o.basket_price, o.source, o.source_priority, \
  \ MAX(o.source_priority) OVER (PARTITION BY o.publish_time) AS max_source_priority \
  \ FROM perps_basket_observations o CROSS JOIN input i \
  \ WHERE o.series_id = i.series_id AND o.publish_time >= i.range_start \
  \ AND o.publish_time < i.range_end\
  \), observed_ranked AS (\
  \ SELECT observation_id, publish_time, basket_price, source, source_priority \
  \ FROM observed_prioritized WHERE source_priority = max_source_priority\
  \), observed_minutes AS (\
  \ SELECT i.series_id, (o.publish_time / 60) * 60 AS bucket_start, \
  \ (array_agg(o.basket_price ORDER BY o.publish_time ASC, o.source_priority DESC, \
  \   o.observation_id ASC))[1] AS open_price, MAX(o.basket_price) AS high_price, \
  \ MIN(o.basket_price) AS low_price, \
  \ (array_agg(o.basket_price ORDER BY o.publish_time DESC, o.source_priority DESC, \
  \   o.observation_id DESC))[1] AS close_price, \
  \ MIN(o.publish_time) AS first_time, MAX(o.publish_time) AS last_time, \
  \ COUNT(*)::integer AS sample_count, \
  \ CASE WHEN bool_and(o.source = 'legacy_sampled') THEN 'legacy_sampled' \
  \      WHEN bool_or(o.source = 'legacy_sampled') THEN 'mixed' ELSE 'observed' END AS quality \
  \ FROM observed_ranked o CROSS JOIN input i \
  \ GROUP BY i.series_id, o.publish_time / 60\
  \), legacy_ranked AS (\
  \ SELECT s.id, s.timestamp, s.basket_price, \
  \ ROW_NUMBER() OVER (PARTITION BY s.timestamp \
  \   ORDER BY s.interval_seconds ASC, s.id DESC) AS timestamp_rank \
  \ FROM perps_basket_snapshots s CROSS JOIN input i \
  \ WHERE s.timestamp >= i.range_start AND s.timestamp < i.range_end\
  \), legacy_samples AS (\
  \ SELECT id, timestamp, basket_price FROM legacy_ranked WHERE timestamp_rank = 1\
  \), legacy_minutes AS (\
  \ SELECT i.series_id, (s.timestamp / 60) * 60 AS bucket_start, \
  \ (array_agg(s.basket_price ORDER BY s.timestamp ASC, s.id ASC))[1] AS open_price, \
  \ MAX(s.basket_price) AS high_price, MIN(s.basket_price) AS low_price, \
  \ (array_agg(s.basket_price ORDER BY s.timestamp DESC, s.id DESC))[1] AS close_price, \
  \ MIN(s.timestamp) AS first_time, MAX(s.timestamp) AS last_time, \
  \ COUNT(*)::integer AS sample_count, 'legacy_sampled'::text AS quality \
  \ FROM legacy_samples s CROSS JOIN input i \
  \ WHERE NOT EXISTS (SELECT 1 FROM observed_minutes o \
  \   WHERE o.series_id = i.series_id AND o.bucket_start = (s.timestamp / 60) * 60) \
  \ GROUP BY i.series_id, s.timestamp / 60\
  \), canonical_minutes AS (SELECT * FROM observed_minutes UNION ALL SELECT * FROM legacy_minutes) \
  \INSERT INTO perps_basket_candles \
  \(series_id, interval_seconds, bucket_start, raw_open_price, raw_high_price, \
  \ raw_low_price, raw_close_price, first_observation_time, last_observation_time, \
  \ sample_count, quality, revision, finalized) \
  \SELECT series_id, 60, bucket_start, open_price, high_price, low_price, close_price, \
  \ first_time, last_time, sample_count, quality, 1, TRUE FROM canonical_minutes"

priceParentRangeSql :: Query
priceParentRangeSql =
  "WITH input AS (SELECT ?::text AS series_id, ?::bigint AS target_interval, \
  \ ?::bigint AS range_start, ?::bigint AS range_end) \
  \INSERT INTO perps_basket_candles \
  \(series_id, interval_seconds, bucket_start, raw_open_price, raw_high_price, \
  \ raw_low_price, raw_close_price, first_observation_time, last_observation_time, \
  \ sample_count, quality, revision, finalized) \
  \SELECT i.series_id, i.target_interval, \
  \ (m.bucket_start / i.target_interval) * i.target_interval AS parent_bucket, \
  \ (array_agg(m.raw_open_price ORDER BY m.bucket_start ASC))[1], MAX(m.raw_high_price), \
  \ MIN(m.raw_low_price), (array_agg(m.raw_close_price ORDER BY m.bucket_start DESC))[1], \
  \ MIN(m.first_observation_time), MAX(m.last_observation_time), SUM(m.sample_count)::integer, \
  \ CASE WHEN bool_and(m.quality = 'legacy_sampled') THEN 'legacy_sampled' \
  \      WHEN bool_and(m.quality = 'observed') THEN 'observed' ELSE 'mixed' END, 1, \
  \ ((MIN(m.bucket_start) / i.target_interval) * i.target_interval >= i.range_start AND \
  \  (MIN(m.bucket_start) / i.target_interval) * i.target_interval + i.target_interval <= i.range_end) \
  \FROM perps_basket_candles m CROSS JOIN input i \
  \WHERE m.series_id = i.series_id AND m.interval_seconds = 60 \
  \AND m.bucket_start >= (i.range_start / i.target_interval) * i.target_interval \
  \AND m.bucket_start < ((i.range_end + i.target_interval - 1) / i.target_interval) * i.target_interval \
  \GROUP BY i.series_id, i.target_interval, i.range_start, i.range_end, parent_bucket"

volumeMinuteBackfillSql :: Query
volumeMinuteBackfillSql =
  "INSERT INTO perps_market_volume_rollups \
  \(chain_id, release_router, interval_seconds, bucket_start, volume_numerator, trade_count, \
  \ first_source_block, last_source_block, revision, finalized) \
  \SELECT ?, ?, 60, (timestamp / 60) * 60, FLOOR(SUM(ABS(size_delta) * price)), \
  \ COUNT(*)::bigint, MIN(block_number), MAX(block_number), 1, TRUE \
  \FROM perps_account_activity WHERE chain_id = ? AND release_router = ? \
  \AND timestamp >= ? AND timestamp < ? \
  \AND activity_type IN ('Open', 'Close', 'Liquidated') \
  \AND size_delta IS NOT NULL AND price IS NOT NULL GROUP BY timestamp / 60"

volumeParentRangeSql :: Query
volumeParentRangeSql =
  "WITH input AS (SELECT ?::bigint AS chain_id, ?::text AS release_router, \
  \ ?::bigint AS target_interval, ?::bigint AS range_start, ?::bigint AS range_end) \
  \INSERT INTO perps_market_volume_rollups \
  \(chain_id, release_router, interval_seconds, bucket_start, volume_numerator, trade_count, \
  \ first_source_block, last_source_block, revision, finalized) \
  \SELECT i.chain_id, i.release_router, i.target_interval, \
  \ (m.bucket_start / i.target_interval) * i.target_interval AS parent_bucket, \
  \ SUM(m.volume_numerator), SUM(m.trade_count), MIN(m.first_source_block), \
  \ MAX(m.last_source_block), 1, \
  \ ((MIN(m.bucket_start) / i.target_interval) * i.target_interval >= i.range_start AND \
  \  (MIN(m.bucket_start) / i.target_interval) * i.target_interval + i.target_interval <= i.range_end) \
  \FROM perps_market_volume_rollups m CROSS JOIN input i \
  \WHERE m.chain_id = i.chain_id AND m.release_router = i.release_router \
  \AND m.interval_seconds = 60 \
  \AND m.bucket_start >= (i.range_start / i.target_interval) * i.target_interval \
  \AND m.bucket_start < ((i.range_end + i.target_interval - 1) / i.target_interval) * i.target_interval \
  \GROUP BY i.chain_id, i.release_router, i.target_interval, i.range_start, i.range_end, parent_bucket"

candleActivityIndexStatement :: Query
candleActivityIndexStatement =
  "CREATE INDEX CONCURRENTLY idx_perps_account_activity_volume_rollup \
  \ON perps_account_activity(chain_id, release_router, timestamp) \
  \INCLUDE (size_delta, price, block_number) \
  \WHERE activity_type IN ('Open','Close','Liquidated') \
  \AND size_delta IS NOT NULL AND price IS NOT NULL"

candleActivityIndexDropStatement :: Query
candleActivityIndexDropStatement =
  "DROP INDEX CONCURRENTLY idx_perps_account_activity_volume_rollup"

candleEventIndexStatement :: Query
candleEventIndexStatement =
  "CREATE INDEX CONCURRENTLY idx_perps_events_candle_bounds \
  \ON perps_events(chain_id, release_router, timestamp)"

candleEventIndexDropStatement :: Query
candleEventIndexDropStatement =
  "DROP INDEX CONCURRENTLY idx_perps_events_candle_bounds"

candleActivityReorgIndexStatement :: Query
candleActivityReorgIndexStatement =
  "CREATE INDEX CONCURRENTLY idx_perps_account_activity_reorg_blocks \
  \ON perps_account_activity(chain_id, release_router, block_number) \
  \INCLUDE (timestamp)"

candleActivityReorgIndexDropStatement :: Query
candleActivityReorgIndexDropStatement =
  "DROP INDEX CONCURRENTLY idx_perps_account_activity_reorg_blocks"

candleEventReorgIndexStatement :: Query
candleEventReorgIndexStatement =
  "CREATE INDEX CONCURRENTLY idx_perps_events_reorg_blocks \
  \ON perps_events(chain_id, release_router, block_number)"

candleEventReorgIndexDropStatement :: Query
candleEventReorgIndexDropStatement =
  "DROP INDEX CONCURRENTLY idx_perps_events_reorg_blocks"

candleTableSchemaStatements :: [Query]
candleTableSchemaStatements =
  [ "CREATE TABLE IF NOT EXISTS perps_basket_definitions (\
    \series_id TEXT PRIMARY KEY, definition_version TEXT NOT NULL, \
    \configuration_hash TEXT NOT NULL, configuration JSONB NOT NULL, \
    \effective_from BIGINT NOT NULL, effective_to BIGINT, active BOOLEAN NOT NULL DEFAULT TRUE, \
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \CHECK (configuration_hash ~ '^sha256:[0-9a-f]{64}$'), CHECK (effective_from >= 0), \
    \CHECK (effective_to IS NULL OR effective_to > effective_from))"
  , "CREATE INDEX IF NOT EXISTS idx_perps_basket_definitions_effective \
    \ON perps_basket_definitions(active, effective_from DESC)"
  , "CREATE TABLE IF NOT EXISTS perps_basket_observations (\
    \series_id TEXT NOT NULL REFERENCES perps_basket_definitions(series_id), \
    \observation_id TEXT NOT NULL, publish_time BIGINT NOT NULL, basket_price BIGINT NOT NULL, \
    \component_prices JSONB NOT NULL, source TEXT NOT NULL, source_priority INTEGER NOT NULL, \
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \PRIMARY KEY (series_id, observation_id))"
  , "CREATE INDEX IF NOT EXISTS idx_perps_basket_observations_series_time \
    \ON perps_basket_observations(series_id, publish_time, source_priority DESC, observation_id)"
  , "CREATE TABLE IF NOT EXISTS perps_basket_candles (\
    \series_id TEXT NOT NULL REFERENCES perps_basket_definitions(series_id), \
    \interval_seconds BIGINT NOT NULL, bucket_start BIGINT NOT NULL, \
    \raw_open_price BIGINT NOT NULL, raw_high_price BIGINT NOT NULL, raw_low_price BIGINT NOT NULL, \
    \raw_close_price BIGINT NOT NULL, first_observation_time BIGINT NOT NULL, \
    \last_observation_time BIGINT NOT NULL, sample_count INTEGER NOT NULL, quality TEXT NOT NULL, \
    \revision BIGINT NOT NULL DEFAULT 1, finalized BOOLEAN NOT NULL DEFAULT FALSE, \
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \PRIMARY KEY (series_id, interval_seconds, bucket_start), \
    \CHECK (interval_seconds IN (60,180,300,900,1800,3600,86400)), \
    \CHECK (bucket_start % interval_seconds = 0), CHECK (sample_count > 0), CHECK (revision > 0), \
    \CHECK (quality IN ('observed','legacy_sampled','mixed')), \
    \CHECK (raw_high_price >= GREATEST(raw_open_price, raw_close_price)), \
    \CHECK (raw_low_price <= LEAST(raw_open_price, raw_close_price)), \
    \CHECK (last_observation_time >= first_observation_time))"
  , "CREATE TABLE IF NOT EXISTS perps_market_volume_rollups (\
    \chain_id BIGINT NOT NULL, release_router TEXT NOT NULL, interval_seconds BIGINT NOT NULL, \
    \bucket_start BIGINT NOT NULL, volume_numerator NUMERIC(78,0) NOT NULL, trade_count BIGINT NOT NULL, \
    \first_source_block BIGINT NOT NULL, last_source_block BIGINT NOT NULL, revision BIGINT NOT NULL DEFAULT 1, \
    \finalized BOOLEAN NOT NULL DEFAULT FALSE, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \PRIMARY KEY (chain_id, release_router, interval_seconds, bucket_start), \
    \CHECK (interval_seconds IN (60,180,300,900,1800,3600,86400)), \
    \CHECK (bucket_start % interval_seconds = 0), CHECK (volume_numerator >= 0), \
    \CHECK (trade_count > 0), CHECK (revision > 0), CHECK (last_source_block >= first_source_block))"
  , "CREATE TABLE IF NOT EXISTS perps_rollup_coverage (\
    \kind TEXT NOT NULL, series_id TEXT NOT NULL DEFAULT '', chain_id BIGINT NOT NULL DEFAULT 0, \
    \release_router TEXT NOT NULL DEFAULT '', interval_seconds BIGINT NOT NULL, \
    \coverage_start BIGINT, coverage_end BIGINT, finalized_through BIGINT, generation BIGINT NOT NULL DEFAULT 1, \
    \complete BOOLEAN NOT NULL DEFAULT FALSE, derivation_version TEXT NOT NULL, last_error TEXT, \
    \maintenance_from BIGINT, maintenance_to BIGINT, \
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \PRIMARY KEY (kind, series_id, chain_id, release_router, interval_seconds), \
    \CHECK (kind IN ('price','volume')), \
    \CHECK (interval_seconds IN (60,180,300,900,1800,3600,86400)), \
    \CHECK (generation > 0 AND generation < 67108864), \
    \CHECK ((kind = 'price' AND series_id <> '' AND chain_id = 0 AND release_router = '') OR \
    \       (kind = 'volume' AND series_id = '' AND chain_id > 0 AND release_router <> '')), \
    \CHECK ((coverage_start IS NULL) = (coverage_end IS NULL)), \
    \CHECK (coverage_start IS NULL OR coverage_start >= 0), \
    \CHECK (coverage_end IS NULL OR coverage_end >= 0), \
    \CHECK (finalized_through IS NULL OR finalized_through >= 0), \
    \CHECK (coverage_start IS NULL OR coverage_start % interval_seconds = 0), \
    \CHECK (coverage_end IS NULL OR coverage_end % interval_seconds = 0), \
    \CHECK (finalized_through IS NULL OR finalized_through % interval_seconds = 0), \
    \CHECK (coverage_start IS NULL OR coverage_end > coverage_start), \
    \CHECK (finalized_through IS NULL OR coverage_start IS NULL OR finalized_through >= coverage_start), \
    \CHECK (finalized_through IS NULL OR coverage_end IS NULL OR finalized_through <= coverage_end), \
    \CONSTRAINT perps_rollup_coverage_maintenance_state_check CHECK (\
    \ (last_error IS NOT DISTINCT FROM 'bounded_admin_repair' AND NOT complete \
    \ AND maintenance_from IS NOT NULL AND maintenance_to IS NOT NULL \
    \ AND maintenance_from >= 0 AND maintenance_to > maintenance_from \
    \ AND maintenance_from % 60 = 0 AND maintenance_to % 60 = 0) OR \
    \ (last_error IS DISTINCT FROM 'bounded_admin_repair' \
    \ AND maintenance_from IS NULL AND maintenance_to IS NULL)))"
  , "ALTER TABLE perps_rollup_coverage ADD COLUMN IF NOT EXISTS maintenance_from BIGINT"
  , "ALTER TABLE perps_rollup_coverage ADD COLUMN IF NOT EXISTS maintenance_to BIGINT"
  , "DO $maintenance_constraint$ BEGIN IF NOT EXISTS (\
    \ SELECT 1 FROM pg_constraint \
    \ WHERE conname = 'perps_rollup_coverage_maintenance_state_check' \
    \ AND conrelid = 'perps_rollup_coverage'::regclass) THEN \
    \ ALTER TABLE perps_rollup_coverage \
    \ ADD CONSTRAINT perps_rollup_coverage_maintenance_state_check CHECK (\
    \ (last_error IS NOT DISTINCT FROM 'bounded_admin_repair' AND NOT complete \
    \ AND maintenance_from IS NOT NULL AND maintenance_to IS NOT NULL \
    \ AND maintenance_from >= 0 AND maintenance_to > maintenance_from \
    \ AND maintenance_from % 60 = 0 AND maintenance_to % 60 = 0) OR \
    \ (last_error IS DISTINCT FROM 'bounded_admin_repair' \
    \ AND maintenance_from IS NULL AND maintenance_to IS NULL)); \
    \ END IF; END $maintenance_constraint$"
  ]
