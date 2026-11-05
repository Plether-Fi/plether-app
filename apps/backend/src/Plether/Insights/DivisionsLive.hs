module Plether.Insights.DivisionsLive
  ( ensureDivisionsLiveSchema
  , runDivisionsLiveCycle
  , rankDivisions
  , LiveScore (..)
  , pinDivisionsWeekReleases
  , divisionsPublicationsQuery
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value, object, (.=), encode, eitherDecodeStrict')
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection, Only (..), Query, execute, execute_, query)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema (PerpsIndexerStatusRow (..), getPerpsIndexerStatus)
import Plether.Ethereum.Client (EthClient)
import Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( getAccountLedgerSnapshotAtBlock
  , getPendingCarryAtBlock
  )
import qualified Plether.Ethereum.Multicall as Multicall
import Plether.Ethereum.Rpc (RpcBlock (..), ethGetBlockByNumber)
import Plether.Insights.Divisions
import Plether.Insights.DivisionsFunding (FundingScope (..), ensureDivisionsFundingSchema, loadDivisionsFunding)
import Plether.Insights.Competition (economicAccountValue)
import Plether.Insights.SnapshotWorker
  ( accountSnapshotMulticallCalls, chunkInOrder, decodeSnapshotResults
  , findLastBlockBeforeTimestamp, inferPendingCarry, ledgerToEquity
  )

-- Exact integer cross products determine rank. Only the displayed return is
-- rounded, so a display tie cannot alter the underlying ordering.
rankDivisions :: [LiveScore] -> [Value]
rankDivisions scores = map render (rankScores scores)
  where
    render (score, rank) = object
      [ "address" .= lsAddress score
      , "rank" .= rank
      , "weeklyReturnBps" .= if lsAdjustedEquity score > 0
          then Just (roundedBps (lsPnl score) (lsAdjustedEquity score)) else (Nothing :: Maybe Integer)
      , "activeDays" .= lsActiveDays score
      ]
    roundedBps numerator denominator =
      let magnitude = (abs numerator * 10000 + denominator `div` 2) `div` denominator
       in if numerator < 0 then negate magnitude else magnitude

ensureDivisionsLiveSchema :: Connection -> IO ()
ensureDivisionsLiveSchema conn = do
  ensureDivisionsFundingSchema conn
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS divisions_live_snapshots (\
    \chain_id BIGINT NOT NULL, release_router TEXT NOT NULL, week_start BIGINT NOT NULL,\
    \baseline_block BIGINT NOT NULL, baseline_hash TEXT NOT NULL,\
    \snapshot_block BIGINT NOT NULL, snapshot_hash TEXT NOT NULL,\
    \snapshot_timestamp BIGINT NOT NULL, wallet_count INTEGER NOT NULL,\
    \standings JSONB NOT NULL, captured_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, release_router, week_start))"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS divisions_publications (\
    \chain_id BIGINT NOT NULL, release_router TEXT NOT NULL, week_start BIGINT NOT NULL,\
    \closing_block BIGINT NOT NULL, publication JSONB NOT NULL,\
    \PRIMARY KEY (chain_id, release_router, week_start))"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS divisions_weeks (\
    \chain_id BIGINT NOT NULL, week_start BIGINT NOT NULL, release_router TEXT NOT NULL,\
    \release_version TEXT NOT NULL, asset_symbol TEXT NOT NULL CHECK (asset_symbol IN ('USDC','USDG')),\
    \CHECK (week_start>=1791752400 AND (week_start-1791752400)%604800=0),\
    \PRIMARY KEY (chain_id,week_start))"
  now <- floor <$> getPOSIXTime
  pinDivisionsWeekReleases conn now

pinDivisionsWeekReleases :: Connection -> Integer -> IO ()
pinDivisionsWeekReleases conn now = do
  rows <- query conn
    "SELECT week_start,release_router,release_version,asset_symbol FROM divisions_weeks WHERE chain_id=?"
    (Only (421614 :: Integer)) :: IO [(Integer,Text,Text,Text)]
  let existing = Map.fromList [(start,DivisionRelease router version asset) | (start,router,version,asset) <- rows]
  forM_ [firstWeekStart,firstWeekStart+weekSeconds..divisionWeekStart now] $ \start ->
    case divisionReleaseForWeek start of
      Nothing -> fail "Divisions week has no scheduled contract release"
      Just expected@DivisionRelease {..} -> do
        stored <- case Map.lookup start existing of
          Just value -> pure [value]
          Nothing -> do
            _ <- execute conn
              "INSERT INTO divisions_weeks(chain_id,week_start,release_router,release_version,asset_symbol) \
              \VALUES (421614,?,?,?,?) ON CONFLICT (chain_id,week_start) DO NOTHING"
              (start,drRouter,drVersion,drAssetSymbol)
            found <- query conn
              "SELECT release_router,release_version,asset_symbol FROM divisions_weeks WHERE chain_id=421614 AND week_start=?"
              (Only start)
            pure [DivisionRelease router version asset | (router,version,asset) <- found]
        unless (stored == [expected]) $ fail "Divisions contract release is already fixed for this week"

isPinnedRelease :: Connection -> Integer -> Text -> Integer -> IO Bool
isPinnedRelease conn chain router start = do
  rows <- query conn
    "SELECT EXISTS (SELECT 1 FROM divisions_weeks WHERE chain_id=? AND week_start=? AND release_router=?)"
    (chain,start,router)
  pure $ rows == [Only True]

-- One publication per week is authoritative, independently of which contracts
-- the API currently serves. Alternative-release rows remain untouched for audit.
divisionsPublicationsQuery :: Query
divisionsPublicationsQuery =
  "WITH weeks AS (SELECT p.week_start,p.closing_block,p.publication,p.release_router, \
  \w.release_version,w.asset_symbol, \
  \LAG(p.week_start) OVER (ORDER BY p.week_start) AS older_week, \
  \LEAD(p.week_start) OVER (ORDER BY p.week_start) AS newer_week \
  \FROM divisions_publications p JOIN divisions_weeks w \
  \ON w.chain_id=p.chain_id AND w.week_start=p.week_start AND w.release_router=p.release_router \
  \WHERE p.chain_id=?) \
  \SELECT p.closing_block,(jsonb_set(p.publication,'{standings}', \
  \(SELECT COALESCE(jsonb_agg(e.value || jsonb_build_object( \
  \'weeklyPnlAssets',COALESCE(e.value->'weeklyPnlAssets',e.value->'weeklyPnlUsdc'), \
  \'adjustedEquityAssets',COALESCE(e.value->'adjustedEquityAssets',e.value->'adjustedEquityUsdc'), \
  \'displayName',l.discord_username) ORDER BY e.ordinality),'[]'::jsonb) \
  \FROM jsonb_array_elements(p.publication->'standings') WITH ORDINALITY AS e(value,ordinality) \
  \LEFT JOIN insights_divisions_links l ON l.trading_account=e.value->>'address')) \
  \|| jsonb_build_object('scoringRelease',jsonb_build_object( \
  \ 'router',p.release_router,'version',p.release_version,'assetSymbol',p.asset_symbol)))::text, \
  \p.older_week,p.newer_week FROM weeks p \
  \WHERE (?::BIGINT IS NULL OR p.week_start=?) ORDER BY p.week_start DESC LIMIT 1"

runDivisionsLiveCycle :: EthClient -> DbPool -> Config -> Int -> IO ()
runDivisionsLiveCycle _ _ cfg _ | cfgPerpsChainId cfg /= 421614 = pure ()
runDivisionsLiveCycle client pool cfg multicallSize = do
  now <- floor <$> getPOSIXTime
  withDb pool $ \conn -> pinDivisionsWeekReleases conn now
  -- Scores need the event-complete history cursor, not the lifecycle cursor.
  -- A newer RPC block can contain account changes whose funding/trades have
  -- not yet reached perps_account_activity.
  mCursor <- withDb pool $ \conn -> do
    let chain = cfgPerpsChainId cfg
        router = cfgPerpsOrderRouter cfg
    current <- getPerpsIndexerStatus conn chain "perps-history-costs-v2:finalized-abi3" router
    case current of
      Just _ -> pure current
      Nothing -> getPerpsIndexerStatus conn chain "perps-history-costs-v1" router
  case mCursor of
    Just cursor@PerpsIndexerStatusRow { pisLastIndexedBlockHash = Just safeHash } -> do
      eSafe <- ethGetBlockByNumber client (pisLastIndexedBlock cursor)
      case eSafe of
        Right safe | T.toLower (rpcBlockHash safe) == T.toLower safeHash -> do
          captureWeek client pool cfg multicallSize cursor safe
          -- Backfill one complete week per cycle without starving live scores.
          publishNextWeek client pool cfg multicallSize cursor safe
        _ -> putStrLn "Divisions live skipped: safe block hash is unavailable or differs from RPC"
    _ -> putStrLn "Divisions live skipped: current release has no finalized indexer cursor"

captureWeek :: EthClient -> DbPool -> Config -> Int -> PerpsIndexerStatusRow -> RpcBlock -> IO ()
captureWeek client pool cfg multicallSize cursor safe
  | rpcBlockTimestamp safe < firstWeekStart = pure ()
  | otherwise = do
      let weekStart = divisionWeekStart (rpcBlockTimestamp safe)
          close = weekStart + 5 * 86400
      pinned <- withDb pool $ \conn -> isPinnedRelease conn (cfgPerpsChainId cfg) (pisReleaseRouter cursor) weekStart
      if not pinned then putStrLn "Divisions live skipped: week is pinned to another release"
      else do
        eBaseline <- findLastBlockBeforeTimestamp (ethGetBlockByNumber client) (rpcBlockNumber safe) weekStart
        eCurrent <- if rpcBlockTimestamp safe < close then pure $ Right $ Just safe
          else findLastBlockBeforeTimestamp (ethGetBlockByNumber client) (rpcBlockNumber safe) close
        case (eBaseline, eCurrent) of
          (Right (Just baseline), Right (Just current)) -> do
            captured <- captureAtBlocks client pool cfg multicallSize cursor baseline current []
            case captured of
              Nothing -> pure ()
              Just scores -> storeLiveSnapshot pool cfg cursor weekStart baseline current scores
          _ -> putStrLn "Divisions live skipped: weekly boundary block unavailable"

captureAtBlocks :: EthClient -> DbPool -> Config -> Int -> PerpsIndexerStatusRow -> RpcBlock -> RpcBlock -> [Text] -> IO (Maybe [LiveScore])
captureAtBlocks client pool cfg multicallSize cursor baseline current priorWallets = do
  let chain = cfgPerpsChainId cfg
      router = pisReleaseRouter cursor
      upper = rpcBlockNumber current
  wallets <- withDb pool $ \conn -> query conn
    "SELECT DISTINCT LOWER(account) FROM perps_account_activity \
    \WHERE chain_id=? AND release_router=? AND block_number<=? \
    \AND activity_type IN ('Open','Close') AND COALESCE(size_delta,0)<>0 \
    \ORDER BY LOWER(account)"
    (chain, router, upper)
  let addresses = Set.toAscList $ Set.fromList $ priorWallets <> [wallet | Only wallet <- wallets]
  funding <- loadDivisionsFunding client pool
    (FundingScope chain router (cfgPerpsSettlementToken cfg) (cfgPerpsMarginClearinghouse cfg))
    (rpcBlockNumber baseline) upper
  case funding of
    Nothing -> putStrLn "Divisions skipped: funding integrity check failed" >> pure Nothing
    Just flows -> do
      days <- withDb pool $ \conn -> query conn
        "SELECT LOWER(account), COUNT(DISTINCT (timestamp+10800)/86400)::BIGINT \
        \FROM perps_account_activity WHERE chain_id=? AND release_router=? \
        \AND block_number>? AND block_number<=? AND activity_type IN ('Open','Close') \
        \AND COALESCE(size_delta,0)<>0 AND EXTRACT(DOW FROM \
        \to_timestamp(timestamp+10800) AT TIME ZONE 'UTC') BETWEEN 1 AND 5 \
        \GROUP BY LOWER(account)"
        (chain, router, rpcBlockNumber baseline, upper)
      let dayCount wallet = fromIntegral $ maybe 0 id $ lookup wallet (days :: [(Text, Int64)])
          flowTotal wallet kind = sum [amount | (account, flowKind, amount, assisted) <- flows,
            account == wallet, flowKind == kind, not assisted]
      eOpening <- if rpcBlockNumber baseline < cfgPerpsIndexerStartBlock cfg
        then pure $ Just $ zip addresses (repeat 0)
        else captureValues client cfg multicallSize (rpcBlockNumber baseline) addresses
      eClosing <- captureValues client cfg multicallSize upper addresses
      case (eOpening, eClosing) of
        (Just opening, Just closing) -> do
          verifiedStart <- ethGetBlockByNumber client (rpcBlockNumber baseline)
          verifiedEnd <- ethGetBlockByNumber client upper
          if not (matchesBlock verifiedStart baseline && matchesBlock verifiedEnd current)
            then putStrLn "Divisions skipped: block hash changed during capture" >> pure Nothing
            else do
              let scores = [LiveScore wallet adjusted (ending - adjusted) (dayCount wallet)
                    | (wallet, ending) <- closing
                    , let adjusted = maybe 0 id (lookup wallet opening) + flowTotal wallet "Deposit" - flowTotal wallet "Withdraw"]
              pure $ Just scores
        _ -> putStrLn "Divisions skipped: one or more account snapshots failed" >> pure Nothing

storeLiveSnapshot :: DbPool -> Config -> PerpsIndexerStatusRow -> Integer -> RpcBlock -> RpcBlock -> [LiveScore] -> IO ()
storeLiveSnapshot pool cfg cursor weekStart baseline current scores = do
  now <- floor <$> getPOSIXTime
  withDb pool $ \conn -> do
    _ <- execute conn
      "INSERT INTO divisions_live_snapshots \
      \(chain_id,release_router,week_start,baseline_block,baseline_hash,\
      \snapshot_block,snapshot_hash,snapshot_timestamp,wallet_count,standings,captured_at) \
      \VALUES (?,?,?,?,?,?,?,?,?,?::jsonb,to_timestamp(?)) \
      \ON CONFLICT (chain_id,release_router,week_start) DO UPDATE SET \
      \baseline_block=EXCLUDED.baseline_block,baseline_hash=EXCLUDED.baseline_hash,\
      \snapshot_block=EXCLUDED.snapshot_block,snapshot_hash=EXCLUDED.snapshot_hash,\
      \snapshot_timestamp=EXCLUDED.snapshot_timestamp,wallet_count=EXCLUDED.wallet_count,\
      \standings=EXCLUDED.standings,captured_at=EXCLUDED.captured_at \
      \WHERE divisions_live_snapshots.snapshot_block<=EXCLUDED.snapshot_block"
      (cfgPerpsChainId cfg, pisReleaseRouter cursor, weekStart, rpcBlockNumber baseline, rpcBlockHash baseline,
       rpcBlockNumber current, rpcBlockHash current, rpcBlockTimestamp current, length scores,
       TE.decodeUtf8 (LBS.toStrict $ encode $ rankDivisions scores), now :: Integer)
    pure ()
  putStrLn $ "Divisions live captured " <> show (length scores) <> " wallets at block " <> show (rpcBlockNumber current)

-- Append-only publications survive deployments. A failed capture cannot advance
-- the chain, and racing worker instances cannot rewrite an awarded week.
publishNextWeek :: EthClient -> DbPool -> Config -> Int -> PerpsIndexerStatusRow -> RpcBlock -> IO ()
publishNextWeek client pool cfg multicallSize cursor safe = do
  previous <- withDb pool $ \conn -> query conn
    "SELECT p.week_start,p.publication::text FROM divisions_publications p \
    \JOIN divisions_weeks w ON w.chain_id=p.chain_id AND w.week_start=p.week_start AND w.release_router=p.release_router \
    \WHERE p.chain_id=? ORDER BY p.week_start DESC LIMIT 1"
    (Only $ cfgPerpsChainId cfg)
  let decodePrevious = case previous :: [(Integer, Text)] of
        [] -> Right (firstWeekStart, Nothing, [])
        (start, encoded):_ -> do
          value <- eitherDecodeStrict' (TE.encodeUtf8 encoded)
          wallets <- publishedWallets value
          pure (start + weekSeconds, Just value, wallets)
  case decodePrevious of
    Left err -> putStrLn $ "Divisions publication skipped: " <> err
    Right (start, prior, priorWallets) -> do
      now <- floor <$> getPOSIXTime
      pinned <- withDb pool $ \conn -> isPinnedRelease conn (cfgPerpsChainId cfg) (pisReleaseRouter cursor) start
      unless pinned $ putStrLn "Divisions publication skipped: week is pinned to another release"
      when (pinned && now >= publicationTime start && rpcBlockTimestamp safe >= start + 5 * 86400) $ do
        eBaseline <- findLastBlockBeforeTimestamp (ethGetBlockByNumber client) (rpcBlockNumber safe) start
        eClosing <- findLastBlockBeforeTimestamp (ethGetBlockByNumber client) (rpcBlockNumber safe) (start + 5 * 86400)
        case (eBaseline, eClosing) of
          (Right (Just baseline), Right (Just closing)) -> do
            captured <- captureAtBlocks client pool cfg multicallSize cursor baseline closing priorWallets
            case captured of
              Nothing -> pure ()
              Just scores -> case publishDivisionWeek start now baseline closing prior scores of
                Left err -> putStrLn $ "Divisions publication rejected: " <> err
                Right publication -> do
                  inserted <- withDb pool $ \conn -> execute conn
                    "INSERT INTO divisions_publications (chain_id,release_router,week_start,closing_block,publication) \
                    \VALUES (?,?,?,?,?::jsonb) ON CONFLICT DO NOTHING"
                    (cfgPerpsChainId cfg, pisReleaseRouter cursor, start, rpcBlockNumber closing,
                     TE.decodeUtf8 (LBS.toStrict $ encode publication))
                  when (inserted == 1) $ putStrLn $ "Divisions published week " <> show start <> " with " <> show (length scores) <> " wallets"
          _ -> putStrLn "Divisions publication skipped: finalized boundary block unavailable"

matchesBlock :: Either e RpcBlock -> RpcBlock -> Bool
matchesBlock (Right actual) expected =
  T.toLower (rpcBlockHash actual) == T.toLower (rpcBlockHash expected)
matchesBlock _ _ = False

captureValues :: EthClient -> Config -> Int -> Integer -> [Text] -> IO (Maybe [(Text, Integer)])
captureValues client cfg multicallSize block wallets = do
  let batches = if multicallSize == 0 then Right $ map (:[]) wallets
        else chunkInOrder multicallSize wallets
  case batches of
    Left _ -> pure Nothing
    Right chunks -> do
      values <- forM chunks $ \chunk -> do
        ledgers <-
          if multicallSize == 0
            then do
              results <- forM chunk $ \wallet ->
                getAccountLedgerSnapshotAtBlock client (cfgPerpsAccountLens cfg) wallet block
              pure $ either (const Nothing) Just $ sequence results
            else do
              result <- Multicall.multicallAtBlock client
                (accountSnapshotMulticallCalls (cfgPerpsAccountLens cfg) chunk) block
              pure $ case result of
                Left _ -> Nothing
                Right rows -> either (const Nothing) Just $ decodeSnapshotResults (length chunk) rows
        case ledgers of
          Nothing -> pure Nothing
          Just snapshots -> fmap sequence $ forM (zip chunk snapshots) $ \(wallet, ledger) -> do
            carry <- case inferPendingCarry ledger of
              Just value -> pure $ Just value
              Nothing -> either (const Nothing) Just <$>
                getPendingCarryAtBlock client (cfgPerpsCfdEngineLens cfg) wallet block
            pure $ fmap (\value -> (wallet, economicAccountValue $ ledgerToEquity ledger value)) carry
      pure $ fmap concat $ sequence values
