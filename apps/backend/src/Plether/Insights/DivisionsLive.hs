module Plether.Insights.DivisionsLive
  ( ensureDivisionsLiveSchema
  , runDivisionsLiveCycle
  , rankDivisions
  , LiveScore (..)
  ) where

import Control.Monad (forM, unless)
import Data.Aeson (Value, object, (.=), encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query)
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
import Plether.Insights.Competition (economicAccountValue)
import Plether.Insights.SnapshotWorker
  ( accountSnapshotMulticallCalls, chunkInOrder, decodeSnapshotResults
  , findLastBlockBeforeTimestamp, inferPendingCarry, ledgerToEquity
  )
import Text.Read (readMaybe)

firstWeekStart :: Integer
firstWeekStart = 1791752400 -- Sunday 2026-10-11 21:00 UTC

weekSeconds :: Integer
weekSeconds = 7 * 86400

data LiveScore = LiveScore
  { lsAddress :: Text
  , lsAdjustedEquity :: Integer
  , lsPnl :: Integer
  , lsActiveDays :: Int
  } deriving stock (Show, Eq)

-- Exact integer cross products determine rank. Only the displayed return is
-- rounded, so a display tie cannot alter the underlying ordering.
rankDivisions :: [LiveScore] -> [Value]
rankDivisions scores = map render sorted
  where
    sorted = sortBy compareScore scores
    compareScore a b
      | lsAdjustedEquity a <= 0 && lsAdjustedEquity b <= 0 = comparing lsAddress a b
      | lsAdjustedEquity a <= 0 = GT
      | lsAdjustedEquity b <= 0 = LT
      | otherwise = compare (lsPnl b * lsAdjustedEquity a) (lsPnl a * lsAdjustedEquity b)
          <> comparing lsAddress a b
    rankAt score
      | lsAdjustedEquity score <= 0 = Nothing
      | otherwise = Just $ 1 + length (takeWhile (\prior -> lsAdjustedEquity prior > 0 &&
          lsPnl prior * lsAdjustedEquity score > lsPnl score * lsAdjustedEquity prior) sorted)
    render score = object
      [ "address" .= lsAddress score
      , "rank" .= rankAt score
      , "weeklyReturnBps" .= if lsAdjustedEquity score > 0
          then Just (roundedBps (lsPnl score) (lsAdjustedEquity score)) else (Nothing :: Maybe Integer)
      , "activeDays" .= lsActiveDays score
      ]
    roundedBps numerator denominator =
      let magnitude = (abs numerator * 10000 + denominator `div` 2) `div` denominator
       in if numerator < 0 then negate magnitude else magnitude

ensureDivisionsLiveSchema :: Connection -> IO ()
ensureDivisionsLiveSchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS divisions_live_snapshots (\
    \chain_id BIGINT NOT NULL, release_router TEXT NOT NULL, week_start BIGINT NOT NULL,\
    \baseline_block BIGINT NOT NULL, baseline_hash TEXT NOT NULL,\
    \snapshot_block BIGINT NOT NULL, snapshot_hash TEXT NOT NULL,\
    \snapshot_timestamp BIGINT NOT NULL, wallet_count INTEGER NOT NULL,\
    \standings JSONB NOT NULL, captured_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, release_router, week_start))"
  pure ()

runDivisionsLiveCycle :: EthClient -> DbPool -> Config -> Int -> IO ()
runDivisionsLiveCycle client pool cfg multicallSize = do
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
        Right safe | T.toLower (rpcBlockHash safe) == T.toLower safeHash ->
          captureWeek client pool cfg multicallSize cursor safe
        _ -> putStrLn "Divisions live skipped: safe block hash is unavailable or differs from RPC"
    _ -> putStrLn "Divisions live skipped: current release has no finalized indexer cursor"

captureWeek :: EthClient -> DbPool -> Config -> Int -> PerpsIndexerStatusRow -> RpcBlock -> IO ()
captureWeek client pool cfg multicallSize cursor safe
  | rpcBlockTimestamp safe < firstWeekStart = pure ()
  | otherwise = do
      let weekStart = firstWeekStart + ((rpcBlockTimestamp safe - firstWeekStart) `div` weekSeconds) * weekSeconds
          close = weekStart + 5 * 86400
      eBaseline <- findLastBlockBeforeTimestamp (ethGetBlockByNumber client) (rpcBlockNumber safe) weekStart
      eCurrent <- if rpcBlockTimestamp safe < close then pure $ Right $ Just safe
        else findLastBlockBeforeTimestamp (ethGetBlockByNumber client) (rpcBlockNumber safe) close
      case (eBaseline, eCurrent) of
        (Right (Just baseline), Right (Just current)) ->
          captureAtBlocks client pool cfg multicallSize cursor weekStart baseline current
        _ -> putStrLn "Divisions live skipped: weekly boundary block unavailable"

captureAtBlocks :: EthClient -> DbPool -> Config -> Int -> PerpsIndexerStatusRow -> Integer -> RpcBlock -> RpcBlock -> IO ()
captureAtBlocks client pool cfg multicallSize cursor weekStart baseline current = do
  let chain = cfgPerpsChainId cfg
      router = pisReleaseRouter cursor
      upper = rpcBlockNumber current
  wallets <- withDb pool $ \conn -> query conn
    "SELECT DISTINCT LOWER(account) FROM perps_account_activity \
    \WHERE chain_id=? AND release_router=? AND block_number<=? \
    \AND activity_type IN ('Open','Close') AND COALESCE(size_delta,0)<>0 \
    \ORDER BY LOWER(account)"
    (chain, router, upper)
  let addresses = [wallet | Only wallet <- wallets]
  unless (null addresses) $ do
    -- Every flow is checked against the indexed USDC transfer and the exact
    -- clearinghouse. Assisted-close deposits are internal and excluded.
    flowRows <- withDb pool $ \conn -> query conn
      "SELECT LOWER(a.account), a.activity_type, a.amount_usdc::text, \
      \LOWER(COALESCE(a.contract_address,'')), LOWER(COALESCE(a.data->>'asset','')), \
      \(SELECT COUNT(*) FROM perps_usdc_transfers x WHERE x.chain_id=a.chain_id \
      \AND x.release_router=a.release_router AND x.tx_hash=a.tx_hash \
      \AND x.block_number=a.block_number AND x.block_hash=a.block_hash \
      \AND LOWER(x.token_address)=LOWER(?) AND x.amount=a.amount_usdc \
      \AND LOWER(x.from_address)=CASE WHEN a.activity_type='Deposit' THEN LOWER(a.account) ELSE LOWER(?) END \
      \AND LOWER(x.to_address)=CASE WHEN a.activity_type='Deposit' THEN LOWER(?) ELSE LOWER(a.account) END), \
      \EXISTS (SELECT 1 FROM aa_close_assistance g WHERE g.verified \
      \AND g.chain_id=a.chain_id AND g.router=a.release_router AND g.account=a.account \
      \AND g.transaction_hash=a.tx_hash AND g.block_number=a.block_number \
      \AND g.block_hash=a.block_hash AND g.deposit_log_index=a.log_index \
      \AND g.amount_usdc=a.amount_usdc) \
      \FROM perps_account_activity a WHERE a.chain_id=? AND a.release_router=? \
      \AND a.block_number>? AND a.block_number<=? AND a.activity_type IN ('Deposit','Withdraw')"
      (cfgPerpsUsdc cfg, cfgPerpsMarginClearinghouse cfg, cfgPerpsMarginClearinghouse cfg,
       chain, router, rpcBlockNumber baseline, upper)
    let parseFlow (wallet, kind, amountText, emitter, asset, matches, assisted) = do
          amount <- readMaybe (T.unpack amountText)
          if amount <= 0 || emitter /= T.toLower (cfgPerpsMarginClearinghouse cfg)
              || asset /= T.toLower (cfgPerpsUsdc cfg) || matches /= (1 :: Int64)
              || (assisted && kind /= ("Deposit" :: Text))
            then Nothing else Just (wallet, kind, amount, assisted)
    case traverse parseFlow (flowRows :: [(Text, Text, Text, Text, Text, Int64, Bool)]) of
      Nothing -> putStrLn "Divisions live skipped: funding integrity check failed"
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
              then putStrLn "Divisions live skipped: block hash changed during capture"
              else do
                let scores = [LiveScore wallet adjusted (ending - adjusted) (dayCount wallet)
                      | (wallet, ending) <- closing
                      , let adjusted = maybe 0 id (lookup wallet opening) + flowTotal wallet "Deposit" - flowTotal wallet "Withdraw"]
                    standings = rankDivisions scores
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
                    (chain, router, weekStart, rpcBlockNumber baseline, rpcBlockHash baseline,
                     upper, rpcBlockHash current, rpcBlockTimestamp current, length addresses,
                     TE.decodeUtf8 (LBS.toStrict $ encode standings), now :: Integer)
                  pure ()
                putStrLn $ "Divisions live captured " <> show (length addresses) <> " wallets at block " <> show upper
          _ -> putStrLn "Divisions live skipped: one or more account snapshots failed"

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
