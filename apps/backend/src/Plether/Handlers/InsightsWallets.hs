module Plether.Handlers.InsightsWallets
  ( DivisionsWeekRecord (..)
  , BalanceEventRow (..)
  , BalanceHistoryPoint (..)
  , getWalletSummaryResponse
  , extractDivisionsHistory
  , buildBalanceHistory
  , classifyVaultTranche
  , ledgerSnapshotToJson
  ) where

import Control.Concurrent.Async (concurrently)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import Database.PostgreSQL.Simple.FromField (ResultError (ConversionFailed), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field, fieldWith)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadOnly)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Ethereum.Client (EthClient)
import Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( AccountLedgerSnapshot (..)
  , getAccountLedgerSnapshot
  )
import Plether.Handlers.ProtectionHistory (readProtectionHistoryPage)
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import Text.Read (readMaybe)

data AccountReleaseStats = AccountReleaseStats
  { arsTotalTrades :: Int64
  , arsOpensCount :: Int64
  , arsClosesCount :: Int64
  , arsLiquidationsCount :: Int64
  , arsVolumeAssets :: String
  , arsRealizedPnlAssets :: String
  , arsActiveDays :: Int64
  , arsFirstActivityAt :: Maybe Integer
  , arsLastActivityAt :: Maybe Integer
  , arsLatestBlock :: Integer
  }
  deriving stock (Eq, Show)

instance FromRow AccountReleaseStats where
  fromRow =
    AccountReleaseStats
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

data AccountActivityRow = AccountActivityRow
  { aarActivityType :: Text
  , aarOrderId :: Maybe Integer
  , aarSide :: Maybe Int
  , aarPrice :: Maybe Integer
  , aarSizeDelta :: Maybe Integer
  , aarAmountAssets :: Maybe Integer
  , aarPnlAssets :: Maybe Integer
  , aarVpiAssets :: Maybe Integer
  , aarFrozenSpreadAssets :: Maybe Integer
  , aarOracleFrozen :: Maybe Bool
  , aarLatencySeconds :: Maybe Integer
  , aarKeeperBountyAssets :: Maybe Integer
  , aarTxHash :: Text
  , aarBlockNumber :: Integer
  , aarLogIndex :: Integer
  , aarTimestamp :: Integer
  , aarExecution :: Maybe Value
  }
  deriving stock (Eq, Show)

instance FromRow AccountActivityRow where
  fromRow =
    AccountActivityRow
      <$> field
      <*> field
      <*> field
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> field
      <*> field
      <*> nullableNumericIntegerField
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data AccountVaultBalanceRow = AccountVaultBalanceRow
  { avbVaultAddress :: Text
  , avbShareBalance :: Text
  , avbUnclaimedDepositShares :: Text
  , avbWithdrawalEscrowShares :: Text
  , avbTotalAttributedShares :: Text
  , avbObservedBlock :: Text
  }
  deriving stock (Eq, Show)

instance FromRow AccountVaultBalanceRow where
  fromRow =
    AccountVaultBalanceRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data AccountVaultRequestStateRow = AccountVaultRequestStateRow
  { avrsVaultAddress :: Text
  , avrsRequestId :: Text
  , avrsPendingDepositAssets :: Text
  , avrsClaimableDepositAssets :: Text
  , avrsClaimableDepositShares :: Text
  , avrsRefundableDepositAssets :: Text
  , avrsPendingRedeemShares :: Text
  , avrsRefundableRedeemShares :: Text
  , avrsRedeemRefundPending :: Bool
  , avrsIsActive :: Bool
  , avrsObservedBlock :: Text
  }
  deriving stock (Eq, Show)

instance FromRow AccountVaultRequestStateRow where
  fromRow =
    AccountVaultRequestStateRow
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

data AccountVaultRequestEventRow = AccountVaultRequestEventRow
  { avreVaultAddress :: Text
  , avreEventName :: Text
  , avreRequestId :: Text
  , avreRawAmount :: Text
  , avreTxHash :: Text
  , avreBlockNumber :: Text
  , avreBlockTimestamp :: Integer
  }
  deriving stock (Eq, Show)

instance FromRow AccountVaultRequestEventRow where
  fromRow =
    AccountVaultRequestEventRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data BalanceEventRow = BalanceEventRow
  { berActivityType :: Text
  , berAmountAssets :: Maybe Integer
  , berPnlAssets :: Maybe Integer
  , berPreSettlementAssets :: Maybe Integer
  , berPostSettlementAssets :: Maybe Integer
  , berPreClaimAssets :: Maybe Integer
  , berPostClaimAssets :: Maybe Integer
  , berBlockNumber :: Integer
  , berLogIndex :: Integer
  , berTimestamp :: Integer
  }
  deriving stock (Eq, Show)

instance FromRow BalanceEventRow where
  fromRow =
    BalanceEventRow
      <$> field
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> field
      <*> field
      <*> field

data BalanceHistoryPoint = BalanceHistoryPoint
  { bhpTimestamp :: Integer
  , bhpBlockNumber :: Integer
  , bhpBalanceAssets :: Integer
  , bhpNetEquityAssets :: Maybe Integer
  , bhpEventType :: Text
  , bhpSource :: Text
  }
  deriving stock (Eq, Show)

data DivisionsWeekRecord = DivisionsWeekRecord
  { dwrWeekStart :: Integer
  , dwrClosingBlock :: Integer
  , dwrWeekStartsAt :: Maybe Text
  , dwrWeekClosesAt :: Maybe Text
  , dwrPublishedAt :: Maybe Text
  , dwrDisplayName :: Maybe Text
  , dwrRank :: Maybe Integer
  , dwrTier :: Maybe Text
  , dwrPreviousTier :: Maybe Text
  , dwrMovement :: Maybe Text
  , dwrWeeklyPnlAssets :: Maybe Text
  , dwrAdjustedEquityAssets :: Maybe Text
  , dwrActiveDays :: Maybe Integer
  , dwrQualifying :: Maybe Bool
  }
  deriving stock (Eq, Show)

getWalletSummaryResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
getWalletSummaryResponse pool client cfg rawAddress requestedLimit = do
  now <- floor <$> getPOSIXTime
  let account = T.toLower $ T.strip rawAddress
      chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      housePool = T.toLower $ T.strip $ cfgPerpsHousePool cfg
      seniorVault = T.toLower $ T.strip $ cfgVaultHistorySeniorVaultAddress cfg
      juniorVault = T.toLower $ T.strip $ cfgVaultHistoryJuniorVaultAddress cfg
      deployBlock = cfgVaultHistoryDeploymentBlock cfg
      accountLens = T.toLower $ T.strip $ cfgPerpsAccountLens cfg
      book = T.toLower Manifest.positionProtectionBookAddress
      limit = min 100 $ max 1 requestedLimit
  (ledgerRes, dbTuple) <-
    concurrently
      (getAccountLedgerSnapshot client accountLens account)
      ( withDb pool $ \conn ->
          withReadSnapshot conn $ do
            handle <- fetchDiscordHandle conn account
            stats <- fetchAccountReleaseStats conn chainId router account
            activity <- fetchAccountActivity conn chainId router account limit
            balanceEvents <- fetchAccountBalanceEvents conn chainId router account
            (protections, _) <- readProtectionHistoryPage conn chainId book account 30 Nothing
            protBlock <- fetchProtectionIndexedBlock conn chainId book
            divHistory <- fetchAccountDivisionsHistory conn chainId router account
            lpBalances <- fetchAccountVaultBalances conn chainId housePool deployBlock account
            lpRequests <- fetchAccountVaultRequestStates conn chainId housePool deployBlock account
            lpEvents <- fetchAccountVaultRequestEvents conn chainId housePool deployBlock account
            pure (handle, stats, activity, balanceEvents, protections, protBlock, divHistory, lpBalances, lpRequests, lpEvents)
      )
  let (mHandle, stats, activity, balanceEvents, protections, protBlock, divHistory, lpBalances, lpRequests, lpEvents) = dbTuple
      mLedger = either (const Nothing) Just ledgerRes
      latestBlock = maximum [arsLatestBlock stats, protBlock, 0]
      balanceHistory = buildBalanceHistory now latestBlock mLedger balanceEvents
      firstDivName = listToMaybe $ mapMaybe dwrDisplayName divHistory
      displayHandle = case mHandle of
        Just h | not (T.null $ T.strip h) -> Just (T.strip h)
        _ -> firstDivName
  pure $
    Right $
      mkResponse latestBlock chainId $
        object
          [ "network"
              .= object
                [ "chainId" .= chainId
                , "releaseVersion" .= Manifest.releaseVersion
                , "settlementAssetSymbol" .= ("USDG" :: Text)
                ]
          , "account" .= account
          , "observedAt" .= isoTimestamp now
          , "identity"
              .= object
                [ "discordUsername" .= displayHandle
                , "isDivisionsLinked" .= maybe False (not . T.null . T.strip) mHandle
                ]
          , "ledgerAvailable" .= maybe False (const True) mLedger
          , "ledger" .= (ledgerSnapshotToJson <$> mLedger)
          , "performance" .= releaseStatsToJson stats
          , "balanceHistory" .= map balanceHistoryPointToJson balanceHistory
          , "activity" .= map accountActivityToJson activity
          , "protections"
              .= object
                [ "indexedThroughBlock" .= show protBlock
                , "items" .= protections
                ]
          , "lp"
              .= object
                [ "balances" .= map (vaultBalanceToJson seniorVault juniorVault) lpBalances
                , "requestStates" .= map (vaultRequestStateToJson seniorVault juniorVault) lpRequests
                , "requestEvents" .= map (vaultRequestEventToJson seniorVault juniorVault) lpEvents
                ]
          , "divisionsHistory" .= map divisionsWeekToJson divHistory
          ]

classifyVaultTranche :: Text -> Text -> Text -> Text
classifyVaultTranche seniorAddr juniorAddr vaultAddr
  | normalized == T.toLower (T.strip seniorAddr) = "senior"
  | normalized == T.toLower (T.strip juniorAddr) = "junior"
  | otherwise = "unknown"
  where
    normalized = T.toLower $ T.strip vaultAddr

-- | Build a chronological 90-day settlement + claim balance history from
-- canonical execution receipts, deposits/withdrawals, and the live ledger
-- snapshot. Preserves the latest pre-90d baseline anchor when present so
-- window slices (7d / 30d / 90d) never start at a manufactured zero.
buildBalanceHistory
  :: Integer
  -> Integer
  -> Maybe AccountLedgerSnapshot
  -> [BalanceEventRow]
  -> [BalanceHistoryPoint]
buildBalanceHistory nowUnix latestBlock mLedger events =
  let (_, rawPoints) = foldl step (0, []) events
      chronological = reverse rawPoints
      withLive = case mLedger of
        Just snap
          | not (null chronological)
              || alsSettlementBalanceAssets snap > 0
              || alsTraderClaimBalanceAssets snap > 0
              || alsHasPosition snap ->
              chronological
                <> [ BalanceHistoryPoint
                      { bhpTimestamp = nowUnix
                      , bhpBlockNumber = latestBlock
                      , bhpBalanceAssets = alsSettlementBalanceAssets snap + alsTraderClaimBalanceAssets snap
                      , bhpNetEquityAssets = Just (alsNetEquityAssets snap)
                      , bhpEventType = "Live"
                      , bhpSource = "ledger"
                      }
                   ]
        _ -> chronological
      cutoff90d = nowUnix - 90 * 86400
      (before90d, in90d) = span (\pt -> bhpTimestamp pt < cutoff90d) withLive
      baseline = maybe [] pure $ listToMaybe (reverse before90d)
   in downsamplePoints 200 (baseline <> in90d)
  where
    step (running, acc) ev =
      case berPostSettlementAssets ev of
        Just postSettle ->
          let postTotal = max 0 (postSettle + fromMaybe 0 (berPostClaimAssets ev))
              pt =
                BalanceHistoryPoint
                  { bhpTimestamp = berTimestamp ev
                  , bhpBlockNumber = berBlockNumber ev
                  , bhpBalanceAssets = postTotal
                  , bhpNetEquityAssets = Nothing
                  , bhpEventType = berActivityType ev
                  , bhpSource = "receipt"
                  }
           in (postTotal, pt : acc)
        Nothing ->
          case berActivityType ev of
            "Deposit" ->
              let nextBal = max 0 (running + fromMaybe 0 (berAmountAssets ev))
                  pt = BalanceHistoryPoint (berTimestamp ev) (berBlockNumber ev) nextBal Nothing "Deposit" "transfer"
               in (nextBal, pt : acc)
            "Withdraw" ->
              let nextBal = max 0 (running - fromMaybe 0 (berAmountAssets ev))
                  pt = BalanceHistoryPoint (berTimestamp ev) (berBlockNumber ev) nextBal Nothing "Withdraw" "transfer"
               in (nextBal, pt : acc)
            "Liquidated" ->
              let nextBal = max 0 (running + fromMaybe 0 (berPnlAssets ev))
                  pt = BalanceHistoryPoint (berTimestamp ev) (berBlockNumber ev) nextBal Nothing "Liquidated" "estimated"
               in (nextBal, pt : acc)
            _ -> (running, acc)

downsamplePoints :: Int -> [BalanceHistoryPoint] -> [BalanceHistoryPoint]
downsamplePoints maxPts pts
  | length pts <= maxPts = pts
  | otherwise =
      let n = length pts
          stride = max 1 ((n - 1) `div` max 1 (maxPts - 1))
          indexed = zip [0 :: Int ..] pts
          picked = [p | (i, p) <- indexed, i == 0 || i == n - 1 || i `mod` stride == 0]
       in picked

extractDivisionsHistory :: Text -> [(Integer, Integer, Value)] -> [DivisionsWeekRecord]
extractDivisionsHistory targetAccount =
  mapMaybe (extractOne (T.toLower $ T.strip targetAccount))
  where
    extractOne acct (weekStart, closingBlock, pubVal) = do
      pubObj <- asObject pubVal
      standingsArr <- lookupArray "standings" pubObj
      standingObj <- listToMaybe $ filter (matchesAddress acct) $ mapMaybe asObject standingsArr
      let weekObj = lookupObject "week" pubObj
      pure
        DivisionsWeekRecord
          { dwrWeekStart = weekStart
          , dwrClosingBlock = closingBlock
          , dwrWeekStartsAt = weekObj >>= lookupText "startsAt"
          , dwrWeekClosesAt = weekObj >>= lookupText "closesAt"
          , dwrPublishedAt = lookupText "publishedAt" pubObj
          , dwrDisplayName = lookupText "displayName" standingObj
          , dwrRank = lookupInteger "rank" standingObj
          , dwrTier = lookupText "tier" standingObj
          , dwrPreviousTier = lookupText "previousTier" standingObj
          , dwrMovement = lookupText "movement" standingObj
          , dwrWeeklyPnlAssets = lookupText "weeklyPnlAssets" standingObj
          , dwrAdjustedEquityAssets = lookupText "adjustedEquityAssets" standingObj
          , dwrActiveDays = lookupInteger "activeDays" standingObj
          , dwrQualifying = lookupBool "qualifying" standingObj
          }

    matchesAddress acct obj =
      fmap (T.toLower . T.strip) (lookupText "address" obj) == Just acct

ledgerSnapshotToJson :: AccountLedgerSnapshot -> Value
ledgerSnapshotToJson AccountLedgerSnapshot {..} =
  object
    [ "settlementBalanceAssets" .= show alsSettlementBalanceAssets
    , "freeSettlementAssets" .= show alsFreeSettlementAssets
    , "activePositionMarginAssets" .= show alsActivePositionMarginAssets
    , "otherLockedMarginAssets" .= show alsOtherLockedMarginAssets
    , "committedMarginAssets" .= show alsCommittedMarginAssets
    , "executionBountyReserveAssets" .= show alsExecutionBountyReserveAssets
    , "traderClaimBalanceAssets" .= show alsTraderClaimBalanceAssets
    , "pendingOrderCount" .= alsPendingOrderCount
    , "accountEquityAssets" .= show alsAccountEquityAssets
    , "freeBuyingPowerAssets" .= show alsFreeBuyingPowerAssets
    , "netEquityAssets" .= show alsNetEquityAssets
    , "hasPosition" .= alsHasPosition
    , "position"
        .= if alsHasPosition
          then
            Just $
              object
                [ "market" .= ("plDXY-PERP" :: Text)
                , "side" .= sideTextInteger alsSide
                , "size" .= show alsSize
                , "marginAssets" .= show alsMargin
                , "entryPrice" .= show alsEntryPrice
                , "unrealizedPnlAssets" .= show alsUnrealizedPnlAssets
                , "liquidatable" .= alsLiquidatable
                ]
          else Nothing
    ]

fetchDiscordHandle :: Connection -> Text -> IO (Maybe Text)
fetchDiscordHandle conn account = do
  rows <-
    query
      conn
      "SELECT discord_username FROM insights_divisions_links WHERE trading_account = ? LIMIT 1"
      (Only account) :: IO [Only Text]
  pure $ fromOnly <$> listToMaybe rows

fetchAccountReleaseStats :: Connection -> Integer -> Text -> Text -> IO AccountReleaseStats
fetchAccountReleaseStats conn chainId router account = do
  rows <-
    query
      conn
      "SELECT \
      \  COUNT(*) FILTER (WHERE activity_type IN ('Open', 'Close', 'Liquidated')), \
      \  COUNT(*) FILTER (WHERE activity_type = 'Open'), \
      \  COUNT(*) FILTER (WHERE activity_type = 'Close'), \
      \  COUNT(*) FILTER (WHERE activity_type = 'Liquidated'), \
      \  COALESCE(SUM(CASE WHEN activity_type IN ('Open', 'Close', 'Liquidated') THEN ABS(COALESCE(amount_assets, 0)) ELSE 0 END), 0)::TEXT, \
      \  COALESCE(SUM(COALESCE(pnl_assets, 0)), 0)::TEXT, \
      \  COUNT(DISTINCT CASE WHEN activity_type IN ('Open', 'Close', 'Liquidated') THEN (to_timestamp(timestamp) AT TIME ZONE 'UTC')::date END), \
      \  MIN(timestamp), \
      \  MAX(timestamp), \
      \  COALESCE(MAX(block_number), 0) \
      \FROM perps_account_activity \
      \WHERE chain_id = ? AND release_router = ? AND account = ?"
      (chainId, router, account) :: IO [AccountReleaseStats]
  pure $
    fromMaybe
      (AccountReleaseStats 0 0 0 0 "0" "0" 0 Nothing Nothing 0)
      (listToMaybe rows)

fetchAccountActivity :: Connection -> Integer -> Text -> Text -> Int -> IO [AccountActivityRow]
fetchAccountActivity conn chainId router account limit =
  query
    conn
    "SELECT a.activity_type, a.order_id, a.side, a.price, a.size_delta, \
    \a.amount_assets, a.pnl_assets, o.execution_vpi_assets, o.execution_frozen_close_spread_assets, \
    \o.execution_oracle_frozen, \
    \CASE WHEN o.commit_timestamp IS NOT NULL AND o.terminal_timestamp IS NOT NULL AND o.terminal_timestamp >= o.commit_timestamp \
    \  THEN o.terminal_timestamp - o.commit_timestamp ELSE NULL END, \
    \CASE WHEN a.data->>'keeperBountyAssets' ~ '^[0-9]+$' THEN (a.data->>'keeperBountyAssets')::NUMERIC ELSE NULL END, \
    \a.tx_hash, a.block_number, a.log_index, a.timestamp, execution.payload \
    \FROM perps_account_activity a \
    \LEFT JOIN perps_orders o \
    \  ON o.chain_id = a.chain_id AND o.order_router = a.release_router AND o.order_id = a.order_id \
    \LEFT JOIN LATERAL (\
    \  SELECT CASE WHEN COUNT(*) = 1 THEN (jsonb_agg(candidate.payload))->0 ELSE NULL END AS payload \
    \  FROM (\
    \    SELECT jsonb_build_object(\
    \      'orderId', o2.order_id::text, 'protocolVersion', 'v1.2.3', \
    \      'status', CASE WHEN o2.settlement_evidence_version = 1 THEN 'complete' ELSE 'pending' END, \
    \      'receipt', o2.receipt_economics, \
    \      'settlements', COALESCE((\
    \        SELECT jsonb_agg(e.data || jsonb_build_object('kind', e.event_name) ORDER BY e.log_index) \
    \        FROM perps_events e \
    \        WHERE e.chain_id = a.chain_id AND e.release_router = a.release_router \
    \          AND e.tx_hash = a.tx_hash AND e.block_hash = a.block_hash AND e.block_number = a.block_number \
    \          AND e.account = a.account AND e.order_id = o2.order_id \
    \          AND e.event_name IN ('ActionChargeSettled', 'ActionRebateSettled', 'FrozenCloseSpreadSettled')\
    \      ), '[]'::jsonb)\
    \    ) AS payload \
    \    FROM perps_events terminal \
    \    JOIN perps_orders o2 ON o2.chain_id = terminal.chain_id AND o2.order_router = terminal.release_router \
    \      AND o2.order_id = terminal.order_id AND o2.terminal_status = 'Executed' \
    \      AND o2.account = a.account AND o2.terminal_tx_hash = a.tx_hash AND o2.terminal_block_number = a.block_number \
    \      AND o2.receipt_economics IS NOT NULL AND o2.receipt_hash = terminal.data->>'receiptHash' \
    \    WHERE terminal.chain_id = a.chain_id AND terminal.release_router = a.release_router \
    \      AND terminal.tx_hash = a.tx_hash AND terminal.block_hash = a.block_hash AND terminal.block_number = a.block_number \
    \      AND terminal.account = a.account AND terminal.event_name = 'OrderFinalized' \
    \      AND terminal.data->>'status' = '2' AND terminal.data->>'terminalReason' = 'Executed' \
    \      AND terminal.log_index > a.log_index AND a.activity_type IN ('Open', 'Close') \
    \      AND NOT EXISTS (\
    \        SELECT 1 FROM perps_events prior WHERE prior.chain_id = a.chain_id AND prior.release_router = a.release_router \
    \          AND prior.tx_hash = a.tx_hash AND prior.block_hash = a.block_hash \
    \          AND prior.event_name = 'OrderFinalized' AND prior.log_index > a.log_index AND prior.log_index < terminal.log_index\
    \      ) \
    \      AND 1 = (\
    \        SELECT COUNT(*) FROM perps_account_activity sibling \
    \        WHERE sibling.chain_id = a.chain_id AND sibling.release_router = a.release_router \
    \          AND sibling.tx_hash = a.tx_hash AND sibling.block_hash = a.block_hash AND sibling.account = a.account \
    \          AND sibling.activity_type IN ('Open', 'Close') AND sibling.log_index < terminal.log_index \
    \          AND sibling.log_index > COALESCE((\
    \            SELECT MAX(prior.log_index) FROM perps_events prior \
    \            WHERE prior.chain_id = a.chain_id AND prior.release_router = a.release_router \
    \              AND prior.tx_hash = a.tx_hash AND prior.block_hash = a.block_hash \
    \              AND prior.event_name = 'OrderFinalized' AND prior.log_index < terminal.log_index\
    \          ), -1)\
    \      )\
    \  ) candidate\
    \) execution ON TRUE \
    \WHERE a.chain_id = ? AND a.release_router = ? AND a.account = ? \
    \ORDER BY a.block_number DESC, a.log_index DESC \
    \LIMIT ?"
    (chainId, router, account, limit)

fetchAccountBalanceEvents :: Connection -> Integer -> Text -> Text -> IO [BalanceEventRow]
fetchAccountBalanceEvents conn chainId router account =
  query
    conn
    "SELECT \
    \  a.activity_type, \
    \  a.amount_assets, \
    \  a.pnl_assets, \
    \  CASE WHEN o.receipt_economics->>'preSettlementBalanceAssets' ~ '^-?[0-9]+$' \
    \    THEN (o.receipt_economics->>'preSettlementBalanceAssets')::NUMERIC ELSE NULL END, \
    \  CASE WHEN o.receipt_economics->>'postSettlementBalanceAssets' ~ '^-?[0-9]+$' \
    \    THEN (o.receipt_economics->>'postSettlementBalanceAssets')::NUMERIC ELSE NULL END, \
    \  CASE WHEN o.receipt_economics->>'preTraderClaimBalanceAssets' ~ '^-?[0-9]+$' \
    \    THEN (o.receipt_economics->>'preTraderClaimBalanceAssets')::NUMERIC ELSE NULL END, \
    \  CASE WHEN o.receipt_economics->>'postTraderClaimBalanceAssets' ~ '^-?[0-9]+$' \
    \    THEN (o.receipt_economics->>'postTraderClaimBalanceAssets')::NUMERIC ELSE NULL END, \
    \  a.block_number, \
    \  a.log_index, \
    \  a.timestamp \
    \FROM perps_account_activity a \
    \LEFT JOIN perps_orders o \
    \  ON o.chain_id = a.chain_id AND o.order_router = a.release_router \
    \  AND o.order_id = a.order_id AND o.terminal_status = 'Executed' \
    \WHERE a.chain_id = ? AND a.release_router = ? AND a.account = ? \
    \  AND a.activity_type IN ('Deposit', 'Withdraw', 'Open', 'Close', 'Liquidated') \
    \ORDER BY a.block_number ASC, a.log_index ASC \
    \LIMIT 2000"
    (chainId, router, account)

fetchProtectionIndexedBlock :: Connection -> Integer -> Text -> IO Integer
fetchProtectionIndexedBlock conn chainId book = do
  rows <-
    query
      conn
      "SELECT block_number FROM perps_protection_checkpoints \
      \WHERE chain_id = ? AND book = ? ORDER BY block_number DESC LIMIT 1"
      (chainId, book) :: IO [Only Integer]
  pure $ maybe 0 fromOnly $ listToMaybe rows

fetchAccountDivisionsHistory :: Connection -> Integer -> Text -> Text -> IO [DivisionsWeekRecord]
fetchAccountDivisionsHistory conn chainId router account = do
  rows <-
    query
      conn
      "SELECT week_start, closing_block, publication \
      \FROM divisions_publications \
      \WHERE chain_id = ? AND release_router = ? \
      \ORDER BY week_start DESC LIMIT 20"
      (chainId, router) :: IO [(Integer, Integer, Value)]
  pure $ extractDivisionsHistory account rows

fetchAccountVaultBalances
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> IO [AccountVaultBalanceRow]
fetchAccountVaultBalances conn chainId housePool deployBlock account =
  query
    conn
    "SELECT vault_address, share_balance::TEXT, unclaimed_deposit_shares::TEXT, \
    \withdrawal_escrow_shares::TEXT, total_attributed_shares::TEXT, observed_block::TEXT \
    \FROM vault_attributed_holder_balances \
    \WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND holder_address = ? \
    \ORDER BY vault_address ASC"
    (chainId, housePool, deployBlock, account)

fetchAccountVaultRequestStates
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> IO [AccountVaultRequestStateRow]
fetchAccountVaultRequestStates conn chainId housePool deployBlock account =
  query
    conn
    "SELECT vault_address, request_id::TEXT, pending_deposit_assets::TEXT, \
    \claimable_deposit_assets::TEXT, claimable_deposit_shares::TEXT, refundable_deposit_assets::TEXT, \
    \pending_redeem_shares::TEXT, refundable_redeem_shares::TEXT, redeem_refund_pending, \
    \is_active, observed_block::TEXT \
    \FROM vault_deposit_request_states \
    \WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND controller_address = ? \
    \ORDER BY is_active DESC, request_id DESC LIMIT 30"
    (chainId, housePool, deployBlock, account)

fetchAccountVaultRequestEvents
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> IO [AccountVaultRequestEventRow]
fetchAccountVaultRequestEvents conn chainId housePool deployBlock account =
  query
    conn
    "SELECT vault_address, event_name, request_id::TEXT, raw_amount::TEXT, \
    \tx_hash, block_number::TEXT, block_timestamp \
    \FROM vault_request_events \
    \WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? \
    \  AND (controller_address = ? OR owner_address = ?) \
    \ORDER BY block_number DESC, log_index DESC LIMIT 30"
    (chainId, housePool, deployBlock, account, account)

balanceHistoryPointToJson :: BalanceHistoryPoint -> Value
balanceHistoryPointToJson BalanceHistoryPoint {..} =
  object $
    catMaybes
      [ Just $ "occurredAt" .= isoTimestamp bhpTimestamp
      , Just $ "timestamp" .= bhpTimestamp
      , Just $ "blockNumber" .= show bhpBlockNumber
      , Just $ "balanceAssets" .= show bhpBalanceAssets
      , ("netEquityAssets" .=) . show <$> bhpNetEquityAssets
      , Just $ "eventType" .= bhpEventType
      , Just $ "source" .= bhpSource
      ]

releaseStatsToJson :: AccountReleaseStats -> Value
releaseStatsToJson AccountReleaseStats {..} =
  object
    [ "totalTrades" .= arsTotalTrades
    , "opensCount" .= arsOpensCount
    , "closesCount" .= arsClosesCount
    , "liquidationsCount" .= arsLiquidationsCount
    , "volumeAssets" .= arsVolumeAssets
    , "realizedPnlAssets" .= arsRealizedPnlAssets
    , "activeDays" .= arsActiveDays
    , "firstActivityAt" .= (isoTimestamp <$> arsFirstActivityAt)
    , "lastActivityAt" .= (isoTimestamp <$> arsLastActivityAt)
    ]

accountActivityToJson :: AccountActivityRow -> Value
accountActivityToJson AccountActivityRow {..} =
  object $
    catMaybes
      [ Just $ "id" .= (aarTxHash <> ":" <> T.pack (show aarLogIndex))
      , Just $ "activityType" .= aarActivityType
      , ("orderId" .=) . show <$> aarOrderId
      , ("side" .=) <$> (sideTextInt <$> aarSide)
      , ("price" .=) . show <$> aarPrice
      , ("sizeDelta" .=) . show <$> aarSizeDelta
      , ("amountAssets" .=) . show <$> aarAmountAssets
      , ("pnlAssets" .=) . show <$> aarPnlAssets
      , ("vpiAssets" .=) . show <$> aarVpiAssets
      , ("frozenCloseSpreadAssets" .=) . show <$> aarFrozenSpreadAssets
      , ("oracleFrozen" .=) <$> aarOracleFrozen
      , ("latencySeconds" .=) <$> aarLatencySeconds
      , ("keeperBountyAssets" .=) . show <$> aarKeeperBountyAssets
      , ("execution" .=) <$> aarExecution
      , Just $ "txHash" .= aarTxHash
      , Just $ "blockNumber" .= show aarBlockNumber
      , Just $ "occurredAt" .= isoTimestamp aarTimestamp
      ]

vaultBalanceToJson :: Text -> Text -> AccountVaultBalanceRow -> Value
vaultBalanceToJson seniorAddr juniorAddr AccountVaultBalanceRow {..} =
  object
    [ "tranche" .= classifyVaultTranche seniorAddr juniorAddr avbVaultAddress
    , "vaultAddress" .= avbVaultAddress
    , "shareBalance" .= avbShareBalance
    , "unclaimedDepositShares" .= avbUnclaimedDepositShares
    , "withdrawalEscrowShares" .= avbWithdrawalEscrowShares
    , "totalAttributedShares" .= avbTotalAttributedShares
    , "observedBlock" .= avbObservedBlock
    ]

vaultRequestStateToJson :: Text -> Text -> AccountVaultRequestStateRow -> Value
vaultRequestStateToJson seniorAddr juniorAddr AccountVaultRequestStateRow {..} =
  object
    [ "tranche" .= classifyVaultTranche seniorAddr juniorAddr avrsVaultAddress
    , "vaultAddress" .= avrsVaultAddress
    , "requestId" .= avrsRequestId
    , "pendingDepositAssets" .= avrsPendingDepositAssets
    , "claimableDepositAssets" .= avrsClaimableDepositAssets
    , "claimableDepositShares" .= avrsClaimableDepositShares
    , "refundableDepositAssets" .= avrsRefundableDepositAssets
    , "pendingRedeemShares" .= avrsPendingRedeemShares
    , "refundableRedeemShares" .= avrsRefundableRedeemShares
    , "redeemRefundPending" .= avrsRedeemRefundPending
    , "isActive" .= avrsIsActive
    , "observedBlock" .= avrsObservedBlock
    ]

vaultRequestEventToJson :: Text -> Text -> AccountVaultRequestEventRow -> Value
vaultRequestEventToJson seniorAddr juniorAddr AccountVaultRequestEventRow {..} =
  object
    [ "tranche" .= classifyVaultTranche seniorAddr juniorAddr avreVaultAddress
    , "vaultAddress" .= avreVaultAddress
    , "eventName" .= avreEventName
    , "requestId" .= avreRequestId
    , "rawAmount" .= avreRawAmount
    , "txHash" .= avreTxHash
    , "blockNumber" .= avreBlockNumber
    , "occurredAt" .= isoTimestamp avreBlockTimestamp
    ]

divisionsWeekToJson :: DivisionsWeekRecord -> Value
divisionsWeekToJson DivisionsWeekRecord {..} =
  object
    [ "weekStart" .= dwrWeekStart
    , "closingBlock" .= show dwrClosingBlock
    , "weekStartsAt" .= dwrWeekStartsAt
    , "weekClosesAt" .= dwrWeekClosesAt
    , "publishedAt" .= dwrPublishedAt
    , "displayName" .= dwrDisplayName
    , "rank" .= dwrRank
    , "tier" .= dwrTier
    , "previousTier" .= dwrPreviousTier
    , "movement" .= dwrMovement
    , "weeklyPnlAssets" .= dwrWeeklyPnlAssets
    , "adjustedEquityAssets" .= dwrAdjustedEquityAssets
    , "activeDays" .= dwrActiveDays
    , "qualifying" .= dwrQualifying
    ]

asObject :: Value -> Maybe (KM.KeyMap Value)
asObject (Object o) = Just o
asObject _ = Nothing

lookupObject :: Text -> KM.KeyMap Value -> Maybe (KM.KeyMap Value)
lookupObject k obj = KM.lookup (Key.fromText k) obj >>= asObject

lookupArray :: Text -> KM.KeyMap Value -> Maybe [Value]
lookupArray k obj = case KM.lookup (Key.fromText k) obj of
  Just (Array arr) -> Just (toList arr)
  _ -> Nothing

lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText k obj = case KM.lookup (Key.fromText k) obj of
  Just (String s) -> Just s
  _ -> Nothing

lookupBool :: Text -> KM.KeyMap Value -> Maybe Bool
lookupBool k obj = case KM.lookup (Key.fromText k) obj of
  Just (Bool b) -> Just b
  _ -> Nothing

lookupInteger :: Text -> KM.KeyMap Value -> Maybe Integer
lookupInteger k obj = case KM.lookup (Key.fromText k) obj of
  Just (Number n) ->
    let rounded = round n
     in if fromInteger rounded == n then Just rounded else Nothing
  Just (String s) -> readMaybe (T.unpack s)
  _ -> Nothing

sideTextInt :: Int -> Text
sideTextInt 0 = "long"
sideTextInt 1 = "short"
sideTextInt _ = "unknown"

sideTextInteger :: Integer -> Text
sideTextInteger 0 = "long"
sideTextInteger 1 = "short"
sideTextInteger _ = "unknown"

withReadSnapshot :: Connection -> IO a -> IO a
withReadSnapshot conn =
  withTransactionMode
    TransactionMode
      { isolationLevel = RepeatableRead
      , readWriteMode = ReadOnly
      }
    conn

nullableNumericIntegerField :: RowParser (Maybe Integer)
nullableNumericIntegerField =
  fieldWith $ \column raw ->
    case raw of
      Nothing -> pure Nothing
      Just bytes -> case readMaybe (BS8.unpack bytes) of
        Just integer -> pure $ Just integer
        Nothing -> returnError ConversionFailed column "Expected nullable integer NUMERIC column"

isoTimestamp :: Integer -> Text
isoTimestamp =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . posixSecondsToUTCTime . fromIntegral
