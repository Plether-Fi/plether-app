module Plether.Handlers.InsightsWallets
  ( AccountReleaseStats (..)
  , AccountActivityRow (..)
  , AccountVaultBalanceRow (..)
  , AccountVaultRequestStateRow (..)
  , AccountVaultRequestEventRow (..)
  , DivisionsWeekRecord (..)
  , DivisionsEquityBridge (..)
  , BalanceEventRow (..)
  , BalanceHistoryPoint (..)
  , getWalletSummaryResponse
  , getDivisionsWalletContextResponse
  , buildDivisionsEquityBridge
  , fetchDiscordHandle
  , fetchAccountReleaseStats
  , fetchAccountActivity
  , fetchAccountBalanceEvents
  , fetchProtectionIndexedBlock
  , fetchAccountDivisionsHistory
  , fetchAccountVaultBalances
  , fetchAccountVaultRequestStates
  , fetchAccountVaultRequestEvents
  , extractDivisionsHistory
  , buildBalanceHistory
  , classifyVaultTranche
  , ledgerSnapshotToJson
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (..), concurrently)
import Data.Aeson (Value (..), decodeStrict', object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
import Plether.Cache (SingleFlightCache, newConcurrentRefreshingSingleFlightCache, runSingleFlightCache)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Ethereum.Client (EthClient)
import Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( AccountLedgerSnapshot (..)
  , getAccountLedgerSnapshot
  , getAccountLedgerSnapshotAtBlock
  , getPendingCarryAtBlock
  )
import Plether.Handlers.InsightsOrderProtections
  ( OrderProtectionsBundle
  , buildOrderProtectionsForOrder
  , extractExecutionOrderId
  , fetchOrderProtectionsBundle
  )
import Plether.Handlers.ProtectionHistory (readProtectionHistoryPage)
import Plether.Insights.Competition (economicAccountValue)
import Plether.Insights.Divisions (divisionWeekStart)
import Plether.Insights.SnapshotWorker (inferPendingCarry, ledgerToEquity)
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
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

{-# NOINLINE walletSummaryCache #-}
walletSummaryCache :: SingleFlightCache (Text, Int) (Either ApiError (ApiResponse Value))
walletSummaryCache =
  unsafePerformIO (newConcurrentRefreshingSingleFlightCache 128 (15 * 1000000000) (300 * 1000000000))

getWalletSummaryResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
getWalletSummaryResponse pool client cfg rawAddress requestedLimit =
  let account = T.toLower $ T.strip rawAddress
      limit = min 100 $ max 1 requestedLimit
   in snd
        <$> runSingleFlightCache
          walletSummaryCache
          (account, limit)
          isRight
          (computeWalletSummaryResponse pool client cfg account limit)

computeWalletSummaryResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
computeWalletSummaryResponse pool client cfg account limit = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      housePool = T.toLower $ T.strip $ cfgPerpsHousePool cfg
      seniorVault = T.toLower $ T.strip $ cfgVaultHistorySeniorVaultAddress cfg
      juniorVault = T.toLower $ T.strip $ cfgVaultHistoryJuniorVaultAddress cfg
      deployBlock = cfgVaultHistoryDeploymentBlock cfg
      accountLens = T.toLower $ T.strip $ cfgPerpsAccountLens cfg
      book = T.toLower Manifest.positionProtectionBookAddress
      hasConfiguredVaults = not (T.null seniorVault) && not (T.null juniorVault)
  (mLedgerRes, dbTuple) <-
    concurrently
      (timeout 2500000 (getAccountLedgerSnapshot client accountLens account))
      ( withDb pool $ \conn ->
          withReadSnapshot conn $ do
            handle <- fetchDiscordHandle conn account
            stats <- fetchAccountReleaseStats conn chainId router account
            activity <- fetchAccountActivity conn chainId router account limit
            let openOrderIds =
                  mapMaybe
                    (\r -> aarOrderId r <|> extractExecutionOrderId (aarExecution r))
                    (filter (\r -> aarActivityType r == "Open") activity)
            protBundle <- fetchOrderProtectionsBundle conn chainId router book openOrderIds
            balanceEvents <- fetchAccountBalanceEvents conn chainId router account
            (protections, _) <- readProtectionHistoryPage conn chainId book account 30 Nothing
            protBlock <- fetchProtectionIndexedBlock conn chainId book
            divHistory <- fetchAccountDivisionsHistory conn chainId router account
            lpBalances <-
              if hasConfiguredVaults
                then fetchAccountVaultBalancesForVaults conn chainId housePool deployBlock seniorVault juniorVault account
                else fetchAccountVaultBalances conn chainId housePool deployBlock account
            lpRequests <-
              if hasConfiguredVaults
                then fetchAccountVaultRequestStatesForVaults conn chainId housePool deployBlock seniorVault juniorVault account
                else fetchAccountVaultRequestStates conn chainId housePool deployBlock account
            lpEvents <-
              if hasConfiguredVaults
                then fetchAccountVaultRequestEventsForVaults conn chainId housePool deployBlock seniorVault juniorVault account
                else fetchAccountVaultRequestEvents conn chainId housePool deployBlock account
            pure (handle, stats, activity, protBundle, balanceEvents, protections, protBlock, divHistory, lpBalances, lpRequests, lpEvents)
      )
  let (mHandle, stats, activity, protBundle, balanceEvents, protections, protBlock, divHistory, lpBalances, lpRequests, lpEvents) = dbTuple
      mLedger = case mLedgerRes of
        Just (Right snap) -> Just snap
        _ -> Nothing
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
          , "activity" .= map (accountActivityToJson protBundle) activity
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
   in downsamplePoints 200 withLive
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
    \          AND sibling.tx_hash = a.tx_hash AND sibling.block_hash = a.block_hash AND sibling.block_number = a.block_number AND sibling.account = a.account \
    \          AND sibling.activity_type IN ('Open', 'Close') AND sibling.log_index < terminal.log_index \
    \          AND sibling.log_index > COALESCE((\
    \            SELECT MAX(prior.log_index) FROM perps_events prior \
    \            WHERE prior.chain_id = a.chain_id AND prior.release_router = a.release_router \
    \              AND prior.tx_hash = a.tx_hash AND prior.block_hash = a.block_hash \
    \              AND prior.event_name = 'OrderFinalized' AND prior.log_index < terminal.log_index\
    \          ), -1)\
    \      )\
    \  ) candidate\
    \) execution ON a.order_id IS NULL AND a.activity_type IN ('Open', 'Close') \
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
    \  CASE WHEN COALESCE(o.receipt_economics, resolved.receipt_economics)->>'preSettlementBalanceAssets' ~ '^-{0,1}[0-9]+$' \
    \    THEN (COALESCE(o.receipt_economics, resolved.receipt_economics)->>'preSettlementBalanceAssets')::NUMERIC ELSE NULL END, \
    \  CASE WHEN COALESCE(o.receipt_economics, resolved.receipt_economics)->>'postSettlementBalanceAssets' ~ '^-{0,1}[0-9]+$' \
    \    THEN (COALESCE(o.receipt_economics, resolved.receipt_economics)->>'postSettlementBalanceAssets')::NUMERIC ELSE NULL END, \
    \  CASE WHEN COALESCE(o.receipt_economics, resolved.receipt_economics)->>'preTraderClaimBalanceAssets' ~ '^-{0,1}[0-9]+$' \
    \    THEN (COALESCE(o.receipt_economics, resolved.receipt_economics)->>'preTraderClaimBalanceAssets')::NUMERIC ELSE NULL END, \
    \  CASE WHEN COALESCE(o.receipt_economics, resolved.receipt_economics)->>'postTraderClaimBalanceAssets' ~ '^-{0,1}[0-9]+$' \
    \    THEN (COALESCE(o.receipt_economics, resolved.receipt_economics)->>'postTraderClaimBalanceAssets')::NUMERIC ELSE NULL END, \
    \  a.block_number, \
    \  a.log_index, \
    \  a.timestamp \
    \FROM perps_account_activity a \
    \LEFT JOIN perps_orders o \
    \  ON o.chain_id = a.chain_id AND o.order_router = a.release_router \
    \  AND o.order_id = a.order_id AND o.terminal_status = 'Executed' \
    \LEFT JOIN LATERAL (\
    \  SELECT CASE WHEN COUNT(*) = 1 THEN (jsonb_agg(candidate.receipt_economics))->0 ELSE NULL END AS receipt_economics \
    \  FROM (\
    \    SELECT o2.receipt_economics \
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
    \          AND sibling.tx_hash = a.tx_hash AND sibling.block_hash = a.block_hash AND sibling.block_number = a.block_number AND sibling.account = a.account \
    \          AND sibling.activity_type IN ('Open', 'Close') AND sibling.log_index < terminal.log_index \
    \          AND sibling.log_index > COALESCE((\
    \            SELECT MAX(prior.log_index) FROM perps_events prior \
    \            WHERE prior.chain_id = a.chain_id AND prior.release_router = a.release_router \
    \              AND prior.tx_hash = a.tx_hash AND prior.block_hash = a.block_hash \
    \              AND prior.event_name = 'OrderFinalized' AND prior.log_index < terminal.log_index\
    \          ), -1)\
    \      )\
    \  ) candidate\
    \) resolved ON a.order_id IS NULL AND a.activity_type IN ('Open', 'Close') \
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

fetchAccountVaultBalancesForVaults
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Text
  -> Text
  -> IO [AccountVaultBalanceRow]
fetchAccountVaultBalancesForVaults conn chainId housePool deployBlock seniorVault juniorVault account =
  query
    conn
    "SELECT vault_address, share_balance::TEXT, unclaimed_deposit_shares::TEXT, \
    \withdrawal_escrow_shares::TEXT, total_attributed_shares::TEXT, observed_block::TEXT \
    \FROM vault_attributed_holder_balances \
    \WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? \
    \  AND vault_address IN (?, ?) AND holder_address = ? \
    \ORDER BY vault_address ASC"
    (chainId, housePool, deployBlock, seniorVault, juniorVault, account)

fetchAccountVaultRequestStatesForVaults
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Text
  -> Text
  -> IO [AccountVaultRequestStateRow]
fetchAccountVaultRequestStatesForVaults conn chainId housePool deployBlock seniorVault juniorVault account =
  query
    conn
    "SELECT vault_address, request_id::TEXT, pending_deposit_assets::TEXT, \
    \claimable_deposit_assets::TEXT, claimable_deposit_shares::TEXT, refundable_deposit_assets::TEXT, \
    \pending_redeem_shares::TEXT, refundable_redeem_shares::TEXT, redeem_refund_pending, \
    \is_active, observed_block::TEXT \
    \FROM vault_deposit_request_states \
    \WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? \
    \  AND vault_address IN (?, ?) AND controller_address = ? \
    \ORDER BY is_active DESC, request_id DESC LIMIT 30"
    (chainId, housePool, deployBlock, seniorVault, juniorVault, account)

fetchAccountVaultRequestEventsForVaults
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Text
  -> Text
  -> IO [AccountVaultRequestEventRow]
fetchAccountVaultRequestEventsForVaults conn chainId housePool deployBlock seniorVault juniorVault account =
  query
    conn
    "SELECT vault_address, event_name, request_id::TEXT, raw_amount::TEXT, \
    \tx_hash, block_number::TEXT, block_timestamp \
    \FROM vault_request_events \
    \WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? \
    \  AND vault_address IN (?, ?) \
    \  AND (controller_address = ? OR owner_address = ?) \
    \ORDER BY block_number DESC, log_index DESC LIMIT 30"
    (chainId, housePool, deployBlock, seniorVault, juniorVault, account, account)

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

accountActivityToJson :: OrderProtectionsBundle -> AccountActivityRow -> Value
accountActivityToJson protBundle AccountActivityRow {..} =
  let effectiveOrderId = aarOrderId <|> extractExecutionOrderId aarExecution
      mOrderProtections =
        if aarActivityType == "Open"
          then effectiveOrderId >>= buildOrderProtectionsForOrder protBundle
          else Nothing
   in object $
        catMaybes
          [ Just $ "id" .= (aarTxHash <> ":" <> T.pack (show aarLogIndex))
          , Just $ "activityType" .= aarActivityType
          , ("orderId" .=) . show <$> effectiveOrderId
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
          , ("orderProtections" .=) <$> mOrderProtections
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

data DivisionsEquityBridge = DivisionsEquityBridge
  { debStartingEquityAssets :: Integer
  , debDepositsAssets :: Integer
  , debWithdrawalsAssets :: Integer
  , debAdjustedEquityAssets :: Integer
  , debCutoffEquityAssets :: Integer
  , debWeeklyPnlAssets :: Integer
  , debPendingCarryAssets :: Integer
  }
  deriving stock (Eq, Show)

buildDivisionsEquityBridge
  :: Maybe Integer
  -> Integer
  -> Integer
  -> Maybe (AccountLedgerSnapshot, Integer)
  -> Maybe DivisionsEquityBridge
buildDivisionsEquityBridge mStartEquity deposits withdrawals mCutoff = do
  startEq <- mStartEquity
  (cutoffSnap, carry) <- mCutoff
  let cutoffEq = economicAccountValue (ledgerToEquity cutoffSnap carry)
      adjusted = startEq + deposits - withdrawals
  pure
    DivisionsEquityBridge
      { debStartingEquityAssets = startEq
      , debDepositsAssets = deposits
      , debWithdrawalsAssets = withdrawals
      , debAdjustedEquityAssets = adjusted
      , debCutoffEquityAssets = cutoffEq
      , debWeeklyPnlAssets = cutoffEq - adjusted
      , debPendingCarryAssets = carry
      }

data DivisionsWeekBoundary = DivisionsWeekBoundary
  { dwbWeekStart :: Integer
  , dwbBaselineBlock :: Integer
  , dwbCutoffBlock :: Integer
  , dwbCutoffTimestamp :: Integer
  , dwbReleaseRouter :: Text
  , dwbReleaseVersion :: Text
  , dwbAssetSymbol :: Text
  }
  deriving stock (Eq, Show)

data DivisionsWeekActivityRow = DivisionsWeekActivityRow
  { dwaTradesCount :: Int64
  , dwaOpensCount :: Int64
  , dwaClosesCount :: Int64
  , dwaLiquidationsCount :: Int64
  , dwaVolumeAssets :: Text
  , dwaRealizedPnlAssets :: Text
  , dwaDepositsAssets :: Integer
  , dwaWithdrawalsAssets :: Integer
  , dwaMon :: Bool
  , dwaTue :: Bool
  , dwaWed :: Bool
  , dwaThu :: Bool
  , dwaFri :: Bool
  }
  deriving stock (Eq, Show)

instance FromRow DivisionsWeekActivityRow where
  fromRow =
    DivisionsWeekActivityRow
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

{-# NOINLINE divisionsWalletContextCache #-}
divisionsWalletContextCache :: SingleFlightCache Text (Either ApiError (ApiResponse Value))
divisionsWalletContextCache =
  unsafePerformIO (newConcurrentRefreshingSingleFlightCache 256 (30 * 1000000000) (600 * 1000000000))

-- | GET /api/insights/v1/divisions/wallets/:address
-- Returns verifiable boundary snapshots (wallet balance + exact open position at
-- baseline and cutoff blocks), equity accounting bridge, Mon-Fri FX session
-- flags, week-scoped trading activity, and multi-week Divisions history.
getDivisionsWalletContextResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> IO (Either ApiError (ApiResponse Value))
getDivisionsWalletContextResponse pool client cfg rawAddress =
  let account = T.toLower $ T.strip rawAddress
   in snd
        <$> runSingleFlightCache
          divisionsWalletContextCache
          account
          isRight
          (computeDivisionsWalletContextResponse pool client cfg account)

computeDivisionsWalletContextResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> IO (Either ApiError (ApiResponse Value))
computeDivisionsWalletContextResponse pool client cfg account = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      activeRouter = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      currentWeekStart = divisionWeekStart now
  (mHandle, mLiveBoundary, mPublishedBoundary, history) <-
    withDb pool $ \conn ->
      withReadSnapshot conn $ do
        handle <- fetchDiscordHandle conn account
        liveRow <- fetchLiveWeekBoundary conn chainId currentWeekStart
        pubRow <- fetchLatestPublishedWeekBoundary conn chainId
        hist <- fetchAllDivisionsHistoryForWallet conn chainId account
        pure (handle, liveRow, pubRow, hist)
  (mLiveSection, mPublishedSection) <-
    concurrently
      (traverse (hydrateDivisionsWeekSection pool client cfg chainId activeRouter account) mLiveBoundary)
      (traverse (hydrateDivisionsWeekSection pool client cfg chainId activeRouter account) mPublishedBoundary)
  let latestBlock =
        max
          (maybe 0 dwbCutoffBlock mLiveBoundary)
          (maybe 0 dwbCutoffBlock mPublishedBoundary)
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
                [ "discordUsername" .= mHandle
                , "isDivisionsLinked" .= maybe False (const True) mHandle
                ]
          , "liveWeek" .= mLiveSection
          , "publishedWeek" .= mPublishedSection
          , "history" .= map divisionsWeekToJson history
          ]

fetchLiveWeekBoundary :: Connection -> Integer -> Integer -> IO (Maybe DivisionsWeekBoundary)
fetchLiveWeekBoundary conn chainId weekStart = do
  rows <-
    query
      conn
      "SELECT s.week_start, s.baseline_block, s.snapshot_block, s.snapshot_timestamp, \
      \w.release_router, w.release_version, w.asset_symbol \
      \FROM divisions_live_snapshots s \
      \JOIN divisions_weeks w \
      \  ON w.chain_id = s.chain_id AND w.week_start = s.week_start AND w.release_router = s.release_router \
      \WHERE s.chain_id = ? AND s.week_start = ? \
      \ORDER BY s.snapshot_block DESC \
      \LIMIT 1"
      (chainId, weekStart) :: IO [(Integer, Integer, Integer, Integer, Text, Text, Text)]
  pure $ case rows of
    (ws, baseBlk, snapBlk, snapTs, rtr, ver, sym) : _ ->
      Just (DivisionsWeekBoundary ws baseBlk snapBlk snapTs (T.toLower rtr) ver sym)
    [] -> Nothing

fetchLatestPublishedWeekBoundary :: Connection -> Integer -> IO (Maybe DivisionsWeekBoundary)
fetchLatestPublishedWeekBoundary conn chainId = do
  rows <-
    query
      conn
      "SELECT p.week_start, p.closing_block, p.publication::TEXT, \
      \w.release_router, w.release_version, w.asset_symbol \
      \FROM divisions_publications p \
      \JOIN divisions_weeks w \
      \  ON w.chain_id = p.chain_id AND w.week_start = p.week_start AND w.release_router = p.release_router \
      \WHERE p.chain_id = ? \
      \ORDER BY p.week_start DESC \
      \LIMIT 1"
      (Only chainId) :: IO [(Integer, Integer, Text, Text, Text, Text)]
  pure $ case rows of
    (ws, closeBlk, pubText, rtr, ver, sym) : _ ->
      let mBaseBlk = do
            val <- decodeStrict' (TE.encodeUtf8 pubText)
            obj <- asObject val
            src <- lookupObject "source" obj
            baseObj <- lookupObject "baselineBlock" src
            lookupInteger "number" baseObj
       in Just $
            DivisionsWeekBoundary
              ws
              (fromMaybe 0 mBaseBlk)
              closeBlk
              (ws + 5 * 86400)
              (T.toLower rtr)
              ver
              sym
    [] -> Nothing

fetchAllDivisionsHistoryForWallet :: Connection -> Integer -> Text -> IO [DivisionsWeekRecord]
fetchAllDivisionsHistoryForWallet conn chainId account = do
  rows <-
    query
      conn
      "SELECT p.week_start, p.closing_block, p.publication \
      \FROM divisions_publications p \
      \JOIN divisions_weeks w \
      \  ON w.chain_id = p.chain_id AND w.week_start = p.week_start AND w.release_router = p.release_router \
      \WHERE p.chain_id = ? \
      \ORDER BY p.week_start DESC LIMIT 20"
      (Only chainId) :: IO [(Integer, Integer, Value)]
  pure $ extractDivisionsHistory account rows

fetchWeekActivityStats
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> IO DivisionsWeekActivityRow
fetchWeekActivityStats conn chainId router account baselineBlock cutoffBlock = do
  rows <-
    query
      conn
      "SELECT \
      \  COUNT(*) FILTER (WHERE a.activity_type IN ('Open', 'Close', 'Liquidated')), \
      \  COUNT(*) FILTER (WHERE a.activity_type = 'Open'), \
      \  COUNT(*) FILTER (WHERE a.activity_type = 'Close'), \
      \  COUNT(*) FILTER (WHERE a.activity_type = 'Liquidated'), \
      \  COALESCE(SUM(CASE WHEN a.activity_type IN ('Open', 'Close', 'Liquidated') THEN ABS(COALESCE(a.amount_assets, 0)) ELSE 0 END), 0)::TEXT, \
      \  COALESCE(SUM(COALESCE(a.pnl_assets, 0)), 0)::TEXT, \
      \  COALESCE(SUM(CASE WHEN a.activity_type = 'Deposit' AND NOT EXISTS (\
      \    SELECT 1 FROM aa_close_assistance g WHERE g.verified \
      \      AND g.chain_id = a.chain_id AND g.router = a.release_router \
      \      AND g.account = a.account AND g.transaction_hash = a.tx_hash \
      \      AND g.block_number = a.block_number AND g.block_hash = a.block_hash \
      \      AND g.deposit_log_index = a.log_index AND g.amount_assets = a.amount_assets\
      \  ) THEN ABS(COALESCE(a.amount_assets, 0)) ELSE 0 END), 0)::BIGINT, \
      \  COALESCE(SUM(CASE WHEN a.activity_type = 'Withdraw' THEN ABS(COALESCE(a.amount_assets, 0)) ELSE 0 END), 0)::BIGINT, \
      \  COALESCE(BOOL_OR(a.activity_type IN ('Open', 'Close') AND COALESCE(a.size_delta, 0) <> 0 AND EXTRACT(DOW FROM to_timestamp(a.timestamp + 10800) AT TIME ZONE 'UTC') = 1), FALSE), \
      \  COALESCE(BOOL_OR(a.activity_type IN ('Open', 'Close') AND COALESCE(a.size_delta, 0) <> 0 AND EXTRACT(DOW FROM to_timestamp(a.timestamp + 10800) AT TIME ZONE 'UTC') = 2), FALSE), \
      \  COALESCE(BOOL_OR(a.activity_type IN ('Open', 'Close') AND COALESCE(a.size_delta, 0) <> 0 AND EXTRACT(DOW FROM to_timestamp(a.timestamp + 10800) AT TIME ZONE 'UTC') = 3), FALSE), \
      \  COALESCE(BOOL_OR(a.activity_type IN ('Open', 'Close') AND COALESCE(a.size_delta, 0) <> 0 AND EXTRACT(DOW FROM to_timestamp(a.timestamp + 10800) AT TIME ZONE 'UTC') = 4), FALSE), \
      \  COALESCE(BOOL_OR(a.activity_type IN ('Open', 'Close') AND COALESCE(a.size_delta, 0) <> 0 AND EXTRACT(DOW FROM to_timestamp(a.timestamp + 10800) AT TIME ZONE 'UTC') = 5), FALSE) \
      \FROM perps_account_activity a \
      \WHERE a.chain_id = ? AND a.release_router = ? AND a.account = ? \
      \  AND a.block_number > ? AND a.block_number <= ?"
      (chainId, router, account, baselineBlock, cutoffBlock) :: IO [DivisionsWeekActivityRow]
  pure $
    fromMaybe
      (DivisionsWeekActivityRow 0 0 0 0 "0" "0" 0 0 False False False False False)
      (listToMaybe rows)

captureHistoricalSnapshotAtBlock
  :: EthClient
  -> Config
  -> Text
  -> Integer
  -> IO (Maybe (AccountLedgerSnapshot, Integer))
captureHistoricalSnapshotAtBlock client cfg account blockNum
  | blockNum <= 0 || T.null (cfgPerpsAccountLens cfg) = pure Nothing
  | otherwise = do
      mLedger <- timeout (2000 * 1000) (getAccountLedgerSnapshotAtBlock client (cfgPerpsAccountLens cfg) account blockNum)
      case mLedger of
        Just (Right snap) -> do
          carry <- case inferPendingCarry snap of
            Just c -> pure c
            Nothing
              | T.null (cfgPerpsCfdEngineLens cfg) -> pure 0
              | otherwise -> do
                  mCarry <- timeout (1500 * 1000) (getPendingCarryAtBlock client (cfgPerpsCfdEngineLens cfg) account blockNum)
                  pure $ case mCarry of
                    Just (Right c) -> c
                    _ -> 0
          pure $ Just (snap, carry)
        _ -> pure Nothing

hydrateDivisionsWeekSection
  :: DbPool
  -> EthClient
  -> Config
  -> Integer
  -> Text
  -> Text
  -> DivisionsWeekBoundary
  -> IO Value
hydrateDivisionsWeekSection pool client cfg chainId activeRouter account boundary = do
  let sameRelease = dwbReleaseRouter boundary == activeRouter
      fetchActivity =
        withDb pool $ \conn ->
          fetchWeekActivityStats
            conn
            chainId
            (dwbReleaseRouter boundary)
            account
            (dwbBaselineBlock boundary)
            (dwbCutoffBlock boundary)
      fetchStartEquity
        | not sameRelease = pure Nothing
        | dwbBaselineBlock boundary <= 0 = pure Nothing
        | dwbBaselineBlock boundary < cfgPerpsIndexerStartBlock cfg = pure (Just 0)
        | otherwise =
            fmap (\(s, c) -> economicAccountValue (ledgerToEquity s c))
              <$> captureHistoricalSnapshotAtBlock client cfg account (dwbBaselineBlock boundary)
      fetchCutoffSnap
        | not sameRelease = pure Nothing
        | otherwise =
            captureHistoricalSnapshotAtBlock client cfg account (dwbCutoffBlock boundary)
  (act, mStartEq, mCutoff) <-
    runConcurrently $
      (,,)
        <$> Concurrently fetchActivity
        <*> Concurrently fetchStartEquity
        <*> Concurrently fetchCutoffSnap
  let mBridge =
        buildDivisionsEquityBridge
          mStartEq
          (dwaDepositsAssets act)
          (dwaWithdrawalsAssets act)
          mCutoff
      cutoffSnapJson =
        case mCutoff of
          Nothing -> Null
          Just (snap, carry) ->
            let econ = economicAccountValue (ledgerToEquity snap carry)
             in case ledgerSnapshotToJson snap of
                  Object base ->
                    Object $
                      KM.insert (Key.fromText "pendingCarryAssets") (String (T.pack (show carry))) $
                        KM.insert (Key.fromText "economicEquityAssets") (String (T.pack (show econ))) base
                  other -> other
  pure $
    object
      [ "weekStart" .= dwbWeekStart boundary
      , "weekStartsAt" .= isoTimestamp (dwbWeekStart boundary)
      , "weekClosesAt" .= isoTimestamp (dwbWeekStart boundary + 5 * 86400)
      , "baselineBlock" .= show (dwbBaselineBlock boundary)
      , "cutoffBlock" .= show (dwbCutoffBlock boundary)
      , "cutoffAt" .= isoTimestamp (dwbCutoffTimestamp boundary)
      , "releaseVersion" .= dwbReleaseVersion boundary
      , "assetSymbol" .= dwbAssetSymbol boundary
      , "activeWeekdays" .= [dwaMon act, dwaTue act, dwaWed act, dwaThu act, dwaFri act]
      , "activity"
          .= object
            [ "tradesCount" .= dwaTradesCount act
            , "opensCount" .= dwaOpensCount act
            , "closesCount" .= dwaClosesCount act
            , "liquidationsCount" .= dwaLiquidationsCount act
            , "volumeAssets" .= dwaVolumeAssets act
            , "realizedPnlAssets" .= dwaRealizedPnlAssets act
            , "depositsAssets" .= show (dwaDepositsAssets act)
            , "withdrawalsAssets" .= show (dwaWithdrawalsAssets act)
            ]
      , "equityBridge" .= (equityBridgeToJson <$> mBridge)
      , "cutoffSnapshot" .= cutoffSnapJson
      ]

equityBridgeToJson :: DivisionsEquityBridge -> Value
equityBridgeToJson DivisionsEquityBridge {..} =
  object
    [ "startingEquityAssets" .= show debStartingEquityAssets
    , "depositsAssets" .= show debDepositsAssets
    , "withdrawalsAssets" .= show debWithdrawalsAssets
    , "adjustedEquityAssets" .= show debAdjustedEquityAssets
    , "cutoffEquityAssets" .= show debCutoffEquityAssets
    , "weeklyPnlAssets" .= show debWeeklyPnlAssets
    , "pendingCarryAssets" .= show debPendingCarryAssets
    ]
