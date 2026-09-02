module Plether.Handlers.ProtocolInsights
  ( ProtocolTransactionFilters (..)
  , ProtocolCursor (..)
  , TrancheHistoryCursor (..)
  , decodeProtocolCursor
  , decodeTrancheHistoryCursor
  , encodeProtocolCursor
  , encodeTrancheHistoryCursor
  , getCurrentProtocolReleaseResponse
  , getProtocolOverviewResponse
  , getProtocolTransactionsResponse
  , getProtocolTransactionResponse
  , getProtocolOrderResponse
  , getHousePoolResponse
  , getTrancheResponse
  , getTrancheHistoryResponse
  , getKeepersResponse
  , getKeeperResponse
  , getOperationalWalletsResponse
  , getOperationalWalletResponse
  , getParametersResponse
  , getParameterChangesResponse
  , orderRevealTiming
  , orderAtConfirmedBlock
  , orderCommitIntentFromActions
  , orderFinalizationFromActions
  , orderEconomics
  , orderEconomicsAvailability
  , mergePendingChanges
  , StateRead (..)
  , StateImpactPair (..)
  , AccountStateImpact (..)
  , TransactionStateImpact (..)
  , OrderStateDelta (..)
  , orderTerminalMarketState
  , orderStateDeltaEvidenceLabel
  , stateDeltaJson
  , stateDeltaAvailability
  , housePoolFinancialEvidence
  , transactionStateImpactJson
  , transactionStateImpactEvidenceLabel
  , transactionActionAnalysisJson
  , transactionEvidenceLabel
  , transactionAvailability
  , protocolProjectionIndexerName
  , selectProjectionListAnchor
  , estimateOperationalTransactionsAtObservedGrossSpend
  , operationalWalletStatus
  , operationalMetricJsonForRoles
  , operationalRolePaginationPolicy
  , qualifiesObservedOperationalWallet
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isHexDigit)
import Data.Function (on)
import Data.Foldable (toList)
import Data.List (find, nubBy, sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection)
import Text.Read (readMaybe)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Protocol
  ( KeeperAggregateRow (..)
  , KeeperNativeCostRow (..)
  , KeeperNativeCostSummaryRow (..)
  , KeeperWindowSummaryRow (..)
  , OperationalWalletActivityRow (..)
  , OperationalWalletCostRow (..)
  , ProtocolActionRow (..)
  , ProtocolEventRow (..)
  , ProtocolStateSnapshotRow (..)
  , ProtocolTransactionRow (..)
  , getKeeperActions
  , getKeeperAggregates
  , getKeeperAggregatesPage
  , getKeeperLatencyPercentiles
  , getKeeperNativeCosts
  , getKeeperNativeCostsForActors
  , getKeeperNativeCostSummary
  , getKeeperRewardLeaders
  , getKeeperWindowSummary
  , getOperationalWalletActivity
  , getOperationalWalletActions
  , getOperationalWalletCostsForActors
  , getParameterChanges
  , getProtocolActionsByOrder
  , getProtocolActionsByTransaction
  , getProtocolEventsByTransaction
  , getProtocolIndexedHead
  , getProtocolOverviewCounts
  , getProtocolPendingOrderTimes
  , getProtocolProjectionHead
  , getProtocolStateSnapshotsAtBlocks
  , getProtocolStateSnapshotsPage
  , getProtocolTransaction
  , getProtocolTransactionsByHashes
  , getTrancheActions
  , listProtocolActions
  )
import Plether.Database.Schema (PerpsOrderRow (..), getPerpsOrderById)
import Plether.Ethereum.Abi
  ( decodeUint256
  , encodeAddress
  , encodeCall
  )
import Plether.Ethereum.Client
  ( CallParams (..)
  , CanonicalBlockRef
  , EthClient
  , canonicalBlockHash
  , canonicalBlockNumber
  , ethBlockNumber
  , ethCallAtCanonicalBlock
  , ethGetCanonicalBlockRef
  )
import Plether.Ethereum.Rpc
  ( RpcBlock (..)
  , ethGetBalanceAtCanonicalBlock
  , ethGetBlockByNumber
  )
import Plether.Perps.HistoryIndexer (perpsIndexerNameForRelease)
import Plether.Protocol.Parameters
  ( ParameterDefinition (..)
  , parameterCatalog
  , parameterDefinitionToJson
  )
import Plether.Protocol.Governance
  ( DecodedGovernanceField (..)
  , GovernanceCategory (..)
  , GovernanceCategoryDefinition (..)
  , GovernanceContractRole (..)
  , GovernanceDecodedValue (..)
  , GovernanceDecodeError (..)
  , GovernanceField (..)
  , GovernanceFieldType (..)
  , GovernanceFunction (..)
  , GovernanceGetterDefinition (..)
  , GovernanceGetterPurpose (..)
  , GovernancePendingState (..)
  , GovernancePayloadEncoding (..)
  , decodeGovernanceGetter
  , decodePendingGovernance
  , governanceCategoryDefinitions
  , governanceContractAddress
  , governanceContractRoleKey
  , governanceGetterMutability
  , governanceGetterTimelockPolicy
  , governancePendingState
  , qualifyGovernanceKey
  , governanceRoleGetters
  )
import Plether.Protocol.Release
  ( ProtocolOperationalWallet (..)
  , ProtocolOperationalWalletEvidence (..)
  , ProtocolRelease (..)
  , currentProtocolRelease
  , protocolReleaseById
  , protocolReleaseToJson
  )
import Plether.Types (ApiError, ApiResponse (..), mkResponse)
import qualified Plether.Types.Error as E
import Plether.Utils.Address (isValidAddress)

data ProtocolTransactionFilters = ProtocolTransactionFilters
  { ptfActionType :: Maybe Text
  , ptfOutcome :: Maybe Text
  , ptfAddress :: Maybe Text
  , ptfAccount :: Maybe Text
  , ptfKeeper :: Maybe Text
  , ptfContract :: Maybe Text
  , ptfTransactionHash :: Maybe Text
  , ptfFromTimestamp :: Maybe Integer
  , ptfToTimestamp :: Maybe Integer
  }
  deriving stock (Show, Eq)

-- | Opaque, endpoint-scoped keyset cursor. The confirmed block hash anchors
-- every subsequent page to one canonical view while the item fields carry the
-- deterministic list position.
data ProtocolCursor = ProtocolCursor
  { pcReleaseId :: Text
  , pcScope :: Text
  , pcConfirmedBlock :: Integer
  , pcConfirmedBlockHash :: Text
  , pcItemBlock :: Integer
  , pcItemLogIndex :: Maybe Integer
  , pcItemId :: Maybe Text
  }
  deriving stock (Show, Eq)

-- | Compound tranche-history continuation. Actions and state checkpoints are
-- independent ordered streams, but both positions share one release/scope and
-- canonical confirmed-block anchor. A completed stream has no item position,
-- allowing the other stream to continue without restarting or repeating it.
data TrancheHistoryCursor = TrancheHistoryCursor
  { thcReleaseId :: Text
  , thcScope :: Text
  , thcConfirmedBlock :: Integer
  , thcConfirmedBlockHash :: Text
  , thcActionBlock :: Maybe Integer
  , thcActionLogIndex :: Maybe Integer
  , thcActionId :: Maybe Text
  , thcActionsComplete :: Bool
  , thcCheckpointBlock :: Maybe Integer
  , thcCheckpointsComplete :: Bool
  , thcCheckpointContinuationUnavailable :: Bool
  }
  deriving stock (Show, Eq)

instance ToJSON ProtocolCursor where
  toJSON ProtocolCursor {..} =
    object
      [ "version" .= (2 :: Int)
      , "releaseId" .= pcReleaseId
      , "scope" .= pcScope
      , "confirmedBlock" .= show pcConfirmedBlock
      , "confirmedBlockHash" .= T.toLower pcConfirmedBlockHash
      , "itemBlock" .= show pcItemBlock
      , "itemLogIndex" .= fmap show pcItemLogIndex
      , "itemId" .= pcItemId
      ]

instance FromJSON ProtocolCursor where
  parseJSON =
    withObject "ProtocolCursor" $ \fields -> do
      version <- fields .: "version"
      if version /= (2 :: Int)
        then fail "unsupported protocol cursor version"
        else do
          rawConfirmedBlock <- fields .: "confirmedBlock"
          rawItemBlock <- fields .: "itemBlock"
          rawItemLogIndex <- fields .:? "itemLogIndex"
          ProtocolCursor
            <$> fields .: "releaseId"
            <*> fields .: "scope"
            <*> parseDecimalCursorField rawConfirmedBlock
            <*> fields .: "confirmedBlockHash"
            <*> parseDecimalCursorField rawItemBlock
            <*> traverse parseDecimalCursorField rawItemLogIndex
            <*> fields .:? "itemId"

instance ToJSON TrancheHistoryCursor where
  toJSON TrancheHistoryCursor {..} =
    object
      [ "version" .= (1 :: Int)
      , "releaseId" .= thcReleaseId
      , "scope" .= thcScope
      , "confirmedBlock" .= show thcConfirmedBlock
      , "confirmedBlockHash" .= T.toLower thcConfirmedBlockHash
      , "actionBlock" .= fmap show thcActionBlock
      , "actionLogIndex" .= fmap show thcActionLogIndex
      , "actionId" .= thcActionId
      , "actionsComplete" .= thcActionsComplete
      , "checkpointBlock" .= fmap show thcCheckpointBlock
      , "checkpointsComplete" .= thcCheckpointsComplete
      , "checkpointContinuationUnavailable" .=
          thcCheckpointContinuationUnavailable
      ]

instance FromJSON TrancheHistoryCursor where
  parseJSON =
    withObject "TrancheHistoryCursor" $ \fields -> do
      version <- fields .: "version"
      if version /= (1 :: Int)
        then fail "unsupported tranche history cursor version"
        else do
          rawConfirmedBlock <- fields .: "confirmedBlock"
          rawActionBlock <- fields .:? "actionBlock"
          rawActionLogIndex <- fields .:? "actionLogIndex"
          rawCheckpointBlock <- fields .:? "checkpointBlock"
          TrancheHistoryCursor
            <$> fields .: "releaseId"
            <*> fields .: "scope"
            <*> parseDecimalCursorField rawConfirmedBlock
            <*> fields .: "confirmedBlockHash"
            <*> traverse parseDecimalCursorField rawActionBlock
            <*> traverse parseDecimalCursorField rawActionLogIndex
            <*> fields .:? "actionId"
            <*> fields .: "actionsComplete"
            <*> traverse parseDecimalCursorField rawCheckpointBlock
            <*> fields .: "checkpointsComplete"
            <*> fields .: "checkpointContinuationUnavailable"

data ConfirmedContext = ConfirmedContext
  { ccBlockNumber :: Integer
  , ccBlockHash :: Text
  , ccBlockTimestamp :: Integer
  , ccBlockRef :: Maybe CanonicalBlockRef
  , ccAvailability :: [Value]
  }

type ProtocolIndexedHead = (Integer, Text, Integer)

data ProjectionListContext = ProjectionListContext
  { plcConfirmedContext :: ConfirmedContext
  , plcChainConfirmedBlock :: Integer
  , plcIndexedHead :: ProtocolIndexedHead
  , plcCursorAnchorReused :: Bool
  , plcAvailability :: [Value]
  }

data StateRead a = StateRead
  { srValue :: Maybe a
  , srUnavailableReason :: Maybe Text
  }

data OperationalWalletRoleSource = OperationalWalletRoleSource
  { owrsAddress :: Text
  , owrsRole :: Text
  , owrsSource :: Text
  , owrsSourceContract :: Maybe Text
  , owrsSourceAddress :: Maybe Text
  , owrsDescription :: Maybe Text
  , owrsRepresentativeEvidence :: Maybe ProtocolOperationalWalletEvidence
  , owrsEvidence :: Text
  }
  deriving stock (Show, Eq)

getCurrentProtocolReleaseResponse
  :: Maybe DbPool
  -> EthClient
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getCurrentProtocolReleaseResponse maybePool client cfg = do
  let release = currentProtocolRelease cfg
  (context, indexedHead) <-
    if cfgProtocolExplorerEnabled cfg
      then do
        confirmed <- confirmedContext client cfg
        indexed <-
          case maybePool of
            Just pool -> withDb pool $ \conn -> getProtocolIndexedHead conn (prId release)
            Nothing -> pure Nothing
        pure (confirmed, indexed)
      else
        pure
          ( ConfirmedContext
              { ccBlockNumber = 0
              , ccBlockHash = ""
              , ccBlockTimestamp = 0
              , ccBlockRef = Nothing
              , ccAvailability =
                  [unavailable "confirmedBlock" "protocol_explorer_disabled"]
              }
          , Nothing
          )
  let response =
        protocolResponse
          release
          context
          indexedHead
          (object ["releaseManifest" .= ("checked_in_configuration" :: Text)])
          (ccAvailability context)
          "release"
          (protocolReleaseToJson release)
  pure $
    Right $
      response
        { respData =
            case respData response of
              Object fields ->
                Object $
                  KM.insert
                    "explorerEnabled"
                    (Bool $ cfgProtocolExplorerEnabled cfg)
                    fields
              value -> value
        }

getProtocolOverviewResponse
  :: DbPool -> EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse Value))
getProtocolOverviewResponse pool client cfg releaseId =
  withRelease cfg releaseId $ \release -> do
    projectionContext <-
      resolveProjectionListContext
        pool
        client
        cfg
        release
        "overview"
        Nothing
    case projectionContext of
      Left err -> pure $ Left err
      Right listContext ->
        getProtocolOverviewAt pool client release listContext

getProtocolOverviewAt
  :: DbPool
  -> EthClient
  -> ProtocolRelease
  -> ProjectionListContext
  -> IO (Either ApiError (ApiResponse Value))
getProtocolOverviewAt pool client release listContext = do
    let context = plcConfirmedContext listContext
        indexedHead = plcIndexedHead listContext
        (indexedBlock, _, _) = indexedHead
    let now = ccBlockTimestamp context
        windowStart = max 0 $ now - 86_400
    (counts, pendingOrderTimes, keepers, parameterChanges, walletActivity, walletCosts) <- withDb pool $ \conn -> do
      counts <- getProtocolOverviewCounts conn releaseId windowStart (ccBlockNumber context)
      pending <- getProtocolPendingOrderTimes conn (prChainId release) (prOrderRouter release) (ccBlockNumber context)
      keeperRows <- getKeeperAggregates conn releaseId windowStart (ccBlockNumber context)
      changes <- getParameterChanges conn releaseId (ccBlockNumber context) 100 Nothing
      operationalActivity <-
        getOperationalWalletActivity conn releaseId windowStart (ccBlockNumber context)
      operationalCosts <-
        getOperationalWalletCostsForActors
          conn
          releaseId
          windowStart
          (ccBlockNumber context)
          (map (T.toLower . powAddress) $ prOperationalWallets release)
      pure
        ( counts
        , pending
        , keeperRows
        , changes
        , operationalActivity
        , operationalCosts
        )
    poolRead <- poolLiquidityAt client release (ccBlockRef context)
    statusRead <- protocolStatusAt client release (ccBlockRef context)
    seniorSupplyRead <-
      mapStateRead (`word` 0)
        <$> callAtExactWords client (prSeniorVault release) "totalSupply()" [] (ccBlockRef context) 1
    juniorSupplyRead <-
      mapStateRead (`word` 0)
        <$> callAtExactWords client (prJuniorVault release) "totalSupply()" [] (ccBlockRef context) 1
    traderClaimsRead <-
      readUintStateAtExact
        client
        (prCfdEngine release)
        "totalTraderClaimBalanceUsdc()"
        0
        1
        (ccBlockRef context)
    rawPoolAssetsRead <-
      mapStateRead (`word` 0)
        <$> callAtExactWords
          client
          (prUsdc release)
          "balanceOf(address)"
          [encodeAddress $ prHousePool release]
          (ccBlockRef context)
          1
    badDebtRead <-
      readUintStateAtExact
        client
        (prCfdEngine release)
        "accumulatedBadDebtUsdc()"
        0
        1
        (ccBlockRef context)
    maxOrderAge <- readUintAtExact client (prOrderRouter release) "maxOrderAge()" 0 1 (ccBlockRef context)
    (timelockAnomalies, timelockAvailability) <-
      readGovernanceTimelockAnomalies client release context
    registeredWalletBalances <-
      mapConcurrently
        (\wallet -> do
          balance <-
            readOperationalNativeBalance
              client
              context
              (T.toLower $ powAddress wallet)
          pure (wallet, balance)
        )
        (prOperationalWallets release)
    let poolResult = srValue poolRead
        statusResult = srValue statusRead
        (actionCount, liquidationCount, activeKeepers, failureCount) = counts
        oldOrders =
          fmap
            (\age -> length $ filter (\committedAt -> committedAt + age < now) pendingOrderTimes)
            maxOrderAge
        indexerLag =
          max 0 $
            plcChainConfirmedBlock listContext
              - indexedBlock
        observedLiquidationRewardTotal = sum $ map karGrossRewardsUsdc keepers
        topObservedLiquidationReward = fromMaybe 0 $ listToMaybe $ map karGrossRewardsUsdc keepers
        concentrationBps =
          if observedLiquidationRewardTotal <= 0
            then Nothing
            else Just $ topObservedLiquidationReward * 10_000 `div` observedLiquidationRewardTotal
        (walletLivenessAnomalies, walletLivenessAvailability) =
          operationalWalletOverviewSignals
            release
            context
            walletActivity
            walletCosts
            registeredWalletBalances
        anomalies =
          catMaybes
            [ if indexerLag > 20
                then Just $ anomaly "indexer_lag" "warning" "The immutable ledger is behind the confirmed chain head."
                  (object ["lagBlocks" .= show indexerLag])
                else Nothing
            , poolResult >>= \words' ->
                if word words' 10 /= 0
                  then Just $ anomaly "degraded_mode" "critical" "HousePool is in degraded mode." Null
                  else Nothing
            , poolResult >>= \words' ->
                if word words' 5 < word words' 7
                  then Just $ anomaly "senior_impairment" "critical" "Senior principal is below its high-water mark."
                    (object ["impairmentGapUsdc" .= show (word words' 7 - word words' 5), "unit" .= ("USDC:6" :: Text)])
                  else Nothing
            , do
                words' <- poolResult
                traderClaims <- srValue traderClaimsRead
                rawPoolAssets <- srValue rawPoolAssetsRead
                let physicalAssets = min rawPoolAssets $ word words' 0
                if physicalAssets < traderClaims
                  then Just $ anomaly "insufficient_claim_coverage" "critical" "Physical HousePool assets do not fully cover outstanding trader claims."
                    (object
                      [ "physicalAssetsUsdc" .= show physicalAssets
                      , "traderClaimsUsdc" .= show traderClaims
                      , "shortfallUsdc" .= show (traderClaims - physicalAssets)
                      , "unit" .= ("USDC:6" :: Text)
                      ])
                  else Nothing
            , do
                badDebt <- srValue badDebtRead
                if badDebt > 0
                  then Just $ anomaly "accumulated_bad_debt" "critical" "The protocol reports accumulated bad debt."
                    (object ["badDebtUsdc" .= show badDebt, "unit" .= ("USDC:6" :: Text)])
                  else Nothing
            , poolResult >>= \words' ->
                if word words' 9 /= 0
                  then Just $ anomaly "oracle_frozen" "warning" "The protocol reports a frozen oracle state." Null
                  else Nothing
            , poolResult >>= \words' ->
                if word words' 8 == 0
                  then Just $ anomaly "mark_stale" "warning" "The HousePool mark is not fresh at the confirmed block." Null
                  else Nothing
            , statusResult >>= \words' ->
                if word words' 4 /= 0
                  then Just $ anomaly "fad_window" "warning" "The protocol is in its scheduled freeze-and-delay window." Null
                  else Nothing
            , statusResult >>= \words' ->
                if word words' 5 == 0
                  then Just $ anomaly "trading_not_active" "warning" "New trading is not active; inspect the reported phase and pause state for close-only or paused operation." $
                    object ["phase" .= show (word words' 0)]
                  else Nothing
            , do
                words' <- poolResult
                supply <- srValue seniorSupplyRead
                if supply > 0 && word words' 5 == 0
                  then Just $ anomaly "senior_tranche_wipeout" "critical" "Senior has shares outstanding but zero protocol principal." Null
                  else Nothing
            , do
                words' <- poolResult
                supply <- srValue juniorSupplyRead
                if supply > 0 && word words' 6 == 0
                  then Just $ anomaly "junior_tranche_wipeout" "critical" "Junior has shares outstanding but zero protocol principal." Null
                  else Nothing
            , if fromMaybe 0 oldOrders > 0
                then Just $ anomaly "stale_order_backlog" "warning" "Orders older than maxOrderAge are awaiting terminal processing."
                  (object ["count" .= fromMaybe 0 oldOrders])
                else Nothing
            , if fromMaybe 0 oldOrders > 0 && activeKeepers == 0
                then Just $ anomaly "no_keeper_activity" "critical" "Eligible cleanup work exists but no successful keeper action was observed in 24 hours." Null
                else Nothing
            , if actionCount >= 10 && failureCount * 5 > actionCount
                then Just $ anomaly "failure_ratio" "warning" "More than 20% of indexed actions in the last 24 hours are non-success outcomes."
                  (object ["failures24h" .= show failureCount, "actions24h" .= show actionCount])
                else Nothing
            , concentrationBps >>= \share ->
                if share > 7500
                  then Just $ anomaly "keeper_concentration" "warning" "One keeper received more than 75% of observed liquidation bounties in 24 hours."
                    (object ["topKeeperObservedLiquidationRewardShareBps" .= show share, "unit" .= ("bps" :: Text)])
                  else Nothing
            ]
            <> timelockAnomalies
            <> walletLivenessAnomalies
            <> recentlyExecutedGovernanceAnomalies now parameterChanges
        stateAvailability =
          stateReadAvailability "housePool" poolRead
            <> stateReadAvailability "protocolStatus" statusRead
            <> stateReadAvailability "anomalies.seniorTrancheWipeout.totalSupply" seniorSupplyRead
            <> stateReadAvailability "anomalies.juniorTrancheWipeout.totalSupply" juniorSupplyRead
            <> stateReadAvailability "anomalies.insufficientClaimCoverage.traderClaims" traderClaimsRead
            <> stateReadAvailability "anomalies.insufficientClaimCoverage.rawPoolAssets" rawPoolAssetsRead
            <> stateReadAvailability "anomalies.accumulatedBadDebt" badDebtRead
            <> [unavailable "anomalies.staleOrderBacklog.maxOrderAge" "rpc_state_unavailable" | maxOrderAge == Nothing]
            <> timelockAvailability
            <> walletLivenessAvailability
            <> [ unavailable "anomalies.backlogGrowth" "historical_pending_order_count_snapshots_unavailable"
               , unavailable "anomalies.failureReasonSpike" "historical_failure_reason_baseline_not_indexed"
               , unavailable "anomalies.latencyTrend" "historical_latency_distribution_snapshots_unavailable"
               , unavailable "anomalies.parameterChangesSinceView" "visitor_local_baseline_is_evaluated_on_parameters_page"
               ]
        anomalyEvaluation =
          if null
              ( ccAvailability context
                  <> plcAvailability listContext
                  <> stateAvailability
              )
            then ("complete" :: Text)
            else "partial"
        overview = object
          [ "windowStart" .= windowStart
          , "windowEnd" .= now
          , "windowUnit" .= ("unix_seconds" :: Text)
          , "counts" .= object
              [ "indexedActions24h" .= show actionCount
              , "liquidations24h" .= show liquidationCount
              , "activeKeepers24h" .= show activeKeepers
              , "nonSuccessOutcomes24h" .= show failureCount
              , "pendingOrders" .= show (length pendingOrderTimes)
              , "ordersOlderThanMaxOrderAge" .= fmap show oldOrders
              ]
          , "housePool" .= maybe Null poolLiquidityJson poolResult
          , "protocolStatus" .= maybe Null protocolStatusJson statusResult
          , "operationalWallets" .= object
              [ "oracleUpdaterIdentityAvailable" .=
                  releasePublishesOracleUpdater release
              , "oracleUpdaterActivityAttributable" .= False
              , "publishedWalletCount" .=
                  show (length $ prOperationalWallets release)
              ]
          , "anomalies" .= anomalies
          , "anomalyEvaluation" .= anomalyEvaluation
          , "indexerLagBlocks" .= show indexerLag
          ]
    pure $ Right $ protocolResponse release context (Just indexedHead)
      (object
        [ "overview" .= ("mixed_exact_and_derived" :: Text)
        , "projectionCoverage" .= projectionCoverageEvidence release listContext
        ])
      (ccAvailability context <> plcAvailability listContext <> stateAvailability)
      "overview"
      overview
  where
    releaseId = prId release

getProtocolTransactionsResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> ProtocolTransactionFilters
  -> Int
  -> Maybe ProtocolCursor
  -> IO (Either ApiError (ApiResponse Value))
getProtocolTransactionsResponse pool client cfg releaseId filters@ProtocolTransactionFilters {..} requestedLimit cursor =
  withRelease cfg releaseId $ \release -> do
    let scope = transactionsCursorScope filters
    if not $ validActionCursor cursor
      then pure $ Left $ E.invalidAmount "cursor does not contain an activity position"
      else do
        contextResult <-
          resolveProjectionListContext pool client cfg release scope cursor
        case contextResult of
          Left err -> pure $ Left err
          Right listContext -> do
            let context = plcConfirmedContext listContext
            let pageLimit = max 1 $ min 100 requestedLimit
            rows <- withDb pool $ \conn -> do
              rows <- listProtocolActions conn releaseId ptfActionType ptfOutcome ptfAddress ptfAccount ptfKeeper
                ptfContract ptfTransactionHash ptfFromTimestamp ptfToTimestamp
                (ccBlockNumber context) (pageLimit + 1) (actionCursorKey cursor)
              pure rows
            let visible = take pageLimit rows
                nextCursor =
                  if length rows > pageLimit
                    then actionCursor release scope context =<< listToMaybe (reverse visible)
                    else Nothing
            pure $ Right $ projectionListResponse release listContext
              (object ["activity" .= ("confirmed_log_actions" :: Text)])
              []
              "transactions"
              (object
                [ "items" .= map actionToJson visible
                , "nextCursor" .= nextCursor
                , "filters" .= object
                    [ "actionType" .= ptfActionType
                    , "outcome" .= ptfOutcome
                    , "address" .= ptfAddress
                    , "account" .= ptfAccount
                    , "keeper" .= ptfKeeper
                    , "contract" .= ptfContract
                    , "transactionHash" .= ptfTransactionHash
                    , "fromTimestamp" .= fmap show ptfFromTimestamp
                    , "toTimestamp" .= fmap show ptfToTimestamp
                    ]
                ])

getProtocolTransactionResponse
  :: DbPool -> EthClient -> Config -> Text -> Text -> IO (Either ApiError (ApiResponse Value))
getProtocolTransactionResponse pool client cfg releaseId txHash =
  withRelease cfg releaseId $ \release -> do
    projectionContext <-
      resolveProjectionListContext
        pool
        client
        cfg
        release
        "transaction"
        Nothing
    case projectionContext of
      Left err -> pure $ Left err
      Right listContext ->
        getProtocolTransactionAt
          pool
          client
          release
          listContext
          txHash

getProtocolTransactionAt
  :: DbPool
  -> EthClient
  -> ProtocolRelease
  -> ProjectionListContext
  -> Text
  -> IO (Either ApiError (ApiResponse Value))
getProtocolTransactionAt pool client release listContext txHash = do
    let context = plcConfirmedContext listContext
        indexedHead = plcIndexedHead listContext
        releaseId = prId release
    (transaction, actions, events) <- withDb pool $ \conn -> do
      transaction <- getProtocolTransaction conn releaseId txHash (ccBlockNumber context)
      actions <- getProtocolActionsByTransaction conn releaseId txHash (ccBlockNumber context)
      events <- getProtocolEventsByTransaction conn releaseId txHash (ccBlockNumber context)
      pure (transaction, actions, events)
    case transaction of
      Nothing -> pure $ Left $ E.notFound "Transaction is not part of this protocol release"
      Just row -> do
        canonicality <- verifyTransactionCanonicality client row
        case canonicality of
          Left err -> pure $ Left err
          Right () -> do
            stateImpact <- transactionStateImpact client release row actions
            let stateImpactEvidence =
                  transactionStateImpactEvidenceLabel stateImpact
                analysis = transactionActionAnalysisJson release actions stateImpact
                analysisEvidence =
                  case stateImpactEvidence of
                    "canonical_hash_bound_block-level_delta" ->
                      ("exact_event_components_plus_block-level_state_analysis" :: Text)
                    "unavailable" ->
                      "exact_event_components_with_unavailable_state_analysis"
                    _ ->
                      "exact_event_components_plus_partial_block-level_state_analysis"
                availability =
                  ccAvailability context
                    <> plcAvailability listContext
                    <> transactionAvailability row
                    <> transactionStateImpactAvailability stateImpact
                    <> transactionActionAnalysisAvailability release actions stateImpact
            pure $ Right $ protocolResponse release context (Just indexedHead)
              (object
                [ "transaction" .= transactionEvidenceLabel row
                , "events" .= ("exact_raw_logs_with_best_effort_decoding" :: Text)
                , "actions" .= ("versioned_projection" :: Text)
                , "stateImpact" .= stateImpactEvidence
                , "analysis" .= analysisEvidence
                , "projectionCoverage" .= projectionCoverageEvidence release listContext
                ])
              availability
              "transaction"
              (object
                [ "chainTransaction" .= transactionToJson release row
                , "actions" .= map actionToJson actions
                , "events" .= map eventToJson events
                , "batchActionCount" .= length actions
                , "stateImpact" .= transactionStateImpactJson stateImpact
                , "analysis" .= analysis
                ])

getProtocolOrderResponse
  :: DbPool -> EthClient -> Config -> Text -> Integer -> IO (Either ApiError (ApiResponse Value))
getProtocolOrderResponse pool client cfg releaseId orderId =
  withRelease cfg releaseId $ \release -> do
    projectionContext <-
      resolveProjectionListContext
        pool
        client
        cfg
        release
        "order"
        Nothing
    case projectionContext of
      Left err -> pure $ Left err
      Right listContext ->
        getProtocolOrderAt pool client release listContext orderId

getProtocolOrderAt
  :: DbPool
  -> EthClient
  -> ProtocolRelease
  -> ProjectionListContext
  -> Integer
  -> IO (Either ApiError (ApiResponse Value))
getProtocolOrderAt pool client release listContext orderId = do
    let context = plcConfirmedContext listContext
        indexedHead = plcIndexedHead listContext
        releaseId = prId release
    (order, actions) <- withDb pool $ \conn -> do
      projectedOrder <-
        getPerpsOrderById conn (prChainId release) (prOrderRouter release) orderId Nothing
      actions <- getProtocolActionsByOrder conn releaseId orderId (ccBlockNumber context)
      pure
        ( projectedOrder >>= orderAtConfirmedBlock (ccBlockNumber context)
        , actions
        )
    case order of
      Nothing -> pure $ Left $ E.notFound "Order was not found in this protocol release"
      Just orderRow -> do
        commitTx <-
          maybe
            (pure Nothing)
            (withDb pool . getProtocolTransactionFor releaseId (ccBlockNumber context))
            (porCommitTxHash orderRow)
        terminalTx <-
          maybe
            (pure Nothing)
            (withDb pool . getProtocolTransactionFor releaseId (ccBlockNumber context))
            (porTerminalTxHash orderRow)
        commitCanonicality <-
          verifyOrderTransactionCanonicality
            client
            "commitment"
            (porCommitTxHash orderRow)
            (porCommitBlockNumber orderRow)
            (porCommitTimestamp orderRow)
            commitTx
        terminalCanonicality <-
          verifyOrderTransactionCanonicality
            client
            "terminal"
            (porTerminalTxHash orderRow)
            (porTerminalBlockNumber orderRow)
            (porTerminalTimestamp orderRow)
            terminalTx
        transactionActions <- withDb pool $ \conn -> do
          commitActions <-
            maybe
              (pure [])
              (\hash -> getProtocolActionsByTransaction conn releaseId hash (ccBlockNumber context))
              (porCommitTxHash orderRow)
          terminalActions <-
            maybe
              (pure [])
              (\hash -> getProtocolActionsByTransaction conn releaseId hash (ccBlockNumber context))
              (porTerminalTxHash orderRow)
          pure $ commitActions <> terminalActions
        commitBlockRef <-
          resolveCanonicalBlockRef client $ porCommitBlockNumber orderRow
        terminalBlockRef <-
          resolveCanonicalBlockRef client $ porTerminalBlockNumber orderRow
        settlementWindowRead <- case porCommitBlockNumber orderRow of
          Just _ ->
            readUintStateAtExact
              client
              (prPletherOracle release)
              "orderSettlementWindow()"
              0
              1
              commitBlockRef
          Nothing -> pure $ StateRead Nothing $ Just "commitment_block_unavailable"
        maxOrderAgeRead <- case porCommitBlockNumber orderRow of
          Just _ ->
            readUintStateAtExact
              client
              (prOrderRouter release)
              "maxOrderAge()"
              0
              1
              commitBlockRef
          Nothing -> pure $ StateRead Nothing $ Just "commitment_block_unavailable"
        pendingOrderRead <-
          pendingOrderViewAt client release orderId orderRow commitBlockRef
        terminalStatusRead <- case porTerminalBlockNumber orderRow of
          Just _ -> protocolStatusAt client release terminalBlockRef
          Nothing -> pure $ StateRead Nothing $ Just "terminal_block_unavailable"
        terminalPoolRead <- case porTerminalBlockNumber orderRow of
          Just _ -> poolLiquidityAt client release terminalBlockRef
          Nothing -> pure $ StateRead Nothing $ Just "terminal_block_unavailable"
        stateDelta <- orderStateDelta client release orderRow
        let correlatedActions =
              sortOn
                (\row -> (parBlockNumber row, parTxIndex row, parLogIndex row))
                $ nubBy ((==) `on` parActionId) (actions <> transactionActions)
            directlyCorrelatedIds = map parActionId actions
            sameTransactionIds =
              [ parActionId row
              | row <- correlatedActions
              , parActionId row `notElem` directlyCorrelatedIds
              ]
            pendingOrderWords = srValue pendingOrderRead
            pendingOrder = pendingOrderViewJson <$> pendingOrderWords
            settlementWindow = srValue settlementWindowRead
            maxOrderAge = srValue maxOrderAgeRead
            resolvedIntent =
              resolveOrderCommitIntent release orderId commitTx correlatedActions
            intent = ociValue <$> resolvedIntent
            intentUnavailableReason =
              orderIntentUnavailableReason release commitTx resolvedIntent
            exactIntentObserved =
              maybe False (const True) $
                exactOrderCommitIntent release orderId correlatedActions
            finalization =
              orderFinalizationFromActions release orderId correlatedActions
            finalizationExecutor =
              orderFinalizationField release orderId "executor" correlatedActions
            finalizationReceiptHash =
              orderFinalizationField release orderId "receiptHash" correlatedActions
            finalizationTerminalReason =
              orderFinalizationField release orderId "terminalReason" correlatedActions
            finalizationExecutionMode =
              orderFinalizationField release orderId "executionMode" correlatedActions
            finalizationFailedConstraint =
              orderFinalizationField release orderId "failedConstraint" correlatedActions
            finalizationFailure = do
              action <- findOrderFinalizationAction release orderId correlatedActions
              objectField "failure" $ parData action
            terminalKeeper =
              finalizationExecutor <|> (String <$> porCleanupActor orderRow)
            (firstEligible, lastEligible) =
              orderRevealTiming (porCommitTimestamp orderRow) settlementWindow
            expiryTimestamp = (+) <$> porCommitTimestamp orderRow <*> maxOrderAge
            terminalLatency = (-) <$> porTerminalTimestamp orderRow <*> porCommitTimestamp orderRow
            firstEligibleLatency =
              max 0 <$> ((-) <$> porTerminalTimestamp orderRow <*> firstEligible)
            terminalObserved =
              porTerminalTxHash orderRow /= Nothing
                || porTerminalBlockNumber orderRow /= Nothing
                || porTerminalTimestamp orderRow /= Nothing
            terminalExecuted = porTerminalStatus orderRow == "Executed"
            positionActivityObserved = porActivityType orderRow /= Nothing
            positionEvidenceAvailable =
              positionActivityObserved && terminalTx /= Nothing
            terminalMarketState =
              orderTerminalMarketState
                (porTerminalBlockNumber orderRow)
                terminalStatusRead
                terminalPoolRead
            slippageBoundary =
              orderSlippageBoundary resolvedIntent (porExecutionPrice orderRow)
            stateDeltaEvidence =
              orderStateDeltaEvidenceLabel stateDelta
            lifecycle = object
              [ "commitment" .= object
                  [ "transactionHash" .= porCommitTxHash orderRow
                  , "blockNumber" .= fmap show (porCommitBlockNumber orderRow)
                  , "timestamp" .= porCommitTimestamp orderRow
                  , "account" .= porAccount orderRow
                  , "side" .= porSide orderRow
                  , "intent" .= fromMaybe Null intent
                  , "onchainReservation" .= fromMaybe Null pendingOrder
                  , "maxOrderAgeSeconds" .= fmap show maxOrderAge
                  , "expiryTimestamp" .= expiryTimestamp
                  , "evidence" .= object
                      [ "event" .=
                          if exactIntentObserved
                            then ("exact_confirmed_intent_registered_event" :: Text)
                            else
                              if commitTx == Nothing
                                then "unavailable"
                                else "exact"
                      , "transactionHash" .=
                          orderEventEvidence commitTx "exact_confirmed_log" (porCommitTxHash orderRow)
                      , "blockNumber" .=
                          orderEventEvidence commitTx "exact_confirmed_log" (porCommitBlockNumber orderRow)
                      , "timestamp" .=
                          orderEventEvidence commitTx "exact_confirmed_log" (porCommitTimestamp orderRow)
                      , "account" .=
                          orderEventEvidence commitTx "exact_confirmed_log" (porAccount orderRow)
                      , "side" .=
                          orderEventEvidence commitTx "exact_confirmed_log" (porSide orderRow)
                      , "intent" .=
                          maybe ("unavailable" :: Text) ociEvidence resolvedIntent
                      , "onchainReservation" .=
                          if pendingOrder == Nothing
                            then ("unavailable" :: Text)
                            else "exact_historical_contract_read"
                      , "maxOrderAgeSeconds" .=
                          if maxOrderAge == Nothing
                            then ("unavailable" :: Text)
                            else "exact_historical_contract_read"
                      , "expiryTimestamp" .=
                          if expiryTimestamp == Nothing
                            then ("unavailable" :: Text)
                            else "derived"
                      ]
                  , "units" .= object
                      [ "blockNumber" .= ("block_number" :: Text)
                      , "timestamp" .= ("unix_seconds" :: Text)
                      , "side" .= ("enum" :: Text)
                      , "maxOrderAgeSeconds" .= ("seconds" :: Text)
                      , "expiryTimestamp" .= ("unix_seconds" :: Text)
                      ]
                  ]
              , "reveal" .= object
                  [ "firstEligibleTimestamp" .= firstEligible
                  , "lastEligibleTimestamp" .= lastEligible
                  , "settlementWindowSeconds" .= fmap show settlementWindow
                  , "pythComponentPublishTimes" .= Null
                  , "evidence" .= object
                      [ "firstEligibleTimestamp" .=
                          if firstEligible == Nothing
                            then ("unavailable" :: Text)
                            else "derived"
                      , "lastEligibleTimestamp" .=
                          if lastEligible == Nothing
                            then ("unavailable" :: Text)
                            else "derived"
                      , "pythComponentPublishTimes" .= ("unavailable" :: Text)
                      ]
                  , "units" .= object
                      [ "firstEligibleTimestamp" .= ("unix_seconds" :: Text)
                      , "lastEligibleTimestamp" .= ("unix_seconds" :: Text)
                      , "settlementWindowSeconds" .= ("seconds" :: Text)
                      ]
                  ]
              , "terminal" .= object
                  [ "transactionHash" .= porTerminalTxHash orderRow
                  , "blockNumber" .= fmap show (porTerminalBlockNumber orderRow)
                  , "timestamp" .= porTerminalTimestamp orderRow
                  , "status" .= porTerminalStatus orderRow
                  , "failureReason" .= porFailureReason orderRow
                  , "receiptHash" .= fromMaybe Null finalizationReceiptHash
                  , "terminalReason" .= fromMaybe Null finalizationTerminalReason
                  , "executionMode" .= fromMaybe Null finalizationExecutionMode
                  , "failedConstraint" .= fromMaybe Null finalizationFailedConstraint
                  , "failure" .= fromMaybe Null finalizationFailure
                  , "executionPrice" .= fmap show (porExecutionPrice orderRow)
                  , "marketAndOracleState" .= terminalMarketState
                  , "slippageBoundary" .= slippageBoundary
                  , "keeper" .= fromMaybe Null terminalKeeper
                  , "finalization" .= fromMaybe Null finalization
                  , "commitToTerminalLatencySeconds" .= fmap show terminalLatency
                  , "firstEligibleToTerminalLatencySeconds" .=
                      fmap show firstEligibleLatency
                  , "revealReadyToTerminalLatencySeconds" .= Null
                  , "evidence" .= object
                      [ "transactionHash" .=
                          orderEventEvidence terminalTx "exact_confirmed_log" (porTerminalTxHash orderRow)
                      , "blockNumber" .=
                          orderEventEvidence terminalTx "exact_confirmed_log" (porTerminalBlockNumber orderRow)
                      , "timestamp" .=
                          orderEventEvidence terminalTx "exact_confirmed_log" (porTerminalTimestamp orderRow)
                      , "status" .=
                          if terminalObserved
                            then
                              if terminalTx == Nothing
                                then ("unavailable" :: Text)
                                else "exact_confirmed_log"
                            else "derived_no_terminal_action_through_projection_anchor"
                      , "failureReason" .=
                          orderEventEvidence terminalTx "exact_confirmed_log" (porFailureReason orderRow)
                      , "receiptHash" .=
                          presentEvidence "exact_confirmed_order_finalized_event" finalizationReceiptHash
                      , "terminalReason" .=
                          presentEvidence "exact_confirmed_order_finalized_event" finalizationTerminalReason
                      , "executionMode" .=
                          presentEvidence "exact_confirmed_order_finalized_event" finalizationExecutionMode
                      , "failedConstraint" .=
                          presentEvidence "exact_confirmed_order_finalized_event" finalizationFailedConstraint
                      , "failure" .=
                          presentEvidence "exact_confirmed_order_finalized_event" finalizationFailure
                      , "executionPrice" .=
                          orderEventEvidence terminalTx "exact_confirmed_log" (porExecutionPrice orderRow)
                      , "marketAndOracleState" .=
                          if terminalTx == Nothing
                            then ("unavailable" :: Text)
                            else
                              case
                                ( stateReadEvidenceCompleteness terminalStatusRead
                                , stateReadEvidenceCompleteness terminalPoolRead
                                )
                              of
                                ("exact", "exact") -> "exact_historical_contract_reads"
                                ("unavailable", "unavailable") -> "unavailable"
                                _ -> "partial"
                      , "slippageBoundary" .=
                          if terminalTx == Nothing
                            || maybe True (const False) resolvedIntent
                            || porExecutionPrice orderRow == Nothing
                            then ("unavailable" :: Text)
                            else "derived"
                      , "keeper" .=
                          case finalizationExecutor of
                            Just _ -> ("exact_confirmed_order_finalized_event" :: Text)
                            Nothing ->
                              orderEventEvidence
                                terminalTx
                                "exact_confirmed_transaction_sender"
                                (porCleanupActor orderRow)
                      , "finalization" .=
                          presentEvidence "exact_confirmed_order_finalized_event" finalization
                      , "commitToTerminalLatencySeconds" .=
                          orderLifecycleDerivedEvidence commitTx terminalTx terminalLatency
                      , "firstEligibleToTerminalLatencySeconds" .=
                          orderLifecycleDerivedEvidence commitTx terminalTx firstEligibleLatency
                      , "revealReadyToTerminalLatencySeconds" .=
                          ("unavailable" :: Text)
                      ]
                  , "units" .= object
                      [ "blockNumber" .= ("block_number" :: Text)
                      , "timestamp" .= ("unix_seconds" :: Text)
                      , "executionPrice" .= ("indexPrice:8" :: Text)
                      , "commitToTerminalLatencySeconds" .= ("seconds" :: Text)
                      , "firstEligibleToTerminalLatencySeconds" .= ("seconds" :: Text)
                      , "revealReadyToTerminalLatencySeconds" .= ("seconds" :: Text)
                      ]
                  ]
              ]
            availability =
              ccAvailability context
                <> [ unavailable
                       "commitment"
                       "canonical_commitment_transaction_evidence_unavailable"
                   | commitTx == Nothing
                   ]
                <> [ unavailable
                       "terminal"
                       "canonical_terminal_transaction_evidence_unavailable"
                   | terminalObserved
                   , terminalTx == Nothing
                   ]
                <> [ unavailable "commitment.transactionHash" "commitment_transaction_unavailable"
                   | porCommitTxHash orderRow == Nothing
                   ]
                <> [ unavailable "commitment.blockNumber" "commitment_block_unavailable"
                   | porCommitBlockNumber orderRow == Nothing
                   ]
                <> [ unavailable "commitment.timestamp" "commitment_timestamp_unavailable"
                   | porCommitTimestamp orderRow == Nothing
                   ]
                <> [ unavailable "commitment.account" "commitment_account_unavailable"
                   | porAccount orderRow == Nothing
                   ]
                <> [ unavailable "commitment.side" "commitment_side_unavailable"
                   | porSide orderRow == Nothing
                   ]
                <> maybe [] (\reason -> [unavailable "commitment.intent" reason]) intentUnavailableReason
                <> stateReadAvailability "commitment.onchainReservation" pendingOrderRead
                <> stateReadAvailability "commitment.maxOrderAgeSeconds" maxOrderAgeRead
                <> [ unavailable "commitment.expiryTimestamp" "commitment_timestamp_unavailable"
                   | porCommitTimestamp orderRow == Nothing
                   ]
                <> [ unavailable "commitment.expiryTimestamp" "max_order_age_source_unavailable"
                   | maxOrderAge == Nothing
                   ]
                <> stateReadAvailability "reveal.settlementWindowSeconds" settlementWindowRead
                <> [ unavailable "reveal.firstEligibleTimestamp" "commitment_timestamp_unavailable"
                   | porCommitTimestamp orderRow == Nothing
                   ]
                <> [ unavailable "reveal.lastEligibleTimestamp" "commitment_timestamp_unavailable"
                   | porCommitTimestamp orderRow == Nothing
                   ]
                <> [ unavailable "reveal.lastEligibleTimestamp" "settlement_window_source_unavailable"
                   | settlementWindow == Nothing
                   ]
                <> [unavailable "reveal.pythComponentPublishTimes" "current_release_telemetry_missing"]
                <> [ unavailable "terminal.transactionHash" "order_terminal_not_observed_at_confirmed_block"
                   | porTerminalTxHash orderRow == Nothing
                   ]
                <> [ unavailable "terminal.blockNumber" "order_terminal_not_observed_at_confirmed_block"
                   | porTerminalBlockNumber orderRow == Nothing
                   ]
                <> [ unavailable "terminal.timestamp" "order_terminal_not_observed_at_confirmed_block"
                   | porTerminalTimestamp orderRow == Nothing
                   ]
                <> [ unavailable
                       "terminal.failureReason"
                       ( if terminalObserved
                           then
                             if terminalExecuted
                               then "not_applicable_to_executed_order"
                               else "terminal_failure_reason_unavailable"
                           else "order_terminal_not_observed_at_confirmed_block"
                       )
                   | porFailureReason orderRow == Nothing
                   ]
                <> [ unavailable
                       "terminal.executionPrice"
                       ( if terminalObserved
                           then
                             if terminalExecuted
                               then "execution_price_telemetry_unavailable"
                               else "not_applicable_to_non_executed_terminal"
                           else "order_terminal_not_observed_at_confirmed_block"
                       )
                   | porExecutionPrice orderRow == Nothing
                   ]
                <> [ unavailable
                       "terminal.keeper"
                       ( if terminalObserved
                           then "terminal_transaction_sender_unavailable"
                           else "order_terminal_not_observed_at_confirmed_block"
                       )
                   | maybe True (const False) terminalKeeper
                   ]
                <> [ unavailable
                       "terminal.finalization"
                       ( if terminalObserved
                           then "order_finalized_event_unavailable"
                           else "order_terminal_not_observed_at_confirmed_block"
                       )
                   | prOrderLifecycleBook release /= Nothing
                   , finalization == Nothing
                   ]
                <> [ unavailable "terminal.commitToTerminalLatencySeconds" "commitment_or_terminal_timestamp_unavailable"
                   | terminalLatency == Nothing
                   ]
                <> [ unavailable "terminal.firstEligibleToTerminalLatencySeconds" "first_eligible_or_terminal_timestamp_unavailable"
                   | firstEligibleLatency == Nothing
                   ]
                <> [unavailable "terminal.revealReadyToTerminalLatencySeconds" "oracle_publish_times_not_indexed"]
                <> orderTerminalMarketStateAvailability
                  "terminal.marketAndOracleState"
                  terminalStatusRead
                  terminalPoolRead
                <> orderSlippageAvailability resolvedIntent (porExecutionPrice orderRow)
                <> orderEconomicsAvailability orderId pendingOrderWords correlatedActions
                <> keeperTransactionCostAvailability terminalTx
                <> liquidationAvailability orderRow correlatedActions stateDelta
                <> stateDeltaAvailability stateDelta
                <> [ unavailable "positionChange" "position_activity_not_observed_for_order"
                   | not positionActivityObserved
                   ]
                <> [ unavailable "positionChange.sizeDelta" "position_size_delta_unavailable"
                   | positionActivityObserved
                   , porActivitySizeDelta orderRow == Nothing
                   ]
                <> [ unavailable "positionChange.price" "position_execution_price_unavailable"
                   | positionActivityObserved
                   , porActivityPrice orderRow == Nothing
                   ]
                <> [ unavailable "positionChange.pnlUsdc" "position_pnl_unavailable"
                   | positionActivityObserved
                   , porActivityPnlUsdc orderRow == Nothing
                   ]
            details = object
              [ "orderId" .= show orderId
              , "lifecycle" .= lifecycle
              , "positionChange" .= object
                  [ "activityType" .= porActivityType orderRow
                  , "sizeDelta" .= fmap show (porActivitySizeDelta orderRow)
                  , "price" .= fmap show (porActivityPrice orderRow)
                  , "pnlUsdc" .= fmap show (porActivityPnlUsdc orderRow)
                  , "evidence" .= object
                      [ "activityType" .=
                          orderEventEvidence terminalTx "exact_confirmed_log_projection" (porActivityType orderRow)
                      , "sizeDelta" .=
                          orderEventEvidence terminalTx "exact_confirmed_log_projection" (porActivitySizeDelta orderRow)
                      , "price" .=
                          orderEventEvidence terminalTx "exact_confirmed_log_projection" (porActivityPrice orderRow)
                      , "pnlUsdc" .=
                          orderEventEvidence terminalTx "exact_confirmed_log_projection" (porActivityPnlUsdc orderRow)
                      ]
                  , "units" .= object ["sizeDelta" .= ("position:18" :: Text), "price" .= ("indexPrice:8" :: Text), "pnlUsdc" .= ("USDC:6" :: Text)]
                  ]
              , "stateImpact" .= stateDeltaJson stateDelta
              , "economics" .= orderEconomics orderId pendingOrderWords correlatedActions
              , "keeperEconomics" .= keeperTransactionCostJson terminalTx
              , "actions" .= map actionToJson correlatedActions
              , "actionCorrelation" .= object
                  [ "directOrderActionIds" .= directlyCorrelatedIds
                  , "sameCommitOrTerminalTransactionActionIds" .= sameTransactionIds
                  , "provenance" .=
                      ("direct_order_id_or_same_canonical_chain_transaction" :: Text)
                  ]
              , "transactions" .= object
                  [ "commitment" .= fmap (transactionToJson release) commitTx
                  , "terminal" .= fmap (transactionToJson release) terminalTx
                  ]
              , "liquidation" .= liquidationDetails orderRow correlatedActions stateDelta
              ]
        case commitCanonicality >> terminalCanonicality of
          Left err -> pure $ Left err
          Right () ->
            pure $ Right $ protocolResponse release context (Just indexedHead)
              (object
                [ "lifecycle" .= object
                    [ "commitment" .=
                        if exactIntentObserved
                          then ("exact_confirmed_intent_event_plus_historical_reads" :: Text)
                          else
                            if commitTx == Nothing
                              then "unavailable"
                              else "exact_confirmed_log_plus_historical_reads"
                    , "reveal" .= ("derived_timing_with_explicit_unavailable_oracle_components" :: Text)
                    , "terminal" .=
                        case finalization of
                          Just _ -> ("exact_confirmed_order_finalized_event_plus_derived_latency" :: Text)
                          Nothing ->
                            if terminalObserved
                              then
                                if terminalTx == Nothing
                                  then "unavailable"
                                  else "exact_confirmed_log_plus_derived_latency"
                              else "derived_pending_through_projection_anchor"
                    ]
                , "positionChange" .=
                    if positionEvidenceAvailable
                      then ("exact_confirmed_log_projection" :: Text)
                      else "unavailable"
                , "stateImpact" .= stateDeltaEvidence
                , "actionCorrelation" .=
                    ("direct_order_id_plus_same_commit_or_terminal_transaction" :: Text)
                , "projectionCoverage" .= projectionCoverageEvidence release listContext
                ])
              (plcAvailability listContext <> availability)
              "order"
              details

-- | The oracle accepts the first tick strictly after commitment and keeps the
-- reveal window open through @commit + settlementWindow@. Keeping this
-- calculation pure and tested prevents the settlement-window upper bound from
-- being mistaken for the first eligible reveal time.
orderRevealTiming
  :: Maybe Integer
  -> Maybe Integer
  -> (Maybe Integer, Maybe Integer)
orderRevealTiming commitTimestamp settlementWindow =
  ( (+ 1) <$> commitTimestamp
  , (+) <$> commitTimestamp <*> settlementWindow
  )

-- | Present the mutable legacy order projection as of the response's
-- confirmed block. A future commitment is not yet visible; a future terminal
-- update is stripped back to the pending state so indexer/RPC skew cannot leak
-- data newer than the response envelope.
orderAtConfirmedBlock :: Integer -> PerpsOrderRow -> Maybe PerpsOrderRow
orderAtConfirmedBlock confirmedBlock orderRow
  | maybe False (> confirmedBlock) (porCommitBlockNumber orderRow) = Nothing
  | maybe False (> confirmedBlock) (porTerminalBlockNumber orderRow) =
      Just
        orderRow
          { porTerminalTxHash = Nothing
          , porTerminalBlockNumber = Nothing
          , porTerminalTimestamp = Nothing
          , porTerminalStatus = "Pending"
          , porFailureReason = Nothing
          , porExecutionPrice = Nothing
          , porCleanupActor = Nothing
          , porActivityType = Nothing
          , porActivitySizeDelta = Nothing
          , porActivityPrice = Nothing
          , porActivityPnlUsdc = Nothing
          , porSortBlock = fromMaybe 0 $ porCommitBlockNumber orderRow
          }
  | otherwise = Just orderRow

getHousePoolResponse
  :: DbPool -> EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse Value))
getHousePoolResponse pool client cfg releaseId =
  withRelease cfg releaseId $ \release -> do
    context <- confirmedContext client cfg
    indexedHead <- withDb pool $ \conn -> getProtocolIndexedHead conn releaseId
    poolRead <- poolLiquidityAt client release (ccBlockRef context)
    statusRead <- protocolStatusAt client release (ccBlockRef context)
    (houseGovernance, houseGovernanceAvailability) <-
      readGovernanceRole client release context HousePoolRole
    longSideRead <- sidesAt client release 0 (ccBlockRef context)
    shortSideRead <- sidesAt client release 1 (ccBlockRef context)
    rawAssetsRead <-
      mapStateRead (`word` 0)
        <$> callAtExactWords
          client
          (prUsdc release)
          "balanceOf(address)"
          [encodeAddress $ prHousePool release]
          (ccBlockRef context)
          1
    accountedAssetsRead <-
      mapStateRead (`word` 0)
        <$> callAtExactWords
          client
          (prHousePool release)
          "accountedAssets()"
          []
          (ccBlockRef context)
          1
    traderClaimsRead <-
      mapStateRead (`word` 0)
        <$> callAtExactWords
          client
          (prCfdEngine release)
          "totalTraderClaimBalanceUsdc()"
          []
          (ccBlockRef context)
          1
    badDebtRead <-
      mapStateRead (`word` 0)
        <$> callAtExactWords
          client
          (prCfdEngine release)
          "accumulatedBadDebtUsdc()"
          []
          (ccBlockRef context)
          1
    let poolResult = srValue poolRead
        statusResult = srValue statusRead
        longSide = srValue longSideRead
        shortSide = srValue shortSideRead
        longLiability = (`word` 0) <$> longSide
        shortLiability = (`word` 0) <$> shortSide
        maximumLiability = max <$> longLiability <*> shortLiability
        canonicalAssets = (`word` 0) <$> poolResult
        accountedAssets = srValue accountedAssetsRead
        rawAssets = srValue rawAssetsRead
        traderClaims = srValue traderClaimsRead
        badDebt = srValue badDebtRead
        physicalAssets = min <$> accountedAssets <*> rawAssets
        excessAssets = max 0 <$> ((-) <$> rawAssets <*> accountedAssets)
        coverageAssets = physicalAssets <|> canonicalAssets
        boundedLiabilitiesAndClaims =
          (+) <$> maximumLiability <*> traderClaims
        coverageBps = do
          assets <- coverageAssets
          liability <- boundedLiabilitiesAndClaims
          if liability == 0
            then Nothing
            else Just $ assets * 10_000 `div` liability
        solvencyHeadroom =
          (-) <$> coverageAssets <*> boundedLiabilitiesAndClaims
        waterfall = do
          words' <- poolResult
          pure $
            object
              [ "seniorPrincipalUsdc" .= show (word words' 5)
              , "juniorPrincipalUsdc" .= show (word words' 6)
              , "seniorHighWaterMarkUsdc" .= show (word words' 7)
              , "seniorImpairmentGapUsdc" .= show (max 0 $ word words' 7 - word words' 5)
              , "juniorFirstLossBufferUsdc" .= show (word words' 6)
              , "unit" .= ("USDC:6" :: Text)
              ]
        availability =
          ccAvailability context
            <> stateReadAvailability "housePool" poolRead
            <> stateReadAvailability "protocolStatus" statusRead
            <> stateReadAvailability "boundedLiability.long" longSideRead
            <> stateReadAvailability "boundedLiability.short" shortSideRead
            <> stateReadAvailability "assets.accountedUsdc" accountedAssetsRead
            <> stateReadAvailability "assets.rawUsdc" rawAssetsRead
            <> stateReadAvailability "traderClaimsUsdc" traderClaimsRead
            <> stateReadAvailability "badDebtUsdc" badDebtRead
            <> houseGovernanceAvailability
            <> [unavailable "assets.excessUsdc" "raw_or_accounted_asset_balance_unavailable" | excessAssets == Nothing]
            <> [unavailable "assets.physicalUsdc" "raw_or_accounted_asset_balance_unavailable" | physicalAssets == Nothing]
            <> [unavailable "grossCoverageRatioBps" "zero_bounded_liability_and_claims" | boundedLiabilitiesAndClaims == Just 0]
            <> [unavailable "grossCoverageRatioBps" "coverage_source_state_unavailable" | coverageBps == Nothing, boundedLiabilitiesAndClaims /= Just 0]
            <> [unavailable "solvencyHeadroomUsdc" "coverage_source_state_unavailable" | solvencyHeadroom == Nothing]
        houseState =
          object
            [ "accounting" .= maybe Null poolLiquidityJson poolResult
            , "protocolStatus" .= maybe Null protocolStatusJson statusResult
            , "governanceState" .= houseGovernance
            , "assets" .= object
                [ "accountedUsdc" .= fmap show accountedAssets
                , "rawUsdc" .= fmap show rawAssets
                , "excessUsdc" .= fmap show excessAssets
                , "physicalUsdc" .= fmap show physicalAssets
                , "canonicalUsdc" .= fmap show canonicalAssets
                , "unit" .= ("USDC:6" :: Text)
                ]
            , "traderClaimsUsdc" .= fmap show traderClaims
            , "badDebtUsdc" .= fmap show badDebt
            , "boundedLiability" .= object
                [ "longUsdc" .= fmap show longLiability
                , "shortUsdc" .= fmap show shortLiability
                , "maximumUsdc" .= fmap show maximumLiability
                , "plusTraderClaimsUsdc" .= fmap show boundedLiabilitiesAndClaims
                , "formulaIdentifier" .= ("protocol.maximum_bounded_liability_plus_trader_claims.v1" :: Text)
                , "formula" .= ("max(longMaximumProfit, shortMaximumProfit) plus total trader claims for coverage" :: Text)
                , "unit" .= ("USDC:6" :: Text)
                ]
            , "grossCoverageRatioBps" .= fmap show coverageBps
            , "solvencyHeadroomUsdc" .= fmap show solvencyHeadroom
            , "units" .= object
                [ "traderClaimsUsdc" .= ("USDC:6" :: Text)
                , "badDebtUsdc" .= ("USDC:6" :: Text)
                , "grossCoverageRatioBps" .= ("bps" :: Text)
                , "solvencyHeadroomUsdc" .= ("USDC:6 signed" :: Text)
                ]
            , "coverageBasis" .= object
                [ "numerator" .=
                    if physicalAssets == Nothing
                      then ("HousePool totalAssetsUsdc" :: Text)
                      else "min(raw USDC balance, accountedAssets)"
                , "denominator" .= ("maximum bounded trader liability plus total trader claims" :: Text)
                , "physicalAssetsIncluded" .= (physicalAssets /= Nothing)
                , "provenance" .= ("derived" :: Text)
                ]
            , "waterfall" .= fromMaybe Null waterfall
            ]
    pure $ Right $ protocolResponse release context indexedHead
      (object $
        [ "poolState" .= presentEvidence "exact_historical_contract_read" poolResult
        , "protocolStatus" .= presentEvidence "exact_historical_contract_read" statusResult
        , "accountedAssets" .= presentEvidence "exact_historical_contract_read" accountedAssets
        , "rawAssets" .= presentEvidence "exact_historical_contract_read" rawAssets
        , "traderClaims" .= presentEvidence "exact_historical_contract_read" traderClaims
        , "badDebt" .= presentEvidence "exact_historical_contract_read" badDebt
        , "governanceState" .=
            if null houseGovernanceAvailability
              then ("strict_exact_historical_admin_getter_reads_at_same_block" :: Text)
              else
                if null houseGovernance
                  then "unavailable"
                  else "partial"
        ]
          <> housePoolFinancialEvidenceFields
            maximumLiability
            coverageBps
            solvencyHeadroom)
      availability
      "housePool"
      houseState

getTrancheResponse
  :: DbPool -> EthClient -> Config -> Text -> Text -> IO (Either ApiError (ApiResponse Value))
getTrancheResponse pool client cfg releaseId tranche =
  withRelease cfg releaseId $ \release ->
    case trancheAddress release tranche of
      Nothing -> pure $ Left $ E.notFound "Tranche must be senior or junior"
      Just vault -> do
        context <- confirmedContext client cfg
        indexedHead <- withDb pool $ \conn -> getProtocolIndexedHead conn releaseId
        assetsRead <-
          mapStateRead (`word` 0)
            <$> callAtExactWords client vault "totalAssets()" [] (ccBlockRef context) 1
        supplyRead <-
          mapStateRead (`word` 0)
            <$> callAtExactWords client vault "totalSupply()" [] (ccBlockRef context) 1
        assetsPerShareRead <-
          mapStateRead (`word` 0)
            <$> callAtExactWords
              client
              vault
              "convertToAssets(uint256)"
              [encodeUintWord $ 10 ^ (18 :: Int)]
              (ccBlockRef context)
              1
        epochRead <-
          mapStateRead (`word` 0)
            <$> callAtExactWords client vault "currentDepositEpoch()" [] (ccBlockRef context) 1
        cooldownRead <-
          mapStateRead (`word` 0)
            <$> callAtExactWords client vault "DEPOSIT_COOLDOWN()" [] (ccBlockRef context) 1
        poolRead <- poolLiquidityAt client release (ccBlockRef context)
        let assets = srValue assetsRead
            supply = srValue supplyRead
            assetsPerShare = srValue assetsPerShareRead
            epoch = srValue epochRead
            cooldown = srValue cooldownRead
            poolResult = srValue poolRead
            principal = poolResult >>= \words' -> Just $ if normalizeTranche tranche == "senior" then word words' 5 else word words' 6
            totalPrincipal = poolResult >>= \words' -> Just $ word words' 5 + word words' 6
            seniorHighWater =
              if normalizeTranche tranche == "senior"
                then poolResult >>= \words' -> Just $ word words' 7
                else Nothing
            poolShare = do
              p <- principal
              total <- totalPrincipal
              if total == 0 then Nothing else Just $ p * 10_000 `div` total
            impairmentGap =
              if normalizeTranche tranche == "senior"
                then max 0 <$> ((-) <$> seniorHighWater <*> principal)
                else Nothing
            firstLossBuffer =
              if normalizeTranche tranche == "junior"
                then principal
                else Nothing
            wipeoutStatus = do
              assetsValue <- assets
              supplyValue <- supply
              principalValue <- principal
              pure $
                if supplyValue > 0 && (assetsValue == 0 || principalValue == 0)
                  then ("wiped_out" :: Text)
                  else "active"
            availability =
              ccAvailability context
                <> stateReadAvailability "navUsdc" assetsRead
                <> stateReadAvailability "shareSupply" supplyRead
                <> stateReadAvailability "assetsPerShare" assetsPerShareRead
                <> stateReadAvailability "currentDepositEpoch" epochRead
                <> stateReadAvailability "depositCooldownSeconds" cooldownRead
                <> stateReadAvailability "principalUsdc" poolRead
                <> stateReadAvailability "sharedPoolFreeUsdc" poolRead
                <> [ unavailable "poolShareBps" "zero_total_tranche_principal"
                   | totalPrincipal == Just 0
                   ]
                <> [ unavailable "poolShareBps" "source_state_unavailable"
                   | poolShare == Nothing
                   , totalPrincipal /= Just 0
                   ]
                <> [unavailable "currentWithdrawalCapacityUsdc" "account_address_and_epoch_state_required"]
                <> [unavailable "maxDeposit" "account_address_required"]
                <> [unavailable "maxMint" "account_address_required"]
                <> [unavailable "maxWithdraw" "account_address_required"]
                <> [unavailable "maxRedeem" "account_address_required"]
                <> [unavailable "depositMode" "current_release_getter_unavailable"]
                <> [unavailable "pendingEpochs" "current_release_getter_unavailable"]
                <> [unavailable "frozenOracleSurchargeBps" "current_release_getter_unavailable"]
                <> [unavailable "wipeoutStatus" "source_state_unavailable" | wipeoutStatus == Nothing]
                <> [ unavailable "seniorHighWaterMarkUsdc" "source_state_unavailable"
                   | normalizeTranche tranche == "senior"
                   , seniorHighWater == Nothing
                   ]
                <> [ unavailable "impairmentGapUsdc" "senior_high_water_or_principal_unavailable"
                   | normalizeTranche tranche == "senior"
                   , impairmentGap == Nothing
                   ]
                <> [ unavailable "seniorHighWaterMarkUsdc" "not_applicable_to_junior_tranche"
                   | normalizeTranche tranche == "junior"
                   ]
                <> [ unavailable "impairmentGapUsdc" "not_applicable_to_junior_tranche"
                   | normalizeTranche tranche == "junior"
                   ]
                <> [ unavailable "firstLossBufferUsdc" "source_state_unavailable"
                   | normalizeTranche tranche == "junior"
                   , firstLossBuffer == Nothing
                   ]
                <> [ unavailable "firstLossBufferUsdc" "not_applicable_to_senior_tranche"
                   | normalizeTranche tranche == "senior"
                   ]
            detail = object
              [ "tranche" .= normalizeTranche tranche
              , "vaultAddress" .= vault
              , "principalUsdc" .= fmap show principal
              , "navUsdc" .= fmap show assets
              , "shareSupply" .= fmap show supply
              , "assetsPerShare" .= fmap show assetsPerShare
              , "poolShareBps" .= fmap show poolShare
              , "sharedPoolFreeUsdc" .= fmap (show . (`word` 1)) poolResult
              , "currentWithdrawalCapacityUsdc" .= Null
              , "currentDepositEpoch" .= fmap show epoch
              , "depositCooldownSeconds" .= fmap show cooldown
              , "depositMode" .= Null
              , "pendingEpochs" .= Null
              , "frozenOracleSurchargeBps" .= Null
              , "maxDeposit" .= Null
              , "maxMint" .= Null
              , "maxWithdraw" .= Null
              , "maxRedeem" .= Null
              , "wipeoutStatus" .= wipeoutStatus
              , "seniorHighWaterMarkUsdc" .= fmap show seniorHighWater
              , "impairmentGapUsdc" .= fmap show impairmentGap
              , "firstLossBufferUsdc" .= fmap show firstLossBuffer
              , "units" .= object
                  [ "principalUsdc" .= ("USDC:6" :: Text)
                  , "navUsdc" .= ("USDC:6" :: Text)
                  , "shareSupply" .= ("shares:18" :: Text)
                  , "assetsPerShare" .= ("USDC:6 per share:18" :: Text)
                  , "poolShareBps" .= ("bps" :: Text)
                  , "sharedPoolFreeUsdc" .= ("USDC:6" :: Text)
                  , "currentWithdrawalCapacityUsdc" .= ("USDC:6" :: Text)
                  , "currentDepositEpoch" .= ("epoch_id" :: Text)
                  , "depositCooldownSeconds" .= ("seconds" :: Text)
                  , "depositMode" .= ("enum" :: Text)
                  , "pendingEpochs" .= ("count" :: Text)
                  , "frozenOracleSurchargeBps" .= ("bps" :: Text)
                  , "maxDeposit" .= ("USDC:6" :: Text)
                  , "maxMint" .= ("shares:18" :: Text)
                  , "maxWithdraw" .= ("USDC:6" :: Text)
                  , "maxRedeem" .= ("shares:18" :: Text)
                  , "wipeoutStatus" .= ("enum" :: Text)
                  , "seniorHighWaterMarkUsdc" .= ("USDC:6" :: Text)
                  , "impairmentGapUsdc" .= ("USDC:6" :: Text)
                  , "firstLossBufferUsdc" .= ("USDC:6" :: Text)
                  ]
              ]
        let exactEvidence value =
              if value == Nothing
                then ("unavailable" :: Text)
                else "exact_historical_contract_read"
            derivedEvidence value =
              if value == Nothing
                then ("unavailable" :: Text)
                else "derived_from_same_block_contract_reads"
            currentStateEvidence =
              object
                [ "principalUsdc" .= exactEvidence principal
                , "navUsdc" .= exactEvidence assets
                , "shareSupply" .= exactEvidence supply
                , "assetsPerShare" .= exactEvidence assetsPerShare
                , "poolShareBps" .= derivedEvidence poolShare
                , "sharedPoolFreeUsdc" .=
                    exactEvidence (poolResult >>= \words' -> Just $ word words' 1)
                , "currentWithdrawalCapacityUsdc" .= ("unavailable" :: Text)
                , "currentDepositEpoch" .= exactEvidence epoch
                , "depositCooldownSeconds" .= exactEvidence cooldown
                , "depositMode" .= ("unavailable" :: Text)
                , "pendingEpochs" .= ("unavailable" :: Text)
                , "frozenOracleSurchargeBps" .= ("unavailable" :: Text)
                , "maxDeposit" .= ("unavailable" :: Text)
                , "maxMint" .= ("unavailable" :: Text)
                , "maxWithdraw" .= ("unavailable" :: Text)
                , "maxRedeem" .= ("unavailable" :: Text)
                , "wipeoutStatus" .= derivedEvidence wipeoutStatus
                , "seniorHighWaterMarkUsdc" .= exactEvidence seniorHighWater
                , "impairmentGapUsdc" .= derivedEvidence impairmentGap
                , "firstLossBufferUsdc" .= exactEvidence firstLossBuffer
                ]
        pure $ Right $ protocolResponse release context indexedHead
          (object ["currentState" .= currentStateEvidence])
          availability
          "tranche"
          detail

getTrancheHistoryResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> Int
  -> Maybe TrancheHistoryCursor
  -> IO (Either ApiError (ApiResponse Value))
getTrancheHistoryResponse pool client cfg releaseId tranche requestedLimit cursor =
  withRelease cfg releaseId $ \release ->
    case trancheAddress release tranche of
      Nothing -> pure $ Left $ E.notFound "Tranche must be senior or junior"
      Just vault -> do
        let normalized = normalizeTranche tranche
            scope = "tranche-history:" <> normalized
        contextResult <-
          resolveTrancheHistoryContext
            pool
            client
            cfg
            release
            scope
            cursor
        case contextResult of
          Left err -> pure $ Left err
          Right listContext -> do
            let context = plcConfirmedContext listContext
            let pageLimit = max 1 $ min 500 requestedLimit
                actionsAlreadyComplete =
                  maybe False thcActionsComplete cursor
                checkpointsAlreadyComplete =
                  maybe False thcCheckpointsComplete cursor
                checkpointContinuationUnavailable =
                  maybe False thcCheckpointContinuationUnavailable cursor
            (actionRows, trancheSnapshotRows, poolSnapshots, longSnapshots, shortSnapshots) <-
              withDb pool $ \conn -> do
                rows <-
                  if actionsAlreadyComplete
                    then pure []
                    else
                      getTrancheActions
                        conn
                        releaseId
                        vault
                        (prHousePool release)
                        (ccBlockNumber context)
                        (pageLimit + 1)
                        (trancheHistoryActionCursorKey cursor)
                snapshotRows <-
                  if checkpointsAlreadyComplete
                    then pure []
                    else
                      getProtocolStateSnapshotsPage
                        conn
                        releaseId
                        ("tranche." <> normalized)
                        (ccBlockNumber context)
                        (pageLimit + 1)
                        (trancheHistoryCheckpointCursorKey cursor)
                let visibleSnapshotRows = take pageLimit snapshotRows
                    checkpointBlocks =
                      map pssBlockNumber visibleSnapshotRows
                poolRows <-
                  getProtocolStateSnapshotsAtBlocks
                    conn
                    releaseId
                    "house-pool.liquidity"
                    (ccBlockNumber context)
                    checkpointBlocks
                longRows <-
                  getProtocolStateSnapshotsAtBlocks
                    conn
                    releaseId
                    "market.long"
                    (ccBlockNumber context)
                    checkpointBlocks
                shortRows <-
                  getProtocolStateSnapshotsAtBlocks
                    conn
                    releaseId
                    "market.short"
                    (ccBlockNumber context)
                    checkpointBlocks
                pure
                  ( rows
                  , snapshotRows
                  , poolRows
                  , longRows
                  , shortRows
                  )
            let visibleActions = take pageLimit actionRows
                visibleSnapshots = take pageLimit trancheSnapshotRows
                actionsHaveMore =
                  not actionsAlreadyComplete
                    && length actionRows > pageLimit
                checkpointsHaveMore =
                  not checkpointsAlreadyComplete
                    && length trancheSnapshotRows > pageLimit
                nextActionPosition =
                  if actionsHaveMore
                    then listToMaybe $ reverse visibleActions
                    else Nothing
                nextCheckpointPosition =
                  if checkpointsHaveMore
                    then listToMaybe $ reverse visibleSnapshots
                    else Nothing
                nextCursor =
                  trancheHistoryCursor
                    release
                    scope
                    context
                    checkpointContinuationUnavailable
                    nextActionPosition
                    nextCheckpointPosition
                nextActionCursor =
                  trancheHistoryCursor
                    release
                    scope
                    context
                    True
                    nextActionPosition
                    Nothing
                nextCheckpointCursor =
                  trancheHistoryCursor
                    release
                    scope
                    context
                    False
                    Nothing
                    nextCheckpointPosition
                checkpoints =
                  trancheSnapshotCheckpoints
                    normalized
                    visibleSnapshots
                    poolSnapshots
                    longSnapshots
                    shortSnapshots
            pure $ Right $ projectionListResponse release listContext
              (object
                [ "history" .= ("best_effort_confirmed_known_and_common_log_actions" :: Text)
                , "checkpoints" .=
                    ("confirmed_range_end_contract_snapshot_keyset_page" :: Text)
                ])
              ([unavailable "beforeAfterState" "action_aligned_block_before_after_snapshots_not_backfilled"]
                <> [unavailable "extendedLifecycleEvents" "current_release_event_decoder_unavailable"]
                <> [ unavailable
                       "checkpointContinuation"
                       "legacy_action_cursor_has_no_checkpoint_position"
                   | checkpointContinuationUnavailable
                   ])
              "history"
              (object
                [ "tranche" .= normalized
                , "items" .= map actionToJson visibleActions
                , "nextCursor" .= nextCursor
                , "nextCursors" .= object
                    [ "combined" .= nextCursor
                    , "actions" .= nextActionCursor
                    , "checkpoints" .= nextCheckpointCursor
                    ]
                , "pagination" .= object
                    [ "actionsComplete" .= not actionsHaveMore
                    , "checkpointsComplete" .=
                        (not checkpointsHaveMore
                          && not checkpointContinuationUnavailable)
                    ]
                , "checkpoints" .= checkpoints
                , "csvColumns" .=
                    ( [ "recordType"
                      , "timestamp"
                      , "blockNumber"
                      , "blockHash"
                      , "actionType"
                      , "transactionHash"
                      , "account"
                      , "orderId"
                      , "principalUsdc"
                      , "navUsdc"
                      , "shareSupply"
                      , "assetsPerShare"
                      , "drawdownUsdc"
                      , "impairmentGapUsdc"
                      , "coverageRatioBps"
                      , "calculationVersion"
                      , "formulaIdentifier"
                      , "formula"
                      , "sourceScopes"
                      , "availability"
                      , "data"
                      , "units"
                      , "evidence"
                      ] :: [Text]
                    )
                ])

getKeepersResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> Int
  -> Maybe ProtocolCursor
  -> IO (Either ApiError (ApiResponse Value))
getKeepersResponse pool client cfg releaseId window requestedLimit cursor =
  withRelease cfg releaseId $ \release -> do
    let (normalizedWindow, seconds) = keeperWindow window
        scope = "keepers:" <> normalizedWindow
    if not $ validKeeperAggregateCursor cursor
      then pure $ Left $ E.invalidAmount "cursor does not contain a keeper-list position"
      else do
        contextResult <-
          resolveProjectionListContext pool client cfg release scope cursor
        case contextResult of
          Left err -> pure $ Left err
          Right listContext -> do
            let context = plcConfirmedContext listContext
            let pageLimit = max 1 $ min 500 requestedLimit
                windowEnd = ccBlockTimestamp context
                windowStart = max 0 $ windowEnd - seconds
            (keepers, leaders, windowSummary, latencyPercentiles, nativeSummary, pageNativeCosts) <-
              withDb pool $ \conn -> do
                rows <-
                  getKeeperAggregatesPage
                    conn
                    releaseId
                    windowStart
                    (ccBlockNumber context)
                    (pageLimit + 1)
                    (keeperAggregateCursorKey cursor)
                leaderRows <-
                  getKeeperRewardLeaders
                    conn
                    releaseId
                    windowStart
                    (ccBlockNumber context)
                aggregateSummary <-
                  getKeeperWindowSummary
                    conn
                    releaseId
                    windowStart
                    (ccBlockNumber context)
                latency <-
                  getKeeperLatencyPercentiles
                    conn
                    releaseId
                    windowStart
                    (ccBlockNumber context)
                costSummary <-
                  getKeeperNativeCostSummary
                    conn
                    releaseId
                    windowStart
                    (ccBlockNumber context)
                let visibleActors = map karActor $ take pageLimit rows
                visibleCosts <-
                  getKeeperNativeCostsForActors
                    conn
                    releaseId
                    windowStart
                    (ccBlockNumber context)
                    visibleActors
                pure
                  ( rows
                  , leaderRows
                  , aggregateSummary
                  , latency
                  , costSummary
                  , visibleCosts
                  )
            let visible = take pageLimit keepers
                nextCursor =
                  if length keepers > pageLimit
                    then keeperAggregateCursor release scope context =<< listToMaybe (reverse visible)
                    else Nothing
                totalObservedLiquidationRewards = kwsrGrossRewardsUsdc windowSummary
                observedLeaderRewards = map karGrossRewardsUsdc leaders
                top1 = sum $ take 1 observedLeaderRewards
                top3 = sum $ take 3 observedLeaderRewards
                share value =
                  if totalObservedLiquidationRewards == 0
                    then Nothing
                    else Just $ value * 10_000 `div` totalObservedLiquidationRewards
                otherRewards =
                  max 0 $
                    totalObservedLiquidationRewards - sum observedLeaderRewards
                donut =
                  map keeperRewardSlice leaders
                    <> [ object
                           [ "address" .= ("Other" :: Text)
                           , "observedLiquidationRewardsUsdc" .= show otherRewards
                           ]
                       | otherRewards > 0
                       ]
                (latencyMedian, latencyP90, latencyP99) = latencyPercentiles
                missingGasReceiptCount = kncsrMissingGasReceiptCount nativeSummary
                missingNativeValueCount = kncsrMissingNativeValueCount nativeSummary
                latencyEvidence =
                  if latencyMedian == Nothing
                    then ("unavailable" :: Text)
                    else "derived_from_confirmed_timestamps"
                concentrationEvidence =
                  if totalObservedLiquidationRewards <= 0
                    then ("unavailable" :: Text)
                    else "derived_from_exact_observed_liquidation_rewards"
                nativeCostsEvidence =
                  case
                      ( missingGasReceiptCount > 0
                      , missingNativeValueCount > 0
                      )
                    of
                    (False, False) ->
                      ("exact_distinct_transaction_receipts_and_native_value" :: Text)
                    _ -> "partial_distinct_transaction_receipts_and_native_value"
                summary = object
                  [ "window" .= normalizedWindow
                  , "windowStart" .= windowStart
                  , "windowEnd" .= windowEnd
                  , "definition" .= ("An active keeper submitted at least one confirmed successful permissionless protocol action in the selected window." :: Text)
                  , "activeKeeperCount" .= show (kwsrActiveKeeperCount windowSummary)
                  , "actionCount" .= show (kwsrActionCount windowSummary)
                  , "backlogProcessed" .=
                      show
                        ( kwsrExecutionCount windowSummary
                            + kwsrCleanupCount windowSummary
                        )
                  , "actionMix" .= object
                      [ "executions" .= show (kwsrExecutionCount windowSummary)
                      , "cleanups" .= show (kwsrCleanupCount windowSummary)
                      , "liquidations" .= show (kwsrLiquidationCount windowSummary)
                      ]
                  , "latencySeconds" .= object
                      [ "commitToTerminalMedian" .= fmap show latencyMedian
                      , "commitToTerminalP90" .= fmap show latencyP90
                      , "commitToTerminalP99" .= fmap show latencyP99
                      , "revealReadyMedian" .= Null
                      , "revealReadyP90" .= Null
                      , "revealReadyP99" .= Null
                      ]
                  , "observedLiquidationRewardsUsdc" .= show totalObservedLiquidationRewards
                  , "totalGrossRewardsUsdc" .= Null
                  , "rewardsByActionCategory" .= object
                      [ "liquidationsUsdc" .= show totalObservedLiquidationRewards
                      , "executionsUsdc" .= Null
                      , "cleanupsUsdc" .= Null
                      , "lpMaintenanceUsdc" .= Null
                      ]
                  , "nativeGasAndPythCosts" .= object
                      [ "gasCostWei" .= show (kncsrGasCostWei nativeSummary)
                      , "transactionNativeValueWei" .=
                          show (kncsrTransactionNativeValueWei nativeSummary)
                      , "missingGasReceiptCount" .= show missingGasReceiptCount
                      , "missingNativeValueCount" .= show missingNativeValueCount
                      , "nativeValueInterpretation" .=
                          ("Sum of available exact transaction values; the aggregate is partial when missingNativeValueCount is non-zero and is not labelled as a Pyth fee because that component cannot be isolated." :: Text)
                      , "profitUsdc" .= Null
                      ]
                  , "observedRewardConcentration" .= object
                      [ "basis" .= ("liquidation_bounties_emitted_by_current_release" :: Text)
                      , "topOneShareBps" .= fmap show (share top1)
                      , "topThreeShareBps" .= fmap show (share top3)
                      , "slices" .= donut
                      ]
                  , "concentrationTrend" .= Null
                  , "longestEligibleWorkGapSeconds" .= Null
                  , "keepers" .=
                      map
                        (\row ->
                          keeperToJson
                            (keeperNativeCostFor (karActor row) pageNativeCosts)
                            row
                        )
                        visible
                  , "nextCursor" .= nextCursor
                  , "units" .= object
                      [ "observedLiquidationRewardsUsdc" .= ("USDC:6" :: Text)
                      , "rewardShareBps" .= ("bps" :: Text)
                      , "latencySeconds" .= ("seconds" :: Text)
                      , "gasCostWei" .= ("wei" :: Text)
                      , "transactionNativeValueWei" .= ("wei" :: Text)
                      , "windowStart" .= ("unix_seconds" :: Text)
                      , "windowEnd" .= ("unix_seconds" :: Text)
                      ]
                  ]
            pure $ Right $ projectionListResponse release listContext
              (object
                [ "keeperIdentity" .= ("confirmed_successful_transaction_sender" :: Text)
                , "observedLiquidationRewards" .= ("exact_liquidation_bounty_event_when_present" :: Text)
                , "totalGrossRewards" .= ("unavailable" :: Text)
                , "nativeCosts" .= nativeCostsEvidence
                , "latency" .= latencyEvidence
                , "rewardConcentration" .= concentrationEvidence
                , "window" .= ("confirmed_block_timestamp_bounds" :: Text)
                , "keeperPage" .= ("confirmed_block_anchored_keyset" :: Text)
                ])
              ([unavailable "totalGrossRewardsUsdc" "keeper_total_gross_rewards_unavailable"]
                <> [unavailable "rewardsByActionCategory.executionsUsdc" "execution_reward_telemetry_unavailable"]
                <> [unavailable "rewardsByActionCategory.cleanupsUsdc" "cleanup_reward_telemetry_unavailable"]
                <> [unavailable "rewardsByActionCategory.lpMaintenanceUsdc" "lp_maintenance_reward_telemetry_unavailable"]
                <> [unavailable "actionMix.markUpdates" "attributable_mark_update_telemetry_unavailable"]
                <> [unavailable "actionMix.lpMaintenance" "attributable_lp_maintenance_telemetry_unavailable"]
                <> [unavailable "nativeGasAndPythCosts.gasCostWei" "partial_receipt_cost_data" | missingGasReceiptCount > 0]
                <> [unavailable "nativeGasAndPythCosts.transactionNativeValueWei" "partial_transaction_value_data" | missingNativeValueCount > 0]
                <> [unavailable "nativeGasAndPythCosts.pythFeeWei" "pyth_fee_component_not_isolated"]
                <> [unavailable "latencySeconds.commitToTerminalMedian" "no_confirmed_latency_samples_in_window" | latencyMedian == Nothing]
                <> [unavailable "latencySeconds.commitToTerminalP90" "no_confirmed_latency_samples_in_window" | latencyP90 == Nothing]
                <> [unavailable "latencySeconds.commitToTerminalP99" "no_confirmed_latency_samples_in_window" | latencyP99 == Nothing]
                <> [unavailable "revealReadyLatency" "oracle_publish_times_not_indexed"]
                <> [unavailable "observedRewardConcentration.topOneShareBps" "no_observed_liquidation_rewards_in_window" | totalObservedLiquidationRewards <= 0]
                <> [unavailable "observedRewardConcentration.topThreeShareBps" "no_observed_liquidation_rewards_in_window" | totalObservedLiquidationRewards <= 0]
                <> [unavailable "concentrationTrend" "historical_reward_snapshots_unavailable"]
                <> [unavailable "longestEligibleWorkGapSeconds" "eligible_work_intervals_not_indexed"]
                <> [unavailable "profit" "historical_native_to_usdc_conversion_unavailable"]
                <> [unavailable "revertedAttempts" "provider_failed_call_indexing_unavailable"])
              "keepers"
              summary

getKeeperResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> Text
  -> Int
  -> Maybe ProtocolCursor
  -> IO (Either ApiError (ApiResponse Value))
getKeeperResponse pool client cfg releaseId address window requestedLimit cursor =
  withRelease cfg releaseId $ \release -> do
    let (normalizedWindow, seconds) = keeperWindow window
        scope =
          "keeper-actions:"
            <> T.toLower address
            <> ":"
            <> normalizedWindow
    contextResult <-
      resolveProjectionListContext pool client cfg release scope cursor
    case contextResult of
      Left err -> pure $ Left err
      Right listContext -> do
        let context = plcConfirmedContext listContext
        let pageLimit = max 1 $ min 500 requestedLimit
            windowEnd = ccBlockTimestamp context
            windowStart = max 0 $ windowEnd - seconds
        (actions, allKeepers, nativeCosts) <- withDb pool $ \conn -> do
          rows <-
            getKeeperActions
              conn
              releaseId
              address
              windowStart
              (ccBlockNumber context)
              (pageLimit + 1)
              (actionCursorKey cursor)
          keeperRows <- getKeeperAggregates conn releaseId windowStart (ccBlockNumber context)
          costs <- getKeeperNativeCosts conn releaseId windowStart (ccBlockNumber context)
          pure (rows, keeperRows, costs)
        let visible = take pageLimit actions
            nextCursor =
              if length actions > pageLimit
                then actionCursor release scope context =<< listToMaybe (reverse visible)
                else Nothing
            keeper = listToMaybe $ filter ((== T.toLower address) . karActor) allKeepers
            nativeCost = keeperNativeCostFor address nativeCosts
            nativeCostAvailability =
              case nativeCost of
                Nothing -> [unavailable "summary.nativeCosts" "transaction_cost_rows_unavailable"]
                Just cost ->
                  [ unavailable "summary.nativeCosts.gasCostWei" "partial_receipt_cost_data"
                  | kncMissingGasReceiptCount cost > 0
                  ]
                    <> [ unavailable "summary.nativeCosts.transactionNativeValueWei" "partial_transaction_value_data"
                       | kncMissingNativeValueCount cost > 0
                       ]
            nativeCostEvidence =
              case nativeCost of
                Nothing -> ("unavailable" :: Text)
                Just cost
                  | kncMissingGasReceiptCount cost > 0
                      || kncMissingNativeValueCount cost > 0 ->
                      "partial_distinct_transaction_receipts_and_native_value"
                  | otherwise ->
                      "exact_distinct_transaction_receipts_and_native_value"
        if null actions && cursor == Nothing
          then pure $ Left $ E.notFound "No successful keeper actions were found for this address and window"
          else pure $ Right $ projectionListResponse release listContext
            (object
              [ "actions" .= ("confirmed_successful_log_actions" :: Text)
              , "summary" .= ("derived_from_confirmed_window_actions" :: Text)
              , "nativeCosts" .= nativeCostEvidence
              , "window" .= ("confirmed_block_timestamp_bounds" :: Text)
              ])
            ([unavailable "summary.totalGrossRewardsUsdc" "keeper_total_gross_rewards_unavailable"]
              <> nativeCostAvailability
              <> [unavailable "summary.nativeCosts.pythFeeWei" "pyth_fee_component_not_isolated"]
              <> [unavailable "actions.markUpdates" "attributable_mark_update_telemetry_unavailable"]
              <> [unavailable "actions.lpMaintenance" "attributable_lp_maintenance_telemetry_unavailable"])
            "keeper"
            (object
              [ "address" .= T.toLower address
              , "window" .= normalizedWindow
              , "windowStart" .= windowStart
              , "windowEnd" .= windowEnd
              , "summary" .= maybe Null (keeperToJson nativeCost) keeper
              , "actions" .= map actionToJson visible
              , "nextCursor" .= nextCursor
              , "units" .= object
                  [ "observedLiquidationRewardsUsdc" .= ("USDC:6" :: Text)
                  , "gasCostWei" .= ("wei" :: Text)
                  , "transactionNativeValueWei" .= ("wei" :: Text)
                  , "latencySeconds" .= ("seconds" :: Text)
                  , "windowStart" .= ("unix_seconds" :: Text)
                  , "windowEnd" .= ("unix_seconds" :: Text)
                  ]
              ])

getOperationalWalletsResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> Int
  -> Maybe ProtocolCursor
  -> IO (Either ApiError (ApiResponse Value))
getOperationalWalletsResponse pool client cfg releaseId window requestedLimit cursor =
  withRelease cfg releaseId $ \release -> do
    let (normalizedWindow, seconds) = keeperWindow window
        scope = "operational-wallets:" <> normalizedWindow
    if not $ validOperationalWalletCursor cursor
      then pure $ Left $ E.invalidAmount "cursor does not contain an operational-wallet position"
      else do
        contextResult <-
          resolveProjectionListContext pool client cfg release scope cursor
        case contextResult of
          Left err -> pure $ Left err
          Right listContext -> do
            let context = plcConfirmedContext listContext
                windowEnd = ccBlockTimestamp context
                windowStart = max 0 $ windowEnd - seconds
                pageLimit = max 1 $ min 50 requestedLimit
            activityRows <-
              withDb pool $ \conn ->
                getOperationalWalletActivity
                  conn
                  releaseId
                  windowStart
                  (ccBlockNumber context)
            (onchainRoles, roleAvailability) <-
              readOperationalOnchainRoles client release context
            let publicRoles = publicOperationalWalletRoles release
                observedRoles = concatMap observedOperationalRoles activityRows
                allRoleSources = publicRoles <> onchainRoles <> observedRoles
                candidateAddresses =
                  sortOn id $
                    nubBy (==) $
                      filter isPublishedOperationalAddress $
                        map (T.toLower . owrsAddress) allRoleSources
                afterCursor =
                  case cursor >>= pcItemId of
                    Nothing -> candidateAddresses
                    Just address ->
                      filter (> T.toLower address) candidateAddresses
                pageCandidates = take (pageLimit + 1) afterCursor
                visibleAddresses = take pageLimit pageCandidates
                (canServePage, canIssueCursor) =
                  operationalRolePaginationPolicy
                    (cursor /= Nothing)
                    (null roleAvailability)
                nextCursor =
                  if canIssueCursor && length pageCandidates > pageLimit
                    then
                      operationalWalletCursor release scope context
                        =<< listToMaybe (reverse visibleAddresses)
                    else Nothing
            costRows <-
              withDb pool $ \conn ->
                getOperationalWalletCostsForActors
                  conn
                  releaseId
                  windowStart
                  (ccBlockNumber context)
                  visibleAddresses
            balanceReads <-
              mapConcurrently
                (readOperationalNativeBalance client context)
                visibleAddresses
            let wallets =
                  [ operationalWalletSummaryJson
                      release
                      context
                      address
                      roleSources
                      (findOperationalActivity address activityRows)
                      ( operationalCostForAttributableRoles
                          roleSources
                          (findOperationalCost address costRows)
                      )
                      balanceRead
                  | (address, balanceRead) <- zip visibleAddresses balanceReads
                  , let roleSources = rolesForAddress address allRoleSources
                  ]
                oracleIdentityAvailability =
                  [ unavailable
                      "wallets.oracleUpdater"
                      "oracle_updater_identity_not_published_by_current_release"
                  | not $ releasePublishesOracleUpdater release
                  ]
                oracleTelemetryAvailability =
                  oracleUpdaterTelemetryAvailability "wallets" release
                rolePaginationAvailability =
                  if null roleAvailability
                    then []
                    else
                      [ unavailable
                          "wallets.pagination.operationalRoles"
                          "operational_role_reads_incomplete_pagination_suppressed"
                      , unavailable
                          "wallets.totalTrackedWalletCount"
                          "operational_role_reads_incomplete"
                      ]
                responseAvailability =
                  roleAvailability
                    <> rolePaginationAvailability
                    <> oracleIdentityAvailability
                    <> oracleTelemetryAvailability
                totalTrackedWalletCount =
                  if null roleAvailability
                    then String $ T.pack $ show $ length candidateAddresses
                    else Null
            if not canServePage
              then
                pure $
                  Left $
                    E.networkError
                      "Operational wallet role reads are temporarily incomplete; retry this page"
              else
                pure $ Right $ projectionListResponse release listContext
                  (object
                    [ "wallets" .= ("mixed_exact_and_derived" :: Text)
                    , "roles" .=
                        ("public_release_registry_exact_onchain_role_reads_and_confirmed_observed_activity" :: Text)
                    , "nativeBalances" .=
                        ("exact_canonical_confirmed_block_eth_getBalance_when_available" :: Text)
                    , "nativeCosts" .=
                        ("gas_and_gross_transaction_value_from_distinct_successful_transactions_when_available; native refunds are not netted" :: Text)
                    , "grossSpendDiagnostic" .=
                        ("derived_from_balance_and_median_available_gross_native_spend" :: Text)
                    ])
                  responseAvailability
                  "wallets"
                  (object
                    [ "window" .= normalizedWindow
                    , "windowStart" .= windowStart
                    , "windowEnd" .= windowEnd
                    , "oracleUpdaterIdentityAvailable" .=
                        releasePublishesOracleUpdater release
                    , "oracleUpdaterActivityAttributable" .= False
                    , "totalTrackedWalletCount" .= totalTrackedWalletCount
                    , "definition" .= operationalWalletDefinition
                    , "wallets" .= wallets
                    , "nextCursor" .= nextCursor
                    , "units" .= operationalWalletUnits
                    ])

getOperationalWalletResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> Text
  -> Int
  -> Maybe ProtocolCursor
  -> IO (Either ApiError (ApiResponse Value))
getOperationalWalletResponse pool client cfg releaseId address window requestedLimit cursor =
  withRelease cfg releaseId $ \release -> do
    let normalizedAddress = T.toLower address
        (normalizedWindow, seconds) = keeperWindow window
        scope =
          "operational-wallet-actions:"
            <> normalizedAddress
            <> ":"
            <> normalizedWindow
    contextResult <-
      resolveProjectionListContext pool client cfg release scope cursor
    case contextResult of
      Left err -> pure $ Left err
      Right listContext -> do
        let context = plcConfirmedContext listContext
            windowEnd = ccBlockTimestamp context
            windowStart = max 0 $ windowEnd - seconds
            pageLimit = max 1 $ min 500 requestedLimit
        activityRows <-
          withDb pool $ \conn ->
            getOperationalWalletActivity
              conn
              releaseId
              windowStart
              (ccBlockNumber context)
        (onchainRoles, roleAvailability) <-
          readOperationalOnchainRoles client release context
        let publicRoles = publicOperationalWalletRoles release
            observedRoles = concatMap observedOperationalRoles activityRows
            allRoleSources = publicRoles <> onchainRoles <> observedRoles
            roleSources = rolesForAddress normalizedAddress allRoleSources
            activity = findOperationalActivity normalizedAddress activityRows
            tracked = not $ null roleSources
            (canServePage, canIssueCursor) =
              operationalRolePaginationPolicy
                (cursor /= Nothing)
                (null roleAvailability)
        if not canServePage
          || (not (null roleAvailability) && not tracked)
          then
            pure $
              Left $
                E.networkError
                  "Operational wallet role reads are temporarily incomplete; retry this page"
          else if not tracked
          then
            pure $
              Left $
                E.notFound
                  "No public role or successful protocol activity was found for this operational wallet and window"
          else do
            (actions, transactions, costRows) <- withDb pool $ \conn -> do
              rows <-
                getOperationalWalletActions
                  conn
                  releaseId
                  normalizedAddress
                  windowStart
                  (ccBlockNumber context)
                  (pageLimit + 1)
                  (actionCursorKey cursor)
              txRows <-
                getProtocolTransactionsByHashes
                  conn
                  releaseId
                  (map parTxHash $ take pageLimit rows)
                  (ccBlockNumber context)
              costs <-
                getOperationalWalletCostsForActors
                  conn
                  releaseId
                  windowStart
                  (ccBlockNumber context)
                  [normalizedAddress]
              pure (rows, txRows, costs)
            balanceRead <-
              readOperationalNativeBalance client context normalizedAddress
            let visible = take pageLimit actions
                nextCursor =
                  if canIssueCursor && length actions > pageLimit
                    then actionCursor release scope context =<< listToMaybe (reverse visible)
                    else Nothing
                cost =
                  operationalCostForAttributableRoles
                    roleSources
                    (findOperationalCost normalizedAddress costRows)
                actionRows =
                  [ operationalActionToJson release row $
                      find
                        ((== T.toLower (parTxHash row)) . T.toLower . ptrTxHash)
                        transactions
                  | row <- visible
                  ]
                oracleIdentityAvailability =
                  [ unavailable
                      "wallet.oracleUpdater"
                      "oracle_updater_identity_not_published_by_current_release"
                  | not $ releasePublishesOracleUpdater release
                  ]
                rolePaginationAvailability =
                  [ unavailable
                      "wallet.pagination.operationalRoles"
                      "operational_role_reads_incomplete_pagination_suppressed"
                  | not $ null roleAvailability
                  ]
                responseAvailability =
                  roleAvailability
                    <> rolePaginationAvailability
                    <> oracleIdentityAvailability
                    <> operationalWalletAvailability
                      roleSources
                      activity
                      cost
                      balanceRead
                    <> operationalRoleTelemetryAvailability
                      "wallet"
                      roleSources
            pure $ Right $ projectionListResponse release listContext
              (object
                [ "wallet" .= ("mixed_exact_and_derived" :: Text)
                , "actions" .= ("confirmed_successful_actor_attributed_protocol_actions" :: Text)
                , "roles" .=
                    ("public_release_registry_exact_onchain_role_reads_and_confirmed_observed_activity" :: Text)
                , "nativeBalance" .=
                    operationalBalanceEvidence context balanceRead
                , "nativeCosts" .=
                    operationalCostEvidenceForRoles roleSources cost
                , "grossSpendDiagnostic" .=
                    operationalRunwayEvidence
                      release
                      context
                      roleSources
                      cost
                      balanceRead
                ])
              responseAvailability
              "wallet"
              (object
                [ "address" .= normalizedAddress
                , "window" .= normalizedWindow
                , "windowStart" .= windowStart
                , "windowEnd" .= windowEnd
                , "oracleUpdaterIdentityAvailable" .=
                    releasePublishesOracleUpdater release
                , "oracleUpdaterActivityAttributable" .= False
                , "roles" .= operationalRoleNames roleSources
                , "roleSources" .=
                    map (operationalRoleSourceToJson release) roleSources
                , "status" .=
                    operationalWalletStatus
                      (srValue balanceRead)
                      (cost >>= owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei)
                , "balances" .= object
                    [ "nativeBalanceWei" .= fmap show (srValue balanceRead)
                    ]
                , "activitySummary" .=
                    operationalActivitySummaryJson roleSources activity cost
                , "runway" .=
                    operationalRunwayJson release roleSources cost balanceRead
                , "actions" .= actionRows
                , "nextCursor" .= nextCursor
                , "evidence" .= object
                    [ "nativeBalance" .= operationalBalanceEvidence context balanceRead
                    , "activity" .= operationalActivityEvidence roleSources
                    , "nativeCosts" .=
                        operationalCostEvidenceForRoles roleSources cost
                    , "runway" .=
                        operationalRunwayEvidence
                          release
                          context
                          roleSources
                          cost
                          balanceRead
                    ]
                , "availability" .=
                    ( operationalWalletAvailability
                        roleSources
                        activity
                        cost
                        balanceRead
                        <> operationalRoleTelemetryAvailability
                          "wallet"
                          roleSources
                    )
                , "definition" .= operationalWalletDefinition
                , "units" .= operationalWalletUnits
                ])

estimateOperationalTransactionsAtObservedGrossSpend
  :: Maybe Integer
  -> Maybe Integer
  -> Maybe Integer
estimateOperationalTransactionsAtObservedGrossSpend balance medianGrossSpend = do
  availableBalance <- balance
  observedMedian <- medianGrossSpend
  if availableBalance < 0 || observedMedian <= 0
    then Nothing
    else Just $ availableBalance `div` observedMedian

-- | Deterministic conservative gross-spend classification. This is never a
-- future-execution guarantee or time estimate: it compares a confirmed native
-- balance with a windowed median successful-transaction gross native spend.
-- Transaction-value refunds and internal transfers are not netted without
-- trace or dedicated telemetry.
operationalWalletStatus :: Maybe Integer -> Maybe Integer -> Text
operationalWalletStatus Nothing _ = "unavailable"
operationalWalletStatus (Just balance) _
  | balance <= 0 = "depleted"
operationalWalletStatus balance medianGrossSpend =
  case estimateOperationalTransactionsAtObservedGrossSpend balance medianGrossSpend of
    Nothing -> "no_cost_baseline"
    Just remaining
      | remaining < operationalCriticalTransactionThreshold -> "critical"
      | remaining < operationalWarningTransactionThreshold -> "warning"
      | otherwise -> "healthy"

operationalCriticalTransactionThreshold :: Integer
operationalCriticalTransactionThreshold = 10

operationalWarningTransactionThreshold :: Integer
operationalWarningTransactionThreshold = 100

operationalRunwayFormulaIdentifier :: Text
operationalRunwayFormulaIdentifier =
  "operational_wallet.available_native_gross_spend.v1"

operationalRunwayCalculationVersion :: Text
operationalRunwayCalculationVersion =
  "operational-wallet-gross-spend-diagnostic-v1"

operationalWalletDefinition :: Value
operationalWalletDefinition =
  object
    [ "trackedIdentity" .=
        ("A public release-registry address, a current exact onchain role address, or an address with a confirmed successful keeper, liquidation, maintenance, or governance action in the selected window." :: Text)
    , "successfulActivity" .=
        ("Confirmed successful protocol actions only; reverted attempts are not inferred." :: Text)
    , "estimatedTransactionsAtObservedGrossSpend" .=
        ("Confirmed native balance divided by the median complete gross native spend of distinct successful operational transactions in the selected window." :: Text)
    , "grossNativeSpend" .=
        ("gasUsed * effectiveGasPriceWei + gross transactionNativeValueWei; both components must be available for a sample." :: Text)
    , "refundTreatment" .=
        ("Transaction-value refunds and internal native transfers are not netted because the current release has no attributable trace or fee-component telemetry." :: Text)
    , "interpretation" .=
        ("This is a conservative historical gross-spend diagnostic, not net cost, profit, or a time runway. One transaction can batch multiple protocol actions." :: Text)
    , "statusPolicy" .= object
        [ "version" .= operationalRunwayCalculationVersion
        , "depleted" .= ("confirmed balance is zero" :: Text)
        , "critical" .=
            ("fewer than 10 transactions at observed gross spend" :: Text)
        , "warning" .=
            ("fewer than 100 transactions at observed gross spend" :: Text)
        , "healthy" .=
            ("at least 100 transactions at observed gross spend" :: Text)
        , "noCostBaseline" .=
            ("no complete successful operational-transaction gross-spend sample is available" :: Text)
        , "unavailable" .=
            ("the confirmed native balance could not be read" :: Text)
        ]
    ]

operationalWalletUnits :: Value
operationalWalletUnits =
  object
    [ "nativeBalanceWei" .= ("wei" :: Text)
    , "observedGasCostWei" .= ("wei" :: Text)
    , "observedTransactionNativeValueWei" .= ("wei" :: Text)
    , "medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei" .= ("wei" :: Text)
    , "estimatedTransactionsAtObservedGrossSpend" .= ("transactions" :: Text)
    , "observedActionCount" .= ("actions" :: Text)
    , "observedTransactionCount" .= ("transactions" :: Text)
    , "windowStart" .= ("unix_seconds" :: Text)
    , "windowEnd" .= ("unix_seconds" :: Text)
    , "lastActivityTimestamp" .= ("unix_seconds" :: Text)
    ]

operationalWalletOverviewSignals
  :: ProtocolRelease
  -> ConfirmedContext
  -> [OperationalWalletActivityRow]
  -> [OperationalWalletCostRow]
  -> [(ProtocolOperationalWallet, StateRead Integer)]
  -> ([Value], [Value])
operationalWalletOverviewSignals release context activities costs registeredBalances =
  ( catMaybes $ map walletSignal registeredBalances
  , identityAvailability
      <> oracleUpdaterTelemetryAvailability
        "anomalies.operationalWallets"
        release
      <> concatMap walletAvailability registeredBalances
  )
  where
    identityAvailability =
      [ unavailable
          "anomalies.operationalWallets.oracleUpdater"
          "oracle_updater_identity_not_published_by_current_release"
      | not $ releasePublishesOracleUpdater release
      ]
    walletSignal (wallet, balanceRead) =
      let address = T.toLower $ powAddress wallet
          activity = findOperationalActivity address activities
          cost =
            if T.toLower (powRole wallet) == "oracle_updater"
              then Nothing
              else findOperationalCost address costs
          medianGrossSpend = cost >>= owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei
          status = operationalWalletStatus (srValue balanceRead) medianGrossSpend
          detail =
            object
              [ "address" .= address
              , "role" .= powRole wallet
              , "description" .= powDescription wallet
              , "nativeBalanceWei" .= fmap show (srValue balanceRead)
              , "medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei" .=
                  fmap show medianGrossSpend
              , "estimatedTransactionsAtObservedGrossSpend" .=
                  fmap show
                    ( estimateOperationalTransactionsAtObservedGrossSpend
                        (srValue balanceRead)
                        medianGrossSpend
                    )
              , "lastActivityTimestamp" .= fmap owaLastActivityAt activity
              , "formulaIdentifier" .= operationalRunwayFormulaIdentifier
              , "calculationVersion" .= operationalRunwayCalculationVersion
              , "estimateKind" .=
                  ("conservative_observed_gross_spend_diagnostic" :: Text)
              , "refundTreatment" .=
                  ("native_refunds_and_internal_transfers_not_netted" :: Text)
              , "sourceBlock" .= object
                  [ "number" .= show (ccBlockNumber context)
                  , "hash" .= ccBlockHash context
                  , "timestamp" .= ccBlockTimestamp context
                  ]
              , "units" .= operationalWalletUnits
              ]
       in case status of
            "depleted" ->
              Just $
                anomaly
                  "operational_wallet_depleted"
                  "critical"
                  "A publicly registered operational wallet has zero native balance at the confirmed block."
                  detail
            "critical" ->
              Just $
                anomaly
                  "operational_wallet_gross_spend_capacity_critical"
                  "critical"
                  "A publicly registered operational wallet funds fewer than 10 transactions at the selected window's observed gross native spend."
                  detail
            "warning" ->
              Just $
                anomaly
                  "operational_wallet_gross_spend_capacity_low"
                  "warning"
                  "A publicly registered operational wallet funds fewer than 100 transactions at the selected window's observed gross native spend."
                  detail
            _ -> Nothing
    walletAvailability (wallet, balanceRead) =
      let address = T.toLower $ powAddress wallet
          fieldPrefix = "anomalies.operationalWallets." <> address
          cost =
            if T.toLower (powRole wallet) == "oracle_updater"
              then Nothing
              else findOperationalCost address costs
       in stateReadAvailability (fieldPrefix <> ".nativeBalanceWei") balanceRead
            <> [ unavailable
                  (fieldPrefix <> ".estimatedTransactionsAtObservedGrossSpend")
                  "successful_operational_transaction_gross_native_spend_sample_unavailable"
               | (cost >>= owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei) == Nothing
               ]
            <> [ unavailable
                  (fieldPrefix <> ".runway.sampleCoverage")
                  "partial_successful_transaction_gross_native_spend_coverage"
               | maybe 0 owcMissingGasReceiptCount cost > 0
                  || maybe 0 owcMissingNativeValueCount cost > 0
               ]
            <> [ unavailable
                  (fieldPrefix <> ".estimatedTransactionsAtObservedGrossSpend.netSpend")
                  "native_refunds_not_netted_without_trace_or_telemetry"
               | maybe 0 owcAvailableGrossNativeSpendSampleCount cost > 0
               ]

operationalWalletSummaryJson
  :: ProtocolRelease
  -> ConfirmedContext
  -> Text
  -> [OperationalWalletRoleSource]
  -> Maybe OperationalWalletActivityRow
  -> Maybe OperationalWalletCostRow
  -> StateRead Integer
  -> Value
operationalWalletSummaryJson release context address roleSources activity cost balanceRead =
  let roleNames = operationalRoleNames roleSources
      telemetryAttributable = operationalTelemetryAttributable roleSources
      metric = operationalMetricJsonForRoles roleNames
   in object
    [ "address" .= T.toLower address
    , "roles" .= operationalRoleNames roleSources
    , "roleSources" .= map (operationalRoleSourceToJson release) roleSources
    , "status" .=
        operationalWalletStatus
          (srValue balanceRead)
          (cost >>= owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei)
    , "nativeBalanceWei" .= fmap show (srValue balanceRead)
    , "observedGasCostWei" .=
        metric (maybe 0 owcObservedGasCostWei cost)
    , "observedTransactionNativeValueWei" .=
        metric (maybe 0 owcObservedTransactionNativeValueWei cost)
    , "observedActionCount" .=
        metric (maybe 0 owaActionCount activity)
    , "observedTransactionCount" .=
        metric (maybe 0 owaTransactionCount activity)
    , "medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei" .=
        fmap show (cost >>= owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei)
    , "estimatedTransactionsAtObservedGrossSpend" .=
        fmap show
          ( estimateOperationalTransactionsAtObservedGrossSpend
              (srValue balanceRead)
              (cost >>= owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei)
          )
    , "runwayFormula" .=
        operationalRunwayFormulaJson release roleSources cost
    , "lastActivityTimestamp" .=
        if telemetryAttributable
          then Aeson.toJSON $ fmap owaLastActivityAt activity
          else Null
    , "evidence" .= object
        [ "nativeBalance" .= operationalBalanceEvidence context balanceRead
        , "activity" .= operationalActivityEvidence roleSources
        , "nativeCosts" .=
            operationalCostEvidenceForRoles roleSources cost
        , "runway" .=
            operationalRunwayEvidence
              release
              context
              roleSources
              cost
              balanceRead
        ]
    , "availability" .=
        ( operationalWalletAvailability
            roleSources
            activity
            cost
            balanceRead
            <> operationalRoleTelemetryAvailability
              "wallet"
              roleSources
        )
    , "units" .= operationalWalletUnits
    ]

operationalActivitySummaryJson
  :: [OperationalWalletRoleSource]
  -> Maybe OperationalWalletActivityRow
  -> Maybe OperationalWalletCostRow
  -> Value
operationalActivitySummaryJson roleSources activity cost =
  let telemetryAttributable = operationalTelemetryAttributable roleSources
      metric =
        operationalMetricJsonForRoles $ operationalRoleNames roleSources
      timestamp selector =
        if telemetryAttributable
          then Aeson.toJSON $ selector <$> activity
          else Null
   in object
    [ "observedActionCount" .= metric (maybe 0 owaActionCount activity)
    , "observedTransactionCount" .= metric (maybe 0 owaTransactionCount activity)
    , "executionCount" .= metric (maybe 0 owaExecutionCount activity)
    , "cleanupCount" .= metric (maybe 0 owaCleanupCount activity)
    , "liquidationCount" .= metric (maybe 0 owaLiquidationCount activity)
    , "maintenanceCount" .= metric (maybe 0 owaMaintenanceCount activity)
    , "governanceActionCount" .= metric (maybe 0 owaGovernanceCount activity)
    , "firstActivityTimestamp" .= timestamp owaFirstActivityAt
    , "lastActivityTimestamp" .= timestamp owaLastActivityAt
    , "observedGasCostWei" .= metric (maybe 0 owcObservedGasCostWei cost)
    , "observedTransactionNativeValueWei" .=
        metric (maybe 0 owcObservedTransactionNativeValueWei cost)
    , "availableGrossNativeSpendSampleCount" .=
        metric (maybe 0 owcAvailableGrossNativeSpendSampleCount cost)
    , "missingGasReceiptCount" .=
        metric (maybe 0 owcMissingGasReceiptCount cost)
    , "missingNativeValueCount" .=
        metric (maybe 0 owcMissingNativeValueCount cost)
    ]

operationalMetricJsonForRoles :: [Text] -> Integer -> Value
operationalMetricJsonForRoles roleNames value
  | any (/= "oracle_updater") roleNames =
      String $ T.pack $ show value
  | otherwise = Null

-- | An initial page may expose explicitly partial role data, but it must not
-- issue a continuation cursor. A continuation page fails closed until every
-- anchored role read succeeds so membership cannot change between pages.
operationalRolePaginationPolicy :: Bool -> Bool -> (Bool, Bool)
operationalRolePaginationPolicy isContinuation roleReadsComplete =
  (not isContinuation || roleReadsComplete, roleReadsComplete)

operationalRunwayJson
  :: ProtocolRelease
  -> [OperationalWalletRoleSource]
  -> Maybe OperationalWalletCostRow
  -> StateRead Integer
  -> Value
operationalRunwayJson release roleSources cost balanceRead =
  object
    [ "estimatedTransactionsAtObservedGrossSpend" .=
        fmap show
          ( estimateOperationalTransactionsAtObservedGrossSpend
              (srValue balanceRead)
              (cost >>= owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei)
          )
    , "medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei" .=
        fmap show (cost >>= owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei)
    , "formulaIdentifier" .= operationalRunwayFormulaIdentifier
    , "calculationVersion" .= operationalRunwayCalculationVersion
    , "releaseCalculationVersion" .= prCalculationVersion release
    , "estimateKind" .=
        ("conservative_observed_gross_spend_diagnostic" :: Text)
    , "refundTreatment" .=
        ("native_refunds_and_internal_transfers_not_netted" :: Text)
    , "expression" .=
        ("floor(nativeBalanceWei / median(distinctSuccessfulOperationalTransaction.gasCostWei + distinctSuccessfulOperationalTransaction.transactionNativeValueWei))" :: Text)
    , "sampleCount" .=
        operationalMetricJsonForRoles
          (operationalRoleNames roleSources)
          (maybe 0 owcAvailableGrossNativeSpendSampleCount cost)
    ]

operationalRunwayFormulaJson
  :: ProtocolRelease
  -> [OperationalWalletRoleSource]
  -> Maybe OperationalWalletCostRow
  -> Value
operationalRunwayFormulaJson release roleSources cost =
  object
    [ "formulaIdentifier" .= operationalRunwayFormulaIdentifier
    , "calculationVersion" .= operationalRunwayCalculationVersion
    , "releaseCalculationVersion" .= prCalculationVersion release
    , "estimateKind" .=
        ("conservative_observed_gross_spend_diagnostic" :: Text)
    , "refundTreatment" .=
        ("native_refunds_and_internal_transfers_not_netted" :: Text)
    , "expression" .=
        ("floor(nativeBalanceWei / median(distinctSuccessfulOperationalTransaction.gasCostWei + distinctSuccessfulOperationalTransaction.transactionNativeValueWei))" :: Text)
    , "sampleCount" .=
        operationalMetricJsonForRoles
          (operationalRoleNames roleSources)
          (maybe 0 owcAvailableGrossNativeSpendSampleCount cost)
    ]

operationalWalletAvailability
  :: [OperationalWalletRoleSource]
  -> Maybe OperationalWalletActivityRow
  -> Maybe OperationalWalletCostRow
  -> StateRead Integer
  -> [Value]
operationalWalletAvailability roleSources _activity cost balanceRead =
  stateReadAvailability "nativeBalanceWei" balanceRead
    <> if operationalTelemetryAttributable roleSources
      then
        [ unavailable
            "medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei"
            "successful_operational_transaction_gross_native_spend_sample_unavailable"
        | (cost >>= owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei) == Nothing
        ]
          <> [ unavailable
                "estimatedTransactionsAtObservedGrossSpend"
                "successful_operational_transaction_gross_native_spend_sample_unavailable"
             | srValue balanceRead /= Nothing
             , (cost >>= owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei) == Nothing
             ]
          <> [ unavailable
                "estimatedTransactionsAtObservedGrossSpend"
                "canonical_native_balance_unavailable"
             | srValue balanceRead == Nothing
             ]
          <> [ unavailable
                "observedGasCostWei"
                "partial_successful_transaction_receipt_coverage"
             | maybe 0 owcMissingGasReceiptCount cost > 0
             ]
          <> [ unavailable
                "observedTransactionNativeValueWei"
                "partial_successful_transaction_native_value_coverage"
             | maybe 0 owcMissingNativeValueCount cost > 0
             ]
          <> [ unavailable
                "runway.sampleCoverage"
                "partial_successful_transaction_gross_native_spend_coverage"
             | maybe 0 owcMissingGasReceiptCount cost > 0
                || maybe 0 owcMissingNativeValueCount cost > 0
             ]
          <> [ unavailable
                "estimatedTransactionsAtObservedGrossSpend.netSpend"
                "native_refunds_not_netted_without_trace_or_telemetry"
             | maybe 0 owcAvailableGrossNativeSpendSampleCount cost > 0
             ]
      else []

operationalCostEvidence :: Maybe OperationalWalletCostRow -> Text
operationalCostEvidence Nothing =
  "no_observed_successful_operational_transaction_gross_spend_rows"
operationalCostEvidence (Just cost)
  | owcMissingGasReceiptCount cost > 0
      || owcMissingNativeValueCount cost > 0 =
      "partial_distinct_successful_transaction_gas_and_gross_value"
  | otherwise =
      "exact_gas_and_gross_transaction_value_net_spend_unavailable"

operationalCostEvidenceForRoles
  :: [OperationalWalletRoleSource]
  -> Maybe OperationalWalletCostRow
  -> Text
operationalCostEvidenceForRoles roleSources cost
  | operationalTelemetryAttributable roleSources =
      operationalCostEvidence cost
  | otherwise =
      "unavailable_oracle_updater_gross_spend_not_attributable_current_release"

operationalBalanceEvidence :: ConfirmedContext -> StateRead Integer -> Value
operationalBalanceEvidence context balanceRead =
  object
    [ "level" .=
        if srValue balanceRead == Nothing
          then ("unavailable" :: Text)
          else "exact"
    , "source" .= ("eth_getBalance_eip1898" :: Text)
    , "sourceBlock" .= object
        [ "number" .= show (ccBlockNumber context)
        , "hash" .= ccBlockHash context
        , "timestamp" .= ccBlockTimestamp context
        ]
    ]

operationalRunwayEvidence
  :: ProtocolRelease
  -> ConfirmedContext
  -> [OperationalWalletRoleSource]
  -> Maybe OperationalWalletCostRow
  -> StateRead Integer
  -> Value
operationalRunwayEvidence release context roleSources cost balanceRead =
  object
    [ "level" .=
        if estimate == Nothing
          then ("unavailable" :: Text)
          else
            if maybe 0 owcMissingGasReceiptCount cost > 0
              || maybe 0 owcMissingNativeValueCount cost > 0
              then "partial"
              else "derived"
    , "formulaIdentifier" .= operationalRunwayFormulaIdentifier
    , "calculationVersion" .= operationalRunwayCalculationVersion
    , "releaseCalculationVersion" .= prCalculationVersion release
    , "estimateKind" .=
        ("conservative_observed_gross_spend_diagnostic" :: Text)
    , "refundTreatment" .=
        ("native_refunds_and_internal_transfers_not_netted" :: Text)
    , "sourceBlock" .= object
        [ "number" .= show (ccBlockNumber context)
        , "hash" .= ccBlockHash context
        , "timestamp" .= ccBlockTimestamp context
        ]
    , "sampleCount" .=
        operationalMetricJsonForRoles
          (operationalRoleNames roleSources)
          (maybe 0 owcAvailableGrossNativeSpendSampleCount cost)
    , "balanceEvidence" .= operationalBalanceEvidence context balanceRead
    , "costEvidence" .=
        operationalCostEvidenceForRoles roleSources cost
    ]
  where
    estimate =
      estimateOperationalTransactionsAtObservedGrossSpend
        (srValue balanceRead)
        (cost >>= owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei)

readOperationalNativeBalance
  :: EthClient
  -> ConfirmedContext
  -> Text
  -> IO (StateRead Integer)
readOperationalNativeBalance client context address =
  case ccBlockRef context of
    Nothing ->
      pure $
        StateRead Nothing $ Just "canonical_block_anchor_unavailable"
    Just blockRef -> do
      result <-
        ethGetBalanceAtCanonicalBlock
          client
          address
          blockRef
      pure $ case result of
        Left _ ->
          StateRead Nothing $ Just "canonical_native_balance_unavailable"
        Right balance ->
          StateRead (Just balance) Nothing

publicOperationalWalletRoles
  :: ProtocolRelease
  -> [OperationalWalletRoleSource]
publicOperationalWalletRoles release =
  [ OperationalWalletRoleSource
      { owrsAddress = T.toLower $ powAddress wallet
      , owrsRole = T.toLower $ powRole wallet
      , owrsSource = "release_manifest_public_registry"
      , owrsSourceContract = Nothing
      , owrsSourceAddress = Nothing
      , owrsDescription = Just $ powDescription wallet
      , owrsRepresentativeEvidence =
          Just $ powRepresentativeEvidence wallet
      , owrsEvidence =
          "checked_in_release_metadata_from_observed_public_transactions"
      }
  | wallet <- prOperationalWallets release
  , isPublishedOperationalAddress $ powAddress wallet
  ]

releasePublishesOracleUpdater :: ProtocolRelease -> Bool
releasePublishesOracleUpdater =
  any
    (\wallet ->
      T.toLower (powRole wallet) == "oracle_updater"
        && isPublishedOperationalAddress (powAddress wallet)
    )
    . prOperationalWallets

oracleUpdaterTelemetryAvailability
  :: Text
  -> ProtocolRelease
  -> [Value]
oracleUpdaterTelemetryAvailability fieldPrefix release =
  if releasePublishesOracleUpdater release
    then
      [ unavailable
          (fieldPrefix <> ".oracleUpdater.activity")
          "oracle_updater_activity_not_attributable_current_release"
      , unavailable
          (fieldPrefix <> ".oracleUpdater.grossNativeSpend")
          "oracle_updater_gross_native_spend_not_attributable_current_release"
      , unavailable
          (fieldPrefix <> ".oracleUpdater.estimatedTransactionsAtObservedGrossSpend")
          "oracle_updater_gross_native_spend_not_attributable_current_release"
      ]
    else []

readOperationalOnchainRoles
  :: EthClient
  -> ProtocolRelease
  -> ConfirmedContext
  -> IO ([OperationalWalletRoleSource], [Value])
readOperationalOnchainRoles client release context = do
  results <-
    mapConcurrently readRole $ operationalOnchainRoleDefinitions release
  pure
    (catMaybes $ map fst results, concatMap snd results)
  where
    readRole (role, sourceContract, contractAddress, signature) = do
      let getterDefinition =
            GovernanceGetterDefinition
              { ggdKey = "governance." <> role
              , ggdPurpose = RoleGetter
              , ggdFunction =
                  GovernanceFunction
                    { governanceFunctionSignature = signature
                    , governanceFunctionSelector = encodeCall signature []
                    }
              , ggdValueType = AddressField
              }
      result <-
        readGovernanceGetterAt
          client
          contractAddress
          (ccBlockRef context)
          getterDefinition
      pure $ case result of
        Right (GovernanceAddress address)
          | isPublishedOperationalAddress address ->
              ( Just
                  OperationalWalletRoleSource
                    { owrsAddress = T.toLower address
                    , owrsRole = sourceContract <> "_" <> role
                    , owrsSource = "historical_contract_read"
                    , owrsSourceContract = Just sourceContract
                    , owrsSourceAddress = Just contractAddress
                    , owrsDescription =
                        Just "Current release-scoped role read directly from canonical contract state."
                    , owrsRepresentativeEvidence = Nothing
                    , owrsEvidence = "exact_historical_contract_read"
                    }
              , []
              )
          | otherwise -> (Nothing, [])
        Right _ ->
          ( Nothing
          , [unavailable ("roles." <> sourceContract <> "." <> role) "malformed_governance_role_type"]
          )
        Left reason ->
          ( Nothing
          , [unavailable ("roles." <> sourceContract <> "." <> role) reason]
          )

operationalOnchainRoleDefinitions
  :: ProtocolRelease
  -> [(Text, Text, Text, Text)]
operationalOnchainRoleDefinitions release =
  [ ("owner", "order_router_admin", prOrderRouterAdmin release, "owner()")
  , ("pending_owner", "order_router_admin", prOrderRouterAdmin release, "pendingOwner()")
  , ("pauser", "order_router_admin", prOrderRouterAdmin release, "pauser()")
  , ("owner", "cfd_engine_admin", prCfdEngineAdmin release, "owner()")
  , ("pending_owner", "cfd_engine_admin", prCfdEngineAdmin release, "pendingOwner()")
  , ("owner", "house_pool", prHousePool release, "owner()")
  , ("pending_owner", "house_pool", prHousePool release, "pendingOwner()")
  , ("pauser", "house_pool", prHousePool release, "pauser()")
  , ("owner", "cfd_engine", prCfdEngine release, "owner()")
  , ("pending_owner", "cfd_engine", prCfdEngine release, "pendingOwner()")
  , ("protocol_treasury", "cfd_engine", prCfdEngine release, "protocolTreasury()")
  ]

observedOperationalRoles
  :: OperationalWalletActivityRow
  -> [OperationalWalletRoleSource]
observedOperationalRoles row =
  [ observed "observed_keeper"
  | owaExecutionCount row > 0
      || owaCleanupCount row > 0
      || owaMaintenanceCount row > 0
  ]
    <> [observed "observed_liquidator" | owaLiquidationCount row > 0]
    <> [observed "observed_governance_actor" | owaGovernanceCount row > 0]
  where
    observed role =
      OperationalWalletRoleSource
        { owrsAddress = T.toLower $ owaAddress row
        , owrsRole = role
        , owrsSource = "confirmed_successful_action_projection"
        , owrsSourceContract = Nothing
        , owrsSourceAddress = Nothing
        , owrsDescription =
            Just "Role inferred only from confirmed successful public protocol activity in the selected window."
        , owrsRepresentativeEvidence = Nothing
        , owrsEvidence = "exact_confirmed_observed_activity"
        }

qualifiesObservedOperationalWallet :: OperationalWalletActivityRow -> Bool
qualifiesObservedOperationalWallet =
  not . null . observedOperationalRoles

rolesForAddress
  :: Text
  -> [OperationalWalletRoleSource]
  -> [OperationalWalletRoleSource]
rolesForAddress address =
  sortOn owrsRole
    . nubBy
      (\left right ->
        owrsRole left == owrsRole right
          && owrsSource left == owrsSource right
          && owrsSourceContract left == owrsSourceContract right
      )
    . filter ((== T.toLower address) . T.toLower . owrsAddress)

operationalRoleNames :: [OperationalWalletRoleSource] -> [Text]
operationalRoleNames =
  sortOn id . nubBy (==) . map owrsRole

operationalTelemetryAttributable :: [OperationalWalletRoleSource] -> Bool
operationalTelemetryAttributable =
  any (/= "oracle_updater") . operationalRoleNames

operationalCostForAttributableRoles
  :: [OperationalWalletRoleSource]
  -> Maybe OperationalWalletCostRow
  -> Maybe OperationalWalletCostRow
operationalCostForAttributableRoles roleSources cost
  | operationalTelemetryAttributable roleSources = cost
  | otherwise = Nothing

operationalActivityEvidence :: [OperationalWalletRoleSource] -> Text
operationalActivityEvidence roleSources
  | not $ operationalTelemetryAttributable roleSources =
      "unavailable_oracle_updater_call_not_attributable_by_current_release_projection"
  | "oracle_updater" `elem` operationalRoleNames roleSources =
      "confirmed_operational_action_projection_oracle_updater_component_unavailable"
  | otherwise =
      "confirmed_successful_operational_action_projection"

operationalRoleTelemetryAvailability
  :: Text
  -> [OperationalWalletRoleSource]
  -> [Value]
operationalRoleTelemetryAvailability fieldPrefix roleSources
  | "oracle_updater" `elem` operationalRoleNames roleSources =
      [ unavailable
          (fieldPrefix <> ".oracleUpdater.activity")
          "oracle_updater_activity_not_attributable_current_release"
      , unavailable
          (fieldPrefix <> ".oracleUpdater.grossNativeSpend")
          "oracle_updater_gross_native_spend_not_attributable_current_release"
      , unavailable
          (fieldPrefix <> ".oracleUpdater.estimatedTransactionsAtObservedGrossSpend")
          "oracle_updater_gross_native_spend_not_attributable_current_release"
      ]
  | otherwise = []

operationalRoleSourceToJson
  :: ProtocolRelease
  -> OperationalWalletRoleSource
  -> Value
operationalRoleSourceToJson release OperationalWalletRoleSource {..} =
  object
    [ "role" .= owrsRole
    , "source" .= owrsSource
    , "sourceContract" .= owrsSourceContract
    , "sourceAddress" .= owrsSourceAddress
    , "description" .= owrsDescription
    , "representativeEvidence" .=
        fmap
          (\ProtocolOperationalWalletEvidence {..} ->
            object
              [ "selector" .= poweSelector
              , "transactionHash" .= poweTransactionHash
              , "blockNumber" .= show poweBlockNumber
              , "explorerUrl" .=
                  explorerTxUrl release poweTransactionHash
              , "interpretation" .=
                  ("Representative public transaction used to publish this permissionless operational identity; it is not proof of an onchain privilege." :: Text)
              ]
          )
          owrsRepresentativeEvidence
    , "evidence" .= owrsEvidence
    ]

findOperationalActivity
  :: Text
  -> [OperationalWalletActivityRow]
  -> Maybe OperationalWalletActivityRow
findOperationalActivity address =
  find ((== T.toLower address) . T.toLower . owaAddress)

findOperationalCost
  :: Text
  -> [OperationalWalletCostRow]
  -> Maybe OperationalWalletCostRow
findOperationalCost address =
  find ((== T.toLower address) . T.toLower . owcAddress)

isPublishedOperationalAddress :: Text -> Bool
isPublishedOperationalAddress address =
  isValidAddress address
    && T.toLower address
      /= "0x0000000000000000000000000000000000000000"

validOperationalWalletCursor :: Maybe ProtocolCursor -> Bool
validOperationalWalletCursor Nothing = True
validOperationalWalletCursor (Just cursor) =
  maybe False isPublishedOperationalAddress $ pcItemId cursor

operationalWalletCursor
  :: ProtocolRelease
  -> Text
  -> ConfirmedContext
  -> Text
  -> Maybe Text
operationalWalletCursor release scope context address
  | not $ isPublishedOperationalAddress address = Nothing
  | otherwise =
      Just $
        encodeProtocolCursor
          ProtocolCursor
            { pcReleaseId = prId release
            , pcScope = scope
            , pcConfirmedBlock = ccBlockNumber context
            , pcConfirmedBlockHash = ccBlockHash context
            , pcItemBlock = ccBlockNumber context
            , pcItemLogIndex = Just 0
            , pcItemId = Just $ T.toLower address
            }

operationalActionToJson
  :: ProtocolRelease
  -> ProtocolActionRow
  -> Maybe ProtocolTransactionRow
  -> Value
operationalActionToJson release action transaction =
  case actionToJson action of
    Object fields ->
      Object $
        KM.insert "explorerUrl" (String $ explorerTxUrl release $ parTxHash action) $
          KM.insert "transactionAvailability" (Aeson.toJSON txAvailability) $
            KM.insert "transactionEvidence" txEvidence $
              KM.insert "transactionNativeValueWei" nativeValue $
                KM.insert "gasCostWei" gasCost fields
    value -> value
  where
    gasCost =
      maybe Null
        (maybe Null (String . T.pack . show) . \row ->
          (*) <$> ptrGasUsed row <*> ptrEffectiveGasPrice row
        )
        transaction
    nativeValue =
      maybe Null (maybe Null (String . T.pack . show) . ptrNativeValue) transaction
    txEvidence = maybe Null ptrEvidence transaction
    txAvailability =
      maybe
        [unavailable "transaction" "protocol_transaction_projection_unavailable"]
        transactionAvailability
        transaction

getParametersResponse
  :: DbPool -> EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse Value))
getParametersResponse pool client cfg releaseId =
  withRelease cfg releaseId $ \release -> do
    context <- confirmedContext client cfg
    (indexedHead, rawChanges) <- withDb pool $ \conn -> do
      headRow <- getProtocolIndexedHead conn releaseId
      changes <- getParameterChanges conn releaseId (ccBlockNumber context) 500 Nothing
      pure (headRow, changes)
    parameterResults <- forM parameterCatalog $ \definition -> do
      let address = parameterContract release (pdContract definition)
      result <- case address of
        Nothing -> pure Nothing
        Just contract ->
          readUintAtExact
            client
            contract
            (pdGetter definition)
            (pdWordIndex definition)
            (parameterGetterWordCount definition)
            (ccBlockRef context)
      let availability =
            [unavailable (pdKey definition) "rpc_state_unavailable" | result == Nothing]
      pure
        ( object
            [ "definition" .= parameterDefinitionToJson definition
            , "rawValue" .= fmap show result
            , "formattedValue" .= fmap (formatParameter definition) result
            , "effectiveBlock" .= show (ccBlockNumber context)
            , "sourceAddress" .= address
            , "evidence" .= if result == Nothing then ("unavailable" :: Text) else "exact_historical_contract_read"
            , "availability" .= availability
            ]
        , availability
        )
    (governanceValues, directPending, governanceAvailability) <-
      readDirectGovernance client release context
    let changes = map (resolveParameterChangeStatus context) rawChanges
        values = map fst parameterResults
        parameterAvailability = concatMap snd parameterResults
        indexedPending = filter isPendingChange changes
        pending = mergePendingChanges directPending indexedPending
        currentEvidence =
          if null parameterAvailability
            then ("exact_historical_contract_reads_at_one_block" :: Text)
            else "partial_historical_contract_reads_at_one_block"
        governanceEvidence =
          if null governanceAvailability
            then ("strict_exact_historical_admin_getter_reads_at_same_block" :: Text)
            else
              if null governanceValues
                then "unavailable"
                else "partial_historical_admin_getter_reads_at_same_block"
        pendingEvidence =
          if not $ null governanceAvailability
            then ("partial_direct_admin_reads_at_same_confirmed_block" :: Text)
            else
              if null pending
                then "exact_direct_admin_reads_no_pending_changes"
                else "partial_direct_admin_reads_with_correlated_projection_metadata"
    pure $ Right $ protocolResponse release context indexedHead
      (object
        [ "current" .= currentEvidence
        , "governanceCurrent" .= governanceEvidence
        , "pending" .= pendingEvidence
        , "pendingProjectionCorrelation" .=
            if null changes
              then ("unavailable_no_projected_governance_actions" :: Text)
              else "confirmed_log_parameter_change_projection"
        , "housePoolCancellationHistory" .=
            ("unavailable_current_contract_does_not_emit_cancellation_event" :: Text)
        ])
      (ccAvailability context
        <> parameterAvailability
        <> governanceAvailability
        <> housePoolCancellationAvailability)
      "parameters"
      (object
        [ "current" .= (values <> governanceValues)
        , "pending" .= pending
        , "parameterChangesPath" .=
            ( "/api/insights/v1/protocol/releases/"
                <> prId release
                <> "/parameter-changes"
            )
        , "catalogVersion" .= ("protocol-parameters-v2" :: Text)
        ])

getParameterChangesResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Int
  -> Maybe ProtocolCursor
  -> IO (Either ApiError (ApiResponse Value))
getParameterChangesResponse pool client cfg releaseId requestedLimit cursor =
  withRelease cfg releaseId $ \release -> do
    let scope = "parameter-changes"
    contextResult <-
      resolveProjectionListContext pool client cfg release scope cursor
    case contextResult of
      Left err -> pure $ Left err
      Right listContext -> do
        let context = plcConfirmedContext listContext
        let pageLimit = max 1 $ min 500 requestedLimit
        rawChanges <- withDb pool $ \conn -> do
          rows <-
            getParameterChanges
              conn
              releaseId
              (ccBlockNumber context)
              (pageLimit + 1)
              (parameterCursorKey cursor)
          pure rows
        let visibleRaw = take pageLimit rawChanges
            changes = map (resolveParameterChangeStatus context) visibleRaw
            nextCursor =
              if length rawChanges > pageLimit
                then parameterChangeCursor release scope context =<< listToMaybe (reverse visibleRaw)
                else Nothing
            noProjection = null changes && cursor == Nothing
        pure $ Right $ projectionListResponse release listContext
          (object
            [ "changes" .=
                if noProjection
                  then ("unavailable_no_projected_governance_actions" :: Text)
                  else "confirmed_log_parameter_change_projection"
            ])
          ([ unavailable "parameterChanges" "no_governance_actions_projected_for_release"
               | noProjection
               ]
            <> housePoolCancellationAvailability)
          "parameterChanges"
          (object ["items" .= changes, "nextCursor" .= nextCursor])

-- HousePool.cancelPoolConfigProposal() clears the pending struct and ETA but
-- emits no event in the current contract. The immutable log ledger therefore
-- cannot claim complete cancellation history; direct pending reads still show
-- the authoritative current state.
housePoolCancellationAvailability :: [Value]
housePoolCancellationAvailability =
  [ unavailable
      "parameterChanges.house_pool_config.cancellations"
      "house_pool_cancellation_event_not_emitted"
  ]

readDirectGovernance ::
  EthClient ->
  ProtocolRelease ->
  ConfirmedContext ->
  IO ([Value], [Value], [Value])
readDirectGovernance client release context = do
  roleResults <-
    forM governanceHostRoles $ \role ->
      readGovernanceRole client release context role
  pendingResults <-
    forM governanceCategoryDefinitions $ \definition ->
      readGovernanceCategory client release context definition
  let currentValues = concatMap fst roleResults
      roleAvailability = concatMap snd roleResults
      pendingValues = concatMap fst pendingResults
      pendingAvailability = concatMap snd pendingResults
  pure (currentValues, pendingValues, roleAvailability <> pendingAvailability)

readGovernanceTimelockAnomalies
  :: EthClient
  -> ProtocolRelease
  -> ConfirmedContext
  -> IO ([Value], [Value])
readGovernanceTimelockAnomalies client release context = do
  results <-
    forM governanceCategoryDefinitions $ \definition -> do
      let categoryKey = governanceCategoryKey $ gcdCategory definition
          contractAddress =
            governanceContractAddress release $ gcdContractRole definition
          activationDefinition =
            GovernanceGetterDefinition
              { ggdKey = "governance.activation_time"
              , ggdPurpose = PolicyGetter
              , ggdFunction = gcdActivationGetter definition
              , ggdValueType = Uint256Field
              }
          availabilityField =
            "anomalies.pendingTimelock."
              <> categoryKey
              <> ".activationTime"
      result <-
        readGovernanceGetterAt
          client
          contractAddress
          (ccBlockRef context)
          activationDefinition
      pure $
        case result of
          Left reason ->
            ([], [unavailable availabilityField reason])
          Right (GovernanceUint 0) ->
            ([], [])
          Right (GovernanceUint eta)
            | ccBlockTimestamp context <= 0 ->
                ( []
                , [ unavailable
                      ("anomalies.pendingTimelock." <> categoryKey <> ".status")
                      "confirmed_block_timestamp_unavailable"
                  ]
                )
            | eta <= ccBlockTimestamp context ->
                let overdueSeconds = ccBlockTimestamp context - eta
                    isOverdue = overdueSeconds > 3_600
                    code =
                      if isOverdue
                        then "timelock_action_overdue:" <> categoryKey
                        else "timelock_action_ready:" <> categoryKey
                    message =
                      if isOverdue
                        then "A timelocked governance action has remained executable for more than one hour."
                        else "A timelocked governance action is ready for execution."
                 in ( [ anomaly
                          code
                          "warning"
                          message
                          (object
                            [ "category" .= categoryKey
                            , "eta" .= show eta
                            , "readyForSeconds" .= show overdueSeconds
                            , "sourceAddress" .= contractAddress
                            , "sourceGetter" .=
                                governanceFunctionSignature
                                  (gcdActivationGetter definition)
                            , "unit" .= ("seconds" :: Text)
                            ])
                      ]
                    , []
                    )
            | otherwise ->
                ([], [])
          Right _ ->
            ([], [unavailable availabilityField "malformed_governance_activation_type"])
  pure (concatMap fst results, concatMap snd results)

recentlyExecutedGovernanceAnomalies :: Integer -> [Value] -> [Value]
recentlyExecutedGovernanceAnomalies now =
  take 10 . catMaybes . map recentlyExecuted
  where
    recentlyExecuted change = do
      statusValue <- objectField "status" change
      case statusValue of
        String "executed" -> pure ()
        _ -> Nothing
      executedAt <-
        objectIntegerField "executedAt" change
          <|> objectIntegerField "terminalAt" change
      if executedAt > now || now - executedAt > 86_400
        then Nothing
        else
          let identity =
                fromMaybe
                  (T.pack $ show executedAt)
                  (jsonText =<< objectField "changeId" change)
           in
          Just $
            anomaly
              ("governance_action_recently_executed:" <> identity)
              "info"
              "A governance or dependency change was executed in the last 24 hours."
              (object
                [ "parameterKey" .= objectField "parameterKey" change
                , "executedAt" .= show executedAt
                , "transactionHash" .=
                    ( objectField "terminalTxHash" change
                        <|> objectField "txHash" change
                    )
                ])

    jsonText (String value) = Just value
    jsonText _ = Nothing

governanceHostRoles :: [GovernanceContractRole]
governanceHostRoles =
  [ OrderRouterAdminRole
  , CfdEngineAdminRole
  , HousePoolRole
  , OrderRouterRole
  , CfdEngineRole
  ]

readGovernanceRole ::
  EthClient ->
  ProtocolRelease ->
  ConfirmedContext ->
  GovernanceContractRole ->
  IO ([Value], [Value])
readGovernanceRole client release context role = do
  let contractAddress = governanceContractAddress release role
  rows <-
    forM (governanceRoleGetters role) $ \definition -> do
      result <-
        readGovernanceGetterAt
          client
          contractAddress
          (ccBlockRef context)
          definition
      let stableKey = governanceGetterStableKey role definition
          reason = either Just (const Nothing) result
          value = either (const Nothing) Just result
      pure
        ( governanceGetterToJson release context role contractAddress definition value reason
        , maybe [] (\readReason -> [unavailable stableKey readReason]) reason
        )
  pure (map fst rows, concatMap snd rows)

readGovernanceCategory ::
  EthClient ->
  ProtocolRelease ->
  ConfirmedContext ->
  GovernanceCategoryDefinition ->
  IO ([Value], [Value])
readGovernanceCategory client release context definition = do
  let contractAddress =
        governanceContractAddress release (gcdContractRole definition)
      activationDefinition =
        GovernanceGetterDefinition
          { ggdKey = "governance.activation_time"
          , ggdPurpose = PolicyGetter
          , ggdFunction = gcdActivationGetter definition
          , ggdValueType = Uint256Field
          }
      activationField =
        "pending."
          <> governanceCategoryKey (gcdCategory definition)
          <> ".activationTime"
  activationResult <-
    readGovernanceGetterAt
      client
      contractAddress
      (ccBlockRef context)
      activationDefinition
  case activationResult of
    Left reason ->
      pure ([], [unavailable activationField reason])
    Right (GovernanceUint 0) ->
      -- Pending structs retain old bytes after cancellation/finalization. The
      -- activation slot is the canonical signal and must win over stale data.
      pure ([], [])
    Right (GovernanceUint eta) ->
      case gcdPendingEncoding definition of
        UnsupportedDynamicGovernancePayload reason -> do
          let rows =
                map
                  ( pendingGovernanceUnavailableToJson
                      release
                      context
                      definition
                      contractAddress
                      eta
                      reason
                  )
                  (gcdFields definition)
              fieldName =
                "pending."
                  <> governanceCategoryKey (gcdCategory definition)
                  <> ".values"
          pure (rows, [unavailable fieldName reason])
        StaticGovernanceWords _ -> do
          payloadResult <-
            readGovernanceFunctionAt
              client
              contractAddress
              (ccBlockRef context)
              (gcdPendingGetter definition)
          case payloadResult >>= firstGovernanceDecode . decodePendingGovernance definition of
            Left reason -> do
              let rows =
                    map
                      ( pendingGovernanceUnavailableToJson
                          release
                          context
                          definition
                          contractAddress
                          eta
                          reason
                      )
                      (gcdFields definition)
                  fieldName =
                    "pending."
                      <> governanceCategoryKey (gcdCategory definition)
                      <> ".values"
              pure (rows, [unavailable fieldName reason])
            Right fields ->
              pure
                ( map
                    (pendingGovernanceFieldToJson release context definition contractAddress eta)
                    fields
                , []
                )
    Right _ ->
      pure ([], [unavailable activationField "malformed_governance_activation_type"])

readGovernanceGetterAt ::
  EthClient ->
  Text ->
  Maybe CanonicalBlockRef ->
  GovernanceGetterDefinition ->
  IO (Either Text GovernanceDecodedValue)
readGovernanceGetterAt client contractAddress blockRef definition = do
  payload <-
    readGovernanceFunctionAt
      client
      contractAddress
      blockRef
      (ggdFunction definition)
  pure $ payload >>= firstGovernanceDecode . decodeGovernanceGetter definition

readGovernanceFunctionAt ::
  EthClient ->
  Text ->
  Maybe CanonicalBlockRef ->
  GovernanceFunction ->
  IO (Either Text BS.ByteString)
readGovernanceFunctionAt client contractAddress blockRef function =
  case blockRef of
    Nothing -> pure $ Left "canonical_block_anchor_unavailable"
    Just canonicalRef -> do
      result <-
        ethCallAtCanonicalBlock
          client
          (CallParams contractAddress $ governanceFunctionSelector function)
          canonicalRef
      pure $ case result of
        Left _ -> Left "canonical_block_state_unavailable"
        Right payload -> Right payload

firstGovernanceDecode ::
  Either GovernanceDecodeError a ->
  Either Text a
firstGovernanceDecode =
  either (Left . governanceDecodeReason) Right

governanceDecodeReason :: GovernanceDecodeError -> Text
governanceDecodeReason = \case
  GovernanceDynamicPayloadUnavailable reason -> reason
  GovernancePayloadLengthMismatch {} -> "malformed_abi_return_word_count"
  GovernanceSchemaWordCountMismatch {} -> "analytics_governance_schema_mismatch"
  GovernanceNonCanonicalAddress {} -> "noncanonical_abi_address"
  GovernanceInvalidBool {} -> "noncanonical_abi_bool"
  GovernanceUnsupportedStaticField {} -> "unsupported_governance_field_type"
  GovernanceUnknownEventTopic {} -> "unknown_governance_event_topic"

governanceGetterToJson ::
  ProtocolRelease ->
  ConfirmedContext ->
  GovernanceContractRole ->
  Text ->
  GovernanceGetterDefinition ->
  Maybe GovernanceDecodedValue ->
  Maybe Text ->
  Value
governanceGetterToJson release context role contractAddress definition result reason =
  let stableKey = governanceGetterStableKey role definition
      (rawScale, displayUnit) = governanceGetterScaleAndUnit definition
   in object
        [ "definition" .= object
            [ "key" .= stableKey
            , "baseKey" .= ggdKey definition
            , "group" .= governanceGetterGroup (ggdPurpose definition)
            , "sourceContract" .= governanceContractRoleKey role
            , "getter" .= governanceFunctionSignature (ggdFunction definition)
            , "rawScale" .= rawScale
            , "displayUnit" .= displayUnit
            , "description" .= governanceGetterDescription definition
            , "riskInterpretation" .= governanceGetterRiskInterpretation definition
            , "mutability" .= governanceGetterMutability definition
            , "timelockPolicy" .= governanceGetterTimelockPolicy definition
            , "documentationLink" .= ("/methodology#protocol-parameters" :: Text)
            ]
        , "rawValue" .= fmap governanceDecodedValueText result
        , "formattedValue" .= fmap governanceDecodedValueText result
        , "effectiveBlock" .= show (ccBlockNumber context)
        , "sourceAddress" .= contractAddress
        , "evidence" .=
            if result == Nothing
              then ("unavailable" :: Text)
              else "exact_historical_contract_read"
        , "evidenceDetail" .= governanceReadEvidence
            release
            context
            contractAddress
            (ggdFunction definition)
            (if result == Nothing then "unavailable" else "exact")
        , "availability" .= maybe [] (\readReason -> [unavailable stableKey readReason]) reason
        ]

pendingGovernanceFieldToJson ::
  ProtocolRelease ->
  ConfirmedContext ->
  GovernanceCategoryDefinition ->
  Text ->
  Integer ->
  DecodedGovernanceField ->
  Value
pendingGovernanceFieldToJson release context definition contractAddress eta decoded =
  let field = dgfDefinition decoded
      value = dgfValue decoded
   in pendingGovernanceToJson
        release
        context
        definition
        contractAddress
        eta
        field
        (Just value)
        Nothing

pendingGovernanceUnavailableToJson ::
  ProtocolRelease ->
  ConfirmedContext ->
  GovernanceCategoryDefinition ->
  Text ->
  Integer ->
  Text ->
  GovernanceField ->
  Value
pendingGovernanceUnavailableToJson release context definition contractAddress eta reason field =
  pendingGovernanceToJson
    release
    context
    definition
    contractAddress
    eta
    field
    Nothing
    (Just reason)

pendingGovernanceToJson ::
  ProtocolRelease ->
  ConfirmedContext ->
  GovernanceCategoryDefinition ->
  Text ->
  Integer ->
  GovernanceField ->
  Maybe GovernanceDecodedValue ->
  Maybe Text ->
  Value
pendingGovernanceToJson release context definition contractAddress eta field pendingValue valueReason =
  let categoryKey = governanceCategoryKey (gcdCategory definition)
      status = governancePendingStatus context eta
      statusAvailability =
        [ unavailable fieldName "confirmed_block_timestamp_unavailable"
        | ccBlockTimestamp context <= 0
        , fieldName <- ["status", "countdownSeconds"]
        ]
      valueAvailability =
        maybe [] (\reason -> [unavailable "newValue" reason]) valueReason
      countdown =
        if ccBlockTimestamp context <= 0
          then Nothing
          else Just $ max 0 $ eta - ccBlockTimestamp context
      stableChangeId =
        "direct:"
          <> categoryKey
          <> ":"
          <> gfKey field
          <> ":"
          <> T.pack (show eta)
   in object
        [ "changeId" .= stableChangeId
        , "parameterKey" .= gfKey field
        , "category" .= categoryKey
        , "status" .= status
        , "oldValue" .= Null
        , "newValue" .= fmap governanceDecodedValueJson pendingValue
        , "rawValue" .= fmap governanceDecodedValueText pendingValue
        , "formattedValue" .= fmap governanceDecodedValueText pendingValue
        , "rawScale" .= gfScale field
        , "unit" .= gfUnit field
        , "valueType" .= governanceFieldTypeKey (gfType field)
        , "proposer" .= Null
        , "executor" .= Null
        , "proposedAt" .= Null
        , "eta" .= show eta
        , "countdownSeconds" .= fmap show countdown
        , "executedAt" .= Null
        , "txHash" .= Null
        , "blockNumber" .= show (ccBlockNumber context)
        , "sourceContract" .= governanceContractRoleKey (gcdContractRole definition)
        , "sourceAddress" .= contractAddress
        , "sourceGetter" .= governanceFunctionSignature (gcdPendingGetter definition)
        , "activationGetter" .= governanceFunctionSignature (gcdActivationGetter definition)
        , "calculationVersion" .= prCalculationVersion release
        , "units" .= object
            [ "eta" .= ("unix_seconds" :: Text)
            , "countdownSeconds" .= ("seconds" :: Text)
            ]
        , "evidence" .= object
            [ "level" .= ("partial" :: Text)
            , "activationTime" .= ("exact_historical_contract_read" :: Text)
            , "pendingValue" .=
                if pendingValue == Nothing
                  then ("unavailable" :: Text)
                  else "exact_historical_contract_read"
            , "oldValue" .= ("unavailable" :: Text)
            , "proposer" .= ("unavailable" :: Text)
            , "proposedAt" .= ("unavailable" :: Text)
            , "txHash" .= ("unavailable" :: Text)
            , "activationRead" .= governanceReadEvidence
                release
                context
                contractAddress
                (gcdActivationGetter definition)
                "exact"
            , "pendingRead" .= governanceReadEvidence
                release
                context
                contractAddress
                (gcdPendingGetter definition)
                (if pendingValue == Nothing then "unavailable" else "exact")
            ]
        , "availability" .=
            ( [ unavailable "oldValue" "prior_applied_value_not_reconstructed"
              , unavailable "proposer" "proposal_transaction_not_correlated"
              , unavailable "proposedAt" "proposal_transaction_not_correlated"
              , unavailable "txHash" "proposal_transaction_not_correlated"
              ]
                <> statusAvailability
                <> valueAvailability
            )
        ]

governanceReadEvidence ::
  ProtocolRelease ->
  ConfirmedContext ->
  Text ->
  GovernanceFunction ->
  Text ->
  Value
governanceReadEvidence release context contractAddress function level =
  object
    [ "level" .= level
    , "releaseId" .= prId release
    , "contractAddress" .= contractAddress
    , "getter" .= governanceFunctionSignature function
    , "selector" .=
        ("0x" <> TE.decodeUtf8 (B16.encode $ governanceFunctionSelector function))
    , "sourceBlock" .= object
        [ "number" .= show (ccBlockNumber context)
        , "hash" .= ccBlockHash context
        , "timestamp" .= ccBlockTimestamp context
        ]
    ]

governanceDecodedValueJson :: GovernanceDecodedValue -> Value
governanceDecodedValueJson = \case
  GovernanceUint value -> String $ T.pack $ show value
  GovernanceAddress value -> String value
  GovernanceBool value -> Bool value

governanceDecodedValueText :: GovernanceDecodedValue -> Text
governanceDecodedValueText = \case
  GovernanceUint value -> T.pack $ show value
  GovernanceAddress value -> value
  GovernanceBool True -> "true"
  GovernanceBool False -> "false"

governancePendingStatus :: ConfirmedContext -> Integer -> Text
governancePendingStatus context eta =
  case governancePendingState confirmedTimestamp eta of
    NoPendingGovernance -> "none"
    PendingGovernance -> "pending"
    ReadyGovernance -> "ready"
    PendingGovernanceTimestampUnavailable -> "unknown"
  where
    confirmedTimestamp =
      if ccBlockTimestamp context <= 0
        then Nothing
        else Just $ ccBlockTimestamp context

mergePendingChanges :: [Value] -> [Value] -> [Value]
mergePendingChanges direct indexed =
  map enrichDirect direct
    <> filter
      (\change ->
        maybe True (`notElem` directKeys) $ changeParameterKey change
      )
      indexed
  where
    directKeys = catMaybes $ map changeParameterKey direct
    enrichDirect directChange =
      case matchingIndexedChange directChange of
        Nothing -> directChange
        Just indexedChange -> enrichDirectPendingChange directChange indexedChange

    matchingIndexedChange directChange = do
      key <- changeParameterKey directChange
      let candidates =
            filter ((== Just key) . changeParameterKey) indexed
          sameEta candidate =
            case
                ( objectIntegerField "eta" directChange
                , objectIntegerField "eta" candidate
                )
              of
              (Just directEta, Just indexedEta) -> directEta == indexedEta
              _ -> False
      find sameEta candidates

enrichDirectPendingChange :: Value -> Value -> Value
enrichDirectPendingChange (Object directFields) (Object indexedFields) =
  let copyableFields =
        [ "oldValue"
        , "proposer"
        , "proposedAt"
        , "txHash"
        , "proposalTxHash"
        , "proposalBlockNumber"
        , "sourceActionId"
        ]
      enrichedFields = foldl copyIfMissing directFields copyableFields
      resolvedAvailabilityFields =
        filter (not . valueMissing . (`KM.lookup` enrichedFields) . Key.fromText)
          ["oldValue", "proposer", "proposedAt", "txHash"]
      cleanedAvailability =
        case KM.lookup (Key.fromText "availability") enrichedFields of
          Just (Array entries) ->
            Aeson.toJSON $
              filter
                (\entry ->
                  maybe True (`notElem` resolvedAvailabilityFields) $
                    availabilityFieldName entry
                )
                (toList entries)
          value -> fromMaybe (Aeson.toJSON ([] :: [Value])) value
      enrichedEvidence =
        let baseEvidence =
              case KM.lookup (Key.fromText "evidence") enrichedFields of
                Just (Object fields) -> fields
                _ -> KM.empty
            withLevel =
              KM.insert
                (Key.fromText "level")
                (String "mixed_exact_and_derived")
                baseEvidence
         in Object $
              foldl
                (\fields fieldName ->
                  KM.insert
                    (Key.fromText fieldName)
                    (String "derived_from_key_eta_correlated_confirmed_log_projection")
                    fields
                )
                withLevel
                resolvedAvailabilityFields
      correlationEvidence =
        object
          [ "level" .= ("derived" :: Text)
          , "formulaIdentifier" .=
              ("governance.pending.direct_indexed_correlation.v1" :: Text)
          , "matchedOn" .= ("parameter_key_and_eta" :: Text)
          , "indexedChangeId" .= KM.lookup (Key.fromText "changeId") indexedFields
          , "indexedEvidence" .= KM.lookup (Key.fromText "evidence") indexedFields
          ]
   in Object $
        KM.insert
          (Key.fromText "correlationEvidence")
          correlationEvidence
          ( KM.insert
              (Key.fromText "availability")
              cleanedAvailability
              ( KM.insert
                  (Key.fromText "evidence")
                  enrichedEvidence
                  enrichedFields
              )
          )
  where
    copyIfMissing fields keyName =
      let key = Key.fromText keyName
       in case (KM.lookup key fields, KM.lookup key indexedFields) of
            (directValue, Just indexedValue)
              | valueMissing directValue
              , not (valueMissing $ Just indexedValue) ->
                  KM.insert key indexedValue fields
            _ -> fields
enrichDirectPendingChange directChange _ = directChange

valueMissing :: Maybe Value -> Bool
valueMissing Nothing = True
valueMissing (Just Null) = True
valueMissing _ = False

availabilityFieldName :: Value -> Maybe Text
availabilityFieldName (Object fields) =
  case KM.lookup (Key.fromText "field") fields of
    Just (String fieldName) -> Just fieldName
    _ -> Nothing
availabilityFieldName _ = Nothing

changeParameterKey :: Value -> Maybe Text
changeParameterKey (Object fields) =
  case KM.lookup (Key.fromText "parameterKey") fields of
    Just (String key) -> Just key
    _ -> Nothing
changeParameterKey _ = Nothing

governanceGetterStableKey ::
  GovernanceContractRole ->
  GovernanceGetterDefinition ->
  Text
governanceGetterStableKey role definition =
  qualifyGovernanceKey
    (governanceContractRoleKey role)
    (ggdKey definition)

governanceCategoryKey :: GovernanceCategory -> Text
governanceCategoryKey = \case
  RouterConfigCategory -> "router_config"
  OracleConfigCategory -> "oracle_config"
  EngineRiskConfigCategory -> "engine_risk_config"
  EngineCalendarConfigCategory -> "engine_calendar_config"
  EngineFreshnessConfigCategory -> "engine_freshness_config"
  HousePoolConfigCategory -> "house_pool_config"

governanceFieldTypeKey :: GovernanceFieldType -> Text
governanceFieldTypeKey = \case
  Uint256Field -> "uint256"
  AddressField -> "address"
  BoolField -> "bool"
  Uint256ArrayField -> "uint256[]"

governanceGetterScaleAndUnit ::
  GovernanceGetterDefinition ->
  (Text, Text)
governanceGetterScaleAndUnit definition =
  case ggdValueType definition of
    AddressField -> ("address", "address")
    BoolField -> ("boolean", "boolean")
    Uint256ArrayField -> ("1", "values")
    Uint256Field
      | ggdKey definition == "governance.timelock_delay" -> ("1", "seconds")
      | otherwise -> ("1", "integer")

governanceGetterGroup :: GovernanceGetterPurpose -> Text
governanceGetterGroup = \case
  RoleGetter -> "Roles and dependencies"
  StatusGetter -> "Roles and dependencies"
  BindingGetter -> "Roles and dependencies"
  PolicyGetter -> "Governance policy"

governanceGetterDescription :: GovernanceGetterDefinition -> Text
governanceGetterDescription definition =
  case ggdPurpose definition of
    RoleGetter -> "Current governance role address on the release-scoped host contract."
    StatusGetter -> "Current emergency or operational status on the release-scoped host contract."
    BindingGetter -> "Current dependency address bound to the release-scoped host contract."
    PolicyGetter -> "Current immutable or governance policy value exposed by the host contract."

governanceGetterRiskInterpretation :: GovernanceGetterDefinition -> Text
governanceGetterRiskInterpretation definition
  | ggdKey definition == "governance.owner" =
      "Controls privileged governance operations; unexpected changes require immediate review."
  | ggdKey definition == "governance.pending_owner" =
      "A non-zero value identifies an ownership transfer that can still be accepted."
  | ggdKey definition == "governance.pauser" =
      "Can invoke emergency pause controls for this protocol component."
  | ggdKey definition == "governance.paused" =
      "A true value restricts normal protocol operation for this component."
  | ggdKey definition == "governance.timelock_delay" =
      "Higher values increase review time and slow legitimate governance response."
  | ggdPurpose definition == BindingGetter =
      "An unexpected address can redirect trusted protocol calls and should be treated as critical."
  | otherwise =
      "Review changes against the documented release and applicable risk policy."

withRelease
  :: Config
  -> Text
  -> (ProtocolRelease -> IO (Either ApiError (ApiResponse Value)))
  -> IO (Either ApiError (ApiResponse Value))
withRelease cfg requested action =
  case protocolReleaseById cfg requested of
    Just release -> action release
    Nothing ->
      pure $
        Left $
          E.notFound
            "This API instance does not serve the requested protocol release"

encodeProtocolCursor :: ProtocolCursor -> Text
encodeProtocolCursor cursor =
  "pc2_"
    <> TE.decodeUtf8
      (B16.encode $ LBS.toStrict $ Aeson.encode cursor)

encodeTrancheHistoryCursor :: TrancheHistoryCursor -> Text
encodeTrancheHistoryCursor cursor =
  "th1_"
    <> TE.decodeUtf8
      (B16.encode $ LBS.toStrict $ Aeson.encode cursor)

decodeProtocolCursor :: Text -> Maybe ProtocolCursor
decodeProtocolCursor rawCursor = do
  encoded <- T.stripPrefix "pc2_" $ T.strip rawCursor
  bytes <- either (const Nothing) Just $ B16.decode $ TE.encodeUtf8 encoded
  cursor <- Aeson.decodeStrict' bytes
  if validProtocolCursor cursor then Just cursor else Nothing

decodeTrancheHistoryCursor :: Text -> Maybe TrancheHistoryCursor
decodeTrancheHistoryCursor rawCursor =
  decodeCompound <|> decodeLegacyActionCursor
  where
    decodeCompound = do
      encoded <- T.stripPrefix "th1_" $ T.strip rawCursor
      bytes <- either (const Nothing) Just $ B16.decode $ TE.encodeUtf8 encoded
      cursor <- Aeson.decodeStrict' bytes
      if validTrancheHistoryCursor cursor then Just cursor else Nothing
    decodeLegacyActionCursor = do
      legacy <- decodeProtocolCursor rawCursor
      if "tranche-history:" `T.isPrefixOf` pcScope legacy
          && validActionCursor (Just legacy)
        then
          Just
            TrancheHistoryCursor
              { thcReleaseId = pcReleaseId legacy
              , thcScope = pcScope legacy
              , thcConfirmedBlock = pcConfirmedBlock legacy
              , thcConfirmedBlockHash = pcConfirmedBlockHash legacy
              , thcActionBlock = Just $ pcItemBlock legacy
              , thcActionLogIndex = pcItemLogIndex legacy
              , thcActionId = pcItemId legacy
              , thcActionsComplete = False
              , thcCheckpointBlock = Nothing
              , thcCheckpointsComplete = True
              , thcCheckpointContinuationUnavailable = True
              }
        else Nothing

parseDecimalCursorField :: MonadFail m => Text -> m Integer
parseDecimalCursorField raw =
  if T.null raw || not (T.all (\char -> char >= '0' && char <= '9') raw)
    then fail "invalid protocol cursor integer"
    else
      case readMaybe $ T.unpack raw of
        Just value
          | value >= 0 && value <= maxDatabaseBigInt -> pure value
        _ -> fail "invalid protocol cursor integer"

validProtocolCursor :: ProtocolCursor -> Bool
validProtocolCursor ProtocolCursor {..} =
  not (T.null pcReleaseId)
    && T.length pcReleaseId <= 256
    && not (T.null pcScope)
    && T.length pcScope <= 4_096
    && pcConfirmedBlock >= 0
    && pcConfirmedBlock <= maxDatabaseBigInt
    && isCanonicalBlockHash pcConfirmedBlockHash
    && pcItemBlock >= 0
    && pcItemBlock <= pcConfirmedBlock
    && pcItemBlock <= maxDatabaseBigInt
    && maybe True (\value -> value >= 0 && value <= maxDatabaseBigInt) pcItemLogIndex
    && maybe True (\value -> not (T.null value) && T.length value <= 1_024) pcItemId

validTrancheHistoryCursor :: TrancheHistoryCursor -> Bool
validTrancheHistoryCursor TrancheHistoryCursor {..} =
  not (T.null thcReleaseId)
    && T.length thcReleaseId <= 256
    && "tranche-history:" `T.isPrefixOf` thcScope
    && T.length thcScope <= 4_096
    && thcConfirmedBlock >= 0
    && thcConfirmedBlock <= maxDatabaseBigInt
    && isCanonicalBlockHash thcConfirmedBlockHash
    && validActionPosition
    && validCheckpointPosition
    && not (thcActionsComplete && thcCheckpointsComplete)
  where
    validActionPosition =
      if thcActionsComplete
        then
          thcActionBlock == Nothing
            && thcActionLogIndex == Nothing
            && thcActionId == Nothing
        else
          case (thcActionBlock, thcActionLogIndex, thcActionId) of
            (Just blockNumber, Just logIndex, Just actionId) ->
              blockNumber >= 0
                && blockNumber <= thcConfirmedBlock
                && blockNumber <= maxDatabaseBigInt
                && logIndex >= 0
                && logIndex <= maxDatabaseBigInt
                && not (T.null actionId)
                && T.length actionId <= 1_024
            _ -> False
    validCheckpointPosition =
      if thcCheckpointsComplete
        then thcCheckpointBlock == Nothing
        else
          case thcCheckpointBlock of
            Just blockNumber ->
              blockNumber >= 0
                && blockNumber <= thcConfirmedBlock
                && blockNumber <= maxDatabaseBigInt
            Nothing -> False

isCanonicalBlockHash :: Text -> Bool
isCanonicalBlockHash value =
  T.length value == 66
    && T.take 2 value == "0x"
    && T.all isHexDigit (T.drop 2 value)

protocolProjectionIndexerName :: ProtocolRelease -> Text
protocolProjectionIndexerName release =
  perpsIndexerNameForRelease
    (prChainId release)
    (prOrderRouter release)
    (prOrderLifecycleBook release)

-- | Choose the newest block for which both the chain-confirmation policy and
-- the contiguous immutable-ledger cursor can make a completeness claim.
-- A projection that has not reached the release deployment block is not a
-- usable list source.
selectProjectionListAnchor
  :: Integer
  -> Integer
  -> Integer
  -> Maybe Integer
selectProjectionListAnchor deploymentBlock chainConfirmedBlock indexedBlock
  | deploymentBlock < 0 = Nothing
  | chainConfirmedBlock < deploymentBlock = Nothing
  | indexedBlock < deploymentBlock = Nothing
  | otherwise = Just $ min chainConfirmedBlock indexedBlock

resolveProjectionListContext
  :: DbPool
  -> EthClient
  -> Config
  -> ProtocolRelease
  -> Text
  -> Maybe ProtocolCursor
  -> IO (Either ApiError ProjectionListContext)
resolveProjectionListContext pool client cfg release expectedScope cursor =
  case validateProjectionListCursor release expectedScope cursor of
    Left err -> pure $ Left err
    Right () -> do
      chainContext <- confirmedContext client cfg
      if not $ isCanonicalBlockHash $ ccBlockHash chainContext
        then
          pure $
            Left $
              E.internalError "Confirmed block anchor is unavailable"
        else do
          indexedHead <-
            withDb pool $ \conn ->
              getProtocolProjectionHead
                conn
                (prId release)
                (protocolProjectionIndexerName release)
          case indexedHead of
            Nothing ->
              pure $
                Left $
                  E.internalError "Protocol projection is not ready"
            Just headRow
              | not $
                  usableProjectionHead
                    (prDeploymentBlock release)
                    headRow ->
                  pure $
                    Left $
                      E.internalError "Protocol projection head is unavailable"
              | otherwise ->
                  case cursor of
                    Nothing ->
                      resolveFirstProjectionListContext
                        client
                        release
                        chainContext
                        headRow
                    Just position ->
                      resolveContinuedProjectionListContext
                        client
                        chainContext
                        headRow
                        position
  where
    usableProjectionHead deploymentBlock (blockNumber, blockHash, timestamp) =
      blockNumber >= deploymentBlock
        && blockNumber >= 0
        && blockNumber <= maxDatabaseBigInt
        && isCanonicalBlockHash (T.toLower blockHash)
        && timestamp >= 0

resolveFirstProjectionListContext
  :: EthClient
  -> ProtocolRelease
  -> ConfirmedContext
  -> ProtocolIndexedHead
  -> IO (Either ApiError ProjectionListContext)
resolveFirstProjectionListContext client release chainContext indexedHead@(indexedBlock, indexedHash, _) =
  case
      selectProjectionListAnchor
        (prDeploymentBlock release)
        (ccBlockNumber chainContext)
        indexedBlock
    of
      Nothing ->
        pure $
          Left $
            E.internalError "Protocol projection has no confirmed coverage"
      Just anchorBlock -> do
        anchorResult <-
          if anchorBlock == ccBlockNumber chainContext
            then pure $ Right chainContext
            else canonicalContextAt client anchorBlock
        pure $ case anchorResult of
          Left _ ->
            Left $
              E.internalError "Protocol projection anchor is unavailable"
          Right anchorContext
            | anchorBlock == indexedBlock
                && T.toLower indexedHash /= ccBlockHash anchorContext ->
                Left $
                  E.internalError "Protocol projection head is not canonical"
            | otherwise ->
                Right $
                  projectionListContext
                    False
                    chainContext
                    indexedHead
                    anchorContext

resolveContinuedProjectionListContext
  :: EthClient
  -> ConfirmedContext
  -> ProtocolIndexedHead
  -> ProtocolCursor
  -> IO (Either ApiError ProjectionListContext)
resolveContinuedProjectionListContext client chainContext indexedHead@(indexedBlock, indexedHash, _) cursor
  | pcConfirmedBlock cursor > ccBlockNumber chainContext =
      pure $
        Left $
          E.internalError "Confirmed chain head does not cover the cursor anchor"
  | indexedBlock < pcConfirmedBlock cursor =
      pure $
        Left $
          E.internalError "Protocol projection no longer covers the cursor anchor"
  | otherwise = do
      anchorResult <- canonicalContextAt client $ pcConfirmedBlock cursor
      pure $ case anchorResult of
        Left _ ->
          Left $ E.invalidAmount "cursor anchor block is unavailable"
        Right anchorContext
          | ccBlockHash anchorContext
              /= T.toLower (pcConfirmedBlockHash cursor) ->
              Left $ E.invalidAmount "cursor anchor is no longer canonical"
          | indexedBlock == pcConfirmedBlock cursor
              && T.toLower indexedHash /= ccBlockHash anchorContext ->
              Left $
                E.internalError "Protocol projection head is not canonical"
          | otherwise ->
              Right $
                projectionListContext
                  True
                  chainContext
                  indexedHead
                  anchorContext

projectionListContext
  :: Bool
  -> ConfirmedContext
  -> ProtocolIndexedHead
  -> ConfirmedContext
  -> ProjectionListContext
projectionListContext reusedCursor chainContext indexedHead@(indexedBlock, _, _) anchorContext =
  ProjectionListContext
    { plcConfirmedContext = anchorContext
    , plcChainConfirmedBlock = ccBlockNumber chainContext
    , plcIndexedHead = indexedHead
    , plcCursorAnchorReused = reusedCursor
    , plcAvailability =
        [ object
            [ "field" .= ("projectionCoverage" :: Text)
            , "reason" .=
                ("protocol_indexer_behind_chain_confirmed_head" :: Text)
            , "indexerLagBlocks" .=
                show (ccBlockNumber chainContext - indexedBlock)
            , "chainConfirmedBlock" .= show (ccBlockNumber chainContext)
            , "contiguousIndexerBlock" .= show indexedBlock
            ]
        | indexedBlock < ccBlockNumber chainContext
        ]
    }

validateProjectionListCursor
  :: ProtocolRelease
  -> Text
  -> Maybe ProtocolCursor
  -> Either ApiError ()
validateProjectionListCursor _ _ Nothing = Right ()
validateProjectionListCursor release expectedScope (Just cursor)
  | pcReleaseId cursor /= prId release =
      Left $ E.invalidAmount "cursor belongs to a different protocol release"
  | pcScope cursor /= expectedScope =
      Left $ E.invalidAmount "cursor does not match this list or its filters"
  | expectedScope == "parameter-changes" && pcItemId cursor == Nothing =
      Left $
        E.invalidAmount "cursor does not contain a parameter-change position"
  | expectedScope /= "parameter-changes"
      && not (validActionCursor $ Just cursor) =
      Left $ E.invalidAmount "cursor does not contain an activity position"
  | otherwise = Right ()

canonicalContextAt
  :: EthClient
  -> Integer
  -> IO (Either Text ConfirmedContext)
canonicalContextAt client blockNumber = do
  blockResult <- ethGetBlockByNumber client blockNumber
  canonicalResult <- ethGetCanonicalBlockRef client blockNumber
  pure $ case (blockResult, canonicalResult) of
    (Right block, Right blockRef)
      | rpcBlockNumber block == blockNumber
          && canonicalBlockNumber blockRef == blockNumber
          && T.toLower (rpcBlockHash block)
            == canonicalBlockHash blockRef ->
          Right
            ConfirmedContext
              { ccBlockNumber = canonicalBlockNumber blockRef
              , ccBlockHash = canonicalBlockHash blockRef
              , ccBlockTimestamp = rpcBlockTimestamp block
              , ccBlockRef = Just blockRef
              , ccAvailability = []
              }
      | otherwise -> Left "canonical_block_anchor_mismatch"
    _ -> Left "canonical_block_anchor_unavailable"

resolveTrancheHistoryContext
  :: DbPool
  -> EthClient
  -> Config
  -> ProtocolRelease
  -> Text
  -> Maybe TrancheHistoryCursor
  -> IO (Either ApiError ProjectionListContext)
resolveTrancheHistoryContext pool client cfg release expectedScope = \case
  Nothing ->
    resolveProjectionListContext
      pool
      client
      cfg
      release
      expectedScope
      Nothing
  Just cursor
    | thcReleaseId cursor /= prId release ->
        pure $ Left $ E.invalidAmount "cursor belongs to a different protocol release"
    | thcScope cursor /= expectedScope ->
        pure $ Left $ E.invalidAmount "cursor does not match this tranche history"
    | otherwise ->
        resolveProjectionListContext
          pool
          client
          cfg
          release
          expectedScope
          (Just $ trancheHistoryAnchorCursor cursor)

trancheHistoryAnchorCursor :: TrancheHistoryCursor -> ProtocolCursor
trancheHistoryAnchorCursor cursor =
  ProtocolCursor
    { pcReleaseId = thcReleaseId cursor
    , pcScope = thcScope cursor
    , pcConfirmedBlock = thcConfirmedBlock cursor
    , pcConfirmedBlockHash = thcConfirmedBlockHash cursor
    , pcItemBlock =
        fromMaybe
          (fromMaybe (thcConfirmedBlock cursor) $ thcCheckpointBlock cursor)
          (thcActionBlock cursor)
    , pcItemLogIndex = Just $ fromMaybe 0 $ thcActionLogIndex cursor
    , pcItemId = Just $ fromMaybe "checkpoint-position" $ thcActionId cursor
    }

actionCursor
  :: ProtocolRelease
  -> Text
  -> ConfirmedContext
  -> ProtocolActionRow
  -> Maybe Text
actionCursor release scope context row
  | not $ isCanonicalBlockHash $ ccBlockHash context = Nothing
  | otherwise =
      Just $
        encodeProtocolCursor
          ProtocolCursor
            { pcReleaseId = prId release
            , pcScope = scope
            , pcConfirmedBlock = ccBlockNumber context
            , pcConfirmedBlockHash = ccBlockHash context
            , pcItemBlock = parBlockNumber row
            , pcItemLogIndex = Just $ parLogIndex row
            , pcItemId = Just $ parActionId row
            }

trancheHistoryCursor
  :: ProtocolRelease
  -> Text
  -> ConfirmedContext
  -> Bool
  -> Maybe ProtocolActionRow
  -> Maybe ProtocolStateSnapshotRow
  -> Maybe Text
trancheHistoryCursor release scope context checkpointContinuationUnavailable actionPosition checkpointPosition
  | not $ isCanonicalBlockHash $ ccBlockHash context = Nothing
  | noPosition actionPosition && noPosition checkpointPosition = Nothing
  | otherwise =
      Just $
        encodeTrancheHistoryCursor
          TrancheHistoryCursor
            { thcReleaseId = prId release
            , thcScope = scope
            , thcConfirmedBlock = ccBlockNumber context
            , thcConfirmedBlockHash = ccBlockHash context
            , thcActionBlock = parBlockNumber <$> actionPosition
            , thcActionLogIndex = parLogIndex <$> actionPosition
            , thcActionId = parActionId <$> actionPosition
            , thcActionsComplete = noPosition actionPosition
            , thcCheckpointBlock = pssBlockNumber <$> checkpointPosition
            , thcCheckpointsComplete = noPosition checkpointPosition
            , thcCheckpointContinuationUnavailable =
                checkpointContinuationUnavailable
                  && noPosition checkpointPosition
            }
  where
    noPosition Nothing = True
    noPosition (Just _) = False

keeperAggregateCursor
  :: ProtocolRelease
  -> Text
  -> ConfirmedContext
  -> KeeperAggregateRow
  -> Maybe Text
keeperAggregateCursor release scope context row
  | not $ isCanonicalBlockHash $ ccBlockHash context = Nothing
  | otherwise =
      Just $
        encodeProtocolCursor
          ProtocolCursor
            { pcReleaseId = prId release
            , pcScope = scope
            , pcConfirmedBlock = ccBlockNumber context
            , pcConfirmedBlockHash = ccBlockHash context
            -- Keeper-list ordering is aggregate based rather than block/log
            -- based. Keep the generic cursor position fields valid and encode
            -- the endpoint-specific tuple in the opaque item identity.
            , pcItemBlock = 0
            , pcItemLogIndex = Just 0
            , pcItemId =
                Just $
                  T.intercalate
                    ":"
                    [ "keeper"
                    , T.pack $ show $ karGrossRewardsUsdc row
                    , T.pack $ show $ karActionCount row
                    , T.toLower $ karActor row
                    ]
            }

keeperAggregateCursorKey
  :: Maybe ProtocolCursor -> Maybe (Integer, Integer, Text)
keeperAggregateCursorKey Nothing = Nothing
keeperAggregateCursorKey (Just cursor)
  | pcItemBlock cursor /= 0 = Nothing
  | pcItemLogIndex cursor /= Just 0 = Nothing
  | otherwise = do
      itemIdentity <- pcItemId cursor
      case T.splitOn ":" itemIdentity of
        ["keeper", rewardText, actionCountText, actor]
          | not (T.null actor) -> do
              reward <- parseKeeperCursorInteger rewardText
              actionCount <- parseKeeperCursorInteger actionCountText
              pure (reward, actionCount, T.toLower actor)
        _ -> Nothing

validKeeperAggregateCursor :: Maybe ProtocolCursor -> Bool
validKeeperAggregateCursor Nothing = True
validKeeperAggregateCursor cursor =
  keeperAggregateCursorKey cursor /= Nothing

parseKeeperCursorInteger :: Text -> Maybe Integer
parseKeeperCursorInteger raw
  | T.null raw = Nothing
  | not $ T.all (\char -> char >= '0' && char <= '9') raw = Nothing
  | otherwise = do
      value <- readMaybe $ T.unpack raw
      if value >= 0 && value <= maxDatabaseBigInt
        then Just value
        else Nothing

parameterChangeCursor
  :: ProtocolRelease
  -> Text
  -> ConfirmedContext
  -> Value
  -> Maybe Text
parameterChangeCursor release scope context change
  | not $ isCanonicalBlockHash $ ccBlockHash context = Nothing
  | otherwise = do
      blockNumber <- objectIntegerField "blockNumber" change
      changeId <- objectField "changeId" change >>= jsonTextValue
      pure $
        encodeProtocolCursor
          ProtocolCursor
            { pcReleaseId = prId release
            , pcScope = scope
            , pcConfirmedBlock = ccBlockNumber context
            , pcConfirmedBlockHash = ccBlockHash context
            , pcItemBlock = blockNumber
            , pcItemLogIndex = Nothing
            , pcItemId = Just changeId
            }
  where
    jsonTextValue (String value) = Just value
    jsonTextValue _ = Nothing

actionCursorKey :: Maybe ProtocolCursor -> Maybe (Integer, Integer)
actionCursorKey cursor = do
  value <- cursor
  logIndex <- pcItemLogIndex value
  pure (pcItemBlock value, logIndex)

trancheHistoryActionCursorKey
  :: Maybe TrancheHistoryCursor
  -> Maybe (Integer, Integer)
trancheHistoryActionCursorKey cursor = do
  value <- cursor
  blockNumber <- thcActionBlock value
  logIndex <- thcActionLogIndex value
  pure (blockNumber, logIndex)

trancheHistoryCheckpointCursorKey
  :: Maybe TrancheHistoryCursor
  -> Maybe Integer
trancheHistoryCheckpointCursorKey cursor =
  cursor >>= thcCheckpointBlock

validActionCursor :: Maybe ProtocolCursor -> Bool
validActionCursor Nothing = True
validActionCursor (Just cursor) =
  pcItemLogIndex cursor /= Nothing
    && pcItemId cursor /= Nothing

parameterCursorKey :: Maybe ProtocolCursor -> Maybe (Integer, Text)
parameterCursorKey cursor = do
  value <- cursor
  changeId <- pcItemId value
  pure (pcItemBlock value, changeId)

transactionsCursorScope :: ProtocolTransactionFilters -> Text
transactionsCursorScope ProtocolTransactionFilters {..} =
  T.intercalate
    ":"
    [ "transactions"
    , cursorScopeValue ptfActionType
    , cursorScopeValue ptfOutcome
    , cursorScopeValue $ T.toLower <$> ptfAddress
    , cursorScopeValue $ T.toLower <$> ptfAccount
    , cursorScopeValue $ T.toLower <$> ptfKeeper
    , cursorScopeValue $ T.toLower <$> ptfContract
    , cursorScopeValue $ T.toLower <$> ptfTransactionHash
    , cursorScopeValue $ T.pack . show <$> ptfFromTimestamp
    , cursorScopeValue $ T.pack . show <$> ptfToTimestamp
    ]

cursorScopeValue :: Maybe Text -> Text
cursorScopeValue =
  maybe
    "-"
    (TE.decodeUtf8 . B16.encode . TE.encodeUtf8)

maxDatabaseBigInt :: Integer
maxDatabaseBigInt = 2 ^ (63 :: Int) - 1

confirmedContext :: EthClient -> Config -> IO ConfirmedContext
confirmedContext client cfg = do
  latestResult <- ethBlockNumber client
  case latestResult of
    Left _ ->
      pure $
        ConfirmedContext
          { ccBlockNumber = 0
          , ccBlockHash = ""
          , ccBlockTimestamp = 0
          , ccBlockRef = Nothing
          , ccAvailability = [unavailable "confirmedBlock" "rpc_head_unavailable"]
          }
    Right latestBlockNumber -> do
      let confirmationDepth = max 1 $ toInteger $ cfgKeeperConfirmations cfg
          target = max 0 $ latestBlockNumber - confirmationDepth
      confirmedResult <- ethGetBlockByNumber client target
      canonicalRefResult <- ethGetCanonicalBlockRef client target
      pure $ case (confirmedResult, canonicalRefResult) of
        (Right block, Right blockRef)
          | rpcBlockNumber block == canonicalBlockNumber blockRef
              && T.toLower (rpcBlockHash block) == canonicalBlockHash blockRef ->
          ConfirmedContext
            { ccBlockNumber = canonicalBlockNumber blockRef
            , ccBlockHash = canonicalBlockHash blockRef
            , ccBlockTimestamp = rpcBlockTimestamp block
            , ccBlockRef = Just blockRef
            , ccAvailability = []
            }
        (Right _, Right _) ->
          ConfirmedContext
            { ccBlockNumber = target
            , ccBlockHash = ""
            , ccBlockTimestamp = 0
            , ccBlockRef = Nothing
            , ccAvailability = [unavailable "confirmedBlock" "canonical_block_anchor_mismatch"]
            }
        _ ->
          ConfirmedContext
            { ccBlockNumber = target
            , ccBlockHash = ""
            , ccBlockTimestamp = 0
            , ccBlockRef = Nothing
            , ccAvailability = [unavailable "confirmedBlock" "canonical_block_anchor_unavailable"]
            }

resolveCanonicalBlockRef
  :: EthClient
  -> Maybe Integer
  -> IO (Maybe CanonicalBlockRef)
resolveCanonicalBlockRef _ Nothing = pure Nothing
resolveCanonicalBlockRef client (Just blockNumber) =
  either (const Nothing) Just
    <$> ethGetCanonicalBlockRef client blockNumber

projectionListResponse
  :: ProtocolRelease
  -> ProjectionListContext
  -> Value
  -> [Value]
  -> Text
  -> Value
  -> ApiResponse Value
projectionListResponse release listContext evidence availability bodyKey body =
  protocolResponse
    release
    context
    (Just $ plcIndexedHead listContext)
    (insertEvidenceField "listProjection" (projectionCoverageEvidence release listContext) evidence)
    (ccAvailability context <> plcAvailability listContext <> availability)
    bodyKey
    body
  where
    context = plcConfirmedContext listContext

projectionCoverageEvidence :: ProtocolRelease -> ProjectionListContext -> Value
projectionCoverageEvidence release listContext =
  object
    [ "level" .=
        if lagBlocks > 0
          then ("partial" :: Text)
          else "exact"
    , "policy" .=
        ("minimum_of_chain_confirmed_and_contiguous_projection_heads" :: Text)
    , "indexerName" .= protocolProjectionIndexerName release
    , "anchorReusedFromCursor" .=
        plcCursorAnchorReused listContext
    , "anchorBlock" .= object
        [ "number" .= show (ccBlockNumber context)
        , "hash" .= ccBlockHash context
        , "timestamp" .= ccBlockTimestamp context
        ]
    , "chainConfirmedBlock" .=
        show (plcChainConfirmedBlock listContext)
    , "contiguousIndexerHead" .= object
        [ "number" .= show indexedBlock
        , "hash" .= T.toLower indexedHash
        , "indexerTimestamp" .= indexedTimestamp
        ]
    , "indexerLagBlocks" .= show lagBlocks
    ]
  where
    context = plcConfirmedContext listContext
    (indexedBlock, indexedHash, indexedTimestamp) =
      plcIndexedHead listContext
    lagBlocks =
      max 0 $ plcChainConfirmedBlock listContext - indexedBlock

insertEvidenceField :: Text -> Value -> Value -> Value
insertEvidenceField fieldName fieldValue = \case
  Object fields ->
    Object $ KM.insert (Key.fromText fieldName) fieldValue fields
  _ -> object [Key.fromText fieldName .= fieldValue]

protocolResponse
  :: ProtocolRelease
  -> ConfirmedContext
  -> Maybe (Integer, Text, Integer)
  -> Value
  -> [Value]
  -> Text
  -> Value
  -> ApiResponse Value
protocolResponse release context indexedHead evidence availability bodyKey body =
  mkResponse (ccBlockNumber context) (prChainId release) $
    object
      [ "releaseId" .= prId release
      , "chainId" .= show (prChainId release)
      , "confirmedBlock" .= object
          [ "number" .= show (ccBlockNumber context)
          , "hash" .= ccBlockHash context
          , "timestamp" .= ccBlockTimestamp context
          ]
      , "indexerTimestamp" .= fmap (\(_, _, timestamp) -> timestamp) indexedHead
      , "calculationVersion" .= prCalculationVersion release
      , "evidence" .= evidence
      , "availability" .=
          ( availability
              <> [ unavailable "indexerTimestamp" "protocol_indexer_state_unavailable"
                 | indexedHead == Nothing
                 ]
          )
      , Key.fromText bodyKey .= body
      ]

actionToJson :: ProtocolActionRow -> Value
actionToJson ProtocolActionRow {..} =
  object
    [ "actionId" .= parActionId
    , "transactionHash" .= parTxHash
    , "blockNumber" .= show parBlockNumber
    , "blockHash" .= parBlockHash
    , "transactionIndex" .= show parTxIndex
    , "logIndex" .= show parLogIndex
    , "timestamp" .= parTimestamp
    , "actionType" .= parActionType
    , "outcome" .= parStatus
    , "account" .= parAccount
    , "keeper" .= parActor
    , "orderId" .= fmap show parOrderId
    , "contractAddress" .= parContractAddress
    , "data" .= parData
    , "evidence" .= parEvidence
    , "units" .= object
        [ "amountUsdc" .= ("USDC:6" :: Text)
        , "keeperBountyUsdc" .= ("USDC:6" :: Text)
        , "marginDelta" .= ("USDC:6" :: Text)
        , "pnl" .= ("USDC:6" :: Text)
        , "assets" .= ("USDC:6" :: Text)
        , "shares" .= ("shares:18" :: Text)
        , "sizeDelta" .= ("position:18" :: Text)
        , "price" .= ("indexPrice:8" :: Text)
        , "executionPrice" .= ("indexPrice:8" :: Text)
        , "acceptablePrice" .= ("indexPrice:8" :: Text)
        ]
    ]

transactionToJson :: ProtocolRelease -> ProtocolTransactionRow -> Value
transactionToJson release row@ProtocolTransactionRow {..} =
  object
    [ "transactionHash" .= ptrTxHash
    , "blockNumber" .= show ptrBlockNumber
    , "blockHash" .= ptrBlockHash
    , "transactionIndex" .= show ptrTxIndex
    , "timestamp" .= ptrTimestamp
    , "sender" .= ptrSender
    , "recipient" .= ptrRecipient
    , "selector" .= ptrSelector
    , "outcome" .= ptrStatus
    , "gasUsed" .= fmap show ptrGasUsed
    , "effectiveGasPriceWei" .= fmap show ptrEffectiveGasPrice
    , "gasCostWei" .= fmap show ((*) <$> ptrGasUsed <*> ptrEffectiveGasPrice)
    , "nativeValueWei" .= fmap show ptrNativeValue
    , "input" .= ptrInputData
    , "explorerUrl" .= explorerTxUrl release ptrTxHash
    , "evidence" .= ptrEvidence
    , "availability" .= transactionAvailability row
    , "units" .= object
        [ "gasUsed" .= ("gas" :: Text)
        , "effectiveGasPriceWei" .= ("wei/gas" :: Text)
        , "gasCostWei" .= ("wei" :: Text)
        , "nativeValueWei" .= ("wei" :: Text)
        ]
    ]

transactionEvidenceLabel :: ProtocolTransactionRow -> Text
transactionEvidenceLabel row
  | transactionInputAvailable row
      && transactionReceiptAvailable row
      && transactionEnvelopeAvailable row =
      "exact_transaction_input_and_receipt"
  | transactionInputAvailable row
      || transactionReceiptAvailable row
      || transactionEnvelopePartiallyAvailable row =
      "partial_transaction_evidence"
  | otherwise = "transaction_evidence_unavailable"

transactionAvailability :: ProtocolTransactionRow -> [Value]
transactionAvailability row =
  [unavailable "transaction.input" "transaction_input_unavailable" | not $ transactionInputAvailable row]
    <> [unavailable "transaction.receipt" "transaction_receipt_unavailable" | not $ transactionReceiptAvailable row]
    <> [unavailable "transaction.sender" "transaction_sender_unavailable" | ptrSender row == Nothing]
    <> [unavailable "transaction.recipient" "transaction_recipient_unavailable" | ptrRecipient row == Nothing]
    <> [unavailable "transaction.selector" "transaction_selector_unavailable" | ptrSelector row == Nothing]
    <> [unavailable "transaction.outcome" "transaction_receipt_status_unavailable" | ptrStatus row `notElem` ["success", "reverted"]]
    <> [unavailable "transaction.gasUsed" "transaction_receipt_gas_used_unavailable" | ptrGasUsed row == Nothing]
    <> [unavailable "transaction.effectiveGasPriceWei" "transaction_receipt_effective_gas_price_unavailable" | ptrEffectiveGasPrice row == Nothing]
    <> [unavailable "transaction.gasCostWei" "transaction_receipt_cost_components_unavailable" | ptrGasUsed row == Nothing || ptrEffectiveGasPrice row == Nothing]
    <> [unavailable "transaction.nativeValueWei" "transaction_native_value_unavailable" | ptrNativeValue row == Nothing]

transactionInputAvailable :: ProtocolTransactionRow -> Bool
transactionInputAvailable row =
  case ptrInputData row of
    Just input -> not $ T.null input
    Nothing -> False

transactionReceiptAvailable :: ProtocolTransactionRow -> Bool
transactionReceiptAvailable row =
  ptrStatus row `elem` ["success", "reverted"]
    && ptrGasUsed row /= Nothing
    && ptrEffectiveGasPrice row /= Nothing

transactionEnvelopeAvailable :: ProtocolTransactionRow -> Bool
transactionEnvelopeAvailable row =
  ptrSender row /= Nothing
    && ptrRecipient row /= Nothing
    && ptrSelector row /= Nothing
    && ptrNativeValue row /= Nothing

transactionEnvelopePartiallyAvailable :: ProtocolTransactionRow -> Bool
transactionEnvelopePartiallyAvailable row =
  any
    (/= Nothing)
    [ ptrSender row
    , ptrRecipient row
    , ptrSelector row
    ]
    || ptrNativeValue row /= Nothing

verifyTransactionCanonicality
  :: EthClient
  -> ProtocolTransactionRow
  -> IO (Either ApiError ())
verifyTransactionCanonicality client row = do
  canonicalBlock <- ethGetCanonicalBlockRef client (ptrBlockNumber row)
  pure $ case canonicalBlock of
    Left _ ->
      Left $
        E.networkError
          "Protocol transaction canonicality could not be verified"
    Right blockRef
      | T.toLower (canonicalBlockHash blockRef)
          == T.toLower (ptrBlockHash row) ->
          Right ()
      | otherwise ->
          Left $
            E.notFound
              "Transaction is no longer canonical for this protocol release"

verifyOrderTransactionCanonicality
  :: EthClient
  -> Text
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Integer
  -> Maybe ProtocolTransactionRow
  -> IO (Either ApiError ())
verifyOrderTransactionCanonicality _ _ Nothing _ _ Nothing =
  pure $ Right ()
verifyOrderTransactionCanonicality _ lifecycle Nothing _ _ (Just _) =
  pure $
    Left $
      E.internalError $
        "Order " <> lifecycle <> " transaction evidence is inconsistent"
verifyOrderTransactionCanonicality _ lifecycle (Just _) _ _ Nothing =
  pure $
    Left $
      E.networkError $
        "Order " <> lifecycle <> " transaction canonicality could not be verified"
verifyOrderTransactionCanonicality client lifecycle (Just expectedHash) expectedBlock expectedTimestamp (Just row)
  | T.toLower expectedHash /= T.toLower (ptrTxHash row) =
      pure $
        Left $
          E.internalError $
            "Order " <> lifecycle <> " transaction evidence is inconsistent"
  | maybe False (/= ptrBlockNumber row) expectedBlock =
      pure $
        Left $
          E.notFound $
            "Order " <> lifecycle <> " transaction is no longer canonical"
  | maybe False (/= ptrTimestamp row) expectedTimestamp =
      pure $
        Left $
          E.internalError $
            "Order " <> lifecycle <> " transaction timestamp is inconsistent"
  | otherwise = verifyTransactionCanonicality client row

eventToJson :: ProtocolEventRow -> Value
eventToJson ProtocolEventRow {..} =
  object
    [ "logIndex" .= show perLogIndex
    , "contractAddress" .= perContractAddress
    , "eventName" .= perEventName
    , "rawTopics" .= perRawTopics
    , "rawData" .= perRawData
    , "decodedFields" .= perDecodedData
    , "evidence" .= perEvidence
    ]

getProtocolTransactionFor :: Text -> Integer -> Text -> Connection -> IO (Maybe ProtocolTransactionRow)
getProtocolTransactionFor releaseId maxBlock txHash conn =
  getProtocolTransaction conn releaseId txHash maxBlock

data OrderCommitIntent = OrderCommitIntent
  { ociSide :: Integer
  , ociAcceptablePrice :: Integer
  , ociIsClose :: Bool
  , ociValue :: Value
  , ociEvidence :: Text
  }

-- | Resolve the canonical V2 intent from the lifecycle event belonging to the
-- requested order. The order-id predicate is important for batch transactions:
-- actions from the other orders in the same transaction are deliberately
-- included in the detail response, but must never supply this order's intent.
orderCommitIntentFromActions
  :: ProtocolRelease
  -> Integer
  -> [ProtocolActionRow]
  -> Maybe Value
orderCommitIntentFromActions release orderId actions =
  ociValue <$> exactOrderCommitIntent release orderId actions

exactOrderCommitIntent
  :: ProtocolRelease
  -> Integer
  -> [ProtocolActionRow]
  -> Maybe OrderCommitIntent
exactOrderCommitIntent release orderId actions = do
  lifecycleBook <- prOrderLifecycleBook release
  action <-
    find
      (\candidate ->
        parOrderId candidate == Just orderId
          && sameAddress (parContractAddress candidate) lifecycleBook
          && parActionType candidate == "order_commitment"
          && objectField "intentHash" (parData candidate) /= Nothing
          && objectField "request" (parData candidate) /= Nothing
      )
      actions
  request <- objectField "request" $ parData action
  policy <- objectField "policy" request <|> objectField "policy" (parData action)
  side <- objectIntegerField "side" request
  sizeDelta <- objectIntegerField "sizeDelta" request
  marginDelta <- objectIntegerField "marginDeltaUsdc" request
  acceptablePrice <- objectIntegerField "acceptablePrice" request
  isClose <- objectBooleanField "isClose" request
  let payload = parData action
      rendered =
        object
          [ "clientOrderId" .= fromMaybe Null (objectField "clientOrderId" payload)
          , "intentHash" .= fromMaybe Null (objectField "intentHash" payload)
          , "executionBountyUsdc" .=
              fromMaybe Null (objectField "executionBountyUsdc" payload)
          , "side" .= show side
          , "action" .=
              if isClose
                then ("reduce_or_close" :: Text)
                else "open_or_increase"
          , "sizeDelta" .= show sizeDelta
          , "marginDeltaUsdc" .= show marginDelta
          , "acceptablePrice" .= show acceptablePrice
          , "isClose" .= isClose
          , "request" .= request
          , "policy" .= policy
          , "evidence" .= object
              [ "level" .= ("exact_confirmed_intent_registered_event" :: Text)
              , "actionId" .= parActionId action
              , "transactionHash" .= parTxHash action
              , "blockNumber" .= show (parBlockNumber action)
              , "logIndex" .= show (parLogIndex action)
              ]
          , "units" .= fromMaybe Null (objectField "units" payload)
          ]
  pure
    OrderCommitIntent
      { ociSide = side
      , ociAcceptablePrice = acceptablePrice
      , ociIsClose = isClose
      , ociValue = rendered
      , ociEvidence = "exact_confirmed_intent_registered_event"
      }

legacyOrderCommitIntent
  :: ProtocolRelease
  -> ProtocolTransactionRow
  -> Maybe OrderCommitIntent
legacyOrderCommitIntent release transaction = do
  args <- either (const Nothing) Just $ canonicalCommitArguments release transaction
  let side = uintWord args 0
      sizeDelta = uintWord args 1
      marginDelta = uintWord args 2
      acceptablePrice = uintWord args 3
      isClose = uintWord args 4 /= 0
      rendered = object
        [ "side" .= show side
        , "action" .= if isClose then ("reduce_or_close" :: Text) else "open_or_increase"
        , "sizeDelta" .= show sizeDelta
        , "marginDeltaUsdc" .= show marginDelta
        , "acceptablePrice" .= show acceptablePrice
        , "units" .= object
            [ "sizeDelta" .= ("position:18" :: Text)
            , "marginDeltaUsdc" .= ("USDC:6" :: Text)
            , "acceptablePrice" .= ("indexPrice:8" :: Text)
            ]
        ]
  pure
    OrderCommitIntent
      { ociSide = side
      , ociAcceptablePrice = acceptablePrice
      , ociIsClose = isClose
      , ociValue = rendered
      , ociEvidence = "decoded_commitment_transaction_input"
      }

resolveOrderCommitIntent
  :: ProtocolRelease
  -> Integer
  -> Maybe ProtocolTransactionRow
  -> [ProtocolActionRow]
  -> Maybe OrderCommitIntent
resolveOrderCommitIntent release orderId transaction actions =
  exactOrderCommitIntent release orderId actions
    <|> (transaction >>= legacyOrderCommitIntent release)

orderIntentUnavailableReason
  :: ProtocolRelease
  -> Maybe ProtocolTransactionRow
  -> Maybe OrderCommitIntent
  -> Maybe Text
orderIntentUnavailableReason _ _ (Just _) = Nothing
orderIntentUnavailableReason release Nothing Nothing
  | prOrderLifecycleBook release /= Nothing =
      Just "intent_registered_event_unavailable"
  | otherwise = Just "commitment_transaction_unavailable"
orderIntentUnavailableReason release (Just transaction) Nothing
  | prOrderLifecycleBook release /= Nothing =
      Just "intent_registered_event_unavailable"
  | otherwise = either Just (const Nothing) $ canonicalCommitArguments release transaction

findOrderFinalizationAction
  :: ProtocolRelease
  -> Integer
  -> [ProtocolActionRow]
  -> Maybe ProtocolActionRow
findOrderFinalizationAction release orderId actions = do
  lifecycleBook <- prOrderLifecycleBook release
  find
    (\candidate ->
      parOrderId candidate == Just orderId
        && sameAddress (parContractAddress candidate) lifecycleBook
        && objectField "receiptHash" (parData candidate) /= Nothing
        && objectField "receipt" (parData candidate) /= Nothing
    )
    actions

-- | Render the full accounting-neutral V2 terminal receipt. The nested
-- receipt/failure/economics objects preserve the canonical event vocabulary;
-- the selected high-value fields make the order page useful without requiring
-- consumers to decode that tuple themselves.
orderFinalizationFromActions
  :: ProtocolRelease
  -> Integer
  -> [ProtocolActionRow]
  -> Maybe Value
orderFinalizationFromActions release orderId actions = do
  action <- findOrderFinalizationAction release orderId actions
  let payload = parData action
      receipt = objectField "receipt" payload
      receiptField keyName = receipt >>= objectField keyName
      payloadField keyName = objectField keyName payload
      exactField keyName = payloadField keyName <|> receiptField keyName
  pure $ object
    [ "receiptHash" .= fromMaybe Null (payloadField "receiptHash")
    , "clientOrderId" .= fromMaybe Null (payloadField "clientOrderId")
    , "intentHash" .= fromMaybe Null (exactField "intentHash")
    , "expectedConfigHash" .= fromMaybe Null (exactField "expectedConfigHash")
    , "observedConfigHash" .= fromMaybe Null (exactField "observedConfigHash")
    , "lifecycleStatus" .= fromMaybe Null (receiptField "lifecycleStatus")
    , "status" .= fromMaybe Null (receiptField "status")
    , "terminalReasonCode" .= fromMaybe Null (receiptField "terminalReasonCode")
    , "terminalReason" .= fromMaybe Null (exactField "terminalReason")
    , "executionModeCode" .= fromMaybe Null (receiptField "executionModeCode")
    , "executionMode" .= fromMaybe Null (exactField "executionMode")
    , "executor" .= fromMaybe Null (exactField "executor")
    , "priceSource" .= fromMaybe Null (receiptField "priceSource")
    , "executionPrice" .= fromMaybe Null (exactField "executionPrice")
    , "neutralMarkPrice" .= fromMaybe Null (receiptField "neutralMarkPrice")
    , "poolDepthUsdc" .= fromMaybe Null (receiptField "poolDepthUsdc")
    , "oraclePublishTime" .= fromMaybe Null (exactField "oraclePublishTime")
    , "priceReachedEngine" .= fromMaybe Null (receiptField "priceReachedEngine")
    , "bountyUsdc" .= fromMaybe Null (exactField "bountyUsdc")
    , "bountyRecipient" .= fromMaybe Null (exactField "bountyRecipient")
    , "bountyDisposition" .= fromMaybe Null (receiptField "bountyDisposition")
    , "failedConstraint" .= fromMaybe Null (payloadField "failedConstraint")
    , "failure" .= fromMaybe Null (objectField "failure" payload)
    , "economics" .= fromMaybe Null (objectField "economics" payload)
    , "receipt" .= fromMaybe Null receipt
    , "evidence" .= object
        [ "level" .= ("exact_confirmed_order_finalized_event" :: Text)
        , "actionId" .= parActionId action
        , "transactionHash" .= parTxHash action
        , "blockNumber" .= show (parBlockNumber action)
        , "logIndex" .= show (parLogIndex action)
        ]
    , "units" .= fromMaybe Null (objectField "units" payload)
    ]

orderFinalizationField
  :: ProtocolRelease
  -> Integer
  -> Text
  -> [ProtocolActionRow]
  -> Maybe Value
orderFinalizationField release orderId keyName actions = do
  action <- findOrderFinalizationAction release orderId actions
  objectField keyName $ parData action

objectBooleanField :: Text -> Value -> Maybe Bool
objectBooleanField keyName value = do
  fieldValue <- objectField keyName value
  case fieldValue of
    Bool boolean -> Just boolean
    _ -> Nothing

sameAddress :: Text -> Text -> Bool
sameAddress left right =
  T.toLower (T.strip left) == T.toLower (T.strip right)

canonicalCommitArguments :: ProtocolRelease -> ProtocolTransactionRow -> Either Text BS.ByteString
canonicalCommitArguments release transaction = do
  recipient <- maybe (Left "transaction_recipient_unavailable") Right $ ptrRecipient transaction
  input <- maybe (Left "transaction_input_unavailable") Right $ ptrInputData transaction
  let expectedSelector = encodeCall "commitOrder(uint8,uint256,uint256,uint256,bool)" []
      expectedSelectorText = "0x" <> TE.decodeUtf8 (B16.encode expectedSelector)
      selectorMetadataMatches =
        maybe True ((== T.toLower expectedSelectorText) . T.toLower) (ptrSelector transaction)
  if T.toLower recipient /= T.toLower (prOrderRouter release)
    then Left "noncanonical_commit_recipient"
    else if not ("0x" `T.isPrefixOf` T.toLower input)
      then Left "malformed_transaction_input"
      else if not selectorMetadataMatches
        then Left "noncanonical_commit_selector"
        else do
          calldata <- maybe (Left "malformed_transaction_input") Right $ decodeHexText $ T.drop 2 input
          if BS.length calldata < 164
            then Left "malformed_commit_calldata"
            else if BS.take 4 calldata /= expectedSelector
              then Left "noncanonical_commit_selector"
              else
                let args = BS.drop 4 calldata
                 in if uintWord args 0 > 255 || uintWord args 4 > 1
                      then Left "noncanonical_commit_arguments"
                      else Right args

data StateImpactPair = StateImpactPair
  { sipBefore :: Maybe Value
  , sipAfter :: Maybe Value
  , sipBeforeReason :: Maybe Text
  , sipAfterReason :: Maybe Text
  }

data AccountStateImpact = AccountStateImpact
  { asiAccount :: Text
  , asiActionIds :: [Text]
  , asiActionTypes :: [Text]
  , asiState :: StateImpactPair
  }

data TransactionStateImpact = TransactionStateImpact
  { tsiAccounts :: [AccountStateImpact]
  , tsiHousePool :: StateImpactPair
  , tsiSenior :: StateImpactPair
  , tsiJunior :: StateImpactPair
  , tsiBeforeBlockNumber :: Maybe Integer
  , tsiBeforeBlockHash :: Maybe Text
  , tsiAfterBlockNumber :: Integer
  , tsiAfterBlockHash :: Text
  , tsiTransactionHash :: Text
  , tsiCalculationVersion :: Text
  }

transactionStateImpact
  :: EthClient
  -> ProtocolRelease
  -> ProtocolTransactionRow
  -> [ProtocolActionRow]
  -> IO TransactionStateImpact
transactionStateImpact client release transaction actions = do
  let afterBlockNumber = ptrBlockNumber transaction
      beforeBlockNumber =
        if afterBlockNumber > 0
          then Just $ afterBlockNumber - 1
          else Nothing
      accounts =
        nubBy
          (\left right -> T.toLower left == T.toLower right)
          (catMaybes $ map parAccount actions)
  afterBlockResult <- ethGetCanonicalBlockRef client afterBlockNumber
  beforeBlockResult <- case beforeBlockNumber of
    Nothing -> pure $ Left ("before_block_unavailable" :: Text)
    Just blockNumber ->
      either (const $ Left "canonical_block_anchor_unavailable") Right
        <$> ethGetCanonicalBlockRef client blockNumber
  let afterBlockRef = either (const Nothing) Just afterBlockResult
      beforeBlockRef = either (const Nothing) Just beforeBlockResult
      afterAnchorMatches =
        maybe
          False
          ( \blockRef ->
              T.toLower (canonicalBlockHash blockRef)
                == T.toLower (ptrBlockHash transaction)
          )
          afterBlockRef
  if not afterAnchorMatches
    then
      pure $
        unavailableTransactionStateImpact
          release
          transaction
          actions
          accounts
          beforeBlockNumber
          (canonicalBlockHash <$> beforeBlockRef)
          ( if afterBlockRef == Nothing
              then "canonical_block_anchor_unavailable"
              else "transaction_block_hash_mismatch"
          )
    else do
      poolBefore <-
        mapStateRead poolLiquidityJson
          <$> poolLiquidityAt client release beforeBlockRef
      poolAfter <-
        mapStateRead poolLiquidityJson
          <$> poolLiquidityAt client release afterBlockRef
      accountImpacts <-
        forM accounts $ \account -> do
          before <- accountSnapshotAt client release account beforeBlockRef
          after <- accountSnapshotAt client release account afterBlockRef
          let accountActions =
                filter
                  (maybe False ((== T.toLower account) . T.toLower) . parAccount)
                  actions
          pure
            AccountStateImpact
              { asiAccount = account
              , asiActionIds = map parActionId accountActions
              , asiActionTypes =
                  nubBy (==) $ map parActionType accountActions
              , asiState = stateImpactPair before after
              }
      seniorBefore <-
        trancheStateAt client release "senior" beforeBlockRef poolBefore
      seniorAfter <-
        trancheStateAt client release "senior" afterBlockRef poolAfter
      juniorBefore <-
        trancheStateAt client release "junior" beforeBlockRef poolBefore
      juniorAfter <-
        trancheStateAt client release "junior" afterBlockRef poolAfter
      pure
        TransactionStateImpact
          { tsiAccounts = accountImpacts
          , tsiHousePool = stateImpactPair poolBefore poolAfter
          , tsiSenior = stateImpactPair seniorBefore seniorAfter
          , tsiJunior = stateImpactPair juniorBefore juniorAfter
          , tsiBeforeBlockNumber = beforeBlockNumber
          , tsiBeforeBlockHash = canonicalBlockHash <$> beforeBlockRef
          , tsiAfterBlockNumber = afterBlockNumber
          , tsiAfterBlockHash =
              maybe
                (T.toLower $ ptrBlockHash transaction)
                canonicalBlockHash
                afterBlockRef
          , tsiTransactionHash = ptrTxHash transaction
          , tsiCalculationVersion = prCalculationVersion release
          }

unavailableTransactionStateImpact
  :: ProtocolRelease
  -> ProtocolTransactionRow
  -> [ProtocolActionRow]
  -> [Text]
  -> Maybe Integer
  -> Maybe Text
  -> Text
  -> TransactionStateImpact
unavailableTransactionStateImpact release transaction actions accounts beforeBlockNumber beforeBlockHash reason =
  let missingPair =
        StateImpactPair
          { sipBefore = Nothing
          , sipAfter = Nothing
          , sipBeforeReason = Just reason
          , sipAfterReason = Just reason
          }
      accountImpact account =
        let accountActions =
              filter
                (maybe False ((== T.toLower account) . T.toLower) . parAccount)
                actions
         in AccountStateImpact
              { asiAccount = account
              , asiActionIds = map parActionId accountActions
              , asiActionTypes = nubBy (==) $ map parActionType accountActions
              , asiState = missingPair
              }
   in TransactionStateImpact
        { tsiAccounts = map accountImpact accounts
        , tsiHousePool = missingPair
        , tsiSenior = missingPair
        , tsiJunior = missingPair
        , tsiBeforeBlockNumber = beforeBlockNumber
        , tsiBeforeBlockHash = beforeBlockHash
        , tsiAfterBlockNumber = ptrBlockNumber transaction
        , tsiAfterBlockHash = T.toLower $ ptrBlockHash transaction
        , tsiTransactionHash = ptrTxHash transaction
        , tsiCalculationVersion = prCalculationVersion release
        }

stateImpactPair :: StateRead Value -> StateRead Value -> StateImpactPair
stateImpactPair before after =
  StateImpactPair
    { sipBefore = srValue before
    , sipAfter = srValue after
    , sipBeforeReason = srUnavailableReason before
    , sipAfterReason = srUnavailableReason after
    }

trancheStateAt
  :: EthClient
  -> ProtocolRelease
  -> Text
  -> Maybe CanonicalBlockRef
  -> StateRead Value
  -> IO (StateRead Value)
trancheStateAt client release tranche blockRef poolRead =
  case trancheAddress release tranche of
    Nothing ->
      pure $ StateRead Nothing $ Just "tranche_contract_unavailable"
    Just vault -> do
      assetsRead <-
        mapStateRead (`word` 0)
          <$> callAtExactWords client vault "totalAssets()" [] blockRef 1
      supplyRead <-
        mapStateRead (`word` 0)
          <$> callAtExactWords client vault "totalSupply()" [] blockRef 1
      assetsPerShareRead <-
        mapStateRead (`word` 0)
          <$> callAtExactWords
            client
            vault
            "convertToAssets(uint256)"
            [encodeUintWord $ 10 ^ (18 :: Int)]
            blockRef
            1
      epochRead <-
        mapStateRead (`word` 0)
          <$> callAtExactWords
            client
            vault
            "currentDepositEpoch()"
            []
            blockRef
            1
      cooldownRead <-
        mapStateRead (`word` 0)
          <$> callAtExactWords
            client
            vault
            "DEPOSIT_COOLDOWN()"
            []
            blockRef
            1
      let pool = srValue poolRead
          principal =
            pool >>= objectIntegerField
              (if tranche == "senior" then "seniorPrincipalUsdc" else "juniorPrincipalUsdc")
          highWater =
            if tranche == "senior"
              then pool >>= objectIntegerField "seniorHighWaterMarkUsdc"
              else Nothing
          impairmentGap = max 0 <$> ((-) <$> highWater <*> principal)
          wipeoutStatus = do
            assets <- srValue assetsRead
            supply <- srValue supplyRead
            tranchePrincipal <- principal
            pure $
              if supply > 0 && (assets == 0 || tranchePrincipal == 0)
                then ("wiped_out" :: Text)
                else "active"
          availability =
            stateReadAvailability "navUsdc" assetsRead
              <> stateReadAvailability "shareSupply" supplyRead
              <> stateReadAvailability "assetsPerShare" assetsPerShareRead
              <> stateReadAvailability "currentDepositEpoch" epochRead
              <> stateReadAvailability "depositCooldownSeconds" cooldownRead
              <> [ unavailable
                    "principalUsdc"
                    (stateImpactUnavailableReason $ srUnavailableReason poolRead)
                 | principal == Nothing
                 ]
          partialReason =
            if null availability
              then Nothing
              else
                srUnavailableReason assetsRead
                  <|> srUnavailableReason supplyRead
                  <|> srUnavailableReason assetsPerShareRead
                  <|> srUnavailableReason epochRead
                  <|> srUnavailableReason cooldownRead
                  <|> srUnavailableReason poolRead
                  <|> Just "partial_tranche_state_unavailable"
          hasAnyState =
            any
              id
              [ srValue assetsRead /= Nothing
              , srValue supplyRead /= Nothing
              , srValue assetsPerShareRead /= Nothing
              , principal /= Nothing
              ]
          detail =
            object
              [ "tranche" .= tranche
              , "vaultAddress" .= vault
              , "principalUsdc" .= fmap show principal
              , "navUsdc" .= fmap show (srValue assetsRead)
              , "shareSupply" .= fmap show (srValue supplyRead)
              , "assetsPerShare" .= fmap show (srValue assetsPerShareRead)
              , "currentDepositEpoch" .= fmap show (srValue epochRead)
              , "depositCooldownSeconds" .= fmap show (srValue cooldownRead)
              , "seniorHighWaterMarkUsdc" .= fmap show highWater
              , "impairmentGapUsdc" .= fmap show impairmentGap
              , "firstLossBufferUsdc" .=
                  if tranche == "junior" then fmap show principal else Nothing
              , "wipeoutStatus" .= wipeoutStatus
              , "provenance" .=
                  ( if partialReason == Nothing
                      then
                        ("exact_canonical_hash_bound_historical_contract_reads" :: Text)
                      else "partial_canonical_hash_bound_historical_contract_reads"
                  )
              , "availability" .= availability
              , "units" .= object
                  [ "principalUsdc" .= ("USDC:6" :: Text)
                  , "navUsdc" .= ("USDC:6" :: Text)
                  , "shareSupply" .= ("shares:18" :: Text)
                  , "assetsPerShare" .= ("USDC:6 per share:18" :: Text)
                  , "currentDepositEpoch" .= ("epoch" :: Text)
                  , "depositCooldownSeconds" .= ("seconds" :: Text)
                  , "seniorHighWaterMarkUsdc" .= ("USDC:6" :: Text)
                  , "impairmentGapUsdc" .= ("USDC:6" :: Text)
                  , "firstLossBufferUsdc" .= ("USDC:6" :: Text)
                  ]
              ]
      pure $
        if hasAnyState
          then StateRead (Just detail) partialReason
          else
            StateRead
              Nothing
              ( srUnavailableReason assetsRead
                  <|> srUnavailableReason supplyRead
                  <|> srUnavailableReason assetsPerShareRead
                  <|> srUnavailableReason poolRead
                  <|> Just "archive_state_unavailable"
              )

data OrderStateDelta = OrderStateDelta
  { osdPositionBefore :: Maybe Value
  , osdPositionAfter :: Maybe Value
  , osdPositionBeforeReason :: Maybe Text
  , osdPositionAfterReason :: Maybe Text
  , osdPoolBefore :: Maybe Value
  , osdPoolAfter :: Maybe Value
  , osdPoolBeforeReason :: Maybe Text
  , osdPoolAfterReason :: Maybe Text
  , osdSeniorBefore :: Maybe Value
  , osdSeniorAfter :: Maybe Value
  , osdSeniorBeforeReason :: Maybe Text
  , osdSeniorAfterReason :: Maybe Text
  , osdJuniorBefore :: Maybe Value
  , osdJuniorAfter :: Maybe Value
  , osdJuniorBeforeReason :: Maybe Text
  , osdJuniorAfterReason :: Maybe Text
  , osdBeforeBlockNumber :: Maybe Integer
  , osdBeforeBlockHash :: Maybe Text
  , osdAfterBlockNumber :: Maybe Integer
  , osdAfterBlockHash :: Maybe Text
  , osdTerminalTxHash :: Maybe Text
  , osdCalculationVersion :: Text
  }

pendingOrderViewAt
  :: EthClient
  -> ProtocolRelease
  -> Integer
  -> PerpsOrderRow
  -> Maybe CanonicalBlockRef
  -> IO (StateRead [Integer])
pendingOrderViewAt client release orderId order commitBlockRef =
  case porCommitBlockNumber order of
    Nothing ->
      pure $ StateRead Nothing $ Just "commitment_block_unavailable"
    Just commitBlock
      | porTerminalBlockNumber order == Just commitBlock ->
          pure $
            StateRead
              Nothing
              (Just "same_block_terminal_state_not_reconstructable")
      | otherwise -> do
          result <-
            callAtExactWords
              client
              (prOrderRouter release)
              "getPendingOrderView(uint64)"
              [encodeUintWord orderId]
              commitBlockRef
              11
          pure $
            case srValue result of
              Nothing -> result
              Just words'
                | word words' 0 /= orderId ->
                    StateRead Nothing $ Just "pending_order_identity_mismatch"
                | word words' 0 > maxUint64
                    || word words' 1 > 1
                    || word words' 2 > 255
                    || word words' 6 > maxUint64
                    || word words' 7 > maxUint64
                    || word words' 10 > maxUint64 ->
                    StateRead Nothing $ Just "malformed_pending_order_view"
                | otherwise -> result

pendingOrderViewJson :: [Integer] -> Value
pendingOrderViewJson words' =
  object
    [ "orderId" .= show (word words' 0)
    , "isClose" .= (word words' 1 == 1)
    , "side" .= show (word words' 2)
    , "sizeDelta" .= show (word words' 3)
    , "marginDeltaUsdc" .= show (word words' 4)
    , "targetPrice" .= show (word words' 5)
    , "commitTime" .= word words' 6
    , "commitBlock" .= show (word words' 7)
    , "committedMarginUsdc" .= show (word words' 8)
    , "executionBountyUsdc" .= show (word words' 9)
    , "nextAccountOrderId" .= show (word words' 10)
    , "fifoIdentity" .= object
        [ "orderId" .= show (word words' 0)
        , "nextAccountOrderIdAtCommitBlock" .= show (word words' 10)
        ]
    , "units" .= object
        [ "sizeDelta" .= ("position:18" :: Text)
        , "marginDeltaUsdc" .= ("USDC:6" :: Text)
        , "targetPrice" .= ("indexPrice:8" :: Text)
        , "committedMarginUsdc" .= ("USDC:6" :: Text)
        , "executionBountyUsdc" .= ("USDC:6" :: Text)
        , "commitTime" .= ("unix_seconds" :: Text)
        ]
    ]

orderTerminalMarketState
  :: Maybe Integer
  -> StateRead [Integer]
  -> StateRead [Integer]
  -> Value
orderTerminalMarketState terminalBlock statusRead poolRead =
  let status = srValue statusRead
      pool = srValue poolRead
      statusEvidence =
        stateReadEvidenceLabel
          "exact_historical_protocol_status_read_at_terminal_block"
          statusRead
      poolEvidence =
        stateReadEvidenceLabel
          "exact_historical_house_pool_read_at_terminal_block"
          poolRead
      tradingActive = status >>= \words' -> Just $ word words' 5 == 1
      fadWindow = status >>= \words' -> Just $ word words' 4 == 1
      statusOracleFrozen = status >>= \words' -> Just $ word words' 3 == 1
      poolOracleFrozen = pool >>= \words' -> Just $ word words' 9 == 1
      mode = do
        isTradingActive <- tradingActive
        inFadWindow <- fadWindow
        protocolFrozen <- statusOracleFrozen
        housePoolFrozen <- poolOracleFrozen
        pure $
          if not isTradingActive
            then ("trading_inactive" :: Text)
            else
              if inFadWindow
                then "fad_window"
                else
                  if protocolFrozen || housePoolFrozen
                    then "oracle_frozen"
                    else "active"
      aggregateStateEvidence =
        case (stateReadEvidenceCompleteness statusRead, stateReadEvidenceCompleteness poolRead) of
          ("exact", "exact") ->
            ("exact_historical_contract_reads_at_terminal_block" :: Text)
          ("unavailable", "unavailable") -> "unavailable"
          _ -> "partial_historical_contract_reads_at_terminal_block"
      marketModeEvidence =
        case mode of
          Nothing -> ("unavailable" :: Text)
          Just _
            | stateReadComplete statusRead && stateReadComplete poolRead ->
                "derived_from_exact_historical_state_flags"
            | otherwise ->
                "derived_from_partial_historical_state_flags"
   in object
        [ "sourceBlockNumber" .= fmap show terminalBlock
        , "marketMode" .= mode
        , "phase" .= fmap (show . (`word` 0)) status
        , "lastMarkPrice" .= fmap (show . (`word` 1)) status
        , "lastMarkTimestamp" .= fmap (`word` 2) status
        , "protocolOracleFrozen" .= statusOracleFrozen
        , "housePoolOracleFrozen" .= poolOracleFrozen
        , "markFresh" .= fmap ((== 1) . (`word` 8)) pool
        , "fadWindow" .= fadWindow
        , "tradingActive" .= tradingActive
        , "withdrawalLive" .= fmap ((== 1) . (`word` 6)) status
        , "degradedMode" .= fmap ((== 1) . (`word` 10)) pool
        , "provenance" .= object
            [ "sourceBlockNumber" .=
                presentEvidence
                  "exact_confirmed_terminal_block_reference"
                  terminalBlock
            , "state" .= aggregateStateEvidence
            , "protocolStatus" .= statusEvidence
            , "housePool" .= poolEvidence
            , "phase" .= statusEvidence
            , "lastMarkPrice" .= statusEvidence
            , "lastMarkTimestamp" .= statusEvidence
            , "protocolOracleFrozen" .= statusEvidence
            , "housePoolOracleFrozen" .= poolEvidence
            , "markFresh" .= poolEvidence
            , "fadWindow" .= statusEvidence
            , "tradingActive" .= statusEvidence
            , "withdrawalLive" .= statusEvidence
            , "degradedMode" .= poolEvidence
            , "marketMode" .= marketModeEvidence
            ]
        , "availability" .=
            orderTerminalMarketStateAvailability
              ""
              statusRead
              poolRead
        , "units" .= object
            [ "lastMarkPrice" .= ("indexPrice:8" :: Text)
            , "lastMarkTimestamp" .= ("unix_seconds" :: Text)
            ]
        ]

orderTerminalMarketStateAvailability
  :: Text
  -> StateRead [Integer]
  -> StateRead [Integer]
  -> [Value]
orderTerminalMarketStateAvailability prefix statusRead poolRead =
  stateReadAvailability (path "protocolStatus") statusRead
    <> stateReadAvailability (path "housePool") poolRead
    <> [ unavailable
          (path "marketMode")
          "terminal_market_state_sources_unavailable"
       | srValue statusRead == Nothing || srValue poolRead == Nothing
       ]
  where
    path fieldName
      | T.null prefix = fieldName
      | otherwise = prefix <> "." <> fieldName

stateReadComplete :: StateRead a -> Bool
stateReadComplete StateRead {srValue = Just _, srUnavailableReason = Nothing} = True
stateReadComplete _ = False

stateReadEvidenceCompleteness :: StateRead a -> Text
stateReadEvidenceCompleteness result@StateRead {srValue = Just _}
  | stateReadComplete result = "exact"
  | otherwise = "partial"
stateReadEvidenceCompleteness _ = "unavailable"

stateReadEvidenceLabel :: Text -> StateRead a -> Text
stateReadEvidenceLabel exactLabel result =
  case stateReadEvidenceCompleteness result of
    "exact" -> exactLabel
    "partial" -> "partial"
    _ -> "unavailable"

orderSlippageBoundary :: Maybe OrderCommitIntent -> Maybe Integer -> Value
orderSlippageBoundary Nothing _ = Null
orderSlippageBoundary (Just intent) executionPrice =
  let side = ociSide intent
      acceptablePrice = ociAcceptablePrice intent
      isClose = ociIsClose intent
      executionMustNotExceed =
        (side == 0 && not isClose)
          || (side == 1 && isClose)
      comparison =
        if executionMustNotExceed
          then ("execution_price_lte_acceptable_price" :: Text)
          else "execution_price_gte_acceptable_price"
      satisfied = do
        execution <- executionPrice
        if acceptablePrice == 0
          then Nothing
          else
            pure $
              if executionMustNotExceed
                then execution <= acceptablePrice
                else execution >= acceptablePrice
   in object
        [ "configured" .= (acceptablePrice /= 0)
        , "acceptablePrice" .= show acceptablePrice
        , "executionPrice" .= fmap show executionPrice
        , "comparison" .= comparison
        , "satisfied" .= satisfied
        , "formulaIdentifier" .= ("protocol.order.slippage_boundary.v1" :: Text)
        , "formula" .=
            ("LONG open and SHORT close require execution <= acceptable; SHORT open and LONG close require execution >= acceptable." :: Text)
        , "provenance" .= object
            [ "acceptablePrice" .= ociEvidence intent
            , "executionPrice" .= ("exact_terminal_event_when_present" :: Text)
            , "satisfied" .= ("derived" :: Text)
            ]
        , "units" .= object
            [ "acceptablePrice" .= ("indexPrice:8" :: Text)
            , "executionPrice" .= ("indexPrice:8" :: Text)
            ]
        ]

orderSlippageAvailability :: Maybe OrderCommitIntent -> Maybe Integer -> [Value]
orderSlippageAvailability Nothing _ =
  [unavailable "terminal.slippageBoundary" "commitment_intent_unavailable"]
orderSlippageAvailability (Just intent) executionPrice =
  [ unavailable "terminal.slippageBoundary.satisfied" "acceptable_price_not_configured"
  | ociAcceptablePrice intent == 0
  ]
    <> [ unavailable "terminal.slippageBoundary.executionPrice" "terminal_execution_price_unavailable"
       | executionPrice == Nothing
       ]
    <> [ unavailable "terminal.slippageBoundary.satisfied" "terminal_execution_price_unavailable"
       | executionPrice == Nothing
       ]

orderStateDelta :: EthClient -> ProtocolRelease -> PerpsOrderRow -> IO OrderStateDelta
orderStateDelta client release order =
  case (porTerminalBlockNumber order, porAccount order) of
    (Just blockNumber, Just account) -> do
      let beforeBlockNumber = max 0 $ blockNumber - 1
      beforeBlockRef <- resolveCanonicalBlockRef client $ Just beforeBlockNumber
      afterBlockRef <- resolveCanonicalBlockRef client $ Just blockNumber
      positionBefore <- accountSnapshotAt client release account beforeBlockRef
      positionAfter <- accountSnapshotAt client release account afterBlockRef
      poolBefore <- mapStateRead poolLiquidityJson <$> poolLiquidityAt client release beforeBlockRef
      poolAfter <- mapStateRead poolLiquidityJson <$> poolLiquidityAt client release afterBlockRef
      seniorBefore <- trancheStateAt client release "senior" beforeBlockRef poolBefore
      seniorAfter <- trancheStateAt client release "senior" afterBlockRef poolAfter
      juniorBefore <- trancheStateAt client release "junior" beforeBlockRef poolBefore
      juniorAfter <- trancheStateAt client release "junior" afterBlockRef poolAfter
      pure OrderStateDelta
        { osdPositionBefore = srValue positionBefore
        , osdPositionAfter = srValue positionAfter
        , osdPositionBeforeReason = srUnavailableReason positionBefore
        , osdPositionAfterReason = srUnavailableReason positionAfter
        , osdPoolBefore = srValue poolBefore
        , osdPoolAfter = srValue poolAfter
        , osdPoolBeforeReason = srUnavailableReason poolBefore
        , osdPoolAfterReason = srUnavailableReason poolAfter
        , osdSeniorBefore = srValue seniorBefore
        , osdSeniorAfter = srValue seniorAfter
        , osdSeniorBeforeReason = srUnavailableReason seniorBefore
        , osdSeniorAfterReason = srUnavailableReason seniorAfter
        , osdJuniorBefore = srValue juniorBefore
        , osdJuniorAfter = srValue juniorAfter
        , osdJuniorBeforeReason = srUnavailableReason juniorBefore
        , osdJuniorAfterReason = srUnavailableReason juniorAfter
        , osdBeforeBlockNumber = Just beforeBlockNumber
        , osdBeforeBlockHash = canonicalBlockHash <$> beforeBlockRef
        , osdAfterBlockNumber = Just blockNumber
        , osdAfterBlockHash = canonicalBlockHash <$> afterBlockRef
        , osdTerminalTxHash = porTerminalTxHash order
        , osdCalculationVersion = prCalculationVersion release
        }
    _ -> pure OrderStateDelta
      { osdPositionBefore = Nothing
      , osdPositionAfter = Nothing
      , osdPositionBeforeReason = Just "terminal_block_or_account_unavailable"
      , osdPositionAfterReason = Just "terminal_block_or_account_unavailable"
      , osdPoolBefore = Nothing
      , osdPoolAfter = Nothing
      , osdPoolBeforeReason = Just "terminal_block_unavailable"
      , osdPoolAfterReason = Just "terminal_block_unavailable"
      , osdSeniorBefore = Nothing
      , osdSeniorAfter = Nothing
      , osdSeniorBeforeReason = Just "terminal_block_unavailable"
      , osdSeniorAfterReason = Just "terminal_block_unavailable"
      , osdJuniorBefore = Nothing
      , osdJuniorAfter = Nothing
      , osdJuniorBeforeReason = Just "terminal_block_unavailable"
      , osdJuniorAfterReason = Just "terminal_block_unavailable"
      , osdBeforeBlockNumber = Nothing
      , osdBeforeBlockHash = Nothing
      , osdAfterBlockNumber = Nothing
      , osdAfterBlockHash = Nothing
      , osdTerminalTxHash = porTerminalTxHash order
      , osdCalculationVersion = prCalculationVersion release
      }

stateDeltaJson :: OrderStateDelta -> Value
stateDeltaJson state@OrderStateDelta {..} =
  object
    [ "position" .=
        stateImpactPairJson
          StateImpactPair
            { sipBefore = osdPositionBefore
            , sipAfter = osdPositionAfter
            , sipBeforeReason = osdPositionBeforeReason
            , sipAfterReason = osdPositionAfterReason
            }
    , "housePool" .=
        stateImpactPairJson
          StateImpactPair
            { sipBefore = osdPoolBefore
            , sipAfter = osdPoolAfter
            , sipBeforeReason = osdPoolBeforeReason
            , sipAfterReason = osdPoolAfterReason
            }
    , "senior" .=
        stateImpactPairJson
          StateImpactPair
            { sipBefore = osdSeniorBefore
            , sipAfter = osdSeniorAfter
            , sipBeforeReason = osdSeniorBeforeReason
            , sipAfterReason = osdSeniorAfterReason
            }
    , "junior" .=
        stateImpactPairJson
          StateImpactPair
            { sipBefore = osdJuniorBefore
            , sipAfter = osdJuniorAfter
            , sipBeforeReason = osdJuniorBeforeReason
            , sipAfterReason = osdJuniorAfterReason
            }
    , "sourceBlocks" .= object
        [ "before" .= object
            [ "number" .= fmap show osdBeforeBlockNumber
            , "hash" .= osdBeforeBlockHash
            ]
        , "after" .= object
            [ "number" .= fmap show osdAfterBlockNumber
            , "hash" .= osdAfterBlockHash
            ]
        ]
    , "formulaIdentifier" .= ("protocol.state.block_before_after.v1" :: Text)
    , "formula" .= ("state(block - 1) compared with state(block); values are not transaction-attributed when multiple protocol transactions share the block" :: Text)
    , "provenance" .= orderStateDeltaEvidenceLabel state
    , "calculationVersion" .= osdCalculationVersion
    , "sourceBlock" .= fmap show osdAfterBlockNumber
    , "sourceBlockHash" .= osdAfterBlockHash
    , "evidenceReferences" .=
        catMaybes
          [ fmap (\txHash -> object ["type" .= ("transaction" :: Text), "transactionHash" .= txHash]) osdTerminalTxHash
          , fmap
              (\blockNumber ->
                object
                  [ "type" .= ("historical_contract_reads" :: Text)
                  , "beforeBlockNumber" .= fmap show osdBeforeBlockNumber
                  , "beforeBlockHash" .= osdBeforeBlockHash
                  , "afterBlockNumber" .= show blockNumber
                  , "afterBlockHash" .= osdAfterBlockHash
                  , "contracts" .=
                      (["AccountLens", "HousePool", "SeniorVault", "JuniorVault"] :: [Text])
                  ])
              osdAfterBlockNumber
          ]
    ]

orderStateDeltaEvidenceLabel :: OrderStateDelta -> Text
orderStateDeltaEvidenceLabel state
  | all stateImpactPairComplete pairs
      && osdBeforeBlockNumber state /= Nothing
      && osdBeforeBlockHash state /= Nothing
      && osdAfterBlockNumber state /= Nothing
      && osdAfterBlockHash state /= Nothing =
      "canonical_hash_bound_block-level_delta"
  | any stateImpactPairHasEvidence pairs =
      "partial_canonical_hash_bound_block-level_state"
  | otherwise = "unavailable"
  where
    pairs =
      [ StateImpactPair
          (osdPositionBefore state)
          (osdPositionAfter state)
          (osdPositionBeforeReason state)
          (osdPositionAfterReason state)
      , StateImpactPair
          (osdPoolBefore state)
          (osdPoolAfter state)
          (osdPoolBeforeReason state)
          (osdPoolAfterReason state)
      , StateImpactPair
          (osdSeniorBefore state)
          (osdSeniorAfter state)
          (osdSeniorBeforeReason state)
          (osdSeniorAfterReason state)
      , StateImpactPair
          (osdJuniorBefore state)
          (osdJuniorAfter state)
          (osdJuniorBeforeReason state)
          (osdJuniorAfterReason state)
      ]

stateDeltaAvailability :: OrderStateDelta -> [Value]
stateDeltaAvailability OrderStateDelta {..} =
  missingState "stateImpact.position.before" osdPositionBefore osdPositionBeforeReason
    <> missingState "stateImpact.position.after" osdPositionAfter osdPositionAfterReason
    <> missingState "stateImpact.housePool.before" osdPoolBefore osdPoolBeforeReason
    <> missingState "stateImpact.housePool.after" osdPoolAfter osdPoolAfterReason
    <> missingState "stateImpact.senior.before" osdSeniorBefore osdSeniorBeforeReason
    <> missingState "stateImpact.senior.after" osdSeniorAfter osdSeniorAfterReason
    <> missingState "stateImpact.junior.before" osdJuniorBefore osdJuniorBeforeReason
    <> missingState "stateImpact.junior.after" osdJuniorAfter osdJuniorAfterReason
    <> [unavailable "stateImpact.sourceBlocks.before.hash" "source_block_header_unavailable" | osdBeforeBlockNumber /= Nothing && osdBeforeBlockHash == Nothing]
    <> [unavailable "stateImpact.sourceBlocks.after.hash" "source_block_header_unavailable" | osdAfterBlockNumber /= Nothing && osdAfterBlockHash == Nothing]
  where
    missingState fieldName value reason =
      case (value, reason) of
        (Nothing, _) -> [unavailable fieldName (stateDeltaReason reason)]
        (Just _, Just partialReason) ->
          [unavailable fieldName (stateDeltaReason $ Just partialReason)]
        (Just _, Nothing) -> []

    stateDeltaReason (Just reason)
      | "malformed_" `T.isPrefixOf` reason = reason
      | "noncanonical_" `T.isPrefixOf` reason = reason
      | "canonical_block_" `T.isPrefixOf` reason = reason
      | "transaction_block_" `T.isPrefixOf` reason = reason
      | "partial_" `T.isPrefixOf` reason = reason
      | otherwise = "archive_state_unavailable"
    stateDeltaReason Nothing = "archive_state_unavailable"

accountSnapshotAt :: EthClient -> ProtocolRelease -> Text -> Maybe CanonicalBlockRef -> IO (StateRead Value)
accountSnapshotAt client release account blockRef = do
  rawResult <-
    callAtExactWords
      client
      (prAccountLens release)
      "getAccountLedgerSnapshot(address)"
      [encodeAddress account]
      blockRef
      23
  let result =
        validateStateWords
          "malformed_account_snapshot_narrow_word"
          (\words' ->
            canonicalBoolean (word words' 15)
              && word words' 16 <= 255
              && canonicalBoolean (word words' 22))
          rawResult
  pure $ mapStateRead
    (\words' -> object
        [ "settlementBalanceUsdc" .= show (word words' 0)
        , "freeSettlementUsdc" .= show (word words' 1)
        , "activePositionMarginUsdc" .= show (word words' 2)
        , "committedMarginUsdc" .= show (word words' 8)
        , "traderClaimBalanceUsdc" .= show (word words' 9)
        , "pendingOrderCount" .= show (word words' 10)
        , "closeReachableUsdc" .= show (word words' 11)
        , "terminalReachableUsdc" .= show (word words' 12)
        , "accountEquityUsdc" .= show (word words' 13)
        , "freeBuyingPowerUsdc" .= show (word words' 14)
        , "hasPosition" .= (word words' 15 /= 0)
        , "side" .= show (word words' 16)
        , "size" .= show (word words' 17)
        , "marginUsdc" .= show (word words' 18)
        , "entryPrice" .= show (word words' 19)
        , "unrealizedPnlUsdc" .= show (signedWord words' 20)
        , "netEquityUsdc" .= show (signedWord words' 21)
        , "liquidatable" .= (word words' 22 /= 0)
        , "units" .= object
            [ "settlementBalanceUsdc" .= ("USDC:6" :: Text)
            , "freeSettlementUsdc" .= ("USDC:6" :: Text)
            , "activePositionMarginUsdc" .= ("USDC:6" :: Text)
            , "committedMarginUsdc" .= ("USDC:6" :: Text)
            , "marginUsdc" .= ("USDC:6" :: Text)
            , "closeReachableUsdc" .= ("USDC:6" :: Text)
            , "terminalReachableUsdc" .= ("USDC:6" :: Text)
            , "traderClaimBalanceUsdc" .= ("USDC:6" :: Text)
            , "accountEquityUsdc" .= ("USDC:6 signed" :: Text)
            , "freeBuyingPowerUsdc" .= ("USDC:6 signed" :: Text)
            , "unrealizedPnlUsdc" .= ("USDC:6 signed" :: Text)
            , "netEquityUsdc" .= ("USDC:6 signed" :: Text)
            , "entryPrice" .= ("indexPrice:8" :: Text)
            , "size" .= ("position:18" :: Text)
            , "pendingOrderCount" .= ("count" :: Text)
            , "side" .= ("market_side_enum" :: Text)
            ]
        ])
    result

orderEconomics :: Integer -> Maybe [Integer] -> [ProtocolActionRow] -> Value
orderEconomics orderId pendingOrderWords actions =
  let receiptEconomics = orderFinalizedEconomics orderId actions
      exact keyName = receiptEconomics >>= objectField keyName
      legacyRealizedPnl = firstDataMaybe "pnl" actions
      realizedPnl = exact "realizedPnlUsdc" <|> legacyRealizedPnl
      intentBounty = orderIntentData orderId "executionBountyUsdc" actions
      historicalReward =
        String . T.pack . show . (`word` 9) <$> pendingOrderWords
      executionReward = intentBounty <|> historicalReward
      keeperBounty = firstDataMaybe "keeperBountyUsdc" actions
      claimDeltas = traderClaimDeltas receiptEconomics
      claimCreated = fst <$> claimDeltas
      claimConsumed = snd <$> claimDeltas
      exactReceiptEvidence keyName =
        presentEvidence
          "exact_confirmed_order_finalized_event"
          (exact keyName)
      usdcFields =
        [ "executionNotionalUsdc"
        , "vpiUsdc"
        , "carryUsdc"
        , "executionFeeUsdc"
        , "frozenSpreadUsdc"
        , "actionChargeAssessedUsdc"
        , "actionChargeCollectedUsdc"
        , "grossAccountDebitUsdc"
        , "preSettlementBalanceUsdc"
        , "postSettlementBalanceUsdc"
        , "preTraderClaimBalanceUsdc"
        , "postTraderClaimBalanceUsdc"
        , "postPositionMarginUsdc"
        , "postPositionEquityUsdc"
        ]
      receiptPairs =
        [ Key.fromText keyName .= fromMaybe Null (exact keyName)
        | keyName <- usdcFields
        ]
          <> [ "postPositionSize" .= fromMaybe Null (exact "postPositionSize")
             , "postLeverageBps" .= fromMaybe Null (exact "postLeverageBps")
             ]
      receiptEvidencePairs =
        [ Key.fromText keyName .= exactReceiptEvidence keyName
        | keyName <- usdcFields <> ["postPositionSize", "postLeverageBps"]
        ]
      unitPairs =
        [ Key.fromText keyName .= ("USDC:6" :: Text)
        | keyName <- usdcFields
        ]
   in object $
        [ "realizedPnlUsdc" .= fromMaybe Null realizedPnl
        , "executionRewardUsdc" .= fromMaybe Null executionReward
        , "protocolFeeUsdc" .= Null
        , "immediatePayoutUsdc" .= Null
        , "claimCreatedUsdc" .= maybe Null (String . T.pack . show) claimCreated
        , "claimConsumedUsdc" .= maybe Null (String . T.pack . show) claimConsumed
        , "seizedCollateralUsdc" .= Null
        , "badDebtUsdc" .= Null
        , "keeperBountyUsdc" .= fromMaybe Null keeperBounty
        , "provenance" .= object
            ( [ "realizedPnlUsdc" .=
                  case exact "realizedPnlUsdc" of
                    Just _ -> ("exact_confirmed_order_finalized_event" :: Text)
                    Nothing -> presentEvidence "exact_event" legacyRealizedPnl
              , "executionRewardUsdc" .=
                  case intentBounty of
                    Just _ -> ("exact_confirmed_intent_registered_event" :: Text)
                    Nothing ->
                      presentEvidence
                        "exact_historical_pending_order_read"
                        historicalReward
              , "keeperBountyUsdc" .=
                  presentEvidence "exact_event" keeperBounty
              , "claimCreatedUsdc" .=
                  presentEvidence
                    "derived_from_exact_order_finalized_claim_balances"
                    claimCreated
              , "claimConsumedUsdc" .=
                  presentEvidence
                    "derived_from_exact_order_finalized_claim_balances"
                    claimConsumed
              , "protocolFeeUsdc" .= ("unavailable" :: Text)
              , "immediatePayoutUsdc" .= ("unavailable" :: Text)
              , "seizedCollateralUsdc" .= ("unavailable" :: Text)
              , "badDebtUsdc" .= ("unavailable" :: Text)
              , "feeBreakdown" .=
                  if receiptEconomics == Nothing
                    then ("unavailable" :: Text)
                    else "exact_confirmed_order_finalized_event"
              ]
                <> receiptEvidencePairs
            )
        , "units" .= object
            ( [ "executionRewardUsdc" .= ("USDC:6" :: Text)
              , "realizedPnlUsdc" .= ("USDC:6 signed" :: Text)
              , "keeperBountyUsdc" .= ("USDC:6" :: Text)
              , "protocolFeeUsdc" .= ("USDC:6" :: Text)
              , "immediatePayoutUsdc" .= ("USDC:6" :: Text)
              , "claimCreatedUsdc" .= ("USDC:6" :: Text)
              , "claimConsumedUsdc" .= ("USDC:6" :: Text)
              , "seizedCollateralUsdc" .= ("USDC:6" :: Text)
              , "badDebtUsdc" .= ("USDC:6" :: Text)
              , "postPositionSize" .= ("position:18" :: Text)
              , "postLeverageBps" .= ("basis_points" :: Text)
              ]
                <> unitPairs
            )
        ]
          <> receiptPairs

orderEconomicsAvailability
  :: Integer
  -> Maybe [Integer]
  -> [ProtocolActionRow]
  -> [Value]
orderEconomicsAvailability orderId pendingOrderWords actions =
  let receiptEconomics = orderFinalizedEconomics orderId actions
      exact keyName = receiptEconomics >>= objectField keyName
      intentBounty = orderIntentData orderId "executionBountyUsdc" actions
      legacyRealizedPnl = firstDataMaybe "pnl" actions
      claimDeltas = traderClaimDeltas receiptEconomics
      exactReceiptFields =
        [ "executionNotionalUsdc"
        , "vpiUsdc"
        , "carryUsdc"
        , "executionFeeUsdc"
        , "frozenSpreadUsdc"
        , "actionChargeAssessedUsdc"
        , "actionChargeCollectedUsdc"
        , "grossAccountDebitUsdc"
        , "preSettlementBalanceUsdc"
        , "postSettlementBalanceUsdc"
        , "preTraderClaimBalanceUsdc"
        , "postTraderClaimBalanceUsdc"
        , "postPositionSize"
        , "postPositionMarginUsdc"
        , "postPositionEquityUsdc"
        , "postLeverageBps"
        ]
   in [ unavailable
          "economics.executionRewardUsdc"
          "intent_event_and_pending_order_snapshot_unavailable"
      | intentBounty == Nothing && pendingOrderWords == Nothing
      ]
        <> [ unavailable
               "economics.realizedPnlUsdc"
               "order_finalized_receipt_and_legacy_event_value_unavailable"
           | exact "realizedPnlUsdc" == Nothing
           , legacyRealizedPnl == Nothing
           ]
        <> [ unavailable
               ("economics." <> keyName)
               "order_finalized_receipt_economics_unavailable"
           | keyName <- exactReceiptFields
           , exact keyName == Nothing
           ]
        <> [ unavailable
               fieldName
               "order_finalized_claim_balances_unavailable"
           | claimDeltas == Nothing
           , fieldName <-
               [ "economics.claimCreatedUsdc"
               , "economics.claimConsumedUsdc"
               ]
           ]
        <> [unavailable "economics.protocolFeeUsdc" "receipt_does_not_isolate_protocol_fee"]
        <> [unavailable "economics.immediatePayoutUsdc" "receipt_does_not_isolate_immediate_payout"]
        <> [unavailable "economics.seizedCollateralUsdc" "receipt_does_not_emit_seized_collateral"]
        <> [unavailable "economics.badDebtUsdc" "receipt_does_not_emit_bad_debt"]

orderFinalizedEconomics :: Integer -> [ProtocolActionRow] -> Maybe Value
orderFinalizedEconomics orderId actions = do
  action <-
    find
      (\candidate ->
        parOrderId candidate == Just orderId
          && objectField "receiptHash" (parData candidate) /= Nothing
          && objectField "receipt" (parData candidate) /= Nothing
      )
      actions
  objectField "economics" $ parData action

orderIntentData :: Integer -> Text -> [ProtocolActionRow] -> Maybe Value
orderIntentData orderId keyName actions =
  listToMaybe
    [ fieldValue
    | action <- actions
    , parOrderId action == Just orderId
    , objectField "intentHash" (parData action) /= Nothing
    , Just fieldValue <- [objectField keyName $ parData action]
    ]

traderClaimDeltas :: Maybe Value -> Maybe (Integer, Integer)
traderClaimDeltas receiptEconomics = do
  economics <- receiptEconomics
  before <- objectSignedIntegerField "preTraderClaimBalanceUsdc" economics
  after <- objectSignedIntegerField "postTraderClaimBalanceUsdc" economics
  pure (max 0 (after - before), max 0 (before - after))

firstDataMaybe :: Text -> [ProtocolActionRow] -> Maybe Value
firstDataMaybe keyName actions =
  listToMaybe $ catMaybes $ map (objectField keyName . parData) actions

keeperTransactionCostJson :: Maybe ProtocolTransactionRow -> Value
keeperTransactionCostJson Nothing = Null
keeperTransactionCostJson (Just transaction) =
  let gasCostWei = (*) <$> ptrGasUsed transaction <*> ptrEffectiveGasPrice transaction
   in object
        [ "gasCostWei" .= fmap show gasCostWei
        , "transactionNativeValueWei" .= fmap show (ptrNativeValue transaction)
        , "pythFeeWei" .= Null
        , "profitUsdc" .= Null
        , "formulaIdentifier" .= ("protocol.keeper.transaction_native_cost.v1" :: Text)
        , "formula" .= ("gasUsed * effectiveGasPrice; transaction native value is reported separately" :: Text)
        , "provenance" .= object
            [ "gasCostWei" .= presentEvidence "exact_transaction_receipt" gasCostWei
            , "transactionNativeValueWei" .=
                presentEvidence "exact_transaction_input" (ptrNativeValue transaction)
            , "pythFeeWei" .= ("unavailable" :: Text)
            , "profitUsdc" .= ("unavailable" :: Text)
            ]
        , "units" .= object
            [ "gasCostWei" .= ("wei" :: Text)
            , "transactionNativeValueWei" .= ("wei" :: Text)
            , "pythFeeWei" .= ("wei" :: Text)
            , "profitUsdc" .= ("USDC:6" :: Text)
            ]
        ]

keeperTransactionCostAvailability :: Maybe ProtocolTransactionRow -> [Value]
keeperTransactionCostAvailability Nothing =
  [unavailable "keeperEconomics" "terminal_transaction_unavailable"]
keeperTransactionCostAvailability (Just transaction) =
  [ unavailable "keeperEconomics.gasCostWei" "receipt_gas_cost_unavailable"
  | ptrGasUsed transaction == Nothing
      || ptrEffectiveGasPrice transaction == Nothing
  ]
    <> [ unavailable "keeperEconomics.transactionNativeValueWei" "transaction_native_value_unavailable"
       | ptrNativeValue transaction == Nothing
       ]
    <> [ unavailable "keeperEconomics.pythFeeWei" "pyth_fee_component_not_isolated"
       , unavailable "keeperEconomics.profitUsdc" "historical_native_to_usdc_conversion_unavailable"
       ]

liquidationDetails :: PerpsOrderRow -> [ProtocolActionRow] -> OrderStateDelta -> Value
liquidationDetails order actions stateDelta
  | porActivityType order /= Just "Liquidated" = Null
  | otherwise =
      let observedEligibility =
            osdPositionBefore stateDelta >>= objectField "liquidatable"
          bounty = firstData "keeperBountyUsdc" actions
          reachableCollateral =
            osdPositionBefore stateDelta >>= objectField "terminalReachableUsdc"
          traderResidual =
            osdPositionAfter stateDelta >>= objectField "traderClaimBalanceUsdc"
          poolPair =
            StateImpactPair
              { sipBefore = osdPoolBefore stateDelta
              , sipAfter = osdPoolAfter stateDelta
              , sipBeforeReason = osdPoolBeforeReason stateDelta
              , sipAfterReason = osdPoolAfterReason stateDelta
              }
       in object
        [ "observedEligibility" .= (osdPositionBefore stateDelta >>= objectField "liquidatable")
        , "executionPrice" .= fmap show (porExecutionPrice order)
        , "bountyUsdc" .= bounty
        , "marginRegime" .= Null
        , "adverseConfidencePricing" .= Null
        , "reachableCollateralUsdc" .= reachableCollateral
        , "clearedPendingOrders" .= clearedPendingOrderCount stateDelta
        , "forfeitedRewardsUsdc" .= Null
        , "traderResidualOrClaimUsdc" .= traderResidual
        , "badDebtUsdc" .= Null
        , "poolSolvencyImpact" .= object
            [ "before" .= osdPoolBefore stateDelta
            , "after" .= osdPoolAfter stateDelta
            , "provenance" .= stateImpactPairEvidenceLabel poolPair
            ]
        , "provenance" .= object
            [ "observedEligibility" .=
                presentEvidence "block-level state" observedEligibility
            , "executionPrice" .=
                presentEvidence "exact_confirmed_log_projection" (porExecutionPrice order)
            , "bountyUsdc" .=
                if bounty == Null
                  then ("unavailable" :: Text)
                  else "exact_event"
            , "reachableCollateralUsdc" .=
                presentEvidence "block-level state" reachableCollateral
            , "clearedPendingOrders" .=
                presentEvidence "block-level state" (clearedPendingOrderCount stateDelta)
            , "traderResidualOrClaimUsdc" .=
                presentEvidence "block-level state" traderResidual
            , "poolSolvencyImpact" .= stateImpactPairEvidenceLabel poolPair
            , "settlementBreakdown" .= ("unavailable" :: Text)
            ]
        , "units" .= object
            [ "observedEligibility" .= ("boolean" :: Text)
            , "executionPrice" .= ("indexPrice:8" :: Text)
            , "bountyUsdc" .= ("USDC:6" :: Text)
            , "marginRegime" .= ("enum" :: Text)
            , "reachableCollateralUsdc" .= ("USDC:6" :: Text)
            , "clearedPendingOrders" .= ("count" :: Text)
            , "forfeitedRewardsUsdc" .= ("USDC:6" :: Text)
            , "traderResidualOrClaimUsdc" .= ("USDC:6" :: Text)
            , "badDebtUsdc" .= ("USDC:6" :: Text)
            ]
        ]

liquidationAvailability :: PerpsOrderRow -> [ProtocolActionRow] -> OrderStateDelta -> [Value]
liquidationAvailability order actions stateDelta
  | porActivityType order /= Just "Liquidated" = []
  | otherwise =
      [ unavailable "liquidation.marginRegime" "historical_margin_regime_not_reconstructed"
      , unavailable "liquidation.adverseConfidencePricing" "oracle_confidence_components_not_indexed"
      , unavailable "liquidation.forfeitedRewardsUsdc" "cleared_order_reward_telemetry_missing"
      , unavailable "liquidation.badDebtUsdc" "current_release_settlement_telemetry_missing"
      ]
        <> [ unavailable "liquidation.observedEligibility" "archive_state_unavailable"
           | (osdPositionBefore stateDelta >>= objectField "liquidatable") == Nothing
           ]
        <> [ unavailable "liquidation.executionPrice" "liquidation_execution_price_unavailable"
           | porExecutionPrice order == Nothing
           ]
        <> [ unavailable "liquidation.bountyUsdc" "liquidation_bounty_event_value_unavailable"
           | firstData "keeperBountyUsdc" actions == Null
           ]
        <> [ unavailable "liquidation.reachableCollateralUsdc" "archive_state_unavailable"
           | (osdPositionBefore stateDelta >>= objectField "terminalReachableUsdc") == Nothing
           ]
        <> [ unavailable "liquidation.clearedPendingOrders" "archive_state_unavailable"
           | clearedPendingOrderCount stateDelta == Nothing
           ]
        <> [ unavailable "liquidation.traderResidualOrClaimUsdc" "archive_state_unavailable"
           | (osdPositionAfter stateDelta >>= objectField "traderClaimBalanceUsdc") == Nothing
           ]
        <> [ unavailable "liquidation.poolSolvencyImpact.before" "archive_state_unavailable"
           | osdPoolBefore stateDelta == Nothing
           ]
        <> [ unavailable "liquidation.poolSolvencyImpact.after" "archive_state_unavailable"
           | osdPoolAfter stateDelta == Nothing
           ]

clearedPendingOrderCount :: OrderStateDelta -> Maybe Text
clearedPendingOrderCount stateDelta = do
  before <- osdPositionBefore stateDelta >>= objectIntegerField "pendingOrderCount"
  after <- osdPositionAfter stateDelta >>= objectIntegerField "pendingOrderCount"
  pure $ T.pack $ show $ max 0 (before - after)

objectIntegerField :: Text -> Value -> Maybe Integer
objectIntegerField key value = do
  fieldValue <- objectField key value
  case fieldValue of
    String text
      | not (T.null text)
          && T.all (\character -> character >= '0' && character <= '9') text ->
          Just $ read $ T.unpack text
    Number scientific ->
      case (floatingOrInteger scientific :: Either Double Integer) of
        Right integer -> Just integer
        Left _ -> Nothing
    _ -> Nothing

poolLiquidityAt :: EthClient -> ProtocolRelease -> Maybe CanonicalBlockRef -> IO (StateRead [Integer])
poolLiquidityAt client release blockRef = do
  result <-
    callAtExactWords
      client
      (prHousePool release)
      "getPoolLiquidityView()"
      []
      blockRef
      11
  pure $
    validateStateWords
      "malformed_pool_liquidity_boolean_word"
      (\words' -> all (canonicalBoolean . word words') [8, 9, 10])
      result

poolLiquidityJson :: [Integer] -> Value
poolLiquidityJson words' =
  object
    [ "totalAssetsUsdc" .= show (word words' 0)
    , "freeUsdc" .= show (word words' 1)
    , "withdrawalReservedUsdc" .= show (word words' 2)
    , "pendingRecapitalizationUsdc" .= show (word words' 3)
    , "pendingTradingRevenueUsdc" .= show (word words' 4)
    , "seniorPrincipalUsdc" .= show (word words' 5)
    , "juniorPrincipalUsdc" .= show (word words' 6)
    , "seniorHighWaterMarkUsdc" .= show (word words' 7)
    , "markFresh" .= (word words' 8 /= 0)
    , "oracleFrozen" .= (word words' 9 /= 0)
    , "degradedMode" .= (word words' 10 /= 0)
    , "unit" .= ("USDC:6" :: Text)
    ]

protocolStatusAt :: EthClient -> ProtocolRelease -> Maybe CanonicalBlockRef -> IO (StateRead [Integer])
protocolStatusAt client release blockRef = do
  result <-
    callAtExactWords
      client
      (prPublicLens release)
      "getProtocolStatus()"
      []
      blockRef
      7
  pure $
    validateStateWords
      "malformed_protocol_status_narrow_word"
      (\words' ->
        word words' 0 <= 255
          && word words' 2 <= maxUint64
          && all (canonicalBoolean . word words') [3, 4, 5, 6])
      result

protocolStatusJson :: [Integer] -> Value
protocolStatusJson words' =
  object
    [ "phase" .= show (word words' 0)
    , "lastMarkPrice" .= show (word words' 1)
    , "lastMarkTimestamp" .= word words' 2
    , "oracleFrozen" .= (word words' 3 /= 0)
    , "fadWindow" .= (word words' 4 /= 0)
    , "tradingActive" .= (word words' 5 /= 0)
    , "withdrawalLive" .= (word words' 6 /= 0)
    , "units" .= object ["lastMarkPrice" .= ("indexPrice:8" :: Text), "lastMarkTimestamp" .= ("unix_seconds" :: Text)]
    ]

sidesAt :: EthClient -> ProtocolRelease -> Integer -> Maybe CanonicalBlockRef -> IO (StateRead [Integer])
sidesAt client release side blockRef =
  callAtExactWords client (prCfdEngine release) "sides(uint256)" [encodeUintWord side] blockRef 4

callAtExactWords :: EthClient -> Text -> Text -> [BS.ByteString] -> Maybe CanonicalBlockRef -> Int -> IO (StateRead [Integer])
callAtExactWords client address signature args blockRef expectedWordCount =
  case blockRef of
    Nothing ->
      pure $ StateRead Nothing $ Just "canonical_block_anchor_unavailable"
    Just canonicalRef -> do
      result <-
        ethCallAtCanonicalBlock
          client
          (CallParams address $ encodeCall signature args)
          canonicalRef
      pure $ case result of
        Left _ -> StateRead Nothing $ Just "canonical_block_state_unavailable"
        Right bytes
          | BS.null bytes -> StateRead Nothing $ Just "empty_contract_return"
          | BS.length bytes `mod` 32 /= 0 -> StateRead Nothing $ Just "malformed_abi_return_bytes"
          | otherwise ->
              let words' = decodeWords bytes
               in if length words' == expectedWordCount
                    then StateRead (Just words') Nothing
                    else StateRead Nothing $ Just "malformed_abi_return_word_count"

mapStateRead :: (a -> b) -> StateRead a -> StateRead b
mapStateRead transform StateRead {..} =
  StateRead
    { srValue = transform <$> srValue
    , srUnavailableReason
    }

validateStateWords
  :: Text
  -> ([Integer] -> Bool)
  -> StateRead [Integer]
  -> StateRead [Integer]
validateStateWords reason isCanonical result@StateRead {srValue = Just words'}
  | not $ isCanonical words' = StateRead Nothing $ Just reason
  | otherwise = result
validateStateWords _ _ result = result

canonicalBoolean :: Integer -> Bool
canonicalBoolean value = value == 0 || value == 1

stateReadAvailability :: Text -> StateRead a -> [Value]
stateReadAvailability fieldName StateRead {..} =
  case (srValue, srUnavailableReason) of
    (Nothing, Just reason) -> [unavailable fieldName reason]
    (Nothing, Nothing) -> [unavailable fieldName "state_unavailable"]
    (Just _, Just reason) -> [unavailable fieldName reason]
    (Just _, Nothing) -> []

readUintAtExact :: EthClient -> Text -> Text -> Int -> Int -> Maybe CanonicalBlockRef -> IO (Maybe Integer)
readUintAtExact client address signature index expectedWordCount blockRef = do
  srValue
    <$> readUintStateAtExact
      client
      address
      signature
      index
      expectedWordCount
      blockRef

readUintStateAtExact
  :: EthClient
  -> Text
  -> Text
  -> Int
  -> Int
  -> Maybe CanonicalBlockRef
  -> IO (StateRead Integer)
readUintStateAtExact client address signature index expectedWordCount blockRef = do
  result <- callAtExactWords client address signature [] blockRef expectedWordCount
  pure $
    if index < 0 || index >= expectedWordCount
      then StateRead Nothing $ Just "analytics_word_index_out_of_bounds"
      else mapStateRead (`word` index) result

parameterGetterWordCount :: ParameterDefinition -> Int
parameterGetterWordCount definition =
  1
    + maximum
      [ pdWordIndex candidate
      | candidate <- parameterCatalog
      , pdContract candidate == pdContract definition
      , pdGetter candidate == pdGetter definition
      ]

decodeWords :: BS.ByteString -> [Integer]
decodeWords bytes
  | BS.length bytes < 32 = []
  | otherwise = decodeUint256 (BS.take 32 bytes) : decodeWords (BS.drop 32 bytes)

word :: [Integer] -> Int -> Integer
word words' index = if index < length words' then words' !! index else 0

signedWord :: [Integer] -> Int -> Integer
signedWord words' index =
  let unsigned = word words' index
   in if unsigned >= 2 ^ (255 :: Int) then unsigned - 2 ^ (256 :: Int) else unsigned

uintWord :: BS.ByteString -> Int -> Integer
uintWord bytes index = decodeUint256 $ BS.take 32 $ BS.drop (index * 32) bytes

encodeUintWord :: Integer -> BS.ByteString
encodeUintWord value =
  let raw = integerBytes value
   in BS.replicate (32 - BS.length raw) 0 <> raw

integerBytes :: Integer -> BS.ByteString
integerBytes 0 = BS.singleton 0
integerBytes value = BS.pack $ reverse $ go value
  where
    go 0 = []
    go n = fromIntegral (n `mod` 256) : go (n `div` 256)

maxUint64 :: Integer
maxUint64 = 2 ^ (64 :: Int) - 1

decodeHexText :: Text -> Maybe BS.ByteString
decodeHexText text =
  case B16.decode $ TE.encodeUtf8 $ T.toLower text of
    Right bytes -> Just bytes
    Left _ -> Nothing

firstData :: Text -> [ProtocolActionRow] -> Value
firstData keyName actions =
  fromMaybe Null $ listToMaybe $ catMaybes $ map (objectField keyName . parData) actions

objectField :: Text -> Value -> Maybe Value
objectField keyName (Object fields) = KM.lookup (Key.fromText keyName) fields
objectField _ _ = Nothing

transactionStateImpactJson :: TransactionStateImpact -> Value
transactionStateImpactJson TransactionStateImpact {..} =
  object
    [ "accounts" .= map accountStateImpactJson tsiAccounts
    , "housePool" .= stateImpactPairJson tsiHousePool
    , "senior" .= stateImpactPairJson tsiSenior
    , "junior" .= stateImpactPairJson tsiJunior
    , "sourceBlocks" .= object
        [ "before" .= object
            [ "number" .= fmap show tsiBeforeBlockNumber
            , "hash" .= tsiBeforeBlockHash
            ]
        , "after" .= object
            [ "number" .= show tsiAfterBlockNumber
            , "hash" .= tsiAfterBlockHash
            ]
        ]
    , "provenance" .=
        transactionStateImpactEvidenceLabel TransactionStateImpact {..}
    , "transactionAttribution" .=
        ("state(block - 1) compared with state(block); the delta includes every state transition in the block" :: Text)
    , "formulaIdentifier" .= ("protocol.transaction.state_block_before_after.v1" :: Text)
    , "formula" .=
        ("state(block - 1) compared with state(block); values are not transaction-attributed when multiple protocol transactions share the block" :: Text)
    , "calculationVersion" .= tsiCalculationVersion
    , "sourceBlock" .= show tsiAfterBlockNumber
    , "sourceBlockHash" .= tsiAfterBlockHash
    , "evidenceReferences" .=
        [ object
            [ "type" .= ("transaction" :: Text)
            , "transactionHash" .= tsiTransactionHash
            ]
        , object
            [ "type" .= ("historical_contract_reads" :: Text)
            , "beforeBlockNumber" .= fmap show tsiBeforeBlockNumber
            , "beforeBlockHash" .= tsiBeforeBlockHash
            , "afterBlockNumber" .= show tsiAfterBlockNumber
            , "afterBlockHash" .= tsiAfterBlockHash
            , "contracts" .=
                (["AccountLens", "HousePool", "SeniorVault", "JuniorVault"] :: [Text])
            ]
        ]
    , "availability" .=
        transactionStateImpactAvailability
          TransactionStateImpact {..}
    ]

accountStateImpactJson :: AccountStateImpact -> Value
accountStateImpactJson AccountStateImpact {..} =
  object
    [ "account" .= asiAccount
    , "actionIds" .= asiActionIds
    , "actionTypes" .= asiActionTypes
    , "before" .= sipBefore asiState
    , "after" .= sipAfter asiState
    , "deltas" .= stateImpactDeltas asiState
    , "deltaUnits" .= stateImpactDeltaUnits asiState
    , "provenance" .= stateImpactPairEvidenceLabel asiState
    , "availability" .= stateImpactPairAvailability "" asiState
    ]

stateImpactPairJson :: StateImpactPair -> Value
stateImpactPairJson state =
  object
    [ "before" .= sipBefore state
    , "after" .= sipAfter state
    , "deltas" .= stateImpactDeltas state
    , "deltaUnits" .= stateImpactDeltaUnits state
    , "provenance" .= stateImpactPairEvidenceLabel state
    , "availability" .= stateImpactPairAvailability "" state
    ]

transactionStateImpactAvailability :: TransactionStateImpact -> [Value]
transactionStateImpactAvailability TransactionStateImpact {..} =
  concatMap accountAvailability tsiAccounts
    <> stateImpactPairAvailability "stateImpact.housePool" tsiHousePool
    <> stateImpactPairAvailability "stateImpact.senior" tsiSenior
    <> stateImpactPairAvailability "stateImpact.junior" tsiJunior
    <> [ unavailable
          "stateImpact.sourceBlocks.before"
          "source_block_header_unavailable"
       | tsiBeforeBlockNumber == Nothing || tsiBeforeBlockHash == Nothing
       ]
  where
    accountAvailability AccountStateImpact {..} =
      stateImpactPairAvailability
        ("stateImpact.accounts." <> T.toLower asiAccount)
        asiState

stateImpactPairAvailability :: Text -> StateImpactPair -> [Value]
stateImpactPairAvailability prefix StateImpactPair {..} =
  sideAvailability "before" sipBefore sipBeforeReason
    <> sideAvailability "after" sipAfter sipAfterReason
  where
    sideAvailability side value reason =
      case (value, reason) of
        (Nothing, _) ->
          [unavailable (path side) (stateImpactUnavailableReason reason)]
        (Just _, Just partialReason) ->
          [ unavailable
              (path side)
              (stateImpactUnavailableReason $ Just partialReason)
          ]
        (Just _, Nothing) -> []

    path suffix
      | T.null prefix = suffix
      | otherwise = prefix <> "." <> suffix

stateImpactUnavailableReason :: Maybe Text -> Text
stateImpactUnavailableReason (Just reason)
  | "malformed_" `T.isPrefixOf` reason = reason
  | "noncanonical_" `T.isPrefixOf` reason = reason
  | "canonical_block_" `T.isPrefixOf` reason = reason
  | "transaction_block_" `T.isPrefixOf` reason = reason
  | "partial_" `T.isPrefixOf` reason = reason
  | otherwise = "archive_state_unavailable"
stateImpactUnavailableReason Nothing = "archive_state_unavailable"

transactionStateImpactEvidenceLabel :: TransactionStateImpact -> Text
transactionStateImpactEvidenceLabel TransactionStateImpact {..}
  | all stateImpactPairComplete pairs
      && tsiBeforeBlockNumber /= Nothing
      && tsiBeforeBlockHash /= Nothing =
      "canonical_hash_bound_block-level_delta"
  | any stateImpactPairHasEvidence pairs =
      "partial_canonical_hash_bound_block-level_state"
  | otherwise = "unavailable"
  where
    pairs =
      map asiState tsiAccounts
        <> [tsiHousePool, tsiSenior, tsiJunior]

stateImpactPairEvidenceLabel :: StateImpactPair -> Text
stateImpactPairEvidenceLabel pair
  | stateImpactPairComplete pair = "block-level delta"
  | stateImpactPairHasEvidence pair = "partial block-level state"
  | otherwise = "unavailable"

stateImpactPairComplete :: StateImpactPair -> Bool
stateImpactPairComplete StateImpactPair {..} =
  sipBefore /= Nothing
    && sipAfter /= Nothing
    && sipBeforeReason == Nothing
    && sipAfterReason == Nothing

stateImpactPairHasEvidence :: StateImpactPair -> Bool
stateImpactPairHasEvidence StateImpactPair {..} =
  sipBefore /= Nothing || sipAfter /= Nothing

stateImpactDeltas :: StateImpactPair -> Value
stateImpactDeltas StateImpactPair {..} =
  case (sipBefore, sipAfter) of
    (Just (Object beforeFields), Just (Object afterFields)) ->
      Object $
        KM.fromList $
          catMaybes $
            map
              ( \key -> do
                  beforeValue <- KM.lookup key beforeFields
                  afterValue <- KM.lookup key afterFields
                  beforeInteger <- valueSignedInteger beforeValue
                  afterInteger <- valueSignedInteger afterValue
                  pure
                    ( Key.fromText $ Key.toText key <> "Delta"
                    , String $ T.pack $ show $ afterInteger - beforeInteger
                    )
              )
              (KM.keys afterFields)
    _ -> Object KM.empty

stateImpactDeltaUnits :: StateImpactPair -> Value
stateImpactDeltaUnits state@StateImpactPair {..} =
  case (stateImpactDeltas state, sipAfter >>= afterUnits) of
    (Object deltaFields, Just (Object unitFields)) ->
      Object $
        KM.fromList $
          catMaybes $
            map
              (\deltaKey -> do
                baseKey <- Key.fromText <$> T.stripSuffix "Delta" (Key.toText deltaKey)
                unitValue <- KM.lookup baseKey unitFields
                pure (deltaKey, unitValue)
              )
              (KM.keys deltaFields)
    (Object deltaFields, Just unitValue@(String _)) ->
      Object $
        KM.fromList
          [(deltaKey, unitValue) | deltaKey <- KM.keys deltaFields]
    _ -> Object KM.empty
  where
    afterUnits after =
      objectField "units" after <|> objectField "unit" after

valueSignedInteger :: Value -> Maybe Integer
valueSignedInteger = \case
  String text -> readSignedDecimal text
  Number scientific ->
    case (floatingOrInteger scientific :: Either Double Integer) of
      Right integer -> Just integer
      Left _ -> Nothing
  _ -> Nothing

transactionActionAnalysisJson
  :: ProtocolRelease
  -> [ProtocolActionRow]
  -> TransactionStateImpact
  -> Value
transactionActionAnalysisJson release actions stateImpact =
  object
    [ "economics" .= transactionEconomicsJson actions
    , "liquidations" .=
        ( map (liquidationActionAnalysisJson stateImpact) $
            filter ((== "liquidation") . parActionType) actions
        )
    , "marginActions" .=
        ( map (marginActionAnalysisJson stateImpact) $
            filter (isMarginAction . parActionType) actions
        )
    , "trancheActions" .=
        ( map (trancheActionAnalysisJson release stateImpact) $
            filter (isTrancheAction . parActionType) actions
        )
    , "provenance" .= object
        [ "eventComponents" .= ("exact_confirmed_log_projection_when_present" :: Text)
        , "stateAnalysis" .= transactionStateImpactEvidenceLabel stateImpact
        , "settlementBreakdown" .= ("unavailable" :: Text)
        ]
    , "formulaIdentifier" .=
        ("protocol.transaction.action_analysis.v1" :: Text)
    , "formula" .=
        ("sum only emitted action components; interpret account, pool, and tranche changes from canonical state(block - 1) and state(block)" :: Text)
    , "evidenceReferences" .=
        map
          ( \action ->
              object
                [ "type" .= ("protocol_action" :: Text)
                , "actionId" .= parActionId action
                , "transactionHash" .= parTxHash action
                , "logIndex" .= show (parLogIndex action)
                ]
          )
          actions
    , "availability" .= transactionActionAnalysisAvailability release actions stateImpact
    ]

transactionEconomicsJson :: [ProtocolActionRow] -> Value
transactionEconomicsJson actions =
  let realizedPnl = sumActionData "pnl" actions
      keeperBounty = sumActionData "keeperBountyUsdc" actions
      marginFlow =
        signedActionFlow isMarginAction "amountUsdc" "margin_withdraw" actions
      trancheAssetFlow =
        signedActionFlow isTrancheAction "assets" "tranche_withdraw" actions
      observedAvailability =
        [ unavailable fieldName "event_component_not_emitted_for_transaction"
        | (fieldName, value) <-
            [ ("observedRealizedPnlUsdc", realizedPnl)
            , ("observedKeeperBountyUsdc", keeperBounty)
            , ("observedMarginFlowUsdc", marginFlow)
            , ("observedTrancheAssetFlowUsdc", trancheAssetFlow)
            ]
        , value == Nothing
        ]
   in object
    [ "observedRealizedPnlUsdc" .= fmap show realizedPnl
    , "observedKeeperBountyUsdc" .= fmap show keeperBounty
    , "observedMarginFlowUsdc" .= fmap show marginFlow
    , "observedTrancheAssetFlowUsdc" .= fmap show trancheAssetFlow
    , "eventComponents" .= map actionEconomicsComponentJson actions
    , "protocolFeeUsdc" .= Null
    , "carryUsdc" .= Null
    , "vpiUsdc" .= Null
    , "frozenSpreadUsdc" .= Null
    , "immediatePayoutUsdc" .= Null
    , "claimCreatedUsdc" .= Null
    , "claimConsumedUsdc" .= Null
    , "seizedCollateralUsdc" .= Null
    , "badDebtUsdc" .= Null
    , "provenance" .= object
        [ "observedComponents" .= ("exact_confirmed_log_projection_when_present" :: Text)
        , "observedRealizedPnlUsdc" .=
            presentEvidence "derived_sum_of_exact_event_components" realizedPnl
        , "observedKeeperBountyUsdc" .=
            presentEvidence "derived_sum_of_exact_event_components" keeperBounty
        , "observedMarginFlowUsdc" .=
            presentEvidence "derived_sum_of_exact_event_components" marginFlow
        , "observedTrancheAssetFlowUsdc" .=
            presentEvidence "derived_sum_of_exact_event_components" trancheAssetFlow
        , "settlementComponents" .= ("unavailable" :: Text)
        ]
    , "formulaIdentifier" .= ("protocol.transaction.observed_economics.v1" :: Text)
    , "availability" .= observedAvailability
    , "units" .= object
        [ "observedRealizedPnlUsdc" .= ("USDC:6" :: Text)
        , "observedKeeperBountyUsdc" .= ("USDC:6" :: Text)
        , "observedMarginFlowUsdc" .= ("USDC:6 signed inflow" :: Text)
        , "observedTrancheAssetFlowUsdc" .= ("USDC:6 signed inflow" :: Text)
        , "protocolFeeUsdc" .= ("USDC:6" :: Text)
        , "carryUsdc" .= ("USDC:6" :: Text)
        , "vpiUsdc" .= ("USDC:6" :: Text)
        , "frozenSpreadUsdc" .= ("USDC:6" :: Text)
        , "immediatePayoutUsdc" .= ("USDC:6" :: Text)
        , "claimCreatedUsdc" .= ("USDC:6" :: Text)
        , "claimConsumedUsdc" .= ("USDC:6" :: Text)
        , "seizedCollateralUsdc" .= ("USDC:6" :: Text)
        , "badDebtUsdc" .= ("USDC:6" :: Text)
        ]
    ]

actionEconomicsComponentJson :: ProtocolActionRow -> Value
actionEconomicsComponentJson action =
  let componentFields =
        [ ("amountUsdc", objectField "amountUsdc" $ parData action)
        , ("marginDeltaUsdc", objectField "marginDelta" $ parData action)
        , ("realizedPnlUsdc", objectField "pnl" $ parData action)
        , ("keeperBountyUsdc", objectField "keeperBountyUsdc" $ parData action)
        , ("trancheAssetsUsdc", objectField "assets" $ parData action)
        , ("trancheShares", objectField "shares" $ parData action)
        , ("sizeDelta", objectField "sizeDelta" $ parData action)
        , ("executionPrice", objectField "price" $ parData action)
        ]
      componentValue key =
        fromMaybe Null $ lookup key componentFields >>= id
      componentEvidence key =
        presentEvidence
          "exact_confirmed_log_action"
          (lookup key componentFields >>= id)
      componentAvailability =
        [ unavailable fieldName "event_component_not_emitted_for_action"
        | (fieldName, value) <- componentFields
        , value == Nothing
        ]
   in object
    [ "actionId" .= parActionId action
    , "actionType" .= parActionType action
    , "account" .= parAccount action
    , "amountUsdc" .= componentValue "amountUsdc"
    , "marginDeltaUsdc" .= componentValue "marginDeltaUsdc"
    , "realizedPnlUsdc" .= componentValue "realizedPnlUsdc"
    , "keeperBountyUsdc" .= componentValue "keeperBountyUsdc"
    , "trancheAssetsUsdc" .= componentValue "trancheAssetsUsdc"
    , "trancheShares" .= componentValue "trancheShares"
    , "sizeDelta" .= componentValue "sizeDelta"
    , "executionPrice" .= componentValue "executionPrice"
    , "evidence" .= object
        [ "source" .= ("confirmed_log_action" :: Text)
        , "actionEvidence" .= parEvidence action
        , "amountUsdc" .= componentEvidence "amountUsdc"
        , "marginDeltaUsdc" .= componentEvidence "marginDeltaUsdc"
        , "realizedPnlUsdc" .= componentEvidence "realizedPnlUsdc"
        , "keeperBountyUsdc" .= componentEvidence "keeperBountyUsdc"
        , "trancheAssetsUsdc" .= componentEvidence "trancheAssetsUsdc"
        , "trancheShares" .= componentEvidence "trancheShares"
        , "sizeDelta" .= componentEvidence "sizeDelta"
        , "executionPrice" .= componentEvidence "executionPrice"
        ]
    , "availability" .= componentAvailability
    , "units" .= object
        [ "amountUsdc" .= ("USDC:6" :: Text)
        , "marginDeltaUsdc" .= ("USDC:6" :: Text)
        , "realizedPnlUsdc" .= ("USDC:6 signed" :: Text)
        , "keeperBountyUsdc" .= ("USDC:6" :: Text)
        , "trancheAssetsUsdc" .= ("USDC:6" :: Text)
        , "trancheShares" .= ("shares:18" :: Text)
        , "sizeDelta" .= ("position:18" :: Text)
        , "executionPrice" .= ("indexPrice:8" :: Text)
        ]
    ]

liquidationActionAnalysisJson
  :: TransactionStateImpact
  -> ProtocolActionRow
  -> Value
liquidationActionAnalysisJson stateImpact action =
  let accountState = parAccount action >>= accountStateFor stateImpact
      beforeState = accountState >>= sipBefore
      afterState = accountState >>= sipAfter
      clearedOrders = accountState >>= clearedPendingOrdersFromPair
      executionPrice = objectField "price" $ parData action
      sizeCleared = objectField "sizeDelta" $ parData action
      bounty = objectField "keeperBountyUsdc" $ parData action
      accountStateEvidence =
        maybe "unavailable" stateImpactPairEvidenceLabel accountState
      availability =
        [ unavailable "marginRegime" "historical_margin_regime_not_reconstructed"
        , unavailable "adverseConfidencePricing" "oracle_confidence_components_not_indexed"
        , unavailable "forfeitedRewardsUsdc" "cleared_order_reward_telemetry_missing"
        , unavailable "badDebtUsdc" "current_release_settlement_telemetry_missing"
        ]
          <> [unavailable "executionPrice" "event_component_not_emitted_for_action" | executionPrice == Nothing]
          <> [unavailable "sizeCleared" "event_component_not_emitted_for_action" | sizeCleared == Nothing]
          <> [unavailable "bountyUsdc" "event_component_not_emitted_for_action" | bounty == Nothing]
          <> [unavailable "observedEligibility" "archive_state_unavailable" | (beforeState >>= objectField "liquidatable") == Nothing]
          <> [unavailable "reachableCollateralUsdc" "archive_state_unavailable" | (beforeState >>= objectField "terminalReachableUsdc") == Nothing]
          <> [unavailable "clearedPendingOrders" "archive_state_unavailable" | clearedOrders == Nothing]
          <> [unavailable "traderResidualOrClaimUsdc" "archive_state_unavailable" | (afterState >>= objectField "traderClaimBalanceUsdc") == Nothing]
   in object
        [ "actionId" .= parActionId action
        , "account" .= parAccount action
        , "observedEligibility" .= (beforeState >>= objectField "liquidatable")
        , "executionPrice" .= fromMaybe Null executionPrice
        , "sizeCleared" .= fromMaybe Null sizeCleared
        , "bountyUsdc" .= fromMaybe Null bounty
        , "marginRegime" .= Null
        , "adverseConfidencePricing" .= Null
        , "reachableCollateralUsdc" .= (beforeState >>= objectField "terminalReachableUsdc")
        , "clearedPendingOrders" .= clearedOrders
        , "forfeitedRewardsUsdc" .= Null
        , "traderResidualOrClaimUsdc" .= (afterState >>= objectField "traderClaimBalanceUsdc")
        , "badDebtUsdc" .= Null
        , "accountStateImpact" .= maybe Null stateImpactPairJson accountState
        , "poolSolvencyImpact" .= stateImpactPairJson (tsiHousePool stateImpact)
        , "waterfallImpact" .= object
            [ "senior" .= stateImpactPairJson (tsiSenior stateImpact)
            , "junior" .= stateImpactPairJson (tsiJunior stateImpact)
            ]
        , "provenance" .= object
            [ "executionPrice" .=
                presentEvidence "exact_confirmed_log_action" executionPrice
            , "sizeCleared" .=
                presentEvidence "exact_confirmed_log_action" sizeCleared
            , "bountyUsdc" .=
                presentEvidence "exact_confirmed_log_action" bounty
            , "observedEligibility" .=
                presentEvidence
                  accountStateEvidence
                  (beforeState >>= objectField "liquidatable")
            , "reachableCollateralUsdc" .=
                presentEvidence
                  accountStateEvidence
                  (beforeState >>= objectField "terminalReachableUsdc")
            , "clearedPendingOrders" .=
                presentEvidence accountStateEvidence clearedOrders
            , "traderResidualOrClaimUsdc" .=
                presentEvidence
                  accountStateEvidence
                  (afterState >>= objectField "traderClaimBalanceUsdc")
            , "settlementBreakdown" .= ("unavailable" :: Text)
            ]
        , "availability" .= availability
        , "units" .= object
            [ "observedEligibility" .= ("boolean" :: Text)
            , "executionPrice" .= ("indexPrice:8" :: Text)
            , "sizeCleared" .= ("position:18" :: Text)
            , "bountyUsdc" .= ("USDC:6" :: Text)
            , "marginRegime" .= ("enum" :: Text)
            , "reachableCollateralUsdc" .= ("USDC:6" :: Text)
            , "clearedPendingOrders" .= ("count" :: Text)
            , "forfeitedRewardsUsdc" .= ("USDC:6" :: Text)
            , "traderResidualOrClaimUsdc" .= ("USDC:6" :: Text)
            , "badDebtUsdc" .= ("USDC:6" :: Text)
            ]
        ]

marginActionAnalysisJson
  :: TransactionStateImpact
  -> ProtocolActionRow
  -> Value
marginActionAnalysisJson stateImpact action =
  let accountState = parAccount action >>= accountStateFor stateImpact
      amount = objectField "amountUsdc" $ parData action
      settlementDelta =
        accountState >>= stateFieldDelta "settlementBalanceUsdc"
      freeSettlementDelta =
        accountState >>= stateFieldDelta "freeSettlementUsdc"
      committedMarginDelta =
        accountState >>= stateFieldDelta "committedMarginUsdc"
      stateEvidence =
        maybe "unavailable" stateImpactPairEvidenceLabel accountState
      availability =
        [unavailable "amountUsdc" "event_component_not_emitted_for_action" | amount == Nothing]
          <> [unavailable "accountStateImpact" "archive_state_unavailable" | maybe True (not . stateImpactPairHasEvidence) accountState]
          <> [unavailable "settlementBalanceDeltaUsdc" "archive_state_unavailable" | settlementDelta == Nothing]
          <> [unavailable "freeSettlementDeltaUsdc" "archive_state_unavailable" | freeSettlementDelta == Nothing]
          <> [unavailable "committedMarginDeltaUsdc" "archive_state_unavailable" | committedMarginDelta == Nothing]
   in object
        [ "actionId" .= parActionId action
        , "actionType" .= parActionType action
        , "account" .= parAccount action
        , "direction" .= marginDirection (parActionType action)
        , "amountUsdc" .= fromMaybe Null amount
        , "accountStateImpact" .= maybe Null stateImpactPairJson accountState
        , "settlementBalanceDeltaUsdc" .= settlementDelta
        , "freeSettlementDeltaUsdc" .= freeSettlementDelta
        , "committedMarginDeltaUsdc" .= committedMarginDelta
        , "provenance" .= object
            [ "amountUsdc" .=
                presentEvidence "exact_confirmed_log_action" amount
            , "accountStateImpact" .= stateEvidence
            , "settlementBalanceDeltaUsdc" .=
                presentEvidence stateEvidence settlementDelta
            , "freeSettlementDeltaUsdc" .=
                presentEvidence stateEvidence freeSettlementDelta
            , "committedMarginDeltaUsdc" .=
                presentEvidence stateEvidence committedMarginDelta
            ]
        , "availability" .= availability
        , "units" .= object
            [ "amountUsdc" .= ("USDC:6" :: Text)
            , "settlementBalanceDeltaUsdc" .= ("USDC:6 signed" :: Text)
            , "freeSettlementDeltaUsdc" .= ("USDC:6 signed" :: Text)
            , "committedMarginDeltaUsdc" .= ("USDC:6 signed" :: Text)
            ]
        ]

trancheActionAnalysisJson
  :: ProtocolRelease
  -> TransactionStateImpact
  -> ProtocolActionRow
  -> Value
trancheActionAnalysisJson release stateImpact action =
  let tranche = actionTranche release action
      trancheState = case tranche of
        Just "senior" -> Just $ tsiSenior stateImpact
        Just "junior" -> Just $ tsiJunior stateImpact
        _ -> Nothing
      assets = objectField "assets" $ parData action
      shares = objectField "shares" $ parData action
      navDelta = trancheState >>= stateFieldDelta "navUsdc"
      principalDelta =
        trancheState >>= stateFieldDelta "principalUsdc"
      shareSupplyDelta =
        trancheState >>= stateFieldDelta "shareSupply"
      stateEvidence =
        maybe "unavailable" stateImpactPairEvidenceLabel trancheState
      availability =
        [unavailable "tranche" "tranche_vault_identity_unavailable" | tranche == Nothing]
          <> [unavailable "assetsUsdc" "event_component_not_emitted_for_action" | assets == Nothing]
          <> [unavailable "shares" "event_component_not_emitted_for_action" | shares == Nothing]
          <> [unavailable "trancheStateImpact" "archive_state_unavailable" | maybe True (not . stateImpactPairHasEvidence) trancheState]
          <> maybe
            []
            (stateImpactPairAvailability "trancheStateImpact")
            trancheState
          <> [unavailable "navDeltaUsdc" "archive_state_unavailable" | navDelta == Nothing]
          <> [unavailable "principalDeltaUsdc" "archive_state_unavailable" | principalDelta == Nothing]
          <> [unavailable "shareSupplyDelta" "archive_state_unavailable" | shareSupplyDelta == Nothing]
   in object
        [ "actionId" .= parActionId action
        , "actionType" .= parActionType action
        , "tranche" .= tranche
        , "vaultAddress" .= parContractAddress action
        , "account" .= parAccount action
        , "assetsUsdc" .= fromMaybe Null assets
        , "shares" .= fromMaybe Null shares
        , "trancheStateImpact" .= maybe Null stateImpactPairJson trancheState
        , "navDeltaUsdc" .= navDelta
        , "principalDeltaUsdc" .= principalDelta
        , "shareSupplyDelta" .= shareSupplyDelta
        , "provenance" .= object
            [ "assetsUsdc" .=
                presentEvidence "exact_confirmed_log_action" assets
            , "shares" .=
                presentEvidence "exact_confirmed_log_action" shares
            , "trancheStateImpact" .= stateEvidence
            , "navDeltaUsdc" .= presentEvidence stateEvidence navDelta
            , "principalDeltaUsdc" .=
                presentEvidence stateEvidence principalDelta
            , "shareSupplyDelta" .=
                presentEvidence stateEvidence shareSupplyDelta
            ]
        , "availability" .= availability
        , "units" .= object
            [ "assetsUsdc" .= ("USDC:6" :: Text)
            , "shares" .= ("shares:18" :: Text)
            , "navDeltaUsdc" .= ("USDC:6 signed" :: Text)
            , "principalDeltaUsdc" .= ("USDC:6 signed" :: Text)
            , "shareSupplyDelta" .= ("shares:18 signed" :: Text)
            ]
        ]

transactionActionAnalysisAvailability
  :: ProtocolRelease
  -> [ProtocolActionRow]
  -> TransactionStateImpact
  -> [Value]
transactionActionAnalysisAvailability release actions stateImpact =
  map
    (`unavailable` "current_release_settlement_telemetry_missing")
    [ "analysis.economics.protocolFeeUsdc"
    , "analysis.economics.carryUsdc"
    , "analysis.economics.vpiUsdc"
    , "analysis.economics.frozenSpreadUsdc"
    , "analysis.economics.immediatePayoutUsdc"
    , "analysis.economics.claimCreatedUsdc"
    , "analysis.economics.claimConsumedUsdc"
    , "analysis.economics.seizedCollateralUsdc"
    , "analysis.economics.badDebtUsdc"
    ]
    <> observedEconomicsAvailability
    <> concatMap componentAvailabilityForAction actions
    <> concatMap liquidationAvailabilityForAction (filter ((== "liquidation") . parActionType) actions)
    <> concatMap marginAvailabilityForAction (filter (isMarginAction . parActionType) actions)
    <> concatMap trancheAvailabilityForAction (filter (isTrancheAction . parActionType) actions)
  where
    observedEconomicsAvailability =
      missing
        "analysis.economics.observedRealizedPnlUsdc"
        (sumActionData "pnl" actions)
        "event_component_not_emitted_for_transaction"
        <> missing
          "analysis.economics.observedKeeperBountyUsdc"
          (sumActionData "keeperBountyUsdc" actions)
          "event_component_not_emitted_for_transaction"
        <> missing
          "analysis.economics.observedMarginFlowUsdc"
          (signedActionFlow isMarginAction "amountUsdc" "margin_withdraw" actions)
          "event_component_not_emitted_for_transaction"
        <> missing
          "analysis.economics.observedTrancheAssetFlowUsdc"
          (signedActionFlow isTrancheAction "assets" "tranche_withdraw" actions)
          "event_component_not_emitted_for_transaction"

    componentAvailabilityForAction action =
      concatMap
        (\(publicField, sourceField) ->
          missing
            ( "analysis.economics.eventComponents."
                <> parActionId action
                <> "."
                <> publicField
            )
            (objectField sourceField $ parData action)
            "event_component_not_emitted_for_action"
        )
        [ ("amountUsdc", "amountUsdc")
        , ("marginDeltaUsdc", "marginDelta")
        , ("realizedPnlUsdc", "pnl")
        , ("keeperBountyUsdc", "keeperBountyUsdc")
        , ("trancheAssetsUsdc", "assets")
        , ("trancheShares", "shares")
        , ("sizeDelta", "sizeDelta")
        , ("executionPrice", "price")
        ]

    liquidationAvailabilityForAction action =
      let prefix = "analysis.liquidations." <> parActionId action
          accountState = parAccount action >>= accountStateFor stateImpact
          beforeState = accountState >>= sipBefore
          afterState = accountState >>= sipAfter
       in map
            (\fieldName -> unavailable (prefix <> "." <> fieldName) "current_release_settlement_telemetry_missing")
            ["marginRegime", "adverseConfidencePricing", "forfeitedRewardsUsdc", "badDebtUsdc"]
            <> missing (prefix <> ".executionPrice") (objectField "price" $ parData action) "event_component_not_emitted_for_action"
            <> missing (prefix <> ".sizeCleared") (objectField "sizeDelta" $ parData action) "event_component_not_emitted_for_action"
            <> missing (prefix <> ".bountyUsdc") (objectField "keeperBountyUsdc" $ parData action) "event_component_not_emitted_for_action"
            <> [unavailable (prefix <> ".observedEligibility") "archive_state_unavailable" | (beforeState >>= objectField "liquidatable") == Nothing]
            <> [unavailable (prefix <> ".reachableCollateralUsdc") "archive_state_unavailable" | (beforeState >>= objectField "terminalReachableUsdc") == Nothing]
            <> [unavailable (prefix <> ".clearedPendingOrders") "archive_state_unavailable" | (accountState >>= clearedPendingOrdersFromPair) == Nothing]
            <> [unavailable (prefix <> ".traderResidualOrClaimUsdc") "archive_state_unavailable" | (afterState >>= objectField "traderClaimBalanceUsdc") == Nothing]

    marginAvailabilityForAction action =
      let prefix = "analysis.marginActions." <> parActionId action
          accountState = parAccount action >>= accountStateFor stateImpact
       in missing (prefix <> ".amountUsdc") (objectField "amountUsdc" $ parData action) "event_component_not_emitted_for_action"
            <> missing (prefix <> ".accountStateImpact") (accountState >>= stateImpactEvidence) "archive_state_unavailable"
            <> missing (prefix <> ".settlementBalanceDeltaUsdc") (accountState >>= stateFieldDelta "settlementBalanceUsdc") "archive_state_unavailable"
            <> missing (prefix <> ".freeSettlementDeltaUsdc") (accountState >>= stateFieldDelta "freeSettlementUsdc") "archive_state_unavailable"
            <> missing (prefix <> ".committedMarginDeltaUsdc") (accountState >>= stateFieldDelta "committedMarginUsdc") "archive_state_unavailable"

    trancheAvailabilityForAction action =
      let prefix = "analysis.trancheActions." <> parActionId action
          tranche = actionTranche release action
          trancheState = case tranche of
            Just "senior" -> Just $ tsiSenior stateImpact
            Just "junior" -> Just $ tsiJunior stateImpact
            _ -> Nothing
       in missing (prefix <> ".tranche") tranche "tranche_vault_identity_unavailable"
            <> missing (prefix <> ".assetsUsdc") (objectField "assets" $ parData action) "event_component_not_emitted_for_action"
            <> missing (prefix <> ".shares") (objectField "shares" $ parData action) "event_component_not_emitted_for_action"
            <> missing (prefix <> ".trancheStateImpact") (trancheState >>= stateImpactEvidence) "archive_state_unavailable"
            <> maybe
              []
              ( stateImpactPairAvailability
                  (prefix <> ".trancheStateImpact")
              )
              trancheState
            <> missing (prefix <> ".navDeltaUsdc") (trancheState >>= stateFieldDelta "navUsdc") "archive_state_unavailable"
            <> missing (prefix <> ".principalDeltaUsdc") (trancheState >>= stateFieldDelta "principalUsdc") "archive_state_unavailable"
            <> missing (prefix <> ".shareSupplyDelta") (trancheState >>= stateFieldDelta "shareSupply") "archive_state_unavailable"

    stateImpactEvidence state
      | stateImpactPairHasEvidence state = Just ()
      | otherwise = Nothing

    missing fieldName value reason =
      [unavailable fieldName reason | value == Nothing]

accountStateFor :: TransactionStateImpact -> Text -> Maybe StateImpactPair
accountStateFor stateImpact account =
  asiState
    <$> find
      ((== T.toLower account) . T.toLower . asiAccount)
      (tsiAccounts stateImpact)

clearedPendingOrdersFromPair :: StateImpactPair -> Maybe Text
clearedPendingOrdersFromPair state = do
  before <- sipBefore state >>= objectIntegerField "pendingOrderCount"
  after <- sipAfter state >>= objectIntegerField "pendingOrderCount"
  pure $ T.pack $ show $ max 0 (before - after)

stateFieldDelta :: Text -> StateImpactPair -> Maybe Text
stateFieldDelta fieldName state = do
  before <- sipBefore state >>= objectSignedIntegerField fieldName
  after <- sipAfter state >>= objectSignedIntegerField fieldName
  pure $ T.pack $ show $ after - before

objectSignedIntegerField :: Text -> Value -> Maybe Integer
objectSignedIntegerField key value = do
  fieldValue <- objectField key value
  case fieldValue of
    String text -> readSignedDecimal text
    Number scientific ->
      case (floatingOrInteger scientific :: Either Double Integer) of
        Right integer -> Just integer
        Left _ -> Nothing
    _ -> Nothing

readSignedDecimal :: Text -> Maybe Integer
readSignedDecimal text =
  case T.uncons text of
    Just ('-', digits)
      | not (T.null digits) && T.all isDecimalDigit digits ->
          Just $ negate $ read $ T.unpack digits
    _
      | not (T.null text) && T.all isDecimalDigit text ->
          Just $ read $ T.unpack text
    _ -> Nothing
  where
    isDecimalDigit character = character >= '0' && character <= '9'

sumActionData :: Text -> [ProtocolActionRow] -> Maybe Integer
sumActionData fieldName actions =
  case catMaybes $ map (objectSignedIntegerField fieldName . parData) actions of
    [] -> Nothing
    values -> Just $ sum values

signedActionFlow
  :: (Text -> Bool)
  -> Text
  -> Text
  -> [ProtocolActionRow]
  -> Maybe Integer
signedActionFlow includes fieldName outflowAction actions =
  case
      [ if parActionType action == outflowAction then negate amount else amount
      | action <- actions
      , includes $ parActionType action
      , Just amount <- [objectSignedIntegerField fieldName $ parData action]
      ] of
    [] -> Nothing
    values -> Just $ sum values

isMarginAction :: Text -> Bool
isMarginAction actionType =
  actionType `elem` ["margin_add", "margin_deposit", "margin_withdraw"]

isTrancheAction :: Text -> Bool
isTrancheAction actionType =
  actionType `elem` ["tranche_deposit", "tranche_withdraw"]

marginDirection :: Text -> Text
marginDirection "margin_withdraw" = "outflow"
marginDirection _ = "inflow"

actionTranche :: ProtocolRelease -> ProtocolActionRow -> Maybe Text
actionTranche release action
  | normalizeAddress (parContractAddress action) == normalizeAddress (prSeniorVault release) =
      Just "senior"
  | normalizeAddress (parContractAddress action) == normalizeAddress (prJuniorVault release) =
      Just "junior"
  | otherwise = Nothing
  where
    normalizeAddress = T.toLower . T.strip

keeperToJson :: Maybe KeeperNativeCostRow -> KeeperAggregateRow -> Value
keeperToJson nativeCost KeeperAggregateRow {..} =
  object
    [ "address" .= karActor
    , "actionCount" .= show karActionCount
    , "executions" .= show karExecutionCount
    , "cleanups" .= show karCleanupCount
    , "liquidations" .= show karLiquidationCount
    , "observedLiquidationRewardsUsdc" .= show karGrossRewardsUsdc
    , "totalGrossRewardsUsdc" .= Null
    , "nativeCosts" .= maybe Null keeperNativeCostToJson nativeCost
    , "firstActionTimestamp" .= karFirstActionAt
    , "lastActionTimestamp" .= karLastActionAt
    ]

keeperNativeCostFor :: Text -> [KeeperNativeCostRow] -> Maybe KeeperNativeCostRow
keeperNativeCostFor address =
  find ((== T.toLower address) . kncActor)

keeperNativeCostToJson :: KeeperNativeCostRow -> Value
keeperNativeCostToJson KeeperNativeCostRow {..} =
  object
    [ "gasCostWei" .= show kncGasCostWei
    , "transactionNativeValueWei" .= show kncTransactionNativeValueWei
    , "missingGasReceiptCount" .= show kncMissingGasReceiptCount
    , "missingNativeValueCount" .= show kncMissingNativeValueCount
    , "nativeValueInterpretation" .=
        ("Sum of available exact transaction values; partial when missingNativeValueCount is non-zero. The Pyth-fee component is not isolated." :: Text)
    , "units" .= object
        [ "gasCostWei" .= ("wei" :: Text)
        , "transactionNativeValueWei" .= ("wei" :: Text)
        ]
    ]

keeperRewardSlice :: KeeperAggregateRow -> Value
keeperRewardSlice row =
  object
    [ "address" .= karActor row
    , "observedLiquidationRewardsUsdc" .= show (karGrossRewardsUsdc row)
    ]

keeperWindow :: Text -> (Text, Integer)
keeperWindow raw =
  case T.toLower raw of
    "24h" -> ("24h", 86_400)
    "30d" -> ("30d", 30 * 86_400)
    _ -> ("7d", 7 * 86_400)

parameterContract :: ProtocolRelease -> Text -> Maybe Text
parameterContract release = \case
  "orderRouter" -> Just $ prOrderRouter release
  "cfdEngine" -> Just $ prCfdEngine release
  "pletherOracle" -> Just $ prPletherOracle release
  "housePool" -> Just $ prHousePool release
  "seniorVault" -> Just $ prSeniorVault release
  "juniorVault" -> Just $ prJuniorVault release
  _ -> Nothing

formatParameter :: ParameterDefinition -> Integer -> Text
formatParameter definition raw =
  case pdRawScale definition of
    -- The catalog's display unit is basis points, so retain the raw bps count.
    -- Converting 500 to 5.00 while still labelling it "bps" would understate
    -- the value by a factor of one hundred.
    "10000" -> T.pack $ show raw
    "1e6" -> decimal 6 raw
    "1e18" -> decimal 18 raw
    "1e8" -> decimal 8 raw
    _ -> T.pack $ show raw

decimal :: Int -> Integer -> Text
decimal places value =
  let scale = 10 ^ places
      whole = value `div` scale
      fraction = value `mod` scale
      padded = T.justifyRight places '0' $ T.pack $ show fraction
   in T.pack (show whole) <> "." <> padded

isPendingChange :: Value -> Bool
isPendingChange (Object fields) =
  case KM.lookup (Key.fromText "status") fields of
    Just (String status) -> status `elem` ["proposed", "pending", "ready", "overdue"]
    _ -> False
isPendingChange _ = False

resolveParameterChangeStatus :: ConfirmedContext -> Value -> Value
resolveParameterChangeStatus context change@(Object fields)
  | ccBlockTimestamp context <= 0 = change
  | Just (String "proposed") <-
      KM.lookup (Key.fromText "status") fields
  , Just eta <- objectIntegerField "eta" change =
      let status =
            if ccBlockTimestamp context < eta
              then ("pending" :: Text)
              else "ready"
          statusEvidence =
            object
              [ "level" .= ("derived" :: Text)
              , "formulaIdentifier" .=
                  ("governance.pending_status.confirmed_eta.v1" :: Text)
              , "confirmedBlockTimestamp" .= show (ccBlockTimestamp context)
              , "eta" .= show eta
              ]
       in Object $
            KM.insert
              (Key.fromText "statusEvidence")
              statusEvidence
              ( KM.insert
                  (Key.fromText "status")
                  (String status)
                  fields
              )
resolveParameterChangeStatus _ change = change

trancheAddress :: ProtocolRelease -> Text -> Maybe Text
trancheAddress release raw =
  case normalizeTranche raw of
    "senior" -> Just $ prSeniorVault release
    "junior" -> Just $ prJuniorVault release
    _ -> Nothing

normalizeTranche :: Text -> Text
normalizeTranche = T.toLower . T.strip

trancheSnapshotCheckpoints
  :: Text
  -> [ProtocolStateSnapshotRow]
  -> [ProtocolStateSnapshotRow]
  -> [ProtocolStateSnapshotRow]
  -> [ProtocolStateSnapshotRow]
  -> [Value]
trancheSnapshotCheckpoints tranche trancheRows poolRows longRows shortRows =
  snd $ foldl buildCheckpoint (Nothing, []) $ sortOn pssBlockNumber trancheRows
  where
    buildCheckpoint
      :: (Maybe Integer, [Value])
      -> ProtocolStateSnapshotRow
      -> (Maybe Integer, [Value])
    buildCheckpoint (observedPeakNav, checkpoints) trancheRow =
      let poolRow = sameBlockSnapshot trancheRow poolRows
          longRow = sameBlockSnapshot trancheRow longRows
          shortRow = sameBlockSnapshot trancheRow shortRows
          nav = snapshotInteger trancheRow "totalAssetsUsdc"
          shareSupply = snapshotInteger trancheRow "totalSupply"
          principalKey =
            if tranche == "senior"
              then "seniorPrincipalUsdc"
              else "juniorPrincipalUsdc"
          principal = poolRow >>= \row -> snapshotInteger row principalKey
          highWater = poolRow >>= \row -> snapshotInteger row "seniorHighWaterMarkUsdc"
          accountedAssets = poolRow >>= \row -> snapshotInteger row "totalAssetsUsdc"
          longLiability = longRow >>= \row -> snapshotInteger row "maxProfitUsdc"
          shortLiability = shortRow >>= \row -> snapshotInteger row "maxProfitUsdc"
          maximumLiability = max <$> longLiability <*> shortLiability
          assetsPerShare = do
            assets <- nav
            supply <- shareSupply
            if supply == 0
              then Nothing
              else Just $ assets * 10 ^ (18 :: Int) `div` supply
          nextPeakNav =
            case nav of
              Nothing -> observedPeakNav
              Just currentNav -> Just $ maybe currentNav (max currentNav) observedPeakNav
          drawdown = do
            currentNav <- nav
            peakNav <- nextPeakNav
            pure $ max 0 $ peakNav - currentNav
          coverageRatioBps = do
            assets <- accountedAssets
            liability <- maximumLiability
            if liability == 0
              then Nothing
              else Just $ assets * 10_000 `div` liability
          impairmentGap =
            if tranche == "senior"
              then max 0 <$> ((-) <$> highWater <*> principal)
              else Nothing
          sourceRows =
            trancheRow : catMaybes [poolRow, longRow, shortRow]
          availability =
            concatMap snapshotAvailabilityItems sourceRows
              <> missingSnapshotAvailability "house-pool.liquidity" poolRow
              <> missingSnapshotAvailability "market.long" longRow
              <> missingSnapshotAvailability "market.short" shortRow
              <> missingValueAvailability "principalUsdc" principal
              <> missingValueAvailability "navUsdc" nav
              <> missingValueAvailability "shareSupply" shareSupply
              <> if shareSupply == Just 0
                   then [unavailable "checkpoint.assetsPerShare" "zero_share_supply"]
                   else missingValueAvailability "assetsPerShare" assetsPerShare
              <> missingValueAvailability "drawdownUsdc" drawdown
              <> if maximumLiability == Just 0
                   then [unavailable "checkpoint.coverageRatioBps" "zero_bounded_liability"]
                   else missingValueAvailability "coverageRatioBps" coverageRatioBps
              <> [ unavailable "checkpoint.impairmentGapUsdc" "senior_high_water_or_principal_unavailable"
                 | tranche == "senior"
                 , impairmentGap == Nothing
                 ]
          checkpoint =
            object
              [ "blockNumber" .= show (pssBlockNumber trancheRow)
              , "blockHash" .= pssBlockHash trancheRow
              , "timestamp" .= pssTimestamp trancheRow
              , "principalUsdc" .= fmap show principal
              , "navUsdc" .= fmap show nav
              , "shareSupply" .= fmap show shareSupply
              , "assetsPerShare" .= fmap show assetsPerShare
              , "drawdownUsdc" .= fmap show drawdown
              , "impairmentGapUsdc" .= fmap show impairmentGap
              , "coverageRatioBps" .= fmap show coverageRatioBps
              , "calculationVersion" .= pssCalculationVersion trancheRow
              , "formulaIdentifier" .=
                  ("protocol.tranche.range_end_checkpoint.v1" :: Text)
              , "formula" .= object
                  [ "assetsPerShare" .=
                      ("tranche totalAssetsUsdc * 1e18 / tranche totalSupply" :: Text)
                  , "drawdownUsdc" .=
                      ("maximum observed tranche NAV through this checkpoint within this returned sparse checkpoint page minus current tranche NAV" :: Text)
                  , "coverageRatioBps" .=
                      ("HousePool accounted totalAssetsUsdc * 10000 / max(LONG maxProfitUsdc, SHORT maxProfitUsdc)" :: Text)
                  , "impairmentGapUsdc" .=
                      ("max(0, Senior high-water mark minus Senior principal)" :: Text)
                  ]
              , "evidence" .= object
                  [ "principalUsdc" .= evidenceFor principal "exact_historical_contract_read"
                  , "navUsdc" .= evidenceFor nav "exact_historical_contract_read"
                  , "shareSupply" .= evidenceFor shareSupply "exact_historical_contract_read"
                  , "assetsPerShare" .= evidenceFor assetsPerShare "derived_from_same_block_snapshots"
                  , "drawdownUsdc" .= evidenceFor drawdown "derived_from_sparse_range_end_snapshots"
                  , "impairmentGapUsdc" .= evidenceFor impairmentGap "derived_from_same_block_snapshots"
                  , "coverageRatioBps" .= evidenceFor coverageRatioBps "derived_from_same_block_snapshots"
                  ]
              , "sourceScopes" .=
                  map
                    (\row ->
                      object
                        [ "scope" .= pssScope row
                        , "blockNumber" .= show (pssBlockNumber row)
                        , "blockHash" .= pssBlockHash row
                        ])
                    sourceRows
              , "availability" .= availability
              , "units" .= object
                  [ "principalUsdc" .= ("USDC:6" :: Text)
                  , "navUsdc" .= ("USDC:6" :: Text)
                  , "shareSupply" .= ("shares:18" :: Text)
                  , "assetsPerShare" .= ("USDC:6 per share:18" :: Text)
                  , "drawdownUsdc" .= ("USDC:6" :: Text)
                  , "impairmentGapUsdc" .= ("USDC:6" :: Text)
                  , "coverageRatioBps" .= ("bps" :: Text)
                  ]
              ]
       in (nextPeakNav, checkpoints <> [checkpoint])
    evidenceFor Nothing _ = ("unavailable" :: Text)
    evidenceFor (Just _) label = label

sameBlockSnapshot
  :: ProtocolStateSnapshotRow
  -> [ProtocolStateSnapshotRow]
  -> Maybe ProtocolStateSnapshotRow
sameBlockSnapshot source =
  find
    (\candidate ->
      pssBlockNumber candidate == pssBlockNumber source
        && pssBlockHash candidate == pssBlockHash source)

snapshotInteger :: ProtocolStateSnapshotRow -> Text -> Maybe Integer
snapshotInteger row fieldName = do
  values <- objectField "values" $ pssState row
  fieldValue <- objectField fieldName values
  raw <- objectField "raw" fieldValue
  case raw of
    String text
      | not (T.null text)
          && T.all (\character -> character >= '0' && character <= '9') text ->
          Just $ read $ T.unpack text
    _ -> Nothing

snapshotAvailabilityItems :: ProtocolStateSnapshotRow -> [Value]
snapshotAvailabilityItems row =
  case pssAvailability row of
    Array values -> concatMap (normalizeSnapshotAvailability row) $ toList values
    Null -> []
    _ ->
      [ unavailable
          ("checkpoint.sourceScopes." <> pssScope row)
          "malformed_snapshot_availability"
      ]

normalizeSnapshotAvailability :: ProtocolStateSnapshotRow -> Value -> [Value]
normalizeSnapshotAvailability row availability =
  case (objectField "reason" availability, objectField "affectedFields" availability) of
    (Just (String reason), Just (Array affectedFields)) ->
      case
        [ fieldName
        | String fieldName <- toList affectedFields
        ] of
        [] ->
          [ unavailable
              ("checkpoint.sourceScopes." <> pssScope row)
              reason
          ]
        fieldNames ->
          map
            (\fieldName ->
              unavailable
                ("checkpoint.sourceScopes." <> pssScope row <> "." <> fieldName)
                reason)
            fieldNames
    (Just (String reason), _) ->
      [ unavailable
          ("checkpoint.sourceScopes." <> pssScope row)
          reason
      ]
    _ ->
      [ unavailable
          ("checkpoint.sourceScopes." <> pssScope row)
          "malformed_snapshot_availability"
      ]

missingSnapshotAvailability
  :: Text
  -> Maybe ProtocolStateSnapshotRow
  -> [Value]
missingSnapshotAvailability scope snapshot =
  [ unavailable ("checkpoint.sourceScopes." <> scope) "same_block_snapshot_unavailable"
  | snapshot == Nothing
  ]

missingValueAvailability :: Text -> Maybe Integer -> [Value]
missingValueAvailability fieldName value =
  [ unavailable ("checkpoint." <> fieldName) "source_snapshot_field_unavailable"
  | value == Nothing
  ]

explorerTxUrl :: ProtocolRelease -> Text -> Text
explorerTxUrl release txHash
  | prChainId release == 421614 = "https://sepolia.arbiscan.io/tx/" <> txHash
  | otherwise = "https://arbiscan.io/tx/" <> txHash

anomaly :: Text -> Text -> Text -> Value -> Value
anomaly code severity message details =
  object
    [ "code" .= code
    , "severity" .= severity
    , "message" .= message
    , "details" .= details
    ]

unavailable :: Text -> Text -> Value
unavailable fieldName reason =
  object
    [ "field" .= fieldName
    , "reason" .= reason
    ]

housePoolFinancialEvidence
  :: Maybe Integer
  -> Maybe Integer
  -> Maybe Integer
  -> Value
housePoolFinancialEvidence maximumLiability coverageBps solvencyHeadroom =
  object $
    housePoolFinancialEvidenceFields
      maximumLiability
      coverageBps
      solvencyHeadroom

housePoolFinancialEvidenceFields
  :: Maybe Integer
  -> Maybe Integer
  -> Maybe Integer
  -> [Pair]
housePoolFinancialEvidenceFields maximumLiability coverageBps solvencyHeadroom =
  [ "boundedLiability" .=
      presentEvidence
        "exact_historical_contract_read"
        maximumLiability
  , "coverageRatio" .=
      presentEvidence
        "derived_from_same_block_state_v1"
        coverageBps
  , "solvencyHeadroom" .=
      presentEvidence
        "derived_from_same_block_state_v1"
        solvencyHeadroom
  ]

presentEvidence :: Text -> Maybe a -> Text
presentEvidence label =
  maybe "unavailable" (const label)

orderEventEvidence
  :: Maybe ProtocolTransactionRow
  -> Text
  -> Maybe a
  -> Text
orderEventEvidence Nothing _ _ = "unavailable"
orderEventEvidence (Just _) label value = presentEvidence label value

orderLifecycleDerivedEvidence
  :: Maybe ProtocolTransactionRow
  -> Maybe ProtocolTransactionRow
  -> Maybe a
  -> Text
orderLifecycleDerivedEvidence (Just _) (Just _) value =
  presentEvidence "derived_from_confirmed_timestamps" value
orderLifecycleDerivedEvidence _ _ _ = "unavailable"
