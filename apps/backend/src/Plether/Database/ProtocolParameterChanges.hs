module Plether.Database.ProtocolParameterChanges
  ( ensureParameterChangeProjectionSchema
  , parameterChangeAlreadyProjectedSql
  , parameterChangeRebuildActionsSql
  , projectParameterChangeAction
  , rebuildParameterChangeProjection
  ) where

import Control.Monad (forM_, when)
import Data.Aeson (Value, encode, object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , Query
  , execute
  , execute_
  , query
  )
import Database.PostgreSQL.Simple.ToField (toField)
import Plether.Protocol.ParameterChanges
  ( ParameterProjection (..)
  , parameterProjectionsForAction
  )

ensureParameterChangeProjectionSchema :: Connection -> IO ()
ensureParameterChangeProjectionSchema conn = do
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS category_key TEXT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS lifecycle TEXT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS source_contract TEXT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS source_action_id TEXT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS terminal_source_action_id TEXT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS raw_scale TEXT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS display_unit TEXT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS value_type TEXT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS proposed_value JSONB"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS proposal_tx_hash TEXT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS proposal_block_number BIGINT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS terminal_tx_hash TEXT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS terminal_block_number BIGINT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS terminal_at BIGINT"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS availability JSONB NOT NULL DEFAULT '[]'::jsonb"
  _ <- execute_ conn
    "ALTER TABLE protocol_parameter_changes ADD COLUMN IF NOT EXISTS calculation_version TEXT"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_protocol_parameter_changes_history \
    \ON protocol_parameter_changes(release_id, block_number DESC, change_id)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_protocol_parameter_changes_active \
    \ON protocol_parameter_changes(release_id, category_key, source_contract, parameter_key) \
    \WHERE status IN ('proposed', 'pending', 'ready', 'overdue')"
  pure ()

projectParameterChangeAction
  :: Connection
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Text
  -> Maybe Text
  -> Text
  -> Value
  -> Value
  -> IO ()
projectParameterChangeAction
  conn
  releaseId
  calculationVersion
  actionId
  txHash
  blockNumber
  logIndex
  timestamp
  actionType
  actor
  contractAddress
  payload
  actionEvidence = do
    let projections =
          parameterProjectionsForAction actionType contractAddress payload
        normalizedContract = T.toLower contractAddress
        normalizedTxHash = T.toLower txHash
    case projections of
      [] -> pure ()
      firstProjection : _ -> do
        processedRows <-
          query conn
            parameterChangeAlreadyProjectedSql
            (releaseId, actionId, actionId)
            :: IO [Only Bool]
        let alreadyProcessed =
              case processedRows of
                Only processed : _ -> processed
                [] -> False
        when (not alreadyProcessed) $
          case ppLifecycle firstProjection of
            "proposed" -> do
              supersedeActive
                conn
                releaseId
                (ppCategory firstProjection)
                normalizedContract
                actionId
                normalizedTxHash
                blockNumber
                timestamp
                calculationVersion
              forM_ projections $
                insertProposal
                  conn
                  releaseId
                  calculationVersion
                  actionId
                  normalizedTxHash
                  blockNumber
                  logIndex
                  timestamp
                  actor
                  normalizedContract
                  actionEvidence
            "cancelled" ->
              terminalize
                conn
                releaseId
                calculationVersion
                actionId
                normalizedTxHash
                blockNumber
                logIndex
                timestamp
                actor
                normalizedContract
                actionEvidence
                "cancelled"
                projections
            "finalized" ->
              terminalize
                conn
                releaseId
                calculationVersion
                actionId
                normalizedTxHash
                blockNumber
                logIndex
                timestamp
                actor
                normalizedContract
                actionEvidence
                "executed"
                projections
            _ -> pure ()

-- | Rebuild the mutable governance projection exclusively from the immutable
-- action ledger. Reorg rewinds call this after orphan actions are deleted, so
-- an earlier proposal correctly becomes active again when its terminal action
-- was on the orphaned branch.
rebuildParameterChangeProjection :: Connection -> Text -> Text -> IO ()
rebuildParameterChangeProjection conn releaseId calculationVersion = do
  _ <- execute conn
    "DELETE FROM protocol_parameter_changes WHERE release_id = ?"
    (Only releaseId)
  rows <-
    query conn
      parameterChangeRebuildActionsSql
      (Only releaseId)
      :: IO [(Text, Text, Integer, Integer, Integer, Text, Maybe Text, Text, Value, Value)]
  forM_ rows $ \(actionId, txHash, blockNumber, logIndex, timestamp, actionType, actor, contractAddress, payload, evidence) ->
    projectParameterChangeAction
      conn
      releaseId
      calculationVersion
      actionId
      txHash
      blockNumber
      logIndex
      timestamp
      actionType
      actor
      contractAddress
      payload
      evidence

parameterChangeAlreadyProjectedSql :: Query
parameterChangeAlreadyProjectedSql =
  "SELECT EXISTS (\
  \SELECT 1 FROM protocol_parameter_changes \
  \WHERE release_id = ? \
  \AND (source_action_id = ? OR terminal_source_action_id = ?))"

parameterChangeRebuildActionsSql :: Query
parameterChangeRebuildActionsSql =
  "SELECT action_id, tx_hash, block_number, log_index, timestamp, action_type, \
  \actor, contract_address, data, evidence \
  \FROM protocol_actions WHERE release_id = ? \
  \AND action_type IN (\
  \'governance_proposal', 'governance_execution', 'governance_cancellation',\
  \'ownership_transfer_started', 'ownership_transfer', 'pauser_update',\
  \'pause', 'unpause', 'protocol_treasury_update', 'governance_role_change') \
  \ORDER BY block_number ASC, log_index ASC"

insertProposal
  :: Connection
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Maybe Text
  -> Text
  -> Value
  -> ParameterProjection
  -> IO ()
insertProposal conn releaseId calculationVersion actionId txHash blockNumber logIndex timestamp proposer contractAddress actionEvidence projection = do
  let proposerAvailability =
        missingActorAvailability "proposer" proposer
      evidencedProjection =
        projection
          { ppAvailability =
              ppAvailability projection <> proposerAvailability
          }
      changeId = projectionChangeId actionId projection
      evidence =
        projectionEvidence
          calculationVersion
          actionId
          txHash
          blockNumber
          logIndex
          actionEvidence
          evidencedProjection
      availability = ppAvailability evidencedProjection
  _ <- execute conn
    "INSERT INTO protocol_parameter_changes (\
    \release_id, change_id, parameter_key, category_key, lifecycle, status,\
    \old_value, new_value, proposed_value, proposer, executor, proposed_at, eta, executed_at,\
    \tx_hash, block_number, source_contract, source_action_id, raw_scale,\
    \display_unit, value_type, proposal_tx_hash, proposal_block_number,\
    \terminal_tx_hash, terminal_block_number, terminal_at, availability,\
    \calculation_version, evidence) \
    \VALUES (?, ?, ?, ?, ?, 'proposed', ?, ?, ?, ?, NULL, ?, ?, NULL, ?, ?, ?, ?,\
    \?, ?, ?, ?, ?, NULL, NULL, NULL, ?, ?, ?) \
    \ON CONFLICT (release_id, change_id) DO NOTHING"
    [ toField releaseId
    , toField changeId
    , toField $ ppParameterKey projection
    , toField $ ppCategory projection
    , toField $ ppLifecycle projection
    , toField $ encode <$> ppOldValue projection
    , toField $ encode <$> ppNewValue projection
    , toField $ encode <$> ppNewValue projection
    , toField $ fmap T.toLower proposer
    , toField timestamp
    , toField $ ppEta projection
    , toField txHash
    , toField blockNumber
    , toField contractAddress
    , toField actionId
    , toField $ ppRawScale projection
    , toField $ ppDisplayUnit projection
    , toField $ ppValueType projection
    , toField txHash
    , toField blockNumber
    , toField $ encode availability
    , toField calculationVersion
    , toField $ encode evidence
    ]
  pure ()

supersedeActive
  :: Connection
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Text
  -> IO ()
supersedeActive conn releaseId category contractAddress actionId txHash blockNumber timestamp calculationVersion = do
  let supersessionEvidence =
        object
          [ "supersededBy" .= object
              [ "sourceActionId" .= actionId
              , "txHash" .= txHash
              , "blockNumber" .= show blockNumber
              , "timestamp" .= show timestamp
              ]
          , "calculationVersion" .= calculationVersion
          ]
  _ <- execute conn
    "UPDATE protocol_parameter_changes SET \
    \status = 'superseded', lifecycle = 'superseded', tx_hash = ?, block_number = ?,\
    \terminal_tx_hash = ?, terminal_block_number = ?, terminal_at = ?,\
    \terminal_source_action_id = ?,\
    \evidence = evidence || ?::jsonb \
    \WHERE release_id = ? AND category_key = ? AND source_contract = ? \
    \AND status IN ('proposed', 'pending', 'ready', 'overdue')"
    ( txHash
    , blockNumber
    , txHash
    , blockNumber
    , timestamp
    , actionId
    , encode supersessionEvidence
    , releaseId
    , category
    , contractAddress
    )
  pure ()

terminalize
  :: Connection
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Maybe Text
  -> Text
  -> Value
  -> Text
  -> [ParameterProjection]
  -> IO ()
terminalize
  conn
  releaseId
  calculationVersion
  actionId
  txHash
  blockNumber
  logIndex
  timestamp
  actor
  contractAddress
  actionEvidence
  terminalStatus
  projections = do
    let firstProjection = head projections
        category = ppCategory firstProjection
        onlyPlaceholder =
          length projections == 1
            && T.isSuffixOf ".*" (ppParameterKey firstProjection)
        terminalEvidence =
          object
            [ "level" .=
                if null terminalActorAvailability
                  then ("exact" :: Text)
                  else "partial"
            , "terminalEvent" .= object
                [ "sourceActionId" .= actionId
                , "txHash" .= txHash
                , "blockNumber" .= show blockNumber
                , "logIndex" .= show logIndex
                , "timestamp" .= show timestamp
                , "evidence" .= actionEvidence
                ]
            , "availability" .= terminalActorAvailability
            , "calculationVersion" .= calculationVersion
            ]
        terminalActorAvailability =
          missingActorAvailability "executor" actor
        terminalAvailability =
          if onlyPlaceholder
            then terminalActorAvailability
            else
              concatMap ppAvailability projections
                <> terminalActorAvailability
    if onlyPlaceholder
      then do
        updated <-
          execute conn
            "UPDATE protocol_parameter_changes SET \
            \status = ?, lifecycle = ?, executor = ?, executed_at = \
            \CASE WHEN ? = 'executed' THEN ? ELSE NULL END, \
            \tx_hash = ?, block_number = ?, terminal_tx_hash = ?,\
            \terminal_block_number = ?, terminal_at = ?,\
            \terminal_source_action_id = ?,\
            \availability = availability || ?::jsonb,\
            \evidence = evidence || ?::jsonb \
            \WHERE release_id = ? AND category_key = ? AND source_contract = ? \
            \AND status IN ('proposed', 'pending', 'ready', 'overdue')"
            ( terminalStatus
            , ppLifecycle firstProjection
            , fmap T.toLower actor
            , terminalStatus
            , timestamp
            , txHash
            , blockNumber
            , txHash
            , blockNumber
            , timestamp
            , actionId
            , encode terminalAvailability
            , encode terminalEvidence
            , releaseId
            , category
            , contractAddress
            )
        when (updated == 0) $
          insertOrphanTerminal
            conn
            releaseId
            calculationVersion
            actionId
            txHash
            blockNumber
            logIndex
            timestamp
            actor
            contractAddress
            actionEvidence
            terminalStatus
            firstProjection
      else
        forM_ projections $ \projection -> do
          updated <-
            updateTerminalField
              conn
              releaseId
              calculationVersion
              actionId
              txHash
              blockNumber
              logIndex
              timestamp
              actor
              contractAddress
              actionEvidence
              terminalStatus
              projection
          when (updated == 0) $
            insertOrphanTerminal
              conn
              releaseId
              calculationVersion
              actionId
              txHash
              blockNumber
              logIndex
              timestamp
              actor
              contractAddress
              actionEvidence
              terminalStatus
              projection

updateTerminalField
  :: Connection
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Maybe Text
  -> Text
  -> Value
  -> Text
  -> ParameterProjection
  -> IO Int64
updateTerminalField conn releaseId calculationVersion actionId txHash blockNumber logIndex timestamp actor contractAddress actionEvidence terminalStatus projection = do
  let terminalEvidence =
        object
          [ "level" .=
              if null executorAvailability
                then ("exact" :: Text)
                else "partial"
          , "terminalEvent" .= object
              [ "sourceActionId" .= actionId
              , "txHash" .= txHash
              , "blockNumber" .= show blockNumber
              , "logIndex" .= show logIndex
              , "timestamp" .= show timestamp
              , "evidence" .= actionEvidence
              ]
          , "availability" .= executorAvailability
          , "calculationVersion" .= calculationVersion
          ]
      executorAvailability =
        missingActorAvailability "executor" actor
  execute conn
    "UPDATE protocol_parameter_changes SET \
    \status = ?, lifecycle = ?, executor = ?, executed_at = \
    \CASE WHEN ? = 'executed' THEN ? ELSE NULL END, \
    \new_value = COALESCE(?::jsonb, new_value), tx_hash = ?, block_number = ?,\
    \terminal_tx_hash = ?, terminal_block_number = ?, terminal_at = ?,\
    \terminal_source_action_id = ?,\
    \raw_scale = COALESCE(?, raw_scale), display_unit = COALESCE(?, display_unit),\
    \value_type = COALESCE(?, value_type),\
    \availability = availability || ?::jsonb,\
    \evidence = evidence || ?::jsonb \
    \WHERE release_id = ? AND category_key = ? AND source_contract = ? \
    \AND parameter_key = ? \
    \AND status IN ('proposed', 'pending', 'ready', 'overdue')"
    [ toField terminalStatus
    , toField $ ppLifecycle projection
    , toField $ fmap T.toLower actor
    , toField terminalStatus
    , toField timestamp
    , toField $ encode <$> ppNewValue projection
    , toField txHash
    , toField blockNumber
    , toField txHash
    , toField blockNumber
    , toField timestamp
    , toField actionId
    , toField $ ppRawScale projection
    , toField $ ppDisplayUnit projection
    , toField $ ppValueType projection
    , toField $ encode $ ppAvailability projection <> executorAvailability
    , toField $ encode terminalEvidence
    , toField releaseId
    , toField $ ppCategory projection
    , toField contractAddress
    , toField $ ppParameterKey projection
    ]

insertOrphanTerminal
  :: Connection
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Maybe Text
  -> Text
  -> Value
  -> Text
  -> ParameterProjection
  -> IO ()
insertOrphanTerminal conn releaseId calculationVersion actionId txHash blockNumber logIndex timestamp actor contractAddress actionEvidence terminalStatus projection = do
  let changeId = projectionChangeId actionId projection
      missingProposal =
        object
          [ "field" .= ("proposal" :: Text)
          , "reason" .= ("prior_proposal_not_present_in_indexed_release_range" :: Text)
          ]
      expectsProposal =
        not (T.isPrefixOf "role:" $ ppCategory projection)
          || T.isSuffixOf ".owner" (ppParameterKey projection)
      availability =
        ppAvailability projection
          <> [missingProposal | expectsProposal]
          <> missingActorAvailability "executor" actor
      evidencedProjection =
        projection
          { ppAvailability = availability
          }
      evidence =
        projectionEvidence
          calculationVersion
          actionId
          txHash
          blockNumber
          logIndex
          actionEvidence
          evidencedProjection
  _ <- execute conn
    "INSERT INTO protocol_parameter_changes (\
    \release_id, change_id, parameter_key, category_key, lifecycle, status,\
    \old_value, new_value, proposed_value, proposer, executor, proposed_at, eta, executed_at,\
    \tx_hash, block_number, source_contract, source_action_id, raw_scale,\
    \display_unit, value_type, proposal_tx_hash, proposal_block_number,\
    \terminal_tx_hash, terminal_block_number, terminal_at, availability,\
    \calculation_version, evidence) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, NULL, NULL, ?, NULL, ?, ?, ?, ?, ?, ?, ?, ?, ?,\
    \NULL, NULL, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (release_id, change_id) DO NOTHING"
    [ toField releaseId
    , toField changeId
    , toField $ ppParameterKey projection
    , toField $ ppCategory projection
    , toField $ ppLifecycle projection
    , toField terminalStatus
    , toField $ encode <$> ppOldValue projection
    , toField $ encode <$> ppNewValue projection
    , toField $ fmap T.toLower actor
    , toField $ ppEta projection
    , toField $
        if terminalStatus == "executed"
          then Just timestamp
          else Nothing
    , toField txHash
    , toField blockNumber
    , toField contractAddress
    , toField actionId
    , toField $ ppRawScale projection
    , toField $ ppDisplayUnit projection
    , toField $ ppValueType projection
    , toField txHash
    , toField blockNumber
    , toField timestamp
    , toField $ encode availability
    , toField calculationVersion
    , toField $ encode evidence
    ]
  pure ()

projectionChangeId :: Text -> ParameterProjection -> Text
projectionChangeId actionId projection =
  "governance:"
    <> T.toLower actionId
    <> ":"
    <> ppParameterKey projection

projectionEvidence
  :: Text
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Value
  -> ParameterProjection
  -> Value
projectionEvidence calculationVersion actionId txHash blockNumber logIndex actionEvidence projection =
  object
    [ "level" .=
        if null (ppAvailability projection)
          then ("exact" :: Text)
          else "partial"
    , "source" .= ("confirmed_log_projection" :: Text)
    , "sourceActionId" .= actionId
    , "sourceBlock" .= show blockNumber
    , "sourceLogIndex" .= show logIndex
    , "txHash" .= txHash
    , "category" .= ppCategory projection
    , "lifecycle" .= ppLifecycle projection
    , "rawScale" .= ppRawScale projection
    , "unit" .= ppDisplayUnit projection
    , "valueType" .= ppValueType projection
    , "availability" .= ppAvailability projection
    , "actionEvidence" .= actionEvidence
    , "calculationVersion" .= calculationVersion
    , "formulaIdentifier" .=
        ("protocol.parameter_change.action_projection.v1" :: Text)
    ]

missingActorAvailability :: Text -> Maybe Text -> [Value]
missingActorAvailability fieldName actor =
  [ object
      [ "field" .= fieldName
      , "reason" .= ("transaction_sender_unavailable" :: Text)
      ]
  | actor == Nothing
  ]
