module Plether.Insights.SnapshotWorker
  ( runInsightsSnapshotCycle
  , findLastBlockBeforeTimestamp
  , snapshotToJson
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value, object, (.=))
import qualified Data.Text as T
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Insights
  ( AccountSnapshotInput (..)
  , CompetitionRow (..)
  , ParticipantRow (..)
  , SnapshotKind (..)
  , getCurrentCompetition
  , hasCompleteAccountSnapshotBatch
  , getLatestIndexedSafeBlock
  , invalidateSnapshotBatchesAfter
  , listCompetitionParticipants
  , publishAccountSnapshotBatch
  , setCompetitionBoundaryBlocks
  )
import Plether.Ethereum.Client (EthClient, RpcError)
import Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( AccountLedgerSnapshot (..)
  , getAccountLedgerSnapshotAtBlock
  )
import Plether.Ethereum.Rpc
  ( RpcBlock (..)
  , ethGetBlockByNumber
  )
import Plether.Insights.Competition (EquitySnapshot (..))

-- | Capture one internally consistent Insights update. The perps history
-- indexer's cursor is already confirmation-delayed, so it is the canonical
-- upper bound for both account state and event-derived statistics.
runInsightsSnapshotCycle :: EthClient -> DbPool -> Config -> IO ()
runInsightsSnapshotCycle client pool cfg = do
  mCompetition <- withDb pool getCurrentCompetition
  case mCompetition of
    Nothing -> putStrLn "Insights snapshot skipped: no competition is configured"
    Just competition -> do
      mSafe <-
        withDb pool $ \conn ->
          getLatestIndexedSafeBlock
            conn
            (icrChainId competition)
            (icrReleaseRouter competition)
      case mSafe of
        Nothing -> putStrLn "Insights snapshot skipped: perps indexer has no finalized cursor"
        Just (_, Nothing) ->
          putStrLn "Insights snapshot skipped: finalized indexer cursor has no block hash"
        Just (safeBlockNumber, Just indexedSafeBlockHash) -> do
          eSafeBlock <- ethGetBlockByNumber client safeBlockNumber
          case eSafeBlock of
            Left err -> logRpcFailure "read finalized indexer block" err
            Right safeBlock
              | not $ sameHash indexedSafeBlockHash (rpcBlockHash safeBlock) ->
                  putStrLn $
                    "Insights snapshot skipped: finalized indexer cursor hash "
                      <> T.unpack indexedSafeBlockHash
                      <> " does not match RPC block hash "
                      <> T.unpack (rpcBlockHash safeBlock)
              | otherwise -> do
                  withDb pool $ \conn ->
                    invalidateSnapshotBatchesAfter
                      conn
                      (icrSlug competition)
                      (rpcBlockNumber safeBlock)
                  updateCompetition client pool cfg competition safeBlock

updateCompetition
  :: EthClient
  -> DbPool
  -> Config
  -> CompetitionRow
  -> RpcBlock
  -> IO ()
updateCompetition client pool cfg competition safeBlock = do
  mStartBlocks <-
    resolveStartBlocks
      client
      safeBlock
      (icrStartTimestamp competition)
      (icrStartBlock competition)
  mCutoffBlock <-
    resolveFinalBlock
      client
      safeBlock
      (icrScoreCutoffTimestamp competition)
      (icrScoreCutoffBlock competition)

  withDb pool $ \conn ->
    setCompetitionBoundaryBlocks
      conn
      (icrSlug competition)
      (boundaryIdentity . snd <$> mStartBlocks)
      (boundaryIdentity <$> mCutoffBlock)

  participants <-
    withDb pool $ \conn ->
      listCompetitionParticipants conn (icrSlug competition)

  forM_ mStartBlocks $ \(startSnapshotBlock, _) ->
    captureUnlessComplete
      client
      pool
      cfg
      competition
      participants
      SnapshotStart
      startSnapshotBlock

  case mCutoffBlock of
    Just cutoffBlock ->
      captureUnlessComplete
        client
        pool
        cfg
        competition
        participants
        SnapshotFinal
        cutoffBlock
    Nothing ->
      when
        ( rpcBlockTimestamp safeBlock >= icrStartTimestamp competition
            && rpcBlockTimestamp safeBlock < icrScoreCutoffTimestamp competition
        )
        (captureUnlessComplete client pool cfg competition participants SnapshotLive safeBlock)

-- The stored start block is the first block in the competition window, while
-- the account baseline is read from the immediately preceding block.
resolveStartBlocks
  :: EthClient
  -> RpcBlock
  -> Integer
  -> Maybe Integer
  -> IO (Maybe (RpcBlock, RpcBlock))
resolveStartBlocks client safeBlock targetTimestamp configuredBlock =
  case configuredBlock of
    Just blockNumber
      | blockNumber <= 0 -> pure Nothing
      | blockNumber > rpcBlockNumber safeBlock -> pure Nothing
      | otherwise -> do
          baselineResult <- ethGetBlockByNumber client (blockNumber - 1)
          boundaryResult <- ethGetBlockByNumber client blockNumber
          case (baselineResult, boundaryResult) of
            (Right baseline, Right boundary) -> pure $ Just (baseline, boundary)
            (Left err, _) -> logRpcFailure "read configured competition baseline" err >> pure Nothing
            (_, Left err) -> logRpcFailure "read configured competition start boundary" err >> pure Nothing
    Nothing
      | rpcBlockTimestamp safeBlock < targetTimestamp -> pure Nothing
      | otherwise -> do
          result <-
            findLastBlockBeforeTimestamp
              (ethGetBlockByNumber client)
              (rpcBlockNumber safeBlock)
              targetTimestamp
          case result of
            Left err -> logRpcFailure "resolve competition baseline" err >> pure Nothing
            Right Nothing -> pure Nothing
            Right (Just baseline) -> do
              boundaryResult <- ethGetBlockByNumber client (rpcBlockNumber baseline + 1)
              case boundaryResult of
                Left err -> logRpcFailure "read competition start boundary" err >> pure Nothing
                Right boundary -> pure $ Just (baseline, boundary)

resolveFinalBlock
  :: EthClient
  -> RpcBlock
  -> Integer
  -> Maybe Integer
  -> IO (Maybe RpcBlock)
resolveFinalBlock client safeBlock targetTimestamp configuredBlock =
  case configuredBlock of
    Just blockNumber
      | blockNumber > rpcBlockNumber safeBlock -> pure Nothing
      | otherwise -> do
          result <- ethGetBlockByNumber client blockNumber
          case result of
            Left err -> logRpcFailure "read configured competition cutoff" err >> pure Nothing
            Right block -> pure $ Just block
    Nothing
      | rpcBlockTimestamp safeBlock < targetTimestamp -> pure Nothing
      | otherwise -> do
          result <-
            findLastBlockBeforeTimestamp
              (ethGetBlockByNumber client)
              (rpcBlockNumber safeBlock)
              targetTimestamp
          case result of
            Left err -> logRpcFailure "resolve competition cutoff" err >> pure Nothing
            Right block -> pure block

captureUnlessComplete
  :: EthClient
  -> DbPool
  -> Config
  -> CompetitionRow
  -> [ParticipantRow]
  -> SnapshotKind
  -> RpcBlock
  -> IO ()
captureUnlessComplete client pool cfg competition participants kind block = do
  complete <-
    withDb pool $ \conn ->
      hasCompleteAccountSnapshotBatch
        conn
        (icrSlug competition)
        kind
        (rpcBlockNumber block)
        (rpcBlockHash block)
  unless complete $
    captureBatch client pool cfg competition participants kind block

captureBatch
  :: EthClient
  -> DbPool
  -> Config
  -> CompetitionRow
  -> [ParticipantRow]
  -> SnapshotKind
  -> RpcBlock
  -> IO ()
captureBatch client pool cfg competition participants kind block
  | null participants = pure ()
  | otherwise = do
      results <- forM participants $ \participant -> do
        result <-
          getAccountLedgerSnapshotAtBlock
            client
            (cfgPerpsAccountLens cfg)
            (iprWallet participant)
            (rpcBlockNumber block)
        pure (participant, result)
      let failures = [(participant, err) | (participant, Left err) <- results]
      if null failures
        then do
          -- A numeric eth_call block tag is resolved independently by the RPC
          -- provider. Re-read the hash after every wallet read so a mid-cycle
          -- reorg cannot publish a mixed-fork batch under the original hash.
          verification <- ethGetBlockByNumber client (rpcBlockNumber block)
          case verification of
            Left err -> logRpcFailure "re-verify snapshot batch block" err
            Right verifiedBlock
              | not $ sameHash (rpcBlockHash block) (rpcBlockHash verifiedBlock) ->
                  putStrLn $
                    "Insights snapshot batch discarded: block "
                      <> show (rpcBlockNumber block)
                      <> " changed from "
                      <> T.unpack (rpcBlockHash block)
                      <> " to "
                      <> T.unpack (rpcBlockHash verifiedBlock)
              | otherwise ->
                  withDb pool $ \conn ->
                    publishAccountSnapshotBatch conn $
                      [ snapshotInput participant ledger
                      | (participant, Right ledger) <- results
                      ]
        else
          forM_ failures $ \(participant, err) ->
            logRpcFailure
              ( "snapshot "
                  <> T.unpack (iprWallet participant)
                  <> " at block "
                  <> show (rpcBlockNumber block)
              )
              err
  where
    snapshotInput participant ledger =
      AccountSnapshotInput
        { asiCompetitionSlug = icrSlug competition
        , asiWallet = iprWallet participant
        , asiKind = kind
        , asiChainId = icrChainId competition
        , asiReleaseRouter = icrReleaseRouter competition
        , asiBlockNumber = rpcBlockNumber block
        , asiBlockHash = rpcBlockHash block
        , asiTimestamp = rpcBlockTimestamp block
        , asiEquity = ledgerToEquity ledger
        , asiRawData = snapshotToJson ledger
        }

ledgerToEquity :: AccountLedgerSnapshot -> EquitySnapshot
ledgerToEquity AccountLedgerSnapshot {..} =
  EquitySnapshot
    { esHasOpenPosition = alsHasPosition
    , esSignedNetEquityUsdc = alsNetEquityUsdc
    , esTerminalReachableUsdc = alsTerminalReachableUsdc
    , esTraderClaimsUsdc = alsTraderClaimBalanceUsdc
    }

snapshotToJson :: AccountLedgerSnapshot -> Value
snapshotToJson AccountLedgerSnapshot {..} =
  object
    [ "settlementBalanceUsdc" .= show alsSettlementBalanceUsdc
    , "freeSettlementUsdc" .= show alsFreeSettlementUsdc
    , "activePositionMarginUsdc" .= show alsActivePositionMarginUsdc
    , "otherLockedMarginUsdc" .= show alsOtherLockedMarginUsdc
    , "positionMarginBucketUsdc" .= show alsPositionMarginBucketUsdc
    , "committedOrderMarginBucketUsdc" .= show alsCommittedOrderMarginBucketUsdc
    , "reservedSettlementBucketUsdc" .= show alsReservedSettlementBucketUsdc
    , "executionBountyReserveUsdc" .= show alsExecutionBountyReserveUsdc
    , "committedMarginUsdc" .= show alsCommittedMarginUsdc
    , "traderClaimBalanceUsdc" .= show alsTraderClaimBalanceUsdc
    , "pendingOrderCount" .= show alsPendingOrderCount
    , "closeReachableUsdc" .= show alsCloseReachableUsdc
    , "terminalReachableUsdc" .= show alsTerminalReachableUsdc
    , "accountEquityUsdc" .= show alsAccountEquityUsdc
    , "freeBuyingPowerUsdc" .= show alsFreeBuyingPowerUsdc
    , "hasPosition" .= alsHasPosition
    , "side" .= show alsSide
    , "size" .= show alsSize
    , "margin" .= show alsMargin
    , "entryPrice" .= show alsEntryPrice
    , "unrealizedPnlUsdc" .= show alsUnrealizedPnlUsdc
    , "netEquityUsdc" .= show alsNetEquityUsdc
    , "liquidatable" .= alsLiquidatable
    ]

-- | Find the greatest block whose timestamp is strictly before the boundary.
-- This defines the baseline for a half-open competition window: account state
-- at B0, then events in (B0, B1].
findLastBlockBeforeTimestamp
  :: (Integer -> IO (Either e RpcBlock))
  -> Integer
  -> Integer
  -> IO (Either e (Maybe RpcBlock))
findLastBlockBeforeTimestamp fetch upperBlock targetTimestamp = do
  highResult <- fetch upperBlock
  case highResult of
    Left err -> pure $ Left err
    Right high
      | rpcBlockTimestamp high < targetTimestamp -> pure $ Right $ Just high
      | otherwise -> do
          lowResult <- fetch 0
          case lowResult of
            Left err -> pure $ Left err
            Right low
              | rpcBlockTimestamp low >= targetTimestamp -> pure $ Right Nothing
              | otherwise -> search low high
  where
    search low high
      | rpcBlockNumber high - rpcBlockNumber low <= 1 = pure $ Right $ Just low
      | otherwise = do
          let midpoint =
                rpcBlockNumber low
                  + (rpcBlockNumber high - rpcBlockNumber low) `div` 2
          midpointResult <- fetch midpoint
          case midpointResult of
            Left err -> pure $ Left err
            Right candidate
              | rpcBlockTimestamp candidate < targetTimestamp -> search candidate high
              | otherwise -> search low candidate

logRpcFailure :: String -> RpcError -> IO ()
logRpcFailure context err =
  putStrLn $ "Insights snapshot failed to " <> context <> ": " <> show err

boundaryIdentity :: RpcBlock -> (Integer, T.Text)
boundaryIdentity block = (rpcBlockNumber block, rpcBlockHash block)

sameHash :: T.Text -> T.Text -> Bool
sameHash left right = T.toLower (T.strip left) == T.toLower (T.strip right)
