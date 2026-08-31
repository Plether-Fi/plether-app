module Plether.Keeper.LpSettlementDatabaseSpec
  ( lpSettlementDatabaseSpec
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , execute
  , execute_
  , query
  , query_
  , withTransaction
  )
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Schema
  ( LpSettlementBroadcastInput (..)
  , LpSettlementBroadcastRow (..)
  , LpSettlementEventOutcome (..)
  , LpSettlementObservationInput (..)
  , LpSettlementReceiptInput (..)
  , LpSettlementSignedIntent (..)
  , LpSettlementTransactionRow (..)
  , appendLpSettlementBroadcast
  , clearLpSettlementReorgedReceiptEvidence
  , ensurePerpsKeeperSchema
  , getActiveLpSettlementTransaction
  , getLpSettlementBroadcasts
  , getLpSettlementObservationObservedBlock
  , getLpSettlementTransactionById
  , getLpSettlementTransactionFamily
  , markLpSettlementAttemptSubmitted
  , markLpSettlementTransactionConfirming
  , markLpSettlementTransactionManualReview
  , prepareLpSettlementTransaction
  , recordLpSettlementObservationV2
  , recordLpSettlementReceipt
  , recordLpSettlementReceiptForManualReview
  , recordLpSettlementSupersededReceipt
  , recordLpSettlementObservation
  , replaceLpSettlementTransaction
  , tryPerpsKeeperLock
  , tryLpSettlementKeeperLock
  , unlockPerpsKeeperLock
  , unlockLpSettlementKeeperLock
  , verifyLpSettlementSchema
  , verifyNoLegacySubmittedLpSettlementAttempts
  )
import Test.Hspec
  ( Expectation
  , Spec
  , describe
  , expectationFailure
  , it
  , shouldBe
  , shouldSatisfy
  )

lpSettlementDatabaseSpec :: Text -> Spec
lpSettlementDatabaseSpec databaseUrl =
  describe "LP settlement keeper PostgreSQL durability" $ do
    it "keeps observation identity immutable" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        let observation = fixtureObservation fixtureMonitor fixtureDigest
        recordLpSettlementObservationV2 conn observation
        recordLpSettlementObservationV2 conn observation

        shouldFail $
          recordLpSettlementObservationV2
            conn
            observation {lsoiWarningMask = lsoiWarningMask observation + 1}
        shouldFail $
          void $
            execute conn
              "UPDATE perps_lp_settlement_observations SET epoch = epoch + 1 \
              \WHERE chain_id = ? AND monitor_address = ? AND observation_digest = ?"
              (fixtureChainId, fixtureMonitor, fixtureDigest)

        warnings <- query conn
          "SELECT warning_mask::text FROM perps_lp_settlement_observations \
          \WHERE chain_id = ? AND monitor_address = ? AND observation_digest = ?"
          (fixtureChainId, fixtureMonitor, fixtureDigest) :: IO [Only Text]
        warnings `shouldBe` [Only "1"]

    it "persists the complete signed intent before any broadcast history" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitor fixtureDigest
        prepared <- prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashA 10 20
        persisted <- getLpSettlementTransactionById conn $ lstrId prepared
        broadcasts <- getLpSettlementBroadcasts conn $ lstrId prepared
        observedBlock <- getLpSettlementObservationObservedBlock conn $ lstrId prepared

        fmap lstrStatus persisted `shouldBe` Just "prepared"
        fmap lstrEpoch persisted `shouldBe` Just fixtureEpoch
        fmap lstrSignerAddress persisted `shouldBe` Just fixtureSigner
        fmap lstrNonce persisted `shouldBe` Just 9
        fmap lstrTargetAddress persisted `shouldBe` Just fixtureTarget
        fmap lstrValue persisted `shouldBe` Just 5
        fmap lstrCalldata persisted `shouldBe` Just fixtureCalldata
        fmap lstrSignedRawTransaction persisted `shouldBe` Just fixtureRawA
        fmap lstrSignedTransactionHash persisted `shouldBe` Just fixtureTxHashA
        broadcasts `shouldBe` []
        observedBlock `shouldBe` Just 100

    it "enforces active monitor and signer lanes, including manual review" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitor fixtureDigest
        recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitorB fixtureDigestB
        first <- prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashA 10 20
        shouldFail $
          prepareLpSettlementTransaction conn $
            (fixtureIntent fixtureMonitor fixtureDigest 10 fixtureTxHashB 10 20)
              {lssiSignerAddress = fixtureSignerB}
        shouldFail $
          prepareLpSettlementTransaction conn $
            fixtureIntent fixtureMonitorB fixtureDigestB 10 fixtureTxHashC 10 20
        markLpSettlementTransactionManualReview conn (lstrId first) "ambiguous node response"
        active <- getActiveLpSettlementTransaction conn fixtureChainId fixtureMonitor fixtureSigner
        fmap lstrStatus active `shouldBe` Just "manual_review"
        shouldFail $
          prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 10 fixtureTxHashB 10 20
        shouldFail $
          void $ execute conn
            "UPDATE perps_lp_settlement_transactions SET status = 'failed' WHERE id = ?"
            (Only $ lstrId first)
        shouldFail $
          void $ execute conn
            "UPDATE perps_lp_settlement_transactions SET status = 'pending' WHERE id = ?"
            (Only $ lstrId first)
        shouldFail $
          void $ execute conn
            "UPDATE perps_lp_settlement_transactions SET status = 'superseded' WHERE id = ?"
            (Only $ lstrId first)

    it "fails closed when monitor and signer rotation each match a different active row" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitor fixtureDigest
        recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitorB fixtureDigestB
        _ <- prepareLpSettlementTransaction conn $
          (fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashA 10 20)
            {lssiSignerAddress = fixtureSignerB}
        _ <- prepareLpSettlementTransaction conn $
          fixtureIntent fixtureMonitorB fixtureDigestB 10 fixtureTxHashB 10 20
        shouldFail $
          getActiveLpSettlementTransaction conn fixtureChainId fixtureMonitor fixtureSigner

    it "retains append-only broadcasts and same-nonce replacement lineage" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitor fixtureDigest
        original <- prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashA 10 20
        firstBroadcast <- appendLpSettlementBroadcast conn
          LpSettlementBroadcastInput
            { lsbiAttemptId = lstrId original
            , lsbiOutcome = "ambiguous"
            , lsbiReturnedTransactionHash = Nothing
            , lsbiRpcError = Just "timeout"
            }
        secondBroadcast <- appendLpSettlementBroadcast conn
          LpSettlementBroadcastInput
            { lsbiAttemptId = lstrId original
            , lsbiOutcome = "rejected"
            , lsbiReturnedTransactionHash = Nothing
            , lsbiRpcError = Just "underpriced"
            }
        lsbrBroadcastSequence firstBroadcast `shouldBe` 1
        lsbrBroadcastSequence secondBroadcast `shouldBe` 2
        active <- getActiveLpSettlementTransaction conn fixtureChainId fixtureMonitor fixtureSigner
        fmap lstrStatus active `shouldBe` Just "pending"

        shouldFailContaining "duplicate key value violates unique constraint" $
          replaceLpSettlementTransaction conn (lstrId original) 11 21 fixtureRawB fixtureTxHashA
        rolledBack <- requireTransaction conn $ lstrId original
        lstrStatus rolledBack `shouldBe` "pending"
        getLpSettlementTransactionFamily conn (lstrId original) >>= (`shouldSatisfy` ((== 1) . length))

        shouldFailContaining "replaced LP settlement transaction must retain a successor" $
          void $ execute conn
            "UPDATE perps_lp_settlement_transactions SET status = 'replaced' WHERE id = ?"
            (Only $ lstrId original)
        stillPending <- requireTransaction conn $ lstrId original
        lstrStatus stillPending `shouldBe` "pending"

        replacementOne <-
          replaceLpSettlementTransaction conn (lstrId original) 11 21 fixtureRawB fixtureTxHashB
        replacementTwo <-
          replaceLpSettlementTransaction conn (lstrId replacementOne) 12 22 fixtureRawC fixtureTxHashC
        lstrReplacementCount replacementOne `shouldBe` 1
        lstrReplacementCount replacementTwo `shouldBe` 2
        lstrReplacesAttemptId replacementOne `shouldBe` Just (lstrId original)
        lstrReplacesAttemptId replacementTwo `shouldBe` Just (lstrId replacementOne)
        lstrNonce replacementTwo `shouldBe` lstrNonce original
        lstrSignerAddress replacementTwo `shouldBe` lstrSignerAddress original
        lstrTargetAddress replacementTwo `shouldBe` lstrTargetAddress original
        lstrValue replacementTwo `shouldBe` lstrValue original
        lstrCalldata replacementTwo `shouldBe` lstrCalldata original
        lstrMaxPriorityFeePerGas replacementTwo `shouldBe` 12
        lstrMaxFeePerGas replacementTwo `shouldBe` 22
        lstrSignedRawTransaction replacementOne `shouldBe` fixtureRawB
        lstrSignedTransactionHash replacementOne `shouldBe` fixtureTxHashB
        lstrSignedRawTransaction replacementTwo `shouldBe` fixtureRawC
        lstrSignedTransactionHash replacementTwo `shouldBe` fixtureTxHashC
        family <- getLpSettlementTransactionFamily conn $ lstrId replacementTwo
        length family `shouldBe` 3
        map lstrReplacementCount family `shouldBe` [2, 1, 0]
        broadcasts <- getLpSettlementBroadcasts conn $ lstrId original
        map lsbrBroadcastSequence broadcasts `shouldBe` [1, 2]

        shouldFail $
          void $
            execute conn
              "UPDATE perps_lp_settlement_broadcasts SET rpc_error = 'rewritten' WHERE id = ?"
              (Only $ lsbrId firstBroadcast)
        shouldFail $
          void $
            execute conn
              "UPDATE perps_lp_settlement_transactions SET target_address = ? WHERE id = ?"
              (hexText 40 "e", lstrId replacementTwo)

    it "records a mined predecessor and supersedes its active replacement atomically" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitor fixtureDigest
        original <- prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashA 10 20
        _ <- appendLpSettlementBroadcast conn
          LpSettlementBroadcastInput
            { lsbiAttemptId = lstrId original
            , lsbiOutcome = "ambiguous"
            , lsbiReturnedTransactionHash = Nothing
            , lsbiRpcError = Just "timeout"
            }
        replacement <-
          replaceLpSettlementTransaction conn (lstrId original) 11 21 fixtureRawB fixtureTxHashB

        recordLpSettlementReceipt conn $
          fixtureReceipt (lstrId original) fixtureTxHashA (Just fixtureEvent)
        predecessor <- requireTransaction conn $ lstrId original
        successor <- requireTransaction conn $ lstrId replacement
        lstrStatus predecessor `shouldBe` "confirmed_success"
        lstrReceiptBlockHash predecessor `shouldBe` Just fixtureReceiptBlockHash
        lstrConfirmationDepth predecessor `shouldBe` Just 6
        lstrCutoffEpoch predecessor `shouldBe` Just fixtureEpoch
        lstrSeniorRedeemAssets predecessor `shouldBe` Just 11
        lstrJuniorRedeemAssets predecessor `shouldBe` Just 12
        lstrJuniorDepositAssets predecessor `shouldBe` Just 13
        lstrSeniorDepositAssets predecessor `shouldBe` Just 14
        lstrSeniorBacklog predecessor `shouldBe` Just True
        lstrJuniorBacklog predecessor `shouldBe` Just False
        lstrEntriesDeferred predecessor `shouldBe` Just True
        lstrStatus successor `shouldBe` "superseded"
        shouldReturnNothing $
          getActiveLpSettlementTransaction conn fixtureChainId fixtureMonitor fixtureSigner
        shouldFailContaining "invalid LP settlement transaction status transition" $
          recordLpSettlementReceipt conn $
            fixtureReceipt (lstrId replacement) fixtureTxHashB (Just fixtureEvent)
        shouldFailContaining "terminal LP settlement evidence is immutable" $
          void $ execute conn
            "UPDATE perps_lp_settlement_transactions SET receipt_block_hash = ? WHERE id = ?"
            (hexText 64 "c", lstrId original)
        shouldFailContaining "duplicate key value violates unique constraint" $
          prepareLpSettlementTransaction conn $
            fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashC 13 23

    it "serializes replacement against a concurrently observed predecessor receipt" $
      withLpSettlementDatabase databaseUrl $ \pool -> do
        original <- withDb pool $ \conn -> do
          recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitor fixtureDigest
          prepared <- prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashA 10 20
          _ <- appendLpSettlementBroadcast conn
            LpSettlementBroadcastInput
              { lsbiAttemptId = lstrId prepared
              , lsbiOutcome = "ambiguous"
              , lsbiReturnedTransactionHash = Nothing
              , lsbiRpcError = Just "timeout"
              }
          pure prepared
        (replacementResult, receiptResult) <- runConcurrently
          (withDb pool $ \conn -> void $
            replaceLpSettlementTransaction conn (lstrId original) 11 21 fixtureRawB fixtureTxHashB)
          (withDb pool $ \conn ->
            recordLpSettlementReceipt conn $
              fixtureReceipt (lstrId original) fixtureTxHashA (Just fixtureEvent))
        receiptResult `shouldSatisfy` isRight
        replacementResult `shouldSatisfy` \result -> isLeft result || isRight result
        withDb pool $ \conn -> do
          predecessor <- requireTransaction conn $ lstrId original
          lstrStatus predecessor `shouldBe` "confirmed_success"
          family <- getLpSettlementTransactionFamily conn $ lstrId original
          map lstrStatus (filter ((/= lstrId original) . lstrId) family)
            `shouldSatisfy` (`elem` [[], ["superseded"]])
          shouldReturnNothing $
            getActiveLpSettlementTransaction conn fixtureChainId fixtureMonitor fixtureSigner

    it "serializes idempotent concurrent receipts for one signed transaction" $
      withLpSettlementDatabase databaseUrl $ \pool -> do
        prepared <- withDb pool $ \conn -> do
          recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitor fixtureDigest
          prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashA 10 20
        let persistReceipt = withDb pool $ \conn ->
              recordLpSettlementReceipt conn $
                fixtureReceipt (lstrId prepared) fixtureTxHashA (Just fixtureEvent)
        (firstResult, secondResult) <- runConcurrently persistReceipt persistReceipt
        firstResult `shouldSatisfy` isRight
        secondResult `shouldSatisfy` isRight
        withDb pool $ \conn -> do
          persisted <- requireTransaction conn $ lstrId prepared
          lstrStatus persisted `shouldBe` "confirmed_success"
          lstrConfirmationDepth persisted `shouldBe` Just 6

    it "clears reorged evidence without releasing manual review" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitor fixtureDigest
        prepared <- prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashA 10 20
        _ <- appendLpSettlementBroadcast conn
          LpSettlementBroadcastInput
            { lsbiAttemptId = lstrId prepared
            , lsbiOutcome = "accepted"
            , lsbiReturnedTransactionHash = Just fixtureTxHashA
            , lsbiRpcError = Nothing
            }
        markLpSettlementTransactionConfirming
          conn (lstrId prepared) fixtureTxHashA 120 fixtureReceiptBlockHash True 3
        clearLpSettlementReorgedReceiptEvidence conn $ lstrId prepared
        pending <- requireTransaction conn $ lstrId prepared
        lstrStatus pending `shouldBe` "pending"
        lstrReceiptTransactionHash pending `shouldBe` Nothing
        lstrReceiptBlockHash pending `shouldBe` Nothing
        lstrConfirmationDepth pending `shouldBe` Nothing

        markLpSettlementTransactionConfirming
          conn (lstrId prepared) fixtureTxHashA 121 (hexText 64 "b") True 2
        markLpSettlementTransactionManualReview conn (lstrId prepared) "invalid settlement event"
        markLpSettlementTransactionConfirming
          conn (lstrId prepared) fixtureTxHashA 121 (hexText 64 "b") True 3
        clearLpSettlementReorgedReceiptEvidence conn $ lstrId prepared
        reviewed <- requireTransaction conn $ lstrId prepared
        lstrStatus reviewed `shouldBe` "manual_review"
        lstrLastError reviewed `shouldBe` Just "invalid settlement event"
        lstrReceiptTransactionHash reviewed `shouldBe` Nothing
        lstrReceiptBlockHash reviewed `shouldBe` Nothing
        lstrConfirmationDepth reviewed `shouldBe` Nothing

    it "keeps invalid receipt evidence active for manual review" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitor fixtureDigest
        prepared <- prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashA 10 20
        shouldFail $
          recordLpSettlementReceipt conn $
            (fixtureReceipt (lstrId prepared) fixtureTxHashA Nothing)
              {lsriSucceeded = False}
        recordLpSettlementReceiptForManualReview
          conn
          (fixtureReceipt (lstrId prepared) fixtureTxHashA Nothing)
          "successful receipt omitted canonical settlement event"
        recordLpSettlementReceiptForManualReview
          conn
          ( (fixtureReceipt (lstrId prepared) fixtureTxHashA Nothing)
              {lsriConfirmationDepth = 7}
          )
          "successful receipt omitted canonical settlement event"
        reviewed <- requireTransaction conn $ lstrId prepared
        lstrStatus reviewed `shouldBe` "manual_review"
        lstrConfirmedAt reviewed `shouldBe` Nothing
        lstrReceiptBlockHash reviewed `shouldBe` Just fixtureReceiptBlockHash
        lstrConfirmationDepth reviewed `shouldBe` Just 7
        active <- getActiveLpSettlementTransaction conn fixtureChainId fixtureMonitor fixtureSigner
        fmap lstrId active `shouldBe` Just (lstrId prepared)
        shouldFail $
          prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 10 fixtureTxHashB 10 20

    it "preserves a predecessor's invalid receipt and blocks on its active replacement" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitor fixtureDigest
        original <- prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashA 10 20
        _ <- appendLpSettlementBroadcast conn
          LpSettlementBroadcastInput
            { lsbiAttemptId = lstrId original
            , lsbiOutcome = "ambiguous"
            , lsbiReturnedTransactionHash = Nothing
            , lsbiRpcError = Just "timeout"
            }
        replacement <-
          replaceLpSettlementTransaction conn (lstrId original) 11 21 fixtureRawB fixtureTxHashB
        recordLpSettlementReceiptForManualReview
          conn
          (fixtureReceipt (lstrId original) fixtureTxHashA Nothing)
          "predecessor receipt omitted canonical settlement event"
        predecessor <- requireTransaction conn $ lstrId original
        successor <- requireTransaction conn $ lstrId replacement
        lstrStatus predecessor `shouldBe` "replaced"
        lstrReceiptTransactionHash predecessor `shouldBe` Just fixtureTxHashA
        lstrConfirmedAt predecessor `shouldBe` Nothing
        lstrStatus successor `shouldBe` "manual_review"
        active <- getActiveLpSettlementTransaction conn fixtureChainId fixtureMonitor fixtureSigner
        fmap lstrId active `shouldBe` Just (lstrId replacement)
        clearLpSettlementReorgedReceiptEvidence conn $ lstrId original
        clearedPredecessor <- requireTransaction conn $ lstrId original
        lstrStatus clearedPredecessor `shouldBe` "replaced"
        lstrLastError clearedPredecessor
          `shouldBe` Just "predecessor receipt omitted canonical settlement event"
        lstrReceiptTransactionHash clearedPredecessor `shouldBe` Nothing
        lstrReceiptBlockHash clearedPredecessor `shouldBe` Nothing

    it "records a benign predecessor revert as receipt-backed superseded history" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        recordLpSettlementObservationV2 conn $ fixtureObservation fixtureMonitor fixtureDigest
        original <- prepareLpSettlementTransaction conn $ fixtureIntent fixtureMonitor fixtureDigest 9 fixtureTxHashA 10 20
        _ <- appendLpSettlementBroadcast conn
          LpSettlementBroadcastInput
            { lsbiAttemptId = lstrId original
            , lsbiOutcome = "ambiguous"
            , lsbiReturnedTransactionHash = Nothing
            , lsbiRpcError = Just "timeout"
            }
        replacement <-
          replaceLpSettlementTransaction conn (lstrId original) 11 21 fixtureRawB fixtureTxHashB
        let revertedReceipt =
              (fixtureReceipt (lstrId original) fixtureTxHashA Nothing)
                {lsriSucceeded = False}
        recordLpSettlementSupersededReceipt
          conn
          revertedReceipt
          "permissionless competitor cleared the matured work"
        predecessor <- requireTransaction conn $ lstrId original
        successor <- requireTransaction conn $ lstrId replacement
        lstrStatus predecessor `shouldBe` "superseded"
        lstrReceiptSucceeded predecessor `shouldBe` Just False
        lstrReceiptBlockHash predecessor `shouldBe` Just fixtureReceiptBlockHash
        lstrConfirmationDepth predecessor `shouldBe` Just 6
        lstrConfirmedAt predecessor `shouldSatisfy` maybe False (const True)
        lstrStatus successor `shouldBe` "superseded"
        lstrReceiptTransactionHash successor `shouldBe` Nothing
        shouldReturnNothing $
          getActiveLpSettlementTransaction conn fixtureChainId fixtureMonitor fixtureSigner

    it "fails closed on same-chain legacy submissions after monitor rotation" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        verifyNoLegacySubmittedLpSettlementAttempts conn fixtureChainId
          `shouldReturnRight` ()
        recordLpSettlementObservation
          conn
          fixtureChainId
          fixtureMonitorB
          fixtureDigestB
          fixtureEpoch
          100
          2
          0
          1
          0
          0
        markLpSettlementAttemptSubmitted
          conn
          fixtureChainId
          fixtureMonitorB
          fixtureDigestB
          fixtureTxHashA
        result <- verifyNoLegacySubmittedLpSettlementAttempts conn fixtureChainId
        result `shouldSatisfy` isLeft

    it "skips malformed legacy observations without fabricating v2 identity" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        recordLpSettlementObservation
          conn
          fixtureChainId
          fixtureMonitorB
          fixtureDigestB
          fixtureEpoch
          100
          2
          (10 ^ (78 :: Int))
          1
          0
          0
        ensurePerpsKeeperSchema conn
        migrated <- query conn
          "SELECT observation_digest FROM perps_lp_settlement_observations \
          \WHERE chain_id = ? AND monitor_address = ? AND observation_digest = ?"
          (fixtureChainId, fixtureMonitorB, fixtureDigestB) :: IO [Only Text]
        migrated `shouldBe` []

    it "repairs a same-name successful epoch check containing the expected tokens plus AND FALSE" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        (do
          void $ execute_ conn
            "ALTER TABLE perps_lp_settlement_transactions \
            \DROP CONSTRAINT perps_lp_settlement_success_epoch_check"
          void $ execute_ conn
            "ALTER TABLE perps_lp_settlement_transactions \
            \ADD CONSTRAINT perps_lp_settlement_success_epoch_check \
            \CHECK ((status <> 'confirmed_success' OR cutoff_epoch = epoch) AND FALSE)"
          verifyLpSettlementSchema conn >>= (`shouldSatisfy` isLeft)
          ensurePerpsKeeperSchema conn
          verifyLpSettlementSchema conn `shouldReturnRight` ()
          ) `finally` do
            void $ execute_ conn
              "ALTER TABLE perps_lp_settlement_transactions \
              \DROP CONSTRAINT IF EXISTS perps_lp_settlement_success_epoch_check"
            ensurePerpsKeeperSchema conn

    it "converges a pre-release status domain that rejects replacement states" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        (do
          void $ execute_ conn
            "DO $$ DECLARE existing record; BEGIN \
            \FOR existing IN SELECT k.conname FROM pg_constraint k \
            \WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass AND k.contype = 'c' \
            \AND k.conkey = ARRAY[(SELECT a.attnum FROM pg_attribute a \
            \ WHERE a.attrelid = k.conrelid AND a.attname = 'status')] \
            \LOOP EXECUTE format('ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT %I', existing.conname); \
            \END LOOP; END $$"
          void $ execute_ conn
            "ALTER TABLE perps_lp_settlement_transactions \
            \ADD CONSTRAINT perps_lp_settlement_status_domain_check CHECK (status IN \
            \('prepared', 'broadcast', 'pending', 'confirming', 'manual_review', \
            \'confirmed_success', 'confirmed_revert', 'superseded'))"
          verifyLpSettlementSchema conn >>= (`shouldSatisfy` isLeft)
          ensurePerpsKeeperSchema conn
          verifyLpSettlementSchema conn `shouldReturnRight` ()
          ) `finally` ensurePerpsKeeperSchema conn

    it "additively installs exact one-successor uniqueness" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        (do
          void $ execute_ conn
            "DO $$ DECLARE existing record; BEGIN \
            \FOR existing IN SELECT conname FROM pg_constraint \
            \WHERE conrelid = 'perps_lp_settlement_transactions'::regclass AND contype = 'u' \
            \AND pg_get_constraintdef(oid, true) = 'UNIQUE (replaces_attempt_id)' \
            \LOOP EXECUTE format('ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT %I', existing.conname); \
            \END LOOP; END $$"
          void $ execute_ conn
            "ALTER TABLE perps_lp_settlement_transactions \
            \ADD CONSTRAINT perps_lp_settlement_replaces_attempt_unique \
            \UNIQUE (replaces_attempt_id, status)"
          verifyLpSettlementSchema conn >>= (`shouldSatisfy` isLeft)
          ensurePerpsKeeperSchema conn
          verifyLpSettlementSchema conn `shouldReturnRight` ()
          ) `finally` do
            void $ execute_ conn
              "ALTER TABLE perps_lp_settlement_transactions \
              \DROP CONSTRAINT IF EXISTS perps_lp_settlement_replaces_attempt_unique"
            ensurePerpsKeeperSchema conn

    it "repairs a pre-release signer index with extra key columns" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        (do
          void $ execute_ conn "DROP INDEX idx_perps_lp_settlement_one_active_signer"
          void $ execute_ conn
            "CREATE UNIQUE INDEX idx_perps_lp_settlement_one_active_signer \
            \ON perps_lp_settlement_transactions(chain_id, signer_address, tx_nonce) \
            \WHERE status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review')"
          drifted <- verifyLpSettlementSchema conn
          drifted `shouldSatisfy` isLeft
          ensurePerpsKeeperSchema conn
          verifyLpSettlementSchema conn `shouldReturnRight` ()
          ) `finally` ensurePerpsKeeperSchema conn

    it "repairs an active signer index whose predicate also includes replaced rows" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        (do
          void $ execute_ conn "DROP INDEX idx_perps_lp_settlement_one_active_signer"
          void $ execute_ conn
            "CREATE UNIQUE INDEX idx_perps_lp_settlement_one_active_signer \
            \ON perps_lp_settlement_transactions(chain_id, signer_address) \
            \WHERE status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review', 'replaced')"
          verifyLpSettlementSchema conn >>= (`shouldSatisfy` isLeft)
          ensurePerpsKeeperSchema conn
          verifyLpSettlementSchema conn `shouldReturnRight` ()
          ) `finally` do
            void $ execute_ conn "DROP INDEX IF EXISTS idx_perps_lp_settlement_one_active_signer"
            ensurePerpsKeeperSchema conn

    it "removes a legacy signer-nonce unique index that prevents legal replacements" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        (do
          void $ execute_ conn
            "CREATE UNIQUE INDEX legacy_lp_settlement_signer_nonce_unique \
            \ON perps_lp_settlement_transactions(chain_id, signer_address, tx_nonce)"
          verifyLpSettlementSchema conn >>= (`shouldSatisfy` isLeft)
          ensurePerpsKeeperSchema conn
          verifyLpSettlementSchema conn `shouldReturnRight` ()
          indexes <- query_ conn
            "SELECT indexname::text FROM pg_indexes \
            \WHERE schemaname = current_schema() \
            \AND indexname = 'legacy_lp_settlement_signer_nonce_unique'"
            :: IO [Only Text]
          indexes `shouldBe` []
          ) `finally` do
            void $ execute_ conn "DROP INDEX IF EXISTS legacy_lp_settlement_signer_nonce_unique"
            ensurePerpsKeeperSchema conn

    it "rejects missing insert-critical defaults" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        (do
          void $ execute_ conn
            "ALTER TABLE perps_lp_settlement_transactions ALTER COLUMN replacement_count DROP DEFAULT"
          verifyLpSettlementSchema conn >>= (`shouldSatisfy` isLeft)
          ) `finally` do
            void $ execute_ conn
              "ALTER TABLE perps_lp_settlement_transactions ALTER COLUMN replacement_count SET DEFAULT 0"
        verifyLpSettlementSchema conn `shouldReturnRight` ()

    it "rejects an exact duplicate constraint definition" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        (do
          void $ execute_ conn
            "ALTER TABLE perps_lp_settlement_transactions \
            \ADD CONSTRAINT test_lp_duplicate_epoch_check CHECK (epoch >= 0)"
          verifyLpSettlementSchema conn >>= (`shouldSatisfy` isLeft)
          ) `finally` do
            void $ execute_ conn
              "ALTER TABLE perps_lp_settlement_transactions \
              \DROP CONSTRAINT IF EXISTS test_lp_duplicate_epoch_check"
        verifyLpSettlementSchema conn `shouldReturnRight` ()

    it "rejects an extra unvalidated constraint" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        (do
          void $ execute_ conn
            "ALTER TABLE perps_lp_settlement_transactions \
            \ADD CONSTRAINT test_lp_unvalidated_blocker CHECK (FALSE) NOT VALID"
          verifyLpSettlementSchema conn >>= (`shouldSatisfy` isLeft)
          ) `finally` do
            void $ execute_ conn
              "ALTER TABLE perps_lp_settlement_transactions \
              \DROP CONSTRAINT IF EXISTS test_lp_unvalidated_blocker"
        verifyLpSettlementSchema conn `shouldReturnRight` ()

    it "rejects a trigger whose function body no longer enforces append-only history" $
      withLpSettlementDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        (do
          void $ execute_ conn
            "CREATE OR REPLACE FUNCTION reject_lp_settlement_broadcast_mutation() \
            \RETURNS trigger AS $$ BEGIN \
            \IF FALSE THEN RAISE EXCEPTION 'perps_lp_settlement_broadcasts is append-only'; END IF; \
            \RETURN OLD; END; $$ LANGUAGE plpgsql"
          drifted <- verifyLpSettlementSchema conn
          drifted `shouldSatisfy` isLeft
          ) `finally` ensurePerpsKeeperSchema conn
        verifyLpSettlementSchema conn `shouldReturnRight` ()

    it "excludes a second LP worker with the dedicated advisory lock" $
      withLpSettlementDatabase databaseUrl $ \pool ->
        withDb pool $ \firstConnection ->
          withDb pool $ \secondConnection ->
            (do
              tryLpSettlementKeeperLock firstConnection `shouldReturnBool` True
              tryLpSettlementKeeperLock secondConnection `shouldReturnBool` False
              unlockLpSettlementKeeperLock firstConnection
              tryLpSettlementKeeperLock secondConnection `shouldReturnBool` True
            ) `finally` do
              unlockLpSettlementKeeperLock firstConnection
              unlockLpSettlementKeeperLock secondConnection

    it "keeps the order and LP keeper advisory locks independent" $
      withLpSettlementDatabase databaseUrl $ \pool ->
        withDb pool $ \orderConnection ->
          withDb pool $ \lpConnection ->
            (do
              tryPerpsKeeperLock orderConnection `shouldReturnBool` True
              tryLpSettlementKeeperLock lpConnection `shouldReturnBool` True
            ) `finally` do
              unlockPerpsKeeperLock orderConnection
              unlockLpSettlementKeeperLock lpConnection

withLpSettlementDatabase :: Text -> (DbPool -> IO a) -> IO a
withLpSettlementDatabase databaseUrl action =
  bracket (newDbPool databaseUrl) destroyAllResources $ \pool -> do
    withDb pool $ \conn -> do
      assertDedicatedDatabase conn
      ensurePerpsKeeperSchema conn
      verifyLpSettlementSchema conn `shouldReturnRight` ()
    cleanupLpSettlementTables pool
    action pool `finally` cleanupLpSettlementTables pool

assertDedicatedDatabase :: Connection -> IO ()
assertDedicatedDatabase conn = do
  rows <- query_ conn "SELECT current_database()" :: IO [Only Text]
  case rows of
    [Only databaseName]
      | "critical_path" `T.isInfixOf` T.toLower databaseName -> pure ()
    [Only databaseName] ->
      expectationFailure $
        "Refusing destructive LP settlement integration cleanup against database "
          <> T.unpack databaseName
          <> "; its name must contain critical_path"
    _ -> expectationFailure "PostgreSQL did not return exactly one current_database() row"

cleanupLpSettlementTables :: DbPool -> IO ()
cleanupLpSettlementTables pool =
  withDb pool $ \conn -> withTransaction conn $ do
    void $ execute_ conn
      "TRUNCATE TABLE perps_lp_settlement_broadcasts, \
      \perps_lp_settlement_transactions, perps_lp_settlement_observations, \
      \perps_lp_settlement_attempts \
      \RESTART IDENTITY"

fixtureObservation :: Text -> Text -> LpSettlementObservationInput
fixtureObservation monitor digest =
  LpSettlementObservationInput
    { lsoiChainId = fixtureChainId
    , lsoiMonitorAddress = monitor
    , lsoiObservationDigest = digest
    , lsoiEpoch = fixtureEpoch
    , lsoiObservedBlock = 100
    , lsoiObservedBlockHash = Just $ hexText 64 "a"
    , lsoiExecutionPath = 2
    , lsoiOperationalBlockerMask = 0
    , lsoiWarningMask = 1
    , lsoiDependencyFailureMask = 0
    , lsoiCriticalFaultMask = 0
    , lsoiSchemaVersion = 1
    , lsoiHealthState = 1
    , lsoiExecutionPathDependencyMask = 0
    , lsoiStatusDependencyFailureMask = 0
    , lsoiHealthDependencyFailureMask = 0
    , lsoiObservationComplete = True
    , lsoiHasMaturedWork = True
    , lsoiLpEpochSettlementPaused = False
    }

fixtureIntent
  :: Text
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> LpSettlementSignedIntent
fixtureIntent monitor digest nonce txHash priorityFee maxFee =
  LpSettlementSignedIntent
    { lssiChainId = fixtureChainId
    , lssiMonitorAddress = monitor
    , lssiObservationDigest = digest
    , lssiEpoch = fixtureEpoch
    , lssiSignerAddress = fixtureSigner
    , lssiNonce = nonce
    , lssiTargetAddress = fixtureTarget
    , lssiValue = 5
    , lssiCalldata = fixtureCalldata
    , lssiGasLimit = 100000
    , lssiMaxPriorityFeePerGas = priorityFee
    , lssiMaxFeePerGas = maxFee
    , lssiSignedRawTransaction = fixtureRawA
    , lssiSignedTransactionHash = txHash
    }

fixtureReceipt :: Integer -> Text -> Maybe LpSettlementEventOutcome -> LpSettlementReceiptInput
fixtureReceipt attemptId txHash eventOutcome =
  LpSettlementReceiptInput
    { lsriAttemptId = attemptId
    , lsriTransactionHash = txHash
    , lsriBlockNumber = 120
    , lsriBlockHash = fixtureReceiptBlockHash
    , lsriSucceeded = True
    , lsriConfirmationDepth = 6
    , lsriEventOutcome = eventOutcome
    }

fixtureEvent :: LpSettlementEventOutcome
fixtureEvent =
  LpSettlementEventOutcome
    { lseoLogIndex = 3
    , lseoCutoffEpoch = fixtureEpoch
    , lseoSeniorRedeemAssets = 11
    , lseoJuniorRedeemAssets = 12
    , lseoJuniorDepositAssets = 13
    , lseoSeniorDepositAssets = 14
    , lseoSeniorBacklog = True
    , lseoJuniorBacklog = False
    , lseoEntriesDeferred = True
    }

requireTransaction :: Connection -> Integer -> IO LpSettlementTransactionRow
requireTransaction conn attemptId =
  getLpSettlementTransactionById conn attemptId >>= \case
    Just row -> pure row
    Nothing -> expectationFailure "expected LP settlement transaction row" >> fail "missing row"

shouldFail :: IO a -> Expectation
shouldFail action = do
  result <- try (void action) :: IO (Either SomeException ())
  result `shouldSatisfy` isLeft

shouldFailContaining :: Text -> IO a -> Expectation
shouldFailContaining expected action = do
  result <- try (void action) :: IO (Either SomeException ())
  case result of
    Left exception ->
      T.pack (show exception) `shouldSatisfy` T.isInfixOf expected
    Right () -> expectationFailure $ "expected failure containing: " <> T.unpack expected

runConcurrently
  :: IO ()
  -> IO ()
  -> IO (Either SomeException (), Either SomeException ())
runConcurrently first second = do
  firstReady <- newEmptyMVar
  secondReady <- newEmptyMVar
  firstGate <- newEmptyMVar
  secondGate <- newEmptyMVar
  firstResult <- newEmptyMVar
  secondResult <- newEmptyMVar
  void $ forkIO $ do
    putMVar firstReady ()
    takeMVar firstGate
    try first >>= putMVar firstResult
  void $ forkIO $ do
    putMVar secondReady ()
    takeMVar secondGate
    try second >>= putMVar secondResult
  takeMVar firstReady
  takeMVar secondReady
  putMVar firstGate ()
  putMVar secondGate ()
  (,) <$> takeMVar firstResult <*> takeMVar secondResult

shouldReturnBool :: IO Bool -> Bool -> Expectation
shouldReturnBool action expected = action >>= (`shouldBe` expected)

shouldReturnNothing :: IO (Maybe a) -> Expectation
shouldReturnNothing action = action >>= \case
  Nothing -> pure ()
  Just _ -> expectationFailure "expected no active LP settlement transaction"

shouldReturnRight :: (Eq a, Show a) => IO (Either Text a) -> a -> Expectation
shouldReturnRight action expected = action >>= (`shouldBe` Right expected)

hexText :: Int -> Text -> Text
hexText count digit = "0x" <> T.replicate count digit

fixtureChainId :: Integer
fixtureChainId = 31337

fixtureEpoch :: Integer
fixtureEpoch = 42

fixtureMonitor :: Text
fixtureMonitor = hexText 40 "1"

fixtureMonitorB :: Text
fixtureMonitorB = hexText 40 "8"

fixtureDigest :: Text
fixtureDigest = hexText 64 "2"

fixtureDigestB :: Text
fixtureDigestB = hexText 64 "9"

fixtureSigner :: Text
fixtureSigner = hexText 40 "3"

fixtureSignerB :: Text
fixtureSignerB = hexText 40 "a"

fixtureTarget :: Text
fixtureTarget = hexText 40 "4"

fixtureTxHashA, fixtureTxHashB, fixtureTxHashC :: Text
fixtureTxHashA = hexText 64 "5"
fixtureTxHashB = hexText 64 "6"
fixtureTxHashC = hexText 64 "7"

fixtureReceiptBlockHash :: Text
fixtureReceiptBlockHash = hexText 64 "b"

fixtureCalldata, fixtureRawA, fixtureRawB, fixtureRawC :: BS.ByteString
fixtureCalldata = BS.pack [1, 2, 3, 4]
fixtureRawA = BS.pack [1, 2]
fixtureRawB = BS.pack [3, 4]
fixtureRawC = BS.pack [5, 6]
