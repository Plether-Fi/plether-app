{-# LANGUAGE LambdaCase #-}

module Plether.Perps.HistoryIndexerSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Aeson (Value (..), object, (.=))
import Data.List (find, isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Plether.Indexer.Contracts (keccak256Text)
import Plether.Perps.HistoryIndexer
  ( BlockInfo (..)
  , PerpsAddresses (..)
  , ParsedPerpsLog (..)
  , ProtocolLogClassification (..)
  , RpcLog (..)
  , TransactionInfo (..)
  , bindTransactionInfoToLog
  , accountSnapshotTargets
  , canAdvanceCompletenessCursor
  , checkpointBlockNumbers
  , classifyProtocolLog
  , classifyProtocolLogForAddresses
  , cursorBlockMatchesCanonical
  , defaultPerpsAddresses
  , findNewestCommonCheckpoint
  , orderFailReasonName
  , parsePerpsLog
  , parsePerpsLogForAddresses
  , snapshotCallData
  , snapshotContractAddress
  , snapshotEthCallParams
  , snapshotReadFromRpcResult
  , terminalStatus
  , transactionInfoFromRpcResults
  )
import Plether.Protocol.Governance
  ( GovernanceCategory (..)
  , GovernanceCategoryDefinition (..)
  , GovernanceContractRole (..)
  , GovernanceEventDefinition (..)
  , GovernanceLifecycle (..)
  , GovernancePayloadEncoding (..)
  , GovernanceRoleEventDefinition (..)
  , governanceCategoryDefinitions
  , governanceRoleEvents
  )
import Plether.Protocol.ParameterChanges
  ( ParameterProjection (..)
  , parameterProjectionsForAction
  )
import Plether.Protocol.Snapshots
  ( MarketSide (..)
  , SnapshotAvailability (..)
  , SnapshotBuildContext (..)
  , SnapshotCallPlan (..)
  , SnapshotDocument (..)
  , SnapshotPlan (..)
  , SnapshotRead (..)
  , SnapshotSourceBlock (..)
  , SnapshotUnavailable (..)
  , accountLedgerSnapshotPlan
  , buildSnapshot
  , globalSnapshotPlans
  , sideSnapshotPlan
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "canAdvanceCompletenessCursor" $ do
    it "does not certify a bounded backfill that skips deployment history" $
      canAdvanceCompletenessCursor 100 0 125 150 `shouldBe` False

    it "advances deployment and cursor-adjacent ranges only" $ do
      canAdvanceCompletenessCursor 100 0 100 124 `shouldBe` True
      canAdvanceCompletenessCursor 100 124 125 149 `shouldBe` True
      canAdvanceCompletenessCursor 100 149 125 149 `shouldBe` False

    it "rejects an inverted range" $
      canAdvanceCompletenessCursor 100 124 125 124 `shouldBe` False

  describe "checkpointBlockNumbers" $ do
    it "retains empty-range boundaries and every observed log block" $ do
      checkpointBlockNumbers 100 120 [] `shouldBe` [100, 120]
      checkpointBlockNumbers 100 120 [115, 101, 115, 120]
        `shouldBe` [100, 101, 115, 120]

  describe "findNewestCommonCheckpoint" $ do
    it "walks past every orphaned checkpoint in a multi-block reorg" $ do
      let stored =
            [ (120, "0xold120")
            , (115, "0xold115")
            , (110, "0xcommon110")
            , (100, "0xcommon100")
            ]
          canonical =
            [ (120, "0xnew120")
            , (115, "0xnew115")
            , (110, "0xCOMMON110")
            , (100, "0xcommon100")
            ]
          resolve blockNumber =
            pure $
              maybe
                (Left ("checkpoint unavailable" :: Text))
                Right
                (lookup blockNumber canonical)
      ancestor <- findNewestCommonCheckpoint resolve stored
      ancestor `shouldBe` Just (110, "0xcommon110")

    it "falls back when no stored checkpoint is still canonical" $ do
      ancestor <-
        findNewestCommonCheckpoint
          (\_ -> pure $ Right ("0xdifferent" :: Text))
          [(10, "0xold"), (5, "0xolder")]
      ancestor `shouldBe` Nothing

  describe "cursorBlockMatchesCanonical" $ do
    it "fails closed when the provider cannot prove the cursor block identity" $
      cursorBlockMatchesCanonical
        canonicalBlockHash
        (Left "https://private-rpc.invalid cursor block unavailable" :: Either Text BlockInfo)
        `shouldBe` Left "cursor_block_unavailable"

    it "distinguishes a canonical match from a reorg mismatch" $ do
      cursorBlockMatchesCanonical canonicalBlockHash (Right canonicalBlockInfo)
        `shouldBe` Right True
      cursorBlockMatchesCanonical otherCanonicalBlockHash (Right canonicalBlockInfo)
        `shouldBe` Right False

  describe "parsePerpsLog" $ do
    it "parses OrderCommitted" $ do
      parsePerpsLog (mkLog orderCommittedTopic [word 42, addressTopic] (word 1))
        `shouldSatisfy` \case
          Just (ParsedOrderCommitted 42 account 1 _) -> account == testAccount
          _ -> False

    it "parses OrderExecuted" $ do
      parsePerpsLog (mkLog orderExecutedTopic [word 42] (word 101250000))
        `shouldBeParsedAs` \case
          ParsedOrderExecuted 42 101250000 _ -> True
          _ -> False

    it "parses OrderFailed and classifies expired cleanup state" $ do
      parsePerpsLog (mkLog orderFailedTopic [word 42] (word 0))
        `shouldBeParsedAs` \case
          ParsedOrderFailed 42 0 "Expired" _ -> True
          _ -> False
      terminalStatus "Expired" `shouldBe` "Expired / Cleaned up"
      terminalStatus "EngineRevert" `shouldBe` "Failed"

    it "parses position lifecycle activity" $ do
      parsePerpsLog (mkLog positionOpenedTopic [addressTopic] (words32 [0, 1_000, 101_000_000, 200_000_000]))
        `shouldBeParsedAs` \case
          ParsedPositionActivity "Open" account 0 (Just 101_000_000) (Just 1_000) (Just 200_000_000) Nothing _ ->
            account == testAccount
          _ -> False

      parsePerpsLog (mkLog positionClosedTopic [addressTopic] (words32 [1, 500, 99_000_000] <> signedWord (-75_000_000)))
        `shouldBeParsedAs` \case
          ParsedPositionActivity "Close" account 1 (Just 99_000_000) (Just 500) Nothing (Just (-75_000_000)) _ ->
            account == testAccount
          _ -> False

      parsePerpsLog (mkLog positionLiquidatedTopic [addressTopic] (words32 [1, 500, 99_000_000, 200_000]))
        `shouldBeParsedAs` \case
          ParsedPositionActivity "Liquidated" account 1 (Just 99_000_000) (Just 500) (Just 200_000) Nothing _ ->
            account == testAccount
          _ -> False

    it "parses margin account activity" $ do
      parsePerpsLog (mkLog marginAddedTopic [addressTopic] (word 5_000_000))
        `shouldBeParsedAs` \case
          ParsedMarginActivity "Add margin" account 5_000_000 _ -> account == testAccount
          _ -> False

      parsePerpsLog ((mkLog depositTopic [addressTopic, otherAddressTopic] (word 100_000_000)) {rlAddress = marginClearinghouse})
        `shouldBeParsedAs` \case
          ParsedMarginActivity "Deposit" account 100_000_000 payload ->
            account == testAccount
              && payload
                == object
                  [ "account" .= testAccount
                  , "asset" .= testAsset
                  , "contractAddress" .= testEmitter
                  , "amountUsdc" .= ("100000000" :: Text)
                  ]
          _ -> False

      parsePerpsLog ((mkLog withdrawTopic [addressTopic, otherAddressTopic] (word 25_000_000)) {rlAddress = marginClearinghouse})
        `shouldBeParsedAs` \case
          ParsedMarginActivity "Withdraw" account 25_000_000 _ -> account == testAccount
          _ -> False

    it "rejects truncated and oversized data across every core event family" $ do
      let validLogs =
            [ mkLog orderCommittedTopic [word 42, addressTopic] (word 1)
            , mkLog orderExecutedTopic [word 42] (word 101_250_000)
            , mkLog orderFailedTopic [word 42] (word 3)
            , mkLog positionOpenedTopic [addressTopic] (words32 [0, 1_000, 101_000_000, 200_000_000])
            , mkLog positionClosedTopic [addressTopic] (words32 [1, 500, 99_000_000, 75_000_000])
            , mkLog positionLiquidatedTopic [addressTopic] (words32 [1, 500, 99_000_000, 200_000])
            , mkLog marginAddedTopic [addressTopic] (word 5_000_000)
            , mkLog depositTopic [addressTopic, otherAddressTopic] (word 100_000_000)
            , mkLog withdrawTopic [addressTopic, otherAddressTopic] (word 25_000_000)
            ]

      mapM_
        ( \logEntry ->
            shouldRemainUnavailable
              "event_data_length_invalid"
              logEntry {rlData = BS.init $ rlData logEntry}
        )
        validLogs
      mapM_
        ( \logEntry ->
            shouldRemainUnavailable
              "event_data_length_invalid"
              logEntry {rlData = rlData logEntry <> BS.singleton 0}
        )
        validLogs

    it "rejects non-canonical indexed addresses and topic shapes across core event families" $ do
      let addressLogs =
            [ mkLog orderCommittedTopic [word 42, nonCanonicalAddressTopic] (word 1)
            , mkLog positionOpenedTopic [nonCanonicalAddressTopic] (words32 [0, 1_000, 101_000_000, 200_000_000])
            , mkLog marginAddedTopic [nonCanonicalAddressTopic] (word 5_000_000)
            , mkLog depositTopic [nonCanonicalAddressTopic, otherAddressTopic] (word 100_000_000)
            , mkLog withdrawTopic [addressTopic, nonCanonicalAddressTopic] (word 25_000_000)
            ]
          extraTopicLogs =
            [ (mkLog orderExecutedTopic [word 42] (word 101_250_000))
                {rlTopics = [orderExecutedTopic, word 42, word 0]}
            , (mkLog positionLiquidatedTopic [addressTopic] (words32 [1, 500, 99_000_000, 200_000]))
                {rlTopics = [positionLiquidatedTopic, addressTopic, word 0]}
            ]

      mapM_
        (shouldRemainUnavailable "event_indexed_address_not_canonical")
        addressLogs
      mapM_
        (shouldRemainUnavailable "event_topic_count_invalid")
        extraTopicLogs
      shouldRemainUnavailable
        "event_topic_word_length_invalid"
        (mkLog orderCommittedTopic [word 42, BS.take 31 addressTopic] (word 1))

    it "rejects dirty high bits in indexed uint64 and data uint8 fields" $ do
      let fixtures =
            [ ( "event_uint64_not_canonical"
              , mkLog orderExecutedTopic [word $ 2 ^ (64 :: Int)] (word 101_250_000)
              )
            , ( "event_uint8_not_canonical"
              , mkLog orderCommittedTopic [word 42, addressTopic] (word 256)
              )
            , ( "event_uint8_not_canonical"
              , mkLog orderFailedTopic [word 42] (word 256)
              )
            , ( "event_uint8_not_canonical"
              , mkLog positionClosedTopic [addressTopic] (words32 [256, 500, 99_000_000, 75_000_000])
              )
            ]

      mapM_ (uncurry shouldRemainUnavailable) fixtures

    it "binds core event topics to their configured release contracts" $ do
      let wrongSourceLogs =
            [ (mkLog orderCommittedTopic [word 42, addressTopic] (word 1))
                {rlAddress = paCfdEngine defaultPerpsAddresses}
            , (mkLog positionOpenedTopic [addressTopic] (words32 [0, 1_000, 101_000_000, 200_000_000]))
                {rlAddress = paOrderRouter defaultPerpsAddresses}
            , (mkLog marginAddedTopic [addressTopic] (word 5_000_000))
                {rlAddress = paHousePool defaultPerpsAddresses}
            , (mkLog depositTopic [addressTopic, otherAddressTopic] (word 100_000_000))
                {rlAddress = paSeniorVault defaultPerpsAddresses}
            ]

      mapM_
        (shouldRemainUnavailable "event_contract_address_mismatch")
        wrongSourceLogs

    it "uses the supplied release manifest instead of default addresses" $ do
      let customAddresses =
            defaultPerpsAddresses
              { paOrderRouter = testAsset
              , paSeniorVault = testAccount
              }
          orderLog =
            (mkLog orderCommittedTopic [word 42, addressTopic] (word 1))
              {rlAddress = testAsset}
          depositLog =
            (mkLog erc4626DepositTopic [addressTopic, otherAddressTopic] (words32 [100, 95]))
              {rlAddress = testAccount}

      parsePerpsLog orderLog `shouldBe` Nothing
      parsePerpsLogForAddresses customAddresses orderLog
        `shouldSatisfy` \case
          Just ParsedOrderCommitted {} -> True
          _ -> False
      classifyProtocolLogForAddresses customAddresses depositLog
        `shouldSatisfy` \case
          Just ProtocolLogClassification
            { plcActionType = "tranche_deposit"
            , plcDecoded = True
            } -> True
          _ -> False

  describe "orderFailReasonName" $ do
    it "matches deployed OrderFailReason ordinals" $ do
      map orderFailReasonName [0 .. 5]
        `shouldBe` ["Expired", "CloseOnly", "SlippageExceeded", "EnginePanic", "AccountLiquidated", "EngineRevert"]

  describe "transactionInfoFromRpcResults" $ do
    it "retains exact metadata only after transaction, receipt, log, and block identities bind" $ do
      let rawInfo =
            transactionInfoFromRpcResults
              (Right canonicalTransactionResponse)
              (Right $ canonicalReceiptResponse canonicalReceiptLogResponse)
          info =
            either
              (error . T.unpack)
              id
              ( bindTransactionInfoToLog
                  canonicalBlockInfo
                  canonicalRpcLog
                  rawInfo
              )
      tiStatus info `shouldBe` "success"
      tiInput info `shouldBe` Just "0x12345678abcdef"
      tiSelector info `shouldBe` Just "0x12345678"
      tiNativeValue info `shouldBe` Just 42
      tiGasUsed info `shouldBe` Just 21_000
      tiEffectiveGasPrice info `shouldBe` Just 1_000_000_000
      show (tiEvidence rawInfo) `shouldContain` "unbound"
      show (tiEvidence rawInfo) `shouldNotContain` "\"exact\""
      show (tiEvidence info) `shouldContain` "\"exact\""
      show (tiEvidence info) `shouldContain` "receiptLogMatched"
      show (tiEvidence info) `shouldContain` T.unpack canonicalTxHash
      show (tiEvidence info) `shouldContain` T.unpack canonicalBlockHash

    it "does not invent input or a successful status when RPC metadata is missing" $ do
      let info =
            transactionInfoFromRpcResults
              (Left "provider detail must not be exposed")
              (Right Null)
      tiInput info `shouldBe` Nothing
      tiSelector info `shouldBe` Nothing
      tiStatus info `shouldBe` "unavailable"
      show (tiEvidence info) `shouldNotContain` "provider detail"
      show (tiEvidence info) `shouldContain` "transaction_rpc_unavailable"
      show (tiEvidence info) `shouldContain` "receipt_not_returned"
      show (tiEvidence info) `shouldContain` "transaction_input_unavailable"
      show (tiEvidence info) `shouldContain` "receipt_status_unavailable"
      bindTransactionInfoToLog canonicalBlockInfo canonicalRpcLog info
        `shouldBe` Left "transaction_identity_unavailable"

    it "marks malformed RPC quantities unavailable instead of coercing them to zero" $ do
      let info =
            transactionInfoFromRpcResults
              ( Right $
                  object
                    [ "from" .= testAccount
                    , "to" .= testAsset
                    , "input" .= ("0x" :: Text)
                    , "value" .= ("not-hex" :: Text)
                    ]
              )
              ( Right $
                  object
                    [ "status" .= ("0x1" :: Text)
                    , "gasUsed" .= ("invalid" :: Text)
                    , "effectiveGasPrice" .= ("0x3b9aca00" :: Text)
                    ]
              )
      tiStatus info `shouldBe` "success"
      tiNativeValue info `shouldBe` Nothing
      tiGasUsed info `shouldBe` Nothing
      tiEffectiveGasPrice info `shouldBe` Just 1_000_000_000
      show (tiEvidence info) `shouldContain` "transaction_native_value_unavailable"
      show (tiEvidence info) `shouldContain` "receipt_gas_used_unavailable"

    it "rejects transaction identity responses from another fork before insertion" $ do
      let cases =
            [ ( canonicalTransactionResponseWith
                  otherCanonicalTxHash
                  "0x7b"
                  canonicalBlockHash
                  "0x1"
              , "transaction_hash_mismatch"
              )
            , ( canonicalTransactionResponseWith
                  canonicalTxHash
                  "0x7c"
                  canonicalBlockHash
                  "0x1"
              , "transaction_block_number_mismatch"
              )
            , ( canonicalTransactionResponseWith
                  canonicalTxHash
                  "0x7b"
                  otherCanonicalBlockHash
                  "0x1"
              , "transaction_block_hash_mismatch"
              )
            , ( canonicalTransactionResponseWith
                  canonicalTxHash
                  "0x7b"
                  canonicalBlockHash
                  "0x2"
              , "transaction_transaction_index_mismatch"
              )
            ]
      forM_ cases $ \(txResponse, expectedReason) -> do
        let info =
              transactionInfoFromRpcResults
                (Right txResponse)
                (Right $ canonicalReceiptResponse canonicalReceiptLogResponse)
        bindTransactionInfoToLog canonicalBlockInfo canonicalRpcLog info
          `shouldBe` Left expectedReason

    it "rejects receipt identity responses from another fork before insertion" $ do
      let cases =
            [ canonicalReceiptResponseWith
                otherCanonicalTxHash
                "0x7b"
                canonicalBlockHash
                "0x1"
                canonicalReceiptLogResponse
            , canonicalReceiptResponseWith
                canonicalTxHash
                "0x7c"
                canonicalBlockHash
                "0x1"
                canonicalReceiptLogResponse
            , canonicalReceiptResponseWith
                canonicalTxHash
                "0x7b"
                otherCanonicalBlockHash
                "0x1"
                canonicalReceiptLogResponse
            , canonicalReceiptResponseWith
                canonicalTxHash
                "0x7b"
                canonicalBlockHash
                "0x2"
                canonicalReceiptLogResponse
            ]
          expectedReasons =
            [ "receipt_hash_mismatch"
            , "receipt_block_number_mismatch"
            , "receipt_block_hash_mismatch"
            , "receipt_transaction_index_mismatch"
            ]
      forM_ (zip cases expectedReasons) $ \(receiptResponse, expectedReason) -> do
        let info =
              transactionInfoFromRpcResults
                (Right canonicalTransactionResponse)
                (Right receiptResponse)
        bindTransactionInfoToLog canonicalBlockInfo canonicalRpcLog info
          `shouldBe` Left expectedReason

    it "rejects non-canonical identity quantities instead of accepting equivalent values" $ do
      let malformedTx =
            canonicalTransactionResponseWith
              canonicalTxHash
              "0x07b"
              canonicalBlockHash
              "0x1"
          info =
            transactionInfoFromRpcResults
              (Right malformedTx)
              (Right $ canonicalReceiptResponse canonicalReceiptLogResponse)
      tiTransactionIdentity info `shouldBe` Nothing
      show (tiEvidence info) `shouldContain` "transaction_block_number_invalid"
      show (tiEvidence info) `shouldNotContain` "\"exact\""
      bindTransactionInfoToLog canonicalBlockInfo canonicalRpcLog info
        `shouldBe` Left "transaction_identity_unavailable"

    it "requires the receipt to round-trip the exact log identity and payload" $ do
      let mismatchedLog =
            canonicalReceiptLogResponseWith
              canonicalTxHash
              "0x7b"
              canonicalBlockHash
              "0x1"
              "0x2"
              testAsset
              ["0x" <> T.replicate 64 "f"]
              "0x"
          info =
            transactionInfoFromRpcResults
              (Right canonicalTransactionResponse)
              (Right $ canonicalReceiptResponse mismatchedLog)
      bindTransactionInfoToLog canonicalBlockInfo canonicalRpcLog info
        `shouldBe` Left "receipt_log_identity_mismatch"

    it "rejects a log and resolved block anchor that disagree" $ do
      let wrongBlock = canonicalBlockInfo {biHash = otherCanonicalBlockHash}
          info =
            transactionInfoFromRpcResults
              (Right canonicalTransactionResponse)
              (Right $ canonicalReceiptResponse canonicalReceiptLogResponse)
      bindTransactionInfoToLog wrongBlock canonicalRpcLog info
        `shouldBe` Left "transaction_evidence_block_hash_mismatch"

  describe "global snapshot indexing fixtures" $ do
    it "resolves every current global plan against the release address set" $ do
      let resolvedAddresses =
            [ snapshotContractAddress defaultPerpsAddresses (scpContract callPlan)
            | plan <- globalSnapshotPlans
            , callPlan <- spCalls plan
            ]

      resolvedAddresses `shouldSatisfy` all (/= Nothing)
      snapshotContractAddress defaultPerpsAddresses (scpContract $ onlySnapshotCall $ sideSnapshotPlan LongSide)
        `shouldBe` Just (paCfdEngine defaultPerpsAddresses)

    it "encodes exact selectors and static ABI arguments" $ do
      let callPlan = onlySnapshotCall $ sideSnapshotPlan ShortSide
          expectedCallData =
            BS.take 4 (keccak256Text "sides(uint256)")
              <> word 1

      snapshotCallData callPlan `shouldBe` Right expectedCallData

    it "pins eth_call to the supplied canonical block hash with EIP-1898" $ do
      let callPlan = onlySnapshotCall $ sideSnapshotPlan ShortSide
          callData =
            BS.take 4 (keccak256Text "sides(uint256)")
              <> word 1
          contractAddress = paCfdEngine defaultPerpsAddresses

      snapshotEthCallParams contractAddress callData canonicalBlockInfo
        `shouldBe`
          [ object
              [ "to" .= contractAddress
              , "data" .= ("0xb3eb738a" <> T.replicate 63 "0" <> "1")
              ]
          , object
              [ "blockHash" .= canonicalBlockHash
              , "requireCanonical" .= True
              ]
          ]

      snapshotCallData callPlan `shouldBe` Right callData

    it "redacts canonical-call rejection as archive-state unavailability" $ do
      let callPlan = onlySnapshotCall $ sideSnapshotPlan LongSide
          snapshotRead =
            snapshotReadFromRpcResult
              callPlan
              (Left "https://private-rpc.invalid:8545 canonical block rejected")

      snapshotRead
        `shouldBe` SnapshotRead
          { srCallId = "market.long.totals"
          , srResult =
              Left
                SnapshotUnavailable
                  { suReason = "archive_state_unavailable"
                  , suDetail = Nothing
                  }
          }
      show snapshotRead `shouldNotContain` "private-rpc"

    it "accepts canonical RPC hex and rejects malformed responses without coercion" $ do
      let callPlan = onlySnapshotCall $ sideSnapshotPlan LongSide
          validBytes = BS.replicate 32 0

      snapshotReadFromRpcResult
        callPlan
        (Right $ String $ "0x" <> T.replicate 64 "0")
        `shouldBe` SnapshotRead
          { srCallId = "market.long.totals"
          , srResult = Right validBytes
          }
      srResult (snapshotReadFromRpcResult callPlan $ Right $ String "not-hex")
        `shouldBe` Left
          SnapshotUnavailable
            { suReason = "archive_state_unavailable"
            , suDetail = Nothing
            }

  describe "account snapshot indexing fixtures" $ do
    it "deduplicates trading-ledger targets by normalized account and block" $ do
      let openLog =
            mkLog
              positionOpenedTopic
              [addressTopic]
              (words32 [0, 1_000, 101_000_000, 200_000_000])
          closeLog =
            (mkLog
              positionClosedTopic
              [addressTopic]
              (words32 [0, 500, 102_000_000, 75_000_000]))
              {rlLogIndex = 3}
          commitmentLog =
            (mkLog orderCommittedTopic [word 42, addressTopic] (word 0))
              {rlLogIndex = 4}
          otherAccountLog =
            (mkLog
              marginAddedTopic
              [otherAddressTopic]
              (word 5_000_000))
              { rlBlockNumber = 124
              , rlLogIndex = 5
              }

      accountSnapshotTargets
        defaultPerpsAddresses
        [openLog, closeLog, commitmentLog, otherAccountLog, openLog]
        `shouldBe`
          [ (123, testAccount)
          , (124, testAsset)
          ]

    it "does not schedule AccountLens reads for LP, governance, malformed, or wrong-source logs" $ do
      let lpDeposit =
            mkLog
              erc4626DepositTopic
              [addressTopic, otherAddressTopic]
              (words32 [100_000_000, 95_000_000])
          pauseLog = mkLog pausedTopic [addressTopic] ""
          malformedPosition =
            mkLog positionOpenedTopic [addressTopic] (word 0)
          wrongSourcePosition =
            (mkLog
              positionOpenedTopic
              [addressTopic]
              (words32 [0, 1_000, 101_000_000, 200_000_000]))
              {rlAddress = paOrderRouter defaultPerpsAddresses}

      accountSnapshotTargets
        defaultPerpsAddresses
        [lpDeposit, pauseLog, malformedPosition, wrongSourcePosition]
        `shouldBe` []

    it "uses the account-scoped plan at the canonical block hash" $ do
      let plan = accountLedgerSnapshotPlan testAccount
          callPlan = onlySnapshotCall plan
          expectedCallData =
            BS.take 4 (keccak256Text "getAccountLedgerSnapshot(address)")
              <> addressTopic

      spScope plan
        `shouldBe`
          "account.0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b.ledger"
      snapshotCallData callPlan `shouldBe` Right expectedCallData
      snapshotEthCallParams
        (paAccountLens defaultPerpsAddresses)
        expectedCallData
        canonicalBlockInfo
        `shouldBe`
          [ object
              [ "to" .= paAccountLens defaultPerpsAddresses
              , "data" .= ("0x" <> bytesToText expectedCallData)
              ]
          , object
              [ "blockHash" .= canonicalBlockHash
              , "requireCanonical" .= True
              ]
          ]

    it "persists an explicit unavailable account document after an archive-read failure" $ do
      let plan = accountLedgerSnapshotPlan testAccount
          callPlan = onlySnapshotCall plan
          snapshotRead =
            snapshotReadFromRpcResult
              callPlan
              (Left "https://private-rpc.invalid archive block missing")
          document =
            buildSnapshot
              SnapshotBuildContext
                { sbcReleaseId = "arbitrum-sepolia-current"
                , sbcCalculationVersion = "protocol-transparency-v1"
                , sbcSourceBlock =
                    SnapshotSourceBlock
                      { ssbNumber = biNumber canonicalBlockInfo
                      , ssbHash = Just $ biHash canonicalBlockInfo
                      , ssbTimestamp = Just $ biTimestamp canonicalBlockInfo
                      }
                }
              plan
              [snapshotRead]

      sdValues document `shouldBe` []
      map saReason (sdAvailability document)
        `shouldBe` ["archive_state_unavailable"]
      show document `shouldNotContain` "private-rpc"

  describe "classifyProtocolLog" $ do
    it "decodes exact ERC-4626 deposit and withdrawal accounting" $ do
      classifyProtocolLog (mkLog erc4626DepositTopic [addressTopic, otherAddressTopic] (words32 [100_000_000, 95_000_000]))
        `shouldSatisfy` \case
          Just ProtocolLogClassification
            { plcEventName = "Deposit"
            , plcActionType = "tranche_deposit"
            , plcAccount = Just owner
            , plcPayload = payload
            } ->
              owner == testAsset
                && payload
                  == object
                    [ "sender" .= testAccount
                    , "owner" .= testAsset
                    , "assets" .= ("100000000" :: Text)
                    , "shares" .= ("95000000" :: Text)
                    , "assetsUnit" .= ("USDC:6" :: Text)
                    , "sharesUnit" .= ("shares:18" :: Text)
                    ]
          _ -> False

      classifyProtocolLog (mkLog erc4626WithdrawTopic [addressTopic, otherAddressTopic, addressTopic] (words32 [25_000_000, 24_000_000]))
        `shouldSatisfy` \case
          Just ProtocolLogClassification
            { plcEventName = "Withdraw"
            , plcActionType = "tranche_withdraw"
            , plcAccount = Just owner
            , plcPayload = payload
            } ->
              owner == testAccount
                && payload
                  == object
                    [ "sender" .= testAccount
                    , "receiver" .= testAsset
                    , "owner" .= testAccount
                    , "assets" .= ("25000000" :: Text)
                    , "shares" .= ("24000000" :: Text)
                    , "assetsUnit" .= ("USDC:6" :: Text)
                    , "sharesUnit" .= ("shares:18" :: Text)
                    ]
          _ -> False

    it "keeps malformed ERC-4626 logs raw and unavailable instead of decoding partial accounting" $ do
      let deposit =
            mkLog
              erc4626DepositTopic
              [addressTopic, otherAddressTopic]
              (words32 [100_000_000, 95_000_000])
          withdrawal =
            mkLog
              erc4626WithdrawTopic
              [addressTopic, otherAddressTopic, addressTopic]
              (words32 [25_000_000, 24_000_000])
          malformed =
            [ ("event_data_length_invalid", deposit {rlData = BS.take 63 $ rlData deposit})
            , ("event_data_length_invalid", deposit {rlData = rlData deposit <> BS.singleton 0})
            , ("event_data_length_invalid", withdrawal {rlData = BS.take 63 $ rlData withdrawal})
            , ("event_data_length_invalid", withdrawal {rlData = rlData withdrawal <> BS.singleton 0})
            , ("event_topic_count_invalid", deposit {rlTopics = rlTopics deposit <> [word 0]})
            , ("event_topic_count_invalid", withdrawal {rlTopics = init $ rlTopics withdrawal})
            , ( "event_indexed_address_not_canonical"
              , deposit {rlTopics = [erc4626DepositTopic, nonCanonicalAddressTopic, otherAddressTopic]}
              )
            , ( "event_indexed_address_not_canonical"
              , withdrawal
                  { rlTopics =
                      [ erc4626WithdrawTopic
                      , addressTopic
                      , otherAddressTopic
                      , nonCanonicalAddressTopic
                      ]
                  }
              )
            ]

      mapM_
        ( \(reason, logEntry) ->
            shouldClassifyUnavailable reason logEntry
        )
        malformed

    it "binds ERC-4626 events to configured tranche vaults" $ do
      let validDeposit =
            mkLog
              erc4626DepositTopic
              [addressTopic, otherAddressTopic]
              (words32 [100_000_000, 95_000_000])
          validWithdrawal =
            mkLog
              erc4626WithdrawTopic
              [addressTopic, otherAddressTopic, addressTopic]
              (words32 [25_000_000, 24_000_000])

      shouldClassifyUnavailable
        "event_contract_address_mismatch"
        validDeposit {rlAddress = paHousePool defaultPerpsAddresses}
      shouldClassifyUnavailable
        "event_contract_address_mismatch"
        validWithdrawal {rlAddress = paMarginClearinghouse defaultPerpsAddresses}

    it "decodes common ownership and pause lifecycle events" $ do
      classifyProtocolLog (mkLog ownershipTransferredTopic [addressTopic, otherAddressTopic] "")
        `shouldSatisfy` \case
          Just ProtocolLogClassification
            { plcEventName = "OwnershipTransferred"
            , plcActionType = "ownership_transfer"
            } -> True
          _ -> False

      classifyProtocolLog (mkLog pausedTopic [addressTopic] "")
        `shouldSatisfy` \case
          Just ProtocolLogClassification {plcActionType = "pause"} -> True
          _ -> False

      classifyProtocolLog (mkLog unpausedTopic [addressTopic] "")
        `shouldSatisfy` \case
          Just ProtocolLogClassification {plcActionType = "unpause"} -> True
          _ -> False

    it "keeps malformed ownership and pause logs unavailable and out of typed actions" $ do
      let ownership =
            mkLog ownershipTransferredTopic [addressTopic, otherAddressTopic] ""
          pauseIndexed =
            mkLog pausedTopic [addressTopic] ""
          pauseData =
            mkLog pausedTopic [] addressTopic
          malformed =
            [ ("governance_role_event_shape_invalid", ownership {rlData = word 0})
            , ("governance_role_event_shape_invalid", ownership {rlTopics = init $ rlTopics ownership})
            , ( "governance_address_not_canonical"
              , ownership
                  { rlTopics =
                      [ ownershipTransferredTopic
                      , nonCanonicalAddressTopic
                      , otherAddressTopic
                      ]
                  }
              )
            , ("governance_role_event_shape_invalid", pauseIndexed {rlData = word 0})
            , ("governance_role_event_shape_invalid", pauseData {rlData = BS.take 31 addressTopic})
            , ("governance_role_event_shape_invalid", pauseData {rlData = addressTopic <> BS.singleton 0})
            , ("governance_address_not_canonical", pauseData {rlData = nonCanonicalAddressTopic})
            ]

      mapM_
        ( \(reason, logEntry) ->
            shouldClassifyUnavailable reason logEntry
        )
        malformed

    it "strictly decodes every static governance lifecycle event" $ do
      let fixtures =
            [ (definition, eventDefinition, wordCount)
            | definition <- governanceCategoryDefinitions
            , eventDefinition <- gcdEvents definition
            , StaticGovernanceWords wordCount <- [gedPayloadEncoding eventDefinition]
            ]
          decoded =
            [ classifyProtocolLog
                (mkLog (gedTopic eventDefinition) [] (BS.replicate (wordCount * 32) 0))
            | (_, eventDefinition, wordCount) <- fixtures
            ]

      length fixtures `shouldBe` 15
      decoded
        `shouldSatisfy`
          all
            ( \case
                Just
                  ProtocolLogClassification
                    { plcActionType
                    , plcPayload
                    , plcDecoded = True
                    , plcAvailability = []
                    } ->
                      plcActionType
                        `elem`
                          [ "governance_proposal"
                          , "governance_execution"
                          , "governance_cancellation"
                          ]
                        && all
                          (`containsJsonText` plcPayload)
                          ["category", "lifecycle", "eventSignature", "fields"]
                _ -> False
            )

    it "feeds decoded governance actions into the parameter change projection" $ do
      let proposal = governanceEvent RouterConfigCategory GovernanceProposed
          logEntry =
            (mkLog (gedTopic proposal) [] (words32 [1 .. 16]))
              {rlAddress = paOrderRouterAdmin defaultPerpsAddresses}

      case classifyProtocolLog logEntry of
        Just classification -> do
          let projections =
                parameterProjectionsForAction
                  (plcActionType classification)
                  (rlAddress logEntry)
                  (plcPayload classification)
          length projections `shouldBe` 15
          map ppCategory projections
            `shouldSatisfy` all (== "router_config")
          map ppEta projections
            `shouldSatisfy` all (== Just 16)
          head projections
            `shouldSatisfy` \projection ->
              ppParameterKey projection == "orders.max_order_age"
                && ppNewValue projection == Just (String "1")
                && ppDisplayUnit projection == Just "seconds"
        Nothing ->
          expectationFailure "router config proposal was not classified"

    it "binds governance topics to the configured category and role hosts" $ do
      let routerProposal =
            governanceEvent RouterConfigCategory GovernanceProposed
          routerProposalWords =
            case gedPayloadEncoding routerProposal of
              StaticGovernanceWords wordCount -> wordCount
              UnsupportedDynamicGovernancePayload _ ->
                error "router proposal must be static"
          pauseTopic = governanceRoleTopic "governance.paused"

      shouldClassifyUnavailable
        "event_contract_address_mismatch"
        ( mkLog
            (gedTopic routerProposal)
            []
            (BS.replicate (routerProposalWords * 32) 0)
        )
          {rlAddress = paCfdEngineAdmin defaultPerpsAddresses}
      shouldClassifyUnavailable
        "event_contract_address_mismatch"
        (mkLog pauseTopic [] addressTopic)
          {rlAddress = paCfdEngineAdmin defaultPerpsAddresses}

    it "retains calendar proposal and finalization identities with explicit unavailable provenance" $ do
      let dynamicEvents =
            [ eventDefinition
            | definition <- governanceCategoryDefinitions
            , gcdCategory definition == EngineCalendarConfigCategory
            , eventDefinition <- gcdEvents definition
            , UnsupportedDynamicGovernancePayload _ <- [gedPayloadEncoding eventDefinition]
            ]
          classifications =
            [ classifyProtocolLog (mkLog (gedTopic eventDefinition) [] (word 32))
            | eventDefinition <- dynamicEvents
            ]

      length classifications `shouldBe` 2
      classifications
        `shouldSatisfy`
          all
            ( \case
                Just
                  ProtocolLogClassification
                    { plcActionType
                    , plcPayload
                    , plcDecoded = False
                    , plcAvailability =
                        [availability]
                    } ->
                      plcActionType `elem` ["governance_proposal", "governance_execution"]
                        && containsJsonText "engine_calendar_config" plcPayload
                        && containsJsonText "calendar_dynamic_tuple_not_supported" plcPayload
                        && containsJsonText "calendar_dynamic_tuple_not_supported" availability
                _ -> False
            )

    it "recognizes malformed static governance logs without claiming decoded fields" $ do
      let eventDefinition =
            governanceEvent RouterConfigCategory GovernanceProposed

      classifyProtocolLog (mkLog (gedTopic eventDefinition) [] (word 1))
        `shouldSatisfy` \case
          Just
            ProtocolLogClassification
              { plcEventName = "RouterConfigProposed"
              , plcActionType = "unclassified_event"
              , plcPayload
              , plcDecoded = False
              , plcAvailability = [availability]
              } ->
                containsJsonText "governance_payload_length_mismatch" plcPayload
                  && containsJsonText "governance_payload_length_mismatch" availability
          _ -> False

    it "classifies ownership proposals, pauser changes, and treasury dependency changes" $ do
      classifyProtocolLog
        (mkLog (governanceRoleTopic "governance.ownership_transfer_started") [addressTopic, otherAddressTopic] "")
        `shouldSatisfy` \case
          Just
            ProtocolLogClassification
              { plcEventName = "OwnershipTransferStarted"
              , plcActionType = "ownership_transfer_started"
              , plcAccount = Just newOwner
              , plcPayload
              , plcDecoded = True
              } ->
                newOwner == testAsset
                  && containsJsonText "order_router_admin" plcPayload
          _ -> False

      classifyProtocolLog
        (mkLog (governanceRoleTopic "governance.pauser_updated") [addressTopic, otherAddressTopic] "")
        `shouldSatisfy` \case
          Just
            ProtocolLogClassification
              { plcEventName = "PauserUpdated"
              , plcActionType = "pauser_update"
              , plcAccount = Just newPauser
              , plcPayload
              } ->
                newPauser == testAsset
                  && containsJsonText "previousPauser" plcPayload
                  && containsJsonText "newPauser" plcPayload
                  && containsJsonText "order_router_admin" plcPayload
          _ -> False

      classifyProtocolLog
        (mkLog (governanceRoleTopic "governance.protocol_treasury_updated") [otherAddressTopic] "")
        `shouldSatisfy` \case
          Just
            ProtocolLogClassification
              { plcEventName = "ProtocolTreasuryUpdated"
              , plcActionType = "protocol_treasury_update"
              , plcAccount = Just treasury
              , plcPayload
              } ->
                treasury == testAsset
                  && containsJsonText "protocolTreasury" plcPayload
                  && containsJsonText "cfd_engine" plcPayload
          _ -> False

    it "decodes the deployed non-indexed Pausable event while preserving indexed compatibility" $ do
      let pauseTopic = governanceRoleTopic "governance.paused"

      classifyProtocolLog (mkLog pauseTopic [] addressTopic)
        `shouldSatisfy` \case
          Just
            ProtocolLogClassification
              { plcActionType = "pause"
              , plcAccount = Just account
              , plcDecoded = True
              } ->
                account == testAccount
          _ -> False

      classifyProtocolLog (mkLog pauseTopic [addressTopic] "")
        `shouldSatisfy` \case
          Just
            ProtocolLogClassification
              { plcActionType = "pause"
              , plcAccount = Just account
              , plcDecoded = True
              } ->
                account == testAccount
          _ -> False

    it "does not claim exact decoding for malformed or unknown logs" $ do
      classifyProtocolLog (mkLog erc4626DepositTopic [addressTopic, otherAddressTopic] (word 1))
        `shouldSatisfy` \case
          Just ProtocolLogClassification
            { plcActionType = "unclassified_event"
            , plcDecoded = False
            } -> True
          _ -> False
      classifyProtocolLog (mkLog (keccak256Text "FutureEvent(bytes32)") [] (word 1))
        `shouldBe` Nothing

shouldBeParsedAs :: Maybe ParsedPerpsLog -> (ParsedPerpsLog -> Bool) -> Expectation
shouldBeParsedAs parsed predicate =
  case parsed of
    Just event | predicate event -> pure ()
    _ -> expectationFailure $ "unexpected parsed event: " <> show parsed

shouldRemainUnavailable :: Text -> RpcLog -> Expectation
shouldRemainUnavailable reason logEntry = do
  parsePerpsLog logEntry `shouldBe` Nothing
  shouldClassifyUnavailable reason logEntry

shouldClassifyUnavailable :: Text -> RpcLog -> Expectation
shouldClassifyUnavailable reason logEntry =
  classifyProtocolLog logEntry
    `shouldSatisfy` \case
      Just
        ProtocolLogClassification
          { plcActionType = "unclassified_event"
          , plcAccount = Nothing
          , plcPayload
          , plcDecoded = False
          , plcAvailability
          } ->
            containsJsonText "unavailable" plcPayload
              && containsJsonText reason plcPayload
              && not (null plcAvailability)
              && all (containsJsonText reason) plcAvailability
      _ -> False

governanceEvent :: GovernanceCategory -> GovernanceLifecycle -> GovernanceEventDefinition
governanceEvent category lifecycle =
  case
      [ eventDefinition
      | definition <- governanceCategoryDefinitions
      , gcdCategory definition == category
      , eventDefinition <- gcdEvents definition
      , gedLifecycle eventDefinition == lifecycle
      ] of
    eventDefinition : _ -> eventDefinition
    [] -> error "missing governance event fixture"

governanceRoleTopic :: Text -> ByteString
governanceRoleTopic key =
  case find ((== key) . gredKey) governanceRoleEvents of
    Just definition -> gredTopic definition
    Nothing -> error "missing governance role event fixture"

containsJsonText :: Text -> Value -> Bool
containsJsonText needle value =
  show needle `isInfixOf` show value

onlySnapshotCall :: SnapshotPlan -> SnapshotCallPlan
onlySnapshotCall plan =
  case spCalls plan of
    [callPlan] -> callPlan
    _ -> error "expected one snapshot call"

bytesToText :: ByteString -> Text
bytesToText = TE.decodeUtf8 . B16.encode

fixtureEmitterForTopic :: ByteString -> Text
fixtureEmitterForTopic topic
  | topic `elem` [orderCommittedTopic, orderExecutedTopic, orderFailedTopic] =
      paOrderRouter defaultPerpsAddresses
  | topic `elem` [positionOpenedTopic, positionClosedTopic, positionLiquidatedTopic] =
      paCfdEngine defaultPerpsAddresses
  | topic `elem` [marginAddedTopic, depositTopic, withdrawTopic] =
      paMarginClearinghouse defaultPerpsAddresses
  | topic `elem` [erc4626DepositTopic, erc4626WithdrawTopic] =
      paSeniorVault defaultPerpsAddresses
  | Just definition <-
      find
        (any ((== topic) . gedTopic) . gcdEvents)
        governanceCategoryDefinitions =
      governanceFixtureRoleAddress $ gcdContractRole definition
  | Just definition <- find ((== topic) . gredTopic) governanceRoleEvents =
      case gredContractRoles definition of
        role : _ -> governanceFixtureRoleAddress role
        [] -> paOrderRouter defaultPerpsAddresses
  | otherwise =
      paOrderRouter defaultPerpsAddresses

governanceFixtureRoleAddress :: GovernanceContractRole -> Text
governanceFixtureRoleAddress = \case
  OrderRouterAdminRole -> paOrderRouterAdmin defaultPerpsAddresses
  CfdEngineAdminRole -> paCfdEngineAdmin defaultPerpsAddresses
  HousePoolRole -> paHousePool defaultPerpsAddresses
  OrderRouterRole -> paOrderRouter defaultPerpsAddresses
  CfdEngineRole -> paCfdEngine defaultPerpsAddresses
  PletherOracleRole -> paPletherOracle defaultPerpsAddresses

canonicalTxHash :: Text
canonicalTxHash = "0x" <> T.replicate 64 "a"

otherCanonicalTxHash :: Text
otherCanonicalTxHash = "0x" <> T.replicate 64 "b"

canonicalBlockHash :: Text
canonicalBlockHash = "0x" <> T.replicate 64 "c"

otherCanonicalBlockHash :: Text
otherCanonicalBlockHash = "0x" <> T.replicate 64 "d"

canonicalBlockInfo :: BlockInfo
canonicalBlockInfo =
  BlockInfo
    { biNumber = 123
    , biHash = canonicalBlockHash
    , biTimestamp = 1_750_000_000
    }

canonicalRpcLog :: RpcLog
canonicalRpcLog =
  RpcLog
    { rlAddress = testAsset
    , rlTopics = []
    , rlData = ""
    , rlTxHash = canonicalTxHash
    , rlBlockNumber = 123
    , rlBlockHash = canonicalBlockHash
    , rlTxIndex = 1
    , rlLogIndex = 2
    }

canonicalTransactionResponse :: Value
canonicalTransactionResponse =
  canonicalTransactionResponseWith
    canonicalTxHash
    "0x7b"
    canonicalBlockHash
    "0x1"

canonicalTransactionResponseWith :: Text -> Text -> Text -> Text -> Value
canonicalTransactionResponseWith transactionHash blockNumber blockHash transactionIndex =
  object
    [ "hash" .= transactionHash
    , "blockNumber" .= blockNumber
    , "blockHash" .= blockHash
    , "transactionIndex" .= transactionIndex
    , "from" .= testAccount
    , "to" .= testAsset
    , "input" .= ("0x12345678abcdef" :: Text)
    , "value" .= ("0x2a" :: Text)
    ]

canonicalReceiptResponse :: Value -> Value
canonicalReceiptResponse =
  canonicalReceiptResponseWith
    canonicalTxHash
    "0x7b"
    canonicalBlockHash
    "0x1"

canonicalReceiptResponseWith :: Text -> Text -> Text -> Text -> Value -> Value
canonicalReceiptResponseWith transactionHash blockNumber blockHash transactionIndex receiptLog =
  object
    [ "transactionHash" .= transactionHash
    , "blockNumber" .= blockNumber
    , "blockHash" .= blockHash
    , "transactionIndex" .= transactionIndex
    , "status" .= ("0x1" :: Text)
    , "gasUsed" .= ("0x5208" :: Text)
    , "effectiveGasPrice" .= ("0x3b9aca00" :: Text)
    , "logs" .= [receiptLog]
    ]

canonicalReceiptLogResponse :: Value
canonicalReceiptLogResponse =
  canonicalReceiptLogResponseWith
    canonicalTxHash
    "0x7b"
    canonicalBlockHash
    "0x1"
    "0x2"
    testAsset
    []
    "0x"

canonicalReceiptLogResponseWith
  :: Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> [Text]
  -> Text
  -> Value
canonicalReceiptLogResponseWith transactionHash blockNumber blockHash transactionIndex logIndex address topics eventData =
  object
    [ "address" .= address
    , "topics" .= topics
    , "data" .= eventData
    , "transactionHash" .= transactionHash
    , "blockNumber" .= blockNumber
    , "blockHash" .= blockHash
    , "transactionIndex" .= transactionIndex
    , "logIndex" .= logIndex
    , "removed" .= False
    ]

mkLog :: ByteString -> [ByteString] -> ByteString -> RpcLog
mkLog topic indexedTopics eventData =
  RpcLog
    { rlAddress = fixtureEmitterForTopic topic
    , rlTopics = topic : indexedTopics
    , rlData = eventData
    , rlTxHash = "0xabc"
    , rlBlockNumber = 123
    , rlBlockHash = "0xblock"
    , rlTxIndex = 1
    , rlLogIndex = 2
    }

word :: Integer -> ByteString
word n = BS.pack $ replicate (32 - length bytes) 0 <> bytes
  where
    bytes = toBytes n

signedWord :: Integer -> ByteString
signedWord n
  | n >= 0 = word n
  | otherwise = word (2 ^ (256 :: Int) + n)

words32 :: [Integer] -> ByteString
words32 = BS.concat . map word

toBytes :: Integer -> [Word8]
toBytes 0 = []
toBytes n = reverse $ go n
  where
    go 0 = []
    go value = fromInteger (value `mod` 256) : go (value `div` 256)

addressTopic :: ByteString
addressTopic = word 0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b

otherAddressTopic :: ByteString
otherAddressTopic = word 0x55e007d79906572ccca8e75b1beb302787348d6e

nonCanonicalAddressTopic :: ByteString
nonCanonicalAddressTopic =
  BS.cons 1 $ BS.drop 1 addressTopic

testAccount :: Text
testAccount = "0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b"

testAsset :: Text
testAsset = "0x55e007d79906572ccca8e75b1beb302787348d6e"

testEmitter :: Text
testEmitter = T.toLower marginClearinghouse

marginClearinghouse :: Text
marginClearinghouse = paMarginClearinghouse defaultPerpsAddresses

orderCommittedTopic :: ByteString
orderCommittedTopic = keccak256Text "OrderCommitted(uint64,address,uint8)"

orderExecutedTopic :: ByteString
orderExecutedTopic = keccak256Text "OrderExecuted(uint64,uint256)"

orderFailedTopic :: ByteString
orderFailedTopic = keccak256Text "OrderFailed(uint64,uint8)"

positionOpenedTopic :: ByteString
positionOpenedTopic = keccak256Text "PositionOpened(address,uint8,uint256,uint256,uint256)"

positionClosedTopic :: ByteString
positionClosedTopic = keccak256Text "PositionClosed(address,uint8,uint256,uint256,int256)"

positionLiquidatedTopic :: ByteString
positionLiquidatedTopic = keccak256Text "PositionLiquidated(address,uint8,uint256,uint256,uint256)"

marginAddedTopic :: ByteString
marginAddedTopic = keccak256Text "MarginAdded(address,uint256)"

depositTopic :: ByteString
depositTopic = keccak256Text "Deposit(address,address,uint256)"

withdrawTopic :: ByteString
withdrawTopic = keccak256Text "Withdraw(address,address,uint256)"

erc4626DepositTopic :: ByteString
erc4626DepositTopic = keccak256Text "Deposit(address,address,uint256,uint256)"

erc4626WithdrawTopic :: ByteString
erc4626WithdrawTopic = keccak256Text "Withdraw(address,address,address,uint256,uint256)"

ownershipTransferredTopic :: ByteString
ownershipTransferredTopic = keccak256Text "OwnershipTransferred(address,address)"

pausedTopic :: ByteString
pausedTopic = keccak256Text "Paused(address)"

unpausedTopic :: ByteString
unpausedTopic = keccak256Text "Unpaused(address)"
