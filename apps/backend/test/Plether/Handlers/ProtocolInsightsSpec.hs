module Plether.Handlers.ProtocolInsightsSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Database.Protocol
  ( OperationalWalletActivityRow (..)
  , ProtocolActionRow (..)
  , ProtocolTransactionRow (..)
  )
import Plether.Database.Schema (PerpsOrderRow (..))
import Plether.Handlers.ProtocolInsights
  ( AccountStateImpact (..)
  , OrderStateDelta (..)
  , StateRead (..)
  , StateImpactPair (..)
  , TransactionStateImpact (..)
  , housePoolFinancialEvidence
  , mergePendingChanges
  , estimateOperationalTransactionsAtObservedGrossSpend
  , operationalWalletStatus
  , operationalMetricJsonForRoles
  , operationalRolePaginationPolicy
  , orderAtConfirmedBlock
  , orderCommitIntentFromActions
  , orderEconomics
  , orderEconomicsAvailability
  , orderFinalizationFromActions
  , orderRevealTiming
  , orderStateDeltaEvidenceLabel
  , orderTerminalMarketState
  , protocolProjectionIndexerName
  , selectProjectionListAnchor
  , qualifiesObservedOperationalWallet
  , stateDeltaAvailability
  , stateDeltaJson
  , transactionActionAnalysisJson
  , transactionAvailability
  , transactionEvidenceLabel
  , transactionStateImpactJson
  , transactionStateImpactEvidenceLabel
  )
import Plether.Protocol.Release (ProtocolRelease (..), knownProtocolReleases)
import Test.Hspec

spec :: Spec
spec = do
  describe "operational-wallet classification and transaction capacity" $ do
    it "never treats an ordinary actor as operational without an operational action class" $ do
      qualifiesObservedOperationalWallet
        operationalActivity
          { owaActionCount = 1
          , owaTransactionCount = 1
          }
        `shouldBe` False

    it "qualifies observed keepers, liquidators, and governance actors" $ do
      qualifiesObservedOperationalWallet
        operationalActivity {owaExecutionCount = 1}
        `shouldBe` True
      qualifiesObservedOperationalWallet
        operationalActivity {owaLiquidationCount = 1}
        `shouldBe` True
      qualifiesObservedOperationalWallet
        operationalActivity {owaGovernanceCount = 1}
        `shouldBe` True

    it "derives only transaction capacity and classifies low balances deterministically" $ do
      estimateOperationalTransactionsAtObservedGrossSpend (Just 1_000) (Just 25)
        `shouldBe` Just 40
      estimateOperationalTransactionsAtObservedGrossSpend (Just 1_000) Nothing
        `shouldBe` Nothing
      operationalWalletStatus Nothing (Just 25)
        `shouldBe` "unavailable"
      operationalWalletStatus (Just 0) Nothing
        `shouldBe` "depleted"
      operationalWalletStatus (Just 225) (Just 25)
        `shouldBe` "critical"
      operationalWalletStatus (Just 2_475) (Just 25)
        `shouldBe` "warning"
      operationalWalletStatus (Just 2_500) (Just 25)
        `shouldBe` "healthy"

    it "renders updater-only unattributable metrics as null rather than factual zero" $ do
      operationalMetricJsonForRoles ["oracle_updater"] 0
        `shouldBe` Null
      operationalMetricJsonForRoles ["order_keeper"] 0
        `shouldBe` String "0"
      operationalMetricJsonForRoles ["oracle_updater", "observed_keeper"] 0
        `shouldBe` String "0"

    it "fails closed for partial role reads on cursor continuations" $ do
      operationalRolePaginationPolicy False False
        `shouldBe` (True, False)
      operationalRolePaginationPolicy True False
        `shouldBe` (False, False)
      operationalRolePaginationPolicy True True
        `shouldBe` (True, True)

  describe "selectProjectionListAnchor" $ do
    it "anchors the first page at the lower chain-confirmed or contiguous projection head" $ do
      selectProjectionListAnchor 100 140 125 `shouldBe` Just 125
      selectProjectionListAnchor 100 120 150 `shouldBe` Just 120
      selectProjectionListAnchor 100 140 140 `shouldBe` Just 140

    it "fails closed until both heads cover the release deployment block" $ do
      selectProjectionListAnchor 100 99 125 `shouldBe` Nothing
      selectProjectionListAnchor 100 125 99 `shouldBe` Nothing

  describe "protocolProjectionIndexerName" $ do
    it "uses the bounded V2 cursor namespace for the current router and lifecycle release" $ do
      let currentRelease =
            listToMaybe
              [ candidateRelease
              | candidateRelease <- knownProtocolReleases
              , prId candidateRelease == "arbitrum-sepolia-2026-08-v1.2.0"
              ]

      fmap protocolProjectionIndexerName currentRelease
        `shouldBe` Just "perps-history-costs-v2:finalized-abi3"

    it "preserves the V1 cursor namespace for the historical router-only release" $ do
      let historicalRelease =
            listToMaybe
              [ candidateRelease
              | candidateRelease <- knownProtocolReleases
              , prId candidateRelease == "arbitrum-sepolia-2026-07"
              ]

      fmap protocolProjectionIndexerName historicalRelease
        `shouldBe` Just "perps-history-costs-v1"

  describe "orderRevealTiming" $ do
    it "opens on the first second after commitment and closes at the settlement-window bound" $
      orderRevealTiming (Just 1_000) (Just 60)
        `shouldBe` (Just 1_001, Just 1_060)

    it "keeps independently knowable bounds when one source is unavailable" $ do
      orderRevealTiming (Just 1_000) Nothing
        `shouldBe` (Just 1_001, Nothing)
      orderRevealTiming Nothing (Just 60)
        `shouldBe` (Nothing, Nothing)

  describe "V2 order lifecycle evidence" $ do
    it "selects the target order's canonical intent and exposes every execution bound" $ do
      let anotherOrder =
            (intentRegisteredAction 99)
              { parData =
                  object
                    [ "intentHash" .= hashOf '9'
                    , "request" .= object
                        [ "side" .= (1 :: Integer)
                        , "sizeDelta" .= ("99" :: Text)
                        , "marginDeltaUsdc" .= ("99" :: Text)
                        , "acceptablePrice" .= ("99" :: Text)
                        , "isClose" .= False
                        , "policy" .= object []
                        ]
                    ]
              }
          intent =
            orderCommitIntentFromActions
              release
              42
              [anotherOrder, intentRegisteredAction 42]

      (intent >>= jsonField ["acceptablePrice"])
        `shouldBe` Just (String "101000000")
      (intent >>= jsonField ["executionBountyUsdc"])
        `shouldBe` Just (String "250000")
      (intent >>= jsonField ["policy", "maxGrossAccountDebitUsdc"])
        `shouldBe` Just (String "5000000")
      (intent >>= jsonField ["policy", "maxPostLeverageBps"])
        `shouldBe` Just (String "25000")
      (intent >>= jsonField ["evidence", "level"])
        `shouldBe` Just (String "exact_confirmed_intent_registered_event")

    it "projects exact terminal metadata and receipt economics without relabelling the fee" $ do
      let actions = [intentRegisteredAction 42, orderFinalizedAction 42]
          finalization = orderFinalizationFromActions release 42 actions
          economics = orderEconomics 42 Nothing actions
          availability = show $ orderEconomicsAvailability 42 Nothing actions

      (finalization >>= jsonField ["executionMode"])
        `shouldBe` Just (String "Permissionless")
      (finalization >>= jsonField ["executor"])
        `shouldBe` Just (String keeper)
      (finalization >>= jsonField ["failure", "failedConstraintCode"])
        `shouldBe` Just (Number 0)
      (finalization >>= jsonField ["receipt", "observedConfigHash"])
        `shouldBe` Just (String $ hashOf '8')
      jsonField ["realizedPnlUsdc"] economics
        `shouldBe` Just (String "-250000")
      jsonField ["carryUsdc"] economics
        `shouldBe` Just (String "-10000")
      jsonField ["executionFeeUsdc"] economics
        `shouldBe` Just (String "5000")
      jsonField ["claimCreatedUsdc"] economics
        `shouldBe` Just (String "40000")
      jsonField ["protocolFeeUsdc"] economics
        `shouldBe` Just Null
      availability `shouldNotContain` "economics.carryUsdc"
      availability `shouldContain` "receipt_does_not_isolate_protocol_fee"

  describe "orderTerminalMarketState" $ do
    it "marks a null market mode unavailable when either source read is missing" $ do
      let terminalState =
            orderTerminalMarketState
              (Just 123)
              exactStatusRead
              (StateRead Nothing $ Just "canonical_block_state_unavailable")

      jsonField ["marketMode"] terminalState
        `shouldBe` Just Null
      jsonField ["provenance", "marketMode"] terminalState
        `shouldBe` Just (String "unavailable")
      jsonField ["provenance", "protocolStatus"] terminalState
        `shouldBe` Just (String "exact_historical_protocol_status_read_at_terminal_block")
      jsonField ["provenance", "housePool"] terminalState
        `shouldBe` Just (String "unavailable")
      jsonField ["provenance", "state"] terminalState
        `shouldBe` Just (String "partial_historical_contract_reads_at_terminal_block")
      show terminalState
        `shouldContain` "terminal_market_state_sources_unavailable"

    it "derives market mode only from complete historical status and pool reads" $ do
      let terminalState =
            orderTerminalMarketState
              (Just 123)
              exactStatusRead
              exactPoolRead

      jsonField ["marketMode"] terminalState
        `shouldBe` Just (String "trading_inactive")
      jsonField ["provenance", "marketMode"] terminalState
        `shouldBe` Just (String "derived_from_exact_historical_state_flags")
      jsonField ["provenance", "state"] terminalState
        `shouldBe` Just (String "exact_historical_contract_reads_at_terminal_block")

  describe "orderAtConfirmedBlock" $ do
    it "hides a commitment newer than the confirmed response envelope" $
      (porOrderId <$> orderAtConfirmedBlock 99 completeOrder)
        `shouldBe` Nothing

    it "strips a future terminal update back to the confirmed pending state" $ do
      let projected = orderAtConfirmedBlock 110 completeOrder

      (porTerminalStatus <$> projected) `shouldBe` Just "Pending"
      (projected >>= porTerminalTxHash) `shouldBe` Nothing
      (projected >>= porTerminalBlockNumber) `shouldBe` Nothing
      (projected >>= porActivityType) `shouldBe` Nothing
      (porSortBlock <$> projected) `shouldBe` Just 100

    it "preserves terminal evidence already inside the confirmed envelope" $ do
      let projected = orderAtConfirmedBlock 120 completeOrder

      (projected >>= porTerminalTxHash)
        `shouldBe` porTerminalTxHash completeOrder
      (porTerminalStatus <$> projected)
        `shouldBe` Just (porTerminalStatus completeOrder)
      (porActivityType =<< projected)
        `shouldBe` porActivityType completeOrder

  describe "transaction evidence completeness" $ do
    it "requires the full transaction envelope, input, and receipt before labelling evidence exact" $ do
      transactionEvidenceLabel completeTransaction
        `shouldBe` "exact_transaction_input_and_receipt"
      transactionAvailability completeTransaction
        `shouldBe` []

    it "labels missing transaction envelope fields as partial with machine-readable reasons" $ do
      let incomplete =
            completeTransaction
              { ptrSender = Nothing
              , ptrNativeValue = Nothing
              }
          rendered = show $ transactionAvailability incomplete

      transactionEvidenceLabel incomplete
        `shouldBe` "partial_transaction_evidence"
      rendered `shouldContain` "transaction.sender"
      rendered `shouldContain` "transaction.nativeValueWei"

  describe "mergePendingChanges" $ do
    it "keeps direct state authoritative while restoring correlated proposal evidence" $ do
      let direct =
            object
              [ "changeId" .= ("direct:risk:max_leverage:2000" :: Text)
              , "parameterKey" .= ("risk.max_leverage" :: Text)
              , "eta" .= ("2000" :: Text)
              , "newValue" .= ("500" :: Text)
              , "proposer" .= (Nothing :: Maybe Text)
              , "proposedAt" .= (Nothing :: Maybe Integer)
              , "txHash" .= (Nothing :: Maybe Text)
              , "availability" .=
                  [ object
                      [ "field" .= ("proposer" :: Text)
                      , "reason" .= ("proposal_transaction_not_correlated" :: Text)
                      ]
                  , object
                      [ "field" .= ("proposedAt" :: Text)
                      , "reason" .= ("proposal_transaction_not_correlated" :: Text)
                      ]
                  , object
                      [ "field" .= ("txHash" :: Text)
                      , "reason" .= ("proposal_transaction_not_correlated" :: Text)
                      ]
                  ]
              ]
          indexed =
            object
              [ "changeId" .= ("proposal:42" :: Text)
              , "parameterKey" .= ("risk.max_leverage" :: Text)
              , "eta" .= ("2000" :: Text)
              , "newValue" .= ("499" :: Text)
              , "proposer" .= ("0x1111111111111111111111111111111111111111" :: Text)
              , "proposedAt" .= (1_900 :: Integer)
              , "txHash" .= hashOf 'd'
              , "evidence" .= object ["level" .= ("exact" :: Text)]
              ]
          rendered = show $ mergePendingChanges [direct] [indexed]

      rendered `shouldContain` "500"
      rendered `shouldNotContain` "499"
      rendered `shouldContain` "0x1111111111111111111111111111111111111111"
      rendered `shouldContain` T.unpack (hashOf 'd')
      rendered `shouldContain` "governance.pending.direct_indexed_correlation.v1"
      rendered `shouldNotContain` "proposal_transaction_not_correlated"

    it "does not copy proposal identity from a superseded projection with another ETA" $ do
      let direct =
            object
              [ "changeId" .= ("direct:risk:max_leverage:2100" :: Text)
              , "parameterKey" .= ("risk.max_leverage" :: Text)
              , "eta" .= ("2100" :: Text)
              , "newValue" .= ("600" :: Text)
              , "proposer" .= (Nothing :: Maybe Text)
              , "proposedAt" .= (Nothing :: Maybe Integer)
              , "txHash" .= (Nothing :: Maybe Text)
              , "availability" .=
                  [ object
                      [ "field" .= ("proposer" :: Text)
                      , "reason" .= ("proposal_transaction_not_correlated" :: Text)
                      ]
                  ]
              ]
          superseded =
            object
              [ "changeId" .= ("proposal:old" :: Text)
              , "parameterKey" .= ("risk.max_leverage" :: Text)
              , "eta" .= ("2000" :: Text)
              , "newValue" .= ("500" :: Text)
              , "proposer" .= ("0x1111111111111111111111111111111111111111" :: Text)
              , "proposedAt" .= (1_900 :: Integer)
              , "txHash" .= hashOf 'd'
              ]
          rendered = show $ mergePendingChanges [direct] [superseded]

      rendered `shouldContain` "600"
      rendered `shouldNotContain` "500"
      rendered `shouldNotContain` "0x1111111111111111111111111111111111111111"
      rendered `shouldNotContain` T.unpack (hashOf 'd')
      rendered `shouldContain` "proposal_transaction_not_correlated"

  describe "transactionStateImpactJson" $ do
    it "labels canonical before/after reads as block-level and derives signed field deltas" $ do
      let rendered = show $ transactionStateImpactJson completeImpact

      rendered `shouldContain` "block-level delta"
      rendered `shouldContain` "protocol.transaction.state_block_before_after.v1"
      rendered `shouldContain` "freeUsdcDelta"
      rendered `shouldContain` "-2000000"
      rendered `shouldContain` "principalUsdcDelta"
      rendered `shouldContain` "1250000"
      rendered `shouldContain` "historical_contract_reads"
      rendered `shouldContain` "SeniorVault"
      rendered `shouldContain` "JuniorVault"

    it "does not label wholly unavailable historical reads as a block-level delta" $ do
      let unavailablePair =
            StateImpactPair
              { sipBefore = Nothing
              , sipAfter = Nothing
              , sipBeforeReason = Just "archive_state_unavailable"
              , sipAfterReason = Just "archive_state_unavailable"
              }
          unavailableImpact =
            completeImpact
              { tsiAccounts = []
              , tsiHousePool = unavailablePair
              , tsiSenior = unavailablePair
              , tsiJunior = unavailablePair
              , tsiBeforeBlockHash = Nothing
              }

      transactionStateImpactEvidenceLabel unavailableImpact
        `shouldBe` "unavailable"

    it "keeps a partially populated tranche read partial and preserves its reason" $ do
      let partialImpact =
            completeImpact
              { tsiSenior =
                  (tsiSenior completeImpact)
                    { sipAfterReason =
                        Just "canonical_block_state_unavailable"
                    }
              }
          rendered = show $ transactionStateImpactJson partialImpact

      transactionStateImpactEvidenceLabel partialImpact
        `shouldBe` "partial_canonical_hash_bound_block-level_state"
      jsonField ["senior", "provenance"] (transactionStateImpactJson partialImpact)
        `shouldBe` Just (String "partial block-level state")
      rendered `shouldContain` "stateImpact.senior.after"
      rendered `shouldContain` "canonical_block_state_unavailable"

      let analysis =
            transactionActionAnalysisJson
              release
              [trancheAction]
              partialImpact
          trancheAnalysis = do
            items <- jsonField ["trancheActions"] analysis
            firstArrayItem items

      (trancheAnalysis >>= jsonField ["provenance", "trancheStateImpact"])
        `shouldBe` Just (String "partial block-level state")
      (trancheAnalysis >>= jsonField ["provenance", "navDeltaUsdc"])
        `shouldBe` Just (String "partial block-level state")
      show analysis `shouldContain` "canonical_block_state_unavailable"

  describe "order state-delta evidence" $ do
    it "does not upgrade partial tranche reads to a complete order delta" $ do
      orderStateDeltaEvidenceLabel partialOrderStateDelta
        `shouldBe` "partial_canonical_hash_bound_block-level_state"
      jsonField ["senior", "provenance"] (stateDeltaJson partialOrderStateDelta)
        `shouldBe` Just (String "partial block-level state")
      show (stateDeltaAvailability partialOrderStateDelta)
        `shouldContain` "canonical_block_state_unavailable"

  describe "HousePool financial evidence" $ do
    it "labels bounded liability, coverage, and headroom independently" $ do
      let evidence =
            housePoolFinancialEvidence
              (Just 1)
              Nothing
              (Just 2)

      jsonField ["boundedLiability"] evidence
        `shouldBe` Just (String "exact_historical_contract_read")
      jsonField ["coverageRatio"] evidence
        `shouldBe` Just (String "unavailable")
      jsonField ["solvencyHeadroom"] evidence
        `shouldBe` Just (String "derived_from_same_block_state_v1")

  describe "transactionActionAnalysisJson" $ do
    it "uses emitted economics and state deltas while leaving settlement telemetry unavailable" $ do
      let analysis =
            transactionActionAnalysisJson
              release
              [liquidationAction, marginAction, trancheAction]
              completeImpact
          rendered = show analysis

      rendered `shouldContain` "observedKeeperBountyUsdc"
      rendered `shouldContain` "750000"
      rendered `shouldContain` "observedMarginFlowUsdc"
      rendered `shouldContain` "-1000000"
      rendered `shouldContain` "observedTrancheAssetFlowUsdc"
      rendered `shouldContain` "2500000"
      rendered `shouldContain` "clearedPendingOrders"
      rendered `shouldContain` "current_release_settlement_telemetry_missing"
      rendered `shouldContain` "protocolFeeUsdc"
      rendered `shouldContain` "Null"
      rendered `shouldContain` "protocol.transaction.action_analysis.v1"

release :: ProtocolRelease
release =
  case knownProtocolReleases of
    current : _ -> current
    [] -> error "The protocol release manifest must define a current release"

completeImpact :: TransactionStateImpact
completeImpact =
  TransactionStateImpact
    { tsiAccounts =
        [ AccountStateImpact
            { asiAccount = account
            , asiActionIds = ["liquidation-action", "margin-action"]
            , asiActionTypes = ["liquidation", "margin_withdraw"]
            , asiState =
                exactPair
                  ( object
                      [ "liquidatable" .= True
                      , "terminalReachableUsdc" .= ("5000000" :: Text)
                      , "pendingOrderCount" .= ("3" :: Text)
                      , "settlementBalanceUsdc" .= ("10000000" :: Text)
                      ]
                  )
                  ( object
                      [ "liquidatable" .= False
                      , "traderClaimBalanceUsdc" .= ("1250000" :: Text)
                      , "pendingOrderCount" .= ("1" :: Text)
                      , "settlementBalanceUsdc" .= ("9000000" :: Text)
                      ]
                  )
            }
        ]
    , tsiHousePool =
        exactPair
          (object ["freeUsdc" .= ("10000000" :: Text)])
          (object ["freeUsdc" .= ("8000000" :: Text)])
    , tsiSenior =
        exactPair
          ( object
              [ "principalUsdc" .= ("5000000" :: Text)
              , "navUsdc" .= ("5100000" :: Text)
              , "shareSupply" .= ("1000000000000000000" :: Text)
              ]
          )
          ( object
              [ "principalUsdc" .= ("6250000" :: Text)
              , "navUsdc" .= ("6350000" :: Text)
              , "shareSupply" .= ("1500000000000000000" :: Text)
              ]
          )
    , tsiJunior =
        exactPair
          (object ["principalUsdc" .= ("5000000" :: Text)])
          (object ["principalUsdc" .= ("1750000" :: Text)])
    , tsiBeforeBlockNumber = Just 122
    , tsiBeforeBlockHash = Just $ hashOf 'a'
    , tsiAfterBlockNumber = 123
    , tsiAfterBlockHash = hashOf 'b'
    , tsiTransactionHash = hashOf 'c'
    , tsiCalculationVersion = "protocol-transparency-v1"
    }

exactPair :: Value -> Value -> StateImpactPair
exactPair beforeState afterState =
  StateImpactPair
    { sipBefore = Just beforeState
    , sipAfter = Just afterState
    , sipBeforeReason = Nothing
    , sipAfterReason = Nothing
    }

exactStatusRead :: StateRead [Integer]
exactStatusRead =
  StateRead
    { srValue = Just $ replicate 11 0
    , srUnavailableReason = Nothing
    }

exactPoolRead :: StateRead [Integer]
exactPoolRead =
  StateRead
    { srValue = Just $ replicate 11 0
    , srUnavailableReason = Nothing
    }

partialOrderStateDelta :: OrderStateDelta
partialOrderStateDelta =
  OrderStateDelta
    { osdPositionBefore = Just $ object ["marginUsdc" .= ("1000000" :: Text)]
    , osdPositionAfter = Just $ object ["marginUsdc" .= ("900000" :: Text)]
    , osdPositionBeforeReason = Nothing
    , osdPositionAfterReason = Nothing
    , osdPoolBefore = Just $ object ["freeUsdc" .= ("10000000" :: Text)]
    , osdPoolAfter = Just $ object ["freeUsdc" .= ("9000000" :: Text)]
    , osdPoolBeforeReason = Nothing
    , osdPoolAfterReason = Nothing
    , osdSeniorBefore =
        Just $
          object
            [ "principalUsdc" .= ("5000000" :: Text)
            , "navUsdc" .= ("5100000" :: Text)
            ]
    , osdSeniorAfter =
        Just $
          object
            [ "principalUsdc" .= ("5250000" :: Text)
            , "navUsdc" .= ("5350000" :: Text)
            ]
    , osdSeniorBeforeReason = Nothing
    , osdSeniorAfterReason = Just "canonical_block_state_unavailable"
    , osdJuniorBefore = Just $ object ["principalUsdc" .= ("5000000" :: Text)]
    , osdJuniorAfter = Just $ object ["principalUsdc" .= ("4750000" :: Text)]
    , osdJuniorBeforeReason = Nothing
    , osdJuniorAfterReason = Nothing
    , osdBeforeBlockNumber = Just 122
    , osdBeforeBlockHash = Just $ hashOf 'a'
    , osdAfterBlockNumber = Just 123
    , osdAfterBlockHash = Just $ hashOf 'b'
    , osdTerminalTxHash = Just $ hashOf 'c'
    , osdCalculationVersion = "protocol-transparency-v1"
    }

liquidationAction :: ProtocolActionRow
liquidationAction =
  action
    "liquidation-action"
    "liquidation"
    ( object
        [ "price" .= ("99000000" :: Text)
        , "sizeDelta" .= ("1000000000000000000" :: Text)
        , "keeperBountyUsdc" .= ("750000" :: Text)
        ]
    )
    (prCfdEngine release)

marginAction :: ProtocolActionRow
marginAction =
  action
    "margin-action"
    "margin_withdraw"
    (object ["amountUsdc" .= ("1000000" :: Text)])
    (prMarginClearinghouse release)

trancheAction :: ProtocolActionRow
trancheAction =
  action
    "tranche-action"
    "tranche_deposit"
    ( object
        [ "assets" .= ("2500000" :: Text)
        , "shares" .= ("500000000000000000" :: Text)
        ]
    )
    (prSeniorVault release)

intentRegisteredAction :: Integer -> ProtocolActionRow
intentRegisteredAction orderId =
  ( action
      ("intent-" <> T.pack (show orderId))
      "order_commitment"
      ( object
          [ "orderId" .= show orderId
          , "clientOrderId" .= hashOf '1'
          , "intentHash" .= hashOf '2'
          , "executionBountyUsdc" .= ("250000" :: Text)
          , "request" .= object
              [ "clientOrderId" .= hashOf '1'
              , "side" .= (0 :: Integer)
              , "sizeDelta" .= ("1000000000000000000" :: Text)
              , "marginDeltaUsdc" .= ("1000000" :: Text)
              , "acceptablePrice" .= ("101000000" :: Text)
              , "isClose" .= False
              , "policy" .= executionPolicy
              ]
          , "policy" .= executionPolicy
          , "units" .= object
              [ "sizeDelta" .= ("position:18" :: Text)
              , "marginDeltaUsdc" .= ("USDC:6" :: Text)
              , "acceptablePrice" .= ("indexPrice:8" :: Text)
              ]
          ]
      )
      lifecycleBook
  )
    { parOrderId = Just orderId
    }
  where
    executionPolicy =
      object
        [ "validUntil" .= ("1785000060" :: Text)
        , "allowedExecutionModes" .= (3 :: Integer)
        , "expectedConfigHash" .= hashOf '7'
        , "maxExecutionBountyUsdc" .= ("250000" :: Text)
        , "maxExecutionNotionalUsdc" .= ("100000000" :: Text)
        , "maxGrossAccountDebitUsdc" .= ("5000000" :: Text)
        , "maxActionChargeUsdc" .= ("25000" :: Text)
        , "maxExplicitFeesUsdc" .= ("50000" :: Text)
        , "maxPostPositionSize" .= ("2000000000000000000" :: Text)
        , "minPostSettlementBalanceUsdc" .= ("100000" :: Text)
        , "minPostPositionEquityUsdc" .= ("500000" :: Text)
        , "maxPostLeverageBps" .= ("25000" :: Text)
        ]

orderFinalizedAction :: Integer -> ProtocolActionRow
orderFinalizedAction orderId =
  ( action
      ("finalized-" <> T.pack (show orderId))
      "order_execution"
      ( object
          [ "orderId" .= show orderId
          , "clientOrderId" .= hashOf '1'
          , "receiptHash" .= hashOf '6'
          , "intentHash" .= hashOf '2'
          , "expectedConfigHash" .= hashOf '7'
          , "observedConfigHash" .= hashOf '8'
          , "terminalReason" .= ("Executed" :: Text)
          , "executionMode" .= ("Permissionless" :: Text)
          , "executor" .= keeper
          , "failedConstraint" .= (Nothing :: Maybe Text)
          , "executionPrice" .= ("100500000" :: Text)
          , "oraclePublishTime" .= ("1785000058" :: Text)
          , "bountyUsdc" .= ("250000" :: Text)
          , "bountyRecipient" .= keeper
          , "failure" .= terminalFailure
          , "economics" .= receiptEconomics
          , "receipt" .= object
              [ "lifecycleStatus" .= (2 :: Integer)
              , "status" .= ("Executed" :: Text)
              , "terminalReasonCode" .= (1 :: Integer)
              , "terminalReason" .= ("Executed" :: Text)
              , "executionModeCode" .= (1 :: Integer)
              , "executionMode" .= ("Permissionless" :: Text)
              , "executor" .= keeper
              , "executionPrice" .= ("100500000" :: Text)
              , "observedConfigHash" .= hashOf '8'
              , "failure" .= terminalFailure
              , "economics" .= receiptEconomics
              ]
          , "units" .= object ["executionPrice" .= ("indexPrice:8" :: Text)]
          ]
      )
      lifecycleBook
  )
    { parOrderId = Just orderId
    }
  where
    terminalFailure =
      object
        [ "selector" .= ("0x00000000" :: Text)
        , "category" .= (0 :: Integer)
        , "code" .= (0 :: Integer)
        , "failedConstraintCode" .= (0 :: Integer)
        , "failedConstraint" .= (Nothing :: Maybe Text)
        , "argument0" .= ("0" :: Text)
        , "argument1" .= ("0" :: Text)
        , "revertDataHash" .= hashOf '0'
        ]
    receiptEconomics =
      object
        [ "executionNotionalUsdc" .= ("100000000" :: Text)
        , "realizedPnlUsdc" .= ("-250000" :: Text)
        , "vpiUsdc" .= ("15000" :: Text)
        , "carryUsdc" .= ("-10000" :: Text)
        , "executionFeeUsdc" .= ("5000" :: Text)
        , "frozenSpreadUsdc" .= ("0" :: Text)
        , "preTraderClaimBalanceUsdc" .= ("100000" :: Text)
        , "postTraderClaimBalanceUsdc" .= ("140000" :: Text)
        ]

lifecycleBook :: Text
lifecycleBook =
  case prOrderLifecycleBook release of
    Just address -> address
    Nothing -> error "The current release must configure OrderLifecycleBook"

action :: Text -> Text -> Value -> Text -> ProtocolActionRow
action actionId actionType actionData contractAddress =
  ProtocolActionRow
    { parActionId = actionId
    , parTxHash = hashOf 'c'
    , parBlockNumber = 123
    , parBlockHash = hashOf 'b'
    , parTxIndex = 1
    , parLogIndex =
        case actionType of
          "liquidation" -> 1
          "margin_withdraw" -> 2
          _ -> 3
    , parTimestamp = 1_785_000_000
    , parActionType = actionType
    , parStatus = "success"
    , parAccount = Just account
    , parActor = Just keeper
    , parOrderId = Nothing
    , parContractAddress = contractAddress
    , parData = actionData
    , parEvidence = object ["level" .= ("exact" :: Text)]
    }

completeOrder :: PerpsOrderRow
completeOrder =
  PerpsOrderRow
    { porOrderId = 42
    , porOrderRouter = prOrderRouter release
    , porAccount = Just account
    , porSide = Just 0
    , porCommitTxHash = Just $ hashOf 'a'
    , porCommitBlockNumber = Just 100
    , porCommitTimestamp = Just 1_000
    , porTerminalTxHash = Just $ hashOf 'b'
    , porTerminalBlockNumber = Just 120
    , porTerminalTimestamp = Just 1_060
    , porTerminalStatus = "Executed"
    , porFailureReason = Nothing
    , porExecutionPrice = Just 101_000_000
    , porCleanupActor = Just keeper
    , porActivityType = Just "Open"
    , porActivitySizeDelta = Just 1_000_000_000_000_000_000
    , porActivityPrice = Just 101_000_000
    , porActivityPnlUsdc = Just 0
    , porSortBlock = 120
    }

completeTransaction :: ProtocolTransactionRow
completeTransaction =
  ProtocolTransactionRow
    { ptrTxHash = hashOf 'e'
    , ptrBlockNumber = 120
    , ptrBlockHash = hashOf 'f'
    , ptrTxIndex = 1
    , ptrTimestamp = 1_060
    , ptrSender = Just keeper
    , ptrRecipient = Just $ prOrderRouter release
    , ptrSelector = Just "0x12345678"
    , ptrStatus = "success"
    , ptrGasUsed = Just 100_000
    , ptrEffectiveGasPrice = Just 1_000_000
    , ptrNativeValue = Just 0
    , ptrInputData = Just "0x12345678"
    , ptrEvidence = object ["level" .= ("exact" :: Text)]
    }

operationalActivity :: OperationalWalletActivityRow
operationalActivity =
  OperationalWalletActivityRow
    { owaAddress = keeper
    , owaActionCount = 0
    , owaTransactionCount = 0
    , owaExecutionCount = 0
    , owaCleanupCount = 0
    , owaLiquidationCount = 0
    , owaMaintenanceCount = 0
    , owaGovernanceCount = 0
    , owaFirstActivityAt = 1_000
    , owaLastActivityAt = 1_000
    }

account :: Text
account = "0x2222222222222222222222222222222222222222"

keeper :: Text
keeper = "0x3333333333333333333333333333333333333333"

hashOf :: Char -> Text
hashOf character =
  "0x" <> T.pack (replicate 64 character)

jsonField :: [Text] -> Value -> Maybe Value
jsonField [] value = Just value
jsonField (fieldName : rest) (Object fields) =
  KM.lookup (Key.fromText fieldName) fields >>= jsonField rest
jsonField _ _ = Nothing

firstArrayItem :: Value -> Maybe Value
firstArrayItem (Array values) = listToMaybe $ toList values
firstArrayItem _ = Nothing
