module Plether.Protocol.GovernanceSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.List (find, nub)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi (encodeAddress, encodeUint256)
import Plether.Protocol.Governance
import Plether.Protocol.Release (knownProtocolReleases, prId)
import Test.Hspec

spec :: Spec
spec = do
  describe "governanceCategoryDefinitions" $ do
    it "defines every current-release governance category once" $ do
      let categories = map gcdCategory governanceCategoryDefinitions

      categories
        `shouldBe`
          [ RouterConfigCategory
          , OracleConfigCategory
          , EngineRiskConfigCategory
          , EngineCalendarConfigCategory
          , EngineFreshnessConfigCategory
          , HousePoolConfigCategory
          ]
      length (nub categories) `shouldBe` length categories

    it "pins every runtime-confirmed getter and mutation selector" $ do
      map selectorFixture governanceCategoryDefinitions
        `shouldBe`
          [ ( "82ec6a73"
            , "c15a7a3b"
            , "f8eb837c"
            , "28859de7"
            , "4f88dc46"
            )
          , ( "5d0bd589"
            , "09e7b278"
            , "2193e77a"
            , "b7dec522"
            , "8b080f37"
            )
          , ( "a5ee7ed8"
            , "caa07406"
            , "bcc125cb"
            , "5a6568de"
            , "5659c3e4"
            )
          , ( "d0b3dc9e"
            , "af72a36c"
            , "ccca7660"
            , "16c130d0"
            , "5c979586"
            )
          , ( "0704169b"
            , "d8ba68b5"
            , "eee4ed17"
            , "7be906ec"
            , "d7363308"
            )
          , ( "aaedd2f8"
            , "02adac16"
            , "8ca41d93"
            , "f304f82b"
            , "8f244a75"
            )
          ]

    it "pins every runtime-confirmed config event topic" $ do
      map eventTopicFixtures governanceCategoryDefinitions
        `shouldBe`
          [ [ "50f43d7d7264018b50f0304d645fc5d7909bfa8525d69ef53224f1d03938a9bc"
            , "42fafd4798207691f5e97b16d3fd2d7be949e7d759c751e1bdee68ad730df764"
            , "e80f928670c85c79018d89c4a8fe74c6288d82c0bcf7ed864a112bda772341e5"
            ]
          , [ "6ea1577c932b357b3674fbfef3be09776909132d4227236c58278d80e2b40f55"
            , "cdf4058d784198d5e257dfeefbd8d940c4581c48ccc75f812992749513e5f89c"
            , "ec35faf1495e9d4f1dc6613312be82aa6c5f616e56fb6c86195c5baeec904e88"
            ]
          , [ "8a804de3afe8694283c32c7c58d74d75a9d8f339ea4e137101ac7f0e2de5cc18"
            , "054eb576372826004de657dcc0aafe42bf4b44355f2755effe96b3b23624caf7"
            , "08853a15841727bd55bafabe3da764a905f55a55c069d08a362fa11dda5e923d"
            ]
          , [ "909a222ebda2019f0995cd002b10dc33cbd8a1b8b21f8e3c3091ed304cd235d6"
            , "5845de658a699f7d74daf066591b09863ffc0def22621e47bb389d86edf738fe"
            , "65ecf5a6e19e40fa40c41744edefb9e9f425799b0ce289556c9f57824b438cb2"
            ]
          , [ "5b07456262c4c68cd43de09c441a31c0b51ac006eb4e06eb3a767f1565a49668"
            , "a066c1efb26aeee4d245e83035be4c136da8a86a9021f782bd0a7a729a308ef8"
            , "e4ce75d3a4032dee0406d9e2db18de441eaceaad43345f88fed7279b5c08b45f"
            ]
          , [ "447a7a5a0a743802ed94b483d11946e0ee1e6409b6fc5d9e50caee5772ab466d"
            , "27da9d357fe11229821df899d492bc7e2d01badf4bb7d98ef78694a81559e6bd"
            ]
          ]

    it "marks only the calendar tuple as dynamically unavailable" $ do
      map gcdPendingEncoding governanceCategoryDefinitions
        `shouldBe`
          [ StaticGovernanceWords 15
          , StaticGovernanceWords 1
          , StaticGovernanceWords 10
          , UnsupportedDynamicGovernancePayload "calendar_dynamic_tuple_not_supported"
          , StaticGovernanceWords 2
          , StaticGovernanceWords 4
          ]

    it "does not invent a HousePool cancellation event the contract does not emit" $ do
      let definition = governanceCategoryDefinition HousePoolConfigCategory

      governanceFunctionSignature (gcdCancelCall definition)
        `shouldBe` "cancelPoolConfigProposal()"
      map gedLifecycle (gcdEvents definition)
        `shouldBe` [GovernanceProposed, GovernanceFinalized]

  describe "decodePendingGovernance" $ do
    it "strictly decodes risk fields with their stable keys and raw values" $ do
      let definition = governanceCategoryDefinition EngineRiskConfigCategory
          rawValues = [5, 40, 30, 45, 300, 500, 1_000_000, 10, 4, 25]
          payload = mconcat (map encodeUint256 rawValues)

      decodePendingGovernance definition payload
        `shouldBe`
          Right
            ( zipWith
                decodedUint
                (gcdFields definition)
                rawValues
            )

    it "decodes the pending oracle as a canonical address" $ do
      decodePendingGovernance
        (governanceCategoryDefinition OracleConfigCategory)
        (encodeAddress oracleAddress)
        `shouldSatisfy` \result ->
          case result of
            Right [DecodedGovernanceField {dgfValue = GovernanceAddress address}] ->
              address == oracleAddress
            _ -> False

    it "rejects truncated, oversized, and noncanonical static responses" $ do
      let freshness = governanceCategoryDefinition EngineFreshnessConfigCategory
          oracle = governanceCategoryDefinition OracleConfigCategory

      decodePendingGovernance freshness (encodeUint256 60)
        `shouldBe` Left (GovernancePayloadLengthMismatch 64 32)
      decodePendingGovernance freshness (mconcat (map encodeUint256 [60, 90, 120]))
        `shouldBe` Left (GovernancePayloadLengthMismatch 64 96)
      decodePendingGovernance oracle (BS.replicate 32 255)
        `shouldBe` Left (GovernanceNonCanonicalAddress "dependencies.order_router.plether_oracle")

    it "keeps the dynamic calendar tuple explicitly unavailable" $ do
      decodePendingGovernance
        (governanceCategoryDefinition EngineCalendarConfigCategory)
        ""
        `shouldBe` Left (GovernanceDynamicPayloadUnavailable "calendar_dynamic_tuple_not_supported")

  describe "governancePendingState" $ do
    it "treats ETA zero as no proposal even when a stale struct still exists" $ do
      governancePendingState (Just 1_800_000_000) 0
        `shouldBe` NoPendingGovernance

    it "derives pending and ready only from the confirmed timestamp and onchain ETA" $ do
      governancePendingState (Just 1_799_999_999) 1_800_000_000
        `shouldBe` PendingGovernance
      governancePendingState (Just 1_800_000_000) 1_800_000_000
        `shouldBe` ReadyGovernance
      governancePendingState Nothing 1_800_000_000
        `shouldBe` PendingGovernanceTimestampUnavailable

  describe "decodeGovernanceEvent" $ do
    it "decodes static proposal data including the onchain activation time" $ do
      let definition = governanceCategoryDefinition HousePoolConfigCategory
          proposal = eventFor GovernanceProposed definition
          rawValues = [800, 60, 25, 75, 1_800_000_000]
          payload = mconcat (map encodeUint256 rawValues)

      decodeGovernanceEvent definition (gedTopic proposal) payload
        `shouldBe`
          Right
            DecodedGovernanceEvent
              { dgeCategory = HousePoolConfigCategory
              , dgeLifecycle = GovernanceProposed
              , dgeFields =
                  zipWith
                    decodedUint
                    (gedFields proposal)
                    rawValues
              }

    it "decodes zero-data finalization and cancellation events exactly" $ do
      let poolDefinition = governanceCategoryDefinition HousePoolConfigCategory
          routerDefinition = governanceCategoryDefinition RouterConfigCategory
          poolFinalized = eventFor GovernanceFinalized poolDefinition
          routerCancelled = eventFor GovernanceCancelled routerDefinition

      decodeGovernanceEvent poolDefinition (gedTopic poolFinalized) ""
        `shouldBe`
          Right
            DecodedGovernanceEvent
              { dgeCategory = HousePoolConfigCategory
              , dgeLifecycle = GovernanceFinalized
              , dgeFields = []
              }
      decodeGovernanceEvent routerDefinition (gedTopic routerCancelled) ""
        `shouldBe`
          Right
            DecodedGovernanceEvent
              { dgeCategory = RouterConfigCategory
              , dgeLifecycle = GovernanceCancelled
              , dgeFields = []
              }

    it "does not pretend to decode dynamic calendar proposal data" $ do
      let definition = governanceCategoryDefinition EngineCalendarConfigCategory
          proposal = eventFor GovernanceProposed definition

      decodeGovernanceEvent definition (gedTopic proposal) (encodeUint256 32)
        `shouldBe` Left (GovernanceDynamicPayloadUnavailable "calendar_dynamic_tuple_not_supported")

    it "rejects an event topic that does not belong to the category" $ do
      let definition = governanceCategoryDefinition OracleConfigCategory
          unknown = BS.replicate 32 170

      decodeGovernanceEvent definition unknown ""
        `shouldBe` Left (GovernanceUnknownEventTopic unknown)

  describe "governance roles" $ do
    it "resolves governance hosts from the release manifest" $ do
      let release =
            find
              ((== "arbitrum-sepolia-2026-07") . prId)
              knownProtocolReleases

      fmap (`governanceContractAddress` OrderRouterAdminRole) release
        `shouldBe` Just "0x3073d6D021eC20b95a8b7C780f5c30c07036ff6C"
      fmap (`governanceContractAddress` CfdEngineAdminRole) release
        `shouldBe` Just "0xb256d4E88d649b2A149aA8B8caa3159260eFBc39"
      fmap (`governanceContractAddress` HousePoolRole) release
        `shouldBe` Just "0xFA654f4c548130F09C3Fb962AbD4bE32c0357C18"

    it "pins role, pause, binding, and policy getter selectors" $ do
      map getterFixture (governanceRoleGetters OrderRouterAdminRole)
        `shouldBe`
          [ ("governance.owner", "8da5cb5b")
          , ("governance.pending_owner", "e30c3978")
          , ("governance.pauser", "9fd0506d")
          , ("governance.paused", "5c975abb")
          , ("dependencies.order_router", "f887ea40")
          , ("governance.timelock_delay", "5ba1c1a9")
          ]

      map getterFixture (governanceRoleGetters CfdEngineAdminRole)
        `shouldBe`
          [ ("governance.owner", "8da5cb5b")
          , ("governance.pending_owner", "e30c3978")
          , ("dependencies.cfd_engine", "c9d4623f")
          , ("governance.timelock_delay", "5ba1c1a9")
          ]

      map getterFixture (governanceRoleGetters OrderRouterRole)
        `shouldBe`
          [("dependencies.plether_oracle", "ae98f6f2")]

      map getterFixture (governanceRoleGetters CfdEngineRole)
        `shouldBe`
          [ ("governance.owner", "8da5cb5b")
          , ("governance.pending_owner", "e30c3978")
          , ("dependencies.protocol_treasury", "803db96d")
          ]

    it "qualifies current and historical role keys through one stable-key rule" $ do
      qualifyGovernanceKey "order_router_admin" "governance.owner"
        `shouldBe` "governance.order_router_admin.owner"
      qualifyGovernanceKey "cfd_engine" "dependencies.protocol_treasury"
        `shouldBe` "dependencies.cfd_engine.protocol_treasury"
      map gfKey
        (gcdFields $ governanceCategoryDefinition OracleConfigCategory)
        `shouldBe`
          [qualifyGovernanceKey "order_router" "dependencies.plether_oracle"]

    it "describes immutable, one-time, timelocked, and immediate bindings accurately" $ do
      let getterFor role key =
            case find ((== key) . ggdKey) (governanceRoleGetters role) of
              Just definition -> definition
              Nothing -> error "missing governance metadata fixture"
          immutableRouter = getterFor OrderRouterAdminRole "dependencies.order_router"
          oneTimeVault = getterFor HousePoolRole "dependencies.senior_vault"
          timelockedOracle = getterFor OrderRouterRole "dependencies.plether_oracle"
          immediateTreasury = getterFor CfdEngineRole "dependencies.protocol_treasury"
          timelockDelay = getterFor OrderRouterAdminRole "governance.timelock_delay"

      map governanceGetterMutability
        [immutableRouter, oneTimeVault, timelockedOracle, immediateTreasury, timelockDelay]
        `shouldBe` ["immutable", "one_time_set", "governance", "governance", "immutable"]
      map governanceGetterTimelockPolicy
        [immutableRouter, oneTimeVault, timelockedOracle, immediateTreasury, timelockDelay]
        `shouldBe`
          [ "not_applicable"
          , "one_time_set_no_timelock"
          , "admin_timelock"
          , "owner_action_no_timelock"
          , "not_applicable"
          ]

    it "uses the same WAD-factor unit for current and governance-history values" $ do
      let riskFields =
            gcdFields $ governanceCategoryDefinition EngineRiskConfigCategory

      fmap gfUnit (find ((== "market.vpi_factor") . gfKey) riskFields)
        `shouldBe` Just "WAD factor"

    it "pins common role and status event topics" $ do
      map (\eventDefinition -> (gredKey eventDefinition, toHex (gredTopic eventDefinition))) governanceRoleEvents
        `shouldBe`
          [ ( "governance.ownership_transfer_started"
            , "38d16b8cac22d99fc7c124b9cd0de2d3fa1faef420bfe791d8c362d765e22700"
            )
          , ( "governance.ownership_transferred"
            , "8be0079c531659141344cd1fd0a4f28419497f9722a3daafe3b4186f6b6457e0"
            )
          , ( "governance.pauser_updated"
            , "1ff153f4b082245afbf3211a8d2d207da4c5df490e965f9a9ad141b0cd001dda"
            )
          , ( "governance.paused"
            , "62e78cea01bee320cd4e420270b5ea74000d11b0c9f74754ebdbfc544b05a258"
            )
          , ( "governance.unpaused"
            , "5db9ee0a495bf2e6ff9c91a7834c1ba4fdd244a5e8aa4e537bd38aeae4b073aa"
            )
          , ( "governance.protocol_treasury_updated"
            , "b141872ee67913e1bc546464f29b6b07a65159d45c6af64fdecf8b4129157faf"
            )
          ]

    it "strictly decodes canonical address, bool, and uint getter words" $ do
      let getters = governanceRoleGetters OrderRouterAdminRole
          findGetter key =
            case find ((== key) . ggdKey) getters of
              Just definition -> definition
              Nothing -> error "missing governance getter fixture"

      decodeGovernanceGetter (findGetter "governance.owner") (encodeAddress oracleAddress)
        `shouldBe` Right (GovernanceAddress oracleAddress)
      decodeGovernanceGetter (findGetter "governance.paused") (encodeUint256 1)
        `shouldBe` Right (GovernanceBool True)
      decodeGovernanceGetter (findGetter "governance.timelock_delay") (encodeUint256 172_800)
        `shouldBe` Right (GovernanceUint 172_800)

    it "rejects malformed, noncanonical, and invalid getter results" $ do
      let getters = governanceRoleGetters OrderRouterAdminRole
          findGetter key =
            case find ((== key) . ggdKey) getters of
              Just definition -> definition
              Nothing -> error "missing governance getter fixture"

      decodeGovernanceGetter (findGetter "governance.owner") (BS.replicate 32 255)
        `shouldBe` Left (GovernanceNonCanonicalAddress "governance.owner")
      decodeGovernanceGetter (findGetter "governance.paused") (encodeUint256 2)
        `shouldBe` Left (GovernanceInvalidBool "governance.paused" 2)
      decodeGovernanceGetter (findGetter "governance.timelock_delay") (BS.replicate 31 0)
        `shouldBe` Left (GovernancePayloadLengthMismatch 32 31)
      decodeGovernanceGetter
        (findGetter "governance.timelock_delay")
        (encodeUint256 1 <> encodeUint256 2)
        `shouldBe` Left (GovernancePayloadLengthMismatch 32 64)

selectorFixture ::
  GovernanceCategoryDefinition ->
  (Text, Text, Text, Text, Text)
selectorFixture definition =
  ( toHex (governanceFunctionSelector (gcdPendingGetter definition))
  , toHex (governanceFunctionSelector (gcdActivationGetter definition))
  , toHex (governanceFunctionSelector (gcdProposalCall definition))
  , toHex (governanceFunctionSelector (gcdFinalizeCall definition))
  , toHex (governanceFunctionSelector (gcdCancelCall definition))
  )

eventTopicFixtures :: GovernanceCategoryDefinition -> [Text]
eventTopicFixtures = map (toHex . gedTopic) . gcdEvents

eventFor ::
  GovernanceLifecycle ->
  GovernanceCategoryDefinition ->
  GovernanceEventDefinition
eventFor lifecycle definition =
  case find ((== lifecycle) . gedLifecycle) (gcdEvents definition) of
    Just eventDefinition -> eventDefinition
    Nothing -> error "missing governance event fixture"

decodedUint :: GovernanceField -> Integer -> DecodedGovernanceField
decodedUint definition value =
  DecodedGovernanceField
    { dgfDefinition = definition
    , dgfValue = GovernanceUint value
    }

getterFixture :: GovernanceGetterDefinition -> (Text, Text)
getterFixture definition =
  ( ggdKey definition
  , toHex (governanceFunctionSelector (ggdFunction definition))
  )

toHex :: ByteString -> Text
toHex = TE.decodeUtf8 . B16.encode

oracleAddress :: Text
oracleAddress = "0xadfed3bf768d810309b97b4df9f9e77eaa3a401c"
