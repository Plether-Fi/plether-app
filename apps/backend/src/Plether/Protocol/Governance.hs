module Plether.Protocol.Governance
  ( GovernanceContractRole (..)
  , GovernanceCategory (..)
  , GovernanceFieldType (..)
  , GovernanceField (..)
  , GovernancePayloadEncoding (..)
  , GovernanceFunction (..)
  , GovernanceLifecycle (..)
  , GovernanceEventDefinition (..)
  , GovernanceCategoryDefinition (..)
  , GovernanceGetterPurpose (..)
  , GovernanceGetterDefinition (..)
  , GovernanceRoleEventDefinition (..)
  , GovernancePendingState (..)
  , GovernanceDecodedValue (..)
  , DecodedGovernanceField (..)
  , DecodedGovernanceEvent (..)
  , GovernanceDecodeError (..)
  , governanceCategoryDefinitions
  , governanceCategoryDefinition
  , governanceContractAddress
  , governanceContractRoleKey
  , qualifyGovernanceKey
  , governanceRoleGetters
  , governanceGetterMutability
  , governanceGetterTimelockPolicy
  , governanceRoleEvents
  , governancePendingState
  , decodeGovernanceGetter
  , decodePendingGovernance
  , decodeGovernanceEvent
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi (decodeAddress, decodeBool, decodeUint256, keccak256, selector)
import Plether.Protocol.Release
  ( ProtocolRelease (..)
  )

-- | Release-manifest role used to resolve a governance contract or its host.
data GovernanceContractRole
  = OrderRouterAdminRole
  | CfdEngineAdminRole
  | HousePoolRole
  | OrderRouterRole
  | CfdEngineRole
  | PletherOracleRole
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data GovernanceCategory
  = RouterConfigCategory
  | OracleConfigCategory
  | EngineRiskConfigCategory
  | EngineCalendarConfigCategory
  | EngineFreshnessConfigCategory
  | HousePoolConfigCategory
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data GovernanceFieldType
  = Uint256Field
  | AddressField
  | BoolField
  | Uint256ArrayField
  deriving stock (Show, Eq)

-- | Stable field metadata. Scale is the raw integer denominator, expressed as
-- text so API consumers never need to round an onchain integer.
data GovernanceField = GovernanceField
  { gfKey :: Text
  , gfType :: GovernanceFieldType
  , gfScale :: Text
  , gfUnit :: Text
  }
  deriving stock (Show, Eq)

-- | Static results are decoded only when their byte length is exact. Dynamic
-- calendar tuples are deliberately marked unsupported until a bounded ABI
-- decoder is wired into the live read path.
data GovernancePayloadEncoding
  = StaticGovernanceWords Int
  | UnsupportedDynamicGovernancePayload Text
  deriving stock (Show, Eq)

data GovernanceFunction = GovernanceFunction
  { governanceFunctionSignature :: Text
  , governanceFunctionSelector :: ByteString
  }
  deriving stock (Show, Eq)

data GovernanceLifecycle
  = GovernanceProposed
  | GovernanceFinalized
  | GovernanceCancelled
  deriving stock (Show, Eq, Ord)

data GovernanceEventDefinition = GovernanceEventDefinition
  { gedLifecycle :: GovernanceLifecycle
  , gedSignature :: Text
  , gedTopic :: ByteString
  , gedPayloadEncoding :: GovernancePayloadEncoding
  , gedFields :: [GovernanceField]
  }
  deriving stock (Show, Eq)

data GovernanceCategoryDefinition = GovernanceCategoryDefinition
  { gcdCategory :: GovernanceCategory
  , gcdContractRole :: GovernanceContractRole
  , gcdPendingGetter :: GovernanceFunction
  , gcdPendingEncoding :: GovernancePayloadEncoding
  , gcdActivationGetter :: GovernanceFunction
  , gcdFields :: [GovernanceField]
  , gcdProposalCall :: GovernanceFunction
  , gcdFinalizeCall :: GovernanceFunction
  , gcdCancelCall :: GovernanceFunction
  , gcdEvents :: [GovernanceEventDefinition]
  }
  deriving stock (Show, Eq)

data GovernanceGetterPurpose
  = RoleGetter
  | StatusGetter
  | BindingGetter
  | PolicyGetter
  deriving stock (Show, Eq)

data GovernanceGetterDefinition = GovernanceGetterDefinition
  { ggdKey :: Text
  , ggdPurpose :: GovernanceGetterPurpose
  , ggdFunction :: GovernanceFunction
  , ggdValueType :: GovernanceFieldType
  }
  deriving stock (Show, Eq)

data GovernanceRoleEventDefinition = GovernanceRoleEventDefinition
  { gredKey :: Text
  , gredSignature :: Text
  , gredTopic :: ByteString
  , gredContractRoles :: [GovernanceContractRole]
  }
  deriving stock (Show, Eq)

data GovernancePendingState
  = NoPendingGovernance
  | PendingGovernance
  | ReadyGovernance
  | PendingGovernanceTimestampUnavailable
  deriving stock (Show, Eq)

data GovernanceDecodedValue
  = GovernanceUint Integer
  | GovernanceAddress Text
  | GovernanceBool Bool
  deriving stock (Show, Eq)

data DecodedGovernanceField = DecodedGovernanceField
  { dgfDefinition :: GovernanceField
  , dgfValue :: GovernanceDecodedValue
  }
  deriving stock (Show, Eq)

data DecodedGovernanceEvent = DecodedGovernanceEvent
  { dgeCategory :: GovernanceCategory
  , dgeLifecycle :: GovernanceLifecycle
  , dgeFields :: [DecodedGovernanceField]
  }
  deriving stock (Show, Eq)

data GovernanceDecodeError
  = GovernanceDynamicPayloadUnavailable Text
  | GovernancePayloadLengthMismatch Int Int
  | GovernanceSchemaWordCountMismatch Int Int
  | GovernanceNonCanonicalAddress Text
  | GovernanceInvalidBool Text Integer
  | GovernanceUnsupportedStaticField Text
  | GovernanceUnknownEventTopic ByteString
  deriving stock (Show, Eq)

governanceCategoryDefinitions :: [GovernanceCategoryDefinition]
governanceCategoryDefinitions =
  [ routerConfigDefinition
  , oracleConfigDefinition
  , engineRiskConfigDefinition
  , engineCalendarConfigDefinition
  , engineFreshnessConfigDefinition
  , housePoolConfigDefinition
  ]

governanceCategoryDefinition :: GovernanceCategory -> GovernanceCategoryDefinition
governanceCategoryDefinition RouterConfigCategory = routerConfigDefinition
governanceCategoryDefinition OracleConfigCategory = oracleConfigDefinition
governanceCategoryDefinition EngineRiskConfigCategory = engineRiskConfigDefinition
governanceCategoryDefinition EngineCalendarConfigCategory = engineCalendarConfigDefinition
governanceCategoryDefinition EngineFreshnessConfigCategory = engineFreshnessConfigDefinition
governanceCategoryDefinition HousePoolConfigCategory = housePoolConfigDefinition

governanceContractAddress :: ProtocolRelease -> GovernanceContractRole -> Text
governanceContractAddress release OrderRouterAdminRole = prOrderRouterAdmin release
governanceContractAddress release CfdEngineAdminRole = prCfdEngineAdmin release
governanceContractAddress release HousePoolRole = prHousePool release
governanceContractAddress release OrderRouterRole = prOrderRouter release
governanceContractAddress release CfdEngineRole = prCfdEngine release
governanceContractAddress release PletherOracleRole = prPletherOracle release

governanceContractRoleKey :: GovernanceContractRole -> Text
governanceContractRoleKey = \case
  OrderRouterAdminRole -> "order_router_admin"
  CfdEngineAdminRole -> "cfd_engine_admin"
  HousePoolRole -> "house_pool"
  OrderRouterRole -> "order_router"
  CfdEngineRole -> "cfd_engine"
  PletherOracleRole -> "plether_oracle"

-- | Qualify a stable governance key with the host whose state it describes.
-- The same helper is used for confirmed current reads and projected history,
-- preventing owner/pauser changes on distinct contracts from collapsing onto
-- one public key.
qualifyGovernanceKey :: Text -> Text -> Text
qualifyGovernanceKey roleKey baseKey =
  case T.breakOn "." baseKey of
    (namespace, rest)
      | not (T.null rest) ->
          namespace <> "." <> roleKey <> rest
    _ -> "governance." <> roleKey <> "." <> baseKey

governanceRoleGetters :: GovernanceContractRole -> [GovernanceGetterDefinition]
governanceRoleGetters OrderRouterAdminRole =
  [ getter "governance.owner" RoleGetter "owner()" AddressField
  , getter "governance.pending_owner" RoleGetter "pendingOwner()" AddressField
  , getter "governance.pauser" RoleGetter "pauser()" AddressField
  , getter "governance.paused" StatusGetter "paused()" BoolField
  , getter "dependencies.order_router" BindingGetter "router()" AddressField
  , getter "governance.timelock_delay" PolicyGetter "TIMELOCK_DELAY()" Uint256Field
  ]
governanceRoleGetters CfdEngineAdminRole =
  [ getter "governance.owner" RoleGetter "owner()" AddressField
  , getter "governance.pending_owner" RoleGetter "pendingOwner()" AddressField
  , getter "dependencies.cfd_engine" BindingGetter "engine()" AddressField
  , getter "governance.timelock_delay" PolicyGetter "TIMELOCK_DELAY()" Uint256Field
  ]
governanceRoleGetters HousePoolRole =
  [ getter "governance.owner" RoleGetter "owner()" AddressField
  , getter "governance.pending_owner" RoleGetter "pendingOwner()" AddressField
  , getter "governance.pauser" RoleGetter "pauser()" AddressField
  , getter "governance.paused" StatusGetter "paused()" BoolField
  , getter "dependencies.senior_vault" BindingGetter "seniorVault()" AddressField
  , getter "dependencies.junior_vault" BindingGetter "juniorVault()" AddressField
  , getter "governance.timelock_delay" PolicyGetter "TIMELOCK_DELAY()" Uint256Field
  ]
governanceRoleGetters OrderRouterRole =
  [ getter "dependencies.plether_oracle" BindingGetter "pletherOracle()" AddressField
  ]
governanceRoleGetters CfdEngineRole =
  [ getter "governance.owner" RoleGetter "owner()" AddressField
  , getter "governance.pending_owner" RoleGetter "pendingOwner()" AddressField
  , getter "dependencies.protocol_treasury" BindingGetter "protocolTreasury()" AddressField
  ]
governanceRoleGetters _ = []

-- | Source-accurate mutability for current governance/dependency rows.
-- These values describe the getter itself, not the broader contract.
governanceGetterMutability :: GovernanceGetterDefinition -> Text
governanceGetterMutability definition
  | ggdKey definition
      `elem` [ "governance.timelock_delay"
             , "dependencies.order_router"
             , "dependencies.cfd_engine"
             ] =
      "immutable"
  | ggdKey definition
      `elem` [ "dependencies.senior_vault"
             , "dependencies.junior_vault"
             ] =
      "one_time_set"
  | ggdKey definition == "governance.paused" = "emergency_action"
  | ggdKey definition
      `elem` [ "governance.owner"
             , "governance.pending_owner"
             , "governance.pauser"
             ] =
      "role_action"
  | otherwise = "governance"

governanceGetterTimelockPolicy :: GovernanceGetterDefinition -> Text
governanceGetterTimelockPolicy definition
  | ggdKey definition
      `elem` [ "governance.timelock_delay"
             , "dependencies.order_router"
             , "dependencies.cfd_engine"
             ] =
      "not_applicable"
  | ggdKey definition
      `elem` [ "dependencies.senior_vault"
             , "dependencies.junior_vault"
             ] =
      "one_time_set_no_timelock"
  | ggdKey definition == "dependencies.plether_oracle" = "admin_timelock"
  | ggdKey definition == "dependencies.protocol_treasury" = "owner_action_no_timelock"
  | ggdKey definition
      `elem` [ "governance.owner"
             , "governance.pending_owner"
             ] =
      "ownable_two_step_no_timelock"
  | ggdKey definition == "governance.pauser" = "owner_action_no_timelock"
  | ggdKey definition == "governance.paused" = "emergency_action_no_timelock"
  | otherwise = "contract_specific"

governanceRoleEvents :: [GovernanceRoleEventDefinition]
governanceRoleEvents =
  [ roleEvent
      "governance.ownership_transfer_started"
      "OwnershipTransferStarted(address,address)"
      [OrderRouterAdminRole, CfdEngineAdminRole, HousePoolRole, CfdEngineRole]
  , roleEvent
      "governance.ownership_transferred"
      "OwnershipTransferred(address,address)"
      [OrderRouterAdminRole, CfdEngineAdminRole, HousePoolRole, CfdEngineRole]
  , roleEvent
      "governance.pauser_updated"
      "PauserUpdated(address,address)"
      [OrderRouterAdminRole, HousePoolRole]
  , roleEvent
      "governance.paused"
      "Paused(address)"
      [OrderRouterAdminRole, HousePoolRole]
  , roleEvent
      "governance.unpaused"
      "Unpaused(address)"
      [OrderRouterAdminRole, HousePoolRole]
  , roleEvent
      "governance.protocol_treasury_updated"
      "ProtocolTreasuryUpdated(address)"
      [CfdEngineRole]
  ]

-- | Interpret the activation slot without inspecting the pending struct.
-- Solidity leaves cancelled/finalized struct bytes behind, so an ETA of zero
-- is always authoritative proof that there is no pending proposal.
governancePendingState :: Maybe Integer -> Integer -> GovernancePendingState
governancePendingState _ 0 = NoPendingGovernance
governancePendingState Nothing _ = PendingGovernanceTimestampUnavailable
governancePendingState (Just confirmedTimestamp) eta
  | confirmedTimestamp < eta = PendingGovernance
  | otherwise = ReadyGovernance

-- | Decode a no-argument governance getter. Getter results are deliberately
-- held to one canonical ABI word: accepting trailing words here would make a
-- changed contract interface look like the expected release.
decodeGovernanceGetter ::
  GovernanceGetterDefinition ->
  ByteString ->
  Either GovernanceDecodeError GovernanceDecodedValue
decodeGovernanceGetter definition payload = do
  let field =
        GovernanceField
          { gfKey = ggdKey definition
          , gfType = ggdValueType definition
          , gfScale = getterScale definition
          , gfUnit = getterUnit definition
          }
  decoded <- decodePayload (StaticGovernanceWords 1) [field] payload
  case decoded of
    [value] -> Right $ dgfValue value
    _ -> Left $ GovernanceSchemaWordCountMismatch 1 (length decoded)

decodePendingGovernance ::
  GovernanceCategoryDefinition ->
  ByteString ->
  Either GovernanceDecodeError [DecodedGovernanceField]
decodePendingGovernance definition payload =
  decodePayload
    (gcdPendingEncoding definition)
    (gcdFields definition)
    payload

decodeGovernanceEvent ::
  GovernanceCategoryDefinition ->
  ByteString ->
  ByteString ->
  Either GovernanceDecodeError DecodedGovernanceEvent
decodeGovernanceEvent definition topic payload =
  case find ((== topic) . gedTopic) (gcdEvents definition) of
    Nothing -> Left (GovernanceUnknownEventTopic topic)
    Just eventDefinition -> do
      fields <-
        decodePayload
          (gedPayloadEncoding eventDefinition)
          (gedFields eventDefinition)
          payload
      pure
        DecodedGovernanceEvent
          { dgeCategory = gcdCategory definition
          , dgeLifecycle = gedLifecycle eventDefinition
          , dgeFields = fields
          }

routerConfigDefinition :: GovernanceCategoryDefinition
routerConfigDefinition =
  category
    RouterConfigCategory
    OrderRouterAdminRole
    "pendingRouterConfig()"
    (staticEncoding routerFields)
    "routerConfigActivationTime()"
    routerFields
    "proposeRouterConfig((uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256))"
    "finalizeRouterConfig()"
    "cancelRouterConfig()"
    [ staticEvent
        GovernanceProposed
        "RouterConfigProposed((uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256),uint256)"
        (routerFields <> [activationTimeField])
    , staticEvent
        GovernanceFinalized
        "RouterConfigFinalized((uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256))"
        routerFields
    , staticEvent GovernanceCancelled "RouterConfigCancelled()" []
    ]

oracleConfigDefinition :: GovernanceCategoryDefinition
oracleConfigDefinition =
  category
    OracleConfigCategory
    OrderRouterAdminRole
    "getPendingOracleConfig()"
    (staticEncoding oracleFields)
    "oracleConfigActivationTime()"
    oracleFields
    "proposeOracleConfig((address))"
    "finalizeOracleConfig()"
    "cancelOracleConfig()"
    [ staticEvent
        GovernanceProposed
        "OracleConfigProposed((address),uint256)"
        (oracleFields <> [activationTimeField])
    , staticEvent GovernanceFinalized "OracleConfigFinalized((address))" oracleFields
    , staticEvent GovernanceCancelled "OracleConfigCancelled()" []
    ]

engineRiskConfigDefinition :: GovernanceCategoryDefinition
engineRiskConfigDefinition =
  category
    EngineRiskConfigCategory
    CfdEngineAdminRole
    "pendingRiskConfig()"
    (staticEncoding engineRiskFields)
    "riskConfigActivationTime()"
    engineRiskFields
    "proposeRiskConfig(((uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256),uint256,uint256))"
    "finalizeRiskConfig()"
    "cancelRiskConfig()"
    [ staticEvent
        GovernanceProposed
        "RiskConfigProposed(((uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256),uint256,uint256),uint256)"
        (engineRiskFields <> [activationTimeField])
    , staticEvent
        GovernanceFinalized
        "RiskConfigFinalized(((uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256),uint256,uint256))"
        engineRiskFields
    , staticEvent GovernanceCancelled "RiskConfigCancelled()" []
    ]

engineCalendarConfigDefinition :: GovernanceCategoryDefinition
engineCalendarConfigDefinition =
  category
    EngineCalendarConfigCategory
    CfdEngineAdminRole
    "getPendingCalendarConfig()"
    calendarDynamicEncoding
    "calendarConfigActivationTime()"
    calendarFields
    "proposeCalendarConfig((uint256[],uint256))"
    "finalizeCalendarConfig()"
    "cancelCalendarConfig()"
    [ dynamicEvent
        GovernanceProposed
        "CalendarConfigProposed((uint256[],uint256),uint256)"
    , dynamicEvent
        GovernanceFinalized
        "CalendarConfigFinalized((uint256[],uint256))"
    , staticEvent GovernanceCancelled "CalendarConfigCancelled()" []
    ]

engineFreshnessConfigDefinition :: GovernanceCategoryDefinition
engineFreshnessConfigDefinition =
  category
    EngineFreshnessConfigCategory
    CfdEngineAdminRole
    "pendingFreshnessConfig()"
    (staticEncoding engineFreshnessFields)
    "freshnessConfigActivationTime()"
    engineFreshnessFields
    "proposeFreshnessConfig((uint256,uint256))"
    "finalizeFreshnessConfig()"
    "cancelFreshnessConfig()"
    [ staticEvent
        GovernanceProposed
        "FreshnessConfigProposed((uint256,uint256),uint256)"
        (engineFreshnessFields <> [activationTimeField])
    , staticEvent
        GovernanceFinalized
        "FreshnessConfigFinalized((uint256,uint256))"
        engineFreshnessFields
    , staticEvent GovernanceCancelled "FreshnessConfigCancelled()" []
    ]

housePoolConfigDefinition :: GovernanceCategoryDefinition
housePoolConfigDefinition =
  category
    HousePoolConfigCategory
    HousePoolRole
    "pendingPoolConfig()"
    (staticEncoding housePoolFields)
    "poolConfigActivationTime()"
    housePoolFields
    "proposePoolConfig((uint256,uint256,uint256,uint256))"
    "finalizePoolConfig()"
    "cancelPoolConfigProposal()"
    [ staticEvent
        GovernanceProposed
        "PoolConfigProposed(uint256,uint256,uint256,uint256,uint256)"
        (housePoolFields <> [activationTimeField])
    , staticEvent GovernanceFinalized "PoolConfigFinalized()" []
    ]

routerFields :: [GovernanceField]
routerFields =
  [ uintField "orders.max_order_age" "1" "seconds"
  , uintField "oracle.execution_staleness_limit" "1" "seconds"
  , uintField "oracle.liquidation_staleness_limit" "1" "seconds"
  , uintField "oracle.max_confidence_ratio_bps" "10000" "bps"
  , uintField "oracle.settlement_window" "1" "seconds"
  , uintField "oracle.max_component_publish_time_divergence" "1" "seconds"
  , uintField "oracle.adverse_confidence_multiplier_bps" "10000" "bps"
  , uintField "orders.minimum_open_notional" "1000000" "USDC"
  , uintField "keepers.open_order_execution_bounty_bps" "10000" "bps"
  , uintField "keepers.minimum_open_order_execution_bounty" "1000000" "USDC"
  , uintField "keepers.maximum_open_order_execution_bounty" "1000000" "USDC"
  , uintField "keepers.close_order_execution_bounty" "1000000" "USDC"
  , uintField "orders.max_pending_orders" "1" "orders"
  , uintField "orders.minimum_engine_gas" "1" "gas"
  , uintField "orders.max_prune_per_call" "1" "orders"
  ]

oracleFields :: [GovernanceField]
oracleFields =
  [ GovernanceField
      { gfKey = "dependencies.order_router.plether_oracle"
      , gfType = AddressField
      , gfScale = "address"
      , gfUnit = "address"
      }
  ]

engineRiskFields :: [GovernanceField]
engineRiskFields =
  [ uintField "market.vpi_factor" "1000000000000000000" "WAD factor"
  , uintField "market.max_skew_ratio" "1000000000000000000" "ratio"
  , uintField "liquidation.maintenance_margin_bps" "10000" "bps"
  , uintField "market.initial_margin_bps" "10000" "bps"
  , uintField "liquidation.fad_margin_bps" "10000" "bps"
  , uintField "fees.base_carry_bps" "10000" "bps"
  , uintField "liquidation.minimum_bounty_usdc" "1000000" "USDC"
  , uintField "liquidation.bounty_bps" "10000" "bps"
  , uintField "fees.execution_fee_bps" "10000" "bps"
  , uintField "market.frozen_close_vpi_factor" "1000000000000000000" "WAD factor"
  ]

calendarFields :: [GovernanceField]
calendarFields =
  [ GovernanceField
      { gfKey = "market.fad_day_timestamps"
      , gfType = Uint256ArrayField
      , gfScale = "1"
      , gfUnit = "unix_seconds"
      }
  , uintField "market.fad_runway_seconds" "1" "seconds"
  ]

engineFreshnessFields :: [GovernanceField]
engineFreshnessFields =
  [ uintField "oracle.frozen_mark_staleness_limit" "1" "seconds"
  , uintField "oracle.engine_mark_staleness_limit" "1" "seconds"
  ]

housePoolFields :: [GovernanceField]
housePoolFields =
  [ uintField "house_pool.senior_rate_bps" "10000" "bps"
  , uintField "house_pool.mark_staleness_limit" "1" "seconds"
  , uintField "house_pool.senior_frozen_lp_fee_bps" "10000" "bps"
  , uintField "house_pool.junior_frozen_lp_fee_bps" "10000" "bps"
  ]

activationTimeField :: GovernanceField
activationTimeField =
  uintField "governance.activation_time" "1" "unix_seconds"

calendarDynamicEncoding :: GovernancePayloadEncoding
calendarDynamicEncoding =
  UnsupportedDynamicGovernancePayload "calendar_dynamic_tuple_not_supported"

category ::
  GovernanceCategory ->
  GovernanceContractRole ->
  Text ->
  GovernancePayloadEncoding ->
  Text ->
  [GovernanceField] ->
  Text ->
  Text ->
  Text ->
  [GovernanceEventDefinition] ->
  GovernanceCategoryDefinition
category categoryKey contractRole pendingSignature pendingEncoding activationSignature fields proposal finalize cancel events =
  GovernanceCategoryDefinition
    { gcdCategory = categoryKey
    , gcdContractRole = contractRole
    , gcdPendingGetter = abiFunction pendingSignature
    , gcdPendingEncoding = pendingEncoding
    , gcdActivationGetter = abiFunction activationSignature
    , gcdFields = fields
    , gcdProposalCall = abiFunction proposal
    , gcdFinalizeCall = abiFunction finalize
    , gcdCancelCall = abiFunction cancel
    , gcdEvents = events
    }

staticEvent ::
  GovernanceLifecycle ->
  Text ->
  [GovernanceField] ->
  GovernanceEventDefinition
staticEvent lifecycle signature fields =
  GovernanceEventDefinition
    { gedLifecycle = lifecycle
    , gedSignature = signature
    , gedTopic = abiEventTopic signature
    , gedPayloadEncoding = staticEncoding fields
    , gedFields = fields
    }

dynamicEvent ::
  GovernanceLifecycle ->
  Text ->
  GovernanceEventDefinition
dynamicEvent lifecycle signature =
  GovernanceEventDefinition
    { gedLifecycle = lifecycle
    , gedSignature = signature
    , gedTopic = abiEventTopic signature
    , gedPayloadEncoding = calendarDynamicEncoding
    , gedFields = calendarFields
    }

abiFunction :: Text -> GovernanceFunction
abiFunction signature =
  GovernanceFunction
    { governanceFunctionSignature = signature
    , governanceFunctionSelector = selector signature
    }

abiEventTopic :: Text -> ByteString
abiEventTopic = keccak256 . TE.encodeUtf8

staticEncoding :: [GovernanceField] -> GovernancePayloadEncoding
staticEncoding = StaticGovernanceWords . length

uintField :: Text -> Text -> Text -> GovernanceField
uintField key scale unit =
  GovernanceField
    { gfKey = key
    , gfType = Uint256Field
    , gfScale = scale
    , gfUnit = unit
    }

getter ::
  Text ->
  GovernanceGetterPurpose ->
  Text ->
  GovernanceFieldType ->
  GovernanceGetterDefinition
getter key purpose signature valueType =
  GovernanceGetterDefinition
    { ggdKey = key
    , ggdPurpose = purpose
    , ggdFunction = abiFunction signature
    , ggdValueType = valueType
    }

getterScale :: GovernanceGetterDefinition -> Text
getterScale definition =
  case ggdValueType definition of
    AddressField -> "address"
    BoolField -> "boolean"
    Uint256ArrayField -> "1"
    Uint256Field -> "1"

getterUnit :: GovernanceGetterDefinition -> Text
getterUnit definition =
  case ggdValueType definition of
    AddressField -> "address"
    BoolField -> "boolean"
    Uint256ArrayField -> "values"
    Uint256Field
      | ggdKey definition == "governance.timelock_delay" -> "seconds"
      | otherwise -> "integer"

roleEvent ::
  Text ->
  Text ->
  [GovernanceContractRole] ->
  GovernanceRoleEventDefinition
roleEvent key signature roles =
  GovernanceRoleEventDefinition
    { gredKey = key
    , gredSignature = signature
    , gredTopic = abiEventTopic signature
    , gredContractRoles = roles
    }

decodePayload ::
  GovernancePayloadEncoding ->
  [GovernanceField] ->
  ByteString ->
  Either GovernanceDecodeError [DecodedGovernanceField]
decodePayload (UnsupportedDynamicGovernancePayload reason) _ _ =
  Left (GovernanceDynamicPayloadUnavailable reason)
decodePayload (StaticGovernanceWords expectedWords) fields payload
  | length fields /= expectedWords =
      Left (GovernanceSchemaWordCountMismatch expectedWords (length fields))
  | BS.length payload /= expectedBytes =
      Left (GovernancePayloadLengthMismatch expectedBytes (BS.length payload))
  | otherwise =
      traverse decodeField (zip fields (words32 payload))
  where
    expectedBytes = expectedWords * 32

decodeField ::
  (GovernanceField, ByteString) ->
  Either GovernanceDecodeError DecodedGovernanceField
decodeField (definition, word) = do
  value <-
    case gfType definition of
      Uint256Field ->
        Right (GovernanceUint (decodeUint256 word))
      AddressField
        | BS.take 12 word == BS.replicate 12 0 ->
            Right (GovernanceAddress (decodeAddress word))
        | otherwise ->
            Left (GovernanceNonCanonicalAddress (gfKey definition))
      BoolField ->
        case decodeUint256 word of
          0 -> Right (GovernanceBool (decodeBool word))
          1 -> Right (GovernanceBool (decodeBool word))
          value -> Left (GovernanceInvalidBool (gfKey definition) value)
      Uint256ArrayField ->
        Left (GovernanceUnsupportedStaticField (gfKey definition))
  pure
    DecodedGovernanceField
      { dgfDefinition = definition
      , dgfValue = value
      }

words32 :: ByteString -> [ByteString]
words32 payload
  | BS.null payload = []
  | otherwise =
      BS.take 32 payload : words32 (BS.drop 32 payload)
