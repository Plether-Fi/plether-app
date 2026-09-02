module Plether.Protocol.Parameters
  ( ParameterDefinition (..)
  , parameterCatalog
  , parameterDefinitionToJson
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

data ParameterDefinition = ParameterDefinition
  { pdKey :: Text
  , pdGroup :: Text
  , pdContract :: Text
  , pdGetter :: Text
  , pdWordIndex :: Int
  , pdRawScale :: Text
  , pdDisplayUnit :: Text
  , pdDescription :: Text
  , pdRiskInterpretation :: Text
  , pdMutability :: Text
  , pdTimelockPolicy :: Text
  , pdDocumentationLink :: Text
  }
  deriving stock (Show, Eq)

parameterCatalog :: [ParameterDefinition]
parameterCatalog =
  [ parameter "market.vpi_factor" "Market risk" "cfdEngine" "riskParams()" 0 "1e18" "WAD factor"
      "Scales the virtual price impact charged as one side becomes crowded."
      "Higher values increase the cost of adding skew and improve pool protection."
  , parameter "market.max_skew_ratio" "Market risk" "cfdEngine" "riskParams()" 1 "1e18" "ratio"
      "Maximum permitted directional skew relative to market capacity."
      "Higher values permit more concentrated directional exposure."
  , parameter "liquidation.maintenance_margin_bps" "Liquidation" "cfdEngine" "riskParams()" 2 "10000" "bps"
      "Minimum equity ratio required outside the FAD regime."
      "Higher values liquidate positions earlier and reduce pool tail risk."
  , parameter "market.initial_margin_bps" "Market risk" "cfdEngine" "riskParams()" 3 "10000" "bps"
      "Minimum initial margin for a newly opened or increased position."
      "Higher values reduce leverage and capital efficiency."
  , parameter "liquidation.fad_margin_bps" "Liquidation" "cfdEngine" "riskParams()" 4 "10000" "bps"
      "Margin requirement used during Friday-after-dark risk handling."
      "Higher values make the scheduled defensive regime more conservative."
  , parameter "fees.base_carry_bps" "Fees, VPI and carry" "cfdEngine" "riskParams()" 5 "10000" "bps"
      "Base annualized carry component used by position settlement."
      "Higher values transfer more value from traders to the pool over time."
  , parameter "liquidation.minimum_bounty_usdc" "Liquidation" "cfdEngine" "riskParams()" 6 "1e6" "USDC"
      "Minimum collateral bounty paid for a successful liquidation."
      "Higher values improve keeper incentives but consume more trader collateral."
  , parameter "liquidation.bounty_bps" "Liquidation" "cfdEngine" "riskParams()" 7 "10000" "bps"
      "Variable liquidation bounty charged against reachable collateral."
      "Higher values improve keeper incentives and increase liquidation cost."
  , parameter "fees.execution_fee_bps" "Fees, VPI and carry" "cfdEngine" "executionFeeBps()" 0 "10000" "bps"
      "Protocol fee assessed on successfully executed notional."
      "Higher values increase protocol revenue and trader execution cost."
  , parameter "orders.max_order_age" "Order queue" "orderRouter" "maxOrderAge()" 0 "1" "seconds"
      "Maximum time a committed order may remain pending before cleanup."
      "Higher values give orders longer to execute but can retain stale queue work."
  , parameter "orders.max_pending_orders" "Order queue" "orderRouter" "maxPendingOrders()" 0 "1" "orders"
      "Maximum pending commitments allowed for one trading account."
      "Higher values improve batching flexibility and increase queue/state load."
  , parameter "orders.minimum_engine_gas" "Order queue" "orderRouter" "minEngineGas()" 0 "1" "gas"
      "Minimum gas that must remain before the router enters the engine execution path."
      "Higher values reduce out-of-gas terminal failures but can reject otherwise executable keeper calls."
  , parameter "orders.max_prune_per_call" "Order queue" "orderRouter" "maxPruneOrdersPerCall()" 0 "1" "orders"
      "Maximum expired queue entries pruned during one permissionless maintenance call."
      "Higher values clear backlog faster while increasing worst-case transaction gas."
  , parameter "keepers.open_order_execution_bounty_bps" "Order queue" "orderRouter" "openOrderExecutionBountyBps()" 0 "10000" "bps"
      "Variable execution reward reserved from an opening order's committed margin."
      "Higher values strengthen execution incentives and increase trader execution cost."
  , parameter "keepers.minimum_open_order_execution_bounty" "Order queue" "orderRouter" "minOpenOrderExecutionBountyUsdc()" 0 "1e6" "USDC"
      "Minimum USDC reward reserved for executing an opening order."
      "Higher values improve keeper incentives but raise the minimum economical order cost."
  , parameter "keepers.maximum_open_order_execution_bounty" "Order queue" "orderRouter" "maxOpenOrderExecutionBountyUsdc()" 0 "1e6" "USDC"
      "Maximum USDC reward reserved for executing an opening order."
      "Higher values permit larger rewards on high-notional commitments."
  , parameter "keepers.close_order_execution_bounty" "Order queue" "orderRouter" "closeOrderExecutionBountyUsdc()" 0 "1e6" "USDC"
      "Fixed USDC reward reserved for executing a reduce or close order."
      "Higher values improve close execution incentives and reduce trader settlement."
  , parameter "orders.minimum_open_notional" "Order queue" "orderRouter" "minOpenNotionalUsdc()" 0 "1e6" "USDC"
      "Minimum notional accepted for an opening order."
      "Higher values reduce dust and exclude smaller traders."
  , parameter "oracle.settlement_window" "Oracle and market states" "pletherOracle" "orderSettlementWindow()" 0 "1" "seconds"
      "Window after commitment in which the first post-commit oracle tick may be revealed; eligibility begins at the next tick."
      "Higher values extend terminal-processing time without delaying the first eligible reveal."
  , parameter "oracle.execution_staleness_limit" "Oracle and market states" "pletherOracle" "orderExecutionStalenessLimit()" 0 "1" "seconds"
      "Maximum permitted oracle age for normal order execution."
      "Higher values improve liveness but accept older price evidence."
  , parameter "oracle.liquidation_staleness_limit" "Oracle and market states" "pletherOracle" "liquidationStalenessLimit()" 0 "1" "seconds"
      "Maximum oracle age accepted for liquidation."
      "Higher values improve liquidation liveness but accept older prices."
  , parameter "oracle.adverse_confidence_multiplier_bps" "Oracle and market states" "pletherOracle" "adverseConfidenceMultiplierBps()" 0 "10000" "bps"
      "Multiplier applied to Pyth confidence when choosing an adverse liquidation price."
      "Higher values make eligibility and settlement more conservative for traders."
  , parameter "oracle.max_confidence_ratio_bps" "Oracle and market states" "pletherOracle" "pythMaxConfidenceRatioBps()" 0 "10000" "bps"
      "Maximum accepted confidence interval relative to price."
      "Higher values tolerate less precise oracle updates."
  , parameter "oracle.max_component_publish_time_divergence" "Oracle and market states" "pletherOracle" "maxComponentPublishTimeDivergence()" 0 "1" "seconds"
      "Maximum timestamp spread permitted across the six Pyth components in one basket update."
      "Higher values improve liveness while accepting less synchronized component prices."
  , parameter "oracle.frozen_mark_staleness_limit" "Oracle and market states" "cfdEngine" "fadMaxStaleness()" 0 "1" "seconds"
      "Maximum cached mark age accepted for defensive frozen-market processing."
      "Higher values improve frozen-mode liveness while relying on older marks."
  , parameter "oracle.engine_mark_staleness_limit" "Oracle and market states" "cfdEngine" "engineMarkStalenessLimit()" 0 "1" "seconds"
      "Maximum mark age accepted by normal engine risk checks."
      "Higher values improve execution liveness while accepting older risk state."
  , parameter "market.fad_runway_seconds" "Oracle and market states" "cfdEngine" "fadRunwaySeconds()" 0 "1" "seconds"
      "Lead time before a scheduled FAD boundary during which defensive restrictions apply."
      "Higher values enter the defensive regime earlier."
  , parameter "market.frozen_close_vpi_factor" "Fees, VPI and carry" "cfdEngine" "frozenCloseVpiFactor()" 0 "1e18" "WAD factor"
      "One-way virtual price impact factor applied only to frozen-oracle reductions and closes."
      "Higher values increase the defensive price impact charged during frozen closes."
  , parameter "house_pool.senior_rate_bps" "HousePool and tranches" "housePool" "seniorRateBps()" 0 "10000" "bps"
      "Annualized coupon target accrued to Senior before residual revenue reaches Junior."
      "Higher values prioritize more protocol revenue to Senior and reduce Junior upside."
  , parameter "house_pool.mark_staleness_limit" "HousePool and tranches" "housePool" "markStalenessLimit()" 0 "1" "seconds"
      "Maximum mark age accepted for HousePool reconciliation and ordinary LP flows."
      "Higher values improve LP liveness while accepting older liability estimates."
  , parameter "house_pool.senior_frozen_lp_fee_bps" "HousePool and tranches" "housePool" "seniorFrozenLpFeeBps()" 0 "10000" "bps"
      "Surcharge applied to Senior LP flows while the oracle is frozen."
      "Higher values discourage frozen-state liquidity changes and increase their cost."
  , parameter "house_pool.junior_frozen_lp_fee_bps" "HousePool and tranches" "housePool" "juniorFrozenLpFeeBps()" 0 "10000" "bps"
      "Surcharge applied to Junior LP flows while the oracle is frozen."
      "Higher values discourage first-loss liquidity changes during uncertain pricing."
  , parameter "tranches.senior.deposit_cooldown" "HousePool and tranches" "seniorVault" "DEPOSIT_COOLDOWN()" 0 "1" "seconds"
      "Minimum time Senior shares must remain after deposit before withdrawal or transfer."
      "Higher values reduce rapid liquidity cycling and make LP capital less liquid."
  , parameter "tranches.junior.deposit_cooldown" "HousePool and tranches" "juniorVault" "DEPOSIT_COOLDOWN()" 0 "1" "seconds"
      "Minimum time Junior shares must remain after deposit before withdrawal or transfer."
      "Higher values reduce rapid first-loss liquidity cycling and make LP capital less liquid."
  , parameter "tranches.senior.deposit_epoch_duration" "HousePool and tranches" "seniorVault" "DEPOSIT_EPOCH_DURATION()" 0 "1" "seconds"
      "Duration of one Senior delayed-deposit pricing epoch."
      "Higher values reduce epoch frequency and increase deposit activation latency."
  , parameter "tranches.junior.deposit_epoch_duration" "HousePool and tranches" "juniorVault" "DEPOSIT_EPOCH_DURATION()" 0 "1" "seconds"
      "Duration of one Junior delayed-deposit pricing epoch."
      "Higher values reduce epoch frequency and increase deposit activation latency."
  , parameter "tranches.senior.deposit_activation_epoch_delay" "HousePool and tranches" "seniorVault" "DEPOSIT_ACTIVATION_EPOCH_DELAY()" 0 "1" "epochs"
      "Number of future epochs before a Senior delayed deposit becomes finalizable."
      "Higher values add more observation time before capital enters the waterfall."
  , parameter "tranches.junior.deposit_activation_epoch_delay" "HousePool and tranches" "juniorVault" "DEPOSIT_ACTIVATION_EPOCH_DELAY()" 0 "1" "epochs"
      "Number of future epochs before a Junior delayed deposit becomes finalizable."
      "Higher values add more observation time before first-loss capital enters the waterfall."
  , parameter "market.cap_price" "Immutable constants" "cfdEngine" "CAP_PRICE()" 0 "1e8" "index price"
      "Upper bound used by the capped index payoff."
      "Higher values expand the supported payoff range."
  , parameter "house_pool.maximum_frozen_lp_fee_bps" "Immutable constants" "housePool" "MAX_FROZEN_LP_FEE_BPS()" 0 "10000" "bps"
      "Hard upper bound accepted for either tranche's frozen-oracle LP surcharge."
      "Higher values permit governance to configure a larger defensive liquidity charge."
  , parameter "house_pool.minimum_tranche_deposit" "Immutable constants" "housePool" "MIN_TRANCHE_DEPOSIT_USDC()" 0 "1e6" "USDC"
      "Smallest tranche deposit accepted by the HousePool."
      "Higher values reduce dust while excluding smaller LP allocations."
  ]
  where
    parameter key groupName contract getter wordIndex rawScale displayUnit description risk =
      let isImmutable =
            groupName == "Immutable constants"
              || getter
                `elem` [ "DEPOSIT_COOLDOWN()"
                       , "DEPOSIT_EPOCH_DURATION()"
                       , "DEPOSIT_ACTIVATION_EPOCH_DELAY()"
                       ]
       in
      ParameterDefinition
        { pdKey = key
        , pdGroup = groupName
        , pdContract = contract
        , pdGetter = getter
        , pdWordIndex = wordIndex
        , pdRawScale = rawScale
        , pdDisplayUnit = displayUnit
        , pdDescription = description
        , pdRiskInterpretation = risk
        , pdMutability = if isImmutable then "immutable" else "governance"
        , pdTimelockPolicy = if isImmutable then "not_applicable" else "admin_timelock"
        , pdDocumentationLink = "/methodology#protocol-parameters"
        }

parameterDefinitionToJson :: ParameterDefinition -> Value
parameterDefinitionToJson ParameterDefinition {..} =
  object
    [ "key" .= pdKey
    , "group" .= pdGroup
    , "sourceContract" .= pdContract
    , "getter" .= pdGetter
    , "rawScale" .= pdRawScale
    , "displayUnit" .= pdDisplayUnit
    , "description" .= pdDescription
    , "riskInterpretation" .= pdRiskInterpretation
    , "mutability" .= pdMutability
    , "timelockPolicy" .= pdTimelockPolicy
    , "documentationLink" .= pdDocumentationLink
    ]
