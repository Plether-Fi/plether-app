const PENDING_ORDER_COMPONENTS = [
  { name: 'orderId', type: 'uint64' },
  { name: 'side', type: 'uint8' },
  { name: 'sizeDelta', type: 'uint256' },
  { name: 'marginDeltaUsdc', type: 'int256' },
  { name: 'acceptablePrice', type: 'uint256' },
  { name: 'isReduceOnly', type: 'bool' },
  { name: 'status', type: 'uint8' },
] as const

const ROUTER_PENDING_ORDER_COMPONENTS = [
  { name: 'orderId', type: 'uint64' },
  { name: 'isClose', type: 'bool' },
  { name: 'side', type: 'uint8' },
  { name: 'sizeDelta', type: 'uint256' },
  { name: 'marginDelta', type: 'uint256' },
  { name: 'targetPrice', type: 'uint256' },
  { name: 'commitTime', type: 'uint64' },
  { name: 'commitBlock', type: 'uint64' },
  { name: 'committedMarginUsdc', type: 'uint256' },
  { name: 'executionBountyUsdc', type: 'uint256' },
] as const

export const PERPS_EXECUTION_BOUNDS_COMPONENTS = [
  { name: 'validUntil', type: 'uint64' },
  { name: 'allowedExecutionModes', type: 'uint8' },
  { name: 'expectedConfigHash', type: 'bytes32' },
  { name: 'maxExecutionBountyUsdc', type: 'uint256' },
  { name: 'maxExecutionNotionalUsdc', type: 'uint256' },
  { name: 'maxGrossAccountDebitUsdc', type: 'uint256' },
  { name: 'maxActionChargeUsdc', type: 'uint256' },
  { name: 'maxExplicitFeesUsdc', type: 'uint256' },
  { name: 'maxPostPositionSize', type: 'uint256' },
  { name: 'minPostSettlementBalanceUsdc', type: 'uint256' },
  { name: 'minPostPositionEquityUsdc', type: 'uint256' },
  { name: 'maxPostLeverageBps', type: 'uint32' },
] as const

export const PERPS_ORDER_REQUEST_V2_COMPONENTS = [
  { name: 'clientOrderId', type: 'bytes32' },
  { name: 'side', type: 'uint8' },
  { name: 'sizeDelta', type: 'uint256' },
  { name: 'marginDelta', type: 'uint256' },
  { name: 'targetPrice', type: 'uint256' },
  { name: 'isClose', type: 'bool' },
  {
    name: 'bounds',
    type: 'tuple',
    components: PERPS_EXECUTION_BOUNDS_COMPONENTS,
  },
] as const

const CFD_ORDER_COMPONENTS = [
  { name: 'account', type: 'address' },
  { name: 'sizeDelta', type: 'uint256' },
  { name: 'marginDelta', type: 'uint256' },
  { name: 'targetPrice', type: 'uint256' },
  { name: 'commitTime', type: 'uint64' },
  { name: 'commitBlock', type: 'uint64' },
  { name: 'orderId', type: 'uint64' },
  { name: 'side', type: 'uint8' },
  { name: 'isClose', type: 'bool' },
] as const

const EXECUTION_ASSESSMENT_COMPONENTS = [
  { name: 'mode', type: 'uint8' },
  { name: 'executionNotionalUsdc', type: 'uint256' },
  { name: 'grossAccountDebitUsdc', type: 'uint256' },
  { name: 'actionChargeAssessedUsdc', type: 'uint256' },
  { name: 'actionChargeCollectedUsdc', type: 'uint256' },
  { name: 'explicitFeesUsdc', type: 'uint256' },
  { name: 'preSettlementBalanceUsdc', type: 'uint256' },
  { name: 'postSettlementBalanceUsdc', type: 'uint256' },
  { name: 'realizedPnlUsdc', type: 'int256' },
  { name: 'vpiUsdc', type: 'int256' },
  { name: 'carryUsdc', type: 'uint256' },
  { name: 'executionFeeUsdc', type: 'uint256' },
  { name: 'frozenSpreadUsdc', type: 'uint256' },
  { name: 'preTraderClaimUsdc', type: 'uint256' },
  { name: 'postTraderClaimUsdc', type: 'uint256' },
  { name: 'postPositionSize', type: 'uint256' },
  { name: 'postPositionMarginUsdc', type: 'uint256' },
  { name: 'postPositionEquityUsdc', type: 'int256' },
  { name: 'postLeverageBps', type: 'uint256' },
] as const

const FAILURE_DETAILS_COMPONENTS = [
  { name: 'selector', type: 'bytes4' },
  { name: 'category', type: 'uint8' },
  { name: 'code', type: 'uint8' },
  { name: 'constraint', type: 'uint8' },
  { name: 'actual', type: 'uint256' },
  { name: 'limit', type: 'uint256' },
  { name: 'revertDataHash', type: 'bytes32' },
] as const

const ORDER_ECONOMICS_COMPONENTS = [
  { name: 'executionNotionalUsdc', type: 'uint256' },
  { name: 'realizedPnlUsdc', type: 'int256' },
  { name: 'vpiUsdc', type: 'int256' },
  { name: 'carryUsdc', type: 'int256' },
  { name: 'executionFeeUsdc', type: 'uint256' },
  { name: 'frozenSpreadUsdc', type: 'uint256' },
  { name: 'actionChargeAssessedUsdc', type: 'uint256' },
  { name: 'actionChargeCollectedUsdc', type: 'uint256' },
  { name: 'grossAccountDebitUsdc', type: 'uint256' },
  { name: 'preSettlementBalanceUsdc', type: 'uint256' },
  { name: 'postSettlementBalanceUsdc', type: 'uint256' },
  { name: 'preTraderClaimBalanceUsdc', type: 'uint256' },
  { name: 'postTraderClaimBalanceUsdc', type: 'uint256' },
  { name: 'postPositionSize', type: 'uint256' },
  { name: 'postPositionMarginUsdc', type: 'uint256' },
  { name: 'postPositionEquityUsdc', type: 'int256' },
  { name: 'postLeverageBps', type: 'uint256' },
] as const

const ORDER_RECEIPT_COMPONENTS = [
  { name: 'orderId', type: 'uint64' },
  { name: 'account', type: 'address' },
  { name: 'clientOrderId', type: 'bytes32' },
  { name: 'intentHash', type: 'bytes32' },
  { name: 'expectedConfigHash', type: 'bytes32' },
  { name: 'observedConfigHash', type: 'bytes32' },
  { name: 'status', type: 'uint8' },
  { name: 'reason', type: 'uint8' },
  { name: 'executionMode', type: 'uint8' },
  { name: 'executor', type: 'address' },
  { name: 'priceSource', type: 'uint8' },
  { name: 'executionPrice', type: 'uint256' },
  { name: 'neutralMarkPrice', type: 'uint256' },
  { name: 'poolDepthUsdc', type: 'uint256' },
  { name: 'oraclePublishTime', type: 'uint64' },
  { name: 'priceReachedEngine', type: 'bool' },
  { name: 'bountyUsdc', type: 'uint256' },
  { name: 'bountyRecipient', type: 'address' },
  { name: 'bountyDisposition', type: 'uint8' },
  {
    name: 'failure',
    type: 'tuple',
    components: FAILURE_DETAILS_COMPONENTS,
  },
  {
    name: 'economics',
    type: 'tuple',
    components: ORDER_ECONOMICS_COMPONENTS,
  },
] as const

const PRICE_SNAPSHOT_COMPONENTS = [
  { name: 'price', type: 'uint256' },
  { name: 'markPrice', type: 'uint256' },
  { name: 'publishTime', type: 'uint64' },
  { name: 'updateFee', type: 'uint256' },
  { name: 'maxStaleness', type: 'uint256' },
  { name: 'closeOnly', type: 'bool' },
  { name: 'oracleFrozen', type: 'bool' },
  { name: 'isFadWindow', type: 'bool' },
] as const

const POLICY_SNAPSHOT_COMPONENTS = [
  { name: 'closeOnly', type: 'bool' },
  { name: 'requireStoredMark', type: 'bool' },
  { name: 'allowAnyStoredMark', type: 'bool' },
  { name: 'maxStaleness', type: 'uint256' },
  { name: 'oracleFrozen', type: 'bool' },
  { name: 'isFadWindow', type: 'bool' },
] as const

const CLOSE_PREVIEW_COMPONENTS = [
  { name: 'valid', type: 'bool' },
  { name: 'invalidReason', type: 'uint8' },
  { name: 'executionPrice', type: 'uint256' },
  { name: 'sizeDelta', type: 'uint256' },
  { name: 'realizedPnlUsdc', type: 'int256' },
  { name: 'vpiDeltaUsdc', type: 'int256' },
  { name: 'vpiUsdc', type: 'uint256' },
  { name: 'executionFeeUsdc', type: 'uint256' },
  { name: 'freshTraderPayoutUsdc', type: 'uint256' },
  { name: 'existingTraderClaimConsumedUsdc', type: 'uint256' },
  { name: 'existingTraderClaimRemainingUsdc', type: 'uint256' },
  { name: 'immediatePayoutUsdc', type: 'uint256' },
  { name: 'traderClaimBalanceUsdc', type: 'uint256' },
  { name: 'seizedCollateralUsdc', type: 'uint256' },
  { name: 'badDebtUsdc', type: 'uint256' },
  { name: 'remainingSize', type: 'uint256' },
  { name: 'remainingMargin', type: 'uint256' },
  { name: 'triggersDegradedMode', type: 'bool' },
  { name: 'postOpDegradedMode', type: 'bool' },
  { name: 'effectiveAssetsAfterUsdc', type: 'uint256' },
  { name: 'maxLiabilityAfterUsdc', type: 'uint256' },
  { name: 'frozenSpreadUsdc', type: 'uint256' },
  { name: 'frozenSpreadPaidUsdc', type: 'uint256' },
  { name: 'frozenSpreadWaivedUsdc', type: 'uint256' },
] as const

const OPEN_PREVIEW_COMPONENTS = [
  { name: 'valid', type: 'bool' },
  { name: 'invalidReason', type: 'uint8' },
  { name: 'failureCategory', type: 'uint8' },
  { name: 'executionPrice', type: 'uint256' },
  { name: 'sizeDelta', type: 'uint256' },
  { name: 'notionalUsdc', type: 'uint256' },
  { name: 'marginDeltaUsdc', type: 'uint256' },
  { name: 'vpiUsdc', type: 'int256' },
  { name: 'executionFeeUsdc', type: 'uint256' },
  { name: 'tradeCostUsdc', type: 'int256' },
  { name: 'poolRebatePayoutUsdc', type: 'uint256' },
  { name: 'pendingCarryUsdc', type: 'uint256' },
  { name: 'initialMarginRequirementUsdc', type: 'uint256' },
  { name: 'maintenanceMarginUsdc', type: 'uint256' },
  { name: 'postSize', type: 'uint256' },
  { name: 'postMarginUsdc', type: 'uint256' },
  { name: 'postEntryPrice', type: 'uint256' },
  { name: 'postVpiAccrued', type: 'int256' },
  { name: 'postUnrealizedPnlUsdc', type: 'int256' },
  { name: 'postEquityUsdc', type: 'int256' },
  { name: 'postHealthBps', type: 'uint256' },
  { name: 'postLiquidatable', type: 'bool' },
  { name: 'hasLiquidationPrice', type: 'bool' },
  { name: 'liquidationPrice', type: 'uint256' },
] as const

const LIQUIDATION_PREVIEW_COMPONENTS = [
  { name: 'liquidatable', type: 'bool' },
  { name: 'oraclePrice', type: 'uint256' },
  { name: 'equityUsdc', type: 'int256' },
  { name: 'pnlUsdc', type: 'int256' },
  { name: 'reachableCollateralUsdc', type: 'uint256' },
  { name: 'liquidationChargeUsdc', type: 'uint256' },
  { name: 'keeperBountyUsdc', type: 'uint256' },
  { name: 'protocolLiquidationFeeUsdc', type: 'uint256' },
  { name: 'lpLiquidationFeeUsdc', type: 'uint256' },
  { name: 'seizedCollateralUsdc', type: 'uint256' },
  { name: 'settlementRetainedUsdc', type: 'uint256' },
  { name: 'freshTraderPayoutUsdc', type: 'uint256' },
  { name: 'existingTraderClaimConsumedUsdc', type: 'uint256' },
  { name: 'existingTraderClaimRemainingUsdc', type: 'uint256' },
  { name: 'immediatePayoutUsdc', type: 'uint256' },
  { name: 'traderClaimBalanceUsdc', type: 'uint256' },
  { name: 'badDebtUsdc', type: 'uint256' },
  { name: 'triggersDegradedMode', type: 'bool' },
  { name: 'postOpDegradedMode', type: 'bool' },
  { name: 'effectiveAssetsAfterUsdc', type: 'uint256' },
  { name: 'maxLiabilityAfterUsdc', type: 'uint256' },
] as const

const ACCOUNT_COLLATERAL_VIEW_COMPONENTS = [
  { name: 'settlementBalanceUsdc', type: 'uint256' },
  { name: 'lockedMarginUsdc', type: 'uint256' },
  { name: 'activePositionMarginUsdc', type: 'uint256' },
  { name: 'otherLockedMarginUsdc', type: 'uint256' },
  { name: 'freeSettlementUsdc', type: 'uint256' },
  { name: 'closeReachableUsdc', type: 'uint256' },
  { name: 'liquidationReachableSettlementUsdc', type: 'uint256' },
  { name: 'terminalPriceCollectibleCapUsdc', type: 'uint256' },
  { name: 'accountEquityUsdc', type: 'uint256' },
  { name: 'freeBuyingPowerUsdc', type: 'uint256' },
  { name: 'traderClaimBalanceUsdc', type: 'uint256' },
] as const

const ACCOUNT_LEDGER_VIEW_COMPONENTS = [
  { name: 'settlementBalanceUsdc', type: 'uint256' },
  { name: 'freeSettlementUsdc', type: 'uint256' },
  { name: 'activePositionMarginUsdc', type: 'uint256' },
  { name: 'otherLockedMarginUsdc', type: 'uint256' },
  { name: 'executionBountyReserveUsdc', type: 'uint256' },
  { name: 'committedMarginUsdc', type: 'uint256' },
  { name: 'traderClaimBalanceUsdc', type: 'uint256' },
  { name: 'pendingOrderCount', type: 'uint256' },
] as const

const ACCOUNT_LEDGER_SNAPSHOT_COMPONENTS = [
  { name: 'settlementBalanceUsdc', type: 'uint256' },
  { name: 'freeSettlementUsdc', type: 'uint256' },
  { name: 'activePositionMarginUsdc', type: 'uint256' },
  { name: 'otherLockedMarginUsdc', type: 'uint256' },
  { name: 'positionMarginBucketUsdc', type: 'uint256' },
  { name: 'committedOrderMarginBucketUsdc', type: 'uint256' },
  { name: 'reservedSettlementBucketUsdc', type: 'uint256' },
  { name: 'executionBountyReserveUsdc', type: 'uint256' },
  { name: 'committedMarginUsdc', type: 'uint256' },
  { name: 'traderClaimBalanceUsdc', type: 'uint256' },
  { name: 'pendingOrderCount', type: 'uint256' },
  { name: 'closeReachableUsdc', type: 'uint256' },
  { name: 'liquidationReachableSettlementUsdc', type: 'uint256' },
  { name: 'terminalPriceCollectibleCapUsdc', type: 'uint256' },
  { name: 'accountEquityUsdc', type: 'uint256' },
  { name: 'freeBuyingPowerUsdc', type: 'uint256' },
  { name: 'hasPosition', type: 'bool' },
  { name: 'side', type: 'uint8' },
  { name: 'size', type: 'uint256' },
  { name: 'margin', type: 'uint256' },
  { name: 'entryPrice', type: 'uint256' },
  { name: 'unrealizedPnlUsdc', type: 'int256' },
  { name: 'netEquityUsdc', type: 'int256' },
  { name: 'liquidatable', type: 'bool' },
] as const

const TRANCHE_VIEW_COMPONENTS = [
  { name: 'totalAssetsUsdc', type: 'uint256' },
  { name: 'totalShares', type: 'uint256' },
  { name: 'effectiveTotalShares', type: 'uint256' },
  { name: 'pendingMaintenanceFeeShares', type: 'uint256' },
  { name: 'maintenanceFeeAprBps', type: 'uint256' },
  { name: 'maintenanceFeeRecipient', type: 'address' },
  { name: 'sharePrice', type: 'uint256' },
  { name: 'maxWithdrawUsdc', type: 'uint256' },
  { name: 'frozenLpFeeBps', type: 'uint256' },
  { name: 'depositEnabled', type: 'bool' },
  { name: 'withdrawEnabled', type: 'bool' },
  { name: 'oracleFrozen', type: 'bool' },
] as const

const TRANCHE_QUEUE_VIEW_COMPONENTS = [
  { name: 'vault', type: 'address' },
  { name: 'currentEpoch', type: 'uint256' },
  { name: 'cutoffEpoch', type: 'uint256' },
  { name: 'nextRequestEpoch', type: 'uint256' },
  { name: 'nextRequestCutoffTime', type: 'uint256' },
  { name: 'depositHeadEpoch', type: 'uint256' },
  { name: 'depositHeadAssets', type: 'uint256' },
  { name: 'redeemHeadEpoch', type: 'uint256' },
  { name: 'redeemHeadShares', type: 'uint256' },
  { name: 'depositBacklog', type: 'bool' },
  { name: 'redeemBacklog', type: 'bool' },
  { name: 'settlementLive', type: 'bool' },
  { name: 'poolPaused', type: 'bool' },
  { name: 'lpEpochSettlementPaused', type: 'bool' },
] as const

const LP_STATUS_VIEW_COMPONENTS = [
  { name: 'tradingActive', type: 'bool' },
  { name: 'withdrawalLive', type: 'bool' },
  { name: 'lastMarkTime', type: 'uint64' },
  { name: 'oracleFresh', type: 'bool' },
  { name: 'oracleFrozen', type: 'bool' },
  { name: 'lpEpochSettlementPaused', type: 'bool' },
] as const

const LP_REQUEST_STATE_VIEW_COMPONENTS = [
  { name: 'vault', type: 'address' },
  { name: 'requestId', type: 'uint256' },
  { name: 'controller', type: 'address' },
  { name: 'pendingDepositAssets', type: 'uint256' },
  { name: 'pendingDepositSharesEstimate', type: 'uint256' },
  { name: 'claimableDepositAssets', type: 'uint256' },
  { name: 'claimableDepositShares', type: 'uint256' },
  { name: 'pendingRedeemShares', type: 'uint256' },
  { name: 'pendingRedeemAssetsEstimate', type: 'uint256' },
  { name: 'claimableRedeemShares', type: 'uint256' },
  { name: 'claimableRedeemAssets', type: 'uint256' },
  { name: 'refundableDepositAssets', type: 'uint256' },
  { name: 'refundableRedeemShares', type: 'uint256' },
  { name: 'redeemRefundPending', type: 'bool' },
] as const

const LP_DEPOSIT_COOLDOWN_STATE_VIEW_COMPONENTS = [
  { name: 'vault', type: 'address' },
  { name: 'requestId', type: 'uint256' },
  { name: 'controller', type: 'address' },
  { name: 'activationTime', type: 'uint256' },
  { name: 'cooldownEnd', type: 'uint256' },
  { name: 'remainingClaimableShares', type: 'uint256' },
  { name: 'directRedeemableShares', type: 'uint256' },
] as const

const POSITION_PROTECTION_VIEW_COMPONENTS = [
  { name: 'protectionId', type: 'uint64' },
  { name: 'parentOrderId', type: 'uint64' },
  { name: 'linkedOrderId', type: 'uint64' },
  { name: 'account', type: 'address' },
  { name: 'side', type: 'uint8' },
  { name: 'size', type: 'uint256' },
  { name: 'takeProfitTriggerPrice', type: 'uint256' },
  { name: 'stopLossTriggerPrice', type: 'uint256' },
  { name: 'triggerBountyUsdc', type: 'uint256' },
  { name: 'executionBountyUsdc', type: 'uint256' },
  { name: 'armedAt', type: 'uint64' },
  { name: 'armedBlock', type: 'uint64' },
  { name: 'triggerMarkPrice', type: 'uint256' },
  { name: 'triggerPublishTime', type: 'uint64' },
  { name: 'triggeredLeg', type: 'uint8' },
  { name: 'status', type: 'uint8' },
] as const

export const PERPS_PUBLIC_LENS_ABI = [
  ...(['ENGINE', 'ORDER_ROUTER', 'HOUSE_POOL'] as const).map((name) => ({
    type: 'function' as const,
    name,
    stateMutability: 'view' as const,
    inputs: [],
    outputs: [{ type: 'address' as const }],
  })),
  {
    type: 'function',
    name: 'getProtocolStatus',
    stateMutability: 'view',
    inputs: [],
    outputs: [
      {
        name: 'viewData',
        type: 'tuple',
        components: [
          { name: 'phase', type: 'uint8' },
          { name: 'lastMarkPrice', type: 'uint256' },
          { name: 'lastMarkTime', type: 'uint64' },
          { name: 'oracleFrozen', type: 'bool' },
          { name: 'fadWindow', type: 'bool' },
          { name: 'tradingActive', type: 'bool' },
          { name: 'withdrawalLive', type: 'bool' },
          { name: 'lpEpochSettlementPaused', type: 'bool' },
        ],
      },
    ],
  },
  {
    type: 'function',
    name: 'getLpStatus',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'viewData', type: 'tuple', components: LP_STATUS_VIEW_COMPONENTS }],
  },
  {
    type: 'function',
    name: 'getTraderAccount',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [
      {
        name: 'viewData',
        type: 'tuple',
        components: [
          { name: 'equityUsdc', type: 'uint256' },
          { name: 'withdrawableUsdc', type: 'uint256' },
          { name: 'pendingOrderMarginUsdc', type: 'uint256' },
          { name: 'pendingExecutionBountyUsdc', type: 'uint256' },
          { name: 'hasOpenPosition', type: 'bool' },
          { name: 'liquidatable', type: 'bool' },
        ],
      },
    ],
  },
  {
    type: 'function',
    name: 'getPosition',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [
      {
        name: 'viewData',
        type: 'tuple',
        components: [
          { name: 'exists', type: 'bool' },
          { name: 'side', type: 'uint8' },
          { name: 'size', type: 'uint256' },
          { name: 'entryPrice', type: 'uint256' },
          { name: 'marginUsdc', type: 'uint256' },
          { name: 'unrealizedPnlUsdc', type: 'int256' },
          { name: 'maintenanceMarginUsdc', type: 'uint256' },
          { name: 'liquidatable', type: 'bool' },
        ],
      },
    ],
  },
  {
    type: 'function',
    name: 'getPendingOrders',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [
      {
        name: 'pending',
        type: 'tuple[]',
        components: PENDING_ORDER_COMPONENTS,
      },
    ],
  },
  {
    type: 'function',
    name: 'getActivePositionProtection',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [
      {
        name: 'protection',
        type: 'tuple',
        components: POSITION_PROTECTION_VIEW_COMPONENTS,
      },
    ],
  },
  {
    type: 'function',
    name: 'isLiquidatable',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [{ type: 'bool' }],
  },
  {
    type: 'function',
    name: 'getSeniorTranche',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'viewData', type: 'tuple', components: TRANCHE_VIEW_COMPONENTS }],
  },
  {
    type: 'function',
    name: 'getJuniorTranche',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'viewData', type: 'tuple', components: TRANCHE_VIEW_COMPONENTS }],
  },
  {
    type: 'function',
    name: 'getTrancheQueues',
    stateMutability: 'view',
    inputs: [{ name: 'isSenior', type: 'bool' }],
    outputs: [{ name: 'viewData', type: 'tuple', components: TRANCHE_QUEUE_VIEW_COMPONENTS }],
  },
  {
    type: 'function',
    name: 'getLpRequestState',
    stateMutability: 'view',
    inputs: [
      { name: 'isSenior', type: 'bool' },
      { name: 'requestId', type: 'uint256' },
      { name: 'controller', type: 'address' },
    ],
    outputs: [{ name: 'viewData', type: 'tuple', components: LP_REQUEST_STATE_VIEW_COMPONENTS }],
  },
  {
    type: 'function',
    name: 'getLpDepositCooldownState',
    stateMutability: 'view',
    inputs: [
      { name: 'isSenior', type: 'bool' },
      { name: 'requestId', type: 'uint256' },
      { name: 'controller', type: 'address' },
    ],
    outputs: [{ name: 'viewData', type: 'tuple', components: LP_DEPOSIT_COOLDOWN_STATE_VIEW_COMPONENTS }],
  },
] as const

export const PERPS_MARGIN_CLEARINGHOUSE_ABI = [
  {
    type: 'function',
    name: 'depositMargin',
    stateMutability: 'nonpayable',
    inputs: [{ name: 'amount', type: 'uint256' }],
    outputs: [],
  },
  {
    type: 'function',
    name: 'withdrawMargin',
    stateMutability: 'nonpayable',
    inputs: [{ name: 'amount', type: 'uint256' }],
    outputs: [],
  },
  {
    type: 'function',
    name: 'getAccountEquityUsdc',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [{ type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'getFreeBuyingPowerUsdc',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [{ type: 'uint256' }],
  },
  {
    type: 'event',
    name: 'Deposit',
    inputs: [
      { name: 'account', type: 'address', indexed: true },
      { name: 'asset', type: 'address', indexed: true },
      { name: 'amount', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'Withdraw',
    inputs: [
      { name: 'account', type: 'address', indexed: true },
      { name: 'asset', type: 'address', indexed: true },
      { name: 'amount', type: 'uint256', indexed: false },
    ],
  },
] as const

export const PERPS_ORDER_ROUTER_ABI = [
  {
    type: 'function',
    name: 'commitOrder',
    stateMutability: 'nonpayable',
    inputs: [
      {
        name: 'request',
        type: 'tuple',
        components: PERPS_ORDER_REQUEST_V2_COMPONENTS,
      },
    ],
    outputs: [{ name: 'orderId', type: 'uint64' }],
  },
  {
    type: 'function',
    name: 'getPendingOrderView',
    stateMutability: 'view',
    inputs: [{ name: 'orderId', type: 'uint64' }],
    outputs: [
      {
        name: 'pending',
        type: 'tuple',
        components: ROUTER_PENDING_ORDER_COMPONENTS,
      },
      { name: 'nextAccountOrderId', type: 'uint64' },
    ],
  },
  {
    type: 'function',
    name: 'maxOrderAge',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'ageSeconds', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'maxPendingOrders',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'limit', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'minOpenNotionalUsdc',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'amount', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'openOrderExecutionBountyBps',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'bps', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'minOpenOrderExecutionBountyUsdc',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'amount', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'maxOpenOrderExecutionBountyUsdc',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'amount', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'closeOrderExecutionBountyUsdc',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'amount', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'engine',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'engineAddress', type: 'address' }],
  },
  {
    type: 'function',
    name: 'lifecycleBook',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'book', type: 'address' }],
  },
  {
    type: 'function',
    name: 'policyEvaluator',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'evaluator', type: 'address' }],
  },
  {
    type: 'function',
    name: 'positionProtectionBook',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'book', type: 'address' }],
  },
  {
    type: 'function',
    name: 'executeOrder',
    stateMutability: 'payable',
    inputs: [
      { name: 'orderId', type: 'uint64' },
      { name: 'pythUpdateData', type: 'bytes[]' },
    ],
    outputs: [
      {
        name: 'result',
        type: 'tuple',
        components: [
          { name: 'orderId', type: 'uint64' },
          { name: 'status', type: 'uint8' },
          { name: 'terminalReason', type: 'uint8' },
          { name: 'pendingReason', type: 'uint8' },
          { name: 'receiptHash', type: 'bytes32' },
        ],
      },
    ],
  },
  {
    type: 'function',
    name: 'executeOrderBatch',
    stateMutability: 'payable',
    inputs: [
      { name: 'maxOrderId', type: 'uint64' },
      { name: 'pythUpdateData', type: 'bytes[]' },
    ],
    outputs: [
      {
        name: 'result',
        type: 'tuple',
        components: [
          { name: 'nextOrderId', type: 'uint64' },
          { name: 'terminalCount', type: 'uint32' },
          { name: 'stopReason', type: 'uint8' },
        ],
      },
    ],
  },
  {
    type: 'function',
    name: 'executeLiquidation',
    stateMutability: 'payable',
    inputs: [
      { name: 'account', type: 'address' },
      { name: 'pythUpdateData', type: 'bytes[]' },
    ],
    outputs: [],
  },
  {
    type: 'function',
    name: 'updateMarkPrice',
    stateMutability: 'payable',
    inputs: [{ name: 'pythUpdateData', type: 'bytes[]' }],
    outputs: [],
  },
  {
    type: 'event',
    name: 'OrderCommitted',
    inputs: [
      { name: 'orderId', type: 'uint64', indexed: true },
      { name: 'account', type: 'address', indexed: true },
      { name: 'side', type: 'uint8', indexed: false },
    ],
  },
  {
    type: 'error',
    name: 'OrderRouter__ZeroClientOrderId',
    inputs: [],
  },
  {
    type: 'error',
    name: 'OrderRouter__InvalidValidUntil',
    inputs: [],
  },
  {
    type: 'error',
    name: 'OrderRouter__InvalidExecutionModeMask',
    inputs: [],
  },
  {
    type: 'error',
    name: 'OrderRouter__ExecutionConfigMismatch',
    inputs: [
      { name: 'expectedConfigHash', type: 'bytes32' },
      { name: 'observedConfigHash', type: 'bytes32' },
    ],
  },
  {
    type: 'error',
    name: 'OrderRouter__ProtectionActive',
    inputs: [],
  },
  {
    type: 'error',
    name: 'OrderRouter__ZeroPostLeverageBound',
    inputs: [],
  },
  {
    type: 'error',
    name: 'OrderRouter__ExecutionBountyAboveGrossDebit',
    inputs: [
      { name: 'executionBountyUsdc', type: 'uint256' },
      { name: 'maxGrossAccountDebitUsdc', type: 'uint256' },
    ],
  },
] as const

export const PERPS_ORDER_LIFECYCLE_BOOK_ABI = [
  ...(['ROUTER', 'ENGINE', 'CLEARINGHOUSE', 'HOUSE_POOL'] as const).map(
    (name) => ({
      type: 'function' as const,
      name,
      stateMutability: 'view' as const,
      inputs: [],
      outputs: [{ type: 'address' as const }],
    })
  ),
  {
    type: 'function',
    name: 'currentExecutionConfigHash',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'configHash', type: 'bytes32' }],
  },
  {
    type: 'function',
    name: 'resolveClientIntent',
    stateMutability: 'view',
    inputs: [
      { name: 'account', type: 'address' },
      {
        name: 'request',
        type: 'tuple',
        components: PERPS_ORDER_REQUEST_V2_COMPONENTS,
      },
    ],
    outputs: [
      { name: 'resolution', type: 'uint8' },
      { name: 'orderId', type: 'uint64' },
      { name: 'intentHash', type: 'bytes32' },
    ],
  },
  {
    type: 'function',
    name: 'clientIntent',
    stateMutability: 'view',
    inputs: [
      { name: 'account', type: 'address' },
      { name: 'clientOrderId', type: 'bytes32' },
    ],
    outputs: [
      {
        name: 'intent',
        type: 'tuple',
        components: [
          { name: 'orderId', type: 'uint64' },
          { name: 'intentHash', type: 'bytes32' },
        ],
      },
    ],
  },
  {
    type: 'function',
    name: 'pendingPolicy',
    stateMutability: 'view',
    inputs: [{ name: 'orderId', type: 'uint64' }],
    outputs: [
      {
        name: 'bounds',
        type: 'tuple',
        components: PERPS_EXECUTION_BOUNDS_COMPONENTS,
      },
    ],
  },
  {
    type: 'function',
    name: 'lifecycleStatus',
    stateMutability: 'view',
    inputs: [{ name: 'orderId', type: 'uint64' }],
    outputs: [{ name: 'status', type: 'uint8' }],
  },
  {
    type: 'function',
    name: 'outcome',
    stateMutability: 'view',
    inputs: [{ name: 'orderId', type: 'uint64' }],
    outputs: [
      {
        name: 'terminalOutcome',
        type: 'tuple',
        components: [
          { name: 'account', type: 'address' },
          { name: 'clientOrderId', type: 'bytes32' },
          { name: 'intentHash', type: 'bytes32' },
          { name: 'expectedConfigHash', type: 'bytes32' },
          { name: 'observedConfigHash', type: 'bytes32' },
          { name: 'status', type: 'uint8' },
          { name: 'reason', type: 'uint8' },
          { name: 'executionMode', type: 'uint8' },
          { name: 'priceSource', type: 'uint8' },
          { name: 'bountyDisposition', type: 'uint8' },
          { name: 'terminalBlock', type: 'uint64' },
          { name: 'terminalTime', type: 'uint64' },
          { name: 'oraclePublishTime', type: 'uint64' },
          { name: 'executor', type: 'address' },
          { name: 'bountyRecipient', type: 'address' },
          { name: 'executionPrice', type: 'uint256' },
          { name: 'bountyUsdc', type: 'uint256' },
          { name: 'failureSelector', type: 'bytes4' },
          { name: 'failureCategory', type: 'uint8' },
          { name: 'failureCode', type: 'uint8' },
          { name: 'failedConstraint', type: 'uint8' },
          { name: 'revertDataHash', type: 'bytes32' },
          { name: 'receiptHash', type: 'bytes32' },
        ],
      },
    ],
  },
  {
    type: 'event',
    name: 'IntentRegistered',
    inputs: [
      { name: 'orderId', type: 'uint64', indexed: true },
      { name: 'account', type: 'address', indexed: true },
      { name: 'clientOrderId', type: 'bytes32', indexed: true },
      { name: 'intentHash', type: 'bytes32', indexed: false },
      { name: 'executionBountyUsdc', type: 'uint256', indexed: false },
      {
        name: 'request',
        type: 'tuple',
        indexed: false,
        components: PERPS_ORDER_REQUEST_V2_COMPONENTS,
      },
    ],
  },
  {
    type: 'event',
    name: 'OrderFinalized',
    inputs: [
      { name: 'orderId', type: 'uint64', indexed: true },
      { name: 'account', type: 'address', indexed: true },
      { name: 'clientOrderId', type: 'bytes32', indexed: true },
      { name: 'receiptHash', type: 'bytes32', indexed: false },
      { name: 'terminalBlock', type: 'uint64', indexed: false },
      { name: 'terminalTime', type: 'uint64', indexed: false },
      {
        name: 'receipt',
        type: 'tuple',
        indexed: false,
        components: ORDER_RECEIPT_COMPONENTS,
      },
    ],
  },
  {
    type: 'error',
    name: 'OrderLifecycleBook__ZeroClientOrderId',
    inputs: [],
  },
  {
    type: 'error',
    name: 'OrderLifecycleBook__ClientIdConflict',
    inputs: [
      { name: 'account', type: 'address' },
      { name: 'clientOrderId', type: 'bytes32' },
      { name: 'existingIntentHash', type: 'bytes32' },
      { name: 'suppliedIntentHash', type: 'bytes32' },
    ],
  },
  {
    type: 'error',
    name: 'OrderLifecycleBook__ClientIdDomainMismatch',
    inputs: [
      { name: 'clientOrderId', type: 'bytes32' },
      { name: 'protocolIntent', type: 'bool' },
    ],
  },
  {
    type: 'error',
    name: 'OrderLifecycleBook__ExecutionBountyAboveBound',
    inputs: [
      { name: 'actualBountyUsdc', type: 'uint256' },
      { name: 'maximumBountyUsdc', type: 'uint256' },
    ],
  },
] as const

export const PERPS_ORDER_POLICY_EVALUATOR_ABI = [
  {
    type: 'function',
    name: 'assessOrder',
    stateMutability: 'view',
    inputs: [
      { name: 'engine', type: 'address' },
      { name: 'order', type: 'tuple', components: CFD_ORDER_COMPONENTS },
      { name: 'executor', type: 'address' },
      { name: 'currentOraclePrice', type: 'uint256' },
      { name: 'poolDepthUsdc', type: 'uint256' },
      { name: 'publishTime', type: 'uint64' },
      {
        name: 'bounds',
        type: 'tuple',
        components: PERPS_EXECUTION_BOUNDS_COMPONENTS,
      },
      { name: 'executionBountyUsdc', type: 'uint256' },
    ],
    outputs: [
      {
        name: 'assessment',
        type: 'tuple',
        components: EXECUTION_ASSESSMENT_COMPONENTS,
      },
    ],
  },
  {
    type: 'error',
    name: 'CfdOrderPolicyEvaluator__ExecutionModeDisallowed',
    inputs: [
      { name: 'mode', type: 'uint8' },
      { name: 'allowedExecutionModes', type: 'uint8' },
    ],
  },
  {
    type: 'error',
    name: 'CfdOrderPolicyEvaluator__ConstraintViolation',
    inputs: [
      { name: 'constraint', type: 'uint8' },
      { name: 'actual', type: 'uint256' },
      { name: 'limit', type: 'uint256' },
    ],
  },
] as const

export { positionProtectionBookAbi as PERPS_POSITION_PROTECTION_BOOK_ABI } from '@plether/perps-aa-client'

export const PERPS_CFD_ENGINE_ABI = [
  {
    type: 'event',
    name: 'PositionOpened',
    inputs: [
      { name: 'account', type: 'address', indexed: true },
      { name: 'side', type: 'uint8', indexed: false },
      { name: 'sizeDelta', type: 'uint256', indexed: false },
      { name: 'price', type: 'uint256', indexed: false },
      { name: 'marginDelta', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'PositionClosed',
    inputs: [
      { name: 'account', type: 'address', indexed: true },
      { name: 'side', type: 'uint8', indexed: false },
      { name: 'sizeDelta', type: 'uint256', indexed: false },
      { name: 'price', type: 'uint256', indexed: false },
      { name: 'pnl', type: 'int256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'PositionLiquidated',
    inputs: [
      { name: 'account', type: 'address', indexed: true },
      { name: 'side', type: 'uint8', indexed: false },
      { name: 'size', type: 'uint256', indexed: false },
      { name: 'price', type: 'uint256', indexed: false },
      { name: 'keeperBounty', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'MarginAdded',
    inputs: [
      { name: 'account', type: 'address', indexed: true },
      { name: 'amount', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'function',
    name: 'sides',
    stateMutability: 'view',
    inputs: [{ name: 'index', type: 'uint256' }],
    outputs: [
      { name: 'maxProfitUsdc', type: 'uint256' },
      { name: 'openInterest', type: 'uint256' },
      { name: 'entryNotional', type: 'uint256' },
      { name: 'totalMargin', type: 'uint256' },
    ],
  },
  {
    type: 'function',
    name: 'riskParams',
    stateMutability: 'view',
    inputs: [],
    outputs: [
      { name: 'vpiFactor', type: 'uint256' },
      { name: 'maxSkewRatio', type: 'uint256' },
      { name: 'maintMarginBps', type: 'uint256' },
      { name: 'initMarginBps', type: 'uint256' },
      { name: 'fadMarginBps', type: 'uint256' },
      { name: 'baseCarryBps', type: 'uint256' },
      { name: 'minBountyUsdc', type: 'uint256' },
      { name: 'bountyBps', type: 'uint256' },
      { name: 'keeperShareBps', type: 'uint256' },
      { name: 'protocolShareBps', type: 'uint256' },
    ],
  },
  {
    type: 'function',
    name: 'CAP_PRICE',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'lastMarkPrice',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'clearinghouse',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'address' }],
  },
  {
    type: 'function',
    name: 'pool',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'address' }],
  },
  {
    type: 'function',
    name: 'isFadWindow',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'bool' }],
  },
  {
    type: 'function',
    name: 'positions',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [
      { name: 'size', type: 'uint256' },
      { name: 'margin', type: 'uint256' },
      { name: 'entryPrice', type: 'uint256' },
      { name: 'maxProfitUsdc', type: 'uint256' },
      { name: 'side', type: 'uint8' },
      { name: 'lastUpdateTime', type: 'uint64' },
      { name: 'vpiAccrued', type: 'int256' },
    ],
  },
  {
    type: 'function',
    name: 'executionFeeBps',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'settleTraderClaim',
    stateMutability: 'nonpayable',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [],
  },
  {
    type: 'function',
    name: 'addMargin',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'account', type: 'address' },
      { name: 'amount', type: 'uint256' },
    ],
    outputs: [],
  },
] as const

export const PERPS_HOUSE_POOL_ABI = [
  {
    type: 'function',
    name: 'totalAssets',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'assets', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'getPoolLiquidityView',
    stateMutability: 'view',
    inputs: [],
    outputs: [
      {
        name: 'viewData',
        type: 'tuple',
        components: [
          { name: 'totalAssetsUsdc', type: 'uint256' },
          { name: 'freeUsdc', type: 'uint256' },
          { name: 'withdrawalReservedUsdc', type: 'uint256' },
          { name: 'pendingRecapitalizationUsdc', type: 'uint256' },
          { name: 'pendingTradingRevenueUsdc', type: 'uint256' },
          { name: 'seniorPrincipalUsdc', type: 'uint256' },
          { name: 'juniorPrincipalUsdc', type: 'uint256' },
          { name: 'seniorHighWaterMarkUsdc', type: 'uint256' },
          { name: 'currentTerminalDeficitUsdc', type: 'uint256' },
          { name: 'markFresh', type: 'bool' },
          { name: 'oracleFrozen', type: 'bool' },
          { name: 'degradedMode', type: 'bool' },
        ],
      },
    ],
  },
  {
    type: 'function',
    name: 'getPendingTrancheState',
    stateMutability: 'view',
    inputs: [],
    outputs: [
      { name: 'seniorPrincipalUsdc', type: 'uint256' },
      { name: 'juniorPrincipalUsdc', type: 'uint256' },
      { name: 'maxSeniorWithdrawUsdc', type: 'uint256' },
      { name: 'maxJuniorWithdrawUsdc', type: 'uint256' },
    ],
  },
  {
    type: 'function',
    name: 'maxSeniorExposureUsdc',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'capacity', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'maxSeniorShareBps',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'capacityBps', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'seniorRateBps',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'rateBps', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'getSeniorDepositCapacity',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'capacity', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'reservedSeniorDepositAssetsUsdc',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'reservedAssets', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'areSeniorDepositReservationsWithinLimits',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'withinLimits', type: 'bool' }],
  },
  {
    type: 'function',
    name: 'minTrancheDepositUsdc',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'minimumAssets', type: 'uint256' }],
  },
] as const

export const PERPS_PLETHER_ORACLE_ABI = [
  {
    type: 'function',
    name: 'getUpdateFee',
    stateMutability: 'view',
    inputs: [{ name: 'pythUpdateData', type: 'bytes[]' }],
    outputs: [{ name: 'pythFee', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'orderSettlementWindow',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'windowSeconds', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'getLatestPrice',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ name: 'latestPrice', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'getLatestPrice',
    stateMutability: 'view',
    inputs: [{ name: 'mode', type: 'uint8' }],
    outputs: [
      {
        name: 'snapshot',
        type: 'tuple',
        components: PRICE_SNAPSHOT_COMPONENTS,
      },
    ],
  },
  {
    type: 'function',
    name: 'claimableEth',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [{ name: 'amount', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'claimEthRefund',
    stateMutability: 'nonpayable',
    inputs: [],
    outputs: [],
  },
  {
    type: 'function',
    name: 'getOrderExecutionPolicy',
    stateMutability: 'view',
    inputs: [{ name: 'isClose', type: 'bool' }],
    outputs: [
      {
        name: 'policy',
        type: 'tuple',
        components: POLICY_SNAPSHOT_COMPONENTS,
      },
    ],
  },
] as const

export const PERPS_CFD_ENGINE_LENS_ABI = [
  {
    type: 'function',
    name: 'previewOpen',
    stateMutability: 'view',
    inputs: [
      { name: 'account', type: 'address' },
      { name: 'side', type: 'uint8' },
      { name: 'sizeDelta', type: 'uint256' },
      { name: 'marginDelta', type: 'uint256' },
      { name: 'oraclePrice', type: 'uint256' },
      { name: 'publishTime', type: 'uint64' },
    ],
    outputs: [
      {
        name: 'preview',
        type: 'tuple',
        components: OPEN_PREVIEW_COMPONENTS,
      },
    ],
  },
  {
    type: 'function',
    name: 'previewOpenRevertCode',
    stateMutability: 'view',
    inputs: [
      { name: 'account', type: 'address' },
      { name: 'side', type: 'uint8' },
      { name: 'sizeDelta', type: 'uint256' },
      { name: 'marginDelta', type: 'uint256' },
      { name: 'oraclePrice', type: 'uint256' },
      { name: 'publishTime', type: 'uint64' },
    ],
    outputs: [{ name: 'code', type: 'uint8' }],
  },
  {
    type: 'function',
    name: 'previewOpenFailurePolicyCategory',
    stateMutability: 'view',
    inputs: [
      { name: 'account', type: 'address' },
      { name: 'side', type: 'uint8' },
      { name: 'sizeDelta', type: 'uint256' },
      { name: 'marginDelta', type: 'uint256' },
      { name: 'oraclePrice', type: 'uint256' },
      { name: 'publishTime', type: 'uint64' },
    ],
    outputs: [{ name: 'category', type: 'uint8' }],
  },
  {
    type: 'function',
    name: 'previewClose',
    stateMutability: 'view',
    inputs: [
      { name: 'account', type: 'address' },
      { name: 'sizeDelta', type: 'uint256' },
      { name: 'oraclePrice', type: 'uint256' },
    ],
    outputs: [
      {
        name: 'preview',
        type: 'tuple',
        components: CLOSE_PREVIEW_COMPONENTS,
      },
    ],
  },
  {
    type: 'function',
    name: 'previewLiquidation',
    stateMutability: 'view',
    inputs: [
      { name: 'account', type: 'address' },
      { name: 'oraclePrice', type: 'uint256' },
    ],
    outputs: [
      {
        name: 'preview',
        type: 'tuple',
        components: LIQUIDATION_PREVIEW_COMPONENTS,
      },
    ],
  },
] as const

export const PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI = [
  {
    type: 'function',
    name: 'getAccountCollateralView',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [
      {
        name: 'viewData',
        type: 'tuple',
        components: ACCOUNT_COLLATERAL_VIEW_COMPONENTS,
      },
    ],
  },
  {
    type: 'function',
    name: 'getWithdrawableUsdc',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [{ name: 'withdrawableUsdc', type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'getAccountLedgerView',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [
      {
        name: 'viewData',
        type: 'tuple',
        components: ACCOUNT_LEDGER_VIEW_COMPONENTS,
      },
    ],
  },
  {
    type: 'function',
    name: 'getAccountLedgerSnapshot',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [
      {
        name: 'snapshot',
        type: 'tuple',
        components: ACCOUNT_LEDGER_SNAPSHOT_COMPONENTS,
      },
    ],
  },
] as const
