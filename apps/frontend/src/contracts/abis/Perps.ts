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
  { name: 'keeperBountyUsdc', type: 'uint256' },
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
  { name: 'terminalReachableUsdc', type: 'uint256' },
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
  { name: 'terminalReachableUsdc', type: 'uint256' },
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

export const PERPS_PUBLIC_LENS_ABI = [
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
        ],
      },
    ],
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
    name: 'isLiquidatable',
    stateMutability: 'view',
    inputs: [{ name: 'account', type: 'address' }],
    outputs: [{ type: 'bool' }],
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
] as const

export const PERPS_ORDER_ROUTER_ABI = [
  {
    type: 'function',
    name: 'commitOrder',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'side', type: 'uint8' },
      { name: 'sizeDelta', type: 'uint256' },
      { name: 'marginDelta', type: 'uint256' },
      { name: 'targetPrice', type: 'uint256' },
      { name: 'isClose', type: 'bool' },
    ],
    outputs: [],
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
    name: 'executeOrder',
    stateMutability: 'payable',
    inputs: [
      { name: 'orderId', type: 'uint64' },
      { name: 'pythUpdateData', type: 'bytes[]' },
    ],
    outputs: [],
  },
  {
    type: 'function',
    name: 'executeOrderBatch',
    stateMutability: 'payable',
    inputs: [
      { name: 'maxOrderId', type: 'uint64' },
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
    type: 'event',
    name: 'OrderExecuted',
    inputs: [
      { name: 'orderId', type: 'uint64', indexed: true },
      { name: 'executionPrice', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'OrderFailed',
    inputs: [
      { name: 'orderId', type: 'uint64', indexed: true },
      { name: 'reason', type: 'uint8', indexed: false },
    ],
  },
] as const

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
] as const

export const PERPS_HOUSE_POOL_ABI = [
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
          { name: 'markFresh', type: 'bool' },
          { name: 'oracleFrozen', type: 'bool' },
          { name: 'degradedMode', type: 'bool' },
        ],
      },
    ],
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
