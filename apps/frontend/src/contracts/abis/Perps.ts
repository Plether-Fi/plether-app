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
] as const

export const PERPS_CFD_ENGINE_ABI = [
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
    name: 'executionFeeBps',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint256' }],
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
