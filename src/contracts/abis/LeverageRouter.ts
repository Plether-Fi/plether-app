// LeverageRouter ABI - Open/close leveraged positions
export const LEVERAGE_ROUTER_ABI = [
  {
    type: 'function',
    name: 'openLeverage',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'principal', type: 'uint256' },
      { name: 'leverage', type: 'uint256' },
      { name: 'maxSlippageBps', type: 'uint256' },
      { name: 'deadline', type: 'uint256' },
    ],
    outputs: [
      { type: 'uint256', name: 'positionSize' },
      { type: 'uint256', name: 'debt' },
    ],
  },
  {
    type: 'function',
    name: 'closeLeverage',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'debtToRepay', type: 'uint256' },
      { name: 'collateralToWithdraw', type: 'uint256' },
      { name: 'maxSlippageBps', type: 'uint256' },
      { name: 'deadline', type: 'uint256' },
    ],
    outputs: [{ type: 'uint256', name: 'usdcReturned' }],
  },
  {
    type: 'function',
    name: 'addCollateral',
    stateMutability: 'nonpayable',
    inputs: [{ name: 'amount', type: 'uint256' }],
    outputs: [],
  },
  {
    type: 'function',
    name: 'removeCollateral',
    stateMutability: 'nonpayable',
    inputs: [{ name: 'amount', type: 'uint256' }],
    outputs: [],
  },
  {
    type: 'function',
    name: 'previewOpenLeverage',
    stateMutability: 'view',
    inputs: [
      { name: 'principal', type: 'uint256' },
      { name: 'leverage', type: 'uint256' },
    ],
    outputs: [
      { type: 'uint256', name: 'positionSize' },
      { type: 'uint256', name: 'debt' },
      { type: 'uint256', name: 'liquidationPrice' },
    ],
  },
  {
    type: 'function',
    name: 'previewCloseLeverage',
    stateMutability: 'view',
    inputs: [
      { name: 'debtToRepay', type: 'uint256' },
      { name: 'collateralToWithdraw', type: 'uint256' },
    ],
    outputs: [{ type: 'uint256', name: 'usdcReturned' }],
  },
  {
    type: 'function',
    name: 'getPosition',
    stateMutability: 'view',
    inputs: [{ name: 'user', type: 'address' }],
    outputs: [
      { type: 'uint256', name: 'collateral' },
      { type: 'uint256', name: 'debt' },
      { type: 'uint256', name: 'leverage' },
    ],
  },
  {
    type: 'function',
    name: 'getHealthFactor',
    stateMutability: 'view',
    inputs: [{ name: 'user', type: 'address' }],
    outputs: [{ type: 'uint256', name: 'healthFactor' }],
  },
  {
    type: 'function',
    name: 'getLiquidationPrice',
    stateMutability: 'view',
    inputs: [{ name: 'user', type: 'address' }],
    outputs: [{ type: 'uint256', name: 'liquidationPrice' }],
  },
  {
    type: 'event',
    name: 'PositionOpened',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'principal', type: 'uint256', indexed: false },
      { name: 'leverage', type: 'uint256', indexed: false },
      { name: 'positionSize', type: 'uint256', indexed: false },
      { name: 'debt', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'PositionClosed',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'collateralReturned', type: 'uint256', indexed: false },
      { name: 'debtRepaid', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'CollateralAdded',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'amount', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'CollateralRemoved',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'amount', type: 'uint256', indexed: false },
    ],
  },
] as const
