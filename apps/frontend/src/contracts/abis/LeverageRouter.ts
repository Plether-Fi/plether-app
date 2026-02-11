// LeverageRouter ABI - Open/close leveraged positions
// Used for both BEAR (LeverageRouter) and BULL (BullLeverageRouter)

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
      { name: 'collateralToWithdraw', type: 'uint256' },
      { name: 'maxSlippageBps', type: 'uint256' },
      { name: 'deadline', type: 'uint256' },
    ],
    outputs: [{ type: 'uint256', name: 'usdcReturned' }],
  },
  {
    type: 'function',
    name: 'openLeverageWithPermit',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'principal', type: 'uint256' },
      { name: 'leverage', type: 'uint256' },
      { name: 'maxSlippageBps', type: 'uint256' },
      { name: 'deadline', type: 'uint256' },
      { name: 'v', type: 'uint8' },
      { name: 'r', type: 'bytes32' },
      { name: 's', type: 'bytes32' },
    ],
    outputs: [
      { type: 'uint256', name: 'positionSize' },
      { type: 'uint256', name: 'debt' },
    ],
  },
  {
    type: 'function',
    name: 'addCollateral',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'usdcAmount', type: 'uint256' },
      { name: 'maxSlippageBps', type: 'uint256' },
      { name: 'deadline', type: 'uint256' },
    ],
    outputs: [],
  },
  {
    type: 'function',
    name: 'addCollateralWithPermit',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'usdcAmount', type: 'uint256' },
      { name: 'maxSlippageBps', type: 'uint256' },
      { name: 'deadline', type: 'uint256' },
      { name: 'v', type: 'uint8' },
      { name: 'r', type: 'bytes32' },
      { name: 's', type: 'bytes32' },
    ],
    outputs: [],
  },
  {
    type: 'function',
    name: 'removeCollateral',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'collateralToWithdraw', type: 'uint256' },
      { name: 'maxSlippageBps', type: 'uint256' },
      { name: 'deadline', type: 'uint256' },
    ],
    outputs: [],
  },
  {
    type: 'function',
    name: 'getCollateral',
    stateMutability: 'view',
    inputs: [{ name: 'user', type: 'address' }],
    outputs: [{ type: 'uint256', name: 'collateral' }],
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
      { type: 'uint256', name: 'loanAmount' },
      { type: 'uint256', name: 'totalUSDC' },
      { type: 'uint256', name: 'expectedPlDxyBear' },
      { type: 'uint256', name: 'expectedDebt' },
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
    outputs: [
      { type: 'uint256', name: 'expectedUSDC' },
      { type: 'uint256', name: 'flashFee' },
      { type: 'uint256', name: 'expectedReturn' },
    ],
  },
  {
    type: 'function',
    name: 'getActualDebt',
    stateMutability: 'view',
    inputs: [{ name: 'user', type: 'address' }],
    outputs: [{ type: 'uint256', name: 'debt' }],
  },
  {
    type: 'function',
    name: 'MORPHO',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'address' }],
  },
  {
    type: 'function',
    name: 'marketParams',
    stateMutability: 'view',
    inputs: [],
    outputs: [
      { type: 'address', name: 'loanToken' },
      { type: 'address', name: 'collateralToken' },
      { type: 'address', name: 'oracle' },
      { type: 'address', name: 'irm' },
      { type: 'uint256', name: 'lltv' },
    ],
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
      { name: 'usdcAmount', type: 'uint256', indexed: false },
      { name: 'usdcReturned', type: 'uint256', indexed: false },
      { name: 'collateralAdded', type: 'uint256', indexed: false },
      { name: 'maxSlippageBps', type: 'uint256', indexed: false },
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

// BEAR Router specific - previewAddCollateral returns 2 values
export const BEAR_LEVERAGE_ROUTER_ABI = [
  ...LEVERAGE_ROUTER_ABI,
  {
    type: 'function',
    name: 'previewAddCollateral',
    stateMutability: 'view',
    inputs: [{ name: 'usdcAmount', type: 'uint256' }],
    outputs: [
      { type: 'uint256', name: 'expectedPlDxyBear' },
      { type: 'uint256', name: 'expectedStakedShares' },
    ],
  },
] as const

// BULL Router specific - previewAddCollateral returns 3 values
export const BULL_LEVERAGE_ROUTER_ABI = [
  ...LEVERAGE_ROUTER_ABI,
  {
    type: 'function',
    name: 'previewAddCollateral',
    stateMutability: 'view',
    inputs: [{ name: 'usdcAmount', type: 'uint256' }],
    outputs: [
      { type: 'uint256', name: 'tokensToMint' },
      { type: 'uint256', name: 'expectedUsdcFromBearSale' },
      { type: 'uint256', name: 'expectedStakedShares' },
    ],
  },
] as const
