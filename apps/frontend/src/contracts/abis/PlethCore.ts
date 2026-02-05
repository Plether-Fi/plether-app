// SyntheticSplitter ABI - mint/burn plDXY-BEAR + plDXY-BULL pairs
export const PLETH_CORE_ABI = [
  {
    type: 'function',
    name: 'currentStatus',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint8', name: 'status' }], // 0=Active, 1=Paused, 2=Settled
  },
  {
    type: 'function',
    name: 'getSystemStatus',
    stateMutability: 'view',
    inputs: [],
    outputs: [
      { type: 'uint8', name: 'status' },
      { type: 'uint256', name: 'totalSupply' },
      { type: 'uint256', name: 'oraclePrice' },
    ],
  },
  {
    type: 'function',
    name: 'mint',
    stateMutability: 'nonpayable',
    inputs: [{ name: 'amount', type: 'uint256' }], // pair amount (18 decimals)
    outputs: [],
  },
  {
    type: 'function',
    name: 'burn',
    stateMutability: 'nonpayable',
    inputs: [{ name: 'amount', type: 'uint256' }], // pair amount (18 decimals)
    outputs: [],
  },
  {
    type: 'function',
    name: 'previewMint',
    stateMutability: 'view',
    inputs: [{ name: 'mintAmount', type: 'uint256' }], // pair amount (18 decimals)
    outputs: [
      { type: 'uint256', name: 'usdcRequired' },
      { type: 'uint256', name: 'depositToAdapter' },
      { type: 'uint256', name: 'keptInBuffer' },
    ],
  },
  {
    type: 'function',
    name: 'previewBurn',
    stateMutability: 'view',
    inputs: [{ name: 'burnAmount', type: 'uint256' }], // pair amount (18 decimals)
    outputs: [
      { type: 'uint256', name: 'usdcToReturn' },
      { type: 'uint256', name: 'withdrawnFromAdapter' },
    ],
  },
  {
    type: 'function',
    name: 'bearToken',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'address' }],
  },
  {
    type: 'function',
    name: 'bullToken',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'address' }],
  },
  {
    type: 'function',
    name: 'usdc',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'address' }],
  },
  {
    type: 'function',
    name: 'CAP',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint256' }],
  },
  {
    type: 'event',
    name: 'Mint',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'usdcAmount', type: 'uint256', indexed: false },
      { name: 'pairAmount', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'Burn',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'pairAmount', type: 'uint256', indexed: false },
      { name: 'usdcAmount', type: 'uint256', indexed: false },
    ],
  },
] as const
