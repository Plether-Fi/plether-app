// ZapRouter ABI - Routes BULL trades via mint + Curve swap
export const ZAP_ROUTER_ABI = [
  {
    type: 'function',
    name: 'zapMint',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'usdcAmount', type: 'uint256' },
      { name: 'minAmountOut', type: 'uint256' },
      { name: 'maxSlippageBps', type: 'uint256' },
      { name: 'deadline', type: 'uint256' },
    ],
    outputs: [{ type: 'uint256', name: 'bullAmount' }],
  },
  {
    type: 'function',
    name: 'zapBurn',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'bullAmount', type: 'uint256' },
      { name: 'minUsdcOut', type: 'uint256' },
      { name: 'deadline', type: 'uint256' },
    ],
    outputs: [{ type: 'uint256', name: 'usdcAmount' }],
  },
  {
    type: 'function',
    name: 'previewZapMint',
    stateMutability: 'view',
    inputs: [{ name: 'usdcAmount', type: 'uint256' }],
    outputs: [{ type: 'uint256', name: 'bullAmount' }],
  },
  {
    type: 'function',
    name: 'previewZapBurn',
    stateMutability: 'view',
    inputs: [{ name: 'bullAmount', type: 'uint256' }],
    outputs: [{ type: 'uint256', name: 'usdcAmount' }],
  },
  {
    type: 'function',
    name: 'splitter',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'address' }],
  },
  {
    type: 'function',
    name: 'curvePool',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'address' }],
  },
  {
    type: 'event',
    name: 'ZapMint',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'usdcIn', type: 'uint256', indexed: false },
      { name: 'bullOut', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'ZapBurn',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'bullIn', type: 'uint256', indexed: false },
      { name: 'usdcOut', type: 'uint256', indexed: false },
    ],
  },
] as const
