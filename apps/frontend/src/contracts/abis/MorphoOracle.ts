// Morpho IOracle interface - returns price in 1e36 scale
export const MORPHO_ORACLE_ABI = [
  {
    type: 'function',
    name: 'price',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint256' }],
  },
] as const
