// Curve StableSwap Pool ABI - USDC/DXY-BEAR trading
export const CURVE_POOL_ABI = [
  {
    type: 'function',
    name: 'exchange',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'i', type: 'int128' },
      { name: 'j', type: 'int128' },
      { name: 'dx', type: 'uint256' },
      { name: 'min_dy', type: 'uint256' },
      { name: 'receiver', type: 'address' },
    ],
    outputs: [{ type: 'uint256', name: 'dy' }],
  },
  {
    type: 'function',
    name: 'get_dy',
    stateMutability: 'view',
    inputs: [
      { name: 'i', type: 'int128' },
      { name: 'j', type: 'int128' },
      { name: 'dx', type: 'uint256' },
    ],
    outputs: [{ type: 'uint256', name: 'dy' }],
  },
  {
    type: 'function',
    name: 'add_liquidity',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'amounts', type: 'uint256[2]' },
      { name: 'min_mint_amount', type: 'uint256' },
      { name: 'receiver', type: 'address' },
    ],
    outputs: [{ type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'remove_liquidity',
    stateMutability: 'nonpayable',
    inputs: [
      { name: '_amount', type: 'uint256' },
      { name: 'min_amounts', type: 'uint256[2]' },
      { name: 'receiver', type: 'address' },
    ],
    outputs: [{ type: 'uint256[2]' }],
  },
  {
    type: 'function',
    name: 'remove_liquidity_one_coin',
    stateMutability: 'nonpayable',
    inputs: [
      { name: '_token_amount', type: 'uint256' },
      { name: 'i', type: 'int128' },
      { name: '_min_amount', type: 'uint256' },
      { name: 'receiver', type: 'address' },
    ],
    outputs: [{ type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'balances',
    stateMutability: 'view',
    inputs: [{ name: 'i', type: 'uint256' }],
    outputs: [{ type: 'uint256' }],
  },
  {
    type: 'function',
    name: 'coins',
    stateMutability: 'view',
    inputs: [{ name: 'i', type: 'uint256' }],
    outputs: [{ type: 'address' }],
  },
  {
    type: 'function',
    name: 'get_virtual_price',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint256' }],
  },
  {
    type: 'event',
    name: 'TokenExchange',
    inputs: [
      { name: 'buyer', type: 'address', indexed: true },
      { name: 'sold_id', type: 'int128', indexed: false },
      { name: 'tokens_sold', type: 'uint256', indexed: false },
      { name: 'bought_id', type: 'int128', indexed: false },
      { name: 'tokens_bought', type: 'uint256', indexed: false },
    ],
  },
] as const
