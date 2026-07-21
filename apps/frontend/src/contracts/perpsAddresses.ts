import type { Address } from 'viem'

export const PERPS_ARBITRUM_SEPOLIA_CHAIN_ID = 421614

export interface PerpsContractAddresses {
  pyth: Address
  usdc: Address
  perpsPublicLens: Address
  marginClearinghouse: Address
  orderRouter: Address
  orderRouterAdmin: Address
  cfdEngine: Address
  cfdEnginePlanner: Address
  cfdEngineSettlementSidecar: Address
  cfdEngineAdmin: Address
  housePool: Address
  seniorVault: Address
  juniorVault: Address
  pletherOracle: Address
  cfdEngineLens: Address
  cfdEngineAccountLens: Address
}

export const PERPS_ARBITRUM_SEPOLIA = {
  pyth: '0x0B73614636C855Bf23F342F307FB981A3e47f42B',
  usdc: '0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3',
  perpsPublicLens: '0x4E202C06e2C378d1a85577ac631e592AB66f23FB',
  marginClearinghouse: '0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e',
  orderRouter: '0x04E3103752f623fBcDcD01f588590Af4c53E4c1E',
  orderRouterAdmin: '0x3073d6D021eC20b95a8b7C780f5c30c07036ff6C',
  cfdEngine: '0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a',
  cfdEnginePlanner: '0x7B4208840E0ea325dd71bDF50ffe6F8FE4018A21',
  cfdEngineSettlementSidecar: '0x0b652c4D4610234e221403076C116292F935b424',
  cfdEngineAdmin: '0xb256d4E88d649b2A149aA8B8caa3159260eFBc39',
  housePool: '0xFA654f4c548130F09C3Fb962AbD4bE32c0357C18',
  seniorVault: '0x4bAb5448C1BD9A48B978ABcb014F1a8F80F100A8',
  juniorVault: '0x7258d6E91fbEFB8a16751575adbe9bBB3086D458',
  pletherOracle: '0xADfEd3bf768D810309B97b4dF9F9E77Eaa3a401c',
  cfdEngineLens: '0xa9aA4097874e9622eAABeE68f65Ff5e3757728C5',
  cfdEngineAccountLens: '0xC4C886A6F1D7CB22C833AC1b29f29Da43AfbcCd1',
} satisfies PerpsContractAddresses
