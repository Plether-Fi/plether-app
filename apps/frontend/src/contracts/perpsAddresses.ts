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
  pyth: '0x4374e5a8b9C22271E9EB878A2AA31DE97DF15DAF',
  usdc: '0xf1e1B188b87525C51ECe4bae8627ae621D769651',
  perpsPublicLens: '0xDdDCfb123569774427802fcA9D19CBF00c14e2Ad',
  marginClearinghouse: '0x731bb0939CE531728459394A277B28Cbff8df049',
  orderRouter: '0x4A0a6c028164A1254e10C3e39cc89Af45090069e',
  orderRouterAdmin: '0xf11858573eE79EF64e38e47572785D67cE7641Ec',
  cfdEngine: '0xA1Ebfb8aD9C90367eA30A29592419d447E3f8224',
  cfdEnginePlanner: '0x7dDC8AdF27456A71e02e517E28a975832D49d195',
  cfdEngineSettlementSidecar: '0x78C79E81fF5221DCdfB6B384A86990bffAFd4D6b',
  cfdEngineAdmin: '0x03957FACB0d371f170737fa0252CDC1088bba78A',
  housePool: '0x793dAbc20Ab0eCEb0AD8060b1fb307212C9EB6df',
  seniorVault: '0x352F2C0Ad6e0Db6EbC3fBE7738857a804327f53b',
  juniorVault: '0x783daF5eC664932764a59Ae387C3eAbD6cC61A74',
  pletherOracle: '0x8c95f554D728215b9f8D15b5F3Da5F5CD7Ba08bA',
  cfdEngineLens: '0xB7F0A32EfD67193782171Efc60D5D13A44bd5177',
  cfdEngineAccountLens: '0xb46f7ECAE1E7D3BC8ebC7FB1cda20d2d9a83cC29',
} satisfies PerpsContractAddresses
