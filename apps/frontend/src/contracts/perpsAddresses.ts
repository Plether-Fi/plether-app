import type { Address } from 'viem'

export const PERPS_ARBITRUM_SEPOLIA_CHAIN_ID = 421614

export interface PerpsContractAddresses {
  pyth: Address
  usdc: Address
  perpsPublicLens: Address
  marginClearinghouse: Address
  orderRouter: Address
  cfdEngine: Address
  housePool: Address
  seniorVault: Address
  juniorVault: Address
  pletherOracle: Address
  cfdEngineLens: Address
  cfdEngineAccountLens: Address
}

export const PERPS_ARBITRUM_SEPOLIA = {
  pyth: '0x4374e5a8b9C22271E9EB878A2AA31DE97DF15DAF',
  usdc: '0x55E007D79906572cCCA8e75B1Beb302787348D6E',
  perpsPublicLens: '0xf3871a2e8247515CC913aad682951Eb3f2A673FB',
  marginClearinghouse: '0x00B89B6e696A43129DA7Ec8a814bb61C9A6189b8',
  orderRouter: '0x485703D16fE36369c134dEe2A61c057733E7830f',
  cfdEngine: '0x128f195B92b50db1eEBCbBd249d5C5e946DCd786',
  housePool: '0x493Ed3466e212Bc3F04075CAaf2837F70b7bAD8a',
  seniorVault: '0x17eC59e7284CB9fb17B5625153c6Af7f58708981',
  juniorVault: '0xfED1D3F433d74148F9C04b34ed6d49752f34ff8E',
  pletherOracle: '0x0e7c23b6Eb951DF97f7d2Fb2382B4405d88318bb',
  cfdEngineLens: '0xEEE25f5bdC515E3676198dE7262b5aF043b8a37a',
  cfdEngineAccountLens: '0x74f0a25BcC4b243E915Df1CA6385250aCff1eeE9',
} satisfies PerpsContractAddresses
