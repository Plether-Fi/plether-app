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
  orderLifecycleBook: Address
  policyEvaluator: Address
  positionProtectionBook: Address
}

export const PERPS_ARBITRUM_SEPOLIA = {
  pyth: '0x0B73614636C855Bf23F342F307FB981A3e47f42B',
  usdc: '0xAbEe441b564DC084857468fA244AEE0A444B07DF',
  perpsPublicLens: '0x269db12a9A275F40b2d3826fDea7eadee8b7CBe9',
  marginClearinghouse: '0x91c85540A1f64C9AEC2C801fcc927F037d619f17',
  orderRouter: '0x2b9790AD11cE5fB1B91aC3415B08cD1Ec7D0cE0B',
  orderRouterAdmin: '0x3d57927C1C989E28B8eb6732c91329Bf218501ad',
  cfdEngine: '0x2CEDc3f0059f0E9C1099bE96974f459E58c428d6',
  cfdEnginePlanner: '0x92B253b6CE6F8BB0f63B7929A8Ca125f1359ACCc',
  cfdEngineSettlementSidecar: '0x6b7366C8C125fc1Ec07A4E600bD13649A4719D78',
  cfdEngineAdmin: '0x1E277162a0Ef336AcC2022A5c2730Aa127db3eCd',
  housePool: '0x7b8b851cb3783611bcDA4CF2F7D5A2F8C6106F98',
  seniorVault: '0xF98e69d808F8c22fCE4210516E2F0B2dAa4CC0B2',
  juniorVault: '0xd6B662D75B102eA360C1B083E1f332e6c1634832',
  pletherOracle: '0x06bb48A53FF7f1c2723e48EFEFF3C0861b789664',
  cfdEngineLens: '0x09858B773E0004B5f05FC9aF8BD3173e0dEDdfc3',
  cfdEngineAccountLens: '0x1707549c4B1B0B335a10aD664Ae3434182Cb8d7B',
  orderLifecycleBook: '0xca57215a3859462eb380ea40969762Ac89D99522',
  policyEvaluator: '0x611b34a98261D60f0aE8584F4Dd1fF09CF663466',
  positionProtectionBook: '0x63973Eb0B5a862dfc95348D4d575FC55C9546F04',
} satisfies PerpsContractAddresses
