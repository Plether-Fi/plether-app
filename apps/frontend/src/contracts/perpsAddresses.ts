import type { Address } from 'viem'

export const PERPS_ARBITRUM_SEPOLIA_CHAIN_ID = 421614
export const PERPS_ARBITRUM_SEPOLIA_DEPLOYMENT_BLOCK = 307_397_196

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
  usdc: '0xf7cbfcc74f2d9eb6fa7dc11941b3bef9fd7f8eb8',
  perpsPublicLens: '0x63a6ee8ef44cf13d0d1f393a1e9f8d25da1abfb4',
  marginClearinghouse: '0xfa6e677ec1062757c1194d411a5e61e1e9644499',
  orderRouter: '0x6215d36fcbd610ca1525252eebcbfd8b223a6072',
  orderRouterAdmin: '0xfc18c770468f0dbbfe054d320509895ab77aff07',
  cfdEngine: '0xafece93321be41aa73474457e2f47cf7b2fb738f',
  cfdEnginePlanner: '0x43ee7271b4ba820179d4e1e4f2d8398fe860ad52',
  cfdEngineSettlementSidecar: '0x69dc8489dc12bcfd74b4f453f59ec7381822f5bb',
  cfdEngineAdmin: '0xe83a05b403f227ceee33d5091a7f1f100e4657f0',
  housePool: '0x87622630fb1941fe02731d4a9fcdec0388efd78b',
  seniorVault: '0x970ac2cfe9a19d4318806812719a5c291711b33a',
  juniorVault: '0x2075a46921fc5fbcf5fca808e3a2c66c6f812d79',
  pletherOracle: '0x9f4d9ae736b94249b18a85a7e14092bfca0688eb',
  cfdEngineLens: '0x8fe702213241482d6e94327f9e70195ad183d1ad',
  cfdEngineAccountLens: '0x29fd3b5faf8de6c84405d28e1aac371e46a104c6',
  orderLifecycleBook: '0x753eb48305ffb88bb70869ade2c4efa941879221',
  policyEvaluator: '0x43c93d3028fcd4c1f578a50639750b8fbfdee799',
  positionProtectionBook: '0x3204c51cd567d6490c011399ccbaaf67b5d3d768',
} satisfies PerpsContractAddresses
