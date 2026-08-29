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
  /** Populated only after the fresh V2 deployment has been verified. */
  orderLifecycleBook?: Address
  /** Populated only after the fresh V2 deployment has been verified. */
  policyEvaluator?: Address
  /** Populated only after the fresh V2 deployment has been verified. */
  positionProtectionBook?: Address
}

export const PERPS_ARBITRUM_SEPOLIA = {
  pyth: '0x0B73614636C855Bf23F342F307FB981A3e47f42B',
  usdc: '0x1647e41f49ED6D688936092B5a291c4B28106343',
  perpsPublicLens: '0xC41e92F541cCF19FA203a96CecF3Ae4D2Ed7F60A',
  marginClearinghouse: '0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211',
  orderRouter: '0x97A901dE2B267c307E264FD5F71403F8072F73e7',
  orderRouterAdmin: '0x3d0e430D670D74988C1B3e76b6ef018e79ab1E37',
  cfdEngine: '0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D',
  cfdEnginePlanner: '0x76E990Cc6d89D7C80E4f05c388d01dab05926a53',
  cfdEngineSettlementSidecar: '0x288F70eC7cF0e16ae4FE4b91B5c266B047c83aFF',
  cfdEngineAdmin: '0xda1240c36f3a4ddcAB3028F66B15Dfe91702dE2A',
  housePool: '0x86939a377A78EDe8EEe5445765ac77c9016E35E2',
  seniorVault: '0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710',
  juniorVault: '0xdf306B52eaC722D5994E2cc93D2818F391d68Adb',
  pletherOracle: '0xC69ec16EfB71F62984E9b2688396F34062277FdC',
  cfdEngineLens: '0x140067daAdd28bE4b04e649EEaCf6F5ECbEe8C79',
  cfdEngineAccountLens: '0x429DA61a7a616DeDD84d2a51eB6Dc1bD72427dC1',
  // PR #71 requires a fresh deployment. These remain deliberately absent
  // until an authoritative deployment artifact passes the binding checks.
  orderLifecycleBook: undefined,
  policyEvaluator: undefined,
  positionProtectionBook: undefined,
} satisfies PerpsContractAddresses
