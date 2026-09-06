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
  usdc: '0xc3CE8590B7EcDE7454f9D5b51a797bbDe96fe56B',
  perpsPublicLens: '0x53B1B00748E7D1A87dc30433e87c331CeDe30149',
  marginClearinghouse: '0xA863F985EedA8BF5BE2320693BB93d109EBB2dBd',
  orderRouter: '0xbd2f286efca5F761E21452673ab9b8C14e17aad7',
  orderRouterAdmin: '0x7447ee8A4a80Fd8668a2dF00F655f2df36D6cCEd',
  cfdEngine: '0x9611E643aC4691E8fDeD8a0c2C22c56438B6f352',
  cfdEnginePlanner: '0x8d5146ed1f8Bd18998235A2DFa26a9a7Bcf15b5F',
  cfdEngineSettlementSidecar: '0x5CBb5A2f75ea005753a6C0AcCE01f7bB02B668D7',
  cfdEngineAdmin: '0xc1c5027a609a1188B745aa04ADcDeEc9Db37ebaE',
  housePool: '0x21D52509Bb9b9857DaBc8c7FD36dD7fed9118918',
  seniorVault: '0x7Bf2B3d3912b5B8D367987C9ADfC6Bd1216E8129',
  juniorVault: '0x41D785d3BcF4D0e306E491a66Ddb0d938135Cc1c',
  pletherOracle: '0x9e7f0a912a9CB3e1c1d77Ed433F171E23E2D7c87',
  cfdEngineLens: '0xE004D20803B484fb62734b78d6144438669Bad18',
  cfdEngineAccountLens: '0xd949E5987c3d33299dA4Da4d06b064729000d2EB',
  orderLifecycleBook: '0x616aD381Df40047e9b060a1E85085B3Ed2CC6D3C',
  policyEvaluator: '0x1ed622ed2Cbd64bd36115dB9D4f4c0006b5894fB',
  positionProtectionBook: '0x35f495fFDbB4d6ae395691D4632629f67603C926',
} satisfies PerpsContractAddresses
