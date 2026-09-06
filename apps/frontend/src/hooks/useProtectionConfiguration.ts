import { useReadContracts } from 'wagmi'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { PROTECTION_CONFIG_ABI, PROTECTION_RELEASE_ENABLED } from '../contracts/positionProtection'

export interface ProtectionConfiguration { enabled: boolean; triggerBountyUsdc?: bigint; executionBountyUsdc?: bigint }
export function useProtectionConfiguration(): ProtectionConfiguration {
  const { data } = useReadContracts({
    contracts: (['positionProtectionTriggerBountyUsdc', 'closeOrderExecutionBountyUsdc'] as const).map(functionName => ({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, address: PERPS_ARBITRUM_SEPOLIA.orderRouter, abi: PROTECTION_CONFIG_ABI, functionName } as const)),
    query: { enabled: PROTECTION_RELEASE_ENABLED, refetchInterval: 15_000 },
  })
  const triggerBountyUsdc = data?.[0]?.result
  const executionBountyUsdc = data?.[1]?.result
  // v1.2.2 removed the on-chain commits flag. Wait for both live reserves;
  // the frontend release flag still controls whether this UI is exposed.
  return { enabled: PROTECTION_RELEASE_ENABLED && typeof triggerBountyUsdc === 'bigint' && typeof executionBountyUsdc === 'bigint', triggerBountyUsdc, executionBountyUsdc }
}
