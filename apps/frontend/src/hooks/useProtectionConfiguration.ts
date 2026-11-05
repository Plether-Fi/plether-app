import { useReadContracts } from 'wagmi'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, PERPS_RELEASE_IS_DEPLOYED } from '../contracts/perpsAddresses'
import { PROTECTION_CONFIG_ABI, PROTECTION_RELEASE_ENABLED } from '../contracts/positionProtection'

export interface ProtectionConfiguration { enabled: boolean; triggerBountyAssets?: bigint; executionBountyAssets?: bigint }
export function useProtectionConfiguration(): ProtectionConfiguration {
  const { data } = useReadContracts({
    contracts: (['positionProtectionTriggerBountyAssets'] as const).map(functionName => ({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, address: PERPS_ARBITRUM_SEPOLIA.orderRouter, abi: PROTECTION_CONFIG_ABI, functionName } as const)),
    query: { enabled: PROTECTION_RELEASE_ENABLED && PERPS_RELEASE_IS_DEPLOYED, refetchInterval: 15_000 },
  })
  const triggerBountyAssets = data?.[0]?.result
  const executionBountyAssets = 0n
  // Current closes require no execution bounty; only the trigger reward is reserved.
  return { enabled: PROTECTION_RELEASE_ENABLED && PERPS_RELEASE_IS_DEPLOYED && typeof triggerBountyAssets === 'bigint' && typeof executionBountyAssets === 'bigint', triggerBountyAssets, executionBountyAssets }
}
