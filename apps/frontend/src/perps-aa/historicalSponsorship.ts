import type { Address, Hex } from 'viem'
import { parsePerpsAaManifest, type PerpsAaDeploymentManifest, type PerpsAaDeploymentManifestV2 } from './manifest'

/** Journal compatibility for USDC grants issued by historical deployments. */
export interface HistoricalCloseAssistance {
  amountUsdc: string
  lens: Address
  lensCodeHash: Hex
  paymasterAddress: Address
}

/** Recover through the journaled paymaster without changing the trading account. */
export function historicalSponsorshipManifest(
  manifest: PerpsAaDeploymentManifest,
  authority: Pick<HistoricalCloseAssistance, 'paymasterAddress'>,
): PerpsAaDeploymentManifestV2 {
  const fields = { ...manifest } as Record<string, unknown>
  delete fields.pimlicoRpcUrl
  return parsePerpsAaManifest({ ...fields,
    bundlerRpcUrl: '/api/perps/v1/aa/rpc', paymasterRpcUrl: '/api/perps/v1/aa/rpc',
    paymasterAddress: authority.paymasterAddress, paymasterVersion: 'plether-verifying-v1', preparationRpcVersion: 1,
  }) as PerpsAaDeploymentManifestV2
}
