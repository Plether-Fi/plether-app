import { isAddressEqual, type Hex } from 'viem'
import type { SponsoredOperation } from './operationStore'
import type { PerpsAaSmartAccountRuntime } from './runtimeContext'
import type { PerpsAaDeploymentManifestV2 } from './manifest'
import type { CloseAssistanceConfig } from './sponsoredClose'

/** Recovery uses the journaled authority, independently of today's issuance flag. */
export function withCloseAssistanceRecovery(
  base: PerpsAaSmartAccountRuntime,
  operations: () => SponsoredOperation[],
  nativeRuntime: (config: Pick<CloseAssistanceConfig, 'paymasterAddress'>) => Promise<PerpsAaSmartAccountRuntime>,
  preparedRuntime?: (manifest: PerpsAaDeploymentManifestV2) => Promise<PerpsAaSmartAccountRuntime>,
): PerpsAaSmartAccountRuntime {
  const cache = new Map<string, Promise<PerpsAaSmartAccountRuntime>>()
  const forPreparation = async (manifest: PerpsAaDeploymentManifestV2) => {
    if (!preparedRuntime) throw new Error('Native preparation recovery is unavailable')
    if (manifest.chainId !== base.chainId || !isAddressEqual(manifest.entryPoint, base.smartAccount.entryPoint)
      || !isAddressEqual(manifest.smartAccountFactory, base.factoryAddress)
      || manifest.smartAccountVersion !== base.accountVersion || manifest.smartAccountIndex !== base.accountIndex) {
      throw new Error('The prepared deployment identity changed')
    }
    const key = JSON.stringify(manifest)
    let pending = cache.get(key)
    if (!pending) {
      pending = preparedRuntime(manifest).then(runtime => {
        if (runtime.chainId !== base.chainId || !isAddressEqual(runtime.ownerAddress, base.ownerAddress)
          || !isAddressEqual(runtime.smartAccount.accountAddress, base.smartAccount.accountAddress)) throw new Error('The prepared account changed')
        return runtime
      }).catch((error: unknown) => { cache.delete(key); throw error })
      cache.set(key, pending)
    }
    return pending
  }
  const forHash = async (hash: Hex) => {
    const operation = operations().find(item => item.userOperationHash?.toLowerCase() === hash.toLowerCase()
      && item.chainId === base.chainId && isAddressEqual(item.ownerAddress, base.ownerAddress)
      && isAddressEqual(item.accountAddress, base.smartAccount.accountAddress))
    if (!operation) return base
    if (operation.nativePreparation) {
      const manifest = operation.nativePreparation.manifest
      if (!operation.sponsorshipAuthority || !isAddressEqual(operation.sponsorshipAuthority.paymasterAddress, manifest.paymasterAddress)) {
        throw new Error('Native recovery authority does not match its journal')
      }
      return forPreparation(manifest)
    }
    const funding = operation.orderRequestV2?.closeAssistance
    const authority = funding ?? operation.sponsorshipAuthority
    if (!authority) return base
    if (!operation.sponsorshipAuthority || !isAddressEqual(operation.sponsorshipAuthority.paymasterAddress, authority.paymasterAddress)) {
      throw new Error('Sponsored close recovery authority does not match its reviewed funding')
    }
    const key = authority.paymasterAddress.toLowerCase()
    let pending = cache.get(key)
    if (!pending) {
      pending = nativeRuntime(funding ?? { paymasterAddress: authority.paymasterAddress }).then(runtime => {
        if (runtime.chainId !== base.chainId || !isAddressEqual(runtime.ownerAddress, base.ownerAddress)
          || !isAddressEqual(runtime.smartAccount.accountAddress, base.smartAccount.accountAddress)
          || !isAddressEqual(runtime.smartAccount.entryPoint, base.smartAccount.entryPoint)) {
          throw new Error('Sponsored close recovery account changed')
        }
        return runtime
      }).catch((error: unknown) => { cache.delete(key); throw error })
      cache.set(key, pending)
    }
    return pending
  }
  return { ...base,
    ...(preparedRuntime ? { getPreparedOperationRuntime: forPreparation } : {}),
    smartAccount: { ...base.smartAccount,
      getUserOperationReceipt: async hash => (await forHash(hash)).smartAccount.getUserOperationReceipt(hash),
      getUserOperationStatus: async hash => (await forHash(hash)).smartAccount.getUserOperationStatus(hash),
    },
    getRecoverySnapshot: async (hash, nonceKey, context) => {
      const runtime = await forHash(hash)
      if (!runtime.getRecoverySnapshot) throw new Error('Safe recovery is unavailable')
      return runtime.getRecoverySnapshot(hash, nonceKey, context)
    },
  }
}
