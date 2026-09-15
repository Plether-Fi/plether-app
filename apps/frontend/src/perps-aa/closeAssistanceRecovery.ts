import { isAddressEqual, type Hex } from 'viem'
import type { SponsoredOperation } from './operationStore'
import type { PerpsAaSmartAccountRuntime } from './runtimeContext'
import type { CloseAssistanceConfig } from './sponsoredClose'

/** Recovery uses the journaled authority, independently of today's issuance flag. */
export function withCloseAssistanceRecovery(
  base: PerpsAaSmartAccountRuntime,
  operations: () => SponsoredOperation[],
  nativeRuntime: (config: CloseAssistanceConfig) => Promise<PerpsAaSmartAccountRuntime>,
): PerpsAaSmartAccountRuntime {
  const cache = new Map<string, Promise<PerpsAaSmartAccountRuntime>>()
  const forHash = async (hash: Hex) => {
    const operation = operations().find(item => item.userOperationHash?.toLowerCase() === hash.toLowerCase()
      && item.chainId === base.chainId && isAddressEqual(item.ownerAddress, base.ownerAddress)
      && isAddressEqual(item.accountAddress, base.smartAccount.accountAddress))
    const funding = operation?.orderRequestV2?.closeAssistance
    if (!funding) return base
    if (!operation.sponsorshipAuthority || !isAddressEqual(operation.sponsorshipAuthority.paymasterAddress, funding.paymasterAddress)) {
      throw new Error('Sponsored close recovery authority does not match its reviewed funding')
    }
    const key = funding.paymasterAddress.toLowerCase()
    let pending = cache.get(key)
    if (!pending) {
      pending = nativeRuntime(funding).then(runtime => {
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
    smartAccount: { ...base.smartAccount,
      getUserOperationReceipt: async hash => (await forHash(hash)).smartAccount.getUserOperationReceipt(hash),
      getUserOperationStatus: async hash => (await forHash(hash)).smartAccount.getUserOperationStatus(hash),
    },
    getRecoverySnapshot: async (hash, nonceKey) => {
      const runtime = await forHash(hash)
      if (!runtime.getRecoverySnapshot) throw new Error('Safe recovery is unavailable')
      return runtime.getRecoverySnapshot(hash, nonceKey)
    },
  }
}
