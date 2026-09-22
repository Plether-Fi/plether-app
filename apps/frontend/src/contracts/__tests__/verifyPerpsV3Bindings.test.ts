import { describe, expect, it, vi } from 'vitest'
import type { Address, Hex, PublicClient } from 'viem'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../../perps-aa/manifest'
import { PERPS_ARBITRUM_SEPOLIA } from '../perpsAddresses'
import { ORDER_V3_INTENT_TYPEHASH, verifyPerpsV3DeploymentBindings } from '../verifyPerpsV3Bindings'

const manifest = parsePerpsAaManifest({ ...rawManifest, orderInterfaceVersion: 3 })
const block = {
  number: 302_300_000n,
  timestamp: 2_000_000_000n,
  hash: `0x${'22'.repeat(32)}` as Hex,
}

function bindingClient(override?: { functionName: string; value: Address }) {
  const bindings: Record<string, Address> = {
    INTENT_TYPEHASH: ORDER_V3_INTENT_TYPEHASH,
    engine: manifest.cfdEngine,
    lifecycleBook: manifest.orderLifecycleBook,
    policyEvaluator: manifest.policyEvaluator,
    positionProtectionBook: manifest.positionProtectionBook,
    ROUTER: manifest.orderRouter,
    ENGINE: manifest.cfdEngine,
    CLEARINGHOUSE: manifest.marginClearinghouse,
    HOUSE_POOL: PERPS_ARBITRUM_SEPOLIA.housePool,
    clearinghouse: manifest.marginClearinghouse,
    pool: PERPS_ARBITRUM_SEPOLIA.housePool,
    ORDER_ROUTER: manifest.orderRouter,
  }
  // Hold the forward binding pending to prove the reverse check starts without
  // waiting for it. Both checks must still succeed before verification returns.
  let releaseForwardBinding!: () => void
  const forwardBindingReady = new Promise<void>((resolve) => {
    releaseForwardBinding = resolve
  })
  const readContract = vi.fn(async ({ address, functionName }: { address: Address; functionName: string }) => {
    if (functionName === 'positionProtectionBook') await forwardBindingReady
    const shouldOverride = override?.functionName === functionName &&
      (functionName !== 'ROUTER' || address === manifest.positionProtectionBook)
    return shouldOverride ? override!.value : bindings[functionName]
  })
  const getBlock = vi.fn(async () => block)
  const client = { getBlock, readContract } as unknown as PublicClient
  return { client, getBlock, readContract, releaseForwardBinding }
}

describe('verifyPerpsV3DeploymentBindings', () => {
  it('checks the reverse protection binding in parallel at the same block and returns that snapshot', async () => {
    const { client, getBlock, readContract, releaseForwardBinding } = bindingClient()
    const verification = verifyPerpsV3DeploymentBindings(client, manifest)
    await Promise.resolve()

    expect(readContract).toHaveBeenCalledWith(expect.objectContaining({
      address: manifest.positionProtectionBook,
      functionName: 'ROUTER',
      blockNumber: block.number,
    }))
    expect(readContract).toHaveBeenCalledTimes(15)
    for (const [request] of readContract.mock.calls) {
      expect(request).toMatchObject({ blockNumber: block.number })
    }

    releaseForwardBinding()
    await expect(verification).resolves.toEqual({
      positionProtectionBook: manifest.positionProtectionBook,
      blockNumber: block.number,
      block,
    })
    expect(getBlock).toHaveBeenCalledExactlyOnceWith({ blockTag: 'latest' })
  })

  it('rejects a V2 graph even if its manifest is relabeled V3', async () => {
    const { client, releaseForwardBinding } = bindingClient({ functionName: 'INTENT_TYPEHASH', value: `0x${'00'.repeat(32)}` })
    const verification = verifyPerpsV3DeploymentBindings(client, manifest)
    releaseForwardBinding()
    await expect(verification).rejects.toThrow('V3 signed order interface')
  })

  it.each(['positionProtectionBook', 'ROUTER'])(
    'still rejects a mismatched %s binding',
    async (functionName) => {
      const { client, releaseForwardBinding } = bindingClient({
        functionName,
        value: '0x0000000000000000000000000000000000000001',
      })
      const verification = verifyPerpsV3DeploymentBindings(client, manifest)
      releaseForwardBinding()
      await expect(verification).rejects.toThrow(functionName === 'ROUTER'
        ? 'Position-protection Router binding mismatch'
        : 'Router position-protection Book binding mismatch')
    }
  )
})
