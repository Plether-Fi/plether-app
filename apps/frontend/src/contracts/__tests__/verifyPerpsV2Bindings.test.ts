import { describe, expect, it, vi } from 'vitest'
import type { Address, Hex, PublicClient } from 'viem'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../../perps-aa/manifest'
import { PERPS_ARBITRUM_SEPOLIA } from '../perpsAddresses'
import { verifyPerpsV2DeploymentBindings } from '../verifyPerpsV2Bindings'

const manifest = parsePerpsAaManifest(rawManifest)
const block = {
  number: 302_300_000n,
  timestamp: 2_000_000_000n,
  hash: `0x${'22'.repeat(32)}` as Hex,
}

function bindingClient(override?: { functionName: string; value: Address }) {
  const bindings: Record<string, Address> = {
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

describe('verifyPerpsV2DeploymentBindings', () => {
  it('checks the reverse protection binding in parallel at the same block and returns that snapshot', async () => {
    const { client, getBlock, readContract, releaseForwardBinding } = bindingClient()
    const verification = verifyPerpsV2DeploymentBindings(client, manifest)
    await Promise.resolve()

    expect(readContract).toHaveBeenCalledWith(expect.objectContaining({
      address: manifest.positionProtectionBook,
      functionName: 'ROUTER',
      blockNumber: block.number,
    }))
    expect(readContract).toHaveBeenCalledTimes(14)
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

  it.each(['positionProtectionBook', 'ROUTER'])(
    'still rejects a mismatched %s binding',
    async (functionName) => {
      const { client, releaseForwardBinding } = bindingClient({
        functionName,
        value: '0x0000000000000000000000000000000000000001',
      })
      const verification = verifyPerpsV2DeploymentBindings(client, manifest)
      releaseForwardBinding()
      await expect(verification).rejects.toThrow(functionName === 'ROUTER'
        ? 'Position-protection Router binding mismatch'
        : 'Router position-protection Book binding mismatch')
    }
  )
})
