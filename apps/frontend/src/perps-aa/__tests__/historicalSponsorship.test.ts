import { describe, expect, it } from 'vitest'
import type { Address } from 'viem'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../manifest'
import { historicalSponsorshipManifest } from '../historicalSponsorship'

const manifest = parsePerpsAaManifest({ ...rawManifest, orderInterfaceVersion: 3 })
const paymasterAddress = '0x3333333333333333333333333333333333333333' as Address

describe('historical sponsorship manifest', () => {
  it('uses the journaled authority while preserving deployment and account identity', () => {
    const recovered = historicalSponsorshipManifest(manifest, { paymasterAddress })
    expect(recovered).toEqual({ ...manifest, paymasterAddress })
    expect(manifest).toEqual(parsePerpsAaManifest({ ...rawManifest, orderInterfaceVersion: 3 }))
  })

  it('recovers legacy RPC journals even when new sponsorship is disabled', () => {
    const fields = { ...rawManifest } as Record<string, unknown>
    for (const name of ['bundlerRpcUrl', 'paymasterRpcUrl', 'paymasterAddress', 'paymasterVersion', 'preparationRpcVersion']) delete fields[name]
    const legacy = parsePerpsAaManifest({ ...fields, pimlicoRpcUrl: '/api/perps/v1/aa/pimlico', sponsorshipEnabled: false })
    const recovered = historicalSponsorshipManifest(legacy, { paymasterAddress })
    expect(recovered).toEqual({ ...manifest, paymasterAddress, sponsorshipEnabled: false })
    expect('pimlicoRpcUrl' in recovered).toBe(false)
  })
})
