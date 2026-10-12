import { afterEach, describe, expect, it, vi } from 'vitest'
import { type Address, type Hex } from 'viem'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../manifest'
import { buildSponsoredCloseAction, closeAssistanceManifest, loadCloseAssistanceConfig, type SponsoredCloseFunding } from '../sponsoredClose'
import { permissivePerpsExecutionBounds, type PerpsOrderRequestV3 } from '../../contracts/perpsOrderV3'

const manifest = parsePerpsAaManifest({ ...rawManifest, orderInterfaceVersion: 3 })
const account = '0x1111111111111111111111111111111111111111' as Address
const request: PerpsOrderRequestV3 = {
  clientOrderId: `0x${'11'.repeat(32)}`, side: 0, sizeDelta: 100n * 10n ** 18n,
  marginDelta: 0n, targetPrice: 101_000_000n, isClose: true,
  bounds: { ...permissivePerpsExecutionBounds({ submitBy: 2_000_000_000n, executionWindowSeconds: 60,
    expectedConfigHash: `0x${'22'.repeat(32)}`, executionBountyUsdc: 200_000n }), allowedExecutionModes: 1 },
}
const funding: SponsoredCloseFunding = { amountUsdc: 198_000n, depositCarryUsdc: 42n, commitmentCarryUsdc: 0n,
  config: { lens: '0x3333333333333333333333333333333333333333', lensCodeHash: `0x${'44'.repeat(32)}` as Hex,
    paymasterAddress: '0x5555555555555555555555555555555555555555' } }

describe('sponsored close action', () => {
  it('rejects subsidy batches for the replacement deployment', () => {
    expect(() => buildSponsoredCloseAction(manifest, account, request, funding)).toThrow('Close assistance is retired')
  })

  it('rejects opening orders and zero or excessive grants', () => {
    expect(() => buildSponsoredCloseAction(manifest, account, { ...request, isClose: false }, funding)).toThrow()
    for (const amountUsdc of [0n, -1n, 200_001n]) {
      expect(() => buildSponsoredCloseAction(manifest, account, request, { ...funding, amountUsdc })).toThrow()
    }
  })

  it('uses the native route without changing the trading account profile', () => {
    const assisted = closeAssistanceManifest(manifest, funding.config)
    expect(assisted.paymasterRpcUrl).toBe('/api/perps/v1/aa/rpc')
    expect(assisted.smartAccountFactory).toBe(manifest.smartAccountFactory)
    expect(assisted.orderRouter).toBe(manifest.orderRouter)
    expect('pimlicoRpcUrl' in assisted).toBe(false)
  })
})

describe('close assistance availability', () => {
  afterEach(() => vi.unstubAllGlobals())
  const serve = (body: unknown) => vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify(body), { status: 200 })))
  const enabled = () => ({ enabled: true, chainId: 421614, ...funding.config, canaryOwners: [] })

  it('limits canary availability to its owners independently of global gas sponsorship', async () => {
    serve({ ...enabled(), canaryOwners: [account] })
    expect(await loadCloseAssistanceConfig(account)).toEqual(funding.config)
    expect(await loadCloseAssistanceConfig(funding.config.lens)).toBeUndefined()
  })
  it('supports all testnet owners without a competition-date cutoff', async () => {
    serve(enabled())
    expect(await loadCloseAssistanceConfig(account)).toEqual(funding.config)
    expect(await loadCloseAssistanceConfig(funding.config.lens)).toEqual(funding.config)
  })
  it('returns ordinary-close availability when issuance is disabled', async () => {
    serve({ enabled: false })
    expect(await loadCloseAssistanceConfig(account)).toBeUndefined()
  })
  it('rejects malformed or cross-chain configuration', async () => {
    for (const invalid of [null, { ...enabled(), chainId: 42161 }, { ...enabled(), canaryOwners: [7] }, { ...enabled(), lensCodeHash: '0x12' }]) {
      serve(invalid)
      await expect(loadCloseAssistanceConfig(account)).rejects.toThrow('Invalid close assistance')
    }
  })
})
