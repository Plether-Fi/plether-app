import { afterEach, describe, expect, it, vi } from 'vitest'
import { fundingManifestUrl, fetchFundingManifest } from './manifest'
import { amount, assertDestination, assertFundingConfig, parseFundingConfig, parseFundingIntent, parseFundingManifest, parseFundingQuote, parseSourceTransaction } from './validation'
import { OTHER_ADDRESS, destinationFixture, intentFixture, quoteFixture, releaseFixture, terminalSourceIntentFixture } from './testFixtures'
import { fundingSourceFailed } from './state'

afterEach(() => vi.unstubAllGlobals())

describe('explicit funding release gate', () => {
  it.each([undefined, null, '', ' ', 'https://example.com/funding.json', '//example.com/funding.json', '/funding.json?release=1', '/funding/../release.json', '/funding.json#test'])('does not enable a route from %s', (url) => {
    expect(fundingManifestUrl(url)).toBeNull()
  })

  it('accepts an explicit same-origin JSON path', () => {
    expect(fundingManifestUrl('/releases/perps-funding-reviewed.json')).toBe('/releases/perps-funding-reviewed.json')
  })

  it('reads the canonical release schema including additional deployment evidence', () => {
    const manifest = parseFundingManifest({ ...releaseFixture(), evidence: { schema: 'plether-perps-bridge-funding', schemaVersion: 1 } })
    expect(manifest).toMatchObject({ destinationChainId: 42161, provider: 'across', confirmations: 12, startBlock: 123456 })
    expect(manifest.sources.map(({ chainId, symbol }) => ({ chainId, symbol }))).toEqual([{ chainId: 1, symbol: 'USDC' }, { chainId: 1, symbol: 'USDT' }])
  })

  it.each([
    { destinationChainId: 421614 }, { destinationChainId: 1 }, { token: OTHER_ADDRESS },
    { receiverFactory: null }, { factoryCodeHash: null }, { clearinghouse: null },
    { clearinghouseCodeHash: undefined }, { clearinghouseCodeHash: null }, { clearinghouseCodeHash: '0x1234' },
    { confirmations: 0 }, { confirmations: 1.5 }, { confirmations: 1001 },
    { startBlock: -1 }, { startBlock: '123456' }, { releaseId: '' },
  ])('rejects incomplete templates and unsupported routes', (override) => {
    expect(() => parseFundingManifest({ ...releaseFixture(), ...override })).toThrow()
  })

  it('requires matching browser and backend release pins before enabling funding', () => {
    const manifest = parseFundingManifest(releaseFixture())
    const config = { ...releaseFixture(), provider: 'across', enabled: true }
    expect(() => assertFundingConfig(manifest, parseFundingConfig(config))).not.toThrow()
    for (const drift of [
      { enabled: false }, { provider: 'privy' }, { destinationChainId: 421614 },
      { releaseId: 'new-release' }, { token: OTHER_ADDRESS }, { clearinghouse: OTHER_ADDRESS },
      { receiverFactory: OTHER_ADDRESS }, { factoryCodeHash: `0x${'55'.repeat(32)}` },
      { clearinghouseCodeHash: `0x${'77'.repeat(32)}` },
      { confirmations: 13 }, { startBlock: 123457 },
    ]) expect(() => assertFundingConfig(manifest, parseFundingConfig({ ...config, ...drift }))).toThrow()
  })

  it('revalidates public release files without forwarding credentials', async () => {
    const fetcher = vi.fn<typeof fetch>().mockResolvedValue(new Response(JSON.stringify(releaseFixture())))
    vi.stubGlobal('fetch', fetcher)
    const signal = new AbortController().signal
    await fetchFundingManifest('/releases/funding.json', signal)
    expect(fetcher).toHaveBeenCalledWith('/releases/funding.json', { cache: 'no-cache', credentials: 'omit', signal })
  })
})

describe('funding response validation', () => {
  it.each([1, -1, 1.5, '', '01', '-1', '1.0', '1e6', ' 10 ', (2n ** 256n).toString()])('rejects noncanonical token amount %s', (value) => {
    expect(() => amount(value)).toThrow()
  })

  it('retains integer token quantities without floating point conversion', () => {
    expect(amount((2n ** 256n - 1n).toString())).toBe((2n ** 256n - 1n).toString())
    expect(parseFundingQuote(quoteFixture())).toMatchObject({ sourceAmount: '100000000', expiresAt: 2_000_000_000 })
  })

  it.each([
    { expiresAt: '2000000000' }, { expiresAt: 2_000_000_000_000 },
    { sourceAmount: '100.0' }, { status: 'delivered' }, { intentId: undefined },
    { intentSalt: undefined }, { intentSalt: null }, { intentSalt: '0x1234' },
    { depositBlockNumber: 123500 }, { depositBlockHash: '0x1234' },
  ])('rejects malformed quote or canonical receipt fields', (override) => {
    expect(() => parseFundingIntent({ ...intentFixture(), ...override })).toThrow()
  })

  it.each(['ownerAddress', 'beneficiary', 'receiverFactory', 'clearinghouse', 'token'] as const)('pins %s independently of the connected source network', (field) => {
    expect(() => assertDestination({ ...quoteFixture(), [field]: OTHER_ADDRESS }, destinationFixture())).toThrow()
  })

  it('rejects unknown transaction operations, malformed hex, and native-value floats', () => {
    const transaction = { kind: 'bridge', chainId: 1, to: OTHER_ADDRESS, data: '0x1234', value: '0' }
    expect(parseSourceTransaction(transaction)).toMatchObject(transaction)
    for (const drift of [{ kind: 'arbitrary-call' }, { chainId: '1' }, { data: '0x123' }, { value: '0.0' }]) {
      expect(() => parseSourceTransaction({ ...transaction, ...drift })).toThrow()
    }
  })

  it('accepts zero confirmations for a source transaction that is still pending', () => {
    expect(parseFundingIntent({ ...intentFixture(), sourceStatus: 'pending', sourceTerminal: false, sourceConfirmations: 0 })).toMatchObject({ sourceStatus: 'pending', sourceTerminal: false, sourceConfirmations: 0 })
  })

  it.each([
    { sourceConfirmations: -1 }, { sourceConfirmations: 1.5 }, { sourceConfirmations: '2' },
    { sourceConfirmations: Number.MAX_SAFE_INTEGER + 1 }, { sourceBlockNumber: 23000000 },
    { sourceBlockNumber: '-1' }, { sourceBlockHash: '0x1234' },
  ])('rejects malformed source receipt evidence', (override) => {
    expect(() => parseFundingIntent({ ...terminalSourceIntentFixture(), ...override })).toThrow()
  })

  it('does not interpret a string terminal flag or unknown source state as definitive failure', () => {
    expect(fundingSourceFailed(parseFundingIntent({ ...terminalSourceIntentFixture(), sourceTerminal: 'true' }))).toBe(false)
    expect(fundingSourceFailed(parseFundingIntent({ ...terminalSourceIntentFixture(), sourceStatus: 'provider-failed' }))).toBe(false)
  })
})
