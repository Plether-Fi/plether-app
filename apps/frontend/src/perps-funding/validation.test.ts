import { afterEach, describe, expect, it, vi } from 'vitest'
import { fundingManifestUrl, fetchFundingManifest } from './manifest'
import { amount, assertDestination, assertFundingConfig, parseFundingConfig, parseFundingIntent, parseFundingManifest, parseFundingQuote, parseSourceTransaction } from './validation'
import { OTHER_ADDRESS, destinationFixture, intentFixture, needsDepositIntentFixture, quoteFixture, releaseFixture, terminalSourceIntentFixture } from './testFixtures'
import { fundingNeedsDeposit, fundingSourceFailed } from './state'

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
    { multicallHandler: null }, { multicallHandlerCodeHash: null }, { clearinghouse: null },
    { destinationSpokePool: null }, { destinationSpokePoolCodeHash: null },
    { destinationSpokePoolImplementation: null }, { destinationSpokePoolImplementationCodeHash: null },
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
      { multicallHandler: OTHER_ADDRESS }, { multicallHandlerCodeHash: `0x${'55'.repeat(32)}` },
      { destinationSpokePool: OTHER_ADDRESS }, { destinationSpokePoolCodeHash: `0x${'55'.repeat(32)}` },
      { destinationSpokePoolImplementation: OTHER_ADDRESS }, { destinationSpokePoolImplementationCodeHash: `0x${'55'.repeat(32)}` },
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
    { quoteId: undefined }, { quoteId: null }, { quoteId: 'quote-test-1' }, { quoteId: '0x1234' },
    { destinationMessage: undefined }, { destinationMessage: null }, { destinationMessage: '0x123' },
    { multicallHandler: undefined }, { destinationSpokePool: undefined },
    { depositBlockNumber: 123500 }, { depositBlockHash: '0x1234' },
  ])('rejects malformed quote or canonical receipt fields', (override) => {
    expect(() => parseFundingIntent({ ...intentFixture(), ...override })).toThrow()
  })

  it.each(['ownerAddress', 'beneficiary', 'multicallHandler', 'destinationSpokePool', 'clearinghouse', 'token'] as const)('pins %s independently of the connected source network', (field) => {
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

describe('fallback receipt response validation', () => {
  it('retains canonical fallback evidence with decimal-string quantities', () => {
    const fallback = needsDepositIntentFixture()
    const parsed = parseFundingIntent(fallback)
    expect(parsed).toMatchObject({ status: 'needs-deposit', fallbackTxHash: fallback.fallbackTxHash, fallbackBlockHash: fallback.fallbackBlockHash, fallbackBlockNumber: '123500', fallbackAmount: '99000000' })
    expect(fundingNeedsDeposit(parsed)).toBe(true)
  })

  it.each([
    { fallbackTxHash: '0x1234' }, { fallbackBlockHash: '0x1234' },
    { fallbackBlockNumber: 123500 }, { fallbackBlockNumber: '-1' }, { fallbackBlockNumber: '001' },
    { fallbackAmount: 99000000 }, { fallbackAmount: '-1' }, { fallbackAmount: '1.5' },
  ])('rejects malformed canonical fallback fields', override => {
    expect(() => parseFundingIntent({ ...needsDepositIntentFixture(), ...override })).toThrow()
  })

  it.each(['fallbackTxHash', 'fallbackBlockNumber', 'fallbackBlockHash', 'fallbackAmount'] as const)(
    'does not enable deposit recovery when the backend omits %s', field => {
      const parsed = parseFundingIntent({ ...needsDepositIntentFixture(), [field]: undefined })
      expect(fundingNeedsDeposit(parsed)).toBe(false)
    },
  )
})
