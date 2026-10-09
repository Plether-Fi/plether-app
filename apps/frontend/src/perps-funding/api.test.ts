import { describe, expect, it, vi } from 'vitest'
import { createFundingApi } from './api'
import { BENEFICIARY, OWNER, QUOTE_ID, SOURCE_HASH, SOURCE_USDC, confirmedIntentFixture, intentFixture, needsDepositIntentFixture, quoteFixture, releaseFixture } from './testFixtures'

function response(data: unknown): Response {
  return new Response(JSON.stringify({ data }), { status: 200, headers: { 'Content-Type': 'application/json' } })
}

describe('funding API contract', () => {
  it('sends both the source owner and destination beneficiary with integer base units', async () => {
    const fetcher = vi.fn<typeof fetch>().mockResolvedValue(response(quoteFixture()))
    const api = createFundingApi(fetcher)
    const input = { ownerAddress: OWNER, beneficiary: BENEFICIARY, sourceChainId: 1, sourceToken: SOURCE_USDC, sourceAmount: '100000000' }
    const quoted = await api.quote(input)
    expect(fetcher).toHaveBeenCalledWith('/api/perps/funding/quotes', {
      method: 'POST', cache: 'no-store', credentials: 'same-origin', signal: undefined,
      headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(input),
    })
    expect(quoted).toMatchObject({ quoteId: QUOTE_ID, expiresAt: 2_000_000_000, ownerAddress: OWNER, multicallHandler: releaseFixture().multicallHandler, destinationSpokePool: releaseFixture().destinationSpokePool, destinationMessage: '0x' })
  })

  it('preserves the caller idempotency key for intent creation', async () => {
    const fetcher = vi.fn<typeof fetch>().mockResolvedValue(response(intentFixture()))
    await createFundingApi(fetcher).createIntent(QUOTE_ID, 'durable-intent-key')
    expect(fetcher).toHaveBeenCalledWith('/api/perps/funding/intents', expect.objectContaining({ method: 'POST', body: JSON.stringify({ quoteId: QUOTE_ID, idempotencyKey: 'durable-intent-key' }) }))
  })

  it('reads decimal-string canonical block numbers and strips private server fields', async () => {
    const fetcher = vi.fn<typeof fetch>().mockResolvedValue(response({ ...confirmedIntentFixture(), signedRawTransaction: 'must-not-surface', providerReference: 'provider-secret' }))
    const signal = new AbortController().signal
    const intent = await createFundingApi(fetcher).intent('intent/with?reserved=characters', signal)
    expect(fetcher).toHaveBeenCalledWith('/api/perps/funding/intents/intent%2Fwith%3Freserved%3Dcharacters', expect.objectContaining({ method: 'GET', signal, cache: 'no-store' }))
    expect(intent).toMatchObject({ intentId: 'intent-test-1', depositBlockNumber: '123500', creditedAmount: '99000000' })
    expect(intent).not.toHaveProperty('signedRawTransaction')
    expect(intent).not.toHaveProperty('providerReference')
  })

  it('reports an existing source transaction without creating or repeating a bridge', async () => {
    const fetcher = vi.fn<typeof fetch>().mockResolvedValue(response(intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH })))
    await createFundingApi(fetcher).sourceSubmitted('intent/1', SOURCE_HASH)
    expect(fetcher).toHaveBeenCalledTimes(1)
    expect(fetcher).toHaveBeenCalledWith('/api/perps/funding/intents/intent%2F1/source', expect.objectContaining({ method: 'POST', body: JSON.stringify({ sourceTxHash: SOURCE_HASH }) }))
  })

  it('reconciles only an existing intent and surfaces destination observation errors', async () => {
    const fetcher = vi.fn<typeof fetch>().mockResolvedValue(response({ ...intentFixture({ status: 'needs-deposit' }), lastError: 'DESTINATION_DEPOSIT_NOT_CREDITED' }))
    const intent = await createFundingApi(fetcher).retry('intent/1')
    expect(fetcher).toHaveBeenCalledWith('/api/perps/funding/intents/intent%2F1/retry', expect.objectContaining({ method: 'POST', body: '{}' }))
    expect(intent.reason).toBe('DESTINATION_DEPOSIT_NOT_CREDITED')
  })

  it('loads disabled config without inventing a destination or provider', async () => {
    const fetcher = vi.fn<typeof fetch>().mockResolvedValue(response({ enabled: false, reason: 'No reviewed release configured.' }))
    const signal = new AbortController().signal
    expect(await createFundingApi(fetcher).config(signal)).toEqual({ enabled: false, reason: 'No reviewed release configured.' })
    expect(fetcher).toHaveBeenCalledWith('/api/perps/funding/config', expect.objectContaining({ method: 'GET', signal }))
  })

  it('fails closed on backend errors and never retries a mutating request automatically', async () => {
    const fetcher = vi.fn<typeof fetch>().mockResolvedValue(new Response('unavailable', { status: 503 }))
    await expect(createFundingApi(fetcher).createIntent(QUOTE_ID, 'durable-intent-key')).rejects.toThrow(/503.*has not been repeated/)
    expect(fetcher).toHaveBeenCalledTimes(1)
  })

  it('rejects responses that omit the API envelope or use an unsupported wire schema', async () => {
    const unwrapped = vi.fn<typeof fetch>().mockResolvedValue(new Response(JSON.stringify(intentFixture())))
    await expect(createFundingApi(unwrapped).intent('intent-test-1')).rejects.toThrow()
    const isoExpiry = vi.fn<typeof fetch>().mockResolvedValue(response({ ...quoteFixture(), expiresAt: '2033-05-18T03:33:20Z' }))
    await expect(createFundingApi(isoExpiry).quote({ ownerAddress: OWNER, beneficiary: BENEFICIARY, sourceChainId: 1, sourceToken: SOURCE_USDC, sourceAmount: '100000000' })).rejects.toThrow()
  })
})

it('preserves canonical fallback evidence for a manual deposit without creating another bridge', async () => {
  const fallback = needsDepositIntentFixture()
  const fetcher = vi.fn<typeof fetch>().mockResolvedValue(response(fallback))
  const observed = await createFundingApi(fetcher).intent(fallback.intentId)
  expect(fetcher).toHaveBeenCalledTimes(1)
  expect(fetcher).toHaveBeenCalledWith(`/api/perps/funding/intents/${fallback.intentId}`, expect.objectContaining({ method: 'GET', cache: 'no-store' }))
  expect(observed).toMatchObject({
    status: 'needs-deposit', fallbackTxHash: fallback.fallbackTxHash,
    fallbackBlockHash: fallback.fallbackBlockHash, fallbackBlockNumber: '123500', fallbackAmount: '99000000',
  })
  expect(observed.depositTxHash).toBeUndefined()
  expect(observed.creditedAmount).toBeUndefined()
})
