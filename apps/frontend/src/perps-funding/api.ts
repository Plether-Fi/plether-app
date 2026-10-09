import type { Address, Hex } from 'viem'
import { parseFundingConfig, parseFundingIntent, parseFundingQuote, record } from './validation'

const FUNDING_API = '/api/perps/funding'

/** Never sends provider credentials or authorization material from the browser. */
export function createFundingApi(fetcher: typeof fetch = globalThis.fetch) {
  async function request(path: string, body?: unknown, signal?: AbortSignal): Promise<unknown> {
    const response = await fetcher(`${FUNDING_API}${path}`, {
      method: body === undefined ? 'GET' : 'POST',
      cache: 'no-store', credentials: 'same-origin', signal,
      ...(body === undefined ? {} : { headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) }),
    })
    if (!response.ok) throw new Error(`Funding request failed (${response.status.toString()}). Your existing transfer has not been repeated.`)
    return record(await response.json()).data
  }
  return {
    config: async (signal?: AbortSignal) => parseFundingConfig(await request('/config', undefined, signal)),
    quote: async (input: { ownerAddress: Address; beneficiary: Address; sourceChainId: number; sourceToken: Address; sourceAmount: string }) => parseFundingQuote(await request('/quotes', input)),
    createIntent: async (quoteId: string, idempotencyKey: string) => parseFundingIntent(await request('/intents', { quoteId, idempotencyKey })),
    intent: async (id: string, signal?: AbortSignal) => parseFundingIntent(await request(`/intents/${encodeURIComponent(id)}`, undefined, signal)),
    retry: async (id: string) => parseFundingIntent(await request(`/intents/${encodeURIComponent(id)}/retry`, {})),
    sourceSubmitted: async (id: string, sourceTxHash: Hex) => parseFundingIntent(await request(`/intents/${encodeURIComponent(id)}/source`, { sourceTxHash })),
  }
}
export type FundingApi = ReturnType<typeof createFundingApi>
