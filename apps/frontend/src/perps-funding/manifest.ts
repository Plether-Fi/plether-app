import { parseFundingManifest } from './validation'

export function fundingManifestUrl(value: unknown): string | null {
  if (typeof value !== 'string' || !value.trim()) return null
  // Deployment config is same-origin; there is no implicit testnet bridge.
  return /^\/[a-zA-Z0-9/_-]+\.json$/.test(value) ? value : null
}

export async function fetchFundingManifest(url: string, signal?: AbortSignal) {
  const response = await fetch(url, { cache: 'no-cache', credentials: 'omit', signal })
  if (!response.ok) throw new Error('The funding release could not be loaded.')
  return parseFundingManifest(await response.json())
}
