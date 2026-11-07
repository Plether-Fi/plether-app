const MAX_AGE_MS = 10 * 60 * 1000

export function saveGoogleLoginIntent(key: string, owner: string | null | undefined) {
  try {
    sessionStorage.setItem(`${key}.google-login`, JSON.stringify({ owner: owner ?? null, startedAt: Date.now() }))
  } catch { /* Same-page popup login can still complete without storage. */ }
}

export function clearGoogleLoginIntent(key: string) {
  try { sessionStorage.removeItem(`${key}.google-login`) } catch { /* Storage may be unavailable. */ }
}

// A full-page OAuth redirect remounts the provider. Restore only an explicit,
// recent request for this app and unchanged owner, on a Google callback URL.
// Privy remains responsible for validating OAuth state and authentication.
export function isGoogleLoginReturn(key: string, owner: string | null | undefined): boolean {
  try {
    const params = new URLSearchParams(window.location.search)
    if (params.get('privy_oauth_provider') !== 'google' || !params.get('privy_oauth_state')
      || !params.get('privy_oauth_code') || params.get('privy_oauth_code') === 'error') return false
    const intent = JSON.parse(sessionStorage.getItem(`${key}.google-login`) ?? 'null') as { owner?: unknown; startedAt?: unknown } | null
    return intent !== null && intent.owner === (owner ?? null) && typeof intent.startedAt === 'number'
      && Date.now() >= intent.startedAt && Date.now() - intent.startedAt < MAX_AGE_MS
  } catch { return false }
}
