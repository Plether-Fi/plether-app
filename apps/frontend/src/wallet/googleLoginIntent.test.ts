import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { clearGoogleLoginIntent, isGoogleLoginReturn, saveGoogleLoginIntent } from './googleLoginIntent'

const key = 'test-google-owner'
const callback = '/?privy_oauth_provider=google&privy_oauth_state=test-state&privy_oauth_code=test-code'
beforeEach(() => {
  sessionStorage.clear()
  window.history.replaceState({}, '', callback)
  vi.spyOn(Date, 'now').mockReturnValue(1_000_000)
})
afterEach(() => { vi.restoreAllMocks(); window.history.replaceState({}, '', '/') })

describe('explicit Google OAuth return', () => {
  it('requires a recent request for the same app and owner', () => {
    expect(isGoogleLoginReturn(key, null)).toBe(false)
    saveGoogleLoginIntent(key, null)
    expect(isGoogleLoginReturn(key, null)).toBe(true)
    expect(isGoogleLoginReturn('other-app', null)).toBe(false)
    expect(isGoogleLoginReturn(key, 'other-owner')).toBe(false)
    clearGoogleLoginIntent(key)
    expect(isGoogleLoginReturn(key, null)).toBe(false)
  })
  it.each(['/', '/?privy_oauth_provider=google', '/?privy_oauth_provider=apple&privy_oauth_state=test&privy_oauth_code=test',
    '/?privy_oauth_provider=google&privy_oauth_state=test&privy_oauth_code=error'])('does not restore a canceled or unrelated callback: %s', url => {
    saveGoogleLoginIntent(key, null)
    window.history.replaceState({}, '', url)
    expect(isGoogleLoginReturn(key, null)).toBe(false)
  })
  it.each([600_001, -1])('rejects an expired or future request (age %s ms)', age => {
    saveGoogleLoginIntent(key, 'owner')
    vi.mocked(Date.now).mockReturnValue(1_000_000 + age)
    expect(isGoogleLoginReturn(key, 'owner')).toBe(false)
  })
  it('ignores malformed storage', () => {
    sessionStorage.setItem(`${key}.google-login`, '{')
    expect(isGoogleLoginReturn(key, null)).toBe(false)
  })
})
