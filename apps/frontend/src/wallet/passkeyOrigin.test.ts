import { describe, expect, it } from 'vitest'
import { isPasskeyOriginSupported, localhostPasskeyUrl } from './passkeyOrigin'

describe('passkey origin requirements', () => {
  it.each(['http://127.0.0.1:5192', 'https://127.0.0.1', 'http://[::1]:5192', 'https://[::1]', 'http://app.example.com'])('rejects unsupported origin %s', origin => {
    expect(isPasskeyOriginSupported(new URL(origin))).toBe(false)
  })
  it.each(['http://localhost:5192', 'http://app.localhost:5192', 'https://app.sepolia.plether.com', 'https://insights.plether.com'])('allows passkeys on %s', origin => {
    expect(isPasskeyOriginSupported(new URL(origin))).toBe(true)
  })
  it('offers a localhost link for loopback addresses without transferring OAuth parameters', () => {
    expect(localhostPasskeyUrl(new URL('http://127.0.0.1:5192/privacy?privy_oauth_code=private#private'))).toBe('http://localhost:5192/privacy')
    expect(localhostPasskeyUrl(new URL('http://[::1]:5192/'))).toBe('http://localhost:5192/')
    expect(localhostPasskeyUrl(new URL('http://192.168.1.1/'))).toBeUndefined()
  })
})
