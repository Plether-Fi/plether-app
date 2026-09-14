import { describe, expect, it } from 'vitest'
import { sanitizedReactException } from './reactErrors'

describe('React exception privacy', () => {
  it('retains exception identity and application frames without provider credentials or causes', () => {
    const error = new TypeError('Cannot read properties of undefined')
    error.stack = `TypeError: ignored\n    at Funding (${location.origin}/assets/app-abc123.js:42:7)\n    at rpc (https://rpc.example/private-key?token=secret:1:2)\nRequest body: secret`
    error.cause = { signature: 'private', request: 'sensitive' }
    const safe = sanitizedReactException(error, `\n    at Funding (${location.origin}/assets/app-abc123.js:42:7)`)
    expect(safe.error.name).toBe('TypeError')
    expect(safe.error.message).toBe(error.message)
    expect(safe.error.stack).toContain('/assets/app-abc123.js:42:7')
    expect(safe.componentStack).toContain('Funding')
    expect(safe.error.cause).toBeUndefined()
    expect(safe.error.stack).not.toMatch(/private-key|secret|rpc\.example/)
  })
  it('redacts URL arguments, credentials, arbitrary hex, email and request payloads', () => {
    const error = new Error(`failure https://rpc.example/key?token=abc 0x123456 user@example.com Bearer secret`)
    const safe = sanitizedReactException(error)
    expect(safe.error.message).not.toMatch(/rpc\.example|0x123456|user@example.com|Bearer secret/)
    expect(sanitizedReactException(new Error('failed signature=private-value')).error.message).not.toContain('private-value')
    expect(sanitizedReactException({ rawRequest: 'secret' }).error.message).toBe('Non-Error React exception')
  })
})
