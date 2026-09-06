import { readFileSync } from 'node:fs'
import { describe, expect, it } from 'vitest'
import { parseGlobalHeaders } from './devServerHeaders'

describe('Vite global response headers', () => {
  it('keeps security headers without applying immutable asset caching to source or HTML', () => {
    const headers = parseGlobalHeaders(readFileSync('public/_headers', 'utf8'))
    expect(headers['Content-Security-Policy']).toContain("default-src 'self'")
    expect(headers['X-Content-Type-Options']).toBe('nosniff')
    expect(headers['X-Frame-Options']).toBe('DENY')
    expect(headers).not.toHaveProperty('Cache-Control')
  })

  it('ignores path-specific overrides and preserves the global policy', () => {
    expect(parseGlobalHeaders('/*\n  Cache-Control: no-cache\n/assets/*\n  Cache-Control: public, max-age=31536000, immutable\n  X-Asset-Only: true'))
      .toEqual({ 'Cache-Control': 'no-cache' })
  })

  it('handles comments, blank lines, CRLF and files without global rules', () => {
    expect(parseGlobalHeaders('# Comment\r\n/*\r\n  X-Content-Type-Options: nosniff\r\n\r\n  # Note\r\n/assets/*\r\n  Cache-Control: immutable'))
      .toEqual({ 'X-Content-Type-Options': 'nosniff' })
    expect(parseGlobalHeaders('/assets/*\n  Cache-Control: immutable')).toEqual({})
  })
})
