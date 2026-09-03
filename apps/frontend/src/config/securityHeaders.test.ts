import fs from 'node:fs'
import path from 'node:path'
import { describe, expect, it } from 'vitest'

const headers = fs.readFileSync(path.join(process.cwd(), 'public/_headers'), 'utf8')

describe('frontend security headers', () => {
  it('allows the configured PostHog asset host to execute its optional scripts', () => {
    const contentSecurityPolicy = headers
      .split('\n')
      .find((line) => line.trimStart().startsWith('Content-Security-Policy:'))
    const scriptSources = contentSecurityPolicy
      ?.split(';')
      .find((directive) => directive.includes('script-src'))

    expect(scriptSources).toContain('https://eu-assets.i.posthog.com')
  })
})
