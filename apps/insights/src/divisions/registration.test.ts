import { afterEach, describe, expect, it, vi } from 'vitest'
import {
  completeDivisionsRegistration,
  createDiscordAuthorization,
  safeDiscordAuthorizationUrl,
  type DivisionsRegistrationSession,
} from './registration'

const callback = 'https://insights.plether.com/api/insights/v1/divisions/registrations/discord/callback'
const state = 'a'.repeat(43)
const validUrl = `https://discord.com/oauth2/authorize?response_type=code&client_id=123456789012345678&redirect_uri=${encodeURIComponent(callback)}&scope=identify&state=${state}`

afterEach(() => { vi.unstubAllGlobals() })

describe('Divisions Discord registration', () => {
  it('accepts only the intended Discord OAuth request', () => {
    expect(safeDiscordAuthorizationUrl(validUrl)).toBe(validUrl)
    for (const unsafe of [
      validUrl.replace('discord.com', 'discord.com.attacker.example'),
      validUrl.replace('scope=identify', 'scope=identify%20guilds.join'),
      validUrl.replace(encodeURIComponent(callback), encodeURIComponent('https://attacker.example/callback')),
      `${validUrl}&state=${state}`,
      `${validUrl}&prompt=none`,
    ]) expect(() => safeDiscordAuthorizationUrl(unsafe)).toThrow('invalid authorization address')
  })

  it('sends the session CSRF token and consents only to the first-party API', async () => {
    const session: DivisionsRegistrationSession = {
      status: 'in_progress', csrfToken: 'csrf', expiresAt: '2026-10-19T12:00:00Z',
      steps: { discord: 'verified', wallet: 'verified', completed: false },
      capabilities: { publicDiscordHandle: true },
      requiredConsents: { rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1' },
    }
    const fetchMock = vi.fn()
      .mockResolvedValueOnce(Response.json({ authorizationUrl: validUrl }))
      .mockResolvedValueOnce(Response.json({ registration: session }))
    vi.stubGlobal('fetch', fetchMock)

    await expect(createDiscordAuthorization('csrf')).resolves.toBe(validUrl)
    await expect(completeDivisionsRegistration(session)).resolves.toEqual(session)
    for (const call of fetchMock.mock.calls) {
      expect(call[0]).toMatch(/^\/api\/insights\/v1\/divisions\/registrations\//)
      expect(call[1].credentials).toBe('include')
      expect(call[1].headers.get('X-Registration-CSRF')).toBe('csrf')
    }
    expect(JSON.parse(fetchMock.mock.calls[1][1].body)).toEqual({
      acceptRules: true, acceptPrivacy: true, showDiscordHandle: false,
      rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1',
    })
    fetchMock.mockResolvedValueOnce(Response.json({ registration: session }))
    await completeDivisionsRegistration(session, true)
    expect(JSON.parse(fetchMock.mock.calls[2][1].body).showDiscordHandle).toBe(true)
    const legacySession = { ...session, capabilities: undefined }
    fetchMock.mockResolvedValueOnce(Response.json({ registration: legacySession }))
    await completeDivisionsRegistration(legacySession)
    expect(JSON.parse(fetchMock.mock.calls[3][1].body)).not.toHaveProperty('showDiscordHandle')
    await expect(completeDivisionsRegistration(legacySession, true)).rejects.toThrow('not available yet')
    expect(fetchMock).toHaveBeenCalledTimes(4)
  })
})
