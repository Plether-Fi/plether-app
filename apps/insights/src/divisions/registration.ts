import { registrationRequest } from '../api/client'

export interface DivisionsRegistrationSession {
  status: 'in_progress' | 'completed'
  csrfToken: string
  expiresAt: string
  steps: { discord: 'pending' | 'verified'; wallet: 'pending' | 'verified'; completed: boolean }
  identity?: { username: string }
  wallet?: { ownerAddress: string; tradingAccount: string }
  requiredConsents: { rulesVersion: string; privacyVersion: string }
}

interface RegistrationEnvelope { registration: DivisionsRegistrationSession }
interface AuthorizationResponse { authorizationUrl: string }

const base = '/divisions/registrations'

export async function getDivisionsRegistrationSession() {
  return (await registrationRequest<RegistrationEnvelope>(`${base}/session`)).registration
}

export async function createDivisionsRegistrationSession(turnstileToken: string) {
  return (await registrationRequest<RegistrationEnvelope>(`${base}/session`, {
    method: 'POST', body: { turnstileToken },
  })).registration
}

export async function createDiscordAuthorization(csrfToken: string) {
  return (await registrationRequest<AuthorizationResponse>(`${base}/discord/authorize`, {
    method: 'POST', csrfToken, body: {},
  })).authorizationUrl
}

export async function createDivisionsWalletChallenge(csrfToken: string, ownerAddress: string) {
  return registrationRequest<{ message: string }>(`${base}/wallet/challenge`, {
    method: 'POST', csrfToken, body: { ownerAddress },
  })
}

export async function verifyDivisionsWallet(csrfToken: string, ownerAddress: string, signature: string) {
  return (await registrationRequest<RegistrationEnvelope>(`${base}/wallet/verify`, {
    method: 'POST', csrfToken, body: { ownerAddress, signature },
  })).registration
}

export async function completeDivisionsRegistration(session: DivisionsRegistrationSession, showDiscordHandle = false) {
  return (await registrationRequest<RegistrationEnvelope>(`${base}/complete`, {
    method: 'POST', csrfToken: session.csrfToken,
    body: {
      acceptRules: true,
      acceptPrivacy: true,
      showDiscordHandle,
      rulesVersion: session.requiredConsents.rulesVersion,
      privacyVersion: session.requiredConsents.privacyVersion,
    },
  })).registration
}

export function safeDiscordAuthorizationUrl(value: string): string {
  const url = new URL(value)
  const allowed = ['response_type', 'client_id', 'redirect_uri', 'scope', 'state']
  const params = url.searchParams
  const valid = url.origin === 'https://discord.com'
    && url.pathname === '/oauth2/authorize'
    && !url.username && !url.password && !url.hash
    && [...params.keys()].length === allowed.length
    && allowed.every((key) => params.getAll(key).length === 1)
    && [...params.keys()].every((key) => allowed.includes(key))
    && params.get('response_type') === 'code'
    && /^\d{17,22}$/.test(params.get('client_id') ?? '')
    && params.get('redirect_uri') === 'https://insights.plether.com/api/insights/v1/divisions/registrations/discord/callback'
    && params.get('scope') === 'identify'
    && /^[A-Za-z0-9_-]{43}$/.test(params.get('state') ?? '')
  if (!valid) throw new Error('Discord returned an invalid authorization address.')
  return url.toString()
}
