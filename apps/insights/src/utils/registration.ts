import { InsightsApiError } from '../api'

const ERROR_MESSAGES: Record<string, string> = {
  CLOSED_REGISTRATION: 'Registration is closed for this competition.',
  EXPIRED_SESSION: 'Your registration session expired. Complete the spam check again to restart.',
  EXPIRED_CHALLENGE: 'This verification challenge expired. Restart the current step and try again.',
  X_EMAIL_UNVERIFIED: 'Your X account does not provide a confirmed email address.',
  X_ACCOUNT_TOO_NEW: 'This X account is too new to enter this competition.',
  X_FOLLOW_REQUIRED: 'We could not confirm that you follow @plether_fi. Follow the account on X, then try verification again.',
  DUPLICATE_REGISTRATION: 'One of these verified identities is already registered for this competition.',
  TRADING_ACCOUNT_EXISTS: 'Wallet verification changed. Verify the wallet again.',
  INVALID_SIGNATURE: 'The wallet signature could not be verified. Try signing a fresh challenge.',
  RATE_LIMITED: 'Too many attempts. Wait a moment before trying again.',
  PROVIDER_UNAVAILABLE: 'A verification provider is temporarily unavailable. Please try again shortly.',
  INVALID_REQUEST: 'This registration request is no longer valid. Refresh the page and try again.',
}

export function registrationErrorCodeMessage(code: string): string {
  return ERROR_MESSAGES[code] ?? 'X verification could not be completed. Please try again.'
}

const X_CALLBACK_URL = 'https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/callback'
const X_OAUTH_PARAMETERS = [
  'response_type',
  'client_id',
  'redirect_uri',
  'scope',
  'state',
  'code_challenge',
  'code_challenge_method',
] as const
const X_OAUTH_SCOPES = ['follows.read', 'tweet.read', 'users.email', 'users.read']

export function safeXAuthorizationUrl(value: string): string {
  const url = new URL(value)
  const validEndpoint = url.origin === 'https://x.com'
    && url.pathname === '/i/oauth2/authorize'
    && url.username === ''
    && url.password === ''
    && url.port === ''
    && url.hash === ''
  const parametersAreUnique = X_OAUTH_PARAMETERS.every(
    (name) => url.searchParams.getAll(name).length === 1,
  ) && [...url.searchParams.keys()].every(
    (name) => X_OAUTH_PARAMETERS.includes(name as typeof X_OAUTH_PARAMETERS[number]),
  ) && [...url.searchParams.keys()].length === X_OAUTH_PARAMETERS.length
  const state = url.searchParams.get('state') ?? ''
  const challenge = url.searchParams.get('code_challenge') ?? ''
  const scopes = (url.searchParams.get('scope') ?? '').split(' ').filter(Boolean).sort()
  const validProtocol = url.searchParams.get('response_type') === 'code'
    && url.searchParams.get('code_challenge_method') === 'S256'
    && (url.searchParams.get('client_id')?.length ?? 0) > 0
    && url.searchParams.get('redirect_uri') === X_CALLBACK_URL
    && /^[A-Za-z0-9_-]{43}$/.test(state)
    && /^[A-Za-z0-9_-]{43}$/.test(challenge)
    && scopes.length === X_OAUTH_SCOPES.length
    && scopes.every((scope, index) => scope === X_OAUTH_SCOPES[index])
  if (!validEndpoint || !parametersAreUnique || !validProtocol) {
    throw new Error('X returned an invalid authorization address.')
  }
  return url.toString()
}

export function registrationErrorMessage(error: unknown): string {
  if (error instanceof InsightsApiError) {
    const base = error.code ? ERROR_MESSAGES[error.code] : undefined
    if (base && error.retryAfterSeconds !== null) {
      return `${base} Try again in ${String(error.retryAfterSeconds)} seconds.`
    }
    return base ?? error.message
  }

  if (error instanceof Error) {
    const normalized = `${error.name} ${error.message}`.toLowerCase()
    if (normalized.includes('user rejected') || normalized.includes('rejected the request')) {
      return 'The request was rejected in your wallet. Try again when you are ready.'
    }
    return error.message
  }

  return 'Something went wrong. Please try again.'
}
