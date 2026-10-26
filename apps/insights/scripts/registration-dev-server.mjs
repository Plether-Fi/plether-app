import { createHash, randomBytes } from 'node:crypto'
import { createServer } from 'node:http'
import { getAddress, isAddress, recoverMessageAddress } from 'viem'

const host = '127.0.0.1'
const port = Number.parseInt(process.env.INSIGHTS_REGISTRATION_DEV_PORT ?? '3003', 10)
const cookieName = 'plether_registration_dev'
const sessions = new Map()
const divisionsSessions = new Map()
const divisionsCookie = 'plether_divisions_dev'
const routePattern = /^\/api\/insights\/v1\/competitions\/([^/]+)\/registrations\/(session|x\/authorize|x\/callback|x\/follow|wallet\/challenge|wallet\/verify|complete)$/
const divisionsRoutePattern = /^\/api\/insights\/v1\/divisions\/registrations\/(session|discord\/authorize|discord\/callback|wallet\/challenge|wallet\/verify|complete)$/

function opaque(bytes = 32) {
  return randomBytes(bytes).toString('base64url')
}

function json(response, status, body, headers = {}) {
  response.writeHead(status, {
    'Cache-Control': 'no-store',
    'Content-Type': 'application/json; charset=utf-8',
    ...headers,
  })
  response.end(JSON.stringify(body))
}

function error(response, status, code, message) {
  json(response, status, { error: { code, message } })
}

function cookies(request) {
  return new Map(
    (request.headers.cookie ?? '')
      .split(';')
      .map((part) => part.trim())
      .filter(Boolean)
      .map((part) => {
        const separator = part.indexOf('=')
        return separator < 0 ? [part, ''] : [part.slice(0, separator), part.slice(separator + 1)]
      }),
  )
}

function sessionFor(request) {
  const token = cookies(request).get(cookieName)
  if (!token) return null
  const session = sessions.get(token)
  if (!session || session.expiresAtMs <= Date.now()) {
    if (token) sessions.delete(token)
    return null
  }
  return { token, session }
}

function view(session) {
  return {
    status: session.status,
    csrfToken: session.csrfToken,
    expiresAt: new Date(session.expiresAtMs).toISOString(),
    oauthErrorCode: null,
    steps: {
      xIdentity: session.xIdentity ? 'verified' : 'pending',
      xFollow: session.xFollow ? 'verified' : 'pending',
      wallet: session.wallet ? 'verified' : 'pending',
      completed: session.status === 'completed',
    },
    requiredConsents: {
      rulesVersion: '2026-09-13',
      privacyVersion: '2026-09-13',
    },
    ...(session.xIdentity ? {
      identity: { xHandle: 'plether_local_tester', maskedEmail: 't***@example.test' },
    } : {}),
    ...(session.wallet ? { wallet: session.wallet } : {}),
  }
}

async function body(request) {
  const chunks = []
  let size = 0
  for await (const chunk of request) {
    size += chunk.length
    if (size > 16 * 1024) throw new Error('request too large')
    chunks.push(chunk)
  }
  if (chunks.length === 0) return {}
  return JSON.parse(Buffer.concat(chunks).toString('utf8'))
}

function requireSession(request, response) {
  const authenticated = sessionFor(request)
  if (!authenticated) {
    error(response, 401, 'EXPIRED_SESSION', 'Your local registration session expired.')
    return null
  }
  return authenticated
}

function requireMutation(request, response) {
  const authenticated = requireSession(request, response)
  if (!authenticated) return null
  if (request.headers['x-registration-csrf'] !== authenticated.session.csrfToken) {
    error(response, 403, 'INVALID_REQUEST', 'The local CSRF token was rejected.')
    return null
  }
  return authenticated
}

function requestOrigin(request) {
  try {
    return new URL(request.headers.origin ?? request.headers.referer ?? '').origin
  } catch {
    return 'http://127.0.0.1:5175'
  }
}

function divisionsView(session) {
  return {
    status: session.completed ? 'completed' : 'in_progress',
    csrfToken: session.csrfToken,
    expiresAt: new Date(session.expiresAtMs).toISOString(),
    steps: {
      discord: session.discordId ? 'verified' : 'pending',
      wallet: session.wallet ? 'verified' : 'pending',
      completed: session.completed,
    },
    ...(session.discordId ? { identity: { username: 'plether_local_tester' } } : {}),
    ...(session.wallet ? { wallet: session.wallet } : {}),
    requiredConsents: { rulesVersion: 'divisions-2026-10', privacyVersion: 'divisions-2026-10' },
  }
}

async function handleDivisions(request, response, action) {
  const token = cookies(request).get(divisionsCookie)
  const session = token ? divisionsSessions.get(token) : null
  const live = session && session.expiresAtMs > Date.now()
  if (action === 'session' && request.method === 'POST') {
    const payload = await body(request)
    if (typeof payload.turnstileToken !== 'string' || payload.turnstileToken.length < 8) {
      error(response, 400, 'TURNSTILE_FAILED', 'The local Turnstile test token was rejected.')
      return
    }
    const nextToken = opaque()
    const next = { csrfToken: opaque(), expiresAtMs: Date.now() + 30 * 60 * 1000,
      discordId: null, wallet: null, challenge: null, completed: false }
    divisionsSessions.set(nextToken, next)
    json(response, 200, { registration: divisionsView(next) }, {
      'Set-Cookie': `${divisionsCookie}=${nextToken}; Path=/; Max-Age=1800; HttpOnly; SameSite=Lax`,
    })
    return
  }
  if (!live) {
    error(response, 401, 'EXPIRED_SESSION', 'Your local Divisions session expired.')
    return
  }
  if (action === 'session' && request.method === 'GET') {
    json(response, 200, { registration: divisionsView(session) })
    return
  }
  if (action === 'discord/callback' && request.method === 'GET') {
    session.discordId = '123456789012345678'
    session.wallet = null
    response.writeHead(303, { 'Cache-Control': 'no-store', Location: '/register' })
    response.end()
    return
  }
  if (request.headers['x-registration-csrf'] !== session.csrfToken) {
    error(response, 403, 'CSRF_FAILED', 'The local CSRF token was rejected.')
    return
  }
  if (action === 'discord/authorize' && request.method === 'POST') {
    session.discordId = null
    session.wallet = null
    json(response, 200, { authorizationUrl: `${requestOrigin(request)}/api/insights/v1/divisions/registrations/discord/callback?mock=1` })
    return
  }
  if (action === 'wallet/challenge' && request.method === 'POST') {
    const payload = await body(request)
    if (!session.discordId || typeof payload.ownerAddress !== 'string' || !isAddress(payload.ownerAddress)) {
      error(response, 400, 'INVALID_REQUEST', 'Connect Discord and a valid owner wallet first.')
      return
    }
    const ownerAddress = getAddress(payload.ownerAddress)
    const message = [
      'Plether Insights local Divisions linking', '',
      'Sign this one-time message to test wallet ownership.', '',
      `Owner: ${ownerAddress}`, 'Chain ID: 421614',
      `Nonce: ${opaque(18)}`, `Issued At: ${new Date().toISOString()}`,
    ].join('\n')
    session.challenge = { ownerAddress, message, expiresAtMs: Date.now() + 5 * 60 * 1000 }
    session.wallet = null
    json(response, 200, { message })
    return
  }
  if (action === 'wallet/verify' && request.method === 'POST') {
    const payload = await body(request)
    const challenge = session.challenge
    session.challenge = null
    if (!challenge || challenge.expiresAtMs <= Date.now()
      || typeof payload.ownerAddress !== 'string' || !isAddress(payload.ownerAddress)
      || getAddress(payload.ownerAddress) !== challenge.ownerAddress
      || typeof payload.signature !== 'string') {
      error(response, 409, 'EXPIRED_CHALLENGE', 'The local wallet challenge is invalid or expired.')
      return
    }
    const signer = await recoverMessageAddress({ message: challenge.message, signature: payload.signature })
    if (getAddress(signer) !== challenge.ownerAddress) {
      error(response, 400, 'INVALID_SIGNATURE', 'The wallet signature does not match the connected owner.')
      return
    }
    const digest = createHash('sha256').update(`divisions:${challenge.ownerAddress}`).digest('hex')
    session.wallet = { ownerAddress: challenge.ownerAddress.toLowerCase(), tradingAccount: `0x${digest.slice(-40)}` }
    json(response, 200, { registration: divisionsView(session) })
    return
  }
  if (action === 'complete' && request.method === 'POST') {
    const payload = await body(request)
    if (!session.discordId || !session.wallet || payload.acceptRules !== true || payload.acceptPrivacy !== true
      || payload.rulesVersion !== 'divisions-2026-10' || payload.privacyVersion !== 'divisions-2026-10') {
      error(response, 409, 'REGISTRATION_INCOMPLETE', 'Complete local Discord and wallet verification and accept the rules.')
      return
    }
    if (payload.showDiscordHandle !== undefined && typeof payload.showDiscordHandle !== 'boolean') {
      error(response, 400, 'INVALID_REQUEST', 'Leaderboard visibility must be a boolean.')
      return
    }
    session.showDiscordHandle = payload.showDiscordHandle ?? false
    session.completed = true
    json(response, 200, { registration: divisionsView(session) })
    return
  }
  error(response, 405, 'INVALID_REQUEST', 'Method not allowed by the local registration backend.')
}

const server = createServer(async (request, response) => {
  const url = new URL(request.url ?? '/', `http://${request.headers.host ?? `${host}:${String(port)}`}`)
  const divisionsMatch = divisionsRoutePattern.exec(url.pathname)
  if (divisionsMatch) {
    try { await handleDivisions(request, response, divisionsMatch[1]) }
    catch (caught) { error(response, 400, 'INVALID_REQUEST', caught instanceof Error ? caught.message : 'Unknown local registration error') }
    return
  }
  const matched = routePattern.exec(url.pathname)
  if (!matched) {
    error(response, 404, 'REGISTRATION_NOT_FOUND', 'Local registration route not found.')
    return
  }

  const slug = decodeURIComponent(matched[1])
  const action = matched[2]
  if (slug !== 'testnet-trading-2026-09') {
    error(response, 404, 'REGISTRATION_NOT_FOUND', 'Local registration is available only for the September competition.')
    return
  }

  try {
    if (action === 'session' && request.method === 'GET') {
      const authenticated = requireSession(request, response)
      if (authenticated) json(response, 200, { registration: view(authenticated.session) })
      return
    }

    if (action === 'session' && request.method === 'POST') {
      const payload = await body(request)
      if (typeof payload.turnstileToken !== 'string' || payload.turnstileToken.length < 8) {
        error(response, 400, 'TURNSTILE_FAILED', 'The local Turnstile test token was rejected.')
        return
      }
      const token = opaque()
      const session = {
        status: 'in_progress',
        csrfToken: opaque(),
        expiresAtMs: Date.now() + 30 * 60 * 1000,
        xIdentity: false,
        xFollow: false,
        wallet: null,
        challenge: null,
      }
      sessions.set(token, session)
      json(response, 200, { registration: view(session) }, {
        'Set-Cookie': `${cookieName}=${token}; Path=/; Max-Age=1800; HttpOnly; SameSite=Lax`,
      })
      return
    }

    if (action === 'x/callback' && request.method === 'GET') {
      const authenticated = requireSession(request, response)
      if (!authenticated) return
      authenticated.session.xIdentity = true
      response.writeHead(303, {
        'Cache-Control': 'no-store',
        Location: `/competitions/${encodeURIComponent(slug)}/register`,
      })
      response.end()
      return
    }

    const authenticated = requireMutation(request, response)
    if (!authenticated) return
    const { session } = authenticated

    if (action === 'x/authorize' && request.method === 'POST') {
      json(response, 200, {
        authorizationUrl: `${requestOrigin(request)}/api/insights/v1/competitions/${encodeURIComponent(slug)}/registrations/x/callback?mock=1`,
      })
      return
    }

    if (action === 'x/follow' && request.method === 'POST') {
      if (!session.xIdentity) {
        error(response, 409, 'X_IDENTITY_REQUIRED', 'Complete local X verification first.')
        return
      }
      // Local development has no X access token. Treat this read-only check as
      // successful so the remaining registration flow can be exercised.
      session.xFollow = true
      json(response, 200, { registration: view(session) })
      return
    }

    if (action === 'wallet/challenge' && request.method === 'POST') {
      if (!session.xFollow) {
        error(response, 409, 'X_FOLLOW_REQUIRED', 'Verify the local follow step first.')
        return
      }
      const payload = await body(request)
      if (typeof payload.ownerAddress !== 'string' || !isAddress(payload.ownerAddress)) {
        error(response, 400, 'INVALID_WALLET', 'Enter a valid owner wallet.')
        return
      }
      const ownerAddress = getAddress(payload.ownerAddress)
      const expiresAtMs = Date.now() + 5 * 60 * 1000
      const message = [
        'Plether Insights local registration',
        '',
        'Sign this one-time message to test wallet ownership.',
        '',
        `Owner: ${ownerAddress}`,
        'Chain ID: 421614',
        `Competition: ${slug}`,
        `Nonce: ${opaque(18)}`,
        `Issued At: ${new Date().toISOString()}`,
        `Expiration Time: ${new Date(expiresAtMs).toISOString()}`,
      ].join('\n')
      session.challenge = { ownerAddress, message, expiresAtMs }
      json(response, 200, { message, expiresAt: new Date(expiresAtMs).toISOString() })
      return
    }

    if (action === 'wallet/verify' && request.method === 'POST') {
      const payload = await body(request)
      const challenge = session.challenge
      if (
        !challenge
        || challenge.expiresAtMs <= Date.now()
        || typeof payload.ownerAddress !== 'string'
        || typeof payload.signature !== 'string'
        || !isAddress(payload.ownerAddress)
        || getAddress(payload.ownerAddress) !== challenge.ownerAddress
      ) {
        session.challenge = null
        error(response, 409, 'EXPIRED_CHALLENGE', 'The local wallet challenge is invalid or expired.')
        return
      }
      const recovered = await recoverMessageAddress({ message: challenge.message, signature: payload.signature })
      if (getAddress(recovered) !== challenge.ownerAddress) {
        session.challenge = null
        error(response, 400, 'INVALID_SIGNATURE', 'The wallet signature does not match the connected owner.')
        return
      }
      const digest = createHash('sha256').update(`${slug}:${challenge.ownerAddress}`).digest('hex')
      session.wallet = {
        ownerAddress: challenge.ownerAddress.toLowerCase(),
        tradingAccount: `0x${digest.slice(-40)}`,
      }
      session.challenge = null
      json(response, 200, { registration: view(session) })
      return
    }

    if (action === 'complete' && request.method === 'POST') {
      const payload = await body(request)
      if (!session.xFollow || !session.wallet) {
        error(response, 409, 'INVALID_REQUEST', 'Complete every local verification step first.')
        return
      }
      if (
        payload.acceptRules !== true
        || payload.acceptPrivacy !== true
        || typeof payload.acceptPromotionalEmail !== 'boolean'
        || payload.rulesVersion !== '2026-09-13'
        || payload.privacyVersion !== '2026-09-13'
      ) {
        error(response, 400, 'INVALID_REQUEST', 'Accept the current rules and privacy notice.')
        return
      }
      session.status = 'completed'
      json(response, 200, { registration: view(session) })
      return
    }

    error(response, 405, 'INVALID_REQUEST', 'Method not allowed by the local registration backend.')
  } catch (caught) {
    const message = caught instanceof Error ? caught.message : 'Unknown local registration error'
    error(response, 400, 'INVALID_REQUEST', message)
  }
})

server.listen(port, host, () => {
  process.stdout.write(`Insights local registration backend listening on http://${host}:${String(port)}\n`)
})
