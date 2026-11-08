import { test, expect, type Page } from '@playwright/test'
import { mkdir, writeFile } from 'node:fs/promises'
import { dirname } from 'node:path'
import { generateKeyPairSync } from 'node:crypto'
import { stringToHex, verifyMessage, type Hex } from 'viem'
import publicConfig from '../fixtures/privy-public-config.json' with { type: 'json' }
import { installTestWallet, TEST_WALLET_ADDRESSES, type TestWalletController } from '../fixtures/signingWallet'

const selectionKey = `plether.privy.${publicConfig.id}.owner`
const staleOwner = '0x1111111111111111111111111111111111111111'
const message = 'Plether wallet integration test; no transaction is submitted.'
const connectButton = (page: Page) => page.getByRole('banner').getByRole('button', { name: 'Connect Wallet', exact: true })
const selectButton = (page: Page) => page.getByRole('banner').getByRole('button', { name: 'Select Wallet', exact: true })
const accountButton = (page: Page) => page.getByRole('banner').getByRole('button', { name: /^Open wallet account / })

type GoogleFixture = 'provider' | 'init-error' | 'callback-error' | 'success'
type PasskeyFixture = 'cancel' | 'verify-error' | 'init-error'
const macMfaKey = Buffer.from('public-offline-mac-passkey').toString('base64url')
const mobileMfaKey = Buffer.from('public-offline-mobile-passkey').toString('base64url')
type MfaUpdateFixture = { outcome: 'success' | 'submit-error'; verified: number; refreshed: number; submissions: string[][] }
async function openApp(page: Page, selection = 'disconnected', hostname?: string, googleFixture: GoogleFixture = 'provider', passkeyFixture: PasskeyFixture = 'cancel', mfaSession: boolean | 'enrolled' = false, mfaUpdate?: MfaUpdateFixture): Promise<TestWalletController> {
  const wallet = await installTestWallet(page)
  let googleCancelUrl = ''
  // This unsigned token and user exist only inside the offline route fixture.
  // They cannot authenticate with Privy or any application backend.
  const mfaToken = `${Buffer.from(JSON.stringify({ alg: 'none' })).toString('base64url')}.${Buffer.from(JSON.stringify({
    sub: 'did:privy:public-mfa-test', aud: publicConfig.id, iss: 'privy.io', exp: Math.floor(Date.now() / 1000) + 3600,
  })).toString('base64url')}.public-test-signature`
  const sessionUser = {
    id: 'did:privy:public-mfa-test', created_at: 1700000000,
    mfa_methods: mfaUpdate ? [{ type: 'passkey', verified_at: 1700000000 }] : mfaSession === 'enrolled' ? [{ type: 'totp', verified_at: 1700000000 }] : [], has_accepted_terms: true,
    linked_accounts: [
      ...(mfaUpdate ? [
        { type: 'passkey', credential_id: macMfaKey, enrolled_in_mfa: true, authenticator_name: 'Mac test passkey', first_verified_at: 1700000000, latest_verified_at: 1700000000 },
        { type: 'passkey', credential_id: mobileMfaKey, enrolled_in_mfa: false, authenticator_name: 'Mobile test passkey', first_verified_at: 1700000001, latest_verified_at: 1700000001 },
      ] : []),
      ...(googleFixture === 'success'
        ? [{ type: 'google_oauth', subject: 'public-google-test', email: 'public-test@example.invalid', name: 'Public test', first_verified_at: 1700000000, latest_verified_at: 1700000000 }]
        : [{ type: 'email', address: 'public-mfa-test@example.invalid', first_verified_at: 1700000000, latest_verified_at: 1700000000 }]),
      { type: 'wallet', address: TEST_WALLET_ADDRESSES[1], chain_type: 'ethereum', wallet_client_type: 'privy', connector_type: 'embedded',
        recovery_method: 'privy', wallet_index: 0, imported: false, delegated: false, first_verified_at: 1700000000, latest_verified_at: 1700000000 },
    ],
  }
  if (mfaSession) await page.addInitScript(token => {
    if (localStorage.getItem('plether.e2e.session-seeded')) return
    localStorage.setItem('plether.e2e.session-seeded', 'true')
    localStorage.setItem('privy:token', JSON.stringify(token))
    localStorage.setItem('privy:pat', JSON.stringify(token))
    localStorage.setItem('privy:refresh_token', JSON.stringify('public-offline-refresh-token'))
  }, mfaToken)
  await page.addInitScript(({ key, initialSelection }) => {
    localStorage.setItem('plether_settings', JSON.stringify({ state: { riskAccepted: true, sepoliaWelcomeDismissed: true }, version: 0 }))
    if (sessionStorage.getItem(key) === null) sessionStorage.setItem(key, initialSelection)
  }, { key: selectionKey, initialSelection: selection })
  // Keep the SDK and application real, but make this suite deterministic and
  // offline. Unexpected external requests fail closed instead of reaching RPC,
  // auth, analytics, or the embedded-wallet service.
  await page.route('**/*', route => {
    const url = new URL(route.request().url())
    if (url.hostname === '127.0.0.1' || url.hostname === 'localhost') {
      if (url.pathname.startsWith('/api/')) return route.fulfill({ status: 503, json: {} })
      return route.continue()
    }
    if (url.hostname === 'auth.privy.io' && url.pathname === `/api/v1/apps/${publicConfig.id}`) {
      return route.fulfill({ json: mfaSession ? { ...publicConfig, mfa_methods: ['passkey', 'totp'] } : publicConfig, headers: {
        'access-control-allow-origin': route.request().headers().origin,
        'access-control-allow-credentials': 'true',
      } })
    }
    if (mfaUpdate && url.hostname === 'auth.privy.io' && url.pathname === '/api/v1/mfa/passkeys/init') {
      const headers = {
        'access-control-allow-origin': route.request().headers().origin,
        'access-control-allow-credentials': 'true',
        'access-control-allow-methods': 'POST, OPTIONS',
        'access-control-allow-headers': route.request().headers()['access-control-request-headers'] ?? '*',
      }
      if (route.request().method() === 'OPTIONS') return route.fulfill({ status: 204, headers })
      return route.fulfill({ headers, json: { options: {
        challenge: Buffer.alloc(32, 2).toString('base64url'), rp_id: 'localhost', user_verification: 'required', timeout: 10000,
        allow_credentials: [{ id: macMfaKey, type: 'public-key', transports: ['internal'] }],
      } } })
    }
    if (mfaUpdate && url.hostname === 'auth.privy.io' && url.pathname === '/api/v1/users/me') {
      const headers = {
        'access-control-allow-origin': route.request().headers().origin,
        'access-control-allow-credentials': 'true',
        'access-control-allow-methods': 'GET, OPTIONS',
        'access-control-allow-headers': route.request().headers()['access-control-request-headers'] ?? '*',
      }
      if (route.request().method() === 'OPTIONS') return route.fulfill({ status: 204, headers })
      if (mfaUpdate.verified) mfaUpdate.refreshed++
      return route.fulfill({ headers, json: { user: sessionUser } })
    }
    if (mfaUpdate && url.hostname === 'auth.privy.io' && url.pathname === '/offline-mfa-test') {
      // This handler stands in for the remote wallet service, not the SDK or
      // browser ceremony. Only synthetic credentials can reach this fixture.
      const input = route.request().postDataJSON() as { event: string; data: {
        mfaMethod?: string; mfaCode?: { id: string; response: { signature: string; authenticator_data: string } }; method?: string; credentialIds?: string[]; removeForLogin?: boolean
      } }
      if (input.event === 'privy:mfa:verify') {
        expect(input.data.mfaMethod).toBe('passkey')
        const assertion = input.data.mfaCode!
        expect(assertion.id).toBe(macMfaKey)
        expect(assertion.response.signature).toBeTruthy()
        expect(assertion.response.authenticator_data).toBeTruthy()
        mfaUpdate.verified++
        return route.fulfill({ json: { data: {} } })
      }
      expect(input.event).toBe('privy:mfa:submit-enrollment')
      expect(mfaUpdate.verified).toBeGreaterThan(0)
      expect(mfaUpdate.refreshed).toBe(mfaUpdate.verified)
      expect(input.data.method).toBe('passkey')
      expect(input.data.removeForLogin).toBe(false)
      expect(input.data.credentialIds).toEqual([macMfaKey, mobileMfaKey])
      mfaUpdate.submissions.push(input.data.credentialIds!)
      if (mfaUpdate.outcome === 'submit-error') return route.fulfill({ json: { error: { type: 'error', message: 'Offline enrollment service failure' } } })
      for (const account of sessionUser.linked_accounts) if (account.type === 'passkey') account.enrolled_in_mfa = true
      return route.fulfill({ json: { data: {} } })
    }
    if (url.hostname === 'auth.privy.io' && url.pathname === '/api/v1/sessions/logout') {
      const headers = {
        'access-control-allow-origin': route.request().headers().origin,
        'access-control-allow-credentials': 'true',
        'access-control-allow-methods': 'POST, OPTIONS',
        'access-control-allow-headers': route.request().headers()['access-control-request-headers'] ?? '*',
      }
      return route.fulfill({ status: 204, headers })
    }
    if ((mfaSession || googleFixture === 'success') && url.hostname === 'auth.privy.io' && url.pathname === '/api/v1/sessions') {
      const headers = {
        'access-control-allow-origin': route.request().headers().origin,
        'access-control-allow-credentials': 'true',
        'access-control-allow-methods': 'POST, OPTIONS',
        'access-control-allow-headers': route.request().headers()['access-control-request-headers'] ?? '*',
      }
      if (route.request().method() === 'OPTIONS') return route.fulfill({ status: 204, headers })
      return route.fulfill({ headers, json: { token: mfaToken, privy_access_token: mfaToken,
        refresh_token: 'public-offline-refresh-token', user: sessionUser } })
    }
    if (url.hostname === 'auth.privy.io' && ['/api/v1/passkeys/authenticate/init', '/api/v1/passkeys/authenticate'].includes(url.pathname)) {
      const headers = {
        'access-control-allow-origin': route.request().headers().origin,
        'access-control-allow-credentials': 'true',
        'access-control-allow-methods': 'POST, OPTIONS',
        'access-control-allow-headers': route.request().headers()['access-control-request-headers'] ?? '*',
      }
      if (route.request().method() === 'OPTIONS') return route.fulfill({ status: 204, headers })
      if (url.pathname.endsWith('/init') && passkeyFixture !== 'init-error') {
        return route.fulfill({ headers, json: {
          options: { challenge: Buffer.alloc(32, 1).toString('base64url'), rp_id: 'localhost', user_verification: 'required', timeout: 10000 },
          relying_party: 'localhost',
        } })
      }
      return route.fulfill({ status: 400, headers, json: { error: 'Invalid test credentials', code: 'invalid_credentials' } })
    }
    if (url.hostname === 'auth.privy.io' && ['/api/v1/oauth/init', '/api/v1/oauth/authenticate'].includes(url.pathname)) {
      const headers = {
        'access-control-allow-origin': route.request().headers().origin,
        'access-control-allow-credentials': 'true',
        'access-control-allow-methods': 'POST, OPTIONS',
        'access-control-allow-headers': route.request().headers()['access-control-request-headers'] ?? '*',
      }
      if (route.request().method() === 'OPTIONS') return route.fulfill({ status: 204, headers })
      if (url.pathname.endsWith('/authenticate') && googleFixture === 'success') {
        return route.fulfill({ headers, json: {
          token: mfaToken, privy_access_token: mfaToken, refresh_token: 'public-offline-refresh-token', is_new_user: false,
          user: sessionUser,
        } })
      }
      if (url.pathname.endsWith('/authenticate') || googleFixture === 'init-error') {
        return route.fulfill({ status: 400, json: { error: 'Invalid test credentials', code: 'invalid_credentials' }, headers })
      }
      const input = route.request().postDataJSON() as { provider: string; redirect_to: string; state_code: string }
      expect(input.provider).toBe('google')
      const callback = new URL(input.redirect_to)
      callback.searchParams.set('privy_oauth_provider', 'google')
      callback.searchParams.set('privy_oauth_state', input.state_code)
      callback.searchParams.set('privy_oauth_code', 'error')
      googleCancelUrl = callback.href
      callback.searchParams.set('privy_oauth_code', 'public-test-code')
      return route.fulfill({ json: { url: googleFixture === 'callback-error' || googleFixture === 'success' ? callback.href : 'https://accounts.google.com/plether-test-oauth' }, headers })
    }
    if (url.hostname === 'accounts.google.com' && url.pathname === '/plether-test-oauth') {
      return route.fulfill({ contentType: 'text/html', body: `<!doctype html><h1>Google account chooser (offline fixture)</h1><a href="${googleCancelUrl.replaceAll('&', '&amp;')}">Cancel Google sign-in</a>` })
    }
    if (url.hostname === 'explorer-api.walletconnect.com' && url.pathname === '/v3/wallets') {
      return route.fulfill({ json: { listings: {}, total: 0 }, headers: { 'access-control-allow-origin': '*' } })
    }
    if (url.hostname === 'auth.privy.io' && url.pathname.endsWith('/embedded-wallets')) {
      // Other than optional MFA enrollment, only readiness/clear are supported.
      // Never fabricate a wallet, transaction, or signing response.
      return route.fulfill({ contentType: 'text/html', body: `<!doctype html><html><body><script>
        addEventListener('message', async event => {
          const input = typeof event.data === 'string' ? JSON.parse(event.data) : event.data;
          if (!input || typeof input.event !== 'string' || !input.event.startsWith('privy:')) return;
          const output = { id: input.id, event: input.event };
          if (input.event === 'privy:iframe:ready' || input.event === 'privy:mfa:clear') output.data = {};
          else if (${!!mfaUpdate} && ['privy:mfa:verify', 'privy:mfa:submit-enrollment'].includes(input.event)) {
            const result = await fetch('/offline-mfa-test', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(input) });
            Object.assign(output, await result.json());
          }
          else output.error = { type: 'unsupported_test_operation', message: 'Embedded wallet operations are not implemented in this external-wallet fixture' };
          event.source.postMessage(output, event.origin);
        });
      </script></body></html>` })
    }
    return route.abort()
  })
  await page.routeWebSocket(/^wss:/, socket => socket.close())
  const url = new URL('/privacy', test.info().project.use.baseURL)
  if (hostname) url.hostname = hostname
  await page.goto(url.href)
  await expect(mfaSession ? selection === 'disconnected' ? selectButton(page) : accountButton(page) : connectButton(page)).toBeEnabled()
  return wallet
}

async function openPicker(page: Page) {
  await connectButton(page).click()
  await expect(page.getByRole('button', { name: 'Log in with Email', exact: true })).toBeVisible()
}

async function approveConnection(page: Page) {
  await openPicker(page)
  await page.locator('#privy-dialog').getByRole('button', { name: 'Plether Test Wallet', exact: true }).click()
  await expect(accountButton(page)).toBeVisible()
  await expect(connectButton(page)).not.toBeVisible()
}

async function selectedOwner(page: Page) {
  return page.evaluate(key => sessionStorage.getItem(key), selectionKey)
}

test('restored Google wallet shows loading then its address without flashing a wallet-choice button', async ({ page }) => {
  await page.addInitScript(() => {
    const probe = window as Window & { walletHeaderLabels: string[] }
    probe.walletHeaderLabels = []
    new MutationObserver(() => {
      const button = document.querySelector('header button[aria-label="Connecting…"], header button[aria-label="Select Wallet"], header button[aria-label="Connect Wallet"], header button[aria-label^="Open wallet account "]')
      const label = button?.getAttribute('aria-label')
      if (label && probe.walletHeaderLabels.at(-1) !== label) probe.walletHeaderLabels.push(label)
    }).observe(document, { subtree: true, childList: true, attributes: true, attributeFilter: ['aria-label'] })
  })
  await openApp(page, TEST_WALLET_ADDRESSES[1].toLowerCase(), 'localhost', 'success', 'cancel', true)
  for (const reload of [false, true]) {
    if (reload) await page.reload()
    await expect(accountButton(page)).toBeEnabled()
    const labels = await page.evaluate(() => (window as Window & { walletHeaderLabels: string[] }).walletHeaderLabels)
    expect(labels).toContain('Connecting…')
    expect(labels).not.toContain('Select Wallet')
    expect(labels).not.toContain('Connect Wallet')
    expect(labels.at(-1)).toMatch(/^Open wallet account /)
  }
})

async function signThroughBridge(page: Page) {
  const result = await page.evaluate(async ({ modulePath, data }) => {
    const { config } = await import(modulePath)
    const connection = config.state.connections.get(config.state.current)
    if (!connection) throw new Error('No active Wagmi connection')
    const provider = await connection.connector.getProvider()
    const [address] = await provider.request({ method: 'eth_accounts' })
    const signature = await provider.request({ method: 'personal_sign', params: [data, address] })
    return { address: address as Hex, signature: signature as Hex }
  }, { modulePath: '/src/config/wagmi.ts', data: stringToHex(message) })
  expect(await verifyMessage({ ...result, message })).toBe(true)
  return result.address
}

async function previousPage(page: Page) {
  await page.getByRole('link', { name: 'Terms of Service', exact: true }).click()
  await expect(page).toHaveURL(/\/terms$/)
  await page.getByRole('link', { name: 'Privacy Policy', exact: true }).click()
  await expect(page).toHaveURL(/\/privacy$/)
}

for (const flow of ['wallet', 'email'] as const) {
  test(`browser Back dismisses the ${flow} modal before navigating to the previous page`, async ({ page }) => {
    await page.setViewportSize({ width: 390, height: 844 })
    await openApp(page)
    await previousPage(page)
    const state = await page.evaluate(() => history.state)
    await openPicker(page)
    if (flow === 'email') {
      await page.getByRole('button', { name: 'Log in with Email', exact: true }).click()
      await expect(page.getByPlaceholder('your@email.com')).toBeVisible()
    }
    await page.goBack()
    await expect(page.getByRole('button', { name: 'Log in with Email', exact: true })).not.toBeVisible()
    await expect(page.getByPlaceholder('your@email.com')).not.toBeVisible()
    await expect(page).toHaveURL(/\/privacy$/)
    await expect.poll(() => page.evaluate(() => history.state)).toEqual(state)
    expect(await selectedOwner(page)).toBe('disconnected')
    await page.goBack()
    await expect(page).toHaveURL(/\/terms$/)
    await expect(page.getByRole('heading', { name: 'Terms of Service', exact: true })).toBeVisible()
  })
}

test('closing and reopening with the X button does not leave extra Back steps', async ({ page }) => {
  await openApp(page)
  await previousPage(page)
  for (let index = 0; index < 3; index++) {
    await openPicker(page)
    await page.locator('#privy-dialog').getByRole('button', { name: 'close modal', exact: true }).click()
    await expect(page.getByRole('button', { name: 'Log in with Email', exact: true })).not.toBeVisible()
  }
  await page.goBack()
  await expect(page).toHaveURL(/\/terms$/)
})

test('Back closes MFA before the underlying wallet account, without disconnecting', async ({ page }) => {
  await openApp(page, TEST_WALLET_ADDRESSES[1].toLowerCase(), 'localhost', 'provider', 'cancel', true)
  await previousPage(page)
  await accountButton(page).click()
  await page.getByRole('button', { name: 'Wallet MFA settings', exact: true }).click()
  await expect(page.locator('#privy-dialog').getByText('Transaction Protection', { exact: true })).toBeVisible()
  await page.goBack()
  await expect(page.locator('#privy-dialog').getByText('Transaction Protection', { exact: true })).not.toBeVisible()
  await expect(page.getByRole('button', { name: 'Wallet MFA settings', exact: true })).toBeVisible()
  await expect(page).toHaveURL(/\/privacy$/)
  await page.goBack()
  await expect(page.getByText('Wallet connected', { exact: true })).not.toBeVisible()
  await expect(accountButton(page)).toBeVisible()
  expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[1].toLowerCase())
  await page.goBack()
  await expect(page).toHaveURL(/\/terms$/)
})

test('Back dismisses application dialogs before navigating', async ({ page }) => {
  await openApp(page)
  await previousPage(page)
  await page.getByRole('button', { name: /^Build / }).click()
  await expect(page.getByRole('dialog')).toBeVisible()
  await page.goBack()
  await expect(page.getByRole('dialog')).not.toBeVisible()
  await expect(page).toHaveURL(/\/privacy$/)
  await page.goBack()
  await expect(page).toHaveURL(/\/terms$/)
})

test('normal Back and Forward navigation skips a closed modal entry', async ({ page }) => {
  await openApp(page)
  await previousPage(page)
  await page.getByRole('button', { name: /^Build / }).click()
  await page.getByRole('button', { name: 'Close dialog', exact: true }).click()
  await expect(page.getByRole('dialog')).not.toBeVisible()
  await page.getByRole('link', { name: 'Terms of Service', exact: true }).click()
  await expect(page).toHaveURL(/\/terms$/)
  await page.goBack()
  await expect(page).toHaveURL(/\/privacy$/)
  await expect(page.getByRole('heading', { name: 'Privacy Policy', exact: true })).toBeVisible()
  await page.goForward()
  await expect(page).toHaveURL(/\/terms$/)
  await expect(page.getByRole('heading', { name: 'Terms of Service', exact: true })).toBeVisible()
})

test('Google redirect activates the embedded wallet on mobile and keeps it connected after reload', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 })
  await openApp(page, 'disconnected', 'localhost', 'success')
  await openPicker(page)
  await page.getByRole('button', { name: 'Log in with Google', exact: true }).click()
  await expect(accountButton(page)).toBeVisible()
  await expect(connectButton(page)).not.toBeVisible()
  expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[1].toLowerCase())
  await page.reload()
  await expect(accountButton(page)).toBeVisible()
  await expect(connectButton(page)).not.toBeVisible()
  // Mobile browsers can discard a tab's session storage while Privy's
  // longer-lived authentication session remains in local storage.
  await page.evaluate(() => sessionStorage.clear())
  await page.reload()
  await expect(accountButton(page)).toBeVisible()
  await expect(connectButton(page)).not.toBeVisible()
})

test('restored Google session on mobile asks to select a wallet and reconnects without another login', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 })
  await openApp(page, 'disconnected', 'localhost', 'success', 'cancel', true)
  await expect(connectButton(page)).not.toBeVisible()
  await selectButton(page).click()
  const dialog = page.locator('#privy-dialog')
  await expect(dialog.getByRole('button', { name: 'Google linked', exact: true })).toBeDisabled()
  await expect(dialog.getByRole('button', { name: 'Add passkey', exact: true })).toBeEnabled()
  await dialog.getByRole('button', { name: 'Use email wallet', exact: true }).click()
  await expect(accountButton(page)).toBeVisible()
  expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[1].toLowerCase())
  await expect(dialog).not.toBeVisible()
})

for (const enrolled of [false, true]) {
  test(`embedded-wallet account dialog opens native MFA settings (already enrolled: ${enrolled})`, async ({ page }, testInfo) => {
    await openApp(page, TEST_WALLET_ADDRESSES[1].toLowerCase(), 'localhost', 'provider', 'cancel', enrolled ? 'enrolled' : true)
    await expect(accountButton(page)).toBeVisible()
    await accountButton(page).click()
    const settings = page.getByRole('button', { name: 'Wallet MFA settings', exact: true })
    await expect(settings).toBeVisible()
    await settings.click()
    const dialog = page.locator('#privy-dialog')
    if (!enrolled) {
      await expect(dialog.getByText('Transaction Protection', { exact: true })).toBeVisible()
      await dialog.getByRole('button', { name: 'Continue', exact: true }).click()
    }
    await expect(dialog.getByText('Choose a verification method', { exact: true })).toBeVisible()
    if (enrolled) await expect(dialog.getByText('Enabled', { exact: true })).toBeVisible()
    await page.screenshot({ animations: 'disabled', path: testInfo.outputPath('native-wallet-mfa.png') })
    await dialog.getByRole('button', { name: 'close modal', exact: true }).click()
    await expect(settings).toBeVisible()
    expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[1].toLowerCase())
    await expect(page.getByRole('banner').getByRole('alert')).not.toBeVisible()
    // A normal dismissal leaves the SDK enrollment promise pending. Reopening
    // must still work and must not require another login or owner selection.
    await settings.click()
    await expect(dialog.getByRole('button', { name: 'close modal', exact: true })).toBeVisible()
    await dialog.getByRole('button', { name: 'close modal', exact: true }).click()
    await expect(settings).toBeVisible()
  })
}

for (const outcome of ['success', 'submit-error'] as const) {
  test(`updates wallet MFA with the existing Mac passkey and preserves both keys (${outcome})`, async ({ page, context }, testInfo) => {
    if (outcome === 'success') await page.setViewportSize({ width: 390, height: 844 })
    const fixture: MfaUpdateFixture = { outcome, verified: 0, refreshed: 0, submissions: [] }
    await openApp(page, TEST_WALLET_ADDRESSES[1].toLowerCase(), 'localhost', 'provider', 'cancel', true, fixture)
    const cdp = await context.newCDPSession(page)
    await cdp.send('WebAuthn.enable')
    const { authenticatorId } = await cdp.send('WebAuthn.addVirtualAuthenticator', { options: {
      protocol: 'ctap2', transport: 'internal', hasResidentKey: true,
      hasUserVerification: true, isUserVerified: true, automaticPresenceSimulation: true,
    } })
    // A disposable synthetic Mac credential. The browser must authenticate
    // using it; the mobile login-only key is not in this authenticator.
    const { privateKey } = generateKeyPairSync('ec', { namedCurve: 'prime256v1' })
    await cdp.send('WebAuthn.addCredential', { authenticatorId, credential: {
      credentialId: Buffer.from(macMfaKey, 'base64url').toString('base64'),
      rpId: 'localhost', isResidentCredential: true, userHandle: Buffer.from([1]).toString('base64'), signCount: 0,
      privateKey: privateKey.export({ type: 'pkcs8', format: 'der' }).toString('base64'),
    } })
    await accountButton(page).click()
    const update = page.getByRole('button', { name: 'Enable additional passkeys for wallet MFA', exact: true })
    await update.scrollIntoViewIfNeeded()
    await page.screenshot({ animations: 'disabled', path: testInfo.outputPath('additional-mfa-passkeys.png') })
    await update.click()
    if (outcome === 'submit-error') {
      await expect(page.getByRole('alert').filter({ hasText: /wallet MFA/i })).toBeVisible()
      await expect(update).toBeEnabled()
      expect(fixture.verified).toBe(1)
      expect(fixture.submissions).toEqual([[macMfaKey, mobileMfaKey]])
      fixture.outcome = 'success'
      await update.click()
    }
    await expect(page.getByText('Linked passkeys are enabled for wallet MFA. Your existing passkeys are still enabled.', { exact: true })).toBeVisible()
    await expect(update).not.toBeVisible()
    expect(fixture.verified).toBe(outcome === 'success' ? 1 : 2)
    expect(fixture.submissions).toEqual(Array.from({ length: fixture.verified }, () => [macMfaKey, mobileMfaKey]))
    expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[1].toLowerCase())
    await expect(page.locator('#privy-dialog').getByRole('button', { name: 'close modal', exact: true })).not.toBeVisible()
    await page.getByRole('dialog', { name: 'Wallet connected', exact: true }).getByRole('button').first().click()
    await expect(accountButton(page)).toBeVisible()
    await cdp.send('WebAuthn.removeVirtualAuthenticator', { authenticatorId })
  })
}

test('Back cancels additional-passkey MFA verification without changing enrollment', async ({ page }) => {
  const fixture: MfaUpdateFixture = { outcome: 'success', verified: 0, refreshed: 0, submissions: [] }
  await openApp(page, TEST_WALLET_ADDRESSES[1].toLowerCase(), 'localhost', 'provider', 'cancel', true, fixture)
  await page.evaluate(() => {
    Object.defineProperty(navigator.credentials, 'get', { configurable: true, value: () => new Promise(() => {}) })
  })
  await accountButton(page).click()
  const update = page.getByRole('button', { name: 'Enable additional passkeys for wallet MFA', exact: true })
  await update.click()
  await expect(page.locator('#privy-dialog').getByText('Verifying with passkey', { exact: true })).toBeVisible()
  await page.goBack()
  await expect(page.locator('#privy-dialog').getByText('Verifying with passkey', { exact: true })).not.toBeVisible()
  await expect(update).toBeEnabled()
  expect(fixture.verified).toBe(0)
  expect(fixture.submissions).toEqual([])
  expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[1].toLowerCase())
})

test('external approval replaces an unavailable saved owner and signs through the real bridge', async ({ page }) => {
  const wallet = await openApp(page, staleOwner)
  await wallet.disconnect()
  await approveConnection(page)
  expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[0].toLowerCase())
  expect(await signThroughBridge(page)).toBe(TEST_WALLET_ADDRESSES[0])
})

test('disconnect, reconnect, and returning sessions keep the actual selected signer', async ({ page }) => {
  await openApp(page)
  await approveConnection(page)
  await accountButton(page).click()
  await page.getByRole('button', { name: 'Disconnect wallet', exact: true }).click()
  await expect(connectButton(page)).toBeVisible()
  expect(await selectedOwner(page)).toBe('disconnected')
  await approveConnection(page)
  expect(await signThroughBridge(page)).toBe(TEST_WALLET_ADDRESSES[0])
  await page.reload()
  await expect(accountButton(page)).toBeVisible()
  expect(await signThroughBridge(page)).toBe(TEST_WALLET_ADDRESSES[0])
})

for (const viewport of [{ name: 'mobile', width: 390, height: 844 }, { name: 'desktop', width: 1280, height: 900 }]) {
  test(`${viewport.name} wallet modal shows an empty placeholder only when no other wallets remain`, async ({ page }, testInfo) => {
    await page.setViewportSize({ width: viewport.width, height: viewport.height })
    const wallet = await openApp(page, TEST_WALLET_ADDRESSES[1].toLowerCase(), 'localhost', 'provider', 'cancel', true)
    await accountButton(page).click()
    const placeholder = page.getByText('No other wallets connected.', { exact: true })
    await expect(placeholder).not.toBeVisible()
    await wallet.disconnect()
    await expect(placeholder).toBeVisible()
    await expect(page.getByRole('button', { name: /Connect new wallet/i })).toBeEnabled()
    const disconnect = page.getByRole('button', { name: 'Disconnect wallet', exact: true })
    await expect(disconnect).toHaveCSS('background-color', 'rgb(255, 87, 45)')
    await expect(disconnect).toHaveCSS('color', 'rgb(255, 255, 255)')
    await page.screenshot({ path: testInfo.outputPath('wallet-empty-placeholder.png'), animations: 'disabled' })
  })

  for (const type of ['external', 'google'] as const) {
    test(`${viewport.name} ${type} wallet disconnects from its account modal and stays disconnected after reload`, async ({ page }, testInfo) => {
      await page.setViewportSize({ width: viewport.width, height: viewport.height })
      if (type === 'google') {
        await openApp(page, TEST_WALLET_ADDRESSES[1].toLowerCase(), 'localhost', 'success', 'cancel', true)
      } else {
        await openApp(page)
        await approveConnection(page)
      }
      await expect(page.getByRole('button', { name: 'Disconnect wallet', exact: true })).not.toBeVisible()
      await accountButton(page).click()
      await expect(page.getByText('Wallet connected', { exact: true })).toBeVisible()
      const disconnect = page.getByRole('button', { name: 'Disconnect wallet', exact: true })
      await expect(disconnect).toBeVisible()
      await expect(disconnect).toBeEnabled()
      await page.screenshot({ path: testInfo.outputPath('wallet-disconnect.png'), animations: 'disabled' })
      await disconnect.click()
      await expect(page.getByText('Wallet connected', { exact: true })).not.toBeVisible()
      await expect(connectButton(page)).toBeVisible()
      expect(await selectedOwner(page)).toBe('disconnected')
      await page.reload()
      await expect(connectButton(page)).toBeVisible()
      await expect(accountButton(page)).not.toBeVisible()
      expect(await selectedOwner(page)).toBe('disconnected')
    })
  }
}

test('a rejected wallet request leaves the app disconnected and can be retried', async ({ page }) => {
  const wallet = await openApp(page)
  await wallet.disconnect()
  await wallet.rejectNextRequest('eth_requestAccounts')
  await openPicker(page)
  await page.locator('#privy-dialog').getByRole('button', { name: 'Plether Test Wallet', exact: true }).click()
  await expect(page.locator('#privy-dialog').getByRole('button', { name: 'Retry', exact: true })).toBeVisible()
  await page.locator('#privy-dialog').getByRole('button', { name: 'close modal', exact: true }).click()
  await expect(connectButton(page)).toBeVisible()
  expect(await selectedOwner(page)).toBe('disconnected')
  await approveConnection(page)
})

for (const dismissal of ['close', 'escape', 'backdrop'] as const) {
  test(`canceling email with ${dismissal} does not select an already available wallet`, async ({ page }) => {
    await openApp(page)
    await openPicker(page)
    await page.getByRole('button', { name: 'Log in with Email', exact: true }).click()
    await expect(page.getByPlaceholder('your@email.com')).toBeVisible()
    if (dismissal === 'close') await page.locator('#privy-dialog').getByRole('button', { name: 'close modal', exact: true }).click()
    else if (dismissal === 'escape') await page.keyboard.press('Escape')
    else await page.mouse.click(5, 5)
    await expect(page.getByPlaceholder('your@email.com')).not.toBeVisible()
    await expect(connectButton(page)).toBeVisible()
    expect(await selectedOwner(page)).toBe('disconnected')
    await expect(page.getByRole('banner').getByRole('alert')).not.toBeVisible()
    await approveConnection(page)
    expect(await signThroughBridge(page)).toBe(TEST_WALLET_ADDRESSES[0])
  })
}

test('an extension account change cannot sign as the saved owner and reconnect selects the new account', async ({ page }) => {
  const wallet = await openApp(page)
  await approveConnection(page)
  await wallet.setAccount(1)
  await expect(connectButton(page)).toBeVisible()
  expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[0].toLowerCase())
  await approveConnection(page)
  expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[1].toLowerCase())
  expect(await signThroughBridge(page)).toBe(TEST_WALLET_ADDRESSES[1])
})

test('the native account picker offers email and canceling preserves the connected owner', async ({ page }) => {
  await openApp(page)
  await approveConnection(page)
  await accountButton(page).click()
  await page.getByRole('button', { name: /Connect new wallet/i }).click()
  const emailOption = page.getByRole('button', { name: 'Log in with Email', exact: true })
  await expect(emailOption).toBeVisible()
  await emailOption.click()
  await expect(page.getByPlaceholder('your@email.com')).toBeVisible()
  await page.locator('#privy-dialog').getByRole('button', { name: 'close modal', exact: true }).click()
  await expect(page.getByPlaceholder('your@email.com')).not.toBeVisible()
  await expect(accountButton(page)).toBeVisible()
  await expect(connectButton(page)).not.toBeVisible()
  expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[0].toLowerCase())
  expect(await signThroughBridge(page)).toBe(TEST_WALLET_ADDRESSES[0])
})

for (const result of ['cancel', 'verify-error', 'init-error'] as const) {
  test(`one passkey click opens WebAuthn directly and handles ${result}`, async ({ page, context }, testInfo) => {
    await openApp(page, 'disconnected', 'localhost', 'provider', result)
    const errors: string[] = []
    page.on('pageerror', error => errors.push(error.message))
    if (result === 'cancel') {
      // Only the OS dismissal is controlled; the installed SDK initializes the
      // ceremony and maps this browser error to passkey_not_allowed itself.
      await page.evaluate(() => {
        const probe = window as Window & { passkeyRequests?: number }
        probe.passkeyRequests = 0
        Object.defineProperty(navigator.credentials, 'get', { configurable: true, value: async () => {
          probe.passkeyRequests!++
          throw new DOMException('Public test: user dismissed device prompt', 'NotAllowedError')
        } })
      })
    } else if (result === 'verify-error') {
      // Use Chromium's real WebAuthn implementation and a disposable virtual
      // passkey. Authentication is rejected at the offline Privy API boundary.
      const cdp = await context.newCDPSession(page)
      await cdp.send('WebAuthn.enable')
      await cdp.send('WebAuthn.addVirtualAuthenticator', { options: {
        protocol: 'ctap2', transport: 'internal', hasResidentKey: true,
        hasUserVerification: true, isUserVerified: true, automaticPresenceSimulation: true,
      } })
      await page.evaluate(async () => {
        await navigator.credentials.create({ publicKey: {
          rp: { id: 'localhost', name: 'Offline Privy login test' },
          user: { id: new Uint8Array([1]), name: 'public-test', displayName: 'Public test' },
          challenge: new Uint8Array(32).fill(1), pubKeyCredParams: [{ type: 'public-key', alg: -7 }],
          authenticatorSelection: { residentKey: 'required', userVerification: 'required' },
        } })
      })
    }
    await openPicker(page)
    const originalPicker = await page.locator('#privy-dialog .ph-no-capture.ph-no-record').elementHandle()
    const submitted = result === 'verify-error' ? page.waitForRequest(request =>
      request.url().endsWith('/api/v1/passkeys/authenticate') && request.method() === 'POST') : null
    await page.getByRole('button', { name: 'Log in with passkey', exact: true }).click()
    if (result === 'cancel') {
      await expect.poll(() => page.evaluate(() => (window as Window & { passkeyRequests?: number }).passkeyRequests)).toBe(1)
      await expect.poll(() => page.evaluate(key => sessionStorage.getItem(`${key}.google-login`), selectionKey)).toBeNull()
      await expect(page.getByRole('banner').getByRole('alert')).not.toBeVisible()
      await expect(page.locator('#privy-dialog').getByRole('button', { name: 'Log in with Email', exact: true })).toBeVisible()
      expect(await originalPicker!.evaluate(element => element.isConnected)).toBe(true)
      await expect(page.locator('#privy-dialog').getByRole('alert')).not.toBeVisible()
      await page.locator('#privy-dialog').getByRole('button', { name: 'close modal', exact: true }).click()
    } else {
      if (submitted) expect((await submitted).postDataJSON()).toHaveProperty('authenticator_response')
      const dialog = page.locator('#privy-dialog')
      await expect(dialog.getByRole('alert')).toContainText('Could not sign in with passkey.')
      await expect(page.getByRole('banner').getByRole('alert')).not.toBeVisible()
      await page.screenshot({ animations: 'disabled', path: testInfo.outputPath('passkey-error-in-modal.png') })
      expect(await originalPicker!.evaluate(element => element.isConnected)).toBe(true)
      // Retry directly from the same picker, then choose email without
      // needing to find the page's Connect Wallet button again.
      const retry = page.waitForRequest(request => request.url().endsWith('/api/v1/passkeys/authenticate/init') && request.method() === 'POST')
      await dialog.getByRole('button', { name: 'Log in with passkey', exact: true }).click()
      await retry
      await expect(dialog.getByRole('alert')).toContainText('Could not sign in with passkey.')
      expect(await originalPicker!.evaluate(element => element.isConnected)).toBe(true)
      await dialog.getByRole('button', { name: 'Log in with Email', exact: true }).click()
      await expect(page.getByPlaceholder('your@email.com')).toBeVisible()
      await expect(dialog.getByRole('alert')).not.toBeVisible()
      await dialog.getByRole('button', { name: 'close modal', exact: true }).click()
    }
    await expect(page.locator('#privy-dialog')).not.toBeVisible()
    await expect(connectButton(page)).toBeEnabled()
    expect(await selectedOwner(page)).toBe('disconnected')
    expect(errors).toEqual([])
  })
}

test('IP-address visitors get a localhost path instead of a failing passkey request', async ({ page }) => {
  await openApp(page)
  await openPicker(page)
  await expect(page.getByRole('button', { name: 'Log in with passkey', exact: true })).toBeDisabled()
  const link = page.getByRole('link', { name: 'Open localhost to use passkeys' })
  await expect(link).toHaveAttribute('href', new URL('/privacy', test.info().project.use.baseURL!.replace('127.0.0.1', 'localhost')).href)
  await link.click()
  await openPicker(page)
  await expect(page.getByRole('button', { name: 'Log in with passkey', exact: true })).toBeEnabled()
  await expect(page.getByText('First time?', { exact: false })).not.toBeVisible()
  expect(await selectedOwner(page)).toBe('disconnected')
})

test('browser WebAuthn rejects loopback IPs and enrolls/authenticates a virtual passkey on localhost', async ({ page, context }) => {
  await openApp(page)
  const cdp = await context.newCDPSession(page)
  await cdp.send('WebAuthn.enable')
  const { authenticatorId } = await cdp.send('WebAuthn.addVirtualAuthenticator', { options: {
    protocol: 'ctap2', transport: 'internal', hasResidentKey: true,
    hasUserVerification: true, isUserVerified: true, automaticPresenceSimulation: true,
  } })
  // Exercise the real browser API with an ephemeral virtual authenticator.
  // No Privy user, physical credential, or blockchain transaction is created.
  const enroll = () => page.evaluate(async () => {
    try {
      const credential = await navigator.credentials.create({ publicKey: {
        rp: { id: location.hostname, name: 'Public WebAuthn origin test' },
        user: { id: new Uint8Array([1]), name: 'public-test', displayName: 'Public test' },
        challenge: new Uint8Array(32).fill(1), pubKeyCredParams: [{ type: 'public-key', alg: -7 }],
        authenticatorSelection: { residentKey: 'required', userVerification: 'required' }, timeout: 1000,
      } }) as PublicKeyCredential
      return { id: credential.id, error: null }
    } catch (error) { return { id: null, error: (error as Error).name } }
  })
  expect(await enroll()).toEqual({ id: null, error: 'SecurityError' })
  const url = new URL(page.url())
  url.hostname = 'localhost'
  await page.goto(url.href)
  const registered = await enroll()
  expect(registered.error).toBeNull()
  expect(registered.id).toBeTruthy()
  const authenticated = await page.evaluate(async () => {
    const credential = await navigator.credentials.get({ publicKey: {
      rpId: location.hostname, challenge: new Uint8Array(32).fill(2), userVerification: 'required', timeout: 1000,
    } }) as PublicKeyCredential
    return credential.id
  })
  expect(authenticated).toBe(registered.id)
  await cdp.send('WebAuthn.removeVirtualAuthenticator', { authenticatorId })
})

test('Google callback failures keep the app visible and show a recoverable result', async ({ page }) => {
  const errors: string[] = []
  page.on('pageerror', error => errors.push(error.message))
  await openApp(page)
  await page.goto('/privacy?privy_oauth_provider=google&privy_oauth_state=public-test-state&privy_oauth_code=public-test-code')
  await expect(page.getByRole('heading', { name: 'Privacy Policy', exact: true })).toBeVisible({ timeout: 10000 })
  await expect(page.getByText('Authentication failed', { exact: true })).toBeVisible()
  await page.locator('#privy-dialog').getByRole('button', { name: 'close modal', exact: true }).click()
  await expect(connectButton(page)).toBeEnabled()
  expect(errors).toEqual([])
})

test('writing browser-test HTML artifacts does not reload an open email login', async ({ page }, testInfo) => {
  await openApp(page)
  await openPicker(page)
  await page.getByRole('button', { name: 'Log in with Email', exact: true }).click()
  const email = page.getByPlaceholder('your@email.com')
  await expect(email).toBeVisible()
  const artifact = testInfo.outputPath('watcher-probe.html')
  await mkdir(dirname(artifact), { recursive: true })
  const reloaded = page.waitForEvent('framenavigated', {
    predicate: frame => frame === page.mainFrame(), timeout: 1500,
  }).then(() => true, () => false)
  await writeFile(artifact, '<!doctype html><title>Public watcher regression fixture</title>')
  expect(await reloaded).toBe(false)
  await expect(email).toBeVisible()
  expect(await selectedOwner(page)).toBe('disconnected')
})

test('one Google click on the main modal goes directly to the OAuth provider', async ({ page }, testInfo) => {
  await openApp(page)
  await openPicker(page)
  await expect(page.locator('#privy-dialog').getByRole('button', { name: 'Plether Test Wallet', exact: true })).toBeInViewport()
  await page.screenshot({ animations: 'disabled', path: testInfo.outputPath('login-options.png') })
  const request = page.waitForRequest(request => request.url().endsWith('/api/v1/oauth/init') && request.method() === 'POST')
  await page.getByRole('button', { name: 'Log in with Google', exact: true }).click()
  expect((await request).postDataJSON()).toMatchObject({ provider: 'google' })
  await expect(page.getByRole('heading', { name: 'Google account chooser (offline fixture)' })).toBeVisible()
  await expect(page.getByRole('button', { name: 'Google', exact: true })).not.toBeVisible()
  await page.getByRole('link', { name: 'Cancel Google sign-in' }).click()
  await expect(connectButton(page)).toBeEnabled()
  expect(await selectedOwner(page)).toBe('disconnected')
  await expect(page.getByRole('banner').getByRole('alert')).not.toBeVisible()
})

for (const failure of ['init-error', 'callback-error'] as const) {
  test(`direct Google ${failure} is recoverable without selecting a wallet`, async ({ page }) => {
    const errors: string[] = []
    page.on('pageerror', error => errors.push(error.message))
    await openApp(page, 'disconnected', undefined, failure)
    await openPicker(page)
    await page.getByRole('button', { name: 'Log in with Google', exact: true }).click()
    await expect(page.getByRole('banner').getByRole('alert')).toContainText(
      failure === 'init-error' ? 'Could not start sign-in. Try again.' : 'Wallet connection could not be completed. Try again.',
    )
    await expect(connectButton(page)).toBeEnabled()
    await expect(page.getByRole('heading', { name: 'Privacy Policy', exact: true })).toBeVisible()
    await expect(page.getByRole('button', { name: 'Google', exact: true })).not.toBeVisible()
    expect(await selectedOwner(page)).toBe('disconnected')
    expect(errors).toEqual([])
  })
}
