// Real Chromium + production browser modules, with a loopback-only HTTP boundary.
// No account connector, signer, private key or public API is configured.
import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import { execFileSync } from 'node:child_process'
import { createRequire } from 'node:module'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { createServer } from 'vite'
import { chromium } from 'playwright'

const directory = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
const require = createRequire(import.meta.url)
const hash = value => createHash('sha256').update(value).digest('hex')
const sourceFiles = [
  'src/perps-aa/execution.ts', 'src/perps-aa/readiness.ts', 'src/perps-aa/operationStore.ts',
  'src/perps-aa/authorizationStore.ts', 'src/perps-aa/errors.ts', 'src/perps-aa/attemptFailure.ts',
  'src/perps-aa/SponsoredOperationRecovery.tsx', 'src/perps-aa/operationReconciler.ts',
  'src/perps-aa/protocolOperationResolution.ts', 'src/perps-aa/runtimeContext.ts',
  'src/utils/transactionFailure.ts', 'test/incident-recovery-browser/harness.mjs',
  'scripts/drill-incident-recovery-browser.mjs',
]
const sourceHashes = () => Object.fromEntries(sourceFiles.map(filename => [filename, hash(fs.readFileSync(path.join(directory, filename)))]))
const sourceBefore = sourceHashes()
const state = { readinessBlocked: true, prepareMode: 'network-drop', preparations: [], signCalls: 0, sendCalls: 0, diagnostics: [],
  signedHash: null, receiptMode: 'not-found', signedQueries: [] }
const requests = [], externalRequests = [], pageErrors = []
const readBody = async request => {
  let body = ''
  for await (const chunk of request) { body += chunk; assert.ok(body.length < 64 * 1024, 'Unexpected fixture request size') }
  return body ? JSON.parse(body) : {}
}
const json = (response, status, body) => {
  response.statusCode = status
  response.setHeader('Content-Type', 'application/json')
  response.setHeader('Cache-Control', 'no-store')
  response.setHeader('Date', new Date().toUTCString())
  response.end(JSON.stringify(body))
}

const server = await createServer({
  configFile: false, envFile: false, root: directory,
  server: { host: '127.0.0.1', port: 0, strictPort: false },
  define: { 'import.meta.env.VITE_BUILD_COMMIT': JSON.stringify('isolated-browser-drill'), 'import.meta.env.VITE_POSTHOG_KEY': JSON.stringify('') },
  plugins: [{
    name: 'isolated-incident-recovery-http-fixture',
    configureServer(vite) {
      vite.middlewares.use((request, response, next) => {
        const pathname = request.url.split('?', 1)[0]
        if (!pathname.startsWith('/api/') && !pathname.startsWith('/__incident-drill/')) return next()
        requests.push({ method: request.method, pathname })
        void (async () => {
          if (pathname === '/api/perps/v1/readiness') {
            const now = Date.now()
            const check = state.readinessBlocked
              ? { component: 'reconciliation', status: 'blocked', reason: 'RECONCILIATION_STALE' }
              : { component: 'reconciliation', status: 'ready', reason: 'READY' }
            return json(response, 200, {
              version: 1, observedAt: now, expiresAt: now + 15_000, enforcementEnabled: false,
              actions: Object.fromEntries(['deposit', 'open', 'close', 'protection'].map(action => [action, [check]])),
            })
          }
          if (pathname === '/api/perps/v1/aa/diagnostics') {
            const body = await readBody(request)
            state.diagnostics.push({ stage: body.stage, reasonCode: body.reasonCode, failureStep: body.failureStep })
            response.statusCode = 204; response.end(); return
          }
          if (pathname === '/__incident-drill/prepare') {
            const body = await readBody(request)
            state.preparations.push(body.preparationId)
            if (state.prepareMode === 'network-drop') { request.socket.destroy(); return }
            return json(response, 503, { reason: 'RECONCILER_STALE' })
          }
          if (['/__incident-drill/receipt', '/__incident-drill/status', '/__incident-drill/recovery-snapshot'].includes(pathname)) {
            const requestedHash = new URL(request.url, 'http://127.0.0.1').searchParams.get('hash')
            assert.equal(requestedHash, state.signedHash, 'Recovery must query the original persisted hash')
            state.signedQueries.push({ pathname, userOperationHash: requestedHash, receiptMode: state.receiptMode, readinessBlocked: state.readinessBlocked })
            if (pathname === '/__incident-drill/receipt') {
              if (state.receiptMode === 'not-found') return json(response, 200, { kind: 'not-found' })
              return json(response, 200, {
                actualGasCost: '1', actualGasUsed: '1', entryPoint: '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108',
                logs: [], nonce: '7', sender: '0x2222222222222222222222222222222222222222', success: true, userOpHash: requestedHash,
                receipt: { transactionHash: `0x${'bb'.repeat(32)}`, status: 'success', blockNumber: '123', blockHash: `0x${'dd'.repeat(32)}` },
              })
            }
            if (pathname === '/__incident-drill/status') return json(response, 200, { status: 'submitted', transactionHash: null })
            return json(response, 200, { blockNumber: '123', blockTimestamp: String(Math.floor(Date.now() / 1000)), accountNonce: '7', userOperationEvidence: { kind: 'inconclusive' } })
          }
          if (pathname === '/__incident-drill/sign') { state.signCalls++; return json(response, 409, { refused: true }) }
          if (pathname === '/__incident-drill/send') { state.sendCalls++; return json(response, 409, { refused: true }) }
          return json(response, 503, { reason: 'UNCONFIGURED_ISOLATED_FIXTURE_ROUTE' })
        })().catch(() => json(response, 500, { reason: 'ISOLATED_FIXTURE_FAILED' }))
      })
    },
  }],
})

let browser
const startedAt = new Date().toISOString()
try {
  await server.listen()
  const address = server.httpServer.address()
  const origin = `http://127.0.0.1:${address.port}`
  browser = await chromium.launch({ headless: true })
  const context = await browser.newContext()
  const guardNetwork = async target => target.route('**/*', route => {
    const url = new URL(route.request().url())
    if (url.origin === origin) return route.continue()
    externalRequests.push(url.origin)
    return route.abort('blockedbyclient')
  })
  await guardNetwork(context)
  const page = await context.newPage()
  page.on('pageerror', error => pageErrors.push(error.message))
  const url = `${origin}/test/incident-recovery-browser/index.html`
  const load = async () => {
    await page.goto(url)
    await page.waitForFunction(() => Boolean(window.incidentRecoveryDrill), { timeout: 60_000 })
  }
  await load()
  const initial = await page.evaluate(() => window.incidentRecoveryDrill.refresh())
  assert.equal(initial.actualBrowserLocksAvailable, true, 'Actual Chromium Web Locks must be available')
  const stale = await page.evaluate(() => window.incidentRecoveryDrill.newAction())
  assert.equal(stale.reason, 'RECONCILIATION_STALE')
  const afterBlocked = await page.evaluate(() => window.incidentRecoveryDrill.snapshot())
  assert.equal(afterBlocked.operationCount, 0)
  assert.equal(state.preparations.length, 0, 'Fresh reconciliation refusal must happen before preparation')
  assert.equal(state.signCalls, 0)
  assert.equal(state.sendCalls, 0)

  state.readinessBlocked = false
  await page.evaluate(() => window.incidentRecoveryDrill.refresh())
  const dropped = await page.evaluate(() => window.incidentRecoveryDrill.newAction(true))
  assert.equal(dropped.message, 'preparation response lost')
  const beforeReload = await page.evaluate(() => window.incidentRecoveryDrill.snapshot())
  assert.equal(beforeReload.operationCount, 1)
  assert.equal(beforeReload.active.status, 'preparation-pending')
  assert.equal(beforeReload.active.journalPresent, true)
  assert.equal(beforeReload.active.hasUserOperationHash, false)
  assert.equal(beforeReload.active.hasSignedOperation, false)
  assert.equal(beforeReload.active.id, beforeReload.active.nativePreparationId)
  assert.equal(beforeReload.authorizations.length, 1)
  assert.equal(beforeReload.boundaryCalls.prepare, 1)
  assert.ok(state.preparations.length >= 1)
  assert.ok(state.preparations.every(id => id === beforeReload.active.id), 'Any browser transport retry must retain the same preparation identity')
  const droppedWireRequests = state.preparations.length

  state.readinessBlocked = true
  state.prepareMode = 'stale-refusal'
  await page.reload()
  await page.waitForFunction(() => Boolean(window.incidentRecoveryDrill), { timeout: 60_000 })
  const restored = await page.evaluate(() => window.incidentRecoveryDrill.refresh())
  assert.equal(restored.active.id, beforeReload.active.id)
  assert.equal(restored.active.nativePreparation, beforeReload.active.nativePreparation)
  assert.deepEqual(restored.authorizations, beforeReload.authorizations)
  const retry = await page.evaluate(() => window.incidentRecoveryDrill.resume())
  assert.equal(retry.reason, 'RECONCILIATION_STALE')
  const afterRetry = await page.evaluate(() => window.incidentRecoveryDrill.snapshot())
  assert.equal(afterRetry.operationCount, 1)
  assert.equal(afterRetry.active.id, beforeReload.active.id)
  assert.equal(afterRetry.active.status, 'preparation-pending')
  assert.equal(afterRetry.active.reason, 'RECONCILIATION_STALE')
  assert.equal(afterRetry.active.nativePreparation, beforeReload.active.nativePreparation)
  assert.deepEqual(afterRetry.authorizations, beforeReload.authorizations)
  assert.equal(afterRetry.boundaryCalls.prepare, 1)
  assert.ok(state.preparations.length > droppedWireRequests)
  assert.ok(state.preparations.every(id => id === beforeReload.active.id))

  await page.reload()
  await page.waitForFunction(() => Boolean(window.incidentRecoveryDrill), { timeout: 60_000 })
  const final = await page.evaluate(() => window.incidentRecoveryDrill.snapshot())
  assert.equal(final.active.id, beforeReload.active.id)
  assert.equal(final.active.reason, 'RECONCILIATION_STALE')
  assert.equal(final.active.nativePreparation, beforeReload.active.nativePreparation)
  assert.deepEqual(final.authorizations, beforeReload.authorizations)
  assert.equal(final.active.journalPresent, true)
  assert.equal(final.active.hasSignedOperation, false)
  assert.equal(final.active.hasUserOperationHash, false)
  assert.equal(state.signCalls, 0)
  assert.equal(state.sendCalls, 0)

  // A separate browser context seeds one explicitly synthetic signed journal.
  // The prior uncertain operation is left intact in its original context.
  const signedContext = await browser.newContext()
  await guardNetwork(signedContext)
  const signedPage = await signedContext.newPage()
  signedPage.on('pageerror', error => pageErrors.push(error.message))
  await signedPage.goto(`${url}?recovery=signed`)
  await signedPage.waitForFunction(() => Boolean(window.incidentRecoveryDrill), { timeout: 60_000 })
  await signedPage.evaluate(() => window.incidentRecoveryDrill.refresh())
  const signedInitial = await signedPage.evaluate(() => window.incidentRecoveryDrill.seedSignedUnknown())
  assert.equal(signedInitial.operation.status, 'submission-unknown')
  assert.equal(signedInitial.operation.hasSignedOperation, true)
  assert.equal(signedInitial.operation.submissionMetadataVersion, 1)
  assert.equal(signedInitial.operation.journalPresent, true)
  assert.equal(signedInitial.activeOperationId, signedInitial.operation.id)
  assert.equal(signedInitial.operationCount, 1)
  assert.equal(signedInitial.authorizations.length, 1)
  const prepareRequestsBeforeSignedRecovery = state.preparations.length
  state.signedHash = signedInitial.operation.userOperationHash
  await signedPage.evaluate(() => window.incidentRecoveryDrill.mountSignedRecovery())
  await signedPage.waitForFunction(() => {
    const value = window.incidentRecoveryDrill.signedSnapshot()
    return value.operation?.status === 'receipt-timeout' && value.boundaryCalls.status >= 1
  }, { timeout: 30_000 })
  const signedWhileStale = await signedPage.evaluate(() => window.incidentRecoveryDrill.signedSnapshot())
  assert.equal(signedWhileStale.activeOperationId, signedInitial.operation.id)
  assert.equal(signedWhileStale.operation.signedUserOperation, signedInitial.operation.signedUserOperation)
  assert.equal(signedWhileStale.operation.userOperationHash, signedInitial.operation.userOperationHash)
  assert.equal(signedWhileStale.operation.retryable, false)
  assert.deepEqual(signedWhileStale.authorizations, signedInitial.authorizations)
  assert.ok(state.signedQueries.some(query => query.pathname === '/__incident-drill/status' && query.readinessBlocked))

  await signedPage.reload()
  await signedPage.waitForFunction(() => Boolean(window.incidentRecoveryDrill), { timeout: 60_000 })
  await signedPage.evaluate(() => window.incidentRecoveryDrill.refresh())
  await signedPage.waitForFunction(() => window.incidentRecoveryDrill.signedSnapshot().boundaryCalls.status >= 1, { timeout: 30_000 })
  const signedRestored = await signedPage.evaluate(() => window.incidentRecoveryDrill.signedSnapshot())
  assert.equal(signedRestored.activeOperationId, signedInitial.operation.id)
  assert.equal(signedRestored.operation.id, signedInitial.operation.id)
  assert.equal(signedRestored.operation.userOperationHash, signedInitial.operation.userOperationHash)
  assert.equal(signedRestored.operation.signedUserOperation, signedInitial.operation.signedUserOperation)
  assert.equal(signedRestored.operationCount, 1)
  assert.equal(signedRestored.operation.journalPresent, true)
  assert.deepEqual(signedRestored.authorizations, signedInitial.authorizations)

  state.readinessBlocked = false
  state.receiptMode = 'confirmed'
  await signedPage.evaluate(() => window.incidentRecoveryDrill.refresh())
  await signedPage.waitForFunction(() => window.incidentRecoveryDrill.signedSnapshot().operation?.status === 'confirmed', { timeout: 45_000 })
  const signedConfirmed = await signedPage.evaluate(() => window.incidentRecoveryDrill.signedSnapshot())
  assert.equal(signedConfirmed.operation.id, signedInitial.operation.id)
  assert.equal(signedConfirmed.operation.userOperationHash, signedInitial.operation.userOperationHash)
  assert.equal(signedConfirmed.operation.signedUserOperation, signedInitial.operation.signedUserOperation)
  assert.equal(signedConfirmed.operationCount, 1)
  assert.equal(signedConfirmed.activeOperationId, null)
  const consumed = signedConfirmed.authorizations.filter(([key]) => key.endsWith(':consumed'))
  assert.equal(consumed.length, 1, 'Only the matching receipt may mark the nonce-owned fixture authorization consumed')
  assert.equal(consumed[0][1], signedInitial.operation.authorizationNonce.toLowerCase())
  assert.deepEqual(signedConfirmed.authorizations.filter(([key]) => !key.endsWith(':consumed')), signedInitial.authorizations,
    'Nonce-owned cleanup must retain the authorization cache rather than risk deleting a newer one')
  assert.equal(signedConfirmed.boundaryCalls.prepare, 0)
  assert.equal(signedConfirmed.boundaryCalls.sign, 0)
  assert.equal(signedConfirmed.boundaryCalls.send, 0)
  assert.equal(state.preparations.length, prepareRequestsBeforeSignedRecovery)
  assert.ok(state.signedQueries.some(query => query.pathname === '/__incident-drill/receipt' && query.receiptMode === 'confirmed'))
  assert.ok(state.signedQueries.every(query => query.userOperationHash === signedInitial.operation.userOperationHash))

  await signedPage.reload()
  await signedPage.waitForFunction(() => Boolean(window.incidentRecoveryDrill), { timeout: 60_000 })
  const signedFinal = await signedPage.evaluate(() => window.incidentRecoveryDrill.signedSnapshot())
  assert.equal(signedFinal.operation.status, 'confirmed')
  assert.equal(signedFinal.activeOperationId, null)
  assert.equal(signedFinal.operationCount, 1)
  assert.equal(signedFinal.operation.signedUserOperation, signedInitial.operation.signedUserOperation)
  assert.equal(state.signCalls, 0)
  assert.equal(state.sendCalls, 0)
  assert.deepEqual(externalRequests, [], 'The isolated browser must never request live endpoints')
  assert.deepEqual(pageErrors, [])
  assert.deepEqual(sourceHashes(), sourceBefore, 'The production modules or drill changed while Chromium was exercising them')

  const evidence = {
    schemaVersion: 1, classification: 'real-chromium-production-browser-modules-with-loopback-http-fixtures',
    startedAt, completedAt: new Date().toISOString(), result: 'passed',
    source: {
      gitCommit: execFileSync('git', ['rev-parse', 'HEAD'], { cwd: directory, encoding: 'utf8' }).trim(),
      fileSha256: sourceBefore,
    },
    browser: { name: 'chromium', version: browser.version(), playwright: require('playwright/package.json').version, actualWebLocks: true, actualLocalStorage: true, actualReloads: 4, separateIsolatedContexts: 2 },
    boundaries: { walletConnected: false, privateKeysLoaded: false, liveEndpointsRequested: false, chainOrContractsExecuted: false, signablePayloadReturned: false },
    freshRefusal: { reason: stale.reason, enforcementEnabled: false, prepareCalls: 0, signCalls: 0, sendCalls: 0, journalEntries: 0 },
    uncertainRecovery: {
      firstPreparationResponse: 'loopback HTTP connection dropped after request receipt',
      retryReason: retry.reason, logicalPreparationCalls: beforeReload.boundaryCalls.prepare + afterRetry.boundaryCalls.prepare,
      httpPreparationRequests: state.preparations.length, droppedConnectionRequests: droppedWireRequests,
      samePreparationId: new Set(state.preparations).size === 1,
      originalOperationId: beforeReload.active.id, finalOperationId: final.active.id,
      preparationSha256Before: hash(beforeReload.active.nativePreparation), preparationSha256After: hash(final.active.nativePreparation),
      unsignedAuthorizationStorageSha256Before: hash(JSON.stringify(beforeReload.authorizations)), unsignedAuthorizationStorageSha256After: hash(JSON.stringify(final.authorizations)),
      operationCount: final.operationCount, finalStatus: final.active.status, journalPresent: final.active.journalPresent,
      signCalls: state.signCalls, sendCalls: state.sendCalls, hasSignedOperation: false, hasUserOperationHash: false,
    },
    signedUnknownRecovery: {
      coordinator: 'production SponsoredOperationRecovery React component',
      seed: 'one synthetic invalid-signature operation persisted with production beginOperation/recordUserOperationHash',
      actualReloads: 2, initialStatus: signedInitial.operation.status, stalePollingStatus: signedWhileStale.operation.status,
      finalStatus: signedFinal.operation.status, sameOperationId: signedInitial.operation.id === signedFinal.operation.id,
      sameUserOperationHash: signedInitial.operation.userOperationHash === signedFinal.operation.userOperationHash,
      signedJournalSha256Before: hash(signedInitial.operation.signedUserOperation), signedJournalSha256After: hash(signedFinal.operation.signedUserOperation),
      operationCount: signedFinal.operationCount, blockedLanePreservedWhileStale: signedRestored.activeOperationId === signedInitial.operation.id,
      unsignedAuthorizationPreservedWhileStale: JSON.stringify(signedRestored.authorizations) === JSON.stringify(signedInitial.authorizations),
      laneReleasedAfterMatchingReceipt: signedFinal.activeOperationId === null,
      exactNonceMarkedConsumedAfterMatchingReceipt: consumed[0][1] === signedInitial.operation.authorizationNonce.toLowerCase(),
      authorizationCachePreservedByNonceOwnedCleanup: true,
      receiptRequests: state.signedQueries.filter(query => query.pathname === '/__incident-drill/receipt').length,
      statusRequests: state.signedQueries.filter(query => query.pathname === '/__incident-drill/status').length,
      safeSnapshotRequests: state.signedQueries.filter(query => query.pathname === '/__incident-drill/recovery-snapshot').length,
      allReadsBoundToOriginalHash: state.signedQueries.every(query => query.userOperationHash === signedInitial.operation.userOperationHash),
      prepareCalls: 0, signCalls: 0, sendCalls: 0, newIntentCreatedDuringRecovery: false,
    },
    diagnostics: state.diagnostics, requestCounts: Object.fromEntries([...new Set(requests.map(request => request.pathname))].sort().map(pathname => [pathname, requests.filter(request => request.pathname === pathname).length])),
    limitations: ['The RPC/provider boundary is simulated over loopback HTTP.', 'Authorization records are unsigned fixtures and the seeded signed journal has an invalid four-byte signature; no wallet signature, real sponsorship, chain transaction or live financial safety state was exercised.', 'The matching receipt is supplied by the fixture runtime boundary; canonical on-chain receipt verification is not exercised by this browser drill.'],
  }
  const output = process.argv[2] ?? path.join(os.tmpdir(), 'plether-isolated-recovery-drill', 'frontend-browser.json')
  fs.mkdirSync(path.dirname(path.resolve(output)), { recursive: true })
  fs.writeFileSync(output, `${JSON.stringify(evidence, null, 2)}\n`)
  console.log(JSON.stringify({ result: 'passed', evidence: path.resolve(output), browser: browser.version(), samePreparationId: true, preservedAuthorizationStorage: true, signCalls: 0, sendCalls: 0 }))
} finally {
  await browser?.close()
  await server.close()
}
