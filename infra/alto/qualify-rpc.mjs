// Read-only VPC canary. No signer or utility-wallet secrets belong in this task.
import { pathToFileURL } from 'node:url'

const GATEWAY = 'https://rpc-internal.sepolia.plether.com/'
const ENTRYPOINT = '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108'
const DEPLOYER = '0x4e59b44847b379578588920ca78fbf26c0b4956c'
const ZERO = `0x${'0'.repeat(40)}`
const DEAD = `0x${'0'.repeat(36)}dead`
const SLOT = `0x${'0'.repeat(64)}`
const SLOT_VALUE = `0x${'0'.repeat(60)}5678`
const OVERRIDE_CODE = '0x30316000540160005260206000f3'
const OVERRIDE_RESULT = `0x${'0'.repeat(60)}68ac`
const HASH = /^0x[0-9a-fA-F]{64}$/
const ADDRESS = /^0x[0-9a-fA-F]{40}$/
const QUANTITY = /^0x[0-9a-fA-F]+$/
const EXPECTED_EXECUTORS = 4
const METRIC = 'alto_user_operations_in_mempool_count'
const RPC_REQUIRED = [
  'configuration', 'chain_and_primary_route', 'not_syncing', 'fresh_head',
  'canonical_latest', 'canonical_safe', 'canonical_finalized', 'safe_state_reads',
  'fee_history', 'gas_price', 'deployed_contracts', 'combined_state_overrides',
  'call_tracer_state_overrides', 'bounded_latest_logs', 'isolated_fallback',
]
const IDLE_REQUIRED = ['baseline_configuration', 'baseline_idle']

function require(ok) { if (!ok) throw new Error('canary_check_failed') }
function quantity(value) { require(typeof value === 'string' && QUANTITY.test(value)); return BigInt(value) }
function validBlock(block) {
  require(block && HASH.test(block.hash) && quantity(block.number) >= 0n && quantity(block.timestamp) > 0n)
  return block
}
function urlConfiguration(env) {
  require(env.ALTO_RPC_URL === GATEWAY)
  require(!env.ALTO_EXECUTOR_PRIVATE_KEYS && !env.ALTO_UTILITY_PRIVATE_KEY)
  require(env.ALTO_CHAIN_TYPE === 'arbitrum' && env.PLETHER_ALTO_NETWORK_CHAIN_ID === '421614')
  require(typeof env.PLETHER_ALTO_RPC_AUTH_TOKEN === 'string' && env.PLETHER_ALTO_RPC_AUTH_TOKEN.length >= 32 &&
    env.PLETHER_ALTO_RPC_AUTH_TOKEN.length <= 4096 && /^[\x21-\x7e]+$/.test(env.PLETHER_ALTO_RPC_AUTH_TOKEN))
  require(!env.ALTO_SEND_TRANSACTION_RPC_URL || env.ALTO_SEND_TRANSACTION_RPC_URL === GATEWAY)
  require(env.ALTO_ENTRYPOINTS?.toLowerCase() === ENTRYPOINT.toLowerCase())
  const simulations = [env.ALTO_ENTRYPOINT_SIMULATION_CONTRACT_V8, env.ALTO_PIMLICO_SIMULATION_CONTRACT]
  require(simulations.every(address => typeof address === 'string' && ADDRESS.test(address) && address !== ZERO))
  const backup = new URL(env.PLETHER_ALTO_BACKUP_RPC_URL)
  require(backup.protocol === 'https:' && backup.hostname === 'arb-sepolia.g.alchemy.com' &&
    !backup.username && !backup.password && !backup.hash && (!backup.port || backup.port === '443'))
  return { backup: backup.href, simulations }
}
function baselineConfiguration(env) {
  require(!env.ALTO_EXECUTOR_PRIVATE_KEYS && !env.ALTO_UTILITY_PRIVATE_KEY && !env.ALTO_RPC_URL &&
    !env.ALTO_SEND_TRANSACTION_RPC_URL && !env.PLETHER_ALTO_RPC_AUTH_TOKEN && !env.PLETHER_ALTO_BACKUP_RPC_URL)
  const baseline = new URL(env.PLETHER_ALTO_BASELINE_URL)
  require(baseline.protocol === 'http:' && !baseline.username && !baseline.password && !baseline.search && !baseline.hash &&
    baseline.pathname === '/' && (!baseline.port || baseline.port === '80') &&
    /^internal-plether-sepolia-alto-[a-z0-9-]+\.ap-southeast-1\.elb\.amazonaws\.com$/.test(baseline.hostname))
  return baseline.href
}

async function boundedText(response, maximum = 1024 * 1024) {
  require(response.body && typeof response.body.getReader === 'function')
  const reader = response.body.getReader()
  const chunks = []
  let size = 0
  for (;;) {
    const { done, value } = await reader.read()
    if (done) break
    size += value.length
    if (size > maximum) { await reader.cancel(); throw new Error('canary_check_failed') }
    chunks.push(value)
  }
  const all = new Uint8Array(size)
  let offset = 0
  for (const chunk of chunks) { all.set(chunk, offset); offset += chunk.length }
  return new TextDecoder().decode(all)
}

// Missing status series must never be interpreted as an empty queue. Pinned
// Alto v1.2.7 exposes outstanding operations and initialized executor gauges;
// it does not expose separate queued/processing/submitted queue gauges.
export function parseIdleMetrics(text) {
  require(typeof text === 'string' && text.length <= 1024 * 1024)
  const observations = new Map()
  const rows = []
  for (const line of text.split('\n')) {
    if (!line || line.startsWith('#')) continue
    const match = /^([a-zA-Z_:][a-zA-Z0-9_:]*)(?:\{([^}]*)\})?\s+([^\s]+)(?:\s+\d+)?$/.exec(line)
    if (!match) continue
    const [, name, labels, raw] = match
    if (![METRIC, 'alto_executor_wallets_available_count', 'alto_executor_wallets_total_count', 'process_start_time_seconds'].includes(name)) continue
    const value = Number(raw)
    require(Number.isFinite(value) && value >= 0)
    if (name === METRIC) {
      const statuses = [...(labels ?? '').matchAll(/(?:^|,)\s*status="([^"\\]*)"/g)]
      require(statuses.length === 1 && /^(outstanding|processing|submitted|queued)$/.test(statuses[0][1]))
      require(Number.isSafeInteger(value) && !rows.some(row => row.status === statuses[0][1]))
      rows.push({ status: statuses[0][1], value })
    } else {
      require(!observations.has(name))
      observations.set(name, value)
    }
  }
  require(rows.some(row => row.status === 'outstanding') && rows.every(row => row.value === 0))
  const available = observations.get('alto_executor_wallets_available_count')
  const total = observations.get('alto_executor_wallets_total_count')
  const startedAt = observations.get('process_start_time_seconds')
  require(available === EXPECTED_EXECUTORS && total === EXPECTED_EXECUTORS && Number.isFinite(startedAt) && startedAt > 0)
  return { available, total, processStartedAt: startedAt }
}

export async function observeIdle({ baseline, fetchImpl, now, sleep, signal, requestTimeoutMs = 10000 }) {
  async function assertAdmissionBarrier() {
    // The owner-tagged ALB POST rule must already have stopped admission. A
    // healthy chain-id response here means new work can still arrive.
    const barrier = await fetchImpl(new URL('/', baseline), {
      method: 'POST', redirect: 'error', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'eth_chainId', params: [] }),
      signal: AbortSignal.any([signal, AbortSignal.timeout(requestTimeoutMs)]),
    })
    require(barrier.status === 503)
    const control = JSON.parse(await boundedText(barrier, 4096))
    require(control && Object.keys(control).length === 1 && control.error === 'alto_rpc_cutover_in_progress')
  }
  await assertAdmissionBarrier()
  // Start this acknowledgement grace only after observing the barrier on the
  // data plane: ALB rule creation alone does not establish propagation. The
  // API's 20-second forward timeout plus margin is not a proof that cancelled
  // requests stopped all internal validation; the metrics gate is separate.
  await sleep(22000, signal)
  const snapshots = []
  for (let i = 0; i < 2; i++) {
    if (i) await sleep(5000, signal)
    await assertAdmissionBarrier()
    const response = await fetchImpl(new URL('/metrics', baseline), {
      method: 'GET', redirect: 'error', cache: 'no-store',
      headers: { 'Cache-Control': 'no-cache' },
      signal: AbortSignal.any([signal, AbortSignal.timeout(requestTimeoutMs)]),
    })
    require(response.status === 200)
    const responseTime = Date.parse(response.headers.get('date') ?? '')
    require(Number.isFinite(responseTime) && Math.abs(now() - responseTime) <= 15000)
    snapshots.push(parseIdleMetrics(await boundedText(response)))
  }
  require(snapshots[0].processStartedAt === snapshots[1].processStartedAt)
  return { idle_sample_count: snapshots.length, idle_observed_at_ms: now() }
}

export async function qualifyFallback({ backup, token, fetchImpl, signal, requestTimeoutMs, gatewayFactory }) {
  // The operator embeds the reviewed gateway source from the same checkout.
  // Its only failed primary is this isolated loopback fixture, never the node.
  if (!gatewayFactory) {
    const source = globalThis.__PLETHER_RPC_GATEWAY_SOURCE__
    require(typeof source === 'string' && source.length > 0 && source.length <= 100000)
    const module = await import(`data:text/javascript;base64,${Buffer.from(source).toString('base64')}`)
    gatewayFactory = module.createGateway
  }
  require(typeof gatewayFactory === 'function')
  const backupConfig = { url: backup }
  const fixture = gatewayFactory({ token, chainId: '0x66eee',
    primary: { url: 'http://127.0.0.1:1' }, backup: backupConfig, reference: backupConfig,
    probeMs: 10000, timeoutMs: 10000 }, { log: () => {} })
  let listening = false
  try {
    await new Promise((resolve, reject) => {
      fixture.server.once('error', reject)
      fixture.server.listen(0, '127.0.0.1', resolve)
    })
    listening = true
    await fixture.probe()
    require(fixture.state.backup === true && fixture.state.primary === false)
    const endpoint = `http://127.0.0.1:${fixture.server.address().port}`
    const init = { method: 'POST', redirect: 'error',
      headers: { 'Content-Type': 'application/json', Authorization: `Bearer ${token}` },
      body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'eth_chainId', params: [] }),
      signal: AbortSignal.any([signal, AbortSignal.timeout(requestTimeoutMs)]),
    }
    const response = await fetchImpl(endpoint + '/', init)
    require(response.status === 200 && response.headers.get('x-plether-rpc-upstream') === 'backup')
    const value = JSON.parse(await boundedText(response))
    require(value?.jsonrpc === '2.0' && value.id === 1 && value.result === '0x66eee' && !value.error)
    const strict = await fetchImpl(endpoint + '/primary', init)
    require(strict.status === 503)
    await boundedText(strict)
  } finally {
    if (listening) {
      fixture.server.closeAllConnections?.()
      await new Promise(resolve => fixture.server.close(resolve))
    }
  }
}

function wait(milliseconds, signal) {
  return new Promise((resolve, reject) => {
    signal.throwIfAborted()
    const timer = setTimeout(() => { signal.removeEventListener('abort', cancel); resolve() }, milliseconds)
    const cancel = () => { clearTimeout(timer); reject(new Error('canary_check_failed')) }
    signal.addEventListener('abort', cancel, { once: true })
  })
}

export async function runQualification({ env = process.env, fetchImpl = globalThis.fetch, now = Date.now,
  sleep = wait, requestTimeoutMs = 10000, deadlineMs = 120000, gatewayFactory } = {}) {
  const mode = env.PLETHER_ALTO_QUALIFY_MODE
  const required = mode === 'rpc' ? RPC_REQUIRED : mode === 'idle' ? IDLE_REQUIRED : ['configuration']
  const tests = Object.fromEntries(required.map(name => [name, false]))
  const startedAt = now()
  const signal = AbortSignal.timeout(deadlineMs)
  let calls = 0
  let idle = { idle_sample_count: 0 }
  const check = async (name, run) => {
    try { await run(); tests[name] = true; return true } catch { return false }
  }
  if (mode === 'idle') {
    let baseline
    if (await check('baseline_configuration', async () => { baseline = baselineConfiguration(env) })) {
      await check('baseline_idle', async () => {
        idle = await observeIdle({ baseline, fetchImpl, now, sleep, signal, requestTimeoutMs })
      })
    }
    return result()
  }
  if (mode !== 'rpc') return result()
  let configuration
  if (!await check('configuration', async () => { configuration = urlConfiguration(env) })) return result()
  async function rpc(primary, method, params = []) {
    const id = ++calls
    const response = await fetchImpl(primary ? new URL('/primary', GATEWAY) : GATEWAY, {
      method: 'POST', redirect: 'error',
      headers: { 'Content-Type': 'application/json', ...(primary ? { Authorization: `Bearer ${env.PLETHER_ALTO_RPC_AUTH_TOKEN}` } : {}) },
      body: JSON.stringify({ jsonrpc: '2.0', id, method, params }),
      signal: AbortSignal.any([signal, AbortSignal.timeout(requestTimeoutMs)]),
    })
    require(response.status === 200 && response.headers.get('x-plether-rpc-upstream') === 'primary')
    const value = JSON.parse(await boundedText(response))
    require(value?.jsonrpc === '2.0' && value.id === id && !value.error && Object.hasOwn(value, 'result'))
    return value.result
  }
  const primary = (method, params) => rpc(true, method, params)
  const ordinary = (method, params) => rpc(false, method, params)
  await check('chain_and_primary_route', async () => {
    const chains = await Promise.all([ordinary('eth_chainId'), primary('eth_chainId')])
    require(chains.every(chain => chain === '0x66eee'))
  })
  await check('not_syncing', async () => require(await primary('eth_syncing') === false))
  let safeHeight
  let latestHeight
  for (const tag of ['latest', 'safe', 'finalized']) {
    await check(`canonical_${tag}`, async () => {
      const heads = (await Promise.all([ordinary('eth_getBlockByNumber', [tag, false]), primary('eth_getBlockByNumber', [tag, false])])).map(validBlock)
      const common = quantity(heads[0].number) < quantity(heads[1].number) ? heads[0].number : heads[1].number
      const pinned = (await Promise.all([ordinary('eth_getBlockByNumber', [common, false]), primary('eth_getBlockByNumber', [common, false])])).map(validBlock)
      require(pinned.every(block => quantity(block.number) === quantity(common)) && pinned[0].hash === pinned[1].hash)
      if (tag === 'safe') safeHeight = common
      if (tag === 'latest') {
        latestHeight = quantity(common)
        const times = heads.map(block => Number(quantity(block.timestamp)))
        require(times.every(time => now() / 1000 - time <= 15 && time - now() / 1000 <= 5) && Math.abs(times[0] - times[1]) <= 5)
        tests.fresh_head = true
      }
    })
  }
  await check('safe_state_reads', async () => {
    require(safeHeight)
    for (const [method, params] of [
      ['eth_getBalance', [ZERO, safeHeight]], ['eth_getCode', [ZERO, safeHeight]],
      ['eth_getStorageAt', [ZERO, '0x0', safeHeight]],
      ['eth_call', [{ to: ZERO, data: '0x', gas: '0xf4240' }, safeHeight]],
    ]) {
      const values = await Promise.all([ordinary(method, params), primary(method, params)])
      for (const value of values) {
        if (method === 'eth_getBalance') require(quantity(value) >= 0n)
        else if (method === 'eth_getStorageAt') require(typeof value === 'string' && HASH.test(value))
        else require(typeof value === 'string' && /^0x(?:[0-9a-fA-F]{2})*$/.test(value))
      }
      require(JSON.stringify(values[0]) === JSON.stringify(values[1]))
    }
  })
  await check('fee_history', async () => {
    const history = await ordinary('eth_feeHistory', ['0xa', 'latest', [20]])
    quantity(history?.oldestBlock)
    require(Array.isArray(history.baseFeePerGas) && history.baseFeePerGas.length === 11 && history.baseFeePerGas.every(value => quantity(value) >= 0n) && history.baseFeePerGas.some(value => quantity(value) > 0n))
    require(Array.isArray(history.gasUsedRatio) && history.gasUsedRatio.length === 10 && history.gasUsedRatio.every(value => Number.isFinite(value) && value >= 0 && value <= 1))
    require(Array.isArray(history.reward) && history.reward.length === 10 && history.reward.every(row => Array.isArray(row) && row.length === 1 && quantity(row[0]) >= 0n))
  })
  await check('gas_price', async () => {
    require(quantity(await ordinary('eth_gasPrice')) > 0n)
    const block = validBlock(await ordinary('eth_getBlockByNumber', ['latest', false]))
    require(quantity(block.baseFeePerGas) > 0n)
  })
  await check('deployed_contracts', async () => {
    for (const address of [ENTRYPOINT, DEPLOYER, ...configuration.simulations]) {
      const code = await ordinary('eth_getCode', [address, 'latest'])
      require(typeof code === 'string' && /^0x(?:[0-9a-fA-F]{2})+$/.test(code) && !/^0x0+$/.test(code))
    }
  })
  const transaction = { to: DEAD, data: '0x', gas: '0xf4240' }
  const overrides = { [DEAD]: { code: OVERRIDE_CODE, balance: '0x1234', stateDiff: { [SLOT]: SLOT_VALUE } } }
  await check('combined_state_overrides', async () => {
    const values = await Promise.all([ordinary('eth_call', [transaction, 'latest', overrides]), primary('eth_call', [transaction, 'latest', overrides])])
    require(values.every(value => typeof value === 'string' && value.toLowerCase() === OVERRIDE_RESULT))
  })
  await check('call_tracer_state_overrides', async () => {
    const traced = await ordinary('debug_traceCall', [transaction, 'latest', { tracer: 'callTracer', timeout: '2s', stateOverrides: overrides }])
    require(traced && !traced.error && typeof traced.output === 'string' && traced.output.toLowerCase() === OVERRIDE_RESULT)
  })
  await check('bounded_latest_logs', async () => {
    require(latestHeight !== undefined)
    const head = quantity(await ordinary('eth_blockNumber'))
    const from = head > 500n ? head - 500n : 0n
    const logs = await ordinary('eth_getLogs', [{ address: ENTRYPOINT, fromBlock: `0x${from.toString(16)}`, toBlock: 'latest' }])
    require(Array.isArray(logs))
  })
  await check('isolated_fallback', async () => {
    await qualifyFallback({ backup: configuration.backup, token: env.PLETHER_ALTO_RPC_AUTH_TOKEN,
      fetchImpl, signal, requestTimeoutMs, gatewayFactory })
  })
  return result()

  function result() {
    return { event: 'alto_rpc_qualification', schema_version: 1, scope: mode === 'rpc' || mode === 'idle' ? mode : 'invalid',
      required_checks_complete: required.every(name => tests[name]),
      tests: required.map(name => ({ name, pass: tests[name] })), rpc_request_count: calls,
      started_at_ms: startedAt, finished_at_ms: now(), ...idle }
  }
}

if (!process.argv[1] || import.meta.url === pathToFileURL(process.argv[1]).href) {
  let proof
  try { proof = await runQualification() }
  catch { proof = { event: 'alto_rpc_qualification', schema_version: 1, required_checks_complete: false,
    tests: [{ name: 'canary_runtime', pass: false }], idle_sample_count: 0 } }
  console.log(JSON.stringify(proof))
  process.exitCode = proof.required_checks_complete ? 0 : 1
}
