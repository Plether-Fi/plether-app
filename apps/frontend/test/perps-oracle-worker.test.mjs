import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createOracleWorker, loadOracleFeeds, validatePythPayload } from '../scripts/perps-oracle-worker.mjs'

const feeds = { pyth: `0x${'11'.repeat(20)}`, feedIds: Array.from({ length: 6 }, (_, i) => `0x${String(i + 1).padStart(64, '0')}`) }
function fixture() {
  const state = { now: 1_000_000, mark: 990n, stored: 980n, publishes: Array(6).fill(990), writes: 0,
    reads: [], logs: [], waitError: undefined, simulationError: undefined, reverted: false, pendingWait: undefined }
  const publicClient = {
    getBlock: async () => ({ number: 42n, timestamp: 1000n }),
    readContract: async input => {
      state.reads.push(input)
      if (input.functionName === 'pyth') return feeds.pyth
      if (input.functionName === 'pythFeedIds') return feeds.feedIds[Number(input.args[0])]
      if (input.functionName === 'getProtocolStatus') return { lastMarkTime: state.mark, lastMarkPrice: 100_000_000n }
      if (input.functionName === 'getPriceUnsafe') return { publishTime: state.stored }
      if (input.functionName === 'getUpdateFee') return 6n
      throw new Error(`Unexpected read ${input.functionName}`)
    },
    getBalance: async () => 1000n,
    simulateContract: async input => {
      if (state.simulationError) throw state.simulationError
      return { request: input }
    },
    waitForTransactionReceipt: async () => {
      if (state.pendingWait) await state.pendingWait
      if (state.waitError) throw state.waitError
      if (!state.reverted) { state.stored = BigInt(Math.min(...state.publishes)); state.mark = state.stored }
      return { status: state.reverted ? 'reverted' : 'success', blockNumber: 43n }
    },
  }
  const options = { publicClient, walletClient: { writeContract: async () => { state.writes++; return '0x1234' } },
    account: { address: feeds.pyth }, feeds, now: () => state.now,
    fetchPayload: async () => ({ updateData: ['0xabcd'], publishTimes: state.publishes }),
    log: (...args) => state.logs.push(args), logEvery: (...args) => state.logs.push(args) }
  return { state, publicClient, options, iterate: createOracleWorker(options) }
}

test('startup discovers the configured Pyth endpoint and six distinct feeds at one block', async () => {
  const f = fixture()
  assert.deepEqual(await loadOracleFeeds(f.publicClient), feeds)
  assert.ok(f.state.reads.every(read => read.blockNumber === 42n))
})
test('repairs stored feeds with a payload equal to the engine mark, then stays healthy', async () => {
  const f = fixture()
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.stored, 990n)
  assert.equal(f.state.mark, 990n)
  assert.equal(f.state.writes, 1)
  assert.ok(f.state.reads.filter(read => read.functionName !== 'getUpdateFee').every(read => read.blockNumber === 42n))
  assert.equal((await f.iterate()).status, 'healthy')
  assert.equal(f.state.writes, 1)
})
test('maximum publish time cannot hide a component older than the mark', async () => {
  const f = fixture(); f.state.publishes = [989, 991, 991, 991, 991, 991]
  assert.equal((await f.iterate()).status, 'waiting_for_payload')
  assert.equal(f.state.writes, 0)
})
test('healthy markets refresh on the ordinary cadence, while lag bypasses it', async () => {
  const f = fixture(); f.state.stored = 990n
  await f.iterate()
  f.state.now += 5000; f.state.publishes.fill(995)
  assert.equal((await f.iterate()).status, 'healthy')
  f.state.mark = 995n
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 1)
  f.state.now += 30_000; f.state.publishes.fill(999)
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 2)
})
test('oldest component age and future publication prevent submission', async () => {
  const f = fixture(); f.state.mark = 940n; f.state.stored = 930n; f.state.publishes = [940, 980, 980, 980, 980, 980]
  assert.equal((await f.iterate()).status, 'stale_payload')
  f.state.publishes.fill(1001)
  f.state.now += 5000
  assert.equal((await f.iterate()).status, 'stale_payload')
  assert.equal(f.state.writes, 0)
})
test('rejects malformed blobs, missing feeds, and noninteger timestamps', () => {
  for (const payload of [undefined, { updateData: ['0xzz'], publishTimes: Array(6).fill(990) },
    { updateData: ['0xab'], publishTimes: [990] }, { updateData: ['0xab'], publishTimes: Array(6).fill('990junk') }]) {
    assert.throws(() => validatePythPayload(payload))
  }
})
test('simulation failure sends nothing and a concurrent mark advancement is re-evaluated', async () => {
  const f = fixture(); f.state.simulationError = new Error('PriceOutOfOrder')
  await assert.rejects(f.iterate(), /PriceOutOfOrder/)
  assert.equal(f.state.writes, 0)
  f.state.mark = 995n; f.state.simulationError = undefined
  f.state.now += 5000
  assert.equal((await f.iterate()).status, 'waiting_for_payload')
  f.state.publishes.fill(995)
  assert.equal((await f.iterate()).status, 'synchronized')
})
test('receipt timeout retains the hash and reconciles before sending anything else', async () => {
  const f = fixture(); f.state.waitError = new Error('receipt timeout')
  await assert.rejects(f.iterate(), /receipt timeout/)
  await assert.rejects(f.iterate(), /receipt timeout/)
  assert.equal(f.state.writes, 1)
  f.state.waitError = undefined
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 1)
})
test('concurrent ticks cannot overlap a pending submission', async () => {
  const f = fixture(); let resolve
  f.state.pendingWait = new Promise(done => { resolve = done })
  const pending = f.iterate()
  assert.equal((await f.iterate()).status, 'busy')
  resolve(); await pending
  assert.equal(f.state.writes, 1)
})
test('reverted receipt is reported and may be retried on a later health check', async () => {
  const f = fixture(); f.state.reverted = true
  await assert.rejects(f.iterate(), /tx failed/)
  assert.equal(f.state.stored, 980n)
  f.state.reverted = false
  f.state.now += 5000
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 2)
})
test('post-receipt reads can report continuing lag instead of claiming recovery', async () => {
  const f = fixture()
  f.publicClient.waitForTransactionReceipt = async () => ({ status: 'success', blockNumber: 43n })
  assert.equal((await f.iterate()).status, 'lagging')
  assert.ok(f.state.logs.some(args => args.includes('oracle_sync_pending')))
})
test('dry run does not send a transaction', async () => {
  const f = fixture()
  assert.equal((await createOracleWorker({ ...f.options, dryRun: true })()).status, 'dry_run')
  assert.equal(f.state.writes, 0)
})

test('refreshes block health after a slow payload request', async () => {
  const f = fixture(); let blockTime = 1000n
  f.publicClient.getBlock = async () => ({ number: 42n, timestamp: blockTime })
  const iterate = createOracleWorker({ ...f.options, fetchPayload: async () => {
    blockTime = 1010n; f.state.publishes.fill(1008)
    return { updateData: ['0xabcd'], publishTimes: f.state.publishes }
  } })
  assert.equal((await iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 1)
})

test('does not submit payloads that expire during fee or balance lookup', async () => {
  for (const stage of ['getUpdateFee', 'balance']) {
    const f = fixture(); let blockTime = 1000n
    f.publicClient.getBlock = async () => ({ number: 42n, timestamp: blockTime })
    const read = f.publicClient.readContract
    f.publicClient.readContract = async input => {
      if (stage === 'getUpdateFee' && input.functionName === stage) blockTime = 1100n
      return read(input)
    }
    f.publicClient.getBalance = async () => { if (stage === 'balance') blockTime = 1100n; return 1000n }
    assert.equal((await f.iterate()).status, 'stale_payload')
    assert.equal(f.state.writes, 0)
  }
})

test('pre-broadcast failures back off at 5/10/20/30 seconds and recover', async () => {
  const f = fixture(); let requests = 0; let broken = true
  const iterate = createOracleWorker({ ...f.options, fetchPayload: async () => {
    requests++; if (broken) throw new Error('backend unavailable')
    return { updateData: ['0xabcd'], publishTimes: f.state.publishes }
  } })
  for (const delay of [5000, 10000, 20000, 30000, 30000]) {
    await assert.rejects(iterate(), /backend unavailable/)
    const previous = requests
    f.state.now += delay - 1
    assert.equal((await iterate()).status, 'backoff')
    assert.equal(requests, previous)
    f.state.now++
  }
  broken = false
  assert.equal((await iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 1)
})
