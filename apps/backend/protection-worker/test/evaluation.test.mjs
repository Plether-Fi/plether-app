import { it } from 'node:test'
import assert from 'node:assert/strict'
import { ProtectionWorker } from '../worker.mjs'

const address = '0x1111111111111111111111111111111111111111'
function fixture({ status = 2, frozen = false, reason = 2, tail = 0n, head = 1n, expired = false, source = 'backend_hermes_latest_v2', pendingCount = 0n, executionEnabled = true } = {}) {
  const calls = []
  const logs = []
  const observations = []
  const release = { network: { chainId: 421614 }, release: { deploymentBlock: 100 }, contracts: Object.fromEntries(['positionProtectionBook', 'orderRouter', 'orderLifecycleBook', 'pletherOracle'].map(name => [name, { address }])) }
  const db = { query: async (sql, args) => {
    if (sql.startsWith('INSERT INTO perps_protection_observations')) observations.push(JSON.parse(args[5]))
    return { rows: sql.includes('DISTINCT ON') ? [{ protection_id: '7' }] : sql.includes('perps_pyth_update_payloads') ? [{ source, min_publish_time: 999, max_publish_time: 999, update_data: ['0x1234'] }] : [] }
  } }
  const publicClient = {
    getBlock: async () => ({ number: 200n, timestamp: 1000n, hash: '0x' + 'ab'.repeat(32) }),
    simulateContract: async () => ({ result: { markPrice: 90n, publishTime: 999n } }),
    readContract: async ({ functionName }) => {
      const values = {
        isOracleFrozen: frozen, getUpdateFee: 13n,
        getPositionProtection: { protectionId: 7n, account: address, status, side: 0, takeProfitTriggerPrice: 90n, stopLossTriggerPrice: 110n, armedAt: 998n, armedBlock: 198n, linkedOrderId: 5n },
        outcome: { reason, status: 3 }, nextExecuteId: head, globalTailOrderId: tail,
        pendingPolicy: { validUntil: expired ? 999n : 1001n }, maxOrderAge: 60n, pendingOrderCounts: pendingCount,
      }
      if (!(functionName in values)) throw new Error(`Unexpected ${functionName}`)
      return values[functionName]
    },
  }
  const worker = new ProtectionWorker({ db, publicClient, account: { address }, release, executionEnabled, log: (...args) => logs.push(args) })
  worker.submit = async (...args) => { calls.push(args); return true }
  return { worker, calls, logs, observations, publicClient }
}
it('passes only the oracle ETH update fee to a triggered Book close, never the USDC reward', async () => {
  const { worker, calls } = fixture()
  await worker.evaluate()
  assert.deepEqual(calls, [['trigger', 7n, address, 'PositionProtectionBook', 'triggerPositionProtection', [7n, ['0x1234']], 13n]])
})
it('does not trigger with a frozen oracle or an unadmitted payload', async () => {
  for (const options of [{ frozen: true }, { source: 'unknown' }]) {
    const { worker, calls } = fixture(options)
    await worker.evaluate()
    assert.deepEqual(calls, [])
  }
})
it('retries a latched expiry using the permissionless zero-value path while the oracle is frozen', async () => {
  const { worker, calls } = fixture({ status: 8, frozen: true, source: 'unknown' })
  await worker.evaluate()
  assert.deepEqual(calls, [['retry', 7n, address, 'PositionProtectionBook', 'retryPositionProtectionClose', [7n]]])
})
it('prunes a sole expired head separately before considering another retry', async () => {
  const { worker, calls } = fixture({ status: 8, head: 8n, tail: 8n, expired: true })
  await worker.evaluate()
  assert.deepEqual(calls, [['prune', 7n, address, 'OrderRouter', 'executeOrder', [8n, []]]])
})
it('does not retry into a FIFO whose projected drain exceeds the expiry safety window', async () => {
  const { worker, calls, logs } = fixture({ status: 8, tail: 100n })
  await worker.evaluate()
  assert.deepEqual(calls, [])
  assert.ok(logs.some(row => row[1] === 'protection_retry_waiting' && row[2].reason === 'queue-congested'))
})
it('escalates non-expiry failures rather than automatically retrying them', async () => {
  const { worker, calls, logs } = fixture({ status: 8, reason: 5 })
  await worker.evaluate()
  assert.deepEqual(calls, [])
  assert.ok(logs.some(row => row[0] === 'error' && row[1] === 'protection_operator_required'))
})

it('publishes each latched retry blocker, scoped to the exact close attempt', async () => {
  for (const [options, expected] of [
    [{ reason: 5 }, 'operator-required'], [{ tail: 100n }, 'queue-congested'],
    [{ pendingCount: 1n }, 'pending-orders'], [{ source: 'unknown' }, 'oracle-unavailable'],
    [{ executionEnabled: false }, 'execution-disabled'],
  ]) {
    const { worker, calls, observations } = fixture({ status: 8, ...options })
    await worker.evaluate()
    assert.deepEqual(calls, [])
    assert.equal(observations.at(-1).reason, expected)
    assert.equal(observations.at(-1).protectionId, '7')
    assert.equal(observations.at(-1).linkedOrderId, '5')
    assert.equal(observations.at(-1).account, address)
    assert.equal(observations.at(-1).protectionStatus, 8)
  }
})
it('reports a missing oracle payload for armed triggers instead of silently skipping it', async () => {
  const { worker, observations, logs } = fixture({ source: 'unknown' })
  await worker.evaluate()
  assert.equal(observations.at(-1).reason, 'oracle-unavailable')
  assert.ok(logs.some(row => row[1] === 'protection_oracle_unavailable'))
})
it('does not publish retry-ready after a failed submission check', async () => {
  const { worker, observations } = fixture({ status: 8 })
  worker.submit = async () => { throw new Error('Simulation rejected') }
  await worker.evaluate()
  assert.equal(observations.at(-1).reason, 'check-failed')
})
it('rejects observations from a block that changed during evaluation', async () => {
  const { worker, calls, observations, publicClient } = fixture({ status: 8 })
  publicClient.getBlock = async ({ blockNumber }) => ({ number: 200n, timestamp: 1000n, hash: blockNumber ? '0xchanged' : '0xoriginal' })
  await assert.rejects(worker.evaluate(), /observation block changed/)
  assert.deepEqual(observations, [])
  assert.deepEqual(calls, [])
})
