import { it } from 'node:test'
import assert from 'node:assert/strict'
import { BaseError, ContractFunctionRevertedError, decodeFunctionResult, encodeErrorResult, encodeFunctionData, encodeFunctionResult, getAbiItem } from 'viem'
import { ProtectionWorker } from '../worker.mjs'
import abi from '../abi.mjs'

const address = '0x1111111111111111111111111111111111111111'
const routerAddress = '0x2222222222222222222222222222222222222222'
const oracleAddress = '0x4444444444444444444444444444444444444444'
const accountAddress = '0x5555555555555555555555555555555555555555'
const snapshot = { price: 90n, markPrice: 90n, publishTime: 999n, updateFee: 0n, maxStaleness: 60n, closeOnly: false, oracleFrozen: false, isFadWindow: false }

function oracleRevert(errorName, args = []) {
  const data = encodeErrorResult({ abi: abi.PletherOracle, errorName, args })
  return new ContractFunctionRevertedError({ abi: abi.PletherOracle, data, functionName: 'getLatestPrice' })
}

function fixture({ status = 2, frozen = false, reason = 2, tail = 0n, head = 1n, expired = false,
  source = 'backend_hermes_latest_v2', acceptedError, acceptedFeedTimes = [985n, 999n], acceptedFeedTimesError,
  signedError, pendingCount = 0n, executionEnabled = true,
} = {}) {
  const calls = []
  const simulations = []
  const reads = []
  const queries = []
  const logs = []
  const observations = []
  const release = { network: { chainId: 421614 }, release: { deploymentBlock: 100 }, contracts: {
    positionProtectionBook: { address }, orderRouter: { address: routerAddress },
    orderLifecycleBook: { address: '0x3333333333333333333333333333333333333333' },
    pletherOracle: { address: oracleAddress },
  } }
  const db = { query: async (sql, args) => {
    queries.push(sql)
    if (sql.startsWith('INSERT INTO perps_protection_observations')) observations.push(JSON.parse(args[5]))
    return { rows: sql.includes('DISTINCT ON') ? [{ protection_id: '7' }] : sql.includes('perps_pyth_update_payloads') ? [{ source, min_publish_time: 999, max_publish_time: 999, update_data: ['0x1234'] }] : [] }
  } }
  const publicClient = {
    getBlock: async () => ({ number: 200n, timestamp: 1000n, hash: '0x' + 'ab'.repeat(32) }),
    simulateContract: async request => {
      encodeFunctionData(request)
      simulations.push(request)
      // The deployed Oracle rejects current updates from every caller except Router.
      assert.equal(request.account, routerAddress, 'Oracle current updates are Router-only')
      if (signedError) throw signedError
      return { result: { ...snapshot, updateFee: 13n } }
    },
    readContract: async request => {
      // Exercise the shipped ABI even when RPC responses are deterministic.
      // A mock must not invent a method or a tuple shape absent from the contract.
      encodeFunctionData(request)
      reads.push(request)
      const { functionName } = request
      if (functionName === 'getLatestPrice' && acceptedError) throw acceptedError
      if (functionName === 'getAcceptedFeedPublishTimes' && acceptedFeedTimesError) throw acceptedFeedTimesError
      const values = {
        isOracleFrozen: frozen, getLatestPrice: snapshot, getAcceptedFeedPublishTimes: acceptedFeedTimes, getUpdateFee: 13n,
        getPositionProtection: { protectionId: 7n, parentOrderId: 0n, linkedOrderId: 5n, account: accountAddress, status, side: 0, size: 100n, takeProfitTriggerPrice: 90n, stopLossTriggerPrice: 110n, triggerBountyAssets: 0n, armedAt: 998n, armedBlock: 198n, triggerMarkPrice: 0n, triggerPublishTime: 0n, triggeredLeg: 0 },
        terminalOutcome: { account: accountAddress, terminalBlock: 199n, reason, status: 3, receiptHash: '0x' + 'cd'.repeat(32) }, nextExecuteId: head, globalTailOrderId: tail,
        orderTiming: { submitBy: 940n, executionWindowSeconds: 60, commitTimestamp: 941n, executionDeadline: expired ? 999n : 1001n }, maxExecutionWindowSeconds: 60n, pendingOrderCounts: pendingCount,
      }
      if (!(functionName in values)) throw new Error(`Unexpected ${functionName}`)
      const overload = getAbiItem({ abi: request.abi, name: functionName, args: request.args })
      const data = encodeFunctionResult({ abi: [overload], functionName, result: values[functionName] })
      return decodeFunctionResult({ ...request, data })
    },
  }
  const worker = new ProtectionWorker({ db, publicClient, account: { address: accountAddress }, release, executionEnabled, log: (...args) => logs.push(args) })
  worker.submit = async (...args) => {
    encodeFunctionData({ abi: abi[args[3]], functionName: args[4], args: args[5] })
    calls.push(args)
    return true
  }
  return { worker, calls, simulations, reads, queries, logs, observations, publicClient }
}
it('uses the block-pinned accepted basket and triggers with an empty payload and no fee', async () => {
  const { worker, calls, reads, queries, simulations } = fixture({ source: 'unknown' })
  await worker.evaluate()
  const read = reads.find(row => row.functionName === 'getLatestPrice')
  assert.equal(read.address, oracleAddress)
  assert.deepEqual(read.args, [1])
  assert.equal(read.blockNumber, 200n)
  assert.equal(reads.find(row => row.functionName === 'getAcceptedFeedPublishTimes').blockNumber, 200n)
  assert.ok(!queries.some(sql => sql.includes('perps_pyth_update_payloads')))
  assert.deepEqual(simulations, [])
  assert.deepEqual(calls, [['trigger', 7n, address, 'PositionProtectionBook', 'triggerPositionProtection', [7n, []], 0n]])
})
it('recovers a rejected accepted basket with an exact Router simulation and matching signed trigger payload and fee', async () => {
  const acceptedError = new BaseError('read failed', { cause: oracleRevert('PletherOracle__PriceOutOfOrder', [997n, 998n]) })
  const { worker, calls, reads, simulations } = fixture({ acceptedError })
  await worker.evaluate()
  assert.equal(simulations[0].account, routerAddress)
  assert.equal(simulations[0].address, oracleAddress)
  assert.deepEqual(simulations[0].args, [accountAddress, ['0x1234'], 1])
  assert.equal(simulations[0].value, 13n)
  assert.equal(simulations[0].blockNumber, 200n)
  const feeRead = reads.find(row => row.functionName === 'getUpdateFee')
  assert.deepEqual(feeRead.args, [['0x1234']])
  assert.equal(feeRead.blockNumber, 200n)
  assert.equal(worker.account.address, accountAddress)
  assert.deepEqual(calls, [['trigger', 7n, address, 'PositionProtectionBook', 'triggerPositionProtection', [7n, ['0x1234']], 13n]])
})
it('uses a fresh signed update when one accepted feed exceeds the local age limit', async () => {
  const { worker, calls, simulations } = fixture({ acceptedFeedTimes: [984n, 999n] })
  await worker.evaluate()
  assert.equal(simulations.length, 1)
  assert.deepEqual(simulations[0].args, [accountAddress, ['0x1234'], 1])
  assert.equal(simulations[0].value, 13n)
  assert.deepEqual(calls, [['trigger', 7n, address, 'PositionProtectionBook', 'triggerPositionProtection', [7n, ['0x1234']], 13n]])
})
it('does not authorize an oracle trigger when individual accepted feed timestamps cannot be verified', async () => {
  for (const options of [
    { acceptedFeedTimes: [] },
    { acceptedFeedTimes: [0n, 999n] },
    { acceptedFeedTimes: [997n, 998n] },
    { acceptedFeedTimes: [999n, 1001n] },
    { acceptedFeedTimesError: new Error('RPC failed') },
  ]) {
    const { worker, calls, simulations, queries, observations } = fixture(options)
    await worker.evaluate()
    assert.deepEqual(calls, [])
    assert.deepEqual(simulations, [])
    assert.ok(!queries.some(sql => sql.includes('perps_pyth_update_payloads')))
    assert.equal(observations.at(-1).reason, 'oracle-unavailable')
  }
})
it('does not trigger with a frozen oracle or an unadmitted recovery payload', async () => {
  for (const options of [{ frozen: true }, { source: 'unknown', acceptedError: oracleRevert('PletherOracle__NoAcceptedBasket') }]) {
    const { worker, calls } = fixture(options)
    await worker.evaluate()
    assert.deepEqual(calls, [])
  }
})
it('only attempts signed recovery for decoded basket errors and logs no provider details', async () => {
  const { worker, calls, simulations, queries, observations, logs } = fixture({ acceptedError: new Error('secret provider detail') })
  await worker.evaluate()
  assert.deepEqual(calls, [])
  assert.deepEqual(simulations, [])
  assert.ok(!queries.some(sql => sql.includes('perps_pyth_update_payloads')))
  assert.equal(observations.at(-1).reason, 'oracle-unavailable')
  assert.ok(logs.some(row => row[1] === 'protection_oracle_unavailable' && row[2].reason === 'accepted-basket-check-failed'))
  assert.ok(!JSON.stringify(logs).includes('secret provider detail'))
})
it('fails closed when the admitted signed recovery payload trails an accepted feed', async () => {
  const { worker, calls, observations, logs } = fixture({
    acceptedError: oracleRevert('PletherOracle__NoAcceptedBasket'),
    signedError: oracleRevert('PletherOracle__FeedTimeOutOfOrder', ['0x' + 'ff'.repeat(32), 999n, 1000n]),
  })
  await worker.evaluate()
  assert.deepEqual(calls, [])
  assert.equal(observations.at(-1).reason, 'oracle-unavailable')
  assert.deepEqual(logs.at(-1), ['warn', 'protection_oracle_unavailable', {
    reason: 'signed-basket-check-failed', oracleError: 'PletherOracle__FeedTimeOutOfOrder', blockNumber: '200',
  }])
})
it('rechecks the exact Book transaction with the signer before any signing or broadcast', async () => {
  const { worker, publicClient, simulations, observations } = fixture()
  worker.submit = ProtectionWorker.prototype.submit.bind(worker)
  worker.walletClient = { prepareTransactionRequest: async () => assert.fail('Rejected simulation must block signing') }
  publicClient.simulateContract = async request => {
    simulations.push(request)
    throw new Error('The accepted basket changed before the Book transaction check')
  }
  await worker.evaluate()
  assert.equal(simulations.length, 1)
  const [request] = simulations
  assert.equal(request.account.address, accountAddress)
  assert.equal(request.functionName, 'triggerPositionProtection')
  assert.deepEqual(request.args, [7n, []])
  assert.equal(request.value, 0n)
  assert.equal(observations.at(-1).reason, 'check-failed')
})
it('defers an empty-payload trigger whose accepted basket ages during candidate processing', async () => {
  const { worker, publicClient, calls, observations, queries } = fixture()
  const getBlock = publicClient.getBlock
  let headReads = 0
  publicClient.getBlock = async request => {
    const block = await getBlock(request)
    if (request.blockTag === 'latest' && ++headReads === 2) {
      return { ...block, number: 201n, timestamp: 1001n }
    }
    return block
  }
  await worker.evaluate()
  assert.equal(headReads, 2)
  assert.deepEqual(calls, [])
  assert.ok(!queries.some(sql => sql.includes('perps_pyth_update_payloads')))
  assert.equal(observations.at(-1).reason, 'check-failed')
})
it('retries a latched expiry using the permissionless zero-value path while the oracle is frozen', async () => {
  const { worker, calls, observations, logs } = fixture({ status: 8, frozen: true, source: 'unknown' })
  await worker.evaluate()
  assert.deepEqual(calls, [['retry', 7n, address, 'PositionProtectionBook', 'retryPositionProtectionClose', [7n]]])
  assert.equal(observations.at(-1).reason, 'retry-ready')
  assert.equal(logs.some(row => row[1] === 'protection_simulation_failed'), false)
})
it('prunes a sole expired head separately before considering another retry', async () => {
  const { worker, calls } = fixture({ status: 8, head: 8n, tail: 8n, expired: true })
  await worker.evaluate()
  assert.deepEqual(calls, [['prune', 7n, routerAddress, 'OrderRouter', 'executeOrder', [8n, []]]])
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
    [{ pendingCount: 1n }, 'pending-orders'],
    [{ source: 'unknown', acceptedError: oracleRevert('PletherOracle__NoAcceptedBasket') }, 'oracle-unavailable'],
    [{ executionEnabled: false }, 'execution-disabled'],
  ]) {
    const { worker, calls, observations } = fixture({ status: 8, ...options })
    await worker.evaluate()
    assert.deepEqual(calls, [])
    assert.equal(observations.at(-1).reason, expected)
    assert.equal(observations.at(-1).protectionId, '7')
    assert.equal(observations.at(-1).linkedOrderId, '5')
    assert.equal(observations.at(-1).account, accountAddress)
    assert.equal(observations.at(-1).protectionStatus, 8)
  }
})
it('reports a missing oracle payload for armed triggers instead of silently skipping it', async () => {
  const { worker, observations, logs } = fixture({ source: 'unknown', acceptedError: oracleRevert('PletherOracle__NoAcceptedBasket') })
  await worker.evaluate()
  assert.equal(observations.at(-1).reason, 'oracle-unavailable')
  assert.ok(logs.some(row => row[1] === 'protection_oracle_unavailable' && row[2].reason === 'no-admitted-fresh-payload'))
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

it('rejects historical release manifests before reading or executing V3 contracts', async () => {
  const release = { network: { chainId: 421614 }, release: { deploymentBlock: 100 }, contracts: Object.fromEntries(['positionProtectionBook', 'orderRouter', 'orderLifecycleBook', 'pletherOracle'].map(name => [name, { address: '0x1111111111111111111111111111111111111111' }])) }
  const worker = new ProtectionWorker({ release })
  await assert.rejects(worker.verifyDeployment(), /verified V3 release manifest/)
})
