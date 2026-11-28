import { createSocket } from 'node:dgram'
import { once } from 'node:events'
import { readFileSync } from 'node:fs'
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { ContractFunctionExecutionError, ContractFunctionRevertedError, createPublicClient, createWalletClient, custom, encodeErrorResult, parseAbi, parseTransaction, toHex } from 'viem'
import { privateKeyToAccount } from 'viem/accounts'
import { arbitrumSepolia } from 'viem/chains'
import { createOracleWakeupReceiver, createOracleWorker, loadOracleFeeds, resolveMarkRefreshGasLimit, validatePythPayload } from '../scripts/perps-oracle-worker.mjs'

const release = JSON.parse(readFileSync(new URL('../../../config/perps/arbitrum-sepolia-v2.1.0.json', import.meta.url), 'utf8'))
const feeds = { pyth: `0x${'11'.repeat(20)}`, feedIds: Array.from({ length: 6 }, (_, i) => `0x${String(i + 1).padStart(64, '0')}`) }
const healthErrorAbi = parseAbi([
  'error PletherOracle__NoAcceptedBasket()',
  'error PletherOracle__PriceOutOfOrder(uint64 publishTime, uint64 lastMarkTime)',
  'error PletherOracle__EqualTimeMarkMismatch(uint64 publishTime, uint256 basketPrice, uint256 enginePrice)',
  'error PletherOracle__StalePrice(uint8 mode, bytes32 feedId, uint256 publishTime, uint256 maxStaleness, uint256 currentTimestamp)',
  'error PletherOracle__BasketConfidenceTooWide(uint8 mode, uint256 basketConfidence, uint256 basketPrice, uint256 maxConfidenceBps)',
])
function healthRevert(errorName, args = []) {
  return new ContractFunctionExecutionError(new ContractFunctionRevertedError({
    abi: healthErrorAbi, functionName: 'getLatestPrice',
    data: encodeErrorResult({ abi: healthErrorAbi, errorName, args }),
  }), { abi: healthErrorAbi, functionName: 'getLatestPrice', args: [1] })
}
function fixture() {
  const state = { now: 1_000_000, mark: 990n, stored: 980n, publishes: Array(6).fill(990), writes: 0,
    accepted: 980n, acceptedTimes: Array(6).fill(980n), hasAccepted: true, frozen: false, timestamp: 1000n,
    markPrice: 100_000_000n, acceptedPrice: 100_000_000n, healthError: undefined,
    reads: [], logs: [], freshFetches: 0, waitError: undefined, simulationError: undefined, estimateError: undefined,
    estimatedGas: 500_000n, reverted: false, pendingWait: undefined }
  const publicClient = {
    getBlock: async () => ({ number: 42n, timestamp: 1000n }),
    readContract: async input => {
      state.reads.push(input)
      if (input.functionName === 'pyth') return feeds.pyth
      if (input.functionName === 'pythFeedIds') return feeds.feedIds[Number(input.args[0])]
      if (input.functionName === 'getProtocolStatus') return { lastMarkTime: state.mark, lastMarkPrice: state.markPrice, oracleFrozen: state.frozen }
      if (input.functionName === 'getAcceptedBasketMark') return [state.hasAccepted, state.acceptedPrice, state.accepted]
      if (input.functionName === 'getLatestPrice') {
        assert.deepEqual(input.args, [1], 'worker must validate MarkRefresh, not open or liquidation policy')
        if (state.healthError) throw state.healthError
        const times = state.frozen ? [state.mark] : state.acceptedTimes
        if (!state.frozen && !state.hasAccepted) throw healthRevert('PletherOracle__NoAcceptedBasket')
        for (const time of times) {
          if (time > state.timestamp || state.timestamp-time > 60n) {
            throw healthRevert('PletherOracle__StalePrice', [1, feeds.feedIds[0], time, 60n, state.timestamp])
          }
        }
        const publishTime = state.frozen ? state.mark : state.accepted
        if (publishTime < state.mark) throw healthRevert('PletherOracle__PriceOutOfOrder', [publishTime, state.mark])
        if (!state.frozen && publishTime === state.mark && state.acceptedPrice !== state.markPrice) {
          throw healthRevert('PletherOracle__EqualTimeMarkMismatch', [publishTime, state.acceptedPrice, state.markPrice])
        }
        return { price: state.frozen ? state.markPrice : state.acceptedPrice,
          markPrice: state.frozen ? state.markPrice : state.acceptedPrice, publishTime }
      }
      if (input.functionName === 'getUpdateFee') return 6n
      throw new Error(`Unexpected read ${input.functionName}`)
    },
    getBalance: async () => 1000n,
    estimateContractGas: async () => {
      if (state.estimateError) throw state.estimateError
      return state.estimatedGas
    },
    simulateContract: async input => {
      if (state.simulationError) throw state.simulationError
      return { request: input }
    },
    waitForTransactionReceipt: async () => {
      if (state.pendingWait) await state.pendingWait
      if (state.waitError) throw state.waitError
      if (!state.reverted) {
        state.accepted = BigInt(Math.max(...state.publishes)); state.mark = state.accepted
        state.acceptedTimes = state.publishes.map(BigInt)
        state.hasAccepted = true; state.acceptedPrice = state.markPrice
      }
      return { status: state.reverted ? 'reverted' : 'success', blockNumber: 43n, gasUsed: 418_000n }
    },
  }
  const options = { publicClient, walletClient: { writeContract: async () => { state.writes++; return '0x1234' } },
    account: { address: feeds.pyth }, now: () => state.now, pause: async () => {},
    fetchPayload: async () => ({ updateData: ['0xabcd'], publishTimes: state.publishes }),
    fetchFreshPayload: async () => { state.freshFetches++; return { updateData: ['0xabcd'], publishTimes: state.publishes } },
    log: (...args) => state.logs.push(args), logEvery: (...args) => state.logs.push(args) }
  return { state, publicClient, options, iterate: createOracleWorker(options) }
}

async function loopbackWakeup(t) {
  const socket = createSocket('udp4')
  const listening = once(socket, 'listening')
  const receiver = createOracleWakeupReceiver({ port: 0, socket })
  t.after(() => receiver.close())
  await listening
  assert.equal(socket.address().address, '127.0.0.1')
  const sender = createSocket('udp4')
  t.after(() => sender.close())
  async function send(packet = 'plether-oracle-wake-v1') {
    const received = once(socket, 'message')
    await new Promise((resolve, reject) => {
      sender.send(packet, socket.address().port, '127.0.0.1', error => error ? reject(error) : resolve())
    })
    await received
  }
  return { receiver, send }
}

test('only the exact keeper packet interrupts the normal poll', async t => {
  const { receiver, send } = await loopbackWakeup(t)
  const poll = receiver.wait(50)
  await send('plether-oracle-wake-v1\n')
  assert.equal(await poll, false)

  const interrupted = receiver.wait(1000)
  await send()
  assert.equal(await interrupted, true)

  const f = fixture()
  assert.equal((await f.iterate()).status, 'synchronized')
  f.state.now += 1000
  f.state.publishes.fill(995)
  assert.equal((await f.iterate()).status, 'healthy')
  assert.equal(f.state.writes, 1)
})

test('keeper packets received during active work queue just one subsequent check', async t => {
  const { receiver, send } = await loopbackWakeup(t)
  const f = fixture()
  let finishReceipt
  f.state.pendingWait = new Promise(resolve => { finishReceipt = resolve })
  const iteration = f.iterate()
  try {
    await send()
    await send()
    await send()
    assert.equal(f.state.writes, 1)
  } finally {
    finishReceipt()
    await iteration
  }
  assert.equal(await receiver.wait(1000), true)
  assert.equal(await receiver.wait(1100), false)
})

test('keeper wake-ups are at least one second apart and cannot delay a due poll', async t => {
  const { receiver, send } = await loopbackWakeup(t)
  await send()
  assert.equal(await receiver.wait(1000), true)
  const firstWake = performance.now()

  await send()
  const pollStart = performance.now()
  assert.equal(await receiver.wait(50), false)
  assert.ok(performance.now() - pollStart < 900, 'ordinary poll waited for the wake-up throttle')

  await send()
  assert.equal(await receiver.wait(1500), true)
  assert.ok(performance.now() - firstWake >= 999, 'keeper notification bypassed the throttle')
})

test('a bound wake-up port leaves ordinary polling available', async t => {
  const occupied = createSocket('udp4')
  t.after(() => occupied.close())
  occupied.bind(0, '127.0.0.1')
  await once(occupied, 'listening')

  const socket = createSocket('udp4')
  const disabled = new Promise(resolve => socket.once('close', resolve))
  const logs = []
  const receiver = createOracleWakeupReceiver({
    socket, port: occupied.address().port, logEvery: (...args) => logs.push(args),
  })
  t.after(() => receiver.close())
  await disabled
  assert.equal(await receiver.wait(25), false)
  assert.equal(logs.length, 1)
  assert.deepEqual(logs[0].slice(0, 3), [60, 'WARN', 'oracle_wakeup_unavailable'])
})

test('startup discovers the configured Pyth endpoint and six distinct feeds at one block', async () => {
  const f = fixture()
  assert.deepEqual(await loadOracleFeeds(f.publicClient), feeds)
  assert.ok(f.state.reads.every(read => read.blockNumber === 42n))
  assert.ok(f.state.reads.every(read => read.address === release.contracts.pletherOracle.address))
})
test('repairs the accepted basket at the engine timestamp without changing Pyth storage', async () => {
  const f = fixture()
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.accepted, 990n)
  assert.equal(f.state.stored, 980n)
  assert.equal(f.state.mark, 990n)
  assert.equal(f.state.writes, 1)
  assert.ok(f.state.reads.filter(read => read.functionName !== 'getUpdateFee').every(read => read.blockNumber === 42n))
  assert.equal((await f.iterate()).status, 'healthy')
  assert.equal(f.state.writes, 1)
})
test('Core v2 permits an older component when the newest component covers the mark and validation succeeds', async () => {
  const f = fixture(); f.state.publishes = [989, 991, 991, 991, 991, 991]
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.accepted, 991n)
  assert.equal(f.state.acceptedTimes[0], 989n)
  assert.equal((await f.iterate()).status, 'healthy')
  assert.equal(f.state.writes, 1)
})
test('healthy markets refresh on the ordinary cadence, while lag bypasses it', async () => {
  const f = fixture(); f.state.accepted = 990n
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
test('fresh accepted prices and old Pyth storage do not create a repair loop', async () => {
  const f = fixture()
  f.state.accepted = f.state.mark
  f.state.acceptedTimes = [989n, 990n, 990n, 990n, 990n, 990n]
  f.state.stored = 1n
  f.state.publishes.fill(995)
  assert.equal((await f.iterate()).status, 'synchronized')
  for (let i = 0; i < 5; i++) {
    f.state.now += 5000
    assert.equal((await f.iterate()).status, 'healthy')
  }
  assert.equal(f.state.writes, 1)
  assert.equal(f.state.stored, 1n)
  assert.equal(f.state.reads.filter(read => read.functionName === 'getPriceUnsafe').length, 0)
  assert.ok(!f.state.logs.some(args => args.includes('oracle_sync_lag') || args.includes('oracle_sync_pending')))
  f.state.now += 5000
  f.state.publishes.fill(999)
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 2)
})
test('each accepted component is validated even when the newest time matches the mark', async () => {
  const f = fixture()
  await f.iterate()
  f.state.now += 5000
  f.state.acceptedTimes[0] = 939n // One expired component, with zero mark-ordering lag.
  f.state.publishes.fill(995)
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 2)
  assert.ok(f.state.logs.some(args => args.includes('oracle_sync_lag')))
})
test('missing and equal-time mismatched accepted baskets are refreshed', async () => {
  for (const scenario of ['missing', 'mismatched']) {
    const f = fixture()
    f.state.accepted = f.state.mark
    if (scenario === 'missing') f.state.hasAccepted = false
    else f.state.acceptedPrice--
    assert.equal((await f.iterate()).status, 'synchronized')
    assert.equal(f.state.writes, 1)
  }
})
test('frozen policy validates the engine mark without trying to repair the unused live basket', async () => {
  const f = fixture()
  f.state.frozen = true
  f.state.hasAccepted = false
  f.state.accepted = 1n
  f.state.acceptedTimes.fill(1n)
  for (let i = 0; i < 10; i++) {
    assert.equal((await f.iterate()).status, 'healthy')
    f.state.now += 5000
  }
  assert.equal(f.state.writes, 0)
  f.state.timestamp = 1100n
  await assert.rejects(f.iterate(), /PletherOracle__StalePrice/)
  assert.equal(f.state.writes, 0)
})
test('a frozen-policy transition during fee lookup prevents simulation and submission', async () => {
  const f = fixture()
  const read = f.publicClient.readContract
  f.publicClient.readContract = async input => {
    const result = await read(input)
    if (input.functionName === 'getUpdateFee') f.state.frozen = true
    return result
  }
  f.publicClient.simulateContract = async () => { throw new Error('must not simulate') }
  assert.equal((await f.iterate()).status, 'healthy')
  assert.equal(f.state.writes, 0)
})
test('RPC failures, unknown reverts and invalid snapshots never authorize a paid refresh', async () => {
  for (const error of [new Error('RPC unavailable'), new Error('PletherOracle__PriceOutOfOrder')]) {
    const f = fixture(); f.state.healthError = error
    await assert.rejects(f.iterate(), error)
    assert.equal(f.state.writes, 0)
  }
  const f = fixture()
  f.state.healthError = new ContractFunctionExecutionError(new ContractFunctionRevertedError({
    abi: parseAbi(['error Unexpected()']), data: encodeErrorResult({ abi: parseAbi(['error Unexpected()']), errorName: 'Unexpected' }),
  }), { abi: [], functionName: 'getLatestPrice' })
  await assert.rejects(f.iterate(), /Unexpected/)
  assert.equal(f.state.writes, 0)
  const invalid = fixture(); invalid.state.accepted = invalid.state.mark
  invalid.state.acceptedPrice = 0n; invalid.state.markPrice = 0n
  await assert.rejects(invalid.iterate(), /invalid mark-refresh snapshot/)
  assert.equal(invalid.state.writes, 0)
})
test('oldest component age and future publication prevent submission', async () => {
  const f = fixture(); f.state.mark = 940n; f.state.accepted = 930n; f.state.acceptedTimes.fill(930n)
  f.state.publishes = [940, 980, 980, 980, 980, 980]
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

test('scheduled refresh and repair preflight and sign at the same gas limit', async () => {
  const f = fixture()
  const account = privateKeyToAccount(`0x${'1'.padStart(64, '0')}`)
  const estimates = [466_000n, 3_782_012n]
  const expectedGasLimits = [1_200_000n, 4_727_515n]
  const simulations = []
  const transactions = []
  let estimateCalls = 0
  const transport = custom({ async request({ method, params }) {
    if (method === 'eth_estimateGas') {
      return toHex(estimates[estimateCalls++])
    }
    if (method === 'eth_call') {
      simulations.push(params[0])
      return '0x'
    }
    if (method === 'eth_fillTransaction') {
      const request = params[0]
      const filledGas = request.gas === undefined ? estimates[estimateCalls++] : 466_000n
      return { tx: { ...request, input: request.data, gas: toHex(filledGas), nonce: toHex(transactions.length),
        type: '0x2', chainId: toHex(arbitrumSepolia.id), maxPriorityFeePerGas: '0x1', maxFeePerGas: '0x2' } }
    }
    if (method === 'eth_sendRawTransaction') {
      transactions.push(parseTransaction(params[0]))
      return `0x${'1'.repeat(64)}`
    }
    throw new Error(`Unexpected mock RPC ${method}`)
  } }, { retryCount: 0 })
  const clientOptions = { chain: arbitrumSepolia, transport }
  const viemPublicClient = createPublicClient(clientOptions)
  f.publicClient.estimateContractGas = viemPublicClient.estimateContractGas
  f.publicClient.simulateContract = viemPublicClient.simulateContract
  const iterate = createOracleWorker({ ...f.options, account, walletClient: createWalletClient(clientOptions) })

  assert.equal((await iterate()).status, 'synchronized')
  f.state.now += 30_000
  f.state.publishes.fill(995)
  assert.equal((await iterate()).status, 'synchronized')
  assert.equal(estimateCalls, 2)
  assert.equal(simulations.length, 2)
  assert.equal(transactions.length, 2)
  for (const [index, tx] of transactions.entries()) {
    const simulation = simulations[index]
    assert.equal(BigInt(simulation.gas), expectedGasLimits[index])
    assert.equal(tx.gas, expectedGasLimits[index])
    assert.equal(tx.to.toLowerCase(), simulation.to.toLowerCase())
    assert.equal(tx.data, simulation.data)
    assert.equal(tx.value, BigInt(simulation.value))
  }
  const minedLogs = f.state.logs.filter(args => args[1] === 'oracle_update_mined').map(args => args[3])
  assert.deepEqual(minedLogs.map(log => log.repair), [true, false])
  assert.deepEqual(minedLogs.map(log => log.gas_limit), expectedGasLimits)
})

test('resolveMarkRefreshGasLimit applies floor, 25% buffer and 40M safety cap', () => {
  assert.equal(resolveMarkRefreshGasLimit(466_000n), 1_200_000n)
  assert.equal(resolveMarkRefreshGasLimit(960_000n), 1_200_000n)
  assert.equal(resolveMarkRefreshGasLimit(3_782_012n), 4_727_515n)
  assert.equal(resolveMarkRefreshGasLimit(3_782_013n), 4_727_517n)
  assert.equal(resolveMarkRefreshGasLimit(35_000_000n), 40_000_000n)
  assert.equal(resolveMarkRefreshGasLimit(40_000_000n), 40_000_000n)
  assert.throws(() => resolveMarkRefreshGasLimit(40_000_001n), /exceeds safety limit/)
  assert.throws(() => resolveMarkRefreshGasLimit(0n), /Invalid mark-refresh gas estimate/)
  assert.throws(() => resolveMarkRefreshGasLimit(-1n), /Invalid mark-refresh gas estimate/)
})

test('gas estimate above the safety cap aborts before simulation and backs off', async () => {
  const f = fixture()
  f.state.estimatedGas = 40_000_001n
  let simulations = 0
  f.publicClient.simulateContract = async () => { simulations++; throw new Error('must not simulate') }
  await assert.rejects(f.iterate(), /exceeds safety limit/)
  assert.equal(simulations, 0)
  assert.equal(f.state.writes, 0)
  f.state.now += 4999
  assert.equal((await f.iterate()).status, 'backoff')
})

test('wide confidence during gas estimation waits without simulating or submitting', async () => {
  const f = fixture()
  f.state.estimateError = confidenceRevert()
  let simulations = 0
  f.publicClient.simulateContract = async () => { simulations++; throw new Error('must not simulate') }
  assert.equal((await f.iterate()).status, 'confidence_too_wide')
  assert.equal(simulations, 0)
  assert.equal(f.state.writes, 0)
})

test('simulation that exhausts the gas limit sends nothing and honors backoff', async () => {
  const f = fixture()
  let simulations = 0
  f.publicClient.simulateContract = async ({ gas }) => {
    assert.equal(gas, 1_200_000n)
    simulations++
    throw new Error('out of gas')
  }
  await assert.rejects(f.iterate(), /out of gas/)
  f.state.now += 4999
  assert.equal((await f.iterate()).status, 'backoff')
  assert.equal(simulations, 1)
  assert.equal(f.state.writes, 0)
})

const confidenceErrorAbi = parseAbi(['error PletherOracle__BasketConfidenceTooWide(uint8 mode, uint256 basketConfidence, uint256 basketPrice, uint256 maxConfidenceBps)'])
function confidenceRevert(abi = confidenceErrorAbi) {
  return new ContractFunctionExecutionError(new ContractFunctionRevertedError({
    abi, functionName: 'updateMarkPrice', data: encodeErrorResult({
      abi: confidenceErrorAbi, errorName: 'PletherOracle__BasketConfidenceTooWide',
      args: [1, 150_000n, 100_000_000n, 10n],
    }),
  }), { abi, functionName: 'updateMarkPrice', args: [['0xabcd']] })
}
test('wide confidence waits without submitting, logs the decoded ratio and recovers with a narrower payload', async () => {
  const f = fixture()
  let rejected = true
  f.publicClient.simulateContract = async input => {
    if (rejected) throw confidenceRevert(input.abi)
    return { request: input }
  }
  assert.equal((await f.iterate()).status, 'confidence_too_wide')
  assert.equal(f.state.writes, 0)
  const warning = f.state.logs.find(args => args[2] === 'oracle_update_confidence_too_wide')
  assert.equal(warning[0], 60)
  assert.equal(warning[1], 'WARN')
  assert.equal(warning[4].confidence_ratio_bps, 15)
  assert.equal(warning[4].max_confidence_bps, 10n)
  assert.equal(warning[4].basket_price, 100_000_000n)
  const reads = f.state.reads.length
  f.state.now += 4999
  assert.equal((await f.iterate()).status, 'backoff')
  assert.equal(f.state.reads.length, reads)
  rejected = false
  f.state.now++
  f.state.publishes.fill(995)
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 1)
})
test('confidence-like error text is not treated as a decoded contract rejection', async () => {
  const f = fixture()
  f.state.simulationError = new Error('RPC failed: PletherOracle__BasketConfidenceTooWide')
  await assert.rejects(f.iterate(), /RPC failed/)
  assert.equal(f.state.writes, 0)
})
test('a receipt error is still reconciled before another submission even when it contains a confidence revert', async () => {
  const f = fixture()
  f.state.waitError = confidenceRevert()
  await assert.rejects(f.iterate(), ContractFunctionExecutionError)
  await assert.rejects(f.iterate(), ContractFunctionExecutionError)
  assert.equal(f.state.writes, 1)
  f.state.waitError = undefined
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 1)
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
  const reverted = f.state.logs.find(args => args[2] === 'oracle_update_reverted')[4]
  assert.equal(reverted.gas_limit, 1_200_000n)
  assert.equal(reverted.gas_used, 418_000n)
  assert.equal(f.state.stored, 980n)
  f.state.reverted = false
  f.state.now += 5000
  assert.equal((await f.iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 2)
})

test('mined gas telemetry reflects the replacement transaction', async () => {
  const f = fixture()
  const wait = f.publicClient.waitForTransactionReceipt
  f.publicClient.waitForTransactionReceipt = async ({ onReplaced }) => {
    onReplaced({ transaction: { hash: '0x4567', gas: 1_400_000n } })
    return wait()
  }
  assert.equal((await f.iterate()).hash, '0x4567')
  const mined = f.state.logs.find(args => args[1] === 'oracle_update_mined')[3]
  assert.equal(mined.transaction_hash, '0x4567')
  assert.equal(mined.gas_limit, 1_400_000n)
  assert.equal(mined.gas_used, 418_000n)
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

test('slow payload fetch is checked against a fresh block, not a pre-fetch timestamp', async () => {
  const f = fixture()
  let timestamp = 1000n
  f.publicClient.getBlock = async () => ({ number: timestamp, hash: `0x${timestamp}`, timestamp })
  const iterate = createOracleWorker({ ...f.options, fetchPayload: async () => {
    timestamp = 1010n
    f.state.timestamp = timestamp
    f.state.publishes.fill(1008)
    return f.options.fetchPayload()
  } })
  assert.equal((await iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 1)
})

test('mark advancement during payload fetch prevents fee lookup and submission', async () => {
  const f = fixture()
  const iterate = createOracleWorker({ ...f.options, fetchPayload: async () => {
    f.state.mark = 995n
    return f.options.fetchPayload()
  } })
  assert.equal((await iterate()).status, 'waiting_for_payload')
  assert.equal(f.state.reads.filter(read => read.functionName === 'getUpdateFee').length, 0)
  assert.equal(f.state.writes, 0)
})

test('stale payload retries fetch fresh data after a bounded delay', async () => {
  const f = fixture()
  f.state.publishes.fill(1001)
  const iterate = createOracleWorker(f.options)
  assert.equal((await iterate()).status, 'stale_payload')
  f.state.publishes.fill(995)
  f.state.now += 4999
  assert.equal((await iterate()).status, 'backoff')
  f.state.now++
  assert.equal((await iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 1)
})

test('rechecks age, future time and concurrent mark changes after slow fee/balance calls', async () => {
  for (const stage of ['fee', 'balance']) {
    for (const change of ['expired', 'future', 'mark_advanced', 'already_repaired']) {
      const f = fixture()
      let timestamp = 1000n
      let blockNumber = 42n
      f.publicClient.getBlock = async () => ({ number: blockNumber, hash: `0x${blockNumber}`, timestamp })
      let simulations = 0
      f.publicClient.simulateContract = async () => { simulations++; throw new Error('must not simulate') }
      const changeChain = () => {
        blockNumber++
        if (change === 'expired') timestamp = 1100n
        if (change === 'future') timestamp = 989n
        if (change === 'mark_advanced') f.state.mark = 995n
        if (change === 'already_repaired') f.state.accepted = f.state.mark
      }
      if (stage === 'fee') {
        const read = f.publicClient.readContract
        f.publicClient.readContract = async input => {
          const result = await read(input)
          if (input.functionName === 'getUpdateFee') changeChain()
          return result
        }
      } else {
        f.publicClient.getBalance = async () => { changeChain(); return 1000n }
      }
      const status = (await f.iterate()).status
      assert.equal(status, ['expired', 'future'].includes(change) ? 'stale_payload' : 'waiting_for_payload', `${stage}: ${change}`)
      assert.equal(simulations, 0)
      assert.equal(f.state.writes, 0)
    }
  }
})

test('same-block freshness checks reuse health reads, but same-height reorgs do not', async () => {
  const f = fixture()
  let hash = '0xaaa'
  f.publicClient.getBlock = async () => ({ number: 42n, hash, timestamp: 1000n })
  await f.iterate()
  // Initial and post-receipt health only; no Pyth storage reads.
  assert.equal(f.state.reads.filter(read => read.functionName === 'getLatestPrice').length, 2)
  assert.equal(f.state.reads.filter(read => read.functionName === 'getAcceptedBasketMark').length, 2)

  f.state.now += 30_000
  f.state.publishes.fill(995)
  f.publicClient.getBalance = async () => { hash = '0xbbb'; f.state.mark = 999n; return 1000n }
  assert.equal((await f.iterate()).status, 'waiting_for_payload')
  assert.equal(f.state.writes, 1)
})

test('an engine ahead of the DB cache is repaired with one newer admitted payload', async () => {
  const f = fixture(); f.state.mark = 995n
  let freshFetches = 0
  const iterate = createOracleWorker({ ...f.options, fetchFreshPayload: async () => {
    freshFetches++
    f.state.publishes.fill(995)
    return f.options.fetchPayload()
  } })
  assert.equal((await iterate()).status, 'synchronized')
  assert.equal(freshFetches, 1)
  assert.equal(f.state.writes, 1)
  assert.equal(f.state.accepted, 995n)
  assert.ok(f.state.logs.some(args => args.includes('oracle_repair_payload_refreshed')))
})

test('equal-time repairs and healthy ordinary refreshes do not request a fresh fallback', async () => {
  for (const healthy of [false, true]) {
    const f = fixture()
    if (healthy) { f.state.accepted = f.state.mark; f.state.publishes.fill(995) }
    assert.equal((await f.iterate()).status, 'synchronized')
    assert.equal(f.state.freshFetches, 0)
    assert.equal(f.state.writes, 1)
  }
})

test('a fresh payload still behind the mark waits without looping or spending', async () => {
  const f = fixture(); f.state.mark = 995n
  assert.equal((await f.iterate()).status, 'waiting_for_payload')
  assert.equal(f.state.freshFetches, 1)
  assert.equal(f.state.writes, 0)
  // Repeated callers cannot amplify fresh provider traffic beyond five seconds.
  f.state.now += 4999
  assert.equal((await f.iterate()).status, 'waiting_for_payload')
  assert.equal(f.state.freshFetches, 1)
  f.state.now++
  assert.equal((await f.iterate()).status, 'waiting_for_payload')
  assert.equal(f.state.freshFetches, 2)
  assert.equal(f.state.writes, 0)
})

test('malformed, future and stale fresh payloads cannot authorize a repair', async () => {
  for (const kind of ['malformed', 'future', 'stale']) {
    const f = fixture(); f.state.mark = 995n
    const iterate = createOracleWorker({ ...f.options, fetchFreshPayload: async () => ({
      updateData: ['0xabcd'],
      publishTimes: kind === 'malformed' ? [] : kind === 'future' ? Array(6).fill(1001) : [949, 995, 995, 995, 995, 995],
    }) })
    if (kind === 'malformed') await assert.rejects(iterate(), /six feed publish times/)
    else assert.equal((await iterate()).status, 'stale_payload')
    assert.equal(f.state.reads.filter(read => read.functionName === 'getUpdateFee').length, 0)
    assert.equal(f.state.writes, 0)
  }
})

test('fresh provider failure uses existing backoff and never submits', async () => {
  const f = fixture(); f.state.mark = 995n
  let freshFetches = 0
  const iterate = createOracleWorker({ ...f.options, fetchFreshPayload: async () => {
    freshFetches++; throw new Error('fresh provider unavailable')
  } })
  await assert.rejects(iterate(), /fresh provider unavailable/)
  f.state.now += 4999
  assert.equal((await iterate()).status, 'backoff')
  assert.equal(freshFetches, 1)
  assert.equal(f.state.writes, 0)
})

test('a failed cached request or unknown health failure never triggers fresh fallback', async () => {
  for (const failure of ['cache', 'health']) {
    const f = fixture(); f.state.mark = 995n
    if (failure === 'health') f.state.healthError = new Error('RPC unavailable')
    const iterate = createOracleWorker({ ...f.options,
      ...(failure === 'cache' ? { fetchPayload: async () => { throw new Error('cache unavailable') } } : {}),
    })
    await assert.rejects(iterate(), /unavailable/)
    assert.equal(f.state.freshFetches, 0)
    assert.equal(f.state.writes, 0)
  }
})

test('fresh fallback rechecks mark and frozen transitions before spending', async () => {
  for (const change of ['mark', 'frozen']) {
    const f = fixture(); f.state.mark = 995n
    const iterate = createOracleWorker({ ...f.options, fetchFreshPayload: async () => {
      f.state.publishes.fill(996)
      if (change === 'mark') f.state.mark = 999n
      else f.state.frozen = true
      return f.options.fetchPayload()
    } })
    assert.equal((await iterate()).status, change === 'mark' ? 'waiting_for_payload' : 'healthy')
    assert.equal(f.state.reads.filter(read => read.functionName === 'getUpdateFee').length, 0)
    assert.equal(f.state.writes, 0)
  }
})

test('same-height reorg during fresh fetch invalidates earlier health evidence', async () => {
  const f = fixture(); f.state.mark = 995n
  let hash = '0xaaa'
  f.publicClient.getBlock = async () => ({ number: 42n, hash, timestamp: 1000n })
  const iterate = createOracleWorker({ ...f.options, fetchFreshPayload: async () => {
    f.state.publishes.fill(996); hash = '0xbbb'; f.state.mark = 999n
    return f.options.fetchPayload()
  } })
  assert.equal((await iterate()).status, 'waiting_for_payload')
  assert.equal(f.state.reads.filter(read => read.functionName === 'getLatestPrice').length, 2)
  assert.equal(f.state.writes, 0)
})

test('mark advancement after the fresh fetch is checked again after the fee read', async () => {
  const f = fixture(); f.state.mark = 995n
  const read = f.publicClient.readContract
  f.publicClient.readContract = async input => {
    const result = await read(input)
    if (input.functionName === 'getUpdateFee') f.state.mark = 999n
    return result
  }
  const iterate = createOracleWorker({ ...f.options, fetchFreshPayload: async () => {
    f.state.publishes.fill(996); return f.options.fetchPayload()
  } })
  assert.equal((await iterate()).status, 'waiting_for_payload')
  assert.equal(f.state.writes, 0)
})

test('receipt uncertainty after a fresh repair retains one hash and does not fetch or submit again', async () => {
  const f = fixture(); f.state.mark = 995n; f.state.waitError = new Error('receipt timeout')
  let freshFetches = 0
  const iterate = createOracleWorker({ ...f.options, fetchFreshPayload: async () => {
    freshFetches++; f.state.publishes.fill(996); return f.options.fetchPayload()
  } })
  await assert.rejects(iterate(), /receipt timeout/)
  await assert.rejects(iterate(), /receipt timeout/)
  assert.equal(f.state.writes, 1)
  assert.equal(freshFetches, 1)
  f.state.waitError = undefined
  assert.equal((await iterate()).status, 'synchronized')
  assert.equal(f.state.writes, 1)
  assert.equal(freshFetches, 1)
})

test('default fresh fallback uses the admitted API endpoint with a bounded timeout', async () => {
  const f = fixture(); f.state.mark = 995n
  const originalFetch = globalThis.fetch
  let freshFetches = 0
  globalThis.fetch = async (url, options) => {
    freshFetches++
    assert.equal(String(url), 'http://127.0.0.1:3001/api/perps/pyth/update')
    assert.ok(options.signal instanceof AbortSignal)
    f.state.publishes.fill(996)
    return { ok: true, json: async () => ({ data: await f.options.fetchPayload() }) }
  }
  try {
    const iterate = createOracleWorker({ ...f.options, backendUrl: 'http://127.0.0.1:3001', fetchFreshPayload: undefined })
    assert.equal((await iterate()).status, 'synchronized')
    assert.equal(freshFetches, 1)
  } finally { globalThis.fetch = originalFetch }
})

test('fresh API rate limits and HTTP failures back off without parsing or submitting', async () => {
  const originalFetch = globalThis.fetch
  try {
    for (const status of [429, 503]) {
      const f = fixture(); f.state.mark = 995n
      let requests = 0
      globalThis.fetch = async () => {
        requests++
        return { ok: false, status, json: async () => { throw new Error('must not parse error body') } }
      }
      const iterate = createOracleWorker({ ...f.options, backendUrl: 'http://127.0.0.1:3001', fetchFreshPayload: undefined })
      await assert.rejects(iterate(), new RegExp(`HTTP ${status}`))
      f.state.now += 4999
      assert.equal((await iterate()).status, 'backoff')
      assert.equal(requests, 1)
      assert.equal(f.state.reads.filter(read => read.functionName === 'getUpdateFee').length, 0)
      assert.equal(f.state.writes, 0)
    }
  } finally { globalThis.fetch = originalFetch }
})

test('a same-payload one-second header race catches up before fee lookup and repair', async () => {
  const f = fixture(); f.state.now = 1_001_000; f.state.publishes.fill(1001)
  let timestamp = 1000n
  const pauses = []
  f.publicClient.getBlock = async () => ({ number: timestamp, hash: `0x${timestamp}`, timestamp })
  const iterate = createOracleWorker({ ...f.options, pause: async ms => {
    pauses.push(ms); f.state.now += ms; timestamp = 1001n; f.state.timestamp = timestamp
  } })
  assert.equal((await iterate()).status, 'synchronized')
  assert.deepEqual(pauses, [1000])
  assert.equal(f.state.freshFetches, 0)
  assert.equal(f.state.accepted, 1001n)
  assert.equal(f.state.writes, 1)
})

test('a near-ahead repair reobserves the head using the default pause', async t => {
  const f = fixture()
  f.state.now = 1_001_000
  f.state.publishes.fill(1001)
  let timestamp = 1000n
  let headReads = 0
  f.publicClient.getBlock = async () => {
    headReads++
    return { number: timestamp, hash: `0x${timestamp}`, timestamp }
  }
  t.mock.timers.enable({ apis: ['setTimeout'] })
  const iterate = createOracleWorker({ ...f.options, pause: undefined })
  const iteration = iterate()
  await new Promise(resolve => setImmediate(resolve))

  t.mock.timers.tick(999)
  assert.equal(headReads, 2)
  assert.equal(f.state.writes, 0)
  timestamp = 1001n
  f.state.timestamp = timestamp
  t.mock.timers.tick(1)
  assert.equal((await iteration).status, 'synchronized')
  assert.equal(f.state.writes, 1)
})

test('a fresh admitted payload waits at most two head reads without fetching it again', async () => {
  const f = fixture(); f.state.now = 1_002_000; f.state.mark = 995n
  let timestamp = 1000n
  let freshFetches = 0
  const pauses = []
  f.publicClient.getBlock = async () => ({ number: timestamp, hash: `0x${timestamp}`, timestamp })
  const iterate = createOracleWorker({ ...f.options,
    fetchFreshPayload: async () => {
      freshFetches++; f.state.publishes.fill(1002); return f.options.fetchPayload()
    },
    pause: async ms => {
      pauses.push(ms); f.state.now += ms; timestamp++; f.state.timestamp = timestamp
    },
  })
  assert.equal((await iterate()).status, 'synchronized')
  assert.deepEqual(pauses, [1000, 1000])
  assert.equal(freshFetches, 1)
  assert.equal(f.state.writes, 1)
  const observed = f.state.logs.find(args => args.includes('oracle_payload_head_reobserved'))
  assert.equal(observed[4].head_reads, 2)
  assert.equal(observed[4].caught_up, true)
})

test('genuinely future, far-ahead and already-stale payloads do not wait for a header', async () => {
  for (const kind of ['future_wall_clock', 'far_ahead', 'stale']) {
    const f = fixture()
    f.state.now = kind === 'future_wall_clock' ? 1_000_000 : 1_003_000
    f.state.publishes = kind === 'stale' ? [949, 1001, 1001, 1001, 1001, 1001] : Array(6).fill(kind === 'far_ahead' ? 1003 : 1001)
    let pauses = 0
    const iterate = createOracleWorker({ ...f.options, pause: async () => { pauses++ } })
    assert.equal((await iterate()).status, 'stale_payload', kind)
    assert.equal(pauses, 0, kind)
    assert.equal(f.state.reads.filter(read => read.functionName === 'getUpdateFee').length, 0)
    assert.equal(f.state.writes, 0)
  }
})

test('a head that never catches the payload stops after two reads and backs off', async () => {
  const f = fixture(); f.state.now = 1_002_000; f.state.publishes.fill(1001)
  f.publicClient.getBlock = async () => ({ number: 42n, hash: '0xaaa', timestamp: 1000n })
  let pauses = 0
  const iterate = createOracleWorker({ ...f.options, pause: async ms => { pauses++; f.state.now += ms } })
  assert.equal((await iterate()).status, 'stale_payload')
  assert.equal(pauses, 2)
  const observed = f.state.logs.find(args => args.includes('oracle_payload_head_reobserved'))
  assert.equal(observed[4].caught_up, false)
  f.state.now += 4999
  assert.equal((await iterate()).status, 'backoff')
  assert.equal(pauses, 2)
  assert.equal(f.state.reads.filter(read => read.functionName === 'getUpdateFee').length, 0)
  assert.equal(f.state.writes, 0)
})

test('head reobservation revalidates same-height reorgs and market transitions before spending', async () => {
  for (const change of ['reorg_mark_advanced', 'frozen']) {
    const f = fixture(); f.state.now = 1_001_000; f.state.publishes.fill(1001)
    let hash = '0xaaa'
    let timestamp = 1000n
    f.publicClient.getBlock = async () => ({ number: 42n, hash, timestamp })
    const iterate = createOracleWorker({ ...f.options, pause: async ms => {
      f.state.now += ms; hash = '0xbbb'; timestamp = 1001n; f.state.timestamp = timestamp
      if (change === 'frozen') f.state.frozen = true
      else f.state.mark = 1002n
    } })
    assert.equal((await iterate()).status, change === 'frozen' ? 'healthy' : 'waiting_for_payload')
    assert.equal(f.state.reads.filter(read => read.functionName === 'getLatestPrice').length, 2)
    assert.equal(f.state.reads.filter(read => read.functionName === 'getUpdateFee').length, 0)
    assert.equal(f.state.writes, 0)
  }
})

test('RPC failure during head reobservation fails closed and retains ordinary backoff', async () => {
  const f = fixture(); f.state.now = 1_001_000; f.state.publishes.fill(1001)
  let paused = false
  f.publicClient.getBlock = async () => {
    if (paused) throw new Error('head RPC unavailable')
    return { number: 42n, hash: '0xaaa', timestamp: 1000n }
  }
  const iterate = createOracleWorker({ ...f.options, pause: async ms => { paused = true; f.state.now += ms } })
  await assert.rejects(iterate(), /head RPC unavailable/)
  f.state.now += 4999
  assert.equal((await iterate()).status, 'backoff')
  assert.equal(f.state.writes, 0)
})

test('the final post-fee mark guard still applies after the head catches up', async () => {
  const f = fixture(); f.state.now = 1_001_000; f.state.publishes.fill(1001)
  let timestamp = 1000n
  f.publicClient.getBlock = async () => ({ number: timestamp, hash: `0x${timestamp}`, timestamp })
  const read = f.publicClient.readContract
  f.publicClient.readContract = async input => {
    const result = await read(input)
    if (input.functionName === 'getUpdateFee') {
      timestamp = 1002n; f.state.timestamp = timestamp; f.state.mark = 1002n
    }
    return result
  }
  let pauses = 0
  const iterate = createOracleWorker({ ...f.options, pause: async ms => {
    pauses++; f.state.now += ms; timestamp = 1001n; f.state.timestamp = timestamp
  } })
  assert.equal((await iterate()).status, 'waiting_for_payload')
  assert.equal(pauses, 1)
  assert.equal(f.state.writes, 0)
})

test('fetch errors back off 5/10/20/30 seconds without RPC work and recover before normal cadence', async () => {
  const f = fixture()
  f.state.accepted = f.state.mark
  let fail = true
  let fetches = 0
  const iterate = createOracleWorker({ ...f.options, fetchPayload: async () => {
    fetches++
    if (fail) throw new Error('backend unavailable')
    return f.options.fetchPayload()
  } })
  for (const delay of [5000, 10_000, 20_000, 30_000, 30_000]) {
    await assert.rejects(iterate(), /backend unavailable/)
    const reads = f.state.reads.length
    const attempts = fetches
    f.state.now += delay - 1
    assert.equal((await iterate()).status, 'backoff')
    assert.equal(f.state.reads.length, reads)
    assert.equal(fetches, attempts)
    f.state.now++
  }
  fail = false
  f.state.publishes.fill(995)
  assert.equal((await iterate()).status, 'synchronized')
  f.state.now += 30_000
  fail = true
  await assert.rejects(iterate(), /backend unavailable/)
  fail = false
  f.state.publishes.fill(999)
  f.state.now += 5000
  assert.equal((await iterate()).status, 'synchronized')
})

test('healthy dry runs retain the normal polling cadence', async () => {
  const f = fixture()
  f.state.accepted = f.state.mark
  f.state.publishes.fill(995)
  const iterate = createOracleWorker({ ...f.options, dryRun: true })
  assert.equal((await iterate()).status, 'dry_run')
  f.state.now += 5000
  assert.equal((await iterate()).status, 'healthy')
  f.state.now += 25_000
  assert.equal((await iterate()).status, 'dry_run')
  assert.equal(f.state.writes, 0)
})

test('both deployable mark updater scripts bind the canonical protocol release', () => {
  for (const file of ['perps-oracle-worker.mjs', 'update-perps-mark-price.mjs']) {
    const source = readFileSync(new URL('../scripts/' + file, import.meta.url), 'utf8')
    const block = source.match(/const (?:DEFAULT_SEPOLIA_)?ADDRESSES = \{([\s\S]*?)\n\}/)?.[1]
    assert.ok(block, file + ' address bindings are missing')
    for (const name of ['orderRouter', 'pletherOracle', 'perpsPublicLens']) {
      assert.match(block, new RegExp(name + ": '" + release.contracts[name].address + "'"))
    }
  }
})

test('fork verification follows the canonical release instead of silently exercising an old deployment', () => {
  const source = readFileSync(new URL('./oracle-sync-fork/OracleSyncFork.t.sol', import.meta.url), 'utf8')
  for (const name of ['orderRouter', 'pletherOracle', 'cfdEngine']) {
    assert.ok(source.includes(`hex'${release.contracts[name].address.slice(2)}'`), `${name} fork address is obsolete`)
  }
  for (const name of ['orderRouter', 'pletherOracle']) {
    assert.ok(source.includes(`hex'${release.contracts[name].runtimeCodeHash.slice(2)}'`), `${name} fork runtime hash is obsolete`)
  }
})
