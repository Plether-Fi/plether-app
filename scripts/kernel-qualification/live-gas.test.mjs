import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { test } from 'node:test'
import { readFile } from 'node:fs/promises'
import { runInNewContext } from 'node:vm'
import { checkEstimate, checkedEvidence, parsePreparation, verifyOne } from './live-gas.mjs'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { encodeAbiParameters, encodeEventTopics, getAddress, parseAbiParameters, toHex, zeroAddress } = require('viem')
const { entryPoint09Abi, entryPoint09Address } = require('viem/account-abstraction')
const { profile: deployment, gas } = JSON.parse(await readFile(
  new URL('../../apps/backend/test/fixtures/kernel-deployment.json', import.meta.url), 'utf8',
))
const sender = getAddress('0x730115Ec79C76CafbE17E3D4a388F3e5B9bA79CE')
const paymaster = getAddress('0x2279B7A0a67DB372996a5FaB50D91eAA73d2eBe6')
const opHash = `0x${'12'.repeat(32)}`
const txHash = `0x${'34'.repeat(32)}`
const blockHash = `0x${'56'.repeat(32)}`

test('browser observer exports only unsigned allowlisted fields and restores fetch', async () => {
  const raw = preparation('sponsored')
  delete raw.operation.signature
  raw.operation.recoveryToken = 'never-copy-operation-field'
  raw.recoveryToken = 'never-copy-gateway-token'
  const reply = Promise.resolve({ ok: true, clone: () => ({ json: async () => ({ result: raw }) }) })
  const originalFetch = () => reply
  const messages = []
  const page = { fetch: originalFetch }
  runInNewContext(await readFile(new URL('./browser-preparation-observer.js', import.meta.url), 'utf8'), {
    window: page, console: { info: (...args) => messages.push(args) },
  })
  const request = page.fetch('/api/perps/v1/aa/kernel/rpc', {
    body: JSON.stringify({ method: 'plether_prepareUserOperation' }),
  })
  assert.equal(request, reply)
  await new Promise(resolve => setImmediate(resolve))
  const safe = page.__pletherKernelGasCapture.take('sponsored')
  assert.equal(safe.includes('never-copy'), false)
  assert.equal(JSON.parse(safe).operation.signature, '0x')
  assert.equal(JSON.parse(safe).operation.paymaster, paymaster)
  assert.equal(JSON.stringify(messages).includes('never-copy'), false)
  assert.throws(() => page.__pletherKernelGasCapture.take('sponsored'), /No sponsored preparation/)
  page.__pletherKernelGasCapture.restore()
  assert.equal(page.fetch, originalFetch)
  assert.equal(page.__pletherKernelGasCapture, undefined)
})

function preparation(mode) {
  const operation = {
    sender, nonce: '0x0', callData: '0x1234', callGasLimit: '0x64',
    verificationGasLimit: '0xc8', preVerificationGas: '0x32',
    maxFeePerGas: '0x2', maxPriorityFeePerGas: '0x1', signature: '0x',
    ...(mode === 'sponsored' ? {
      paymaster, paymasterData: `0x${'11'.repeat(157)}`,
      paymasterVerificationGasLimit: '0x32', paymasterPostOpGasLimit: '0x0',
    } : {}),
  }
  return { mode, entryPoint: entryPoint09Address, operation,
    maxNetworkGasWei: mode === 'eth' ? '0x2bc' : '0x320',
    expiresAt: Math.floor(Date.now() / 1000) + 120 }
}

function preparationAtGas(mode, limits) {
  const raw = preparation(mode)
  for (const [field, value] of Object.entries(limits)) raw.operation[field] = toHex(value)
  const units = ['callGasLimit', 'verificationGasLimit', 'preVerificationGas',
    'paymasterVerificationGasLimit', 'paymasterPostOpGasLimit']
    .reduce((total, field) => total + BigInt(raw.operation[field] ?? 0), 0n)
  raw.maxNetworkGasWei = toHex(units * BigInt(raw.operation.maxFeePerGas))
  return raw
}

function evidence() {
  return {
    schema: 1, chainId: 421614, entryPoint: entryPoint09Address, mode: 'eth',
    sender, paymaster: zeroAddress, userOperationHash: opHash,
    capturedAt: new Date().toISOString(), expiresAt: Math.floor(Date.now() / 1000) + 120,
    quoteMaxWei: '0x7d0', preparedGas: {
      callGasLimit: '0x190', verificationGasLimit: '0x12c', preVerificationGas: '0x12c',
      maxFeePerGas: '0x2',
    }, estimatedGas: {
      callGasLimit: '0x180', verificationGasLimit: '0x100', preVerificationGas: '0x100',
    }, nitroFeeControl: {
      blockNumber: '0x9', blockHash, gasEstimate: '0x5000', gasEstimateForL1: '0x2',
      baseFee: '0x1', l1BaseFeeEstimate: '0x1',
    }, fundingControl: {
      blockNumber: '0x9', payer: sender, entryPointDepositWei: '0x7d0',
      nativeBalanceWei: '0x0', shortfallWei: '0x0',
    },
  }
}

test('prepared ETH and sponsor quotes bind the exact gas cap and payer', () => {
  const eth = parsePreparation(preparation('eth'))
  const sponsored = parsePreparation(preparation('sponsored'))
  assert.equal(eth.quoteMaxWei, 700n)
  assert.equal(sponsored.quoteMaxWei, 800n)
  assert.notEqual(eth.userOperationHash, sponsored.userOperationHash)
  assert.equal(sponsored.operation.paymaster, paymaster)
  const unsigned = preparation('eth')
  delete unsigned.operation.signature
  assert.equal(parsePreparation(unsigned).userOperationHash, eth.userOperationHash)
  assert.throws(() => parsePreparation({ ...preparation('eth'), maxNetworkGasWei: '0x2bb' }), /quote/)
  assert.throws(() => parsePreparation({ ...preparation('eth'), operation: {
    ...preparation('eth').operation, paymaster,
  } }), /paymaster/)
})

test('accepts the shared deployment entry point and recorded Nitro L1 gas', () => {
  const eth = preparationAtGas('eth', { preVerificationGas: BigInt(gas.nitroFixturePreVerificationGas) })
  const parsed = parsePreparation({ ...eth, deployment: deployment.deployment, entryPoint: deployment.entryPoint })
  assert.equal(parsed.deployment, deployment.deployment)
  assert.equal(parsed.operation.preVerificationGas, BigInt(gas.nitroFixturePreVerificationGas))
})

test('rejects preparations for a previous release or an indirect factory', () => {
  const eth = preparation('eth')
  assert.throws(() => parsePreparation({ ...eth, deployment: 'kernel-sepolia-canary-20261121' }), /Wrong Kernel deployment/u)
  assert.throws(() => parsePreparation({ ...eth, operation: { ...eth.operation, factory: sender, factoryData: '0x1234' } }),
    /pinned KernelFactory/u)
  const bound = { ...eth, operation: { ...eth.operation, factory: deployment.factory, factoryData: '0x1234' } }
  assert.equal(parsePreparation(bound).operation.factory, getAddress(deployment.factory))
})

test('enforces the shared gas and fee boundaries', () => {
  const limits = {
    callGasLimit: gas.callGasLimit,
    verificationGasLimit: gas.verificationGasLimit,
    preVerificationGas: gas.preVerificationGas,
    paymasterVerificationGasLimit: gas.paymasterVerificationGasLimit,
    paymasterPostOpGasLimit: gas.paymasterPostOpGasLimit,
  }
  const maximumGas = parsePreparation(preparationAtGas('sponsored', limits))
  assert.equal(maximumGas.quoteMaxWei, BigInt(gas.aggregateGas) * maximumGas.operation.maxFeePerGas)
  for (const [field, cap] of Object.entries(limits)) {
    const excessive = preparationAtGas('sponsored', { ...limits, [field]: cap + 1 })
    assert.throws(() => parsePreparation(excessive), { message: `${field} is out of range` })
  }
  for (const field of ['maxFeePerGas', 'maxPriorityFeePerGas']) {
    const valid = preparationAtGas('eth', { maxFeePerGas: gas.maxFeePerGas, [field]: gas[field] })
    assert.equal(parsePreparation(valid).operation[field], BigInt(gas[field]))
    const excessive = preparationAtGas('eth', { maxFeePerGas: gas.maxFeePerGas, [field]: gas[field] + 1 })
    assert.throws(() => parsePreparation(excessive), { message: `${field} is out of range` })
  }
})

test('enforces the shared ETH prefund ceiling independently of gas units', () => {
  const budget = BigInt(gas.ethMaxPrefundWei)
  const fee = BigInt(gas.maxFeePerGas)
  const work = budget / fee - 2n
  const quote = preVerificationGas => preparationAtGas('eth', {
    callGasLimit: 1n, verificationGasLimit: 1n, preVerificationGas, maxFeePerGas: fee,
  })
  assert.equal(parsePreparation(quote(work)).quoteMaxWei, budget)
  assert.throws(() => parsePreparation(quote(work + 1n)), { message: 'maxNetworkGasWei is out of range' })
})

test('a live Alto estimate must fit every prepared limit', () => {
  const op = parsePreparation(preparation('sponsored')).operation
  assert.deepEqual(checkEstimate({ callGasLimit: '0x63', verificationGasLimit: '0xc8', preVerificationGas: '0x32',
    paymasterVerificationGasLimit: '0x31', paymasterPostOpGasLimit: '0x0' }, op), {
    callGasLimit: '0x63', verificationGasLimit: '0xc8', preVerificationGas: '0x32',
    paymasterVerificationGasLimit: '0x31', paymasterPostOpGasLimit: '0x0',
  })
  assert.throws(() => checkEstimate({ callGasLimit: '0x65', verificationGasLimit: '0xc8', preVerificationGas: '0x32',
    paymasterVerificationGasLimit: '0x31', paymasterPostOpGasLimit: '0x0' }, op), /does not cover/)
})

test('saved evidence cannot switch payer or understate its own cap', () => {
  assert.equal(checkedEvidence(evidence(), 'eth').cap, 2_000n)
  assert.throws(() => checkedEvidence(evidence(), 'sponsored'), /mode or chain/)
  assert.throws(() => checkedEvidence({ ...evidence(), quoteMaxWei: '0x7cf' }, 'eth'), /gas cap mismatch/)
  assert.throws(() => checkedEvidence({ ...evidence(), paymaster }, 'eth'), /payer mode mismatch/)
  assert.throws(() => checkedEvidence({ ...evidence(), fundingControl: {
    ...evidence().fundingControl, entryPointDepositWei: '0x1',
  } }, 'eth'), /shortfall/)
  assert.throws(() => checkedEvidence({ ...evidence(), estimatedGas: {
    ...evidence().estimatedGas, callGasLimit: '0x191',
  } }, 'eth'), /does not cover/)
})

test('sponsored evidence checks paymaster estimate and deposit', () => {
  const sponsored = {
    ...evidence(), mode: 'sponsored', paymaster, quoteMaxWei: '0x834',
    preparedGas: { ...evidence().preparedGas, paymasterVerificationGasLimit: '0x32', paymasterPostOpGasLimit: '0x0' },
    estimatedGas: { ...evidence().estimatedGas, paymasterVerificationGasLimit: '0x28', paymasterPostOpGasLimit: '0x0' },
    fundingControl: { ...evidence().fundingControl, payer: paymaster,
      entryPointDepositWei: '0x834', nativeBalanceWei: '0x0' },
  }
  assert.equal(checkedEvidence(sponsored, 'sponsored').cap, 2_100n)
  assert.throws(() => checkedEvidence({ ...sponsored, estimatedGas: {
    ...sponsored.estimatedGas, paymasterVerificationGasLimit: '0x33',
  } }, 'sponsored'), /does not cover/)
  assert.throws(() => checkedEvidence({ ...sponsored, fundingControl: {
    ...sponsored.fundingControl, entryPointDepositWei: '0x833',
  } }, 'sponsored'), /shortfall/)
})

test('included receipt is checked against the canonical EntryPoint event and quote', async t => {
  const actual = 1_400n
  const used = 700n
  const topics = encodeEventTopics({ abi: entryPoint09Abi, eventName: 'UserOperationEvent',
    args: { userOpHash: opHash, sender, paymaster: zeroAddress } })
  const data = encodeAbiParameters(parseAbiParameters('uint256,bool,uint256,uint256'), [0n, true, actual, used])
  const responseFor = {
    eth_getUserOperationReceipt: {
      userOpHash: opHash, sender, actualGasCost: toHex(actual),
      actualGasUsed: toHex(used), success: true,
      receipt: { transactionHash: txHash, blockHash, blockNumber: '0x10' },
    },
    eth_getTransactionReceipt: {
      transactionHash: txHash, blockHash, blockNumber: '0x10', status: '0x1',
      logs: [{ address: entryPoint09Address, topics, data }],
    },
    eth_getTransactionByHash: { to: entryPoint09Address, from: sender, input: null,
      hash: txHash, blockNumber: '0x10', blockHash },
    eth_getBlockByNumber: { hash: blockHash, number: '0x10' },
  }
  t.mock.method(globalThis, 'fetch', async (_url, request) => {
    const { method } = JSON.parse(request.body)
    const result = responseFor[method]
    assert.notEqual(result, undefined, `Unexpected RPC method ${method}`)
    return Response.json({ jsonrpc: '2.0', id: 1, result })
  })
  const url = 'https://sepolia-rpc.example.invalid/'
  const result = await verifyOne(url, url, evidence(), 'eth')
  assert.equal(result.actualGasCostWei, toHex(actual))
  assert.equal(result.quoteUsedBps, 7_000)
  assert.deepEqual(await verifyOne(url, url, evidence(), 'eth', txHash), result)
  responseFor.eth_getUserOperationReceipt.actualGasCost = '0x7d1'
  await assert.rejects(verifyOne(url, url, evidence(), 'eth'), /gas charge exceeded/)
  responseFor.eth_getUserOperationReceipt.actualGasCost = toHex(actual)
  responseFor.eth_getTransactionReceipt.logs[0].data = encodeAbiParameters(
    parseAbiParameters('uint256,bool,uint256,uint256'), [0n, true, actual + 1n, used])
  await assert.rejects(verifyOne(url, url, evidence(), 'eth'), /onchain UserOperationEvent disagrees/)
})
