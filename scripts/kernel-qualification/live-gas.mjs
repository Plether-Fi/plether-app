#!/usr/bin/env node
// Read-only two-phase check. Capture a fresh gateway preparation before wallet
// approval, then verify both included operations against their onchain events.
import { createRequire } from 'node:module'
import { readFile, writeFile } from 'node:fs/promises'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'
import { resolve } from 'node:path'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const {
  decodeEventLog, decodeFunctionResult, encodeFunctionData, getAddress, isAddress,
  keccak256, parseAbi, toHex, zeroAddress,
} = require('viem')
const {
  entryPoint09Abi, formatUserOperationRequest, getUserOperationHash,
} = require('viem/account-abstraction')
const pins = JSON.parse(await readFile(new URL('./pins.json', import.meta.url), 'utf8'))
const ENTRY_POINT = getAddress(pins.entryPoint.address)
const NODE_INTERFACE = '0x00000000000000000000000000000000000000c8'
const MAX_QUOTE_WEI = 50_000_000_000_000_000n
const nodeInterfaceAbi = parseAbi([
  'function gasEstimateComponents(address to,bool contractCreation,bytes data) view returns (uint256 gasEstimate,uint256 gasEstimateForL1,uint256 baseFee,uint256 l1BaseFeeEstimate)',
  'function gasEstimateL1Component(address to,bool contractCreation,bytes data) view returns (uint256 gasEstimateForL1,uint256 baseFee,uint256 l1BaseFeeEstimate)',
])
const STUB_SIGNATURE = '0xfffffffffffffffffffffffffffffff0000000000000000000000000000000007aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1c'
const HEX_BYTES = /^0x(?:[0-9a-fA-F]{2})*$/
const HEX_QUANTITY = /^0x(?:0|[1-9a-fA-F][0-9a-fA-F]*)$/

function fail(message) { throw new Error(message) }
function quantity(value, label, max = (1n << 256n) - 1n) {
  if (typeof value !== 'string' || !HEX_QUANTITY.test(value)) fail(`${label} must be a canonical hex quantity`)
  const n = BigInt(value)
  if (n > max) fail(`${label} is out of range`)
  return n
}
function hex(value, label, bytes, minBytes = bytes) {
  if (typeof value !== 'string' || !HEX_BYTES.test(value) || value.length < 2 + minBytes * 2 ||
      (bytes !== undefined && value.length > 2 + bytes * 2)) fail(`${label} must be hex bytes`)
  return value.toLowerCase()
}
function address(value, label) {
  if (!isAddress(value)) fail(`${label} must be an address`)
  return getAddress(value)
}
function record(value, label) {
  if (!value || typeof value !== 'object' || Array.isArray(value)) fail(`${label} must be an object`)
  return value
}
function keys(value, required, optional = []) {
  for (const key of required) if (!(key in value)) fail(`Missing ${key}`)
  for (const key of Object.keys(value)) if (![...required, ...optional].includes(key)) fail(`Unexpected ${key}`)
}
function gasLimitTotal(op) {
  return op.callGasLimit + op.verificationGasLimit + op.preVerificationGas +
    (op.paymasterVerificationGasLimit ?? 0n) + (op.paymasterPostOpGasLimit ?? 0n)
}
function parsePreparation(source) {
  const raw = record(source, 'preparation')
  keys(raw, ['mode', 'entryPoint', 'operation', 'maxNetworkGasWei', 'expiresAt'], ['deployment'])
  if (!['eth', 'sponsored'].includes(raw.mode)) fail('Unknown gas mode')
  if (address(raw.entryPoint, 'entryPoint') !== ENTRY_POINT) fail('Wrong EntryPoint')
  if (raw.deployment !== undefined && !/^[a-z0-9._-]{1,96}$/.test(raw.deployment)) fail('Invalid deployment')
  if (!Number.isSafeInteger(raw.expiresAt) || raw.expiresAt < Date.now() / 1000 + 30) fail('Preparation expired or nearly expired')
  const sourceOp = record(raw.operation, 'operation')
  keys(sourceOp,
    ['sender', 'nonce', 'callData', 'callGasLimit', 'verificationGasLimit', 'preVerificationGas',
      'maxFeePerGas', 'maxPriorityFeePerGas'],
    ['factory', 'factoryData', 'paymaster', 'paymasterData', 'paymasterVerificationGasLimit', 'paymasterPostOpGasLimit', 'signature'])
  if (sourceOp.signature !== undefined && sourceOp.signature !== '0x') fail('Capture expects the unsigned gateway preparation')
  const op = {
    sender: address(sourceOp.sender, 'sender'),
    nonce: quantity(sourceOp.nonce, 'nonce', (1n << 64n) - 1n),
    callData: hex(sourceOp.callData, 'callData', 16_384, 1),
    callGasLimit: quantity(sourceOp.callGasLimit, 'callGasLimit', 3_000_000n),
    verificationGasLimit: quantity(sourceOp.verificationGasLimit, 'verificationGasLimit', 1_000_000n),
    preVerificationGas: quantity(sourceOp.preVerificationGas, 'preVerificationGas', 1_000_000n),
    maxFeePerGas: quantity(sourceOp.maxFeePerGas, 'maxFeePerGas', 10_000_000_000n),
    maxPriorityFeePerGas: quantity(sourceOp.maxPriorityFeePerGas, 'maxPriorityFeePerGas', 2_000_000_000n),
    signature: '0x',
  }
  if (op.maxFeePerGas === 0n || op.maxPriorityFeePerGas > op.maxFeePerGas ||
      op.callGasLimit === 0n || op.verificationGasLimit === 0n || op.preVerificationGas === 0n) fail('Invalid gas limits or price')
  if ((sourceOp.factory === undefined) !== (sourceOp.factoryData === undefined)) fail('Incomplete factory fields')
  if (sourceOp.factory !== undefined) {
    op.factory = address(sourceOp.factory, 'factory')
    op.factoryData = hex(sourceOp.factoryData, 'factoryData', 2_048, 1)
  }
  const paymasterFields = ['paymaster', 'paymasterData', 'paymasterVerificationGasLimit', 'paymasterPostOpGasLimit']
  if (raw.mode === 'sponsored') {
    if (!paymasterFields.every(field => sourceOp[field] !== undefined)) fail('Incomplete sponsored operation')
    op.paymaster = address(sourceOp.paymaster, 'paymaster')
    op.paymasterData = hex(sourceOp.paymasterData, 'paymasterData', 157, 157)
    op.paymasterVerificationGasLimit = quantity(sourceOp.paymasterVerificationGasLimit, 'paymasterVerificationGasLimit', 1_000_000n)
    op.paymasterPostOpGasLimit = quantity(sourceOp.paymasterPostOpGasLimit, 'paymasterPostOpGasLimit', 1_000_000n)
  } else if (paymasterFields.some(field => sourceOp[field] !== undefined)) fail('ETH operation contains paymaster fields')
  const quoteMaxWei = quantity(raw.maxNetworkGasWei, 'maxNetworkGasWei', MAX_QUOTE_WEI)
  if (quoteMaxWei === 0n || quoteMaxWei !== gasLimitTotal(op) * op.maxFeePerGas) fail('Gateway quote does not match operation gas cap')
  const userOperationHash = getUserOperationHash({ userOperation: op, chainId: pins.chainId,
    entryPointAddress: ENTRY_POINT, entryPointVersion: '0.9' }).toLowerCase()
  return { mode: raw.mode, deployment: raw.deployment, expiresAt: raw.expiresAt,
    operation: op, quoteMaxWei, userOperationHash }
}

function rpcUrl(raw) {
  const url = new URL(raw)
  if (url.username || url.password || url.search || url.hash ||
      !(url.protocol === 'https:' || (url.protocol === 'http:' && ['127.0.0.1', 'localhost'].includes(url.hostname)))) {
    fail('RPC URLs must be HTTPS or localhost HTTP without URL credentials or query')
  }
  return url.toString()
}
async function rpc(url, method, params, maxBytes = 2_000_000) {
  const response = await fetch(url, { method: 'POST', redirect: 'error', signal: AbortSignal.timeout(15_000),
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ jsonrpc: '2.0', id: 1, method, params }) })
  if (!response.ok) fail(`${method} returned HTTP ${response.status}`)
  if (Number(response.headers.get('content-length')) > maxBytes) fail(`${method} response exceeded the size bound`)
  let size = 0
  const chunks = []
  for await (const chunk of response.body) {
    size += chunk.byteLength
    if (size > maxBytes) fail(`${method} response exceeded the size bound`)
    chunks.push(chunk)
  }
  const body = Buffer.concat(chunks).toString('utf8')
  const value = JSON.parse(body)
  if (value.id !== 1 || value.jsonrpc !== '2.0' || value.error) fail(`${method} did not return a successful RPC result`)
  return value.result
}
function parseComponents(result, label) {
  if (!Array.isArray(result) || result.length !== 4) fail(`${label} returned invalid components`)
  const [total, l1, baseFee, l1BaseFee] = result
  if (total <= 0n || l1 < 0n || l1 > total || baseFee <= 0n || l1BaseFee < 0n) fail(`${label} returned invalid gas values`)
  return { gasEstimate: total, gasEstimateForL1: l1, baseFee, l1BaseFeeEstimate: l1BaseFee }
}
async function nitroFeeControl(nodeRpc, sender) {
  const block = record(await rpc(nodeRpc, 'eth_getBlockByNumber', ['safe', false]), 'safe block')
  const blockNumber = quantity(block.number, 'safe block number')
  const blockHash = hex(block.hash, 'safe block hash', 32, 32)
  const data = encodeFunctionData({ abi: entryPoint09Abi, functionName: 'getNonce', args: [sender, 0n] })
  const at = toHex(blockNumber)
  const transaction = { from: zeroAddress, to: ENTRY_POINT, data }
  const [rawGas, rawComponents] = await Promise.all([
    rpc(nodeRpc, 'eth_estimateGas', [transaction, at]),
    rpc(nodeRpc, 'eth_call', [{ to: NODE_INTERFACE,
      data: encodeFunctionData({ abi: nodeInterfaceAbi, functionName: 'gasEstimateComponents', args: [ENTRY_POINT, false, data] }) }, at]),
  ])
  const estimate = quantity(rawGas, 'eth_estimateGas')
  const components = parseComponents(decodeFunctionResult({ abi: nodeInterfaceAbi,
    functionName: 'gasEstimateComponents', data: rawComponents }), 'NodeInterface')
  if (estimate !== components.gasEstimate) fail('Nitro eth_estimateGas and NodeInterface disagree at one block')
  return { blockNumber: at, blockHash, ...Object.fromEntries(Object.entries(components).map(([key, value]) => [key, toHex(value)])) }
}
function checkFundingSnapshot(mode, quoteMaxWei, raw) {
  const source = record(raw, 'funding snapshot')
  keys(source, ['blockNumber', 'payer', 'entryPointDepositWei', 'nativeBalanceWei', 'shortfallWei'])
  const deposit = quantity(source.entryPointDepositWei, 'EntryPoint deposit')
  const native = quantity(source.nativeBalanceWei, 'native balance')
  if (mode === 'sponsored' && native !== 0n) fail('Sponsored evidence must not count paymaster native ETH')
  const shortfall = quoteMaxWei > deposit + (mode === 'eth' ? native : 0n)
    ? quoteMaxWei - deposit - (mode === 'eth' ? native : 0n) : 0n
  if (quantity(source.shortfallWei, 'gas shortfall') !== shortfall || shortfall !== 0n) {
    fail(`${mode} payer has an EntryPoint prefund shortfall`)
  }
  return { deposit, native, shortfall }
}
async function fundingControl(nodeRpc, nitro, mode, sender, paymaster, quoteMaxWei) {
  const payer = mode === 'eth' ? sender : paymaster
  const at = nitro.blockNumber
  const [rawDeposit, rawNative] = await Promise.all([
    rpc(nodeRpc, 'eth_call', [{ to: ENTRY_POINT,
      data: encodeFunctionData({ abi: entryPoint09Abi, functionName: 'balanceOf', args: [payer] }) }, at]),
    mode === 'eth' ? rpc(nodeRpc, 'eth_getBalance', [payer, at]) : Promise.resolve('0x0'),
  ])
  const deposit = decodeFunctionResult({ abi: entryPoint09Abi, functionName: 'balanceOf', data: rawDeposit })
  const native = quantity(rawNative, 'native payer balance')
  const shortfall = quoteMaxWei > deposit + native ? quoteMaxWei - deposit - native : 0n
  const snapshot = { blockNumber: at, payer, entryPointDepositWei: toHex(deposit),
    nativeBalanceWei: toHex(native), shortfallWei: toHex(shortfall) }
  checkFundingSnapshot(mode, quoteMaxWei, snapshot)
  return snapshot
}
function checkEstimate(raw, op) {
  const estimated = record(raw, 'Alto estimate')
  const fields = ['callGasLimit', 'verificationGasLimit', 'preVerificationGas',
    ...(op.paymaster ? ['paymasterVerificationGasLimit', 'paymasterPostOpGasLimit'] : [])]
  const result = {}
  for (const field of fields) {
    const value = quantity(estimated[field], `estimated ${field}`)
    if (value > op[field] || (field !== 'paymasterPostOpGasLimit' && value === 0n)) fail(`Prepared ${field} does not cover the live Alto estimate`)
    result[field] = toHex(value)
  }
  return result
}
async function attestStack(altoRpc, nodeRpc) {
  const [chainId, code, supported] = await Promise.all([
    rpc(nodeRpc, 'eth_chainId', []),
    rpc(nodeRpc, 'eth_getCode', [ENTRY_POINT, 'safe']),
    rpc(altoRpc, 'eth_supportedEntryPoints', []),
  ])
  if (quantity(chainId, 'chainId') !== BigInt(pins.chainId) ||
      !Array.isArray(supported) || supported.length !== 1 || address(supported[0], 'supported EntryPoint') !== ENTRY_POINT ||
      keccak256(code).toLowerCase() !== pins.entryPoint.codeHash.toLowerCase()) fail('Canary chain, EntryPoint code or Alto profile differs from the pinned release')
}
async function capture(altoRpc, nodeRpc, inputPath, outputPath) {
  const source = JSON.parse(await readFile(inputPath, 'utf8'))
  const preparation = parsePreparation(source)
  await attestStack(altoRpc, nodeRpc)
  const operation = { ...preparation.operation, signature: STUB_SIGNATURE }
  const [estimate, nitro] = await Promise.all([
    rpc(altoRpc, 'eth_estimateUserOperationGas', [formatUserOperationRequest(operation), ENTRY_POINT]),
    nitroFeeControl(nodeRpc, preparation.operation.sender),
  ])
  const checked = checkEstimate(estimate, preparation.operation)
  const funding = await fundingControl(nodeRpc, nitro, preparation.mode, preparation.operation.sender,
    preparation.operation.paymaster, preparation.quoteMaxWei)
  if (Date.now() / 1000 + 10 >= preparation.expiresAt) fail('Preparation expired during gas capture')
  const evidence = {
    schema: 1, chainId: pins.chainId, entryPoint: ENTRY_POINT, deployment: preparation.deployment,
    mode: preparation.mode, sender: preparation.operation.sender, paymaster: preparation.operation.paymaster ?? zeroAddress,
    userOperationHash: preparation.userOperationHash, capturedAt: new Date().toISOString(),
    expiresAt: preparation.expiresAt, quoteMaxWei: toHex(preparation.quoteMaxWei),
    preparedGas: Object.fromEntries(['callGasLimit', 'verificationGasLimit', 'preVerificationGas',
      'paymasterVerificationGasLimit', 'paymasterPostOpGasLimit', 'maxFeePerGas'].filter(key => preparation.operation[key] !== undefined)
      .map(key => [key, toHex(preparation.operation[key])])),
    estimatedGas: checked, nitroFeeControl: nitro, fundingControl: funding,
  }
  await writeFile(outputPath, `${JSON.stringify(evidence, null, 2)}\n`, { flag: 'wx', mode: 0o600 })
  return { mode: evidence.mode, userOperationHash: evidence.userOperationHash, output: outputPath }
}
function checkedEvidence(source, mode) {
  const evidence = record(source, 'evidence')
  keys(evidence, ['schema', 'chainId', 'entryPoint', 'mode', 'sender', 'paymaster',
    'userOperationHash', 'capturedAt', 'expiresAt', 'quoteMaxWei', 'preparedGas',
    'estimatedGas', 'nitroFeeControl', 'fundingControl'], ['deployment'])
  if (evidence.schema !== 1 || evidence.chainId !== pins.chainId || address(evidence.entryPoint, 'evidence EntryPoint') !== ENTRY_POINT ||
      evidence.mode !== mode) fail('Evidence is not for this canary mode or chain')
  const hash = hex(evidence.userOperationHash, 'userOperationHash', 32, 32)
  const sender = address(evidence.sender, 'sender')
  const paymaster = address(evidence.paymaster, 'paymaster')
  if ((mode === 'eth') !== (paymaster.toLowerCase() === zeroAddress)) fail('Evidence payer mode mismatch')
  const cap = quantity(evidence.quoteMaxWei, 'quoteMaxWei', MAX_QUOTE_WEI)
  const gas = record(evidence.preparedGas, 'preparedGas')
  const prepared = {}
  for (const key of ['callGasLimit', 'verificationGasLimit', 'preVerificationGas', 'maxFeePerGas',
    ...(mode === 'sponsored' ? ['paymasterVerificationGasLimit', 'paymasterPostOpGasLimit'] : [])]) {
    prepared[key] = quantity(gas[key], `prepared ${key}`)
  }
  if (mode === 'eth' && (gas.paymasterVerificationGasLimit !== undefined || gas.paymasterPostOpGasLimit !== undefined)) {
    fail('ETH evidence contains paymaster gas fields')
  }
  keys(gas, ['callGasLimit', 'verificationGasLimit', 'preVerificationGas', 'maxFeePerGas',
    ...(mode === 'sponsored' ? ['paymasterVerificationGasLimit', 'paymasterPostOpGasLimit'] : [])])
  const total = ['callGasLimit', 'verificationGasLimit', 'preVerificationGas',
    'paymasterVerificationGasLimit', 'paymasterPostOpGasLimit']
    .reduce((sum, key) => sum + (prepared[key] ?? 0n), 0n)
  if (cap === 0n || cap !== total * prepared.maxFeePerGas) fail('Evidence gas cap mismatch')
  keys(record(evidence.estimatedGas, 'estimatedGas'), ['callGasLimit', 'verificationGasLimit', 'preVerificationGas',
    ...(mode === 'sponsored' ? ['paymasterVerificationGasLimit', 'paymasterPostOpGasLimit'] : [])])
  checkEstimate(evidence.estimatedGas, { ...prepared, ...(mode === 'sponsored' ? { paymaster } : {}) })
  const control = record(evidence.nitroFeeControl, 'Nitro fee control')
  const controlTotal = quantity(control.gasEstimate, 'Nitro total gas')
  const controlL1 = quantity(control.gasEstimateForL1, 'Nitro L1 gas')
  if (controlTotal === 0n || controlL1 > controlTotal || quantity(control.baseFee, 'Nitro base fee') === 0n) {
    fail('Evidence has invalid Nitro fee components')
  }
  quantity(control.l1BaseFeeEstimate, 'Nitro L1 base fee')
  quantity(control.blockNumber, 'Nitro safe block')
  hex(control.blockHash, 'Nitro safe block hash', 32, 32)
  keys(control, ['blockNumber', 'blockHash', 'gasEstimate', 'gasEstimateForL1', 'baseFee', 'l1BaseFeeEstimate'])
  const funding = record(evidence.fundingControl, 'funding control')
  if (quantity(funding.blockNumber, 'funding block') !== quantity(control.blockNumber, 'Nitro safe block') ||
      address(funding.payer, 'funding payer') !== (mode === 'eth' ? sender : paymaster)) fail('Funding snapshot is for another payer or block')
  checkFundingSnapshot(mode, cap, funding)
  const capturedAt = Date.parse(evidence.capturedAt)
  if (!Number.isFinite(capturedAt) || !Number.isSafeInteger(evidence.expiresAt) ||
      capturedAt / 1_000 > evidence.expiresAt || capturedAt > Date.now() + 5 * 60_000) {
    fail('Evidence has invalid capture time')
  }
  return { hash, sender, paymaster, cap, total, evidence }
}
async function verifyOne(altoRpc, nodeRpc, source, mode) {
  const { hash, sender, paymaster, cap, total, evidence } = checkedEvidence(source, mode)
  const [bundlerReceipt, operation, capturedBlock] = await Promise.all([
    rpc(altoRpc, 'eth_getUserOperationReceipt', [hash]),
    rpc(altoRpc, 'eth_getUserOperationByHash', [hash]),
    rpc(nodeRpc, 'eth_getBlockByNumber', [evidence.nitroFeeControl.blockNumber, false]),
  ])
  if (hex(capturedBlock?.hash, 'captured Nitro block hash', 32, 32) !== evidence.nitroFeeControl.blockHash) {
    fail('Nitro fee-control block is no longer canonical')
  }
  const receipt = record(bundlerReceipt, `${mode} bundler receipt`)
  const byHash = record(operation, `${mode} UserOperation by hash`)
  if (hex(receipt.userOpHash, 'receipt hash', 32, 32) !== hash ||
      address(receipt.sender, 'receipt sender') !== sender ||
      address(receipt.paymaster, 'receipt paymaster') !== paymaster ||
      hex(byHash.userOperation?.sender, 'operation sender', 20, 20) !== sender.toLowerCase()) fail(`${mode} receipt does not match the captured quote`)
  const actual = quantity(receipt.actualGasCost, 'actualGasCost')
  const actualGasUsed = quantity(receipt.actualGasUsed, 'actualGasUsed')
  if (!receipt.success || actual === 0n || actual > cap || actualGasUsed === 0n || actualGasUsed > total) {
    fail(`${mode} gas charge exceeded the reviewed quote or the operation failed`)
  }
  const txHash = hex(receipt.receipt?.transactionHash, 'transaction hash', 32, 32)
  const [txReceipt, tx, block] = await Promise.all([
    rpc(nodeRpc, 'eth_getTransactionReceipt', [txHash]),
    rpc(nodeRpc, 'eth_getTransactionByHash', [txHash]),
    rpc(nodeRpc, 'eth_getBlockByNumber', [receipt.receipt.blockNumber, false]),
  ])
  if (txReceipt?.status !== '0x1' || hex(txReceipt.transactionHash, 'transaction hash', 32, 32) !== txHash ||
      hex(txReceipt.blockHash, 'transaction block hash', 32, 32) !== hex(receipt.receipt.blockHash, 'bundler block hash', 32, 32) ||
      hex(block?.hash, 'canonical block hash', 32, 32) !== hex(txReceipt.blockHash, 'transaction block hash', 32, 32) ||
      address(tx?.to, 'transaction destination') !== ENTRY_POINT) fail(`${mode} transaction is not canonical EntryPoint inclusion`)
  const matches = txReceipt.logs.filter(log => address(log.address, 'log address') === ENTRY_POINT &&
    log.topics?.[0]?.toLowerCase() === keccak256(toHex('UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)')))
    .flatMap(log => {
      try {
        const event = decodeEventLog({ abi: entryPoint09Abi, eventName: 'UserOperationEvent', data: log.data, topics: log.topics })
        return event.args.userOpHash.toLowerCase() === hash ? [event.args] : []
      } catch { return [] }
    })
  if (matches.length !== 1 || matches[0].sender !== sender || matches[0].paymaster !== paymaster ||
      matches[0].actualGasCost !== actual || matches[0].actualGasUsed !== actualGasUsed || !matches[0].success) {
    fail(`${mode} onchain UserOperationEvent disagrees with Alto receipt`)
  }
  // Nitro's NodeInterface estimates the whole handleOps transaction. It cannot
  // attribute L1 posting cost to one operation when the bundler batches them.
  let bundleL1 = { available: false }
  if (quantity(receipt.receipt.blockNumber, 'inclusion block') > 0n && typeof tx.input === 'string') {
    try {
      const previous = toHex(BigInt(receipt.receipt.blockNumber) - 1n)
      const raw = await rpc(nodeRpc, 'eth_call', [{ from: tx.from, to: NODE_INTERFACE,
        data: encodeFunctionData({ abi: nodeInterfaceAbi, functionName: 'gasEstimateComponents',
          args: [ENTRY_POINT, false, tx.input] }) }, previous])
      const components = parseComponents(decodeFunctionResult({ abi: nodeInterfaceAbi,
        functionName: 'gasEstimateComponents', data: raw }), 'historical bundle')
      bundleL1 = { available: true, ...Object.fromEntries(Object.entries(components).map(([key, value]) => [key, toHex(value)])) }
    } catch { /* Archive access or same-block state can make this unavailable. */ }
  }
  return { mode, userOperationHash: hash, transactionHash: txHash, actualGasUsed: toHex(actualGasUsed),
    actualGasCostWei: toHex(actual), quoteMaxWei: toHex(cap), quoteUsedBps: Number(actual * 10_000n / cap),
    estimatedGas: evidence.estimatedGas, nitroFeeControl: evidence.nitroFeeControl, bundleL1 }
}
async function verify(altoRpc, nodeRpc, ethPath, sponsoredPath) {
  await attestStack(altoRpc, nodeRpc)
  const [eth, sponsored] = await Promise.all([
    checkedEvidence(JSON.parse(await readFile(ethPath, 'utf8')), 'eth'),
    checkedEvidence(JSON.parse(await readFile(sponsoredPath, 'utf8')), 'sponsored'),
  ])
  if (eth.hash === sponsored.hash) fail('Gas modes must use distinct UserOperations')
  return Promise.all([
    verifyOne(altoRpc, nodeRpc, eth.evidence, 'eth'),
    verifyOne(altoRpc, nodeRpc, sponsored.evidence, 'sponsored'),
  ])
}

async function main(argv) {
  const [action, ...rest] = argv
  if (!['capture', 'verify'].includes(action)) fail('Use capture or verify')
  const { values } = parseArgs({ args: rest, options: {
    'alto-rpc': { type: 'string' }, 'node-rpc': { type: 'string' },
    input: { type: 'string' }, output: { type: 'string' },
    'eth-evidence': { type: 'string' }, 'sponsored-evidence': { type: 'string' },
  }, strict: true })
  const altoRpc = rpcUrl(values['alto-rpc'])
  const nodeRpc = rpcUrl(values['node-rpc'])
  const result = action === 'capture'
    ? await capture(altoRpc, nodeRpc, resolve(values.input), resolve(values.output))
    : await verify(altoRpc, nodeRpc, resolve(values['eth-evidence']), resolve(values['sponsored-evidence']))
  console.log(JSON.stringify(result, null, 2))
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main(process.argv.slice(2)).catch(error => {
    console.error(error instanceof Error ? error.message : 'Canary gas verification failed')
    process.exitCode = 1
  })
}

export { checkEstimate, checkedEvidence, parsePreparation, verifyOne }
