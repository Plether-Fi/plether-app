#!/usr/bin/env node
// One-use bridge for the isolated Plether Kernel canary. The default mode only reads chain state.
import assert from 'node:assert/strict'
import {
  closeSync, existsSync, fsyncSync, openSync, readFileSync, renameSync, writeSync,
} from 'node:fs'
import { createRequire } from 'node:module'
import { dirname, resolve } from 'node:path'
import { parseArgs } from 'node:util'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const {
  concatHex, createPublicClient, decodeEventLog, getAddress, hexToBytes, http, isAddressEqual,
  keccak256, parseAbi, parseEther, toEventSelector, toHex, toRlp,
} = require('viem')
const { privateKeyToAccount } = require('viem/accounts')
const { arbitrumSepolia, sepolia } = require('viem/chains')

export const dedicated = '0xF4b32B03f0F357aEa01DfaB5FcEe0c8637A492Cb'
export const inbox = '0xaAe29B0366299461418F5324a79Afc425BE5ae21'
export const bridge = '0x38f918D0E9F1b721EDaA41302E399fa1B79333a9'
export const fundingHash = '0xf36b9016bdfd091e43a1444176289c38eb9a7c98a7d800b7acefa080ac8a2620'
export const fundingSender = '0x6cc9397c3b38739dacbfaa68ead5f5d77ba5f455'
export const fundingValueWei = parseEther('2.5')
export const defaultAmountWei = parseEther('2.48')
export const maximumAmountWei = parseEther('2.49')
export const reserveWei = parseEther('0.01')
export const selector = '0x439370b1' // depositEth()
export const l1ChainId = 11155111
export const l2ChainId = 421614
const maxGasLimit = 250_000n
const maxFeePerGasLimit = 10_000_000_000n
const priorityFee = 100_000_000n
const aliasOffset = 0x1111000000000000000000000000000000001111n
const addressModulus = 1n << 160n
const inboxAbi = parseAbi([
  'function bridge() view returns (address)',
  'event InboxMessageDelivered(uint256 indexed messageNum, bytes data)',
])
const bridgeAbi = parseAbi([
  'event MessageDelivered(uint256 indexed messageIndex, bytes32 indexed beforeInboxAcc, address inbox, uint8 kind, address sender, bytes32 messageDataHash, uint256 baseFeeL1, uint64 timestamp)',
])
const inboxEventTopic = toEventSelector('InboxMessageDelivered(uint256,bytes)')
const bridgeEventTopic = toEventSelector('MessageDelivered(uint256,bytes32,address,uint8,address,bytes32,uint256,uint64)')
let phase = 'configuration'

function usage() {
  console.log(`Usage: node bridge-sepolia.mjs --config PUBLIC_JSON --intent PUBLIC_JSON [--broadcast --confirm bridge:DEPLOYMENT:AMOUNT_WEI | --attest]

Default is a read-only plan. Broadcast requires the exact confirmation and KERNEL_CANARY_DEPLOYER_PRIVATE_KEY from the approved signer process. It writes a public, durable signed intent before sending exactly once. Attest requires the existing intent and never signs.
The dedicated signer must remain an EOA with nonce zero on L1 and L2 before broadcast. Never retry a broadcast after an uncertain result; use --attest to look up the recorded hash.`)
}

function checkedRpc(value, label) {
  assert.ok(value, `Provide ${label}`)
  const url = new URL(value)
  assert.equal(url.protocol, 'https:', `${label} must use HTTPS`)
  assert.ok(!url.username && !url.password && !url.search && !url.hash,
    `${label} cannot contain URL credentials, query, or fragment`)
  return url
}

function rpcPair(primaryValue, secondaryValue, chain, label) {
  const primary = checkedRpc(primaryValue, `${label} primary RPC`)
  const secondary = checkedRpc(secondaryValue, `${label} secondary RPC`)
  assert.notEqual(primary.hostname, secondary.hostname, `${label} RPC hosts must be independent`)
  return {
    hosts: [primary.hostname, secondary.hostname],
    clients: [primary.href, secondary.href].map(url => createPublicClient({
      chain, transport: http(url, { timeout: 15_000 }),
    })),
  }
}

export function checkedAmount(value) {
  assert.match(value ?? '', /^[1-9][0-9]*$/u, 'Bridge amount must be a positive decimal Wei string')
  const amount = BigInt(value)
  assert.ok(amount <= maximumAmountWei, 'Bridge amount exceeds the reviewed 2.49 ETH ceiling')
  return amount
}

export function assertSpend({ balanceWei, amountWei, gas, maxFeePerGas }) {
  const amount = BigInt(amountWei)
  const balance = BigInt(balanceWei)
  const gasLimit = BigInt(gas)
  const fee = BigInt(maxFeePerGas)
  assert.ok(amount > 0n && amount <= maximumAmountWei, 'Bridge amount is outside the reviewed range')
  assert.ok(gasLimit >= 21_000n && gasLimit <= maxGasLimit, 'Bridge gas limit exceeds the reviewed cap')
  assert.ok(fee > 0n && fee <= maxFeePerGasLimit, 'Bridge fee cap exceeds 10 gwei')
  const maxCost = gasLimit * fee
  assert.ok(balance >= amount + maxCost + reserveWei,
    'Bridge would leave less than 0.01 ETH on L1 after maximum gas')
  return { maxGasCostWei: maxCost, minimumRemainingWei: balance - amount - maxCost }
}

export function assertCleanNonce(counts, expected = 0n) {
  assert.equal(counts.length, 4, 'Two latest and two pending nonce observations are required')
  const [latestA, pendingA, latestB, pendingB] = counts.map(BigInt)
  assert.equal(latestA, latestB, 'Independent RPCs disagree on latest nonce')
  assert.equal(pendingA, pendingB, 'Independent RPCs disagree on pending nonce')
  assert.equal(pendingA, latestA, 'Signer has a pending transaction')
  assert.equal(latestA, BigInt(expected), 'Signer nonce differs from the reviewed one-use state')
  return latestA
}

export function l1ToL2Alias(address) {
  return getAddress(toHex((BigInt(address) + aliasOffset) % addressModulus, { size: 20 }))
}

// Offchain Labs SDK: EthDepositMessage.calculateDepositTxId. The 0x64 envelope and
// RLP field order are defined by ArbitrumDepositTx in Offchain Labs' go-ethereum fork.
export function depositTxHash(messageIndex, fromAlias, to, valueWei) {
  const fields = [
    toHex(BigInt(l2ChainId)), toHex(BigInt(messageIndex), { size: 32 }),
    getAddress(fromAlias), getAddress(to), toHex(BigInt(valueWei)),
  ]
  return keccak256(concatHex(['0x64', toRlp(fields)]))
}

function loadConfig(path) {
  const c = JSON.parse(readFileSync(resolve(path), 'utf8'))
  assert.match(c.deployment ?? '', /^[a-z0-9][a-z0-9._-]{0,95}$/u,
    'deployment must be a stable public identifier')
  if (c.deployer) assert.ok(isAddressEqual(getAddress(c.deployer), dedicated),
    'Canary deployment config points at another signer')
  const amountWei = checkedAmount(c.amountWei ?? String(defaultAmountWei))
  const l1 = rpcPair(c.l1PrimaryRpc ?? 'https://sepolia.gateway.tenderly.co',
    process.env.KERNEL_L1_SECONDARY_RPC_URL ?? c.l1SecondaryRpc, sepolia, 'Ethereum Sepolia')
  const l2 = rpcPair(c.l2PrimaryRpc ?? 'https://sepolia-rollup.arbitrum.io/rpc',
    process.env.KERNEL_L2_SECONDARY_RPC_URL ?? c.l2SecondaryRpc, arbitrumSepolia, 'Arbitrum Sepolia')
  return { deployment: c.deployment, amountWei, l1, l2 }
}

async function commonSafeBlock(clients, expectedChainId, label) {
  const [idA, idB, safeA, safeB] = await Promise.all([
    clients[0].getChainId(), clients[1].getChainId(),
    clients[0].getBlock({ blockTag: 'safe' }), clients[1].getBlock({ blockTag: 'safe' }),
  ])
  assert.equal(idA, expectedChainId, `Primary RPC is not ${label}`)
  assert.equal(idB, expectedChainId, `Secondary RPC is not ${label}`)
  assert.ok(safeA.number != null && safeB.number != null, `${label} safe block unavailable`)
  const number = safeA.number < safeB.number ? safeA.number : safeB.number
  const [blockA, blockB] = await Promise.all(clients.map(client => client.getBlock({ blockNumber: number })))
  assert.equal(blockA.hash?.toLowerCase(), blockB.hash?.toLowerCase(),
    `${label} RPCs disagree on common safe block`)
  return { number, hash: blockA.hash }
}

async function nonceSnapshot(clients, address, expected) {
  const counts = await Promise.all(clients.flatMap(client => [
    client.getTransactionCount({ address, blockTag: 'latest' }),
    client.getTransactionCount({ address, blockTag: 'pending' }),
  ]))
  return assertCleanNonce(counts, expected)
}

function assertEmptyCode(code, label) {
  assert.ok(!code || code === '0x', `${label} has EVM code; EOA deposit may be aliased`)
}

async function codeSnapshot(clients, address, safe, label, expectedEmpty = true) {
  const [safeA, latestA, safeB, latestB] = await Promise.all(clients.flatMap(client => [
    client.getCode({ address, blockNumber: safe.number }), client.getCode({ address }),
  ]))
  if (expectedEmpty) {
    for (const [name, code] of [['primary safe', safeA], ['primary latest', latestA],
      ['secondary safe', safeB], ['secondary latest', latestB]]) {
      assertEmptyCode(code, `${label} ${name}`)
    }
    return '0x'
  }
  for (const code of [safeA, safeB, latestA, latestB]) {
    assert.ok(code && code !== '0x', `${label} contract code missing`)
    assert.equal(keccak256(code), keccak256(safeA), `${label} code differs across RPCs or blocks`)
  }
  return keccak256(safeA)
}

async function fundingAttestation(clients, safe) {
  const [txA, txB, receiptA, receiptB] = await Promise.all([
    clients[0].getTransaction({ hash: fundingHash }), clients[1].getTransaction({ hash: fundingHash }),
    clients[0].getTransactionReceipt({ hash: fundingHash }),
    clients[1].getTransactionReceipt({ hash: fundingHash }),
  ])
  for (const [tx, receipt] of [[txA, receiptA], [txB, receiptB]]) {
    assert.equal(tx.hash.toLowerCase(), fundingHash, 'L1 funding hash changed')
    assert.ok(isAddressEqual(tx.from, fundingSender), 'L1 funding sender changed')
    assert.ok(tx.to && isAddressEqual(tx.to, dedicated), 'L1 funding recipient changed')
    assert.equal(tx.value, fundingValueWei, 'L1 funding value differs from 2.5 ETH')
    assert.equal(tx.input, '0x', 'L1 funding transaction contains calldata')
    assert.equal(tx.chainId, l1ChainId, 'Funding was not on Ethereum Sepolia')
    assert.equal(receipt.status, 'success', 'L1 funding transaction reverted')
    assert.equal(receipt.blockHash.toLowerCase(), tx.blockHash.toLowerCase(),
      'L1 funding transaction and receipt disagree')
    assert.ok(receipt.blockNumber <= safe.number, 'L1 funding receipt is not on the common safe chain')
  }
  assert.equal(receiptA.blockHash.toLowerCase(), receiptB.blockHash.toLowerCase(),
    'L1 funding RPCs disagree on inclusion block')
  const [blockA, blockB] = await Promise.all(clients.map(client =>
    client.getBlock({ blockNumber: receiptA.blockNumber })))
  assert.equal(blockA.hash.toLowerCase(), receiptA.blockHash.toLowerCase(),
    'L1 funding receipt is not canonical on primary')
  assert.equal(blockB.hash.toLowerCase(), receiptA.blockHash.toLowerCase(),
    'L1 funding receipt is not canonical on secondary')
  const prior = receiptA.blockNumber - 1n
  const [beforeA, afterA, beforeB, afterB] = await Promise.all(clients.flatMap(client => [
    client.getBalance({ address: dedicated, blockNumber: prior }),
    client.getBalance({ address: dedicated, blockNumber: receiptA.blockNumber }),
  ]))
  assert.equal(beforeA, 0n, 'Dedicated signer had prior L1 balance on primary')
  assert.equal(beforeB, 0n, 'Dedicated signer had prior L1 balance on secondary')
  assert.equal(afterA, fundingValueWei, 'L1 funding block did not credit exactly 2.5 ETH on primary')
  assert.equal(afterB, fundingValueWei, 'L1 funding block did not credit exactly 2.5 ETH on secondary')
  return { blockNumber: receiptA.blockNumber, blockHash: receiptA.blockHash }
}

export async function readBalances(clients, address, safe, label) {
  const [safeA, latestA, safeB, latestB] = await Promise.all(clients.flatMap(client => [
    client.getBalance({ address, blockNumber: safe.number }), client.getBalance({ address }),
  ]))
  assert.equal(safeA, safeB, `${label} RPCs disagree on safe balance`)
  assert.equal(latestA, latestB, `${label} RPCs disagree on latest balance`)
  return { safeWei: safeA, latestWei: latestA }
}

function higher(a, b) { return a > b ? a : b }
async function labelled(label, operation) {
  try { return await operation }
  catch (error) { throw new Error(`${label}: ${error?.shortMessage ?? error?.message ?? error}`) }
}

async function readOnlyPlan(config) {
  const [l1Safe, l2Safe] = await Promise.all([
    labelled('L1 safe heads', commonSafeBlock(config.l1.clients, l1ChainId, 'Ethereum Sepolia')),
    labelled('L2 safe heads', commonSafeBlock(config.l2.clients, l2ChainId, 'Arbitrum Sepolia')),
  ])
  const [funding, l1Code, l2Code, inboxCodeHash, l1Balance, l2Balance,
    l1Nonce, l2Nonce, bridgeA, bridgeB] = await Promise.all([
    labelled('L1 funding provenance', fundingAttestation(config.l1.clients, l1Safe)),
    labelled('L1 signer code', codeSnapshot(config.l1.clients, dedicated, l1Safe, 'L1 signer')),
    labelled('L2 signer code', codeSnapshot(config.l2.clients, dedicated, l2Safe, 'L2 signer')),
    labelled('L1 Inbox code', codeSnapshot(config.l1.clients, inbox, l1Safe, 'L1 Inbox', false)),
    labelled('L1 signer balance', readBalances(config.l1.clients, dedicated, l1Safe, 'L1 signer')),
    labelled('L2 signer balance', readBalances(config.l2.clients, dedicated, l2Safe, 'L2 signer')),
    labelled('L1 signer nonce', nonceSnapshot(config.l1.clients, dedicated, 0n)),
    labelled('L2 signer nonce', nonceSnapshot(config.l2.clients, dedicated, 0n)),
    labelled('L1 primary Inbox Bridge', config.l1.clients[0].readContract({ address: inbox, abi: inboxAbi,
      functionName: 'bridge', blockNumber: l1Safe.number })),
    labelled('L1 secondary Inbox Bridge', config.l1.clients[1].readContract({ address: inbox, abi: inboxAbi,
      functionName: 'bridge', blockNumber: l1Safe.number })),
  ])
  assert.equal(l1Code, '0x')
  assert.equal(l2Code, '0x')
  assert.equal(l1Nonce, 0n)
  assert.equal(l2Nonce, 0n)
  assert.ok(isAddressEqual(bridgeA, bridge) && isAddressEqual(bridgeB, bridge),
    'L1 Inbox does not point to the pinned Arbitrum Sepolia Bridge')
  assert.equal(l1Balance.safeWei, fundingValueWei,
    'L1 signer safe balance differs from the attested 2.5 ETH funding')
  assert.equal(l1Balance.latestWei, fundingValueWei,
    'L1 signer latest balance differs from the attested 2.5 ETH funding')
  assert.equal(l2Balance.safeWei, 0n, 'L2 signer already has a safe balance before bridging')
  assert.equal(l2Balance.latestWei, 0n, 'L2 signer already has a latest balance before bridging')
  const [estimateA, estimateB, priceA, priceB, blockA, blockB] = await Promise.all([
    labelled('L1 primary gas estimate', config.l1.clients[0].estimateGas({ account: dedicated, to: inbox,
      data: selector, value: config.amountWei, nonce: 0 })),
    labelled('L1 secondary gas estimate', config.l1.clients[1].estimateGas({ account: dedicated, to: inbox,
      data: selector, value: config.amountWei, nonce: 0 })),
    labelled('L1 primary gas price', config.l1.clients[0].getGasPrice()),
    labelled('L1 secondary gas price', config.l1.clients[1].getGasPrice()),
    labelled('L1 primary latest block', config.l1.clients[0].getBlock({ blockTag: 'latest' })),
    labelled('L1 secondary latest block', config.l1.clients[1].getBlock({ blockTag: 'latest' })),
  ])
  const estimate = higher(estimateA, estimateB)
  const gas = (estimate * 125n + 99n) / 100n
  assert.ok(blockA.baseFeePerGas != null && blockB.baseFeePerGas != null,
    'L1 base fee unavailable')
  const baseFee = higher(blockA.baseFeePerGas, blockB.baseFeePerGas)
  const maxFeePerGas = higher(higher(priceA, priceB), baseFee * 2n + priorityFee)
  const spend = assertSpend({ balanceWei: l1Balance.latestWei,
    amountWei: config.amountWei, gas, maxFeePerGas })
  await Promise.all(config.l1.clients.map((client, index) => labelled(`L1 ${index === 0 ? 'primary' : 'secondary'} deposit simulation`,
    client.call({ account: dedicated, to: inbox, data: selector, value: config.amountWei }))))
  return {
    l1Safe, l2Safe, funding, inboxCodeHash, l1Balance, l2Balance,
    gas, maxFeePerGas, maxPriorityFeePerGas: priorityFee, spend,
  }
}

function fsyncDirectory(path) {
  const fd = openSync(dirname(path), 'r')
  try { fsyncSync(fd) } finally { closeSync(fd) }
}

function writeDurable(path, value, exclusive) {
  const target = resolve(path)
  const temporary = exclusive ? target : `${target}.${process.pid}.tmp`
  const fd = openSync(temporary, 'wx', 0o600)
  try {
    writeSync(fd, `${JSON.stringify(value, null, 2)}\n`)
    fsyncSync(fd)
  } finally { closeSync(fd) }
  if (!exclusive) renameSync(temporary, target)
  fsyncDirectory(target)
}

function loadIntent(path, config) {
  const record = JSON.parse(readFileSync(resolve(path), 'utf8'))
  assert.equal(record.schemaVersion, 1)
  assert.equal(record.deployment, config.deployment)
  assert.equal(record.l1ChainId, l1ChainId)
  assert.equal(record.l2ChainId, l2ChainId)
  assert.ok(isAddressEqual(record.from, dedicated), 'Intent signer changed')
  assert.ok(isAddressEqual(record.to, inbox), 'Intent Inbox changed')
  assert.equal(record.data, selector, 'Intent calldata changed')
  assert.equal(record.valueWei, String(config.amountWei), 'Intent value changed')
  assert.equal(record.nonce, '0', 'Intent nonce changed')
  assert.equal(record.fundingHash, fundingHash, 'Intent funding provenance changed')
  assert.match(record.hash ?? '', /^0x[0-9a-fA-F]{64}$/u, 'Intent hash is invalid')
  assertSpend({ balanceWei: fundingValueWei, amountWei: config.amountWei,
    gas: record.gas, maxFeePerGas: record.maxFeePerGas })
  assert.equal(record.maxPriorityFeePerGas, String(priorityFee), 'Intent priority fee changed')
  return record
}

async function finalGuard(config, plan) {
  const [l1Safe, l2Safe] = await Promise.all([
    commonSafeBlock(config.l1.clients, l1ChainId, 'Ethereum Sepolia'),
    commonSafeBlock(config.l2.clients, l2ChainId, 'Arbitrum Sepolia'),
  ])
  const [l1Balance, , inboxHash, , l2Balance, bridgeA, bridgeB, baseA, baseB] = await Promise.all([
    readBalances(config.l1.clients, dedicated, l1Safe, 'L1 signer'),
    codeSnapshot(config.l1.clients, dedicated, l1Safe, 'L1 signer'),
    codeSnapshot(config.l1.clients, inbox, l1Safe, 'L1 Inbox', false),
    codeSnapshot(config.l2.clients, dedicated, l2Safe, 'L2 signer'),
    readBalances(config.l2.clients, dedicated, l2Safe, 'L2 signer'),
    config.l1.clients[0].readContract({ address: inbox, abi: inboxAbi, functionName: 'bridge' }),
    config.l1.clients[1].readContract({ address: inbox, abi: inboxAbi, functionName: 'bridge' }),
    config.l1.clients[0].getBlock({ blockTag: 'latest' }),
    config.l1.clients[1].getBlock({ blockTag: 'latest' }),
  ])
  assert.equal(inboxHash, plan.inboxCodeHash, 'Inbox code changed since the plan')
  assert.equal(l1Balance.safeWei, fundingValueWei, 'L1 safe balance changed since plan')
  assert.equal(l1Balance.latestWei, fundingValueWei, 'L1 latest balance changed since plan')
  assert.equal(l2Balance.safeWei, 0n, 'L2 safe balance changed since plan')
  assert.equal(l2Balance.latestWei, 0n, 'L2 latest balance changed since plan')
  assert.ok(isAddressEqual(bridgeA, bridge) && isAddressEqual(bridgeB, bridge),
    'Inbox Bridge changed since the plan')
  assert.ok(baseA.baseFeePerGas != null && baseB.baseFeePerGas != null)
  assert.ok(higher(baseA.baseFeePerGas, baseB.baseFeePerGas) + priorityFee <= plan.maxFeePerGas,
    'L1 base fee rose above the signed fee cap')
  assertSpend({ balanceWei: l1Balance.latestWei, amountWei: config.amountWei,
    gas: plan.gas, maxFeePerGas: plan.maxFeePerGas })
  await Promise.all([
    nonceSnapshot(config.l1.clients, dedicated, 0n),
    nonceSnapshot(config.l2.clients, dedicated, 0n),
  ])
}

async function broadcast(config, intentPath, confirmation) {
  assert.equal(confirmation, `bridge:${config.deployment}:${config.amountWei}`,
    'Pass exact --confirm bridge:DEPLOYMENT:AMOUNT_WEI')
  assert.ok(!existsSync(resolve(intentPath)), 'Bridge intent already exists; attest its hash before any retry')
  phase = 'read-only bridge preflight'
  const plan = await readOnlyPlan(config)
  phase = 'signer address check'
  const secret = process.env.KERNEL_CANARY_DEPLOYER_PRIVATE_KEY
  assert.match(secret ?? '', /^0x[0-9a-fA-F]{64}$/u,
    'Inject the dedicated canary key through the approved signer process')
  const signer = privateKeyToAccount(secret)
  assert.ok(isAddressEqual(signer.address, dedicated), 'Signer does not match the dedicated canary address')
  const raw = await signer.signTransaction({ type: 'eip1559', chainId: l1ChainId,
    to: inbox, data: selector, value: config.amountWei, nonce: 0,
    gas: plan.gas, maxFeePerGas: plan.maxFeePerGas,
    maxPriorityFeePerGas: plan.maxPriorityFeePerGas })
  const hash = keccak256(raw)
  const intent = {
    schemaVersion: 1, deployment: config.deployment, status: 'signed-intent',
    l1ChainId, l2ChainId, from: dedicated, to: inbox, data: selector,
    valueWei: String(config.amountWei), nonce: '0', hash, fundingHash,
    gas: String(plan.gas), maxFeePerGas: String(plan.maxFeePerGas),
    maxPriorityFeePerGas: String(plan.maxPriorityFeePerGas),
    maximumGasCostWei: String(plan.spend.maxGasCostWei),
    minimumL1RemainingWei: String(plan.spend.minimumRemainingWei),
    inboxCodeHash: plan.inboxCodeHash,
    l1SafeBlock: String(plan.l1Safe.number), l1SafeHash: plan.l1Safe.hash,
    l2SafeBlock: String(plan.l2Safe.number), l2SafeHash: plan.l2Safe.hash,
    l1Providers: config.l1.hosts, l2Providers: config.l2.hosts,
  }
  phase = 'durable signed intent'
  writeDurable(intentPath, intent, true)
  phase = 'final independent pre-send guard'
  await finalGuard(config, plan)
  phase = 'one-time L1 broadcast'
  let sentHash
  try { sentHash = await config.l1.clients[0].sendRawTransaction({ serializedTransaction: raw }) }
  catch { throw new Error('Broadcast outcome uncertain; inspect the durable intent hash on both L1 RPCs. Never retry automatically.') }
  assert.equal(sentHash.toLowerCase(), hash.toLowerCase(),
    'RPC returned a different hash; inspect the durable intent. Never retry automatically.')
  writeDurable(intentPath, { ...intent, status: 'submitted' }, false)
  return { mode: 'submitted', hash, intent: resolve(intentPath), valueWei: String(config.amountWei) }
}

function parseDepositEvents(receipt) {
  const inboxLogs = receipt.logs.filter(log => isAddressEqual(log.address, inbox) &&
    log.topics[0]?.toLowerCase() === inboxEventTopic.toLowerCase())
  const bridgeLogs = receipt.logs.filter(log => isAddressEqual(log.address, bridge) &&
    log.topics[0]?.toLowerCase() === bridgeEventTopic.toLowerCase())
  assert.equal(inboxLogs.length, 1, 'L1 receipt must contain exactly one Inbox deposit event')
  assert.equal(bridgeLogs.length, 1, 'L1 receipt must contain exactly one Bridge message event')
  const inboxEvent = decodeEventLog({ abi: inboxAbi, ...inboxLogs[0] })
  const bridgeEvent = decodeEventLog({ abi: bridgeAbi, ...bridgeLogs[0] })
  const payload = inboxEvent.args.data
  assert.equal(hexToBytes(payload).length, 52, 'Inbox ETH deposit payload is not 52 bytes')
  const recipient = getAddress(`0x${payload.slice(2, 42)}`)
  const value = BigInt(`0x${payload.slice(42)}`)
  assert.equal(BigInt(bridgeEvent.args.messageIndex), BigInt(inboxEvent.args.messageNum),
    'Inbox and Bridge message indexes differ')
  assert.ok(isAddressEqual(bridgeEvent.args.inbox, inbox), 'Bridge message came from another Inbox')
  assert.equal(Number(bridgeEvent.args.kind), 12, 'Bridge message is not an ETH deposit')
  assert.ok(isAddressEqual(bridgeEvent.args.sender, l1ToL2Alias(dedicated)),
    'Bridge sender is not the dedicated EOA alias')
  assert.equal(bridgeEvent.args.messageDataHash.toLowerCase(), keccak256(payload).toLowerCase(),
    'Bridge message hash differs from Inbox data')
  return { messageIndex: BigInt(inboxEvent.args.messageNum), fromAlias: bridgeEvent.args.sender,
    recipient, value, payload }
}

export function assertDepositEvents(receipt, expectedValueWei) {
  const event = parseDepositEvents(receipt)
  assert.ok(isAddressEqual(event.recipient, dedicated), 'L2 deposit destination changed')
  assert.equal(event.value, BigInt(expectedValueWei), 'Inbox deposit value changed')
  return { ...event, childTxHash: depositTxHash(event.messageIndex,
    event.fromAlias, event.recipient, event.value) }
}

async function attest(config, intentPath) {
  const intent = loadIntent(intentPath, config)
  const [l1Safe, l2Safe] = await Promise.all([
    commonSafeBlock(config.l1.clients, l1ChainId, 'Ethereum Sepolia'),
    commonSafeBlock(config.l2.clients, l2ChainId, 'Arbitrum Sepolia'),
  ])
  await fundingAttestation(config.l1.clients, l1Safe)
  const [txA, txB, receiptA, receiptB] = await Promise.all([
    config.l1.clients[0].getTransaction({ hash: intent.hash }),
    config.l1.clients[1].getTransaction({ hash: intent.hash }),
    config.l1.clients[0].getTransactionReceipt({ hash: intent.hash }),
    config.l1.clients[1].getTransactionReceipt({ hash: intent.hash }),
  ])
  for (const [tx, receipt] of [[txA, receiptA], [txB, receiptB]]) {
    assert.equal(tx.hash.toLowerCase(), intent.hash.toLowerCase(), 'L1 bridge hash changed')
    assert.ok(isAddressEqual(tx.from, dedicated), 'L1 bridge signer changed')
    assert.ok(tx.to && isAddressEqual(tx.to, inbox), 'L1 bridge target changed')
    assert.equal(tx.value, config.amountWei, 'L1 bridge value changed')
    assert.equal(tx.input, selector, 'L1 bridge calldata changed')
    assert.equal(tx.chainId, l1ChainId, 'Bridge transaction is on another L1 chain')
    assert.equal(BigInt(tx.nonce), 0n, 'L1 bridge nonce changed')
    assert.equal(BigInt(tx.gas), BigInt(intent.gas), 'L1 bridge gas limit changed')
    assert.equal(BigInt(tx.maxFeePerGas), BigInt(intent.maxFeePerGas), 'L1 bridge fee cap changed')
    assert.equal(BigInt(tx.maxPriorityFeePerGas), BigInt(intent.maxPriorityFeePerGas),
      'L1 bridge priority fee changed')
    assert.equal(receipt.status, 'success', 'L1 bridge transaction reverted')
    assert.equal(receipt.blockHash.toLowerCase(), tx.blockHash.toLowerCase(),
      'L1 bridge receipt and transaction disagree')
    assert.ok(receipt.blockNumber <= l1Safe.number, 'L1 bridge receipt is not on common safe chain')
  }
  assert.equal(receiptA.blockHash.toLowerCase(), receiptB.blockHash.toLowerCase(),
    'L1 RPCs disagree on bridge inclusion block')
  const [l1BlockA, l1BlockB] = await Promise.all(config.l1.clients.map(client =>
    client.getBlock({ blockNumber: receiptA.blockNumber })))
  assert.equal(l1BlockA.hash.toLowerCase(), receiptA.blockHash.toLowerCase(), 'L1 bridge block not canonical')
  assert.equal(l1BlockB.hash.toLowerCase(), receiptA.blockHash.toLowerCase(), 'L1 bridge block not canonical')
  const depositA = assertDepositEvents(receiptA, config.amountWei)
  const depositB = assertDepositEvents(receiptB, config.amountWei)
  assert.equal(depositA.childTxHash.toLowerCase(), depositB.childTxHash.toLowerCase(),
    'L1 RPCs disagree on L2 deposit transaction hash')
  const childHash = depositA.childTxHash
  const [childTxA, childTxB, childReceiptA, childReceiptB] = await Promise.all([
    config.l2.clients[0].request({ method: 'eth_getTransactionByHash', params: [childHash] }),
    config.l2.clients[1].request({ method: 'eth_getTransactionByHash', params: [childHash] }),
    config.l2.clients[0].request({ method: 'eth_getTransactionReceipt', params: [childHash] }),
    config.l2.clients[1].request({ method: 'eth_getTransactionReceipt', params: [childHash] }),
  ])
  for (const [tx, receipt] of [[childTxA, childReceiptA], [childTxB, childReceiptB]]) {
    assert.ok(tx && receipt, 'L2 deposit is still pending; do not repeat the L1 bridge')
    assert.equal(tx.hash.toLowerCase(), childHash.toLowerCase(), 'L2 deposit hash changed')
    assert.equal(BigInt(tx.type), 100n, 'L2 transaction is not an Arbitrum ETH deposit')
    assert.ok(isAddressEqual(tx.from, depositA.fromAlias), 'L2 deposit sender alias changed')
    assert.ok(isAddressEqual(tx.to, dedicated), 'L2 deposit recipient changed')
    assert.equal(BigInt(tx.value), config.amountWei, 'L2 deposit value changed')
    assert.equal(tx.input, '0x', 'L2 deposit unexpectedly carries calldata')
    // Arbitrum's ETH deposit RPC response may omit requestId; the derived
    // type-0x64 hash already binds the L1 message index and deposit fields.
    assert.equal(BigInt(receipt.status), 1n, 'L2 deposit failed')
    assert.equal(receipt.transactionHash.toLowerCase(), childHash.toLowerCase(),
      'L2 receipt hash changed')
    assert.equal(receipt.blockHash.toLowerCase(), tx.blockHash.toLowerCase(),
      'L2 receipt and transaction disagree')
    assert.ok(BigInt(receipt.blockNumber) <= l2Safe.number,
      'L2 deposit is not on the common safe chain')
  }
  assert.equal(childReceiptA.blockHash.toLowerCase(), childReceiptB.blockHash.toLowerCase(),
    'L2 RPCs disagree on deposit inclusion block')
  const childBlock = BigInt(childReceiptA.blockNumber)
  const [l2BlockA, l2BlockB, l1Balance, l2Balance, l1Nonce, l2Nonce] = await Promise.all([
    config.l2.clients[0].getBlock({ blockNumber: childBlock }),
    config.l2.clients[1].getBlock({ blockNumber: childBlock }),
    readBalances(config.l1.clients, dedicated, l1Safe, 'L1 signer'),
    readBalances(config.l2.clients, dedicated, l2Safe, 'L2 signer'),
    nonceSnapshot(config.l1.clients, dedicated, 1n),
    nonceSnapshot(config.l2.clients, dedicated, 0n),
  ])
  assert.equal(l2BlockA.hash.toLowerCase(), childReceiptA.blockHash.toLowerCase(),
    'L2 deposit block not canonical on primary')
  assert.equal(l2BlockB.hash.toLowerCase(), childReceiptA.blockHash.toLowerCase(),
    'L2 deposit block not canonical on secondary')
  assert.equal(l1Nonce, 1n)
  assert.equal(l2Nonce, 0n)
  assert.ok(l1Balance.safeWei >= reserveWei && l1Balance.latestWei >= reserveWei,
    'L1 post-bridge reserve fell below 0.01 ETH')
  assert.ok(l2Balance.safeWei >= config.amountWei && l2Balance.latestWei >= config.amountWei,
    'L2 balance does not include the bridged ETH')
  await Promise.all([
    codeSnapshot(config.l1.clients, dedicated, l1Safe, 'L1 signer'),
    codeSnapshot(config.l2.clients, dedicated, l2Safe, 'L2 signer'),
  ])
  const prior = childBlock - 1n
  const [beforeA, afterA, beforeB, afterB] = await Promise.all(config.l2.clients.flatMap(client => [
    client.getBalance({ address: dedicated, blockNumber: prior }),
    client.getBalance({ address: dedicated, blockNumber: childBlock }),
  ]))
  // A concurrent independent faucet transfer could share the same L2 block.
  // The derived child transaction/receipt above attributes the deposit; the
  // balance delta only needs to include at least that amount.
  assert.ok(afterA - beforeA >= config.amountWei,
    'L2 deposit block did not credit the bridged value on primary')
  assert.ok(afterB - beforeB >= config.amountWei,
    'L2 deposit block did not credit the bridged value on secondary')
  const attestation = {
    l1Hash: intent.hash, l1BlockNumber: String(receiptA.blockNumber),
    l1BlockHash: receiptA.blockHash, l1SafeBlock: String(l1Safe.number),
    messageIndex: String(depositA.messageIndex), childTxHash: childHash,
    l2BlockNumber: String(childBlock), l2BlockHash: childReceiptA.blockHash,
    l2SafeBlock: String(l2Safe.number), l1RemainingWei: String(l1Balance.latestWei),
    l2BalanceWei: String(l2Balance.latestWei), valueWei: String(config.amountWei),
  }
  if (intent.status !== 'attested') writeDurable(intentPath, { ...intent, status: 'attested', attestation }, false)
  return { mode: 'attested', intent: resolve(intentPath), ...attestation }
}

async function main() {
  const { values: args, positionals } = parseArgs({ options: {
    config: { type: 'string' }, intent: { type: 'string' },
    broadcast: { type: 'boolean' }, attest: { type: 'boolean' },
    confirm: { type: 'string' }, help: { type: 'boolean' },
  }, allowPositionals: true })
  if (args.help) return usage()
  assert.equal(positionals.length, 0, 'Unexpected positional arguments')
  assert.ok(args.config && args.intent, 'Pass --config and --intent')
  assert.ok(!(args.broadcast && args.attest), 'Choose broadcast or attest')
  assert.ok(!args.confirm || args.broadcast, '--confirm is valid only with --broadcast')
  const config = loadConfig(args.config)
  if (args.broadcast) {
    console.log(JSON.stringify(await broadcast(config, args.intent, args.confirm), null, 2))
  } else if (args.attest) {
    phase = 'independent L1 and L2 safe attestation'
    console.log(JSON.stringify(await attest(config, args.intent), null, 2))
  } else {
    phase = 'read-only bridge plan'
    const plan = await readOnlyPlan(config)
    console.log(JSON.stringify({
      mode: 'read-only-plan', deployment: config.deployment, from: dedicated, to: inbox,
      data: selector, amountWei: String(config.amountWei), fundingHash,
      l1SafeBlock: String(plan.l1Safe.number), l1SafeHash: plan.l1Safe.hash,
      l2SafeBlock: String(plan.l2Safe.number), l2SafeHash: plan.l2Safe.hash,
      l1BalanceWei: String(plan.l1Balance.latestWei),
      l2BalanceWei: String(plan.l2Balance.latestWei), gas: String(plan.gas),
      maxFeePerGas: String(plan.maxFeePerGas),
      maximumGasCostWei: String(plan.spend.maxGasCostWei),
      minimumL1RemainingWei: String(plan.spend.minimumRemainingWei),
      l1Providers: config.l1.hosts, l2Providers: config.l2.hosts,
      intentExists: existsSync(resolve(args.intent)),
      broadcastRequires: ['--broadcast', `--confirm bridge:${config.deployment}:${config.amountWei}`,
        'KERNEL_CANARY_DEPLOYER_PRIVATE_KEY'],
    }, null, 2))
  }
}

if (process.argv[1] && import.meta.url === new URL(`file://${resolve(process.argv[1])}`).href) {
  main().catch(error => {
    const secret = process.env.KERNEL_CANARY_DEPLOYER_PRIVATE_KEY
    const scrubbed = String(error?.shortMessage ?? error?.message ?? error)
      .replaceAll(secret || '\u0000', '[REDACTED]')
      .replace(/https:\/\/[^\s)]+/gu, '[RPC]')
    console.error(`Canary L1-to-L2 bridge stopped during ${phase}: ${scrubbed}`)
    process.exitCode = 1
  })
}
