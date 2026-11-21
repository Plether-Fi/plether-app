#!/usr/bin/env node
// One-time Sepolia funding handoff from the busy Plether signer to an isolated canary deployer.
import assert from 'node:assert/strict'
import { closeSync, existsSync, fsyncSync, openSync, readFileSync, renameSync, writeSync } from 'node:fs'
import { createRequire } from 'node:module'
import { resolve } from 'node:path'
import { setTimeout as sleep } from 'node:timers/promises'
import { parseArgs } from 'node:util'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { createPublicClient, getAddress, http, isAddressEqual, keccak256, parseEther, zeroAddress } = require('viem')
const { privateKeyToAccount } = require('viem/accounts')
const { arbitrumSepolia } = require('viem/chains')

const chainId = 421614
const sourceDeployer = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
const valueWei = parseEther('4.6')
const sourceReserveWei = parseEther('1')
const maxGas = 1_000_000n
const maxFeePerGas = 1_000_000_000n
const maxTransactionCostWei = parseEther('0.01')
let phase = 'configuration'

function usage() {
  console.log('Usage: node scripts/kernel-qualification/fund-dedicated-deployer.mjs --config PUBLIC_JSON --manifest PUBLIC_JSON [--broadcast --confirm DEPLOYMENT | --attest]\nDefault is read-only. Broadcast requires KERNEL_CANARY_FUNDING_PRIVATE_KEY from the existing Plether Core signer process. The exact transfer is 4.6 Sepolia ETH.')
}

function loadConfig(path) {
  const config = JSON.parse(readFileSync(resolve(path), 'utf8'))
  assert.match(config.deployment ?? '', /^[a-z0-9][a-z0-9._-]{0,95}$/u, 'deployment must be a stable public id')
  const source = getAddress(config.sourceDeployer)
  const recipient = getAddress(config.dedicatedDeployer)
  assert.ok(isAddressEqual(source, sourceDeployer), 'Funding source must be the existing reviewed Sepolia deployer')
  assert.ok(!isAddressEqual(recipient, zeroAddress), 'Dedicated deployer must not be the zero address')
  assert.ok(!isAddressEqual(source, recipient), 'Dedicated deployer must differ from the funding signer')
  const primaryRpc = new URL(config.primaryRpc)
  const secondaryRpc = new URL(process.env.KERNEL_SECONDARY_RPC_URL || config.secondaryRpc)
  assert.equal(primaryRpc.protocol, 'https:', 'Primary Sepolia RPC must use HTTPS')
  assert.equal(secondaryRpc.protocol, 'https:', 'Secondary Sepolia RPC must use HTTPS')
  assert.notEqual(primaryRpc.hostname, secondaryRpc.hostname, 'Sepolia RPCs must have independent hosts')
  return { deployment: config.deployment, source, recipient, primaryRpc: primaryRpc.href,
    secondaryRpc: secondaryRpc.href, providers: [primaryRpc.hostname, secondaryRpc.hostname] }
}

function clientsFor(config) {
  return [config.primaryRpc, config.secondaryRpc].map(url => createPublicClient({
    chain: arbitrumSepolia, transport: http(url, { timeout: 10_000 }),
  }))
}

export function assertCleanNonce(counts) {
  assert.equal(counts.length, 4, 'Four latest/pending nonce observations are required')
  const [latestA, pendingA, latestB, pendingB] = counts.map(BigInt)
  assert.equal(latestA, latestB, 'Independent RPCs disagree on the funding source latest nonce')
  assert.equal(pendingA, pendingB, 'Independent RPCs disagree on the funding source pending nonce')
  assert.equal(pendingA, latestA, 'Funding source has a pending transaction')
  return latestA
}

async function nonceSnapshot(clients, source) {
  return assertCleanNonce(await Promise.all(clients.flatMap(client => [
    client.getTransactionCount({ address: source, blockTag: 'latest' }),
    client.getTransactionCount({ address: source, blockTag: 'pending' }),
  ])))
}

export async function observeStableNonce(readNonce, wait, expected, samples = 10) {
  for (let index = 0; index < samples; index++) {
    await wait(2_500)
    assert.equal(await readNonce(), expected,
      'Existing deployer nonce changed during the quiet-window observation; no funding was signed')
  }
  return expected
}

async function commonSafeBlock(clients) {
  const [chainA, chainB, safeA, safeB] = await Promise.all([
    clients[0].getChainId(), clients[1].getChainId(),
    clients[0].getBlock({ blockTag: 'safe' }), clients[1].getBlock({ blockTag: 'safe' }),
  ])
  assert.equal(chainA, chainId, 'Primary RPC is not Arbitrum Sepolia')
  assert.equal(chainB, chainId, 'Secondary RPC is not Arbitrum Sepolia')
  const number = safeA.number < safeB.number ? safeA.number : safeB.number
  const [blockA, blockB] = await Promise.all(clients.map(client => client.getBlock({ blockNumber: number })))
  assert.equal(blockA.hash.toLowerCase(), blockB.hash.toLowerCase(), 'Independent RPCs disagree on the common safe block')
  return { number, hash: blockA.hash }
}

async function fundingPlan(config, clients) {
  const safe = await commonSafeBlock(clients)
  const [codeA, codeB, recipientA, recipientB, latestCodeA, latestCodeB,
    latestRecipientA, latestRecipientB, sourceA, sourceB, nonce] = await Promise.all([
    clients[0].getCode({ address: config.recipient, blockNumber: safe.number }),
    clients[1].getCode({ address: config.recipient, blockNumber: safe.number }),
    clients[0].getBalance({ address: config.recipient, blockNumber: safe.number }),
    clients[1].getBalance({ address: config.recipient, blockNumber: safe.number }),
    clients[0].getCode({ address: config.recipient }),
    clients[1].getCode({ address: config.recipient }),
    clients[0].getBalance({ address: config.recipient }),
    clients[1].getBalance({ address: config.recipient }),
    clients[0].getBalance({ address: config.source }),
    clients[1].getBalance({ address: config.source }),
    nonceSnapshot(clients, config.source),
  ])
  assert.ok(!codeA || codeA === '0x', 'Dedicated deployer already has code on the primary RPC')
  assert.ok(!codeB || codeB === '0x', 'Dedicated deployer already has code on the secondary RPC')
  assert.equal(recipientA, 0n, 'Dedicated deployer must start unfunded on the primary RPC')
  assert.equal(recipientB, 0n, 'Dedicated deployer must start unfunded on the secondary RPC')
  assert.ok(!latestCodeA || latestCodeA === '0x', 'Dedicated deployer gained code on the primary RPC')
  assert.ok(!latestCodeB || latestCodeB === '0x', 'Dedicated deployer gained code on the secondary RPC')
  assert.equal(latestRecipientA, 0n, 'Dedicated deployer is already funded on the primary RPC')
  assert.equal(latestRecipientB, 0n, 'Dedicated deployer is already funded on the secondary RPC')
  const required = valueWei + sourceReserveWei + maxTransactionCostWei
  assert.ok(sourceA >= required && sourceB >= required, 'Existing deployer lacks 4.6 ETH plus live-order reserve and gas')
  return { safe, nonce, sourceBalanceWei: sourceA < sourceB ? sourceA : sourceB }
}

function writeDurable(path, value, exclusive) {
  const target = resolve(path)
  const output = `${JSON.stringify(value, null, 2)}\n`
  const temporary = exclusive ? target : `${target}.${process.pid}.tmp`
  const fd = openSync(temporary, 'wx', 0o600)
  try { writeSync(fd, output); fsyncSync(fd) } finally { closeSync(fd) }
  if (!exclusive) renameSync(temporary, target)
}

function loadManifest(path, config) {
  const record = JSON.parse(readFileSync(resolve(path), 'utf8'))
  assert.equal(record.schemaVersion, 1)
  assert.equal(record.deployment, config.deployment)
  assert.ok(isAddressEqual(record.source, config.source))
  assert.ok(isAddressEqual(record.recipient, config.recipient))
  assert.equal(record.valueWei, String(valueWei))
  assert.match(record.hash ?? '', /^0x[0-9a-fA-F]{64}$/u)
  return record
}

export async function verifyFundingReceipt(clients, record) {
  const safe = await commonSafeBlock(clients)
  const [txA, txB, receiptA, receiptB] = await Promise.all([
    clients[0].getTransaction({ hash: record.hash }), clients[1].getTransaction({ hash: record.hash }),
    clients[0].getTransactionReceipt({ hash: record.hash }), clients[1].getTransactionReceipt({ hash: record.hash }),
  ])
  for (const [tx, receipt] of [[txA, receiptA], [txB, receiptB]]) {
    assert.ok(isAddressEqual(tx.from, record.source), 'Funding transaction signer changed')
    assert.ok(isAddressEqual(tx.to, record.recipient), 'Funding recipient changed')
    assert.equal(tx.value, valueWei, 'Funding amount changed')
    assert.equal(BigInt(tx.nonce), BigInt(record.nonce), 'Funding nonce changed')
    assert.equal(tx.input, '0x', 'Funding transaction contains calldata')
    assert.equal(tx.chainId, chainId, 'Funding transaction chain changed')
    assert.equal(tx.hash.toLowerCase(), record.hash.toLowerCase(), 'Funding transaction hash changed')
    assert.equal(receipt.status, 'success', 'Funding transaction reverted')
    assert.equal(receipt.transactionHash.toLowerCase(), record.hash.toLowerCase(), 'Funding receipt hash changed')
    assert.equal(receipt.blockHash.toLowerCase(), tx.blockHash.toLowerCase(), 'Funding transaction block changed')
    assert.equal(receipt.blockNumber, tx.blockNumber, 'Funding transaction height changed')
    assert.ok(receipt.blockNumber <= safe.number, 'Funding transaction is not on the common safe chain yet')
  }
  assert.equal(receiptA.blockHash.toLowerCase(), receiptB.blockHash.toLowerCase(), 'Funding RPCs disagree on inclusion block')
  assert.equal(receiptA.blockNumber, receiptB.blockNumber, 'Funding RPCs disagree on inclusion height')
  const [blockA, blockB] = await Promise.all(clients.map(client => client.getBlock({ blockNumber: receiptA.blockNumber })))
  assert.equal(blockA.hash.toLowerCase(), receiptA.blockHash.toLowerCase(), 'Funding primary receipt is not canonical')
  assert.equal(blockB.hash.toLowerCase(), receiptA.blockHash.toLowerCase(), 'Funding secondary receipt is not canonical')
  const beforeBlock = receiptA.blockNumber - 1n
  const [beforeA, afterA, beforeB, afterB] = await Promise.all(clients.flatMap(client => [
    client.getBalance({ address: record.recipient, blockNumber: beforeBlock }),
    client.getBalance({ address: record.recipient, blockNumber: receiptA.blockNumber }),
  ]))
  assert.equal(afterA - beforeA, valueWei, 'Funding recipient balance delta is not exactly 4.6 ETH on primary')
  assert.equal(afterB - beforeB, valueWei, 'Funding recipient balance delta is not exactly 4.6 ETH on secondary')
  return { blockNumber: String(receiptA.blockNumber), blockHash: receiptA.blockHash, safeBlockNumber: String(safe.number),
    recipientBalanceWei: String(afterA) }
}

export async function broadcast(config, manifestPath, clients, confirmation, options = {}) {
  assert.equal(confirmation, config.deployment, 'Pass --confirm with the exact deployment id')
  assert.ok(!existsSync(resolve(manifestPath)), 'Funding intent manifest already exists; review it before any retry')
  const plan = await fundingPlan(config, clients)
  phase = 'quiet source nonce observation'
  await observeStableNonce(() => nonceSnapshot(clients, config.source), options.wait ?? sleep,
    plan.nonce, options.stableSamples ?? 10)
  const secret = process.env.KERNEL_CANARY_FUNDING_PRIVATE_KEY
  assert.match(secret ?? '', /^0x[0-9a-fA-F]{64}$/u, 'Inject the existing Plether Core signer through the approved process')
  const signer = privateKeyToAccount(secret)
  assert.ok(isAddressEqual(signer.address, config.source), 'Funding signer does not match the reviewed Sepolia deployer')
  const [gasA, gasB, priceA, priceB] = await Promise.all([
    clients[0].estimateGas({ account: config.source, to: config.recipient, value: valueWei, nonce: Number(plan.nonce) }),
    clients[1].estimateGas({ account: config.source, to: config.recipient, value: valueWei, nonce: Number(plan.nonce) }),
    clients[0].getGasPrice(), clients[1].getGasPrice(),
  ])
  const gas = (gasA > gasB ? gasA : gasB) * 5n / 4n + 1n
  const fee = priceA > priceB ? priceA : priceB
  assert.ok(gas >= 21_000n && gas <= maxGas, 'Funding gas estimate is outside the reviewed cap')
  assert.ok(fee > 0n && fee <= maxFeePerGas, 'Funding fee price is outside the reviewed cap')
  assert.ok(gas * fee <= maxTransactionCostWei, 'Funding maximum fee exceeds the reviewed cap')
  const raw = await signer.signTransaction({ type: 'eip1559', chainId, to: config.recipient,
    value: valueWei, nonce: Number(plan.nonce), gas, maxFeePerGas: fee, maxPriorityFeePerGas: 0n })
  const hash = keccak256(raw)
  const record = { schemaVersion: 1, deployment: config.deployment, source: config.source,
    recipient: config.recipient, valueWei: String(valueWei), nonce: String(plan.nonce), hash,
    gas: String(gas), maxFeePerGas: String(fee), maxPriorityFeePerGas: '0',
    safeBlockNumber: String(plan.safe.number), safeBlockHash: plan.safe.hash,
    providers: config.providers, status: 'signed-intent' }
  writeDurable(manifestPath, record, true)
  phase = 'final independent nonce guard'
  const nonce = await nonceSnapshot(clients, config.source)
  assert.equal(nonce, plan.nonce, 'Existing deployer nonce advanced before the funding send; intent was not broadcast')
  phase = 'funding broadcast'
  let sentHash
  try { sentHash = await clients[0].sendRawTransaction({ serializedTransaction: raw }) }
  catch { throw new Error('Funding broadcast outcome is uncertain; inspect the manifest hash on both RPCs before any retry') }
  assert.equal(sentHash.toLowerCase(), hash.toLowerCase(), 'Funding RPC returned a different transaction hash')
  writeDurable(manifestPath, { ...record, status: 'submitted' }, false)
  return { mode: 'submitted', manifest: resolve(manifestPath), hash, source: config.source,
    recipient: config.recipient, valueWei: String(valueWei), nonce: String(plan.nonce) }
}

async function main() {
  const { values: args } = parseArgs({ options: {
    config: { type: 'string' }, manifest: { type: 'string' }, broadcast: { type: 'boolean' },
    attest: { type: 'boolean' }, confirm: { type: 'string' }, help: { type: 'boolean' },
  } })
  if (args.help) return usage()
  assert.ok(args.config && args.manifest, 'Pass --config and --manifest public JSON paths')
  assert.ok(!(args.broadcast && args.attest), 'Choose broadcast or attest')
  assert.ok(!args.confirm || args.broadcast, '--confirm is valid only for broadcast')
  const config = loadConfig(args.config)
  const clients = clientsFor(config)
  if (args.broadcast) {
    phase = 'funding preflight and signed intent'
    console.log(JSON.stringify(await broadcast(config, args.manifest, clients, args.confirm), null, 2))
  } else if (args.attest) {
    phase = 'independent safe funding attestation'
    const record = loadManifest(args.manifest, config)
    const attestation = await verifyFundingReceipt(clients, record)
    writeDurable(args.manifest, { ...record, status: 'attested', attestation }, false)
    console.log(JSON.stringify({ mode: 'attested', manifest: resolve(args.manifest), hash: record.hash,
      recipient: config.recipient, valueWei: String(valueWei), attestation }, null, 2))
  } else {
    phase = 'read-only funding plan'
    const plan = await fundingPlan(config, clients)
    console.log(JSON.stringify({ mode: 'read-only-plan', deployment: config.deployment,
      source: config.source, recipient: config.recipient, valueWei: String(valueWei),
      providers: config.providers, safeBlockNumber: String(plan.safe.number),
      safeBlockHash: plan.safe.hash, sourceBalanceWei: String(plan.sourceBalanceWei),
      sourceNonce: String(plan.nonce), manifestExists: existsSync(resolve(args.manifest)) }, null, 2))
  }
}

if (process.argv[1] && import.meta.url === new URL(`file://${process.argv[1]}`).href) {
  main().catch(error => {
    const secret = process.env.KERNEL_CANARY_FUNDING_PRIVATE_KEY
    const scrubbed = String(error?.shortMessage ?? error?.message ?? error)
      .replaceAll(secret || '\u0000', '[REDACTED]').replace(/https:\/\/[^\s)]+/gu, '[RPC]')
    console.error(`Canary deployer funding stopped during ${phase}: ${scrubbed}`)
    process.exitCode = 1
  })
}
