#!/usr/bin/env node
// Read-only attestation of a dedicated canary funding transfer.
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { createRequire } from 'node:module'
import { resolve } from 'node:path'
import { parseArgs } from 'node:util'
import { isCliEntry, scrubRpcError, writePublicEvidence } from './local-safety.mjs'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { createPublicClient, getAddress, http, isAddressEqual, zeroAddress } = require('viem')
const { arbitrumSepolia } = require('viem/chains')

const chainId = 421614
function loadConfig(path) {
  const config = JSON.parse(readFileSync(resolve(path), 'utf8'))
  assert.match(config.deployment ?? '', /^[a-z0-9][a-z0-9._-]{0,95}$/u, 'deployment must be a stable public id')
  const source = getAddress(config.sourceDeployer)
  const recipient = getAddress(config.dedicatedDeployer)
  assert.ok(!isAddressEqual(source, zeroAddress), 'Funding source must not be the zero address')
  assert.ok(!isAddressEqual(recipient, zeroAddress), 'Dedicated deployer must not be the zero address')
  assert.ok(!isAddressEqual(source, recipient), 'Dedicated deployer must differ from the funding signer')
  assert.equal(typeof config.valueWei, 'string', 'Funding amount must be a decimal wei string')
  assert.match(config.valueWei, /^[1-9][0-9]*$/u, 'Funding amount must be supplied in decimal wei')
  const primaryRpc = new URL(config.primaryRpc)
  const secondaryRpc = new URL(process.env.KERNEL_SECONDARY_RPC_URL || config.secondaryRpc)
  assert.equal(primaryRpc.protocol, 'https:', 'Primary Sepolia RPC must use HTTPS')
  assert.equal(secondaryRpc.protocol, 'https:', 'Secondary Sepolia RPC must use HTTPS')
  assert.notEqual(primaryRpc.hostname, secondaryRpc.hostname, 'Sepolia RPCs must have independent hosts')
  return { deployment: config.deployment, source, recipient, valueWei: config.valueWei, primaryRpc: primaryRpc.href,
    secondaryRpc: secondaryRpc.href, providers: [primaryRpc.hostname, secondaryRpc.hostname] }
}

function clientsFor(config) {
  return [config.primaryRpc, config.secondaryRpc].map(url => createPublicClient({
    chain: arbitrumSepolia, transport: http(url, { timeout: 10_000 }),
  }))
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

function loadManifest(path, config) {
  const record = JSON.parse(readFileSync(resolve(path), 'utf8'))
  assert.equal(record.schemaVersion, 1)
  assert.equal(record.deployment, config.deployment)
  assert.ok(isAddressEqual(record.source, config.source))
  assert.ok(isAddressEqual(record.recipient, config.recipient))
  assert.equal(record.valueWei, config.valueWei)
  assert.match(record.hash ?? '', /^0x[0-9a-fA-F]{64}$/u)
  return record
}

export async function verifyFundingReceipt(clients, record) {
  const valueWei = BigInt(record.valueWei)
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
  assert.equal(afterA - beforeA, valueWei, 'Primary funding recipient balance delta differs from reviewed amount')
  assert.equal(afterB - beforeB, valueWei, 'Secondary funding recipient balance delta differs from reviewed amount')
  return { blockNumber: String(receiptA.blockNumber), blockHash: receiptA.blockHash, safeBlockNumber: String(safe.number),
    recipientBalanceWei: String(afterA) }
}

async function main() {
  const { values: args } = parseArgs({ options: {
    config: { type: 'string' }, manifest: { type: 'string' },
    attest: { type: 'boolean' }, help: { type: 'boolean' },
  } })
  if (args.help) {
    console.log('Usage: node scripts/kernel-qualification/fund-dedicated-deployer.mjs --config PUBLIC_JSON --manifest EXISTING_PUBLIC_JSON --attest')
    return
  }
  assert.ok(args.config && args.manifest && args.attest, 'Pass --config and --manifest public JSON paths and --attest')
  const config = loadConfig(args.config)
  const record = loadManifest(args.manifest, config)
  const attestation = await verifyFundingReceipt(clientsFor(config), record)
  writePublicEvidence(args.manifest, JSON.stringify({ ...record, status: 'attested', attestation }, null, 2) + '\n')
  console.log(JSON.stringify({ mode: 'attested', manifest: resolve(args.manifest), hash: record.hash,
    recipient: config.recipient, valueWei: config.valueWei, attestation }, null, 2))
}

if (isCliEntry(import.meta.url)) {
  main().catch(error => {
    console.error('Canary funding attestation stopped: ' + scrubRpcError(error))
    process.exitCode = 1
  })
}
