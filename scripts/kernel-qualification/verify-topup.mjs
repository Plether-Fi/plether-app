#!/usr/bin/env node
// Read-only canary L2 ETH top-up attestation. It never signs or sends.
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { resolve } from 'node:path'
import { parseArgs } from 'node:util'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { createPublicClient, getAddress, http, isAddressEqual, parseEther } = require('viem')
const { arbitrumSepolia } = require('viem/chains')

export const chainId = 421614
export const recipient = '0xF4b32B03f0F357aEa01DfaB5FcEe0c8637A492Cb'
export const minimumTopupWei = parseEther('0.27')
export const minimumTotalWei = parseEther('2.75')
const defaultPrimary = 'https://sepolia-rollup.arbitrum.io/rpc'
const defaultSecondary = 'https://arbitrum-sepolia.gateway.tenderly.co'

function checkedRpc(raw, label) {
  const url = new URL(raw)
  assert.equal(url.protocol, 'https:', `${label} must use HTTPS`)
  assert.ok(!url.username && !url.password && !url.search && !url.hash,
    `${label} cannot contain URL credentials, query, or fragment`)
  return url
}

function clientsFor(primaryRaw, secondaryRaw) {
  const primary = checkedRpc(primaryRaw, 'primary RPC')
  const secondary = checkedRpc(secondaryRaw, 'secondary RPC')
  assert.notEqual(primary.hostname, secondary.hostname, 'RPC hosts must be independent')
  return {
    hosts: [primary.hostname, secondary.hostname],
    clients: [primary.href, secondary.href].map(url => createPublicClient({
      chain: arbitrumSepolia,
      transport: http(url, { timeout: 15_000 }),
    })),
  }
}

function hexHash(hash) {
  assert.match(hash ?? '', /^0x[0-9a-fA-F]{64}$/u, 'Provide a public L2 transaction hash')
  return hash.toLowerCase()
}

export function assertDirectTopup(tx, receipt, hash, safeBlock) {
  assert.ok(tx && receipt, 'Top-up transaction or receipt is unavailable')
  const expectedHash = hexHash(hash)
  assert.equal(tx.hash?.toLowerCase(), expectedHash, 'Top-up hash changed')
  assert.equal(receipt.transactionHash?.toLowerCase(), expectedHash, 'Top-up receipt hash changed')
  assert.ok(tx.to && isAddressEqual(tx.to, recipient), 'Top-up recipient is not dedicated signer')
  assert.ok(tx.from && !isAddressEqual(tx.from, recipient),
    'Top-up must come from another address')
  assert.equal(tx.input, '0x', 'Top-up must be a direct ETH transfer without calldata')
  assert.ok(tx.type != null && [0n, 1n, 2n].includes(BigInt(tx.type)),
    'Top-up must be a standard L2 user transaction')
  if (tx.chainId != null) assert.equal(BigInt(tx.chainId), BigInt(chainId),
    'Top-up transaction chain ID changed')
  const value = BigInt(tx.value)
  assert.ok(value >= minimumTopupWei, 'Top-up value is below 0.27 ETH')
  assert.equal(BigInt(receipt.status), 1n, 'Top-up transaction reverted')
  assert.ok(tx.blockNumber != null && tx.blockHash &&
    receipt.blockNumber != null && receipt.blockHash,
  'Top-up is not mined')
  const blockNumber = BigInt(tx.blockNumber)
  assert.equal(BigInt(receipt.blockNumber), blockNumber,
    'Top-up transaction and receipt block numbers differ')
  assert.equal(receipt.blockHash.toLowerCase(), tx.blockHash.toLowerCase(),
    'Top-up transaction and receipt block hashes differ')
  assert.ok(blockNumber <= BigInt(safeBlock),
    'Top-up is not on the common safe chain')
  return {
    from: getAddress(tx.from), valueWei: value, blockNumber,
    blockHash: tx.blockHash.toLowerCase(),
  }
}

export function assertTwoRpcAgreement(a, b) {
  assert.equal(a.from.toLowerCase(), b.from.toLowerCase(), 'RPCs disagree on sender')
  assert.equal(a.valueWei, b.valueWei, 'RPCs disagree on transfer value')
  assert.equal(a.blockNumber, b.blockNumber, 'RPCs disagree on inclusion block')
  assert.equal(a.blockHash, b.blockHash, 'RPCs disagree on inclusion block hash')
  return a
}

export function assertBalanceEvidence(before, after, safe, latest, valueWei) {
  assert.equal(before.length, 2)
  assert.equal(after.length, 2)
  assert.equal(safe.length, 2)
  assert.equal(latest.length, 2)
  for (const observations of [before, after, safe, latest])
    assert.equal(observations[0], observations[1], 'RPCs disagree on signer balance')
  assert.ok(after[0] - before[0] >= valueWei,
    'Top-up inclusion block did not credit the transfer value')
  assert.ok(safe[0] >= minimumTotalWei, 'Signer has less than 2.75 ETH at common safe block')
  assert.ok(latest[0] >= minimumTotalWei, 'Signer has less than 2.75 ETH at latest block')
  return { beforeWei: before[0], afterWei: after[0], safeWei: safe[0], latestWei: latest[0] }
}

export function assertUnusedSigner(code, latestNonce, pendingNonce, label) {
  assert.ok(!code || code === '0x', `${label} dedicated signer has code`)
  // Viem getTransactionCount returns a number; raw JSON-RPC returns a hex string.
  assert.equal(BigInt(latestNonce), 0n, `${label} dedicated signer already sent an L2 transaction`)
  assert.equal(BigInt(pendingNonce), 0n, `${label} dedicated signer has a pending L2 transaction`)
}

async function commonSafeBlock(clients) {
  const [idA, idB, headA, headB] = await Promise.all([
    clients[0].getChainId(), clients[1].getChainId(),
    clients[0].getBlock({ blockTag: 'safe' }), clients[1].getBlock({ blockTag: 'safe' }),
  ])
  assert.equal(idA, chainId, 'Primary RPC is not Arbitrum Sepolia')
  assert.equal(idB, chainId, 'Secondary RPC is not Arbitrum Sepolia')
  assert.ok(headA.number != null && headB.number != null, 'Safe block unavailable')
  const number = headA.number < headB.number ? headA.number : headB.number
  const [a, b] = await Promise.all(clients.map(client => client.getBlock({ blockNumber: number })))
  assert.equal(a.hash?.toLowerCase(), b.hash?.toLowerCase(),
    'RPCs disagree on common safe block')
  return { number, hash: a.hash }
}

async function verify(hash, primaryRaw, secondaryRaw) {
  const pair = clientsFor(primaryRaw, secondaryRaw)
  const safe = await commonSafeBlock(pair.clients)
  const [txA, txB, receiptA, receiptB] = await Promise.all([
    pair.clients[0].request({ method: 'eth_getTransactionByHash', params: [hash] }),
    pair.clients[1].request({ method: 'eth_getTransactionByHash', params: [hash] }),
    pair.clients[0].request({ method: 'eth_getTransactionReceipt', params: [hash] }),
    pair.clients[1].request({ method: 'eth_getTransactionReceipt', params: [hash] }),
  ])
  const transfer = assertTwoRpcAgreement(
    assertDirectTopup(txA, receiptA, hash, safe.number),
    assertDirectTopup(txB, receiptB, hash, safe.number),
  )
  const included = await Promise.all(pair.clients.map(client =>
    client.getBlock({ blockNumber: transfer.blockNumber })))
  for (const block of included) assert.equal(block.hash?.toLowerCase(), transfer.blockHash,
    'Top-up block is not canonical')
  const previous = transfer.blockNumber - 1n
  const [beforeA, afterA, safeA, latestA, beforeB, afterB, safeB, latestB] =
    await Promise.all(pair.clients.flatMap(client => [
      client.getBalance({ address: recipient, blockNumber: previous }),
      client.getBalance({ address: recipient, blockNumber: transfer.blockNumber }),
      client.getBalance({ address: recipient, blockNumber: safe.number }),
      client.getBalance({ address: recipient }),
    ]))
  const balance = assertBalanceEvidence(
    [beforeA, beforeB], [afterA, afterB], [safeA, safeB], [latestA, latestB],
    transfer.valueWei,
  )
  const [codeA, nonceLatestA, noncePendingA, codeB, nonceLatestB, noncePendingB] =
    await Promise.all(pair.clients.flatMap(client => [
      client.getCode({ address: recipient, blockNumber: safe.number }),
      client.getTransactionCount({ address: recipient, blockTag: 'latest' }),
      client.getTransactionCount({ address: recipient, blockTag: 'pending' }),
    ]))
  assertUnusedSigner(codeA, nonceLatestA, noncePendingA, 'Primary')
  assertUnusedSigner(codeB, nonceLatestB, noncePendingB, 'Secondary')
  return {
    mode: 'read-only-l2-topup-attestation', chainId,
    hash, from: transfer.from, to: recipient, valueWei: String(transfer.valueWei),
    inclusionBlock: String(transfer.blockNumber), inclusionBlockHash: transfer.blockHash,
    commonSafeBlock: String(safe.number), commonSafeHash: safe.hash,
    balanceBeforeWei: String(balance.beforeWei), balanceAfterWei: String(balance.afterWei),
    safeBalanceWei: String(balance.safeWei), latestBalanceWei: String(balance.latestWei),
    rpcHosts: pair.hosts,
  }
}

async function main() {
  const { values, positionals } = parseArgs({ options: {
    hash: { type: 'string' }, primary: { type: 'string' }, secondary: { type: 'string' },
    help: { type: 'boolean' },
  }, allowPositionals: true })
  if (values.help) {
    console.log('Usage: node verify-topup.mjs --hash 0x... [--primary HTTPS_RPC --secondary HTTPS_RPC]')
    return
  }
  assert.equal(positionals.length, 0, 'Unexpected positional arguments')
  const hash = hexHash(values.hash)
  console.log(JSON.stringify(await verify(
    hash, values.primary ?? defaultPrimary, values.secondary ?? defaultSecondary,
  ), null, 2))
}

if (process.argv[1] && import.meta.url === new URL(`file://${resolve(process.argv[1])}`).href)
  main().catch(error => {
    const scrubbed = String(error?.shortMessage ?? error?.message ?? error)
      .replace(/https:\/\/[^\s)]+/gu, '[RPC]')
    console.error(`L2 top-up attestation stopped: ${scrubbed}`)
    process.exitCode = 1
  })
