import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { readFileSync } from 'node:fs'
import { execFileSync, spawn } from 'node:child_process'
import { verifyNonceConflict, recoverySql, postgresEnvironment } from './recover-faucet-nonce.mjs'
const require = createRequire(process.env.FAUCET_NODE_PACKAGE ?? new URL('../../frontend/package.json', import.meta.url))
const { keccak256, encodeFunctionData, parseAbi, toHex } = require('viem')
const { privateKeyToAccount } = require('viem/accounts')
const signer = privateKeyToAccount(`0x${'1'.repeat(64)}`) // Public test fixture only.
const address = `0x${'2'.repeat(40)}`, token = `0x${'3'.repeat(40)}`
const replacementHash = `0x${'a'.repeat(64)}`, blockHash = `0x${'b'.repeat(64)}`
const raw = await signer.signTransaction({ chainId: 421614, nonce: 12, type: 'eip1559', to: token,
  data: encodeFunctionData({ abi: parseAbi(['function mint(address,uint256)']), functionName: 'mint', args: [address, 100_000_000_000n] }),
  gas: 61000n, maxFeePerGas: 1n, maxPriorityFeePerGas: 0n, value: 0n })
const fixture = () => ({ address, token_address: token, amount: '100000000000', status: 'submitted', tx_hash: keccak256(raw), raw_tx: raw })
function provider(overrides = {}) {
  return async (method, params) => {
    const values = {
      eth_chainId: '0x66eee',
      eth_getTransactionReceipt: params[0] === replacementHash ? { transactionHash: replacementHash, from: signer.address,
        blockHash, blockNumber: '0x64', status: '0x1', logs: [] } : null,
      eth_getTransactionByHash: { hash: replacementHash, from: signer.address, nonce: '0xc', blockHash, blockNumber: '0x64' },
      eth_getBlockByNumber: { hash: blockHash, number: params[0] === 'safe' ? '0x65' : '0x64', transactions: [replacementHash] },
      eth_call: `0x${'0'.repeat(64)}`,
    }
    const value = structuredClone(values[method])
    return overrides[method] ? overrides[method](value, params) : value
  }
}
test('proves the exact persisted mint was replaced safely on two providers', async () => {
  const proof = await verifyNonceConflict(fixture(), replacementHash, [provider(), provider()])
  assert.equal(proof.nonce, 12)
  assert.equal(proof.sender, signer.address.toLowerCase())
  assert.equal(proof.blockNumber, '100')
})
for (const [name, overrides, code] of [
  ['wrong chain', { eth_chainId: () => '0x1' }, 'WRONG_CHAIN'],
  ['original receipt found', { eth_getTransactionReceipt: (v, p) => p[0] !== replacementHash ? {} : v }, 'ORIGINAL_RECEIPT_EXISTS'],
  ['missing replacement', { eth_getTransactionByHash: () => null }, 'MISSING_CHAIN_EVIDENCE'],
  ['missing receipt', { eth_getTransactionReceipt: () => null }, 'MISSING_CHAIN_EVIDENCE'],
  ['different signer', { eth_getTransactionByHash: v => ({ ...v, from: address }) }, 'SENDER_NONCE_MISMATCH'],
  ['different nonce', { eth_getTransactionByHash: v => ({ ...v, nonce: '0xd' }) }, 'SENDER_NONCE_MISMATCH'],
  ['mismatched receipt hash', { eth_getTransactionReceipt: v => v && ({ ...v, transactionHash: blockHash }) }, 'RECEIPT_MISMATCH'],
  ['unsafe replacement', { eth_getBlockByNumber: (v, p) => ({ ...v, number: p[0] === 'safe' ? '0x63' : v.number }) }, 'REPLACEMENT_NOT_SAFE'],
  ['canonical block mismatch', { eth_getBlockByNumber: (v, p) => ({ ...v, hash: p[0] === 'safe' ? v.hash : replacementHash }) }, 'NON_CANONICAL_REPLACEMENT'],
  ['block omits replacement', { eth_getBlockByNumber: v => ({ ...v, transactions: [] }) }, 'NON_CANONICAL_REPLACEMENT'],
  ['missing logs', { eth_getTransactionReceipt: v => v && ({ ...v, logs: undefined }) }, 'MISSING_RECEIPT_LOGS'],
  ['replacement minted', { eth_getTransactionReceipt: v => v && ({ ...v, logs: [{ address: token, topics: [
    keccak256(toHex('Transfer(address,address,uint256)')), `0x${'0'.repeat(64)}`, `0x${address.slice(2).padStart(64, '0')}`,
  ] }] }) }, 'REPLACEMENT_MINTED_TO_RECIPIENT'],
  ['already funded', { eth_call: () => `0x${'0'.repeat(63)}1` }, 'RECIPIENT_ALREADY_FUNDED'],
  ['malformed balance', { eth_call: () => '0x' }, 'RECIPIENT_ALREADY_FUNDED'],
]) test(`fails closed: ${name}`, async () => {
  await assert.rejects(verifyNonceConflict(fixture(), replacementHash, [provider(), provider(overrides)]), new RegExp(code))
})
test('fails closed when providers disagree on a canonical block', async () => {
  const overrides = {
    eth_getTransactionByHash: v => ({ ...v, blockHash: replacementHash }),
    eth_getTransactionReceipt: v => v && ({ ...v, blockHash: replacementHash }),
    eth_getBlockByNumber: v => ({ ...v, hash: replacementHash }),
  }
  await assert.rejects(verifyNonceConflict(fixture(), replacementHash, [provider(), provider(overrides)]), /PROVIDERS_DISAGREE/)
})
test('RPC failure never authorizes a retry', async () => {
  await assert.rejects(verifyNonceConflict(fixture(), replacementHash, [provider(), async () => { throw new Error('unavailable') }]), /unavailable/)
})
for (const [name, mutate, code] of [
  ['changed hash', c => { c.tx_hash = blockHash }, 'SIGNED_HASH_MISMATCH'],
  ['changed recipient', c => { c.address = token }, 'SIGNED_INTENT_MISMATCH'],
  ['changed token', c => { c.token_address = address }, 'SIGNED_INTENT_MISMATCH'],
  ['changed amount', c => { c.amount = '1' }, 'SIGNED_INTENT_MISMATCH'],
  ['already succeeded', c => { c.status = 'success' }, 'CLAIM_NOT_SUBMITTED'],
]) test(`rejects ${name}`, async () => {
  const c = fixture(); mutate(c)
  await assert.rejects(verifyNonceConflict(c, replacementHash, [provider(), provider()]), new RegExp(code))
})

test('database recovery is atomic, idempotent and fences changed claims', { skip: !process.env.FAUCET_TEST_DATABASE_URL }, async () => {
  const psql = query => execFileSync(process.env.PSQL ?? 'psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], {
    input: `SET search_path TO faucet_recovery_test;\n${query}`, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'],
    env: postgresEnvironment(process.env.FAUCET_TEST_DATABASE_URL),
  }).trim()
  const claim = fixture(), proof = await verifyNonceConflict(claim, replacementHash, [provider(), provider()])
  psql(`CREATE SCHEMA faucet_recovery_test;
    CREATE TABLE testnet_faucet_claims (address TEXT, token_address TEXT, amount BIGINT, status TEXT, tx_hash TEXT, raw_tx TEXT, error TEXT, updated_at TIMESTAMPTZ);
    ${readFileSync(new URL('../config/migrations/testnet-faucet-nonce-recovery-v1.sql', import.meta.url), 'utf8')}`)
  try {
    psql(`INSERT INTO testnet_faucet_claims VALUES ('${address}', '${token}', 100000000000, 'submitted', '${claim.tx_hash}', '${raw}', NULL, NOW());`)
    const reset = () => psql("UPDATE testnet_faucet_claims SET status='submitted'; DELETE FROM testnet_faucet_nonce_recoveries;")
    const count = () => psql('SELECT COUNT(*) FROM testnet_faucet_nonce_recoveries;')
    assert.match(psql(recoverySql(claim, proof)), /"recovered" : 1/)
    assert.equal(psql('SELECT status FROM testnet_faucet_claims;'), 'failed')
    assert.equal(psql('SELECT original_raw_tx FROM testnet_faucet_nonce_recoveries;'), raw)
    assert.match(psql(recoverySql(claim, proof)), /"recovered" : 0/)
    assert.equal(count(), '1')
    reset()
    const concurrent = () => new Promise((resolve, reject) => {
      const child = spawn(process.env.PSQL ?? 'psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], {
        env: postgresEnvironment(process.env.FAUCET_TEST_DATABASE_URL), stdio: ['pipe', 'pipe', 'pipe'],
      })
      let output = ''
      child.stdout.on('data', chunk => { output += chunk })
      child.stderr.resume()
      child.on('error', reject)
      child.on('close', code => code === 0 ? resolve(output) : reject(new Error('concurrent recovery failed')))
      child.stdin.end(`SET search_path TO faucet_recovery_test;\n${recoverySql(claim, proof)}`)
    })
    const races = await Promise.all([concurrent(), concurrent()])
    assert.equal(races.filter(output => output.includes('"recovered" : 1')).length, 1)
    assert.equal(races.filter(output => output.includes('"recovered" : 0')).length, 1)
    assert.equal(count(), '1')
    reset()
    psql("UPDATE testnet_faucet_claims SET status='success';")
    assert.match(psql(recoverySql(claim, proof)), /"recovered" : 0/)
    assert.equal(count(), '0')
    reset()
    psql(`UPDATE testnet_faucet_claims SET tx_hash='${blockHash}';`)
    assert.match(psql(recoverySql(claim, proof)), /"recovered" : 0/)
    assert.equal(count(), '0')
    psql(`UPDATE testnet_faucet_claims SET tx_hash='${claim.tx_hash}';`)
    assert.match(psql(recoverySql(claim, proof)), /"recovered" : 1/)
    // An audit conflict rolls back the claim transition instead of losing evidence.
    psql("UPDATE testnet_faucet_claims SET status='submitted';")
    assert.throws(() => psql(recoverySql(claim, proof)))
    assert.equal(psql('SELECT status FROM testnet_faucet_claims;'), 'submitted')
  } finally { psql('DROP SCHEMA faucet_recovery_test CASCADE;') }
})
