// Operator-only repair. No signing key, explorer, transaction submission, or automatic retry.
import { existsSync } from 'node:fs'
import { createRequire } from 'node:module'
import { execFileSync } from 'node:child_process'
import { pathToFileURL } from 'node:url'

const require = createRequire(process.env.FAUCET_NODE_PACKAGE ?? (existsSync('/app/oracle/package.json') ? '/app/oracle/package.json' : new URL('../../frontend/package.json', import.meta.url)))
const { keccak256, parseTransaction, recoverTransactionAddress, encodeFunctionData, parseAbi, toHex } = require('viem')
const chainId = 421614
const lockId = '4216140100000001' // Same lock as withFaucetSignerLock.
const zeroTopic = `0x${'0'.repeat(64)}`
const transferTopic = keccak256(toHex('Transfer(address,address,uint256)'))
const eq = (a, b) => typeof a === 'string' && typeof b === 'string' && a.toLowerCase() === b.toLowerCase()
const requireProof = (value, code) => { if (!value) throw new Error(code) }
const hash = value => typeof value === 'string' && /^0x[0-9a-f]{64}$/i.test(value)
const address = value => typeof value === 'string' && /^0x[0-9a-f]{40}$/i.test(value)
const quantity = value => typeof value === 'string' && /^0x(?:0|[1-9a-f][0-9a-f]*)$/i.test(value)

export async function verifyNonceConflict(claim, replacementHash, rpcProviders) {
  requireProof(claim.status === 'submitted' && hash(claim.tx_hash) && hash(replacementHash), 'CLAIM_NOT_SUBMITTED')
  requireProof(!eq(claim.tx_hash, replacementHash), 'REPLACEMENT_IS_ORIGINAL')
  requireProof(address(claim.address) && address(claim.token_address), 'INVALID_CLAIM_SCOPE')
  requireProof(typeof claim.raw_tx === 'string' && /^0x02[0-9a-f]+$/i.test(claim.raw_tx) && claim.raw_tx.length <= 4096, 'INVALID_SIGNED_TRANSACTION')
  requireProof(eq(keccak256(claim.raw_tx), claim.tx_hash), 'SIGNED_HASH_MISMATCH')
  const tx = parseTransaction(claim.raw_tx)
  const sender = await recoverTransactionAddress({ serializedTransaction: claim.raw_tx })
  const amount = BigInt(claim.amount)
  const data = encodeFunctionData({ abi: parseAbi(['function mint(address,uint256)']), functionName: 'mint', args: [claim.address, amount] })
  requireProof(tx.chainId === chainId && tx.type === 'eip1559' && (tx.value ?? 0n) === 0n &&
    eq(tx.to, claim.token_address) && eq(tx.data, data) && amount === 100_000_000_000n, 'SIGNED_INTENT_MISMATCH')
  requireProof(Number.isSafeInteger(tx.nonce) && tx.nonce >= 0, 'INVALID_NONCE')
  requireProof(rpcProviders.length === 2, 'TWO_PROVIDERS_REQUIRED')

  const proofs = await Promise.all(rpcProviders.map(async rpc => {
    requireProof(BigInt(await rpc('eth_chainId', [])) === BigInt(chainId), 'WRONG_CHAIN')
    const [original, replacement, receipt, safe] = await Promise.all([
      rpc('eth_getTransactionReceipt', [claim.tx_hash]),
      rpc('eth_getTransactionByHash', [replacementHash]),
      rpc('eth_getTransactionReceipt', [replacementHash]),
      rpc('eth_getBlockByNumber', ['safe', false]),
    ])
    requireProof(original === null, 'ORIGINAL_RECEIPT_EXISTS')
    requireProof(replacement && receipt && safe && hash(safe.hash) && quantity(safe.number), 'MISSING_CHAIN_EVIDENCE')
    requireProof(eq(replacement.hash, replacementHash) && eq(replacement.from, sender) &&
      quantity(replacement.nonce) && BigInt(replacement.nonce) === BigInt(tx.nonce), 'SENDER_NONCE_MISMATCH')
    requireProof(hash(receipt.blockHash) && quantity(receipt.blockNumber) &&
      eq(receipt.transactionHash, replacementHash) && eq(receipt.from, sender) &&
      eq(receipt.blockHash, replacement.blockHash) && receipt.blockNumber === replacement.blockNumber &&
      (receipt.status === '0x0' || receipt.status === '0x1'), 'RECEIPT_MISMATCH')
    requireProof(BigInt(receipt.blockNumber) <= BigInt(safe.number), 'REPLACEMENT_NOT_SAFE')
    const canonical = await rpc('eth_getBlockByNumber', [receipt.blockNumber, false])
    requireProof(canonical && eq(canonical.hash, receipt.blockHash) && canonical.number === receipt.blockNumber &&
      Array.isArray(canonical.transactions) && canonical.transactions.some(h => eq(h, replacementHash)), 'NON_CANONICAL_REPLACEMENT')
    requireProof(Array.isArray(receipt.logs), 'MISSING_RECEIPT_LOGS')
    // A replacement can itself fulfill the mint. Never clear such a claim, even if funds were spent.
    const recipientTopic = `0x${claim.address.slice(2).toLowerCase().padStart(64, '0')}`
    requireProof(!receipt.logs.some(log => eq(log.address, claim.token_address) &&
      eq(log.topics?.[0], transferTopic) && eq(log.topics?.[1], zeroTopic) && eq(log.topics?.[2], recipientTopic)), 'REPLACEMENT_MINTED_TO_RECIPIENT')
    const balanceData = encodeFunctionData({ abi: parseAbi(['function balanceOf(address) view returns (uint256)']), functionName: 'balanceOf', args: [claim.address] })
    const balances = await Promise.all(['safe', 'latest'].map(block => rpc('eth_call', [{ to: claim.token_address, data: balanceData }, block])))
    requireProof(balances.every(value => /^0x[0-9a-f]{64}$/i.test(value) && BigInt(value) === 0n), 'RECIPIENT_ALREADY_FUNDED')
    return { replacementHash: replacementHash.toLowerCase(), blockHash: receipt.blockHash.toLowerCase(), blockNumber: BigInt(receipt.blockNumber).toString() }
  }))
  requireProof(JSON.stringify(proofs[0]) === JSON.stringify(proofs[1]), 'PROVIDERS_DISAGREE')
  return { chainId, sender: sender.toLowerCase(), nonce: tx.nonce, originalHash: claim.tx_hash.toLowerCase(), ...proofs[0] }
}

export function recoverySql(claim, proof) {
  // Values are not printed. SQL runs in one transaction; compare-and-swap prevents
  // a concurrent retry/finalizer or a repeated admin run from resetting newer work.
  const literal = value => `'${String(value).replaceAll("'", "''")}'`
  return `BEGIN;
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15s';
SELECT pg_advisory_xact_lock(${lockId});
WITH changed AS (
 UPDATE testnet_faucet_claims SET status = 'failed', error = 'verified_nonce_conflict', updated_at = NOW()
 WHERE address = ${literal(claim.address)} AND token_address = ${literal(claim.token_address)}
 AND amount = ${literal(claim.amount)} AND status = 'submitted'
 AND tx_hash = ${literal(claim.tx_hash)} AND raw_tx = ${literal(claim.raw_tx)}
 RETURNING address, token_address, amount, tx_hash, raw_tx
), audit AS (
 INSERT INTO testnet_faucet_nonce_recoveries
 (original_tx_hash, address, token_address, amount, original_raw_tx, chain_id, sender, nonce, replacement_tx_hash, replacement_block_number, replacement_block_hash)
 SELECT tx_hash, address, token_address, amount, raw_tx, ${proof.chainId}, ${literal(proof.sender)}, ${proof.nonce},
 ${literal(proof.replacementHash)}, ${literal(proof.blockNumber)}, ${literal(proof.blockHash)} FROM changed
 RETURNING original_tx_hash
)
SELECT json_build_object('recovered', (SELECT COUNT(*) FROM audit));
COMMIT;`
}

export function postgresEnvironment(connectionString) {
  const url = new URL(connectionString)
  requireProof(['postgres:', 'postgresql:'].includes(url.protocol), 'INVALID_DATABASE_URL')
  const env = {
    ...process.env, PGHOST: url.hostname, PGPORT: url.port || '5432',
    PGUSER: decodeURIComponent(url.username), PGPASSWORD: decodeURIComponent(url.password),
    PGDATABASE: decodeURIComponent(url.pathname.slice(1)), PGCONNECT_TIMEOUT: '10',
  }
  const parameters = { sslmode: 'PGSSLMODE', sslrootcert: 'PGSSLROOTCERT', sslcert: 'PGSSLCERT', sslkey: 'PGSSLKEY', connect_timeout: 'PGCONNECT_TIMEOUT', application_name: 'PGAPPNAME', options: 'PGOPTIONS' }
  for (const [key, value] of url.searchParams) {
    requireProof(Object.hasOwn(parameters, key), 'UNSUPPORTED_DATABASE_PARAMETER')
    env[parameters[key]] = value
  }
  return env
}

function sql(query) {
  try {
    return execFileSync('psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], {
      input: query, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'],
      env: postgresEnvironment(process.env.DATABASE_URL), timeout: 25_000,
    }).trim()
  } catch { throw new Error('DATABASE_OPERATION_FAILED') } // Never print SQL/raw signatures/URLs.
}

function rpcProvider(url, token) {
  return async (method, params) => {
    try {
      const response = await fetch(url, {
        method: 'POST', headers: { 'content-type': 'application/json', ...(token ? { Authorization: `Bearer ${token}` } : {}) },
        body: JSON.stringify({ jsonrpc: '2.0', id: 1, method, params }), signal: AbortSignal.timeout(12_000),
      })
      requireProof(response.ok, 'RPC_HTTP_FAILED')
      const body = await response.json()
      requireProof(!body.error && Object.hasOwn(body, 'result'), 'RPC_RESPONSE_FAILED')
      return body.result
    } catch { throw new Error('RPC_CHECK_UNAVAILABLE') }
  }
}

export async function main(args) {
  const [originalHash, replacementHash, mode] = args
  requireProof(hash(originalHash) && hash(replacementHash) && ['--check', '--apply'].includes(mode) && args.length === 3, 'USAGE: original-hash replacement-hash --check|--apply')
  requireProof(process.env.DATABASE_URL && process.env.PERPS_RPC_URL, 'DATABASE_AND_PRIMARY_RPC_REQUIRED')
  const secondary = 'https://sepolia-rollup.arbitrum.io/rpc'
  requireProof(new URL(process.env.PERPS_RPC_URL).hostname !== new URL(secondary).hostname, 'INDEPENDENT_PRIMARY_RPC_REQUIRED')
  const rows = JSON.parse(sql(`SELECT COALESCE(json_agg(c), '[]'::json) FROM (
    SELECT address, token_address, amount::text, status, tx_hash, raw_tx FROM testnet_faucet_claims WHERE tx_hash = '${originalHash.toLowerCase()}'
  ) c;`))
  if (rows.length === 0 || (rows.length === 1 && rows[0].status === 'failed')) {
    const recovered = JSON.parse(sql(`SELECT json_build_object('recovered', COUNT(*)) FROM testnet_faucet_nonce_recoveries WHERE original_tx_hash = '${originalHash.toLowerCase()}' AND replacement_tx_hash = '${replacementHash.toLowerCase()}';`))
    requireProof(Number(recovered.recovered) === 1, 'CLAIM_NOT_SUBMITTED')
    console.log(JSON.stringify({ originalHash, status: 'already_recovered' }))
    return
  }
  requireProof(rows.length === 1, 'EXACTLY_ONE_CLAIM_REQUIRED')
  const claim = rows[0]
  const proof = await verifyNonceConflict(claim, replacementHash, [
    rpcProvider(process.env.PERPS_RPC_URL, process.env.PERPS_RPC_AUTH_TOKEN), rpcProvider(secondary),
  ])
  if (mode === '--apply') {
    const result = sql(recoverySql(claim, proof)).split('\n').find(line => line.startsWith('{'))
    requireProof(result && JSON.parse(result).recovered === 1, 'CLAIM_CHANGED_NO_REPAIR_APPLIED')
  }
  console.log(JSON.stringify({ ...proof, status: mode === '--apply' ? 'recovered_retry_available' : 'verified_no_changes' }))
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main(process.argv.slice(2)).catch(error => {
    // All operational failures are bounded codes, never raw RPC/SQL or signed bytes.
    console.error(/^[A-Z_]+$/.test(error.message) ? error.message : 'FAUCET_RECOVERY_FAILED')
    process.exitCode = 1
  })
}
