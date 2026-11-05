// Operator-only repair. Retains every prior funding phase and signed intent.
import { createRequire } from 'node:module'
import { existsSync } from 'node:fs'
import { execFileSync } from 'node:child_process'
import { pathToFileURL } from 'node:url'
import { postgresEnvironment, rpcProvider } from './recover-faucet-nonce.mjs'
const require = createRequire(process.env.FAUCET_NODE_PACKAGE ?? (existsSync('/app/oracle/package.json') ? '/app/oracle/package.json' : new URL('../../frontend/package.json', import.meta.url)))
const { keccak256, parseTransaction, recoverTransactionAddress, encodeFunctionData, parseAbi } = require('viem')
const eq = (a,b) => typeof a === 'string' && typeof b === 'string' && a.toLowerCase() === b.toLowerCase()
const hash = v => typeof v === 'string' && /^0x[0-9a-f]{64}$/i.test(v)
const address = v => typeof v === 'string' && /^0x[0-9a-f]{40}$/i.test(v)
const quantity = v => typeof v === 'string' && /^0x(?:0|[1-9a-f][0-9a-f]*)$/i.test(v)
const proof = (value, code) => { if (!value) throw new Error(code) }
const token = '0x3d051dbbf5ff427a4ae8ad09746902666af8fb9d'
const clearinghouse = '0xb5b851f627d6df3cf3f096e23e20a58884d7b89a'
const chainId = 421614
const literal = v => `'${String(v).replaceAll("'", "''")}'`

export async function verifyMarginNonceConflict(row, replacementHash, providers) {
  proof(row.tx_status === 'submitted' && ['pending','recovery'].includes(row.status) && hash(row.tx_hash) && hash(replacementHash) && !eq(row.tx_hash,replacementHash), 'INTENT_NOT_SUBMITTED')
  proof(address(row.address) && address(row.signer) && eq(row.token_address,token) && eq(row.clearinghouse,clearinghouse) && Number(row.chain_id) === chainId && row.release_id === 'v2.0.0' && BigInt(row.amount) === 100_000_000_000n, 'INVALID_FUNDING_SCOPE')
  proof(['mint','approval','deposit'].includes(row.phase) && typeof row.raw_tx === 'string' && /^0x02[0-9a-f]+$/i.test(row.raw_tx) && row.raw_tx.length <= 4096 && eq(keccak256(row.raw_tx),row.tx_hash), 'INVALID_SIGNED_INTENT')
  const tx = parseTransaction(row.raw_tx)
  const sender = await recoverTransactionAddress({serializedTransaction:row.raw_tx})
  const fn = {mint:'mint',approval:'approve',deposit:'depositFor'}[row.phase]
  const recipient = {mint:row.signer,approval:clearinghouse,deposit:row.address}[row.phase]
  const target = row.phase === 'deposit' ? clearinghouse : token
  const calldata = encodeFunctionData({abi:parseAbi([`function ${fn}(address,uint256)`]),functionName:fn,args:[recipient,BigInt(row.amount)]})
  proof(tx.type === 'eip1559' && tx.chainId === chainId && eq(sender,row.signer) && eq(tx.to,target) && eq(row.target,target) && eq(tx.data,calldata) && eq(row.calldata,calldata) && (tx.value ?? 0n) === 0n && Number.isSafeInteger(tx.nonce) && tx.nonce >= 0 && BigInt(tx.nonce) === BigInt(row.nonce), 'SIGNED_INTENT_MISMATCH')
  proof(providers.length === 2, 'TWO_PROVIDERS_REQUIRED')
  const results = await Promise.all(providers.map(async rpc => {
    proof(BigInt(await rpc('eth_chainId',[])) === BigInt(chainId),'WRONG_CHAIN')
    const [original,replacement,receipt,safe] = await Promise.all([
      rpc('eth_getTransactionReceipt',[row.tx_hash]),rpc('eth_getTransactionByHash',[replacementHash]),
      rpc('eth_getTransactionReceipt',[replacementHash]),rpc('eth_getBlockByNumber',['safe',false]),
    ])
    proof(original === null,'ORIGINAL_RECEIPT_EXISTS')
    proof(replacement && receipt && safe && hash(safe.hash) && quantity(safe.number),'MISSING_CHAIN_EVIDENCE')
    proof(eq(replacement.hash,replacementHash) && eq(replacement.from,sender) && quantity(replacement.nonce) && BigInt(replacement.nonce) === BigInt(tx.nonce),'SENDER_NONCE_MISMATCH')
    proof(eq(receipt.transactionHash,replacementHash) && eq(receipt.from,sender) && hash(receipt.blockHash) && quantity(receipt.blockNumber) && eq(receipt.blockHash,replacement.blockHash) && receipt.blockNumber === replacement.blockNumber && ['0x0','0x1'].includes(receipt.status),'RECEIPT_MISMATCH')
    proof(BigInt(receipt.blockNumber) <= BigInt(safe.number),'REPLACEMENT_NOT_SAFE')
    const canonical = await rpc('eth_getBlockByNumber',[receipt.blockNumber,false])
    proof(canonical && eq(canonical.hash,receipt.blockHash) && canonical.number === receipt.blockNumber && Array.isArray(canonical.transactions) && canonical.transactions.some(h => eq(h,replacementHash)),'NON_CANONICAL_REPLACEMENT')
    // Only reverted replacements or empty self-cancellations prove that no
    // mint, approval or margin credit could have fulfilled the original phase.
    // Arbitrary successful calls stay blocked for separate operator review.
    proof(Array.isArray(receipt.logs) && receipt.logs.length === 0 &&
      (receipt.status === '0x0' || (eq(replacement.to,sender) && replacement.value === '0x0' && replacement.input === '0x')), 'REPLACEMENT_MAY_FULFILL_INTENT')
    return {replacementHash:replacementHash.toLowerCase(),blockNumber:BigInt(receipt.blockNumber).toString(),blockHash:receipt.blockHash.toLowerCase()}
  }))
  proof(JSON.stringify(results[0]) === JSON.stringify(results[1]),'PROVIDERS_DISAGREE')
  return {originalHash:row.tx_hash.toLowerCase(),claimId:row.claim_id,phase:row.phase,signer:sender.toLowerCase(),nonce:tx.nonce,...results[0]}
}

export function marginRecoverySql(row, verified) {
  return `BEGIN;
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15s';
SELECT pg_advisory_xact_lock(4216140100000001);
WITH changed AS (
 UPDATE testnet_margin_funding_transactions t SET status='replaced'
 FROM testnet_margin_funding c
 WHERE c.id=t.claim_id AND c.id=${literal(row.claim_id)}::uuid AND c.phase=${literal(row.phase)}
 AND c.status IN ('pending','recovery') AND t.status='submitted' AND t.phase=c.phase
 AND c.signer=${literal(row.signer)} AND c.address=${literal(row.address)} AND c.token_address=${literal(row.token_address)}
 AND c.clearinghouse=${literal(row.clearinghouse)} AND c.chain_id=421614 AND c.release_id=${literal(row.release_id)} AND c.amount=100000000000
 AND t.tx_hash=${literal(row.tx_hash)} AND t.raw_tx=${literal(row.raw_tx)}
 AND t.signer=${literal(row.signer)} AND t.nonce=${literal(row.nonce)}
 RETURNING t.claim_id,t.phase,t.signer,t.nonce,t.tx_hash
), audit AS (
 INSERT INTO testnet_margin_funding_nonce_recoveries(original_tx_hash,claim_id,phase,signer,nonce,replacement_tx_hash,replacement_block_number,replacement_block_hash)
 SELECT tx_hash,claim_id,phase,signer,nonce,${literal(verified.replacementHash)},${literal(verified.blockNumber)},${literal(verified.blockHash)} FROM changed
 RETURNING claim_id
), reset AS (
 UPDATE testnet_margin_funding SET status='failed',reason='VERIFIED_NONCE_CONFLICT',updated_at=NOW()
 WHERE id IN (SELECT claim_id FROM audit) RETURNING id
)
SELECT json_build_object('recovered',(SELECT COUNT(*) FROM reset));
COMMIT;`
}
function sql(query) {
  try { return execFileSync('psql',['-X','-qAt','-v','ON_ERROR_STOP=1'],{input:query,encoding:'utf8',stdio:['pipe','pipe','pipe'],env:postgresEnvironment(process.env.DATABASE_URL),timeout:25_000}).trim() }
  catch { throw new Error('DATABASE_OPERATION_FAILED') }
}
export async function main(args) {
  const [original,replacement,mode] = args
  proof(args.length === 3 && hash(original) && hash(replacement) && ['--check','--apply'].includes(mode),'INVALID_ARGUMENTS')
  proof(process.env.DATABASE_URL && process.env.PERPS_RPC_URL,'DATABASE_AND_PRIMARY_RPC_REQUIRED')
  const secondary = 'https://sepolia-rollup.arbitrum.io/rpc'
  proof(new URL(process.env.PERPS_RPC_URL).hostname !== new URL(secondary).hostname,'INDEPENDENT_PRIMARY_RPC_REQUIRED')
  const rows = JSON.parse(sql(`SELECT COALESCE(json_agg(r),'[]'::json) FROM (
    SELECT c.id::text claim_id,c.address,c.token_address,c.clearinghouse,c.chain_id,c.release_id,c.amount::text,c.phase,c.status,
    t.signer,t.nonce::text,t.target,t.calldata,t.tx_hash,t.raw_tx,t.status tx_status
    FROM testnet_margin_funding c JOIN testnet_margin_funding_transactions t ON t.claim_id=c.id WHERE t.tx_hash=${literal(original.toLowerCase())}
  ) r;`))
  proof(rows.length === 1,'EXACTLY_ONE_INTENT_REQUIRED')
  const row = rows[0]
  if (row.tx_status === 'replaced') {
    const result = JSON.parse(sql(`SELECT json_build_object('recovered',COUNT(*)) FROM testnet_margin_funding_nonce_recoveries WHERE original_tx_hash=${literal(original.toLowerCase())} AND replacement_tx_hash=${literal(replacement.toLowerCase())};`))
    proof(result.recovered === 1,'RECOVERY_PROOF_MISMATCH')
    console.log(JSON.stringify({originalHash:original,status:'already_recovered'})); return
  }
  const verified = await verifyMarginNonceConflict(row,replacement,[rpcProvider(process.env.PERPS_RPC_URL,process.env.PERPS_RPC_AUTH_TOKEN),rpcProvider(secondary)])
  if (mode === '--apply') {
    const result = sql(marginRecoverySql(row,verified)).split('\n').find(line => line.startsWith('{'))
    proof(result && JSON.parse(result).recovered === 1,'INTENT_CHANGED_NO_REPAIR_APPLIED')
  }
  console.log(JSON.stringify({...verified,status:mode === '--apply' ? 'recovered_retry_available' : 'verified_no_changes'}))
}
if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) main(process.argv.slice(2)).catch(error => {
  console.error(/^[A-Z_]+$/.test(error.message) ? error.message : 'MARGIN_RECOVERY_FAILED'); process.exitCode=1
})
