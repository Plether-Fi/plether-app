import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { readFileSync } from 'node:fs'
import { execFileSync } from 'node:child_process'
import { verifyMarginNonceConflict, marginRecoverySql } from './recover-margin-funding-nonce.mjs'
import { postgresEnvironment } from './recover-faucet-nonce.mjs'
const require = createRequire(process.env.FAUCET_NODE_PACKAGE ?? new URL('../../frontend/package.json',import.meta.url))
const { keccak256, encodeFunctionData, parseAbi } = require('viem')
const { privateKeyToAccount } = require('viem/accounts')
const account = privateKeyToAccount(`0x${'1'.repeat(64)}`) // Public test fixture.
const token='0x3d051dbbf5ff427a4ae8ad09746902666af8fb9d', clearinghouse='0xb5b851f627d6df3cf3f096e23e20a58884d7b89a'
const beneficiary=`0x${'2'.repeat(40)}`, replacement=`0x${'a'.repeat(64)}`, blockHash=`0x${'b'.repeat(64)}`
async function fixture(phase='mint') {
  const fn={mint:'mint',approval:'approve',deposit:'depositFor'}[phase]
  const recipient={mint:account.address,approval:clearinghouse,deposit:beneficiary}[phase]
  const target=phase==='deposit' ? clearinghouse : token
  const calldata=encodeFunctionData({abi:parseAbi([`function ${fn}(address,uint256)`]),functionName:fn,args:[recipient,100_000_000_000n]})
  const raw=await account.signTransaction({chainId:421614,nonce:12,type:'eip1559',to:target,data:calldata,gas:100000n,maxFeePerGas:1n,maxPriorityFeePerGas:0n,value:0n})
  return {claim_id:'a6415ba2-5668-4ba5-a058-f627bd1b938d',address:beneficiary,token_address:token,clearinghouse,chain_id:421614,release_id:'v2.0.0',amount:'100000000000',phase,status:'recovery',signer:account.address.toLowerCase(),nonce:'12',target,calldata,tx_hash:keccak256(raw),raw_tx:raw,tx_status:'submitted'}
}
function provider(overrides={}) { return async (method,params) => {
  const values={eth_chainId:'0x66eee',eth_getTransactionReceipt:params[0]===replacement ? {transactionHash:replacement,from:account.address,blockHash,blockNumber:'0x64',status:'0x1',logs:[]} : null,
    eth_getTransactionByHash:{hash:replacement,from:account.address,to:account.address,nonce:'0xc',blockHash,blockNumber:'0x64',value:'0x0',input:'0x'},
    eth_getBlockByNumber:{hash:blockHash,number:params[0]==='safe' ? '0x65' : '0x64',transactions:[replacement]}}
  const value=structuredClone(values[method]); return overrides[method] ? overrides[method](value,params) : value
} }
for (const phase of ['mint','approval','deposit']) test(`verifies ${phase} cancellation without losing prior phases`,async () => {
  const row=await fixture(phase), verified=await verifyMarginNonceConflict(row,replacement,[provider(),provider()])
  assert.equal(verified.phase,phase); assert.equal(verified.nonce,12)
})
for (const [name,overrides,code] of [
  ['wrong chain',{eth_chainId:()=> '0x1'},'WRONG_CHAIN'],
  ['original exists',{eth_getTransactionReceipt:(v,p)=>p[0]===replacement?v:{}},'ORIGINAL_RECEIPT_EXISTS'],
  ['wrong nonce',{eth_getTransactionByHash:v=>({...v,nonce:'0xd'})},'SENDER_NONCE_MISMATCH'],
  ['unsafe block',{eth_getBlockByNumber:(v,p)=>({...v,number:p[0]==='safe'?'0x63':v.number})},'REPLACEMENT_NOT_SAFE'],
  ['reorg',{eth_getBlockByNumber:(v,p)=>({...v,hash:p[0]==='safe'?v.hash:replacement})},'NON_CANONICAL_REPLACEMENT'],
  ['successful arbitrary call',{eth_getTransactionByHash:v=>({...v,input:'0x12345678'})},'REPLACEMENT_MAY_FULFILL_INTENT'],
  ['fulfilling logs',{eth_getTransactionReceipt:v=>v&&({...v,logs:[{}]})},'REPLACEMENT_MAY_FULFILL_INTENT'],
]) test(`fails closed: ${name}`,async()=>assert.rejects(verifyMarginNonceConflict(await fixture('deposit'),replacement,[provider(),provider(overrides)]),new RegExp(code)))
test('reverted replacement can safely release a nonce',async()=> {
  const rpc=provider({eth_getTransactionByHash:v=>({...v,to:token,input:'0x1234'}),eth_getTransactionReceipt:v=>v&&({...v,status:'0x0'})})
  await verifyMarginNonceConflict(await fixture('approval'),replacement,[rpc,rpc])
})
for (const field of ['phase','signer','nonce','target','calldata']) test(`rejects changed ${field}`,async()=> {
  const row=await fixture(); row[field]=field==='phase'?'deposit':field==='nonce'?'13':beneficiary
  await assert.rejects(verifyMarginNonceConflict(row,replacement,[provider(),provider()]),/SIGNED_INTENT_MISMATCH/)
})
test('database repair is atomic, preserves phase, and fences concurrent finalization',{skip:!process.env.FAUCET_TEST_DATABASE_URL},async()=> {
  const row=await fixture('deposit'), verified=await verifyMarginNonceConflict(row,replacement,[provider(),provider()])
  const sql=query=>execFileSync(process.env.PSQL??'psql',['-X','-qAt','-v','ON_ERROR_STOP=1'],{input:`SET search_path TO margin_recovery_test;\n${query}`,encoding:'utf8',stdio:['pipe','pipe','pipe'],env:postgresEnvironment(process.env.FAUCET_TEST_DATABASE_URL)}).trim()
  sql(`CREATE SCHEMA margin_recovery_test; CREATE TABLE testnet_faucet_claims(address VARCHAR(42),token_address VARCHAR(42),PRIMARY KEY(address,token_address));
    ${readFileSync(new URL('../config/migrations/testnet-margin-funding-v1.sql',import.meta.url),'utf8')}`)
  try {
    sql(`INSERT INTO testnet_faucet_claims(address,token_address) VALUES ('${beneficiary}','${token}');
      INSERT INTO testnet_margin_funding(id,owner_address,address,token_address,chain_id,clearinghouse,signer,release_id,amount,phase,status)
      VALUES ('${row.claim_id}','${beneficiary}','${beneficiary}','${token}',421614,'${clearinghouse}','${row.signer}','v2.0.0',100000000000,'deposit','recovery');
      INSERT INTO testnet_margin_funding_transactions(claim_id,phase,attempt,signer,nonce,target,calldata,tx_hash,raw_tx)
      VALUES ('${row.claim_id}','deposit',1,'${row.signer}',12,'${row.target}','${row.calldata}','${row.tx_hash}','${row.raw_tx}');`)
    assert.match(sql(marginRecoverySql(row,verified)),/"recovered" : 1/)
    assert.equal(sql('SELECT phase,status FROM testnet_margin_funding;'),'deposit|failed')
    assert.equal(sql('SELECT status FROM testnet_margin_funding_transactions;'),'replaced')
    assert.match(sql(marginRecoverySql(row,verified)),/"recovered" : 0/)
    sql("DELETE FROM testnet_margin_funding_nonce_recoveries; UPDATE testnet_margin_funding SET status='recovery'; UPDATE testnet_margin_funding_transactions SET status='submitted';")
    // Audit failure must roll back both the nonce and parent state change.
    sql('ALTER TABLE testnet_margin_funding_nonce_recoveries ADD CONSTRAINT deny_audit CHECK(false);')
    assert.throws(()=>sql(marginRecoverySql(row,verified)))
    assert.equal(sql('SELECT status FROM testnet_margin_funding_transactions;'),'submitted')
    assert.equal(sql('SELECT status FROM testnet_margin_funding;'),'recovery')
    sql("ALTER TABLE testnet_margin_funding_nonce_recoveries DROP CONSTRAINT deny_audit; UPDATE testnet_margin_funding_transactions SET status='confirmed'; UPDATE testnet_margin_funding SET status='funded';")
    assert.match(sql(marginRecoverySql(row,verified)),/"recovered" : 0/)
  } finally { sql('DROP SCHEMA margin_recovery_test CASCADE;') }
})
