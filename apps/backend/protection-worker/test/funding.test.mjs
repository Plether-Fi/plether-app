import { describe, it } from 'node:test'
import assert from 'node:assert/strict'
import { parseTransaction } from 'viem'
import { privateKeyToAccount } from 'viem/accounts'
import { components, parseMonitors, pendingLiability, classifyReserve, observeFunding, journalLiabilities, transactionLiability } from '../funding.mjs'

const address = '0x1111111111111111111111111111111111111111'
const spec = { component: 'alto', address, gasLimit: '30000', valueWei: '0', feeBufferBps: '2500' }
const monitors = parseMonitors(JSON.stringify([spec]))
const release = { network: { chainId: 421614 }, contracts: Object.fromEntries(['orderRouter','cfdEngine','positionProtectionBook','settlementMonitorLens'].map(k => [k,{ address }])) }
const block = { number: 10n, hash: '0xaaa', timestamp: 1000n }
const emptyDb = { query: async () => ({ rows: [] }) }
function client(overrides = {}) { return {
  getChainId: async () => 421614,
  getBlock: async request => request.blockTag === 'pending' ? { number: 11n, parentHash: block.hash, transactions: [] } : block,
  getGasPrice: async () => 2n, estimateMaxPriorityFeePerGas: async () => 1n,
  getBalance: async () => 10000000n, getTransactionCount: async () => 0,
  ...overrides,
} }
function run(c, extra = {}) { return observeFunding({ client: c, db: emptyDb, release, monitors, now: () => 1000n, ...extra }) }

describe('funding inventory and liabilities', () => {
  it('rejects secrets, unknown fields, duplicates, malformed numbers and unbounded inventories', () => {
    for (const value of [[],[spec,spec],Array(17).fill(spec),[{ ...spec, privateKey: 'secret' }],[{ ...spec, gasLimit: '-1' }],[{ ...spec, valueWei: 0 }],[{ ...spec, feeBufferBps: '100001' }],[{ ...spec, component: 'private' }]]) assert.throws(() => parseMonitors(JSON.stringify(value)))
    assert.equal(monitors[0].gasLimit,30000n)
  })
  it('reserves the most expensive replacement once, and includes prepared future nonces', () => {
    assert.equal(pendingLiability(5n,7n,[{nonce:4n,cost:900n},{nonce:5n,cost:20n},{nonce:5n,cost:30n},{nonce:6n,cost:10n},{nonce:8n,cost:50n}]),90n)
  })
  it('does not invent zero liabilities for missing nonce families, invalid fields or regressed nonce', () => {
    assert.equal(pendingLiability(5n,7n,[{nonce:6n,cost:10n}]),undefined)
    assert.equal(pendingLiability(5n,4n,[]),undefined)
    assert.equal(pendingLiability(0n,0n,[{nonce:1n,cost:-1n}]),undefined)
    assert.equal(pendingLiability(0n,2000n,[]),undefined)
  })
  it('quotes fee plus value, including legacy and EIP-1559 transactions', () => {
    assert.deepEqual(transactionLiability({ nonce: 3, gas: 100n, value: 7n, maxFeePerGas: 4n }),{nonce:3n,cost:407n})
    assert.deepEqual(transactionLiability({ nonce: 3, gas: 100n, value: 7n, gasPrice: 2n }),{nonce:3n,cost:207n})
    assert.throws(() => transactionLiability({ nonce: 3, gas: 100n, value: 7n }))
  })
  it('subtracts liabilities and warns below ten reserves, without confusing upper and lower bounds', () => {
    const input = { balance: 100n, liability: 0n, reserve: 10n, timestamp: 1000n, now: 1000n }
    assert.deepEqual(classifyReserve(input),{state:'ready',reason:'READY'})
    assert.deepEqual(classifyReserve({...input,liability:1n}),{state:'ready',reason:'FUNDING_LOW'})
    assert.deepEqual(classifyReserve({...input,liability:91n}),{state:'unknown',reason:'FUNDING_LOW'})
    assert.deepEqual(classifyReserve({...input,balance:0n}),{state:'blocked',reason:'WORKER_INSUFFICIENT_FUNDS'})
    for (const changes of [{liability:undefined},{timestamp:984n},{timestamp:1003n},{reserve:0n}]) assert.equal(classifyReserve({...input,...changes}).reason,'FUNDING_UNVERIFIED')
  })
})

describe('fixed-block funding observer', () => {
  for (const type of ['legacy','eip1559']) for (const nonce of [0,7]) for (const value of [0n,100n]) {
    it(`reads verified ${type} journal bytes with nonce=${nonce} value=${value} without modifying the journal`, async () => {
      const signer=privateKeyToAccount(`0x${'1'.repeat(64)}`) // disposable public test key
      const raw=await signer.signTransaction({chainId:421614,type,nonce,gas:100000n,
        ...(type==='legacy'?{gasPrice:5n}:{maxFeePerGas:5n,maxPriorityFeePerGas:1n}),to:signer.address,value})
      if (value===0n) assert.equal(parseTransaction(raw).value,undefined)
      const monitor={...monitors[0],component:'protection',address:signer.address.toLowerCase()}
      const db={query:async sql=>{assert.match(sql,/^SELECT raw_transaction/);return {rows:[{raw_transaction:raw}]}}}
      const rows=await journalLiabilities(db,421614,release,monitor)
      assert.deepEqual(rows,[{nonce:BigInt(nonce),cost:500000n+value}])
      const result=await run(client({getTransactionCount:async r=>r.blockTag==='pending'?nonce+1:nonce}),{monitors:[monitor],db})
      assert.equal(result.results[0].reason,'READY')
      assert.equal(result.results[0].liability,500000n+value)
      assert.equal(pendingLiability(BigInt(nonce+1),BigInt(nonce+1),rows),0n)
      for (const changes of [{address},{chainId:1}]) {
        await assert.rejects(journalLiabilities(db,changes.chainId??421614,release,{...monitor,...changes}),/JOURNAL_IDENTITY_MISMATCH/)
      }
    })
  }
  it('never invents zero values for incomplete or invalid RPC transaction evidence', () => {
    const tx={nonce:0,gas:100n,value:0n,maxFeePerGas:5n}
    for (const key of ['nonce','gas','value','maxFeePerGas']) for (const value of [undefined,null,false,'',NaN,-1n]) {
      assert.throws(()=>transactionLiability({...tx,[key]:value}))
    }
  })
  it('observes every configured role without needing a first trade or a private key', async () => {
    const inventory = parseMonitors(JSON.stringify(components.map((component,i)=>({...spec,component,address:`0x${String(i+1).repeat(40)}`}))))
    const result = await run(client(),{monitors:inventory})
    assert.deepEqual(result.results.map(r=>r.component),components)
    assert.ok(result.results.every(r=>r.reason==='READY' && r.liability===0n))
  })
  it('works before the first trade and pins account reads to the verified header', async () => {
    const calls=[]
    const result = await run(client({getBalance:async r => {calls.push(r);return 10000000n},getTransactionCount:async r=>{calls.push(r);return 0}}))
    assert.equal(result.results[0].reason,'READY')
    assert.equal(result.results[0].reserve,90000n)
    assert.equal(calls.filter(c=>c.blockNumber===10n).length,2)
    assert.equal(calls.filter(c=>c.blockTag==='pending').length,1)
  })
  it('accounts for Alto pending bundles and same-nonce journal replacements', async () => {
    const tx={from:address,nonce:0,gas:100n,maxFeePerGas:3n,value:5n}
    const result=await run(client({getTransactionCount:async r=>r.blockTag==='pending'?1:0,
      getBlock:async r=>r.blockTag==='pending'?{number:11n,parentHash:block.hash,transactions:[tx]}:block}))
    assert.equal(result.results[0].liability,305n)
  })
  it('unknown mempool liabilities stay unknown even if the wallet has plenty of ETH', async () => {
    const result=await run(client({getTransactionCount:async r=>r.blockTag==='pending'?1:0}))
    assert.equal(result.results[0].reason,'FUNDING_UNVERIFIED')
    assert.equal(result.results[0].diagnostic,'PENDING_LIABILITY_UNKNOWN')
    const hidden=await run(client({getTransactionCount:async r=>r.blockTag==='pending'?1:0,getBlock:async r=>r.blockTag==='pending'?{parentHash:'other',transactions:[]}:block}))
    assert.equal(hidden.results[0].reason,'FUNDING_UNVERIFIED')
  })
  it('rejects same-height reorgs, regressions, stale/future heads and wrong chains', async () => {
    await assert.rejects(run(client(),{previous:{...block,hash:'different'}}))
    await assert.rejects(run(client(),{previous:{...block,number:11n}}))
    await assert.rejects(run(client({getChainId:async()=>1})))
    await assert.rejects(run(client(),{now:()=>1016n}))
    await assert.rejects(run(client(),{now:()=>997n}))
    await assert.rejects(run(client({getBlock:async r=>r.blockNumber?{...block,hash:'different'}:block})))
  })
  it('isolates a failed signer observation, supports priority fallback and recovery after funding', async () => {
    assert.equal((await run(client({getBalance:async()=>{throw Error('secret')}}))).results[0].reason,'FUNDING_UNVERIFIED')
    assert.equal((await run(client({getBalance:async()=>0n}))).results[0].state,'blocked')
    assert.equal((await run(client({estimateMaxPriorityFeePerGas:async()=>{throw Error('unsupported')}}))).results[0].state,'ready')
  })
  it('starts all independent account reads concurrently without timing thresholds', async () => {
    let n=0, releaseReads
    const barrier=new Promise(resolve=>{releaseReads=resolve})
    const read=async value=>{if(++n===3) releaseReads();await barrier;return value}
    await run(client({getBalance:()=>read(10000000n),getTransactionCount:()=>read(0)}))
    assert.equal(n,3)
  })
  it('reads durable LP and liquidation liabilities with deployment and signer scope', async () => {
    for (const component of ['lp_settlement','liquidation']) {
      let captured
      const rows=await journalLiabilities({query:async(q,args)=>{captured={q,args};return {rows:[{nonce:'5',cost:'91'}]}}},421614,release,{...monitors[0],component})
      assert.deepEqual(rows,[{nonce:5n,cost:91n}])
      assert.deepEqual(captured.args,[421614,address,address])
      assert.match(captured.q,/LIMIT 4097/)
    }
  })
  it('rejects malformed protection journal payloads without publishing signed data', async () => {
    const result=await run(client(),{monitors:[{...monitors[0],component:'protection'}],db:{query:async()=>({rows:[{raw_transaction:'0xsecret'}]})}})
    assert.equal(result.results[0].reason,'FUNDING_UNVERIFIED')
    assert.equal(result.results[0].diagnostic,'JOURNAL_DECODE_FAILED')
    assert.ok(!JSON.stringify(result.results[0],(_,v)=>typeof v==='bigint'?v.toString():v).includes('secret'))
  })
  it('reports a bounded journal read failure without leaking database exceptions', async () => {
    const result=await run(client(),{monitors:[{...monitors[0],component:'protection'}],db:{query:async()=>{throw Error('postgres://private-credential')}}})
    assert.equal(result.results[0].diagnostic,'JOURNAL_READ_FAILED')
    assert.ok(!JSON.stringify(result.results,(_,v)=>typeof v==='bigint'?v.toString():v).includes('private-credential'))
  })
})
