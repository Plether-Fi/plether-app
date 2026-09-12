import assert from 'node:assert/strict'
import { before, after, test } from 'node:test'
import fs from 'node:fs'
import net from 'node:net'
import { spawn } from 'node:child_process'
import { createRequire } from 'node:module'
const require = createRequire(new URL('../apps/frontend/package.json', import.meta.url))
const { createPublicClient, createWalletClient, http, parseAbi, encodeFunctionData, decodeFunctionData, decodeEventLog, decodeErrorResult, decodeAbiParameters, concatHex, toHex, padHex, keccak256, parseEther } = require('viem')
const { privateKeyToAccount } = require('viem/accounts')
const { entryPoint08Abi } = require('viem/account-abstraction')
const fixture = JSON.parse(fs.readFileSync(new URL('./fixtures/aa-deposit-gas-20260912.json', import.meta.url)))
const manifest = JSON.parse(fs.readFileSync(new URL('../config/perps/arbitrum-sepolia-v2.json', import.meta.url)))
const paymaster = '0x9761091045616A388f5fE1433721B272c78fe31b'
// Public deterministic LOCAL TEST keys, never production keys.
const owner = privateKeyToAccount(toHex(1n, { size: 32 }))
const signer = privateKeyToAccount(toHex(2n, { size: 32 }))
let child, client, wallet, baseline
const rpc = (method, params = []) => client.request({ method, params })
const tuple = entryPoint08Abi.find(f => f.name === 'getUserOpHash').inputs[0]
const pmAbi = [
  { type: 'function', name: 'getSponsorshipHash', stateMutability: 'view', inputs: [tuple], outputs: [{ type: 'bytes32' }] },
  ...parseAbi(['function policyId() view returns(bytes32)', 'function sponsorSigner() view returns(address)']),
]
const accountAbi = parseAbi(['function owner() view returns(address)', 'function executeBatch((address target,uint256 value,bytes data)[] calls)'])
const tokenAbi = parseAbi(['function balanceOf(address) view returns(uint256)', 'function approve(address,uint256) returns(bool)'])
const depositAbi = parseAbi(['function depositMargin(uint256)'])
const gasPolicy = estimate => (estimate * 3n + 1n) / 2n > estimate + 100000n ? (estimate * 3n + 1n) / 2n : estimate + 100000n

before(async () => {
  assert.deepEqual(fixture.scenarios.map(s=>s.name),['open_commit','close_commit','protection_create'])
  assert.deepEqual(fixture.scenarios.map(s=>s.expectedSuccess),[false,true,false], 'fixture regeneration cannot silently redefine expected execution outcomes')
  assert.equal(fixture.accounts[fixture.entryPoint.toLowerCase()].codeHash,'0xe3f30f78ae55058acdefea00952c8e44f2263215cf720fe1b27b6f148add0278')
  assert.equal(fixture.accounts[paymaster.toLowerCase()].codeHash,'0xb8ae276b01850fdbb8d9d7fd32ec7b9b1c7ab7af20f5d62179a76f6b4912c528')
  const server = net.createServer()
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
  const port = server.address().port
  await new Promise(resolve => server.close(resolve))
  child = spawn('anvil', ['--host','127.0.0.1','--port',String(port),'--chain-id','421614','--timestamp',String(fixture.timestamp),'--base-fee','0','--gas-limit','100000000','--silent'], { stdio: 'ignore' })
  let startupError
  child.on('error', error => { startupError = error })
  child.on('exit', code => { startupError = Error(`Isolated Anvil exited (${code})`) })
  client = createPublicClient({ transport: http(`http://127.0.0.1:${port}`, { retryCount: 0, timeout: 5000 }) })
  wallet = createWalletClient({ account: owner, transport: http(`http://127.0.0.1:${port}`, { retryCount: 0, timeout: 5000 }) })
  for (let i = 0; i < 100; i++) {
    if (startupError) throw startupError
    try { if (await client.getChainId() === 421614) break } catch { /* local startup only */ }
    if (i === 99) throw Error('Anvil startup failed; test must not silently skip')
    await new Promise(resolve => setTimeout(resolve, 50))
  }
  for (const [address, state] of Object.entries(fixture.accounts)) {
    assert.equal(keccak256(state.code), state.codeHash)
    const core = Object.values(manifest.contracts).find(c => c.address.toLowerCase() === address)
    if (core) assert.equal(state.codeHash, core.runtimeCodeHash)
    await rpc('anvil_setCode', [address,state.code])
    await rpc('anvil_setBalance', [address,state.balance])
    await rpc('anvil_setNonce', [address,state.nonce])
    for (const [slot,value] of Object.entries(state.storage)) await rpc('anvil_setStorageAt', [address,slot,value])
  }
  // Only local test ownership is substituted; all Core contracts and recorded
  // margin/position/carry state remain unchanged. Never fork/send to live RPC.
  await rpc('anvil_setStorageAt', [fixture.sender,toHex(0n,{size:32}),padHex(owner.address,{size:32})])
  await rpc('anvil_setStorageAt', [paymaster,toHex(4n,{size:32}),padHex(signer.address,{size:32})])
  await rpc('anvil_setBalance', [owner.address,toHex(parseEther('10'))])
  assert.equal((await client.readContract({address:fixture.sender,abi:accountAbi,functionName:'owner'})).toLowerCase(),owner.address.toLowerCase())
  assert.equal((await client.readContract({address:paymaster,abi:pmAbi,functionName:'sponsorSigner'})).toLowerCase(),signer.address.toLowerCase())
  baseline = await rpc('evm_snapshot')
})
after(() => { child?.kill() })

async function reset() {
  assert.equal(await rpc('evm_revert',[baseline]),true)
  baseline = await rpc('evm_snapshot')
}
async function makeOperation(callGas, callData, sender, initCode, nonce) {
  const policy = await client.readContract({address:paymaster,abi:pmAbi,functionName:'policyId'})
  const block = await client.getBlock()
  const envelope = concatHex([paymaster,toHex(100000n,{size:16}),toHex(0n,{size:16}),toHex(block.timestamp+120n,{size:6}),toHex(block.timestamp-30n,{size:6}),toHex(parseEther('0.01'),{size:16}),policy,fixture.accounts[fixture.sender.toLowerCase()].codeHash])
  const op = {sender,nonce,initCode,callData,accountGasLimits:concatHex([toHex(initCode==='0x'?100000n:450000n,{size:16}),toHex(callGas,{size:16})]),preVerificationGas:55092n,gasFees:concatHex([toHex(1n,{size:16}),toHex(1000000000n,{size:16})]),paymasterAndData:concatHex([envelope,toHex(0n,{size:65})]),signature:'0x'}
  const digest = await client.readContract({address:paymaster,abi:pmAbi,functionName:'getSponsorshipHash',args:[op]})
  op.paymasterAndData = concatHex([envelope,await signer.sign({hash:digest})])
  const hash = await client.readContract({address:fixture.entryPoint,abi:entryPoint08Abi,functionName:'getUserOpHash',args:[op]})
  op.signature = await owner.sign({hash})
  return {op,hash}
}
async function execute(callGas, callData = fixture.callData, {sender=fixture.sender,initCode='0x',precedingCallData,beforeSubmit} = {}) {
  const nonce = await client.readContract({address:fixture.entryPoint,abi:entryPoint08Abi,functionName:'getNonce',args:[sender,0n]})
  const preceding = precedingCallData ? await makeOperation(callGas,precedingCallData,sender,initCode,nonce) : null
  const {op,hash} = await makeOperation(callGas,callData,sender,preceding?'0x':initCode,nonce+(preceding?1n:0n))
  if (beforeSubmit) await beforeSubmit()
  const before = await client.readContract({address:fixture.entryPoint,abi:entryPoint08Abi,functionName:'balanceOf',args:[paymaster]})
  const txHash = await wallet.sendTransaction({chain:null,to:fixture.entryPoint,data:encodeFunctionData({abi:entryPoint08Abi,functionName:'handleOps',args:[preceding?[preceding.op,op]:[op],owner.address]}),gas:5000000n,maxFeePerGas:1000000000n,maxPriorityFeePerGas:1n})
  const receipt = await client.waitForTransactionReceipt({hash:txHash})
  if (receipt.status !== 'success') {
    const trace = await rpc('debug_traceTransaction',[txHash,{tracer:'callTracer'}])
    const error = trace.output ? decodeErrorResult({abi:entryPoint08Abi,data:trace.output}) : {error:trace.error,calls:trace.calls?.map(c=>({to:c.to,error:c.error}))}
    assert.fail(JSON.stringify(error,(_,v)=>typeof v==='bigint'?String(v):v))
  }
  assert.equal(receipt.status,'success','EntryPoint bundle itself must succeed')
  const events = receipt.logs.flatMap(log => { try { return [decodeEventLog({abi:entryPoint08Abi,data:log.data,topics:log.topics})] } catch { return [] } })
  const event = events.find(e=>e.eventName==='UserOperationEvent'&&e.args.userOpHash===hash)
  assert.ok(event,'must assert UserOperation success, not just bundle inclusion')
  const remaining = await client.readContract({address:fixture.entryPoint,abi:entryPoint08Abi,functionName:'balanceOf',args:[paymaster]})
  const operationEvents = events.filter(e=>e.eventName==='UserOperationEvent')
  assert.equal(before-remaining,operationEvents.reduce((sum,e)=>sum+e.args.actualGasCost,0n),'paymaster accounting matches actual receipt costs')
  if (preceding) assert.equal(operationEvents.find(e=>e.args.userOpHash===preceding.hash)?.args.success,true)
  return {...event.args,revertReason:events.find(e=>e.eventName==='UserOperationRevertReason'&&e.args.userOpHash===hash)?.args.revertReason}
}

test('historical deposit executes through real account, paymaster, EntryPoint and Core bytecode', async () => {
  await reset()
  const original = await execute(419155n)
  assert.equal(original.success,false,'original underfunded operation must reproduce failure')
  await reset()
  const padded = await execute(gasPolicy(419155n))
  assert.equal(padded.success,true,'padded operation must actually execute')
  assert.ok(padded.actualGasCost>0n)
})

test('repeat deposit succeeds without applying gas headroom twice', async () => {
  await reset()
  assert.equal((await execute(gasPolicy(419155n))).success,true)
  assert.equal((await execute(gasPolicy(419155n))).success,true)
})

test('a queued operation warms storage without invalidating the padded deposit', async () => {
  await reset()
  assert.equal((await execute(gasPolicy(419155n),fixture.callData,{precedingCallData:depositCalls(1n)})).success,true)
})

test('extra unused execution allowance increases the real EntryPoint charge', async () => {
  await reset()
  const padded = await execute(gasPolicy(419155n))
  await reset()
  const excess = await execute(1_200_000n)
  assert.equal(padded.success,true)
  assert.equal(excess.success,true)
  // Same bytecode, storage, fees and action: extra gas is not free. EntryPoint
  // actualGasUsed includes its unused-gas penalty, not just EVM work performed.
  assert.ok(excess.actualGasUsed>padded.actualGasUsed)
  assert.ok(excess.actualGasCost>padded.actualGasCost)
})

test('token state changed after signing remains a genuine failure, not an automatic retry', async () => {
  await reset()
  const token = manifest.contracts.mockUsdc.address
  const beforeSubmit = async () => {
    const balance = await client.readContract({address:token,abi:tokenAbi,functionName:'balanceOf',args:[fixture.sender]})
    const data = encodeFunctionData({abi:accountAbi,functionName:'executeBatch',args:[[{target:token,value:0n,data:encodeFunctionData({abi:parseAbi(['function transfer(address,uint256) returns(bool)']),functionName:'transfer',args:[owner.address,balance]})}]]})
    const tx = await wallet.sendTransaction({chain:null,to:fixture.sender,data,gas:150000n})
    assert.equal((await client.waitForTransactionReceipt({hash:tx})).status,'success')
  }
  assert.equal((await execute(gasPolicy(419155n),fixture.callData,{beforeSubmit})).success,false)
})

test('genuine insufficient-token-balance reverts even with extra gas', async () => {
  await reset()
  const calls = decodeFunctionData({abi:accountAbi,data:fixture.callData}).args[0]
  const amount = 10n**30n
  const changed = [
    {...calls[0],data:encodeFunctionData({abi:tokenAbi,functionName:'approve',args:[calls[1].target,amount]})},
    {...calls[1],data:encodeFunctionData({abi:depositAbi,functionName:'depositMargin',args:[amount]})},
  ]
  assert.equal((await execute(gasPolicy(419155n),encodeFunctionData({abi:accountAbi,functionName:'executeBatch',args:[changed]}))).success,false)
})

function depositCalls(amount) {
  const [approve,deposit] = decodeFunctionData({abi:accountAbi,data:fixture.callData}).args[0]
  return encodeFunctionData({abi:accountAbi,functionName:'executeBatch',args:[[
    {...approve,data:encodeFunctionData({abi:tokenAbi,functionName:'approve',args:[deposit.target,amount]})},
    {...deposit,data:encodeFunctionData({abi:depositAbi,functionName:'depositMargin',args:[amount]})},
  ]]})
}

for (const amount of [1n,1_000_000n,100_000_000n,10_000_000_000n]) {
  test(`deposit ${amount} token atoms with existing position/carry state`, async () => {
    await reset()
    const token = manifest.contracts.usdc?.address ?? '0xf7cbfcc74f2d9eb6fa7dc11941b3bef9fd7f8eb8'
    const before = await client.readContract({address:token,abi:tokenAbi,functionName:'balanceOf',args:[fixture.sender]})
    assert.equal((await execute(gasPolicy(419155n),depositCalls(amount))).success,true)
    const after = await client.readContract({address:token,abi:tokenAbi,functionName:'balanceOf',args:[fixture.sender]})
    assert.equal(before-after,amount,'completed deposit transfers exactly the reviewed amount')
  })
}

test('counterfactual factory creation and first deposit with real account runtime', async () => {
  await reset()
  const factory = '0x13E9ed32155810FDbd067D4522C492D6f68E5944'
  const abi = parseAbi(['function getAddress(address,uint256) view returns(address)','function createAccount(address,uint256) returns(address)'])
  const sender = await client.readContract({address:factory,abi,functionName:'getAddress',args:[owner.address,0n]})
  assert.equal(await client.getCode({address:sender}),undefined)
  const token = '0xf7cbfcc74f2d9eb6fa7dc11941b3bef9fd7f8eb8'
  const mint = await wallet.sendTransaction({chain:null,to:token,data:encodeFunctionData({abi:parseAbi(['function mint(address,uint256)']),functionName:'mint',args:[sender,100_000_000n]}),gas:100000n})
  assert.equal((await client.waitForTransactionReceipt({hash:mint})).status,'success')
  const initCode = concatHex([factory,encodeFunctionData({abi,functionName:'createAccount',args:[owner.address,0n]})])
  assert.equal((await execute(gasPolicy(419155n),depositCalls(100_000_000n),{sender,initCode})).success,true)
  assert.equal(keccak256(await client.getCode({address:sender})),fixture.accounts[fixture.sender.toLowerCase()].codeHash)
  assert.equal((await execute(gasPolicy(419155n),depositCalls(1n),{sender})).success,false,'completed transfer must not repeat when the wallet has no tokens')
})

for (const scenario of fixture.scenarios ?? []) {
  test(`historical ${scenario.name}: ${scenario.expectedSuccess?'execution':'rejection'} parity, not just inclusion`, async () => {
    await reset()
    const result = await execute(gasPolicy(1_000_000n),scenario.callData)
    assert.equal(result.success,scenario.expectedSuccess,`revert: ${result.revertReason}`)
    if (!scenario.expectedSuccess) {
      assert.equal(result.revertReason?.slice(0,10),'0x5a154675','must contain the account batch execution error')
      const [index,reason] = decodeAbiParameters([{type:'uint256'},{type:'bytes'}],`0x${result.revertReason.slice(10)}`)
      assert.equal(index,2n)
      assert.equal(reason,scenario.name==='open_commit'?'0xc85d2497':'0x2d4b169c','must retain the exact Core rejection, not OOG')
    }
  })
}
