// Capture PUBLIC historical state only. Never reads wallet keys or signs/sends.
// Explicit operator command; not run by CI. The resulting fixture is offline.
import fs from 'node:fs'
import { createRequire } from 'node:module'
const require = createRequire(new URL('../apps/frontend/package.json', import.meta.url))
const { createPublicClient, http, decodeFunctionData, encodeFunctionData, parseAbi, toHex, keccak256 } = require('viem')
const { entryPoint08Abi } = require('viem/account-abstraction')
const hash = '0xa2be35e8ce226cf9c183d5937b9bd873cde7a956b33262c71f3004093362ed16'
try {
  if (!process.env.ARBITRUM_SEPOLIA_RPC_URL || process.argv[2] !== '--capture') throw Error('explicit capture required')
  const c = createPublicClient({ transport: http(process.env.ARBITRUM_SEPOLIA_RPC_URL, { retryCount: 0, timeout: 20_000 }) })
  if (await c.getChainId() !== 421614) throw Error('wrong chain')
  const tx = await c.getTransaction({ hash })
  if (tx.blockNumber !== 308224346n) throw Error('wrong incident')
  const block = await c.getBlock({ blockNumber: tx.blockNumber })
  const [op] = decodeFunctionData({ abi: entryPoint08Abi, data: tx.input }).args[0]
  const original = await c.request({ method: 'debug_traceTransaction', params: [hash, { tracer: 'prestateTracer' }] })
  // Capture the complete successful deposit path too, not just storage accessed
  // before the OOG. State is at the end of the failed transaction's block.
  const expanded = await c.request({ method: 'debug_traceCall', params: [
    { from: tx.to, to: op.sender, data: op.callData, gas: toHex(2_000_000n) }, toHex(tx.blockNumber), { tracer: 'prestateTracer' },
  ] })
  const manifest = JSON.parse(fs.readFileSync(new URL('../config/perps/arbitrum-sepolia-v2.json', import.meta.url)))
  const batchAbi = parseAbi(['function executeBatch((address target,uint256 value,bytes data)[] calls)'])
  const deposit = decodeFunctionData({abi:batchAbi,data:op.callData}).args[0]
  const fundingCalls = [
    {...deposit[0],data:encodeFunctionData({abi:parseAbi(['function approve(address,uint256)']),functionName:'approve',args:[deposit[1].target,10_000_000_000n]})},
    {...deposit[1],data:encodeFunctionData({abi:parseAbi(['function depositMargin(uint256)']),functionName:'depositMargin',args:[10_000_000_000n]})},
  ]
  // Use the app's Core v1.2.3 bounded V2 tuple, NOT the package's legacy five-argument API.
  const orderAbi = parseAbi(['function commitOrder((bytes32 clientOrderId,uint8 side,uint256 sizeDelta,uint256 marginDelta,uint256 targetPrice,bool isClose,(uint64 validUntil,uint8 allowedExecutionModes,bytes32 expectedConfigHash,uint256 maxExecutionBountyUsdc,uint256 maxExecutionNotionalUsdc,uint256 maxGrossAccountDebitUsdc,uint256 maxActionChargeUsdc,uint256 maxExplicitFeesUsdc,uint256 maxPostPositionSize,uint256 minPostSettlementBalanceUsdc,uint256 minPostPositionEquityUsdc,uint32 maxPostLeverageBps) bounds) request) returns(uint64)'])
  const expectedConfigHash = await c.readContract({address:manifest.contracts.orderLifecycleBook.address,abi:parseAbi(['function currentExecutionConfigHash() view returns(bytes32)']),functionName:'currentExecutionConfigHash',blockNumber:tx.blockNumber})
  const position = await c.readContract({address:manifest.contracts.cfdEngine.address,abi:parseAbi(['function positions(address) view returns(uint256,uint256,uint256,uint256,uint8,uint64,int256)']),functionName:'positions',args:[op.sender],blockNumber:tx.blockNumber})
  const request = isClose => ({clientOrderId:keccak256(toHex(isClose?'gas-regression-close':'gas-regression-open')),side:position[4],sizeDelta:isClose?position[0]:1000n*10n**18n,marginDelta:isClose?0n:100_000_000n,targetPrice:100_000_000n,isClose,bounds:{validUntil:block.timestamp+60n,allowedExecutionModes:isClose?7:1,expectedConfigHash,maxExecutionBountyUsdc:1_000_000n,maxExecutionNotionalUsdc:1_000_000_000_000n,maxGrossAccountDebitUsdc:1_000_000_000_000n,maxActionChargeUsdc:1_000_000_000n,maxExplicitFeesUsdc:1_000_000_000n,maxPostPositionSize:10_000_000n*10n**18n,minPostSettlementBalanceUsdc:0n,minPostPositionEquityUsdc:0n,maxPostLeverageBps:100000}})
  const candidates = [
    ['open_commit',manifest.contracts.orderRouter.address,encodeFunctionData({abi:orderAbi,functionName:'commitOrder',args:[request(false)]})],
    ['close_commit',manifest.contracts.orderRouter.address,encodeFunctionData({abi:orderAbi,functionName:'commitOrder',args:[request(true)]})],
    ['protection_create',manifest.contracts.positionProtectionBook.address,encodeFunctionData({abi:parseAbi(['function createPositionProtection((uint256 takeProfitTriggerPrice,uint256 stopLossTriggerPrice) params)']),functionName:'createPositionProtection',args:[{takeProfitTriggerPrice:150_000_000n,stopLossTriggerPrice:50_000_000n}]})],
  ]
  const scenarios = []
  for (const [name,target,data] of candidates) {
    const callData = encodeFunctionData({abi:batchAbi,functionName:'executeBatch',args:[[...fundingCalls,{target,value:0n,data}]]})
    const call = {from:tx.to,to:op.sender,data:callData,gas:toHex(2_000_000n)}
    const state = await c.request({method:'debug_traceCall',params:[call,toHex(tx.blockNumber),{tracer:'prestateTracer'}]})
    const trace = await c.request({method:'debug_traceCall',params:[call,toHex(tx.blockNumber),{tracer:'callTracer'}]})
    scenarios.push({name,callData,expectedSuccess:!trace.error})
    for (const [address,account] of Object.entries(state)) expanded[address] = {...account,storage:{...expanded[address]?.storage,...account.storage}}
  }
  // EntryPoint's immutable SenderCreator is not touched by an already-deployed
  // account; include its real code for counterfactual-account regression tests.
  const addresses = [...new Set([...Object.keys(original), ...Object.keys(expanded), '0x449ed7c3e6fee6a97311d4b55475df59c44add33'])].sort()
  const accounts = {}
  for (const address of addresses) {
    if (!/^0x[0-9a-fA-F]{40}$/.test(address)) throw Error('invalid account')
    const keys = [...new Set([...Object.keys(original[address]?.storage ?? {}), ...Object.keys(expanded[address]?.storage ?? {})])].sort()
    const [code, balance, nonce, values] = await Promise.all([
      c.getCode({ address, blockNumber: tx.blockNumber }), c.getBalance({ address, blockNumber: tx.blockNumber }),
      c.getTransactionCount({ address, blockNumber: tx.blockNumber }),
      Promise.all(keys.map(slot => c.getStorageAt({ address, slot, blockNumber: tx.blockNumber }))),
    ])
    accounts[address.toLowerCase()] = { code: code ?? '0x', codeHash: keccak256(code ?? '0x'), balance: toHex(balance), nonce: toHex(nonce), storage: Object.fromEntries(keys.map((key, i) => [key, values[i]])) }
  }
  if ((await c.getBlock({ blockNumber: tx.blockNumber })).hash !== block.hash) throw Error('reorg')
  const fixture = {
    version: 1, chainId: 421614, transaction: hash, block: String(tx.blockNumber), blockHash: block.hash,
    timestamp: Number(block.timestamp), baseFeePerGas: toHex(block.baseFeePerGas),
    entryPoint: tx.to, sender: op.sender, callData: op.callData,
    originalCallGas: '419155', paddedCallGas: '628733',
    note: 'Public touched-slot end-of-block snapshot, expanded for successful deposit execution. No owner or paymaster signatures. Not a complete chain snapshot.',
    accounts, scenarios,
  }
  const out = new URL('./fixtures/aa-deposit-gas-20260912.json', import.meta.url)
  fs.writeFileSync(out, JSON.stringify(fixture, null, 2) + '\n', { flag: process.argv.includes('--refresh') ? 'w' : 'wx' })
  console.log(JSON.stringify({ accounts: addresses.length, fixture: out.pathname }))
} catch { console.error('Public regression capture failed; no credentials or provider messages printed.'); process.exitCode = 1 }
