import fs from 'node:fs'
import path from 'node:path'
import os from 'node:os'
import { execFileSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { createServer } from 'vite'
import react from '@vitejs/plugin-react-swc'
import { createPublicClient, createWalletClient, http, parseUnits, toHex, keccak256 } from 'viem'
import { arbitrumSepolia } from 'viem/chains'

// Deliberately fixed loopback endpoint. Never accept a remote execution RPC.
const rpcUrl = 'http://127.0.0.1:18545'
const port = 5182
const origin = `http://127.0.0.1:${port}`
const client = createPublicClient({ chain: arbitrumSepolia, transport: http(rpcUrl) })
const wallet = createWalletClient({ chain: arbitrumSepolia, transport: http(rpcUrl) })
const rpc = (method, params = []) => client.request({ method, params })
if (!(await rpc('web3_clientVersion')).toLowerCase().includes('anvil') || await client.getChainId() !== 421614) throw new Error('Expected isolated Anvil fork on port 18545')
const release = JSON.parse(fs.readFileSync('../../config/perps/arbitrum-sepolia-v2.json', 'utf8'))
const manifest = JSON.parse(fs.readFileSync('public/perps-aa-manifest.json', 'utf8'))
const addresses = Object.fromEntries(Object.entries(release.contracts).map(([k, v]) => [k, v.address]))
const owner = release.release.owner
const trader = '0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266'
const keeper = '0x70997970C51812dc3A010C7d01b50e0d17dc79C8'
const bundle = process.env.LOCAL_PERPS_ABI_BUNDLE
if (!bundle) throw new Error('Set LOCAL_PERPS_ABI_BUNDLE to the checksum-verified v1.2.3 release tar.gz')
if (release.release.version !== 'v1.2.3' || createHash('sha256').update(fs.readFileSync(bundle)).digest('hex') !== release.release.bundleSha256) throw new Error('Expected the pinned v1.2.3 ABI bundle')
const loadAbi = name => JSON.parse(execFileSync('tar', ['-xOzf', bundle, `perps-${release.release.version}-arbitrum-sepolia/abi/${name}.json`], { encoding: 'utf8' }))
const abis = { mockUsdc: loadAbi('MockUSDC'), housePool: loadAbi('ArbitrumSepoliaReleaseHousePool'), orderRouter: loadAbi('ArbitrumSepoliaReleaseRouter'), pletherOracle: loadAbi('ArbitrumSepoliaReleaseOracle'), cfdEngine: loadAbi('CfdEngine'), perpsPublicLens: loadAbi('PerpsPublicLens'), marginClearinghouse: loadAbi('MarginClearinghouse'), positionProtectionBook: loadAbi('PositionProtectionBook'), orderLifecycleBook: loadAbi('OrderLifecycleBook') }
const log = []
const note = (action, tx) => { log.unshift({ action, tx, time: new Date().toISOString() }); log.splice(40); console.log(action, tx ?? '') }
const read = (key, functionName, args = []) => client.readContract({ address: addresses[key], abi: abis[key], functionName, args })
async function write(key, functionName, args = [], account = keeper) {
  // Historical execution requires a tick strictly after the commit timestamp.
  // Advance only this synthetic clock so a fast manual retry can execute too.
  if (['executeOrder', 'triggerPositionProtection'].includes(functionName)) { await rpc('evm_increaseTime', [2]); await rpc('evm_mine') }
  const { request, result } = await client.simulateContract({ address: addresses[key], abi: abis[key], functionName, args, account })
  // Router execution intentionally returns Pending on low gas; eth_estimateGas
  // can select that cheap path rather than the full execution path.
  const hash = await wallet.writeContract({ ...request, ...(['executeOrder', 'triggerPositionProtection'].includes(functionName) ? { gas: 15_000_000n } : {}) })
  const receipt = await client.waitForTransactionReceipt({ hash })
  if (receipt.status !== 'success') throw new Error(`${functionName} reverted: ${hash}`)
  if (functionName !== 'updateMarkPrice') note(functionName, hash)
  return result
}

// Verify fork identity before any funding, impersonation or code replacement.
for (const key of ['cfdEngine', 'orderRouter', 'positionProtectionBook', 'pletherOracle', 'housePool']) {
  if (keccak256(await client.getCode({ address: addresses[key] })) !== release.contracts[key].runtimeCodeHash) throw new Error(`Fork deployment mismatch: ${key}`)
}
for (const address of [owner, trader, keeper]) {
  await rpc('anvil_impersonateAccount', [address])
  await rpc('anvil_setBalance', [address, toHex(parseUnits('1000', 18))])
}
await rpc('evm_setAutomine', [true])
const work = fs.mkdtempSync(path.join(os.tmpdir(), 'plether-local-market-'))
execFileSync('forge', ['build', '--root', 'scripts/local-perps/contracts', '--contracts', '.', '--out', `${work}/out`, '--cache-path', `${work}/cache`, '--use', '0.8.35'], { stdio: 'pipe' })
const mock = JSON.parse(fs.readFileSync(`${work}/out/MockPyth.sol/LocalPyth.json`, 'utf8'))
const pyth = release.release.pyth
await rpc('anvil_setCode', [pyth, mock.deployedBytecode.object])
addresses.localPyth = pyth; abis.localPyth = mock.abi
for (let i = 0n; i < 6n; i++) {
  const [id, base, inverse] = await Promise.all(['pythFeedIds', 'basePrices', 'inversions'].map(name => read('pletherOracle', name, [i])))
  await write('localPyth', 'configure', [id, base, inverse])
}
await write('localPyth', 'setTarget', [100_000_000n])
const block = await client.getBlock()
const openDate = new Date(Number(block.timestamp) * 1000)
openDate.setUTCDate(openDate.getUTCDate() + ((8 - openDate.getUTCDay()) % 7 || 7))
openDate.setUTCHours(12, 0, 0, 0)
await rpc('evm_setNextBlockTimestamp', [Math.floor(openDate.getTime() / 1000)])
await rpc('evm_mine')
await write('orderRouter', 'updateMarkPrice', [['0x00']])
await write('mockUsdc', 'mint', [owner, parseUnits('2000000', 6)], owner)
await write('mockUsdc', 'approve', [addresses.housePool, parseUnits('2000000', 6)], owner)
for (const senior of [false, true]) {
  if (!await read('housePool', senior ? 'seniorSeedInitialized' : 'juniorSeedInitialized')) await write('housePool', 'initializeSeedPosition', [senior, parseUnits(senior ? '100000' : '1000000', 6), owner], owner)
}
if (!await read('housePool', 'isTradingActive')) await write('housePool', 'activateTrading', [], owner)
await write('mockUsdc', 'mint', [trader, parseUnits('100000', 6)], owner)
await write('mockUsdc', 'approve', [addresses.marginClearinghouse, parseUnits('100000', 6)], trader)
await write('marginClearinghouse', 'deposit', [trader, parseUnits('100000', 6)], trader)
const startBlock = await client.getBlockNumber()
let resetSnapshot = await rpc('evm_snapshot')
let lastProtectionId = 0n
let autoExecute = true
let queue = Promise.resolve()
const serialize = task => { const next = queue.then(task); queue = next.catch(() => {}); return next }
const encode = data => JSON.stringify(data, (_, v) => typeof v === 'bigint' ? { $bigint: v.toString() } : v)
const decode = text => JSON.parse(text, (_, v) => v && typeof v === 'object' && '$bigint' in v ? BigInt(v.$bigint) : v)
async function state() {
  const [protocol, position, account, activeId, configuration, reservations] = await Promise.all([
    read('perpsPublicLens', 'getProtocolStatus'), read('perpsPublicLens', 'getPosition', [trader]), read('perpsPublicLens', 'getTraderAccount', [trader]), read('positionProtectionBook', 'activePositionProtectionId', [trader]),
    Promise.all(['positionProtectionTriggerBountyUsdc', 'closeOrderExecutionBountyUsdc'].map(n => read('orderRouter', n))), read('orderRouter', 'getAccountReservations', [trader]),
  ])
  if (activeId) lastProtectionId = activeId
  const protection = lastProtectionId ? await read('positionProtectionBook', 'getPositionProtection', [lastProtectionId]) : undefined
  return { protocol, position: { ...position, direction: position.side === 0 ? 'long' : 'short' }, account, trader, protection, cap: 200_000_000n, configuration: { enabled: true, triggerBountyUsdc: configuration[0], executionBountyUsdc: configuration[1] }, pendingOrders: Number(reservations.pendingOrderCount), autoExecute, log, startBlock }
}
async function tick() {
  await write('orderRouter', 'updateMarkPrice', [['0x00']])
  const s = await state()
  const p = s.protection
  if (p?.status === 2) {
    const raw = s.protocol.lastMarkPrice
    const tp = p.takeProfitTriggerPrice && (p.side === 0 ? raw <= p.takeProfitTriggerPrice : raw >= p.takeProfitTriggerPrice)
    const sl = p.stopLossTriggerPrice && (p.side === 0 ? raw >= p.stopLossTriggerPrice : raw <= p.stopLossTriggerPrice)
    if (tp || sl) { await write('positionProtectionBook', 'triggerPositionProtection', [p.protectionId, ['0x00']]); return }
  }
  if (autoExecute) {
    const head = await read('orderRouter', 'accountHeadOrderId', [trader])
    if (head) await write('orderRouter', 'executeOrder', [head, ['0x00']])
  }
}

const vite = await createServer({ configFile: false, plugins: [react(), { name: 'local-market-api', configureServer(server) {
  server.middlewares.use('/local-market/api', async (req, res) => {
    res.setHeader('Content-Type', 'application/json'); res.setHeader('Cache-Control', 'no-store')
    try {
      if (req.headers.host !== `127.0.0.1:${port}`) throw new Error('Use the loopback sandbox origin')
      if (req.method === 'GET' && req.url === '/state') { res.end(encode(await serialize(state))); return }
      if (req.method !== 'POST' || req.headers.origin !== origin || !req.headers['content-type']?.startsWith('application/json')) { res.statusCode = 403; res.end(encode({ error: 'Local same-origin POST required' })); return }
      let body = ''; for await (const chunk of req) { body += chunk; if (body.length > 16_384) throw new Error('Request too large') }
      const input = decode(body)
      await serialize(async () => {
        if (req.url === '/price') { await write('localPyth', 'setTarget', [200_000_000n - parseUnits(String(input.price), 8)]); await write('orderRouter', 'updateMarkPrice', [['0x00']]) }
        else if (req.url === '/execute') { const head = await read('orderRouter', 'accountHeadOrderId', [trader]); if (head) await write('orderRouter', 'executeOrder', [head, ['0x00']]) }
        else if (req.url === '/auto') { autoExecute = Boolean(input.enabled) }
        else if (req.url === '/expire') {
          autoExecute = false
          await rpc('evm_increaseTime', [Number(await read('orderRouter', 'maxOrderAge')) + 1]); await rpc('evm_mine')
          const head = await read('orderRouter', 'accountHeadOrderId', [trader]); if (head) await write('orderRouter', 'executeOrder', [head, ['0x00']])
        } else if (req.url === '/retry') { await write('positionProtectionBook', 'retryPositionProtectionClose', [lastProtectionId]) }
        else if (req.url === '/reset') { await rpc('evm_revert', [resetSnapshot]); resetSnapshot = await rpc('evm_snapshot'); lastProtectionId = 0n; autoExecute = true; log.length = 0 }
        else if (req.url === '/manage') {
          const fn = { create: 'createPositionProtection', replace: 'replacePositionProtection', cancel: 'cancelPositionProtection' }[input.action]
          if (!fn) throw new Error('Unknown management action')
          await write('positionProtectionBook', fn, input.action === 'create' ? [input.params] : input.action === 'replace' ? [input.protectionId, input.params] : [input.protectionId], trader)
        } else if (req.url === '/open') {
          const { preparePerpsOrderV2 } = await vite.ssrLoadModule('/src/contracts/preparePerpsOrderV2.ts')
          const prepared = await preparePerpsOrderV2(client, manifest, { account: trader, direction: input.direction, side: input.direction === 'long' ? 0 : 1, sizeDelta: parseUnits(String(input.size), 18), marginDelta: parseUnits(String(input.margin), 6), slippagePercent: 1, isClose: false, selectedMaxLeverageBps: 50_000, positionProtection: input.params })
          const result = await write('positionProtectionBook', 'commitOpenOrderWithProtection', [prepared.request, input.params], trader)
          lastProtectionId = result[1]
        } else throw new Error('Unknown sandbox action')
      })
      res.end(encode(await state()))
    } catch (error) { console.error(error.shortMessage ?? error.message, error.cause?.raw ?? error.cause?.data ?? ''); res.statusCode = 400; res.end(encode({ error: `${error.shortMessage ?? error.message} ${error.cause?.raw ?? ''}` })) }
  })
} }], server: { host: '127.0.0.1', port, strictPort: true }, define: { 'import.meta.env.VITE_PERPS_POSITION_PROTECTION_ENABLED': '"true"' } })
await vite.listen()
console.log(`Local TP/SL sandbox: ${origin}/local-market.html`)
const interval = setInterval(() => { void serialize(tick).catch(error => note(`Keeper: ${error.shortMessage ?? error.message}`)) }, 4000)
for (const signal of ['SIGINT', 'SIGTERM']) process.on(signal, () => { clearInterval(interval); void vite.close().then(() => process.exit()) })
