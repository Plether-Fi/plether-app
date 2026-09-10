// Contract-backed account-risk checks. Requires the disposable local-perps sandbox;
// every mutation is restricted to its fixed localhost Anvil RPC.
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { execFileSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { createPublicClient, createWalletClient, http, keccak256, parseUnits, toHex } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import { createServer } from 'vite'
import { calculatePriceRisk, findLiquidationThreshold } from '../src/utils/perpsRisk.ts'

const origin = 'http://127.0.0.1:5182'
const client = createPublicClient({ chain: arbitrumSepolia, transport: http('http://127.0.0.1:18545'), cacheTime: 0 })
const wallet = createWalletClient({ chain: arbitrumSepolia, transport: http('http://127.0.0.1:18545') })
const rpc = (method, params = []) => client.request({ method, params })
assert.match(await rpc('web3_clientVersion'), /anvil/i)
assert.equal(await client.getChainId(), 421614)
const release = JSON.parse(fs.readFileSync('../../config/perps/arbitrum-sepolia-v2.json', 'utf8'))
const bundle = process.env.LOCAL_PERPS_ABI_BUNDLE
assert.ok(bundle, 'LOCAL_PERPS_ABI_BUNDLE is required')
assert.equal(createHash('sha256').update(fs.readFileSync(bundle)).digest('hex'), release.release.bundleSha256)
const contracts = Object.fromEntries(Object.entries({ cfdEngine: 'CfdEngine', cfdEngineLens: 'CfdEngineLens',
  cfdEngineAccountLens: 'CfdEngineAccountLens', marginClearinghouse: 'MarginClearinghouse',
  mockUsdc: 'MockUSDC', housePool: 'ArbitrumSepoliaReleaseHousePool', orderRouter: 'ArbitrumSepoliaReleaseRouter',
}).map(([key, name]) => [key, { address: release.contracts[key].address, abi: JSON.parse(execFileSync('tar', ['-xOzf', bundle, `perps-${release.release.version}-arbitrum-sepolia/abi/${name}.json`], { encoding: 'utf8' })) }]))
for (const [key, contract] of Object.entries(contracts)) assert.equal(keccak256(await client.getCode(contract)), release.contracts[key].runtimeCodeHash, key)
const trader = '0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266'
const keeper = '0x70997970C51812dc3A010C7d01b50e0d17dc79C8'
const read = (key, functionName, args = [], blockNumber) => client.readContract({ ...contracts[key], functionName, args, blockNumber })
async function write(key, functionName, args = [], account = trader) {
  const hash = await wallet.writeContract({ ...contracts[key], functionName, args, account, gas: 15_000_000n })
  assert.equal((await client.waitForTransactionReceipt({ hash })).status, 'success', functionName)
}
const encode = value => JSON.stringify(value, (_, v) => typeof v === 'bigint' ? { $bigint: v.toString() } : v)
async function action(path, body) {
  const response = await fetch(`${origin}/local-market/api/${path}`, body === undefined ? undefined : {
    method: 'POST', headers: { Origin: origin, 'Content-Type': 'application/json' }, body: encode(body),
  })
  const data = JSON.parse(await response.text(), (_, v) => v && typeof v === 'object' && '$bigint' in v ? BigInt(v.$bigint) : v)
  assert.equal(response.ok, true, data.error)
  return data
}
async function snapshot() {
  const blockNumber = await client.getBlockNumber()
  const [position, entryCostUsdcAtoms, ledger, params, fad, capPrice, mark] = await Promise.all([
    read('cfdEngine', 'positions', [trader], blockNumber), read('cfdEngine', 'positionEntryCostUsdcAtoms', [trader], blockNumber),
    read('cfdEngineAccountLens', 'getAccountLedgerSnapshot', [trader], blockNumber), read('cfdEngine', 'riskParams', [], blockNumber),
    read('cfdEngine', 'isFadWindow', [], blockNumber), read('cfdEngine', 'CAP_PRICE', [], blockNumber), read('cfdEngine', 'lastMarkPrice', [], blockNumber),
  ])
  const input = { size: position[0], side: Number(position[4]), positionMarginUsdc: ledger.netEquityUsdc - ledger.unrealizedPnlUsdc - ledger.traderClaimBalanceUsdc, entryCostUsdcAtoms,
    traderClaimBalanceUsdc: ledger.traderClaimBalanceUsdc, capPrice, maintenanceMarginBps: params[fad ? 4 : 2] }
  const threshold = findLiquidationThreshold(input)
  assert.equal(calculatePriceRisk(input, mark).equityUsdc, ledger.netEquityUsdc, 'signed position equity')
  return { input, ledger, threshold, blockNumber, entryPrice: position[2] }
}
async function checkBoundary() {
  const state = await snapshot()
  assert.equal(state.threshold.status, 'boundary')
  const price = state.threshold.price
  const at = await read('cfdEngineLens', 'previewLiquidation', [trader, price], state.blockNumber)
  const before = await read('cfdEngineLens', 'previewLiquidation', [trader, price + (state.input.side === 0 ? -1n : 1n)], state.blockNumber)
  assert.equal(at.liquidatable, true)
  assert.equal(before.liquidatable, false)
  return state
}
const vite = await createServer({ configFile: false, server: { middlewareMode: true } })
try {
  for (const direction of ['long', 'short']) {
    await action('reset', {})
    await action('auto', { enabled: false })
    await action('open', { direction, size: '2000', margin: '500', params: { takeProfitTriggerPrice: direction === 'long' ? 50_000_000n : 150_000_000n, stopLossTriggerPrice: 0n } })
    const preview = await read('cfdEngineLens', 'previewOpen', [trader, direction === 'long' ? 0 : 1,
      parseUnits('2000', 18), parseUnits('500', 6), await read('cfdEngine', 'lastMarkPrice'), (await client.getBlock()).timestamp])
    assert.equal(preview.valid, true)
    await action('execute', {})
    const initial = await checkBoundary()
    assert.equal(initial.input.positionMarginUsdc, preview.postMarginUsdc, 'equivalent execution margin')
    assert.equal(initial.entryPrice, preview.postEntryPrice, 'equivalent execution price')
    assert.equal(initial.threshold.price, preview.liquidationPrice, 'open preview matches the resulting position')
    await write('mockUsdc', 'mint', [trader, parseUnits('100', 6)], release.release.owner)
    await write('mockUsdc', 'approve', [contracts.marginClearinghouse.address, parseUnits('100', 6)])
    await write('marginClearinghouse', 'deposit', [trader, parseUnits('100', 6)])
    assert.deepEqual((await checkBoundary()).threshold, initial.threshold, 'free deposits do not move price threshold')
    await write('cfdEngine', 'addMargin', [trader, parseUnits('100', 6)])
    const added = await checkBoundary()
    assert.equal(added.input.positionMarginUsdc, initial.input.positionMarginUsdc + parseUnits('100', 6))
    assert.ok(direction === 'long' ? added.threshold.price > initial.threshold.price : added.threshold.price < initial.threshold.price)
    const withdrawable = await read('cfdEngineAccountLens', 'getWithdrawableUsdc', [trader])
    assert.ok(withdrawable > 0n)
    await assert.rejects(client.simulateContract({ ...contracts.marginClearinghouse, functionName: 'withdraw', args: [trader, withdrawable + 1_000_000n], account: trader }), 'contract withdrawal ceiling is enforced')
    await write('marginClearinghouse', 'withdraw', [trader, 1_000_000n])
    assert.deepEqual((await checkBoundary()).threshold, added.threshold, 'free withdrawals do not reduce price backing')
    console.log(`PASS ${direction}: open preview agreement, exact boundary and adjacent tick, deposits, margin additions, withdrawal ceiling`)
  }

  const { preparePerpsOrderV2 } = await vite.ssrLoadModule('/src/contracts/preparePerpsOrderV2.ts')
  const manifest = JSON.parse(fs.readFileSync('public/perps-aa-manifest.json', 'utf8'))
  for (const direction of ['long', 'short']) {
    await action('reset', {})
    await action('auto', { enabled: false })
    await action('open', { direction, size: '6000', margin: '1500', params: { takeProfitTriggerPrice: direction === 'long' ? 50_000_000n : 150_000_000n, stopLossTriggerPrice: 0n } })
    const opened = await action('execute', {})
    await action('manage', { action: 'cancel', protectionId: opened.protection.protectionId })
    await action('price', { price: '1.00012345' })
    for (const isClose of [false, true]) {
      const prepared = await preparePerpsOrderV2(client, manifest, { account: trader, direction, side: direction === 'long' ? 0 : 1,
        sizeDelta: parseUnits('3100', 18), marginDelta: isClose ? 0n : parseUnits('800', 6), slippagePercent: 1, isClose, selectedMaxLeverageBps: 50_000 })
      await write('orderRouter', 'commitOrder', [prepared.request])
      await rpc('evm_increaseTime', [2])
      await rpc('evm_mine')
      await action('execute', {})
      const result = await checkBoundary()
      assert.notEqual(result.input.entryCostUsdcAtoms, result.input.size / 10n ** 20n * result.entryPrice, 'fixture exercises exact basis beyond rounded entry')
    }
    console.log(`PASS ${direction}: exact entry-cost rounding after an increase and partial reduction`)
  }

  // Create a real same-account claim through a profitable partial close during a
  // simulated pool-cash shortfall. Only the disposable fork's token balance is changed.
  await action('reset', {})
  await action('auto', { enabled: false })
  await action('open', { direction: 'long', size: '6000', margin: '1500', params: { takeProfitTriggerPrice: 50_000_000n, stopLossTriggerPrice: 0n } })
  let state = await action('execute', {})
  await action('manage', { action: 'cancel', protectionId: state.protection.protectionId })
  await action('price', { price: '1.05' })
  const prepared = await preparePerpsOrderV2(client, manifest, { account: trader, direction: 'long', side: 0,
    sizeDelta: parseUnits('3000', 18), marginDelta: 0n, slippagePercent: 1, isClose: true, selectedMaxLeverageBps: 50_000 })
  await write('orderRouter', 'commitOrder', [prepared.request])
  const pool = contracts.housePool.address
  const poolCash = await read('mockUsdc', 'balanceOf', [pool])
  await rpc('anvil_impersonateAccount', [pool])
  await rpc('anvil_setBalance', [pool, toHex(parseUnits('1', 18))])
  await write('mockUsdc', 'transfer', [keeper, poolCash - 1n], pool)
  await rpc('evm_increaseTime', [2]); await rpc('evm_mine')
  await action('execute', {})
  const withClaim = await snapshot()
  assert.ok(withClaim.input.traderClaimBalanceUsdc > 0n, 'partial close creates a claim')
  assert.equal(withClaim.input.size, parseUnits('3000', 18))
  await write('mockUsdc', 'transfer', [pool, poolCash - 1n], keeper)
  await write('cfdEngine', 'settleTraderClaim', [trader])
  const settled = await snapshot()
  assert.equal(settled.input.traderClaimBalanceUsdc, 0n)
  assert.equal(settled.input.positionMarginUsdc, withClaim.input.positionMarginUsdc + withClaim.input.traderClaimBalanceUsdc)
  assert.deepEqual(settled.threshold, withClaim.threshold, 'settling a live claim preserves price collateral')
  console.log('PASS real partial-close claim, exact basis, and settlement into live position margin')
} finally {
  await vite.close()
  await action('reset', {})
}
