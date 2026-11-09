import assert from 'node:assert/strict'
import { readFile } from 'node:fs/promises'
import { fileURLToPath } from 'node:url'
import { performance } from 'node:perf_hooks'
import ts from 'typescript'
import { createPublicClient, custom, isAddress, keccak256 } from 'viem'

// Read-only differential check against the pinned deployed protocol. This never
// deploys, signs, changes state, uses a wallet, or sends a transaction. The helper
// is created only inside an eth_call and disappears with that call's EVM state.
if (process.argv.includes('--help')) {
  console.log(`Read-only differential check for the deployless Max quoter.

Usage:
  ARB_SEPOLIA_RPC_URL=https://your-archive-rpc npm run test:max-open-quote-live

Environment:
  ARB_SEPOLIA_RPC_URL  Required Arbitrum Sepolia archive RPC URL.
  MAX_OPEN_BLOCK      Optional block; defaults to 326380770.
  MAX_OPEN_ACCOUNT    Optional funded sample; defaults to the public operator\'s
                      0x9314586D4068C73B23a64d7406Ca8FfEeCc2cBFc account.

Checks canonical deployment code hashes, full legacy/new quote parity, reward
reservations and one eth_call per Max calculation. Nothing is broadcast.`)
  process.exit(0)
}

const rpc = process.env.ARB_SEPOLIA_RPC_URL
assert.ok(rpc, 'Set ARB_SEPOLIA_RPC_URL to an Arbitrum Sepolia archive RPC')
const account = process.env.MAX_OPEN_ACCOUNT ?? '0x9314586D4068C73B23a64d7406Ca8FfEeCc2cBFc'
assert.ok(isAddress(account), 'MAX_OPEN_ACCOUNT must be an address')
const blockNumber = BigInt(process.env.MAX_OPEN_BLOCK ?? '326380770')
const root = new URL('../../../', import.meta.url)
const manifest = JSON.parse(await readFile(new URL('config/perps/arbitrum-sepolia-v2.0.0.json', root), 'utf8'))
assert.ok(blockNumber >= BigInt(manifest.release.deploymentBlock), 'Block predates the pinned deployment')
const at = name => manifest.contracts[name].address

async function importTypeScript(url) {
  const source = (await readFile(url, 'utf8')).replace(/^export \{.*\} from .*$/mg, '')
  const { outputText, diagnostics } = ts.transpileModule(source, {
    compilerOptions: { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ESNext },
    reportDiagnostics: true,
  })
  assert.equal(diagnostics?.length ?? 0, 0, `Cannot transpile ${fileURLToPath(url)}`)
  return import(`data:text/javascript;base64,${Buffer.from(outputText).toString('base64')}`)
}
const abi = await importTypeScript(new URL('../src/contracts/abis/Perps.ts', import.meta.url))
const generated = await importTypeScript(new URL('../src/contracts/generated/perpsMaxOpenQuoter.ts', import.meta.url))
const helperAbi = generated.PERPS_MAX_OPEN_QUOTER_ABI
const helperCode = generated.PERPS_MAX_OPEN_QUOTER_BYTECODE
// EVM computation budget for these read-only quotes, not transaction gas.
const quoteGas = 10_000_000n
assert.ok(helperAbi && helperCode, 'Missing generated Max quoter ABI or creation bytecode')
const requests = []
const client = createPublicClient({ transport: custom({ request: async ({ method, params }) => {
  requests.push(method)
  const response = await fetch(rpc, {
    method: 'POST', headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ jsonrpc: '2.0', id: 1, method, params }),
    signal: AbortSignal.timeout(60_000),
  })
  assert.ok(response.ok, `RPC HTTP ${response.status}`)
  const result = await response.json()
  if (result.error) throw Object.assign(new Error(result.error.message), result.error)
  return result.result
} }, { retryCount: 0 }) })
assert.equal(await client.getChainId(), manifest.network.chainId)
for (const name of ['cfdEngine', 'cfdEngineLens', 'orderRouter']) {
  const code = await client.getCode({ address: at(name), blockNumber })
  assert.ok(code, `${name} missing at block ${blockNumber}`)
  assert.equal(keccak256(code), manifest.contracts[name].runtimeCodeHash, `${name} code hash differs from the pinned release`)
}
const read = (name, addressName, abiName, args = []) => client.readContract({
  address: at(addressName), abi: abi[abiName], functionName: name, args, blockNumber,
})
const [availableAssets, maximumBounty, capPrice, status] = await Promise.all([
  read('getFreeBuyingPowerAssets', 'marginClearinghouse', 'PERPS_MARGIN_CLEARINGHOUSE_ABI', [account]),
  read('maxOpenOrderExecutionBountyAssets', 'orderRouter', 'PERPS_ORDER_ROUTER_ABI'),
  read('CAP_PRICE', 'cfdEngine', 'PERPS_CFD_ENGINE_ABI'),
  read('getProtocolStatus', 'perpsPublicLens', 'PERPS_PUBLIC_LENS_ABI'),
])
const oraclePrice = status.lastMarkPrice
const quantum = 100n * 10n ** 18n
const scale = 10n ** 20n
const price = oraclePrice < capPrice ? oraclePrice : capPrice
function withinLeverage(preview, limit) {
  assert.ok(preview.valid, `Intermediate canonical planner rejection: ${preview.invalidReason}`)
  assert.ok(preview.postEquityAssets > 0n, 'Nonpositive post-trade equity')
  const notional = preview.postSize * price / scale
  return (notional * 10_000n + preview.postEquityAssets - 1n) / preview.postEquityAssets <= limit
}
async function originalQuote(side, marginDelta, limit) {
  const quote = await read('quoteMaxOpen', 'cfdEngineLens', 'PERPS_CFD_ENGINE_LENS_ABI', [account, side, marginDelta, oraclePrice])
  if (!quote.preview.valid || quote.maxSizeDelta === 0n || withinLeverage(quote.preview, limit)) return quote
  let lower = 0n
  let upper = (quote.maxSizeDelta + quantum - 1n) / quantum
  let best
  for (let attempt = 0; lower + 1n < upper; attempt++) {
    assert.ok(attempt < 64, 'Legacy leverage search did not converge')
    const midpoint = (lower + upper) / 2n
    const sizeDelta = midpoint * quantum
    const preview = await read('previewOpen', 'cfdEngineLens', 'PERPS_CFD_ENGINE_LENS_ABI', [account, side, sizeDelta, marginDelta, oraclePrice])
    if (withinLeverage(preview, limit)) {
      lower = midpoint
      best = { sizeDelta, preview }
    } else upper = midpoint
  }
  return { ...quote, maxSizeDelta: best?.sizeDelta ?? 0n, preview: best?.preview ?? quote.preview }
}
let constrainedPositive = 0
for (const scenario of [
  { side: 0, leverage: 100_000n, protection: 0n },
  { side: 0, leverage: 1n, protection: 0n },
  { side: 0, leverage: 330_000n, protection: 400_000n },
  { side: 0, leverage: 4_294_967_295n, protection: 0n },
  { side: 1, leverage: 100_000n, protection: 0n },
]) {
  const reserved = maximumBounty + scenario.protection
  const marginDelta = availableAssets > reserved ? availableAssets - reserved : 0n
  assert.ok(marginDelta > 0n, 'Differential sample must have funded free buying power')
  const oldStart = performance.now()
  const oldReadsStart = requests.length
  const expected = await originalQuote(scenario.side, marginDelta, scenario.leverage)
  const oldMilliseconds = Math.round(performance.now() - oldStart)
  const oldRequests = requests.length - oldReadsStart
  const newStart = performance.now()
  const newReadsStart = requests.length
  const [actualMargin, actual] = await client.readContract({
    code: helperCode, abi: helperAbi, functionName: 'quoteAvailable',
    args: [at('cfdEngineLens'), at('cfdEngine'), at('orderRouter'), account,
      scenario.side, availableAssets, scenario.protection, oraclePrice, scenario.leverage],
    blockNumber, gas: quoteGas,
  })
  const newMilliseconds = Math.round(performance.now() - newStart)
  const newRequests = requests.slice(newReadsStart)
  assert.deepEqual(newRequests, ['eth_call'], 'A Max quote must use exactly one RPC request')
  assert.equal(actualMargin, marginDelta, 'Execution/protection reward reservations differ')
  assert.deepEqual(actual, expected, 'Single-RPC quote differs from the original canonical-lens search')
  const fixed = await client.readContract({
    code: helperCode, abi: helperAbi, functionName: 'quoteWithMargin',
    args: [at('cfdEngineLens'), at('cfdEngine'), account, scenario.side,
      marginDelta, oraclePrice, scenario.leverage], blockNumber, gas: quoteGas,
  })
  assert.deepEqual(fixed, expected, 'Review-specific fixed-margin quote differs')
  if (expected.maxSizeDelta > 0n && oldRequests > 1) constrainedPositive++
  console.log(JSON.stringify({ side: scenario.side, leverageBps: scenario.leverage.toString(),
    protectionAssets: scenario.protection.toString(), maxTokens: (expected.maxSizeDelta / 10n ** 18n).toString(),
    oldRequests, singleRpcRequests: newRequests.length, oldMilliseconds, singleRpcMilliseconds: newMilliseconds }))
}
assert.ok(constrainedPositive > 0, 'Scenarios must exercise a positive leverage-constrained maximum')
console.log(`Single-RPC Max matches all canonical quote fields at Arbitrum Sepolia block ${blockNumber}.`)
