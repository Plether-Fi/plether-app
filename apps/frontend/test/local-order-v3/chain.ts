import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import path from 'node:path'
import { execFileSync } from 'node:child_process'
import { createPublicClient, createWalletClient, http, getContractAddress, keccak256, parseEther, toHex, zeroAddress, type Abi, type Address, type Hex } from 'viem'
import { mnemonicToAccount } from 'viem/accounts'
import { arbitrumSepolia } from 'viem/chains'
import { entryPoint08Address } from 'viem/account-abstraction'

function requiredEnv(name: string): string {
  const value = process.env[name]
  assert.ok(value, `Missing ${name}`)
  return value
}
const rpcUrl = requiredEnv('PLETHER_LOCAL_RPC')
assert.match(rpcUrl, /^http:\/\/127\.0\.0\.1:\d+$/)
export const client = createPublicClient({ chain: arbitrumSepolia, transport: http(rpcUrl), cacheTime: 0 })
const mnemonic = 'test test test test test test test test test test test junk'
export const admin = mnemonicToAccount(mnemonic)
export const owner = mnemonicToAccount(mnemonic, { addressIndex: 1 })
export const sponsor = mnemonicToAccount(mnemonic, { addressIndex: 2 })
export const keeper = mnemonicToAccount(mnemonic, { addressIndex: 3 })
const wallet = createWalletClient({ chain: arbitrumSepolia, account: admin, transport: http(rpcUrl) })
export const rpc = (method: string, params: unknown[] = []) => client.request({ method, params } as never)
export const core = requiredEnv('PLETHER_CORE_PATH')
export function artifact(name: string, pkg = 'perps'): { abi: Abi; bytecode: Hex } {
  const raw = JSON.parse(readFileSync(path.join(core, 'packages', pkg, 'out', `${name}.sol`, `${name}.json`), 'utf8')) as { abi: Abi; bytecode: { object: Hex } }
  return { abi: raw.abi, bytecode: raw.bytecode.object }
}
export function aaArtifact(name: string): { abi: Abi; bytecode: Hex } {
  return JSON.parse(readFileSync(path.resolve(`test/local-order-v3/node_modules/@account-abstraction/contracts/artifacts/${name}.json`), 'utf8')) as { abi: Abi; bytecode: Hex }
}
export interface Contract { address: Address; abi: Abi }
export async function deploy(name: string, args: readonly unknown[] = [], pkg = 'perps'): Promise<Contract> {
  const a = pkg === 'aa' ? aaArtifact(name) : artifact(name, pkg)
  const hash = await wallet.deployContract({ ...a, args, gas: 28_000_000n })
  const receipt = await client.waitForTransactionReceipt({ hash })
  assert.equal(receipt.status, 'success', `Deploy fixture ${name}`)
  assert.ok(receipt.contractAddress)
  return { address: receipt.contractAddress, abi: a.abi }
}
export const read = (contract: Contract, functionName: string, args: readonly unknown[] = []) => client.readContract({ ...contract, functionName, args })
export async function write(contract: Contract, functionName: string, args: readonly unknown[] = [], account = admin, value = 0n) {
  const hash = await wallet.writeContract({ ...contract, functionName, args, account, value, gas: 20_000_000n })
  const receipt = await client.waitForTransactionReceipt({ hash })
  assert.equal(receipt.status, 'success', `${functionName} ${hash}`)
  return receipt
}
export const now = async () => (await client.getBlock()).timestamp
export async function warp(timestamp: bigint) { await rpc('evm_setNextBlockTimestamp', [Number(timestamp)]); await rpc('evm_mine') }
export function policyDeadline(timestamp: bigint, configuredExpiry: bigint, callData: Hex): bigint {
  return BigInt(execFileSync(requiredEnv('PLETHER_LOCAL_POLICY_BIN'), [String(timestamp), String(configuredExpiry), callData.slice(2)], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim())
}

/** Production protocol + official ERC-4337 contracts; only token and Pyth are fixtures. */
export async function createStack() {
  assert.match(String(await rpc('web3_clientVersion')), /anvil/i)
  assert.equal(await client.getChainId(), 421614)
  await rpc('anvil_reset')
  const token = await deploy('MockUSDC')
  const clearinghouse = await deploy('MarginClearinghouse', [token.address])
  const risk = { vpiFactor: 0n, maxSkewRatio: 400_000_000_000_000_000n, maintMarginBps: 100n, initMarginBps: 150n,
    fadMarginBps: 300n, baseCarryBps: 500n, minBountyUsdc: 1_000_000n, bountyBps: 10n, keeperShareBps: 5000n, protocolShareBps: 0n }
  const engine = await deploy('CfdEngine', [token.address, clearinghouse.address, 200_000_000n, risk, 50n])
  const planner = await deploy('CfdEnginePlanner')
  const settlement = await deploy('CfdEngineSettlementSidecar', [engine.address])
  const engineAdmin = await deploy('CfdEngineAdmin', [engine.address, admin.address])
  await write(engine, 'setDependencies', [planner.address, settlement.address, engineAdmin.address])
  await write(engine, 'setProtocolTreasury', [admin.address])
  const terminal = await deploy('TerminalNavBookV2', [engine.address, 200_000_000])
  await write(engine, 'setTerminalNavBook', [terminal.address])
  const engineLens = await deploy('CfdEngineLens', [engine.address])
  const accountLens = await deploy('CfdEngineAccountLens', [engine.address])
  const math = await deploy('HousePoolRedemptionMathSidecar')
  const pool = await deploy('HousePool', [token.address, engine.address, math.address])
  const junior = await deploy('TrancheVault', [token.address, pool.address, false, 'Local junior', 'LJ', 0n, zeroAddress])
  const senior = await deploy('TrancheVault', [token.address, pool.address, true, 'Local senior', 'LS', 0n, zeroAddress])
  await write(pool, 'setJuniorVault', [junior.address]); await write(pool, 'setSeniorVault', [senior.address])
  await write(engine, 'setPool', [pool.address]); await write(clearinghouse, 'setEngine', [engine.address])
  const pyth = await deploy('MockPyth')
  const feeds = [toHex(1, { size: 32 }), toHex(2, { size: 32 })]
  const oracle = await deploy('PletherOracle', [engine.address, pool.address, pyth.address, feeds, [500_000_000_000_000_000n, 500_000_000_000_000_000n], [100_000_000n, 100_000_000n], [false, false]])
  const evaluator = await deploy('CfdOrderPolicyEvaluator')
  const execution = await deploy('OrderRouterV3ExecutionSidecar')
  const nonce = await client.getTransactionCount({ address: admin.address })
  const routerAddress = getContractAddress({ from: admin.address, nonce: BigInt(nonce + 2) })
  const book = await deploy('OrderLifecycleBook', [routerAddress, engine.address, clearinghouse.address, pool.address])
  const commit = await deploy('OrderRouterLiquidationBatchSidecar', [routerAddress])
  const router = await deploy('OrderRouter', [engine.address, engineLens.address, pool.address, oracle.address, commit.address, evaluator.address, execution.address, book.address])
  assert.equal(router.address.toLowerCase(), routerAddress.toLowerCase())
  await write(engine, 'setOrderRouter', [router.address])
  const lens = await deploy('PerpsPublicLens', [accountLens.address, engine.address, router.address, pool.address])
  const protection: Contract = { address: await read(router, 'positionProtectionBook') as Address, abi: artifact('PositionProtectionBook').abi }
  await write(pool, 'proposePoolConfig', [{ seniorRateBps: 800n, markStalenessLimit: 60n, seniorFrozenLpFeeBps: 25n, juniorFrozenLpFeeBps: 75n, maxSeniorExposureUsdc: 40_000_000_000_000n, maxSeniorShareBps: 8000n }])
  await warp(await read(pool, 'poolConfigActivationTime') as bigint)
  await write(pool, 'finalizePoolConfig')
  async function mark() { await write(pyth, 'setAllPrices', [feeds, 100_000_000n, -8, await now()]); await write(router, 'updateMarkPrice', [['0x00']]) }
  await mark()
  await write(token, 'mint', [admin.address, 2_000_000_000_000n]); await write(token, 'approve', [pool.address, 2_000_000_000_000n])
  await write(pool, 'initializeSeedPosition', [false, 1_000_000_000_000n, admin.address])
  await write(pool, 'initializeSeedPosition', [true, 1000_000_000n, admin.address]); await write(pool, 'activateTrading')

  // Copy the initialized reference runtime to the canonical local address. Its
  // genuine SenderCreator remains present; the account is created before trading.
  const ep = await deploy('EntryPoint', [], 'aa')
  await rpc('anvil_setCode', [entryPoint08Address, await client.getCode({ address: ep.address })])
  const entryPoint: Contract = { address: entryPoint08Address, abi: ep.abi }
  const factory = await deploy('SimpleAccountFactory', [entryPoint.address], 'aa')
  const implementation = await read(factory, 'accountImplementation') as Address
  const account: Contract = { address: await read(factory, 'getAddress', [owner.address, 0n]) as Address, abi: aaArtifact('SimpleAccount').abi }
  // Local setup only: the reference factory restricts creation to SenderCreator.
  const creator = await read(entryPoint, 'senderCreator') as Address
  await rpc('anvil_impersonateAccount', [creator])
  await rpc('anvil_setBalance', [creator, toHex(parseEther('1'))])
  const creationHash = await wallet.writeContract({ ...factory, functionName: 'createAccount', args: [owner.address, 0n], account: creator })
  assert.equal((await client.waitForTransactionReceipt({ hash: creationHash })).status, 'success')
  await rpc('anvil_stopImpersonatingAccount', [creator])
  const accountCode = await client.getCode({ address: account.address })
  assert.ok(accountCode)
  const accountHash = keccak256(accountCode)
  const policyId = '0x8dd77324b94da492342191f762a32cdf99e828a7f24d77c8ed5ace90cf4f5ae3'
  const paymaster = await deploy('PletherVerifyingPaymaster', [entryPoint.address, admin.address, sponsor.address, parseEther('1'), policyId, accountHash, factory.address, implementation], 'perps-aa')
  await write(paymaster, 'unpause'); await write(paymaster, 'deposit', [], admin, parseEther('10'))
  await write(token, 'mint', [account.address, 100_000_000_000n])
  const { encodeFunctionData } = await import('viem')
  await write(account, 'execute', [token.address, 0n, encodeFunctionData({ abi: token.abi, functionName: 'approve', args: [clearinghouse.address, 100_000_000_000n] })], owner)
  await write(account, 'execute', [clearinghouse.address, 0n, encodeFunctionData({ abi: clearinghouse.abi, functionName: 'depositMargin', args: [100_000_000_000n] })], owner)
  async function execute(orderId: bigint, publishTime: bigint, executionTimestamp?: bigint) {
    await write(pyth, 'setAllUniquePrices', [feeds, 100_000_000n, 0n, -8, publishTime, publishTime - 1n])
    if (executionTimestamp !== undefined) await rpc('evm_setNextBlockTimestamp', [Number(executionTimestamp)])
    return write(router, 'executeOrder', [orderId, ['0x00']], keeper)
  }
  return { token, clearinghouse, engine, pool, oracle, evaluator, book, router, lens, accountLens, engineLens, protection, entryPoint, factory, account, paymaster, accountHash, policyId, mark, execute }
}
