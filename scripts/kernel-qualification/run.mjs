import assert from 'node:assert/strict'
import { execFileSync, spawn } from 'node:child_process'
import { readFileSync } from 'node:fs'
import { createRequire } from 'node:module'
import { createServer } from 'node:net'
import { resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { parseArgs } from 'node:util'
import { setTimeout as sleep } from 'node:timers/promises'
import { assertEmbeddedPerpsOwner } from '../../apps/frontend/src/perps-aa/embeddedOwner.ts'
import { createKernelTradingAccount, encodeKernelCalls, verifyKernelTradingAccountAtBlock } from '../../apps/frontend/src/perps-aa/kernelAccount.ts'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const {
  concatHex, createPublicClient, createWalletClient, custom, decodeEventLog, encodeFunctionData,
  getAddress, getContractAddress, http, keccak256, parseAbi, parseEther, toHex, zeroAddress, zeroHash,
} = require('viem')
const { mnemonicToAccount, privateKeyToAccount } = require('viem/accounts')
const {
  entryPoint09Abi, formatUserOperationRequest, getUserOperationHash, toPackedUserOperation,
} = require('viem/account-abstraction')

const pins = JSON.parse(readFileSync(new URL('./pins.json', import.meta.url)))
assert.equal(require('viem/package.json').version, pins.viemVersion, 'Use the app’s locked Viem version')
const { values: args } = parseArgs({ options: {
  'kernel-repo': { type: 'string' },
  'validator-repo': { type: 'string' },
  'solc': { type: 'string' },
  'runtime-dir': { type: 'string' },
  'paymaster-artifact': { type: 'string' },
  'alto-image': { type: 'string' },
  'safe-mode': { type: 'boolean' },
}})
for (const key of ['kernel-repo', 'validator-repo', 'solc', 'runtime-dir', 'paymaster-artifact']) {
  assert.ok(args[key], `Pass --${key}; see scripts/kernel-qualification/README.md`)
}

const factoryAbi = parseAbi([
  'struct Install { uint256 moduleType; address module; bytes moduleData; bytes internalData; }',
  'function UUPS() view returns (address)',
  'function getAddress(Install[] initialPackages,uint256 nonce) view returns (address)',
  'function deploy(Install[] initialPackages,uint256 nonce) payable returns (address)',
])
const stakerAbi = parseAbi([
  'constructor(address owner)',
  'function deployWithFactory(address factory,bytes createData) payable returns (address)',
  'function approveFactory(address factory,bool approval) payable',
  'function approved(address factory) view returns (bool)',
  'function stake(address entryPoint,uint32 unstakeDelay) payable',
  'function owner() view returns (address)',
])
const stakeManagerAbi = parseAbi([
  'function getDepositInfo(address account) view returns ((uint112 deposit,bool staked,uint112 stake,uint32 unstakeDelaySec,uint48 withdrawTime))',
])
const targetAbi = parseAbi(['function record()', 'function calls(address account) view returns (uint256)'])
const gasLimit = 10_000_000n
const maxCost = parseEther('0.05')
const sponsorGasLimit = 150_000n
const policyId = keccak256(toHex('Plether Kernel qualification only'))
const chain = {
  id: pins.chainId,
  name: 'Local Kernel qualification',
  nativeCurrency: { name: 'Ether', symbol: 'ETH', decimals: 18 },
  rpcUrls: { default: { http: [] } },
}

// Public Anvil test mnemonic, used only with the private loopback process below.
const mnemonic = 'test test test test test test test test test test test junk'
const localAccount = addressIndex => mnemonicToAccount(mnemonic, { addressIndex })
const utility = localAccount(0)
const executor = localAccount(1)
const owners = [localAccount(2), localAccount(3)]
const sponsor = privateKeyToAccount(toHex(localAccount(4).getHdKey().privateKey))
const counterfactualOwner = localAccount(7)

function command(file, params, options = {}) {
  return execFileSync(file, params, { encoding: 'utf8', maxBuffer: 12 * 1024 * 1024, ...options })
}

function readExactRuntime(filename, pin) {
  const code = readFileSync(resolve(args['runtime-dir'], filename), 'utf8').trim()
  assert.equal(keccak256(code), pin.codeHash, `${filename} must match Arbitrum Sepolia block ${pins.referenceBlock}`)
  return code
}

function compileValidator() {
  const repo = resolve(args['validator-repo'])
  assert.equal(command('git', ['-C', repo, 'rev-parse', 'HEAD']).trim(), pins.validator.commit)
  const changes = command('git', ['-C', repo, 'status', '--porcelain', '--untracked-files=no'])
  assert.equal(changes.trim(), '', 'Validator checkout must match the pinned commit')
  const source = 'src/validators/ECDSAValidator.sol'
  const output = command(resolve(args.solc), [
    '--base-path', '.', '--via-ir', '--optimize', '--optimize-runs', '20000', '--evm-version', 'prague',
    '--combined-json', 'bin,bin-runtime',
    'solady/=dependencies/solady-0.1.26/src/',
    'account-abstraction/=dependencies/eth-infinitism-account-abstraction-0.8.0/contracts/',
    source,
  ], { cwd: repo })
  const bytecodes = JSON.parse(output).contracts[`${source}:ECDSAValidator`]
  assert.equal(keccak256(`0x${bytecodes.bin}`), pins.validator.initCodeHash)
  assert.equal(keccak256(`0x${bytecodes['bin-runtime']}`), pins.validator.codeHash)
  return `0x${bytecodes.bin}`
}

async function freePort() {
  const server = createServer()
  const port = await new Promise((resolvePort, reject) => {
    server.once('error', reject)
    server.listen(0, '127.0.0.1', () => resolvePort(server.address().port))
  })
  await new Promise((done, reject) => server.close(error => error ? reject(error) : done()))
  return port
}

let requestId = 0
async function request(url, method, params = []) {
  const id = ++requestId
  const response = await fetch(url, {
    method: 'POST', headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ jsonrpc: '2.0', method, params, id }),
    signal: AbortSignal.timeout(5_000),
  })
  const payload = await response.json()
  assert.equal(payload.id, id)
  assert.ok(!payload.error, `${method} failed with HTTP ${response.status}: ${JSON.stringify(payload.error)}`)
  assert.ok(response.ok, `${method} failed with HTTP ${response.status}`)
  return payload.result
}

async function waitFor(check, label) {
  let lastError
  const deadline = Date.now() + 20_000
  while (Date.now() < deadline) {
    try {
      const result = await check()
      if (result) return result
    } catch (error) {
      lastError = error
    }
    await sleep(250)
  }
  throw new Error(`Timed out waiting for ${label}`, { cause: lastError })
}

const solcVersion = command(resolve(args.solc), ['--version'])
assert.ok(solcVersion.includes(pins.validator.solc), `Unexpected Solidity compiler: ${solcVersion}`)
const validatorBytecode = compileValidator()
const kernelRelease = JSON.parse(command('git', [
  '-C', resolve(args['kernel-repo']), 'show', `${pins.kernel.commit}:${pins.kernel.release}`,
]))
const stakerRelease = kernelRelease.contracts.find(contract => contract.name === 'Staker')
assert.ok(stakerRelease?.bytecode?.startsWith('0x'), 'Pinned Kernel release must include Staker creation code')
assert.equal(getAddress(kernelRelease.create2.factory), getAddress(pins.create2Deployer.address))
assert.equal(kernelRelease.create2.salt, zeroHash)
const entryPointCode = readExactRuntime('entrypoint-runtime.hex', pins.entryPoint)
const senderCreatorCode = readExactRuntime('sender-creator-runtime.hex', pins.senderCreator)
const deployerCode = readExactRuntime('create2-runtime.hex', pins.create2Deployer)
const paymasterArtifact = JSON.parse(readFileSync(resolve(args['paymaster-artifact']), 'utf8'))
const altoImage = args['alto-image'] ?? pins.alto.image
assert.ok(Object.values(paymasterArtifact.metadata.settings.compilationTarget).includes('PletherKernelVerifyingPaymaster'))
const targetSource = fileURLToPath(new URL('./QualificationTarget.sol', import.meta.url))
const targetArtifacts = JSON.parse(command(resolve(args.solc), ['--optimize', '--combined-json', 'bin', targetSource]))
const targetContract = Object.entries(targetArtifacts.contracts).find(([name]) => name.endsWith(':QualificationTarget'))[1]

const [chainPort, bundlerPort] = await Promise.all([freePort(), freePort()])
assert.notEqual(chainPort, bundlerPort)
const chainRpc = `http://127.0.0.1:${chainPort}`
const bundlerRpc = `http://127.0.0.1:${bundlerPort}`
const publicClient = createPublicClient({ chain, transport: http(chainRpc) })
const utilityWallet = createWalletClient({ account: utility, chain, transport: http(chainRpc) })
const anvil = spawn('anvil', [
  '--host', '127.0.0.1', '--port', String(chainPort), '--chain-id', String(chain.id),
  '--hardfork', 'prague', '--mnemonic', mnemonic, '--accounts', '7', '--silent',
], { stdio: ['ignore', 'ignore', 'inherit'] })
const altoName = `plether-kernel-qualification-${process.pid}`
let altoStarted = false

const entryPoint = pins.entryPoint.address
const factoryPin = pins.kernel.contracts.find(c => c.name === 'KernelFactory')
const implementationPin = pins.kernel.contracts.find(c => c.name === 'KernelUUPS')
const factory = getAddress(factoryPin.address)
const implementation = getAddress(implementationPin.address)
const validator = pins.validator.address
const accountProfile = {
  chainId: chain.id, entryPoint, entryPointVersion: '0.9', factory, validator, accountVersion: 'kernel-v4-uups', accountIndex: 0,
  entryPointCodeHash: pins.entryPoint.codeHash, factoryCodeHash: factoryPin.codeHash,
  implementation, implementationCodeHash: implementationPin.codeHash,
  validatorCodeHash: pins.validator.codeHash, accountCodeHash: pins.kernel.proxyCodeHash,
  staker: zeroAddress, stakerCodeHash: zeroHash,
}

async function read(address, abi, functionName, args = []) {
  return publicClient.readContract({ address, abi, functionName, args })
}

async function confirmed(hash) {
  const receipt = await publicClient.waitForTransactionReceipt({ hash, timeout: 12_000 })
  assert.equal(receipt.status, 'success', `Transaction ${hash} reverted`)
  return receipt
}

function deploy(bytecode, abi, args = []) {
  return utilityWallet.deployContract({ abi, bytecode, args }).then(confirmed)
}

async function deployCreate2(code, expected) {
  const from = pins.create2Deployer.address
  const address = getContractAddress({ bytecode: code, from, salt: zeroHash, opcode: 'CREATE2' })
  assert.equal(address, getAddress(expected), 'Pinned deterministic address mismatch')
  await confirmed(await utilityWallet.sendTransaction({ to: from, data: concatHex([zeroHash, code]) }))
  const deployed = await publicClient.getCode({ address })
  assert.ok(deployed && deployed !== '0x', `No code at ${address}`)
  return keccak256(deployed)
}

function packagesFor(owner) {
  return [{ moduleType: 1n, module: validator, moduleData: owner.address, internalData: '0x' }]
}

async function accountAddress(owner) {
  return read(factory, factoryAbi, 'getAddress', [packagesFor(owner), 0n])
}

async function tradingAccount(owner, walletFamily) {
  if (walletFamily === 'embedded') {
    assertEmbeddedPerpsOwner({ address: owner.address, embeddedWalletInfo: { accountType: 'eoa' } }, owner.address)
  }
  const wallet = createWalletClient({
    account: owner.address,
    chain,
    transport: custom({ request: async ({ method, params }) => {
      assert.equal(method, 'personal_sign')
      const [message, address] = params
      assert.equal(getAddress(address), owner.address)
      return owner.signMessage({ message: { raw: message } })
    } }),
  })
  const blockNumber = await publicClient.getBlockNumber({ cacheTime: 0 })
  const account = await createKernelTradingAccount(publicClient, accountProfile,
    { address: owner.address, signMessage: ({ message }) => wallet.signMessage({ message }) }, blockNumber)
  await checkAccount(account.address, owner, blockNumber, false)
  const createData = encodeFunctionData({ abi: factoryAbi, functionName: 'deploy', args: [packagesFor(owner), 0n] })
  const expectedFactoryData = encodeFunctionData({
    abi: stakerAbi, functionName: 'deployWithFactory', args: [factory, createData],
  })
  assert.deepEqual({ factory: account.factoryArgs.factory, data: account.factoryArgs.factoryData }, {
    factory: accountProfile.staker, data: expectedFactoryData.toLowerCase(),
  })
  return account
}

async function gasFunds(sender) {
  const [liquid, deposit] = await Promise.all([
    publicClient.getBalance({ address: sender }), read(entryPoint, entryPoint09Abi, 'balanceOf', [sender]),
  ])
  return liquid + deposit
}

async function fundAccount(sender, owner) {
  const accountWallet = owner ? createWalletClient({ account: owner, chain, transport: http(chainRpc) }) : utilityWallet
  const liquid = await publicClient.getBalance({ address: sender })
  const deposit = await read(entryPoint, entryPoint09Abi, 'balanceOf', [sender])
  const code = await publicClient.getCode({ address: sender })
  const value = parseEther('0.2')
  const receipt = await confirmed(await accountWallet.sendTransaction({ to: sender, value }))
  assert.equal(getAddress(receipt.from), accountWallet.account.address)
  assert.equal(getAddress(receipt.to), sender)
  assert.equal(await publicClient.getBalance({ address: sender }), liquid + value)
  assert.equal(await read(entryPoint, entryPoint09Abi, 'balanceOf', [sender]), deposit)
  assert.equal(await publicClient.getCode({ address: sender }), code, 'Funding must preserve the account deployment state')
}

async function checkAccount(sender, owner, blockNumber, deployed) {
  assert.equal(sender, await accountAddress(owner))
  const account = await verifyKernelTradingAccountAtBlock(publicClient, accountProfile, owner.address, blockNumber, sender)
  assert.deepEqual(account, { address: sender, ownerAddress: owner.address, deployed, blockNumber })
}

async function feeQuote() {
  const { fast } = await request(bundlerRpc, 'pimlico_getUserOperationGasPrice')
  return {
    maxFeePerGas: BigInt(fast.maxFeePerGas),
    maxPriorityFeePerGas: BigInt(fast.maxPriorityFeePerGas),
  }
}

function singleCall(target, data, value = 0n) {
  return encodeKernelCalls([{ to: target, data, value }])
}

async function estimationOperation({ account, nonce, callData, first, fees }) {
  const operation = {
    sender: account.address, nonce, callData,
    callGasLimit: gasLimit,
    verificationGasLimit: gasLimit,
    preVerificationGas: 100_000n,
    ...fees,
    signature: await account.getStubSignature(),
  }
  return first ? { ...operation, ...account.factoryArgs } : operation
}

function maxPrefund(operation) {
  return (operation.callGasLimit + operation.verificationGasLimit + operation.preVerificationGas) * operation.maxFeePerGas
}

async function prepareOperation({ account, nonce, callData, first, fees, sponsorship }) {
  const operation = await estimationOperation({ account, nonce, callData, first, fees })
  let sponsorPrefix
  if (sponsorship) {
    const { timestamp } = await publicClient.getBlock()
    sponsorPrefix = concatHex([
      toHex(timestamp + 300n, { size: 6 }), toHex(timestamp - 5n, { size: 6 }),
      toHex(maxCost, { size: 16 }), policyId, sponsorship.proxyHash,
    ])
    operation.paymaster = sponsorship.paymaster
    operation.paymasterData = concatHex([sponsorPrefix, toHex(0n, { size: 65 })])
    operation.paymasterVerificationGasLimit = sponsorGasLimit
    operation.paymasterPostOpGasLimit = 0n
  }
  const estimate = await request(bundlerRpc, 'eth_estimateUserOperationGas', [formatUserOperationRequest(operation), entryPoint])
  for (const field of ['callGasLimit', 'verificationGasLimit', 'preVerificationGas']) {
    operation[field] = BigInt(estimate[field])
    assert.ok(operation[field] > 0n, `Alto must return nonzero ${field}`)
  }
  if (sponsorship) {
    assert.equal(BigInt(estimate.paymasterPostOpGasLimit), 0n, 'The selected paymaster has no postOp')
    assert.ok(sponsorGasLimit >= BigInt(estimate.paymasterVerificationGasLimit), 'Raise the reviewed local sponsor gas cap')
    const sponsorHash = await read(sponsorship.paymaster, paymasterArtifact.abi, 'getSponsorshipHash', [toPackedUserOperation(operation)])
    operation.paymasterData = concatHex([sponsorPrefix, await sponsor.sign({ hash: sponsorHash })])
  } else {
    assert.ok(await gasFunds(account.address) >= maxPrefund(operation), 'ETH account must cover the prefund')
    assert.equal(toPackedUserOperation(operation).paymasterAndData, '0x')
  }
  const hash = getUserOperationHash({ entryPointAddress: entryPoint, entryPointVersion: '0.9', chainId: chain.id, userOperation: operation })
  assert.equal(await read(entryPoint, entryPoint09Abi, 'getUserOpHash', [toPackedUserOperation(operation)]), hash)
  operation.signature = await account.signUserOperation(operation)
  return { operation, hash }
}

function payerBalances(sender, paymaster) {
  return Promise.all([
    gasFunds(sender), read(entryPoint, entryPoint09Abi, 'balanceOf', [paymaster]),
  ])
}

async function submitOperation({ operation, hash }) {
  assert.equal(await request(bundlerRpc, 'eth_sendUserOperation', [formatUserOperationRequest(operation), entryPoint]), hash)
}

async function checkInclusion({ operation, hash }, paymaster, [gasBefore, paymasterBefore], transferred = 0n) {
  const { sender, nonce } = operation
  const receipt = await waitFor(() => request(bundlerRpc, 'eth_getUserOperationReceipt', [hash]), `Alto inclusion for ${hash}`)
  assert.equal(receipt.success, true, `Kernel call must succeed through Alto: ${hash}`)
  assert.equal(receipt.userOpHash, hash)
  const tx = await publicClient.getTransactionReceipt({ hash: receipt.receipt.transactionHash })
  const events = tx.logs.flatMap(log => {
    if (log.address.toLowerCase() !== entryPoint.toLowerCase()) return []
    try {
      const decoded = decodeEventLog({ abi: entryPoint09Abi, data: log.data, topics: log.topics })
      return decoded.eventName === 'UserOperationEvent' ? [decoded.args] : []
    } catch {
      return []
    }
  }).filter(event => event.userOpHash === hash)
  assert.equal(events.length, 1)
  const [event] = events
  assert.equal(getAddress(event.sender), getAddress(sender))
  assert.equal(event.nonce, nonce)
  assert.equal(event.success, true)
  assert.equal(getAddress(event.paymaster), getAddress(operation.paymaster ?? zeroAddress))
  assert.equal(event.actualGasCost, BigInt(receipt.actualGasCost))
  assert.ok(event.actualGasCost > 0n)
  const [gasAfter, paymasterAfter] = await payerBalances(sender, paymaster)
  assert.equal(gasBefore - gasAfter, transferred + (operation.paymaster ? 0n : event.actualGasCost))
  assert.equal(paymasterBefore - paymasterAfter, operation.paymaster ? event.actualGasCost : 0n)
  assert.equal(await read(entryPoint, entryPoint09Abi, 'getNonce', [sender, 0n]), nonce + 1n)
  return { actualGasCost: event.actualGasCost, blockNumber: tx.blockNumber }
}

async function sendViaAlto({ account, owner, target, paymaster, proxyHash, sponsored, first, priorCalls }) {
  const sender = account.address
  assert.equal(!(await publicClient.getCode({ address: sender })), first)
  const nonce = await account.getNonce()
  const record = encodeFunctionData({ abi: targetAbi, functionName: 'record' })
  const recordCall = { to: target, value: 0n, data: record }
  const callData = first ? singleCall(target, record) : encodeKernelCalls([recordCall, recordCall])
  const prepared = await prepareOperation({
    account, nonce, callData, first, fees: await feeQuote(),
    sponsorship: sponsored ? { paymaster, proxyHash } : undefined,
  })
  const balances = await payerBalances(sender, paymaster)
  await submitOperation(prepared)
  const { actualGasCost, blockNumber } = await checkInclusion(prepared, paymaster, balances)
  assert.equal(await read(target, targetAbi, 'calls', [sender]), priorCalls + (first ? 1n : 2n))
  await checkAccount(sender, owner, blockNumber, true)
  console.log(`${first ? 'First deployment' : 'Subsequent operation'} with ${sponsored ? 'sponsored gas' : 'account ETH'}: ${prepared.hash}, ${actualGasCost} wei`)
}

async function quoteUnfundedDeployment(account, target) {
  const sender = account.address
  assert.equal(await gasFunds(sender), 0n)
  assert.equal(await publicClient.getCode({ address: sender }), undefined)
  const fees = await feeQuote()
  assert.ok(fees.maxFeePerGas > 0n && fees.maxPriorityFeePerGas > 0n)
  const record = encodeFunctionData({ abi: targetAbi, functionName: 'record' })
  const operation = await estimationOperation({ account, nonce: 0n, callData: singleCall(target, record), first: true, fees })
  const params = [formatUserOperationRequest(operation), entryPoint]
  await assert.rejects(request(bundlerRpc, 'eth_estimateUserOperationGas', params), error => {
    assert.match(error.message, /AA21 didn't pay prefund/u)
    assert.match(error.message, /"code":-32500/u)
    console.log('Unfunded estimate response:', error.message)
    return true
  })
  const estimate = await request(bundlerRpc, 'eth_estimateUserOperationGas', [
    ...params, { [sender]: { balance: toHex(parseEther('0.2')) } },
  ])
  const prefund = maxPrefund({
    ...operation, callGasLimit: BigInt(estimate.callGasLimit), verificationGasLimit: BigInt(estimate.verificationGasLimit),
    preVerificationGas: BigInt(estimate.preVerificationGas),
  })
  assert.ok(prefund > 0n)
  assert.equal(await gasFunds(sender), 0n)
  assert.equal(await publicClient.getCode({ address: sender }), undefined)
  assert.equal(await account.getNonce(), 0n)
  console.log(`Advisory shortfall for the unfunded deployment: ${prefund} wei at the quoted gas price. Chain state is unchanged.`)
}

async function withdrawViaAlto({ account, owner, paymaster, proxyHash, amount, fromDeposit, sponsored }) {
  const sender = account.address
  const [liquidBefore, depositBefore, ownerBefore] = await Promise.all([
    publicClient.getBalance({ address: sender }), read(entryPoint, entryPoint09Abi, 'balanceOf', [sender]),
    publicClient.getBalance({ address: owner.address }),
  ])
  const callData = fromDeposit
    ? singleCall(entryPoint, encodeFunctionData({ abi: entryPoint09Abi, functionName: 'withdrawTo', args: [owner.address, amount] }))
    : singleCall(owner.address, '0x', amount)
  const prepared = await prepareOperation({
    account, nonce: await account.getNonce(), callData, first: false, fees: await feeQuote(),
    sponsorship: sponsored ? { paymaster, proxyHash } : undefined,
  })
  const prefund = sponsored ? 0n : maxPrefund(prepared.operation)
  if (fromDeposit) {
    assert.ok(depositBefore >= amount + prefund, 'Deposit withdrawal must leave the full validation prefund available')
  } else {
    assert.ok(liquidBefore >= amount + (prefund > depositBefore ? prefund - depositBefore : 0n))
  }
  const balances = await payerBalances(sender, paymaster)
  await submitOperation(prepared)
  const { actualGasCost, blockNumber } = await checkInclusion(prepared, paymaster, balances, amount)
  assert.equal(await publicClient.getBalance({ address: owner.address }), ownerBefore + amount)
  if (fromDeposit) {
    assert.equal(await read(entryPoint, entryPoint09Abi, 'balanceOf', [sender]), depositBefore - amount - (sponsored ? 0n : actualGasCost))
    assert.equal(await publicClient.getBalance({ address: sender }), liquidBefore)
  } else if (prefund <= depositBefore) {
    assert.equal(await publicClient.getBalance({ address: sender }), liquidBefore - amount)
  }
  await checkAccount(sender, owner, blockNumber, true)
  console.log(`Withdrew ${amount} wei from ${fromDeposit ? 'EntryPoint deposit' : 'liquid account ETH'} with ${sponsored ? 'sponsored gas' : 'account ETH'}: ${prepared.hash}, ${actualGasCost} wei gas`)
}

async function checkOutstanding(operation) {
  const outstanding = await request(bundlerRpc, 'debug_bundler_dumpMempool', [entryPoint])
  assert.equal(outstanding.length, 1)
  const [pending] = outstanding
  assert.equal(getAddress(pending.sender), getAddress(operation.sender))
  assert.equal(BigInt(pending.nonce), operation.nonce)
  assert.equal(pending.callData.toLowerCase(), operation.callData.toLowerCase())
  assert.equal(pending.signature.toLowerCase(), operation.signature.toLowerCase())
  assert.equal(BigInt(pending.maxFeePerGas), operation.maxFeePerGas)
  assert.equal(BigInt(pending.maxPriorityFeePerGas), operation.maxPriorityFeePerGas)
}

async function cancelViaAlto({ account, owner, target, paymaster, first }) {
  const sender = account.address
  assert.equal(!(await publicClient.getCode({ address: sender })), first)
  const nonce = await account.getNonce()
  const targetCalls = await read(target, targetAbi, 'calls', [sender])
  const startBlock = await publicClient.getBlockNumber({ cacheTime: 0 })
  const balances = await payerBalances(sender, paymaster)
  const record = encodeFunctionData({ abi: targetAbi, functionName: 'record' })
  const fees = await feeQuote()
  const original = await prepareOperation({
    account, nonce, callData: singleCall(target, record), first, fees,
  })
  await submitOperation(original)
  await checkOutstanding(original.operation)

  const bumpedFee = fee => (fee * 110n + 99n) / 100n
  const replacement = await prepareOperation({
    account, nonce, callData: singleCall(sender, '0x'), first,
    fees: {
      maxFeePerGas: bumpedFee(fees.maxFeePerGas),
      maxPriorityFeePerGas: bumpedFee(fees.maxPriorityFeePerGas),
    },
  })
  assert.notEqual(replacement.hash, original.hash)
  assert.equal(replacement.operation.factoryData, original.operation.factoryData)
  await submitOperation(replacement)
  await checkOutstanding(replacement.operation)

  assert.equal(await request(bundlerRpc, 'debug_bundler_sendBundleNow'), 'ok')
  const { actualGasCost, blockNumber } = await checkInclusion(replacement, paymaster, balances)
  assert.equal(await request(bundlerRpc, 'eth_getUserOperationReceipt', [original.hash]), null)
  const events = await publicClient.getContractEvents({
    address: entryPoint, abi: entryPoint09Abi, eventName: 'UserOperationEvent',
    args: { sender }, fromBlock: startBlock + 1n, strict: true,
  })
  assert.deepEqual(events.filter(event => event.args.nonce === nonce).map(event => event.args.userOpHash), [replacement.hash])
  assert.equal(await read(target, targetAbi, 'calls', [sender]), targetCalls)
  await checkAccount(sender, owner, blockNumber, true)
  console.log(`${first ? 'Counterfactual' : 'Deployed'} same-nonce cancellation: ${original.hash} → ${replacement.hash}, ${actualGasCost} wei paid by the account`)
}

try {
  await waitFor(() => request(chainRpc, 'eth_chainId'), 'local Anvil')
  assert.equal(BigInt(await request(chainRpc, 'eth_chainId')), BigInt(chain.id))
  assert.ok(await request(chainRpc, 'anvil_nodeInfo'), 'Qualification requires an isolated Anvil instance')
  for (const [address, code] of [
    [entryPoint, entryPointCode], [pins.senderCreator.address, senderCreatorCode], [pins.create2Deployer.address, deployerCode],
  ]) {
    await request(chainRpc, 'anvil_setCode', [address, code])
  }
  const hashes = {}
  for (const pin of pins.kernel.contracts) {
    const artifact = kernelRelease.contracts.find(c => c.name === pin.name)
    assert.ok(artifact, `Pinned release missing ${pin.name}`)
    assert.equal(keccak256(artifact.init_code), pin.initCodeHash)
    hashes[pin.name] = await deployCreate2(artifact.init_code, pin.address)
    assert.equal(hashes[pin.name], pin.codeHash)
  }
  assert.equal(await deployCreate2(validatorBytecode, validator), pins.validator.codeHash)
  assert.equal(getAddress(await read(factory, factoryAbi, 'UUPS')), implementation)

  const staker = (await deploy(stakerRelease.bytecode, stakerAbi, [utility.address])).contractAddress
  assert.ok(staker)
  const stakerCode = await publicClient.getCode({ address: staker })
  assert.ok(stakerCode && stakerCode !== '0x')
  accountProfile.staker = getAddress(staker)
  accountProfile.stakerCodeHash = keccak256(stakerCode)
  assert.equal(getAddress(await read(staker, stakerAbi, 'owner')), utility.address)
  await confirmed(await utilityWallet.writeContract({
    address: staker, abi: stakerAbi, functionName: 'approveFactory', args: [factory, true],
  }))
  await confirmed(await utilityWallet.writeContract({
    address: staker, abi: stakerAbi, functionName: 'stake', args: [entryPoint, 86_400], value: parseEther('1'),
  }))
  assert.equal(await read(staker, stakerAbi, 'approved', [factory]), true)
  const stakerInfo = await read(entryPoint, stakeManagerAbi, 'getDepositInfo', [staker])
  assert.equal(stakerInfo.staked, true)
  assert.ok(stakerInfo.stake >= parseEther('1') && stakerInfo.unstakeDelaySec >= 86_400)

  // Derive the proxy runtime hash from a sacrificial account with the same factory and implementation.
  const proxySample = await accountAddress(utility)
  const proxyReceipt = await confirmed(await utilityWallet.writeContract({ address: factory, abi: factoryAbi, functionName: 'deploy', args: [packagesFor(utility), 0n] }))
  const proxyHash = keccak256(await publicClient.getCode({ address: proxySample }))
  assert.equal(proxyHash, pins.kernel.proxyCodeHash)
  await checkAccount(proxySample, utility, proxyReceipt.blockNumber, true)
  const paymasterReceipt = await deploy(paymasterArtifact.bytecode.object, paymasterArtifact.abi, [
    entryPoint, utility.address, sponsor.address, maxCost, policyId, proxyHash, staker, factory, implementation, validator,
  ])
  const paymaster = paymasterReceipt.contractAddress
  assert.ok(paymaster)
  await confirmed(await utilityWallet.writeContract({ address: paymaster, abi: paymasterArtifact.abi, functionName: 'deposit', value: parseEther('2') }))
  await confirmed(await utilityWallet.writeContract({
    address: paymaster, abi: paymasterArtifact.abi, functionName: 'addStake', args: [86_400], value: parseEther('1'),
  }))
  const paymasterInfo = await read(entryPoint, stakeManagerAbi, 'getDepositInfo', [paymaster])
  assert.equal(paymasterInfo.staked, true)
  assert.ok(paymasterInfo.stake >= parseEther('1') && paymasterInfo.unstakeDelaySec >= 86_400)
  await confirmed(await utilityWallet.writeContract({ address: paymaster, abi: paymasterArtifact.abi, functionName: 'unpause' }))
  const target = (await deploy(`0x${targetContract.bin}`, [])).contractAddress
  assert.ok(target)
  console.log('Kernel runtime hashes:', JSON.stringify({ ...hashes, Staker: accountProfile.stakerCodeHash, proxy: proxyHash }))

  const env = {
    ALTO_RPC_URL: `http://host.docker.internal:${chainPort}`,
    ALTO_ENTRYPOINTS: entryPoint,
    ALTO_CHAIN_TYPE: 'default',
    ALTO_UTILITY_PRIVATE_KEY: toHex(utility.getHdKey().privateKey),
    ALTO_EXECUTOR_PRIVATE_KEYS: toHex(executor.getHdKey().privateKey),
    ALTO_DEPLOY_SIMULATIONS_CONTRACT: 'true',
    ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION: 'false',
    ALTO_SAFE_MODE: args['safe-mode'] ? 'true' : 'false',
    // The loopback qualification process uses manual bundling to stage replacements.
    ALTO_ENABLE_DEBUG_ENDPOINTS: 'true',
    ALTO_BUNDLE_MODE: 'auto',
    ALTO_SIMULATION_PAYMASTER_POST_OP_GAS_LIMIT: '0',
    ALTO_V7_PAYMASTER_POST_OP_GAS_LIMIT_MULTIPLIER: '0',
    ALTO_BLOCK_TIME: '1000',
    ALTO_LOG_LEVEL: 'warn',
  }
  console.log(`Alto qualification image: ${altoImage}; SafeMode: ${args['safe-mode'] === true}`)
  command('docker', ['run', '--detach', '--rm', '--pull', 'never', '--name', altoName,
    '-p', `127.0.0.1:${bundlerPort}:3000`, ...Object.keys(env).flatMap(key => ['--env', key]), altoImage],
  { env: { ...process.env, ...env } })
  altoStarted = true
  const supported = await waitFor(() => request(bundlerRpc, 'eth_supportedEntryPoints'), 'pinned Alto')
  assert.deepEqual(supported.map(getAddress), [getAddress(entryPoint)])

  const [paidOwner, sponsoredOwner] = owners
  const [paidAccount, sponsoredAccount] = await Promise.all([
    tradingAccount(paidOwner, 'external'), tradingAccount(sponsoredOwner, 'embedded'),
  ])
  const { address: paidSender } = paidAccount
  const { address: sponsoredSender } = sponsoredAccount
  assert.notEqual(getAddress(paidSender), getAddress(sponsoredSender))
  await quoteUnfundedDeployment(paidAccount, target)
  await fundAccount(paidSender, paidOwner)
  assert.equal(await paidAccount.getNonce(), 0n)
  console.log(`External owner funded the verified counterfactual Trading Account ${paidSender} without deploying it`)
  assert.equal(await gasFunds(sponsoredSender), 0n)
  const common = { target, paymaster, proxyHash }
  await sendViaAlto({ ...common, account: paidAccount, owner: paidOwner, sponsored: false, first: true, priorCalls: 0n })
  await sendViaAlto({ ...common, account: paidAccount, owner: paidOwner, sponsored: true, first: false, priorCalls: 1n })
  await sendViaAlto({ ...common, account: sponsoredAccount, owner: sponsoredOwner, sponsored: true, first: true, priorCalls: 0n })
  assert.equal(await gasFunds(sponsoredSender), 0n)
  await fundAccount(sponsoredSender, sponsoredOwner)
  await sendViaAlto({ ...common, account: sponsoredAccount, owner: sponsoredOwner, sponsored: false, first: false, priorCalls: 1n })
  for (const { owner, walletFamily, sponsored } of [
    { owner: localAccount(5), walletFamily: 'embedded', sponsored: false },
    { owner: localAccount(6), walletFamily: 'external', sponsored: true },
  ]) {
    const account = await tradingAccount(owner, walletFamily)
    if (!sponsored) await fundAccount(account.address, owner)
    else assert.equal(await gasFunds(account.address), 0n)
    await sendViaAlto({ ...common, account, owner, sponsored, first: true, priorCalls: 0n })
  }
  console.log(`Qualified both gas modes with each wallet boundary and both payer switches through Alto; SafeMode: ${args['safe-mode'] === true}`)

  const withdrawal = { ...common, account: paidAccount, owner: paidOwner }
  await confirmed(await utilityWallet.writeContract({ address: entryPoint, abi: entryPoint09Abi, functionName: 'depositTo', args: [paidSender], value: parseEther('0.02') }))
  await confirmed(await utilityWallet.writeContract({ address: paymaster, abi: paymasterArtifact.abi, functionName: 'pause' }))
  assert.equal(await read(paymaster, paymasterArtifact.abi, 'paused'), true)
  await withdrawViaAlto({ ...withdrawal, amount: parseEther('0.025'), fromDeposit: false, sponsored: false })
  await withdrawViaAlto({ ...withdrawal, amount: parseEther('0.01'), fromDeposit: true, sponsored: false })
  await confirmed(await utilityWallet.writeContract({ address: paymaster, abi: paymasterArtifact.abi, functionName: 'unpause' }))
  const remainingDeposit = await read(entryPoint, entryPoint09Abi, 'balanceOf', [paidSender])
  await withdrawViaAlto({ ...withdrawal, amount: remainingDeposit, fromDeposit: true, sponsored: true })
  assert.equal(await read(entryPoint, entryPoint09Abi, 'balanceOf', [paidSender]), 0n)

  assert.equal(await request(bundlerRpc, 'debug_bundler_setBundlingMode', ['manual']), 'ok')
  await cancelViaAlto({ ...common, account: paidAccount, owner: paidOwner, first: false })
  const counterfactualAccount = await tradingAccount(counterfactualOwner, 'external')
  await fundAccount(counterfactualAccount.address)
  await cancelViaAlto({ ...common, account: counterfactualAccount, owner: counterfactualOwner, first: true })
} catch (error) {
  if (altoStarted) {
    try { console.error(command('docker', ['logs', '--tail', '50', altoName])) } catch { /* preserve qualification error */ }
  }
  throw error
} finally {
  if (altoStarted) {
    try { command('docker', ['rm', '-f', altoName]) } catch { /* an exited --rm container is already removed */ }
  }
  anvil.kill()
}
