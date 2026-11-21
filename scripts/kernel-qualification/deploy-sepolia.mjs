#!/usr/bin/env node
// Fresh Arbitrum Sepolia Kernel canary. The default action only reads chain state.
import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'
import { existsSync, readFileSync } from 'node:fs'
import { createRequire } from 'node:module'
import { resolve } from 'node:path'
import { parseArgs } from 'node:util'
import { setTimeout as sleep } from 'node:timers/promises'
import { reconcileExternalNonces, verifyRecordedExternalNonces } from './external-nonce-reconciliation.mjs'
import {
  completeMinedDeployment, deploymentFeeFields, fingerprintDeploymentRequest, readMinedDeployment,
  rebroadcastRecordedDeployment, signDeploymentTransaction,
} from './deployment-intent.mjs'
import { scrubRpcError, writePublicEvidence } from './local-safety.mjs'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const {
  concatHex, createPublicClient, createWalletClient, encodeDeployData, encodeFunctionData, getAddress,
  getContractAddress, http, isAddressEqual, keccak256, parseAbi, parseEther, zeroAddress,
  zeroHash,
} = require('viem')
const { privateKeyToAccount } = require('viem/accounts')
const { arbitrumSepolia } = require('viem/chains')
const pins = JSON.parse(readFileSync(new URL('./pins.json', import.meta.url)))

// This exact bytecode is the Staker-aware Core artifact built from commit 469c40dd5757041dd57cafc35d0feb01d77a1aa2.
// The deployed code includes constructor immutables; attest its runtime hash and every immutable on both RPCs.
const paymasterCreationCodeHash = '0xb81b8d5409186c3a0e8faaa1de5ebb3e8d91ffc220b79f087c15f63e68743125'
const paymasterRuntimeTemplateHash = '0x8400664934f2993e97adb86aa8ff7c465533432cb2f9b0770b01e58603b1c4a3'
const paymasterImmutableLayoutHash = '0x382813125aaaae6c50ea0f9762497906b261252c168f53caf217d0fe5f88fed5'
const corePaymasterCommit = '469c40dd5757041dd57cafc35d0feb01d77a1aa2'
const corePaymasterSourceHash = '0xa94db3722ad1222d79128aa1046a786141b3698c42121ee0bb34242417a77034'
const stakerCreationCodeHash = '0x89f185a3ea592ce85bf90f573b1864317505fa4d8d0906098645e1182f15b216'
const stakerReleaseInitCodeHash = '0x15ffa4371818c1b8e2ce98d157e1d6c0c2e230c7d473af14ddb621cd4cb09bae'
const stakerRuntimeCodeHash = '0xc8c723ed62cabf8190cb76172e38d6d6c2e0b2b4d031fdcb89543d9b2682ced5'
const simulationCreationCodeHashes = {
  entryPoint09: '0xf14a39f60bb7cad29bee4f2e4459ce0ab412e2d8f29205575881cf2cf500a937',
  pimlico: '0xfdb3e336f78a7bcb01c7b3d1dd930f939e15ea17f00b763eda06700687c6f94d',
}
const entryPoint09RuntimeTemplateHash = '0xa53cc2557e16052adcde46993831707e5f24a13dade5758767ac6b9c601a89b8'
const entryPoint09ImmutableLayoutHash = '0x934f7e7f598823c4467ac1d57ea973c4bb9ba637134318ec020d55e0b9afb577'
const pimlicoSimulationRuntimeHash = '0xbf6f6629d08dd42ef5af9e7ba6ff5e1e7b53083f035d2f457fd8d4561182fcbd'
const minStake = parseEther('1')
const unstakeDelay = 86_400
const gasReserve = parseEther('0.5')
const sharedOrderRouterSigner = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
const factoryAbi = parseAbi([
  'struct Install { uint256 moduleType; address module; bytes moduleData; bytes internalData; }',
  'function UUPS() view returns (address)',
  'function getAddress(Install[] initialPackages,uint256 nonce) view returns (address)',
  'function deploy(Install[] initialPackages,uint256 nonce) payable returns (address)',
])
const stakerAbi = parseAbi([
  'constructor(address owner)',
  'function owner() view returns (address)',
  'function approved(address factory) view returns (bool)',
  'function approveFactory(address factory,bool approval) payable',
  'function stake(address entryPoint,uint32 unstakeDelay) payable',
])
const entryPointAbi = parseAbi([
  'function getDepositInfo(address account) view returns ((uint112 deposit,bool staked,uint112 stake,uint32 unstakeDelaySec,uint48 withdrawTime))',
  'function senderCreator() view returns (address)',
])
const validatorAbi = parseAbi(['function ecdsaValidatorStorage(address account) view returns (address)'])
const accountAbi = parseAbi(['function root() view returns (bytes21)'])
const implementationSlot = '0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc'
const beaconSlot = '0xa3f0ad74e5423aebfd80d3ef4346578335a9a72aeaee59ff6cb3582b35133d50'
const rootSlot = '0xded5d420c407eac3c615e6abe13ab4a0bd7173e5045ea543765b46f0df6e260c'
let phase = 'configuration'

function usage() {
  console.log(`Usage: node scripts/kernel-qualification/deploy-sepolia.mjs --config /path/to/public-config.json [--manifest /path/to/public-manifest.json] [--check-artifacts | --attest | --reconcile-inflight --confirm DEPLOYMENT | --rebroadcast-inflight --confirm DEPLOYMENT | --reconcile-external-nonces --external-tx HASH ... --confirm DEPLOYMENT | --broadcast --confirm DEPLOYMENT]

Default: verify pinned artifacts and independent safe-block chain state, then print a read-only plan.
--check-artifacts: verify pinned source/build inputs without any RPC call.
--attest: verify an existing manifest on two RPCs, with no transactions.
--reconcile-inflight: record a known deployment transaction's canonical result on two RPCs; never signs or broadcasts.
--rebroadcast-inflight: sign and send exactly the recorded transaction using the dedicated signer and the same untouched nonce.
--reconcile-external-nonces: verify explicit, contiguous unrelated Perps order hashes on two RPCs; requires confirmation but never signs or broadcasts.
--broadcast: deploy missing contracts, approve/stake, and attest; requires explicit deployment confirmation and KERNEL_CANARY_DEPLOYER_PRIVATE_KEY.
The config and manifest contain public values only. See deploy-sepolia.md.`)
}

function command(file, args, options = {}) {
  const { PATH, TMPDIR } = process.env
  return execFileSync(file, args, { encoding: 'utf8', maxBuffer: 12 * 1024 * 1024, ...options, env: { PATH, TMPDIR } })
}

function checkedAddress(value, field) {
  assert.equal(typeof value, 'string', `${field} must be an address`)
  const address = getAddress(value)
  assert.notEqual(address, zeroAddress, `${field} must not be zero`)
  return address
}

function checkedHash(value, field) {
  assert.match(value ?? '', /^0x[0-9a-fA-F]{64}$/u, `${field} must be 32 bytes`)
  assert.notEqual(value.toLowerCase(), zeroHash, `${field} must not be zero`)
  return value.toLowerCase()
}

function loadConfig(path) {
  const c = JSON.parse(readFileSync(resolve(path), 'utf8'))
  assert.match(c.deployment ?? '', /^[a-z0-9][a-z0-9._-]{0,95}$/u, 'deployment must be a stable public id')
  const primary = new URL(c.primaryRpc)
  const secondaryRpc = process.env.KERNEL_SECONDARY_RPC_URL || c.secondaryRpc
  assert.ok(secondaryRpc, 'Provide a second RPC through KERNEL_SECONDARY_RPC_URL or secondaryRpc')
  const secondary = new URL(secondaryRpc)
  assert.equal(primary.protocol, 'https:', 'Primary RPC must use HTTPS')
  assert.equal(secondary.protocol, 'https:', 'Secondary RPC must use HTTPS')
  assert.notEqual(primary.hostname, secondary.hostname, 'RPCs must have independent hostnames')
  const deposit = BigInt(c.paymasterDepositWei ?? parseEther('2'))
  const maxSponsoredCost = BigInt(c.maxSponsoredCostWei)
  assert.ok(deposit > 0n && maxSponsoredCost > 0n, 'Deposit and maximum sponsored cost must be positive')
  assert.ok(deposit >= maxSponsoredCost, 'Initial paymaster deposit must cover the maximum sponsored cost')
  const config = {
    ...c,
    secondaryRpc,
    deployer: checkedAddress(c.deployer, 'deployer'),
    sponsorSigner: checkedAddress(c.sponsorSigner, 'sponsorSigner'),
    sampleOwner: checkedAddress(c.sampleOwner, 'sampleOwner'),
    externalCanaryOwner: c.externalCanaryOwner ? checkedAddress(c.externalCanaryOwner, 'externalCanaryOwner') : undefined,
    privyCanaryOwner: c.privyCanaryOwner ? checkedAddress(c.privyCanaryOwner, 'privyCanaryOwner') : undefined,
    altoUtilityAddress: checkedAddress(c.altoUtilityAddress, 'altoUtilityAddress'),
    policyId: checkedHash(c.policyId, 'policyId'),
    paymasterDepositWei: deposit,
    maxSponsoredCostWei: maxSponsoredCost,
    primaryHost: primary.hostname,
    secondaryHost: secondary.hostname,
  }
  const signers = [config.deployer, config.sampleOwner, config.externalCanaryOwner, config.privyCanaryOwner,
    config.sponsorSigner, config.altoUtilityAddress].filter(Boolean)
  assert.equal(new Set(signers.map(address => address.toLowerCase())).size, signers.length,
    'Canary signer roles must use distinct addresses')
  return config
}

function loadPinnedArtifacts(c) {
  assert.equal(require('viem/package.json').version, pins.viemVersion, 'Use the locked Viem version')
  const kernelRepo = resolve(c.kernelRepo)
  const validatorRepo = resolve(c.validatorRepo)
  assert.equal(command('git', ['-C', kernelRepo, 'rev-parse', 'HEAD']).trim(), pins.kernel.commit, 'Kernel checkout must match the pinned commit')
  assert.equal(command('git', ['-C', validatorRepo, 'rev-parse', 'HEAD']).trim(), pins.validator.commit, 'Validator checkout must match the pinned commit')
  for (const repo of [kernelRepo, validatorRepo]) {
    assert.equal(command('git', ['-C', repo, 'status', '--porcelain', '--untracked-files=no']).trim(), '', 'Pinned source checkout has tracked changes')
  }
  const release = JSON.parse(command('git', ['-C', kernelRepo, 'show', `${pins.kernel.commit}:${pins.kernel.release}`]))
  assert.equal(getAddress(release.create2.factory), getAddress(pins.create2Deployer.address))
  assert.equal(release.create2.salt, zeroHash)
  const artifacts = new Map()
  const staker = release.contracts.find(item => item.name === 'Staker')
  assert.ok(staker?.bytecode?.startsWith('0x') && staker?.init_code?.startsWith('0x'), 'Pinned release missing Staker creation code')
  assert.equal(keccak256(staker.bytecode), stakerCreationCodeHash, 'Generic Staker creation code changed')
  assert.equal(keccak256(staker.init_code), stakerReleaseInitCodeHash, 'Release Staker initialization code changed')
  artifacts.set('Staker', staker)
  for (const pin of pins.kernel.contracts) {
    const artifact = release.contracts.find(item => item.name === pin.name)
    assert.ok(artifact, `Pinned Kernel release missing ${pin.name}`)
    assert.equal(getAddress(artifact.expected_address), getAddress(pin.address))
    assert.equal(keccak256(artifact.init_code), pin.initCodeHash, `${pin.name} creation code changed`)
    assert.equal(getContractAddress({ bytecode: artifact.init_code, from: pins.create2Deployer.address, salt: zeroHash, opcode: 'CREATE2' }), getAddress(pin.address))
    artifacts.set(pin.name, artifact)
  }
  const solc = resolve(c.solc)
  assert.ok(command(solc, ['--version']).includes(pins.validator.solc), 'Use pinned Solidity 0.8.30 compiler')
  const source = 'src/validators/ECDSAValidator.sol'
  const compiled = JSON.parse(command(solc, [
    '--base-path', '.', '--via-ir', '--optimize', '--optimize-runs', '20000', '--evm-version', 'prague',
    '--combined-json', 'bin,bin-runtime',
    'solady/=dependencies/solady-0.1.26/src/',
    'account-abstraction/=dependencies/eth-infinitism-account-abstraction-0.8.0/contracts/',
    source,
  ], { cwd: validatorRepo }))
  const validator = compiled.contracts[`${source}:ECDSAValidator`]
  assert.ok(validator, 'Pinned validator compiler output missing')
  const validatorCode = `0x${validator.bin}`
  assert.equal(keccak256(validatorCode), pins.validator.initCodeHash)
  assert.equal(keccak256(`0x${validator['bin-runtime']}`), pins.validator.codeHash)
  assert.equal(getContractAddress({ bytecode: validatorCode, from: pins.create2Deployer.address, salt: zeroHash, opcode: 'CREATE2' }), getAddress(pins.validator.address))
  const coreRepo = resolve(c.coreRepo)
  assert.equal(command('git', ['-C', coreRepo, 'rev-parse', 'HEAD']).trim(), corePaymasterCommit,
    'Core checkout must match the reviewed Staker-aware paymaster commit')
  const coreSource = readFileSync(resolve(coreRepo, 'packages/perps-aa/src/PletherKernelVerifyingPaymaster.sol'))
  assert.equal(keccak256(`0x${coreSource.toString('hex')}`), corePaymasterSourceHash,
    'Core paymaster source differs from reviewed commit')
  const paymaster = JSON.parse(readFileSync(resolve(c.paymasterArtifact), 'utf8'))
  assert.equal(paymaster.metadata.compiler.version, '0.8.35+commit.47b9dedd')
  assert.equal(paymaster.metadata.settings.compilationTarget['src/PletherKernelVerifyingPaymaster.sol'], 'PletherKernelVerifyingPaymaster')
  assert.equal(paymaster.metadata.sources['src/PletherKernelVerifyingPaymaster.sol'].keccak256, corePaymasterSourceHash,
    'Core artifact does not correspond to the reviewed source')
  assert.equal(keccak256(paymaster.bytecode.object), paymasterCreationCodeHash, 'Core Staker-aware paymaster artifact changed')
  assert.equal(keccak256(paymaster.deployedBytecode.object), paymasterRuntimeTemplateHash, 'Core paymaster runtime template changed')
  assert.equal(keccak256(`0x${Buffer.from(JSON.stringify(paymaster.deployedBytecode.immutableReferences ?? {})).toString('hex')}`),
    paymasterImmutableLayoutHash, 'Core paymaster immutable reference layout changed')
  assert.deepEqual(paymaster.bytecode.linkReferences, {}, 'Paymaster must have no unresolved libraries')
  const simulations = {}
  for (const [name, path] of [
    ['entryPoint09', c.entryPoint09SimulationArtifact], ['pimlico', c.pimlicoSimulationArtifact],
  ]) {
    const artifact = JSON.parse(readFileSync(resolve(path), 'utf8'))
    assert.equal(artifact.metadata.compiler.version, '0.8.28+commit.7893614a', `${name} simulator compiler changed`)
    assert.equal(keccak256(artifact.bytecode.object), simulationCreationCodeHashes[name], `${name} simulator creation code changed`)
    assert.deepEqual(artifact.bytecode.linkReferences, {}, `${name} simulator has unresolved libraries`)
    assert.match(artifact.deployedBytecode.object, /^0x[0-9a-fA-F]+$/u, `${name} runtime template is invalid`)
    simulations[name] = artifact
  }
  assert.equal(keccak256(simulations.pimlico.deployedBytecode.object), pimlicoSimulationRuntimeHash)
  assert.equal(keccak256(simulations.entryPoint09.deployedBytecode.object), entryPoint09RuntimeTemplateHash)
  assert.equal(keccak256(`0x${Buffer.from(JSON.stringify(simulations.entryPoint09.deployedBytecode.immutableReferences ?? {})).toString('hex')}`),
    entryPoint09ImmutableLayoutHash, 'EntryPoint 0.9 simulation immutable reference layout changed')
  return { artifacts, validatorCode, paymaster, simulations }
}

function simulationAddresses(c, artifacts) {
  // Match Alto's deploySimulationsContract.ts salt exactly, while using the separate contract deployer EOA.
  const salt = keccak256(c.altoUtilityAddress)
  const addresses = {}
  for (const name of ['entryPoint09', 'pimlico']) {
    addresses[name] = getContractAddress({
      bytecode: artifacts.simulations[name].bytecode.object, from: pins.create2Deployer.address,
      salt, opcode: 'CREATE2',
    })
  }
  return { salt, addresses }
}

function ownerAddressesToCheck(c) {
  return [
    ['Dedicated deployer', c.deployer],
    ['Throwaway sample owner', c.sampleOwner],
    ['External canary owner', c.externalCanaryOwner],
    ['Privy canary owner', c.privyCanaryOwner],
    ['Alto utility signer', c.altoUtilityAddress],
    ['KMS sponsor signer', c.sponsorSigner],
  ].filter(([, address]) => address)
}

function matchesRuntimeTemplate(actual, artifact) {
  const expected = artifact.deployedBytecode.object.toLowerCase()
  const code = actual.toLowerCase()
  if (code.length !== expected.length) return false
  const refs = Object.values(artifact.deployedBytecode.immutableReferences ?? {}).flat()
  const ignore = new Uint8Array((code.length - 2) / 2)
  for (const ref of refs) {
    assert.ok(Number.isSafeInteger(ref.start) && Number.isSafeInteger(ref.length))
    assert.ok(ref.start >= 0 && ref.length > 0 && ref.start + ref.length <= ignore.length)
    ignore.fill(1, ref.start, ref.start + ref.length)
  }
  for (let i = 0; i < ignore.length; i++) {
    if (ignore[i] === 0 && code.slice(2 + i * 2, 4 + i * 2) !== expected.slice(2 + i * 2, 4 + i * 2)) return false
  }
  return true
}

function deploymentBinding(c) {
  return {
    deployer: c.deployer, sampleOwner: c.sampleOwner, altoUtilityAddress: c.altoUtilityAddress,
    sponsorSigner: c.sponsorSigner, policyId: c.policyId,
    maxSponsoredCostWei: String(c.maxSponsoredCostWei), paymasterDepositWei: String(c.paymasterDepositWei),
    corePaymasterCommit, corePaymasterSourceHash, paymasterCreationCodeHash,
  }
}

function loadManifest(path, c) {
  if (!existsSync(path)) return { schemaVersion: 1, deployment: c.deployment, chainId: pins.chainId,
    binding: deploymentBinding(c), transactions: [] }
  const manifest = JSON.parse(readFileSync(path, 'utf8'))
  assert.equal(manifest.schemaVersion, 1)
  assert.equal(manifest.deployment, c.deployment)
  assert.equal(manifest.chainId, pins.chainId)
  assert.deepEqual(manifest.binding, deploymentBinding(c), 'Manifest deployment binding differs from config')
  assert.ok(Array.isArray(manifest.transactions))
  return manifest
}

function saveManifest(path, manifest) {
  // Public evidence only. Never write a signer key, RPC URL, auth token, or full config.
  writePublicEvidence(path, `${JSON.stringify(manifest, null, 2)}\n`)
}

function makeClients(c) {
  const primary = createPublicClient({ chain: arbitrumSepolia, transport: http(c.primaryRpc, { timeout: 10_000 }) })
  const secondary = createPublicClient({ chain: arbitrumSepolia, transport: http(c.secondaryRpc, { timeout: 10_000 }) })
  return { primary, secondary }
}

async function commonSafeBlock(clients, atLeast = 0n, timeoutMs = 15_000) {
  const deadline = Date.now() + timeoutMs
  while (Date.now() < deadline) {
    const [primaryChain, secondaryChain, primarySafe, secondarySafe] = await Promise.all([
      clients.primary.getChainId(), clients.secondary.getChainId(),
      clients.primary.getBlock({ blockTag: 'safe' }), clients.secondary.getBlock({ blockTag: 'safe' }),
    ])
    assert.equal(primaryChain, pins.chainId, 'Primary RPC has the wrong chain ID')
    assert.equal(secondaryChain, pins.chainId, 'Secondary RPC has the wrong chain ID')
    const number = primarySafe.number < secondarySafe.number ? primarySafe.number : secondarySafe.number
    if (number >= atLeast) {
      const [a, b] = await Promise.all([
        clients.primary.getBlock({ blockNumber: number }), clients.secondary.getBlock({ blockNumber: number }),
      ])
      const hash = checkedHash(a.hash, 'Primary safe block hash')
      assert.equal(hash, checkedHash(b.hash, 'Secondary safe block hash'), 'Independent RPCs disagree on the safe block hash')
      return { number, hash }
    }
    await sleep(3_000)
  }
  throw new Error('Timed out waiting for both RPCs to reach the required safe block')
}

async function cleanLiveDeployerNonce(clients, deployer, expected) {
  const deadline = Date.now() + (expected === undefined ? 0 : 30_000)
  do {
    const counts = await Promise.all([clients.primary, clients.secondary].flatMap(client => [
      client.getTransactionCount({ address: deployer, blockTag: 'latest' }),
      client.getTransactionCount({ address: deployer, blockTag: 'pending' }),
    ]))
    const target = expected ?? counts[0]
    if (expected !== undefined) {
      assert.ok(counts.every(nonce => nonce <= target), 'Deployer nonce advanced outside the canary')
    }
    if (counts.every(nonce => nonce === target)) return target
    if (expected !== undefined) await sleep(1_000)
  } while (Date.now() < deadline)
  throw new Error('Deployer nonce is pending or has not converged across both RPCs')
}

async function codeHashAt(clients, address, blockNumber) {
  const [a, b] = await Promise.all([
    clients.primary.getCode({ address, blockNumber }), clients.secondary.getCode({ address, blockNumber }),
  ])
  assert.equal(a ?? '0x', b ?? '0x', `Independent RPCs disagree on code at ${address}`)
  return !a || a === '0x' ? undefined : keccak256(a)
}

async function verifySimulation(clients, artifacts, name, address, blockNumber, required = false) {
  const [a, b] = await Promise.all([
    clients.primary.getCode({ address, blockNumber }), clients.secondary.getCode({ address, blockNumber }),
  ])
  assert.equal(a ?? '0x', b ?? '0x', `${name} simulation code differs between RPCs`)
  if (!a || a === '0x') {
    assert.equal(required, false, `${name} simulation has no code`)
    return undefined
  }
  const hash = keccak256(a)
  if (name === 'pimlico') assert.equal(hash, pimlicoSimulationRuntimeHash, 'Pimlico simulation runtime hash mismatch')
  else assert.ok(matchesRuntimeTemplate(a, artifacts.simulations[name]), 'EntryPoint 0.9 simulation runtime differs from pinned Alto artifact outside immutables')
  return hash
}

async function simulationStatus(clients, c, artifacts, blockNumber) {
  const { addresses } = simulationAddresses(c, artifacts)
  const status = {}
  for (const name of ['entryPoint09', 'pimlico']) {
    const hash = await verifySimulation(clients, artifacts, name, addresses[name], blockNumber)
    status[name] = { address: addresses[name], codeHash: hash ?? null, state: hash ? 'already deployed' : 'missing' }
  }
  return status
}

async function requireHash(clients, address, expected, blockNumber, label) {
  const actual = await codeHashAt(clients, address, blockNumber)
  assert.equal(actual?.toLowerCase(), expected.toLowerCase(), `${label} code hash mismatch or no code at ${address}`)
}

async function statusOfPinnedContracts(clients, blockNumber) {
  for (const [name, pin] of [
    ['EntryPoint 0.9', pins.entryPoint], ['SenderCreator', pins.senderCreator], ['CREATE2 deployer', pins.create2Deployer],
  ]) await requireHash(clients, getAddress(pin.address), pin.codeHash, blockNumber, name)
  const creators = await Promise.all([clients.primary, clients.secondary].map(client =>
    client.readContract({ address: pins.entryPoint.address, abi: entryPointAbi, functionName: 'senderCreator', blockNumber })))
  assert.ok(creators.every(creator => isAddressEqual(creator, pins.senderCreator.address)), 'EntryPoint SenderCreator differs from pin')
  const status = {}
  for (const pin of [...pins.kernel.contracts, { name: 'ECDSAValidator', ...pins.validator }]) {
    const actual = await codeHashAt(clients, getAddress(pin.address), blockNumber)
    assert.ok(!actual || actual.toLowerCase() === pin.codeHash.toLowerCase(), `${pin.name} has unexpected code at the pinned address`)
    status[pin.name] = actual ? 'already deployed' : 'missing'
  }
  return status
}

async function requireUndeployedOwner(clients, address, blockNumber, label) {
  assert.equal(await codeHashAt(clients, address, blockNumber), undefined, `${label} must be an undeployed EOA`)
}

async function verifyStake(clients, address, blockNumber, label, minDeposit = 0n) {
  const [info, secondaryInfo] = await Promise.all([clients.primary, clients.secondary].map(client => client.readContract({
    address: pins.entryPoint.address, abi: entryPointAbi, functionName: 'getDepositInfo', args: [address], blockNumber,
  })))
  assert.deepEqual(info, secondaryInfo, `${label} stake differs between RPCs`)
  assert.equal(info.staked, true, `${label} is not staked`)
  assert.ok(info.stake >= minStake && info.unstakeDelaySec >= unstakeDelay, `${label} stake must be at least 1 ETH for one day`)
  assert.ok(info.deposit >= minDeposit, `${label} deposit below the configured floor`)
  return info
}

async function verifySample(clients, c, blockNumber, expected) {
  const factory = getAddress(pins.kernel.contracts.find(x => x.name === 'KernelFactory').address)
  const implementation = getAddress(pins.kernel.contracts.find(x => x.name === 'KernelUUPS').address)
  const packages = [{ moduleType: 1n, module: getAddress(pins.validator.address), moduleData: c.sampleOwner, internalData: '0x' }]
  const [account, independentAccount] = await Promise.all([clients.primary, clients.secondary].map(client =>
    client.readContract({ address: factory, abi: factoryAbi, functionName: 'getAddress', args: [packages, 0n], blockNumber })))
  assert.ok(isAddressEqual(account, independentAccount), 'Independent RPCs disagree on the sample address')
  if (expected) assert.ok(isAddressEqual(account, expected), 'Sample account address changed')
  await requireHash(clients, account, pins.kernel.proxyCodeHash, blockNumber, 'sample Kernel proxy')
  await Promise.all([clients.primary, clients.secondary].map(async client => {
    const [implSlot, beacon, root, validatorOwner, storedRoot] = await Promise.all([
      client.getStorageAt({ address: account, slot: implementationSlot, blockNumber }),
      client.getStorageAt({ address: account, slot: beaconSlot, blockNumber }),
      client.readContract({ address: account, abi: accountAbi, functionName: 'root', blockNumber }),
      client.readContract({ address: pins.validator.address, abi: validatorAbi, functionName: 'ecdsaValidatorStorage', args: [account], blockNumber }),
      client.getStorageAt({ address: account, slot: rootSlot, blockNumber }),
    ])
    assert.equal(implSlot?.toLowerCase(), `0x${'0'.repeat(24)}${implementation.slice(2).toLowerCase()}`, 'Sample proxy implementation mismatch')
    assert.equal(beacon?.toLowerCase(), zeroHash, 'Sample proxy beacon must be empty')
    assert.equal(root.toLowerCase(), `0x01${pins.validator.address.slice(2).toLowerCase()}`, 'Sample root validator mismatch')
    assert.ok(isAddressEqual(validatorOwner, c.sampleOwner), 'Sample validator signer mismatch')
    assert.equal(storedRoot?.toLowerCase(), `0x${'0'.repeat(22)}01${pins.validator.address.slice(2).toLowerCase()}`, 'Sample root storage mismatch')
  }))
  return account
}

function paymasterConstructorArgs(c, manifest) {
  const factory = getAddress(pins.kernel.contracts.find(x => x.name === 'KernelFactory').address)
  const implementation = getAddress(pins.kernel.contracts.find(x => x.name === 'KernelUUPS').address)
  return [pins.entryPoint.address, c.deployer, c.sponsorSigner, c.maxSponsoredCostWei, c.policyId,
    pins.kernel.proxyCodeHash, manifest.staker, factory, implementation, pins.validator.address]
}

function deploymentRequest(c, manifest, artifacts, name, depositValue) {
  const create2 = data => ({ to: getAddress(pins.create2Deployer.address), data })
  const call = (to, abi, functionName, args = [], value = 0n) => ({
    to: checkedAddress(to, name), data: encodeFunctionData({ abi, functionName, args }), value,
  })
  const factory = getAddress(pins.kernel.contracts.find(item => item.name === 'KernelFactory').address)
  const pinnedArtifact = pins.kernel.contracts.find(item => name === `Deploy ${item.name}`)
  if (pinnedArtifact) return create2(concatHex([zeroHash, artifacts.artifacts.get(pinnedArtifact.name).init_code]))
  if (name === 'Deploy ECDSAValidator') return create2(concatHex([zeroHash, artifacts.validatorCode]))
  for (const simulation of ['entryPoint09', 'pimlico']) {
    if (name === `Deploy ${simulation} simulation`) {
      return create2(concatHex([simulationAddresses(c, artifacts).salt, artifacts.simulations[simulation].bytecode.object]))
    }
  }
  switch (name) {
    case 'Deploy Plether Staker':
      return { data: encodeDeployData({ abi: stakerAbi, bytecode: artifacts.artifacts.get('Staker').bytecode, args: [c.deployer] }) }
    case 'Approve Kernel factory':
      return call(manifest.staker, stakerAbi, 'approveFactory', [factory, true])
    case 'Stake Plether Staker':
      return call(manifest.staker, stakerAbi, 'stake', [pins.entryPoint.address, unstakeDelay], minStake)
    case 'Deploy sample Kernel proxy': {
      const packages = [{ moduleType: 1n, module: getAddress(pins.validator.address), moduleData: c.sampleOwner, internalData: '0x' }]
      return call(factory, factoryAbi, 'deploy', [packages, 0n])
    }
    case 'Deploy paused Core paymaster':
      return { data: encodeDeployData({ abi: artifacts.paymaster.abi, bytecode: artifacts.paymaster.bytecode.object,
        args: paymasterConstructorArgs(c, manifest) }) }
    case 'Deposit Core paymaster':
      assert.ok(depositValue > 0n && depositValue <= c.paymasterDepositWei, 'Unexpected reviewed paymaster deposit amount')
      return call(manifest.paymaster, artifacts.paymaster.abi, 'deposit', [], depositValue)
    case 'Stake Core paymaster':
      return call(manifest.paymaster, artifacts.paymaster.abi, 'addStake', [unstakeDelay], minStake)
    default:
      throw new Error('In-flight record names an unknown deployment action')
  }
}

async function recoverDeployment(clients, c, manifest, artifacts) {
  const intent = manifest.inFlight
  assert.ok(intent?.hash, 'An intent without a recorded hash requires external nonce reconciliation or operator review')
  const receipt = await readMinedDeployment({
    clients, manifest, intent, awaitSafeBlock: (rpcClients, block) => commonSafeBlock(rpcClients, block, 900_000),
    expectedRequest: async (name, blockNumber) => {
      let depositValue
      if (name === 'Deposit Core paymaster') {
        if (intent.request) depositValue = BigInt(intent.request.valueWei)
        else {
          const [previous, independent] = await Promise.all([clients.primary, clients.secondary].map(client => client.readContract({
            address: pins.entryPoint.address, abi: entryPointAbi, functionName: 'getDepositInfo',
            args: [manifest.paymaster], blockNumber: blockNumber - 1n,
          })))
          assert.deepEqual(previous, independent, 'Independent RPCs disagree on the prior paymaster deposit')
          depositValue = c.paymasterDepositWei - previous.deposit
        }
      }
      return fingerprintDeploymentRequest(deploymentRequest(c, manifest, artifacts, name, depositValue))
    },
  })
  return { manifest: completeMinedDeployment(manifest, receipt), status: receipt.status, hash: intent.hash }
}

async function verifyDeploymentTransaction(clients, manifest, name, address, expectedInput, safeBlock) {
  const record = manifest.transactions.findLast(item => item.name === name)
  assert.ok(record, `${name} deployment transaction is missing from manifest`)
  const [receiptA, receiptB, txA, txB] = await Promise.all([
    clients.primary.getTransactionReceipt({ hash: record.hash }),
    clients.secondary.getTransactionReceipt({ hash: record.hash }),
    clients.primary.getTransaction({ hash: record.hash }),
    clients.secondary.getTransaction({ hash: record.hash }),
  ])
  for (const receipt of [receiptA, receiptB]) {
    assert.equal(receipt.transactionHash?.toLowerCase(), record.hash.toLowerCase(), `${name} deployment receipt hash differs`)
    assert.equal(receipt.status, 'success', `${name} deployment receipt reverted`)
    assert.equal(receipt.blockHash?.toLowerCase(), record.blockHash.toLowerCase(), `${name} deployment block changed`)
    assert.equal(String(receipt.blockNumber), record.blockNumber, `${name} deployment height changed`)
    assert.ok(receipt.contractAddress && isAddressEqual(receipt.contractAddress, address), `${name} deployment address changed`)
    if (safeBlock !== undefined) assert.ok(receipt.blockNumber <= safeBlock, `${name} deployment is not safe yet`)
  }
  for (const tx of [txA, txB]) {
    assert.equal(tx.hash.toLowerCase(), record.hash.toLowerCase(), `${name} deployment transaction hash differs`)
    assert.equal(tx.blockHash?.toLowerCase(), record.blockHash.toLowerCase(), `${name} deployment block hash differs`)
    assert.equal(String(tx.blockNumber), record.blockNumber, `${name} deployment block height differs`)
    assert.equal(tx.chainId, manifest.chainId, `${name} deployment chain differs`)
    assert.equal(tx.value, 0n, `${name} deployment unexpectedly sends native ETH`)
    assert.equal(tx.to, null, `${name} must be a direct contract creation`)
    assert.ok(isAddressEqual(tx.from, manifest.binding.deployer), `${name} deployer differs from binding`)
    assert.equal(tx.input.toLowerCase(), expectedInput.toLowerCase(), `${name} deployment input differs from pinned artifact and constructor`)
  }
  return receiptA
}

async function recoverCreatedContract(clients, manifest, name, expectedInput) {
  const record = manifest.transactions.findLast(item => item.name === name)
  if (!record) return undefined
  const receipt = await clients.primary.getTransactionReceipt({ hash: record.hash })
  assert.ok(receipt.contractAddress, `${name} receipt has no contract address`)
  await verifyDeploymentTransaction(clients, manifest, name, receipt.contractAddress, expectedInput)
  return { address: getAddress(receipt.contractAddress), receipt }
}

async function attest(clients, c, manifest, artifacts, atLeast = 0n) {
  assert.ok(!manifest.inFlight, 'Resolve the manifest transaction intent before attesting')
  assert.ok(manifest.staker && manifest.paymaster && manifest.sampleAccount, 'Manifest is incomplete')
  atLeast = manifest.transactions.reduce((max, tx) => {
    const block = BigInt(tx.blockNumber)
    return block > max ? block : max
  }, atLeast)
  const safe = await commonSafeBlock(clients, atLeast, atLeast > 0n ? 900_000 : 15_000)
  const status = await statusOfPinnedContracts(clients, safe.number)
  assert.ok(Object.values(status).every(value => value === 'already deployed'), 'Pinned Kernel contracts are incomplete')
  const { addresses: expectedSimulations } = simulationAddresses(c, artifacts)
  for (const name of ['entryPoint09', 'pimlico']) {
    assert.ok(manifest.simulations?.[name], `${name} simulation missing from manifest`)
    assert.ok(isAddressEqual(manifest.simulations[name].address, expectedSimulations[name]), `${name} simulation address mismatch`)
    const hash = await verifySimulation(clients, artifacts, name, expectedSimulations[name], safe.number, true)
    assert.equal(hash, manifest.simulations[name].codeHash, `${name} simulation hash changed`)
  }
  await Promise.all(ownerAddressesToCheck(c).map(([label, address]) =>
    requireUndeployedOwner(clients, address, safe.number, label)))
  const factory = getAddress(pins.kernel.contracts.find(x => x.name === 'KernelFactory').address)
  const implementation = getAddress(pins.kernel.contracts.find(x => x.name === 'KernelUUPS').address)
  await requireHash(clients, manifest.staker, manifest.stakerCodeHash, safe.number, 'Plether-owned Staker')
  assert.equal(manifest.stakerCodeHash, stakerRuntimeCodeHash, 'Staker runtime differs from pinned local release deployment')
  await verifyDeploymentTransaction(clients, manifest, 'Deploy Plether Staker', manifest.staker,
    encodeDeployData({ abi: stakerAbi, bytecode: artifacts.artifacts.get('Staker').bytecode, args: [c.deployer] }), safe.number)
  await Promise.all([clients.primary, clients.secondary].map(async client => {
    const [stakerOwner, approved, factoryImplementation] = await Promise.all([
      client.readContract({ address: manifest.staker, abi: stakerAbi, functionName: 'owner', blockNumber: safe.number }),
      client.readContract({ address: manifest.staker, abi: stakerAbi, functionName: 'approved', args: [factory], blockNumber: safe.number }),
      client.readContract({ address: factory, abi: factoryAbi, functionName: 'UUPS', blockNumber: safe.number }),
    ])
    assert.ok(isAddressEqual(stakerOwner, c.deployer), 'Staker owner differs from dedicated deployer')
    assert.equal(approved, true, 'Staker has not approved the Kernel factory')
    assert.ok(isAddressEqual(factoryImplementation, implementation), 'Kernel factory implementation differs from pin')
  }))
  await verifyStake(clients, manifest.staker, safe.number, 'Staker')
  assert.ok(isAddressEqual(await verifySample(clients, c, safe.number, manifest.sampleAccount), manifest.sampleAccount))
  await requireHash(clients, manifest.paymaster, manifest.paymasterCodeHash, safe.number, 'Core paymaster')
  const paymasterRuntime = await clients.primary.getCode({ address: manifest.paymaster, blockNumber: safe.number })
  assert.ok(matchesRuntimeTemplate(paymasterRuntime, artifacts.paymaster), 'Core paymaster runtime differs from pinned artifact outside constructor immutables')
  await verifyDeploymentTransaction(clients, manifest, 'Deploy paused Core paymaster', manifest.paymaster,
    encodeDeployData({ abi: artifacts.paymaster.abi, bytecode: artifacts.paymaster.bytecode.object,
      args: paymasterConstructorArgs(c, manifest) }), safe.number)
  const paymasterReads = {
    entryPoint: pins.entryPoint.address,
    entryPointCodeHash: pins.entryPoint.codeHash,
    owner: c.deployer,
    sponsorSigner: c.sponsorSigner,
    policyId: c.policyId,
    approvedAccountCodeHash: pins.kernel.proxyCodeHash,
    accountStaker: manifest.staker,
    accountStakerCodeHash: manifest.stakerCodeHash,
    accountFactory: factory,
    accountFactoryCodeHash: pins.kernel.contracts.find(x => x.name === 'KernelFactory').codeHash,
    accountImplementation: implementation,
    accountImplementationCodeHash: pins.kernel.contracts.find(x => x.name === 'KernelUUPS').codeHash,
    rootValidator: pins.validator.address,
    rootValidatorCodeHash: pins.validator.codeHash,
    maxSponsoredCost: c.maxSponsoredCostWei,
    paused: true,
  }
  for (const [name, expected] of Object.entries(paymasterReads)) {
    const [primary, secondary] = await Promise.all([clients.primary, clients.secondary].map(client => client.readContract({
      address: manifest.paymaster, abi: artifacts.paymaster.abi, functionName: name, blockNumber: safe.number,
    })))
    assert.deepEqual(primary, secondary, `Paymaster ${name} differs between RPCs`)
    if (typeof expected === 'string' && expected.length === 42) assert.ok(isAddressEqual(primary, expected), `Paymaster ${name} mismatch`)
    else assert.equal(typeof primary === 'string' ? primary.toLowerCase() : primary, expected, `Paymaster ${name} mismatch`)
  }
  const paymasterStake = await verifyStake(clients, manifest.paymaster, safe.number, 'Paymaster', c.paymasterDepositWei)
  assert.ok(manifest.paymasterDeploymentBlock && manifest.paymasterDeploymentBlockHash, 'Paymaster deployment receipt missing')
  const [deploymentA, deploymentB] = await Promise.all([
    clients.primary.getBlock({ blockNumber: BigInt(manifest.paymasterDeploymentBlock) }),
    clients.secondary.getBlock({ blockNumber: BigInt(manifest.paymasterDeploymentBlock) }),
  ])
  const deploymentHash = checkedHash(deploymentA.hash, 'Primary paymaster deployment block hash')
  assert.equal(deploymentHash, checkedHash(deploymentB.hash, 'Secondary paymaster deployment block hash'),
    'Paymaster deployment block differs between RPCs')
  assert.equal(deploymentHash, checkedHash(manifest.paymasterDeploymentBlockHash, 'Recorded paymaster deployment block hash'),
    'Paymaster deployment block hash changed')
  manifest.profile = {
    deployment: c.deployment, chainId: pins.chainId, entryPoint: getAddress(pins.entryPoint.address),
    entryPointCodeHash: pins.entryPoint.codeHash, factory,
    factoryCodeHash: pins.kernel.contracts.find(x => x.name === 'KernelFactory').codeHash,
    staker: getAddress(manifest.staker), stakerCodeHash: manifest.stakerCodeHash,
    implementation, implementationCodeHash: pins.kernel.contracts.find(x => x.name === 'KernelUUPS').codeHash,
    validator: getAddress(pins.validator.address), validatorCodeHash: pins.validator.codeHash,
    accountCodeHash: pins.kernel.proxyCodeHash,
    entryPointVersion: '0.9', accountVersion: 'kernel-v4-uups', accountIndex: 0,
  }
  manifest.attestation = {
    blockNumber: String(safe.number), blockHash: safe.hash,
    providers: [c.primaryHost, c.secondaryHost],
    stakerStakeWei: String((await verifyStake(clients, manifest.staker, safe.number, 'Staker')).stake),
    paymasterStakeWei: String(paymasterStake.stake), paymasterDepositWei: String(paymasterStake.deposit),
    paymasterPaused: true,
  }
  return manifest
}

function dedicatedSigner(c, confirmation) {
  assert.equal(confirmation, c.deployment, 'Pass --confirm with the exact deployment id')
  assert.ok(!isAddressEqual(c.deployer, sharedOrderRouterSigner), 'Use a dedicated deployer distinct from the live OrderRouter signer')
  const secret = process.env.KERNEL_CANARY_DEPLOYER_PRIVATE_KEY
  assert.match(secret ?? '', /^0x[0-9a-fA-F]{64}$/u, 'Inject KERNEL_CANARY_DEPLOYER_PRIVATE_KEY from the approved Plether signer process')
  const signer = privateKeyToAccount(secret)
  assert.ok(isAddressEqual(signer.address, c.deployer), 'Signer does not match the dedicated deployer')
  return signer
}

function deploymentWallet(c, signer) {
  return createWalletClient({ account: signer, chain: arbitrumSepolia, transport: http(c.primaryRpc, { timeout: 30_000, retryCount: 0 }) })
}

async function rebroadcastDeployment(clients, c, manifest, artifacts, confirmation) {
  const intent = manifest.inFlight
  assert.ok(intent?.fees && intent.request, 'Legacy or missing intent can only be reconciled after mining')
  const signer = dedicatedSigner(c, confirmation)
  const wallet = deploymentWallet(c, signer)
  const deposit = intent.name === 'Deposit Core paymaster' ? BigInt(intent.request.valueWei) : undefined
  return rebroadcastRecordedDeployment({ manifest, request: deploymentRequest(c, manifest, artifacts, intent.name, deposit),
    signTransaction: transaction => signer.signTransaction(transaction),
    assertCleanNonce: nonce => cleanLiveDeployerNonce(clients, c.deployer, nonce),
    sendRawTransaction: serializedTransaction => wallet.sendRawTransaction({ serializedTransaction }),
  })
}

async function broadcast(clients, c, manifest, artifacts, manifestPath, confirmation) {
  assert.ok(!manifest.inFlight, 'Manifest has an unresolved transaction intent; reconcile its nonce/hash before resuming')
  const signer = dedicatedSigner(c, confirmation)
  const stakerInput = deploymentRequest(c, manifest, artifacts, 'Deploy Plether Staker').data
  const recoveredStaker = await recoverCreatedContract(clients, manifest, 'Deploy Plether Staker', stakerInput)
  if (recoveredStaker) {
    if (manifest.staker) assert.ok(isAddressEqual(manifest.staker, recoveredStaker.address), 'Manifest Staker differs from deployment receipt')
    manifest.staker = recoveredStaker.address
    manifest.stakerCodeHash = stakerRuntimeCodeHash
    const code = await clients.primary.getCode({ address: manifest.staker })
    assert.ok(code && keccak256(code) === stakerRuntimeCodeHash, 'Recovered Staker code differs from pin')
  } else assert.ok(!manifest.staker, 'Manifest Staker lacks a deployment receipt')
  let recoveredPaymaster
  if (manifest.staker) {
    const paymasterInput = deploymentRequest(c, manifest, artifacts, 'Deploy paused Core paymaster').data
    recoveredPaymaster = await recoverCreatedContract(clients, manifest, 'Deploy paused Core paymaster', paymasterInput)
  }
  if (recoveredPaymaster) {
    if (manifest.paymaster) assert.ok(isAddressEqual(manifest.paymaster, recoveredPaymaster.address), 'Manifest paymaster differs from deployment receipt')
    manifest.paymaster = recoveredPaymaster.address
    manifest.paymasterDeploymentBlock = String(recoveredPaymaster.receipt.blockNumber)
    manifest.paymasterDeploymentBlockHash = recoveredPaymaster.receipt.blockHash
    const code = await clients.primary.getCode({ address: manifest.paymaster })
    assert.ok(code && code !== '0x' && matchesRuntimeTemplate(code, artifacts.paymaster), 'Recovered paymaster runtime differs from pin')
    manifest.paymasterCodeHash = keccak256(code)
  } else assert.ok(!manifest.paymaster, 'Manifest paymaster lacks a deployment receipt')
  saveManifest(manifestPath, manifest)
  const stakerInfoBefore = manifest.staker ? await clients.primary.readContract({
    address: pins.entryPoint.address, abi: entryPointAbi, functionName: 'getDepositInfo', args: [manifest.staker],
  }) : undefined
  const paymasterInfoBefore = manifest.paymaster ? await clients.primary.readContract({
    address: pins.entryPoint.address, abi: entryPointAbi, functionName: 'getDepositInfo', args: [manifest.paymaster],
  }) : undefined
  const stakerStillNeeded = stakerInfoBefore?.staked && stakerInfoBefore.stake >= minStake && stakerInfoBefore.unstakeDelaySec >= unstakeDelay ? 0n : minStake
  const paymasterStakeStillNeeded = paymasterInfoBefore?.staked && paymasterInfoBefore.stake >= minStake && paymasterInfoBefore.unstakeDelaySec >= unstakeDelay ? 0n : minStake
  if (stakerInfoBefore && stakerStillNeeded > 0n) assert.equal(stakerInfoBefore.stake, 0n, 'Review partial Staker stake before resuming')
  if (paymasterInfoBefore && paymasterStakeStillNeeded > 0n) assert.equal(paymasterInfoBefore.stake, 0n, 'Review partial paymaster stake before resuming')
  const depositStillNeeded = paymasterInfoBefore?.deposit >= c.paymasterDepositWei ? 0n :
    c.paymasterDepositWei - (paymasterInfoBefore?.deposit ?? 0n)
  const [latestNonce, balance] = await Promise.all([
    cleanLiveDeployerNonce(clients, c.deployer, manifest.expectedNonce === undefined ? undefined : Number(manifest.expectedNonce)),
    clients.primary.getBalance({ address: c.deployer }),
  ])
  if (manifest.expectedNonce === undefined) {
    assert.equal(manifest.transactions.length, 0, 'Recorded transactions lack a nonce checkpoint; reconcile manually')
    manifest.expectedNonce = String(latestNonce)
  }
  assert.equal(String(latestNonce), manifest.expectedNonce, 'Deployer nonce changed since the last confirmed script transaction; reconcile manually')
  const needed = stakerStillNeeded + paymasterStakeStillNeeded + depositStillNeeded + gasReserve
  assert.ok(balance >= needed, `Plether deployer needs at least ${needed} wei before broadcasting`)
  saveManifest(manifestPath, manifest)
  const wallet = deploymentWallet(c, signer)
  let nextNonce = latestNonce
  let lastBlock = manifest.transactions.reduce((max, tx) => {
    const block = BigInt(tx.blockNumber)
    return block > max ? block : max
  }, 0n)
  async function confirmed(name, depositValue) {
    await cleanLiveDeployerNonce(clients, c.deployer, nextNonce)
    assert.equal(manifest.expectedNonce, String(nextNonce), 'Manifest nonce checkpoint changed before the next send')
    const request = deploymentRequest(c, manifest, artifacts, name, depositValue)
    const prepared = await wallet.prepareTransactionRequest({ ...request, type: 'eip1559', nonce: nextNonce })
    const fees = deploymentFeeFields(prepared)
    const { signed, hash } = await signDeploymentTransaction({ request, chainId: pins.chainId, nonce: nextNonce, fees,
      signTransaction: transaction => signer.signTransaction(transaction) })
    await cleanLiveDeployerNonce(clients, c.deployer, nextNonce)
    manifest.inFlight = { name, nonce: String(nextNonce), hash, request: fingerprintDeploymentRequest(request), fees }
    saveManifest(manifestPath, manifest)
    const sentHash = await wallet.sendRawTransaction({ serializedTransaction: signed })
    assert.equal(sentHash, hash, 'Provider returned a different deployment transaction hash')
    await Promise.all([clients.primary, clients.secondary].map(client => client.waitForTransactionReceipt({
      hash, timeout: 180_000, confirmations: 1, checkReplacement: false,
    })))
    const receipt = await readMinedDeployment({ clients, manifest, intent: manifest.inFlight,
      expectedRequest: () => fingerprintDeploymentRequest(request) })
    // A reverted send also consumes a nonce. Finish it via safe reconciliation before retrying.
    assert.equal(receipt.status, 'success', `${name} transaction reverted; run --reconcile-inflight`)
    lastBlock = receipt.blockNumber > lastBlock ? receipt.blockNumber : lastBlock
    const completed = completeMinedDeployment(manifest, receipt)
    nextNonce += 1
    manifest.transactions = completed.transactions
    manifest.expectedNonce = completed.expectedNonce
    delete manifest.inFlight
    saveManifest(manifestPath, manifest)
    console.log(`${name}: ${hash}`)
    return receipt
  }
  async function latestCode(address, expected) {
    const code = await clients.primary.getCode({ address, blockTag: 'latest' })
    if (code && code !== '0x') {
      assert.equal(keccak256(code).toLowerCase(), expected.toLowerCase(), `Unexpected live code at ${address}`)
      return true
    }
    return false
  }
  for (const pin of pins.kernel.contracts) {
    const address = getAddress(pin.address)
    if (await latestCode(address, pin.codeHash)) continue
    await confirmed(`Deploy ${pin.name}`)
    assert.equal(await latestCode(address, pin.codeHash), true)
  }
  if (!(await latestCode(pins.validator.address, pins.validator.codeHash))) {
    await confirmed('Deploy ECDSAValidator')
    assert.equal(await latestCode(pins.validator.address, pins.validator.codeHash), true)
  }
  const { addresses: simulations } = simulationAddresses(c, artifacts)
  manifest.simulations ??= {}
  for (const name of ['entryPoint09', 'pimlico']) {
    let current = await verifySimulation({ primary: clients.primary, secondary: clients.primary }, artifacts, name,
      simulations[name], await clients.primary.getBlockNumber())
    if (!current) {
      await confirmed(`Deploy ${name} simulation`)
      current = await verifySimulation({ primary: clients.primary, secondary: clients.primary }, artifacts, name,
        simulations[name], await clients.primary.getBlockNumber(), true)
    }
    if (manifest.simulations[name]) {
      assert.ok(isAddressEqual(manifest.simulations[name].address, simulations[name]))
      assert.equal(manifest.simulations[name].codeHash, current)
    }
    manifest.simulations[name] = { address: simulations[name], codeHash: current }
    saveManifest(manifestPath, manifest)
  }
  const factory = getAddress(pins.kernel.contracts.find(x => x.name === 'KernelFactory').address)
  const implementation = getAddress(pins.kernel.contracts.find(x => x.name === 'KernelUUPS').address)
  assert.ok(isAddressEqual(await clients.primary.readContract({ address: factory, abi: factoryAbi, functionName: 'UUPS' }), implementation))
  if (!manifest.staker) {
    const receipt = await confirmed('Deploy Plether Staker')
    assert.ok(receipt.contractAddress, 'Staker deployment did not return an address')
    manifest.staker = getAddress(receipt.contractAddress)
    const code = await clients.primary.getCode({ address: manifest.staker })
    assert.ok(code && code !== '0x')
    manifest.stakerCodeHash = keccak256(code)
    assert.equal(manifest.stakerCodeHash, stakerRuntimeCodeHash, 'Deployed Staker runtime differs from pinned release')
    saveManifest(manifestPath, manifest)
  }
  assert.equal(manifest.stakerCodeHash, stakerRuntimeCodeHash, 'Manifest Staker runtime hash mismatch')
  assert.equal(await latestCode(manifest.staker, stakerRuntimeCodeHash), true)
  assert.ok(isAddressEqual(await clients.primary.readContract({ address: manifest.staker, abi: stakerAbi, functionName: 'owner' }), c.deployer))
  if (!(await clients.primary.readContract({ address: manifest.staker, abi: stakerAbi, functionName: 'approved', args: [factory] }))) {
    await confirmed('Approve Kernel factory')
  }
  const stakerStake = await clients.primary.readContract({ address: pins.entryPoint.address, abi: entryPointAbi, functionName: 'getDepositInfo', args: [manifest.staker] })
  if (!stakerStake.staked || stakerStake.stake < minStake || stakerStake.unstakeDelaySec < unstakeDelay) {
    assert.equal(stakerStake.stake, 0n, 'Review partial Staker stake manually before resuming')
    await confirmed('Stake Plether Staker')
  }
  const packages = [{ moduleType: 1n, module: getAddress(pins.validator.address), moduleData: c.sampleOwner, internalData: '0x' }]
  for (const [label, owner] of ownerAddressesToCheck(c)) {
    const code = await clients.primary.getCode({ address: owner, blockTag: 'latest' })
    assert.ok(!code || code === '0x', `${label} gained deployed code`)
  }
  const sample = await clients.primary.readContract({ address: factory, abi: factoryAbi, functionName: 'getAddress', args: [packages, 0n] })
  if (!(await latestCode(sample, pins.kernel.proxyCodeHash))) {
    await confirmed('Deploy sample Kernel proxy')
    assert.equal(await latestCode(sample, pins.kernel.proxyCodeHash), true)
  }
  manifest.sampleAccount = getAddress(sample)
  saveManifest(manifestPath, manifest)
  await verifySample({ primary: clients.primary, secondary: clients.primary }, c, await clients.primary.getBlockNumber(), sample)
  if (!manifest.paymaster) {
    const receipt = await confirmed('Deploy paused Core paymaster')
    assert.ok(receipt.contractAddress, 'Paymaster deployment did not return an address')
    manifest.paymaster = getAddress(receipt.contractAddress)
    manifest.paymasterDeploymentBlock = String(receipt.blockNumber)
    manifest.paymasterDeploymentBlockHash = receipt.blockHash
    const code = await clients.primary.getCode({ address: manifest.paymaster })
    assert.ok(code && code !== '0x' && matchesRuntimeTemplate(code, artifacts.paymaster),
      'Deployed paymaster runtime differs from pinned artifact')
    manifest.paymasterCodeHash = keccak256(code)
    saveManifest(manifestPath, manifest)
  }
  assert.ok(manifest.paymasterCodeHash && await latestCode(manifest.paymaster, manifest.paymasterCodeHash))
  assert.equal(await clients.primary.readContract({ address: manifest.paymaster, abi: artifacts.paymaster.abi, functionName: 'paused' }), true, 'Canary paymaster must remain paused')
  const paymasterInfo = await clients.primary.readContract({ address: pins.entryPoint.address, abi: entryPointAbi, functionName: 'getDepositInfo', args: [manifest.paymaster] })
  if (paymasterInfo.deposit < c.paymasterDepositWei) {
    await confirmed('Deposit Core paymaster', c.paymasterDepositWei - paymasterInfo.deposit)
  }
  if (!paymasterInfo.staked || paymasterInfo.stake < minStake || paymasterInfo.unstakeDelaySec < unstakeDelay) {
    assert.equal(paymasterInfo.stake, 0n, 'Review partial paymaster stake manually before resuming')
    await confirmed('Stake Core paymaster')
  }
  return attest(clients, c, manifest, artifacts, lastBlock)
}

async function main() {
  const { values: args } = parseArgs({ options: {
    config: { type: 'string' }, manifest: { type: 'string' }, attest: { type: 'boolean' },
    broadcast: { type: 'boolean' }, confirm: { type: 'string' }, help: { type: 'boolean' },
    'check-artifacts': { type: 'boolean' }, 'reconcile-external-nonces': { type: 'boolean' }, 'reconcile-inflight': { type: 'boolean' },
    'rebroadcast-inflight': { type: 'boolean' },
    'external-tx': { type: 'string', multiple: true },
  } })
  if (args.help) return usage()
  assert.ok(args.config, 'Pass --config; see deploy-sepolia.md')
  assert.ok([args.attest, args.broadcast, args['check-artifacts'], args['reconcile-external-nonces'], args['reconcile-inflight'], args['rebroadcast-inflight']]
    .filter(Boolean).length <= 1, 'Choose only one action')
  assert.ok(!args['external-tx'] || args['reconcile-external-nonces'], '--external-tx requires --reconcile-external-nonces')
  const c = loadConfig(args.config)
  const manifestPath = resolve(args.manifest ?? `kernel-${c.deployment}-manifest.json`)
  phase = 'pinned artifacts'
  const artifacts = loadPinnedArtifacts(c)
  if (args['check-artifacts']) {
    console.log(JSON.stringify({ mode: 'read-only-artifact-check', kernelCommit: pins.kernel.commit,
      validatorCommit: pins.validator.commit, corePaymasterCommit, corePaymasterSourceHash, stakerCreationCodeHash,
      stakerReleaseInitCodeHash, stakerRuntimeCodeHash, paymasterCreationCodeHash,
      simulationCreationCodeHashes,
      releaseContracts: [...artifacts.artifacts.keys()], validatorAddress: pins.validator.address,
    }, null, 2))
    return
  }
  const manifest = loadManifest(manifestPath, c)
  const clients = makeClients(c)
  phase = 'independent safe block'
  const safe = await commonSafeBlock(clients)
  phase = 'pinned chain code'
  const status = await statusOfPinnedContracts(clients, safe.number)
  phase = 'simulation code'
  const simulators = await simulationStatus(clients, c, artifacts, safe.number)
  phase = 'canary owner EOAs'
  await Promise.all(ownerAddressesToCheck(c).map(([label, address]) =>
    requireUndeployedOwner(clients, address, safe.number, label)))
  phase = 'independent deployer balance'
  const [primaryBalance, secondaryBalance] = await Promise.all([clients.primary, clients.secondary].map(client =>
    client.getBalance({ address: c.deployer, blockNumber: safe.number })))
  assert.equal(primaryBalance, secondaryBalance, 'Deployer balance differs between RPCs at the common safe block')
  if (args['reconcile-inflight']) {
    phase = 'in-flight deployment reconciliation'
    assert.equal(args.confirm, c.deployment, 'Pass --confirm with the exact deployment id')
    const result = await recoverDeployment(clients, c, manifest, artifacts)
    saveManifest(manifestPath, result.manifest)
    console.log(JSON.stringify({ mode: 'in-flight-reconciled', manifest: manifestPath,
      status: result.status, hash: result.hash, nextNonce: result.manifest.expectedNonce }, null, 2))
  } else if (args['rebroadcast-inflight']) {
    phase = 'rebroadcast of exact in-flight transaction'
    const hash = await rebroadcastDeployment(clients, c, manifest, artifacts, args.confirm)
    console.log(JSON.stringify({ mode: 'in-flight-rebroadcast', manifest: manifestPath, hash }, null, 2))
  } else if (args['reconcile-external-nonces']) {
    phase = 'external nonce reconciliation'
    assert.equal(args.confirm, c.deployment, 'Pass --confirm with the exact deployment id')
    const reconciled = await reconcileExternalNonces({
      clients, manifest, hashes: args['external-tx'],
      awaitSafeBlock: (rpcClients, block) => commonSafeBlock(rpcClients, block, 900_000),
    })
    saveManifest(manifestPath, reconciled)
    console.log(JSON.stringify({ mode: 'external-nonces-reconciled', manifest: manifestPath,
      firstNonce: reconciled.externalNonceReconciliations.at(-1).firstNonce,
      nextNonce: reconciled.expectedNonce,
      observedDeployerNonce: reconciled.externalNonceReconciliations.at(-1).observedDeployerNonce,
      observedPendingNonce: reconciled.externalNonceReconciliations.at(-1).observedPendingNonce,
      transactionHashes: reconciled.externalNonceReconciliations.at(-1).transactionHashes,
    }, null, 2))
  } else if (args.broadcast) {
    phase = 'broadcast sequence'
    assert.ok(!manifest.inFlight, 'Manifest has an unresolved transaction intent; reconcile its nonce/hash before resuming')
    // No wallet client or private key is read until pinned source, two RPCs and chain state pass.
    await verifyRecordedExternalNonces({ clients, manifest,
      awaitSafeBlock: (rpcClients, block) => commonSafeBlock(rpcClients, block, 900_000) })
    const complete = await broadcast(clients, c, manifest, artifacts, manifestPath, args.confirm)
    saveManifest(manifestPath, complete)
    console.log(JSON.stringify({ mode: 'broadcast-complete', manifest: manifestPath, profile: complete.profile, attestation: complete.attestation }, null, 2))
  } else if (args.attest) {
    phase = 'final independent attestation'
    assert.ok(!manifest.inFlight, 'Manifest has an unresolved transaction intent; reconcile its nonce/hash before attesting')
    await verifyRecordedExternalNonces({ clients, manifest,
      awaitSafeBlock: (rpcClients, block) => commonSafeBlock(rpcClients, block, 900_000) })
    const complete = await attest(clients, c, manifest, artifacts)
    saveManifest(manifestPath, complete)
    console.log(JSON.stringify({ mode: 'attested', manifest: manifestPath, profile: complete.profile, attestation: complete.attestation }, null, 2))
  } else {
    // Deliberately no writes, including no manifest file, in the default mode.
    console.log(JSON.stringify({ mode: 'read-only-plan', deployment: c.deployment, chainId: pins.chainId,
      safeBlock: String(safe.number), safeBlockHash: safe.hash, providers: [c.primaryHost, c.secondaryHost],
      pinnedContracts: status, simulations: simulators,
      deployer: c.deployer, sampleOwner: c.sampleOwner, sponsorSigner: c.sponsorSigner,
      deployerBalanceWei: String(primaryBalance), fundingReady: primaryBalance >= minStake * 2n + c.paymasterDepositWei + gasReserve,
      requiredDeployerBalanceWei: String(minStake * 2n + c.paymasterDepositWei + gasReserve),
      broadcastRequires: ['--broadcast', `--confirm ${c.deployment}`, 'KERNEL_CANARY_DEPLOYER_PRIVATE_KEY'],
    }, null, 2))
  }
}

try {
  await main()
} catch (error) {
  const scrubbed = scrubRpcError(error, [process.env.KERNEL_CANARY_DEPLOYER_PRIVATE_KEY])
  console.error(`Sepolia Kernel qualification failed during ${phase}: ${scrubbed}`)
  process.exitCode = 1
}
