#!/usr/bin/env node
// Direct Kernel v0.9 Nitro/Alto smoke. No transaction is sent without --send-eth.
import assert from 'node:assert/strict'
import { existsSync, readFileSync } from 'node:fs'
import { createRequire } from 'node:module'
import { resolve } from 'node:path'
import { parseArgs } from 'node:util'
import { setTimeout as sleep } from 'node:timers/promises'
import { createKernelTradingAccount, encodeKernelCalls, verifyKernelTradingAccountAtBlock } from '../../apps/frontend/src/perps-aa/kernelAccount.ts'
import { boundedGasEstimate, gasCap, maxFeeCap, maxPrefundCap } from './nitro-sepolia-gas.mjs'
import { assertExactGasDebit, assertRecoveredEthSmokeOperation, readSafeBundleReceipts, validateSmokeRpcUrls } from './nitro-sepolia-smoke-guards.mjs'
import { recoverOperationFromBundle } from './nitro-sepolia-bundle-recovery.mjs'
import { scrubRpcError, writePublicEvidence } from './local-safety.mjs'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const {
  createPublicClient, decodeEventLog, encodeFunctionData, getAddress, http, isAddressEqual,
  keccak256, parseAbi, parseEther, toHex, zeroAddress,
} = require('viem')
const { privateKeyToAccount } = require('viem/accounts')
const { arbitrumSepolia } = require('viem/chains')
const { entryPoint09Abi, formatUserOperationRequest, getUserOperationHash, toPackedUserOperation } = require('viem/account-abstraction')
const pins = JSON.parse(readFileSync(new URL('./pins.json', import.meta.url)))
const advisoryBalance = parseEther('0.2')
const entryPoint = getAddress(pins.entryPoint.address)
const stakerAbi = parseAbi(['function owner() view returns (address)', 'function approved(address factory) view returns (bool)'])
const stakeAbi = parseAbi(['function getDepositInfo(address account) view returns ((uint112 deposit,bool staked,uint112 stake,uint32 unstakeDelaySec,uint48 withdrawTime))'])
const paymasterAbi = parseAbi([
  'function paused() view returns (bool)', 'function sponsorSigner() view returns (address)',
  'function policyId() view returns (bytes32)', 'function maxSponsoredCost() view returns (uint256)',
])
const bigintJson = value => JSON.stringify(value, (_, item) => typeof item === 'bigint' ? item.toString() : item, 2)

function usage() {
  console.log(`Usage: node scripts/kernel-qualification/nitro-sepolia-smoke.mjs [plan | send-eth | verify-eth | verify-sponsored] --manifest PATH --owner ADDRESS [--evidence PATH] [--sponsorship PATH] [--user-op-hash 0x...] [--transaction-hash 0x...] [--confirm DEPLOYMENT]

Default plan is read-only and needs no private key. RPC URLs come only from KERNEL_SMOKE_ALTO_RPC_URL, KERNEL_SMOKE_NITRO_RPC_URL and KERNEL_SMOKE_SECONDARY_RPC_URL. An optional KERNEL_SMOKE_NITRO_BEARER stays in the environment.
send-eth is the only transmitting command. It requires --confirm, an unused evidence path, a funded fresh account, and KERNEL_SMOKE_OWNER_PRIVATE_KEY. verify-eth and verify-sponsored are read-only.`)
}

function address(value, name) {
  assert.equal(typeof value, 'string', `${name} is required`)
  const result = getAddress(value)
  assert.ok(!isAddressEqual(result, zeroAddress), `${name} cannot be zero`)
  return result
}

function hash(value, name) {
  assert.match(value ?? '', /^0x[0-9a-fA-F]{64}$/u, `${name} must be a 32-byte hash`)
  return value.toLowerCase()
}

function readManifest(path) {
  const manifest = JSON.parse(readFileSync(resolve(path), 'utf8'))
  assert.equal(manifest.schemaVersion, 1)
  assert.equal(manifest.chainId, pins.chainId)
  assert.ok(manifest.attestation?.blockNumber && manifest.attestation?.blockHash, 'Deployment must be independently attested')
  assert.equal(manifest.profile?.chainId, pins.chainId)
  assert.ok(isAddressEqual(manifest.profile.entryPoint, entryPoint), 'Wrong EntryPoint')
  assert.equal(manifest.profile.entryPointCodeHash, pins.entryPoint.codeHash)
  assert.equal(manifest.profile.accountCodeHash, pins.kernel.proxyCodeHash)
  assert.equal(manifest.profile.deployment, manifest.deployment)
  assert.ok(manifest.staker && manifest.paymaster && manifest.simulations?.entryPoint09 && manifest.simulations?.pimlico)
  return manifest
}

function urls() {
  const values = {
    alto: process.env.KERNEL_SMOKE_ALTO_RPC_URL,
    nitro: process.env.KERNEL_SMOKE_NITRO_RPC_URL,
    secondary: process.env.KERNEL_SMOKE_SECONDARY_RPC_URL,
  }
  validateSmokeRpcUrls(values, {
    alto: process.env.KERNEL_SMOKE_EXPECTED_ALTO_ALB_HOST,
    nitro: process.env.KERNEL_SMOKE_EXPECTED_NITRO_ALB_HOST,
  })
  if (new URL(values.nitro).hostname.startsWith('internal-')) {
    assert.ok(process.env.KERNEL_SMOKE_NITRO_BEARER, 'Private Nitro ALB requires its bearer token')
  }
  return values
}

function clients(values) {
  const headers = process.env.KERNEL_SMOKE_NITRO_BEARER
    ? { authorization: `Bearer ${process.env.KERNEL_SMOKE_NITRO_BEARER}` } : undefined
  return {
    nitro: createPublicClient({ chain: arbitrumSepolia,
      transport: http(values.nitro, { timeout: 15_000, fetchOptions: { headers, redirect: 'error' } }) }),
    secondary: createPublicClient({ chain: arbitrumSepolia, transport: http(values.secondary, { timeout: 15_000 }) }),
  }
}

let requestId = 0
async function altoRequest(url, method, params = []) {
  const id = ++requestId
  const response = await fetch(url, {
    method: 'POST', headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ jsonrpc: '2.0', id, method, params }),
    signal: AbortSignal.timeout(30_000),
  })
  const payload = await response.json()
  assert.equal(payload.id, id, `${method} returned the wrong RPC id`)
  if (!response.ok || payload.error) {
    const aa21 = method === 'eth_estimateUserOperationGas' && payload.error?.code === -32500 &&
      payload.error?.message === "UserOperation reverted during simulation with reason: AA21 didn't pay prefund"
    throw new Error(`${method} failed with code ${payload.error?.code ?? response.status}${aa21 ? ' AA21' : ''}`)
  }
  return payload.result
}

async function commonSafe(clients, minimum) {
  const [chainA, chainB, safeA, safeB] = await Promise.all([
    clients.nitro.getChainId(), clients.secondary.getChainId(),
    clients.nitro.getBlock({ blockTag: 'safe' }), clients.secondary.getBlock({ blockTag: 'safe' }),
  ])
  assert.equal(chainA, pins.chainId)
  assert.equal(chainB, pins.chainId)
  const number = safeA.number < safeB.number ? safeA.number : safeB.number
  assert.ok(number >= minimum, 'RPC safe blocks precede contract attestation')
  const [a, b] = await Promise.all([
    clients.nitro.getBlock({ blockNumber: number }), clients.secondary.getBlock({ blockNumber: number }),
  ])
  assert.equal(a.hash, b.hash, 'Nitro and secondary RPCs disagree at the common safe block')
  return { number, hash: a.hash }
}

async function waitCommonSafe(clients, minimum) {
  const deadline = Date.now() + 180_000
  while (Date.now() < deadline) {
    try { return await commonSafe(clients, minimum) }
    catch (error) {
      if (!String(error?.message).includes('precede contract attestation')) throw error
      await sleep(3_000)
    }
  }
  throw new Error('Independent RPC safe blocks have not reached the UserOperation inclusion')
}

async function codeHashBoth(clients, address, blockNumber, expected) {
  const [a, b] = await Promise.all([clients.nitro, clients.secondary].map(client =>
    client.getCode({ address, blockNumber })))
  assert.equal(a ?? '0x', b ?? '0x', `RPCs disagree on code at ${address}`)
  assert.ok(a && a !== '0x' && keccak256(a).toLowerCase() === expected.toLowerCase(),
    `Unexpected code at ${address}`)
}

async function profileCheck(clients, manifest, owner) {
  const safe = await commonSafe(clients, BigInt(manifest.attestation.blockNumber))
  const historic = await Promise.all([clients.nitro, clients.secondary].map(client =>
    client.getBlock({ blockNumber: BigInt(manifest.attestation.blockNumber) })))
  for (const block of historic) assert.equal(manifest.attestation.blockHash, block.hash,
    'Deployment attestation block changed')
  const profile = manifest.profile
  await Promise.all([
    codeHashBoth(clients, profile.entryPoint, safe.number, profile.entryPointCodeHash),
    codeHashBoth(clients, profile.factory, safe.number, profile.factoryCodeHash),
    codeHashBoth(clients, profile.staker, safe.number, profile.stakerCodeHash),
    codeHashBoth(clients, profile.implementation, safe.number, profile.implementationCodeHash),
    codeHashBoth(clients, profile.validator, safe.number, profile.validatorCodeHash),
    codeHashBoth(clients, manifest.paymaster, safe.number, manifest.paymasterCodeHash),
    codeHashBoth(clients, manifest.simulations.entryPoint09.address, safe.number, manifest.simulations.entryPoint09.codeHash),
    codeHashBoth(clients, manifest.simulations.pimlico.address, safe.number, manifest.simulations.pimlico.codeHash),
  ])
  const [stakerOwner, approved, stakerStake, paymasterStake, independentPaymasterStake, accountA, accountB] = await Promise.all([
    clients.nitro.readContract({ address: profile.staker, abi: stakerAbi, functionName: 'owner', blockNumber: safe.number }),
    clients.nitro.readContract({ address: profile.staker, abi: stakerAbi, functionName: 'approved', args: [profile.factory], blockNumber: safe.number }),
    clients.nitro.readContract({ address: entryPoint, abi: stakeAbi, functionName: 'getDepositInfo', args: [profile.staker], blockNumber: safe.number }),
    clients.nitro.readContract({ address: entryPoint, abi: stakeAbi, functionName: 'getDepositInfo', args: [manifest.paymaster], blockNumber: safe.number }),
    clients.secondary.readContract({ address: entryPoint, abi: stakeAbi, functionName: 'getDepositInfo', args: [manifest.paymaster], blockNumber: safe.number }),
    verifyKernelTradingAccountAtBlock(clients.nitro, profile, owner, safe.number),
    verifyKernelTradingAccountAtBlock(clients.secondary, profile, owner, safe.number),
  ])
  assert.ok(isAddressEqual(stakerOwner, manifest.binding.deployer) && approved, 'Staker owner or factory approval mismatch')
  for (const [label, info] of [['Staker', stakerStake], ['paymaster', paymasterStake]]) {
    assert.ok(info.staked && info.stake >= parseEther('1') && info.unstakeDelaySec >= 86_400,
      `${label} must have the reviewed one-day 1 ETH stake`)
  }
  assert.deepEqual(paymasterStake, independentPaymasterStake, 'Paymaster stake differs between RPCs')
  assert.deepEqual(accountA, accountB, 'Nitro and secondary RPCs disagree on Kernel account state')
  return { safe, account: accountA, paymasterDeposit: paymasterStake.deposit }
}

async function accountFunds(client, sender, blockNumber) {
  const args = blockNumber === undefined ? {} : { blockNumber }
  const [balance, deposit] = await Promise.all([
    client.getBalance({ address: sender, ...args }),
    client.readContract({ address: entryPoint, abi: entryPoint09Abi, functionName: 'balanceOf', args: [sender], ...args }),
  ])
  return { balance, deposit, total: balance + deposit }
}

function smokeCall(sender) {
  return encodeKernelCalls([{ to: entryPoint, value: 0n,
    data: encodeFunctionData({ abi: entryPoint09Abi, functionName: 'balanceOf', args: [sender] }) }])
}

async function makeAccount(client, profile, owner, signer, blockNumber) {
  return createKernelTradingAccount(client, profile, {
    address: owner,
    signMessage: signer ? ({ message }) => signer.signMessage({ message }) : async () => { throw new Error('Read-only plan cannot sign') },
  }, blockNumber)
}

async function estimateEth(altoUrl, account, sender, funded) {
  const supported = await altoRequest(altoUrl, 'eth_supportedEntryPoints')
  assert.deepEqual(supported.map(getAddress), [entryPoint], 'Canary Alto must serve only EntryPoint 0.9')
  const quote = await altoRequest(altoUrl, 'pimlico_getUserOperationGasPrice')
  const fees = {
    maxFeePerGas: BigInt(quote.fast.maxFeePerGas),
    maxPriorityFeePerGas: BigInt(quote.fast.maxPriorityFeePerGas),
  }
  assert.ok(fees.maxFeePerGas > 0n && fees.maxFeePerGas <= maxFeeCap &&
    fees.maxPriorityFeePerGas <= fees.maxFeePerGas, 'Alto returned an unreviewed gas price')
  const operation = {
    sender, nonce: 0n, callData: smokeCall(sender),
    callGasLimit: 10_000_000n, verificationGasLimit: 10_000_000n, preVerificationGas: 100_000n,
    ...fees, signature: await account.getStubSignature(), ...account.factoryArgs,
  }
  const params = [formatUserOperationRequest(operation), entryPoint]
  let estimate
  let advisory = false
  if (funded) {
    try { estimate = await altoRequest(altoUrl, 'eth_estimateUserOperationGas', params) }
    catch (error) {
      if (!/eth_estimateUserOperationGas failed with code -32500 AA21/u.test(String(error?.message))) throw error
    }
  }
  if (!estimate) {
    advisory = true
    estimate = await altoRequest(altoUrl, 'eth_estimateUserOperationGas', [
      ...params, { [sender]: { balance: toHex(advisoryBalance) } },
    ])
  }
  Object.assign(operation, boundedGasEstimate(estimate))
  const prefund = (operation.callGasLimit + operation.verificationGasLimit + operation.preVerificationGas) * fees.maxFeePerGas
  assert.ok(prefund > 0n && prefund <= maxPrefundCap, 'ETH prefund exceeds the 0.05 ETH smoke cap')
  assert.equal(estimate.paymaster, undefined, 'ETH estimate unexpectedly has a paymaster')
  return { operation, prefund, advisory }
}

function saveEvidence(path, evidence, exclusive = false) {
  writePublicEvidence(path, `${bigintJson(evidence)}\n`, exclusive)
}

async function operationReceipt(altoUrl, operationHash) {
  const deadline = Date.now() + 180_000
  while (Date.now() < deadline) {
    const receipt = await altoRequest(altoUrl, 'eth_getUserOperationReceipt', [operationHash])
    if (receipt) return receipt
    await sleep(2_000)
  }
  throw new Error(`No recent Alto inclusion receipt for ${operationHash}; pass the public bundle transaction hash to verify the safe chain`)
}

async function verifyIncluded(clients, altoUrl, manifest, owner, operationHash, paymaster, transactionHint) {
  const receipt = transactionHint ? undefined : await operationReceipt(altoUrl, operationHash)
  if (receipt) {
    assert.equal(receipt.userOpHash?.toLowerCase(), operationHash)
    assert.equal(receipt.success, true, 'UserOperation failed')
  }
  const txHash = hash(transactionHint ?? receipt.receipt?.transactionHash, 'transaction hash')
  const readers = [clients.nitro, clients.secondary]
  const [tx, independentTx] = await readSafeBundleReceipts(clients, txHash, waitCommonSafe)
  const [blocks, transactions] = await Promise.all([
    Promise.all(readers.map(client => client.getBlock({ blockNumber: tx.blockNumber }))),
    Promise.all(readers.map(client => client.getTransaction({ hash: txHash }))),
  ])
  assert.equal(blocks[0].timestamp, blocks[1].timestamp, 'Bundle block timestamp differs between RPCs')
  const matchingEvents = entry => entry.logs.flatMap(log => {
    if (!isAddressEqual(log.address, entryPoint)) return []
    try {
      const decoded = decodeEventLog({ abi: entryPoint09Abi, topics: log.topics, data: log.data })
      return decoded.eventName === 'UserOperationEvent' && decoded.args.userOpHash.toLowerCase() === operationHash.toLowerCase()
        ? [decoded.args] : []
    } catch { return [] }
  })
  const event = matchingEvents(tx)[0]
  for (const [index, entry] of [tx, independentTx].entries()) {
    assert.equal(entry.transactionHash, txHash, 'Bundle receipt hash differs')
    assert.equal(entry.status, 'success', 'Bundle transaction reverted')
    assert.equal(entry.blockNumber, tx.blockNumber, 'Bundle transaction height differs between RPCs')
    assert.equal(entry.blockHash, tx.blockHash, 'Bundle transaction block differs between RPCs')
    assert.equal(blocks[index].hash, tx.blockHash, 'Bundle receipt block is not canonical')
    assert.deepEqual(matchingEvents(entry), [event], 'Expected exactly one matching EntryPoint UserOperationEvent on both RPCs')
  }
  const operation = recoverOperationFromBundle({ transactions, receipt: tx, entryPoint, chainId: pins.chainId,
    operationHash, paymaster })
  assert.equal(event.nonce, operation.nonce, 'Event nonce differs from the included operation')
  const account = await verifyKernelTradingAccountAtBlock(clients.nitro, manifest.profile, owner, tx.blockNumber)
  const independentAccount = await verifyKernelTradingAccountAtBlock(clients.secondary, manifest.profile, owner, tx.blockNumber)
  assert.deepEqual(account, independentAccount, 'Kernel account inclusion state differs between RPCs')
  assert.ok(account.deployed && isAddressEqual(event.sender, account.address), 'Wrong or undeployed Kernel sender')
  assert.ok(isAddressEqual(event.paymaster, paymaster ?? zeroAddress), 'Wrong UserOperation gas payer')
  assert.ok(event.success && event.actualGasCost > 0n, 'EntryPoint reports a failed or uncharged operation')
  if (receipt) assert.equal(event.actualGasCost, BigInt(receipt.actualGasCost), 'Event and Alto receipt gas disagree')
  return { account, event, operation, tx, txHash, block: blocks[0] }
}

async function payerDebit(clients, payer, blockNumber, balanceField, actual, label) {
  const debits = await Promise.all([clients.nitro, clients.secondary].map(async client => {
    const [before, after] = await Promise.all([
      accountFunds(client, payer, blockNumber - 1n), accountFunds(client, payer, blockNumber),
    ])
    assertExactGasDebit(before[balanceField], after[balanceField], actual, label)
    return before[balanceField] - after[balanceField]
  }))
  return debits[0]
}

async function planOrSendEth(command, args, manifest, clients, altoUrl, owner) {
  const { safe, account } = await profileCheck(clients, manifest, owner)
  assert.equal(account.deployed, false, 'Smoke owner must have a fresh, undeployed Kernel account')
  const funds = await accountFunds(clients.nitro, account.address, safe.number)
  const signer = command === 'send-eth' ? privateKeyToAccount(process.env.KERNEL_SMOKE_OWNER_PRIVATE_KEY) : undefined
  if (signer) assert.ok(isAddressEqual(signer.address, owner), 'Owner signer does not match --owner')
  const adapter = await makeAccount(clients.nitro, manifest.profile, owner, signer)
  assert.ok(isAddressEqual(adapter.address, account.address))
  const { operation, prefund, advisory } = await estimateEth(altoUrl, adapter, account.address, funds.total > 0n)
  const plan = {
    mode: 'eth', deployment: manifest.deployment, chainId: pins.chainId,
    safeBlock: String(safe.number), safeBlockHash: safe.hash, owner, sender: account.address,
    accountDeployed: false, accountBalanceWei: String(funds.balance), entryPointDepositWei: String(funds.deposit),
    maxPrefundWei: String(prefund), advisoryEstimate: advisory,
    fundingShortfallWei: String(funds.total >= prefund ? 0n : prefund - funds.total),
    estimate: Object.fromEntries(Object.keys(gasCap).map(field => [field, String(operation[field])])),
  }
  if (command !== 'send-eth') return console.log(bigintJson({ mode: 'read-only-plan', ...plan }))
  assert.equal(args.confirm, manifest.deployment, 'Pass --confirm with the exact deployment id')
  assert.ok(args.evidence, 'send-eth needs --evidence for a new public-only receipt record')
  const evidencePath = resolve(args.evidence)
  assert.ok(!existsSync(evidencePath), 'Evidence already exists; use verify-eth, never resubmit blindly')
  assert.equal(advisory, false, 'Fund the counterfactual Kernel account and rerun before sending')
  assert.ok(funds.total >= prefund, 'Account ETH and EntryPoint deposit do not cover the prefund')
  const nonce = await clients.nitro.readContract({ address: entryPoint, abi: entryPoint09Abi,
    functionName: 'getNonce', args: [account.address, 0n] })
  assert.equal(nonce, 0n, 'Fresh smoke account must use nonce zero')
  assert.ok(!operation.paymaster && !operation.paymasterData, 'ETH smoke cannot use sponsorship')
  const operationHash = getUserOperationHash({ userOperation: operation, chainId: pins.chainId,
    entryPointAddress: entryPoint, entryPointVersion: '0.9' })
  assert.ok(isAddressEqual(operation.factory, manifest.profile.staker), 'First operation must use the pinned Staker')
  const onchainHash = await clients.nitro.readContract({ address: entryPoint, abi: entryPoint09Abi,
    functionName: 'getUserOpHash', args: [toPackedUserOperation(operation)] })
  assert.equal(onchainHash, operationHash, 'Viem and EntryPoint disagree on the operation hash')
  const signed = { ...operation, signature: await adapter.signUserOperation(operation) }
  const confirmedHash = getUserOperationHash({ userOperation: signed, chainId: pins.chainId,
    entryPointAddress: entryPoint, entryPointVersion: '0.9' })
  assert.equal(confirmedHash, operationHash)
  const intent = { schemaVersion: 1, status: 'send-intent', ...plan, userOperationHash: operationHash }
  saveEvidence(evidencePath, intent, true) // A crash after broadcast cannot cause a blind retry.
  const returned = await altoRequest(altoUrl, 'eth_sendUserOperation', [formatUserOperationRequest(signed), entryPoint])
  assert.equal(returned.toLowerCase(), operationHash.toLowerCase(), 'Alto returned a different UserOperation hash')
  intent.status = 'submitted'
  saveEvidence(evidencePath, intent)
  const { account: deployed, event, tx, txHash } = await verifyIncluded(clients, altoUrl, manifest, owner, operationHash)
  assert.equal(event.nonce, 0n)
  assert.ok(event.actualGasCost <= prefund, 'Actual gas exceeded reviewed prefund')
  const debit = await payerDebit(clients, account.address, tx.blockNumber, 'total', event.actualGasCost, 'ETH smoke')
  const result = { ...intent, status: 'included', transactionHash: txHash,
    inclusionBlock: String(tx.blockNumber), inclusionBlockHash: tx.blockHash,
    actualGasCostWei: String(event.actualGasCost), payerDebitWei: String(debit),
    accountDeployed: deployed.deployed }
  saveEvidence(evidencePath, result)
  console.log(bigintJson(result))
}

async function verifyEth(args, manifest, clients, altoUrl, owner) {
  assert.ok(args.evidence, 'verify-eth needs --evidence from send-eth')
  const path = resolve(args.evidence)
  const prior = JSON.parse(readFileSync(path, 'utf8'))
  assert.equal(prior.schemaVersion, 1)
  assert.equal(prior.deployment, manifest.deployment)
  assert.ok(['send-intent', 'submitted', 'included'].includes(prior.status), 'Invalid ETH smoke evidence status')
  assert.ok(isAddressEqual(prior.owner, owner))
  const operationHash = hash(prior.userOperationHash, 'evidence UserOperation hash')
  const transactionHint = args['transaction-hash'] ?? prior.transactionHash
  const { event, operation, tx, txHash } = await verifyIncluded(clients, altoUrl, manifest, owner, operationHash, undefined, transactionHint)
  assert.ok(isAddressEqual(prior.sender, event.sender), 'Evidence sender differs from included operation')
  assert.equal(event.nonce, 0n)
  assert.ok(event.actualGasCost <= BigInt(prior.maxPrefundWei))
  const adapter = await makeAccount(clients.nitro, manifest.profile, owner, undefined, tx.blockNumber - 1n)
  assertRecoveredEthSmokeOperation(operation, {
    sender: event.sender, callData: smokeCall(event.sender),
    factory: manifest.profile.staker, factoryData: adapter.factoryArgs.factoryData,
    estimate: prior.estimate, maxPrefundWei: prior.maxPrefundWei,
    gasCap, maxFeeCap, maxPrefundCap,
  })
  assert.equal(getUserOperationHash({ userOperation: operation, chainId: pins.chainId,
    entryPointAddress: entryPoint, entryPointVersion: '0.9' }).toLowerCase(), operationHash,
  'Recovered ETH operation hash differs from intent')
  const debit = await payerDebit(clients, event.sender, tx.blockNumber, 'total', event.actualGasCost, 'ETH smoke')
  const result = { ...prior, status: 'included', transactionHash: txHash,
    inclusionBlock: String(tx.blockNumber), inclusionBlockHash: tx.blockHash,
    actualGasCostWei: String(event.actualGasCost), payerDebitWei: String(debit), accountDeployed: true }
  saveEvidence(path, result)
  console.log(bigintJson(result))
}

function parseSponsorEnvelope(paymasterData) {
  assert.match(paymasterData ?? '', /^0x[0-9a-fA-F]{314}$/u, 'Expected exact 157-byte signed paymasterData')
  const body = paymasterData.slice(2).toLowerCase()
  return {
    validUntil: BigInt(`0x${body.slice(0, 12)}`), validAfter: BigInt(`0x${body.slice(12, 24)}`),
    maxCost: BigInt(`0x${body.slice(24, 56)}`), policyId: `0x${body.slice(56, 120)}`,
    accountCodeHash: `0x${body.slice(120, 184)}`, signature: `0x${body.slice(184)}`,
  }
}

async function verifySponsored(args, manifest, clients, altoUrl, owner) {
  assert.ok(args.sponsorship, 'Pass the bounded canary sponsorship JSON')
  const sponsorship = JSON.parse(readFileSync(resolve(args.sponsorship), 'utf8'))
  assert.ok(isAddressEqual(sponsorship.paymaster, manifest.paymaster))
  assert.equal(sponsorship.paymasterCodeHash, manifest.paymasterCodeHash)
  assert.ok(sponsorship.canaryOwners.some(candidate => isAddressEqual(candidate, owner)), 'Owner is outside sponsored canary')
  const operationHash = hash(args['user-op-hash'], 'sponsored UserOperation hash')
  const { account, event, operation, tx, txHash, block } = await verifyIncluded(clients, altoUrl, manifest,
    owner, operationHash, manifest.paymaster, args['transaction-hash'])
  await codeHashBoth(clients, manifest.paymaster, tx.blockNumber, manifest.paymasterCodeHash)
  assert.ok(isAddressEqual(operation.sender, account.address), 'Included sponsored operation sender differs')
  assert.ok(isAddressEqual(operation.paymaster, manifest.paymaster), 'Operation paymaster mismatch')
  const envelope = parseSponsorEnvelope(operation.paymasterData)
  assert.equal(envelope.policyId, sponsorship.policyId.toLowerCase())
  assert.equal(envelope.accountCodeHash, manifest.profile.accountCodeHash.toLowerCase())
  assert.ok(envelope.maxCost > 0n && envelope.maxCost <= BigInt(sponsorship.budget.maxCostWei),
    'Sponsor envelope exceeds canary budget')
  assert.ok(event.actualGasCost <= envelope.maxCost, 'Sponsored gas exceeds signed max cost')
  assert.ok(BigInt(operation.paymasterVerificationGasLimit) <= BigInt(sponsorship.verificationGasLimit))
  assert.equal(BigInt(operation.paymasterPostOpGasLimit), 0n)
  assert.ok(block.timestamp >= envelope.validAfter && block.timestamp <= envelope.validUntil,
    'Sponsored operation included outside its signed validity window')
  const states = await Promise.all([clients.nitro, clients.secondary].map(client => Promise.all(
    ['sponsorSigner', 'policyId', 'maxSponsoredCost', 'paused'].map(functionName =>
      client.readContract({ address: manifest.paymaster, abi: paymasterAbi, functionName, blockNumber: tx.blockNumber })))))
  assert.deepEqual(states[0], states[1], 'Independent RPCs disagree on included sponsor configuration')
  const [onchainSigner, onchainPolicy, onchainCap, paused] = states[0]
  assert.equal(paused, false, 'Paymaster was paused at sponsored inclusion')
  assert.ok(isAddressEqual(onchainSigner, sponsorship.signerAddress))
  assert.equal(onchainPolicy.toLowerCase(), sponsorship.policyId.toLowerCase())
  assert.ok(event.actualGasCost <= onchainCap)
  const debit = await payerDebit(clients, manifest.paymaster, tx.blockNumber, 'deposit', event.actualGasCost, 'Sponsored paymaster deposit')
  console.log(bigintJson({ mode: 'sponsored-verification', deployment: manifest.deployment,
    owner, sender: account.address, paymaster: manifest.paymaster,
    userOperationHash: operationHash, transactionHash: txHash, inclusionBlock: String(tx.blockNumber),
    inclusionBlockHash: tx.blockHash, actualGasCostWei: String(event.actualGasCost),
    signedMaxCostWei: String(envelope.maxCost), payerDebitWei: String(debit),
    policyId: envelope.policyId }))
}

async function main() {
  const { values: args, positionals } = parseArgs({ allowPositionals: true, options: {
    manifest: { type: 'string' }, owner: { type: 'string' }, evidence: { type: 'string' },
    sponsorship: { type: 'string' }, 'user-op-hash': { type: 'string' }, 'transaction-hash': { type: 'string' },
    confirm: { type: 'string' }, help: { type: 'boolean' },
  } })
  if (args.help) return usage()
  const command = positionals[0] ?? 'plan'
  assert.ok(['plan', 'send-eth', 'verify-eth', 'verify-sponsored'].includes(command), 'Unknown command')
  assert.ok(!args['transaction-hash'] || command.startsWith('verify-'), '--transaction-hash is only for verification')
  assert.ok(args.manifest, 'Pass --manifest with the independently attested deployment')
  const manifest = readManifest(args.manifest)
  const owner = address(args.owner, 'throwaway owner')
  assert.ok(!isAddressEqual(owner, manifest.binding.deployer), 'Smoke owner must differ from contract deployer')
  assert.ok(!isAddressEqual(owner, manifest.binding.sampleOwner), 'Smoke owner must differ from sacrificial sample owner')
  assert.ok(!isAddressEqual(owner, manifest.binding.altoUtilityAddress), 'Smoke owner must differ from Alto utility')
  assert.ok(!isAddressEqual(owner, manifest.binding.sponsorSigner), 'Smoke owner must differ from sponsor signer')
  if (command === 'send-eth') {
    assert.match(process.env.KERNEL_SMOKE_OWNER_PRIVATE_KEY ?? '', /^0x[0-9a-fA-F]{64}$/u,
      'Send requires KERNEL_SMOKE_OWNER_PRIVATE_KEY from a throwaway wallet; never pass it as a CLI argument')
  }
  const values = urls()
  const rpcClients = clients(values)
  if (command === 'plan' || command === 'send-eth') return planOrSendEth(command, args, manifest, rpcClients, values.alto, owner)
  if (command === 'verify-eth') return verifyEth(args, manifest, rpcClients, values.alto, owner)
  return verifySponsored(args, manifest, rpcClients, values.alto, owner)
}

try { await main() } catch (error) {
  const message = scrubRpcError(error, [process.env.KERNEL_SMOKE_OWNER_PRIVATE_KEY, process.env.KERNEL_SMOKE_NITRO_BEARER])
  console.error(`Sepolia Nitro smoke failed: ${message}`)
  process.exitCode = 1
}
