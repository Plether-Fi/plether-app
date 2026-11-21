import { describe, expect, it, vi } from 'vitest'
import { encodeAbiParameters, encodeEventTopics, getAddress, parseAbiParameters, toHex, zeroAddress, type Address, type Hex } from 'viem'
import { entryPoint09Abi, entryPoint09Address } from 'viem/account-abstraction'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_RELEASE } from '../../contracts/perpsAddresses'
import { encodeKernelCalls } from '../kernelAccount'
import { kernelSepoliaCanaryPaymaster, kernelSepoliaCanaryProfile } from '../kernelDeploymentPins'
import {
  assertKernelPreparedOperation,
  findKernelGasFundingError,
  parseKernelManifest,
  parseKernelOperationStatus,
  parseKernelPreparation,
  prepareKernelCancellation,
  prepareKernelOperation,
  sendKernelOperation,
  type KernelPreparationIntent,
  type KernelStatusLocator,
} from '../kernelRpc'

const address = (value: number) => toHex(value, { size: 20 })
const hash = (value: number) => toHex(value, { size: 32 })
const entryPoint = entryPoint09Address.toLowerCase() as Address
const sender = address(2)
const paymaster = kernelSepoliaCanaryPaymaster
const { accountCodeHash } = kernelSepoliaCanaryProfile
const preparationId = hash(5)
const now = 1_790_000_000
const recoveryToken = `v1.${preparationId}.${hash(6)}.${now + 604_800}.${'7'.repeat(64)}`
const callData = encodeKernelCalls([{ to: address(7), data: '0x1234' }])
const validUntil = now + 120
const manifestWire = {
  version: 1,
  profile: { ...kernelSepoliaCanaryProfile, factory: getAddress(kernelSepoliaCanaryProfile.factory) },
  perpsRelease: PERPS_RELEASE.release.version,
  settlementToken: PERPS_ARBITRUM_SEPOLIA.settlementToken,
  marginClearinghouse: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
  orderRouter: PERPS_ARBITRUM_SEPOLIA.orderRouter,
  cfdEngine: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
  positionProtectionBook: PERPS_ARBITRUM_SEPOLIA.positionProtectionBook,
  protectionCommitsEnabled: false, paymaster, readiness: { eth: true, sponsored: true },
}
const manifest = parseKernelManifest(manifestWire)
const intent: KernelPreparationIntent = { preparationId, mode: 'eth', sender, callData, expectedNonce: 4n }
const rawOperation = {
  sender, nonce: '0x4', callData, callGasLimit: '0x10000', verificationGasLimit: '0x20000',
  preVerificationGas: '0x1000', maxFeePerGas: '0x64', maxPriorityFeePerGas: '0x1',
}
const ethCost = (65_536n + 131_072n + 4096n) * 100n
const ethPreparation = {
  version: 1, preparationId, mode: 'eth', entryPoint, operation: rawOperation, expiresAt: now + 100,
  maxNetworkGasWei: toHex(ethCost), recoveryToken,
}
const sponsoredCost = ethCost + 100_000n * 100n
const paymasterData = `0x${toHex(validUntil, { size: 6 }).slice(2)}${toHex(now - 30, { size: 6 }).slice(2)}${toHex(sponsoredCost, { size: 16 }).slice(2)}${hash(1).slice(2)}${accountCodeHash.slice(2)}${'ab'.repeat(65)}` as Hex
const sponsoredPreparation = {
  ...ethPreparation, mode: 'sponsored', expiresAt: validUntil - 30, maxNetworkGasWei: toHex(sponsoredCost),
  operation: { ...rawOperation, paymaster, paymasterData, paymasterVerificationGasLimit: toHex(100_000), paymasterPostOpGasLimit: '0x0' },
}

describe('Kernel manifest and quotes', () => {
  it('keeps readiness independent and binds the compiled Perps release', () => {
    expect(parseKernelManifest({ ...manifestWire, paymaster: null, readiness: { eth: true, sponsored: false } }).readiness)
      .toEqual({ eth: true, sponsored: false })
    expect(() => parseKernelManifest({ ...manifestWire, orderRouter: address(9) })).toThrow('invalid response')
    expect(() => parseKernelManifest({ ...manifestWire, perpsRelease: 'other' })).toThrow('invalid response')
    expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, entryPointVersion: '0.8' } })).toThrow('invalid response')
    expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, staker: undefined } })).toThrow('invalid response')
    expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, staker: manifestWire.profile.factory } })).toThrow('invalid response')
  })

  it('accepts the pinned profile in checksum casing and rejects alternate contract identities', () => {
    expect(manifest.profile).toEqual(kernelSepoliaCanaryProfile)
    for (const field of ['factory', 'staker', 'implementation', 'validator'] as const) {
      const codeHash = `${field}CodeHash` as const
      expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, [field]: address(9) } })).toThrow('invalid response')
      expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, [codeHash]: hash(9) } })).toThrow('invalid response')
    }
    expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, deployment: 'different-canary' } })).toThrow('invalid response')
    expect(() => parseKernelManifest({ ...manifestWire, paymaster: address(9) })).toThrow('invalid response')
    expect(() => parseKernelManifest({ ...manifestWire, paymaster: null })).toThrow('invalid response')
  })

  it('verifies both payer envelopes and computes distinct version 0.9 hashes', () => {
    const eth = parseKernelPreparation(ethPreparation, manifest, intent, now)
    const sponsored = parseKernelPreparation(sponsoredPreparation, manifest, { ...intent, mode: 'sponsored' }, now)
    expect(eth.operation).not.toHaveProperty('paymaster')
    expect(eth.maxNetworkGasWei).toBe(ethCost)
    expect(sponsored.operation.paymaster).toBe(paymaster)
    expect(sponsored.maxNetworkGasWei).toBe(sponsoredCost)
    expect(eth.userOperationHash).not.toBe(sponsored.userOperationHash)
    expect(() => assertKernelPreparedOperation(eth)).not.toThrow()
    expect(() => assertKernelPreparedOperation(sponsored)).not.toThrow()
  })

  it('binds single-call ETH withdrawals with checksummed recipients to normalized gateway calldata', () => {
    const recipient = getAddress(kernelSepoliaCanaryProfile.factory)
    const withdrawal = encodeKernelCalls([{ to: recipient, value: 1n }])
    const mixedCase = withdrawal.replace(recipient.slice(2).toLowerCase(), recipient.slice(2)) as Hex
    expect(mixedCase).not.toBe(withdrawal)
    const preparation = { ...ethPreparation, operation: { ...rawOperation, callData: mixedCase } }
    expect(parseKernelPreparation(preparation, manifest, { ...intent, callData: withdrawal }, now).operation.callData).toBe(withdrawal)
  })

  it('binds first deployment to the approved Staker', () => {
    const factoryData = '0x1234' as const
    const firstIntent = { ...intent, factory: manifest.profile.staker, factoryData }
    const firstPreparation = { ...ethPreparation, operation: { ...rawOperation, factory: manifest.profile.staker, factoryData } }
    expect(parseKernelPreparation(firstPreparation, manifest, firstIntent, now).operation.factory).toBe(manifest.profile.staker)
    expect(() => parseKernelPreparation({ ...firstPreparation, operation: { ...firstPreparation.operation, factory: manifest.profile.factory } },
      manifest, { ...firstIntent, factory: manifest.profile.factory }, now)).toThrow('invalid response')
  })

  it('rejects a changed payer, account, calls, fee bounds, nonce key, expiry or recovery scope', () => {
    for (const operation of [
      { ...rawOperation, paymaster: null }, { ...rawOperation, sender: address(4) }, { ...rawOperation, callData: '0x1234' },
      { ...rawOperation, maxFeePerGas: '0x2540be401' }, { ...rawOperation, nonce: '0x10000000000000000' },
      { ...rawOperation, nonce: '0x5' }, { ...rawOperation, factory: address(1) },
    ]) {
      expect(() => parseKernelPreparation({ ...ethPreparation, operation }, manifest, intent, now)).toThrow('invalid response')
    }
    for (const preparation of [
      { ...ethPreparation, expiresAt: now }, { ...ethPreparation, maxNetworkGasWei: '0x1' },
      { ...ethPreparation, recoveryToken: recoveryToken.replace(preparationId, hash(99)) },
    ]) {
      expect(() => parseKernelPreparation(preparation, manifest, intent, now)).toThrow('invalid response')
    }
    const eth = parseKernelPreparation(ethPreparation, manifest, intent, now)
    expect(() => assertKernelPreparedOperation({ ...eth, operation: { ...eth.operation, nonce: 5n } })).toThrow('invalid response')
  })

  it('admits Nitro-sized preverification estimates and rejects out-of-range gas or ETH prefund', () => {
    const quote = (preVerificationGas: bigint, maxFeePerGas: bigint) => ({
      ...ethPreparation,
      operation: { ...rawOperation, preVerificationGas: toHex(preVerificationGas), maxFeePerGas: toHex(maxFeePerGas) },
      maxNetworkGasWei: toHex((65_536n + 131_072n + preVerificationGas) * maxFeePerGas),
    })
    expect(parseKernelPreparation(quote(5_861_566n, 100n), manifest, intent, now).operation.preVerificationGas).toBe(5_861_566n)
    expect(() => parseKernelPreparation(quote(12_000_001n, 100n), manifest, intent, now)).toThrow('invalid response')
    expect(() => parseKernelPreparation(quote(12_000_000n, 10_000_000_000n), manifest, intent, now)).toThrow('invalid response')
  })

  it('posts the exact operation only to the same-origin route and rejects a different relay hash', async () => {
    const eth = parseKernelPreparation(ethPreparation, manifest, intent, now)
    const signature = `0x${'01'.repeat(65)}` as Hex
    const expected = [{ version: 1, preparationId, mode: 'eth', operation: { ...rawOperation, signature }, recoveryToken }]
    let returnedHash = eth.userOperationHash
    const fetcher = vi.spyOn(globalThis, 'fetch').mockImplementation(async (input, init) => {
      expect(input).toBe('/api/perps/v1/aa/kernel/rpc')
      expect(init).toMatchObject({ credentials: 'same-origin', redirect: 'error', cache: 'no-store' })
      const payload = JSON.parse(String(init?.body)) as { id: string }
      expect(payload).toMatchObject({ method: 'plether_sendUserOperation', params: expected })
      return Response.json({ jsonrpc: '2.0', id: payload.id, result: returnedHash })
    })
    try {
      await expect(sendKernelOperation(eth, signature)).resolves.toBe(eth.userOperationHash)
      returnedHash = hash(88)
      await expect(sendKernelOperation(eth, signature)).rejects.toThrow('invalid response')
    } finally {
      fetcher.mockRestore()
    }
  })

  it('keeps a validated ETH top-up suggestion in the error instead of issuing a preparation', async () => {
    const funding = {
      balanceWei: toHex(20n), entryPointDepositWei: toHex(50n), maxNetworkGasWei: toHex(100n),
      callValueWei: toHex(5n), shortfallWei: toHex(35n),
    }
    const fetcher = vi.spyOn(globalThis, 'fetch').mockImplementation(async (_input, init) => {
      const payload = JSON.parse(String(init?.body)) as { id: string; params: unknown[] }
      expect(payload.params[0]).not.toHaveProperty('expectedNonce')
      return Response.json({ jsonrpc: '2.0', id: payload.id, error: {
        code: -32001, message: 'Not enough ETH', data: { reason: 'INSUFFICIENT_GAS_ETH', retryable: false, funding },
      } }, { status: 400 })
    })
    try {
      const error: unknown = await prepareKernelOperation(manifest, intent).catch((cause: unknown) => cause)
      expect(findKernelGasFundingError(new Error('Review failed', { cause: error }))).toMatchObject({
        reason: 'INSUFFICIENT_GAS_ETH', funding: { balanceWei: 20n, entryPointDepositWei: 50n, maxNetworkGasWei: 100n, callValueWei: 5n, shortfallWei: 35n },
      })
      funding.shortfallWei = toHex(34n)
      await expect(prepareKernelOperation(manifest, intent)).rejects.toMatchObject({ reason: 'INVALID_RESPONSE' })
    } finally {
      fetcher.mockRestore()
    }
  })

  it('binds cancellation to the original nonce, self-call and Alto fee bump', async () => {
    const eth = parseKernelPreparation(ethPreparation, manifest, intent, now)
    const cancellationId = hash(14)
    const replacement = {
      ...ethPreparation, preparationId: cancellationId,
      operation: { ...rawOperation, callData: encodeKernelCalls([{ to: sender }]), maxFeePerGas: '0x6e', maxPriorityFeePerGas: '0x2' },
      maxNetworkGasWei: toHex(ethCost / 100n * 110n),
      cancelsUserOperationHash: eth.userOperationHash,
      recoveryToken: recoveryToken.replace(preparationId, cancellationId),
    }
    const fetcher = vi.spyOn(globalThis, 'fetch').mockImplementation(async (_input, init) => {
      const payload = JSON.parse(String(init?.body)) as { id: string }
      expect(payload).toMatchObject({
        method: 'plether_prepareUserOperationCancellation',
        params: [{ version: 1, originalPreparationId: preparationId, originalUserOperationHash: eth.userOperationHash,
          preparationId: cancellationId, recoveryToken }],
      })
      return Response.json({ jsonrpc: '2.0', id: payload.id, result: replacement }, { headers: { Date: new Date(now * 1000).toUTCString() } })
    })
    try {
      await expect(prepareKernelCancellation(manifest, eth, { preparationId: cancellationId }))
        .resolves.toMatchObject({ mode: 'eth', cancelsUserOperationHash: eth.userOperationHash, operation: { nonce: 4n } })
      replacement.operation.maxPriorityFeePerGas = '0x1'
      await expect(prepareKernelCancellation(manifest, eth, { preparationId: cancellationId })).rejects.toThrow('invalid response')
    } finally {
      fetcher.mockRestore()
    }
  })

  it('supplies the exact signed original and its token to recover a send the gateway never accepted', async () => {
    const eth = parseKernelPreparation(ethPreparation, manifest, intent, now)
    const cancellationId = hash(14)
    const originalSignature = `0x${'42'.repeat(65)}` as Hex
    const fetcher = vi.spyOn(globalThis, 'fetch').mockImplementationOnce(async (_input, init) => {
      const payload = JSON.parse(String(init?.body)) as { id: string }
      expect(payload).toMatchObject({
        method: 'plether_prepareUserOperationCancellation',
        params: [{ version: 1, originalPreparationId: preparationId, originalUserOperationHash: eth.userOperationHash,
          preparationId: cancellationId, recoveryToken, originalOperation: { ...rawOperation, signature: originalSignature } }],
      })
      return Response.json({ jsonrpc: '2.0', id: payload.id, error: {
        code: -32000, message: 'Cancellation is temporarily unavailable', data: { reason: 'CANCELLATION_UNAVAILABLE', retryable: true },
      } })
    })
    try {
      await expect(prepareKernelCancellation(manifest, eth, { preparationId: cancellationId, originalSignature }))
        .rejects.toMatchObject({ reason: 'CANCELLATION_UNAVAILABLE' })
      await expect(prepareKernelCancellation(manifest, eth, { preparationId: cancellationId, originalSignature: '0x42' }))
        .rejects.toMatchObject({ reason: 'INVALID_RESPONSE' })
      expect(fetcher).toHaveBeenCalledTimes(1)
    } finally {
      fetcher.mockRestore()
    }
  })
})

describe('Kernel recovery evidence', () => {
  const operationHash = hash(20)
  const cancellationHash = hash(21)
  const locator: KernelStatusLocator = { preparationId, userOperationHash: operationHash, sender, entryPoint, nonce: 4n, paymaster: zeroAddress }
  const logLocator = { address: entryPoint, transactionHash: hash(22), blockHash: hash(23), blockNumber: '0xaa', transactionIndex: '0x0', removed: false }
  const before = { ...logLocator, logIndex: '0x1', topics: encodeEventTopics({ abi: entryPoint09Abi, eventName: 'BeforeExecution' }), data: '0x' }
  function canonicalReceipt(userOpHash: Hex) {
    const event = {
      ...logLocator, logIndex: '0x2',
      topics: encodeEventTopics({ abi: entryPoint09Abi, eventName: 'UserOperationEvent', args: { userOpHash, sender, paymaster: zeroAddress } }),
      data: encodeAbiParameters(parseAbiParameters('uint256,bool,uint256,uint256'), [4n, false, 16n, 8n]),
    }
    return {
      userOpHash, sender, entryPoint, nonce: '0x4', paymaster: zeroAddress, actualGasCost: '0x10', actualGasUsed: '0x8', success: false,
      logs: [event], receipt: { ...logLocator, status: '0x1', logs: [before, event] },
    }
  }

  it('uses the canonical revert event and refuses crossed identities or scoped logs', () => {
    const receipt = canonicalReceipt(operationHash)
    expect(parseKernelOperationStatus({ state: 'included', receipt }, locator)).toMatchObject({
      state: 'included', receipt: { nonce: 4n, actualGasCost: 16n, actualGasUsed: 8n, success: false },
    })
    for (const invalid of [
      { ...receipt, actualGasCost: '0x11' }, { ...receipt, nonce: '0x5' },
      { ...receipt, paymaster }, { ...receipt, logs: [before, ...receipt.logs] },
    ]) {
      expect(() => parseKernelOperationStatus({ state: 'included', receipt: invalid }, locator)).toThrow('invalid response')
    }
    expect(() => parseKernelOperationStatus({ state: 'sponsorship_expired', receipt: null }, locator)).toThrow('invalid response')
  })

  it('labels a proven approved replacement even if it reverted and records unknown displacement', () => {
    const receipt = canonicalReceipt(cancellationHash)
    expect(parseKernelOperationStatus({ state: 'cancelled', receipt }, { ...locator, cancellationHash })).toMatchObject({
      state: 'cancelled', receipt: { userOpHash: cancellationHash, success: false },
    })
    expect(parseKernelOperationStatus({ state: 'cancelled', receipt }, locator)).toEqual({ state: 'nonce_consumed', receipt: null })
  })
})
