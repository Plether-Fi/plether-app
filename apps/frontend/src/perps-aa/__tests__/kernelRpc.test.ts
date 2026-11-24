import { describe, expect, it, vi } from 'vitest'
import { encodeAbiParameters, encodeEventTopics, getAddress, parseAbiParameters, toHex, zeroAddress, type Address, type Hex } from 'viem'
import { entryPoint09Abi, entryPoint09Address } from 'viem/account-abstraction'
import kernelDeployment from '../../../../backend/test/fixtures/kernel-deployment.json'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_RELEASE } from '../../contracts/perpsAddresses'
import { encodeKernelCalls } from '../kernelAccount'
import * as kernelDeploymentPins from '../kernelDeploymentPins'
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
const paymaster = address(3)
const { kernelSepoliaCanaryProfile } = kernelDeploymentPins
const { profile, gas } = kernelDeployment
const { accountCodeHash } = profile
const preparationId = hash(5)
const now = 1_790_000_000
const recoveryToken = `v1.${preparationId}.${hash(6)}.${now + 604_800}.${'7'.repeat(64)}`
const callData = encodeKernelCalls([{ to: address(7), data: '0x1234' }])
const validUntil = now + 120
const manifestWire = {
  version: 1,
  profile: { ...profile, factory: getAddress(profile.factory) },
  perpsRelease: PERPS_RELEASE.release.version,
  settlementToken: PERPS_ARBITRUM_SEPOLIA.settlementToken,
  marginClearinghouse: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
  orderRouter: PERPS_ARBITRUM_SEPOLIA.orderRouter,
  cfdEngine: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
  positionProtectionBook: PERPS_ARBITRUM_SEPOLIA.positionProtectionBook,
  protectionCommitsEnabled: false, paymaster: null, readiness: { eth: true, sponsored: false },
}
const manifest = parseKernelManifest(manifestWire)
const sponsoredManifest = { ...manifest, paymaster, readiness: { ...manifest.readiness, sponsored: true } }
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
const encodePaymasterData = (cost: bigint) => `0x${toHex(validUntil, { size: 6 }).slice(2)}${toHex(now - 30, { size: 6 }).slice(2)}${toHex(cost, { size: 16 }).slice(2)}${hash(1).slice(2)}${accountCodeHash.slice(2)}${'ab'.repeat(65)}` as Hex
const sponsoredPreparation = {
  ...ethPreparation, mode: 'sponsored', expiresAt: validUntil - 30, maxNetworkGasWei: toHex(sponsoredCost),
  operation: { ...rawOperation, paymaster, paymasterData: encodePaymasterData(sponsoredCost), paymasterVerificationGasLimit: toHex(100_000), paymasterPostOpGasLimit: '0x0' },
}

type GasField = Exclude<keyof typeof gas, 'aggregateGas' | 'ethMaxPrefundWei' | 'nitroFixturePreVerificationGas'>

function parseGasQuote(overrides: Partial<Record<GasField, number>>, mode: KernelPreparationIntent['mode'] = 'eth') {
  const preparation = mode === 'eth' ? ethPreparation : sponsoredPreparation
  const operation: Record<string, string> = {
    ...preparation.operation,
    ...Object.fromEntries(Object.entries(overrides).map(([field, value]) => [field, toHex(value)])),
  }
  const units = BigInt(operation.callGasLimit) + BigInt(operation.verificationGasLimit) + BigInt(operation.preVerificationGas) +
    BigInt(operation.paymasterVerificationGasLimit ?? 0) + BigInt(operation.paymasterPostOpGasLimit ?? 0)
  const cost = units * BigInt(operation.maxFeePerGas)
  if (mode === 'sponsored') operation.paymasterData = encodePaymasterData(cost)
  return parseKernelPreparation({ ...preparation, operation, maxNetworkGasWei: toHex(cost) },
    mode === 'eth' ? manifest : sponsoredManifest, { ...intent, mode }, now)
}

describe('Kernel manifest and quotes', () => {
  it('keeps readiness independent and binds the compiled Perps release', () => {
    expect(manifest.readiness).toEqual({ eth: true, sponsored: false })
    expect(() => parseKernelManifest({ ...manifestWire, orderRouter: address(9) })).toThrow('invalid response')
    expect(() => parseKernelManifest({ ...manifestWire, perpsRelease: 'other' })).toThrow('invalid response')
    expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, entryPointVersion: '0.8' } })).toThrow('invalid response')
    expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, factory: undefined } })).toThrow('invalid response')
    expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, staker: address(9) } })).toThrow('invalid response')
  })

  it('accepts the pinned profile in checksum casing and rejects alternate contract identities', () => {
    expect(kernelSepoliaCanaryProfile).toEqual(profile)
    expect(manifest.profile).toEqual(profile)
    for (const field of ['factory', 'implementation', 'validator'] as const) {
      const codeHash = `${field}CodeHash` as const
      expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, [field]: address(9) } })).toThrow('invalid response')
      expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, [codeHash]: hash(9) } })).toThrow('invalid response')
    }
    expect(() => parseKernelManifest({ ...manifestWire, profile: { ...manifestWire.profile, deployment: 'different-canary' } })).toThrow('invalid response')
  })

  it('keeps ETH available and sponsorship blocked while the gateway paymaster is unpinned', () => {
    const pin = vi.spyOn(kernelDeploymentPins, 'kernelSepoliaCanaryPaymaster', 'get').mockReturnValue(undefined)
    try {
      const paused = parseKernelManifest({ ...manifestWire, paymaster })
      expect(paused).toMatchObject({ paymaster: null, readiness: { eth: true, sponsored: false } })
      expect(parseKernelPreparation(ethPreparation, paused, intent, now).mode).toBe('eth')
      expect(() => parseKernelPreparation(sponsoredPreparation, paused, { ...intent, mode: 'sponsored' }, now)).toThrow('invalid response')
      expect(() => parseKernelManifest({ ...manifestWire, paymaster: '0x1234' })).toThrow('invalid response')
      const readiness = { eth: true, sponsored: true }
      expect(() => parseKernelManifest({ ...manifestWire, paymaster, readiness })).toThrow('invalid response')
      expect(() => parseKernelManifest({ ...manifestWire, readiness })).toThrow('invalid response')
    } finally {
      pin.mockRestore()
    }
  })

  it('requires the attested direct paymaster even when sponsorship is paused', () => {
    const attestedPaymaster = '0x9ff288e30489c4e29b13d6ba960612b5d6db8d50'
    expect(kernelDeploymentPins.kernelSepoliaCanaryPaymaster).toEqual({
      address: attestedPaymaster,
      codeHash: '0x0ff0bb843a663a4a22dbcc64b2efda8654eca777298d867a812db9073cd8c115',
    })
    const advertisedPaymaster = getAddress(attestedPaymaster)
    const paused = parseKernelManifest({ ...manifestWire, paymaster: advertisedPaymaster })
    expect(paused).toMatchObject({ paymaster: attestedPaymaster, readiness: { eth: true, sponsored: false } })
    const readiness = { eth: true, sponsored: true }
    expect(parseKernelManifest({ ...manifestWire, paymaster: advertisedPaymaster, readiness }))
      .toMatchObject({ paymaster: attestedPaymaster, readiness })
    expect(() => parseKernelManifest({ ...manifestWire, paymaster })).toThrow('invalid response')
    expect(() => parseKernelManifest({ ...manifestWire, readiness })).toThrow('invalid response')
  })

  it('verifies both payer envelopes and computes distinct version 0.9 hashes', () => {
    const eth = parseKernelPreparation(ethPreparation, manifest, intent, now)
    const sponsored = parseKernelPreparation(sponsoredPreparation, sponsoredManifest, { ...intent, mode: 'sponsored' }, now)
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

  it('binds first deployment to the approved direct factory and reviewed calldata', () => {
    const factoryData = '0x1234' as const
    const factory = manifest.profile.factory
    const firstIntent = { ...intent, factory, factoryData }
    const firstPreparation = { ...ethPreparation, operation: { ...rawOperation, factory, factoryData } }
    expect(parseKernelPreparation(firstPreparation, manifest, firstIntent, now).operation.factory).toBe(factory)
    const otherFactory = address(9)
    expect(() => parseKernelPreparation({ ...firstPreparation, operation: { ...firstPreparation.operation, factory: otherFactory } },
      manifest, { ...firstIntent, factory: otherFactory }, now)).toThrow('invalid response')
    expect(() => parseKernelPreparation({ ...firstPreparation, operation: { ...firstPreparation.operation, factoryData: '0x4321' } },
      manifest, firstIntent, now)).toThrow('invalid response')
  })

  it('rejects a changed payer, account, calls, nonce key, expiry or recovery scope', () => {
    for (const operation of [
      { ...rawOperation, paymaster: null }, { ...rawOperation, sender: address(4) }, { ...rawOperation, callData: '0x1234' },
      { ...rawOperation, nonce: '0x10000000000000000' },
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

  it('admits the recorded local Nitro preverification estimate', () => {
    const { nitroFixturePreVerificationGas } = gas
    expect(parseGasQuote({ preVerificationGas: nitroFixturePreVerificationGas }).operation.preVerificationGas)
      .toBe(BigInt(nitroFixturePreVerificationGas))
  })

  it.each(['callGasLimit', 'verificationGasLimit', 'preVerificationGas', 'maxFeePerGas', 'maxPriorityFeePerGas'] as const)(
    'admits the ETH %s limit and rejects one above', field => {
      const maximum = gas[field]
      const fees = field === 'maxPriorityFeePerGas' ? { maxFeePerGas: gas.maxFeePerGas } : {}
      expect(parseGasQuote({ ...fees, [field]: maximum }).operation[field]).toBe(BigInt(maximum))
      expect(() => parseGasQuote({ ...fees, [field]: maximum + 1 })).toThrow('invalid response')
    },
  )

  it.each(['paymasterVerificationGasLimit', 'paymasterPostOpGasLimit'] as const)(
    'admits the sponsored %s limit and rejects one above', field => {
      const maximum = gas[field]
      expect(parseGasQuote({ [field]: maximum }, 'sponsored').operation[field]).toBe(BigInt(maximum))
      expect(() => parseGasQuote({ [field]: maximum + 1 }, 'sponsored')).toThrow('invalid response')
    },
  )

  it('admits all components of the sponsored aggregate gas limit together', () => {
    const { callGasLimit, verificationGasLimit, preVerificationGas, paymasterVerificationGasLimit, paymasterPostOpGasLimit } = gas
    const limits = { callGasLimit, verificationGasLimit, preVerificationGas, paymasterVerificationGasLimit, paymasterPostOpGasLimit }
    expect(parseGasQuote(limits, 'sponsored').maxNetworkGasWei).toBe(BigInt(gas.aggregateGas) * BigInt(rawOperation.maxFeePerGas))
  })

  it('admits an ETH quote at the prefund limit and rejects the next gas unit', () => {
    const maximum = BigInt(gas.ethMaxPrefundWei)
    const maxFeePerGas = gas.maxFeePerGas
    const baseGas = BigInt(rawOperation.callGasLimit) + BigInt(rawOperation.verificationGasLimit)
    const preVerificationGas = Number(maximum / BigInt(maxFeePerGas) - baseGas)
    expect(parseGasQuote({ preVerificationGas, maxFeePerGas }).maxNetworkGasWei).toBe(maximum)
    expect(() => parseGasQuote({ preVerificationGas: preVerificationGas + 1, maxFeePerGas })).toThrow('invalid response')
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

      const maximum = BigInt(gas.ethMaxPrefundWei)
      funding.maxNetworkGasWei = toHex(maximum)
      funding.shortfallWei = toHex(maximum - 65n)
      await expect(prepareKernelOperation(manifest, intent)).rejects.toMatchObject({
        reason: 'INSUFFICIENT_GAS_ETH', funding: { maxNetworkGasWei: maximum },
      })
      funding.maxNetworkGasWei = toHex(maximum + 1n)
      funding.shortfallWei = toHex(maximum - 64n)
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

  it.each(['included', 'observed'] as const)('uses the exact revert event and refuses crossed identities or scoped logs for %s', state => {
    const receipt = canonicalReceipt(operationHash)
    expect(parseKernelOperationStatus({ state, receipt }, locator)).toMatchObject({
      state, receipt: { nonce: 4n, actualGasCost: 16n, actualGasUsed: 8n, success: false },
    })
    for (const invalid of [
      { ...receipt, actualGasCost: '0x11' }, { ...receipt, nonce: '0x5' },
      { ...receipt, paymaster }, { ...receipt, logs: [before, ...receipt.logs] },
    ]) {
      expect(() => parseKernelOperationStatus({ state, receipt: invalid }, locator)).toThrow('invalid response')
    }
    expect(() => parseKernelOperationStatus({ state, receipt: canonicalReceipt(cancellationHash) }, locator)).toThrow('invalid response')
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
