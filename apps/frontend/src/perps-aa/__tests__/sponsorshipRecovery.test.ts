import { describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import type { SponsoredOperation } from '../operationStore'
import type { PerpsAaSmartAccountRuntime } from '../runtimeContext'
import { withSponsorshipRecovery } from '../sponsorshipRecovery'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { historicalSponsorshipManifest } from '../historicalSponsorship'
import { parsePerpsAaManifest } from '../manifest'

const account = '0x1111111111111111111111111111111111111111' as Address
const owner = '0x2222222222222222222222222222222222222222' as Address
const paymaster = '0x3333333333333333333333333333333333333333' as Address
const hash = `0x${'ab'.repeat(32)}` as Hex
const otherHash = `0x${'cd'.repeat(32)}` as Hex
const manifest = historicalSponsorshipManifest(parsePerpsAaManifest(rawManifest), { paymasterAddress: paymaster })
function runtime() {
  return { chainId: 421614, ownerAddress: owner, factoryAddress: manifest.smartAccountFactory,
    accountVersion: manifest.smartAccountVersion, accountIndex: manifest.smartAccountIndex,
    smartAccount: { accountAddress: account, entryPoint: manifest.entryPoint,
      getUserOperationReceipt: vi.fn(async () => ({ success: true })),
      getUserOperationStatus: vi.fn(async () => ({ status: 'included' })),
    }, getRecoverySnapshot: vi.fn(async () => ({ blockNumber: 100n })),
  } as unknown as PerpsAaSmartAccountRuntime
}
function operation() {
  return { chainId: 421614, ownerAddress: owner, accountAddress: account, userOperationHash: hash,
    sponsorshipAuthority: { version: 1, paymasterAddress: paymaster, validUntil: '1' },
    orderRequestV3: { closeAssistance: { amountUsdc: '198000', lens: account, lensCodeHash: hash, paymasterAddress: paymaster } },
  } as SponsoredOperation
}
describe('sponsorship recovery routing', () => {
  it('recovers a journaled native operation under a legacy runtime after issuance has ended', async () => {
    const legacy = runtime(), native = runtime(), factory = vi.fn(async () => native)
    const routed = withSponsorshipRecovery(legacy, () => [operation()], factory)
    await routed.smartAccount.getUserOperationReceipt(hash)
    await routed.smartAccount.getUserOperationStatus(hash)
    const evidence = { transactionHash: otherHash, nonce: 0n, paymaster }
    await routed.getRecoverySnapshot!(hash, undefined, evidence)
    expect(native.smartAccount.getUserOperationReceipt).toHaveBeenCalledWith(hash)
    expect(native.getRecoverySnapshot).toHaveBeenCalledWith(hash, undefined, evidence)
    expect(factory).toHaveBeenCalledTimes(1)
    expect(legacy.smartAccount.getUserOperationReceipt).not.toHaveBeenCalled()
    await routed.smartAccount.getUserOperationReceipt(otherHash)
    expect(legacy.smartAccount.getUserOperationReceipt).toHaveBeenCalledWith(otherHash)
  })
  it('routes ordinary native operations using their journaled paymaster after a flag change', async () => {
    const entry = operation()
    delete entry.orderRequestV3
    const native = runtime(), factory = vi.fn(async () => native)
    const routed = withSponsorshipRecovery(runtime(), () => [entry], factory)
    await routed.smartAccount.getUserOperationReceipt(hash)
    expect(factory).toHaveBeenCalledWith({ paymasterAddress: paymaster })
    expect(native.smartAccount.getUserOperationReceipt).toHaveBeenCalledWith(hash)
  })
  it('rejects a mismatched persisted paymaster instead of guessing a recovery route', async () => {
    const entry = operation()
    entry.sponsorshipAuthority!.paymasterAddress = owner
    const factory = vi.fn(async () => runtime())
    const routed = withSponsorshipRecovery(runtime(), () => [entry], factory)
    await expect(routed.smartAccount.getUserOperationReceipt(hash)).rejects.toThrow('authority')
    expect(factory).not.toHaveBeenCalled()
  })

  it.each(['ownerAddress', 'accountAddress', 'chainId'] as const)('does not route a foreign journal with changed %s', async field => {
    const entry = operation()
    if (field === 'chainId') entry.chainId = 1
    else entry[field] = paymaster
    const base = runtime(), factory = vi.fn(async () => runtime())
    const routed = withSponsorshipRecovery(base, () => [entry], factory)
    await routed.smartAccount.getUserOperationReceipt(hash)
    expect(base.smartAccount.getUserOperationReceipt).toHaveBeenCalledWith(hash)
    expect(factory).not.toHaveBeenCalled()
  })

  it.each(['ownerAddress', 'accountAddress', 'entryPoint', 'chainId'] as const)('rejects a recovery runtime with changed %s', async field => {
    const recovered = runtime()
    if (field === 'chainId') recovered.chainId = 1
    else if (field === 'ownerAddress') recovered.ownerAddress = paymaster
    else recovered.smartAccount[field] = paymaster
    const routed = withSponsorshipRecovery(runtime(), () => [operation()], async () => recovered)
    await expect(routed.smartAccount.getUserOperationReceipt(hash)).rejects.toThrow('account changed')
    expect(recovered.smartAccount.getUserOperationReceipt).not.toHaveBeenCalled()
  })

  it('requires the recorded sponsorship authority for historical grants', async () => {
    const entry = operation()
    delete entry.sponsorshipAuthority
    const factory = vi.fn(async () => runtime())
    const routed = withSponsorshipRecovery(runtime(), () => [entry], factory)
    await expect(routed.smartAccount.getUserOperationReceipt(hash)).rejects.toThrow('authority')
    expect(factory).not.toHaveBeenCalled()
  })

  function preparedOperation() {
    const entry = operation()
    delete entry.orderRequestV3
    entry.nativePreparation = { version: 1, preparationId: 'recorded-preparation', manifest,
      action: { kind: 'place-order', account, calls: [] } }
    return entry
  }

  it('recovers generic prepared operations through their complete recorded manifest', async () => {
    const entry = preparedOperation()
    const prepared = runtime(), historicalFactory = vi.fn(async () => runtime()), preparedFactory = vi.fn(async () => prepared)
    const routed = withSponsorshipRecovery(runtime(), () => [entry], historicalFactory, preparedFactory)
    await routed.smartAccount.getUserOperationReceipt(hash)
    await routed.smartAccount.getUserOperationStatus(hash)
    await routed.getPreparedOperationRuntime!(manifest)
    expect(preparedFactory).toHaveBeenCalledExactlyOnceWith(manifest)
    expect(prepared.smartAccount.getUserOperationReceipt).toHaveBeenCalledWith(hash)
    expect(prepared.smartAccount.getUserOperationStatus).toHaveBeenCalledWith(hash)
    expect(historicalFactory).not.toHaveBeenCalled()
  })

  it('rejects preparation authority mismatch before constructing a runtime', async () => {
    const entry = preparedOperation()
    entry.sponsorshipAuthority!.paymasterAddress = owner
    const factory = vi.fn(async () => runtime())
    const routed = withSponsorshipRecovery(runtime(), () => [entry], factory, factory)
    await expect(routed.smartAccount.getUserOperationReceipt(hash)).rejects.toThrow('authority')
    expect(factory).not.toHaveBeenCalled()
  })

  it.each(['chainId', 'entryPoint', 'smartAccountFactory', 'smartAccountVersion', 'smartAccountIndex'] as const)(
    'rejects changed prepared deployment %s', async field => {
      const entry = preparedOperation()
      const changed = { ...manifest, [field]: field === 'chainId' ? 1 : field === 'smartAccountIndex' ? '1' : paymaster }
      entry.nativePreparation!.manifest = changed
      const factory = vi.fn(async () => runtime())
      const routed = withSponsorshipRecovery(runtime(), () => [entry], factory, factory)
      await expect(routed.smartAccount.getUserOperationReceipt(hash)).rejects.toThrow('deployment identity changed')
      expect(factory).not.toHaveBeenCalled()
    },
  )

  it.each(['ownerAddress', 'accountAddress', 'chainId'] as const)('rejects a prepared runtime with changed %s', async field => {
    const recovered = runtime()
    if (field === 'chainId') recovered.chainId = 1
    else if (field === 'ownerAddress') recovered.ownerAddress = paymaster
    else recovered.smartAccount.accountAddress = paymaster
    const routed = withSponsorshipRecovery(runtime(), () => [preparedOperation()], async () => runtime(), async () => recovered)
    await expect(routed.smartAccount.getUserOperationReceipt(hash)).rejects.toThrow('prepared account changed')
    expect(recovered.smartAccount.getUserOperationReceipt).not.toHaveBeenCalled()
  })
})
