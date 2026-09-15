import { describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import type { SponsoredOperation } from '../operationStore'
import type { PerpsAaSmartAccountRuntime } from '../runtimeContext'
import { withCloseAssistanceRecovery } from '../closeAssistanceRecovery'

const account = '0x1111111111111111111111111111111111111111' as Address
const owner = '0x2222222222222222222222222222222222222222' as Address
const paymaster = '0x3333333333333333333333333333333333333333' as Address
const hash = `0x${'ab'.repeat(32)}` as Hex
const otherHash = `0x${'cd'.repeat(32)}` as Hex
function runtime() {
  return { chainId: 421614, ownerAddress: owner,
    smartAccount: { accountAddress: account, entryPoint: account,
      getUserOperationReceipt: vi.fn(async () => ({ success: true })),
      getUserOperationStatus: vi.fn(async () => ({ status: 'included' })),
    }, getRecoverySnapshot: vi.fn(async () => ({ blockNumber: 100n })),
  } as unknown as PerpsAaSmartAccountRuntime
}
function operation() {
  return { chainId: 421614, ownerAddress: owner, accountAddress: account, userOperationHash: hash,
    sponsorshipAuthority: { version: 1, paymasterAddress: paymaster, validUntil: '1' },
    orderRequestV2: { closeAssistance: { amountUsdc: '198000', lens: account, lensCodeHash: hash, paymasterAddress: paymaster } },
  } as SponsoredOperation
}
describe('sponsored close recovery routing', () => {
  it('recovers a journaled native operation under a legacy runtime after issuance has ended', async () => {
    const legacy = runtime(), native = runtime(), factory = vi.fn(async () => native)
    const routed = withCloseAssistanceRecovery(legacy, () => [operation()], factory)
    await routed.smartAccount.getUserOperationReceipt(hash)
    await routed.smartAccount.getUserOperationStatus(hash)
    await routed.getRecoverySnapshot!(hash)
    expect(native.smartAccount.getUserOperationReceipt).toHaveBeenCalledWith(hash)
    expect(native.getRecoverySnapshot).toHaveBeenCalledWith(hash, undefined)
    expect(factory).toHaveBeenCalledTimes(1)
    expect(legacy.smartAccount.getUserOperationReceipt).not.toHaveBeenCalled()
    await routed.smartAccount.getUserOperationReceipt(otherHash)
    expect(legacy.smartAccount.getUserOperationReceipt).toHaveBeenCalledWith(otherHash)
  })
  it('rejects a mismatched persisted paymaster instead of guessing a recovery route', async () => {
    const entry = operation()
    entry.sponsorshipAuthority!.paymasterAddress = owner
    const factory = vi.fn(async () => runtime())
    const routed = withCloseAssistanceRecovery(runtime(), () => [entry], factory)
    await expect(routed.smartAccount.getUserOperationReceipt(hash)).rejects.toThrow('authority')
    expect(factory).not.toHaveBeenCalled()
  })
})
