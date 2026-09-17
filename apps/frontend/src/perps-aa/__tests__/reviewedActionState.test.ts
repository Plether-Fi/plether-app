import { describe, expect, it, vi } from 'vitest'
import type { PublicClient } from 'viem'
import { readReviewedActionState } from '../reviewedActionState'
import type { PerpsAaDeploymentManifest } from '../manifest'
const account = '0x1111111111111111111111111111111111111111'
const manifest = { cfdEngine: account, positionProtectionBook: account, orderLifecycleBook: account } as PerpsAaDeploymentManifest
function fixture() {
  let size = 10n, trigger = 20n, orderId = 0n
  const readContract = vi.fn(async ({ functionName }: { functionName: string }) => {
    if (functionName === 'positions') return [size, 100n, 200n, 300n, 0, 123n, 0n]
    if (functionName === 'activePositionProtectionId') return 1n
    if (functionName === 'getPositionProtection') return { takeProfitTriggerPrice: trigger }
    if (functionName === 'clientIntent') return { orderId }
  })
  return { client: { getBlockNumber: vi.fn(async () => 100n), readContract } as unknown as PublicClient,
    readContract, changePosition: () => { size++ }, changeProtection: () => { trigger++ }, consumeIntent: () => { orderId++ } }
}
describe('reviewed position and protection bindings', () => {
  it('binds state reads to one explicit block and detects changed positions and protection parameters', async () => {
    const f = fixture()
    const read = () => readReviewedActionState(f.client, manifest, account, { action: 'replace-protection' })
    const original = await read()
    expect(await read()).toBe(original)
    expect(f.readContract.mock.calls.every(([input]) => 'blockNumber' in input && input.blockNumber === 100n)).toBe(true)
    f.changePosition()
    const changed = await read()
    expect(changed).not.toBe(original)
    f.changeProtection()
    expect(await read()).not.toBe(changed)
  })
  it('refuses an already committed client intent', async () => {
    const f = fixture()
    f.consumeIntent()
    await expect(readReviewedActionState(f.client, manifest, account, { action: 'place-order', clientOrderId: `0x${'11'.repeat(32)}` })).rejects.toThrow('already committed')
  })
  it('leaves deposit authorization validity and consumed-state checks to exact batch simulation', async () => {
    const f = fixture()
    expect(await readReviewedActionState(f.client, manifest, account, { action: 'deposit' })).toBe('v1:no-position-binding')
    expect(f.readContract).not.toHaveBeenCalled()
  })
})
