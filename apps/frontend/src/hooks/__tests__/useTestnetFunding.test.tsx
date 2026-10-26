import { act, renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { PERPS_RELEASE_IS_DEPLOYED } from '../../contracts/perpsAddresses'

const mocks = vi.hoisted(() => ({ mint: vi.fn(), code: vi.fn(), balance: vi.fn(), deposit: vi.fn() }))
vi.mock('wagmi', () => ({
  useWriteContract: () => ({ writeContractAsync: mocks.mint }),
  usePublicClient: () => ({ getCode: mocks.code, readContract: mocks.balance }),
}))
vi.mock('../../perps-aa', () => ({ usePerpsIdentity: () => ({
  accountAddress: '0x2222222222222222222222222222222222222222',
  ownerAddress: '0x1111111111111111111111111111111111111111',
  chainId: 421614, status: 'ready', isAaManifestConfigured: true, sponsorshipEnabled: true,
}) }))
vi.mock('../usePerpsTrading', () => ({ usePerpsTrading: () => ({ depositMargin: mocks.deposit }) }))
import { useTestnetFunding } from '../useTestnetFunding'

beforeEach(() => vi.clearAllMocks())
describe('prepared release funding gate', () => {
  it.each([true, false])('does not read or send transactions while prepared, even when enabled=%s', async enabled => {
    expect(PERPS_RELEASE_IS_DEPLOYED).toBe(false)
    const account = '0x2222222222222222222222222222222222222222'
    const { result } = renderHook(() => useTestnetFunding(account, enabled))
    await act(async () => { await result.current.requestFunds(account) })
    for (const mock of Object.values(mocks)) expect(mock).not.toHaveBeenCalled()
    expect(result.current.phase).toBe('idle')
  })
})
