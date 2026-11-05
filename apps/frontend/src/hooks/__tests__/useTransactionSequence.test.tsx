import { act, renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { captureAnalyticsEvent } from '../../analytics/client'
import { useTransactionStore } from '../../stores/transactionStore'
import { useTransactionModal } from '../useTransactionModal'
import { useTransactionSequence } from '../useTransactionSequence'

const mocks = vi.hoisted(() => ({
  waitForTransactionReceipt: vi.fn(),
  getTransaction: vi.fn(),
  call: vi.fn(),
}))

vi.mock('../../analytics/client', () => ({ captureAnalyticsEvent: vi.fn(), captureFrontendLog: vi.fn() }))

vi.mock('wagmi', () => ({
  usePublicClient: () => ({
    waitForTransactionReceipt: mocks.waitForTransactionReceipt,
    getTransaction: mocks.getTransaction,
    call: mocks.call,
  }),
}))

describe('useTransactionSequence', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    useTransactionModal.getState().reset()
    useTransactionStore.setState({ transactions: [], activeOperations: {} })
    mocks.waitForTransactionReceipt.mockResolvedValue({
      status: 'success',
      blockNumber: 1n,
    })
  })

  it('reports inline progress without opening the shared transaction modal', async () => {
    const { result } = renderHook(() => useTransactionSequence())
    const action = vi.fn().mockResolvedValue('0xabc')

    await act(async () => {
      await result.current.execute({
        title: 'Queue vault deposit',
        type: 'supply',
        showModal: false,
        buildSteps: () => [{ label: 'Queue deposit', action }],
      })
    })

    expect(action).toHaveBeenCalledTimes(1)
    expect(useTransactionModal.getState().isOpen).toBe(false)
    expect(useTransactionStore.getState().transactions).toHaveLength(0)
    expect(result.current.status).toBe('success')
    expect(result.current.phase).toBe('complete')
    expect(result.current.steps).toEqual(['Queue deposit'])
    expect(result.current.hash).toBe('0xabc')
  })

  it('continues opening the shared transaction modal by default', async () => {
    const { result } = renderHook(() => useTransactionSequence())

    await act(async () => {
      await result.current.execute({
        title: 'Claim vault shares',
        type: 'supply',
        buildSteps: () => [{
          label: 'Claim shares',
          action: vi.fn().mockResolvedValue('0xdef'),
        }],
      })
    })

    expect(useTransactionModal.getState().isOpen).toBe(true)
    expect(useTransactionStore.getState().transactions).toHaveLength(1)
    expect(result.current.status).toBe('success')
  })
  it('shows a reference for embedded preparation errors and records it once', async () => {
    const { result } = renderHook(() => useTransactionSequence())
    await act(async () => {
      await result.current.execute({ title: 'Deposit', type: 'supply', showModal: false,
        buildSteps: () => { throw new Error('Failed to fetch') },
      })
    })
    expect(result.current.error).toContain('Support reference:')
    expect(result.current.status).toBe('error')
    expect(captureAnalyticsEvent).toHaveBeenCalledWith('transaction failed', expect.objectContaining({
      surface: 'vault', action_kind: 'supply', stage: 'preparation', reason_code: 'NETWORK_ERROR',
      support_reference: result.current.error!.split('Support reference: ')[1],
    }))
  })

  it('keeps wallet rejection in the submission stage after an earlier approval succeeds', async () => {
    const { result } = renderHook(() => useTransactionSequence())
    await act(async () => {
      await result.current.execute({ title: 'Deposit', type: 'supply', buildSteps: () => [
        { label: 'Approve', action: async () => '0xabc' },
        { label: 'Deposit', action: () => Promise.reject(Object.assign(new Error('User rejected the request'), { code: 4001 })) },
      ] })
    })
    const event = vi.mocked(captureAnalyticsEvent).mock.calls.find(([name]) => name === 'transaction failed')!
    expect(event[1]).toMatchObject({ stage: 'submission', reason_code: 'WALLET_DECLINED' })
    expect(useTransactionStore.getState().transactions[0].supportReference).toBe(event[1]?.support_reference)
    expect(vi.mocked(captureAnalyticsEvent).mock.calls.filter(([name]) => name === 'transaction failed')).toHaveLength(1)
  })

})
