import { act, renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { useTransactionStore } from '../../stores/transactionStore'
import { useTransactionModal } from '../useTransactionModal'
import { useTransactionSequence } from '../useTransactionSequence'

const mocks = vi.hoisted(() => ({
  waitForTransactionReceipt: vi.fn(),
  getTransaction: vi.fn(),
  call: vi.fn(),
}))

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
})
