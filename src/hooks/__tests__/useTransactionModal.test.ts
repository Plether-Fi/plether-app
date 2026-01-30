import { describe, it, expect, beforeEach } from 'vitest'
import { useTransactionModal, useCurrentTransaction, useIsCurrentTransactionInProgress } from '../useTransactionModal'
import { useTransactionStore } from '../../stores/transactionStore'
import { renderHook } from '@testing-library/react'

describe('useTransactionModal', () => {
  beforeEach(() => {
    useTransactionModal.getState().reset()
    useTransactionStore.setState({ transactions: [] })
  })

  describe('open and close', () => {
    it('opens modal and sets current index', () => {
      const txId = 'test-tx-123'
      useTransactionStore.getState().addTransaction({
        id: txId,
        type: 'mint',
        status: 'pending',
        title: 'Minting',
        steps: [{ label: 'Step 1', status: 'pending' }],
      })

      useTransactionModal.getState().open({ transactionId: txId })

      const state = useTransactionModal.getState()
      expect(state.isOpen).toBe(true)
      expect(state.currentIndex).toBe(0)
    })

    it('closes modal', () => {
      const txId = 'test-tx-123'
      useTransactionStore.getState().addTransaction({
        id: txId,
        type: 'mint',
        status: 'pending',
        title: 'Minting',
        steps: [{ label: 'Step 1', status: 'pending' }],
      })

      useTransactionModal.getState().open({ transactionId: txId })
      useTransactionModal.getState().close()

      const state = useTransactionModal.getState()
      expect(state.isOpen).toBe(false)
    })

    it('stores onRetry callback per transaction', () => {
      const txId = 'test-tx-123'
      const mockRetry = () => {}
      useTransactionStore.getState().addTransaction({
        id: txId,
        type: 'mint',
        status: 'pending',
        title: 'Minting',
        steps: [{ label: 'Step 1', status: 'pending' }],
      })

      useTransactionModal.getState().open({ transactionId: txId, onRetry: mockRetry })

      expect(useTransactionModal.getState().getRetryCallback(txId)).toBe(mockRetry)
    })

    it('stores multiple retry callbacks for parallel transactions', () => {
      const mockRetry1 = () => {}
      const mockRetry2 = () => {}

      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'First',
        steps: [],
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        title: 'Second',
        steps: [],
      })

      useTransactionModal.getState().open({ transactionId: 'tx-1', onRetry: mockRetry1 })
      useTransactionModal.getState().open({ transactionId: 'tx-2', onRetry: mockRetry2 })

      expect(useTransactionModal.getState().getRetryCallback('tx-1')).toBe(mockRetry1)
      expect(useTransactionModal.getState().getRetryCallback('tx-2')).toBe(mockRetry2)
    })
  })

  describe('navigation', () => {
    it('navigatePrev moves to previous transaction', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'success',
        title: 'First',
        steps: [],
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        title: 'Second',
        steps: [],
      })

      useTransactionModal.getState().open({ transactionId: 'tx-2' })
      expect(useTransactionModal.getState().currentIndex).toBe(1)

      useTransactionModal.getState().navigatePrev()

      expect(useTransactionModal.getState().currentIndex).toBe(0)
    })

    it('navigateNext moves to next transaction', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'success',
        title: 'First',
        steps: [],
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        title: 'Second',
        steps: [],
      })

      useTransactionModal.getState().open({ transactionId: 'tx-2' })
      useTransactionModal.getState().navigatePrev()
      expect(useTransactionModal.getState().currentIndex).toBe(0)

      useTransactionModal.getState().navigateNext()

      expect(useTransactionModal.getState().currentIndex).toBe(1)
    })

    it('cannot navigate before first transaction', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'First',
        steps: [],
      })

      useTransactionModal.getState().open({ transactionId: 'tx-1' })
      useTransactionModal.getState().navigatePrev()

      expect(useTransactionModal.getState().currentIndex).toBe(0)
    })

    it('viewTransaction jumps to specific index', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'success',
        title: 'First',
        steps: [],
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'success',
        title: 'Second',
        steps: [],
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-3',
        type: 'burn',
        status: 'pending',
        title: 'Third',
        steps: [],
      })

      useTransactionModal.getState().open({ transactionId: 'tx-3' })
      useTransactionModal.getState().viewTransaction(0)

      expect(useTransactionModal.getState().currentIndex).toBe(0)
    })
  })

  describe('useCurrentTransaction', () => {
    it('returns transaction at current index', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'Test Transaction',
        steps: [{ label: 'Step 1', status: 'pending' }],
      })

      useTransactionModal.getState().open({ transactionId: 'tx-1' })

      const { result } = renderHook(() => useCurrentTransaction())
      expect(result.current?.id).toBe('tx-1')
      expect(result.current?.title).toBe('Test Transaction')
    })

    it('returns transaction at navigated index', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'success',
        title: 'First',
        steps: [],
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        title: 'Second',
        steps: [],
      })

      useTransactionModal.getState().open({ transactionId: 'tx-2' })
      useTransactionModal.getState().navigatePrev()

      const { result } = renderHook(() => useCurrentTransaction())
      expect(result.current?.id).toBe('tx-1')
      expect(result.current?.title).toBe('First')
    })

    it('returns null when no transaction at index', () => {
      const { result } = renderHook(() => useCurrentTransaction())
      expect(result.current).toBeNull()
    })
  })

  describe('useIsCurrentTransactionInProgress', () => {
    it('returns true for pending transaction', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'Test',
        steps: [],
      })

      useTransactionModal.getState().open({ transactionId: 'tx-1' })

      const { result } = renderHook(() => useIsCurrentTransactionInProgress())
      expect(result.current).toBe(true)
    })

    it('returns true for confirming transaction', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'confirming',
        title: 'Test',
        steps: [],
      })

      useTransactionModal.getState().open({ transactionId: 'tx-1' })

      const { result } = renderHook(() => useIsCurrentTransactionInProgress())
      expect(result.current).toBe(true)
    })

    it('returns false for completed transaction', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'success',
        title: 'Test',
        steps: [],
      })

      useTransactionModal.getState().open({ transactionId: 'tx-1' })

      const { result } = renderHook(() => useIsCurrentTransactionInProgress())
      expect(result.current).toBe(false)
    })

    it('returns false for failed transaction', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'failed',
        title: 'Test',
        steps: [],
      })

      useTransactionModal.getState().open({ transactionId: 'tx-1' })

      const { result } = renderHook(() => useIsCurrentTransactionInProgress())
      expect(result.current).toBe(false)
    })
  })

  describe('step updates via transactionStore', () => {
    it('setStepInProgress updates step in store', () => {
      const txId = 'tx-1'
      useTransactionStore.getState().addTransaction({
        id: txId,
        type: 'mint',
        status: 'pending',
        title: 'Minting',
        steps: [
          { label: 'Approve', status: 'pending' },
          { label: 'Mint', status: 'pending' },
        ],
      })

      useTransactionStore.getState().setStepInProgress(txId, 0)

      const tx = useTransactionStore.getState().transactions[0]
      expect(tx.steps[0].status).toBe('in_progress')
      expect(tx.steps[1].status).toBe('pending')
    })

    it('setStepInProgress marks previous steps as completed', () => {
      const txId = 'tx-1'
      useTransactionStore.getState().addTransaction({
        id: txId,
        type: 'mint',
        status: 'pending',
        title: 'Minting',
        steps: [
          { label: 'Step 1', status: 'pending' },
          { label: 'Step 2', status: 'pending' },
          { label: 'Step 3', status: 'pending' },
        ],
      })

      useTransactionStore.getState().setStepInProgress(txId, 2)

      const tx = useTransactionStore.getState().transactions[0]
      expect(tx.steps[0].status).toBe('completed')
      expect(tx.steps[1].status).toBe('completed')
      expect(tx.steps[2].status).toBe('in_progress')
    })

    it('setStepError sets step to error state', () => {
      const txId = 'tx-1'
      useTransactionStore.getState().addTransaction({
        id: txId,
        type: 'mint',
        status: 'pending',
        title: 'Minting',
        steps: [
          { label: 'Approve', status: 'completed' },
          { label: 'Mint', status: 'in_progress' },
        ],
      })

      useTransactionStore.getState().setStepError(txId, 1, 'User rejected')

      const tx = useTransactionStore.getState().transactions[0]
      expect(tx.steps[0].status).toBe('completed')
      expect(tx.steps[1].status).toBe('error')
      expect(tx.status).toBe('failed')
      expect(tx.errorMessage).toBe('User rejected')
    })

    it('setStepSuccess marks all steps as completed', () => {
      const txId = 'tx-1'
      useTransactionStore.getState().addTransaction({
        id: txId,
        type: 'mint',
        status: 'confirming',
        title: 'Minting',
        steps: [
          { label: 'Approve', status: 'completed' },
          { label: 'Mint', status: 'in_progress' },
        ],
      })

      useTransactionStore.getState().setStepSuccess(txId, '0xhash123')

      const tx = useTransactionStore.getState().transactions[0]
      expect(tx.steps[0].status).toBe('completed')
      expect(tx.steps[1].status).toBe('completed')
      expect(tx.status).toBe('success')
      expect(tx.hash).toBe('0xhash123')
    })
  })
})
