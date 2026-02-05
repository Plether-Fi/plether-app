import { describe, it, expect, beforeEach, vi } from 'vitest'
import { useTransactionStore } from '../transactionStore'

describe('transactionStore', () => {
  beforeEach(() => {
    useTransactionStore.setState({ transactions: [] })
  })

  describe('addTransaction', () => {
    it('adds a transaction with timestamp', () => {
      const now = Date.now()
      vi.setSystemTime(now)

      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'Minting tokens',
        steps: [{ label: 'Mint', status: 'pending' }],
      })

      const { transactions } = useTransactionStore.getState()
      expect(transactions).toHaveLength(1)
      expect(transactions[0]).toEqual({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'Minting tokens',
        steps: [{ label: 'Mint', status: 'pending' }],
        timestamp: now,
      })

      vi.useRealTimers()
    })

    it('appends to existing transactions', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'First tx',
        steps: [{ label: 'Mint', status: 'pending' }],
      })

      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        title: 'Second tx',
        steps: [{ label: 'Swap', status: 'pending' }],
      })

      const { transactions } = useTransactionStore.getState()
      expect(transactions).toHaveLength(2)
      expect(transactions[0].id).toBe('tx-1')
      expect(transactions[1].id).toBe('tx-2')
    })
  })

  describe('updateTransaction', () => {
    it('updates transaction status', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'Minting tokens',
        steps: [{ label: 'Mint', status: 'pending' }],
      })

      useTransactionStore.getState().updateTransaction('tx-1', {
        status: 'confirming',
        hash: '0xabc123',
      })

      const { transactions } = useTransactionStore.getState()
      expect(transactions[0].status).toBe('confirming')
      expect(transactions[0].hash).toBe('0xabc123')
    })

    it('only updates the matching transaction', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'First',
        steps: [{ label: 'Mint', status: 'pending' }],
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        title: 'Second',
        steps: [{ label: 'Swap', status: 'pending' }],
      })

      useTransactionStore.getState().updateTransaction('tx-1', {
        status: 'success',
      })

      const { transactions } = useTransactionStore.getState()
      expect(transactions[0].status).toBe('success')
      expect(transactions[1].status).toBe('pending')
    })

    it('does nothing if transaction not found', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'Test',
        steps: [{ label: 'Mint', status: 'pending' }],
      })

      useTransactionStore.getState().updateTransaction('non-existent', {
        status: 'success',
      })

      const { transactions } = useTransactionStore.getState()
      expect(transactions[0].status).toBe('pending')
    })
  })

  describe('removeTransaction', () => {
    it('removes transaction by id', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'Test',
        steps: [{ label: 'Mint', status: 'pending' }],
      })

      useTransactionStore.getState().removeTransaction('tx-1')

      const { transactions } = useTransactionStore.getState()
      expect(transactions).toHaveLength(0)
    })

    it('only removes the matching transaction', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'First',
        steps: [{ label: 'Mint', status: 'pending' }],
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        title: 'Second',
        steps: [{ label: 'Swap', status: 'pending' }],
      })

      useTransactionStore.getState().removeTransaction('tx-1')

      const { transactions } = useTransactionStore.getState()
      expect(transactions).toHaveLength(1)
      expect(transactions[0].id).toBe('tx-2')
    })
  })

  describe('clearTransactions', () => {
    it('removes all transactions', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        title: 'First',
        steps: [{ label: 'Mint', status: 'pending' }],
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        title: 'Second',
        steps: [{ label: 'Swap', status: 'pending' }],
      })

      useTransactionStore.getState().clearTransactions()

      const { transactions } = useTransactionStore.getState()
      expect(transactions).toHaveLength(0)
    })
  })

  describe('cleanupOldTransactions', () => {
    it('keeps recent completed/failed transactions for history (24h)', () => {
      const now = Date.now()
      vi.setSystemTime(now)

      useTransactionStore.setState({
        transactions: [
          { id: 'tx-1', type: 'mint', status: 'success', title: 'Done', steps: [], timestamp: now },
          { id: 'tx-2', type: 'swap', status: 'failed', title: 'Failed', steps: [], timestamp: now },
          { id: 'tx-3', type: 'burn', status: 'pending', title: 'Pending', steps: [], timestamp: now },
        ],
      })

      useTransactionStore.getState().cleanupOldTransactions()

      const { transactions } = useTransactionStore.getState()
      expect(transactions).toHaveLength(3)

      vi.useRealTimers()
    })

    it('removes completed transactions older than 24 hours', () => {
      const now = Date.now()
      const twentyFiveHoursAgo = now - 25 * 60 * 60 * 1000
      vi.setSystemTime(now)

      useTransactionStore.setState({
        transactions: [
          { id: 'tx-old-success', type: 'mint', status: 'success', title: 'Old success', steps: [], timestamp: twentyFiveHoursAgo },
          { id: 'tx-old-failed', type: 'swap', status: 'failed', title: 'Old failed', steps: [], timestamp: twentyFiveHoursAgo },
          { id: 'tx-new-success', type: 'burn', status: 'success', title: 'New success', steps: [], timestamp: now },
        ],
      })

      useTransactionStore.getState().cleanupOldTransactions()

      const { transactions } = useTransactionStore.getState()
      expect(transactions).toHaveLength(1)
      expect(transactions[0].id).toBe('tx-new-success')

      vi.useRealTimers()
    })

    it('removes old pending transactions (> 1 hour)', () => {
      const now = Date.now()
      const twoHoursAgo = now - 2 * 60 * 60 * 1000
      vi.setSystemTime(now)

      useTransactionStore.setState({
        transactions: [
          { id: 'tx-old', type: 'mint', status: 'pending', title: 'Old', steps: [], timestamp: twoHoursAgo },
          { id: 'tx-new', type: 'swap', status: 'pending', title: 'New', steps: [], timestamp: now },
        ],
      })

      useTransactionStore.getState().cleanupOldTransactions()

      const { transactions } = useTransactionStore.getState()
      expect(transactions).toHaveLength(1)
      expect(transactions[0].id).toBe('tx-new')

      vi.useRealTimers()
    })

    it('keeps recent pending and confirming transactions', () => {
      const now = Date.now()
      vi.setSystemTime(now)

      useTransactionStore.setState({
        transactions: [
          { id: 'tx-1', type: 'mint', status: 'pending', title: 'Pending', steps: [], timestamp: now },
          { id: 'tx-2', type: 'swap', status: 'confirming', title: 'Confirming', steps: [], timestamp: now },
        ],
      })

      useTransactionStore.getState().cleanupOldTransactions()

      const { transactions } = useTransactionStore.getState()
      expect(transactions).toHaveLength(2)

      vi.useRealTimers()
    })
  })
})
