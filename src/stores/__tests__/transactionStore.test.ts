import { describe, it, expect, beforeEach, vi } from 'vitest'
import { useTransactionStore } from '../transactionStore'

describe('transactionStore', () => {
  beforeEach(() => {
    useTransactionStore.setState({ pendingTransactions: [] })
  })

  describe('addTransaction', () => {
    it('adds a transaction with timestamp', () => {
      const now = Date.now()
      vi.setSystemTime(now)

      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        description: 'Minting tokens',
      })

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions).toHaveLength(1)
      expect(pendingTransactions[0]).toEqual({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        description: 'Minting tokens',
        timestamp: now,
      })

      vi.useRealTimers()
    })

    it('appends to existing transactions', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        description: 'First tx',
      })

      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        description: 'Second tx',
      })

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions).toHaveLength(2)
      expect(pendingTransactions[0].id).toBe('tx-1')
      expect(pendingTransactions[1].id).toBe('tx-2')
    })
  })

  describe('updateTransaction', () => {
    it('updates transaction status', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        description: 'Minting tokens',
      })

      useTransactionStore.getState().updateTransaction('tx-1', {
        status: 'confirming',
        hash: '0xabc123',
      })

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions[0].status).toBe('confirming')
      expect(pendingTransactions[0].hash).toBe('0xabc123')
    })

    it('only updates the matching transaction', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        description: 'First',
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        description: 'Second',
      })

      useTransactionStore.getState().updateTransaction('tx-1', {
        status: 'success',
      })

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions[0].status).toBe('success')
      expect(pendingTransactions[1].status).toBe('pending')
    })

    it('does nothing if transaction not found', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        description: 'Test',
      })

      useTransactionStore.getState().updateTransaction('non-existent', {
        status: 'success',
      })

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions[0].status).toBe('pending')
    })
  })

  describe('removeTransaction', () => {
    it('removes transaction by id', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        description: 'Test',
      })

      useTransactionStore.getState().removeTransaction('tx-1')

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions).toHaveLength(0)
    })

    it('only removes the matching transaction', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        description: 'First',
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        description: 'Second',
      })

      useTransactionStore.getState().removeTransaction('tx-1')

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions).toHaveLength(1)
      expect(pendingTransactions[0].id).toBe('tx-2')
    })
  })

  describe('clearTransactions', () => {
    it('removes all transactions', () => {
      useTransactionStore.getState().addTransaction({
        id: 'tx-1',
        type: 'mint',
        status: 'pending',
        description: 'First',
      })
      useTransactionStore.getState().addTransaction({
        id: 'tx-2',
        type: 'swap',
        status: 'pending',
        description: 'Second',
      })

      useTransactionStore.getState().clearTransactions()

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions).toHaveLength(0)
    })
  })

  describe('cleanupOldTransactions', () => {
    it('removes completed transactions', () => {
      const now = Date.now()
      vi.setSystemTime(now)

      useTransactionStore.setState({
        pendingTransactions: [
          { id: 'tx-1', type: 'mint', status: 'success', description: 'Done', timestamp: now },
          { id: 'tx-2', type: 'swap', status: 'failed', description: 'Failed', timestamp: now },
          { id: 'tx-3', type: 'burn', status: 'pending', description: 'Pending', timestamp: now },
        ],
      })

      useTransactionStore.getState().cleanupOldTransactions()

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions).toHaveLength(1)
      expect(pendingTransactions[0].id).toBe('tx-3')

      vi.useRealTimers()
    })

    it('removes old pending transactions (> 1 hour)', () => {
      const now = Date.now()
      const twoHoursAgo = now - 2 * 60 * 60 * 1000
      vi.setSystemTime(now)

      useTransactionStore.setState({
        pendingTransactions: [
          { id: 'tx-old', type: 'mint', status: 'pending', description: 'Old', timestamp: twoHoursAgo },
          { id: 'tx-new', type: 'swap', status: 'pending', description: 'New', timestamp: now },
        ],
      })

      useTransactionStore.getState().cleanupOldTransactions()

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions).toHaveLength(1)
      expect(pendingTransactions[0].id).toBe('tx-new')

      vi.useRealTimers()
    })

    it('keeps recent pending and confirming transactions', () => {
      const now = Date.now()
      vi.setSystemTime(now)

      useTransactionStore.setState({
        pendingTransactions: [
          { id: 'tx-1', type: 'mint', status: 'pending', description: 'Pending', timestamp: now },
          { id: 'tx-2', type: 'swap', status: 'confirming', description: 'Confirming', timestamp: now },
        ],
      })

      useTransactionStore.getState().cleanupOldTransactions()

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions).toHaveLength(2)

      vi.useRealTimers()
    })
  })
})
