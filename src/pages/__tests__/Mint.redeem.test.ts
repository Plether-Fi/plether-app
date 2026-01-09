import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act, waitFor } from '@testing-library/react'
import { useState, useEffect, useRef } from 'react'

const mockBurn = vi.fn()
const mockApproveBear = vi.fn()
const mockApproveBull = vi.fn()
const mockRefetchBearAllowance = vi.fn()
const mockRefetchBullAllowance = vi.fn()

let mockBearAllowance = 0n
let mockBullAllowance = 0n
let mockBearApproveSuccess = false
let mockBullApproveSuccess = false

type PendingAction = 'mint' | 'burn' | null

function useMintPageLogicFixed() {
  const [pendingAction, setPendingAction] = useState<PendingAction>(null)
  const pendingAmountRef = useRef<bigint>(0n)
  const bearApproveHandledRef = useRef(false)
  const bullApproveHandledRef = useRef(false)

  const pairAmountBigInt = 100n * 10n ** 18n
  const needsBearApproval = mockBearAllowance < pairAmountBigInt
  const needsBullApproval = mockBullAllowance < pairAmountBigInt

  useEffect(() => {
    if (mockBearApproveSuccess && !bearApproveHandledRef.current) {
      bearApproveHandledRef.current = true
      mockRefetchBearAllowance()
      if (pendingAction === 'burn' && pendingAmountRef.current > 0n) {
        if (mockBullAllowance < pendingAmountRef.current) {
          mockApproveBull(pendingAmountRef.current)
        } else {
          mockBurn(pendingAmountRef.current)
          pendingAmountRef.current = 0n
          setPendingAction(null)
        }
      }
    }
  }, [mockBearApproveSuccess, pendingAction, mockBullAllowance])

  useEffect(() => {
    if (mockBullApproveSuccess && !bullApproveHandledRef.current) {
      bullApproveHandledRef.current = true
      mockRefetchBullAllowance()
      if (pendingAction === 'burn' && pendingAmountRef.current > 0n) {
        mockBurn(pendingAmountRef.current)
        pendingAmountRef.current = 0n
        setPendingAction(null)
      }
    }
  }, [mockBullApproveSuccess, pendingAction])

  const handleRedeem = async () => {
    bearApproveHandledRef.current = false
    bullApproveHandledRef.current = false

    if (needsBearApproval) {
      pendingAmountRef.current = pairAmountBigInt
      setPendingAction('burn')
      await mockApproveBear(pairAmountBigInt)
      return
    }
    if (needsBullApproval) {
      pendingAmountRef.current = pairAmountBigInt
      setPendingAction('burn')
      await mockApproveBull(pairAmountBigInt)
      return
    }
    await mockBurn(pairAmountBigInt)
  }

  return { handleRedeem, pendingAction }
}

describe('Mint page redeem flow - demonstrates why refs are needed', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockBearAllowance = 0n
    mockBullAllowance = 0n
    mockBearApproveSuccess = false
    mockBullApproveSuccess = false
  })

  it('effect runs on every dependency change when condition stays true', async () => {
    let effectRunCount = 0

    function useEffectCounter() {
      const [triggerCount, setTriggerCount] = useState(0)

      useEffect(() => {
        if (mockBearApproveSuccess) {
          effectRunCount++
        }
      }, [triggerCount])

      return { trigger: () => setTriggerCount(c => c + 1) }
    }

    mockBearApproveSuccess = true
    const { result } = renderHook(() => useEffectCounter())

    expect(effectRunCount).toBe(1)

    await act(async () => {
      result.current.trigger()
    })

    await act(async () => {
      result.current.trigger()
    })

    expect(effectRunCount).toBe(3)
  })
})

describe('Mint page redeem flow - FIXED logic', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockBearAllowance = 0n
    mockBullAllowance = 0n
    mockBearApproveSuccess = false
    mockBullApproveSuccess = false
  })

  it('should call approveBear when BEAR allowance is insufficient', async () => {
    const { result } = renderHook(() => useMintPageLogicFixed())

    await act(async () => {
      await result.current.handleRedeem()
    })

    expect(mockApproveBear).toHaveBeenCalledTimes(1)
    expect(mockApproveBear).toHaveBeenCalledWith(100n * 10n ** 18n)
    expect(result.current.pendingAction).toBe('burn')
  })

  it('should call approveBull after BEAR approval succeeds', async () => {
    const { result, rerender } = renderHook(() => useMintPageLogicFixed())

    await act(async () => {
      await result.current.handleRedeem()
    })

    expect(mockApproveBear).toHaveBeenCalledTimes(1)

    // Simulate BEAR approval success
    mockBearApproveSuccess = true
    mockBearAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockApproveBull).toHaveBeenCalledTimes(1)
    })
  })

  it('should call burn after BULL approval succeeds', async () => {
    const { result, rerender } = renderHook(() => useMintPageLogicFixed())

    await act(async () => {
      await result.current.handleRedeem()
    })

    // Simulate BEAR approval success
    mockBearApproveSuccess = true
    mockBearAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockApproveBull).toHaveBeenCalledTimes(1)
    })

    // Simulate BULL approval success
    mockBullApproveSuccess = true
    mockBullAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockBurn).toHaveBeenCalledTimes(1)
      expect(mockBurn).toHaveBeenCalledWith(100n * 10n ** 18n)
    })
  })

  it('FIXED: should NOT call approveBull multiple times when dependencies change', async () => {
    const { result, rerender } = renderHook(() => useMintPageLogicFixed())

    await act(async () => {
      await result.current.handleRedeem()
    })

    // Simulate BEAR approval success
    mockBearApproveSuccess = true
    mockBearAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockApproveBull).toHaveBeenCalledTimes(1)
    })

    // Rerender multiple times while bearApproveSuccess is still true
    rerender()
    rerender()
    rerender()

    // FIXED: approveBull only called once
    expect(mockApproveBull).toHaveBeenCalledTimes(1)
  })

  it('FIXED: should NOT call burn multiple times when dependencies change', async () => {
    const { result, rerender } = renderHook(() => useMintPageLogicFixed())

    await act(async () => {
      await result.current.handleRedeem()
    })

    // Simulate BEAR approval success
    mockBearApproveSuccess = true
    mockBearAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockApproveBull).toHaveBeenCalledTimes(1)
    })

    // Simulate BULL approval success
    mockBullApproveSuccess = true
    mockBullAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockBurn).toHaveBeenCalledTimes(1)
    })

    // Rerender multiple times while bullApproveSuccess is still true
    rerender()
    rerender()
    rerender()

    // FIXED: burn only called once
    expect(mockBurn).toHaveBeenCalledTimes(1)
  })

  it('should skip BEAR approval if already approved', async () => {
    mockBearAllowance = 100n * 10n ** 18n // Already approved

    const { result } = renderHook(() => useMintPageLogicFixed())

    await act(async () => {
      await result.current.handleRedeem()
    })

    expect(mockApproveBear).not.toHaveBeenCalled()
    expect(mockApproveBull).toHaveBeenCalledTimes(1)
  })

  it('should skip both approvals if already approved', async () => {
    mockBearAllowance = 100n * 10n ** 18n
    mockBullAllowance = 100n * 10n ** 18n

    const { result } = renderHook(() => useMintPageLogicFixed())

    await act(async () => {
      await result.current.handleRedeem()
    })

    expect(mockApproveBear).not.toHaveBeenCalled()
    expect(mockApproveBull).not.toHaveBeenCalled()
    expect(mockBurn).toHaveBeenCalledTimes(1)
  })

  it('should go directly to burn after BEAR approval if BULL already approved', async () => {
    mockBullAllowance = 100n * 10n ** 18n // BULL already approved

    const { result, rerender } = renderHook(() => useMintPageLogicFixed())

    await act(async () => {
      await result.current.handleRedeem()
    })

    expect(mockApproveBear).toHaveBeenCalledTimes(1)

    // Simulate BEAR approval success
    mockBearApproveSuccess = true
    mockBearAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      // Should skip approveBull and go directly to burn
      expect(mockApproveBull).not.toHaveBeenCalled()
      expect(mockBurn).toHaveBeenCalledTimes(1)
    })
  })
})
