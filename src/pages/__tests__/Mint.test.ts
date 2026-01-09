import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act, waitFor } from '@testing-library/react'
import { useState, useEffect, useRef } from 'react'
import { parseUnits } from 'viem'

// ============================================================================
// Decimal Conversion Tests (Pure logic - no mocking needed)
// ============================================================================

describe('Mint page decimal conversions', () => {
  function calculatePairAmount(inputAmount: string, mode: 'mint' | 'redeem'): bigint {
    if (!inputAmount || isNaN(parseFloat(inputAmount))) return 0n
    try {
      if (mode === 'mint') {
        const pairAmount = parseFloat(inputAmount) / 2
        return parseUnits(pairAmount.toString(), 18)
      } else {
        return parseUnits(inputAmount, 18)
      }
    } catch {
      return 0n
    }
  }

  function calculateUsdcAmount(inputAmount: string): bigint {
    if (!inputAmount || isNaN(parseFloat(inputAmount))) return 0n
    try {
      return parseUnits(inputAmount, 6)
    } catch {
      return 0n
    }
  }

  describe('mint mode - USDC to pair conversion', () => {
    it('converts 100 USDC to 50 pairs (18 decimals)', () => {
      const pairAmount = calculatePairAmount('100', 'mint')
      expect(pairAmount).toBe(50n * 10n ** 18n)
    })

    it('converts 2 USDC to 1 pair', () => {
      const pairAmount = calculatePairAmount('2', 'mint')
      expect(pairAmount).toBe(1n * 10n ** 18n)
    })

    it('converts 1 USDC to 0.5 pairs', () => {
      const pairAmount = calculatePairAmount('1', 'mint')
      expect(pairAmount).toBe(5n * 10n ** 17n)
    })

    it('handles decimal USDC input', () => {
      const pairAmount = calculatePairAmount('10.5', 'mint')
      expect(pairAmount).toBe(parseUnits('5.25', 18))
    })

    it('returns 0n for empty input', () => {
      expect(calculatePairAmount('', 'mint')).toBe(0n)
    })

    it('returns 0n for invalid input', () => {
      expect(calculatePairAmount('abc', 'mint')).toBe(0n)
    })
  })

  describe('redeem mode - pair amount (no conversion)', () => {
    it('uses input directly as pair amount (18 decimals)', () => {
      const pairAmount = calculatePairAmount('50', 'redeem')
      expect(pairAmount).toBe(50n * 10n ** 18n)
    })

    it('handles decimal pair input', () => {
      const pairAmount = calculatePairAmount('1.5', 'redeem')
      expect(pairAmount).toBe(parseUnits('1.5', 18))
    })
  })

  describe('USDC amount for approval (6 decimals)', () => {
    it('converts 100 USDC to correct bigint', () => {
      const usdcAmount = calculateUsdcAmount('100')
      expect(usdcAmount).toBe(100n * 10n ** 6n)
    })

    it('converts 0.01 USDC (1 cent) correctly', () => {
      const usdcAmount = calculateUsdcAmount('0.01')
      expect(usdcAmount).toBe(10000n)
    })

    it('handles large amounts', () => {
      const usdcAmount = calculateUsdcAmount('1000000')
      expect(usdcAmount).toBe(1000000n * 10n ** 6n)
    })
  })

  describe('CRITICAL: pair vs USDC amount difference', () => {
    it('pair amount and USDC amount are different for same input', () => {
      const input = '100'
      const pairAmount = calculatePairAmount(input, 'mint')
      const usdcAmount = calculateUsdcAmount(input)

      // This is the bug we had: sending usdcAmount to mint() instead of pairAmount
      expect(pairAmount).toBe(50n * 10n ** 18n)  // 50 with 18 decimals
      expect(usdcAmount).toBe(100n * 10n ** 6n)  // 100 with 6 decimals

      // They should NOT be equal - this was the original bug
      expect(pairAmount).not.toBe(usdcAmount)
    })
  })
})

// ============================================================================
// Output Display Tests
// ============================================================================

describe('Mint page output display', () => {
  function calculateOutputDisplay(inputAmount: string, mode: 'mint' | 'redeem'): string {
    const inputNum = parseFloat(inputAmount) || 0
    if (mode === 'mint') {
      return (inputNum / 2).toFixed(4)
    } else {
      return (inputNum * 2).toFixed(2)
    }
  }

  describe('mint mode - shows pairs received', () => {
    it('100 USDC shows 50.0000 pairs', () => {
      expect(calculateOutputDisplay('100', 'mint')).toBe('50.0000')
    })

    it('1 USDC shows 0.5000 pairs', () => {
      expect(calculateOutputDisplay('1', 'mint')).toBe('0.5000')
    })

    it('empty input shows 0.0000', () => {
      expect(calculateOutputDisplay('', 'mint')).toBe('0.0000')
    })
  })

  describe('redeem mode - shows USDC received', () => {
    it('50 pairs shows 100.00 USDC', () => {
      expect(calculateOutputDisplay('50', 'redeem')).toBe('100.00')
    })

    it('1 pair shows 2.00 USDC', () => {
      expect(calculateOutputDisplay('1', 'redeem')).toBe('2.00')
    })

    it('0.5 pairs shows 1.00 USDC', () => {
      expect(calculateOutputDisplay('0.5', 'redeem')).toBe('1.00')
    })
  })
})

// ============================================================================
// Balance Validation Tests
// ============================================================================

describe('Mint page balance validation', () => {
  function getMinBalance(bearBalance: bigint, bullBalance: bigint): bigint {
    return bearBalance < bullBalance ? bearBalance : bullBalance
  }

  it('returns BEAR balance when BEAR < BULL', () => {
    const bearBalance = 100n * 10n ** 18n
    const bullBalance = 200n * 10n ** 18n
    expect(getMinBalance(bearBalance, bullBalance)).toBe(bearBalance)
  })

  it('returns BULL balance when BULL < BEAR', () => {
    const bearBalance = 200n * 10n ** 18n
    const bullBalance = 100n * 10n ** 18n
    expect(getMinBalance(bearBalance, bullBalance)).toBe(bullBalance)
  })

  it('returns either when equal', () => {
    const balance = 100n * 10n ** 18n
    expect(getMinBalance(balance, balance)).toBe(balance)
  })

  it('returns 0 when one balance is 0', () => {
    expect(getMinBalance(0n, 100n * 10n ** 18n)).toBe(0n)
    expect(getMinBalance(100n * 10n ** 18n, 0n)).toBe(0n)
  })
})

// ============================================================================
// Mint Flow Tests (USDC approval → mint)
// ============================================================================

const mockMint = vi.fn()
const mockApproveUsdc = vi.fn()
const mockRefetchUsdcAllowance = vi.fn()

let mockUsdcAllowance = 0n
let mockUsdcApproveSuccess = false
let mockMintSuccess = false

function useMintFlowLogic(inputAmount: string) {
  const [pendingAction, setPendingAction] = useState<'mint' | 'burn' | null>(null)
  const pendingAmountRef = useRef<bigint>(0n)
  const usdcApproveHandledRef = useRef(false)

  const pairAmountBigInt = (() => {
    if (!inputAmount || isNaN(parseFloat(inputAmount))) return 0n
    try {
      const pairAmount = parseFloat(inputAmount) / 2
      return parseUnits(pairAmount.toString(), 18)
    } catch {
      return 0n
    }
  })()

  const usdcAmountBigInt = (() => {
    if (!inputAmount || isNaN(parseFloat(inputAmount))) return 0n
    try {
      return parseUnits(inputAmount, 6)
    } catch {
      return 0n
    }
  })()

  const needsUsdcApproval = usdcAmountBigInt > 0n && mockUsdcAllowance < usdcAmountBigInt

  useEffect(() => {
    if (mockUsdcApproveSuccess && !usdcApproveHandledRef.current) {
      usdcApproveHandledRef.current = true
      mockRefetchUsdcAllowance()
      if (pendingAction === 'mint' && pendingAmountRef.current > 0n) {
        mockMint(pendingAmountRef.current)
        pendingAmountRef.current = 0n
        setPendingAction(null)
      }
    }
  }, [mockUsdcApproveSuccess, pendingAction])

  useEffect(() => {
    if (mockMintSuccess) {
      // Reset state on success
    }
  }, [mockMintSuccess])

  const handleMint = async () => {
    usdcApproveHandledRef.current = false
    if (needsUsdcApproval) {
      pendingAmountRef.current = pairAmountBigInt
      setPendingAction('mint')
      await mockApproveUsdc(usdcAmountBigInt)
      return
    }
    await mockMint(pairAmountBigInt)
  }

  return {
    handleMint,
    pendingAction,
    pairAmountBigInt,
    usdcAmountBigInt,
    needsUsdcApproval,
  }
}

describe('Mint flow - USDC approval chain', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockUsdcAllowance = 0n
    mockUsdcApproveSuccess = false
    mockMintSuccess = false
  })

  it('should call approveUsdc with USDC amount (6 decimals)', async () => {
    const { result } = renderHook(() => useMintFlowLogic('100'))

    await act(async () => {
      await result.current.handleMint()
    })

    expect(mockApproveUsdc).toHaveBeenCalledTimes(1)
    expect(mockApproveUsdc).toHaveBeenCalledWith(100n * 10n ** 6n)
  })

  it('should call mint with pair amount (18 decimals) after approval', async () => {
    const { result, rerender } = renderHook(() => useMintFlowLogic('100'))

    await act(async () => {
      await result.current.handleMint()
    })

    expect(mockApproveUsdc).toHaveBeenCalledTimes(1)

    mockUsdcApproveSuccess = true
    mockUsdcAllowance = 100n * 10n ** 6n
    rerender()

    await waitFor(() => {
      expect(mockMint).toHaveBeenCalledTimes(1)
      expect(mockMint).toHaveBeenCalledWith(50n * 10n ** 18n)
    })
  })

  it('should skip approval if USDC already approved', async () => {
    mockUsdcAllowance = 100n * 10n ** 6n

    const { result } = renderHook(() => useMintFlowLogic('100'))

    await act(async () => {
      await result.current.handleMint()
    })

    expect(mockApproveUsdc).not.toHaveBeenCalled()
    expect(mockMint).toHaveBeenCalledTimes(1)
    expect(mockMint).toHaveBeenCalledWith(50n * 10n ** 18n)
  })

  it('should NOT call mint multiple times when dependencies change', async () => {
    const { result, rerender } = renderHook(() => useMintFlowLogic('100'))

    await act(async () => {
      await result.current.handleMint()
    })

    mockUsdcApproveSuccess = true
    mockUsdcAllowance = 100n * 10n ** 6n
    rerender()

    await waitFor(() => {
      expect(mockMint).toHaveBeenCalledTimes(1)
    })

    rerender()
    rerender()
    rerender()

    expect(mockMint).toHaveBeenCalledTimes(1)
  })

  it('CRITICAL: approves USDC amount but mints pair amount', async () => {
    const { result, rerender } = renderHook(() => useMintFlowLogic('100'))

    await act(async () => {
      await result.current.handleMint()
    })

    // Approval uses USDC amount (6 decimals)
    expect(mockApproveUsdc).toHaveBeenCalledWith(100n * 10n ** 6n)

    mockUsdcApproveSuccess = true
    mockUsdcAllowance = 100n * 10n ** 6n
    rerender()

    await waitFor(() => {
      // Mint uses pair amount (18 decimals, divided by 2)
      expect(mockMint).toHaveBeenCalledWith(50n * 10n ** 18n)
    })

    // These are very different values - the original bug confused them
    const approveArg = mockApproveUsdc.mock.calls[0][0]
    const mintArg = mockMint.mock.calls[0][0]
    expect(approveArg).not.toBe(mintArg)
  })
})

// ============================================================================
// Redeem Flow Tests (existing tests, kept for completeness)
// ============================================================================

const mockBurn = vi.fn()
const mockApproveBear = vi.fn()
const mockApproveBull = vi.fn()
const mockRefetchBearAllowance = vi.fn()
const mockRefetchBullAllowance = vi.fn()

let mockBearAllowance = 0n
let mockBullAllowance = 0n
let mockBearApproveSuccess = false
let mockBullApproveSuccess = false

function useRedeemFlowLogic() {
  const [pendingAction, setPendingAction] = useState<'mint' | 'burn' | null>(null)
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

describe('Redeem flow - demonstrates why refs are needed', () => {
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

describe('Redeem flow - BEAR/BULL approval chain', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockBearAllowance = 0n
    mockBullAllowance = 0n
    mockBearApproveSuccess = false
    mockBullApproveSuccess = false
  })

  it('should call approveBear when BEAR allowance is insufficient', async () => {
    const { result } = renderHook(() => useRedeemFlowLogic())

    await act(async () => {
      await result.current.handleRedeem()
    })

    expect(mockApproveBear).toHaveBeenCalledTimes(1)
    expect(mockApproveBear).toHaveBeenCalledWith(100n * 10n ** 18n)
    expect(result.current.pendingAction).toBe('burn')
  })

  it('should call approveBull after BEAR approval succeeds', async () => {
    const { result, rerender } = renderHook(() => useRedeemFlowLogic())

    await act(async () => {
      await result.current.handleRedeem()
    })

    expect(mockApproveBear).toHaveBeenCalledTimes(1)

    mockBearApproveSuccess = true
    mockBearAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockApproveBull).toHaveBeenCalledTimes(1)
    })
  })

  it('should call burn after BULL approval succeeds', async () => {
    const { result, rerender } = renderHook(() => useRedeemFlowLogic())

    await act(async () => {
      await result.current.handleRedeem()
    })

    mockBearApproveSuccess = true
    mockBearAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockApproveBull).toHaveBeenCalledTimes(1)
    })

    mockBullApproveSuccess = true
    mockBullAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockBurn).toHaveBeenCalledTimes(1)
      expect(mockBurn).toHaveBeenCalledWith(100n * 10n ** 18n)
    })
  })

  it('FIXED: should NOT call approveBull multiple times', async () => {
    const { result, rerender } = renderHook(() => useRedeemFlowLogic())

    await act(async () => {
      await result.current.handleRedeem()
    })

    mockBearApproveSuccess = true
    mockBearAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockApproveBull).toHaveBeenCalledTimes(1)
    })

    rerender()
    rerender()
    rerender()

    expect(mockApproveBull).toHaveBeenCalledTimes(1)
  })

  it('FIXED: should NOT call burn multiple times', async () => {
    const { result, rerender } = renderHook(() => useRedeemFlowLogic())

    await act(async () => {
      await result.current.handleRedeem()
    })

    mockBearApproveSuccess = true
    mockBearAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockApproveBull).toHaveBeenCalledTimes(1)
    })

    mockBullApproveSuccess = true
    mockBullAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockBurn).toHaveBeenCalledTimes(1)
    })

    rerender()
    rerender()
    rerender()

    expect(mockBurn).toHaveBeenCalledTimes(1)
  })

  it('should skip BEAR approval if already approved', async () => {
    mockBearAllowance = 100n * 10n ** 18n

    const { result } = renderHook(() => useRedeemFlowLogic())

    await act(async () => {
      await result.current.handleRedeem()
    })

    expect(mockApproveBear).not.toHaveBeenCalled()
    expect(mockApproveBull).toHaveBeenCalledTimes(1)
  })

  it('should skip both approvals if already approved', async () => {
    mockBearAllowance = 100n * 10n ** 18n
    mockBullAllowance = 100n * 10n ** 18n

    const { result } = renderHook(() => useRedeemFlowLogic())

    await act(async () => {
      await result.current.handleRedeem()
    })

    expect(mockApproveBear).not.toHaveBeenCalled()
    expect(mockApproveBull).not.toHaveBeenCalled()
    expect(mockBurn).toHaveBeenCalledTimes(1)
  })

  it('should go directly to burn after BEAR approval if BULL already approved', async () => {
    mockBullAllowance = 100n * 10n ** 18n

    const { result, rerender } = renderHook(() => useRedeemFlowLogic())

    await act(async () => {
      await result.current.handleRedeem()
    })

    expect(mockApproveBear).toHaveBeenCalledTimes(1)

    mockBearApproveSuccess = true
    mockBearAllowance = 100n * 10n ** 18n
    rerender()

    await waitFor(() => {
      expect(mockApproveBull).not.toHaveBeenCalled()
      expect(mockBurn).toHaveBeenCalledTimes(1)
    })
  })
})
