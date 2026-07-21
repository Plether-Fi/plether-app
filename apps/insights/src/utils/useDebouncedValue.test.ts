import { act, renderHook } from '@testing-library/react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { useDebouncedValue } from './useDebouncedValue'

afterEach(() => {
  vi.useRealTimers()
})

describe('useDebouncedValue', () => {
  it('waits for a quiet period and resets the timer when the value changes', () => {
    vi.useFakeTimers()
    const { result, rerender } = renderHook(
      ({ value }) => useDebouncedValue(value, 350),
      { initialProps: { value: '' } },
    )

    rerender({ value: 'a' })
    act(() => {
      vi.advanceTimersByTime(200)
    })
    rerender({ value: 'al' })
    act(() => {
      vi.advanceTimersByTime(349)
    })
    expect(result.current).toBe('')

    act(() => {
      vi.advanceTimersByTime(1)
    })
    expect(result.current).toBe('al')
  })
})
