import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'
import { usePerpsVaultHistory } from '../api'
import { JuniorVaultReturnBreakdownSection } from './JuniorVaultReturnBreakdown'
import { createVaultAssetHistory } from '../stories/fixtures/vaultAssetHistory'

vi.mock('../api', () => ({ usePerpsVaultHistory: vi.fn() }))

describe('junior waterfall history integration', () => {
  it('fetches the selected range and withholds a response from a different range', () => {
    const history = createVaultAssetHistory()
    vi.mocked(usePerpsVaultHistory).mockReturnValue({ data: { data: history }, isLoading: false, isError: false } as ReturnType<typeof usePerpsVaultHistory>)
    render(<JuniorVaultReturnBreakdownSection />)
    expect(screen.getByRole('status')).toHaveTextContent('Component attribution is not available')
    fireEvent.click(screen.getByRole('button', { name: '30 days' }))
    expect(usePerpsVaultHistory).toHaveBeenLastCalledWith('30d')
    expect(screen.getByRole('status')).toHaveTextContent('Complete, fresh diluted share-price history is not yet available')
  })

  it('withholds history from an unrelated vault deployment', () => {
    const history = createVaultAssetHistory()
    history.deployment.juniorVault = '0x0000000000000000000000000000000000000001'
    vi.mocked(usePerpsVaultHistory).mockReturnValue({ data: { data: history }, isLoading: false, isError: false } as ReturnType<typeof usePerpsVaultHistory>)
    render(<JuniorVaultReturnBreakdownSection />)
    expect(screen.getByRole('status')).toHaveTextContent('Complete, fresh diluted share-price history is not yet available')
  })

  it('labels cached results after refresh failure', () => {
    vi.mocked(usePerpsVaultHistory).mockReturnValue({ data: { data: createVaultAssetHistory() }, isLoading: false, isError: true } as ReturnType<typeof usePerpsVaultHistory>)
    render(<JuniorVaultReturnBreakdownSection />)
    expect(screen.getByText('Refresh failed. Showing the last received reporting period.')).toBeInTheDocument()
  })
})
