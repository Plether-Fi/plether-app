import { render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { describe, expect, it } from 'vitest'
import { manualFeeShareApproval } from '../feeShareApprovals'
import { ManualFeeShareBadge, ManualFeeShareCategory } from './FeeShare'

const slug = 'testnet-trading-2026-09'
const wallet = '0x505ae6017be53b8dde88cff349c873eca8dd4cb5'

describe('manual fee-share approvals', () => {
  it('matches wallet case-insensitively but scopes approval to the exact competition', () => {
    expect(manualFeeShareApproval(slug, wallet.toUpperCase())?.handle).toBe('nightpuper')
    expect(manualFeeShareApproval('another-competition', wallet)).toBeUndefined()
    expect(manualFeeShareApproval(slug, '0x1111111111111111111111111111111111111111')).toBeUndefined()
  })

  it('lists both decisions with their scope and wallet links', () => {
    render(<MemoryRouter><ManualFeeShareCategory competitionSlug={slug} /></MemoryRouter>)
    expect(screen.getByRole('heading', { name: 'Eligible after manual verification' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: '@nightpuper ↗' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: '@FishesReal72265 ↗' })).toBeInTheDocument()
    expect(screen.getByText(/Normal fee-share eligibility conditions were waived/)).toHaveTextContent('competition rankings and cash prizes are unchanged')
    expect(screen.getByRole('link', { name: /0x505a/ })).toHaveAttribute('href', `/competitions/${slug}/wallets/${wallet}`)
  })

  it('does not show the category for other competitions', () => {
    const { container } = render(<ManualFeeShareCategory competitionSlug="another-competition" />)
    expect(container).toBeEmptyDOMElement()
  })

  it('shows the explicit organizer decision on approved badges only', () => {
    const { rerender } = render(<ManualFeeShareBadge competitionSlug={slug} address={wallet} />)
    expect(screen.getByText('Fee share: eligible after manual verification')).toHaveAttribute('title', expect.stringContaining('Approved personally by Stan'))
    rerender(<ManualFeeShareBadge competitionSlug={slug} address="0x1111111111111111111111111111111111111111" />)
    expect(screen.queryByText('Fee share: eligible after manual verification')).not.toBeInTheDocument()
  })
})
