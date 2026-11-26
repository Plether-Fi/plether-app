import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { ShareCardModal, type ShareCardOption } from './ShareCardModal'

const account = `0x${'a'.repeat(40)}`

const CARDS: ShareCardOption[] = [
  {
    id: 'division',
    label: 'Divisions Badge',
    data: {
      kind: 'division',
      traderLabel: '@alice_fx',
      account,
      networkName: 'Arbitrum Sepolia',
      tier: 'Gold',
      rank: 2,
      weeklyReturnLabel: '+14.20%',
      activeDays: 4,
      weekLabel: 'Live Week',
      provisional: true,
    },
  },
  {
    id: 'position',
    label: 'Open Position',
    data: {
      kind: 'position',
      traderLabel: '@alice_fx',
      account,
      networkName: 'Arbitrum Sepolia',
      releaseVersion: 'v2.0.0',
      market: 'plDXY-PERP',
      side: 'long',
      size: '500000000000000000000',
      entryPrice: '104500000000000000000',
      marginAssets: '200000000',
      unrealizedPnlAssets: '50000000',
      liquidatable: false,
    },
  },
]

describe('ShareCardModal', () => {
  it('opens the share studio, switches between available cards, and exposes Download PNG / Copy / X links', () => {
    render(<ShareCardModal cards={CARDS} triggerLabel="Share card" />)

    fireEvent.click(screen.getByRole('button', { name: 'Share card' }))
    expect(screen.getByText('Shareable social card')).toBeInTheDocument()
    expect(screen.getByAltText('Preview of Divisions Badge share card')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Download PNG' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Copy image' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Post on X ↗' })).toHaveAttribute(
      'href',
      expect.stringContaining('https://x.com/intent/tweet?'),
    )

    fireEvent.click(screen.getByRole('tab', { name: 'Open Position' }))
    expect(screen.getByAltText('Preview of Open Position share card')).toBeInTheDocument()
  })
})
