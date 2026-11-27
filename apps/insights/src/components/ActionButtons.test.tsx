import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'
import { ARBITRUM_ONE_CHAIN_ID, arbiscanLinkKey } from '../utils/chain'
import { ArbiscanButton, CopyButton } from './ActionButtons'

describe('ActionButtons (ArbiscanButton & CopyButton)', () => {
  it('renders ArbiscanButton with canonical URL, accessible Arbiscan label, and hover-expanding label span', () => {
    const txHash = `0x${'a'.repeat(64)}`
    render(<ArbiscanButton kind="tx" value={txHash} chainId={421614} />)

    const link = screen.getByRole('link', { name: `View ${txHash} on Arbiscan` })
    expect(link).toHaveAttribute('href', `https://sepolia.arbiscan.io/tx/${txHash}`)
    expect(link).toHaveAttribute('target', '_blank')
    expect(link).toHaveAttribute('rel', 'noreferrer')
    expect(link.textContent).toContain('Arbiscan')
  })

  it('resolves mainnet Arbiscan address links and normalizes dedup keys case-insensitively', () => {
    const addr = `0x${'Ab'.repeat(20)}`
    render(<ArbiscanButton kind="address" value={addr} chainId={ARBITRUM_ONE_CHAIN_ID} />)

    const link = screen.getByRole('link', { name: `View ${addr} on Arbiscan` })
    expect(link).toHaveAttribute('href', `https://arbiscan.io/address/${addr}`)
    expect(arbiscanLinkKey('address', addr, ARBITRUM_ONE_CHAIN_ID)).toBe(
      arbiscanLinkKey('address', addr.toLowerCase(), ARBITRUM_ONE_CHAIN_ID),
    )
  })

  it('renders CopyButton matching ArbiscanButton geometry, copies text, and displays Copied feedback', () => {
    const writeText = vi.fn().mockResolvedValue(undefined)
    Object.defineProperty(navigator, 'clipboard', {
      value: { writeText },
      configurable: true,
    })

    const address = `0x${'1'.repeat(40)}`
    render(<CopyButton value={address} />)

    const button = screen.getByRole('button', { name: `Copy ${address}` })
    fireEvent.click(button)
    expect(writeText).toHaveBeenCalledWith(address)
    expect(screen.getByRole('button', { name: 'Copied' })).toBeInTheDocument()
  })
})
