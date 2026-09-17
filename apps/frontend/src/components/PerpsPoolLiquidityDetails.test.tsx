import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { PerpsPoolLiquidityDetails } from './PerpsPoolLiquidityDetails'

const HEALTHY_PROPS = {
  poolAssetsUsdc: 10_000_000n * 1_000_000n,
  freeUsdc: 6_300_000n * 1_000_000n,
  juniorPrincipal: '3.2M USDC',
  seniorPrincipal: '6.8M USDC',
  juniorSharePercent: 32,
  seniorSharePercent: 68,
  seniorStatus: 'at-high-water-mark' as const,
}

describe('PerpsPoolLiquidityDetails', () => {
  it('distinguishes pool assets and free liquidity from tranche accounting capital', () => {
    render(<PerpsPoolLiquidityDetails {...HEALTHY_PROPS} />)

    expect(screen.getByText('Total pool assets')).toBeVisible()
    expect(screen.getByText('10 000 000')).toBeVisible()
    expect(screen.getByText('Reserved pool assets')).toBeVisible()
    expect(screen.getByText('3 700 000')).toBeVisible()
    expect(screen.getByText('37.0%')).toBeVisible()
    expect(screen.getByRole('heading', { name: 'What this means for trading' })).toBeVisible()
    expect(screen.getByText(/New orders must satisfy both/)).toBeVisible()
    expect(screen.getByText(/Use Max in the trade ticket/)).toBeVisible()
    expect(screen.getByText(/Tranche balances are accounting values/)).toBeVisible()
    expect(screen.queryByText(/trading capacity/i)).not.toBeInTheDocument()
    expect(screen.getByRole('img', {
      name: 'Vault capital: Senior 68%; Junior 32%',
    })).toBeVisible()
    expect(screen.getByText('Junior · 32%').parentElement).toHaveStyle({ width: '32%' })
    expect(screen.getByText('Senior · 68%').parentElement).toHaveStyle({ width: '68%' })
    expect(screen.queryByText('(32%)')).not.toBeInTheDocument()
    expect(screen.queryByText('(68%)')).not.toBeInTheDocument()
    expect(screen.getByText('Senior is at its protected balance')).toBeVisible()
    expect(screen.getByText(/Junior protects Senior from the first/)).toHaveTextContent(
      'Junior protects Senior from the first 3.2M USDC of pool losses'
    )
    expect(screen.getByText(/Withdrawals depend on the liquidity available/)).toHaveTextContent(
      'Withdrawals depend on the liquidity available at each hourly processing time.'
    )
  })

  it('explains a nearly reserved pool without advertising executable capacity', () => {
    render(<PerpsPoolLiquidityDetails {...HEALTHY_PROPS}
      poolAssetsUsdc={590_100_000n * 1_000_000n}
      freeUsdc={1_100n * 1_000_000n}
      juniorPrincipal="585.1M USDC" seniorPrincipal="5M USDC"
      juniorSharePercent={99.2} seniorSharePercent={0.8}
    />)

    expect(screen.getByText('590 100 000')).toBeVisible()
    expect(screen.getByText('590 098 900')).toBeVisible()
    expect(screen.queryByText('1.1K')).not.toBeInTheDocument()
    expect(screen.getByText('>99.9%')).toBeVisible()
    expect(screen.getByText(/some orders may still fit/)).toBeVisible()
    expect(screen.queryByText(/trading capacity/i)).not.toBeInTheDocument()
  })

  it.each([
    { assets: 1_000_000n, free: 0n, share: '100.0%' },
    { assets: 1_000_000n, free: 1_000n, share: '99.9%' },
    { assets: 1_000_000n, free: 1_000_000n, share: '0.0%' },
    { assets: 0n, free: 0n, share: undefined },
    { assets: 1_000_000n, free: 2_000_000n, share: undefined },
    { assets: undefined, free: 0n, share: undefined },
    { assets: 1_000_000n, free: undefined, share: undefined },
  ])('handles reserved share for $assets assets and $free free USDC', ({ assets, free, share }) => {
    render(<PerpsPoolLiquidityDetails {...HEALTHY_PROPS} poolAssetsUsdc={assets} freeUsdc={free} />)
    if (share) expect(screen.getByText(share)).toBeVisible()
    else expect(screen.queryByText(/Share of pool assets reserved/)).not.toBeInTheDocument()
    const amounts = screen.getAllByRole('definition').slice(0, 4)
    if (assets === undefined || free === undefined || free > assets) {
      expect(amounts.some((amount) => amount.textContent === '--')).toBe(true)
    }
  })

  it('shows loading placeholders instead of previous liquidity or a percentage', () => {
    render(<PerpsPoolLiquidityDetails {...HEALTHY_PROPS} isLoading />)
    expect(screen.getAllByText('...')).toHaveLength(2)
    expect(screen.queryByText('3 700 000')).not.toBeInTheDocument()
    expect(screen.queryByText(/Share of pool assets reserved/)).not.toBeInTheDocument()
  })

  it('keeps a tiny positive reserve distinct from zero', () => {
    render(<PerpsPoolLiquidityDetails {...HEALTHY_PROPS} poolAssetsUsdc={1_000_000n} freeUsdc={999_999n} />)
    expect(screen.getByText('<0.01')).toBeVisible()
    expect(screen.getByText('<0.1%')).toBeVisible()
  })

  it('shows an empty pool without a percentage', () => {
    render(<PerpsPoolLiquidityDetails poolAssetsUsdc={0n} freeUsdc={0n} isEmpty />)
    expect(screen.getAllByText('0')).toHaveLength(2)
    expect(screen.getByText('No vault capital yet')).toBeVisible()
    expect(screen.queryByText(/Share of pool assets reserved/)).not.toBeInTheDocument()
  })

  it('shows factual exhausted and impaired states', () => {
    const { rerender } = render(
      <PerpsPoolLiquidityDetails
        {...HEALTHY_PROPS}
        juniorPrincipal="0 USDC"
        juniorSharePercent={0}
        seniorSharePercent={100}
        isJuniorExhausted
      />
    )

    expect(screen.getByText('Junior protection is depleted')).toBeVisible()
    expect(screen.getByText("Further pool losses would reduce Senior's value")).toBeVisible()

    rerender(
      <PerpsPoolLiquidityDetails
        {...HEALTHY_PROPS}
        juniorPrincipal="0 USDC"
        seniorPrincipal="5.9M USDC"
        juniorSharePercent={0}
        seniorSharePercent={100}
        seniorStatus="impaired"
        seniorImpairment="900K USDC"
        isJuniorExhausted
      />
    )

    expect(screen.getByText(/Senior is below its protected balance by/)).toHaveTextContent(
      'Senior is below its protected balance by 900K USDC'
    )
    expect(screen.queryByText("Further pool losses would reduce Senior's value")).not.toBeInTheDocument()
  })
})
