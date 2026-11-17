import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { PerpsPoolLiquidityDetails } from './PerpsPoolLiquidityDetails'

const HEALTHY_PROPS = {
  poolAssets: 10_000_000n * 1_000_000n,
  freeAssets: 6_300_000n * 1_000_000n,
  juniorPrincipal: '3.2M USDG',
  seniorPrincipal: '6.8M USDG',
  juniorSharePercent: 32,
  seniorSharePercent: 68,
  seniorStatus: 'at-high-water-mark' as const,
}

describe('PerpsPoolLiquidityDetails', () => {
  it('distinguishes pool assets and free liquidity from tranche accounting capital', () => {
    render(<PerpsPoolLiquidityDetails {...HEALTHY_PROPS} docsLink={{
      href: 'https://docs.plether.com/providing-liquidity', title: 'Pool liquidity',
    }} />)

    expect(screen.getByText('Total pool USDG')).toBeVisible()
    expect(screen.getByText('10 000 000')).toBeVisible()
    expect(screen.getByText('Reserved pool USDG')).toBeVisible()
    expect(screen.getByText('3 700 000')).toBeVisible()
    expect(screen.getByText('37.0%')).toBeVisible()
    expect(screen.getByText('Free liquidity')).toBeVisible()
    expect(screen.getByText('6 300 000')).toBeVisible()
    expect(screen.getByText('63.0%')).toBeVisible()
    expect(screen.getByRole('heading', { name: 'Pool USDG' })).toBeVisible()
    expect(screen.getByRole('heading', { name: 'Vault capital' })).toBeVisible()
    expect(screen.getByText(/Free liquidity alone does not set trade size/)).toBeVisible()
    expect(screen.getByText(/Vault balances are accounting values/)).toBeVisible()
    expect(screen.getByRole('img', {
      name: 'Pool USDG: 37.0% reserved; 63.0% free',
    })).toBeVisible()
    const composition = screen.getByRole('img', { name: 'Vault capital: Senior 68%; Junior 32%' })
    expect(composition).toBeVisible()
    expect(composition.children[0]).toHaveStyle({ width: '68%' })
    expect(composition.children[1]).toHaveStyle({ width: '32%' })
    expect(screen.getByText('68%')).toBeVisible()
    expect(screen.getByText('32%')).toBeVisible()
    expect(screen.getAllByText('3.2M USDG')).toHaveLength(1)
    expect(screen.getAllByText('6.8M USDG')).toHaveLength(1)
    expect(screen.getByText('Senior is at its protected balance')).toBeVisible()
    expect(screen.getByText(/Withdrawals depend on liquidity at hourly processing/)).toBeVisible()
    expect(screen.getByRole('link', { name: 'Read: Pool liquidity' })).toHaveAttribute(
      'href', 'https://docs.plether.com/providing-liquidity'
    )
  })

  it('explains a nearly reserved pool without advertising executable capacity', () => {
    render(<PerpsPoolLiquidityDetails {...HEALTHY_PROPS}
      poolAssets={590_100_000n * 1_000_000n}
      freeAssets={1_100n * 1_000_000n}
      juniorPrincipal="585.1M USDG" seniorPrincipal="5M USDG"
      juniorSharePercent={99.2} seniorSharePercent={0.8}
    />)

    expect(screen.getByText('590 100 000')).toBeVisible()
    expect(screen.getByText('590 098 900')).toBeVisible()
    expect(screen.queryByText('1.1K')).not.toBeInTheDocument()
    expect(screen.getByText('>99.9%')).toBeVisible()
    expect(screen.getByText('<0.1%')).toBeVisible()
    expect(screen.getByText('1 100')).toBeVisible()
    expect(screen.getByText(/Free liquidity alone does not set trade size/)).toBeVisible()
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
  ])('handles reserved share for $assets assets and $free free USDG', ({ assets, free, share }) => {
    render(<PerpsPoolLiquidityDetails {...HEALTHY_PROPS} poolAssets={assets} freeAssets={free} />)
    if (share) expect(screen.getByText(share)).toBeVisible()
    else expect(screen.queryByRole('img', { name: /^Pool USDG:/ })).not.toBeInTheDocument()
    const amounts = screen.getAllByRole('definition').slice(0, 4)
    if (assets === undefined || free === undefined || free > assets) {
      expect(amounts.some((amount) => amount.textContent === '--')).toBe(true)
    }
  })

  it('shows loading placeholders instead of previous liquidity or a percentage', () => {
    render(<PerpsPoolLiquidityDetails {...HEALTHY_PROPS} isLoading />)
    expect(screen.getAllByText('...')).toHaveLength(5)
    expect(screen.queryByText('3 700 000')).not.toBeInTheDocument()
    expect(screen.queryByText('3.2M USDG')).not.toBeInTheDocument()
    expect(screen.queryByText('68%')).not.toBeInTheDocument()
    expect(screen.queryByRole('img', { name: /^Vault capital:/ })).not.toBeInTheDocument()
    expect(screen.queryByRole('img', { name: /^Pool USDG:/ })).not.toBeInTheDocument()
  })

  it('keeps a tiny positive reserve distinct from zero', () => {
    render(<PerpsPoolLiquidityDetails {...HEALTHY_PROPS} poolAssets={1_000_000n} freeAssets={999_999n} />)
    expect(screen.getByText('<0.01')).toBeVisible()
    expect(screen.getByText('<0.1%')).toBeVisible()
  })

  it('shows an empty pool without a percentage', () => {
    render(<PerpsPoolLiquidityDetails poolAssets={0n} freeAssets={0n} isEmpty />)
    expect(screen.getAllByText('0')).toHaveLength(3)
    expect(screen.getByText('No vault capital yet')).toBeVisible()
    expect(screen.queryByRole('img', { name: /^Pool USDG:/ })).not.toBeInTheDocument()
  })

  it('shows factual exhausted and impaired states', () => {
    const { rerender } = render(
      <PerpsPoolLiquidityDetails
        {...HEALTHY_PROPS}
        juniorPrincipal="0 USDG"
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
        juniorPrincipal="0 USDG"
        seniorPrincipal="5.9M USDG"
        juniorSharePercent={0}
        seniorSharePercent={100}
        seniorStatus="impaired"
        seniorImpairment="900K USDG"
        isJuniorExhausted
      />
    )

    expect(screen.getByText(/Senior is below its protected balance by/)).toHaveTextContent(
      'Senior is below its protected balance by 900K USDG'
    )
    expect(screen.queryByText("Further pool losses would reduce Senior's value")).not.toBeInTheDocument()
  })
})
