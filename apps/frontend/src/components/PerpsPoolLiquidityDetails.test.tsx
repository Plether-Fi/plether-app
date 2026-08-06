import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { PerpsPoolLiquidityDetails } from './PerpsPoolLiquidityDetails'

const HEALTHY_PROPS = {
  longCapacity: '2.8M USDC',
  shortCapacity: '1.1M USDC',
  juniorPrincipal: '3.2M USDC',
  seniorPrincipal: '6.8M USDC',
  juniorSharePercent: 32,
  seniorSharePercent: 68,
  seniorStatus: 'at-high-water-mark' as const,
}

describe('PerpsPoolLiquidityDetails', () => {
  it('shows opening capacity and the Junior-to-Senior capital waterfall', () => {
    render(<PerpsPoolLiquidityDetails {...HEALTHY_PROPS} />)

    expect(screen.getByText('Estimated LONG capacity')).toBeVisible()
    expect(screen.getByText('2.8M USDC')).toBeVisible()
    expect(screen.getByText('Estimated SHORT capacity')).toBeVisible()
    expect(screen.getByText('1.1M USDC')).toBeVisible()
    expect(screen.getByRole('img', {
      name: 'LP principal composition: Junior first loss 32%; Senior last loss 68%',
    })).toBeVisible()
    expect(screen.getByText('Junior · 32%').parentElement).toHaveStyle({ width: '32%' })
    expect(screen.getByText('Senior · 68%').parentElement).toHaveStyle({ width: '68%' })
    expect(screen.queryByText('(32%)')).not.toBeInTheDocument()
    expect(screen.queryByText('(68%)')).not.toBeInTheDocument()
    expect(screen.getByText('Senior principal at high-water mark')).toBeVisible()
    expect(screen.getByText(/Junior absorbs the first/)).toHaveTextContent(
      'Junior absorbs the first 3.2M USDC of realized pool losses'
    )
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

    expect(screen.getByText('Junior first-loss buffer exhausted')).toBeVisible()
    expect(screen.getByText('The next realized pool loss would reduce Senior principal')).toBeVisible()

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

    expect(screen.getByText(/Senior impaired by/)).toHaveTextContent('Senior impaired by 900K USDC')
    expect(screen.queryByText('The next realized pool loss would reduce Senior principal')).not.toBeInTheDocument()
  })
})
