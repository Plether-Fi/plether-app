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

    expect(screen.getByText('Estimated LONG trading capacity')).toBeVisible()
    expect(screen.getByText('2.8M USDC')).toBeVisible()
    expect(screen.getByText('Estimated SHORT trading capacity')).toBeVisible()
    expect(screen.getByText('1.1M USDC')).toBeVisible()
    expect(screen.getByRole('img', {
      name: 'Vault capital: Junior 32%; Senior 68%',
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
      'Available trading capacity is an estimate and can change before a trade is submitted. Withdrawals depend on the liquidity available at each hourly processing time.'
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
