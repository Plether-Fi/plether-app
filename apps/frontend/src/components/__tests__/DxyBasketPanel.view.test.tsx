import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { DxyBasketPanelView } from '../DxyBasketPanel'
import type { BasketLatest } from '../../api'

const latest: BasketLatest = {
  timestamp: 100,
  basketPrice: '97600000',
  components: [],
  generatedAt: 101,
  source: 'database',
}

describe('DxyBasketPanelView', () => {
  it('does not duplicate the price summary above the chart', () => {
    render(
      <DxyBasketPanelView
        latest={latest}
        chartInterval="5m"
        useAdvancedChart={false}
        isLoading
        onChartIntervalChange={() => {}}
      />
    )

    expect(screen.queryByText('plDXY Perp Price')).not.toBeInTheDocument()
    expect(screen.queryByText('1.0240')).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: '5 minute interval' })).toBeInTheDocument()
  })
})
