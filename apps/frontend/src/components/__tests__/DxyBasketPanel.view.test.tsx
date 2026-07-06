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
  it('keeps the header price visible while interval history is loading', () => {
    render(
      <DxyBasketPanelView
        latest={latest}
        chartInterval="5m"
        isLoading
        onChartIntervalChange={() => {}}
      />
    )

    expect(screen.getByText('1.0240')).toBeInTheDocument()
  })
})
