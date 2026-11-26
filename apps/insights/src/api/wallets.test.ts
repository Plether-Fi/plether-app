import { describe, expect, it } from 'vitest'
import {
  accountActivityToWalletActivity,
  enrichBalanceHistory,
  formatPlDxyQuantity,
  formatVaultShares,
  sliceBalanceHistory,
} from './wallets'

describe('wallets API helpers', () => {
  it('formats 9-decimal vault shares without manufacturing zeros on invalid values', () => {
    expect(formatVaultShares('1250500000000')).toBe('1,250.50 shares')
    expect(formatVaultShares('0')).toBe('0.00 shares')
    expect(formatVaultShares(null)).toBe('Unavailable')
    expect(formatVaultShares('not-a-number')).toBe('Unavailable')
  })

  it('formats 18-decimal plDXY position quantities cleanly', () => {
    expect(formatPlDxyQuantity('500250000000000000000')).toBe('500.25 plDXY')
    expect(formatPlDxyQuantity(undefined)).toBe('Unavailable')
  })

  it('maps AccountActivityRecord to WalletActivity for PositionBreakdown', () => {
    const mapped = accountActivityToWalletActivity({
      id: '0x1:0',
      activityType: 'Close',
      side: 'short',
      price: '103000000000000000000',
      sizeDelta: '250000000000000000000',
      amountAssets: '25000000000',
      pnlAssets: '120000000',
      vpiAssets: '-50000',
      txHash: `0x${'a'.repeat(64)}`,
      blockNumber: '332880000',
      occurredAt: '2026-11-26T12:00:00Z',
      execution: null,
    })
    expect(mapped.market).toBe('plDXY-PERP')
    expect(mapped.side).toBe('short')
    expect(mapped.type).toBe('Close')
    expect(mapped.pnl).toBe('120000000')
    expect(mapped.vpi).toBe('-50000')
  })

  it('slices balanceHistory across 7d, 30d, and 90d while carrying forward pre-window baseline', () => {
    const now = 1_800_000_000
    const points = [
      {
        occurredAt: new Date((now - 50 * 86_400) * 1000).toISOString(),
        timestamp: now - 50 * 86_400,
        blockNumber: '100',
        balanceAssets: '500000000',
        eventType: 'Deposit',
        source: 'transfer',
      },
      {
        occurredAt: new Date((now - 10 * 86_400) * 1000).toISOString(),
        timestamp: now - 10 * 86_400,
        blockNumber: '200',
        balanceAssets: '750000000',
        eventType: 'Close',
        source: 'receipt',
      },
    ]
    const slice7d = sliceBalanceHistory(points, '7d', now)
    // Carries the 10d-old balance horizontally across the 7d window
    expect(slice7d).toHaveLength(2)
    expect(slice7d[0].balanceAssets).toBe('750000000')
    expect(slice7d[1].balanceAssets).toBe('750000000')
    expect(slice7d[1].timestamp).toBe(now)

    const slice30d = sliceBalanceHistory(points, '30d', now)
    expect(slice30d).toHaveLength(2)
    expect(slice30d[0].balanceAssets).toBe('500000000')
    expect(slice30d[1].balanceAssets).toBe('750000000')

    const sliceAll = sliceBalanceHistory(points, 'all', now)
    expect(sliceAll).toHaveLength(2)
    expect(sliceAll[0].timestamp).toBe(now - 50 * 86_400)
  })

  it('enriches balanceHistory with canonical execution receipt post-balances from activity', () => {
    const merged = enrichBalanceHistory(
      [
        {
          occurredAt: '2026-11-09T14:23:53Z',
          timestamp: Math.floor(Date.parse('2026-11-09T14:23:53Z') / 1000),
          blockNumber: '100',
          balanceAssets: '100000000000',
          eventType: 'Deposit',
          source: 'transfer',
        },
      ],
      [
        {
          id: '0x1:5',
          activityType: 'Close',
          txHash: `0x${'a'.repeat(64)}`,
          blockNumber: '200',
          occurredAt: '2026-11-16T18:16:35Z',
          execution: {
            orderId: '42',
            protocolVersion: 'v1.2.3',
            status: 'complete',
            receipt: {
              realizedPnlAssets: '5000000000',
              executionFeeAssets: '1000000',
              vpiAssets: '0',
              carryAssets: '0',
              frozenSpreadAssets: '0',
              executionBountyAssets: '0',
              preSettlementBalanceAssets: '100000000000',
              postSettlementBalanceAssets: '132498767272',
              preTraderClaimBalanceAssets: '0',
              postTraderClaimBalanceAssets: '1000000',
              executionNotionalAssets: '50000000000',
              postPositionSize: '0',
              postPositionMarginAssets: '0',
            },
            settlements: [],
          },
        },
      ],
    )
    expect(merged).toHaveLength(2)
    expect(merged[1].source).toBe('receipt')
    expect(merged[1].balanceAssets).toBe('132499767272')
  })
})
