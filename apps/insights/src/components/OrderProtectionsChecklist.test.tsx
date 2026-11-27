import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import type { OrderProtectionsPayload } from '../api'
import {
  ActivePositionProtectionStrip,
  OrderProtectionsChecklistPanel,
  OrderProtectionsInlineChecklist,
} from './OrderProtectionsChecklist'
import { normalizeAccountProtection } from '../utils/orderProtections'

const WEB_OPEN_PROTECTIONS: OrderProtectionsPayload = {
  orderId: '62301',
  boundsSource: 'commit_receipt',
  fullBoundsAvailable: true,
  summary: {
    v3EnforcedCount: 5,
    v3PassedCount: 5,
    v3FailedCount: 0,
    v3UnboundedCount: 8,
    hasTakeProfit: true,
    hasStopLoss: true,
    positionProtectionStatus: 'Armed',
  },
  executionChecks: [
    {
      key: 'price_limit',
      category: 'core',
      label: 'Price / slippage limit',
      status: 'passed',
      limitValue: '95000000',
      actualValue: '94756243',
      unit: 'price',
      detail: 'Fill price cleared committed slippage limit',
    },
    {
      key: 'execution_window',
      category: 'core',
      label: 'Execution window',
      status: 'passed',
      limitValue: '60',
      actualValue: '2',
      unit: 'seconds',
      detail: 'Filled in 2s within 60s window',
    },
    {
      key: 'execution_mode',
      category: 'core',
      label: 'Execution regime',
      status: 'passed',
      limitValue: 'Live only',
      actualValue: 'Live',
      unit: 'mode',
      detail: 'Filled in Live regime (Live only)',
    },
    {
      key: 'config_hash',
      category: 'core',
      label: 'Protocol config pin',
      status: 'passed',
      limitValue: '0xff9a3bbf31030658aa5644732816f101ba033ba1b980f84bf30c0fa408072805',
      actualValue: '0xff9a3bbf31030658aa5644732816f101ba033ba1b980f84bf30c0fa408072805',
      unit: 'hash',
      detail: 'Matched pinned config 0xff9a3bbf…072805',
    },
    {
      key: 'max_execution_bounty',
      category: 'core',
      label: 'Max keeper bounty',
      status: 'passed',
      limitValue: '210000',
      actualValue: '200000',
      unit: 'usdg',
      detail: 'Within committed ceiling',
    },
    {
      key: 'max_post_leverage',
      category: 'accounting',
      label: 'Max post-trade leverage',
      status: 'not_set',
      limitValue: null,
      actualValue: '9998',
      unit: 'bps',
      detail: 'Unbounded',
    },
  ],
  positionProtection: {
    protectionId: '42',
    parentOrderId: '62301',
    linkedOrderId: null,
    status: 'Armed',
    triggeredLeg: null,
    takeProfitTriggerPrice: '92500000',
    stopLossTriggerPrice: '96500000',
    triggerMarkPrice: null,
    triggerBountyAssets: '200000',
    armedBlock: '333100202',
    updatedBlock: '333100202',
    txHash: '0xabc',
  },
}

describe('OrderProtectionsChecklist', () => {
  it('renders compact V3 and TP/SL checklist chips on Open rows', () => {
    render(<OrderProtectionsInlineChecklist protections={WEB_OPEN_PROTECTIONS} />)
    expect(screen.getByText('5/5 V3')).toBeInTheDocument()
    expect(screen.getByText(/TP 1\.0750/)).toBeInTheDocument()
    expect(screen.getByText(/SL 1\.0350/)).toBeInTheDocument()
  })

  it('renders fallback dash when order protections are unavailable', () => {
    render(<OrderProtectionsInlineChecklist protections={null} />)
    expect(screen.getByText('—')).toBeInTheDocument()
  })

  it('renders full V3 execution guardrails, toggleable accounting bounds, and TP/SL legs', () => {
    render(<OrderProtectionsChecklistPanel protections={WEB_OPEN_PROTECTIONS} side="short" />)

    expect(screen.getByText('Order & Position Protections Checklist')).toBeInTheDocument()
    expect(screen.getByText('5/5 V3 guards passed')).toBeInTheDocument()
    expect(screen.getByText('TP & SL · Armed')).toBeInTheDocument()
    expect(screen.getByText('Price / slippage limit')).toBeInTheDocument()
    expect(screen.getByText('Execution window')).toBeInTheDocument()
    expect(screen.getByText('Take Profit (TP)')).toBeInTheDocument()
    expect(screen.getByText('Stop Loss (SL)')).toBeInTheDocument()
    expect(screen.getByText('Protection #42')).toBeInTheDocument()

    // Unbounded accounting bounds start collapsed and expand on click
    expect(screen.queryByText('Max post-trade leverage')).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: /Strict accounting bounds/i }))
    expect(screen.getByText('Max post-trade leverage')).toBeInTheDocument()
    expect(screen.getAllByText('Unbounded').length).toBeGreaterThanOrEqual(1)
  })

  it('normalizes raw PositionProtectionBook snapshots including numeric status codes', () => {
    const parsed = normalizeAccountProtection({
      protectionId: '77',
      parentOrderId: '62301',
      linkedOrderId: '0',
      status: '2',
      triggeredLeg: '0',
      takeProfitTriggerPrice: '92500000',
      stopLossTriggerPrice: '0',
      updatedBlock: '333100250',
    })
    expect(parsed.protectionId).toBe('77')
    expect(parsed.status).toBe('Armed')
    expect(parsed.parentOrderId).toBe('62301')
    expect(parsed.linkedOrderId).toBeNull()
    expect(parsed.takeProfitTriggerPrice).toBe('92500000')
    expect(parsed.stopLossTriggerPrice).toBeNull()
  })

  it('renders active TP/SL checklist strip on open positions', () => {
    render(
      <ActivePositionProtectionStrip
        protections={[
          {
            protectionId: '99',
            statusName: 'Armed',
            takeProfitTriggerPrice: '92500000',
            stopLossTriggerPrice: '96500000',
          },
        ]}
      />,
    )
    expect(screen.getByText('#99 · Armed')).toBeInTheDocument()
    expect(screen.getByText('1.0750')).toBeInTheDocument()
    expect(screen.getByText('1.0350')).toBeInTheDocument()
  })
})
