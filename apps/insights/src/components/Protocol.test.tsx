import { render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { describe, expect, it } from 'vitest'
import type { ProtocolAction } from '../api'
import { ActionTable, EvidenceBadge, ObjectTable } from './Protocol'

describe('protocol evidence rendering', () => {
  it('does not style mixed exact-and-derived evidence as exact', () => {
    render(<EvidenceBadge level="mixed_exact_and_derived" />)

    const badge = screen.getByText('mixed_exact_and_derived')
    expect(badge).toHaveClass('text-brand-peach')
    expect(badge).not.toHaveClass('text-positive')
  })

  it('does not style best-effort confirmed evidence as exact', () => {
    render(<EvidenceBadge level="best_effort_confirmed_known_and_common_log_actions" />)

    const badge = screen.getByText('best_effort_confirmed_known_and_common_log_actions')
    expect(badge).not.toHaveClass('text-positive')
  })

  it('uses the declared level and exposes every field for structured evidence', () => {
    render(
      <EvidenceBadge
        level={{
          level: 'exact',
          source: 'confirmed_log_projection',
          sourceBlock: { number: '123', hash: '0xabc' },
        }}
      />,
    )

    const details = screen.getByLabelText('Evidence details: exact')
    expect(within(details).getByText('exact')).toHaveClass('text-positive')
    expect(details).toHaveTextContent('confirmed_log_projection')
    expect(details).toHaveTextContent('"number": "123"')
    expect(details).not.toHaveTextContent('unavailable')
  })

  it('summarizes mixed structured evidence as partial when no top-level level is supplied', () => {
    render(
      <EvidenceBadge
        level={{
          activationTime: 'exact_historical_contract_read',
          proposer: 'unavailable',
        }}
      />,
    )

    const details = screen.getByLabelText('Evidence details: partial structured evidence')
    expect(within(details).getByText('partial structured evidence')).toHaveClass('text-brand-yellow')
    expect(details).toHaveTextContent('exact_historical_contract_read')
    expect(details).toHaveTextContent('proposer')
  })

  it('keeps units and provenance visible in data tables', () => {
    render(
      <MemoryRouter>
        <ObjectTable
          value={{
            valueUsdc: '1000000',
            units: { valueUsdc: 'USDC:6' },
            provenance: 'block-level delta',
          }}
        />
      </MemoryRouter>,
    )

    expect(screen.getByText('Units')).toBeInTheDocument()
    expect(screen.getByText('{"valueUsdc":"USDC:6"}')).toHaveClass('break-all')
    expect(screen.getByText('Provenance')).toBeInTheDocument()
    expect(screen.getByText('block-level delta')).toBeInTheDocument()
  })

  it('renders structured ObjectTable evidence as expandable provenance instead of unavailable', () => {
    render(
      <ObjectTable
        value={{ value: '42' }}
        evidence={{
          level: 'derived',
          formulaIdentifier: 'protocol.example.v1',
          sourceBlock: '123',
        }}
      />,
    )

    expect(screen.getByText('Evidence')).toBeInTheDocument()
    const details = screen.getByLabelText('Evidence details: derived')
    expect(details).toHaveTextContent('protocol.example.v1')
    expect(details).toHaveTextContent('"sourceBlock": "123"')
    expect(details).not.toHaveTextContent('unavailable')
  })

  it('never treats missing action evidence as exact', () => {
    const action: ProtocolAction = {
      actionId: 'action-1',
      transactionHash: `0x${'1'.repeat(64)}`,
      blockNumber: '1',
      blockHash: `0x${'2'.repeat(64)}`,
      transactionIndex: '0',
      logIndex: '0',
      timestamp: 1,
      actionType: 'unclassified_event',
      outcome: 'unavailable',
      account: null,
      keeper: null,
      orderId: null,
      contractAddress: `0x${'3'.repeat(40)}`,
      data: {},
      evidence: {},
      units: {},
    }

    render(
      <MemoryRouter>
        <ActionTable actions={[action]} />
      </MemoryRouter>,
    )

    expect(screen.getByText('unavailable')).not.toHaveClass('text-positive')
  })

  it('preserves the release when linking an action to its keeper', () => {
    const action: ProtocolAction = {
      actionId: 'action-keeper-link',
      transactionHash: `0x${'1'.repeat(64)}`,
      blockNumber: '1',
      blockHash: `0x${'2'.repeat(64)}`,
      transactionIndex: '0',
      logIndex: '0',
      timestamp: 1,
      actionType: 'order_execution',
      outcome: 'success',
      account: null,
      keeper: '0x3333333333333333333333333333333333333333',
      orderId: null,
      contractAddress: `0x${'4'.repeat(40)}`,
      data: {},
      evidence: { level: 'exact' },
      units: {},
    }

    render(
      <MemoryRouter>
        <ActionTable actions={[action]} releaseId="archived/release" />
      </MemoryRouter>,
    )

    expect(screen.getByRole('link', { name: '0x3333…3333' })).toHaveAttribute(
      'href',
      '/keepers/0x3333333333333333333333333333333333333333?release=archived%2Frelease',
    )
  })
})
