import { render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { describe, expect, it } from 'vitest'
import { PageBreadcrumbBar, WalletPerspectiveSwitcher } from './PageBreadcrumbBar'

describe('PageBreadcrumbBar', () => {
  it('renders hierarchical breadcrumbs with clickable ancestors and current page state', () => {
    render(
      <MemoryRouter>
        <PageBreadcrumbBar
          items={[
            { label: 'Markets', to: '/execution' },
            { label: '@inshogami', to: '/wallets/0x1111111111111111111111111111111111111111' },
            { label: 'Position #412' },
          ]}
        />
      </MemoryRouter>,
    )

    const nav = screen.getByRole('navigation', { name: 'Breadcrumb' })
    expect(within(nav).getByRole('link', { name: 'Markets' })).toHaveAttribute('href', '/execution')
    expect(within(nav).getByRole('link', { name: '@inshogami' })).toHaveAttribute(
      'href',
      '/wallets/0x1111111111111111111111111111111111111111',
    )
    const current = within(nav).getByText('Position #412')
    expect(current).toHaveAttribute('aria-current', 'page')
  })

  it('renders WalletPerspectiveSwitcher with active perspective and sibling links', () => {
    const addr = '0x1111111111111111111111111111111111111111'
    render(
      <MemoryRouter>
        <WalletPerspectiveSwitcher address={addr} active="wallet" />
      </MemoryRouter>,
    )

    const switcher = screen.getByRole('navigation', { name: 'Account views' })
    expect(within(switcher).getByText('Wallet Ledger')).toHaveAttribute('aria-current', 'page')
    expect(within(switcher).getByRole('link', { name: 'Kernel v4' })).toHaveAttribute(
      'href',
      `/trading-accounts/${addr}`,
    )
    expect(within(switcher).getByRole('link', { name: 'Divisions' })).toHaveAttribute(
      'href',
      `/divisions/wallets/${addr}`,
    )
  })
})
