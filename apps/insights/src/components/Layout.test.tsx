import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { describe, expect, it, vi } from 'vitest'
import { resolveActivePillar, resolveSectionSubNav } from '../navigation/routes'
import { Layout } from './Layout'

vi.mock('../api', () => ({
  useInsightsStatus: () => ({
    data: {
      healthy: true,
      latestIndexedBlock: 325000000,
      latestIndexedAt: '2026-11-26T10:00:00Z',
      chainId: 421614,
    },
  }),
}))

vi.mock('../divisions/query', () => ({
  useLiveDivisionsSummary: () => ({ data: undefined }),
}))

describe('Layout unified navigation', () => {
  it('renders 5 clean top-level pillars, global header search, and collapsible mobile drawer', () => {
    render(
      <MemoryRouter initialEntries={['/']}>
        <Layout>
          <div>body</div>
        </Layout>
      </MemoryRouter>,
    )

    const primaryNav = screen.getByRole('navigation', { name: 'Primary navigation' })
    expect(within(primaryNav).getByRole('link', { name: 'Overview' })).toHaveAttribute('aria-current', 'page')
    expect(within(primaryNav).getByRole('link', { name: 'Markets' })).toHaveAttribute('href', '/execution')
    expect(within(primaryNav).getByRole('link', { name: 'Liquidity' })).toHaveAttribute('href', '/liquidity')
    expect(within(primaryNav).getByRole('link', { name: 'System' })).toHaveAttribute('href', '/protocol')
    expect(within(primaryNav).getByRole('link', { name: 'Divisions' })).toHaveAttribute('href', '/divisions')

    // No Tier-2 section sub-nav on Overview
    expect(screen.queryByRole('navigation', { name: /sections$/i })).not.toBeInTheDocument()

    // Global header search is present
    expect(screen.getByRole('search', { name: /Search accounts/i })).toBeInTheDocument()

    // Collapsible mobile drawer opens and closes cleanly on Escape
    const menuBtn = screen.getByRole('button', { name: 'Open navigation menu' })
    expect(menuBtn).toHaveAttribute('aria-expanded', 'false')
    expect(screen.queryByRole('navigation', { name: 'Mobile navigation' })).not.toBeInTheDocument()

    fireEvent.click(menuBtn)
    expect(menuBtn).toHaveAttribute('aria-expanded', 'true')
    const drawer = screen.getByRole('navigation', { name: 'Mobile navigation' })
    expect(within(drawer).getByRole('link', { name: /Oracle & FX Calendar/i })).toHaveAttribute('href', '/oracle')
    expect(within(drawer).getByRole('link', { name: /Weekly Results/i })).toHaveAttribute('href', '/results')
    expect(within(drawer).getByRole('link', { name: /AWS Infra/i })).toHaveAttribute('href', '/infra')

    fireEvent.keyDown(document, { key: 'Escape' })
    expect(screen.queryByRole('navigation', { name: 'Mobile navigation' })).not.toBeInTheDocument()
  })

  it('shows Tier-2 section tabs on pillar pages and hides them on drill-down detail pages', () => {
    const { unmount } = render(
      <MemoryRouter initialEntries={['/oracle']}>
        <Layout>
          <div>oracle body</div>
        </Layout>
      </MemoryRouter>,
    )

    const primaryNav = screen.getByRole('navigation', { name: 'Primary navigation' })
    expect(within(primaryNav).getByRole('link', { name: 'Markets' })).toHaveAttribute('aria-current', 'page')

    const subNav = screen.getByRole('navigation', { name: 'Markets sections' })
    expect(within(subNav).getByRole('link', { name: 'Execution & FAD Risk' })).toHaveAttribute('href', '/execution')
    expect(within(subNav).getByRole('link', { name: 'Oracle & FX Calendar' })).toHaveAttribute('aria-current', 'page')

    unmount()

    // Detail page highlights Markets pillar in Tier-1 but omits Tier-2 section tabs (page renders PageBreadcrumbBar)
    render(
      <MemoryRouter initialEntries={['/wallets/0x1111111111111111111111111111111111111111/positions/412']}>
        <Layout>
          <div>position detail</div>
        </Layout>
      </MemoryRouter>,
    )

    const detailPrimary = screen.getByRole('navigation', { name: 'Primary navigation' })
    expect(within(detailPrimary).getByRole('link', { name: 'Markets' })).toHaveAttribute('aria-current', 'page')
    expect(screen.queryByRole('navigation', { name: 'Markets sections' })).not.toBeInTheDocument()
  })

  it('maps every route in Plether Insights to an unambiguous parent pillar', () => {
    const cases: [string, string, boolean][] = [
      ['/', 'overview', false],
      ['/execution', 'markets', true],
      ['/oracle', 'markets', true],
      ['/oracle/updates/2026935', 'markets', false],
      ['/wallets/0x1111111111111111111111111111111111111111', 'markets', false],
      ['/wallets/0x1111111111111111111111111111111111111111/positions/12', 'markets', false],
      ['/tx/0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', 'markets', false],
      ['/liquidity', 'liquidity', true],
      ['/liquidity/epochs/496900', 'liquidity', false],
      ['/treasury', 'liquidity', true],
      ['/protocol', 'system', true],
      ['/keepers', 'system', true],
      ['/keepers/0x1111111111111111111111111111111111111111', 'system', false],
      ['/trading-accounts', 'system', true],
      ['/trading-accounts/0x1111111111111111111111111111111111111111', 'system', false],
      ['/sponsored', 'system', true],
      ['/infra', 'system', true],
      ['/divisions', 'divisions', true],
      ['/divisions/wallets/0x1111111111111111111111111111111111111111', 'divisions', false],
      ['/results', 'divisions', true],
      ['/register', 'divisions', true],
      ['/methodology', 'divisions', true],
      ['/archive/september-2026', 'divisions', true],
      ['/archive/september-2026/methodology', 'divisions', false],
      ['/competitions/september-2026/wallets/0x1111111111111111111111111111111111111111', 'divisions', false],
    ]

    for (const [path, expectedPillar, expectedSubNav] of cases) {
      expect(resolveActivePillar(path).id).toBe(expectedPillar)
      expect(resolveSectionSubNav(path).show).toBe(expectedSubNav)
    }
  })
})
