import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { describe, expect, it, vi } from 'vitest'
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
  useLiveDivisions: () => ({ data: undefined }),
}))

describe('Layout navigation', () => {
  it('groups operational routes inside an accessible Operations dropdown', () => {
    render(
      <MemoryRouter initialEntries={['/']}>
        <Layout>
          <div>body</div>
        </Layout>
      </MemoryRouter>,
    )

    const primaryNav = screen.getByRole('navigation', { name: 'Primary navigation' })
    const opsButton = within(primaryNav).getByRole('button', { name: /Operations/i })
    expect(opsButton).toHaveAttribute('aria-expanded', 'false')
    expect(screen.queryByRole('menu', { name: 'Operations pages' })).not.toBeInTheDocument()

    // Mobile nav also groups operational routes under Operations instead of dumping all links
    const mobileNav = screen.getByRole('navigation', { name: 'Mobile navigation' })
    expect(within(mobileNav).getByRole('button', { name: /Operations/i })).toBeInTheDocument()

    // Global header search is present and Trading Accounts lives inside Operations
    expect(screen.getByRole('search', { name: /Search accounts/i })).toBeInTheDocument()

    fireEvent.click(opsButton)
    expect(opsButton).toHaveAttribute('aria-expanded', 'true')
    const menu = screen.getByRole('menu', { name: 'Operations pages' })
    expect(menu).toBeInTheDocument()
    expect(screen.getByRole('menuitem', { name: /Oracle/i })).toHaveAttribute('href', '/oracle')
    expect(screen.getByRole('menuitem', { name: /Keepers/i })).toHaveAttribute('href', '/keepers')
    expect(screen.getByRole('menuitem', { name: /^Trading Accounts\b/i })).toHaveAttribute(
      'href',
      '/trading-accounts',
    )
    expect(screen.getByRole('menuitem', { name: /^Sponsored\b/i })).toHaveAttribute('href', '/sponsored')
    expect(screen.getByRole('menuitem', { name: /^Infra\b/i })).toHaveAttribute('href', '/infra')

    fireEvent.keyDown(document, { key: 'Escape' })
    expect(opsButton).toHaveAttribute('aria-expanded', 'false')
    expect(screen.queryByRole('menu', { name: 'Operations pages' })).not.toBeInTheDocument()
  })

  it('highlights the active Operations sub-route in the dropdown trigger label', () => {
    render(
      <MemoryRouter initialEntries={['/oracle']}>
        <Layout>
          <div>oracle body</div>
        </Layout>
      </MemoryRouter>,
    )

    const primaryNav = screen.getByRole('navigation', { name: 'Primary navigation' })
    expect(within(primaryNav).getByRole('button', { name: /Ops · Oracle/i })).toBeInTheDocument()
  })
})
