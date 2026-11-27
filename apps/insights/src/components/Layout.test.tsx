import { fireEvent, render, screen } from '@testing-library/react'
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

    const opsButton = screen.getByRole('button', { name: /Operations/i })
    expect(opsButton).toHaveAttribute('aria-expanded', 'false')
    expect(screen.queryByRole('menu', { name: 'Operations pages' })).not.toBeInTheDocument()

    fireEvent.click(opsButton)
    expect(opsButton).toHaveAttribute('aria-expanded', 'true')
    const menu = screen.getByRole('menu', { name: 'Operations pages' })
    expect(menu).toBeInTheDocument()
    expect(screen.getByRole('menuitem', { name: /Oracle/i })).toHaveAttribute('href', '/oracle')
    expect(screen.getByRole('menuitem', { name: /Keepers/i })).toHaveAttribute('href', '/keepers')
    expect(screen.getByRole('menuitem', { name: /Sponsored/i })).toHaveAttribute('href', '/sponsored')
    expect(screen.getByRole('menuitem', { name: /Infra/i })).toHaveAttribute('href', '/infra')

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

    expect(screen.getByRole('button', { name: /Ops · Oracle/i })).toBeInTheDocument()
  })
})
