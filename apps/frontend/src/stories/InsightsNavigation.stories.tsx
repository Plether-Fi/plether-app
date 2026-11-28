import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo, type ReactNode } from 'react'
import { MemoryRouter } from 'react-router-dom'
import { Layout } from '../../../insights/src/components/Layout'
import {
  PageBreadcrumbBar,
  WalletPerspectiveSwitcher,
} from '../../../insights/src/components/PageBreadcrumbBar'
import './InsightsPositionBreakdown.css'

const DEMO_WALLET = '0x94d23c1a2b4c5d6e7f8091a2b3c4d5e6f708192a'

function NavigationFrame({
  initialPath,
  children,
}: {
  initialPath: string
  children: ReactNode
}) {
  const client = useMemo(() => {
    const qc = new QueryClient({
      defaultOptions: {
        queries: {
          retry: false,
          refetchInterval: false,
          refetchOnMount: false,
          refetchOnWindowFocus: false,
          refetchOnReconnect: false,
        },
      },
    })
    qc.setQueryData(['insights', 'status'], {
      healthy: true,
      latestIndexedBlock: 333462755,
      latestIndexedAt: '2026-11-28T17:33:14Z',
      chainId: 421614,
    })
    qc.setQueryData(['insights', 'divisions', 'live', 'summary'], {
      status: 'ready',
      scoringRelease: { version: 'v2.0.0', assetSymbol: 'USDG' },
      weekStartsAt: '2026-11-22T21:00:00Z',
      snapshotAt: '2026-11-28T17:33:14Z',
      snapshotBlock: '333462755',
      walletCount: 48,
      topStandings: [],
    })
    return qc
  }, [])

  return (
    <QueryClientProvider client={client}>
      <MemoryRouter initialEntries={[initialPath]}>
        <Layout>{children}</Layout>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta = {
  title: 'Insights/Unified Navigation & Breadcrumbs',
  parameters: {
    layout: 'fullscreen',
    backgrounds: { default: 'dark' },
  },
}

export default meta

export const MarketsSectionSubNav: StoryObj = {
  render: () => (
    <NavigationFrame initialPath="/oracle">
      <div className="space-y-4">
        <p className="font-mono text-xs uppercase tracking-[0.16em] text-brand-peach">
          Tier 1 (Markets active) + Tier 2 Section Tabs
        </p>
        <h1 className="text-2xl font-semibold">Oracle &amp; FX Calendar</h1>
        <p className="max-w-2xl text-sm text-content-secondary">
          All sibling pages within a pillar sit in a consistent flush sub-navigation bar directly
          below the sticky header—no dropdown hunting and no ad-hoc bottom cross-links.
        </p>
      </div>
    </NavigationFrame>
  ),
}

export const SystemSectionSubNav: StoryObj = {
  render: () => (
    <NavigationFrame initialPath="/trading-accounts">
      <div className="space-y-4">
        <p className="font-mono text-xs uppercase tracking-[0.16em] text-brand-peach">
          Tier 1 (System active) + 5 System Section Tabs
        </p>
        <h1 className="text-2xl font-semibold">Trading Accounts (Kernel v4)</h1>
        <p className="max-w-2xl text-sm text-content-secondary">
          Protocol &amp; Contracts, Keepers, Trading Accounts, Sponsored AA, and AWS Infra are all
          1-click peers inside the System pillar.
        </p>
      </div>
    </NavigationFrame>
  ),
}

export const PositionDetailBreadcrumb: StoryObj = {
  render: () => (
    <NavigationFrame initialPath={`/wallets/${DEMO_WALLET}/positions/412`}>
      <div className="space-y-6">
        <PageBreadcrumbBar
          items={[
            { label: 'Markets', to: '/execution' },
            { label: '@inshogami', to: `/wallets/${DEMO_WALLET}` },
            { label: 'Position #412' },
          ]}
        />
        <div className="space-y-2">
          <p className="font-mono text-xs uppercase tracking-[0.16em] text-brand-peach">
            Detail Page Breadcrumb Bar
          </p>
          <h1 className="text-2xl font-semibold">LONG plDXY-PERP · Position #412</h1>
          <p className="max-w-2xl text-sm text-content-secondary">
            Replaces the floating left/right back-link bar with a crisp hierarchical breadcrumb
            trail while keeping Markets highlighted in the top bar.
          </p>
        </div>
      </div>
    </NavigationFrame>
  ),
}

export const WalletWithPerspectiveSwitcher: StoryObj = {
  render: () => (
    <NavigationFrame initialPath={`/wallets/${DEMO_WALLET}`}>
      <div className="space-y-6">
        <PageBreadcrumbBar
          items={[
            { label: 'Markets', to: '/execution' },
            { label: '@inshogami' },
          ]}
          rightSlot={<WalletPerspectiveSwitcher address={DEMO_WALLET} active="wallet" />}
        />
        <div className="space-y-2">
          <p className="font-mono text-xs uppercase tracking-[0.16em] text-brand-peach">
            Wallet Breadcrumb + Segmented Perspective Switcher
          </p>
          <h1 className="text-2xl font-semibold">@inshogami</h1>
          <p className="max-w-2xl text-sm text-content-secondary">
            Switch seamlessly between Wallet Ledger, Kernel v4 smart account diagnostics, and
            Divisions profile from one consistent control.
          </p>
        </div>
      </div>
    </NavigationFrame>
  ),
}
