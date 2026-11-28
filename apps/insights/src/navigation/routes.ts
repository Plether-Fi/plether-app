export type NavPillarId = 'overview' | 'markets' | 'liquidity' | 'system' | 'divisions'

export interface NavSectionTab {
  to: string
  label: string
  shortLabel: string
  description: string
}

export interface NavPillar {
  id: NavPillarId
  label: string
  to: string
  tabs: readonly NavSectionTab[]
}

export const NAV_PILLARS: readonly NavPillar[] = [
  {
    id: 'overview',
    label: 'Overview',
    to: '/',
    tabs: [],
  },
  {
    id: 'markets',
    label: 'Markets',
    to: '/execution',
    tabs: [
      {
        to: '/execution',
        label: 'Execution & FAD Risk',
        shortLabel: 'Execution',
        description: 'Order reliability, FAD liquidation watchlist, protections & trade tape',
      },
      {
        to: '/oracle',
        label: 'Oracle & FX Calendar',
        shortLabel: 'Oracle',
        description: 'Pyth 6-feed FX basket admission, value-guards & FX freeze schedule',
      },
    ],
  },
  {
    id: 'liquidity',
    label: 'Liquidity',
    to: '/liquidity',
    tabs: [
      {
        to: '/liquidity',
        label: 'Solvency & LP Ops',
        shortLabel: 'LP Ops',
        description: 'Pool efficiency, solvency envelope, custody parity & epoch settlements',
      },
      {
        to: '/treasury',
        label: 'Treasury',
        shortLabel: 'Treasury',
        description: 'Protocol fee revenue, treasury balances & disbursements',
      },
    ],
  },
  {
    id: 'system',
    label: 'System',
    to: '/protocol',
    tabs: [
      {
        to: '/protocol',
        label: 'Protocol & Contracts',
        shortLabel: 'Protocol',
        description: 'Verified contract manifest, live risk parameters & safety controls',
      },
      {
        to: '/keepers',
        label: 'Keepers',
        shortLabel: 'Keepers',
        description: 'Permissionless keeper fleet, latency & bounties',
      },
      {
        to: '/trading-accounts',
        label: 'Trading Accounts',
        shortLabel: 'Accounts',
        description: 'ZeroDev Kernel v4 smart wallets, ETH vs. sponsored gas & ops',
      },
      {
        to: '/sponsored',
        label: 'Sponsored AA',
        shortLabel: 'Sponsored',
        description: 'ERC-4337 Paymaster runway & UserOp receipts',
      },
      {
        to: '/infra',
        label: 'AWS Infra',
        shortLabel: 'Infra',
        description: 'ECS workers, RDS pool, Nitro RPC & bundler health',
      },
    ],
  },
  {
    id: 'divisions',
    label: 'Divisions',
    to: '/divisions',
    tabs: [
      {
        to: '/divisions',
        label: 'Live Leaderboard',
        shortLabel: 'Leaderboard',
        description: 'Current weekly Divisions standings & live returns',
      },
      {
        to: '/results',
        label: 'Weekly Results',
        shortLabel: 'Results',
        description: 'Published weekly Divisions standings & tier awards',
      },
      {
        to: '/register',
        label: 'Link Discord',
        shortLabel: 'Link Discord',
        description: 'Register a Trading Account & Discord handle for Divisions',
      },
      {
        to: '/methodology',
        label: 'Methodology',
        shortLabel: 'Methodology',
        description: 'Divisions scoring, qualification & tier rules',
      },
      {
        to: '/archive/september-2026',
        label: 'Sept 2026 Archive',
        shortLabel: 'Archive',
        description: 'Archived September 2026 trading competition results',
      },
    ],
  },
] as const

function normalizePath(pathname: string): string {
  const trimmed = pathname.trim()
  if (!trimmed || trimmed === '/') return '/'
  return trimmed.endsWith('/') ? trimmed.slice(0, -1) : trimmed
}

function matchesPrefix(pathname: string, prefix: string): boolean {
  return pathname === prefix || pathname.startsWith(`${prefix}/`)
}

/**
 * Deterministically maps every route in Plether Insights to its parent pillar
 * so the sticky header always displays an unambiguous active highlight.
 */
export function resolveActivePillar(pathname: string): NavPillar {
  const p = normalizePath(pathname)

  if (
    matchesPrefix(p, '/execution') ||
    matchesPrefix(p, '/oracle') ||
    matchesPrefix(p, '/wallets') ||
    matchesPrefix(p, '/tx')
  ) {
    return NAV_PILLARS[1]
  }

  if (matchesPrefix(p, '/liquidity') || matchesPrefix(p, '/treasury')) {
    return NAV_PILLARS[2]
  }

  if (
    matchesPrefix(p, '/protocol') ||
    matchesPrefix(p, '/keepers') ||
    matchesPrefix(p, '/trading-accounts') ||
    matchesPrefix(p, '/sponsored') ||
    matchesPrefix(p, '/infra')
  ) {
    return NAV_PILLARS[3]
  }

  if (
    matchesPrefix(p, '/divisions') ||
    matchesPrefix(p, '/results') ||
    matchesPrefix(p, '/register') ||
    matchesPrefix(p, '/methodology') ||
    matchesPrefix(p, '/archive') ||
    matchesPrefix(p, '/competitions')
  ) {
    return NAV_PILLARS[4]
  }

  return NAV_PILLARS[0]
}

/**
 * Returns the Tier-2 section tabs to display flush below the header when on a
 * pillar's section pages, or an empty array on Overview and drill-down detail
 * pages (which render PageBreadcrumbBar instead).
 */
export function resolveSectionSubNav(pathname: string): {
  pillar: NavPillar
  activeTab: NavSectionTab | null
  show: boolean
} {
  const p = normalizePath(pathname)
  const pillar = resolveActivePillar(p)
  if (pillar.tabs.length === 0) {
    return { pillar, activeTab: null, show: false }
  }
  const activeTab = pillar.tabs.find((tab) => tab.to === p) ?? null
  return { pillar, activeTab, show: activeTab !== null }
}
