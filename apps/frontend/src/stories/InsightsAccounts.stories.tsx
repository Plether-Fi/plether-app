import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useEffect, useMemo } from 'react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import type {
  WalletDirectoryEntry,
  WalletDirectoryFilter,
  WalletDirectoryResponse,
  WalletDirectorySort,
} from '../../../insights/src/api/wallets'
import { AccountsPage } from '../../../insights/src/pages/AccountsPage'
import './InsightsPositionBreakdown.css'

const SAMPLE_ITEMS: WalletDirectoryEntry[] = [
  {
    account: '0xfd685c86261f389c6872dbf2beec81416c203079',
    discordUsername: 'gkn190761',
    divisionsTier: 'Gold',
    openSide: 'short',
    openSize: '1161900000000000000000000',
    openNotionalAssets: '1099222489638',
    totalTrades: 42,
    opensCount: 24,
    closesCount: 18,
    liquidationsCount: 0,
    volumeAssets: '4820500000000',
    realizedPnlAssets: '14280450000',
    firstActivityAt: '2026-10-19T08:15:00Z',
    lastActivityAt: '2026-11-27T13:36:27Z',
  },
  {
    account: '0xfe2c91e9d193a5de484f6b4a7cca3dc05a3533c2',
    discordUsername: 'mnitarik',
    divisionsTier: 'Silver',
    openSide: 'long',
    openSize: '582000000000000000000000',
    openNotionalAssets: '550604603640',
    totalTrades: 31,
    opensCount: 17,
    closesCount: 14,
    liquidationsCount: 0,
    volumeAssets: '2940120000000',
    realizedPnlAssets: '8145200000',
    firstActivityAt: '2026-10-22T14:10:00Z',
    lastActivityAt: '2026-11-27T13:20:10Z',
  },
  {
    account: '0x0790e29dd22247cc6f3c211f2bac8f31b4a26952',
    discordUsername: 'macro_fox',
    divisionsTier: 'Platinum',
    openSide: 'long',
    openSize: '412500000000000000000000',
    openNotionalAssets: '390248108250',
    totalTrades: 88,
    opensCount: 46,
    closesCount: 41,
    liquidationsCount: 1,
    volumeAssets: '8142000000000',
    realizedPnlAssets: '22410900000',
    firstActivityAt: '2026-10-12T06:00:00Z',
    lastActivityAt: '2026-11-27T13:18:45Z',
  },
  {
    account: '0x080dd7fb7bf78ad4740e4dc9a3b65542a2375601',
    discordUsername: null,
    divisionsTier: 'Bronze',
    openSide: 'short',
    openSize: '264000000000000000000000',
    openNotionalAssets: '249758789280',
    totalTrades: 19,
    opensCount: 11,
    closesCount: 8,
    liquidationsCount: 0,
    volumeAssets: '1125400000000',
    realizedPnlAssets: '-1842300000',
    firstActivityAt: '2026-11-04T11:30:00Z',
    lastActivityAt: '2026-11-27T12:48:11Z',
  },
  {
    account: '0xcc89401f2b3a91c0d4e5819a21f30c9d8b7e920f',
    discordUsername: 'pavilion6018',
    divisionsTier: 'Diamond',
    openSide: 'long',
    openSize: '198000000000000000000000',
    openNotionalAssets: '187319091960',
    totalTrades: 114,
    opensCount: 59,
    closesCount: 55,
    liquidationsCount: 0,
    volumeAssets: '12450800000000',
    realizedPnlAssets: '39120600000',
    firstActivityAt: '2026-10-12T00:45:00Z',
    lastActivityAt: '2026-11-27T12:12:04Z',
  },
  {
    account: '0xa0117c9e24581bd091aa73b6e52c8104f1a2701b',
    discordUsername: null,
    divisionsTier: 'Bronze',
    openSide: 'short',
    openSize: '95400000000000000000000',
    openNotionalAssets: '90253744308',
    totalTrades: 14,
    opensCount: 8,
    closesCount: 6,
    liquidationsCount: 0,
    volumeAssets: '612900000000',
    realizedPnlAssets: '-412800000',
    firstActivityAt: '2026-11-11T16:20:00Z',
    lastActivityAt: '2026-11-27T10:05:32Z',
  },
  {
    account: '0xc8b7d4109f3e22a6c7b1e8f9a0d2c3b4a5e6c911',
    discordUsername: 'quangduc0119',
    divisionsTier: 'Iron',
    openSide: 'flat',
    openSize: '0',
    openNotionalAssets: '0',
    totalTrades: 27,
    opensCount: 14,
    closesCount: 13,
    liquidationsCount: 0,
    volumeAssets: '1842300000000',
    realizedPnlAssets: '2640150000',
    firstActivityAt: '2026-10-28T09:15:00Z',
    lastActivityAt: '2026-11-26T22:40:00Z',
  },
  {
    account: '0x940fe812b34c910a7d5e6f8123c4b5a69870a30b',
    discordUsername: 'jungleer',
    divisionsTier: 'Iron',
    openSide: 'flat',
    openSize: '0',
    openNotionalAssets: '0',
    totalTrades: 9,
    opensCount: 5,
    closesCount: 3,
    liquidationsCount: 1,
    volumeAssets: '428000000000',
    realizedPnlAssets: '-925400000',
    firstActivityAt: '2026-11-18T19:00:00Z',
    lastActivityAt: '2026-11-25T15:11:00Z',
  },
]

const SAMPLE_SUMMARY: WalletDirectoryResponse['summary'] = {
  totalAccounts: 7617,
  accountsWithOpenPosition: 1482,
  longCount: 814,
  shortCount: 668,
  longOpenInterestAssets: '216585334940000',
  shortOpenInterestAssets: '173851227670000',
  totalOpenInterestAssets: '390436562610000',
  netSkewAssets: '42734107270000',
  avgLeverageX100: 117,
  active24hCount: 318,
  discordLinkedCount: 71,
  totalVolumeAssets: '954280000000000',
}

function filterAndSortItems(
  items: WalletDirectoryEntry[],
  search: string,
  filter: WalletDirectoryFilter,
  sort: WalletDirectorySort,
): WalletDirectoryEntry[] {
  const q = search.trim().replace(/^@/, '').toLowerCase()
  const filtered = items.filter((item) => {
    if (filter === 'open' && item.openSide === 'flat') return false
    if (filter === 'long' && item.openSide !== 'long') return false
    if (filter === 'short' && item.openSide !== 'short') return false
    if (filter === 'profitable' && BigInt(item.realizedPnlAssets) <= 0n) return false
    if (filter === 'discord' && !item.discordUsername) return false
    if (!q) return true
    return (
      item.account.toLowerCase().includes(q) ||
      (item.discordUsername?.toLowerCase().includes(q) ?? false)
    )
  })

  return [...filtered].sort((a, b) => {
    if (sort === 'volume') return Number(BigInt(b.volumeAssets) - BigInt(a.volumeAssets))
    if (sort === 'pnl') return Number(BigInt(b.realizedPnlAssets) - BigInt(a.realizedPnlAssets))
    if (sort === 'trades') return b.totalTrades - a.totalTrades
    if (sort === 'recent') return (b.lastActivityAt ?? '').localeCompare(a.lastActivityAt ?? '')
    return Number(BigInt(b.openNotionalAssets) - BigInt(a.openNotionalAssets))
  })
}

function AccountsStoryCanvas({ items }: { items: WalletDirectoryEntry[] }) {
  useEffect(() => {
    const originalFetch = window.fetch.bind(window)
    window.fetch = async (input, init) => {
      const urlStr = typeof input === 'string' ? input : input instanceof URL ? input.toString() : input.url
      const parsed = new URL(urlStr, window.location.origin)
      if (parsed.pathname === '/api/insights/v1/wallets') {
        const search = parsed.searchParams.get('search') ?? ''
        const filter = (parsed.searchParams.get('filter') ?? 'all') as WalletDirectoryFilter
        const sort = (parsed.searchParams.get('sort') ?? 'notional') as WalletDirectorySort
        const matched = filterAndSortItems(items, search, filter, sort)
        const payload: { data: WalletDirectoryResponse } = {
          data: {
            network: {
              chainId: 421614,
              releaseVersion: 'v2.0.0',
              settlementAssetSymbol: 'USDG',
            },
            observedAt: '2026-11-27T14:31:00Z',
            currentMarkPrice: '94605602',
            markObservedAt: '2026-11-27T14:30:55Z',
            summary: SAMPLE_SUMMARY,
            totalMatched: matched.length,
            limit: 50,
            offset: 0,
            items: matched,
          },
        }
        return new Response(JSON.stringify(payload), {
          status: 200,
          headers: { 'Content-Type': 'application/json' },
        })
      }
      if (parsed.pathname === '/api/insights/v1/divisions/live') {
        return new Response(JSON.stringify({ data: { status: 'pending' } }), {
          status: 200,
          headers: { 'Content-Type': 'application/json' },
        })
      }
      return originalFetch(input, init)
    }
    return () => {
      window.fetch = originalFetch
    }
  }, [items])

  const queryClient = useMemo(() => {
    const qc = new QueryClient({
      defaultOptions: {
        queries: {
          retry: false,
          staleTime: Infinity,
          refetchInterval: false,
          refetchOnMount: false,
          refetchOnWindowFocus: false,
          refetchOnReconnect: false,
        },
      },
    })

    qc.setQueryData(['insights', 'status'], {
      chainId: 421614,
      releaseVersion: 'v2.0.0',
      latestIndexedBlock: '333171209',
      latestIndexedAt: '2026-11-27T14:31:00Z',
      healthy: true,
    })

    // Seed interactive query combinations so filter pills, search, and sorting all work live in Storybook
    const filters: WalletDirectoryFilter[] = ['all', 'open', 'long', 'short', 'profitable', 'discord']
    const sorts: WalletDirectorySort[] = ['notional', 'volume', 'pnl', 'trades', 'recent']
    for (const f of filters) {
      for (const s of sorts) {
        const matched = filterAndSortItems(items, '', f, s)
        qc.setQueryData(['insights', 'wallets-directory', '', f, s, 50, 0], {
          network: {
            chainId: 421614,
            releaseVersion: 'v2.0.0',
            settlementAssetSymbol: 'USDG',
          },
          observedAt: '2026-11-27T14:31:00Z',
          currentMarkPrice: '94605602',
          markObservedAt: '2026-11-27T14:30:55Z',
          summary: SAMPLE_SUMMARY,
          totalMatched: matched.length,
          limit: 50,
          offset: 0,
          items: matched,
        } satisfies WalletDirectoryResponse)
      }
    }

    return qc
  }, [items])

  return (
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/accounts']}>
        <div className="min-h-screen bg-app-bg p-6 text-content-primary sm:p-8">
          <div className="mx-auto max-w-7xl">
            <Routes>
              <Route path="/accounts" element={<AccountsPage />} />
            </Routes>
          </div>
        </div>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta<typeof AccountsStoryCanvas> = {
  title: 'Insights/Trading Accounts Directory',
  component: AccountsStoryCanvas,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof AccountsStoryCanvas>

export const LiveDirectory: Story = {
  args: {
    items: SAMPLE_ITEMS,
  },
}
