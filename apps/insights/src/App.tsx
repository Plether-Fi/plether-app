import type { ReactNode } from 'react'
import { BrowserRouter, Navigate, Route, Routes, useParams } from 'react-router-dom'
import { DEFAULT_COMPETITION_SLUG, useCurrentProtocolRelease } from './api'
import { Layout } from './components/Layout'
import { ErrorState } from './components/ui'
import { LeaderboardPage } from './pages/LeaderboardPage'
import { HousePoolPage } from './pages/HousePoolPage'
import { KeeperDetailPage } from './pages/KeeperDetailPage'
import { KeepersPage } from './pages/KeepersPage'
import { MethodologyPage } from './pages/MethodologyPage'
import { OrderDetailPage } from './pages/OrderDetailPage'
import { OverviewPage } from './pages/OverviewPage'
import { ParametersPage } from './pages/ParametersPage'
import { ProtocolWalletDetailPage } from './pages/ProtocolWalletDetailPage'
import { ProtocolWalletsPage } from './pages/ProtocolWalletsPage'
import { TranchePage } from './pages/TranchePage'
import { TransactionDetailPage } from './pages/TransactionDetailPage'
import { TransactionsPage } from './pages/TransactionsPage'
import { WalletPage } from './pages/WalletPage'

function LegacyWalletRedirect() {
  const { address = '' } = useParams()
  return <Navigate to={`/competitions/${DEFAULT_COMPETITION_SLUG}/wallets/${address}`} replace />
}

type ExplorerState = 'loading' | 'enabled' | 'disabled' | 'error'

function ExplorerRoute({
  children,
  errorMessage,
  onRetry,
  state,
}: {
  children: ReactNode
  errorMessage?: string
  onRetry: () => void
  state: ExplorerState
}) {
  if (state === 'loading') {
    return (
      <div className="space-y-7" role="status" aria-live="polite">
        <p className="sr-only">Loading Protocol Explorer configuration</p>
        <div className="skeleton h-36" />
        <div className="skeleton h-64" />
      </div>
    )
  }
  if (state === 'error') {
    return (
      <ErrorState
        title="Protocol Explorer configuration unavailable"
        message={errorMessage}
        onRetry={onRetry}
      />
    )
  }
  if (state === 'disabled') {
    return <Navigate to={`/competitions/${DEFAULT_COMPETITION_SLUG}`} replace />
  }
  return children
}

export function AppRoutes() {
  const release = useCurrentProtocolRelease()
  const explorerState: ExplorerState = release.isLoading
    ? 'loading'
    : release.isError
      ? 'error'
      : release.data?.explorerEnabled === true
      ? 'enabled'
      : 'disabled'

  const explorerRoute = (page: ReactNode) => (
    <ExplorerRoute
      errorMessage={release.error?.message}
      onRetry={() => { void release.refetch() }}
      state={explorerState}
    >
      {page}
    </ExplorerRoute>
  )

  return (
    <Layout
      explorerEnabled={explorerState === 'enabled'}
      protocolReleaseId={release.data?.releaseId}
    >
      <Routes>
        <Route path="/" element={explorerRoute(<OverviewPage />)} />
        <Route path="/transactions" element={explorerRoute(<TransactionsPage />)} />
        <Route path="/transactions/:txHash" element={explorerRoute(<TransactionDetailPage />)} />
        <Route path="/orders/:releaseId/:orderId" element={explorerRoute(<OrderDetailPage />)} />
        <Route path="/house-pool" element={explorerRoute(<HousePoolPage />)} />
        <Route path="/house-pool/:tranche" element={explorerRoute(<TranchePage />)} />
        <Route path="/keepers" element={explorerRoute(<KeepersPage />)} />
        <Route path="/keepers/:address" element={explorerRoute(<KeeperDetailPage />)} />
        <Route path="/protocol-wallets" element={explorerRoute(<ProtocolWalletsPage />)} />
        <Route path="/protocol-wallets/:address" element={explorerRoute(<ProtocolWalletDetailPage />)} />
        <Route path="/parameters" element={explorerRoute(<ParametersPage />)} />
        <Route path="/competitions/:slug" element={<LeaderboardPage />} />
        <Route path="/competitions/:slug/wallets/:address" element={<WalletPage />} />
        <Route path="/leaderboard" element={<Navigate to={`/competitions/${DEFAULT_COMPETITION_SLUG}`} replace />} />
        <Route path="/wallets/:address" element={<LegacyWalletRedirect />} />
        <Route path="/methodology" element={<MethodologyPage />} />
        <Route
          path="*"
          element={
            explorerState === 'loading' || explorerState === 'error'
              ? explorerRoute(<></>)
              : (
                  <Navigate
                    to={explorerState === 'enabled' ? '/' : `/competitions/${DEFAULT_COMPETITION_SLUG}`}
                    replace
                  />
                )
          }
        />
      </Routes>
    </Layout>
  )
}

export default function App() {
  return (
    <BrowserRouter>
      <AppRoutes />
    </BrowserRouter>
  )
}
