import { BrowserRouter, Navigate, Route, Routes, useParams } from 'react-router-dom'
import { DEFAULT_COMPETITION_SLUG } from './api'
import { Layout } from './components/Layout'
import { AccountPage } from './pages/AccountPage'
import { CompetitionArchivePage } from './pages/CompetitionArchivePage'
import { CompetitionMethodologyPage } from './pages/CompetitionMethodologyPage'
import { DivisionsRegistrationPage } from './pages/DivisionsRegistrationPage'
import { DivisionsWalletPage } from './pages/DivisionsWalletPage'
import { ExecutionPage } from './pages/ExecutionPage'
import { InfraPage } from './pages/InfraPage'
import { KeepersPage } from './pages/KeepersPage'
import { LeaderboardPage } from './pages/LeaderboardPage'
import { LiquidityOpsPage } from './pages/LiquidityOpsPage'
import { MethodologyPage } from './pages/MethodologyPage'
import { OverviewPage } from './pages/OverviewPage'
import { ProtocolPage } from './pages/ProtocolPage'
import { TransactionPage } from './pages/TransactionPage'
import { WalletPage } from './pages/WalletPage'
import { WeeklyResultsPage } from './pages/WeeklyResultsPage'

function LegacyCompetitionWalletRedirect() {
  const { address = '' } = useParams()
  return <Navigate to={`/competitions/${DEFAULT_COMPETITION_SLUG}/wallets/${address}`} replace />
}

export default function App() {
  return (
    <BrowserRouter>
      <Layout>
        <Routes>
          <Route path="/" element={<OverviewPage />} />
          <Route path="/execution" element={<ExecutionPage />} />
          <Route path="/liquidity" element={<LiquidityOpsPage />} />
          <Route path="/keepers" element={<KeepersPage />} />
          <Route path="/protocol" element={<ProtocolPage />} />
          <Route path="/infra" element={<InfraPage />} />
          <Route path="/wallets/:address" element={<AccountPage />} />
          <Route path="/tx/:txHash" element={<TransactionPage />} />
          <Route path="/divisions" element={<LeaderboardPage />} />
          <Route path="/divisions/wallets/:address" element={<DivisionsWalletPage />} />
          <Route path="/results" element={<WeeklyResultsPage />} />
          <Route path="/register" element={<DivisionsRegistrationPage />} />
          <Route path="/competitions/:slug/register" element={<Navigate to="/archive/september-2026" replace />} />
          <Route path="/archive/september-2026" element={<CompetitionArchivePage />} />
          <Route path="/archive/september-2026/methodology" element={<CompetitionMethodologyPage />} />
          <Route path="/competitions/:slug/wallets/:address" element={<WalletPage />} />
          <Route path="/archive/wallets/:address" element={<LegacyCompetitionWalletRedirect />} />
          <Route path="/methodology" element={<MethodologyPage />} />
          <Route path="*" element={<Navigate to="/" replace />} />
        </Routes>
      </Layout>
    </BrowserRouter>
  )
}
