import { BrowserRouter, Navigate, Route, Routes, useParams } from 'react-router-dom'
import { DEFAULT_COMPETITION_SLUG } from './api'
import { Layout } from './components/Layout'
import { LeaderboardPage } from './pages/LeaderboardPage'
import { MethodologyPage } from './pages/MethodologyPage'
import { CompetitionArchivePage } from './pages/CompetitionArchivePage'
import { CompetitionMethodologyPage } from './pages/CompetitionMethodologyPage'
import { WalletPage } from './pages/WalletPage'

function LegacyWalletRedirect() {
  const { address = '' } = useParams()
  return <Navigate to={`/competitions/${DEFAULT_COMPETITION_SLUG}/wallets/${address}`} replace />
}

export default function App() {
  return (
    <BrowserRouter>
      <Layout>
        <Routes>
          <Route path="/" element={<LeaderboardPage />} />
          <Route path="/register" element={<Navigate to="/methodology" replace />} />
          <Route path="/competitions/:slug/register" element={<Navigate to="/archive/september-2026" replace />} />
          <Route path="/archive/september-2026" element={<CompetitionArchivePage />} />
          <Route path="/archive/september-2026/methodology" element={<CompetitionMethodologyPage />} />
          <Route path="/competitions/:slug/wallets/:address" element={<WalletPage />} />
          <Route path="/wallets/:address" element={<LegacyWalletRedirect />} />
          <Route path="/methodology" element={<MethodologyPage />} />
          <Route path="*" element={<Navigate to="/" replace />} />
        </Routes>
      </Layout>
    </BrowserRouter>
  )
}
