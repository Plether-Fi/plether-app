import { BrowserRouter, Navigate, Route, Routes, useParams } from 'react-router-dom'
import { DEFAULT_COMPETITION_SLUG } from './api'
import { RouteAnalytics } from './analytics/RouteAnalytics'
import { Layout } from './components/Layout'
import { LeaderboardPage } from './pages/LeaderboardPage'
import { MethodologyPage } from './pages/MethodologyPage'
import { WalletPage } from './pages/WalletPage'

function LegacyWalletRedirect() {
  const { address = '' } = useParams()
  return <Navigate to={`/competitions/${DEFAULT_COMPETITION_SLUG}/wallets/${address}`} replace />
}

export default function App() {
  return (
    <BrowserRouter>
      <RouteAnalytics />
      <Layout>
        <Routes>
          <Route path="/" element={<LeaderboardPage />} />
          <Route path="/competitions/:slug/wallets/:address" element={<WalletPage />} />
          <Route path="/wallets/:address" element={<LegacyWalletRedirect />} />
          <Route path="/methodology" element={<MethodologyPage />} />
          <Route path="*" element={<Navigate to="/" replace />} />
        </Routes>
      </Layout>
    </BrowserRouter>
  )
}
