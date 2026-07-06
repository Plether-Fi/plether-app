import { lazy, Suspense } from 'react'
import { BrowserRouter, Navigate, Routes, Route } from 'react-router-dom'
import { Layout } from './components/layout'
import { TransactionModal } from './components/TransactionModal'
import { RiskDisclaimer } from './components/RiskDisclaimer'
import { TestnetWelcomeModal } from './components/TestnetWelcomeModal'
import { Spinner } from './components/ui/Spinner'
import { isPrimaryAppDeployment, isSepoliaDeployment } from './utils/deployment'

const Perps = lazy(() => import('./pages/Perps'))
const Dashboard = lazy(() => import('./pages/Dashboard'))
const Mint = lazy(() => import('./pages/Mint'))
const Stake = lazy(() => import('./pages/Stake'))
const History = lazy(() => import('./pages/History'))
const Terms = lazy(() => import('./pages/Terms'))
const Privacy = lazy(() => import('./pages/Privacy'))
const RiskDisclosurePage = lazy(() => import('./pages/RiskDisclosure'))

function App() {
  const shouldDefaultToSpot = isPrimaryAppDeployment()

  return (
    <BrowserRouter>
      <Layout>
        <Suspense fallback={<div className="flex items-center justify-center min-h-[50vh]"><Spinner size="lg" /></div>}>
          <Routes>
            <Route path="/" element={shouldDefaultToSpot ? <Navigate to="/spot" replace /> : <Perps />} />
            <Route path="/spot" element={<Dashboard />} />
            <Route path="/leverage" element={<Dashboard />} />
            <Route path="/lending" element={<Dashboard />} />
            <Route path="/mint" element={<Mint />} />
            <Route path="/stake" element={<Stake />} />
            <Route path="/history" element={<History />} />
            <Route path="/terms" element={<Terms />} />
            <Route path="/privacy" element={<Privacy />} />
            <Route path="/risk" element={<RiskDisclosurePage />} />
          </Routes>
        </Suspense>
      </Layout>
      <TransactionModal />
      {isSepoliaDeployment() ? <TestnetWelcomeModal /> : <RiskDisclaimer />}
    </BrowserRouter>
  )
}

export default App
