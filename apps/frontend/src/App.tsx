import { lazy, Suspense } from 'react'
import { BrowserRouter, Routes, Route } from 'react-router-dom'
import { Layout } from './components/layout'
import { TransactionModal } from './components/TransactionModal'
import { Spinner } from './components/ui/Spinner'
import { useApiChainSync } from './api'

const Dashboard = lazy(() => import('./pages/Dashboard'))
const Mint = lazy(() => import('./pages/Mint'))
const Stake = lazy(() => import('./pages/Stake'))
const History = lazy(() => import('./pages/History'))

function App() {
  useApiChainSync()

  return (
    <BrowserRouter>
      <Layout>
        <Suspense fallback={<div className="flex items-center justify-center min-h-[50vh]"><Spinner size="lg" /></div>}>
          <Routes>
            <Route path="/" element={<Dashboard />} />
            <Route path="/leverage" element={<Dashboard />} />
            <Route path="/lending" element={<Dashboard />} />
            <Route path="/mint" element={<Mint />} />
            <Route path="/stake" element={<Stake />} />
            <Route path="/history" element={<History />} />
          </Routes>
        </Suspense>
      </Layout>
      <TransactionModal />
    </BrowserRouter>
  )
}

export default App
