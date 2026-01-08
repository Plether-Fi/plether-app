import { BrowserRouter, Routes, Route } from 'react-router-dom'
import { Layout } from './components/layout'
import {
  Dashboard,
  Mint,
  Stake,
  History,
} from './pages'

function App() {
  return (
    <BrowserRouter>
      <Layout>
        <Routes>
          <Route path="/" element={<Dashboard />} />
          <Route path="/leverage" element={<Dashboard />} />
          <Route path="/yield" element={<Dashboard />} />
          <Route path="/mint" element={<Mint />} />
          <Route path="/stake" element={<Stake />} />
          <Route path="/history" element={<History />} />
        </Routes>
      </Layout>
    </BrowserRouter>
  )
}

export default App
