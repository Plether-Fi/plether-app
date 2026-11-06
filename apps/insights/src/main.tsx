import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { QueryClient } from '@tanstack/react-query'
import '@fontsource/uncut-sans/latin.css'
import './index.css'
import App from './App'
import { InsightsApiError } from './api'
import { WalletProvider } from './wallet/WalletProvider'

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: (failureCount, error) => {
        if (error instanceof InsightsApiError && error.status >= 400 && error.status < 500) return false
        return failureCount < 2
      },
      refetchOnWindowFocus: false,
    },
  },
})

const root = document.getElementById('root')
if (!root) throw new Error('Root element not found')

createRoot(root).render(
  <StrictMode>
    <WalletProvider queryClient={queryClient}>
      <App />
    </WalletProvider>
  </StrictMode>,
)
