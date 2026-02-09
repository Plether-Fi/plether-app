import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { WagmiProvider } from 'wagmi'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { ReactQueryDevtools } from '@tanstack/react-query-devtools'
import { createAppKit } from '@reown/appkit/react-core'
import { config, wagmiAdapter, appkitNetworks, WALLETCONNECT_PROJECT_ID } from './config/wagmi'
import './index.css'
import App from './App'

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: (failureCount, error) => {
        const status = (error as { status?: number }).status;
        if (status && status >= 400 && status < 500) {
          return false;
        }
        return failureCount < 3;
      },
    },
  },
})

createAppKit({
  adapters: [wagmiAdapter],
  networks: appkitNetworks,
  projectId: WALLETCONNECT_PROJECT_ID,
  metadata: {
    name: 'Plether',
    description: 'plDXY-BEAR and plDXY-BULL trading protocol',
    url: window.location.origin,
    icons: [`${window.location.origin}/logo.png`],
  },
  themeMode: 'dark',
  themeVariables: {
    '--w3m-accent': '#22c55e',
    '--w3m-border-radius-master': '8px',
  },
})

const rootElement = document.getElementById('root')
if (!rootElement) {
  throw new Error('Root element not found')
}

createRoot(rootElement).render(
  <StrictMode>
    <WagmiProvider config={config}>
      <QueryClientProvider client={queryClient}>
        <App />
        <ReactQueryDevtools initialIsOpen={false} />
      </QueryClientProvider>
    </WagmiProvider>
  </StrictMode>
)
