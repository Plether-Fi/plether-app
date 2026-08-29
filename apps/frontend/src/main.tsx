import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { WagmiProvider } from 'wagmi'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { config, scheduleAppKitInitialization } from './config/wagmi'
import '@fontsource/uncut-sans/latin.css'
import './index.css'
import App from './App'
import { captureFrontendLog, scheduleAnalyticsInitialization } from './analytics/client'
import { PerpsAaProvider } from './perps-aa'

scheduleAnalyticsInitialization()
scheduleAppKitInitialization()

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

const rootElement = document.getElementById('root')
if (!rootElement) {
  throw new Error('Root element not found')
}

createRoot(rootElement, {
  onCaughtError: () => {
    captureFrontendLog('error', 'react render error caught', {
      component: 'react_root',
      operation: 'render',
      outcome: 'failure',
      error_category: 'caught_error',
    })
  },
  onUncaughtError: () => {
    captureFrontendLog('fatal', 'react render error uncaught', {
      component: 'react_root',
      operation: 'render',
      outcome: 'failure',
      error_category: 'uncaught_error',
    })
  },
  onRecoverableError: () => {
    captureFrontendLog('warn', 'react render error recovered', {
      component: 'react_root',
      operation: 'render',
      outcome: 'recovered',
      error_category: 'recoverable_error',
    })
  },
}).render(
  <StrictMode>
    <WagmiProvider config={config}>
      <QueryClientProvider client={queryClient}>
        <PerpsAaProvider>
          <App />
        </PerpsAaProvider>
      </QueryClientProvider>
    </WagmiProvider>
  </StrictMode>
)
