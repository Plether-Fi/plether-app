import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { QueryClient } from '@tanstack/react-query'
import '@fontsource/uncut-sans/latin.css'
import './index.css'
import App from './App'
import { captureFrontendLog, captureReactException, scheduleAnalyticsInitialization } from './analytics/client'
import { PerpsAaProvider } from './perps-aa'
import { AppErrorBoundary } from './components/AppErrorBoundary'
import { WalletProvider } from './wallet/WalletProvider'
import { WalletTelemetry } from './wallet/WalletTelemetry'

scheduleAnalyticsInitialization()

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
  // AppErrorBoundary reports caught errors once, with its visible support ID.
  // Suppress React's default raw-error console logging for these caught errors.
  onCaughtError: () => { /* Reported by AppErrorBoundary with sanitized context. */ },
  onUncaughtError: (error, info) => {
    captureReactException(error, info, 'uncaught')
    captureFrontendLog('fatal', 'react render error uncaught', {
      component: 'react_root',
      operation: 'render',
      outcome: 'failure',
      error_category: 'uncaught_error',
    })
  },
  onRecoverableError: (error, info) => {
    captureReactException(error, info, 'recoverable')
    captureFrontendLog('warn', 'react render error recovered', {
      component: 'react_root',
      operation: 'render',
      outcome: 'recovered',
      error_category: 'recoverable_error',
    })
  },
}).render(
  <StrictMode>
    <AppErrorBoundary>
      <WalletProvider queryClient={queryClient}>
        <WalletTelemetry />
        <PerpsAaProvider>
          <App />
        </PerpsAaProvider>
      </WalletProvider>
    </AppErrorBoundary>
  </StrictMode>
)
