/* eslint-disable react-refresh/only-export-components */
import { type ReactNode } from 'react'
import { WagmiProvider, type Config } from 'wagmi'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { testConfig } from './wagmi'

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false,
      gcTime: 0,
      staleTime: 0,
    },
  },
})

interface TestWrapperProps {
  children: ReactNode
}

export function TestWrapper({ children }: TestWrapperProps) {
  return (
    <WagmiProvider config={testConfig as Config}>
      <QueryClientProvider client={queryClient}>
        {children}
      </QueryClientProvider>
    </WagmiProvider>
  )
}

export function createTestWrapper() {
  const client = new QueryClient({
    defaultOptions: {
      queries: {
        retry: false,
        gcTime: 0,
        staleTime: 0,
      },
    },
  })

  return function Wrapper({ children }: { children: ReactNode }) {
    return (
      <WagmiProvider config={testConfig as Config}>
        <QueryClientProvider client={client}>
          {children}
        </QueryClientProvider>
      </WagmiProvider>
    )
  }
}
