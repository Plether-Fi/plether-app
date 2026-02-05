import { type ReactNode } from 'react'
import { WagmiProvider, type Config } from 'wagmi'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { createTestConfig } from './wagmi'

interface TestWrapperProps {
  children: ReactNode
  config?: Config
}

export function TestWrapper({ children, config }: TestWrapperProps) {
  const wagmiConfig = config ?? createTestConfig()
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: {
        retry: false,
        gcTime: 0,
        staleTime: 0,
      },
    },
  })

  return (
    <WagmiProvider config={wagmiConfig as Config}>
      <QueryClientProvider client={queryClient}>
        {children}
      </QueryClientProvider>
    </WagmiProvider>
  )
}

export function createTestWrapper() {
  const config = createTestConfig()
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
      <WagmiProvider config={config as Config}>
        <QueryClientProvider client={client}>
          {children}
        </QueryClientProvider>
      </WagmiProvider>
    )
  }
}
