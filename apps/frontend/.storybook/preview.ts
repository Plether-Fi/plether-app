import type { Preview } from '@storybook/react-vite'
import { createElement } from 'react'
import { WagmiProvider } from 'wagmi'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { config } from '../src/config/wagmi'
import '../src/index.css'

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false,
      refetchOnWindowFocus: false,
    },
  },
})

const preview: Preview = {
  decorators: [
    (Story) =>
      createElement(
        WagmiProvider,
        { config },
        createElement(QueryClientProvider, { client: queryClient }, createElement(Story))
      ),
  ],
  parameters: {
    backgrounds: {
      default: 'plether-bg',
      values: [
        { name: 'plether-bg', value: '#250917' },
        { name: 'plether-surface', value: '#3B212D' },
      ],
    },
    controls: {
      matchers: {
        color: /(background|color)$/i,
        date: /Date$/i,
      },
    },
    a11y: {
      test: 'todo'
    }
  },
};

export default preview;
