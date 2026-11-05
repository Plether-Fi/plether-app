import type { Preview } from '@storybook/react-vite'
import { createElement } from 'react'
import { QueryClient } from '@tanstack/react-query'
import { WalletProvider } from '../src/wallet/WalletProvider'
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
        WalletProvider,
        { queryClient, children: createElement(Story) }
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
