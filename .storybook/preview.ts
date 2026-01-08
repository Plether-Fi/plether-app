import type { Preview } from '@storybook/react-vite'
import '../src/index.css'

const preview: Preview = {
  parameters: {
    backgrounds: {
      default: 'cyber-dark',
      values: [
        { name: 'cyber-dark', value: '#0a0a0f' },
        { name: 'cyber-surface', value: '#12121a' },
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
