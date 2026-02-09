/// <reference types="vitest/config" />
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react-swc';
import { visualizer } from 'rollup-plugin-visualizer';

// https://vite.dev/config/
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { storybookTest } from '@storybook/addon-vitest/vitest-plugin';
import { playwright } from '@vitest/browser-playwright';
import type { Plugin } from 'vite';
const dirname = typeof __dirname !== 'undefined' ? __dirname : path.dirname(fileURLToPath(import.meta.url));

// @reown/appkit is pulled transitively by @walletconnect/ethereum-provider.
// It duplicates @web3modal's ModalController and w3m-modal custom element,
// causing the wallet connect modal to never open (two separate state objects).
// Since we use @web3modal/wagmi for the modal, stub out @reown/appkit entirely.
function stubReownAppkit(): Plugin {
  return {
    name: 'stub-reown-appkit',
    enforce: 'pre',
    resolveId(source) {
      if (source.startsWith('@reown/appkit')) {
        return '\0reown-appkit-stub';
      }
    },
    load(id) {
      if (id === '\0reown-appkit-stub') {
        return 'export default {};';
      }
    },
  };
}

// More info at: https://storybook.js.org/docs/next/writing-tests/integrations/vitest-addon
export default defineConfig({
  plugins: [
    stubReownAppkit(),
    react(),
    visualizer({
      filename: 'bundle-stats.html',
      gzipSize: true,
      brotliSize: true,
    }),
  ],
  build: {
    rollupOptions: {
      output: {
        manualChunks(id) {
          if (id.includes('node_modules')) {
            if (id.includes('react-dom') || id.includes('react-router')) {
              return 'react-vendor';
            }
            if (id.includes('/viem/')) {
              return 'web3-core';
            }
            if (id.includes('/wagmi/') || id.includes('@tanstack/react-query')) {
              return 'web3-wagmi';
            }
            if (id.includes('@web3modal')) {
              return 'web3-modal';
            }
          }
        },
      },
    },
  },
  test: {
    projects: [
      {
        extends: true,
        plugins: [
          storybookTest({
            configDir: path.join(dirname, '.storybook')
          })
        ],
        test: {
          name: 'storybook',
          browser: {
            enabled: true,
            headless: true,
            provider: playwright({}),
            instances: [{
              browser: 'chromium'
            }]
          },
          setupFiles: ['.storybook/vitest.setup.ts']
        }
      },
      {
        extends: true,
        test: {
          name: 'unit',
          environment: 'happy-dom',
          include: ['src/**/*.test.{ts,tsx}'],
          exclude: ['src/**/*.stories.tsx', 'src/**/*.integration.test.{ts,tsx}'],
          setupFiles: ['./src/test/setup.ts'],
          globals: true,
        }
      },
      {
        extends: true,
        test: {
          name: 'integration',
          environment: 'happy-dom',
          include: ['src/**/*.integration.test.{ts,tsx}'],
          setupFiles: ['./src/test/setup.ts', './src/test/integration.setup.ts'],
          globals: true,
          testTimeout: 30000,
          hookTimeout: 30000,
        }
      }
    ]
  }
});