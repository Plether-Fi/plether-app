/// <reference types="vitest/config" />
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react-swc';
import { visualizer } from 'rollup-plugin-visualizer';

// https://vite.dev/config/
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { storybookTest } from '@storybook/addon-vitest/vitest-plugin';
import { playwright } from '@vitest/browser-playwright';
const dirname = typeof __dirname !== 'undefined' ? __dirname : path.dirname(fileURLToPath(import.meta.url));

const stubbedPackages = ['@reown/appkit-scaffold-ui', '@reown/appkit-ui', '@reown/appkit-pay'];
const STUB_PREFIX = '\0stub:';

// More info at: https://storybook.js.org/docs/next/writing-tests/integrations/vitest-addon
export default defineConfig({
  plugins: [
    {
      name: 'stub-appkit-ui',
      enforce: 'pre',
      resolveId(source) {
        if (stubbedPackages.some(pkg => source === pkg || source.startsWith(pkg + '/'))) {
          return STUB_PREFIX + source;
        }
      },
      load(id) {
        if (id.startsWith(STUB_PREFIX)) {
          const source = id.slice(STUB_PREFIX.length);
          if (source === '@reown/appkit-ui') {
            return 'export const setColorTheme = () => {};\nexport const setThemeVariables = () => {};';
          }
          return 'export default {};';
        }
      },
    },
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
            if (id.includes('@walletconnect/')) {
              return 'web3-walletconnect';
            }
            if (id.includes('qrcode')) {
              return 'web3-qr';
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