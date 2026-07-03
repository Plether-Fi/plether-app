/// <reference types="vitest/config" />
import { defineConfig, type ProxyOptions } from 'vite';
import react from '@vitejs/plugin-react-swc';
import { visualizer } from 'rollup-plugin-visualizer';

import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { storybookTest } from '@storybook/addon-vitest/vitest-plugin';
import { playwright } from '@vitest/browser-playwright';
const dirname = typeof __dirname !== 'undefined' ? __dirname : path.dirname(fileURLToPath(import.meta.url));
const DEFAULT_API_PROXY_TARGET = 'http://127.0.0.1:3001';
const PRESERVE_API_PROXY_PATH = process.env.VITE_API_PROXY_PRESERVE_PATH === '1';

function parseHeadersFile(): Record<string, string> {
  const raw = fs.readFileSync(path.join(dirname, 'public/_headers'), 'utf-8');
  const headers: Record<string, string> = {};
  for (const line of raw.split('\n')) {
    const match = /^\s+([A-Za-z-]+):\s*(.+)$/.exec(line);
    if (match) headers[match[1]] = match[2];
  }
  return headers;
}

function apiProxyConfig(): ProxyOptions {
  return {
    target: process.env.VITE_API_PROXY_TARGET ?? DEFAULT_API_PROXY_TARGET,
    changeOrigin: true,
    rewrite: PRESERVE_API_PROXY_PATH
      ? undefined
      : (proxyPath) => proxyPath.replace(/^\/api\/(?:v1|sepolia_v1)/, '/api'),
  };
}

function pythHermesProxyConfig(): ProxyOptions {
  return {
    target: process.env.VITE_PYTH_HERMES_PROXY_TARGET ?? 'https://hermes.pyth.network',
    changeOrigin: true,
    rewrite: (proxyPath) => proxyPath.replace(/^\/pyth-hermes/, ''),
  };
}

function buildCommit(): string {
  const envCommit = process.env.VITE_BUILD_COMMIT ?? process.env.CF_PAGES_COMMIT_SHA;
  if (envCommit) return envCommit.slice(0, 12);

  try {
    return execFileSync('git', ['rev-parse', '--short=12', 'HEAD'], {
      cwd: dirname,
      encoding: 'utf-8',
    }).trim();
  } catch {
    return 'dev';
  }
}

// More info at: https://storybook.js.org/docs/next/writing-tests/integrations/vitest-addon
export default defineConfig({
  define: {
    'import.meta.env.VITE_BUILD_COMMIT': JSON.stringify(buildCommit()),
    'import.meta.env.VITE_DEV_API_PROXY_TARGET': JSON.stringify(process.env.VITE_API_PROXY_TARGET ?? DEFAULT_API_PROXY_TARGET),
  },
  plugins: [
    react(),
    visualizer({
      filename: 'bundle-stats.html',
      gzipSize: true,
      brotliSize: true,
    }),
  ],
  server: {
    headers: parseHeadersFile(),
    proxy: {
      '/api/v1': apiProxyConfig(),
      '/api/sepolia_v1': apiProxyConfig(),
      '/api': apiProxyConfig(),
      '/pyth-hermes': pythHermesProxyConfig(),
    },
  },
  preview: { headers: parseHeadersFile() },
  build: {
    modulePreload: { polyfill: false },
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
            if (id.includes('@walletconnect/') || id.includes('@reown/')) {
              return 'web3-walletconnect';
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
          exclude: [
            'src/**/*.stories.tsx',
            'src/**/*.integration.test.{ts,tsx}',
            'src/**/*.perps-integration.test.{ts,tsx}',
            'src/**/*.perps-fork.test.{ts,tsx}',
          ],
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
      },
      {
        extends: true,
        test: {
          name: 'perps-integration',
          environment: 'happy-dom',
          include: ['src/**/*.perps-integration.test.{ts,tsx}'],
          setupFiles: ['./src/test/setup.ts'],
          globals: true,
          testTimeout: 30000,
          hookTimeout: 30000,
        }
      },
      {
        extends: true,
        test: {
          name: 'perps-fork',
          environment: 'node',
          include: ['src/**/*.perps-fork.test.{ts,tsx}'],
          globals: true,
          testTimeout: 120000,
          hookTimeout: 60000,
        }
      }
    ]
  }
});
