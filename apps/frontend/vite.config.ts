/// <reference types="vitest/config" />
import { defineConfig, loadEnv, type ProxyOptions } from 'vite';
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
const AA_PROXY_PATHS = new Set([
  '/api/perps/v1/aa/pimlico',
  '/api/aa/pimlico',
  '/api/perps/v1/aa/rpc',
  '/api/aa/rpc',
]);
const AA_PROXY_AUTH_HEADER = 'X-Plether-AA-Proxy-Token';
const FAUCET_PROXY_PATH = '/api/perps/v1/testnet/faucet';
const FAUCET_BACKEND_PROXY_PATH = '/api/testnet/faucet';
const FAUCET_PROXY_AUTH_HEADER = 'X-Plether-Faucet-Proxy-Token';

type RuntimeEnv = Record<string, string | undefined>;

function parseHeadersFile(): Record<string, string> {
  const raw = fs.readFileSync(path.join(dirname, 'public/_headers'), 'utf-8');
  const headers: Record<string, string> = {};
  for (const line of raw.split('\n')) {
    const match = /^\s+([A-Za-z-]+):\s*(.+)$/.exec(line);
    if (match) headers[match[1]] = match[2];
  }
  return headers;
}

function apiProxyConfig(
  env: RuntimeEnv,
  authenticateProtectedRoutes = false
): ProxyOptions {
  const target = env.VITE_API_PROXY_TARGET ?? DEFAULT_API_PROXY_TARGET;
  const preserveProxyPath = env.VITE_API_PROXY_PRESERVE_PATH === '1';

  return {
    target,
    changeOrigin: true,
    rewrite: preserveProxyPath
      ? undefined
      : (proxyPath) => proxyPath.replace(/^\/api\/(?:spot|perps)\/v1/, '/api'),
    configure: authenticateProtectedRoutes
      ? (proxy) => {
          proxy.on('proxyReq', (proxyRequest, request) => {
            proxyRequest.removeHeader(AA_PROXY_AUTH_HEADER);
            proxyRequest.removeHeader(FAUCET_PROXY_AUTH_HEADER);
            const requestPath = request.url?.split('?', 1)[0];
            if (
              requestPath &&
              AA_PROXY_PATHS.has(requestPath) &&
              env.AA_PROXY_ORIGIN_TOKEN
            ) {
              proxyRequest.removeHeader('CF-Connecting-IP');
              proxyRequest.setHeader(
                AA_PROXY_AUTH_HEADER,
                env.AA_PROXY_ORIGIN_TOKEN
              );
              proxyRequest.setHeader(
                'CF-Connecting-IP',
                request.socket.remoteAddress ?? '127.0.0.1'
              );
            }
            if (
              (
                requestPath === FAUCET_PROXY_PATH ||
                requestPath === FAUCET_BACKEND_PROXY_PATH
              ) &&
              env.FAUCET_PROXY_ORIGIN_TOKEN
            ) {
              proxyRequest.removeHeader('CF-Connecting-IP');
              proxyRequest.setHeader(
                FAUCET_PROXY_AUTH_HEADER,
                env.FAUCET_PROXY_ORIGIN_TOKEN
              );
              proxyRequest.setHeader(
                'CF-Connecting-IP',
                request.socket.remoteAddress ?? '127.0.0.1'
              );
            }
          });
        }
      : undefined,
  };
}

function pythHermesProxyConfig(env: RuntimeEnv): ProxyOptions {
  return {
    target: env.VITE_PYTH_HERMES_PROXY_TARGET ?? 'https://hermes.pyth.network',
    changeOrigin: true,
    rewrite: (proxyPath) => proxyPath.replace(/^\/pyth-hermes/, ''),
  };
}

function buildCommit(env: RuntimeEnv): string {
  const envCommit = env.VITE_BUILD_COMMIT ?? env.CF_PAGES_COMMIT_SHA;
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
export default defineConfig(({ mode }) => {
  const env: RuntimeEnv = {
    ...loadEnv(mode, dirname, ''),
    ...process.env,
  };

  return {
  define: {
    'import.meta.env.VITE_BUILD_COMMIT': JSON.stringify(buildCommit(env)),
    'import.meta.env.VITE_DEV_API_PROXY_TARGET': JSON.stringify(env.VITE_API_PROXY_TARGET ?? DEFAULT_API_PROXY_TARGET),
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
      '/api/spot/v1': apiProxyConfig(env),
      '/api/perps/v1': apiProxyConfig(env, true),
      '/pyth-hermes': pythHermesProxyConfig(env),
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
  },
  };
});
