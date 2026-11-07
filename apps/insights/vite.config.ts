/// <reference types="vitest/config" />
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'
import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'

const privyUiHash = createHash('sha256')
  .update(readFileSync(new URL('./node_modules/@privy-io/react-auth/dist/esm/ui.mjs', import.meta.url)))
  .digest('hex').slice(0, 12)

const apiTarget = process.env.VITE_API_PROXY_TARGET ?? 'http://127.0.0.1:3001'
const registrationApiTarget = process.env.VITE_REGISTRATION_API_PROXY_TARGET ?? 'http://127.0.0.1:3003'

export default defineConfig({
  cacheDir: `node_modules/.vite-privy-${privyUiHash}`,
  plugins: [react()],
  server: {
    proxy: {
      '^/api/insights/v1/divisions/registrations(?:/|$)': {
        target: registrationApiTarget,
        changeOrigin: true,
      },
      '^/api/insights/v1/competitions/[^/]+/registrations(?:/|$)': {
        target: registrationApiTarget,
        changeOrigin: true,
      },
      '/api/insights/v1': {
        target: apiTarget,
        changeOrigin: true,
      },
    },
  },
  preview: {
    port: 4174,
  },
  test: {
    // Transform the real bridge so integration tests can replace only Privy's
    // authentication boundary while exercising its Wagmi synchronization.
    server: { deps: { inline: ['@privy-io/wagmi'] } },
    environment: 'happy-dom',
    include: ['src/**/*.test.{ts,tsx}'],
    setupFiles: ['./src/test/setup.ts'],
    globals: true,
  },
})
