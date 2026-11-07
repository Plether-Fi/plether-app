import { defineConfig } from '@playwright/test'

const port = process.env.E2E_PORT ?? '5187'
const baseURL = `http://127.0.0.1:${port}`

// This checks the deliberately unconfigured fallback, not live Privy login.
export default defineConfig({
  testDir: './tests',
  outputDir: '../test-results/privy-unconfigured',
  testMatch: 'privy-unconfigured.spec.ts',
  forbidOnly: !!process.env.CI,
  reporter: 'list',
  use: { baseURL, trace: 'on-first-retry' },
  projects: [{ name: 'privy-unconfigured', use: { browserName: 'chromium', viewport: { width: 1280, height: 900 } } }],
  webServer: {
    command: `npm run dev -- --host 127.0.0.1 --port ${port} --strictPort`,
    url: baseURL,
    reuseExistingServer: false,
    timeout: 120 * 1000,
    env: {
      PLETHER_VITE_CACHE_DIR: `node_modules/.vite-playwright-privy-unconfigured-${port}`,
      VITE_PRIVY_APP_ID: '',
      VITE_PRIVY_CLIENT_ID: '',
      VITE_DEFAULT_CHAIN_ID: '421614',
      VITE_POSTHOG_KEY: '',
    },
  },
})
