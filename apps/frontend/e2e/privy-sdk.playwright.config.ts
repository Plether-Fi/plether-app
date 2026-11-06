import { defineConfig, devices } from '@playwright/test'

const port = process.env.E2E_PORT ?? '5189'
const baseURL = `http://127.0.0.1:${port}`

// Actual Privy UI and Wagmi bridge; the public config and external services are
// intercepted by the fixture. No live email, wallet keys, RPC, or app is needed.
export default defineConfig({
  testDir: './tests',
  testMatch: 'privy-sdk.spec.ts',
  outputDir: '../.test-results/privy-sdk',
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  failOnFlakyTests: true,
  workers: 1,
  retries: 0,
  reporter: [['list'], ['junit', { outputFile: '.test-results/privy-sdk.xml' }]],
  use: { baseURL, trace: 'retain-on-failure' },
  projects: [{ name: 'privy-sdk-chromium', use: { ...devices['Desktop Chrome'] } }],
  webServer: {
    command: `npm run dev -- --host 127.0.0.1 --port ${port} --strictPort --force`,
    url: baseURL,
    reuseExistingServer: false,
    timeout: 120_000,
    env: {
      VITE_PRIVY_APP_ID: 'c000000000000000000000000',
      VITE_PRIVY_CLIENT_ID: '',
      VITE_DEFAULT_CHAIN_ID: '421614',
      VITE_POSTHOG_KEY: '',
    },
  },
})
