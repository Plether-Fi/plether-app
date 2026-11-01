import { defineConfig, devices } from '@playwright/test'

const port = process.env.E2E_PORT ?? '5187'
const baseURL = `http://127.0.0.1:${port}`

export default defineConfig({
  testDir: './tests',
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  failOnFlakyTests: !!process.env.CI,
  workers: process.env.CI ? 1 : undefined,
  outputDir: '.test-results/e2e',
  reporter: [
    ['list'],
    ['html', { outputFolder: '.test-results/e2e-report', open: 'never' }],
    ['junit', { outputFile: '.test-results/e2e.xml' }],
  ],
  use: {
    baseURL,
    trace: 'on-first-retry',
  },
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
  ],
  webServer: {
    command: `npm run dev -- --host 127.0.0.1 --port ${port} --strictPort`,
    url: baseURL,
    reuseExistingServer: false,
    timeout: 120 * 1000,
  },
})
