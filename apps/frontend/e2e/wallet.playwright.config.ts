import { defineConfig, devices } from '@playwright/test'

// Provider contract checks run on about:blank; no app server or network is used.
export default defineConfig({
  testDir: './tests',
  outputDir: '../test-results/wallet-provider',
  testMatch: 'wallet-provider.spec.ts',
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  reporter: 'list',
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
})
