import { defineConfig } from 'vitest/config'
export default defineConfig({ test: {
  environment: 'happy-dom', setupFiles: ['src/test/setup.ts'], include: ['test/local-order-v3/*.test.ts'],
  testTimeout: 120_000, hookTimeout: 120_000, fileParallelism: false,
} })
