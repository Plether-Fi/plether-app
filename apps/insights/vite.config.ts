/// <reference types="vitest/config" />
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'

const apiTarget = process.env.VITE_API_PROXY_TARGET ?? 'http://127.0.0.1:3001'
const registrationApiTarget = process.env.VITE_REGISTRATION_API_PROXY_TARGET ?? 'http://127.0.0.1:3003'

export default defineConfig({
  plugins: [react()],
  server: {
    proxy: {
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
    environment: 'happy-dom',
    include: ['src/**/*.test.{ts,tsx}'],
    setupFiles: ['./src/test/setup.ts'],
    globals: true,
  },
})
