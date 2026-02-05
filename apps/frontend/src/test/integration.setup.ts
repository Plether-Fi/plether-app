import '@testing-library/jest-dom/vitest'
import { beforeAll, afterAll, beforeEach } from 'vitest'
import { testClient } from './anvil'

let snapshotId: `0x${string}`

beforeAll(async () => {
  try {
    // Take a snapshot of the initial Anvil state
    snapshotId = await testClient.snapshot()
  } catch (error) {
    console.error('Failed to connect to Anvil. Make sure Anvil is running:')
    console.error('  npm run anvil')
    console.error('  (port 8546, chain ID 31338)')
    throw error
  }
})

beforeEach(async () => {
  // Revert to snapshot before each test for isolation
  if (snapshotId) {
    await testClient.revert({ id: snapshotId })
    // Take a new snapshot for the next test
    snapshotId = await testClient.snapshot()
  }
})

afterAll(async () => {
  // Reset Anvil to original fork state
  await testClient.reset()
})
