import '@testing-library/jest-dom/vitest'
import { beforeAll, afterAll, beforeEach } from 'vitest'
import { publicClient, testClient } from './anvil'
import { frontendIntegrationState, integrationPrerequisitesAvailable } from './integrationPrerequisites'

let snapshotId: `0x${string}` | undefined

beforeAll(async () => {
  try {
    const chainId = await publicClient.getChainId()
    if (chainId !== 31337) throw new Error(`Expected local Anvil chain ID 31337, received ${chainId}`)
    // Take a snapshot of the initial Anvil state
    snapshotId = await testClient.snapshot()
    frontendIntegrationState.anvilAvailable = true
  } catch (error) {
    frontendIntegrationState.anvilAvailable = false
    if (!integrationPrerequisitesAvailable('FRONTEND_INTEGRATION_REQUIRED', 'Spot integration', [{
      available: false,
      requirement: 'a snapshot-capable local Anvil Sepolia fork at http://127.0.0.1:8546 (chain ID 31337)',
    }], error)) {
      console.warn('Skipping optional spot integration: local Anvil is unavailable.', error)
    }
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
  if (snapshotId !== undefined) await testClient.reset()
})
