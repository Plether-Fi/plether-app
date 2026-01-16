import { createConfig, http } from 'wagmi'
import { foundry } from 'wagmi/chains'
import { mock } from 'wagmi/connectors'

export const ANVIL_RPC_URL = 'http://127.0.0.1:8546'

// Anvil's default test accounts (each has 10000 ETH)
export const TEST_ACCOUNTS = [
  '0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266',
  '0x70997970C51812dc3A010C7d01b50e0d17dc79C8',
  '0x3C44CdDdB6a900fa2b585dd299e03d12FA4293BC',
  '0x90F79bf6EB2c4f870365E785982E1f101E93b906',
  '0x15d34AAf54267DB7D7c367839AAf71A00a2C6A65',
] as const

export const testConfig = createConfig({
  chains: [foundry],
  connectors: [
    mock({
      accounts: [...TEST_ACCOUNTS],
    }),
  ],
  transports: {
    [foundry.id]: http(ANVIL_RPC_URL),
  },
})
