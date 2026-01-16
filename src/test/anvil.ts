import {
  createTestClient,
  createPublicClient,
  createWalletClient,
  http,
  parseEther,
  parseUnits,
  type Address,
  type Hash,
} from 'viem'
import { foundry } from 'viem/chains'
import { ERC20_ABI } from '../contracts/abis'
import { ANVIL_RPC_URL, TEST_ACCOUNTS } from './wagmi'

export const testClient = createTestClient({
  chain: foundry,
  mode: 'anvil',
  transport: http(ANVIL_RPC_URL),
})

export const publicClient = createPublicClient({
  chain: foundry,
  transport: http(ANVIL_RPC_URL),
})

export const walletClient = createWalletClient({
  chain: foundry,
  transport: http(ANVIL_RPC_URL),
})

export async function resetAnvil() {
  await testClient.reset()
}

export async function mineBlock() {
  await testClient.mine({ blocks: 1 })
}

export async function setBalance(address: Address, value: bigint) {
  await testClient.setBalance({ address, value })
}

export async function impersonateAccount(address: Address) {
  await testClient.impersonateAccount({ address })
}

export async function stopImpersonating(address: Address) {
  await testClient.stopImpersonatingAccount({ address })
}

export async function getERC20Balance(tokenAddress: Address, account: Address): Promise<bigint> {
  return publicClient.readContract({
    address: tokenAddress,
    abi: ERC20_ABI,
    functionName: 'balanceOf',
    args: [account],
  })
}

export async function approveERC20(
  tokenAddress: Address,
  spender: Address,
  amount: bigint,
  account: Address
): Promise<Hash> {
  await impersonateAccount(account)
  const hash = await walletClient.writeContract({
    address: tokenAddress,
    abi: ERC20_ABI,
    functionName: 'approve',
    args: [spender, amount],
    account,
    chain: foundry,
  })
  await stopImpersonating(account)
  return hash
}

export async function dealERC20(
  tokenAddress: Address,
  to: Address,
  amount: bigint,
  whaleAddress: Address
): Promise<void> {
  await impersonateAccount(whaleAddress)
  await walletClient.writeContract({
    address: tokenAddress,
    abi: ERC20_ABI,
    functionName: 'transfer',
    args: [to, amount],
    account: whaleAddress,
    chain: foundry,
  })
  await stopImpersonating(whaleAddress)
}

export async function waitForTransaction(hash: Hash) {
  return publicClient.waitForTransactionReceipt({ hash })
}

export { TEST_ACCOUNTS, parseEther, parseUnits }
