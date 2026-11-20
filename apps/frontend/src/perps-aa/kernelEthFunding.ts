import type { PerpsActionPlan } from '@plether-fi/perps-aa-client'
import { encodeFunctionData, isAddressEqual, parseAbi, parseEther, type Address, type Hex, type PublicClient, type WalletClient } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import { PERPS_ARBITRUM_SEPOLIA } from '../contracts/perpsAddresses'
import { PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI } from '../contracts/abis'
import type { KernelPerpsAaSmartAccountRuntime } from './runtimeContext'

const entryPointDepositAbi = parseAbi([
  'function balanceOf(address account) view returns (uint256)',
  'function withdrawTo(address payable withdrawAddress, uint256 withdrawAmount)',
])

export type KernelEthWithdrawalSource = 'account' | 'entrypoint'

export function parseKernelEthAmount(value: string): bigint | undefined {
  const amount = value.trim()
  if (!/^(?:\d{1,60}(?:\.\d{1,18})?|\.\d{1,18})$/.test(amount)) return undefined
  const wei = parseEther(amount.startsWith('.') ? `0${amount}` : amount)
  return wei > 0n && wei < 2n ** 256n ? wei : undefined
}

export async function verifyKernelEthRecipient(runtime: KernelPerpsAaSmartAccountRuntime) {
  const verified = await runtime.kernel.verifyAccount()
  if (!isAddressEqual(verified.address, runtime.smartAccount.accountAddress) ||
      !isAddressEqual(verified.ownerAddress, runtime.ownerAddress)) {
    throw new Error('The verified Trading Account does not match this wallet.')
  }
  return verified
}

export async function readKernelEthFunds(runtime: KernelPerpsAaSmartAccountRuntime, client: PublicClient) {
  if (client.chain?.id !== arbitrumSepolia.id || runtime.chainId !== arbitrumSepolia.id) {
    throw new Error('Use Arbitrum Sepolia to verify Trading Account funds.')
  }
  const { address, blockNumber } = await verifyKernelEthRecipient(runtime)
  const [account, entrypoint, margin] = await Promise.all([
    client.getBalance({ address, blockNumber }),
    client.readContract({ address: runtime.smartAccount.entryPoint, abi: entryPointDepositAbi, functionName: 'balanceOf', args: [address], blockNumber }),
    client.readContract({ address: PERPS_ARBITRUM_SEPOLIA.cfdEngineAccountLens, abi: PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI,
      functionName: 'getAccountLedgerSnapshot', args: [address], blockNumber }).catch(() => undefined),
  ])
  return { address, account, entrypoint, margin: margin?.settlementBalanceAssets }
}

export function buildKernelEthWithdrawal(runtime: KernelPerpsAaSmartAccountRuntime, source: KernelEthWithdrawalSource, amount: bigint): PerpsActionPlan {
  const owner = runtime.ownerAddress
  const { accountAddress, entryPoint } = runtime.smartAccount
  if (amount <= 0n) throw new Error('Enter an ETH amount above zero.')
  return {
    kind: 'withdraw-to-owner',
    account: accountAddress,
    calls: [source === 'account'
      ? { to: owner, value: amount, data: '0x' }
      : { to: entryPoint, value: 0n, data: encodeFunctionData({ abi: entryPointDepositAbi, functionName: 'withdrawTo', args: [owner, amount] }) }],
  }
}

export async function sendKernelEthTopUp(runtime: KernelPerpsAaSmartAccountRuntime, walletClient: WalletClient, amount: bigint) {
  if (amount <= 0n) throw new Error('Enter an ETH amount above zero.')
  const recipient = await verifyKernelEthRecipient(runtime)
  const [addresses, chainId] = await Promise.all([walletClient.getAddresses(), walletClient.getChainId()])
  const selected = addresses.at(0)
  if (chainId !== arbitrumSepolia.id || selected === undefined || !isAddressEqual(selected, recipient.ownerAddress)) {
    throw new Error('Connect the Owner Wallet on Arbitrum Sepolia to send ETH.')
  }
  return walletClient.sendTransaction({ account: recipient.ownerAddress, chain: arbitrumSepolia, to: recipient.address, value: amount })
}

export async function confirmKernelEthTopUp(client: PublicClient, hash: Hex, sender: Address, receiver: Address, value: bigint) {
  if (client.chain?.id !== arbitrumSepolia.id) throw new Error('Use Arbitrum Sepolia to confirm funding.')
  const receipt = await client.waitForTransactionReceipt({ hash, timeout: 60_000 })
  const [transfer, block] = await Promise.all([
    client.getTransaction({ hash: receipt.transactionHash }),
    client.getBlock({ blockNumber: receipt.blockNumber }),
  ])
  if (receipt.status !== 'success' || block.hash !== receipt.blockHash || transfer.blockHash !== receipt.blockHash ||
      transfer.hash !== receipt.transactionHash || transfer.blockNumber !== receipt.blockNumber ||
      !isAddressEqual(transfer.from, sender) || !transfer.to || !isAddressEqual(transfer.to, receiver) ||
      transfer.value !== value || transfer.input !== '0x') {
    throw new Error('The confirmed transaction does not match this ETH top-up. Check your wallet before retrying.')
  }
  return receipt.transactionHash
}
