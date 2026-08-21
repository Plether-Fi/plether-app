import { useCallback } from 'react'
import { type Address } from 'viem'
import { getAccount } from '@wagmi/core'
import { useConfig, usePublicClient, useWriteContract } from 'wagmi'
import { ERC20_ABI, TRANCHE_VAULT_READ_ABI } from '../contracts/abis'
import {
  PERPS_ARBITRUM_SEPOLIA,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
} from '../contracts/perpsAddresses'
import { useTransactionSequence, type TransactionStep } from './useTransactionSequence'

interface UseVaultTransactionsOptions {
  vaultAddress: Address
  allowance?: bigint
  onSuccess?: () => void
}

export function useVaultTransactions({
  vaultAddress,
  allowance,
  onSuccess,
}: UseVaultTransactionsOptions) {
  const config = useConfig()
  const publicClient = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const { writeContractAsync } = useWriteContract()
  const sequence = useTransactionSequence()

  const requireTransactionContext = useCallback((expectedAddress: Address | undefined) => {
    const currentAccount = getAccount(config)
    if (!currentAccount.address) {
      throw new Error('Connect a wallet before submitting a vault transaction.')
    }
    if (expectedAddress?.toLowerCase() !== currentAccount.address.toLowerCase()) {
      throw new Error('The connected wallet account changed. Restart the vault action.')
    }
    if (currentAccount.chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID) {
      throw new Error('Switch to Arbitrum Sepolia before submitting a vault transaction.')
    }
    return { address: currentAccount.address, publicClient }
  }, [config, publicClient])

  const deposit = useCallback((amount: bigint) => {
    const expectedAddress = getAccount(config).address
    void sequence.execute({
      title: 'Depositing USDC into Plether Vault',
      type: 'supply',
      buildSteps: (): TransactionStep[] => {
        const steps: TransactionStep[] = []

        if (allowance === undefined || allowance < amount) {
          steps.push({
            label: 'Approve USDC',
            action: async () => {
              const context = requireTransactionContext(expectedAddress)
              await context.publicClient.simulateContract({
                account: context.address,
                address: PERPS_ARBITRUM_SEPOLIA.usdc,
                abi: ERC20_ABI,
                functionName: 'approve',
                args: [vaultAddress, amount],
              })
              return writeContractAsync({
                account: context.address,
                chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
                address: PERPS_ARBITRUM_SEPOLIA.usdc,
                abi: ERC20_ABI,
                functionName: 'approve',
                args: [vaultAddress, amount],
              })
            },
          })
        }

        steps.push({
          label: 'Deposit USDC',
          action: async () => {
            const context = requireTransactionContext(expectedAddress)
            await context.publicClient.simulateContract({
              account: context.address,
              address: vaultAddress,
              abi: TRANCHE_VAULT_READ_ABI,
              functionName: 'deposit',
              args: [amount, context.address],
            })
            return writeContractAsync({
              account: context.address,
              chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
              address: vaultAddress,
              abi: TRANCHE_VAULT_READ_ABI,
              functionName: 'deposit',
              args: [amount, context.address],
            })
          },
        })

        return steps
      },
      onSuccess,
    })
  }, [allowance, config, onSuccess, requireTransactionContext, sequence, vaultAddress, writeContractAsync])

  const requestDeposit = useCallback((amount: bigint) => {
    const expectedAddress = getAccount(config).address
    void sequence.execute({
      title: 'Queueing USDC for Plether Vault',
      type: 'supply',
      buildSteps: (): TransactionStep[] => {
        const steps: TransactionStep[] = []

        if (allowance === undefined || allowance < amount) {
          steps.push({
            label: 'Approve USDC',
            action: async () => {
              const context = requireTransactionContext(expectedAddress)
              await context.publicClient.simulateContract({
                account: context.address,
                address: PERPS_ARBITRUM_SEPOLIA.usdc,
                abi: ERC20_ABI,
                functionName: 'approve',
                args: [vaultAddress, amount],
              })
              return writeContractAsync({
                account: context.address,
                chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
                address: PERPS_ARBITRUM_SEPOLIA.usdc,
                abi: ERC20_ABI,
                functionName: 'approve',
                args: [vaultAddress, amount],
              })
            },
          })
        }

        steps.push({
          label: 'Queue deposit',
          action: async () => {
            const context = requireTransactionContext(expectedAddress)
            await context.publicClient.simulateContract({
              account: context.address,
              address: vaultAddress,
              abi: TRANCHE_VAULT_READ_ABI,
              functionName: 'requestDeposit',
              args: [amount, context.address],
            })
            return writeContractAsync({
              account: context.address,
              chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
              address: vaultAddress,
              abi: TRANCHE_VAULT_READ_ABI,
              functionName: 'requestDeposit',
              args: [amount, context.address],
            })
          },
        })

        return steps
      },
      onSuccess,
    })
  }, [allowance, config, onSuccess, requireTransactionContext, sequence, vaultAddress, writeContractAsync])

  const withdraw = useCallback((amount: bigint) => {
    const expectedAddress = getAccount(config).address
    void sequence.execute({
      title: 'Withdrawing USDC from Plether Vault',
      type: 'withdraw',
      buildSteps: (): TransactionStep[] => [{
        label: 'Withdraw USDC',
        action: async () => {
          const context = requireTransactionContext(expectedAddress)
          await context.publicClient.simulateContract({
            account: context.address,
            address: vaultAddress,
            abi: TRANCHE_VAULT_READ_ABI,
            functionName: 'withdraw',
            args: [amount, context.address, context.address],
          })
          return writeContractAsync({
            account: context.address,
            chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
            address: vaultAddress,
            abi: TRANCHE_VAULT_READ_ABI,
            functionName: 'withdraw',
            args: [amount, context.address, context.address],
          })
        },
      }],
      onSuccess,
    })
  }, [config, onSuccess, requireTransactionContext, sequence, vaultAddress, writeContractAsync])

  const cancelPendingDeposit = useCallback((epochId: bigint) => {
    const expectedAddress = getAccount(config).address
    void sequence.execute({
      title: 'Cancelling queued vault deposit',
      type: 'withdraw',
      buildSteps: (): TransactionStep[] => [{
        label: 'Cancel request',
        action: async () => {
          const context = requireTransactionContext(expectedAddress)
          await context.publicClient.simulateContract({
            account: context.address,
            address: vaultAddress,
            abi: TRANCHE_VAULT_READ_ABI,
            functionName: 'cancelPendingDeposit',
            args: [epochId],
          })
          return writeContractAsync({
            account: context.address,
            chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
            address: vaultAddress,
            abi: TRANCHE_VAULT_READ_ABI,
            functionName: 'cancelPendingDeposit',
            args: [epochId],
          })
        },
      }],
      onSuccess,
    })
  }, [config, onSuccess, requireTransactionContext, sequence, vaultAddress, writeContractAsync])

  const finalizeDepositEpoch = useCallback((epochId: bigint) => {
    const expectedAddress = getAccount(config).address
    void sequence.execute({
      title: 'Finalizing vault deposit epoch',
      type: 'supply',
      buildSteps: (): TransactionStep[] => [{
        label: 'Finalize epoch',
        action: async () => {
          const context = requireTransactionContext(expectedAddress)
          await context.publicClient.simulateContract({
            account: context.address,
            address: vaultAddress,
            abi: TRANCHE_VAULT_READ_ABI,
            functionName: 'finalizeDepositEpoch',
            args: [epochId],
          })
          return writeContractAsync({
            account: context.address,
            chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
            address: vaultAddress,
            abi: TRANCHE_VAULT_READ_ABI,
            functionName: 'finalizeDepositEpoch',
            args: [epochId],
          })
        },
      }],
      onSuccess,
    })
  }, [config, onSuccess, requireTransactionContext, sequence, vaultAddress, writeContractAsync])

  const claimDepositShares = useCallback((epochId: bigint) => {
    const expectedAddress = getAccount(config).address
    void sequence.execute({
      title: 'Claiming Plether Vault shares',
      type: 'supply',
      buildSteps: (): TransactionStep[] => [{
        label: 'Claim shares',
        action: async () => {
          const context = requireTransactionContext(expectedAddress)
          await context.publicClient.simulateContract({
            account: context.address,
            address: vaultAddress,
            abi: TRANCHE_VAULT_READ_ABI,
            functionName: 'claimDepositShares',
            args: [epochId],
          })
          return writeContractAsync({
            account: context.address,
            chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
            address: vaultAddress,
            abi: TRANCHE_VAULT_READ_ABI,
            functionName: 'claimDepositShares',
            args: [epochId],
          })
        },
      }],
      onSuccess,
    })
  }, [config, onSuccess, requireTransactionContext, sequence, vaultAddress, writeContractAsync])

  return {
    deposit,
    requestDeposit,
    withdraw,
    cancelPendingDeposit,
    finalizeDepositEpoch,
    claimDepositShares,
    isRunning: sequence.isRunning,
    isSuccess: sequence.isSuccess,
    isError: sequence.isError,
    error: sequence.error,
    reset: sequence.reset,
  }
}
