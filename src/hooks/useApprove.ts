import { useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { type Address } from 'viem'
import { ERC20_ABI } from '../contracts/abis'
import { useTransactionStore } from '../stores/transactionStore'

export function useApprove(tokenAddress: Address, spenderAddress: Address) {
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess } = useWaitForTransactionReceipt({
    hash,
  })

  const approve = async (amount: bigint) => {
    const txId = crypto.randomUUID()
    addTransaction({
      id: txId,
      type: 'approve',
      status: 'pending',
      hash: undefined,
      description: 'Approving token spend',
    })

    try {
      writeContract(
        {
          address: tokenAddress,
          abi: ERC20_ABI,
          functionName: 'approve',
          args: [spenderAddress, amount],
        },
        {
          onSuccess: (hash) => {
            updateTransaction(txId, { hash, status: 'confirming' })
          },
          onError: () => {
            updateTransaction(txId, { status: 'failed' })
          },
        }
      )
    } catch {
      updateTransaction(txId, { status: 'failed' })
    }
  }

  return {
    approve,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
  }
}
