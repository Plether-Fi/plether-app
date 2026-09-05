import { useState } from 'react'
import { useAccount, usePublicClient, useWriteContract } from 'wagmi'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { PERPS_POSITION_PROTECTION_BOOK_ABI } from '../contracts/abis'
import { POSITION_PROTECTION_STATUS, positionProtectionMessage } from '../contracts/perpsProtection'
import { getPerpsErrorMessage } from '../utils/perpsErrors'
import { verifyProtectionRetryBindings } from '../contracts/verifyPerpsV2Bindings'

export function PerpsPositionProtection({ id, status, linkedOrderId, onRefresh }: {
  id: bigint
  status: number
  linkedOrderId: bigint
  onRefresh: () => Promise<void>
}) {
  const { address, chainId } = useAccount()
  const client = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const { writeContractAsync } = useWriteContract()
  const [pending, setPending] = useState(false)
  const [error, setError] = useState<string>()
  const [queuedOrderId, setQueuedOrderId] = useState<bigint>()
  if (id === 0n) return null

  async function retry() {
    if (!address) return
    setPending(true)
    setError(undefined)
    try {
      const blockNumber = await client.getBlockNumber()
      await verifyProtectionRetryBindings(client, blockNumber)
      await client.simulateContract({
        address: PERPS_ARBITRUM_SEPOLIA.positionProtectionBook,
        abi: PERPS_POSITION_PROTECTION_BOOK_ABI,
        functionName: 'retryPositionProtectionClose', args: [id], account: address,
      })
      const hash = await writeContractAsync({
        address: PERPS_ARBITRUM_SEPOLIA.positionProtectionBook,
        abi: PERPS_POSITION_PROTECTION_BOOK_ABI,
        functionName: 'retryPositionProtectionClose', args: [id],
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      })
      const receipt = await client.waitForTransactionReceipt({ hash })
      if (receipt.status !== 'success') throw new Error('The retry transaction reverted. Refresh the protection before trying again.')
      const protection = await client.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.positionProtectionBook,
        abi: PERPS_POSITION_PROTECTION_BOOK_ABI,
        functionName: 'getPositionProtection', args: [id],
      })
      setQueuedOrderId(protection.linkedOrderId)
      await onRefresh()
    } catch (cause) {
      setError(getPerpsErrorMessage(cause, 'retryProtection'))
      void onRefresh()
    } finally {
      setPending(false)
    }
  }

  return (
    <PerpsPositionProtectionPanel
      id={id}
      status={status}
      linkedOrderId={linkedOrderId}
      pending={pending}
      error={error}
      queuedOrderId={queuedOrderId}
      canRetry={Boolean(address) && chainId === PERPS_ARBITRUM_SEPOLIA_CHAIN_ID}
      walletOnNetwork={chainId === PERPS_ARBITRUM_SEPOLIA_CHAIN_ID}
      onRetry={() => void retry()}
    />
  )
}

export function PerpsPositionProtectionPanel({
  id, status, linkedOrderId, pending = false, error, queuedOrderId,
  canRetry = false, walletOnNetwork = false, onRetry,
}: {
  id: bigint
  status: number
  linkedOrderId: bigint
  pending?: boolean
  error?: string
  queuedOrderId?: bigint
  canRetry?: boolean
  walletOnNetwork?: boolean
  onRetry: () => void
}) {
  if (id === 0n) return null
  return (
    <section aria-label="Position protection" className="mb-4 rounded-lg border border-border p-4 text-sm">
      <p>{positionProtectionMessage(id, status)}</p>
      {linkedOrderId > 0n && <p className="mt-2">Latest close attempt: #{linkedOrderId.toString()}</p>}
      {status === POSITION_PROTECTION_STATUS.Latched && <>
        <p className="mt-2">Keepers retry expired attempts when execution is available. You can also queue a new attempt using your wallet; you pay network gas. Execution price and timing are not guaranteed.</p>
        <button type="button" className="mt-3 underline" onClick={onRetry}
          disabled={pending || !canRetry}>
          {pending ? 'Queuing close attempt…' : 'Retry protection close'}
        </button>
        {!walletOnNetwork && <p>Connect your wallet to Arbitrum Sepolia to retry.</p>}
      </>}
      {queuedOrderId !== undefined && <p role="status">Close attempt #{queuedOrderId.toString()} queued.</p>}
      {error && <p role="alert" className="mt-2 text-warning">{error}</p>}
    </section>
  )
}
