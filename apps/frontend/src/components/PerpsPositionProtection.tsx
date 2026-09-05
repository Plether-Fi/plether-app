import { useId, useState } from 'react'
import { useAccount, usePublicClient, useWriteContract } from 'wagmi'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { PERPS_POSITION_PROTECTION_BOOK_ABI } from '../contracts/abis'
import { POSITION_PROTECTION_STATUS, POSITION_PROTECTION_STATUS_LABELS, positionProtectionMessage } from '../contracts/perpsProtection'
import { getPerpsErrorMessage } from '../utils/perpsErrors'
import { verifyProtectionRetryBindings } from '../contracts/verifyPerpsV2Bindings'
import { formatDisplayDxyPrice } from '../utils/perps'

interface ProtectionTriggerPrices {
  takeProfitTriggerPrice?: bigint
  stopLossTriggerPrice?: bigint
}

function formatTriggerPrice(price: bigint | undefined): string {
  return price === 0n ? 'Not set' : formatDisplayDxyPrice(price)
}

export function PerpsPositionProtection({ id, status, linkedOrderId, takeProfitTriggerPrice, stopLossTriggerPrice, onRefresh }: ProtectionTriggerPrices & {
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
      takeProfitTriggerPrice={takeProfitTriggerPrice}
      stopLossTriggerPrice={stopLossTriggerPrice}
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
  takeProfitTriggerPrice, stopLossTriggerPrice,
  canRetry = false, walletOnNetwork = false, onRetry,
}: ProtectionTriggerPrices & {
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
  const [expanded, setExpanded] = useState(false)
  const detailsId = useId()
  if (id === 0n) return null
  const latched = status === POSITION_PROTECTION_STATUS.Latched
  const statusLabel = POSITION_PROTECTION_STATUS_LABELS[status] ?? 'Unknown'
  return (
    <section aria-label="Position protection" className="mb-4 border-b border-brand-border/20 pb-3 text-sm">
      <div className="flex flex-wrap items-center justify-between gap-x-4 gap-y-1">
        <div className="flex flex-wrap items-center gap-x-5 gap-y-1">
          <p className="text-content-secondary">SL/TP <span className={latched ? 'ml-2 text-[#FFAB96]' : 'ml-2 text-content-primary'}>{statusLabel}</span></p>
          <dl className="flex flex-wrap gap-x-4 gap-y-1 tabular-nums">
            <div className="flex gap-1.5" title="Stop-loss trigger price">
              <dt className="text-content-secondary">SL</dt>
              <dd aria-label="Stop-loss trigger price" className="text-content-primary">{formatTriggerPrice(stopLossTriggerPrice)}</dd>
            </div>
            <div className="flex gap-1.5" title="Take-profit trigger price">
              <dt className="text-content-secondary">TP</dt>
              <dd aria-label="Take-profit trigger price" className="text-content-primary">{formatTriggerPrice(takeProfitTriggerPrice)}</dd>
            </div>
          </dl>
        </div>
        <button type="button" aria-expanded={expanded} aria-controls={detailsId}
          className="cursor-pointer text-xs text-content-secondary underline underline-offset-4 hover:text-content-primary"
          onClick={() => { setExpanded(!expanded) }}>
          {expanded ? 'Hide details' : latched ? 'Details & retry' : 'Details'}
        </button>
      </div>
      <div id={detailsId} hidden={!expanded} className="mt-3 space-y-2 text-xs leading-5 text-content-secondary">
        <p>{positionProtectionMessage(id, status)}</p>
        {linkedOrderId > 0n && <p className="mt-2">Latest close attempt: #{linkedOrderId.toString()}</p>}
        {status === POSITION_PROTECTION_STATUS.Latched && <>
          <p className="mt-2">Keepers retry expired attempts when execution is available. You can also queue a new attempt using your wallet; you pay network gas. Execution price and timing are not guaranteed.</p>
          <button type="button" className="mt-3 cursor-pointer text-sm text-[#FFAB96] underline underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50" onClick={onRetry}
            disabled={pending || !canRetry}>
            {pending ? 'Queuing close attempt…' : 'Retry protection close'}
          </button>
          {!walletOnNetwork && <p>Connect your wallet to Arbitrum Sepolia to retry.</p>}
        </>}
        {queuedOrderId !== undefined && <p role="status">Close attempt #{queuedOrderId.toString()} queued.</p>}
        {error && <p role="alert" className="mt-2 text-warning">{error}</p>}
      </div>
    </section>
  )
}
