import { useEffect, useState } from 'react'
import { usePublicClient } from 'wagmi'
import { PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { deriveCloseMarginReleased } from '../utils/perpsCloseMargin'
import type { PerpsOrderHistoryRow } from './usePerpsHistory'

export function usePerpsCloseMargin(order: PerpsOrderHistoryRow | undefined): bigint | undefined {
  const client = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const [result, setResult] = useState<{ key: string; amount: bigint | undefined }>()
  const key = order === undefined ? undefined
    : [order.orderId.toString(), order.revealTxHash, order.terminalBlockHash, order.receiptHash].join(':')

  useEffect(() => {
    if (!order?.revealTxHash || !key) return
    let active = true
    void client.getTransactionReceipt({ hash: order.revealTxHash }).then(transaction => {
      const amount = deriveCloseMarginReleased(transaction, order)
      if (active) setResult({ key, amount })
    }).catch(() => {
      if (active) setResult({ key, amount: undefined })
    })
    return () => { active = false }
  }, [client, key, order])

  return result?.key === key ? result?.amount : undefined
}
