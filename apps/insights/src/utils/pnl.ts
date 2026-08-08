import type { WalletDetails } from '../api'

export interface PnlBreakdown {
  realized: string
  unrealized: string
  directional: string
  costsAndAdjustments: string
  net: string
}

function parseIntegerUnits(value: string | null | undefined): bigint | null {
  if (value == null || !/^-?\d+$/.test(value)) return null
  try {
    return BigInt(value)
  } catch {
    return null
  }
}

export function calculatePnlBreakdown(wallet: WalletDetails): PnlBreakdown | null {
  const realized = parseIntegerUnits(wallet.realizedPnl)
  const unrealized = parseIntegerUnits(wallet.position?.unrealizedPnl ?? '0')
  const net = parseIntegerUnits(wallet.pnl)
  if (realized === null || unrealized === null || net === null) return null

  const directional = realized + unrealized
  return {
    realized: realized.toString(),
    unrealized: unrealized.toString(),
    directional: directional.toString(),
    costsAndAdjustments: (net - directional).toString(),
    net: net.toString(),
  }
}
