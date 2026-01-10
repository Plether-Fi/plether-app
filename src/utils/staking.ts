import { parseUnits, formatUnits } from 'viem'

export const ASSET_DECIMALS = 18
export const SHARE_DECIMALS = 21 // 18 + 3 offset for inflation attack protection

export function getStakingDecimals(mode: 'stake' | 'unstake'): number {
  return mode === 'stake' ? ASSET_DECIMALS : SHARE_DECIMALS
}

export function parseStakingAmount(amount: string, mode: 'stake' | 'unstake'): bigint {
  if (!amount || isNaN(parseFloat(amount))) return 0n
  const decimals = getStakingDecimals(mode)
  try {
    return parseUnits(amount, decimals)
  } catch {
    return 0n
  }
}

export function formatStakingBalance(balance: bigint, mode: 'stake' | 'unstake'): string {
  const decimals = getStakingDecimals(mode)
  return formatUnits(balance, decimals)
}

export function isInsufficientStakingBalance(
  amount: string,
  mode: 'stake' | 'unstake',
  tokenBalance: bigint,
  stakedBalance: bigint
): boolean {
  const amountBigInt = parseStakingAmount(amount, mode)
  if (amountBigInt === 0n) return false
  const balance = mode === 'stake' ? tokenBalance : stakedBalance
  return amountBigInt > balance
}
