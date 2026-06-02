import { type ReactNode } from 'react'
import { TokenLabel } from './TokenLabel'

interface TokenAmountProps {
  amount: ReactNode
  token?: string
  className?: string
  amountClassName?: string
}

export function TokenAmount({
  amount,
  token = 'USDC',
  className = '',
  amountClassName = '',
}: TokenAmountProps) {
  return (
    <span className={`inline-flex items-baseline gap-1.5 ${className}`}>
      <span className={amountClassName}>{amount}</span>
      <TokenLabel token={token} />
    </span>
  )
}
