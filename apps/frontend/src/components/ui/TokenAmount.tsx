import { type ReactNode } from 'react'
import { TokenLabel } from './TokenLabel'

interface TokenAmountProps {
  amount: ReactNode
  token?: string
  className?: string
  amountClassName?: string
  wrap?: boolean
}

export function TokenAmount({
  amount,
  token = 'USDC',
  className = '',
  amountClassName = '',
  wrap = false,
}: TokenAmountProps) {
  return (
    <span className={`inline-flex items-baseline gap-1 sm:gap-1.5 ${wrap ? 'max-w-full flex-wrap' : 'whitespace-nowrap'} ${className}`}>
      <span className={`${wrap ? 'whitespace-nowrap' : ''} ${amountClassName}`}>{amount}</span>
      <TokenLabel token={token} />
    </span>
  )
}
