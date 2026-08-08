interface TokenLabelProps {
  token: string
  className?: string
}

export function TokenLabel({ token, className = '' }: TokenLabelProps) {
  return (
    <span className={`inline-flex max-w-full shrink-0 items-center justify-center whitespace-nowrap border border-content-secondary/50 px-1.5 py-0.5 align-middle font-mono text-xs leading-none text-content-secondary ${className}`}>
      {token}
    </span>
  )
}
