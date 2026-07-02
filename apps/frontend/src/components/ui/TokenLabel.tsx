interface TokenLabelProps {
  token: string
  className?: string
}

export function TokenLabel({ token, className = '' }: TokenLabelProps) {
  return (
    <span className={`inline-block max-w-full overflow-hidden text-ellipsis whitespace-nowrap border border-content-secondary/50 px-1.5 py-0.5 align-baseline font-mono text-xs text-content-secondary ${className}`}>
      {token}
    </span>
  )
}
