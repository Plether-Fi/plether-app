interface TokenLabelProps {
  token: string
}

export function TokenLabel({ token }: TokenLabelProps) {
  return (
    <span className="font-mono text-xs px-1.5 py-0.5 border border-content-secondary/50 text-content-secondary">
      {token}
    </span>
  )
}
