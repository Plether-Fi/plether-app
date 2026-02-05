interface TokenLabelProps {
  token: string
}

export function TokenLabel({ token }: TokenLabelProps) {
  return (
    <span className="font-mono text-xs px-1.5 py-0.5 border border-cyber-text-secondary/50 text-cyber-text-secondary">
      {token}
    </span>
  )
}
