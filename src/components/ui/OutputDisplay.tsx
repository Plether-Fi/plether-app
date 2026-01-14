type TokenVariant = 'BEAR' | 'BULL' | 'neutral'

interface OutputDisplayProps {
  label: string
  value: string
  token: string
  variant?: TokenVariant
}

const variantColors: Record<TokenVariant, string> = {
  BEAR: 'text-cyber-electric-fuchsia',
  BULL: 'text-cyber-neon-green',
  neutral: 'text-cyber-text-primary',
}

export function OutputDisplay({ label, value, token, variant = 'neutral' }: OutputDisplayProps) {
  return (
    <div className="bg-cyber-surface-light p-3 border border-cyber-border-glow/30">
      <div className="flex justify-between items-center text-sm">
        <span className="text-cyber-text-secondary">{label}</span>
        <span className={`font-medium ${variantColors[variant]}`}>
          {value} {token}
        </span>
      </div>
    </div>
  )
}
