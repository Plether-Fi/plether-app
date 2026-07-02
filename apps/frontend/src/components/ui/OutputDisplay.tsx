type TokenVariant = 'BEAR' | 'BULL' | 'neutral'

interface OutputDisplayProps {
  label: string
  value: string
  token: string
  variant?: TokenVariant
}

const variantColors: Record<TokenVariant, string> = {
  BEAR: 'text-brand-orange',
  BULL: 'text-positive',
  neutral: 'text-content-primary',
}

export function OutputDisplay({ label, value, token, variant = 'neutral' }: OutputDisplayProps) {
  return (
    <div className="bg-surface-muted p-3 border border-brand-border/30">
      <div className="flex justify-between items-center text-sm">
        <span className="text-content-secondary">{label}</span>
        <span className={`font-medium ${variantColors[variant]}`}>
          {value} {token}
        </span>
      </div>
    </div>
  )
}
