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
      <div className="flex min-w-0 flex-col gap-1 text-sm min-[400px]:flex-row min-[400px]:items-center min-[400px]:justify-between min-[400px]:gap-3">
        <span className="text-content-secondary">{label}</span>
        <span className={`min-w-0 break-words font-medium min-[400px]:text-right ${variantColors[variant]}`}>
          {value} {token}
        </span>
      </div>
    </div>
  )
}
