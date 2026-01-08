type TokenSide = 'BEAR' | 'BULL'
type TokenIconSize = 'sm' | 'md' | 'lg'

interface TokenIconProps {
  side: TokenSide
  size?: TokenIconSize
}

const sizeStyles: Record<TokenIconSize, string> = {
  sm: 'w-8 h-8 text-xs',
  md: 'w-10 h-10 text-sm',
  lg: 'w-12 h-12 text-base',
}

export function TokenIcon({ side, size = 'md' }: TokenIconProps) {
  const isBear = side === 'BEAR'
  const colorStyles = isBear
    ? 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia shadow-cyber-electric-fuchsia/10'
    : 'bg-cyber-neon-green/20 text-cyber-neon-green shadow-cyber-neon-green/10'

  return (
    <div
      className={`
         flex items-center justify-center font-bold shadow-md
        ${sizeStyles[size]}
        ${colorStyles}
      `}
    >
      {isBear ? 'BR' : 'BL'}
    </div>
  )
}
