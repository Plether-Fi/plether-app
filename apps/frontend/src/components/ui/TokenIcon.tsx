type TokenSide = 'BEAR' | 'BULL' | 'USDC'
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

const colorStyles: Record<TokenSide, string> = {
  BEAR: 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia',
  BULL: 'bg-cyber-neon-green/20 text-cyber-neon-green',
  USDC: 'bg-cyber-bright-blue/20 text-cyber-bright-blue',
}

const labels: Record<TokenSide, string> = {
  BEAR: 'BR',
  BULL: 'BL',
  USDC: '$',
}

export function TokenIcon({ side, size = 'md' }: TokenIconProps) {
  return (
    <div
      className={`
         flex items-center justify-center font-bold
        ${sizeStyles[size]}
        ${colorStyles[side]}
      `}
    >
      {labels[side]}
    </div>
  )
}
