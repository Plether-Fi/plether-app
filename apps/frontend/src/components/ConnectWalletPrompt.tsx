import { useAppKit } from '@reown/appkit/react-core'

export interface ConnectWalletPromptProps {
  description?: string
}

export function ConnectWalletPrompt({ description }: ConnectWalletPromptProps) {
  const { open } = useAppKit()

  return (
    <div className="bg-cyber-surface-dark p-12 text-center border border-cyber-border-glow/30 shadow-lg">
      <div className="w-16 h-16 mx-auto mb-4 rounded-full bg-cyber-surface-light flex items-center justify-center">
        <span className="material-symbols-outlined text-3xl text-cyber-text-secondary">lock</span>
      </div>
      <button
        onClick={() => void open()}
        className="text-xl font-semibold text-cyber-electric-fuchsia hover:text-cyber-electric-fuchsia/80 mb-2 cursor-pointer transition-colors"
      >
        Connect Your Wallet
      </button>
      {description ? (
        <p className="text-cyber-text-secondary">{description}</p>
      ) : (
        <>
          <p className="text-cyber-text-secondary mb-6 max-w-md mx-auto">
            Connect your wallet to view your portfolio, trade plDXY-BEAR and plDXY-BULL,
            and access all Plether features.
          </p>
          <p className="text-sm text-cyber-text-secondary">
            You can browse prices and protocol stats without connecting.
          </p>
        </>
      )}
    </div>
  )
}
