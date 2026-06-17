import { useAppKit } from '@reown/appkit/react'

export interface ConnectWalletPromptProps {
  description?: string
}

export function ConnectWalletPrompt({ description }: ConnectWalletPromptProps) {
  const { open } = useAppKit()

  return (
    <div className="bg-cyber-surface-dark p-12 text-center border border-cyber-border-glow/30">
      <div className="w-16 h-16 mx-auto mb-4 rounded-full bg-cyber-surface-light flex items-center justify-center">
        <span className="material-symbols-outlined text-3xl text-cyber-text-secondary">lock</span>
      </div>
      <button
        onClick={() => void open()}
        className="mb-2 border border-[#FF572D] bg-[#FF572D] px-5 py-2 text-xl font-semibold text-[#FFF5F9] transition-colors hover:border-[#FFF5F9] hover:bg-[#FFF5F9] hover:text-[#250917] hover:underline hover:underline-offset-4"
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
