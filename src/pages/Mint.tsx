import { useState } from 'react'
import { useAccount } from 'wagmi'
import { formatAmount } from '../utils/formatters'
import { TokenIcon } from '../components/ui'

type MintMode = 'mint' | 'redeem'

export function Mint() {
  const { isConnected } = useAccount()
  const [mode, setMode] = useState<MintMode>('mint')
  const [inputAmount, setInputAmount] = useState('')

  const usdcBalance = 10000n * 10n ** 6n
  const bearBalance = 500n * 10n ** 18n
  const bullBalance = 500n * 10n ** 18n

  const outputAmount = inputAmount ? parseFloat(inputAmount).toFixed(4) : '0'
  const minBalance = bearBalance < bullBalance ? bearBalance : bullBalance

  const handleMint = async () => {
    console.log('Mint:', { amount: inputAmount })
  }

  const handleRedeem = async () => {
    console.log('Redeem:', { amount: inputAmount })
  }

  return (
    <div className="space-y-10 max-w-xl mx-auto">
      <div className="mb-8">
        <h1 className="text-3xl font-semibold text-cyber-text-primary mb-1">Mint & Redeem</h1>
        <p className="text-cyber-text-secondary font-light">Create or redeem DXY-BEAR + DXY-BULL pairs</p>
      </div>

      <div className="bg-cyber-surface-dark  border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
        {/* Tabs */}
        <div className="flex border-b border-cyber-border-glow/30">
          <button
            onClick={() => { setMode('mint'); setInputAmount('') }}
            className={`flex-1 flex items-center justify-center gap-2 px-6 py-4 text-sm font-semibold transition-colors ${
              mode === 'mint'
                ? 'bg-cyber-surface-light text-cyber-neon-green border-b-2 border-cyber-neon-green'
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue border-b-2 border-transparent'
            }`}
          >
            <span className="material-symbols-outlined text-xl">add_circle</span>
            Mint Pairs
          </button>
          <button
            onClick={() => { setMode('redeem'); setInputAmount('') }}
            className={`flex-1 flex items-center justify-center gap-2 px-6 py-4 text-sm font-semibold transition-colors ${
              mode === 'redeem'
                ? 'bg-cyber-surface-light text-cyber-electric-fuchsia border-b-2 border-cyber-electric-fuchsia'
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue border-b-2 border-transparent'
            }`}
          >
            <span className="material-symbols-outlined text-xl">swap_horiz</span>
            Redeem
          </button>
        </div>

        <div className="p-6 md:p-8 space-y-6">
          {mode === 'mint' ? (
            <>
              {/* Info box */}
              <div className="bg-cyber-bright-blue/10 border border-cyber-bright-blue/30  p-4 flex items-start gap-3">
                <span className="material-symbols-outlined text-cyber-bright-blue mt-0.5">info</span>
                <p className="text-sm text-cyber-bright-blue">
                  Mint equal amounts of DXY-BEAR and DXY-BULL from USDC.
                  You'll receive both tokens in a 1:1 ratio.
                </p>
              </div>

              {/* Input */}
              <div className="space-y-2">
                <div className="flex justify-between text-sm">
                  <span className="text-cyber-text-secondary">USDC to deposit</span>
                  <span className="text-cyber-text-secondary">
                    Balance: <span className="text-cyber-text-primary">{formatAmount(usdcBalance, 6)}</span>
                  </span>
                </div>
                <div className="relative">
                  <input
                    type="number"
                    value={inputAmount}
                    onChange={(e) => setInputAmount(e.target.value)}
                    placeholder="0.00"
                    className="w-full bg-cyber-surface-light border border-cyber-border-glow/30  py-4 pl-4 pr-28 text-xl font-medium text-cyber-text-primary focus:ring-1 focus:ring-cyber-bright-blue focus:border-cyber-bright-blue outline-none"
                  />
                  <div className="absolute right-4 top-1/2 -translate-y-1/2 flex items-center gap-2">
                    <button
                      onClick={() => setInputAmount((Number(usdcBalance) / 1e6).toString())}
                      className="text-xs font-semibold text-cyber-neon-green hover:opacity-80 px-2 py-1 bg-cyber-neon-green/10"
                    >
                      MAX
                    </button>
                    <span className="font-medium text-cyber-text-secondary">USDC</span>
                  </div>
                </div>
              </div>

              {/* Arrow */}
              <div className="flex justify-center -my-2 z-10 relative">
                <div className="bg-cyber-surface-light p-2 rounded-full border border-cyber-border-glow/30">
                  <span className="material-symbols-outlined text-cyber-bright-blue text-lg block">arrow_downward</span>
                </div>
              </div>

              {/* Output preview */}
              <div className="bg-cyber-surface-light  p-4 space-y-3 border border-cyber-border-glow/30">
                <p className="text-sm text-cyber-text-secondary">You will receive:</p>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BEAR" size="sm" />
                    <span className="text-cyber-electric-fuchsia font-medium">DXY-BEAR</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{outputAmount}</span>
                </div>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BULL" size="sm" />
                    <span className="text-cyber-neon-green font-medium">DXY-BULL</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{outputAmount}</span>
                </div>
              </div>

              {/* Action button */}
              {isConnected ? (
                <button
                  onClick={handleMint}
                  disabled={!inputAmount || parseFloat(inputAmount) <= 0}
                  className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-4 px-6  shadow-lg shadow-cyber-neon-green/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
                >
                  Mint Pairs
                </button>
              ) : (
                <button
                  disabled
                  className="w-full bg-cyber-surface-light text-cyber-text-secondary font-semibold py-4 px-6  cursor-not-allowed"
                >
                  Connect Wallet to Mint
                </button>
              )}
            </>
          ) : (
            <>
              {/* Info box */}
              <div className="bg-cyber-warning-bg border border-cyber-warning-text/30  p-4 flex items-start gap-3">
                <span className="material-symbols-outlined text-cyber-warning-text mt-0.5">info</span>
                <p className="text-sm text-cyber-warning-text">
                  Redeem equal amounts of DXY-BEAR and DXY-BULL to get back USDC.
                  You need equal amounts of both tokens.
                </p>
              </div>

              {/* Your balances */}
              <div className="bg-cyber-surface-light  p-4 space-y-3 border border-cyber-border-glow/30">
                <p className="text-sm text-cyber-text-secondary">Your balances:</p>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BEAR" size="sm" />
                    <span className="text-cyber-electric-fuchsia font-medium">DXY-BEAR</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{formatAmount(bearBalance, 18)}</span>
                </div>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BULL" size="sm" />
                    <span className="text-cyber-neon-green font-medium">DXY-BULL</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{formatAmount(bullBalance, 18)}</span>
                </div>
              </div>

              {/* Input */}
              <div className="space-y-2">
                <div className="flex justify-between text-sm">
                  <span className="text-cyber-text-secondary">Amount to redeem (of each token)</span>
                  <span className="text-cyber-text-secondary">
                    Max: <span className="text-cyber-text-primary">{formatAmount(minBalance, 18)}</span>
                  </span>
                </div>
                <div className="relative">
                  <input
                    type="number"
                    value={inputAmount}
                    onChange={(e) => setInputAmount(e.target.value)}
                    placeholder="0.00"
                    className="w-full bg-cyber-surface-light border border-cyber-border-glow/30  py-4 pl-4 pr-28 text-xl font-medium text-cyber-text-primary focus:ring-1 focus:ring-cyber-bright-blue focus:border-cyber-bright-blue outline-none"
                  />
                  <div className="absolute right-4 top-1/2 -translate-y-1/2 flex items-center gap-2">
                    <button
                      onClick={() => setInputAmount((Number(minBalance) / 1e18).toString())}
                      className="text-xs font-semibold text-cyber-electric-fuchsia hover:opacity-80 px-2 py-1 bg-cyber-electric-fuchsia/10"
                    >
                      MAX
                    </button>
                    <span className="font-medium text-cyber-text-secondary">PAIR</span>
                  </div>
                </div>
              </div>

              {/* Arrow */}
              <div className="flex justify-center -my-2 z-10 relative">
                <div className="bg-cyber-surface-light p-2 rounded-full border border-cyber-border-glow/30">
                  <span className="material-symbols-outlined text-cyber-bright-blue text-lg block">arrow_downward</span>
                </div>
              </div>

              {/* Output preview */}
              <div className="bg-cyber-surface-light  p-4 border border-cyber-border-glow/30">
                <div className="flex justify-between items-center">
                  <span className="text-cyber-text-secondary">You will receive</span>
                  <span className="text-cyber-text-primary font-semibold text-lg">{outputAmount} USDC</span>
                </div>
              </div>

              {/* Action button */}
              {isConnected ? (
                <button
                  onClick={handleRedeem}
                  disabled={!inputAmount || parseFloat(inputAmount) <= 0}
                  className="w-full bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/90 text-cyber-text-primary font-semibold py-4 px-6  shadow-lg shadow-cyber-electric-fuchsia/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
                >
                  Redeem for USDC
                </button>
              ) : (
                <button
                  disabled
                  className="w-full bg-cyber-surface-light text-cyber-text-secondary font-semibold py-4 px-6  cursor-not-allowed"
                >
                  Connect Wallet to Redeem
                </button>
              )}
            </>
          )}
        </div>
      </div>
    </div>
  )
}
