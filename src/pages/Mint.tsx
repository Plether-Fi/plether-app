import { useState, useMemo, useEffect } from 'react'
import { useAccount } from 'wagmi'
import { parseUnits, formatUnits } from 'viem'
import { formatAmount } from '../utils/formatters'
import { Alert, TokenIcon } from '../components/ui'
import { TokenInput } from '../components/TokenInput'
import { useTokenBalances, usePreviewMint, usePreviewBurn, useMint, useBurn, useAllowance, useApprove } from '../hooks'
import { getAddresses } from '../contracts/addresses'

type MintMode = 'mint' | 'redeem'

export function Mint() {
  const { isConnected, chainId } = useAccount()
  const [mode, setMode] = useState<MintMode>('mint')
  const [inputAmount, setInputAmount] = useState('')

  const addresses = getAddresses(chainId ?? 1)
  const { usdcBalance, bearBalance, bullBalance, refetch: refetchBalances } = useTokenBalances()

  const inputBigInt = useMemo(() => {
    if (!inputAmount || isNaN(parseFloat(inputAmount))) return 0n
    try {
      return mode === 'mint'
        ? parseUnits(inputAmount, 6)
        : parseUnits(inputAmount, 18)
    } catch {
      return 0n
    }
  }, [inputAmount, mode])

  const { pairAmount: mintOutput, isLoading: mintLoading } = usePreviewMint(inputBigInt)
  const { usdcAmount: burnOutput, isLoading: burnLoading } = usePreviewBurn(inputBigInt)

  const { allowance: usdcAllowance, refetch: refetchUsdcAllowance } = useAllowance(addresses.USDC, addresses.PLETH_CORE)
  const { allowance: bearAllowance, refetch: refetchBearAllowance } = useAllowance(addresses.DXY_BEAR, addresses.PLETH_CORE)
  const { allowance: bullAllowance, refetch: refetchBullAllowance } = useAllowance(addresses.DXY_BULL, addresses.PLETH_CORE)

  const { approve: approveUsdc, isPending: usdcApprovePending, isSuccess: usdcApproveSuccess } = useApprove(addresses.USDC, addresses.PLETH_CORE)
  const { approve: approveBear, isPending: bearApprovePending, isSuccess: bearApproveSuccess } = useApprove(addresses.DXY_BEAR, addresses.PLETH_CORE)
  const { approve: approveBull, isPending: bullApprovePending, isSuccess: bullApproveSuccess } = useApprove(addresses.DXY_BULL, addresses.PLETH_CORE)

  const { mint, isPending: mintPending, isSuccess: mintSuccess } = useMint()
  const { burn, isPending: burnPending, isSuccess: burnSuccess } = useBurn()

  useEffect(() => {
    if (usdcApproveSuccess) refetchUsdcAllowance()
  }, [usdcApproveSuccess, refetchUsdcAllowance])

  useEffect(() => {
    if (bearApproveSuccess) refetchBearAllowance()
  }, [bearApproveSuccess, refetchBearAllowance])

  useEffect(() => {
    if (bullApproveSuccess) refetchBullAllowance()
  }, [bullApproveSuccess, refetchBullAllowance])

  useEffect(() => {
    if (mintSuccess || burnSuccess) {
      refetchBalances()
      setInputAmount('')
    }
  }, [mintSuccess, burnSuccess, refetchBalances])

  const needsUsdcApproval = mode === 'mint' && inputBigInt > 0n && usdcAllowance < inputBigInt
  const needsBearApproval = mode === 'redeem' && inputBigInt > 0n && bearAllowance < inputBigInt
  const needsBullApproval = mode === 'redeem' && inputBigInt > 0n && bullAllowance < inputBigInt

  const outputDisplay = useMemo(() => {
    if (mode === 'mint') {
      return mintLoading ? '...' : formatUnits(mintOutput, 18)
    } else {
      return burnLoading ? '...' : formatUnits(burnOutput, 6)
    }
  }, [mode, mintOutput, burnOutput, mintLoading, burnLoading])

  const minBalance = bearBalance < bullBalance ? bearBalance : bullBalance

  const handleMint = async () => {
    if (needsUsdcApproval) {
      await approveUsdc(inputBigInt)
      return
    }
    await mint(inputBigInt)
  }

  const handleRedeem = async () => {
    if (needsBearApproval) {
      await approveBear(inputBigInt)
      return
    }
    if (needsBullApproval) {
      await approveBull(inputBigInt)
      return
    }
    await burn(inputBigInt)
  }

  const getMintButtonText = () => {
    if (mintPending) return 'Minting...'
    if (usdcApprovePending) return 'Approving USDC...'
    if (needsUsdcApproval) return 'Approve USDC'
    return 'Mint Pairs'
  }

  const getRedeemButtonText = () => {
    if (burnPending) return 'Redeeming...'
    if (bearApprovePending) return 'Approving DXY-BEAR...'
    if (bullApprovePending) return 'Approving DXY-BULL...'
    if (needsBearApproval) return 'Approve DXY-BEAR'
    if (needsBullApproval) return 'Approve DXY-BULL'
    return 'Redeem for USDC'
  }

  const isActionDisabled = !inputAmount || parseFloat(inputAmount) <= 0 ||
    mintPending || burnPending || usdcApprovePending || bearApprovePending || bullApprovePending

  return (
    <div className="space-y-10 max-w-xl mx-auto">
      <div className="mb-8">
        <h1 className="text-3xl font-semibold text-cyber-text-primary mb-1">Mint & Redeem</h1>
        <p className="text-cyber-text-secondary font-light">Create or redeem DXY-BEAR + DXY-BULL pairs</p>
      </div>

      <div className="bg-cyber-surface-dark  border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
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
              <Alert variant="info">
                Mint equal amounts of DXY-BEAR and DXY-BULL from USDC.
                You'll receive both tokens in a 1:1 ratio.
              </Alert>

              <TokenInput
                label="USDC to deposit"
                value={inputAmount}
                onChange={setInputAmount}
                token={{ symbol: 'USDC', decimals: 6 }}
                balance={usdcBalance}
              />

              <div className="flex justify-center -my-2 z-10 relative">
                <div className="bg-cyber-surface-light p-2 rounded-full border border-cyber-border-glow/30">
                  <span className="material-symbols-outlined text-cyber-bright-blue text-lg block">arrow_downward</span>
                </div>
              </div>

              <div className="bg-cyber-surface-light  p-4 space-y-3 border border-cyber-border-glow/30">
                <p className="text-sm text-cyber-text-secondary">You will receive:</p>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BEAR" size="sm" />
                    <span className="text-cyber-electric-fuchsia font-medium">DXY-BEAR</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{outputDisplay}</span>
                </div>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BULL" size="sm" />
                    <span className="text-cyber-neon-green font-medium">DXY-BULL</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{outputDisplay}</span>
                </div>
              </div>

              {isConnected ? (
                <button
                  onClick={handleMint}
                  disabled={isActionDisabled}
                  className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-4 px-6  shadow-lg shadow-cyber-neon-green/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
                >
                  {getMintButtonText()}
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
              <Alert variant="warning" icon="info">
                Redeem equal amounts of DXY-BEAR and DXY-BULL to get back USDC.
                You need equal amounts of both tokens.
              </Alert>

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

              <TokenInput
                label="Amount to redeem (of each token)"
                value={inputAmount}
                onChange={setInputAmount}
                token={{ symbol: 'PAIR', decimals: 18 }}
                balance={minBalance}
                balanceLabel="Max:"
              />

              <div className="flex justify-center -my-2 z-10 relative">
                <div className="bg-cyber-surface-light p-2 rounded-full border border-cyber-border-glow/30">
                  <span className="material-symbols-outlined text-cyber-bright-blue text-lg block">arrow_downward</span>
                </div>
              </div>

              <div className="bg-cyber-surface-light  p-4 border border-cyber-border-glow/30">
                <div className="flex justify-between items-center">
                  <span className="text-cyber-text-secondary">You will receive</span>
                  <span className="text-cyber-text-primary font-semibold text-lg">{outputDisplay} USDC</span>
                </div>
              </div>

              {isConnected ? (
                <button
                  onClick={handleRedeem}
                  disabled={isActionDisabled}
                  className="w-full bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/90 text-cyber-text-primary font-semibold py-4 px-6  shadow-lg shadow-cyber-electric-fuchsia/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
                >
                  {getRedeemButtonText()}
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
