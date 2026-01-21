import { useState } from 'react'
import { useAccount } from 'wagmi'
import { Alert } from '../components/ui'
import { PortfolioCard } from '../components/PortfolioCard'
import { PositionCard } from '../components/PositionCard'
import { AdjustPositionModal } from '../components/AdjustPositionModal'
import { TradeCard } from '../components/TradeCard'
import { YieldCard } from '../components/YieldCard'
import { LeverageCard } from '../components/LeverageCard'
import { MainTabNav } from '../components/MainTabNav'
import { ConnectWalletPrompt } from '../components/ConnectWalletPrompt'
import { Link, useLocation, useNavigate } from 'react-router-dom'
import { HEALTH_FACTOR_WARNING } from '../config/constants'
import { useTokenBalances, useLeveragePosition, useStakedBalance, useTokenPrices, useTransactionSequence, type TransactionStep } from '../hooks'
import { useWriteContract } from 'wagmi'
import { LEVERAGE_ROUTER_ABI } from '../contracts/abis'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
import { useSettingsStore } from '../stores/settingsStore'
import type { LeveragePosition } from '../types'

export function Dashboard() {
  const { isConnected, chainId } = useAccount()
  const location = useLocation()
  const navigate = useNavigate()

  type MainTab = 'trade' | 'leverage' | 'yield'

  const mainTab: MainTab =
    location.pathname === '/leverage' ? 'leverage' :
    location.pathname === '/yield' ? 'yield' : 'trade'

  const handleTabChange = (tab: MainTab) => {
    if (tab === 'leverage') void navigate('/leverage')
    else if (tab === 'yield') void navigate('/yield')
    else void navigate('/')
  }

  const [selectedPosition, setSelectedPosition] = useState<LeveragePosition | null>(null)
  const [adjustModalOpen, setAdjustModalOpen] = useState(false)

  const {
    usdcBalance,
    bearBalance,
    bullBalance,
    isLoading: balancesLoading,
    refetch: refetchBalances
  } = useTokenBalances()

  const { assets: stakedBearAssets, isLoading: stakedBearLoading } = useStakedBalance('BEAR')
  const { assets: stakedBullAssets, isLoading: stakedBullLoading } = useStakedBalance('BULL')

  const { bearPrice, bullPrice } = useTokenPrices()

  const bearPosition = useLeveragePosition('BEAR')
  const bullPosition = useLeveragePosition('BULL')

  const slippage = useSettingsStore((s) => s.slippage)
  const addresses = getAddresses(chainId ?? DEFAULT_CHAIN_ID)
  const { writeContractAsync } = useWriteContract()
  const closeSequence = useTransactionSequence()

  const handleClosePosition = (position: LeveragePosition) => {
    const collateralToClose = position.side === 'BEAR' ? bearPosition.collateral : bullPosition.collateral
    const routerAddress = position.side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER

    const buildCloseSteps = (): TransactionStep[] => {
      const slippageBps = BigInt(Math.floor(slippage * 100))
      const deadline = BigInt(Math.floor(Date.now() / 1000) + 1800)

      return [{
        label: `Close ${position.side} position`,
        action: () => {
          console.log('[closeLeverage] args:', {
            side: position.side,
            collateralToWithdraw: collateralToClose.toString(),
            slippageBps: slippageBps.toString(),
          })

          return writeContractAsync({
            address: routerAddress,
            abi: LEVERAGE_ROUTER_ABI,
            functionName: 'closeLeverage',
            args: [collateralToClose, slippageBps, deadline],
          })
        },
      }]
    }

    void closeSequence.execute({
      title: `Closing ${position.side} leverage position`,
      buildSteps: buildCloseSteps,
      onSuccess: () => {
        void bearPosition.refetch()
        void bullPosition.refetch()
        void refetchBalances()
      },
    })
  }

  const positions: LeveragePosition[] = []

  if (bearPosition.hasPosition) {
    const leverageNum = Number(bearPosition.leverage) / 100
    // Convert collateral to USD: shares * price / 10^23 (gives 6-decimal USDC)
    // Shares have 21 effective decimals (18 + 1000x offset), price has 8 decimals
    const positionValue = bearPosition.collateral * bearPrice / 10n ** 23n
    const equity = positionValue > bearPosition.debt ? positionValue - bearPosition.debt : 0n
    positions.push({
      id: 'bear-position',
      side: 'BEAR',
      size: positionValue,
      collateral: equity,
      leverage: leverageNum,
      entryPrice: 0n,
      liquidationPrice: bearPosition.liquidationPrice,
      healthFactor: Number(bearPosition.healthFactor) / 1e18,
      pnl: 0n,
      pnlPercentage: 0,
    })
  }

  if (bullPosition.hasPosition) {
    const leverageNum = Number(bullPosition.leverage) / 100
    // Convert collateral to USD: shares * price / 10^23 (gives 6-decimal USDC)
    const positionValue = bullPosition.collateral * bullPrice / 10n ** 23n
    const equity = positionValue > bullPosition.debt ? positionValue - bullPosition.debt : 0n
    console.log('[Dashboard BULL]', {
      collateral: bullPosition.collateral.toString(),
      bullPrice: bullPrice.toString(),
      positionValue: positionValue.toString(),
      debt: bullPosition.debt.toString(),
      equity: equity.toString(),
    })
    positions.push({
      id: 'bull-position',
      side: 'BULL',
      size: positionValue,
      collateral: equity,
      leverage: leverageNum,
      entryPrice: 0n,
      liquidationPrice: bullPosition.liquidationPrice,
      healthFactor: Number(bullPosition.healthFactor) / 1e18,
      pnl: 0n,
      pnlPercentage: 0,
    })
  }

  const hasLowHealth = positions.some((p) => p.healthFactor > 0 && p.healthFactor < HEALTH_FACTOR_WARNING)

  // Portfolio values: token balances (18 dec) * price (8 dec) / 10^20 = 6 dec USDC
  const bearSpotValue = bearBalance * bearPrice / 10n ** 20n
  const bullSpotValue = bullBalance * bullPrice / 10n ** 20n
  const spotValue = usdcBalance + bearSpotValue + bullSpotValue

  // Staked values: assets (18 dec) * price (8 dec) / 10^20 = 6 dec USDC
  const stakedBearValue = stakedBearAssets * bearPrice / 10n ** 20n
  const stakedBullValue = stakedBullAssets * bullPrice / 10n ** 20n
  const stakedValue = stakedBearValue + stakedBullValue

  const leverageValue = positions.reduce((acc, p) => acc + p.collateral, 0n)
  const lendingValue = 0n

  return (
    <div className="space-y-10">
      {/* Page title */}
      <div className="mb-8">
        <h1 className="text-3xl font-semibold text-cyber-text-primary mb-1">Dashboard</h1>
        <p className="text-cyber-text-secondary font-light">Your portfolio overview</p>
      </div>

      {isConnected ? (
        <>
          {/* Portfolio breakdown */}
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-10">
            <PortfolioCard
              title="Spot Holdings"
              value={spotValue}
              description="USDC, DXY-BEAR, DXY-BULL"
              link="/"
              isLoading={balancesLoading}
              colorClass="text-cyber-bright-blue"
            />
            <PortfolioCard
              title="Staked"
              value={stakedValue}
              description="sDXY-BEAR, sDXY-BULL"
              link="/stake"
              isLoading={stakedBearLoading || stakedBullLoading}
              colorClass="text-cyber-bright-blue"
            />
            <PortfolioCard
              title="Leverage"
              value={leverageValue}
              description="Open positions"
              link="/leverage"
              isLoading={bearPosition.isLoading || bullPosition.isLoading}
              colorClass="text-cyber-electric-fuchsia"
            />
            <PortfolioCard
              title="Lending"
              value={lendingValue}
              description="Morpho supplied"
              link="/yield"
              isLoading={false}
              colorClass="text-cyber-neon-green"
            />
          </div>

          {/* Positions section */}
          {positions.length > 0 && (
            <div className="mb-12">
              <h2 className="text-xl font-semibold text-cyber-text-primary mb-4">Open Positions</h2>

              {hasLowHealth && (
                <Alert variant="warning" title="Low Health Factor Warning" className="mb-6 shadow-lg shadow-cyber-warning-text/10">
                  One or more positions have low health factors and may be at risk of liquidation.
                </Alert>
              )}

              <div className="space-y-4">
                {positions.map((position) => (
                  <PositionCard
                    key={position.id}
                    position={position}
                    onAdjust={() => {
                      setSelectedPosition(position)
                      setAdjustModalOpen(true)
                    }}
                    onClose={() => handleClosePosition(position)}
                    isClosing={closeSequence.isRunning}
                  />
                ))}
              </div>
            </div>
          )}

          {/* Trade / Leverage / Yield widget */}
          <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 overflow-hidden shadow-lg shadow-cyber-border-glow/10">
            <MainTabNav activeTab={mainTab} onTabChange={handleTabChange} />

            {/* Tab content */}
            <div className="p-6 md:p-8 lg:p-12">
              {mainTab === 'trade' && (
                <TradeCard
                  usdcBalance={usdcBalance}
                  bearBalance={bearBalance}
                  bullBalance={bullBalance}
                  refetchBalances={() => void refetchBalances()}
                />
              )}

              {mainTab === 'leverage' && (
                <LeverageCard usdcBalance={usdcBalance} refetchBalances={() => void refetchBalances()} />
              )}

              {mainTab === 'yield' && (
                <YieldCard
                  suppliedAmount={5000n * 10n ** 6n}
                  borrowedAmount={1000n * 10n ** 6n}
                  availableToBorrow={3000n * 10n ** 6n}
                  supplyApy={3.5}
                  borrowApy={5.2}
                  usdcBalance={usdcBalance}
                  suppliedBalance={5000n * 10n ** 6n}
                />
              )}
            </div>
          </div>

          {/* Transaction History link */}
          <div className="mt-8 text-center pb-8">
            <Link
              to="/history"
              className="inline-flex items-center gap-2 text-cyber-electric-fuchsia hover:text-cyber-electric-fuchsia/80 font-medium text-sm transition-colors"
            >
              View Transaction History
              <span className="material-symbols-outlined text-sm">arrow_forward</span>
            </Link>
          </div>

          {selectedPosition && (
            <AdjustPositionModal
              isOpen={adjustModalOpen}
              onClose={() => {
                setAdjustModalOpen(false)
                setSelectedPosition(null)
              }}
              position={selectedPosition}
              onSuccess={() => {
                void bearPosition.refetch()
                void bullPosition.refetch()
                void refetchBalances()
              }}
            />
          )}
        </>
      ) : (
        <ConnectWalletPrompt />
      )}
    </div>
  )
}

export default Dashboard
