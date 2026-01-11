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
import { useTokenBalances, useLeveragePosition, useCloseLeverage, useStakedBalance } from '../hooks'
import { useSettingsStore } from '../stores/settingsStore'
import type { LeveragePosition } from '../types'

export function Dashboard() {
  const { isConnected } = useAccount()
  const location = useLocation()
  const navigate = useNavigate()

  type MainTab = 'trade' | 'leverage' | 'yield'

  const mainTab: MainTab =
    location.pathname === '/leverage' ? 'leverage' :
    location.pathname === '/yield' ? 'yield' : 'trade'

  const handleTabChange = (tab: MainTab) => {
    if (tab === 'leverage') navigate('/leverage')
    else if (tab === 'yield') navigate('/yield')
    else navigate('/')
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

  const bearPosition = useLeveragePosition('BEAR')
  const bullPosition = useLeveragePosition('BULL')

  const slippage = useSettingsStore((s) => s.slippage)
  const { closePosition: closeBearPosition, isPending: bearClosePending } = useCloseLeverage('BEAR')
  const { closePosition: closeBullPosition, isPending: bullClosePending } = useCloseLeverage('BULL')

  const handleClosePosition = async (position: LeveragePosition) => {
    const slippageBps = BigInt(Math.floor(slippage * 100))
    const deadline = BigInt(Math.floor(Date.now() / 1000) + 1800)

    if (position.side === 'BEAR') {
      await closeBearPosition(bearPosition.debt, bearPosition.collateral, slippageBps, deadline)
    } else {
      await closeBullPosition(bullPosition.debt, bullPosition.collateral, slippageBps, deadline)
    }

    bearPosition.refetch()
    bullPosition.refetch()
    refetchBalances()
  }

  const positions: LeveragePosition[] = []

  if (bearPosition.hasPosition) {
    const leverageNum = Number(bearPosition.leverage) / 100
    positions.push({
      id: 'bear-position',
      side: 'BEAR',
      size: bearPosition.collateral * BigInt(Math.floor(leverageNum * 100)) / 100n,
      collateral: bearPosition.collateral,
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
    positions.push({
      id: 'bull-position',
      side: 'BULL',
      size: bullPosition.collateral * BigInt(Math.floor(leverageNum * 100)) / 100n,
      collateral: bullPosition.collateral,
      leverage: leverageNum,
      entryPrice: 0n,
      liquidationPrice: bullPosition.liquidationPrice,
      healthFactor: Number(bullPosition.healthFactor) / 1e18,
      pnl: 0n,
      pnlPercentage: 0,
    })
  }

  const hasLowHealth = positions.some((p) => p.healthFactor > 0 && p.healthFactor < HEALTH_FACTOR_WARNING)

  const spotValue = usdcBalance + (bearBalance * 1n) / 10n ** 12n + (bullBalance * 1n) / 10n ** 12n
  const stakedValue = (stakedBearAssets * 1n) / 10n ** 12n + (stakedBullAssets * 1n) / 10n ** 12n
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
                    isClosing={position.side === 'BEAR' ? bearClosePending : bullClosePending}
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
                  refetchBalances={refetchBalances}
                />
              )}

              {mainTab === 'leverage' && (
                <LeverageCard usdcBalance={usdcBalance} refetchBalances={refetchBalances} />
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
                bearPosition.refetch()
                bullPosition.refetch()
                refetchBalances()
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
