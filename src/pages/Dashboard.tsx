import { useState } from 'react'
import { useAccount } from 'wagmi'
import { Alert } from '../components/ui'
import { PortfolioCard } from '../components/PortfolioCard'
import { PositionCard } from '../components/PositionCard'
import { AdjustPositionModal } from '../components/AdjustPositionModal'
import { TradeCard } from '../components/TradeCard'
import { YieldCard } from '../components/YieldCard'
import { LeverageCard } from '../components/LeverageCard'
import { Link, useLocation, useNavigate } from 'react-router-dom'
import { HEALTH_FACTOR_WARNING } from '../config/constants'
import type { LeveragePosition } from '../types'

type MainTab = 'trade' | 'leverage' | 'yield'

const mockPositions: LeveragePosition[] = [
  {
    id: '1',
    side: 'BEAR',
    size: 5000n * 10n ** 6n,
    collateral: 2500n * 10n ** 6n,
    leverage: 2,
    entryPrice: 103n * 10n ** 6n,
    liquidationPrice: 95n * 10n ** 6n,
    healthFactor: 1.8,
    pnl: 250n * 10n ** 6n,
    pnlPercentage: 10,
  },
  {
    id: '2',
    side: 'BULL',
    size: 3000n * 10n ** 6n,
    collateral: 1000n * 10n ** 6n,
    leverage: 3,
    entryPrice: 102n * 10n ** 6n,
    liquidationPrice: 110n * 10n ** 6n,
    healthFactor: 1.3,
    pnl: -100n * 10n ** 6n,
    pnlPercentage: -10,
  },
]

export function Dashboard() {
  const { isConnected } = useAccount()
  const location = useLocation()
  const navigate = useNavigate()

  const mainTab: MainTab =
    location.pathname === '/leverage' ? 'leverage' :
    location.pathname === '/yield' ? 'yield' : 'trade'

  const handleTabChange = (tab: string) => {
    if (tab === 'leverage') navigate('/leverage')
    else if (tab === 'yield') navigate('/yield')
    else navigate('/')
  }

  const [selectedPosition, setSelectedPosition] = useState<LeveragePosition | null>(null)
  const [adjustModalOpen, setAdjustModalOpen] = useState(false)
  const positions = mockPositions
  const hasLowHealth = positions.some((p) => p.healthFactor < HEALTH_FACTOR_WARNING)

  const usdcBalance = 10000n * 10n ** 6n
  const bearBalance = 500n * 10n ** 18n
  const bullBalance = 500n * 10n ** 18n

  const isLoading = false
  const spotValue = 5000n * 10n ** 6n
  const stakedValue = 3000n * 10n ** 6n
  const leverageValue = 1500n * 10n ** 6n
  const lendingValue = 500n * 10n ** 6n

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
              isLoading={isLoading}
              colorClass="text-cyber-bright-blue"
            />
            <PortfolioCard
              title="Staked"
              value={stakedValue}
              description="sDXY-BEAR, sDXY-BULL"
              link="/stake"
              isLoading={isLoading}
              colorClass="text-cyber-bright-blue"
            />
            <PortfolioCard
              title="Leverage"
              value={leverageValue}
              description="Open positions"
              link="/leverage"
              isLoading={isLoading}
              colorClass="text-cyber-electric-fuchsia"
            />
            <PortfolioCard
              title="Lending"
              value={lendingValue}
              description="Morpho supplied"
              link="/yield"
              isLoading={isLoading}
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
                  />
                ))}
              </div>
            </div>
          )}

          {/* Trade / Leverage / Yield widget */}
          <div className="bg-cyber-surface-dark border border-cyber-border-glow/30  overflow-hidden shadow-lg shadow-cyber-border-glow/10">
            {/* Folder tabs */}
            <div className={`flex flex-col sm:flex-row border-b-2 ${
              mainTab === 'trade' ? 'border-cyber-bright-blue shadow-[0_2px_10px_-2px] shadow-cyber-bright-blue/50' :
              mainTab === 'leverage' ? 'border-cyber-electric-fuchsia shadow-[0_2px_10px_-2px] shadow-cyber-electric-fuchsia/50' :
              'border-cyber-neon-green shadow-[0_2px_10px_-2px] shadow-cyber-neon-green/50'
            }`}>
              <button
                onClick={() => handleTabChange('trade')}
                className={`
                  flex-1 flex items-center gap-3 px-6 py-5 text-left transition-colors -mb-[2px]
                  ${mainTab === 'trade'
                    ? 'bg-cyber-surface-light border-b-2 border-cyber-bright-blue shadow-md shadow-cyber-bright-blue/10'
                    : 'hover:bg-cyber-surface-light border-b-2 border-transparent opacity-60 hover:opacity-100 hover:border-cyber-bright-blue/50'
                  }
                `}
              >
                <div className={`p-2  ${mainTab === 'trade' ? 'bg-cyber-bright-blue/20 text-cyber-bright-blue' : 'bg-cyber-text-secondary/20 text-cyber-text-secondary'}`}>
                  <span className="material-symbols-outlined text-xl">security</span>
                </div>
                <div>
                  <div className={`font-semibold ${mainTab === 'trade' ? 'text-cyber-bright-blue' : 'text-cyber-text-primary'}`}>Dollar Hedge</div>
                  <div className={`text-xs ${mainTab === 'trade' ? 'text-cyber-bright-blue/70' : 'text-cyber-text-secondary'}`}>Spot trading</div>
                </div>
              </button>

              <button
                onClick={() => handleTabChange('leverage')}
                className={`
                  flex-1 flex items-center gap-3 px-6 py-5 text-left transition-colors -mb-[2px]
                  ${mainTab === 'leverage'
                    ? 'bg-cyber-surface-light border-b-2 border-cyber-electric-fuchsia shadow-md shadow-cyber-electric-fuchsia/10'
                    : 'hover:bg-cyber-surface-light border-b-2 border-transparent opacity-60 hover:opacity-100 hover:border-cyber-electric-fuchsia/50'
                  }
                `}
              >
                <div className={`p-2  ${mainTab === 'leverage' ? 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia' : 'bg-cyber-text-secondary/20 text-cyber-text-secondary'}`}>
                  <span className="material-symbols-outlined text-xl">trending_up</span>
                </div>
                <div>
                  <div className={`font-semibold ${mainTab === 'leverage' ? 'text-cyber-electric-fuchsia' : 'text-cyber-text-primary'}`}>Leverage</div>
                  <div className={`text-xs ${mainTab === 'leverage' ? 'text-cyber-electric-fuchsia/70' : 'text-cyber-text-secondary'}`}>Margin trading</div>
                </div>
              </button>

              <button
                onClick={() => handleTabChange('yield')}
                className={`
                  flex-1 flex items-center gap-3 px-6 py-5 text-left transition-colors -mb-[2px]
                  ${mainTab === 'yield'
                    ? 'bg-cyber-surface-light border-b-2 border-cyber-neon-green shadow-md shadow-cyber-neon-green/10'
                    : 'hover:bg-cyber-surface-light border-b-2 border-transparent opacity-60 hover:opacity-100 hover:border-cyber-neon-green/50'
                  }
                `}
              >
                <div className={`p-2  ${mainTab === 'yield' ? 'bg-cyber-neon-green/20 text-cyber-neon-green' : 'bg-cyber-text-secondary/20 text-cyber-text-secondary'}`}>
                  <span className="material-symbols-outlined text-xl">grass</span>
                </div>
                <div>
                  <div className={`font-semibold ${mainTab === 'yield' ? 'text-cyber-neon-green' : 'text-cyber-text-primary'}`}>Yield</div>
                  <div className={`text-xs ${mainTab === 'yield' ? 'text-cyber-neon-green/70' : 'text-cyber-text-secondary'}`}>Liquidity providing</div>
                </div>
              </button>
            </div>

            {/* Tab content */}
            <div className="p-6 md:p-8 lg:p-12">
              {mainTab === 'trade' && (
                <TradeCard
                  usdcBalance={usdcBalance}
                  bearBalance={bearBalance}
                  bullBalance={bullBalance}
                />
              )}

              {mainTab === 'leverage' && (
                <LeverageCard usdcBalance={usdcBalance} />
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
            />
          )}
        </>
      ) : (
        <div className="bg-cyber-surface-dark  p-12 text-center border border-cyber-border-glow/30 shadow-lg">
          <div className="w-16 h-16 mx-auto mb-4 rounded-full bg-cyber-surface-light flex items-center justify-center">
            <span className="material-symbols-outlined text-3xl text-cyber-text-secondary">lock</span>
          </div>
          <h2 className="text-xl font-semibold text-cyber-text-primary mb-2">Connect Your Wallet</h2>
          <p className="text-cyber-text-secondary mb-6 max-w-md mx-auto">
            Connect your wallet to view your portfolio, trade DXY-BEAR and DXY-BULL,
            and access all Plether features.
          </p>
          <p className="text-sm text-cyber-text-secondary">
            You can browse prices and protocol stats without connecting.
          </p>
        </div>
      )}
    </div>
  )
}
