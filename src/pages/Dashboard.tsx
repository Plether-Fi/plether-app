import { useState } from 'react'
import { useAccount } from 'wagmi'
import { SkeletonCard, InfoTooltip, Modal } from '../components/ui'
import { formatUsd, formatPercent } from '../utils/formatters'
import { Link, useLocation, useNavigate } from 'react-router-dom'
import { HEALTH_FACTOR_WARNING, HEALTH_FACTOR_DANGER } from '../config/constants'
import type { LeveragePosition } from '../types'

type MainTab = 'trade' | 'leverage' | 'yield'
type TradeMode = 'buy' | 'sell'
type TokenSide = 'BEAR' | 'BULL'

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

  const [mode, setMode] = useState<TradeMode>('buy')
  const [selectedToken, setSelectedToken] = useState<TokenSide>('BEAR')
  const [inputAmount, setInputAmount] = useState('')
  const [showDetails, setShowDetails] = useState(false)

  const [selectedSide, setSelectedSide] = useState<TokenSide>('BEAR')
  const [collateralAmount, setCollateralAmount] = useState('')
  const [leverage, setLeverage] = useState(2)

  const [supplyMode, setSupplyMode] = useState<'supply' | 'withdraw'>('supply')
  const [borrowMode, setBorrowMode] = useState<'borrow' | 'repay'>('borrow')
  const [supplyAmount, setSupplyAmount] = useState('')
  const [borrowAmount, setBorrowAmount] = useState('')

  const suppliedAmount = 5000n * 10n ** 6n
  const borrowedAmount = 1000n * 10n ** 6n
  const supplyApy = 3.5
  const borrowApy = 5.2
  const availableToBorrow = 3000n * 10n ** 6n

  const [selectedPosition, setSelectedPosition] = useState<LeveragePosition | null>(null)
  const [adjustModalOpen, setAdjustModalOpen] = useState(false)
  const positions = mockPositions
  const hasLowHealth = positions.some((p) => p.healthFactor < HEALTH_FACTOR_WARNING)

  const usdcBalance = 10000n * 10n ** 6n
  const bearBalance = 500n * 10n ** 18n
  const bullBalance = 500n * 10n ** 18n

  const inputToken = mode === 'buy'
    ? { symbol: 'USDC', decimals: 6 }
    : { symbol: `DXY-${selectedToken}`, decimals: 18 }

  const outputToken = mode === 'buy'
    ? { symbol: `DXY-${selectedToken}`, decimals: 18 }
    : { symbol: 'USDC', decimals: 6 }

  const inputBalance = mode === 'buy'
    ? usdcBalance
    : selectedToken === 'BEAR' ? bearBalance : bullBalance

  const outputAmount = inputAmount ? (parseFloat(inputAmount) * 0.98).toFixed(4) : '0'

  const collateralNum = parseFloat(collateralAmount) || 0
  const positionSize = collateralNum * leverage
  const liquidationPrice = collateralNum > 0 ? (103.45 * (1 - 1 / leverage)).toFixed(2) : '0.00'

  const handleSwap = async () => {
    console.log('Swap:', { mode, selectedToken, inputAmount })
  }

  const handleOpenPosition = async () => {
    console.log('Open position:', { selectedSide, collateralAmount, leverage })
  }

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
              link="/trade"
              isLoading={isLoading}
              colorClass="text-cyber-neon-green"
            />
            <PortfolioCard
              title="Staked"
              value={stakedValue}
              description="sDXY-BEAR, sDXY-BULL"
              link="/stake"
              isLoading={isLoading}
              colorClass="text-cyber-electric-fuchsia"
            />
            <PortfolioCard
              title="Leverage"
              value={leverageValue}
              description="Open positions"
              link="/leverage"
              isLoading={isLoading}
              colorClass="text-cyber-bright-blue"
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
                <div className="mb-6 bg-cyber-warning-bg border border-cyber-warning-text/40 rounded-lg p-4 flex items-start gap-3 shadow-lg shadow-cyber-warning-text/10">
                  <span className="material-symbols-outlined text-cyber-warning-text mt-0.5">warning</span>
                  <div>
                    <h3 className="font-medium text-cyber-warning-text text-sm">Low Health Factor Warning</h3>
                    <p className="text-sm text-cyber-warning-text/80 mt-1">
                      One or more positions have low health factors and may be at risk of liquidation.
                    </p>
                  </div>
                </div>
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
          <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 rounded-xl overflow-hidden shadow-lg shadow-cyber-border-glow/10">
            {/* Folder tabs */}
            <div className="flex flex-col sm:flex-row border-b border-cyber-border-glow/30">
              <button
                onClick={() => handleTabChange('trade')}
                className={`
                  flex-1 flex items-center gap-3 px-6 py-5 text-left transition-colors
                  ${mainTab === 'trade'
                    ? 'bg-cyber-surface-light border-b-2 border-cyber-bright-blue shadow-md shadow-cyber-bright-blue/10'
                    : 'hover:bg-cyber-surface-light border-b-2 border-transparent opacity-60 hover:opacity-100 hover:border-cyber-bright-blue/50'
                  }
                `}
              >
                <div className={`p-2 rounded-lg ${mainTab === 'trade' ? 'bg-cyber-bright-blue/20 text-cyber-bright-blue' : 'bg-cyber-text-secondary/20 text-cyber-text-secondary'}`}>
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
                  flex-1 flex items-center gap-3 px-6 py-5 text-left transition-colors
                  ${mainTab === 'leverage'
                    ? 'bg-cyber-surface-light border-b-2 border-cyber-electric-fuchsia shadow-md shadow-cyber-electric-fuchsia/10'
                    : 'hover:bg-cyber-surface-light border-b-2 border-transparent opacity-60 hover:opacity-100 hover:border-cyber-electric-fuchsia/50'
                  }
                `}
              >
                <div className={`p-2 rounded-lg ${mainTab === 'leverage' ? 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia' : 'bg-cyber-text-secondary/20 text-cyber-text-secondary'}`}>
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
                  flex-1 flex items-center gap-3 px-6 py-5 text-left transition-colors
                  ${mainTab === 'yield'
                    ? 'bg-cyber-surface-light border-b-2 border-cyber-neon-green shadow-md shadow-cyber-neon-green/10'
                    : 'hover:bg-cyber-surface-light border-b-2 border-transparent opacity-60 hover:opacity-100 hover:border-cyber-neon-green/50'
                  }
                `}
              >
                <div className={`p-2 rounded-lg ${mainTab === 'yield' ? 'bg-cyber-neon-green/20 text-cyber-neon-green' : 'bg-cyber-text-secondary/20 text-cyber-text-secondary'}`}>
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
                <div className="max-w-xl mx-auto space-y-6">
                  {/* Buy/Sell tabs */}
                  <div className="bg-cyber-surface-light p-1 rounded-lg flex text-sm font-medium mb-8 border border-cyber-border-glow/30">
                    <button
                      onClick={() => setMode('buy')}
                      className={`flex-1 py-2 px-4 rounded-md transition-all ${
                        mode === 'buy'
                          ? 'bg-cyber-surface-dark text-cyber-neon-green shadow-sm shadow-cyber-neon-green/10 border border-cyber-neon-green/50'
                          : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
                      }`}
                    >
                      Buy
                    </button>
                    <button
                      onClick={() => setMode('sell')}
                      className={`flex-1 py-2 px-4 rounded-md transition-all ${
                        mode === 'sell'
                          ? 'bg-cyber-surface-dark text-cyber-neon-green shadow-sm shadow-cyber-neon-green/10 border border-cyber-neon-green/50'
                          : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
                      }`}
                    >
                      Sell
                    </button>
                  </div>

                  {/* Token selector */}
                  <div className="space-y-2">
                    <label className="text-sm font-medium text-cyber-text-secondary">Select Token</label>
                    <div className="grid grid-cols-2 gap-4">
                      <button
                        onClick={() => setSelectedToken('BEAR')}
                        className={`relative p-4 rounded-xl text-center transition-all ${
                          selectedToken === 'BEAR'
                            ? 'border-2 border-cyber-bright-blue bg-cyber-bright-blue/10 shadow-md shadow-cyber-bright-blue/20'
                            : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-bright-blue/50 opacity-60 hover:opacity-100'
                        }`}
                      >
                        <div className={`font-semibold ${selectedToken === 'BEAR' ? 'text-cyber-bright-blue' : 'text-cyber-text-primary'}`}>DXY-BEAR</div>
                        <div className={`text-xs mt-1 ${selectedToken === 'BEAR' ? 'text-cyber-bright-blue/70' : 'text-cyber-text-secondary'}`}>Bearish on USD</div>
                        {selectedToken === 'BEAR' && (
                          <div className="absolute top-2 right-2 w-2 h-2 rounded-full bg-cyber-neon-green shadow-md shadow-cyber-neon-green/50" />
                        )}
                      </button>
                      <button
                        onClick={() => setSelectedToken('BULL')}
                        className={`relative p-4 rounded-xl text-center transition-all ${
                          selectedToken === 'BULL'
                            ? 'border-2 border-cyber-bright-blue bg-cyber-bright-blue/10 shadow-md shadow-cyber-bright-blue/20'
                            : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-bright-blue/50 opacity-60 hover:opacity-100'
                        }`}
                      >
                        <div className={`font-semibold ${selectedToken === 'BULL' ? 'text-cyber-bright-blue' : 'text-cyber-text-primary'}`}>DXY-BULL</div>
                        <div className={`text-xs mt-1 ${selectedToken === 'BULL' ? 'text-cyber-bright-blue/70' : 'text-cyber-text-secondary'}`}>Bullish on USD</div>
                        {selectedToken === 'BULL' && (
                          <div className="absolute top-2 right-2 w-2 h-2 rounded-full bg-cyber-neon-green shadow-md shadow-cyber-neon-green/50" />
                        )}
                      </button>
                    </div>
                  </div>

                  {/* Input amount */}
                  <div className="space-y-4">
                    <div className="space-y-2">
                      <div className="flex justify-between text-sm">
                        <span className="text-cyber-text-secondary">{mode === 'buy' ? 'You pay' : 'You sell'}</span>
                        <span className="text-cyber-text-secondary">
                          Balance: <span className="text-cyber-text-primary">{formatUsd(inputBalance)}</span>
                        </span>
                      </div>
                      <div className="relative">
                        <input
                          type="number"
                          value={inputAmount}
                          onChange={(e) => setInputAmount(e.target.value)}
                          placeholder="0.00"
                          className="w-full bg-cyber-surface-light border border-cyber-border-glow/30 rounded-xl py-4 pl-4 pr-24 text-xl font-medium text-cyber-text-primary focus:ring-1 focus:ring-cyber-bright-blue focus:border-cyber-bright-blue outline-none transition-shadow shadow-sm shadow-cyber-border-glow/10"
                        />
                        <div className="absolute right-4 top-1/2 -translate-y-1/2 flex items-center gap-2">
                          <button
                            onClick={() => setInputAmount((Number(inputBalance) / 1e6).toString())}
                            className="text-xs font-semibold text-cyber-neon-green hover:text-cyber-neon-green/80 px-2 py-1 rounded bg-cyber-neon-green/10 shadow-sm shadow-cyber-neon-green/10"
                          >
                            MAX
                          </button>
                          <span className="font-medium text-cyber-text-secondary">{inputToken.symbol}</span>
                        </div>
                      </div>
                    </div>

                    {/* Arrow divider */}
                    <div className="flex justify-center -my-2 z-10 relative">
                      <div className="bg-cyber-surface-light p-2 rounded-full border border-cyber-border-glow/30 shadow-sm shadow-cyber-border-glow/10">
                        <span className="material-symbols-outlined text-cyber-bright-blue text-lg block">arrow_downward</span>
                      </div>
                    </div>

                    {/* Output display */}
                    <div className="space-y-2">
                      <div className="flex justify-between text-sm">
                        <span className="text-cyber-text-secondary">You receive</span>
                      </div>
                      <div className="relative">
                        <div className="w-full bg-cyber-surface-dark border border-cyber-border-glow/30 rounded-xl py-4 pl-4 pr-24 text-xl font-medium text-cyber-text-primary flex items-center h-[62px] shadow-sm shadow-cyber-border-glow/10">
                          {outputAmount}
                        </div>
                        <div className="absolute right-4 top-1/2 -translate-y-1/2 flex items-center gap-2">
                          <span className="font-medium text-cyber-text-secondary">{outputToken.symbol}</span>
                        </div>
                      </div>
                    </div>
                  </div>

                  {/* Slippage settings */}
                  <div className="flex items-center justify-end gap-2 text-xs text-cyber-text-secondary">
                    <span className="material-symbols-outlined text-[14px]">settings</span>
                    <span>1% slippage</span>
                  </div>

                  {/* Swap details */}
                  <div className="border-t border-cyber-border-glow/30 pt-4">
                    <button
                      onClick={() => setShowDetails(!showDetails)}
                      className="w-full flex justify-between items-center text-sm text-cyber-text-secondary hover:text-cyber-bright-blue"
                    >
                      <span>Swap details</span>
                      <span className="material-symbols-outlined text-lg">{showDetails ? 'expand_less' : 'expand_more'}</span>
                    </button>
                  </div>

                  {showDetails && (
                    <div className="bg-cyber-surface-light rounded-lg p-3 space-y-2 text-sm border border-cyber-border-glow/30">
                      <div className="flex justify-between">
                        <span className="text-cyber-text-secondary">Route</span>
                        <span className="text-cyber-text-primary">
                          {selectedToken === 'BEAR' ? 'USDC → Curve → DXY-BEAR' : 'USDC → ZapRouter → DXY-BULL'}
                        </span>
                      </div>
                      <div className="flex justify-between">
                        <span className="text-cyber-text-secondary">Price Impact</span>
                        <span className="text-cyber-text-primary">~0.1%</span>
                      </div>
                      <div className="flex justify-between">
                        <span className="text-cyber-text-secondary">Estimated Gas</span>
                        <span className="text-cyber-text-primary">~$2.50</span>
                      </div>
                    </div>
                  )}

                  {/* Action button */}
                  <button
                    onClick={handleSwap}
                    disabled={!inputAmount || parseFloat(inputAmount) <= 0}
                    className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-4 px-6 rounded-xl shadow-lg shadow-cyber-neon-green/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
                  >
                    {mode === 'buy' ? 'Buy' : 'Sell'} DXY-{selectedToken}
                  </button>
                </div>
              )}

              {mainTab === 'leverage' && (
                <div className="max-w-xl mx-auto space-y-6">
                  {/* Side selector */}
                  <div className="space-y-2">
                    <label className="text-sm font-medium text-cyber-text-secondary">Position Side</label>
                    <div className="grid grid-cols-2 gap-4">
                      <button
                        onClick={() => setSelectedSide('BEAR')}
                        className={`relative p-4 rounded-xl text-center transition-all ${
                          selectedSide === 'BEAR'
                            ? 'border-2 border-cyber-electric-fuchsia bg-cyber-electric-fuchsia/10 shadow-md shadow-cyber-electric-fuchsia/20'
                            : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-electric-fuchsia/50 opacity-60 hover:opacity-100'
                        }`}
                      >
                        <div className={`font-semibold ${selectedSide === 'BEAR' ? 'text-cyber-electric-fuchsia' : 'text-cyber-text-primary'}`}>BEAR</div>
                        <div className={`text-xs mt-1 ${selectedSide === 'BEAR' ? 'text-cyber-electric-fuchsia/70' : 'text-cyber-text-secondary'}`}>Short USD</div>
                      </button>
                      <button
                        onClick={() => setSelectedSide('BULL')}
                        className={`relative p-4 rounded-xl text-center transition-all ${
                          selectedSide === 'BULL'
                            ? 'border-2 border-cyber-neon-green bg-cyber-neon-green/10 shadow-md shadow-cyber-neon-green/20'
                            : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-neon-green/50 opacity-60 hover:opacity-100'
                        }`}
                      >
                        <div className={`font-semibold ${selectedSide === 'BULL' ? 'text-cyber-neon-green' : 'text-cyber-text-primary'}`}>BULL</div>
                        <div className={`text-xs mt-1 ${selectedSide === 'BULL' ? 'text-cyber-neon-green/70' : 'text-cyber-text-secondary'}`}>Long USD</div>
                      </button>
                    </div>
                  </div>

                  {/* Collateral input */}
                  <div className="space-y-2">
                    <div className="flex justify-between text-sm">
                      <span className="text-cyber-text-secondary">Collateral (USDC)</span>
                      <span className="text-cyber-text-secondary">
                        Balance: <span className="text-cyber-text-primary">{formatUsd(usdcBalance)}</span>
                      </span>
                    </div>
                    <div className="relative">
                      <input
                        type="number"
                        value={collateralAmount}
                        onChange={(e) => setCollateralAmount(e.target.value)}
                        placeholder="0.00"
                        className="w-full bg-cyber-surface-light border border-cyber-border-glow/30 rounded-xl py-4 pl-4 pr-24 text-xl font-medium text-cyber-text-primary focus:ring-1 focus:ring-cyber-bright-blue focus:border-cyber-bright-blue outline-none transition-shadow shadow-sm shadow-cyber-border-glow/10"
                      />
                      <div className="absolute right-4 top-1/2 -translate-y-1/2 flex items-center gap-2">
                        <button
                          onClick={() => setCollateralAmount((Number(usdcBalance) / 1e6).toString())}
                          className="text-xs font-semibold text-cyber-neon-green hover:text-cyber-neon-green/80 px-2 py-1 rounded bg-cyber-neon-green/10 shadow-sm shadow-cyber-neon-green/10"
                        >
                          MAX
                        </button>
                        <span className="font-medium text-cyber-text-secondary">USDC</span>
                      </div>
                    </div>
                  </div>

                  {/* Leverage slider */}
                  <div>
                    <div className="flex items-center justify-between mb-2">
                      <label className="text-sm text-cyber-text-secondary flex items-center gap-1">
                        Leverage
                        <InfoTooltip content="Higher leverage increases both potential profits and liquidation risk" />
                      </label>
                      <span className="text-cyber-text-primary font-medium">{leverage}x</span>
                    </div>
                    <input
                      type="range"
                      min="1.1"
                      max="5"
                      step="0.1"
                      value={leverage}
                      onChange={(e) => setLeverage(parseFloat(e.target.value))}
                      className="w-full h-2 bg-cyber-surface-light rounded-lg appearance-none cursor-pointer accent-cyber-bright-blue"
                    />
                    <div className="flex justify-between text-xs text-cyber-text-secondary mt-1">
                      <span>1.1x</span>
                      <span>5x</span>
                    </div>
                  </div>

                  {/* Position preview */}
                  <div className="bg-cyber-surface-light rounded-lg p-4 space-y-3 border border-cyber-border-glow/30">
                    <h4 className="text-sm font-medium text-cyber-text-secondary">Position Preview</h4>
                    <div className="flex justify-between">
                      <span className="text-cyber-text-secondary text-sm">Position Size</span>
                      <span className="text-cyber-text-primary">{formatUsd(BigInt(Math.floor(positionSize * 1e6)))}</span>
                    </div>
                    <div className="flex justify-between">
                      <span className="text-cyber-text-secondary text-sm">Collateral</span>
                      <span className="text-cyber-text-primary">{formatUsd(BigInt(Math.floor(collateralNum * 1e6)))}</span>
                    </div>
                    <div className="flex justify-between">
                      <span className="text-cyber-text-secondary text-sm flex items-center gap-1">
                        Liquidation Price
                        <InfoTooltip content="If DXY reaches this price, your position will be liquidated" />
                      </span>
                      <span className="text-cyber-warning-text">${liquidationPrice}</span>
                    </div>
                  </div>

                  {/* Action button */}
                  <button
                    onClick={handleOpenPosition}
                    disabled={!collateralAmount || parseFloat(collateralAmount) <= 0}
                    className="w-full bg-cyber-bright-blue hover:bg-cyber-bright-blue/90 text-cyber-bg font-semibold py-4 px-6 rounded-xl shadow-lg shadow-cyber-bright-blue/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
                  >
                    Open {selectedSide} Position
                  </button>

                  <p className="text-xs text-cyber-text-secondary text-center">
                    Leverage trading carries significant risk. You may lose your entire collateral.
                  </p>
                </div>
              )}

              {mainTab === 'yield' && (
                <div className="max-w-xl mx-auto space-y-6">
                  {/* Overview stats */}
                  <div className="grid grid-cols-3 gap-4">
                    <div className="bg-cyber-surface-light rounded-lg p-3 border border-cyber-border-glow/30">
                      <p className="text-xs text-cyber-text-secondary">Supplied</p>
                      <p className="text-lg font-bold text-cyber-text-primary">{formatUsd(suppliedAmount)}</p>
                      <p className="text-xs text-cyber-neon-green">+{formatPercent(supplyApy)} APY</p>
                    </div>
                    <div className="bg-cyber-surface-light rounded-lg p-3 border border-cyber-border-glow/30">
                      <p className="text-xs text-cyber-text-secondary">Borrowed</p>
                      <p className="text-lg font-bold text-cyber-text-primary">{formatUsd(borrowedAmount)}</p>
                      <p className="text-xs text-cyber-warning-text">-{formatPercent(borrowApy)} APY</p>
                    </div>
                    <div className="bg-cyber-surface-light rounded-lg p-3 border border-cyber-border-glow/30">
                      <p className="text-xs text-cyber-text-secondary">Available</p>
                      <p className="text-lg font-bold text-cyber-text-primary">{formatUsd(availableToBorrow)}</p>
                      <p className="text-xs text-cyber-text-secondary">to borrow</p>
                    </div>
                  </div>

                  {/* Supply section */}
                  <div className="bg-cyber-surface-light rounded-lg p-4 border border-cyber-border-glow/30">
                    <div className="flex items-center justify-between mb-4">
                      <h4 className="font-medium text-cyber-text-primary">Supply USDC</h4>
                      <span className="text-sm text-cyber-neon-green">{formatPercent(supplyApy)} APY</span>
                    </div>
                    <div className="bg-cyber-surface-dark p-1 rounded-lg flex text-sm font-medium mb-4 border border-cyber-border-glow/30">
                      <button
                        onClick={() => { setSupplyMode('supply'); setSupplyAmount('') }}
                        className={`flex-1 py-2 px-4 rounded-md transition-all ${
                          supplyMode === 'supply'
                            ? 'bg-cyber-surface-light text-cyber-neon-green shadow-sm shadow-cyber-neon-green/10 border border-cyber-neon-green/50'
                            : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
                        }`}
                      >
                        Supply
                      </button>
                      <button
                        onClick={() => { setSupplyMode('withdraw'); setSupplyAmount('') }}
                        className={`flex-1 py-2 px-4 rounded-md transition-all ${
                          supplyMode === 'withdraw'
                            ? 'bg-cyber-surface-light text-cyber-neon-green shadow-sm shadow-cyber-neon-green/10 border border-cyber-neon-green/50'
                            : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
                        }`}
                      >
                        Withdraw
                      </button>
                    </div>
                    <div className="space-y-4">
                      <div className="relative">
                        <input
                          type="number"
                          value={supplyAmount}
                          onChange={(e) => setSupplyAmount(e.target.value)}
                          placeholder="0.00"
                          className="w-full bg-cyber-surface-dark border border-cyber-border-glow/30 rounded-xl py-3 pl-4 pr-20 text-lg font-medium text-cyber-text-primary focus:ring-1 focus:ring-cyber-bright-blue focus:border-cyber-bright-blue outline-none"
                        />
                        <div className="absolute right-4 top-1/2 -translate-y-1/2">
                          <span className="font-medium text-cyber-text-secondary">USDC</span>
                        </div>
                      </div>
                      <button
                        disabled={!supplyAmount || parseFloat(supplyAmount) <= 0}
                        className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-3 px-6 rounded-xl shadow-lg shadow-cyber-neon-green/40 transition-all disabled:opacity-50 disabled:cursor-not-allowed"
                      >
                        {supplyMode === 'supply' ? 'Supply' : 'Withdraw'} USDC
                      </button>
                    </div>
                  </div>

                  {/* Borrow section */}
                  <div className="bg-cyber-surface-light rounded-lg p-4 border border-cyber-border-glow/30">
                    <div className="flex items-center justify-between mb-4">
                      <h4 className="font-medium text-cyber-text-primary">Borrow USDC</h4>
                      <span className="text-sm text-cyber-warning-text">{formatPercent(borrowApy)} APY</span>
                    </div>
                    <div className="bg-cyber-surface-dark p-1 rounded-lg flex text-sm font-medium mb-4 border border-cyber-border-glow/30">
                      <button
                        onClick={() => { setBorrowMode('borrow'); setBorrowAmount('') }}
                        className={`flex-1 py-2 px-4 rounded-md transition-all ${
                          borrowMode === 'borrow'
                            ? 'bg-cyber-surface-light text-cyber-electric-fuchsia shadow-sm shadow-cyber-electric-fuchsia/10 border border-cyber-electric-fuchsia/50'
                            : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
                        }`}
                      >
                        Borrow
                      </button>
                      <button
                        onClick={() => { setBorrowMode('repay'); setBorrowAmount('') }}
                        className={`flex-1 py-2 px-4 rounded-md transition-all ${
                          borrowMode === 'repay'
                            ? 'bg-cyber-surface-light text-cyber-electric-fuchsia shadow-sm shadow-cyber-electric-fuchsia/10 border border-cyber-electric-fuchsia/50'
                            : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
                        }`}
                      >
                        Repay
                      </button>
                    </div>
                    <div className="space-y-4">
                      <div className="relative">
                        <input
                          type="number"
                          value={borrowAmount}
                          onChange={(e) => setBorrowAmount(e.target.value)}
                          placeholder="0.00"
                          className="w-full bg-cyber-surface-dark border border-cyber-border-glow/30 rounded-xl py-3 pl-4 pr-20 text-lg font-medium text-cyber-text-primary focus:ring-1 focus:ring-cyber-bright-blue focus:border-cyber-bright-blue outline-none"
                        />
                        <div className="absolute right-4 top-1/2 -translate-y-1/2">
                          <span className="font-medium text-cyber-text-secondary">USDC</span>
                        </div>
                      </div>
                      {borrowMode === 'borrow' && (
                        <p className="text-xs text-cyber-text-secondary">
                          Borrowing requires staked collateral (sDXY-BEAR or sDXY-BULL)
                        </p>
                      )}
                      <button
                        disabled={!borrowAmount || parseFloat(borrowAmount) <= 0}
                        className="w-full bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/90 text-cyber-text-primary font-semibold py-3 px-6 rounded-xl shadow-lg shadow-cyber-electric-fuchsia/40 transition-all disabled:opacity-50 disabled:cursor-not-allowed"
                      >
                        {borrowMode === 'borrow' ? 'Borrow' : 'Repay'} USDC
                      </button>
                    </div>
                  </div>
                </div>
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
        <div className="bg-cyber-surface-dark rounded-xl p-12 text-center border border-cyber-border-glow/30 shadow-lg">
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

interface PortfolioCardProps {
  title: string
  value: bigint
  description: string
  link: string
  isLoading: boolean
  colorClass: string
}

function PortfolioCard({ title, value, description, link, isLoading, colorClass }: PortfolioCardProps) {
  if (isLoading) {
    return <SkeletonCard />
  }

  return (
    <Link to={link}>
      <div className="bg-cyber-surface-dark rounded-xl p-5 border border-cyber-border-glow/30 shadow-md hover:border-cyber-bright-blue/50 transition-colors cursor-pointer h-full">
        <p className="text-xs text-cyber-text-secondary uppercase tracking-wider font-medium mb-2">{title}</p>
        <div className={`text-2xl font-bold mb-1 ${colorClass}`}>{formatUsd(value)}</div>
        <p className="text-xs text-cyber-text-secondary truncate">{description}</p>
      </div>
    </Link>
  )
}

interface PositionCardProps {
  position: LeveragePosition
  onAdjust: () => void
}

function PositionCard({ position, onAdjust }: PositionCardProps) {
  const sideColor = position.side === 'BEAR' ? 'text-cyber-electric-fuchsia' : 'text-cyber-neon-green'
  const sideBg = position.side === 'BEAR' ? 'bg-cyber-electric-fuchsia/20' : 'bg-cyber-neon-green/20'
  const sideShadow = position.side === 'BEAR' ? 'shadow-cyber-electric-fuchsia/10' : 'shadow-cyber-neon-green/10'
  const pnlColor = position.pnl >= 0n ? 'text-cyber-neon-green' : 'text-cyber-electric-fuchsia'
  const healthColor = position.healthFactor >= HEALTH_FACTOR_WARNING
    ? 'text-cyber-neon-green'
    : position.healthFactor >= HEALTH_FACTOR_DANGER
      ? 'text-cyber-warning-text'
      : 'text-cyber-electric-fuchsia'

  return (
    <div className="bg-cyber-surface-dark rounded-lg p-4 border border-cyber-border-glow/30 hover:border-cyber-bright-blue/50 transition-all shadow-md">
      <div className="flex flex-col md:flex-row md:items-center justify-between gap-4">
        <div className="flex items-center gap-4">
          <div className={`w-10 h-10 rounded ${sideBg} ${sideColor} flex items-center justify-center font-bold text-lg shadow-md ${sideShadow}`}>
            {position.side[0]}
          </div>
          <div>
            <div className="flex items-center gap-2">
              <span className={`font-semibold ${sideColor}`}>DXY-{position.side}</span>
              <span className="px-1.5 py-0.5 rounded bg-cyber-surface-light text-xs text-cyber-text-secondary font-medium border border-cyber-border-glow/30">
                {position.leverage}x
              </span>
            </div>
            <div className="text-xs text-cyber-text-secondary mt-1">
              Size: {formatUsd(position.size)} | Collateral: {formatUsd(position.collateral)}
            </div>
          </div>
        </div>

        <div className="flex flex-wrap items-center gap-6 lg:gap-12 flex-1 md:justify-end">
          <div className="flex flex-col">
            <span className="text-xs text-cyber-text-secondary mb-1">PnL</span>
            <span className={`text-sm font-semibold ${pnlColor}`}>
              {formatUsd(position.pnl)} ({position.pnlPercentage > 0 ? '+' : ''}{formatPercent(position.pnlPercentage)})
            </span>
          </div>
          <div className="flex flex-col">
            <span className="text-xs text-cyber-text-secondary mb-1">Liq. Price</span>
            <span className="text-sm font-semibold text-cyber-text-primary">
              ${(Number(position.liquidationPrice) / 1e6).toFixed(2)}
            </span>
          </div>
          <div className="flex flex-col">
            <div className="flex items-center gap-1 text-xs text-cyber-text-secondary mb-1">
              Health
              <span className="material-symbols-outlined text-[10px] text-cyber-text-secondary">help</span>
            </div>
            <span className={`text-sm font-semibold ${healthColor}`}>
              {position.healthFactor.toFixed(2)}
            </span>
          </div>

          <div className="flex items-center gap-2 mt-2 md:mt-0">
            <button
              onClick={onAdjust}
              className="px-3 py-1.5 rounded text-sm border border-cyber-border-glow/30 text-cyber-text-secondary hover:bg-cyber-surface-light hover:text-cyber-bright-blue transition-colors"
            >
              Adjust
            </button>
            <button className="px-3 py-1.5 rounded text-sm bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/80 text-cyber-text-primary transition-colors shadow-md shadow-cyber-electric-fuchsia/20">
              Close
            </button>
          </div>
        </div>
      </div>
    </div>
  )
}

interface AdjustPositionModalProps {
  isOpen: boolean
  onClose: () => void
  position: LeveragePosition
}

function AdjustPositionModal({ isOpen, onClose, position }: AdjustPositionModalProps) {
  const [action, setAction] = useState<'add' | 'remove' | 'adjust'>('add')
  const [amount, setAmount] = useState('')

  return (
    <Modal isOpen={isOpen} onClose={onClose} title={`Adjust ${position.side} Position`}>
      <div className="space-y-4">
        <div className="flex gap-2">
          <button
            onClick={() => setAction('add')}
            className={`flex-1 py-2 px-3 rounded text-sm font-medium transition-colors ${
              action === 'add'
                ? 'bg-cyber-neon-green/20 text-cyber-neon-green border border-cyber-neon-green/50'
                : 'bg-cyber-surface-light text-cyber-text-secondary border border-cyber-border-glow/30 hover:text-cyber-bright-blue'
            }`}
          >
            Add Collateral
          </button>
          <button
            onClick={() => setAction('remove')}
            className={`flex-1 py-2 px-3 rounded text-sm font-medium transition-colors ${
              action === 'remove'
                ? 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia border border-cyber-electric-fuchsia/50'
                : 'bg-cyber-surface-light text-cyber-text-secondary border border-cyber-border-glow/30 hover:text-cyber-bright-blue'
            }`}
          >
            Remove
          </button>
          <button
            onClick={() => setAction('adjust')}
            className={`flex-1 py-2 px-3 rounded text-sm font-medium transition-colors ${
              action === 'adjust'
                ? 'bg-cyber-bright-blue/20 text-cyber-bright-blue border border-cyber-bright-blue/50'
                : 'bg-cyber-surface-light text-cyber-text-secondary border border-cyber-border-glow/30 hover:text-cyber-bright-blue'
            }`}
          >
            Adjust Leverage
          </button>
        </div>

        {(action === 'add' || action === 'remove') && (
          <div className="relative">
            <input
              type="number"
              value={amount}
              onChange={(e) => setAmount(e.target.value)}
              placeholder="0.00"
              className="w-full bg-cyber-surface-light border border-cyber-border-glow/30 rounded-xl py-3 pl-4 pr-20 text-lg font-medium text-cyber-text-primary focus:ring-1 focus:ring-cyber-bright-blue focus:border-cyber-bright-blue outline-none"
            />
            <div className="absolute right-4 top-1/2 -translate-y-1/2">
              <span className="font-medium text-cyber-text-secondary">USDC</span>
            </div>
          </div>
        )}

        {action === 'adjust' && (
          <div>
            <label className="block text-sm text-cyber-text-secondary mb-2">New Leverage</label>
            <input
              type="range"
              min="1.1"
              max="5"
              step="0.1"
              defaultValue={position.leverage}
              className="w-full h-2 bg-cyber-surface-light rounded-lg appearance-none cursor-pointer accent-cyber-bright-blue"
            />
          </div>
        )}

        <div className="bg-cyber-surface-light rounded-lg p-3 space-y-2 text-sm border border-cyber-border-glow/30">
          <div className="flex justify-between">
            <span className="text-cyber-text-secondary">New Liquidation Price</span>
            <span className="text-cyber-warning-text">$92.50</span>
          </div>
          <div className="flex justify-between">
            <span className="text-cyber-text-secondary">New Health Factor</span>
            <span className="text-cyber-neon-green">2.1</span>
          </div>
        </div>

        <button className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-3 px-6 rounded-xl shadow-lg shadow-cyber-neon-green/40 transition-all">
          Confirm {action === 'add' ? 'Add' : action === 'remove' ? 'Remove' : 'Adjust'}
        </button>
      </div>
    </Modal>
  )
}
