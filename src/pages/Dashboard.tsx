import { useState } from 'react'
import { useAccount } from 'wagmi'
import { Card, Button, SkeletonCard, Tabs, InfoTooltip, Badge, Modal } from '../components/ui'
import { TokenInput } from '../components/TokenInput'
import { SlippageSelector } from '../components/SlippageSelector'
import { formatUsd, formatPercent, getHealthFactorColor } from '../utils/formatters'
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

  // Trade state
  const [mode, setMode] = useState<TradeMode>('buy')
  const [selectedToken, setSelectedToken] = useState<TokenSide>('BEAR')
  const [inputAmount, setInputAmount] = useState('')
  const [showDetails, setShowDetails] = useState(false)

  // Leverage state
  const [selectedSide, setSelectedSide] = useState<TokenSide>('BEAR')
  const [collateralAmount, setCollateralAmount] = useState('')
  const [leverage, setLeverage] = useState(2)

  // Yield state
  const [supplyMode, setSupplyMode] = useState<'supply' | 'withdraw'>('supply')
  const [borrowMode, setBorrowMode] = useState<'borrow' | 'repay'>('borrow')
  const [supplyAmount, setSupplyAmount] = useState('')
  const [borrowAmount, setBorrowAmount] = useState('')

  // Mock yield data
  const suppliedAmount = 5000n * 10n ** 6n
  const borrowedAmount = 1000n * 10n ** 6n
  const supplyApy = 3.5
  const borrowApy = 5.2
  const availableToBorrow = 3000n * 10n ** 6n

  // Positions state
  const [selectedPosition, setSelectedPosition] = useState<LeveragePosition | null>(null)
  const [adjustModalOpen, setAdjustModalOpen] = useState(false)
  const positions = mockPositions
  const hasLowHealth = positions.some((p) => p.healthFactor < HEALTH_FACTOR_WARNING)

  // Mock balances - replace with actual hooks
  const usdcBalance = 10000n * 10n ** 6n
  const bearBalance = 500n * 10n ** 18n
  const bullBalance = 500n * 10n ** 18n

  // Trade calculations
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

  // Leverage calculations
  const collateralNum = parseFloat(collateralAmount) || 0
  const positionSize = collateralNum * leverage
  const liquidationPrice = collateralNum > 0 ? (103.45 * (1 - 1 / leverage)).toFixed(2) : '0.00'

  const handleSwap = async () => {
    console.log('Swap:', { mode, selectedToken, inputAmount })
  }

  const handleOpenPosition = async () => {
    console.log('Open position:', { selectedSide, collateralAmount, leverage })
  }

  // TODO: Replace with actual hooks
  const isLoading = false
  const spotValue = 5000n * 10n ** 6n
  const stakedValue = 3000n * 10n ** 6n
  const leverageValue = 1500n * 10n ** 6n
  const lendingValue = 500n * 10n ** 6n

  return (
    <div className="space-y-6">
      {/* Page title */}
      <div>
        <h1 className="text-2xl font-bold text-white">Dashboard</h1>
        <p className="text-gray-400">Your portfolio overview</p>
      </div>

      {/* Portfolio overview */}
      {isConnected ? (
        <>
          {/* Portfolio breakdown */}
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            <PortfolioCard
              title="Spot Holdings"
              value={spotValue}
              description="USDC, DXY-BEAR, DXY-BULL"
              link="/trade"
              isLoading={isLoading}
            />
            <PortfolioCard
              title="Staked"
              value={stakedValue}
              description="sDXY-BEAR, sDXY-BULL"
              link="/stake"
              isLoading={isLoading}
            />
            <PortfolioCard
              title="Leverage"
              value={leverageValue}
              description="Open positions"
              link="/leverage"
              isLoading={isLoading}
            />
            <PortfolioCard
              title="Lending"
              value={lendingValue}
              description="Morpho supplied"
              link="/yield"
              isLoading={isLoading}
            />
          </div>

          {/* Positions section */}
          {positions.length > 0 && (
            <div className="space-y-4">
              <div className="flex items-center justify-between">
                <h2 className="text-lg font-semibold text-white">Open Positions</h2>
              </div>

              {hasLowHealth && (
                <div className="bg-yellow-900/30 border border-yellow-800 rounded-lg p-4 flex items-center gap-3">
                  <svg className="w-6 h-6 text-yellow-500 flex-shrink-0" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
                  </svg>
                  <div>
                    <p className="text-yellow-400 font-medium">Low Health Factor Warning</p>
                    <p className="text-yellow-300/70 text-sm">
                      One or more positions have low health factors and may be at risk of liquidation.
                    </p>
                  </div>
                </div>
              )}

              <div className="grid gap-4">
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

          {/* Trade / Leverage / Yield widget with folder tabs */}
          <div>
            {/* Folder tabs */}
            <div className="inline-flex">
              <button
                onClick={() => handleTabChange('trade')}
                className={`
                  w-80 px-4 py-4 rounded-t-xl transition-all relative text-left
                  ${mainTab === 'trade'
                    ? 'bg-blue-900 text-blue-300 border-t border-l border-r border-blue-700'
                    : 'bg-blue-950 text-blue-400 hover:text-blue-300 hover:bg-blue-900 border-t border-l border-r border-transparent'
                  }
                `}
              >
                <div className="flex items-center gap-3">
                  <svg className="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 12l2 2 4-4m5.618-4.016A11.955 11.955 0 0112 2.944a11.955 11.955 0 01-8.618 3.04A12.02 12.02 0 003 9c0 5.591 3.824 10.29 9 11.622 5.176-1.332 9-6.03 9-11.622 0-1.042-.133-2.052-.382-3.016z" />
                  </svg>
                  <div>
                    <span className="block text-xl font-semibold">Dollar Hedge</span>
                    <span className="block text-xs text-blue-400">Spot trading</span>
                  </div>
                </div>
              </button>
              <button
                onClick={() => handleTabChange('leverage')}
                className={`
                  w-80 px-4 py-4 rounded-t-xl transition-all relative text-left
                  ${mainTab === 'leverage'
                    ? 'bg-orange-900 text-orange-300 border-t border-l border-r border-orange-700'
                    : 'bg-orange-950 text-orange-400 hover:text-orange-300 hover:bg-orange-900 border-t border-l border-r border-transparent'
                  }
                `}
              >
                <div className="flex items-center gap-3">
                  <svg className="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 7h8m0 0v8m0-8l-8 8-4-4-6 6" />
                  </svg>
                  <div>
                    <span className="block text-xl font-semibold">Leverage</span>
                    <span className="block text-xs text-orange-400">Margin trading</span>
                  </div>
                </div>
              </button>
              <button
                onClick={() => handleTabChange('yield')}
                className={`
                  w-80 px-4 py-4 rounded-t-xl transition-all relative text-left
                  ${mainTab === 'yield'
                    ? 'bg-green-900 text-green-300 border-t border-l border-r border-green-700'
                    : 'bg-green-950 text-green-400 hover:text-green-300 hover:bg-green-900 border-t border-l border-r border-transparent'
                  }
                `}
              >
                <div className="flex items-center gap-3">
                  <svg className="w-6 h-6" viewBox="0 0 24 24" fill="currentColor">
                    <path d="M12 2c-1.5 2-2.5 4-2.5 6 0 2.5 1.5 4 2.5 5.5 1-1.5 2.5-3 2.5-5.5 0-2-1-4-2.5-6zm-4 8c-1 1.5-1.5 3-1.5 4.5 0 2 1 3.5 2 4.5.5-1 1.5-2 1.5-4 0-1.5-.5-3-2-5zm8 0c-1.5 2-2 3.5-2 5 0 2 1 3 1.5 4 1-1 2-2.5 2-4.5 0-1.5-.5-3-1.5-4.5zM12 15c-1 1.5-1.5 3-1.5 4.5 0 1.5.5 2.5 1.5 3.5 1-1 1.5-2 1.5-3.5 0-1.5-.5-3-1.5-4.5z" />
                  </svg>
                  <div>
                    <span className="block text-xl font-semibold">Yield</span>
                    <span className="block text-xs text-green-400">Liquidity providing</span>
                  </div>
                </div>
              </button>
            </div>

            {/* Tab content */}
            <div className={`
              rounded-b-xl rounded-tr-xl p-6 border
              ${mainTab === 'trade' ? 'bg-gradient-to-b from-blue-900 to-blue-950 border-blue-700' : ''}
              ${mainTab === 'leverage' ? 'bg-gradient-to-b from-orange-900 to-orange-950 border-orange-700' : ''}
              ${mainTab === 'yield' ? 'bg-gradient-to-b from-green-900 to-green-950 border-green-700' : ''}
            `}>
              {/* Trade content */}
              {mainTab === 'trade' && (
                <div className="max-w-md mx-auto">
                {/* Buy/Sell tabs */}
                <Tabs
                  tabs={[
                    { id: 'buy', label: 'Buy' },
                    { id: 'sell', label: 'Sell' },
                  ]}
                  activeTab={mode}
                  onChange={(id) => setMode(id as TradeMode)}
                />

                <div className="mt-6 space-y-4">
                  {/* Token selector */}
                  <div>
                    <label className="block text-sm text-gray-400 mb-2">Select Token</label>
                    <div className="flex gap-2">
                      <button
                        onClick={() => setSelectedToken('BEAR')}
                        className={`flex-1 px-4 py-3 rounded-lg border transition-colors ${
                          selectedToken === 'BEAR'
                            ? 'bg-bear/10 border-bear text-bear'
                            : 'bg-surface-200 border-gray-700 text-gray-300 hover:border-gray-600'
                        }`}
                      >
                        <span className="font-medium">DXY-BEAR</span>
                        <span className="block text-xs opacity-70">Bearish on USD</span>
                      </button>
                      <button
                        onClick={() => setSelectedToken('BULL')}
                        className={`flex-1 px-4 py-3 rounded-lg border transition-colors ${
                          selectedToken === 'BULL'
                            ? 'bg-bull/10 border-bull text-bull'
                            : 'bg-surface-200 border-gray-700 text-gray-300 hover:border-gray-600'
                        }`}
                      >
                        <span className="font-medium">DXY-BULL</span>
                        <span className="block text-xs opacity-70">Bullish on USD</span>
                      </button>
                    </div>
                  </div>

                  {/* Input amount */}
                  <TokenInput
                    label={mode === 'buy' ? 'You pay' : 'You sell'}
                    value={inputAmount}
                    onChange={setInputAmount}
                    token={inputToken}
                    balance={isConnected ? inputBalance : undefined}
                  />

                  {/* Arrow divider */}
                  <div className="flex justify-center">
                    <div className="w-10 h-10 rounded-full bg-surface-200 flex items-center justify-center">
                      <svg className="w-5 h-5 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 14l-7 7m0 0l-7-7m7 7V3" />
                      </svg>
                    </div>
                  </div>

                  {/* Output display */}
                  <div className="bg-surface-200 rounded-lg p-4">
                    <div className="flex justify-between items-center">
                      <span className="text-sm text-gray-400">You receive</span>
                      <span className="text-gray-400">{outputToken.symbol}</span>
                    </div>
                    <p className="text-2xl font-semibold text-white mt-1">
                      {outputAmount}
                    </p>
                  </div>

                  {/* Slippage settings */}
                  <div className="flex justify-end">
                    <SlippageSelector />
                  </div>

                  {/* Swap details (collapsible) */}
                  <button
                    onClick={() => setShowDetails(!showDetails)}
                    className="w-full flex items-center justify-between text-sm text-gray-400 hover:text-white transition-colors"
                  >
                    <span>Swap details</span>
                    <svg
                      className={`w-4 h-4 transition-transform ${showDetails ? 'rotate-180' : ''}`}
                      fill="none"
                      stroke="currentColor"
                      viewBox="0 0 24 24"
                    >
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 9l-7 7-7-7" />
                    </svg>
                  </button>

                  {showDetails && (
                    <div className="bg-surface-200 rounded-lg p-3 space-y-2 text-sm">
                      <div className="flex justify-between">
                        <span className="text-gray-400">Route</span>
                        <span className="text-white">
                          {selectedToken === 'BEAR'
                            ? 'USDC → Curve → DXY-BEAR'
                            : 'USDC → ZapRouter → DXY-BULL'}
                        </span>
                      </div>
                      <div className="flex justify-between">
                        <span className="text-gray-400">Price Impact</span>
                        <span className="text-white">~0.1%</span>
                      </div>
                      <div className="flex justify-between">
                        <span className="text-gray-400">Estimated Gas</span>
                        <span className="text-white">~$2.50</span>
                      </div>
                    </div>
                  )}

                  {/* Action button */}
                  {isConnected ? (
                    <Button
                      variant="primary"
                      className="w-full"
                      disabled={!inputAmount || parseFloat(inputAmount) <= 0}
                      onClick={handleSwap}
                    >
                      {mode === 'buy' ? 'Buy' : 'Sell'} DXY-{selectedToken}
                    </Button>
                  ) : (
                    <Button variant="secondary" className="w-full" disabled>
                      Connect Wallet to Trade
                    </Button>
                  )}
                </div>
              </div>
            )}

            {/* Leverage content */}
            {mainTab === 'leverage' && (
              <div className="max-w-md mx-auto space-y-4">
                {/* Side selector */}
                <div>
                  <label className="block text-sm text-gray-400 mb-2">Position Side</label>
                  <div className="flex gap-2">
                    <button
                      onClick={() => setSelectedSide('BEAR')}
                      className={`flex-1 px-4 py-3 rounded-lg border transition-colors ${
                        selectedSide === 'BEAR'
                          ? 'bg-bear/10 border-bear text-bear'
                          : 'bg-surface-200 border-gray-700 text-gray-300 hover:border-gray-600'
                      }`}
                    >
                      <span className="font-medium">BEAR</span>
                      <span className="block text-xs opacity-70">Short USD</span>
                    </button>
                    <button
                      onClick={() => setSelectedSide('BULL')}
                      className={`flex-1 px-4 py-3 rounded-lg border transition-colors ${
                        selectedSide === 'BULL'
                          ? 'bg-bull/10 border-bull text-bull'
                          : 'bg-surface-200 border-gray-700 text-gray-300 hover:border-gray-600'
                      }`}
                    >
                      <span className="font-medium">BULL</span>
                      <span className="block text-xs opacity-70">Long USD</span>
                    </button>
                  </div>
                </div>

                {/* Collateral input */}
                <TokenInput
                  label="Collateral (USDC)"
                  value={collateralAmount}
                  onChange={setCollateralAmount}
                  token={{ symbol: 'USDC', decimals: 6 }}
                  balance={isConnected ? usdcBalance : undefined}
                />

                {/* Leverage slider */}
                <div>
                  <div className="flex items-center justify-between mb-2">
                    <label className="text-sm text-gray-400 flex items-center gap-1">
                      Leverage
                      <InfoTooltip content="Higher leverage increases both potential profits and liquidation risk" />
                    </label>
                    <span className="text-white font-medium">{leverage}x</span>
                  </div>
                  <input
                    type="range"
                    min="1.1"
                    max="5"
                    step="0.1"
                    value={leverage}
                    onChange={(e) => setLeverage(parseFloat(e.target.value))}
                    className="w-full h-2 bg-surface-200 rounded-lg appearance-none cursor-pointer accent-primary-500"
                  />
                  <div className="flex justify-between text-xs text-gray-500 mt-1">
                    <span>1.1x</span>
                    <span>5x</span>
                  </div>
                </div>

                {/* Position preview */}
                <div className="bg-surface-200 rounded-lg p-4 space-y-3">
                  <h4 className="text-sm font-medium text-gray-400">Position Preview</h4>
                  <div className="flex justify-between">
                    <span className="text-gray-400 text-sm">Position Size</span>
                    <span className="text-white">{formatUsd(BigInt(Math.floor(positionSize * 1e6)))}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-gray-400 text-sm">Collateral</span>
                    <span className="text-white">{formatUsd(BigInt(Math.floor(collateralNum * 1e6)))}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-gray-400 text-sm flex items-center gap-1">
                      Liquidation Price
                      <InfoTooltip content="If DXY reaches this price, your position will be liquidated" />
                    </span>
                    <span className="text-yellow-500">${liquidationPrice}</span>
                  </div>
                </div>

                {/* Slippage */}
                <div className="flex justify-end">
                  <SlippageSelector />
                </div>

                {/* Action button */}
                {isConnected ? (
                  <Button
                    variant="primary"
                    className="w-full"
                    disabled={!collateralAmount || parseFloat(collateralAmount) <= 0}
                    onClick={handleOpenPosition}
                  >
                    Open {selectedSide} Position
                  </Button>
                ) : (
                  <Button variant="secondary" className="w-full" disabled>
                    Connect Wallet to Trade
                  </Button>
                )}

                {/* Risk warning */}
                <p className="text-xs text-gray-500 text-center">
                  Leverage trading carries significant risk. You may lose your entire collateral.
                </p>
              </div>
            )}

            {/* Yield content */}
            {mainTab === 'yield' && (
              <div className="max-w-md mx-auto space-y-6">
                {/* Overview stats */}
                <div className="grid grid-cols-3 gap-4">
                  <div className="bg-surface-200 rounded-lg p-3">
                    <p className="text-xs text-gray-400">Supplied</p>
                    <p className="text-lg font-bold text-white">{formatUsd(suppliedAmount)}</p>
                    <p className="text-xs text-green-500">+{formatPercent(supplyApy)} APY</p>
                  </div>
                  <div className="bg-surface-200 rounded-lg p-3">
                    <p className="text-xs text-gray-400">Borrowed</p>
                    <p className="text-lg font-bold text-white">{formatUsd(borrowedAmount)}</p>
                    <p className="text-xs text-yellow-500">-{formatPercent(borrowApy)} APY</p>
                  </div>
                  <div className="bg-surface-200 rounded-lg p-3">
                    <p className="text-xs text-gray-400">Available</p>
                    <p className="text-lg font-bold text-white">{formatUsd(availableToBorrow)}</p>
                    <p className="text-xs text-gray-500">to borrow</p>
                  </div>
                </div>

                {/* Supply section */}
                <div className="bg-surface-200 rounded-lg p-4">
                  <div className="flex items-center justify-between mb-4">
                    <h4 className="font-medium text-white">Supply USDC</h4>
                    <span className="text-sm text-green-500">{formatPercent(supplyApy)} APY</span>
                  </div>
                  <Tabs
                    tabs={[
                      { id: 'supply', label: 'Supply' },
                      { id: 'withdraw', label: 'Withdraw' },
                    ]}
                    activeTab={supplyMode}
                    onChange={(id) => {
                      setSupplyMode(id as 'supply' | 'withdraw')
                      setSupplyAmount('')
                    }}
                  />
                  <div className="mt-4 space-y-4">
                    <TokenInput
                      value={supplyAmount}
                      onChange={setSupplyAmount}
                      token={{ symbol: 'USDC', decimals: 6 }}
                      balance={isConnected ? (supplyMode === 'supply' ? usdcBalance : suppliedAmount) : undefined}
                      label={supplyMode === 'supply' ? 'Amount to supply' : 'Amount to withdraw'}
                    />
                    {isConnected ? (
                      <Button
                        variant="primary"
                        className="w-full"
                        disabled={!supplyAmount || parseFloat(supplyAmount) <= 0}
                        onClick={() => console.log(supplyMode, supplyAmount)}
                      >
                        {supplyMode === 'supply' ? 'Supply' : 'Withdraw'} USDC
                      </Button>
                    ) : (
                      <Button variant="secondary" className="w-full" disabled>
                        Connect Wallet
                      </Button>
                    )}
                  </div>
                </div>

                {/* Borrow section */}
                <div className="bg-surface-200 rounded-lg p-4">
                  <div className="flex items-center justify-between mb-4">
                    <h4 className="font-medium text-white">Borrow USDC</h4>
                    <span className="text-sm text-yellow-500">{formatPercent(borrowApy)} APY</span>
                  </div>
                  <Tabs
                    tabs={[
                      { id: 'borrow', label: 'Borrow' },
                      { id: 'repay', label: 'Repay' },
                    ]}
                    activeTab={borrowMode}
                    onChange={(id) => {
                      setBorrowMode(id as 'borrow' | 'repay')
                      setBorrowAmount('')
                    }}
                  />
                  <div className="mt-4 space-y-4">
                    <TokenInput
                      value={borrowAmount}
                      onChange={setBorrowAmount}
                      token={{ symbol: 'USDC', decimals: 6 }}
                      balance={isConnected ? (borrowMode === 'borrow' ? availableToBorrow : borrowedAmount) : undefined}
                      label={borrowMode === 'borrow' ? 'Amount to borrow' : 'Amount to repay'}
                    />
                    {borrowMode === 'borrow' && (
                      <p className="text-xs text-gray-500">
                        Borrowing requires staked collateral (sDXY-BEAR or sDXY-BULL)
                      </p>
                    )}
                    {isConnected ? (
                      <Button
                        variant="primary"
                        className="w-full"
                        disabled={!borrowAmount || parseFloat(borrowAmount) <= 0}
                        onClick={() => console.log(borrowMode, borrowAmount)}
                      >
                        {borrowMode === 'borrow' ? 'Borrow' : 'Repay'} USDC
                      </Button>
                    ) : (
                      <Button variant="secondary" className="w-full" disabled>
                        Connect Wallet
                      </Button>
                    )}
                  </div>
                </div>
              </div>
            )}
            </div>
          </div>

          {/* Transaction History link */}
          <div className="text-center">
            <Link
              to="/history"
              className="text-primary-500 hover:text-primary-400 text-sm"
            >
              View Transaction History →
            </Link>
          </div>

          {/* Adjust Position Modal */}
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
        /* Not connected state */
        <Card className="text-center py-12">
          <div className="w-16 h-16 mx-auto mb-4 rounded-full bg-surface-50 flex items-center justify-center">
            <svg className="w-8 h-8 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z" />
            </svg>
          </div>
          <h2 className="text-xl font-semibold text-white mb-2">Connect Your Wallet</h2>
          <p className="text-gray-400 mb-6 max-w-md mx-auto">
            Connect your wallet to view your portfolio, trade DXY-BEAR and DXY-BULL,
            and access all Plether features.
          </p>
          <p className="text-sm text-gray-500">
            You can browse prices and protocol stats without connecting.
          </p>
        </Card>
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
}

function PortfolioCard({ title, value, description, link, isLoading }: PortfolioCardProps) {
  if (isLoading) {
    return <SkeletonCard />
  }

  return (
    <Link to={link}>
      <Card className="hover:border-gray-700 transition-colors cursor-pointer h-full">
        <p className="text-sm text-gray-400 mb-1">{title}</p>
        <p className="text-2xl font-bold text-white mb-2">{formatUsd(value)}</p>
        <p className="text-xs text-gray-500">{description}</p>
      </Card>
    </Link>
  )
}

interface PositionCardProps {
  position: LeveragePosition
  onAdjust: () => void
}

function PositionCard({ position, onAdjust }: PositionCardProps) {
  const sideColor = position.side === 'BEAR' ? 'text-bear' : 'text-bull'
  const sideBg = position.side === 'BEAR' ? 'bg-bear/10' : 'bg-bull/10'
  const pnlColor = position.pnl >= 0n ? 'text-green-500' : 'text-red-500'
  const healthColor = getHealthFactorColor(position.healthFactor)

  const isWarning = position.healthFactor < HEALTH_FACTOR_WARNING
  const isDanger = position.healthFactor < HEALTH_FACTOR_DANGER

  return (
    <Card className={isDanger ? 'border-red-800' : isWarning ? 'border-yellow-800' : ''}>
      <div className="flex flex-col md:flex-row md:items-center justify-between gap-4">
        <div className="flex items-center gap-4">
          <div className={`w-12 h-12 rounded-lg ${sideBg} flex items-center justify-center`}>
            <span className={`font-bold ${sideColor}`}>{position.side[0]}</span>
          </div>
          <div>
            <div className="flex items-center gap-2">
              <span className={`font-semibold ${sideColor}`}>DXY-{position.side}</span>
              <Badge variant="default">{position.leverage}x</Badge>
            </div>
            <p className="text-sm text-gray-400">
              Size: {formatUsd(position.size)} | Collateral: {formatUsd(position.collateral)}
            </p>
          </div>
        </div>

        <div className="flex flex-wrap gap-6 text-sm">
          <div>
            <p className="text-gray-400">PnL</p>
            <p className={`font-medium ${pnlColor}`}>
              {formatUsd(position.pnl)} ({position.pnlPercentage > 0 ? '+' : ''}{formatPercent(position.pnlPercentage)})
            </p>
          </div>
          <div>
            <p className="text-gray-400">Liq. Price</p>
            <p className="text-white">${(Number(position.liquidationPrice) / 1e6).toFixed(2)}</p>
          </div>
          <div>
            <p className="text-gray-400 flex items-center gap-1">
              Health
              <InfoTooltip content="Health factor indicates position safety. Below 1.2 is high risk." />
            </p>
            <p className={`font-medium ${healthColor}`}>{position.healthFactor.toFixed(2)}</p>
          </div>
        </div>

        <div className="flex gap-2">
          <Button variant="secondary" size="sm" onClick={onAdjust}>
            Adjust
          </Button>
          <Button variant="danger" size="sm">
            Close
          </Button>
        </div>
      </div>
    </Card>
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
          <Button
            variant={action === 'add' ? 'primary' : 'secondary'}
            size="sm"
            onClick={() => setAction('add')}
          >
            Add Collateral
          </Button>
          <Button
            variant={action === 'remove' ? 'primary' : 'secondary'}
            size="sm"
            onClick={() => setAction('remove')}
          >
            Remove Collateral
          </Button>
          <Button
            variant={action === 'adjust' ? 'primary' : 'secondary'}
            size="sm"
            onClick={() => setAction('adjust')}
          >
            Adjust Leverage
          </Button>
        </div>

        {action === 'add' && (
          <TokenInput
            label="Add USDC Collateral"
            value={amount}
            onChange={setAmount}
            token={{ symbol: 'USDC', decimals: 6 }}
          />
        )}

        {action === 'remove' && (
          <TokenInput
            label="Remove USDC Collateral"
            value={amount}
            onChange={setAmount}
            token={{ symbol: 'USDC', decimals: 6 }}
          />
        )}

        {action === 'adjust' && (
          <div>
            <label className="block text-sm text-gray-400 mb-2">New Leverage</label>
            <input
              type="range"
              min="1.1"
              max="5"
              step="0.1"
              defaultValue={position.leverage}
              className="w-full"
            />
          </div>
        )}

        <div className="bg-surface-200 rounded-lg p-3 space-y-2 text-sm">
          <div className="flex justify-between">
            <span className="text-gray-400">New Liquidation Price</span>
            <span className="text-yellow-500">$92.50</span>
          </div>
          <div className="flex justify-between">
            <span className="text-gray-400">New Health Factor</span>
            <span className="text-green-500">2.1</span>
          </div>
        </div>

        <Button variant="primary" className="w-full">
          Confirm {action === 'add' ? 'Add' : action === 'remove' ? 'Remove' : 'Adjust'}
        </Button>
      </div>
    </Modal>
  )
}
