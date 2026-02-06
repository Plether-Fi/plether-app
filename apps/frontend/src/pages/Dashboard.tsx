import { useState } from 'react'
import { useAccount } from 'wagmi'
import { DashboardTile } from '../components/DashboardTile'
import { PositionsSection } from '../components/PositionsSection'
import { AdjustPositionModal } from '../components/AdjustPositionModal'
import { TradeCard } from '../components/TradeCard'
import { YieldCard } from '../components/YieldCard'
import { LeverageCard } from '../components/LeverageCard'
import { MainTabNav } from '../components/MainTabNav'
import { ConnectWalletPrompt } from '../components/ConnectWalletPrompt'
import { Link, useLocation, useNavigate } from 'react-router-dom'
import { useLeveragePosition, useTransactionSequence, useCombinedLendingPosition, useAvailableToBorrow, type TransactionStep } from '../hooks'
import { useUserDashboard, apiQueryKeys } from '../api'
import { useQueryClient } from '@tanstack/react-query'
import { useWriteContract } from 'wagmi'
import { LEVERAGE_ROUTER_ABI } from '../contracts/abis'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
import { useSettingsStore } from '../stores/settingsStore'
import type { LeveragePosition } from '../types'
import { getDeadline } from '../utils/deadline'

export function Dashboard() {
  const { isConnected, address, chainId } = useAccount()
  const location = useLocation()
  const navigate = useNavigate()
  const queryClient = useQueryClient()

  type MainTab = 'trade' | 'leverage' | 'lending'

  const mainTab: MainTab =
    location.pathname === '/leverage' ? 'leverage' :
    location.pathname === '/lending' ? 'lending' : 'trade'

  const handleTabChange = (tab: MainTab) => {
    if (tab === 'leverage') void navigate('/leverage')
    else if (tab === 'lending') void navigate('/lending')
    else void navigate('/')
  }

  const [selectedPosition, setSelectedPosition] = useState<LeveragePosition | null>(null)
  const [adjustModalOpen, setAdjustModalOpen] = useState(false)

  const { data: dashboardData, isLoading: dashboardLoading } = useUserDashboard(address)
  const balances = dashboardData?.data.balances

  const usdcBalance = balances ? BigInt(balances.usdc) : 0n
  const bearBalance = balances ? BigInt(balances.bear) : 0n
  const bullBalance = balances ? BigInt(balances.bull) : 0n
  const stakedBearAssets = balances ? BigInt(balances.stakedBearAssets) : 0n
  const stakedBullAssets = balances ? BigInt(balances.stakedBullAssets) : 0n

  const refetchBalances = () => {
    if (address) {
      void queryClient.invalidateQueries({ queryKey: apiQueryKeys.user.dashboard(address) })
    }
  }

  const bearPosition = useLeveragePosition('BEAR')
  const bullPosition = useLeveragePosition('BULL')

  const lendingPosition = useCombinedLendingPosition()
  const bearBorrowable = useAvailableToBorrow('BEAR')
  const bullBorrowable = useAvailableToBorrow('BULL')

  const slippage = useSettingsStore((s) => s.slippage)
  const addresses = getAddresses(chainId ?? DEFAULT_CHAIN_ID)
  const { writeContractAsync } = useWriteContract()
  const closeSequence = useTransactionSequence()

  const handleClosePosition = (position: LeveragePosition) => {
    const collateralToClose = position.side === 'BEAR' ? bearPosition.collateral : bullPosition.collateral
    const routerAddress = position.side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER

    const buildCloseSteps = (): TransactionStep[] => {
      const slippageBps = BigInt(Math.floor(slippage * 100))
      const deadline = getDeadline()

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
        refetchBalances()
      },
    })
  }

  const positions: LeveragePosition[] = []

  if (bearPosition.hasPosition && bearPosition.collateralUsdc > 0n) {
    const leverageNum = Number(bearPosition.leverage) / 100
    positions.push({
      id: 'bear-position',
      side: 'BEAR',
      size: bearPosition.collateralUsdc,
      collateral: bearPosition.collateralUsdc > bearPosition.debt ? bearPosition.collateralUsdc - bearPosition.debt : 0n,
      leverage: leverageNum,
      entryPrice: 0n,
      liquidationPrice: bearPosition.liquidationPrice,
      healthFactor: bearPosition.healthFactor,
      pnl: 0n,
      pnlPercentage: 0,
    })
  }

  if (bullPosition.hasPosition && bullPosition.collateralUsdc > 0n) {
    const leverageNum = Number(bullPosition.leverage) / 100
    positions.push({
      id: 'bull-position',
      side: 'BULL',
      size: bullPosition.collateralUsdc,
      collateral: bullPosition.collateralUsdc > bullPosition.debt ? bullPosition.collateralUsdc - bullPosition.debt : 0n,
      leverage: leverageNum,
      entryPrice: 0n,
      liquidationPrice: bullPosition.liquidationPrice,
      healthFactor: bullPosition.healthFactor,
      pnl: 0n,
      pnlPercentage: 0,
    })
  }

  const totalSupplied = lendingPosition.totalSupplied

  return (
    <div className="space-y-10">
      {/* Page title */}
      <div className="mb-8">
        <h1 className="text-3xl font-semibold text-cyber-text-primary mb-1">Dashboard</h1>
        <p className="text-cyber-text-secondary font-light">Your portfolio overview</p>
      </div>

      {/* Portfolio tiles - always visible */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 mb-10">
        <DashboardTile
          variant="BULL"
          title="Total BULL Position"
          balance={isConnected ? bullBalance + stakedBullAssets : 0n}
          balanceDecimals={18}
          balanceToken="plDXY-BULL"
          secondaryValue={isConnected ? stakedBullAssets : 0n}
          secondaryLabel="Staked Balance"
          secondaryDecimals={18}
          secondaryToken="plDXY-BULL"
          isLoading={isConnected && dashboardLoading}
        />
        <DashboardTile
          variant="USDC"
          title="Total USDC Position"
          balance={isConnected ? usdcBalance + totalSupplied : 0n}
          balanceDecimals={6}
          balanceToken="USDC"
          secondaryValue={isConnected ? totalSupplied : 0n}
          secondaryLabel="Total Lending"
          secondaryDecimals={6}
          secondaryToken="USDC"
          isLoading={isConnected && (dashboardLoading || lendingPosition.isLoading)}
        />
        <DashboardTile
          variant="BEAR"
          title="Total BEAR Position"
          balance={isConnected ? bearBalance + stakedBearAssets : 0n}
          balanceDecimals={18}
          balanceToken="plDXY-BEAR"
          secondaryValue={isConnected ? stakedBearAssets : 0n}
          secondaryLabel="Staked Balance"
          secondaryDecimals={18}
          secondaryToken="plDXY-BEAR"
          isLoading={isConnected && dashboardLoading}
        />
      </div>

      {/* Positions section - always visible */}
      <PositionsSection
        positions={isConnected ? positions : []}
        isLoading={isConnected && (bearPosition.isLoading || bullPosition.isLoading)}
        isClosing={closeSequence.isRunning}
        onAdjust={(position) => {
          setSelectedPosition(position)
          setAdjustModalOpen(true)
        }}
        onClose={handleClosePosition}
      />

      {isConnected ? (
        <>
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
                <LeverageCard
                  usdcBalance={usdcBalance}
                  refetchBalances={refetchBalances}
                  onPositionOpened={() => {
                    void bearPosition.refetch()
                    void bullPosition.refetch()
                  }}
                />
              )}

              {mainTab === 'lending' && (
                <YieldCard
                  bearMarket={{
                    suppliedAmount: lendingPosition.bearPosition.suppliedAssets,
                    borrowedAmount: lendingPosition.bearPosition.borrowedAssets,
                    availableToBorrow: bearBorrowable.availableToBorrow,
                    collateral: bearBorrowable.collateralUsd,
                  }}
                  bullMarket={{
                    suppliedAmount: lendingPosition.bullPosition.suppliedAssets,
                    borrowedAmount: lendingPosition.bullPosition.borrowedAssets,
                    availableToBorrow: bullBorrowable.availableToBorrow,
                    collateral: bullBorrowable.collateralUsd,
                  }}
                  usdcBalance={usdcBalance}
                  onSuccess={() => {
                    void lendingPosition.refetch()
                    void bearPosition.refetch()
                    void bullPosition.refetch()
                    refetchBalances()
                  }}
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

export default Dashboard
