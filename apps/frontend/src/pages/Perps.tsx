import { useEffect, useMemo, useState, type ReactNode } from 'react'
import { DxyBasketPanel } from '../components/DxyBasketPanel'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'
import { PerpsInstrumentPanel, type PerpsInstrumentStat } from '../components/PerpsInstrumentPanel'
import { PerpsMarketStatePanel } from '../components/PerpsMarketStatePanel'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import { TokenAmount } from '../components/ui'
import { usePerpsAccount, usePerpsHistory, usePerpsMarket } from '../hooks'

function displayValue(value: string | undefined, isLoading: boolean): string {
  if (value) return value
  return isLoading ? '...' : '--'
}

function usdcValue(value: string | undefined, isLoading: boolean): ReactNode {
  if (value) return <TokenAmount amount={value} />
  return isLoading ? '...' : '--'
}

function formatMarkAge(ageSeconds: number): string {
  if (!Number.isFinite(ageSeconds) || ageSeconds < 0) return 'unknown age'
  if (ageSeconds < 60) return `${ageSeconds}s ago`

  const minutes = Math.floor(ageSeconds / 60)
  const seconds = ageSeconds % 60
  if (minutes < 60) return seconds > 0 ? `${minutes}m ${seconds}s ago` : `${minutes}m ago`

  const hours = Math.floor(minutes / 60)
  const remainingMinutes = minutes % 60
  if (hours < 24) return remainingMinutes > 0 ? `${hours}h ${remainingMinutes}m ago` : `${hours}h ago`

  const days = Math.floor(hours / 24)
  const remainingHours = hours % 24
  return remainingHours > 0 ? `${days}d ${remainingHours}h ago` : `${days}d ago`
}

export function Perps() {
  const perpsMarket = usePerpsMarket()
  const perpsAccount = usePerpsAccount(perpsMarket.raw.markPrice)
  const perpsHistory = usePerpsHistory(perpsMarket.raw.markPrice)
  const [nowSeconds, setNowSeconds] = useState(() => Math.floor(Date.now() / 1000))

  useEffect(() => {
    const interval = window.setInterval(() => {
      setNowSeconds(Math.floor(Date.now() / 1000))
    }, 5_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [])

  const dxyFreshnessTooltip = useMemo(() => {
    if (!perpsMarket.lastMarkTime) return undefined

    const ageSeconds = Math.max(0, nowSeconds - perpsMarket.lastMarkTime)
    return `updated ${formatMarkAge(ageSeconds)}`
  }, [nowSeconds, perpsMarket.lastMarkTime])

  const instrumentStats = useMemo<PerpsInstrumentStat[]>(
    () => [
      {
        label: 'DXY price',
        value: displayValue(perpsMarket.oraclePrice, perpsMarket.isLoading),
        freshness: perpsMarket.oracleFreshness,
        freshnessTooltip: dxyFreshnessTooltip,
      },
      { label: '24h change', value: '--' },
      { label: '24h volume', value: '--' },
      {
        label: 'Long open interest',
        value: usdcValue(perpsMarket.longOpenInterest, perpsMarket.isLoading),
        tone: 'positive',
      },
      {
        label: 'Short open interest',
        value: usdcValue(perpsMarket.shortOpenInterest, perpsMarket.isLoading),
        tone: 'negative',
      },
      {
        label: 'Available liquidity',
        value: usdcValue(perpsMarket.availableLiquidity, perpsMarket.isLoading),
      },
      { label: 'Cost of carry', value: displayValue(perpsMarket.costOfCarry, perpsMarket.isLoading) },
    ],
    [
      perpsMarket.availableLiquidity,
      perpsMarket.costOfCarry,
      dxyFreshnessTooltip,
      perpsMarket.isLoading,
      perpsMarket.longOpenInterest,
      perpsMarket.oracleFreshness,
      perpsMarket.oraclePrice,
      perpsMarket.shortOpenInterest,
    ]
  )

  return (
    <div className="flex flex-col lg:flex-row gap-6">
      <div className="flex flex-col gap-6 lg:w-3/4 min-w-0">
        <PerpsInstrumentPanel stats={instrumentStats} />
        <DxyBasketPanel />
        <PerpsAccountPanel
          position={perpsAccount.position}
          pendingOrders={perpsAccount.pendingOrders}
          orderHistory={perpsHistory.orderHistory}
          tradeHistory={perpsHistory.tradeHistory}
          isConnected={perpsAccount.isConnected}
          isLoading={perpsAccount.isLoading}
          isHistoryLoading={perpsHistory.isLoading}
          historyError={perpsHistory.error}
          onAccountRefresh={() => {
            void perpsAccount.refetch()
            void perpsMarket.refetch()
          }}
        />
      </div>
      <div className="flex flex-col gap-2 lg:w-1/4 min-w-0">
        <PerpsMarketStatePanel currentPhase={perpsMarket.marketPhase} />
        <PerpsTradeTicket
          enableLiveTrading
          oraclePriceRaw={perpsMarket.raw.markPrice}
          oraclePublishTime={perpsMarket.lastMarkTime}
          oraclePriceDisplay={perpsMarket.oraclePrice}
          availableToTradeRaw={perpsAccount.freeBuyingPowerUsdc ?? perpsAccount.withdrawableUsdc}
          availableToTradeAmount={perpsAccount.display.availableToTrade}
          portfolioValueRaw={perpsAccount.equityUsdc}
          withdrawableUsdcRaw={perpsAccount.withdrawableUsdc}
          walletUsdcRaw={perpsAccount.walletUsdc}
          marginAllowanceUsdc={perpsAccount.marginAllowanceUsdc}
          currentPosition={perpsAccount.position}
          currentPositionSide={perpsAccount.position?.direction}
          currentPositionAmount={perpsAccount.display.positionNotional}
          pendingOrders={perpsAccount.pendingOrders}
          pendingOrderCount={perpsAccount.pendingOrders.length}
          pendingOrderIds={perpsAccount.pendingOrders.map((order) => order.orderId)}
          maxPendingOrders={perpsAccount.maxPendingOrders}
          firstPendingOrderId={perpsAccount.firstPendingOrderId}
          firstPendingOrderExpiryTime={perpsAccount.firstPendingOrderExpiryTime}
          longOpenCapacityUsdc={perpsMarket.raw.longOpenCapacityUsdc}
          shortOpenCapacityUsdc={perpsMarket.raw.shortOpenCapacityUsdc}
          minOpenNotionalUsdc={perpsMarket.raw.minOpenNotionalUsdc}
          executionFeeBps={perpsMarket.raw.executionFeeBps}
          onAccountRefresh={() => {
            void perpsAccount.refetch()
            void perpsMarket.refetch()
          }}
        />
      </div>
    </div>
  )
}

export default Perps
