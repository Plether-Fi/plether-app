import { useEffect, useMemo, useState, type ReactNode } from 'react'
import { DxyBasketPanel } from '../components/DxyBasketPanel'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'
import { PerpsInstrumentPanel, type PerpsInstrumentStat } from '../components/PerpsInstrumentPanel'
import { PerpsMarketStatePanel } from '../components/PerpsMarketStatePanel'
import { getPerpsMarketSchedule } from '../utils/perpsMarketSchedule'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import { TokenAmount } from '../components/ui'
import { usePerpsAccount, usePerpsHistory, usePerpsMarket } from '../hooks'
import { dxyExposureFromContractNotional, formatPerpsUsdc } from '../utils/perps'

function displayValue(value: string | undefined, isLoading: boolean): string {
  if (value) return value
  return isLoading ? '...' : '--'
}

function usdcValue(value: string | undefined, isLoading: boolean): ReactNode {
  if (value) return <TokenAmount amount={value} />
  return isLoading ? '...' : '--'
}

function capacityTooltipValue(value: bigint | undefined, markPrice: bigint | undefined): string {
  if (value === undefined) return '--'
  return formatPerpsUsdc(dxyExposureFromContractNotional(value, markPrice) ?? value)
}

function formatMarkAge(ageSeconds: number): string {
  if (!Number.isFinite(ageSeconds) || ageSeconds < 0) return 'unknown age'
  if (ageSeconds < 60) return `${ageSeconds.toString()}s ago`

  const minutes = Math.floor(ageSeconds / 60)
  const seconds = ageSeconds % 60
  if (minutes < 60) return seconds > 0 ? `${minutes.toString()}m ${seconds.toString()}s ago` : `${minutes.toString()}m ago`

  const hours = Math.floor(minutes / 60)
  const remainingMinutes = minutes % 60
  if (hours < 24) return remainingMinutes > 0 ? `${hours.toString()}h ${remainingMinutes.toString()}m ago` : `${hours.toString()}h ago`

  const days = Math.floor(hours / 24)
  const remainingHours = hours % 24
  return remainingHours > 0 ? `${days.toString()}d ${remainingHours.toString()}h ago` : `${days.toString()}d ago`
}

export function Perps() {
  const perpsMarket = usePerpsMarket()
  const perpsAccount = usePerpsAccount(perpsMarket.raw.markPrice)
  const perpsHistory = usePerpsHistory()
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
    if (perpsMarket.oracleFreshness === 'checking') return 'checking backend for a fresh update'

    if (!perpsMarket.oracleFreshnessTime) return undefined

    const ageSeconds = Math.max(0, nowSeconds - perpsMarket.oracleFreshnessTime)
    return `updated ${formatMarkAge(ageSeconds)}`
  }, [
    nowSeconds,
    perpsMarket.oracleFreshness,
    perpsMarket.oracleFreshnessTime,
  ])

  const marketSchedule = useMemo(
    () => getPerpsMarketSchedule(new Date(nowSeconds * 1000), perpsMarket.marketPhase),
    [nowSeconds, perpsMarket.marketPhase]
  )

  const instrumentStats = useMemo<PerpsInstrumentStat[]>(
    () => {
      const poolLiquidityTooltip = (
        <div className="w-full space-y-2 text-left">
          <div className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-4">
            <span className="min-w-0 text-cyber-text-secondary">Long capacity</span>
            <span className="whitespace-nowrap font-semibold text-cyber-text-primary">
              {capacityTooltipValue(perpsMarket.raw.longOpenCapacityUsdc, perpsMarket.raw.markPrice)} USDC
            </span>
          </div>
          <div className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-4">
            <span className="min-w-0 text-cyber-text-secondary">Short capacity</span>
            <span className="whitespace-nowrap font-semibold text-cyber-text-primary">
              {capacityTooltipValue(perpsMarket.raw.shortOpenCapacityUsdc, perpsMarket.raw.markPrice)} USDC
            </span>
          </div>
          <div className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-4">
            <span className="min-w-0 text-cyber-text-secondary">Minimum order size</span>
            <span className="whitespace-nowrap font-semibold text-cyber-text-primary">
              {capacityTooltipValue(perpsMarket.raw.minOpenNotionalUsdc, perpsMarket.raw.markPrice)} USDC
            </span>
          </div>
          <div className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-4">
            <span className="min-w-0 text-cyber-text-secondary">Minimum new position</span>
            <span className="whitespace-nowrap font-semibold text-cyber-text-primary">
              {capacityTooltipValue(perpsMarket.raw.minNewPositionNotionalUsdc, perpsMarket.raw.markPrice)} USDC
            </span>
          </div>
        </div>
      )
      const costOfCarryTooltip = (
        <div className="w-full space-y-3 text-left leading-5">
          <p>
            Annualized max carry paid by traders to LPs for the part of a position&apos;s worst-case
            payout backed by pool capital.
          </p>
          <p>
            This is not a funding rate; both sides can pay carry at the same time. The actual
            USDC amount depends on borrow base, side utilization, and elapsed time.
          </p>
        </div>
      )

      return [
        {
          label: 'plDXY Perp price',
          value: displayValue(perpsMarket.oraclePrice, perpsMarket.isLoading),
          freshness: perpsMarket.oracleFreshness,
          freshnessTooltip: dxyFreshnessTooltip,
        },
        {
          label: '24h change',
          value: displayValue(perpsMarket.priceChange24h, perpsMarket.isStatsLoading),
          tone: perpsMarket.priceChange24hTone,
        },
        {
          label: '24h volume',
          value: usdcValue(perpsMarket.volume24h, perpsMarket.isStatsLoading),
        },
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
          label: 'Pool liquidity',
          value: usdcValue(perpsMarket.availableLiquidity, perpsMarket.isLoading),
          tooltip: poolLiquidityTooltip,
          tooltipClassName: 'w-[400px] whitespace-normal p-4',
          tooltipPosition: 'left',
        },
        {
          label: 'Cost of carry',
          value: displayValue(perpsMarket.costOfCarry, perpsMarket.isLoading),
          tooltip: costOfCarryTooltip,
          tooltipClassName: 'w-[520px] whitespace-normal p-4',
          tooltipPosition: 'left',
        },
      ]
    },
    [
      perpsMarket.availableLiquidity,
      perpsMarket.costOfCarry,
      dxyFreshnessTooltip,
      perpsMarket.isLoading,
      perpsMarket.isStatsLoading,
      perpsMarket.longOpenInterest,
      perpsMarket.oracleFreshness,
      perpsMarket.oraclePrice,
      perpsMarket.priceChange24h,
      perpsMarket.priceChange24hTone,
      perpsMarket.raw.longOpenCapacityUsdc,
      perpsMarket.raw.markPrice,
      perpsMarket.raw.minOpenNotionalUsdc,
      perpsMarket.raw.minNewPositionNotionalUsdc,
      perpsMarket.raw.shortOpenCapacityUsdc,
      perpsMarket.shortOpenInterest,
      perpsMarket.volume24h,
    ]
  )

  return (
    <div className="flex flex-col lg:flex-row gap-6">
      <div className="flex flex-col gap-6 lg:w-3/4 min-w-0">
        <PerpsInstrumentPanel stats={instrumentStats} />
        <DxyBasketPanel />
        <PerpsAccountPanel
          position={perpsAccount.position}
          equityUsdc={perpsAccount.equityUsdc}
          freeBuyingPowerUsdc={perpsAccount.freeBuyingPowerUsdc}
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
            void perpsHistory.refetch()
          }}
        />
      </div>
      <div className="flex flex-col gap-2 lg:w-1/4 min-w-0">
        <PerpsMarketStatePanel currentPhase={perpsMarket.marketPhase} />
        <PerpsTradeTicket
          enableLiveTrading
          oraclePriceRaw={perpsMarket.raw.markPrice}
          oraclePublishTime={perpsMarket.oracleFreshnessTime}
          oraclePriceDisplay={perpsMarket.oraclePrice}
          oracleFreshness={perpsMarket.oracleFreshness}
          oracleFreshnessTooltip={dxyFreshnessTooltip}
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
          orderHistory={perpsHistory.orderHistory}
          pendingOrderCount={perpsAccount.pendingOrders.length}
          maxPendingOrders={perpsAccount.maxPendingOrders}
          firstPendingOrderId={perpsAccount.firstPendingOrderId}
          firstPendingOrderExpiryTime={perpsAccount.firstPendingOrderExpiryTime}
          longOpenCapacityUsdc={perpsMarket.raw.longOpenCapacityUsdc}
          shortOpenCapacityUsdc={perpsMarket.raw.shortOpenCapacityUsdc}
          minOpenNotionalUsdc={perpsMarket.raw.minOpenNotionalUsdc}
          minNewPositionNotionalUsdc={perpsMarket.raw.minNewPositionNotionalUsdc}
          maintenanceMarginBps={perpsMarket.raw.maintenanceMarginBps}
          executionFeeBps={perpsMarket.raw.executionFeeBps}
          marketPhase={perpsMarket.marketPhase}
          marketCurrentDuration={marketSchedule.currentDuration}
          onAccountRefresh={() => {
            void perpsAccount.refetch()
            void perpsMarket.refetch()
            void perpsHistory.refetch()
          }}
        />
      </div>
    </div>
  )
}

export default Perps
