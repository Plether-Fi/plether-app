import { useMemo, type ReactNode } from 'react'
import { DxyBasketPanel } from '../components/DxyBasketPanel'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'
import { PerpsInstrumentPanel, type PerpsInstrumentStat } from '../components/PerpsInstrumentPanel'
import { PerpsMarketStatePanel } from '../components/PerpsMarketStatePanel'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import { TokenAmount } from '../components/ui'
import { usePerpsMarket } from '../hooks'

function displayValue(value: string | undefined, isLoading: boolean): string {
  if (value) return value
  return isLoading ? '...' : '--'
}

function usdcValue(value: string | undefined, isLoading: boolean): ReactNode {
  if (value) return <TokenAmount amount={value} />
  return isLoading ? '...' : '--'
}

export function Perps() {
  const perpsMarket = usePerpsMarket()

  const instrumentStats = useMemo<PerpsInstrumentStat[]>(
    () => [
      {
        label: 'Oracle price',
        value: displayValue(perpsMarket.oraclePrice, perpsMarket.isLoading),
        freshness: perpsMarket.oracleFreshness,
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
        <PerpsAccountPanel />
      </div>
      <div className="flex flex-col gap-2 lg:w-1/4 min-w-0">
        <PerpsMarketStatePanel />
        <PerpsTradeTicket />
      </div>
    </div>
  )
}

export default Perps
