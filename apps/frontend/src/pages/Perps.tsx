import { DxyBasketPanel } from '../components/DxyBasketPanel'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'
import { PerpsInstrumentPanel } from '../components/PerpsInstrumentPanel'
import { PerpsMarketStatePanel } from '../components/PerpsMarketStatePanel'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'

export function Perps() {
  return (
    <div className="flex flex-col lg:flex-row gap-6">
      <div className="flex flex-col gap-6 lg:w-3/4 min-w-0">
        <PerpsInstrumentPanel />
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
