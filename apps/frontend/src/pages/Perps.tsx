import { DxyBasketPanel } from '../components/DxyBasketPanel'

export function Perps() {
  return (
    <div className="flex flex-col lg:flex-row gap-6">
      <div className="flex flex-col gap-6 lg:w-3/4 min-w-0">
        <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 p-6">
          A
        </div>
        <DxyBasketPanel />
        <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 p-6">
          D
        </div>
      </div>
      <div className="lg:w-1/4 min-w-0">
        <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 p-6">
          B
        </div>
      </div>
    </div>
  )
}

export default Perps
