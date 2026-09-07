import { useEffect, useState } from 'react'
import { createRoot } from 'react-dom/client'
import { formatUnits } from 'viem'
import { ProtectionInputs } from '../components/ProtectionInputs'
import { PositionProtectionManager, type ProtectionManagementRequest } from '../components/PerpsProtectionPanel'
import { EMPTY_PROTECTION_DRAFT, protectionParamsFromInputs, type PositionProtection } from '../contracts/positionProtection'
import type { PerpsPosition } from '../hooks/usePerpsAccount'
import type { ProtectionConfiguration } from '../hooks/useProtectionConfiguration'
import { protectionPrice, protectionStatusLabel } from '../utils/positionProtection'
import '../index.css'

interface State {
  protocol: { phase: number; lastMarkPrice: bigint; tradingActive: boolean; oracleFrozen: boolean; fadWindow: boolean }
  position: PerpsPosition
  account: { equityUsdc: bigint }
  protection?: PositionProtection
  configuration: ProtectionConfiguration
  cap: bigint
  trader: string
  pendingOrders: number
  autoExecute: boolean
  log: { action: string; tx?: string; time: string }[]
}
const button = 'border border-brand-border px-3 py-2 text-sm hover:bg-white/10 disabled:opacity-40'
const input = 'min-w-0 w-full border border-brand-border bg-app-bg p-2 text-content-primary'
const encode = (data: unknown) => JSON.stringify(data, (_, v: unknown) => typeof v === 'bigint' ? { $bigint: v.toString() } : v)
async function api(path: string, body?: unknown): Promise<State> {
  const response = await fetch(`/local-market/api/${path}`, body === undefined ? undefined : { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: encode(body) })
  const value = JSON.parse(await response.text(), (_, v: unknown) => v && typeof v === 'object' && '$bigint' in v ? BigInt(String(v.$bigint)) : v) as State & { error?: string }
  if (!response.ok) throw new Error(value.error ?? 'Local market request failed')
  return value
}
// Standalone development entrypoint, not a reusable HMR module.
// eslint-disable-next-line react-refresh/only-export-components
function LocalMarket() {
  const [state, setState] = useState<State>()
  const [error, setError] = useState('')
  const [busy, setBusy] = useState(false)
  const [direction, setDirection] = useState<'long' | 'short'>('long')
  const [size, setSize] = useState('2000')
  const [margin, setMargin] = useState('500')
  const [price, setPrice] = useState('1')
  const [draft, setDraft] = useState({ ...EMPTY_PROTECTION_DRAFT, takeProfit: '1.1', stopLoss: '0.9' })
  useEffect(() => {
    let active = true
    const refresh = () => { void api('state').then(next => { if (active) setState(next) }).catch((cause: unknown) => { if (active) setError(String(cause)) }) }
    refresh(); const interval = setInterval(refresh, 2000)
    return () => { active = false; clearInterval(interval) }
  }, [])
  async function act(path: string, data: unknown = {}) {
    setBusy(true); setError('')
    try { setState(await api(path, data)) } catch (cause) { setError(String(cause)); throw cause } finally { setBusy(false) }
  }
  function run(path: string, data: unknown = {}) { void act(path, data).catch(() => { /* act displays the error */ }) }
  function open() {
    if (!state) return
    try { run('open', { direction, size, margin, params: protectionParamsFromInputs({ ...draft, direction, rawMark: state.protocol.lastMarkPrice, cap: state.cap }) }) }
    catch (cause) { setError(String(cause)) }
  }
  return <main className="mx-auto max-w-5xl space-y-6 p-5 text-content-primary">
    <header className="border border-yellow-400 bg-yellow-400/10 p-4">
      <h1 className="text-xl font-semibold">Anvil · Local TP/SL test market</h1>
      <p className="mt-2 text-sm">Isolated fork. Mock Pyth prices. Local test funds and transactions only—no wallet connection or Sepolia transactions.</p>
      <p className="mt-1 text-xs text-content-secondary">Uses production TP/SL inputs, review logic and protection manager with real forked contracts. Local keeper controls replace hosted sponsorship and production worker scheduling.</p>
    </header>
    {error && <p role="alert" className="break-words border border-brand-orange p-3 text-brand-orange">{error}</p>}
    {!state ? <p>Preparing local market…</p> : <>
      <section className="space-y-3 border border-brand-border bg-surface-panel p-4">
        <div className="flex flex-wrap justify-between gap-3"><h2 className="font-semibold">Market controls</h2><span>{state.protocol.tradingActive && !state.protocol.fadWindow && !state.protocol.oracleFrozen ? 'Market open' : 'Market restricted'} · Price {protectionPrice(state.protocol.lastMarkPrice, state.cap)} USDC</span></div>
        <div className="flex flex-wrap items-center gap-3">
          <label>Displayed price <input aria-label="Market price" className={`${input} !w-32`} value={price} onChange={e => { setPrice(e.target.value) }} /></label>
          <button className={button} disabled={busy} onClick={() => { run('price', { price }) }}>Set price</button>
          <label className="flex items-center gap-2"><input type="checkbox" checked={state.autoExecute} disabled={busy} onChange={e => { run('auto', { enabled: e.target.checked }) }} />Auto-execute orders</label>
          <button className={button} disabled={busy || !state.pendingOrders} onClick={() => { run('execute') }}>Execute queued order</button>
          <button className={button} disabled={busy || !state.pendingOrders} onClick={() => { run('expire') }}>Expire queued order</button>
          <button className={button} disabled={busy || state.protection?.status !== 8} onClick={() => { run('retry') }}>Retry latched close</button>
          <button className={button} disabled={busy} onClick={() => { run('reset') }}>Reset test market</button>
        </div>
        <p className="text-xs text-content-secondary">To inspect a queued close, turn auto-execution off before crossing a trigger. Expire that close to test Latched, then retry it. Reset restores only this disposable fork’s funded starting state.</p>
      </section>
      <div className="grid items-start gap-5 md:grid-cols-2">
        <section className="space-y-4 border border-brand-border bg-surface-panel p-4">
          <h2 className="font-semibold">Open a protected position</h2>
          <div className="flex gap-2">{(['long', 'short'] as const).map(side => <button key={side} aria-pressed={direction === side} className={`${button} ${direction === side ? 'bg-[#FFAB96] !text-app-bg' : ''}`} onClick={() => { setDirection(side); setDraft({ ...EMPTY_PROTECTION_DRAFT, takeProfit: side === 'long' ? '1.1' : '0.9', stopLoss: side === 'long' ? '0.9' : '1.1' }) }}>{side === 'long' ? 'Long' : 'Short'}</button>)}</div>
          <div className="grid grid-cols-2 gap-3"><label>Quantity · plDXY<input aria-label="Order quantity" className={input} value={size} onChange={e => { setSize(e.target.value) }} /></label><label>Margin · USDC<input aria-label="Order margin" className={input} value={margin} onChange={e => { setMargin(e.target.value) }} /></label></div>
          <ProtectionInputs value={draft} onChange={setDraft} direction={direction} rawMark={state.protocol.lastMarkPrice} cap={state.cap} disabled={busy || state.position.exists || state.pendingOrders > 0} />
          <button className={`${button} bg-[#FFAB96] !text-app-bg`} disabled={busy || state.position.exists || state.pendingOrders > 0} onClick={open}>{busy ? 'Submitting local transaction…' : 'Open with TP/SL on Anvil'}</button>
        </section>
        <section className="space-y-4 border border-brand-border bg-surface-panel p-4">
          <h2 className="font-semibold">Live on-chain state</h2>
          <p>Account equity: {formatUnits(state.account.equityUsdc, 6)} USDC</p>
          <p>{state.position.exists ? `${state.position.direction} · ${formatUnits(state.position.size, 18)} plDXY` : 'No open position'} · {state.pendingOrders} queued orders</p>
          <p role="status">Protection: {protectionStatusLabel(state.protection?.status)}</p>
          <PositionProtectionManager protection={state.protection} position={state.position} rawMark={state.protocol.lastMarkPrice} cap={state.cap} configuration={state.configuration} pendingOrders={state.pendingOrders} accountAddress={state.trader} onManage={(request: ProtectionManagementRequest) => act('manage', request)} />
        </section>
      </div>
      <section className="border border-brand-border p-4"><h2 className="mb-3 font-semibold">Local transaction log</h2><ul className="space-y-2 text-xs">{state.log.map(item => <li key={item.tx ?? item.time} className="break-all"><strong>{item.action}</strong> {item.tx}</li>)}</ul></section>
    </>}
  </main>
}
if (!import.meta.env.DEV || window.location.hostname !== '127.0.0.1' || window.location.port !== '5182') throw new Error('Local market is restricted to its dedicated loopback development server')
const root = document.getElementById('root')
if (!root) throw new Error('Missing local market root')
createRoot(root).render(<LocalMarket />)
