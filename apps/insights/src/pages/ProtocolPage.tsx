import { useMemo, useState } from 'react'
import { Link } from 'react-router-dom'
import {
  InsightsApiError,
  deriveProtocolHealthBanner,
  deriveProtocolStateSummary,
  formatProtocolParamValue,
  parameterImpactHint,
  resolveParameterRange,
  shortHash,
  useProtocolManifest,
  type ProtocolContractCategory,
  type ProtocolContractRecord,
  type ProtocolParameterCategory,
  type ProtocolParameterRecord,
  type ProtocolParameterStatus,
} from '../api'
import { AddressAvatar } from '../components/AddressAvatar'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard } from '../components/StatCard'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { explorerAddressUrl, resolvePerpsChain } from '../utils/chain'
import { formatPrice, formatUtc, shortAddress } from '../utils/format'

type ParamFilter = 'all' | ProtocolParameterCategory
type ContractFilter = 'all' | ProtocolContractCategory

const PARAM_TABS: { id: ParamFilter; label: string }[] = [
  { id: 'all', label: 'All parameters' },
  { id: 'trader', label: 'Trader & Margin' },
  { id: 'liquidation', label: 'Liquidations' },
  { id: 'lp', label: 'LP Vaults' },
  { id: 'oracle', label: 'Oracle & Timing' },
]

const CONTRACT_TABS: { id: ContractFilter; label: string }[] = [
  { id: 'all', label: 'All' },
  { id: 'core', label: 'Core' },
  { id: 'orders', label: 'Orders' },
  { id: 'liquidity', label: 'Liquidity' },
  { id: 'oracle', label: 'Oracle & Lenses' },
  { id: 'safety', label: 'Safety & Admin' },
]

const CONTRACT_CATEGORY_LABEL: Record<ProtocolContractCategory, string> = {
  core: 'Core',
  orders: 'Orders',
  liquidity: 'Liquidity',
  oracle: 'Oracle & Lenses',
  safety: 'Safety & Admin',
}

function paramStatusPill(status: ProtocolParameterStatus): { tone: StatusTone; label: string } {
  switch (status) {
    case 'match':
      return { tone: 'live', label: 'Match' }
    case 'drifted':
      return { tone: 'degraded', label: 'Drifted' }
    case 'live_only':
      return { tone: 'unknown', label: 'On-chain' }
    case 'unavailable':
      return { tone: 'stale', label: 'Unavailable' }
  }
}

function contractVerifyPill(verified: boolean | null): { tone: StatusTone; label: string } {
  if (verified === true) return { tone: 'live', label: 'Verified' }
  if (verified === false) return { tone: 'blocked', label: 'Mismatch' }
  return { tone: 'unknown', label: 'Unchecked' }
}

function SafetyControlTile({
  label,
  source,
  tone,
  statusLabel,
  detail,
}: {
  label: string
  source: string
  tone: StatusTone
  statusLabel: string
  detail: string
}) {
  return (
    <div className="flex flex-col justify-between gap-2.5 border border-brand-border/25 bg-app-bg p-4">
      <div className="flex items-start justify-between gap-3">
        <div>
          <p className="text-sm font-semibold text-content-primary">{label}</p>
          <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">{source}</p>
        </div>
        <StatusPill tone={tone} label={statusLabel} />
      </div>
      <p className="text-xs leading-5 text-content-secondary">{detail}</p>
    </div>
  )
}

function CopyableChip({
  value,
  display,
  copiedKey,
  itemKey,
  onCopy,
}: {
  value: string
  display: string
  copiedKey: string | null
  itemKey: string
  onCopy: (key: string, text: string) => void
}) {
  const isCopied = copiedKey === itemKey
  return (
    <button
      type="button"
      onClick={() => { onCopy(itemKey, value) }}
      title={`Copy ${value}`}
      className="inline-flex items-center gap-1.5 font-mono text-xs text-content-primary transition-colors hover:text-brand-peach"
    >
      <span>{display}</span>
      <span className="text-[10px] uppercase tracking-wider text-content-tertiary">
        {isCopied ? 'Copied' : 'Copy'}
      </span>
    </button>
  )
}

function ParameterRow({ param }: { param: ProtocolParameterRecord }) {
  const badge = paramStatusPill(param.status)
  const hint = parameterImpactHint(param)
  const liveFormatted = formatProtocolParamValue(param.liveValue, param.unit)
  const manifestFormatted =
    param.manifestDefault != null
      ? formatProtocolParamValue(param.manifestDefault, param.unit)
      : null
  const range = resolveParameterRange(param)
  const isDrifted = param.status === 'drifted'
  const showDefaultTick =
    isDrifted && range.defaultPercent != null && range.livePercent != null
  const dotClass =
    isDrifted
      ? 'border-brand-yellow bg-brand-yellow'
      : param.status === 'match'
        ? 'border-positive bg-positive'
        : 'border-brand-peach bg-brand-peach'
  const fillClass =
    isDrifted
      ? 'bg-brand-yellow/45'
      : param.status === 'match'
        ? 'bg-positive/35'
        : 'bg-brand-peach/35'

  return (
    <tr
      className={`border-b border-brand-border/15 transition-colors last:border-0 hover:bg-app-bg/35 ${
        isDrifted ? 'bg-brand-yellow/[0.04]' : ''
      }`}
    >
      <td className="px-4 py-3.5 align-top">
        <div className="flex flex-wrap items-center gap-2">
          <span className="font-semibold text-content-primary">{param.label}</span>
          <span className="border border-brand-border/25 bg-app-bg px-1.5 py-0.5 font-mono text-[11px] text-content-tertiary">
            {param.contract}
          </span>
          {hint ? (
            <span className="font-mono text-[11px] text-brand-peach">{hint}</span>
          ) : null}
        </div>
        <p className="mt-1 max-w-xl text-xs leading-5 text-content-secondary">
          {param.description}
        </p>
      </td>
      <td className="w-[290px] min-w-[240px] px-4 py-3.5 align-top">
        <div className="space-y-2">
          <div className="flex flex-wrap items-baseline justify-between gap-2">
            <span
              className={`font-mono text-sm font-semibold tabular-nums ${
                isDrifted
                  ? 'text-brand-yellow'
                  : param.status === 'unavailable'
                    ? 'text-content-tertiary'
                    : 'text-content-primary'
              }`}
            >
              {liveFormatted}
            </span>
            {isDrifted && manifestFormatted ? (
              <span className="font-mono text-[11px] tabular-nums text-content-tertiary">
                default {manifestFormatted}
              </span>
            ) : param.manifestDefault == null ? (
              <span className="font-mono text-[11px] text-content-tertiary">on-chain</span>
            ) : null}
          </div>

          <div
            className="relative flex h-3 items-center"
            role="meter"
            aria-label={`${param.label} range`}
            aria-valuemin={0}
            aria-valuemax={100}
            aria-valuenow={range.livePercent != null ? Math.round(range.livePercent) : undefined}
            aria-valuetext={liveFormatted}
          >
            <div className="h-1 w-full bg-app-bg ring-1 ring-inset ring-brand-border/20" />
            {range.livePercent != null ? (
              <div
                className={`absolute left-0 h-1 ${fillClass}`}
                style={{ width: `${String(range.livePercent)}%` }}
              />
            ) : null}
            {showDefaultTick && range.defaultPercent != null ? (
              <span
                className="absolute top-0.5 h-2 w-0.5 -translate-x-1/2 bg-content-secondary"
                style={{ left: `${String(range.defaultPercent)}%` }}
                title={`Manifest default: ${manifestFormatted ?? ''}`}
              />
            ) : null}
            {range.livePercent != null ? (
              <span
                className={`absolute h-2.5 w-2.5 -translate-x-1/2 rounded-full border shadow-sm ${dotClass}`}
                style={{ left: `${String(range.livePercent)}%` }}
                title={`Live on-chain: ${liveFormatted}`}
              />
            ) : null}
          </div>

          <div className="flex items-center justify-between font-mono text-[10px] tabular-nums text-content-tertiary">
            <span>{range.minLabel}</span>
            <span>{range.maxLabel}</span>
          </div>
        </div>
      </td>
      <td className="whitespace-nowrap px-4 py-3.5 align-top text-right">
        <StatusPill tone={badge.tone} label={badge.label} />
      </td>
    </tr>
  )
}

function ContractRow({
  contract,
  chainId,
  explorerName,
  copiedKey,
  onCopy,
}: {
  contract: ProtocolContractRecord
  chainId: number
  explorerName: string
  copiedKey: string | null
  onCopy: (key: string, text: string) => void
}) {
  const badge = contractVerifyPill(contract.verified)
  const hashTitle = contract.actualCodeHash
    ? `Expected: ${contract.expectedCodeHash}\nOn-chain: ${contract.actualCodeHash}`
    : `Expected: ${contract.expectedCodeHash}`

  return (
    <tr
      className={`border-b border-brand-border/15 transition-colors last:border-0 hover:bg-app-bg/35 ${
        contract.verified === false ? 'bg-brand-orange/[0.06]' : ''
      }`}
    >
      <td className="px-4 py-3.5 align-top">
        <div className="flex items-start gap-2.5">
          <AddressAvatar address={contract.address} size={18} className="mt-0.5" />
          <div className="min-w-0">
            <div className="flex flex-wrap items-center gap-2">
              <span className="font-semibold text-content-primary">{contract.name}</span>
              <span className="border border-brand-border/25 bg-app-bg px-1.5 py-0.5 text-[11px] text-content-tertiary">
                {CONTRACT_CATEGORY_LABEL[contract.category]}
              </span>
            </div>
            <p className="mt-0.5 max-w-md text-xs leading-5 text-content-secondary">
              {contract.role}
            </p>
          </div>
        </div>
      </td>
      <td className="whitespace-nowrap px-4 py-3.5 align-top">
        <div className="flex items-center gap-2">
          <CopyableChip
            value={contract.address}
            display={shortAddress(contract.address)}
            copiedKey={copiedKey}
            itemKey={`addr:${contract.key}`}
            onCopy={onCopy}
          />
          <a
            href={explorerAddressUrl(contract.address, chainId)}
            target="_blank"
            rel="noreferrer"
            className="text-xs font-semibold text-brand-peach hover:underline"
            title={`Inspect ${contract.name} on ${explorerName}`}
          >
            {explorerName} ↗
          </a>
        </div>
      </td>
      <td className="whitespace-nowrap px-4 py-3.5 align-top font-mono text-xs text-content-secondary">
        <div title={hashTitle}>
          <CopyableChip
            value={contract.expectedCodeHash}
            display={shortHash(contract.expectedCodeHash)}
            copiedKey={copiedKey}
            itemKey={`hash:${contract.key}`}
            onCopy={onCopy}
          />
          {contract.verified === false && contract.actualCodeHash ? (
            <p className="mt-1 text-[11px] text-brand-orange">
              Live: {shortHash(contract.actualCodeHash)}
            </p>
          ) : null}
        </div>
      </td>
      <td className="whitespace-nowrap px-4 py-3.5 align-top text-right">
        <StatusPill tone={badge.tone} label={badge.label} />
      </td>
    </tr>
  )
}

export function ProtocolPage() {
  const ctx = useProtocolContext()
  const manifestQuery = useProtocolManifest()

  const [paramFilter, setParamFilter] = useState<ParamFilter>('all')
  const [driftOnly, setDriftOnly] = useState(false)
  const [contractFilter, setContractFilter] = useState<ContractFilter>('all')
  const [contractSearch, setContractSearch] = useState('')
  const [copiedKey, setCopiedKey] = useState<string | null>(null)

  const data = manifestQuery.data
  const chain = resolvePerpsChain(data?.network.chainId ?? ctx.chain.chainId)
  const isEndpointMissing =
    manifestQuery.isError
    && manifestQuery.error instanceof InsightsApiError
    && manifestQuery.error.status === 404

  function handleCopy(key: string, text: string) {
    if ('clipboard' in navigator) {
      void navigator.clipboard.writeText(text)
    }
    setCopiedKey(key)
    window.setTimeout(() => {
      setCopiedKey((current) => (current === key ? null : current))
    }, 1500)
  }

  const filteredParams = useMemo(() => {
    if (!data) return []
    return data.parameters.filter((param) => {
      if (paramFilter !== 'all' && param.category !== paramFilter) return false
      if (driftOnly && param.status !== 'drifted') return false
      return true
    })
  }, [data, paramFilter, driftOnly])

  const filteredContracts = useMemo(() => {
    if (!data) return []
    const needle = contractSearch.trim().toLowerCase()
    return data.contracts.filter((contract) => {
      if (contractFilter !== 'all' && contract.category !== contractFilter) return false
      if (!needle) return true
      return (
        contract.name.toLowerCase().includes(needle)
        || contract.key.toLowerCase().includes(needle)
        || contract.role.toLowerCase().includes(needle)
        || contract.address.toLowerCase().includes(needle)
      )
    })
  }, [data, contractFilter, contractSearch])

  const pauseCoordinator = data?.contracts.find((c) => c.key === 'emergencyPauseCoordinator')

  const banner = data
    ? deriveProtocolHealthBanner(data)
    : manifestQuery.isLoading
      ? {
          tone: 'unknown' as StatusTone,
          label: 'Verifying protocol manifest…',
          note: 'Reading live on-chain bytecode hashes and risk parameters',
        }
      : isEndpointMissing
        ? {
            tone: 'unknown' as StatusTone,
            label: 'Awaiting Stage 4 backend deploy',
            note: 'Connected backend does not serve /api/insights/v1/protocol/manifest yet',
          }
        : {
            tone: 'blocked' as StatusTone,
            label: 'Protocol manifest unavailable',
            note: 'Could not verify on-chain protocol contracts or parameters',
          }

  const stateSummary = data ? deriveProtocolStateSummary(data.safety) : null

  const verifiedCardTone = !data
    ? 'neutral'
    : data.verification.mismatchCount > 0
      ? 'negative'
      : data.verification.uncheckedCount > 0
        ? 'warning'
        : 'positive'

  const driftCardTone = !data
    ? 'neutral'
    : data.verification.driftedParameters > 0
      ? 'warning'
      : data.verification.unavailableParameters > 0
        ? 'neutral'
        : 'positive'

  const liveOnlyCount = data
    ? data.parameters.filter((p) => p.status === 'live_only').length
    : 0

  return (
    <div className="space-y-6 sm:space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={data?.network.releaseVersion ?? ctx.releaseVersion}
        blockNumber={data?.verifiedAtBlock ?? ctx.blockNumber}
        observedAt={data?.observedAt ?? ctx.observedAt}
        tone={banner.tone}
        statusLabel={banner.label}
        note={banner.note}
      />

      <header className="flex flex-wrap items-end justify-between gap-4">
        <div className="max-w-3xl space-y-2.5">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
            Protocol Mechanics &amp; Verification
          </p>
          <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">
            Contracts, Parameters &amp; Safety Controls
          </h1>
          <p className="text-sm leading-6 text-content-secondary">
            Live on-chain bytecode hash verification against the pinned Core release manifest,
            real-time risk parameters compared to release defaults, and emergency pause controls.
          </p>
        </div>

        {data?.settlementToken ? (
          <div className="border border-brand-border/30 bg-surface-panel px-4 py-2.5 text-xs">
            <p className="text-content-tertiary">Canonical settlement asset</p>
            <div className="mt-1 flex items-center gap-2 font-mono">
              <span className="font-semibold text-content-primary">
                {data.settlementToken.symbol ?? 'USDG'}
              </span>
              <span className="text-content-tertiary">
                ({data.settlementToken.decimals ?? 6} decimals
                {data.settlementToken.isMock ? ' · testnet mock' : ''})
              </span>
              <a
                href={explorerAddressUrl(data.settlementToken.address, chain.chainId)}
                target="_blank"
                rel="noreferrer"
                className="text-brand-peach hover:underline"
              >
                {shortAddress(data.settlementToken.address)} ↗
              </a>
            </div>
          </div>
        ) : null}
      </header>

      <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
        <StatCard
          label="Verified contracts"
          loading={manifestQuery.isLoading}
          unavailable={!data || !data.rpcAvailable}
          tone={verifiedCardTone}
          value={
            data && data.rpcAvailable
              ? `${String(data.verification.verifiedCount)} / ${String(data.verification.totalContracts)}`
              : undefined
          }
          detail={
            data
              ? !data.rpcAvailable
                ? `${String(data.verification.totalContracts)} contracts pinned in manifest; RPC unreachable`
                : data.verification.mismatchCount > 0
                  ? `${String(data.verification.mismatchCount)} bytecode mismatch${data.verification.mismatchCount === 1 ? '' : 'es'} detected`
                  : data.verification.uncheckedCount > 0
                    ? `${String(data.verification.uncheckedCount)} contract reads unchecked`
                    : 'Runtime keccak256 matches pinned manifest'
              : 'Runtime code hash checks'
          }
        />

        <StatCard
          label="Protocol state"
          loading={manifestQuery.isLoading}
          unavailable={!stateSummary || !data?.safety.available}
          tone={stateSummary?.tone ?? 'neutral'}
          value={stateSummary?.label}
          detail={stateSummary?.detail ?? 'PerpsPublicLens & HousePool pause flags'}
        />

        <StatCard
          label="Parameter drift"
          loading={manifestQuery.isLoading}
          unavailable={!data || !data.rpcAvailable}
          tone={driftCardTone}
          value={
            data && data.rpcAvailable
              ? `${String(data.verification.driftedParameters)} drifted`
              : undefined
          }
          detail={
            data
              ? !data.rpcAvailable
                ? 'Live parameter reads unavailable'
                : `${String(data.verification.matchedParameters)} matched · ${String(liveOnlyCount)} on-chain only${
                    data.verification.unavailableParameters > 0
                      ? ` · ${String(data.verification.unavailableParameters)} unavailable`
                      : ''
                  }`
              : 'Live contracts vs. release defaults'
          }
        />

        <StatCard
          label="Active release"
          loading={manifestQuery.isLoading}
          unavailable={!data?.release.version}
          tone="accent"
          value={data?.release.version ?? undefined}
          detail={
            data
              ? `Block ${
                  data.release.deploymentBlock
                    ? Number(data.release.deploymentBlock).toLocaleString('en-US')
                    : '—'
                }${data.release.sourceCommit ? ` · ${data.release.sourceCommit.slice(0, 8)}` : ''}`
              : 'Pinned Core release manifest'
          }
        />
      </div>

      {manifestQuery.isLoading ? (
        <Panel>
          <LoadingState rows={6} />
        </Panel>
      ) : isEndpointMissing ? (
        <Panel className="p-6">
          <h2 className="text-lg font-semibold">Stage 4 backend deployment pending</h2>
          <p className="mt-2 max-w-2xl text-sm leading-6 text-content-secondary">
            Connected backend does not serve <code className="font-mono text-xs">/api/insights/v1/protocol/manifest</code> yet.
            Once the Stage 4 backend release is deployed to {chain.name}, this page will display live
            on-chain bytecode verification for all 30 pinned contracts, economic parameter drift
            checks, and emergency pause controls without synthetic fallbacks.
          </p>
          <div className="mt-4 flex flex-wrap items-center gap-4 text-sm font-semibold">
            <button
              type="button"
              onClick={() => void manifestQuery.refetch()}
              className="border border-brand-border/40 px-3.5 py-1.5 text-content-primary hover:border-brand-peach hover:text-brand-peach"
            >
              Recheck endpoint
            </button>
            <Link to="/" className="text-brand-peach hover:underline">
              ← Back to Overview
            </Link>
          </div>
        </Panel>
      ) : manifestQuery.isError || !data ? (
        <ErrorState
          title="Protocol manifest unavailable"
          message="Could not fetch the verified contract registry and live on-chain parameters. Please try again in a moment."
          onRetry={() => void manifestQuery.refetch()}
        />
      ) : (
        <>
          <section aria-labelledby="safety-controls-title" className="space-y-3">
            <div className="flex flex-wrap items-baseline justify-between gap-3">
              <div>
                <h2 id="safety-controls-title" className="text-lg font-semibold">
                  Safety &amp; emergency pause controls
                </h2>
                <p className="mt-1 text-xs text-content-tertiary">
                  Direct on-chain reads from PerpsPublicLens, HousePool, and CfdEngine at block{' '}
                  {Number(data.verifiedAtBlock).toLocaleString('en-US')}
                </p>
              </div>
              {pauseCoordinator ? (
                <a
                  href={explorerAddressUrl(pauseCoordinator.address, chain.chainId)}
                  target="_blank"
                  rel="noreferrer"
                  className="font-mono text-xs text-brand-peach hover:underline"
                >
                  EmergencyPauseCoordinator {shortAddress(pauseCoordinator.address)} ↗
                </a>
              ) : null}
            </div>

            <Panel className="p-4 sm:p-5">
              <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
                <SafetyControlTile
                  label="Perpetuals trading"
                  source="PerpsPublicLens.getProtocolStatus()"
                  tone={
                    data.safety.tradingActive === true
                      ? 'live'
                      : data.safety.tradingActive === false
                        ? 'blocked'
                        : 'unknown'
                  }
                  statusLabel={
                    data.safety.tradingActive === true
                      ? 'Active'
                      : data.safety.tradingActive === false
                        ? 'Paused'
                        : 'Unavailable'
                  }
                  detail={
                    data.safety.tradingActive === true
                      ? 'Order commits, fills, and position modifications are enabled.'
                      : data.safety.tradingActive === false
                        ? 'New order execution is currently halted on-chain.'
                        : 'Live trading flag could not be read from RPC.'
                  }
                />

                <SafetyControlTile
                  label="LP withdrawals"
                  source="PerpsPublicLens.getProtocolStatus()"
                  tone={
                    data.safety.withdrawalLive === true
                      ? 'live'
                      : data.safety.withdrawalLive === false
                        ? 'blocked'
                        : 'unknown'
                  }
                  statusLabel={
                    data.safety.withdrawalLive === true
                      ? 'Live'
                      : data.safety.withdrawalLive === false
                        ? 'Halted'
                        : 'Unavailable'
                  }
                  detail={
                    data.safety.withdrawalLive === true
                      ? 'Senior and Junior vault redemptions and claims are operational.'
                      : data.safety.withdrawalLive === false
                        ? 'LP withdrawal claims are currently restricted on-chain.'
                        : 'Live withdrawal flag could not be read from RPC.'
                  }
                />

                <SafetyControlTile
                  label="LP epoch settlement"
                  source="PerpsPublicLens.getProtocolStatus()"
                  tone={
                    data.safety.lpEpochSettlementPaused === false
                      ? 'live'
                      : data.safety.lpEpochSettlementPaused === true
                        ? 'blocked'
                        : 'unknown'
                  }
                  statusLabel={
                    data.safety.lpEpochSettlementPaused === false
                      ? 'Active'
                      : data.safety.lpEpochSettlementPaused === true
                        ? 'Paused'
                        : 'Unavailable'
                  }
                  detail={
                    data.safety.lpEpochSettlementPaused === false
                      ? 'Hourly permissionless epoch settlement is running normally.'
                      : data.safety.lpEpochSettlementPaused === true
                        ? 'LP epoch settlement is explicitly paused by emergency controls.'
                        : 'LP settlement pause status unavailable.'
                  }
                />

                <SafetyControlTile
                  label="Pyth DXY oracle feed"
                  source="PerpsPublicLens.getProtocolStatus()"
                  tone={
                    data.safety.oracleFrozen === false
                      ? 'live'
                      : data.safety.oracleFrozen === true
                        ? 'blocked'
                        : 'unknown'
                  }
                  statusLabel={
                    data.safety.oracleFrozen === false
                      ? 'Live'
                      : data.safety.oracleFrozen === true
                        ? 'Frozen'
                        : 'Unavailable'
                  }
                  detail={
                    data.safety.lastMarkPrice
                      ? `Last on-chain mark: ${formatPrice(data.safety.lastMarkPrice)}${
                          data.safety.lastMarkTime ? ` · ${formatUtc(data.safety.lastMarkTime)}` : ''
                        }`
                      : 'Oracle mark telemetry unavailable from RPC.'
                  }
                />

                <SafetyControlTile
                  label="HousePool circuit breaker"
                  source="HousePool.paused()"
                  tone={
                    data.safety.housePoolPaused === false
                      ? 'live'
                      : data.safety.housePoolPaused === true
                        ? 'blocked'
                        : 'unknown'
                  }
                  statusLabel={
                    data.safety.housePoolPaused === false
                      ? 'Operational'
                      : data.safety.housePoolPaused === true
                        ? 'Paused'
                        : 'Unavailable'
                  }
                  detail={
                    data.safety.housePoolPaused === false
                      ? 'Counterparty vault accounting is unpaused.'
                      : data.safety.housePoolPaused === true
                        ? 'HousePool is paused; vault state transitions are halted.'
                        : 'HousePool pause flag unavailable from RPC.'
                  }
                />

                <SafetyControlTile
                  label="Engine mode & FX window"
                  source="CfdEngine.degradedMode() · fadWindow"
                  tone={
                    data.safety.degradedMode === true
                      ? 'blocked'
                      : data.safety.fadWindow === true
                        ? 'degraded'
                        : data.safety.degradedMode === false
                          ? 'live'
                          : 'unknown'
                  }
                  statusLabel={
                    data.safety.degradedMode === true
                      ? 'Degraded'
                      : data.safety.fadWindow === true
                        ? 'FAD window'
                        : data.safety.degradedMode === false
                          ? 'Normal'
                          : 'Unavailable'
                  }
                  detail={
                    data.safety.degradedMode === true
                      ? 'Engine degraded mode is active; risk-reducing actions prioritized.'
                      : data.safety.fadWindow === true
                        ? 'Weekend FX closure active — elevated FAD margin applies.'
                        : data.safety.degradedMode === false
                          ? 'Standard FX trading window; engine operating in normal mode.'
                          : 'Engine mode could not be read from RPC.'
                  }
                />
              </div>
            </Panel>
          </section>

          <section aria-labelledby="parameters-title" className="space-y-3">
            <div className="flex flex-wrap items-end justify-between gap-3">
              <div>
                <h2 id="parameters-title" className="text-lg font-semibold">
                  Live economic &amp; risk parameters
                </h2>
                <p className="mt-1 text-xs text-content-tertiary">
                  Every parameter read directly from on-chain contracts and compared against Core{' '}
                  {data.network.releaseVersion} manifest defaults
                </p>
              </div>
              {data.verification.driftedParameters > 0 ? (
                <button
                  type="button"
                  onClick={() => { setDriftOnly((prev) => !prev) }}
                  className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
                    driftOnly
                      ? 'border-brand-yellow bg-brand-yellow/15 text-brand-yellow'
                      : 'border-brand-border/35 text-content-secondary hover:border-brand-yellow/60 hover:text-brand-yellow'
                  }`}
                >
                  {driftOnly
                    ? `Showing drifted only (${String(data.verification.driftedParameters)})`
                    : `Filter drifted (${String(data.verification.driftedParameters)})`}
                </button>
              ) : null}
            </div>

            <div className="flex flex-wrap gap-1.5" role="tablist" aria-label="Parameter categories">
              {PARAM_TABS.map((tab) => {
                const count =
                  tab.id === 'all'
                    ? data.parameters.length
                    : data.parameters.filter((p) => p.category === tab.id).length
                const active = paramFilter === tab.id
                return (
                  <button
                    key={tab.id}
                    type="button"
                    role="tab"
                    aria-selected={active}
                    onClick={() => { setParamFilter(tab.id) }}
                    className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
                      active
                        ? 'border-brand-orange bg-brand-orange text-content-primary'
                        : 'border-brand-border/30 bg-surface-panel text-content-secondary hover:border-brand-peach/50 hover:text-content-primary'
                    }`}
                  >
                    {tab.label}{' '}
                    <span className="ml-1 font-mono opacity-75">{count}</span>
                  </button>
                )
              })}
            </div>

            <Panel>
              {filteredParams.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[720px] text-left text-sm">
                    <caption className="sr-only">
                      Live on-chain economic parameters compared to release manifest defaults
                    </caption>
                    <thead className="border-b border-brand-border/25 bg-app-bg/40 text-xs font-medium text-content-secondary">
                      <tr>
                        <th scope="col" className="px-4 py-3">Parameter &amp; trader / LP impact</th>
                        <th scope="col" className="px-4 py-3">Live value &amp; range</th>
                        <th scope="col" className="px-4 py-3 text-right">Status</th>
                      </tr>
                    </thead>
                    <tbody>
                      {filteredParams.map((param) => (
                        <ParameterRow key={param.key} param={param} />
                      ))}
                    </tbody>
                  </table>
                </div>
              ) : (
                <EmptyState
                  title="No matching parameters"
                  message="No parameters match the current category or drift filter."
                />
              )}
            </Panel>
          </section>

          <section aria-labelledby="contracts-title" className="space-y-3">
            <div className="flex flex-wrap items-end justify-between gap-3">
              <div>
                <h2 id="contracts-title" className="text-lg font-semibold">
                  Verified contract registry
                </h2>
                <p className="mt-1 text-xs text-content-tertiary">
                  All {String(data.contracts.length)} pinned contracts hashed on-chain via{' '}
                  <code className="font-mono">eth_getCode</code> + <code className="font-mono">keccak256</code>
                </p>
              </div>
              <p className="font-mono text-xs text-content-tertiary">
                Showing {filteredContracts.length} of {data.contracts.length}
              </p>
            </div>

            <div className="flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between">
              <div className="flex flex-wrap gap-1.5" role="tablist" aria-label="Contract categories">
                {CONTRACT_TABS.map((tab) => {
                  const count =
                    tab.id === 'all'
                      ? data.contracts.length
                      : data.contracts.filter((c) => c.category === tab.id).length
                  const active = contractFilter === tab.id
                  return (
                    <button
                      key={tab.id}
                      type="button"
                      role="tab"
                      aria-selected={active}
                      onClick={() => { setContractFilter(tab.id) }}
                      className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
                        active
                          ? 'border-brand-orange bg-brand-orange text-content-primary'
                          : 'border-brand-border/30 bg-surface-panel text-content-secondary hover:border-brand-peach/50 hover:text-content-primary'
                      }`}
                    >
                      {tab.label}{' '}
                      <span className="ml-1 font-mono opacity-75">{count}</span>
                    </button>
                  )
                })}
              </div>

              <div className="w-full sm:w-72">
                <label htmlFor="protocol-contract-search" className="sr-only">
                  Filter contracts by name, role, or address
                </label>
                <input
                  id="protocol-contract-search"
                  type="search"
                  value={contractSearch}
                  onChange={(e) => { setContractSearch(e.target.value) }}
                  placeholder="Filter name, role, or 0x…"
                  className="w-full border border-brand-border/35 bg-surface-panel px-3 py-1.5 text-xs text-content-primary placeholder:text-content-tertiary focus:border-brand-peach focus:outline-none"
                />
              </div>
            </div>

            <Panel>
              {filteredContracts.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[760px] text-left text-sm">
                    <caption className="sr-only">
                      Verified on-chain protocol contracts and runtime bytecode hashes
                    </caption>
                    <thead className="border-b border-brand-border/25 bg-app-bg/40 text-xs font-medium text-content-secondary">
                      <tr>
                        <th scope="col" className="px-4 py-3">Contract &amp; role</th>
                        <th scope="col" className="px-4 py-3">Address</th>
                        <th scope="col" className="px-4 py-3">Pinned code hash</th>
                        <th scope="col" className="px-4 py-3 text-right">Bytecode</th>
                      </tr>
                    </thead>
                    <tbody>
                      {filteredContracts.map((contract) => (
                        <ContractRow
                          key={contract.key}
                          contract={contract}
                          chainId={chain.chainId}
                          explorerName={chain.explorerName}
                          copiedKey={copiedKey}
                          onCopy={handleCopy}
                        />
                      ))}
                    </tbody>
                  </table>
                </div>
              ) : (
                <EmptyState
                  title="No contracts match filter"
                  message="Try clearing the search query or switching contract categories."
                />
              )}
            </Panel>
          </section>

          <section aria-labelledby="provenance-title" className="space-y-3">
            <h2 id="provenance-title" className="text-lg font-semibold">
              Release history &amp; cryptographic provenance
            </h2>
            <div className="grid gap-4 lg:grid-cols-[1.2fr_1fr]">
              <Panel className="p-5 sm:p-6">
                <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                  Core Upgrade Timeline
                </p>
                <ol className="mt-4 space-y-4">
                  {[...data.releaseHistory].reverse().map((rel) => {
                    const isActive = rel.status === 'active'
                    return (
                      <li
                        key={rel.version}
                        className={`border-l-2 pl-4 ${
                          isActive ? 'border-positive' : 'border-brand-border/35'
                        }`}
                      >
                        <div className="flex flex-wrap items-center gap-2">
                          <span className="font-mono text-sm font-semibold text-content-primary">
                            Core {rel.version}
                          </span>
                          <StatusPill
                            tone={isActive ? 'live' : 'unknown'}
                            label={isActive ? 'Active' : 'Superseded'}
                          />
                          <span className="font-mono text-xs text-content-tertiary">
                            {rel.date}
                            {rel.deploymentBlock
                              ? ` · Block ${Number(rel.deploymentBlock).toLocaleString('en-US')}`
                              : ''}
                          </span>
                        </div>
                        <p className="mt-1.5 text-xs leading-5 text-content-secondary">
                          {rel.summary}
                        </p>
                      </li>
                    )
                  })}
                </ol>
              </Panel>

              <Panel className="p-5 sm:p-6">
                <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                  Active Release Provenance
                </p>
                <dl className="mt-4 divide-y divide-brand-border/15 text-xs">
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Release &amp; phase</dt>
                    <dd className="font-mono font-semibold text-content-primary">
                      {data.release.version ?? '—'} · {data.release.phase ?? '—'}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Schema / order interface</dt>
                    <dd className="font-mono text-content-primary">
                      Schema v{data.release.schemaVersion ?? '—'} · Order V
                      {data.release.orderInterfaceVersion ?? '—'}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Deployment block</dt>
                    <dd className="font-mono text-content-primary">
                      {data.release.deploymentBlock
                        ? Number(data.release.deploymentBlock).toLocaleString('en-US')
                        : '—'}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Source commit</dt>
                    <dd>
                      {data.release.sourceCommit ? (
                        <CopyableChip
                          value={data.release.sourceCommit}
                          display={shortHash(data.release.sourceCommit)}
                          copiedKey={copiedKey}
                          itemKey="prov:commit"
                          onCopy={handleCopy}
                        />
                      ) : (
                        <span className="font-mono text-content-tertiary">Unavailable</span>
                      )}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Compiler (Forge)</dt>
                    <dd className="font-mono text-content-primary">
                      {data.release.forgeVersion ?? '—'}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Manifest SHA-256</dt>
                    <dd>
                      {data.release.manifestSha256 ? (
                        <CopyableChip
                          value={data.release.manifestSha256}
                          display={shortHash(data.release.manifestSha256)}
                          copiedKey={copiedKey}
                          itemKey="prov:manifestSha"
                          onCopy={handleCopy}
                        />
                      ) : (
                        <span className="font-mono text-content-tertiary">Unavailable</span>
                      )}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Bundle SHA-256</dt>
                    <dd>
                      {data.release.bundleSha256 ? (
                        <CopyableChip
                          value={data.release.bundleSha256}
                          display={shortHash(data.release.bundleSha256)}
                          copiedKey={copiedKey}
                          itemKey="prov:bundleSha"
                          onCopy={handleCopy}
                        />
                      ) : (
                        <span className="font-mono text-content-tertiary">Unavailable</span>
                      )}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Pyth contract</dt>
                    <dd>
                      {data.release.pythAddress ? (
                        <a
                          href={explorerAddressUrl(data.release.pythAddress, chain.chainId)}
                          target="_blank"
                          rel="noreferrer"
                          className="font-mono text-brand-peach hover:underline"
                        >
                          {shortAddress(data.release.pythAddress)} ↗
                        </a>
                      ) : (
                        <span className="font-mono text-content-tertiary">Unavailable</span>
                      )}
                    </dd>
                  </div>
                </dl>
              </Panel>
            </div>
          </section>
        </>
      )}
    </div>
  )
}
