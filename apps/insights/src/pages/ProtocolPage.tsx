import { useMemo, useState } from 'react'
import { Link } from 'react-router-dom'
import {
  InsightsApiError,
  PROTOCOL_CORE_GITHUB_REPO,
  deriveProtocolHealthBanner,
  deriveProtocolStateSummary,
  formatProtocolParamValue,
  parameterImpactHint,
  protocolCommitGitHubUrl,
  protocolContractGitHubUrl,
  protocolTreeGitHubUrl,
  resolveParameterRange,
  shortHash,
  useProtocolManifest,
  type ProtocolContractCategory,
  type ProtocolContractRecord,
  type ProtocolParameterCategory,
  type ProtocolParameterRecord,
  type ProtocolParameterStatus,
} from '../api'
import { ArbiscanButton, CopyButton } from '../components/ActionButtons'
import { AddressAvatar } from '../components/AddressAvatar'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard } from '../components/StatCard'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain } from '../utils/chain'
import {
  formatAssets,
  formatCompactAssets,
  formatPrice,
  formatUtc,
  shortAddress,
} from '../utils/format'

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

const TVL_BUCKET_COLORS: Record<string, string> = {
  lp_vaults: '#6EE7B7',
  margin_clearinghouse: '#FFB78B',
  kernel_v4_accounts: '#7CC4FA',
  other_contracts: '#C4B5FD',
}

function formatTvlShareBps(bps: number | null | undefined): string {
  if (bps == null || !Number.isFinite(bps)) return '0.00%'
  return `${(bps / 100).toFixed(2)}%`
}

function formatTvlAssetDisplay(raw: string | null | undefined): string {
  if (!raw || !/^\d+$/.test(raw)) return '—'
  const val = BigInt(raw)
  return val >= 10_000_000_000_000n ? formatCompactAssets(raw) : formatAssets(raw)
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
    <div className="flex min-w-0 flex-col justify-between gap-2.5 border border-brand-border/25 bg-app-bg p-3.5 sm:p-4">
      <div className="flex items-start justify-between gap-2.5">
        <div className="min-w-0 flex-1">
          <p className="text-sm font-semibold text-content-primary">{label}</p>
          <p className="mt-0.5 break-all font-mono text-[11px] text-content-tertiary">{source}</p>
        </div>
        <div className="shrink-0">
          <StatusPill tone={tone} label={statusLabel} />
        </div>
      </div>
      <p className="text-xs leading-5 text-content-secondary">{detail}</p>
    </div>
  )
}

function CopyableValue({
  value,
  display,
}: {
  value: string
  display: string
}) {
  return (
    <span className="inline-flex items-center gap-1.5 whitespace-nowrap font-mono text-xs text-content-primary">
      <span title={value}>{display}</span>
      <CopyButton value={value} />
    </span>
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

function GitHubIcon({ className = 'h-3 w-3 shrink-0' }: { className?: string }) {
  return (
    <svg
      viewBox="0 0 16 16"
      fill="currentColor"
      aria-hidden="true"
      className={className}
    >
      <path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27s1.36.09 2 .27c1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.01 8.01 0 0 0 16 8c0-4.42-3.58-8-8-8Z" />
    </svg>
  )
}

function ContractRow({
  contract,
  chainId,
  sourceCommit,
  settlementTokenIsMock,
  showArbiscan,
}: {
  contract: ProtocolContractRecord
  chainId: number
  sourceCommit: string | null
  settlementTokenIsMock: boolean | null
  showArbiscan: boolean
}) {
  const badge = contractVerifyPill(contract.verified)
  const github = protocolContractGitHubUrl(contract, sourceCommit, { settlementTokenIsMock })
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
              {github ? (
                <a
                  href={github.url}
                  target="_blank"
                  rel="noreferrer"
                  className="inline-flex items-center gap-1 border border-brand-border/25 bg-app-bg px-1.5 py-0.5 text-[11px] font-medium text-content-secondary transition-colors hover:border-brand-peach/50 hover:text-brand-peach"
                  title={`View ${github.path} in ${PROTOCOL_CORE_GITHUB_REPO}@${github.ref.slice(0, 8)}`}
                  aria-label={`View ${contract.name} source on GitHub`}
                >
                  <GitHubIcon />
                  <span>GitHub ↗</span>
                </a>
              ) : null}
            </div>
            <p className="mt-0.5 max-w-md text-xs leading-5 text-content-secondary">
              {contract.role}
            </p>
          </div>
        </div>
      </td>
      <td className="whitespace-nowrap px-4 py-3.5 align-top">
        <div className="flex items-center gap-1.5">
          <CopyableValue value={contract.address} display={shortAddress(contract.address)} />
          {showArbiscan ? (
            <ArbiscanButton
              kind="address"
              value={contract.address}
              chainId={chainId}
              label={`Inspect ${contract.name} on Arbiscan`}
            />
          ) : null}
        </div>
      </td>
      <td className="whitespace-nowrap px-4 py-3.5 align-top text-right font-mono text-xs tabular-nums">
        {contract.usdgBalanceAssets != null ? (
          <span
            className={
              contract.usdgBalanceAssets !== '0'
                ? 'font-semibold text-content-primary'
                : 'text-content-tertiary'
            }
          >
            {formatAssets(contract.usdgBalanceAssets)}
          </span>
        ) : (
          <span className="text-content-tertiary">—</span>
        )}
      </td>
      <td className="whitespace-nowrap px-4 py-3.5 align-top font-mono text-xs text-content-secondary">
        <div title={hashTitle}>
          <CopyableValue value={contract.expectedCodeHash} display={shortHash(contract.expectedCodeHash)} />
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

  const data = manifestQuery.data
  const chain = resolvePerpsChain(data?.network.chainId ?? ctx.chain.chainId)
  const sourceCommitUrl = protocolCommitGitHubUrl(data?.release.sourceCommit)
  const sourceCommitShort = sourceCommitUrl && data?.release.sourceCommit
    ? data.release.sourceCommit.trim().slice(0, 8)
    : null
  const isEndpointMissing =
    manifestQuery.isError
    && manifestQuery.error instanceof InsightsApiError
    && manifestQuery.error.status === 404

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

  // Deduplicate Arbiscan links across the entire Protocol page: the Verified
  // Contract Registry owns canonical links for any visible contract rows, and
  // callout badges only render an Arbiscan link if not already shown elsewhere.
  const registryExternalKeys = new Set(
    filteredContracts.map((c) => arbiscanLinkKey('address', c.address, chain.chainId)),
  )
  const seenOutsideRegistry = new Set<string>()
  function claimOutsideRegistry(address: string): boolean {
    const key = arbiscanLinkKey('address', address, chain.chainId)
    if (registryExternalKeys.has(key) || seenOutsideRegistry.has(key)) return false
    seenOutsideRegistry.add(key)
    return true
  }
  const showSettlementTokenArbiscan = data?.settlementToken
    ? claimOutsideRegistry(data.settlementToken.address)
    : false
  const showPauseCoordinatorArbiscan = pauseCoordinator
    ? claimOutsideRegistry(pauseCoordinator.address)
    : false
  const showPythArbiscan = data?.release.pythAddress
    ? claimOutsideRegistry(data.release.pythAddress)
    : false
  const seenRegistryRows = new Set<string>()

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
          <h1 className="text-2xl font-semibold tracking-tight sm:text-4xl">
            Contracts, Parameters &amp; Safety Controls
          </h1>
          <p className="text-sm leading-6 text-content-secondary">
            Live on-chain bytecode hash verification against the pinned Core release manifest,
            real-time risk parameters compared to release defaults, and emergency pause controls.
          </p>
        </div>

        {data?.settlementToken ? (
          <div className="min-w-0 max-w-full border border-brand-border/30 bg-surface-panel px-3.5 py-2.5 text-xs sm:px-4">
            <p className="text-content-tertiary">Canonical settlement asset</p>
            <div className="mt-1 flex flex-wrap items-center gap-2 font-mono">
              <span className="font-semibold text-content-primary">
                {data.settlementToken.symbol ?? 'USDG'}
              </span>
              <span className="text-content-tertiary">
                ({data.settlementToken.decimals ?? 6} decimals
                {data.settlementToken.isMock ? ' · testnet mock' : ''})
              </span>
              <CopyableValue
                value={data.settlementToken.address}
                display={shortAddress(data.settlementToken.address)}
              />
              {showSettlementTokenArbiscan ? (
                <ArbiscanButton
                  kind="address"
                  value={data.settlementToken.address}
                  chainId={chain.chainId}
                />
              ) : null}
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
          {data.tvl ? (
            <section aria-labelledby="protocol-tvl-title" className="space-y-3">
              <div className="flex flex-wrap items-baseline justify-between gap-3">
                <div>
                  <h2 id="protocol-tvl-title" className="text-lg font-semibold">
                    Total Protocol TVL (Contracts &amp; Kernel v4 User Margin)
                  </h2>
                  <p className="mt-1 text-xs text-content-tertiary">
                    Atomic on-chain <code className="font-mono">USDG.balanceOf</code> sum at block{' '}
                    {Number(data.tvl.verifiedAtBlock).toLocaleString('en-US')} across all pinned
                    protocol contracts and ZeroDev Kernel v4 trading accounts — strictly disjoint
                    with zero double counting
                  </p>
                </div>
                <div className="flex flex-wrap items-center gap-2">
                  <StatusPill
                    tone={
                      !data.tvl.available
                        ? 'unknown'
                        : data.tvl.isComplete
                          ? 'live'
                          : 'degraded'
                    }
                    label={
                      !data.tvl.available
                        ? 'TVL unavailable'
                        : data.tvl.isComplete
                          ? 'Complete on-chain sum'
                          : 'Partial snapshot'
                    }
                  />
                  <Link
                    to="/trading-accounts"
                    className="font-mono text-xs font-semibold text-brand-peach hover:underline"
                  >
                    Kernel v4 accounts →
                  </Link>
                </div>
              </div>

              {data.tvl.available && data.tvl.totalTvlAssets != null ? (
                <Panel className="space-y-5 p-4 sm:p-6">
                  <div className="flex flex-wrap items-end justify-between gap-4 border-b border-brand-border/20 pb-5">
                    <div className="min-w-0">
                      <p className="font-mono text-[11px] font-semibold uppercase tracking-[0.14em] text-brand-peach">
                        Total Value Locked (All Contracts + Kernel v4 Margin)
                      </p>
                      <p className="mt-1 break-words font-mono text-3xl font-bold tracking-tight tabular-nums text-content-primary sm:text-4xl">
                        {formatAssets(data.tvl.totalTvlAssets)}
                      </p>
                    </div>

                    <div className="grid w-full grid-cols-1 gap-3 font-mono text-xs sm:w-auto sm:grid-cols-3">
                      <div className="border border-brand-border/20 bg-app-bg px-3.5 py-2.5">
                        <div className="text-[11px] text-content-tertiary">Protocol contracts</div>
                        <div className="mt-0.5 font-semibold tabular-nums text-content-primary">
                          {formatTvlAssetDisplay(data.tvl.protocolContractsTvlAssets)}
                        </div>
                        <div className="mt-0.5 text-[10px] text-content-tertiary">
                          {String(data.tvl.contractsWithBalance ?? 0)} funded /{' '}
                          {String(data.tvl.contractsTracked ?? data.contracts.length)} pinned
                        </div>
                      </div>
                      <div className="border border-brand-border/20 bg-app-bg px-3.5 py-2.5">
                        <div className="text-[11px] text-content-tertiary">
                          Kernel v4 smart wallets
                        </div>
                        <div className="mt-0.5 font-semibold tabular-nums text-[#7CC4FA]">
                          {formatTvlAssetDisplay(data.tvl.kernelTradingAccountsAssets)}
                        </div>
                        <div className="mt-0.5 text-[10px] text-content-tertiary">
                          {data.tvl.kernelAccountsAvailable
                            ? `${String(data.tvl.kernelAccountsFunded ?? 0)} funded / ${String(data.tvl.kernelAccountsTracked ?? 0)} tracked`
                            : 'Account ledger unavailable'}
                        </div>
                      </div>
                      <div className="border border-brand-border/20 bg-app-bg px-3.5 py-2.5">
                        <div className="text-[11px] text-content-tertiary">
                          Combined user margin
                        </div>
                        <div className="mt-0.5 font-semibold tabular-nums text-brand-peach">
                          {formatTvlAssetDisplay(data.tvl.totalTraderMarginAssets)}
                        </div>
                        <div className="mt-0.5 text-[10px] text-content-tertiary">
                          Clearinghouse + Kernel v4
                        </div>
                      </div>
                    </div>
                  </div>

                  {data.tvl.buckets && data.tvl.buckets.length > 0 ? (
                    <div className="space-y-4">
                      <div
                        className="flex h-3 w-full overflow-hidden bg-app-bg ring-1 ring-inset ring-brand-border/25"
                        role="img"
                        aria-label="Protocol TVL distribution across LP pools, Margin Clearinghouse, Kernel v4 trading accounts, and other contracts"
                      >
                        {data.tvl.buckets.map((bucket) => {
                          const sharePct =
                            bucket.shareBps != null ? Math.max(0, bucket.shareBps / 100) : 0
                          if (sharePct <= 0) return null
                          const color = TVL_BUCKET_COLORS[bucket.key] ?? '#FFB78B'
                          return (
                            <div
                              key={bucket.key}
                              style={{
                                width: `${sharePct.toFixed(2)}%`,
                                backgroundColor: color,
                              }}
                              title={`${bucket.label}: ${formatAssets(bucket.assets)} (${formatTvlShareBps(bucket.shareBps)})`}
                              className="h-full border-r border-surface-panel/70 last:border-r-0"
                            />
                          )
                        })}
                      </div>

                      <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
                        {data.tvl.buckets.map((bucket) => {
                          const color = TVL_BUCKET_COLORS[bucket.key] ?? '#FFB78B'
                          const isLp = bucket.key === 'lp_vaults'
                          return (
                            <div
                              key={bucket.key}
                              className="flex min-w-0 flex-col justify-between border border-brand-border/25 bg-app-bg p-3.5"
                            >
                              <div>
                                <div className="flex items-center justify-between gap-2">
                                  <div className="flex min-w-0 items-center gap-2">
                                    <span
                                      className="h-2.5 w-2.5 shrink-0"
                                      style={{ backgroundColor: color }}
                                      aria-hidden="true"
                                    />
                                    <span className="truncate text-xs font-semibold text-content-primary">
                                      {bucket.label}
                                    </span>
                                  </div>
                                  <span className="shrink-0 font-mono text-xs font-semibold tabular-nums text-content-secondary">
                                    {formatTvlShareBps(bucket.shareBps)}
                                  </span>
                                </div>
                                <div className="mt-2 break-words font-mono text-base font-bold tabular-nums text-content-primary">
                                  {formatTvlAssetDisplay(bucket.assets)}
                                </div>
                                <p className="mt-1 text-[11px] leading-4 text-content-tertiary">
                                  {bucket.detail}
                                </p>
                              </div>

                              {isLp ? (
                                <div className="mt-2.5 space-y-0.5 border-t border-brand-border/15 pt-2 font-mono text-[10px] tabular-nums text-content-tertiary">
                                  <div className="flex justify-between gap-2">
                                    <span>HousePool</span>
                                    <span className="text-content-secondary">
                                      {formatTvlAssetDisplay(data.tvl?.housePoolAssets)}
                                    </span>
                                  </div>
                                  <div className="flex justify-between gap-2">
                                    <span>Senior Vault</span>
                                    <span className="text-content-secondary">
                                      {formatTvlAssetDisplay(data.tvl?.seniorVaultAssets)}
                                    </span>
                                  </div>
                                  <div className="flex justify-between gap-2">
                                    <span>Junior Vault</span>
                                    <span className="text-content-secondary">
                                      {formatTvlAssetDisplay(data.tvl?.juniorVaultAssets)}
                                    </span>
                                  </div>
                                </div>
                              ) : (
                                <div className="mt-2.5 border-t border-brand-border/15 pt-2 font-mono text-[10px] text-content-tertiary">
                                  {bucket.key === 'kernel_v4_accounts'
                                    ? `${String(bucket.addressCount)} funded wallet${bucket.addressCount === 1 ? '' : 's'} of ${String(data.tvl?.kernelAccountsTracked ?? 0)} tracked`
                                    : `${String(bucket.addressCount)} funded contract${bucket.addressCount === 1 ? '' : 's'}`}
                                </div>
                              )}
                            </div>
                          )
                        })}
                      </div>
                    </div>
                  ) : null}

                  <div className="grid gap-4 border-t border-brand-border/20 pt-5 lg:grid-cols-2">
                    <div className="min-w-0 space-y-2.5">
                      <div className="flex items-baseline justify-between gap-2">
                        <h3 className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                          Funded Protocol Contracts
                        </h3>
                        <span className="font-mono text-[11px] text-content-tertiary">
                          {String(data.tvl.contractHoldings?.length ?? 0)} holding USDG
                        </span>
                      </div>
                      {data.tvl.contractHoldings && data.tvl.contractHoldings.length > 0 ? (
                        <ul className="divide-y divide-brand-border/15 border border-brand-border/20 bg-app-bg">
                          {data.tvl.contractHoldings.map((holding) => (
                            <li
                              key={holding.key}
                              className="flex flex-wrap items-center justify-between gap-2 px-3.5 py-2.5 text-xs"
                            >
                              <div className="min-w-0">
                                <div className="flex flex-wrap items-center gap-2">
                                  <span className="font-semibold text-content-primary">
                                    {holding.name}
                                  </span>
                                  <span className="font-mono text-[11px] text-content-tertiary">
                                    {shortAddress(holding.address)}
                                  </span>
                                </div>
                              </div>
                              <div className="flex shrink-0 items-baseline gap-2.5 font-mono tabular-nums">
                                <span className="font-semibold text-content-primary">
                                  {formatAssets(holding.assets)}
                                </span>
                                <span className="w-14 text-right text-[11px] text-content-tertiary">
                                  {formatTvlShareBps(holding.shareBps)}
                                </span>
                              </div>
                            </li>
                          ))}
                        </ul>
                      ) : (
                        <div className="border border-brand-border/20 bg-app-bg px-3.5 py-4 text-xs text-content-tertiary">
                          No pinned protocol contracts currently hold non-zero USDG balances.
                        </div>
                      )}
                    </div>

                    <div className="min-w-0 space-y-2.5">
                      <div className="flex items-baseline justify-between gap-2">
                        <h3 className="text-xs font-semibold uppercase tracking-[0.14em] text-[#7CC4FA]">
                          Top Funded Kernel v4 Trading Accounts
                        </h3>
                        <span className="font-mono text-[11px] text-content-tertiary">
                          {String(data.tvl.kernelAccountsFunded ?? 0)} funded /{' '}
                          {String(data.tvl.kernelAccountsTracked ?? 0)} tracked
                        </span>
                      </div>
                      {data.tvl.topKernelAccounts && data.tvl.topKernelAccounts.length > 0 ? (
                        <ul className="divide-y divide-brand-border/15 border border-brand-border/20 bg-app-bg">
                          {data.tvl.topKernelAccounts.map((acct) => (
                            <li
                              key={acct.address}
                              className="flex flex-wrap items-center justify-between gap-2 px-3.5 py-2.5 text-xs"
                            >
                              <div className="flex min-w-0 items-center gap-2">
                                <AddressAvatar address={acct.address} size={16} />
                                <Link
                                  to={`/trading-accounts/${acct.address}`}
                                  className="font-mono font-semibold text-brand-peach hover:underline"
                                >
                                  {shortAddress(acct.address)}
                                </Link>
                                <Link
                                  to={`/wallets/${acct.address}`}
                                  className="font-mono text-[11px] text-content-tertiary hover:text-content-primary hover:underline"
                                >
                                  wallet
                                </Link>
                              </div>
                              <div className="flex shrink-0 items-baseline gap-2.5 font-mono tabular-nums">
                                <span className="font-semibold text-content-primary">
                                  {formatAssets(acct.assets)}
                                </span>
                                <span className="w-14 text-right text-[11px] text-content-tertiary">
                                  {formatTvlShareBps(acct.shareBps)}
                                </span>
                              </div>
                            </li>
                          ))}
                        </ul>
                      ) : (
                        <div className="border border-brand-border/20 bg-app-bg px-3.5 py-4 text-xs text-content-tertiary">
                          {data.tvl.kernelAccountsAvailable
                            ? 'All tracked Kernel v4 trading accounts currently hold 0.00 USDG outside MarginClearinghouse.'
                            : 'Kernel v4 trading account ledger is currently unavailable.'}
                        </div>
                      )}
                    </div>
                  </div>
                </Panel>
              ) : (
                <Panel className="p-5">
                  <EmptyState
                    title="Protocol TVL reads unavailable"
                    message="Live on-chain USDG balance reads across protocol contracts and Kernel v4 trading accounts could not reach RPC."
                  />
                </Panel>
              )}
            </section>
          ) : null}

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
                <div className="inline-flex items-center gap-1.5 font-mono text-xs text-content-secondary">
                  <span>EmergencyPauseCoordinator</span>
                  <CopyableValue
                    value={pauseCoordinator.address}
                    display={shortAddress(pauseCoordinator.address)}
                  />
                  {showPauseCoordinatorArbiscan ? (
                    <ArbiscanButton
                      kind="address"
                      value={pauseCoordinator.address}
                      chainId={chain.chainId}
                    />
                  ) : null}
                </div>
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
                <p className="mt-1 flex flex-wrap items-center gap-x-2 gap-y-1 text-xs text-content-tertiary">
                  <span>
                    All {String(data.contracts.length)} pinned contracts hashed on-chain via{' '}
                    <code className="font-mono">eth_getCode</code> + <code className="font-mono">keccak256</code>
                  </span>
                  <span aria-hidden="true">·</span>
                  <a
                    href={protocolTreeGitHubUrl(data.release.sourceCommit)}
                    target="_blank"
                    rel="noreferrer"
                    className="inline-flex items-center gap-1 font-mono text-brand-peach hover:underline"
                    title={`Browse verified Solidity source in ${PROTOCOL_CORE_GITHUB_REPO}`}
                  >
                    <GitHubIcon />
                    <span>
                      {PROTOCOL_CORE_GITHUB_REPO}
                      {sourceCommitShort ? `@${sourceCommitShort}` : ''} ↗
                    </span>
                  </a>
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
                        <th scope="col" className="px-4 py-3 text-right">USDG balance</th>
                        <th scope="col" className="px-4 py-3">Pinned code hash</th>
                        <th scope="col" className="px-4 py-3 text-right">Bytecode</th>
                      </tr>
                    </thead>
                    <tbody>
                      {filteredContracts.map((contract) => {
                        const key = arbiscanLinkKey('address', contract.address, chain.chainId)
                        const showArbiscan = !seenRegistryRows.has(key)
                        if (showArbiscan) seenRegistryRows.add(key)
                        return (
                          <ContractRow
                            key={contract.key}
                            contract={contract}
                            chainId={chain.chainId}
                            sourceCommit={data.release.sourceCommit}
                            settlementTokenIsMock={data.settlementToken.isMock}
                            showArbiscan={showArbiscan}
                          />
                        )
                      })}
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
                        <div className="flex items-center gap-2">
                          <CopyableValue
                            value={data.release.sourceCommit}
                            display={shortHash(data.release.sourceCommit)}
                          />
                          {sourceCommitUrl ? (
                            <a
                              href={sourceCommitUrl}
                              target="_blank"
                              rel="noreferrer"
                              className="inline-flex items-center gap-1 font-semibold text-brand-peach hover:underline"
                              title={`Inspect commit ${data.release.sourceCommit} on GitHub`}
                            >
                              <GitHubIcon />
                              <span>GitHub ↗</span>
                            </a>
                          ) : null}
                        </div>
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
                        <CopyableValue
                          value={data.release.manifestSha256}
                          display={shortHash(data.release.manifestSha256)}
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
                        <CopyableValue
                          value={data.release.bundleSha256}
                          display={shortHash(data.release.bundleSha256)}
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
                        <div className="inline-flex items-center gap-1.5">
                          <CopyableValue
                            value={data.release.pythAddress}
                            display={shortAddress(data.release.pythAddress)}
                          />
                          {showPythArbiscan ? (
                            <ArbiscanButton
                              kind="address"
                              value={data.release.pythAddress}
                              chainId={chain.chainId}
                            />
                          ) : null}
                        </div>
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
