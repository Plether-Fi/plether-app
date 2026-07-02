import { type ReactNode, useMemo } from 'react'
import { useChainId } from 'wagmi'
import {
  getConfiguredApiBaseUrl,
  getConfiguredApiSource,
  getConfiguredApiWsUrl,
  useProtocolConfig,
} from '../../api'
import { BUILD_COMMIT, DEV_API_PROXY_TARGET } from '../../config/buildInfo'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../../contracts/perpsAddresses'
import { Modal } from '../ui/Modal'

interface BuildDetailsModalProps {
  isOpen: boolean
  onClose: () => void
}

type DetailValue = string | number | boolean | null | undefined
type DetailRows = object

function resolveHttpUrl(value: string): string {
  if (value.startsWith('http')) return value
  try {
    return new URL(value, window.location.origin).href
  } catch {
    return value
  }
}

function displayValue(value: DetailValue): string {
  if (value === null || value === undefined || value === '') return 'n/a'
  return String(value)
}

function workerBackendEnvKey(chainId: number): string {
  return chainId === 11155111 ? 'SEPOLIA_BACKEND_URL' : 'MAINNET_BACKEND_URL'
}

function proxyMode(apiSource: string): string {
  if (apiSource === 'VITE_API_URL') return 'direct'
  return import.meta.env.DEV ? 'Vite dev proxy' : 'Cloudflare worker proxy'
}

function proxyUpstream(apiSource: string, chainId: number): string {
  if (apiSource === 'VITE_API_URL') return 'not proxied'
  if (import.meta.env.DEV) return DEV_API_PROXY_TARGET
  return `runtime env ${workerBackendEnvKey(chainId)}`
}

function proxyRewrite(apiSource: string, chainId: number): string {
  if (apiSource === 'VITE_API_URL') return 'n/a'
  const upstream = import.meta.env.DEV ? DEV_API_PROXY_TARGET : `$${workerBackendEnvKey(chainId)}`
  return `${getConfiguredApiBaseUrl(chainId)}/* -> ${upstream}/api/*`
}

function DetailTable({ rows }: { rows: DetailRows }) {
  const entries = Object.entries(rows) as [string, DetailValue][]

  return (
    <dl className="grid grid-cols-[minmax(7rem,0.45fr)_minmax(0,1fr)] gap-x-4 gap-y-2 text-xs">
      {entries.map(([label, value]) => (
        <div key={label} className="contents">
          <dt className="text-content-secondary">{label}</dt>
          <dd className="min-w-0 break-all font-mono text-content-primary">{displayValue(value)}</dd>
        </div>
      ))}
    </dl>
  )
}

function Section({ title, children }: { title: string; children: ReactNode }) {
  return (
    <section className="space-y-3 border-t border-brand-border/30 pt-4 first:border-t-0 first:pt-0">
      <h3 className="text-sm font-semibold text-content-primary">{title}</h3>
      {children}
    </section>
  )
}

export function BuildDetailsModal({ isOpen, onClose }: BuildDetailsModalProps) {
  const chainId = useChainId()
  const protocolConfig = useProtocolConfig()

  const apiBaseUrl = getConfiguredApiBaseUrl(chainId)
  const apiSource = getConfiguredApiSource()
  const resolvedApiBaseUrl = useMemo(() => resolveHttpUrl(apiBaseUrl), [apiBaseUrl])
  const backendConfig = protocolConfig.data?.data
  const backendMeta = protocolConfig.data?.meta

  return (
    <Modal isOpen={isOpen} onClose={onClose} title="Build Details" size="xl">
      <div className="space-y-5">
        <Section title="Build">
          <DetailTable
            rows={{
              Commit: BUILD_COMMIT,
              Mode: import.meta.env.MODE,
              Origin: window.location.origin,
            }}
          />
        </Section>

        <Section title="Backend">
          <DetailTable
            rows={{
              Source: apiSource,
              'API base': apiBaseUrl,
              'Resolved API base': resolvedApiBaseUrl,
              WebSocket: getConfiguredApiWsUrl(chainId),
              'Wallet chain': chainId,
              'Proxy mode': proxyMode(apiSource),
              'Proxy upstream': proxyUpstream(apiSource, chainId),
              'Proxy rewrite': proxyRewrite(apiSource, chainId),
            }}
          />
        </Section>

        <Section title="Backend Protocol Config">
          {protocolConfig.isLoading ? (
            <p className="text-sm text-content-secondary">Loading protocol config...</p>
          ) : protocolConfig.isError ? (
            <p className="text-sm text-content-secondary">
              Protocol config unavailable from the selected backend.
            </p>
          ) : backendConfig ? (
            <div className="space-y-4">
              <DetailTable
                rows={{
                  'Config chain': backendConfig.chainId,
                  'Meta chain': backendMeta?.chainId,
                  'Meta block': backendMeta?.blockNumber,
                  Cached: backendMeta?.cached,
                  Stale: backendMeta?.stale,
                }}
              />
              <DetailTable rows={backendConfig.contracts} />
            </div>
          ) : (
            <p className="text-sm text-content-secondary">Protocol config returned no contract data.</p>
          )}
        </Section>

        <Section title="Perps Contracts">
          <DetailTable rows={{ Chain: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, ...PERPS_ARBITRUM_SEPOLIA }} />
        </Section>
      </div>
    </Modal>
  )
}
