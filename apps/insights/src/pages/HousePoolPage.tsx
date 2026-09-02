import { Link } from 'react-router-dom'
import { useCurrentProtocolRelease, useHousePool } from '../api'
import { AvailabilityList, DisplayValue, EvidenceBadge, Metric, ObjectTable, PageTitle, ProtocolMeta, Section } from '../components/Protocol'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { formatUsdc } from '../utils/format'
import { readRecord, readString } from '../utils/protocol'

export function HousePoolPage() {
  const release = useCurrentProtocolRelease()
  const releaseId = release.data?.releaseId ?? ''
  const query = useHousePool(releaseId)

  if (release.isLoading || query.isLoading) return <Panel><LoadingState rows={9} /></Panel>
  if (query.isError) return <ErrorState title="HousePool state unavailable" message={query.error.message} onRetry={() => void query.refetch()} />
  const data = query.data
  if (!data) return <ErrorState title="HousePool state unavailable" />
  const house = readRecord(data.housePool)
  const accounting = readRecord(house.accounting)
  const assets = readRecord(house.assets)
  const liability = readRecord(house.boundedLiability)
  const protocolStatus = readRecord(house.protocolStatus)
  const maximumLiabilityUsdc = maximumBoundedLiability(liability)
  const solvencyHeadroomUsdc = readString(house.solvencyHeadroomUsdc)
  const traderClaimsUsdc = readString(house.traderClaimsUsdc)
  const badDebtUsdc = readString(house.badDebtUsdc)
  const coverageBasis = readRecord(house.coverageBasis)
  const waterfall = readRecord(house.waterfall)

  return (
    <div className="space-y-7">
      <PageTitle title="HousePool" description="Assets, liquidity reservations, bounded trader liabilities, solvency headroom, oracle state, and the current Senior/Junior waterfall at one confirmed block." />
      <ProtocolMeta data={data} />
      <AvailabilityList items={data.availability} />
      <section className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
        <Metric label="Raw assets" value={formatUsdc(readString(assets.rawUsdc))} hint="Literal USDC balance held by the HousePool." />
        <Metric label="Accounted assets" value={formatUsdc(readString(assets.accountedUsdc) ?? readString(accounting.totalAssetsUsdc))} hint="USDC recognized by protocol accounting." />
        <Metric label="Physical assets" value={formatUsdc(readString(assets.physicalUsdc))} hint="Conservative backing: min(raw, accounted)." />
        <Metric label="Excess assets" value={formatUsdc(readString(assets.excessUsdc))} hint="Unsolicited USDC not yet assigned to protocol economics." />
        <Metric label="Free USDC" value={formatUsdc(readString(accounting.freeUsdc))} hint="Liquidity not presently reserved by pool accounting." />
        <Metric
          label="Outstanding trader claims"
          value={<MetricWithEvidence value={formatUsdc(traderClaimsUsdc)} evidence={data.evidence.traderClaims} />}
          hint="Confirmed trader settlement obligations included in the coverage denominator."
          tone={isPositiveUnsigned(traderClaimsUsdc) ? 'warning' : 'default'}
        />
        <Metric
          label="Accumulated bad debt"
          value={<MetricWithEvidence value={formatUsdc(badDebtUsdc)} evidence={data.evidence.badDebt} />}
          hint="Collateral shortfall already recognized by the engine."
          tone={isPositiveUnsigned(badDebtUsdc) ? 'critical' : 'default'}
        />
        <Metric label="Maximum bounded liability" value={formatUsdc(maximumLiabilityUsdc)} hint="The larger of LONG or SHORT maximum-profit liability; directional liabilities are not added together." />
        <Metric
          label="Gross coverage ratio"
          value={<MetricWithEvidence value={formatBps(readString(house.grossCoverageRatioBps))} evidence={data.evidence.coverageRatio} />}
          hint={`Physical backing divided by maximum bounded liability plus trader claims. Basis: ${readString(coverageBasis.numerator) ?? 'unavailable'}.`}
        />
        <Metric
          label="Solvency headroom"
          value={<MetricWithEvidence value={formatUsdc(solvencyHeadroomUsdc)} evidence={data.evidence.solvencyHeadroom} />}
          tone={solvencyHeadroomUsdc?.startsWith('-') === true ? 'critical' : solvencyHeadroomUsdc === null ? 'default' : 'positive'}
          hint="Physical backing minus maximum bounded liability and outstanding trader claims."
        />
      </section>
      <div className="grid gap-5 lg:grid-cols-2">
        <Section title="Asset recognition" description="Raw, accounted, excess, and physical backing at the same confirmed block."><ObjectTable value={assets} evidence={data.evidence.poolState} /></Section>
        <Section title="Pool accounting" description="Free cash, protected withdrawals, pending recapitalization/revenue, principals, and protocol state."><ObjectTable value={accounting} evidence={data.evidence.poolState} /></Section>
        <Section title="Bounded trader liability" description="Maximum pool-side payout by directional side."><ObjectTable value={liability} evidence={data.evidence.boundedLiability} /></Section>
        <Section title="Coverage basis" description="Formula inputs and provenance for gross coverage and solvency headroom."><ObjectTable value={coverageBasis} evidence={data.evidence.coverageRatio} /></Section>
        <Section title="Protocol operating state" description="Market phase, oracle freeze, FAD, trading, and withdrawal state read at the same confirmed block.">
          <div className="flex items-center justify-between gap-3 border-b border-brand-border/15 px-5 py-3 text-xs text-content-tertiary">
            <span>Read-set provenance</span>
            <EvidenceBadge level={data.evidence.protocolStatus} />
          </div>
          <ObjectTable value={protocolStatus} />
        </Section>
        <Section title="Governance and dependency state" description="Current HousePool ownership, role, dependency, and policy getters. Each row retains its own historical-read evidence.">
          <GovernanceStateTable value={house.governanceState} evidence={data.evidence.governanceState} />
        </Section>
      </div>
      <Section title="Waterfall position" description="Junior absorbs first loss; Senior impairment is measured against the high-water mark.">
        <ObjectTable value={waterfall} evidence={data.evidence.poolState} />
        <div className="grid gap-3 border-t border-brand-border/15 p-5 sm:grid-cols-2">
          <Link to="/house-pool/senior" className="border border-brand-border/30 p-4 hover:border-brand-peach"><strong>Senior tranche</strong><span className="mt-1 block text-sm text-content-secondary">Principal, NAV, impairment, shares, epochs, and full indexed history →</span></Link>
          <Link to="/house-pool/junior" className="border border-brand-border/30 p-4 hover:border-brand-peach"><strong>Junior tranche</strong><span className="mt-1 block text-sm text-content-secondary">First-loss buffer, NAV, shares, flows, and full indexed history →</span></Link>
        </div>
      </Section>
    </div>
  )
}

function formatBps(value: string | null): string {
  if (value === null) return 'unavailable'
  return `${(Number(value) / 100).toFixed(2)}%`
}

function maximumBoundedLiability(liability: Record<string, unknown>): string | null {
  const explicitMaximum = readString(liability.maximumUsdc)
  if (explicitMaximum !== null) return explicitMaximum

  const long = parseUnsignedUnits(readString(liability.longUsdc))
  const short = parseUnsignedUnits(readString(liability.shortUsdc))
  if (long === null || short === null) return null
  return (long > short ? long : short).toString()
}

function parseUnsignedUnits(value: string | null): bigint | null {
  if (value === null || !/^\d+$/.test(value)) return null
  try {
    return BigInt(value)
  } catch {
    return null
  }
}

function isPositiveUnsigned(value: string | null): boolean {
  const parsed = parseUnsignedUnits(value)
  return parsed !== null && parsed > 0n
}

function MetricWithEvidence({ value, evidence }: { value: string; evidence: unknown }) {
  return (
    <span className="flex flex-wrap items-center gap-2">
      <span>{value}</span>
      <EvidenceBadge level={evidence} />
    </span>
  )
}

function GovernanceStateTable({ value, evidence }: { value: unknown; evidence: unknown }) {
  const rows = Array.isArray(value)
    ? value.map(readRecord)
    : []

  return (
    <div>
      <div className="flex items-center justify-between gap-3 border-b border-brand-border/15 px-5 py-3 text-xs text-content-tertiary">
        <span>Read-set provenance</span>
        <EvidenceBadge level={evidence} />
      </div>
      {rows.length === 0 ? (
        <EmptyState
          title="Governance state unavailable"
          message="No HousePool governance or dependency getter could be read at this confirmed block."
        />
      ) : (
        <div className="overflow-x-auto">
          <table className="w-full min-w-[760px] text-left text-sm">
            <thead>
              <tr className="border-b border-brand-border/20 text-xs uppercase tracking-wide text-content-tertiary">
                <th className="px-5 py-3">Role or dependency</th>
                <th className="px-5 py-3">Current value</th>
                <th className="px-5 py-3">Source</th>
                <th className="px-5 py-3">Evidence</th>
              </tr>
            </thead>
            <tbody>
              {rows.map((row, index) => {
                const definition = readRecord(row.definition)
                const key = readString(definition.key) ?? `governance-read-${String(index)}`
                const sourceContract = readString(definition.sourceContract)
                const getter = readString(definition.getter)
                return (
                  <tr key={key} className="border-b border-brand-border/10 align-top last:border-0">
                    <th scope="row" className="px-5 py-4">
                      <code className="break-all text-xs text-brand-peach">{key}</code>
                      <span className="mt-1 block text-xs font-normal text-content-tertiary">
                        {readString(definition.description) ?? 'Current governance or dependency address.'}
                      </span>
                    </th>
                    <td className="px-5 py-4">
                      <DisplayValue
                        field={key}
                        value={row.formattedValue ?? row.rawValue}
                      />
                    </td>
                    <td className="px-5 py-4 text-xs text-content-secondary">
                      <span className="block">{sourceContract ?? 'Unavailable'}</span>
                      <code className="mt-1 block break-all text-content-tertiary">{getter ?? 'Getter unavailable'}</code>
                      <DisplayValue value={row.sourceAddress} />
                    </td>
                    <td className="px-5 py-4"><EvidenceBadge level={row.evidence} /></td>
                  </tr>
                )
              })}
            </tbody>
          </table>
        </div>
      )}
    </div>
  )
}
