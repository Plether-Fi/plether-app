import type { LpSettlementRecord, ReturnReportAudit } from '../api'

export type ContinuityLevel = 'clear' | 'warning' | 'blocked' | 'unknown'

export interface ContinuityBlock {
  id: string
  label: string
  level: ContinuityLevel
  detail: string
  timestamp: string
  epoch?: number
}

export interface ResidualAccuracyResult {
  available: boolean
  exact: boolean
  attributedPct: number | null
  residualPct: number | null
}

export function classifySettlementBlock(row: LpSettlementRecord): ContinuityLevel {
  if (row.status === 'confirmed_revert' || row.status === 'failed' || row.status === 'manual_review') {
    return 'blocked'
  }
  if (
    row.status === 'pending'
    || row.status === 'broadcast'
    || row.status === 'confirming'
    || row.status === 'replaced'
  ) {
    return 'warning'
  }
  if (row.status === 'confirmed_success') {
    const hasAdvisory =
      row.seniorBacklog === true
      || row.juniorBacklog === true
      || row.entriesDeferred === true
      || row.replacementCount > 0
    return hasAdvisory ? 'warning' : 'clear'
  }
  return 'unknown'
}

export function classifyReportBlock(report: ReturnReportAudit): ContinuityLevel {
  if (report.unexplainedUsdg === null && report.unexplainedPp === null) return 'unknown'
  if (report.unexplainedUsdg === '0' || report.unexplainedPp === 0) return 'clear'
  if (report.unexplainedPp !== null && Math.abs(report.unexplainedPp) >= 0.05) return 'blocked'
  return 'warning'
}

function absBigInt(n: bigint): bigint {
  return n < 0n ? -n : n
}

export function computeResidualAccuracy(report: ReturnReportAudit): ResidualAccuracyResult {
  if (report.unexplainedUsdg === null && report.unexplainedPp === null) {
    return { available: false, exact: false, attributedPct: null, residualPct: null }
  }

  if (report.unexplainedUsdg === '0' || report.unexplainedPp === 0) {
    return { available: true, exact: true, attributedPct: 100, residualPct: 0 }
  }

  if (
    report.netReturnUsdg !== null
    && report.unexplainedUsdg !== null
    && /^-?\d+$/.test(report.netReturnUsdg)
    && /^-?\d+$/.test(report.unexplainedUsdg)
  ) {
    const net = BigInt(report.netReturnUsdg)
    const unexp = BigInt(report.unexplainedUsdg)
    const attrAbs = absBigInt(net - unexp)
    const unexpAbs = absBigInt(unexp)
    const denom = attrAbs + unexpAbs
    if (denom > 0n) {
      const attrPct = Number((attrAbs * 10_000n) / denom) / 100
      return {
        available: true,
        exact: false,
        attributedPct: attrPct,
        residualPct: Math.max(0, 100 - attrPct),
      }
    }
  }

  if (report.reportedReturnPp !== null && report.unexplainedPp !== null) {
    const attrAbs = Math.abs(report.reportedReturnPp - report.unexplainedPp)
    const unexpAbs = Math.abs(report.unexplainedPp)
    const denom = attrAbs + unexpAbs
    if (denom > 0) {
      const attrPct = (attrAbs / denom) * 100
      return {
        available: true,
        exact: false,
        attributedPct: attrPct,
        residualPct: Math.max(0, 100 - attrPct),
      }
    }
  }

  return { available: true, exact: false, attributedPct: 0, residualPct: 100 }
}
