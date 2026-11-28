import { useInsightsStatus } from '../api'
import { useLiveDivisionsSummary } from '../divisions/query'
import { resolvePerpsChain } from '../utils/chain'

export function useProtocolContext() {
  const statusQuery = useInsightsStatus()
  const liveQuery = useLiveDivisionsSummary()
  const live = liveQuery.data?.status === 'ready' ? liveQuery.data : null
  const chainId = statusQuery.data?.chainId ?? null
  const chain = resolvePerpsChain(chainId)

  return {
    chainId,
    chain,
    releaseVersion: live?.scoringRelease?.version ?? null,
    blockNumber: live?.snapshotBlock ?? null,
    observedAt: live?.snapshotAt ?? null,
  }
}
