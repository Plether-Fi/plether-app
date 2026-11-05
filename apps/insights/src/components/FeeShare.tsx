import { manualFeeShareApprovals, manualFeeShareExplanation } from '../feeShareApprovals'
import { Panel, WalletIdentity } from './ui'

export function ManualFeeShareCategory({ competitionSlug }: { competitionSlug: string }) {
  const approvals = manualFeeShareApprovals.filter((approval) => approval.competitionSlug === competitionSlug)
  if (!approvals.length) return null
  return (
    <Panel className="space-y-4 border-positive/30 p-5 sm:p-7">
      <div>
        <p className="text-xs font-semibold uppercase tracking-wider text-positive">Fee share</p>
        <h2 className="mt-2 text-xl font-semibold">Eligible after manual verification</h2>
        <p className="mt-2 text-sm leading-6 text-content-secondary">{manualFeeShareExplanation} This approval applies to fee share only; competition rankings and cash prizes are unchanged.</p>
      </div>
      <ul className="grid gap-4 sm:grid-cols-2">
        {approvals.map((approval) => (
          <li key={approval.address}>
            <WalletIdentity address={approval.address} displayName={approval.handle} competitionSlug={competitionSlug} />
            <p className="mt-1 text-xs text-content-tertiary">Approved <time dateTime={approval.approvedAt}>{approval.approvedAt}</time></p>
          </li>
        ))}
      </ul>
    </Panel>
  )
}
