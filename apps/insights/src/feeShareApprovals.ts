// Organizer decisions are scoped to a competition and immutable wallet identity,
// never to a mutable display name. This registry does not affect cash prizes.
export const manualFeeShareApprovals = [
  {
    competitionSlug: 'testnet-trading-2026-09',
    address: '0x505ae6017be53b8dde88cff349c873eca8dd4cb5',
    handle: 'nightpuper',
    approvedAt: '2026-10-08',
  },
  {
    competitionSlug: 'testnet-trading-2026-09',
    address: '0x07f5bdb61891a09d58e0ee2c373c287e9526386b',
    handle: 'FishesReal72265',
    approvedAt: '2026-10-08',
  },
] as const

export const manualFeeShareExplanation = 'Approved personally by Stan after a verification call. Normal fee-share eligibility conditions were waived by his decision.'

export function manualFeeShareApproval(competitionSlug: string, address: string) {
  return manualFeeShareApprovals.find((approval) =>
    approval.competitionSlug === competitionSlug && approval.address === address.toLowerCase(),
  )
}
