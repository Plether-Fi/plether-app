import type { Competition, Standing } from '../api/types'

export function eligibilityPresentation(
  standing: Pick<Standing, 'eligible' | 'eligibilityStatus'>,
  competitionStatus: Competition['status'],
) {
  if (standing.eligible) return { label: 'Prize eligible', neutral: false, explanation: null }
  if (standing.eligibilityStatus === 'pending') {
    const beforeReview = competitionStatus === 'scheduled' || competitionStatus === 'live'
    return {
      label: beforeReview ? 'Registered' : 'Awaiting prize review',
      neutral: beforeReview,
      explanation: beforeReview
        ? 'Registration is complete. Prize eligibility will be reviewed after the competition ends.'
        : 'Registration is complete. Final prize eligibility is awaiting review.',
    }
  }
  if (standing.eligibilityStatus === 'under_review') {
    return { label: 'Prize review in progress', neutral: false, explanation: null }
  }
  return { label: 'Not eligible', neutral: false, explanation: null }
}
