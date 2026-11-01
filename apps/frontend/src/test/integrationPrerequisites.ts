type RequiredMode = 'FRONTEND_INTEGRATION_REQUIRED' | 'PERPS_FORK_REQUIRED'

export const frontendIntegrationState = { anvilAvailable: false }

/** Missing optional fixtures are skips; required jobs must fail before any skip. */
export function integrationPrerequisitesAvailable(
  requiredMode: RequiredMode,
  suite: string,
  prerequisites: readonly { available: boolean; requirement: string }[],
  cause?: unknown,
): boolean {
  const missing = prerequisites
    .filter(prerequisite => !prerequisite.available)
    .map(prerequisite => prerequisite.requirement)
  if (missing.length > 0 && process.env[requiredMode] === '1') {
    throw new Error(`${suite}: ${requiredMode}=1 requires ${missing.join('; ')}`, { cause })
  }
  return missing.length === 0
}
