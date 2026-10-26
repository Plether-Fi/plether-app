import { PERPS_SETTLEMENT_TOKEN_LABEL } from '../contracts/perpsAddresses'

export function PerpsDeploymentPending() {
  return (
    <section role="status" className="mx-auto max-w-2xl border border-brand-border/30 bg-surface-panel p-8 text-center">
      <h1 className="text-2xl font-semibold text-content-primary">{PERPS_SETTLEMENT_TOKEN_LABEL} perps are being prepared</h1>
      <p className="mt-3 text-sm leading-6 text-content-secondary">
        Trading, testnet funding, and vault deposits will be available after deployment.
      </p>
    </section>
  )
}
