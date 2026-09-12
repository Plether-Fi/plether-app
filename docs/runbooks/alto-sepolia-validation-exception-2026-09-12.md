# Approved Arbitrum Sepolia Alto validation exception

The owner explicitly approved a Sepolia-only safe-mode exception on 2026-09-12
after reviewing the custom-tracing requirement and RPC costs. No RPC purchase,
new funding allowance, Core change, or mainnet exception is authorized.

## Scope

- Chain 421614, AWS account 932542905614, ap-southeast-1, plether-sepolia-alto.
- Explicit Terraform opt-in: `alto_sepolia_safe_mode_exception=true`. Default false.
- `ALTO_SAFE_MODE=false`; `ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION=false`.
- Runtime metadata: `PLETHER_ALTO_VALIDATION_POLICY=sepolia-testnet-exception-v1`
  and `PLETHER_ALTO_NETWORK_CHAIN_ID=421614`. Metadata is enforced by deployment
  gates; the RPC chain ID is independently verified before promotion.
- Keep the existing pinned Alto image, normal simulation, contract signature
  checks, expiry checks, private ingress, KMS, account/action restrictions,
  authentication, rate limits, reservation/budget rules and safe reconciliation.

This drops Alto's additional trace-based validation. Simulation is not a
guarantee of inclusion: state changes or adversarial operations can invalidate
bundles, waste executor test ETH and cause denial of service. RPC/compute costs
are still real. This is accepted for Sepolia, not evidence of mainnet readiness.

## Deployment and qualification

1. Run configuration and negative-scope tests. Review a saved Terraform plan
   changing only the dormant Alto task template; preserve the active service
   revision until the protected deployment workflow promotes it.
2. Dispatch `deploy-alto.yml` from the reviewed master SHA, action `deploy`.
   Preserve exact-run approval and all existing image/topology/RPC checks.
3. Verify the active task uses the approved exception and normal validation.
   Check bundler health and current reconciliation/pause/funding state.
4. Qualify a fresh allowlisted sponsored deposit, then open/close when permitted
   by market/oracle conditions. Keep previous liabilities and exact journals;
   expired operations require a new explicit preparation, never silent renewal.
5. Public activation and hosted frontend cutover require successful native
   submission/inclusion qualification and the existing separate release checks.
   This exception does not itself enable issuance, public access or funding.

Rollback: set the exception false and redeploy the safe-mode template, or use
the protected workflow to restore the previous reviewed safe-mode revision.
With the current Alchemy RPC this restores the tracing blocker; stop new issuance
if needed while keeping reconciliation and existing operation recovery running.
Do not erase liabilities or use skipped validation as a workaround.
