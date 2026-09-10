# Temporary AA test stack outside Singapore

## Operator intent

The user wants to deploy the full stack later into a different AWS region to
test native AA without impacting the live Singapore testnet. No deployment,
Terraform apply, DNS change, or change to the current Singapore environment is
authorized by this preparation record. Target region is pending selection.

The current single-provider implementation is a separate source change. Its
`sepolia-aa-canary-preparation.tfvars` file still describes Singapore and is
**not** the temporary stack's deployment profile. Do not apply that file to
satisfy this request. All deployment and funding switches remain off.

## Isolation requirements before a deployment plan

- Introduce a distinct deployment identity (for example `sepolia-aa-temp`)
  separate from the blockchain network (`arbitrum-sepolia`, chain 421614).
  Resolve the exact AWS account/region/identity as a reviewed target tuple.
- Use a separate Terraform state key and lock, with an explicitly selected
  backend. Never reuse `plether/sepolia/terraform.tfstate`, migrate the existing
  state, or import Singapore resources into the temporary state.
- Create a new VPC, database, ECS cluster/services, load balancers, log groups,
  signer KMS key, secret namespace, alarms, and regional image repositories.
  Account-global IAM role/policy names must also include the deployment identity;
  a different region alone does not isolate IAM.
- Provision separate database credentials, edge-to-origin credentials, and
  service wallets. The Alto keys already created under Singapore's
  `/plether/sepolia/` namespace are not automatically portable or reusable.
  No key may be used by two running services/stacks concurrently.
- The user approved Alchemy as the current RPC provider. Sharing its exact key
  also shares quota/rate-limit and credential-rotation impact with Singapore.
  For strict non-impact, use a separate Alchemy application/key or approve and
  measure a bounded shared quota; do not call regional separation sufficient.
- Use separate GitHub protected environments, OIDC subjects and deployment
  target mapping. Preserve exact account/region/resource checks. Replace the
  hardcoded Singapore assumptions with reviewed per-target bindings, not broad
  wildcards or disabled safety checks.
- Use a separate frontend preview project/domain and Worker configuration.
  Do not overwrite `plether-testnet`, its domains, manifests or secrets. Pin
  the temporary frontend to its own backend, origin credential and manifest.
- Isolate scheduled jobs, keeper identities, notification routing, analytics
  and other outbound integrations. Keep write-capable workers off until their
  on-chain scope is explicitly resolved.
- Review the saved plan for **only** the temporary target's creations/changes;
  any Singapore resource update, deletion, DNS change or IAM alteration outside
  the temporary namespace is a blocker. Record tested rollback and teardown
  targets before applying. Do not use a broad teardown command.

## On-chain isolation is a separate decision

AWS regions do not isolate Arbitrum Sepolia state. Pointing the temporary stack
at the current Core deployment means trades, collateral movements, keepers,
oracle updates, faucets and settlement activity can still affect the running
testnet. A separate paymaster isolates its sponsorship deposit, not Core state.

Before a spend-authorizing deployment, explicitly choose:

1. A fresh Core deployment and isolated protocol addresses/wallets for an
   independent end-to-end environment. This requires updating and qualifying
   the pinned deployment bindings in both app and AA policy; do not bypass them.
2. Shared Core contracts with explicitly accepted on-chain impact and a narrow
   test cohort, while disabling duplicate/conflicting write workers. This is
   infrastructure isolation only and does not meet a strict no-impact promise.

A local fork can support isolated preliminary tests but is not evidence that
the public Arbitrum Sepolia deployment path has been qualified.

## Proposed sequence

1. Merge the explicitly gated single-provider-mode PR after CI passes.
2. Confirm region, deployment identity, on-chain isolation and RPC-quota choice.
3. Implement target-aware Terraform/workflow/frontend bindings in a separate PR,
   with negative tests proving Singapore targets cannot be selected accidentally.
4. Prepare an isolated dormant-stack plan and explicit funding allocations.
5. Review before apply; deploy paused/unfunded contracts and stopped services as
   appropriate, then complete the existing AA qualification gates for this exact
   deployment. No automatic public-manifest switch or mainnet rollout.
6. After testing, tear down only the reviewed temporary resources, preserving
   required audit evidence and accounting for on-chain funds/stake delays.
