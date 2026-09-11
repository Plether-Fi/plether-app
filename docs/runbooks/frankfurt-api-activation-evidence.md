# Frankfurt API-only activation — 2026-09-11

User request: perform steps 1–4, beginning with API-only activation and localhost
access. AWS `plether` authentication was renewed through its configured
`plether-company` credential-process bridge; account 932542905614 verified.

## Applied changes

1. Applied the image-only plan: 12 task-definition replacements, no service-count,
   network, IAM, database or secret changes. Old revisions were deregistered,
   not running tasks or user data deleted; definitions remain in configuration
   and can be registered again if required.
   Plan hash: `669e6120fe7c948298b121e4c85435df72d173ed26cd948c88741c9bc2f2a7c9`.
   Private workspace: `/var/folders/xz/scmmjjld2pq8k763z54mzjtr0000gn/T/plether-frankfurt-plan-2l8PDl`.
2. Verified and promoted `plether-sepolia-aa-temp:2` to the stopped API service.
   No desired-count change in this promotion; legacy Terraform intentionally
   ignores task-definition promotions, and that behavior was preserved.
3. Applied the separate API-only plan: API desired count 0 → 1 and matching
   `terraform_data.perps_candle_rollout_guard` metadata only.
   Plan hash: `cb87c4932788d92c9cdb42e80e955d0cfa1316659bf96e9971938c5a58f68ca5`.
   Private workspace: `/var/folders/xz/scmmjjld2pq8k763z54mzjtr0000gn/T/plether-frankfurt-plan-59RIRt`.

Both binaries contain secrets: do not publish them or replay applied plans.
`review-frankfurt-api-plan.mjs` checks the incremental allowlist in memory without
printing private plan values. No GitHub workflow was dispatched.

Post-apply Terraform convergence returned exit 0: no changes. Local verification:
18 mocked Terraform plans, 19 focused Node tests, the real Vite proxy test,
Terraform formatting, workflow YAML parsing and `git diff --check` passed.

## Runtime checks

- API task: `255406c88e864461bd9d5edeae3ad3a9`, definition revision 2.
- API desired/running 1, pending 0; all nine other services desired/running/pending 0.
- Backend/log-router use the exact digests in the image evidence.
- `/api/health` through the SSM tunnel and `/api/perps/v1/health` through Vite
  returned HTTP 200 at 05:25 UTC. Local frontend `/` returned HTTP 200.
- Startup emitted `perps_v2_release_verified` at block 307661045, with the v1.2.3
  router and lifecycle book, followed by `api_database_ready`.
- Core engine `owner()` rechecked as `0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B`.
  No ownership change or Core deployment.
- Pyth history ingestion explicitly false. API embedded spot/vault read indexers
  and logging run; this consumes shared Alchemy quota and PostHog ingestion.
- Local manifest uses v1.2.3 bindings and `sponsorshipEnabled=false`; local AA
  requests return 403. No origin token/paymaster address needed in API-only mode.
  Its legacy-shaped AA URL is local and blocked, not a Pimlico fallback.

Startup revealed the legacy spot tables are not created by the API's newer
schema initialization. The fixed-target `bootstrap-frankfurt-spot-schema.mjs`
initializes only `transactions`, `indexer_state`, `price_snapshots` and
`staking_snapshots` from the hash-verified image's schema prefix. It uses one
transaction, verified database TLS and credentials in process environment, never
command arguments. Successful bootstrap task
`eb9eb20b87dd47758c33e88f0fd80aff` stopped with both containers at exit 0 and
emitted `frankfurt_spot_schema_bootstrapped` with `tables=4`. Earlier attempts
failed; the successful helper uses explicit libpq environment fields rather
than placing a connection URI in `PGDATABASE`, plus `psql --file=-` for the
single-transaction stdin input. No credentials were passed in command arguments.
The API subsequently emitted `ethereum_indexer_progress`; final readback showed
API 1/1, all nine other services stopped, no running bootstrap tasks, and localhost
API health HTTP 200. History backfill completeness is not claimed.

## Local access

The live tunnel and Vite processes were left running for testing. Open
`http://127.0.0.1:5173`. They stop when their sessions are closed; to restart from
the repository root, use two terminals:

```bash
PATH=/Users/stan/.local/bin:$PATH node scripts/aa-frankfurt-tunnel.mjs \
  /var/folders/xz/scmmjjld2pq8k763z54mzjtr0000gn/T/plether-frankfurt-plan-RGtpjn/frankfurt-outputs.json
```

```bash
AA_FRANKFURT_MODE=api-readonly \
AA_FRANKFURT_OUTPUTS_FILE=/var/folders/xz/scmmjjld2pq8k763z54mzjtr0000gn/T/plether-frankfurt-plan-RGtpjn/frankfurt-outputs.json \
node apps/frontend/node_modules/vite/bin/vite.js --config scripts/aa-frankfurt.vite.config.mts
```

Keep applying `frankfurt-aa-preparation.tfvars` **plus**
`frankfurt-api-readonly.tfvars` for the active stage. The preparation profile alone
deliberately plans API back to zero. The historical `preparation_only` output is
an identity marker, not a live service-health indicator; inspect ECS counts.

## Remaining steps

Steps 3–4 are not complete: no paymaster deployed or funded, no KMS signing,
no Alto/reconciler activation and no sponsorship. Need the funding wallet and
explicit allocation/cap, signer/admin qualification, paymaster deployment and
readback, simulations and one-wallet tests. Existing ECR findings remain
unresolved; the user chose to proceed with this isolated API test.

No frontend hosting, DNS/Cloudflare, Singapore resource changes or blockchain
transactions were performed. API infrastructure charges now include one Fargate
task in addition to the previously provisioned dormant resources.
