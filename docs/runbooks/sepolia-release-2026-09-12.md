# Singapore Sepolia deployment record — 2026-09-12

Target: account `932542905614`, region `ap-southeast-1`, existing
`plether-sepolia` database and cluster. Application commit:
`cd417902fa69259d8adc1ef1945ac889615cbb2f` (Core v1.2.3 bindings).

## Completed preparation

- Retained encrypted pre-release snapshot `plether-sepolia-pre-v123-20260912`.
- Reviewed and applied staged AA infrastructure with native configuration,
  issuance, preparation and submission disabled; Alto/reconciler desired counts
  remain zero. No Core transactions, signer changes or funding transfers.
- Applied the native AA base schema and the preparation, observability v1/v2
  and funding migrations atomically to the existing database.
- Rehearsed the migrations twice on a private snapshot clone. Existing history
  and a synthetic signed liability/reservation survived the second application.
  The synthetic fixture was created only in the clone.
- Live migration verified unchanged history: 36,481 keeper records, 36,561 order
  records and 11 indexer records. The new live authorization table is empty and
  issuance is paused. This does **not** waive reconciliation of retained prior
  authorizations for the existing paymaster before native activation.
- Deleted the temporary rehearsal clone after successful tests; retained the
  source snapshot and operational logs. Deregistered the three completed
  one-off preflight/rehearsal/migration task definitions.

## Backend release

[Run 34683855800](https://github.com/Plether-Fi/plether-app/actions/runs/34683855800)
uses `environment=sepolia`, `bootstrap=false`, `deployment_scope=all`.
Stanley explicitly approved this exact run in the deployment task.

The first attempt stopped before building: the deployment role lacked
`ssm:DescribeParameters`. The Terraform repair adds region-bound metadata
listing, `kms:DescribeKey` for the exact RPC encryption keys, and read-only
inspection of the exact API/reconciler execution-role policies. No decrypt,
signing, secret-value or mutation grants were added. The reviewed repair plan
also restored standard Terraform tags on one existing alarm; it replaced no
resources. Both live preflights passed on retry. Regression verification:
14 mocked Terraform cases and 69 Node release/approval tests passed (including
the frontend health-response regression added below).

The backend workflow completed successfully, including all seven deployment
jobs. The public API reports verified v1.2.3 bindings, manifest version
`perps-aa-arbitrum-sepolia-20260910-v2`, and deployment block `307397196`.
Its health endpoint and load-balancer target pass checks. Expected active
services reached steady state; standalone consolidated-worker duplicates and
the native reconciler remain at their configured zero desired counts.

Frontend [run 34685097355](https://github.com/Plether-Fi/plether-app/actions/runs/34685097355)
was dispatched from the same application commit after backend success.
The frontend run failed **before publication** at its origin health parser:
the existing API returns a JSON-encoded string containing the health object.
The workflow now accepts one legacy JSON wrapper or the proper object, while
still requiring HTTP 200 and `status=ok`. Malformed, unhealthy and multiply
encoded bodies are covered by regression tests and remain rejected. The fix
must land on master before a new frontend dispatch. Local authenticated
malformed-request probes return the intended `400 / INVALID_REQUEST`; the
unauthenticated probe returns `403 / PROXY_AUTH_FAILED`.

Post-deployment logs also show repeated LP settlement simulation failures with
`HousePool__NoLpEpochProgress()` (`0x86cca6b8`). The worker refuses to send that
transaction. This is an unresolved operational issue, not a funding diagnosis
or proof of successful LP settlement; do not silence or bypass the invariant.

Frontend publication and final smoke tests remain pending. Do not treat this
record as proof of native-AA qualification. Native activation still requires dedicated Alto funding,
signer attestation/owner approval, retained-liability reconciliation and the
release gates in `singapore-sepolia-aa-release.md`.
