# Frankfurt preparation rollout — 2026-09-11

User approved deployment for interactive testing. Scope: account 932542905614,
eu-central-1, cluster plether-sepolia-aa-temp, API service only, and the existing
localhost frontend. No Singapore, Core, funding, DNS or hosted frontend changes.

## Immutable source and additive migration

Source commit: `48e074619f04344c9943f63a52ca468bf040d168`.
The Linux ARM64 API image is built from a clean git archive, without local
environment files. Existing reconciler, Alto and trading worker images stay put.

Migration task `c0bff195280047beb5d6539211ded8e3` used existing API revision 8.
It verified the exact Frankfurt database hostname, database name and TLS mode,
applied only the new `aa_preparations` table, and exited zero. CloudWatch recorded
`aa_preparation_migration_verified`, with SELECT/INSERT/UPDATE access and seven
constraints verified. Migration content SHA-256 (without the final newline):
`dd0d117372f560aac49140bca343bdbec2ad6fd96427f1d06f4914be5fde4f72`.
The additive table and existing sponsorship ledger must survive rollback.

## Exploratory comparison, not performance qualification

Before API replacement, three deposit preparation calls through the localhost
proxy completed in 2175.75, 1983.34 and 1987.25 ms. Each made four AA requests.
Stub requests took 919.43, 820.62 and 760.15 ms; final sponsorship took 965.75,
934.17 and 978.45 ms. The account-discovery adapter permits read-only wallet
methods and rejects all wallet signing and submission. These checks prepared a
1 mock-USDC deposit but never submitted it; reservations recover normally.

These repeated preparations are smoke/comparison samples, not the three
100-new-preparation mixed-action performance runs. No p50/p95 release claim is
made. An initial harness failure occurred during read-only wallet discovery,
before any AA preparation request; allowing account discovery fixed the harness.

## Activation evidence

Published API digest:
`sha256:ebbc8bd563bd470989d4ec489963b7b70e6b1e33a76ac18c6bce8ae46ff52775`.
Image label/config digest were verified against the published ECR manifest;
read-only offline `ldd` found all API dependencies. Registered API revision 9
and promoted it from revision 8 with preparation disabled. Registration readback
was compared after sorting environment variables (AWS changes their order).
Only the API image and preparation flag differ; all other configuration is kept.

Instrumented standard-flow deposit checks returned 1015.89, 911.24 and 814.32 ms,
all successful, each with four AA HTTP requests and request IDs/Server-Timing.
These use the new shared verification improvements but not the single-request
method. Discovery/fee requests passed for both localhost origins; an untrusted
origin remained rejected with HTTP 403.

Enabled revision 10 was registered from revision 9, changing only
`AA_NATIVE_PREPARATION_ENABLED` from false to true. It uses the same image.
The single-wallet allowlist, global-rollout=false, KMS key, RPC mode, budgets,
safe-head allowance and all Core addresses are unchanged. ECS service promotion
is explicit (the repository intentionally ignores task_definition on services).
The Frankfurt-only Terraform image pin and trading overlay reflect this desired
API configuration; no broad Terraform apply or other service promotion ran.

After API verification, the local launcher must inherit
`AA_FRANKFURT_PREPARATION_ENABLED=true` to advertise `preparationRpcVersion: 1`.
The public manifest remains untouched. Removing that environment setting and
restarting localhost restores the standard flow; disable the API preparation
flag as well to stop new native preparation attempts.

Revision 10 reached stable status, with Alto and reconciler still at revision 4.
Its first fast-path check failed in 462.52 ms before estimation/reservation:
the pinned Alto rejects the internal string request ID. Read-only fee probes
confirmed failure with `"preparation"` and success with numeric `1`. The localhost
capability was not activated, so user requests remain on the working standard
flow. Fix commit `8e68565` changes internal IDs to numeric and removes repeated
timing-header emission; the backend regression suite passed before rebuilding.
This changes the internal JSON-RPC transport ID only, not the durable preparation
ID or its retry semantics. The final local backend suite has 1,056 passing tests.

Corrected source `8e685657e225c511b5218eae674ccc2ba7a04f0c` was built and
verified at digest
`sha256:8f43121f73dae10928531e6b2a3cb2b8a7d63456008055019b73d345beafdb6e`.
API revision 11 promotes this image from revision 10 without changing any
environment variable, role, secret reference or service count. The final
Frankfurt-only Terraform image pin matches it.

Revision 11 reached ECS `COMPLETED`, with one running API task. Its first fresh
preparation completed in 582.56 ms (HTTP 538.27 ms, server 458 ms); account
evidence was cold (137 ms). An identical retry completed in 207.22 ms, returning
the exact same operation, with no estimation, reservation or signing stage.
Both passed the real frontend operation/envelope/hash validator and emitted
exactly one AA request and one opaque request-ID header each.

The localhost launcher was restarted with preparation enabled. Readback verified
chain 421614, sponsorship=true, preparationRpcVersion=1, and the unchanged
Frankfurt paymaster. Discovery/fees succeeded from both allowed localhost
origins, and a foreign origin remained denied (403).

A second runtime prepared three distinct deposits (1.000002, 1.000003 and
1.000004 mock USDC), each with a new preparation ID:

| Sample | Complete preparation | AA HTTP | Backend |
| --- | ---: | ---: | ---: |
| Fresh client, warm backend account evidence | 598.56 ms | 358.40 ms | 283 ms |
| Reused client, new intent | 294.24 ms | 292.51 ms | 225 ms |
| Reused client, new intent | 365.58 ms | 364.80 ms | 276 ms |

The first includes client-side account discovery and is not reduced to HTTP
latency. All checks stop before wallet signing: no deposit/trade was submitted,
no funds were transferred, and normal reservation reconciliation remains in
charge. These few successful samples do not satisfy the three-run 300-sample
mixed-action performance gate, which remains pending, as does a separately
recorded sponsored open/close test on this new preparation path.

Interactive testing is enabled only for the existing allowlisted owner. Refresh
the existing localhost tab to load the new manifest; retain its operation
journal and do not clear browser storage. No Singapore or Core change was made.
