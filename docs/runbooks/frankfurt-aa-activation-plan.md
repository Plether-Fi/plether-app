# Frankfurt AA: image preparation and controlled activation

## Authorization boundary

Update 2026-09-11, later: the user authorized continuing through a test transaction
and approving testnet wallet transactions, capped at 0.1 test ETH. The paymaster
is now deployed, funded and staked, still paused. See
[paymaster evidence](frankfurt-paymaster-evidence.md) for receipts and current
qualification status. The phase labels below preserve the original staged plan;
they do not supersede that later explicit authorization. AWS operations in this
session use the named operator profile, not an existing Singapore workflow.

Update 2026-09-11: the user requested steps 1–4. API-only activation and localhost
testing have now been applied; see [activation evidence](frankfurt-api-activation-evidence.md).
Paymaster deployment and funded sponsorship remain pending the operator's funding
source/cap and the remaining qualification work. The earlier scope below records
the image-preparation boundary, not the current API service count.

The isolated dormant stack is applied. The current authorization covers preparing
images and this plan, **not** starting ECS tasks, signing with KMS, funding wallets,
deploying a paymaster, unpausing, or enabling sponsorship. No Core deployment,
frontend deployment, custom DNS, Cloudflare or Singapore mutation is part of any
phase below. Later phases need separate approval and exact reviewed changes.

Target: AWS account `932542905614`, `eu-central-1`, cluster
`plether-sepolia-aa-temp`, chain `421614`, existing Core **v1.2.3**. The existing
state bucket/key and SSM relay are described in the preparation evidence.

## 1. Prepare images, keep all services stopped — current scope

Use `node scripts/aa-frankfurt-images.mjs --publish` with a working local Docker
Desktop engine and the authenticated `plether` profile. It archives committed
source `fd472f471b4731d8bad316d87c6586ef9bc27ab9`, tree
`398d5688cee10fc3f2374eb265cf6e8a08570fce`, rather than building the dirty worktree.
GitHub's current master `2967aa5ea348c5314ae6adbce5abf4c5bb49c43a` had identical
file contents when checked; its Backend and Tests runs passed. The old published
Singapore backend predates this AA implementation and is not a suitable substitute.

The API uses the separately hash-pinned `scripts/frankfurt-backend.Dockerfile`,
SHA-256 `5534710ccd61653e6d21b48f56905ad1f2c97abea36beeaba834e9527d69ab9b`.
Its only build-path changes are a Node CA-bundle stage and HTTPS APT transport in
the two Debian stages. Repeated HTTP package downloads failed checksums; direct
HTTPS downloads of the same package matched the signed-index checksum on both
host and container. Package signatures/checksums are not bypassed. Application
inputs remain the committed archive; the API image also labels this recipe hash.
The shared backend Dockerfile and existing deployment workflows are unchanged.

The helper builds linux/arm64 API and log-router images, checks the API image's
embedded v1.2.3 release and required executables offline with no network, and
mirrors the approved Alto v1.2.7 ARM64 digest. It publishes only:

- `plether-api-sepolia-aa-temp:source-fd472f471b4731d8bad316d87c6586ef9bc27ab9`
- `plether-otel-log-router-sepolia-aa-temp:source-fd472f471b4731d8bad316d87c6586ef9bc27ab9`
- `plether-alto-sepolia-aa-temp:v1.2.7-arm64-28cee87ea6b5`

No `latest` tag is written, no existing tag is overwritten, and partial retries
inspect existing images. An isolated temporary Docker auth directory is removed
after use. The result is a private `images.json` evidence file with registry
digests; validate it with `validateImageRecord` before preparing activation.
Source pinning is not a claim of bit-reproducible builds: the existing Dockerfiles
still resolve some base images and package indexes dynamically. Preserve these
exact tested output digests rather than rebuilding for deployment.
For Alto, Docker/ECR may convert OCI descriptor media types and therefore change
the manifest digest. The helper hashes the raw pinned GHCR manifest and compares
the ECR configuration and every ordered layer digest/size, using the same content
identity rule as the existing Alto deployment workflow. Pin the resulting ECR
digest for activation; do not claim its manifest digest equals the upstream one.

Exit gates: all three digests published/read back, linux/arm64 verified, offline
API smoke passed, ECR scan status/findings reviewed, and all 10 ECS services still
at desired/running/pending zero. Scan completion alone is not a security approval.

## 2. Prepare and approve an API-only infrastructure change

Implementation required before activation:

1. Add explicit immutable image-reference inputs for the API, init containers,
   log router, reconciler, admin and Alto task definitions. Validate exact
   Frankfurt repository prefixes plus `@sha256:` digests from the image record.
   Do not solve missing `latest` references by publishing mutable aliases.
2. Extend the hard dormant guard with a narrowly defined `api-readonly` stage.
   Permit **API=1 only**, with every other service count zero; keep native AA
   configuration, sponsorship, submission, global rollout, protection execution,
   protection commits, LP settlement and legacy Pimlico proxy off. Keep faucet
   credentials and registration mutation features absent/disabled.
3. Preserve all existing account/region/Core, private-ALB, secret-namespace and
   localhost-CORS guards. Existing ECS services ignore task-definition changes:
   first apply digest-pinned definitions with desired count zero, then promote
   only the verified API revision using `update-service --task-definition` while
   still stopped. Do not change desired counts through this promotion command.
   Generate/review the separate Terraform API-only count change afterwards.
4. Add negative mocked tests: no worker/Alto/reconciler start in this stage; no
   mutable/foreign image references; no new public ingress; no Singapore changes.
5. Produce a fresh isolated-state plan. It may register image-pinned definitions
   and raise only the API desired count. Reject networking/IAM expansion,
   database replacement, changes to secrets/keys, and on-chain activation.
6. Present the exact plan and image digests for operator approval before apply.

**API-only is not side-effect-free:** startup verifies the release through RPC,
initializes database schemas, starts embedded spot/vault read indexers, and sends
logs through the log router. Keep `PYTH_INGESTION_ENABLED=false`. Agree on bounded
Alchemy quota and PostHog sharing before starting: these credentials are shared
with the existing backend, so regional separation alone cannot guarantee zero
quota/analytics impact. Audit the initial database migration path and tables
before applying; do not restore Singapore's database into Frankfurt.

After approval: start the SSM tunnel, require `/api/health` HTTP 200, verify API
logs and Core runtime hashes, database readiness and private routing. Native AA
issuance/submission must remain disabled; unauthorized wallets must not be
sponsored. Stop immediately on wrong chain/bindings, migration failure, repeated
RPC throttling or unexpected tasks. Roll back via a reviewed plan to API=0,
preserving the database and logs. There are no live sponsored operations to drain
at this stage. A successful 503 tunnel check from the dormant stage is not enough.

## 3. Qualify AA without issuing sponsorship

Separate reviewed authorization and implementation are required for:

- A Frankfurt-specific protected workflow/environment. Existing backend, Alto
  and AA-admin workflows are Singapore-only: **do not dispatch them for this
  stack**. Require master-only provenance, operator review, no administrator
  bypass, exact Frankfurt account/resource checks, explicit image digests and
  duplicate-run protection; use `gh` for dispatch and monitoring.
- Real operator alarm routing and the inventory of existing on-chain writers.
  Keep keeper, oracle, liquidation, protection and consolidated workers off.
- Deriving and attesting the new KMS signer through the reviewed fixed-digest
  admin path; binding it to a separate paymaster with owner matching the verified
  Core owner. Owner/test EOA: `0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B`.
  Recheck current on-chain ownership; do not infer it solely from this document.
- Separately approved paymaster deployment/funding and any missing Alto simulation
  contracts. Never deploy Core. Resolve exact existing contract code before any
  transaction, with bounded allocations, destinations and maximum gas cost.
- Alto startup/refill behavior and its four executors plus utility wallet. Funding
  the utility wallet can trigger executor refills; it is not a passive balance.
- Reconciler startup with issuance off, continuous safe-block coverage, database
  accounting, native profile attestation, restart/recovery and failure tests.
  Use the approved explicit Alchemy-only mode, not a pretend independent fallback.

Follow the existing self-hosted AA qualification runbook, adapting every target
binding to Frankfurt rather than copying Singapore commands. Do not enable
submission until expired/replayed/unauthorized requests, RPC failure, signer
mismatch, budget reservation, receipt handling and pause/recovery tests pass.

## 4. One-wallet sponsored canary — later approval

Resolve both the owner EOA and its expected smart-account address. The owner
allowlist applies to `0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B`; the smart account
is the UserOperation sender. Keep global rollout disabled and all write workers
off. First enable submission under the reviewed profile, then issuance only after
all backend gates and operator readiness checks pass. Enable localhost manifest
sponsorship last, using the deployed paymaster address and server-only origin token.

The currently approved policy ceilings are **0.01 ETH/operation**, **0.02 ETH
outstanding**, **0.05 ETH/hour**, **0.1 ETH/day**, 20 final requests/minute. These
are maximum-cost reservation caps, not wallet-funding instructions or guaranteed
transaction counts. Current settings also require a 0.05 ETH minimum paymaster
deposit; agree on its relationship to the actual funding allocation before use.
Keep deposit/stake/refill allocations separate and bounded; never automatically
fund to a policy limit. Require a staffed first transaction and reconcile actual
cost, receipt, deposit delta and reservation release before further testing.

Rollback after issuance is different from API-only rollback: disable new issuance
first; preserve the reconciler and required read/submission path while outstanding
authorizations drain. Use the existing pause/drain runbook and on-chain owner
pause when required. Do not stop everything or delete the ledger while signatures
can still be used. Preserve audit evidence and account for paymaster stake delays.

## Current status

All three candidate images are published and verified. API-only activation was
applied on 2026-09-11 following the user's instruction to proceed; nine other
services remain stopped. The guard now permits only dormant or API-only mode,
never sponsorship or writer activation. ECR findings remain unresolved; no
remediation or claim of exploitability validation is implied. See
[image evidence](frankfurt-aa-image-evidence.md) and
[current activation evidence](frankfurt-api-activation-evidence.md).
