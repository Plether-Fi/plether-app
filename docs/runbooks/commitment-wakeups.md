# Commitment preparation and keeper wakeups

Sepolia native AA submissions can start read-only commitment preparation before
canonical keeper indexing discovers the order. A verified receipt can warm the
exact historical Pyth payload, and notifications interrupt idle keeper and price
prefetch waits. The existing confirmed log scanner, FIFO queue, deadline checks,
price admission, gas/fee limits, singleton signer and broadcast journal continue
to authorize every execution.

## Modes

`PERPS_COMMITMENT_WAKEUPS_MODE` defaults to `off`. Only the configured Arbitrum
Sepolia deployment can enable it.

| Mode | Behaviour |
| --- | --- |
| `off` | Existing polling and execution path. No hint watcher. |
| `observe` | Persist and verify advisory hints, record stage timings; retain ordinary keeper and price scheduling. |
| `active` | Observe plus keeper and prefetch wakeups and provisional price preparation. |

Set the same mode on the API, keeper and basket worker, including their inactive
standalone task families. The guarded operator below prepares all four families
without starting dormant services.

## Submission and recovery

The AA gateway enqueues the locally expected user-operation hash after the
existing submission journal succeeds and again after a matching Alto response.
Enqueue is bounded and nonblocking. A failed optimization cannot change the
submission response or replay the signed operation. Missing acknowledgements
remain under observation through the existing durable journal.

An asynchronous watcher repairs missing hints from submitted authorizations,
using an anti-join rather than a submission-time high-water cursor. It verifies
the exact successful EntryPoint operation segment, paymaster, sender, original
client order ID, router/lifecycle events, receipt identity, pinned pending state
and canonical headers. Arbitrum commitment block numbers are L2 block numbers;
the header's `l1BlockNumber` is a different value.

Hints are separate from `perps_keeper_orders` and never advance its index cursor.
They expire independently of sponsorship and transaction state. Provisional price
candidates require recent canonical verification. Terminal canonical orders
retire their hints. Admission is capped at 128 active hints per deployment and
five per account; database leases fence stale observers.

The prefetcher reserves at most one of its two slots for provisional work, keeps
canonical work first and retains provider cooldowns and shared-tick deduplication.
A cached exact payload wakes the keeper only after normal payload admission.

PostgreSQL notifications contain only the deployment router. Dedicated listener
sessions reconnect and trigger a rescan after `LISTEN` commits. Notification loss,
provider errors, browser closure and listener failure retain ordinary polling.

## Trade screen execution progress

The keeper's pre-send journal also records the orders targeted by its signed
transaction. The existing order wait endpoint can return that transaction hash
with `submitting` or `submitted` progress before terminal history is indexed.
Only an exact RPC acknowledgement earns `submitted`. Planned batch membership
does not prove that an order executed, because a batch may stop early.

Progress is scoped to the canonical commitment's router, account, client order
ID and commitment transaction. An `afterExecution` cursor lets the browser wait
for a change without repeatedly receiving the same hash. Lightweight progress
reads run every 250 milliseconds; full history reads retain a one-second cadence.
The endpoint never returns signed transaction bytes or promotes keeper database
status into canonical order history.

The browser checks the hinted transaction receipt, the authenticated lifecycle
receipt, the order identity and canonical block headers before displaying a
terminal result. The required depth is at least one block and uses the stricter
of the API's keeper and `PERPS_INDEXER_CONFIRMATIONS` settings. Keep the latter
equal to the indexer's configured confirmation depth when preparing task
definitions; Terraform mirrors the existing indexer variable into the API.
The authenticated lifecycle receipt's contract block clock and the RPC receipt's
Arbitrum L2 block number remain distinct.

Verified execution and the receipt's fees, VPI and settlement amounts appear
immediately while account balances refresh separately. Supplemental oracle and
lifetime attribution continue to come from indexed evidence. Indexed history
enriches the result and handles canonical rewinds. The existing safe-block lookup
remains available when execution progress is unavailable. These browser updates
do not require active commitment wakeups and do not alter execution authorization
or confirmation rules.

## Staged Sepolia deployment

Follow [AGENTS.md](../../AGENTS.md) for AWS identity, GitHub CLI authentication,
exact master commit verification, duplicate-run checks and protected deployments.
Use the named `plether` AWS profile and account `932542905614` in
`ap-southeast-1`. Merge the tested implementation before preparing live revisions.
Do not apply the application Terraform root with synthetic secrets.

Use a private operator directory for each stage:

```bash
python infra/rpc/ops/prepare_commitment_wakeups.py prepare \
  --mode observe --directory /private/tmp/commitment-wakeups-observe

gh workflow run deploy-backend.yml --repo Plether-Fi/plether-app --ref master \
  -f environment=sepolia -f bootstrap=false -f deployment_scope=all \
  -f close_assistance_mode=preserve

python infra/rpc/ops/prepare_commitment_wakeups.py verify \
  --directory /private/tmp/commitment-wakeups-observe
```

Capture and verify the workflow's commit and monitor it to success before
verification. Preparation only registers task definitions. Verification requires
stable service counts, the actual running mode, unchanged non-mode configuration
and matching dormant/consolidated keeper configuration. Only normal application
image and log-router release-version changes are excluded from its policy hash.

After observation passes, repeat with `--mode active` and a new private directory.
The second deployment of the same source is intentional: it changes the mode.
The Sepolia own-RPC Terraform overlay records the desired active mode; defaults
for other deployments remain off.

For rollback, repeat preparation and deployment with `--mode off` in a third
private directory. Ordinary one-second keeper reconciliation remains available
throughout. Disabling the feature does not erase evidence or alter AA recovery.

## Qualification

Require healthy API/worker readiness, stable services, passing operator policy
verification and no repeated hint or listener errors. Check these structured
backend events:

- `commitment_hint_watcher_started`: configured mode and work limits.
- `commitment_hint_summary`: per-process cumulative queue, drop, coalescing,
  recovery and verification counters.
- `commitment_hint_inclusion_verified`: advisory stage and submission-to-observation
  duration. Multiple stages can be observed for the same order.
- `commitment_wakeup_listener_connected`, `commitment_wakeup_received` and
  `keeper_commitment_wakeup`: listener and idle-wait activity.
- `reveal_payload_fetch_completed`: validated exact price availability.
- `keeper_transaction_submitted`: exact broadcast acknowledgement, target count
  and signing-to-acknowledgement and RPC-send durations.

Compare submission, canonical discovery, exact price availability, execution
broadcast and terminal receipt timings. Keep chain inclusion, confirmation,
queue-head blocking and oracle publication time distinct from scheduling delay.
Without representative trades, deployment health proves readiness but does not
establish an execution-latency percentile.

Tests cover submission response isolation and forwarding once; exact bundled
receipt binding; duplicate/missing hints; database leases, expiry, restart and
notification recovery; reorg evidence invalidation; and prefetch priority,
coalescing and provider cooldowns. No synthetic trade is required on the live
chain to run these tests.
