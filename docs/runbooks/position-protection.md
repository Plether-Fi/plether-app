# Full-position TP/SL — canonical MockUSDG source

The current source uses SDK 0.4.0 and `config/perps/mock-usdg-prepared.json`.
Its null pins keep protection consumers unavailable until a verified new deployment
exists. See `config/perps/ORDER_V3_TIMING.md` and the MockUSDG integration runbook.
Historical Book addresses and SDKs are not compatible with current consumers.

## User behavior

- A flat-account opening order can atomically attach TP, SL, or both. The
  protection is `PendingOpen` until that exact parent executes, then `Armed`.
  Every protection intent includes explicit close financial limits; the current
  web ticket uses uint256-max bounds and does not claim finite cost protection.
- An existing position with no pending orders can create protection. Pending
  and armed records can be replaced or cancelled. Replacement retains its ID
  and reward reserve. Cancelling a pending protection does **not** cancel its
  parent opening order.
- The first eligible leg commits one full-position close through the normal
  FIFO. A trigger price is not a fill guarantee. Triggered and latched records
  cannot be edited or cancelled. Add-margin remains available; discretionary
  orders are blocked until protection terminates or is cancelled.
- UI prices use `CAP_PRICE - rawPrice`; contract prices are the raw inverse
  basket price. Percentages mean unlevered movement from the current displayed
  mark, not leveraged PnL. Zero disables a leg; both legs cannot be zero.
- Funding includes the parent order requirement plus the
  separate trigger bounty in settlement assets. Ordinary/protected closes have
  zero execution bounty. The keeper's native ETH value covers the oracle fee.
- `Latched` means a close failed but its original trigger remains binding.
  Expired attempts may be retried automatically; other terminal reasons need
  operator review. History preserves all attempts and transaction links.

### Execution-status UX

The frontend covers every Book state: Not set, Waiting for position, Active,
Close queued, Close delayed, Closed, Not completed, Removed, and Liquidated.
Terminal records remain in TP/SL activity; the latest terminal record opens
automatically when there is no current protection. Storybook has fixtures for
every state, both single-leg configurations, wallet pending/rejection, and all
latched retry blockers.

`GET /api/perps/protections/:id/execution?book=...` returns a no-store advisory
worker observation. It is scoped to chain, Book, protection, account, current
linked close order, and observed contract status. The UI polls every 5 seconds,
rejects mismatched attempts, and expires reports after 60 seconds (server age
plus elapsed client time). A missing, malformed, stale, or failed response is
explicitly unavailable; it must not imply that the worker is executing a close.

The worker publishes oracle-unavailable/frozen, pending-orders,
queue-congested/cleanup, operator-required, execution-disabled, preparation,
and failed-check reasons. Signed transaction references are joined only for
the same linked close attempt; signed bytes are never exposed by this API.
Reorg rollback removes observations above the retained canonical boundary.
These are timestamped last checks, not execution guarantees or a replacement
for the on-chain state. Retry remains keeper/operator-managed; the frontend
does not add a manual or sponsored retry action.

## Build provenance

The SDK package record and frontend lockfile pin the immutable 0.4.0 artifact.
Current frontend and worker ABIs are generated together from compiled canonical
Core sources, not a historical deployment bundle:

```bash
node scripts/generate-perps-consumer-abis.mjs --artifacts-dir /path/to/plether-core/packages/perps/out --source-root /path/to/plether-core/packages/perps
```

Use `--check` to verify reproducibility. Generated provenance identifies the
source and artifact hashes without claiming deployed addresses. Never edit
installed SDK files or generated ABIs by hand.

## Worker and history

`apps/backend/protection-worker/main.mjs` runs independently of the existing
order keeper. The order keeper still executes the linked closes. Both the API
and worker apply the same additive, idempotent schema extracted from
`apps/backend/schema.sql`. The breaking token-neutral SQL migration must be
reviewed and applied separately; it has not been run. Current startup rejects the
old perps schema before additive setup.

Required environment: `DATABASE_URL`, `PERPS_RPC_URL`; optionally
`PERPS_RPC_AUTH_TOKEN` for the authenticated RPC. Production PostgreSQL must
retain certificate-verified TLS. The release manifest defaults to the checked-in
file; the container uses `/app/config/perps/mock-usdg-prepared.json`.

The default is observe-only: `PROTECTION_WORKER_EXECUTION_ENABLED=false`.
Execution requires `PROTECTION_WORKER_PRIVATE_KEY`, a dedicated funded EOA.
Never share it with the order, liquidation, LP, or oracle workers. Terraform
rejects the zero key and reuse of another configured transaction signer.

The worker:

- checks RPC chain, pinned runtime hashes and Book/Router/Engine bindings;
- owns a session advisory lock and a dedicated PostgreSQL connection;
- indexes Book events in 2,000-block ranges, 12 confirmations behind latest,
  with block hashes, resumable checkpoints and rollback/replay on reorg;
- evaluates live state only after catching up, rotating through 50 candidates
  per pass (5 seconds by default); snapshots in history are end-of-block state;
- admits only complete, fresh `backend_hermes_latest_v2` Pyth payloads (15-second
  maximum age), and simulates the exact oracle update and Book transaction;
- retries only latched `Expired` outcomes, requiring no pending account order,
  an available oracle or supported frozen mode, and estimated FIFO drain within
  `maxOrderAge - 15 seconds`; configured keeper batch size/cadence must match
  the real order keeper;
- prunes a sole expired FIFO head in a separate simulated transaction and
  re-reads state before retrying;
- persists signed bytes and nonce **before** broadcast; ambiguous submissions
  and reorgs rebroadcast those exact bytes, never a newly built action;
- releases a submission lane after 12 canonical confirmations but continues
  reconciling retained included transactions through RPC `safe` finality.

Observe mode writes index/checkpoint data and execution-disabled observations,
but does not prepare Book actions, sign, or send. `--once`
runs one bounded pass and exits; it is not a read-only database preflight and
does not catch up an entire deployment in one invocation.

History endpoints (chain and Book scoped, newest first):

```text
GET /api/perps/accounts/:address/protections?book=:book&limit=25&cursor=:id
GET /api/perps/protections/:id/events?book=:book&limit=25&cursor=:block:logIndex
```

All integer monetary/ID fields are decimal strings. `nextCursor` is null at the
end. The account response exposes `indexedThroughBlock`; reads may lag the live
active protection. Both log identity and event payloads remain durable.

## Staged rollout

1. Merge/review the upstream AA client and this application change. Keep
   `protection_worker_desired_count=0`,
   `protection_worker_execution_enabled=false`,
   `aa_protection_commits_enabled=false`, and the frontend feature flag false.
2. Apply the Sepolia Terraform update and deploy the backend first. Use the
   named `plether` AWS profile and GitHub CLI deployment/authentication/commit
   checks from `AGENTS.md`; do not dispatch duplicate runs. Backend image
   deployment now includes `plether-position-protection-worker`.
3. Set desired count to 1 in observe mode. Require runtime binding verification,
   a caught-up `protection_worker_heartbeat`, working history endpoints, and a
   configured, tested `operations_alarm_sns_topic_arn`. Confirm the six-feed Pyth
   producer and normal order keeper are healthy. Initial backfill can take time.
4. Provision and fund the dedicated signer through the existing secret-management
   process, then enable worker execution. Verify its balance, simulations,
   transaction journal and exact receipt recovery before enabling new users.
   Maintain more than 0.001 ETH; lower balances emit an operator alert.
5. Confirm trading/bootstrap prerequisites for this release. The owner must
   propose the **complete existing RouterConfig**, changing only the intended
   protection fields, via `OrderRouterAdmin.proposeRouterConfig`. Respect the
   48-hour timelock and finalize through `finalizeRouterConfig`. Re-read the
   Router flag and live settlement-asset trigger bounty; never overwrite unrelated queue/risk
   settings with defaults.
6. Set `aa_protection_commits_enabled=true`, deploy the backend and verify the
   current protection-action sponsorship allowlist. Cancellation remains sponsored
   even when this flag is false; trigger/retry worker calls are never sponsored
   by the frontend proxy.
7. Set repository variable `VITE_PERPS_POSITION_PROTECTION_ENABLED_SEPOLIA=true`
   and manually deploy the frontend from `master`. The UI additionally requires
   the on-chain commits flag before showing create/replace inputs.
8. Run controlled testnet canaries before broad access: protected long/short
   opens; TP-only/SL-only/OCO; existing-position create; pending/armed replace;
   cancel before parent execution; exact trigger thresholds; liquidation;
   expired-close latch/retry; ambiguous broadcast/restart; frozen/paused risk-off
   behavior. Match account, Book, parent ID and each close ID to actual receipts.
   These live canaries have **not** been executed by the implementation task.

## Incidents and rollback

Disable frontend creation and backend sponsorship first to stop new intents.
Keep the worker and normal keeper running for existing protections and latched
closes. Do not delete journals, reset checkpoints casually, rotate the signer,
or submit a later nonce manually while a transaction is unresolved. Reverting
to an application that lacks the new Book is not an acceptable compatibility
fallback.

CloudWatch alarms cover error events, repeated degraded passes and a missing
caught-up heartbeat for three minutes. Inspect `protection_operator_required`
with its terminal reason, failure selector and revert-data hash;
`protection_retry_waiting` with queue/oracle constraints;
`protection_transaction_stalled` after 120 seconds; and signer low balance.
An underpriced/dropped transaction is deliberately **not** replaced with a new
nonce or higher-fee variant automatically. Diagnose nonce, receipt and fee state
and approve a same-nonce recovery separately, preserving original signed bytes.

## Verification

```bash
cd apps/frontend
npm test
npm run lint
npm run build
```

```bash
cd apps/backend
cabal test plether-api-test --test-show-details=direct
```

```bash
cd apps/backend/protection-worker
npm ci
PROTECTION_TEST_DATABASE_URL=postgres://user:password@127.0.0.1:5432/test npm test
```

The last suite creates/removes only its own uniquely named schema. Without
`PROTECTION_TEST_DATABASE_URL`, PostgreSQL integration tests are explicitly
skipped. CI runs them against a disposable PostgreSQL 16 service.
