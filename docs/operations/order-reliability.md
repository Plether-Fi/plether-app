# Order reliability monitoring

[Sepolia dashboard](https://eu.posthog.com/project/208816/dashboard/967523)
(project 208816). Saved query definitions and insight IDs are in
`order-reliability-posthog.json`. The dashboard exists; new canonical snapshot
metrics require the backend and FireLens image deployed together. Alerts have
not been created: the connected PostHog integration lacks `alert:write` and
`logs:write` scopes. No notification destination was configured.

## Counting contract

The active order keeper emits `keeper_order_reliability_snapshot` once a minute,
with separate 3,600- and 86,400-second windows. Each commitment cohort is
`[observed_time - 120 - window_seconds, observed_time - 120)` so normal
submission and execution get two minutes to settle. Outcomes are the keeper's
current indexed canonical outcomes, not client-side banners or signature events.

- Count each `(router, order_id)` once and scope to the configured router.
- Count case-normalized Trading Accounts, not browser sessions or people.
- Expiration is `status=failed` and canonical terminal reason 2.
- Repeatedly affected means at least two expired orders for the same account
  within that window. Other failures and pending orders remain separate.
- All order types are included. This does not identify the root cause of every
  expiration or imply every expired order was a close.
- Use the latest snapshot for each window. Never sum snapshots or add unique
  account counts across time windows. Replay of log records cannot inflate the
  latest snapshot's canonical counts.
- Snapshots older than ten minutes are unavailable, not healthy zeros. An
  explicit all-zero snapshot means a successfully queried empty cohort.
- Snapshot freshness proves reporting freshness, not chain-indexer freshness.
  Existing keeper/readiness monitoring remains necessary for ingestion stalls.

Only aggregates are exported. Wallets, order IDs, transaction hashes, amounts,
raw exception text and calldata remain excluded. Failed lifecycle logs now
export `terminal_reason` from a strict numeric enum mapping; legacy records
remain `NOT_EXPORTED` in the dashboard. Log observation counts are explicitly
labeled and must not be used as canonical order counts.

The observer owns a short-lived read-only connection, with a 1.5s SQL timeout,
0.5s lock timeout and 3s overall deadline. It never uses the keeper execution
connection or application pool, and failures emit an unavailable diagnostic
without interrupting execution. A `(order_router, commit_time)` index bounds the
cohort scan. It only runs in non-dry-run loop mode while the keeper owns its
advisory lock. It is cancelled when that keeper session ends.

## Activation

1. Merge and deploy the backend normally with the repository's `gh` workflow.
   This rebuilds the backend and log-router images; no frontend release needed.
2. Verify both windows arrive in PostHog. Compare a snapshot's exact cohort end
   and window to a read-only SQL aggregate before treating it as live monitoring.
3. Reconnect PostHog with `alert:read` / `alert:write` for SQL-insight alerts, or
   `logs:write` for log alerts. Confirm the desired notification recipient.
4. Create these initial rules and verify their saved destinations. The rules are
   starting thresholds, not established SLOs; tune after observing traffic.

| Rule | Initial condition | Insight |
| --- | --- | --- |
| Elevated expirations | Hourly expiry percentage >5%, minimum 20 committed orders; hourly check | `lzi8tBYj` |
| Repeated failures | More than 2 accounts with repeated expirations in the hourly cohort; hourly check | `uU0kqEEi` |
| Missing telemetry | No snapshot for 10 minutes; check every 5 minutes with a log alert, or hourly with the saved SQL insight | `s6nKPGDW` |

Scope every alert to Sepolia and `plether-keeper`. For the missing-telemetry log
alert, filter `event=keeper_order_reliability_snapshot`, require fewer than one
match in ten minutes, and enable only after rollout. SQL insight rules produce
no rows for stale impact telemetry, which is why the missing-data rule is
separate. The expiry-rate metric returns zero below its minimum traffic floor;
the canonical impact table still shows the actual counts.

Browser deadline and recovery panels deduplicate by random attempt UUID within
each reason. Reasons can overlap, blocked analytics can undercount, and these
panels must never be presented as unique trader counts.

## Validation

Run `lua posthog-projection.test.lua` in `apps/backend/otel-log-router` and
`node --test apps/backend/otel-log-router/routing.test.mjs` from the repo root.
The PostgreSQL integration cases (`Order reliability PostgreSQL`) cover case
normalization, repeated failures, router isolation, exact cohort boundaries,
pending-to-executed transitions, repeated observations and empty windows.
They use only a session-local temporary table. Existing database diagnostic
tests verify read-only connection settings, timeouts and cancellation.
