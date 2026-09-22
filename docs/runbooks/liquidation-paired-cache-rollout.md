# Liquidation paired cache rollout

The September 22 Sepolia alerts included 14 basket/payload timestamp mismatches
and one stale merged-price rejection in two hours. Separate reads of the latest
payload and mutable minute snapshot can mix observations even though the writer
commits both together.

## Release A: writer and additive schema

Deploy paired payload writes while the liquidation reader remains unchanged.
The payload row now stores its signed basket price and components. A database
constraint requires both fields to be present or absent together. Source
precedence applies to the entire upsert. Legacy helper writes clear both fields.
No public API or chart snapshot semantics change.

For closed markets, a response re-admitted through the existing on-chain parser
can hydrate missing components only when the persisted source, full publish-time
vector, bounds and payload bytes match exactly. Hydration does not insert a
payload, change fetched_at, or promote a price observation. Never backfill these
fields from a minute snapshot.

Before Release B:

1. Follow AGENTS.md: check GitHub authentication, remote master SHA and recent
   runs; dispatch deploy-backend.yml from master with environment=sepolia and
   bootstrap=false. Verify the run headSha and successful terminal result.
2. Verify every active basket-writing task uses Release A, including any
   consolidated workers. Wait for old tasks to drain.
3. Require repeated pyth_payload_basket_pair_ready logs from the new writer.
   This event reads back the newest eligible row after source precedence has
   been applied. Require no subsequent pyth_payload_basket_pair_missing events.
4. If exact hydration is unavailable during a market closure, postpone Release B
   until a new admitted paired payload exists. Do not relax admission rules.
5. Check ECS stability and the deployment's public endpoint smoke checks.

## Release B: paired reader

Use the newest admitted latest-source payload and its embedded basket in one
query. Missing components on the newest row must be an explicit failure; do not
fall back to an older row or the chart cache. Existing timestamp, reconstructed
price, stored-price merge and exact-block freshness validation remain required.
Failed classifications must continue aging in the watchdog.

Deploy with the same GitHub CLI checks. Maintain one active liquidation execution
owner and preserve pending transactions. Confirm successful sweeps and inspect
structured risk-input failures and watchdog samples.

Observe at least two complete hourly alarm periods. Report two results separately:

- Pairing correctness: no missing/corrupt-pair failures during live updates,
  successful classifications, and no unchecked-age spikes caused by pairing.
- Operational recovery: the oldest-unchecked and risk-inputs-unavailable alarms
  return to OK. Genuine stale-price failures may prevent alarm recovery and need
  separate investigation; alarm silence alone does not prove correct pairing.

Do not change alarm thresholds, freshness limits or liquidation eligibility.

## Rollback order

Roll back the liquidation reader to Release A first, leaving paired writes and
the additive columns in place. Verify the paired reader is no longer active
before reverting any writer to a version predating Release A: old binaries do
not clear embedded components when they overwrite payload metadata.

Never delete pending transactions or start a second signer. Reader rollback
restores the previous mismatch risk and is not full incident recovery.
