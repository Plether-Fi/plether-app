# Insights snapshot publication diagnostics

Snapshot publication rejects obsolete inputs after collection without publishing a
partial batch. `insights_snapshot_rejected` records the reason **after rollback**
and after temporary staging cleanup. Its timing fields include that cleanup, so
`lock_held_ms` is a conservative upper bound, not an additional lock operation.
Missing timing/context fields are JSON null, not zero or guessed values.

| Reason | Severity | Interpretation |
| --- | --- | --- |
| `input_epoch_changed` | INFO | Competition inputs changed during indexed writes. |
| `participant_set_changed` | INFO | Collected wallets no longer match the registered roster, including same-size remaps. |
| `competition_finalized` | INFO | The competition became immutable. |
| `competition_missing` | INFO | The competition is no longer available. |
| `history_cursor_not_ready` | INFO | Canonical history does not cover this batch. |
| `account_lens_changed` | INFO | The configured account lens differs from the collected inputs. |
| `mixed_batch_identity` | ERROR | Internal defect: incompatible snapshots in one batch. |
| `duplicate_participant` | ERROR | Internal defect: duplicate wallets in one batch. |

Rejections include snapshot kind, block number, known input/current epochs,
known participant counts, elapsed time and available lock timing. They do not
include wallets, trader references, hashes, RPC bodies, tokens or exception text.
The `retryable` field classifies expected state changes versus internal defects;
it does not change the worker loop. The worker does not also emit a generic
failure for an already logged rejection.
There is no immediate retry; the existing polling loop remains unchanged.

`insights_snapshot_cycle_failed` distinguishes database errors (with SQLSTATE)
from unexpected exceptions (ERROR, without their payload). A failure of the
post-cycle health query has its own `insights_snapshot_health_failed` event and
does not falsely report a failed publication. Async cancellation
is rethrown. SQL timeouts remain failures rather than expected rejections.

## Progress and recovery

One bounded, read-only query after each cycle reads the latest committed live
batch publication time. Its transaction uses the existing one-second lock and
five-second statement limits. It changes no competition or participant rows.
The progress state resets only when the committed publication timestamp advances;
a normal return after an RPC skip cannot falsely reset it.

`insights_snapshot_progress` reports `consecutive_unsuccessful_cycles`,
`last_publication_age_seconds`, `seconds_without_publication`, `active`, and
`stalled`. These are process-local observations and restart with the worker.
If there has never been a publication, the age is null and the worker start time
provides the grace period. Outside the mutable live competition window, progress
is inactive and alerts are suppressed.

`insights_snapshot_stalled` is a WARN after three consecutive cycles without a
new publication, or a publication age greater than twice the configured polling
interval. Repeated warnings are rate-limited to once per five minutes.
`insights_snapshot_recovered` records the transition back to healthy publication.
These are structured log signals for the existing logging/alerting pipeline;
this change does not provision a new CloudWatch alarm or notification destination.

Count all rejection events by `reason`; do not treat an individual roster race
as an outage. Investigate ERROR reasons immediately, and use stalled/recovered
signals to distinguish transient races from sustained publication trouble.
An epoch rejection may happen before the roster is read, so a null current count
is expected. The first failed validation wins; logs do not claim to enumerate
all changes that occurred concurrently.

## Release

Deploy the consolidated backend worker service after merge. No migration,
frontend change, pool-size change or competition-rule change is required.
Rollback uses the previous worker image; structured log consumers should tolerate
new additive event names and fields. No production data belongs in this runbook.
