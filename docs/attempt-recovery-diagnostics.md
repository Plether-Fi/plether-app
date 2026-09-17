# Investigating interrupted Trading Account actions

The support reference is sufficient to look up an attempt. `aa_attempt_diagnostics`
contains its canonical backend outcome; `aa_attempt_events` retains the stages
observed while preparing, approving and submitting it. Query both on a read-only
support connection, using the reference as a bound parameter:

```sql
SELECT attempt_id, chain_id, sender, operation_hash, stage, reason, created_at, updated_at
FROM aa_attempt_diagnostics WHERE attempt_id = $1::uuid;
SELECT source, stage, observed_at
FROM aa_attempt_events WHERE attempt_id = $1::uuid ORDER BY observed_at, source, stage;
```

Browser events are advisory, client-scoped reports. They cannot prove submission,
rejection or expiry and never change the canonical outcome or release trading locks.
Backend stages distinguish receipt, rate/security/identity/policy checks, submission
journaling, forwarding, and the bundler response. `bundler_acknowledged` does not
mean the operation was included or an order filled. The timestamp records receipt
by the diagnostic service; reports may arrive out of order. Each stage is stored
once per source and attempt, so a later retry does not replace earlier evidence.

Telemetry is best effort with bounded requests and a bounded backend queue. A
missing event is not proof that a step never happened. In particular, browser
suspension, offline use, changing IP, diagnostic outages, or a full queue can leave
gaps. No raw errors, wallet signatures, signed operations, RPC URLs, or recovery
credentials are accepted in these events. Browser reports cannot impersonate backend
stages or write to an attempt belonging to a different client scope.

## Expiry recovery

Receipt lookup failure alone never unlocks an attempt. Recovery can nevertheless
prove expiry from a safe block after the signed sponsorship deadline and an unused
EntryPoint nonce at that exact block. The signed payload must match its stored hash,
account, chain and sponsorship authority. An advanced nonce, existing transaction
hint, conflicting receipt, or inclusion awaiting safe confirmation stays unresolved.
A resolved attempt becomes `expired` and releases its trading lane without signing
or sending another operation. Existing background recovery and manual checks both
use this rule.

If wallet approval returns too late, submission remains blocked, the signed payload
is saved for recovery, and the UI explains that Plether did not send it. The browser
clock does not authorize lane release; safe chain evidence still must establish expiry.

## Rollout and rollback

1. Apply `apps/backend/config/migrations/aa-attempt-events-v1.sql` with the migration role.
2. Grant the existing API runtime role SELECT/INSERT on `aa_attempt_events`.
3. Deploy the backend, then frontend. The new endpoint uses the existing authenticated,
   uncached diagnostics proxy path; no proxy secret or browser credentials change.
4. Smoke-check a controlled test attempt by support reference and verify both sources.

A missing migration/permission drops diagnostics and logs `aa_attempt_event_dropped`
or `aa_diagnostic_export_dropped`; it must not block trades. Keep the additive table
on rollback. Maintenance should delete event rows for diagnostics whose `terminal_at`
is more than seven days old, before deleting those diagnostic mappings. Unresolved
attempts must not be purged. Do not grant runtime DELETE or UPDATE access.
