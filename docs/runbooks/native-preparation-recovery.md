# Native preparation recovery

## Recovery contract

The frontend journals a versioned request before preparing and the complete unsigned
UserOperation before opening the wallet. The operation's existing activity ID is
also its preparation ID. Resume calls the idempotent preparation endpoint with
that ID, the original factory binding, and `resumeOnly: true` once an authorized
payload exists. This mode cannot create a preparation or a sponsorship.

Before signing again, the backend rechecks account/deployment identity, current
nonce, action policy, assisted-close eligibility, and batch estimation. Estimates
must fit the saved limits. The frontend compares every unsigned field and the hash,
checks deadline headroom, and compares stored position/protection state and client
intent. EIP-3009 data remains inside the exact calldata; batch simulation checks
its validity and consumption. No resumed action rebuilds calldata or identifiers.

`plether_getPreparationStatus` accepts one object with `version: 1`,
`chainId: "0x66eee"`, `sender`, and exactly one `preparationId` or
`userOperationHash`. Preparation IDs use the existing client identity. Hash reads
can also use the existing hash-bound recovery capability. The response contains
status, authorized locators, server time and the timestamp of the canonical block
at the reconciler cursor; it contains no payload, signature, or client key.

Observed inclusion requires a matching EntryPoint event in a direct transaction
receipt and a matching canonical block hash. It is distinct from settlement by
the safe reconciler. An observed inclusion is checked again on each status read,
so a reorg retracts it. Neither missing receipts nor browser outcomes release
backend sponsorship liability.

The activity view polls every 15 seconds, backs off to 60 seconds after errors,
and refreshes on focus. Polling never signs or submits. A fresh-review action
rechecks status while holding the browser lane lock. Assistance reservations
block only assisted actions. Declined/unknown wallet outcomes have ordered
revisions across tabs. Abandoned preparations remain journaled until their
reservation is resolved, even after the usual 24-hour history cutoff.

Storage v2 preserves v1 records without inventing payloads. Older records can
show status guidance; they cannot resume a missing transaction. Existing signed
native operations route recovery by their journaled paymaster even after an
issuance flag or frontend provider change.

## Rollout gate

1. Run backend unit and AA PostgreSQL integration tests, frontend tests/build,
   and `node --test scripts/aa-execution-regression.test.mjs`. The latter runs
   captured deployed account, EntryPoint and paymaster bytecode in local Anvil.
2. Review and merge the implementation. Record the remote commit that will be
   deployed. Do not dispatch an older `master` and call it this implementation.
3. Follow `AGENTS.md`: check `gh auth status`, `gh api user --jq .login`, the
   remote commit and recent workflow runs. Dispatch the Sepolia backend first
   with `bootstrap=false`, verify the run's `headSha`, and await success.
4. Dispatch the Sepolia frontend from the same reviewed commit. Verify its
   `headSha`, workflow success, readiness and public endpoint smoke checks.
5. With a canary wallet, decline a deposit signature and an order signature;
   resume each original operation. Check identical preparation ID/hash,
   unchanged calldata, one authorization, and no pre-approval submission.
6. Separately decline an assisted close for 198000 USDC atoms ($0.198). Verify
   exact resume while valid. For another attempt, let authorization expire;
   confirm the wait persists while the safe cursor lags, then clears only after
   reconciler resolution. Do not alter deadlines or reservations to accelerate it.

## Monitoring and rollback

Correlate `signature-declined`, `resume-started`, and `confirmed` (with
`resumed: true`) by the existing `attempt_id`. Preparation outcomes distinguish
wallet rejection, sponsorship refusal and unknown responses from on-chain failure.
Use the existing authorization/preparation linkage to check duplicate issuance,
and monitor reconciler lag and unmatched assistance evidence using the existing
AA diagnostics. Measure reservation wait from authorization expiry to reconciler
resolution; status reports both server time and reconciled block time without
promising a clearance deadline.

Rollback may disable new preparation issuance. Keep the status/recovery route,
authorization and preparation tables, browser journals and reconciler running.
Do not delete outstanding reservations, mark signed authorizations cancelled, or
change sponsorship validity windows/gas policy as part of rollback.
