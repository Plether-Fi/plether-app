# Transaction and Perps Analytics Event Taxonomy

PostHog is used for anonymous product analytics and transaction diagnostics. Do not send wallet
addresses, transaction hashes, order IDs, signatures, permits, RPC payloads,
exact input amounts, exact balances, email addresses, or user identifiers.

Allowed properties are intentionally coarse: `surface`, `button_id`,
`modal_id`, `duration_ms`, `close_reason`, `market_phase`, `lifecycle_state`,
`direction`, `reduce_only`, `connected_state`, `chain_state`,
`error_category`, `validation_reason`, `size_bucket`, and the non-identifying
gas-sponsorship fields `manifest_version`, `account_mode`, `action_kind`,
`sponsorship_status`, `sponsorship_accepted`, `reason_code`, `retry_count`,
`terminal_outcome`, `wallet_family`, and `wallet_version`.

UserOperation and transaction hashes are retained only in the local sponsored
operation activity store until an approved operational telemetry sink and
retention policy are available. They must not be sent to anonymous PostHog.

Use the typed helpers in `perps.ts` instead of calling PostHog directly. If a
new property is genuinely needed, add it to the allow-list in `client.ts` and
include a test proving sensitive values are still dropped or redacted.

Sponsored actions that stop before the persisted operation tracker starts emit
`perps sponsored operation` with `sponsorship_status=preflight_failed`,
`terminal_outcome=preflight_failed`, and a stable `reason_code`. Supported
preflight reasons are `TRADING_ACCOUNT_UNAVAILABLE`, `INVALID_AMOUNT`,
`MANIFEST_NOT_CONFIGURED`, `IDENTITY_NOT_READY`, `MANIFEST_UNAVAILABLE`,
`MANIFEST_MISMATCH`, `SPONSORSHIP_DISABLED`, `RUNTIME_UNAVAILABLE`,
`OWNER_AUTHORIZATION_UNAVAILABLE`, `OWNER_AUTHORIZATION_FAILED`,
`ACTION_BUILD_FAILED`, `OPERATION_STORE_UNAVAILABLE`, `LANE_BUSY`,
`BROWSER_COORDINATION_UNAVAILABLE`, and `ACCOUNT_NOT_TRUSTED`. Use `UNKNOWN`
only for genuinely unclassified failures; never send raw exception messages.

PostHog Logs uses the same privacy boundary. Send logs through
`captureFrontendLog` only; it applies a second allow-list and a `beforeSend`
safety filter. Log bodies must be stable descriptions written by developers,
never raw exception messages or serialized request/wallet data. Console
autocapture remains disabled because application and third-party console output
can contain wallet addresses, transaction hashes, exact amounts, or RPC data.

The frontend is reported as service `plether-web`, with
`VITE_DEPLOYMENT_ENV` and the build commit attached as resource metadata.
Current structured logs cover React render failures, backend API failures, and
failed Perps order or margin lifecycle operations. Expected validation blocks
remain product events rather than high-volume operational logs.

## Order preparation failures

`perps order preparation finished` retains its existing `reason_code` source
(`background`, `cold`, `refresh`) and `error_category` outcome (`none`,
`cancelled`, `preparation_failed`). Active failures additionally emit:

- `error_code`: an ABI-allowlisted contract error name, or a stable classification
  such as `undecoded_revert`, `funding_shortfall`, `review_validation`,
  `review_leverage`, `network_failure`, `timeout`, `review_expired`, `aborted`,
  `arithmetic_panic`, `solidity_panic`, or `unknown`. Contract error arguments and revert bytes are never included.
- `stage`: `preflight`, `deployment_verification`, `context_read`,
  `max_size_quote`, `order_assessment`, `review_validation`, `funding_check`,
  `commit_simulation`, `preparation_timeout`, or `review_freshness`.
- `contract_function`: a source-defined function name when a directly
  instrumented contract call failed. It is omitted for local checks, timeouts,
  and deployment verification; it is never copied from an RPC error.

Successes and obsolete/cancelled requests do not emit failure fields. Timeouts
emit once even if the underlying request later rejects. UI error normalization
retains diagnostic metadata through the local cause chain, without sending
messages, stack traces, addresses, amounts, request parameters, or raw payloads.

To investigate the generic revert during order review, filter on
`error_category=preparation_failed` and `error_code=undecoded_revert`, then
break down by `stage` and `contract_function`. Limit `reason_code` to `cold`
and `refresh` for requests started in review; a `background` request can also
be reused by an open review. These fields are available only after deployment
and cannot classify historical preparation failures retroactively. Unique
PostHog identities remain anonymous and nonpersistent, not verified people.

Close-preview failures use `contract_function=previewClose` and an allowlisted
`assessment_point` (`current`, `midpoint`, `limit`). `getPreparationDebugContext`
retains chain, lens, account, block/hash, sample price and the original RPC error
in local error metadata only. Never serialize this debug object to analytics.

## Transaction failures and support references

`transaction failed` is the common event for wallet, vault, and Perps failures,
including preparation, approvals, submission, confirmation, and sponsored
operation recovery. Call `reportTransactionFailure` at a transaction boundary;
use `reportedTransactionError` when the UI receives an Error. Keep the original
cause when wrapping errors so the reference survives across layers.

Properties: `surface`, `action_kind`, `stage`, `support_reference`, `reason_code`,
`error_code`, and `build_commit`. Sponsored attempts also include `attempt_id`.
The displayed support reference is a random UUID (or `tx-` UUID); it is never a
wallet address, order ID, transaction hash, or session identifier. Transaction
hashes and explorer links remain available in the UI when known. User-facing
copy explains known failures and gives recovery guidance for unknown outcomes.
A matching structured PostHog log uses the same reference. Wallet declines are
recorded at warning level; other failures at error level.

In the connected Plether project, open Activity, select `transaction failed`,
and filter `support_reference` by the value supplied by the user. Break down
failures by `reason_code`, `action_kind`, `stage`, and `build_commit`. For failure
counts, count distinct support references: a recovered operation can acquire a
more precise reason later, and browser reloads can repeat recovery reporting.
Repeated callbacks with the same reference and reason are deduplicated within
the current page session (bounded to the most recent 1,000 pairs).

Existing Perps lifecycle events remain available. Order preparation events
also carry `support_reference`, preserving their more detailed preparation
stage and contract classifications. Cancelled/obsolete preparations do not
emit transaction failures.

New fields and the common event apply after this frontend is deployed; they
cannot enrich historical events. Delivery uses the existing PostHog queue and
configuration, so offline clients, disabled analytics, and blocked ingestion
can prevent receipt. Do not use client analytics as an authoritative ledger.
