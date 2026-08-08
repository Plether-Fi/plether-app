# Perps Analytics Event Taxonomy

PostHog is used for anonymous Perps product analytics only. Do not send wallet
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
