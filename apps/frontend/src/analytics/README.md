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
