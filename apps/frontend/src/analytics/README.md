# Perps Analytics Event Taxonomy

PostHog is used for anonymous Perps product analytics only. Do not send wallet
addresses, transaction hashes, order IDs, signatures, permits, RPC payloads,
exact input amounts, exact balances, email addresses, or user identifiers.

Allowed properties are intentionally coarse: `surface`, `button_id`,
`modal_id`, `duration_ms`, `close_reason`, `market_phase`, `lifecycle_state`,
`direction`, `reduce_only`, `connected_state`, `chain_state`,
`error_category`, `validation_reason`, and `size_bucket`.

Use the typed helpers in `perps.ts` instead of calling PostHog directly. If a
new property is genuinely needed, add it to the allow-list in `client.ts` and
include a test proving sensitive values are still dropped or redacted.
