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
