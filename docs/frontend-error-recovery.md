# Frontend error recovery and private source maps

Sepolia deployment requires the GitHub repository secret `POSTHOG_CLI_API_KEY`:
a PostHog personal API key authorized for project **208816** with error tracking
write and organization read scopes. Set it through GitHub's secret settings or
`gh secret set POSTHOG_CLI_API_KEY --repo Plether-Fi/plether-app` (interactive stdin).
Never put this key in a `VITE_` variable, source code, a chat message or an artifact.
The public ingestion key `VITE_POSTHOG_KEY` is not an upload credential.

The workflow generates hidden maps, injects chunk identifiers and uploads only
`dist/assets` to EU PostHog, identifying the release by its full Git SHA. It uses
a pinned action and CLI, with `symbol-set` release association for compatibility
with the installed browser SDK. TradingView's separately copied runtime is not uploaded.
Upload failure blocks publishing. Cleanup runs even on failure and removes every
`.map` from the generated publish directory. Ordinary builds emit no source maps.
Do not publish or archive the pre-cleanup build directory.

Before rollout, run `node --test scripts/remove-source-maps.test.mjs` from
`apps/frontend`. After deployment verify the upload step succeeded, the deployed
JavaScript contains injected chunk IDs, no public source-map responses contain
JSON source data, and a controlled frontend exception resolves to application
source in PostHog. Old releases without maps cannot be retroactively symbolicated
from a different build.

Known module-fetch failures get one bounded retry, then a manual reload screen.
There is no automatic page reload, clearing of site storage or transaction retry.
The recovery screen tells users to check Transaction History before resubmitting.
Module diagnostics retain only same-origin static asset filenames, not URL query
strings, provider endpoints, credentials, recipient addresses or signed payloads.

Preparation logs now correlate `aa_preparation_failed`, `aa_preparation_rpc_failed`
and timings by the server-generated `request_id`. The preparation failure also
retains the existing validated `attempt_id` when available, allowing correlation
to frontend attempts without logging accounts or calldata. Filter the preparation
failure event and count distinct attempt IDs (when present) or request IDs, not
all log lines. Request IDs are not distinct trades or users. The known
opposite-position refusal is `MUST_CLOSE_OPPOSING`; it remains non-retryable and
does not trigger signing or broadcast. The frontend checks positions at the
review's verified block; the backend still handles races after that block.
