# Frontend crash recovery

The application-level React error boundary wraps the wallet, query and trading
providers. Render, effect and rejected lazy-import errors show an independent
recovery screen instead of removing the entire UI.

## Supporting an affected user

1. Ask for a screenshot containing the support reference (`ui-…`), build, and
   approximate time/timezone. Browser console access is not required.
2. Find the matching PostHog `$exception` using `properties.support_reference`.
   Correlate with `build_commit`, `deployment_name`, and the structured frontend
   log `react application recovery screen shown` (same `support_reference`).
3. The exception contains a sanitized message, same-origin asset stack locations
   and React component stack. Use the matching release to investigate the cause.
4. Ask the user to reload manually. If they were submitting a transaction, check
   Transaction History before retrying. A UI crash does not establish whether a
   transaction was submitted or included.

Never advise clearing site data as a first-line recovery step: saved transaction
recovery records may be needed. The fallback does not clear storage, remount the
application automatically, or retry any transaction.

## Reporting and limits

Exception reports retain the SDK's public project ingestion `token`; it is not
an RPC, wallet or server credential. Other browser context is still allowlisted.
Crash reporting starts analytics initialization immediately if it has not loaded
yet. It remains best-effort: blocked analytics, offline clients, missing project
configuration, or a failed SDK download can prevent delivery. A visible support
reference is not a guarantee that a report was delivered.

The boundary does not catch failures before React mounts (for example, failure
to download the entry bundle), event-handler errors, or arbitrary asynchronous
callbacks. This change improves recovery and evidence collection; it does not
prove that the original startup-crash cause is fixed.

## Verification

Run frontend tests and the production build. The SDK transport regression test
intercepts fetch with a fake project key; it must not send data to a live project.
After a separately authorized deployment, verify affected users receive the new
build and correlate any new recovery-screen references with received exceptions.
