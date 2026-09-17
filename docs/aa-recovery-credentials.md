# AA recovery credentials and React diagnostics

## Recovery access

The native Sepolia gateway issues `X-Plether-AA-Recovery` after it has stored
and validated the final sponsorship signature, before the browser signs or
submits the operation. The browser saves this response header before returning
the preparation result. A lost submission response therefore does not lose the
credential. Successful legacy IP-authorized receipt/status reads also issue it.

The credential is a seven-day HMAC-SHA256 capability scoped to the exact
UserOperation hash, original client pseudonym, Arbitrum Sepolia, Alto, and
configured paymaster. Its domain-separated key derives from the existing AA
origin secret; no new infrastructure secret or database migration is required.
Verification is constant-time. A valid capability selects the original client
for the existing recovery-authorization and receipt-locator database checks;
it does not bypass row expiry, provider binding, or canonical receipt checks.
IP quotas still use the current trusted Cloudflare IP. Credentials cannot
authorize sponsorship, transaction submission, or another operation's reads.

The native browser transport attaches credentials only to receipt/by-hash/status
reads on its configured endpoint. Preparation and bundler endpoints share an
explicit credential scope. Redirects are rejected. Credentials are persisted
locally with expiry pruning and a 1,000-entry per-browser bound (not a global
transaction quota); storage failure falls back to memory. Worker forwarding
strips the header from unrelated API routes. Replay metadata omits request and
response headers/bodies; no capability is sent to analytics.

### Compatibility and limits

- Old clients remain supported through the existing IP authorization path.
- Old operations can acquire credentials while their original network is still
  authorized. An already locked-out operation without a credential is **not**
  retroactively unlocked; a separate owner-proof migration would be needed.
- Clearing browser storage, eviction beyond 1,000 entries, secret rotation or
  expiry can remove cross-IP recovery access. The original-IP fallback remains
  subject to the existing database authorization and seven-day expiry.
- `RECOVERY_PENDING` is a retryable JSON-RPC error with HTTP `Retry-After: 60`,
  not a null receipt or failure proof. Background recovery waits at least a
  minute before another receipt scan and does not release the lane or mark the
  operation safe to retry. Existing overall recovery deadlines remain in force.
- Invalid canonical evidence remains `RECOVERY_EVIDENCE_UNAVAILABLE`.

## Bundler request IDs

Alto 1.2.7 rejects string JSON-RPC IDs. The gateway uses a numeric ID for each
independent upstream HTTP call, validates the upstream response ID, then restores
the caller's original string or numeric ID. Valid upstream RPC errors remain
errors; mismatched IDs and malformed responses remain fail-closed.

## React diagnostics

All three React root error callbacks now capture sanitized exceptions separately
from the existing structured logs. Capture retains the exception name/message,
application-asset stack locations, component stack, release and deployment.
It drops causes/custom exception properties, raw RPC URLs, bearer credentials,
hex payloads and request details. SDK context on exception events is allow-listed;
global exception/console autocapture remains disabled. Reporting and initialization
queue failures cannot throw into application code.

This enables diagnosis of the previously opaque render failures. It does not
claim to repair the still-unidentified crashing component. Stack frames remain
minified until source-map upload is configured; no PostHog settings are changed
by this PR. See [PostHog exception capture](https://posthog.com/docs/error-tracking/capture).

## Rollout and verification

Deploy the backend first, followed by the Sepolia frontend/Worker. No deployment
is performed as part of this PR. Verify numeric and string-ID read-only requests;
prepare an operation, confirm credential persistence, submit, then switch networks
and recover the same hash. Check that another hash and expired credentials remain
forbidden. Confirm pending evidence does not unlock the transaction lane.

Exercise a controlled React exception in a non-production environment and inspect
the sanitized Error Tracking event. After deployment, monitor validation errors,
recovery outcomes and newly captured React issues. Existing historical generic
crash logs cannot be reconstructed. Backend and frontend can be rolled back
together; stored capabilities will be ignored by the previous backend.
