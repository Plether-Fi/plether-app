# Native AA submission timeout

## Evidence and scope

Investigation window: 2026-09-21 19:42:14 through 2026-09-22 07:42:14 UTC.

- Six frontend attempts reported `SUBMISSION_OUTCOME_UNKNOWN`. Five spent
  approximately ten seconds between submission starting and the failure.
- Native preparation explicitly used a 30-second HTTP timeout, but native
  submission inherited viem's ten-second default.
- Fourteen distinct attempts separately failed the deadline-headroom check
  after wallet signing. Fifteen log records included one duplicate. This is
  not the same failure as losing a submission acknowledgement.
- Some backend requests spent most of their time receiving the streamed body:
  41.59 seconds of a 44.25-second request, and 35.98 of a 37.68-second request.
  That locates the delay before normal processing, but does not establish which
  network hop caused it or correlate those requests to the six frontend attempts.
- The 117 generic frontend API timeout records do not identify the endpoint;
  they must not be reported as 117 failed trades.

## Change

Use a dedicated native submission transport with an explicit 30-second HTTP
timeout and zero automatic transport retries. Apply it both with and without
preparation-recovery headers. Continue sending the exact signed operation through
an accountless bundler client: do not prepare, sponsor, or sign it again.

Keep the stored hash, signed payload and active account lane when the response
is lost. The existing recovery flow, not a new trade, resolves the outcome.

The pinned viem transport's timeout covers waiting for response headers, not a
separate hard deadline on JSON-body parsing. This patch deliberately retains the
standard transport rather than introducing a custom timeout implementation.

Unchanged: preparation, read/polling timeouts, transaction validity, expiry
headroom, chain checks, receipt recovery, Worker body streaming and telemetry.
No backend deployment or database migration is required.

## Regression coverage

- A 15-second acknowledgement succeeds with one send, with and without recovery
  credentials, despite a caller's default retry count of three.
- A missing response aborts at 30 seconds and is not resent automatically.
- Network failures and HTTP 502 responses are not resent automatically.
- The runtime uses the dedicated policy even without recovery credentials.
- Execution retains the signed payload, hash and active lane after a timeout;
  the outcome remains unknown and non-retryable, not a confirmed chain failure.
- Existing signing-expiry checks continue to prohibit late submission.

## Rollout and remaining investigation

Review and merge, then deploy the Sepolia frontend. Monitor submission outcomes
and existing recovery diagnostics. Do not interpret fewer ten-second failures
as proof that the body stalls or wallet-signing delays have been resolved.

Further work should investigate the streamed-body delay and signing-stage
duration independently. Correlated edge/backend timing would help, but adding
new externally exported telemetry is outside this patch.
