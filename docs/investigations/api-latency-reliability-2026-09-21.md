# API latency and intermittent ALB 502s: reliability patch

## Evidence and scope

During 2026-09-21 11:28:45–12:28:45 UTC, 809 preparation requests had
median/p95 durations of 1.43/3.65 seconds. Requests reaching fresh sponsorship
signing had median/p95 3.47/4.14 seconds. Security verification typically took
1.05 seconds, estimation 98 ms, and signing 18 ms. One 27.77-second request
contained only 1.90 seconds in the measured `prepare` stage. The remainder is
not yet attributed. Stage timings are nested, not additive.

Ten ALB-generated 502s occurred throughout that hour. The ALB idle timeout is
75 seconds but the API previously inherited Warp 3.4.12's 30-second default.
This mismatch is a documented possible cause of 502s, not proof that it caused
those ten errors. No raw ALB access logs are enabled by this patch.

The API log router also emitted 42,478 severity conversion errors. First-party
records already contain numeric severity, while the type converter expected a
string. This is a confirmed logging defect, not a confirmed cause of stalls.

## Changes

- Use explicit Warp connection settings with a 120-second idle timeout, above
  the ALB's 75 seconds. Warp pauses this timer while the application executes;
  this is not a 120-second request deadline. Existing route deadlines, including
  intentional 60-second long polls, are unchanged.
- Normalize both numeric and string severity without rejecting records. Valid
  numeric records bypass Lua reserialization; strings become OTLP integers;
  malformed values fall back to INFO. Both CloudWatch and PostHog copies remain.
- Add `body_read`, `recovery_authorization`, `lease_token`, `lease_acquire`,
  `lease_release`, `preparation_claim`, `preparation_bind`, `recovery_bind`,
  `preparation_save`, `preparation_result`, `diagnostic_link`, and
  `preparation_release` timings to the existing server-generated request ID.
- Record the validated RPC-method enum as a bounded `method_*` counter.
- Measure log-output lock waiting and write/flush time on the request thread.
  The observer is scoped and exception-safe, restores nested observers, and
  excludes unrelated threads. No global-counter deltas or extra per-log records.
  `log_lock_wait_ms` and `log_write_ms` are also allowed through PostHog's existing
  privacy projection; raw bodies, credentials, and addresses remain excluded.

## Interpreting the measurements

`http_total` includes intake and dispatch. `prepare` contains its nested stages;
`preparation_result` contains rejection logging or authorization/signing and
response construction. Repeated release stages represent existing separate
release calls, not duplicate instrumentation. Logging durations overlap the
stages in which logging occurred: never sum every stage to reconstruct total.

The logging observer measures completed writes on the handler thread, not child
RPC threads. It intentionally excludes its own final timing-record export and
the request-completion middleware. If the final timing record itself blocks,
the foreground middleware duration can exceed `http_total`; compare both.
Cancellation/uncaught exceptions can prevent the final timing record from being
emitted. Missing timings are not zero latency and do not prove a stage was fast.

This patch does not optimize security RPCs, weaken independent verification,
change authorization, add retries, change database leases, or raise alarm
thresholds. It does not promise to clear the p95 alarm by itself.

## Validation and deployment

Run the backend unit suite and API build, both Lua suites, and the real Fluent
Bit routing tests (`AA_LOG_ROUTER_TEST_IMAGE` must be set; otherwise they skip).
The server regression uses a single raw TCP socket across an 80-second idle
period, with no client reconnection/retry, and separately exercises a long poll.
No database migrations or frontend release are required.

After review, deploy the backend and rebuilt log-router image through the normal
manual Sepolia workflow. Compare ALB 502 counts, target health, foreground p95,
and the newly measured stages under comparable traffic. Check that numeric and
legacy string log records arrive at both destinations without conversion errors.
Exercise one authorized preparation and one normal rejection; never replay an
unknown signed transaction for a smoke test. Keep the current p95 alarm enabled.

Rollback by deploying the previous backend/router images together. There is no
data migration to reverse. Revisit the server timeout if the ALB timeout changes.

References:
- https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-troubleshooting.html
- https://docs.fluentbit.io/manual/data-pipeline/filters/lua
