# Trading Account confirmation and recovery

`ACCOUNT_DEPLOYMENT_PENDING` means the account was observed at latest but not at
the backend's safe block. It can be returned before a preparation row exists.
The frontend retains that exact reason; it never infers it from a 403. The saved
account/deployment monitor checks every 15 seconds while visible and on focus.
RPC failures are displayed separately from a successful check that finds no
code. Waiting time is diagnostic only. Confirmation enables a manual retry of
the original ID; it does not sign, submit, or release the browser lane.

## Recovery protocol

All methods use the existing authenticated AA proxy and a single version-1
locator: `version`, `chainId`, `sender`, `preparationId`. Scope additionally
includes the configured paymaster. Inputs and existing per-IP/account rates are
bounded. The browser prefers its existing hash-scoped read credential when it
knows the operation hash.

- `plether_getRecoveryChallenge`: also accepts `owner` and exact configured
  `origin`. Issues a five-minute challenge bound to origin, chain, paymaster,
  account, preparation ID, owner, purpose, nonce and expiry.
- `plether_verifyRecoveryChallenge`: accepts `challengeId` and personal-sign
  `signature`. Verifies the pinned factory, deterministic account and deployed
  identity (or counterfactual identity) at one latest block. Consumption is
  atomic and single-use. Returns a 15-minute session; the database stores its
  SHA-256 digest and the browser keeps the credential only in runtime memory.
- `plether_getRecoveryStatus`: requires `X-Plether-AA-Preparation-Recovery`.
  Returns a bounded projection across historical client namespaces. No client
  keys, signatures, complete operations or credentials appear in this result.
- `plether_retirePreparation`: requires the same session and the rollout flag.
  Returns `retired` only after an atomic registry check proves retirement safe.
  Otherwise returns `unresolved`, retaining the saved attempt.

Recovery states are missing, ambiguous, unresolved, retired, or the existing
preparation phase with `recoveryVerified` and `canRetire`. Ambiguous records
expose at most 20 operation hashes and durable outcomes and cannot be resumed.
A verified single match selects its original client namespace. Preparation
still checks the immutable intent, nonce, gas, expiry, budgets and safe state.
Submission using a recovery session selects only that preparation's original
namespace for the exact sponsored hash and owner. It still needs the real
transaction signature and passes existing submission checks. Hash-only recovery
credentials remain read-only.

## Registry and discard safety

Preparation claims, reservation, signing and delivery participate in the
registry fence and the existing sponsorship advisory transaction lock. A
retired ID retains its tombstone and incremented generation. Expired leases
cannot reserve or claim new work after retirement.

Retirement checks every historical row for the saved sender/ID, requires a
verified chain/router/paymaster binding, and conservatively checks sender-wide
outstanding leases and authorizations. This also catches reservations whose
preparation linkage was lost. Active signed/submitted liabilities block discard
even past their wall-clock expiry; the existing reconciler must prove terminal
settlement or safe expiry. Unknown historical bindings remain unresolved.
The browser marks the preparation resolved and cancels/releases its lane only
after an authoritative retirement response to an explicit Discard click.
Lost responses can be retried: retirement is idempotent.

## Sepolia rollout

1. Verify the exact release SHA and run frontend AA/store/execution tests,
   TypeScript, lint, worker tests, backend unit and AA PostgreSQL integration
   tests. Apply the migration twice in an isolated database and retain results.
2. Follow `AGENTS.md` and the Singapore release runbook. With the database-owner
   role apply `apps/backend/config/migrations/aa-preparation-recovery-v1.sql`
   after existing preparation and observability migrations. It preserves old
   rows and backfills only paymasters supported by durable authorization data.
   Old unsigned authorization payloads omit the paymaster. After owner
   verification, recovery recomputes their persisted EIP-712 sponsorship digest
   under the configured deployment before binding and linking them. This is
   limited to 21 matches and requires the recorded chain/router to agree.
   Unknown bindings remain null and cannot be discarded through recovery.
3. Grant the existing API database role SELECT/INSERT/UPDATE on
   `aa_preparation_registry` and `aa_preparation_authorizations`, and
   SELECT/INSERT/UPDATE/DELETE on `aa_preparation_recovery_challenges` and
   `aa_preparation_recovery_sessions` (DELETE is only for expired proof cleanup).
   Keep existing preparation/sponsorship access. Never grant registry deletion
   or schema ownership to the API role.
4. Deploy the fenced backend first with retirement false. Set Terraform
   `aa_preparation_recovery_origin` to the exact Sepolia frontend origin. Keep
   existing issuance, submission, safe-state, budget and cohort policy unchanged.
   Confirm every running API task uses the approved image digest and old tasks
   have drained. A partially upgraded fleet must not enable retirement.
5. Enable `aa_preparation_retirement_enabled` only after that fleet check and
   deployment completion. Then manually deploy the Sepolia frontend from the
   same master SHA. Use GitHub CLI authentication, remote SHA verification,
   duplicate-run checks, dispatch and terminal monitoring for every deployment.
6. Run the canary below, record run IDs, image digests, task definitions and
   endpoint checks. Do not log signatures or recovery tokens. Logs contain only
   bounded outcome/reason codes, optional attempt references and durations.

Rollback must retain registry enforcement. If reverting to an older image,
disable issuance and preparation first and keep them disabled. Never delete
retirement rows, clear liabilities or relax safe-block confirmation policy.

## Canary acceptance

Use a designated Sepolia owner wallet and an ordinary reviewed test intent.
Keep every wallet action explicit; this runbook grants no automatic submission.

1. Observe a newly deployed account before safe confirmation. Confirm the saved
   action reads “Account confirmation required”, shows elapsed time and last
   successful check, and does not query a missing preparation. Reload and hide
   the page, then focus it. Simulate an RPC failure and check that it changes
   diagnostics without unlocking Resume.
2. Once safe code appears, confirm no wallet prompt or submission occurred.
   Click Resume and verify the original preparation ID and immutable intent.
3. Interrupt a prepared attempt, change the trusted client IP, and reopen it.
   Verify there is no automatic wallet prompt. Click Verify wallet, review one
   gas-free scoped message, and confirm the original status is recovered.
   Explicit Resume must preserve the expected operation hash.
4. Test wrong owner, another account/deployment, expired/replayed challenge and
   expired session. All must remain blocked and must not send a transaction.
5. Verify Discard on an unissued attempt. Confirm an authoritative retired
   response precedes local lane release and the same ID cannot prepare again.
   Retry after a lost retirement response. An outstanding issued authorization
   must prevent discard even after its validity deadline until safe reconciliation.
6. Inspect a legacy record without its reason and one with ambiguous matches.
   Never reinterpret a 403 as confirmation pending; show bounded existing
   outcomes, preserve unresolved records and never select an arbitrary match.
