# Plether Insights September 2026 operations runbook

This runbook covers the first-party registration and operation of the September
2026 Arbitrum Sepolia competition. The immutable rules and accounting semantics
live in `insights-testnet-competition.md`.

| Event | UTC |
|---|---|
| Registration opens | When the enabled API deployment first starts successfully |
| Trading starts | `2026-09-13T21:00:00Z` |
| Registration closes | `2026-09-20T21:00:00Z` |
| New-risk and scoring cutoff | `2026-09-25T21:00:00Z` |
| Results | `2026-09-28T12:00:00Z` |
| Payout deadline | `2026-10-03T00:00:00Z` |

Registration and trading intervals are half-open. A registration completion or
trade at the exact closing timestamp is rejected.

## Launch invariant

Keep `enable_insights_registration = false` until the canonical Insights UI and
Pages Worker are reachable. The first healthy API process started with
`INSIGHTS_REGISTRATION_ENABLED=true` atomically records the actual registration
open time. Enabling it before the edge and UI are live silently shortens the
public registration window.

Use GitHub CLI for every GitHub Actions deployment operation. Do not deploy or
inspect Actions through the web UI, and do not update ECS services directly.
Terraform may register new task-definition revisions; the approved GitHub
workflow deploys those revisions.

## Pre-launch configuration

### September release manifest

Set both activation guards to the exact slug:

```hcl
insights_active_competition_slug = "testnet-trading-2026-09"
insights_competition_release_id  = "testnet-trading-2026-09"
```

Supply the reviewed September values for every manifest field:

- `perps_usdc`
- `perps_order_router`
- `perps_plether_oracle`
- `perps_cfd_engine`
- `perps_cfd_engine_settlement_sidecar`
- `perps_cfd_engine_lens`
- `perps_margin_clearinghouse`
- `perps_account_lens`
- `perps_indexer_start_block`

All eight addresses must be valid nonzero addresses, mutually distinct, and
absent from the entire pinned July manifest. The indexer start block must be a
new positive value. Verify the deployed bytecode and frontend release manifest,
then record their reviewed addresses and deployment transaction hashes in the
restricted launch record. Terraform rejects a partial, inherited, or role-
swapped July manifest.

### Registration database encryption and TLS gate

First-party registration may be provisioned only on encrypted RDS storage. Set
`db_storage_encrypted = true` for a new or already encrypted database and leave
`db_kms_key_id = ""` to use the AWS-managed RDS key, or supply a reviewed
customer-managed KMS key ARN. Pin `db_ca_cert_identifier` to a supported RDS G1
CA and keep:

```hcl
db_ssl_root_cert_path = "/etc/ssl/certs/aws-rds-global-bundle.pem"
```

Terraform generates `DATABASE_URL` with percent-encoded credentials,
`sslmode=verify-full`, and that explicit `sslrootcert`. The shared backend image
contains the checksum-pinned AWS global RDS trust bundle at the configured path;
the API, indexers, snapshot workers, admin tasks, and all other database
consumers use that image and the same SSM URL. Smoke-test at least the API,
selected worker topology, indexer, and a one-shot admin command so a missing CA
file or hostname mismatch fails before registration is enabled.

The `Database Management` workflow derives its minimal one-off task from the
deployed backend image, attaches only the existing `DATABASE_URL` SSM secret,
and executes the schema bundled in that image. It must not revert to a stock
Postgres image that lacks the pinned CA, decrypt the URL into a workflow output,
or interpolate database credentials into an ECS task definition.

`schema.sql` and the Haskell `ensure*Schema` routines are two representations of
the same schema contract. The image copies `schema.sql` from the exact source
commit, and backend tests assert the static registration tables and privacy
constraints, but the static script alone is not proof that an activation is
ready. Every schema change must update both representations and their parity
tests. After any `init-schema` task, deploy/restart the same reviewed API image so
its idempotent runtime ensure functions run, then verify registration and
competition schema health before enabling registration.

An existing unencrypted RDS instance cannot be encrypted in place. Do **not**
change `db_storage_encrypted` and apply a replacement: the resource has
`prevent_destroy`, and registration provisioning fails closed while storage is
unencrypted. Prepare a separately reviewed migration that copies a final source
snapshot with encryption enabled, restores it under a temporary identifier,
validates row counts and certificate-verified connections, quiesces writers,
performs the final delta/cutover, and then deliberately reconciles the endpoint
and Terraform state. Retain the source instance and snapshots through the
rollback window. Never disable `prevent_destroy` as a shortcut in the ordinary
registration plan.

### X developer application

Before provisioning the application:

1. Fund the X developer account and confirm that its API credit balance covers
   the expected registrations plus retries.
2. Configure OAuth 2.0 Authorization Code with PKCE and the scopes
   `users.read users.email follows.read`.
3. Register this exact callback, without a query string or fragment:

   ```text
   https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/callback
   ```

4. Confirm that the app can read the authenticated user's confirmed email,
   creation date, stable user ID, and username.
5. Resolve and independently verify the stable numeric X user ID for
   `@plether_fi`. Configure it as `x_target_user_id`; never derive the target
   from a mutable handle at request time.

Set `x_target_handle = "plether_fi"`. Registration accepts accounts created on
or before `2026-06-15T21:00:00Z`.

### Turnstile and Reown

Create a Cloudflare Turnstile widget restricted to
`insights.plether.com`. Its browser and server action is exactly
`competition_registration`.

Configure these GitHub repository values for **Deploy Insights**:

```text
INSIGHTS_TURNSTILE_SITE_KEY
WALLETCONNECT_PROJECT_ID
INSIGHTS_REGISTRATION_ORIGIN_TOKEN_SLOT=current
```

Authorize `https://insights.plether.com` in the Reown project. Confirm that the
registration wallet flow is restricted to Arbitrum Sepolia and that the public
RPC origin remains `https://sepolia-rollup.arbitrum.io/rpc`, matching the Pages
Worker CSP.

### Private registration material

Set `provision_insights_registration = true` while keeping
`enable_insights_registration = false`. Generate independent random values for:

- `insights_registration_origin_token`: at least 32 printable non-whitespace
  ASCII characters. Store the identical value in Terraform/SSM and the GitHub
  Actions secret `INSIGHTS_REGISTRATION_ORIGIN_TOKEN` used by Pages.
- `insights_registration_origin_token_next`: leave empty normally. During a
  controlled rotation, set it to a distinct token and mirror it in the optional
  GitHub Actions secret `INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT`.
- `turnstile_secret_key`.
- `x_oauth_client_secret`.
- `insights_registration_email_keys`: a versioned AES-256-GCM keyring such as
  `{ v1 = "<32-byte-base64-key>" }`.
- `insights_registration_email_hmac_key_base64`: a separate 32-byte key used
  for normalized-email uniqueness.

Configure the non-secret X client ID and exact callback, Turnstile hostname and
action, X target ID and handle, session/rate limits, and these immutable consent
versions:

```hcl
insights_registration_rules_version   = "2026-09-13"
insights_registration_privacy_version = "2026-09-13"
```

Do not place an OAuth secret, Turnstile secret, email key, HMAC key, origin
token, email, X ID, OAuth token, signature, or owner-to-Trading-Account mapping
in a `VITE_*` variable, workflow output, log, public case reason, or launch
artifact. The email-encryption and HMAC SSM parameters have deletion protection;
retain encrypted-email keys for as long as any row or retained backup needs
them.

## Validation before deployment

Authenticate the named AWS profile and validate the exact Terraform plan:

```sh
aws --profile plether sts get-caller-identity
cd infra/terraform
AWS_PROFILE=plether terraform fmt -check -recursive
AWS_PROFILE=plether terraform validate
AWS_PROFILE=plether terraform plan -var-file=<sepolia-private.tfvars>
```

The plan must show `INSIGHTS_REGISTRATION_PROVISIONED=true` and
`INSIGHTS_REGISTRATION_ENABLED=false` on the API, registration secrets attached
despite activation being false, the September release manifest on the API and
both snapshot-worker topologies, and no secret values in plain ECS environment
variables. It must also show encrypted RDS storage, the pinned RDS CA, and no
database destroy or replacement. If the existing database is unencrypted, stop
here and execute the separately reviewed encrypted snapshot copy/restore
migration before provisioning registration.

Run the backend suite and the Insights lint, tests, and production build. The
Insights worker tests are a launch gate because they cover canonical-host
enforcement, credentialed cache bypass, origin-header replacement, cookies,
CSRF, callback redirects, and no-store behavior.

Before each workflow dispatch, run:

```sh
gh auth status
gh api user --jq .login
gh api repos/Plether-Fi/plether-app/commits/master --jq .sha
gh run list --repo Plether-Fi/plether-app --branch master --limit 20
```

Confirm that `master` contains the reviewed release and that no unintended run
has already deployed that commit.

## Two-phase deployment and opening

### Phase 1: deploy with registration disabled

1. Apply the reviewed Sepolia Terraform plan with
   `enable_insights_registration = false`. This provisions SSM values and
   registers the API, indexer, and snapshot-worker task definitions. The
   provisioned API initializes registration schema/routes and cleanup without
   recording the open timestamp.
2. Dispatch the backend deployment from `master`:

   ```sh
   gh workflow run deploy-backend.yml \
     --repo Plether-Fi/plether-app \
     --ref master \
     -f environment=sepolia \
     -f bootstrap=false
   ```

3. Capture the run ID, verify its `headSha`, and wait for success:

   ```sh
   gh run view <run-id> \
     --repo Plether-Fi/plether-app \
     --json event,headBranch,headSha,status,conclusion,url
   gh run watch <run-id> \
     --repo Plether-Fi/plether-app \
     --exit-status
   ```

4. Verify that the API, Perps history indexer, and selected snapshot-worker
   topology are healthy. The indexer must retain activity from the Order
   Router, Margin Clearinghouse, and official mock-USDC asset from the reviewed
   start block.
5. Dispatch **Deploy Insights** from the same reviewed `master` commit and
   monitor it to success with `gh run view` and `gh run watch`:

   ```sh
   gh workflow run deploy-insights.yml \
     --repo Plether-Fi/plether-app \
     --ref master
   ```

6. Confirm that the canonical page
   `https://insights.plether.com/competitions/testnet-trading-2026-09/register`
   loads, its CSP permits Turnstile/Reown and Arbitrum Sepolia RPC only, and the
   public endpoints below return the September competition while registration
   remains unavailable:

   ```text
   GET https://insights.plether.com/api/insights/v1/competitions/current
   GET https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/leaderboard
   GET https://insights.plether.com/api/insights/v1/status
   ```

### Phase 2: enable registration

1. Change only `enable_insights_registration` to `true`, review the Terraform
   plan, and apply it. The preconditions fail closed unless the canonical X
   callback, complete release manifest, and provisioned credentials agree.
2. Intentionally dispatch **Deploy Backend** a second time for the same reviewed
   commit with `bootstrap=false`. This deploys the enabled task-definition
   revision. Record it as the planned configuration rollout so it is not
   mistaken for an accidental duplicate deployment.
3. Monitor the run to success. Record the registration `opensAt` returned by
   `GET /api/insights/v1/competitions/current`; it must be the enabled rollout
   time, before the configured close, and must not change on later restarts.
4. Perform the registration smoke test immediately. After `opensAt` is
   persisted, `INSIGHTS_REGISTRATION_ENABLED=false` is **not** a pause or close
   control and rolling back to a previously disabled task definition does not
   undo activation. There is no database pause/manual-close admin command. If a
   serious defect remains after activation, follow the pre-reviewed incident
   design: block the registration namespace at the trusted canonical Pages edge
   or roll back registration-serving tasks only when that rollback is known to
   contain traffic while preserving the database window for a safe resume.
   Otherwise ship a corrective deployment. Do not improvise a database flag or
   close/finalize SQL, bypass the Pages Worker, or relax provider verification.

## Registration security smoke test

Exercise a dedicated test X identity and two dedicated owner EOAs. Do not use a
production participant identity for launch testing.

Verify all of the following:

- The Pages preview hostname rejects the registration namespace before it
  injects the edge secret. Direct calls to the public backend without the
  `X-Plether-Registration-Origin` secret also fail. A browser-supplied copy of
  that header is overwritten by Pages.
- A request sent to the canonical URL with an `Origin` from a sibling Plether
  site is rejected. An Origin-less session GET is accepted only with
  `Sec-Fetch-Site: same-origin` or a same-origin `Referer`; Pages then supplies
  the canonical Origin upstream. The X callback is the only cross-site/no-Origin
  exception.
- A valid Turnstile response for hostname `insights.plether.com` and action
  `competition_registration` creates the session. Reuse, wrong hostname,
  wrong action, and expired responses fail.
- The response sets `__Host-plether_registration` with `Secure`, `HttpOnly`,
  `SameSite=Lax`, `Path=/`, and no `Domain`. State-changing requests require
  the returned `X-Registration-CSRF` value and exact canonical `Origin`.
- Every registration response, including session status and X callback, has
  `Cache-Control: private, no-store`, `Pragma: no-cache`, and
  `Referrer-Policy: no-referrer`. Public requests carrying Cookie,
  Authorization, or Range do not hit the anonymous edge cache.
- `POST .../registrations/x/authorize` returns a JSON `authorizationUrl` on the
  fixed `https://x.com/i/oauth2/authorize` endpoint. It does not return a
  redirect. The callback returns only a `303` to
  `https://insights.plether.com/competitions/testnet-trading-2026-09/register`,
  with no provider query or fragment, while preserving rotated/cleared cookies.
- X rejects an unconfirmed email and an account newer than
  `2026-06-15T21:00:00Z`. After the participant follows `@plether_fi` directly
  on X, the read-only verification action confirms `following` without a
  pending follow request for the configured numeric target ID and deletes the
  temporary OAuth token.
- Wallet verification switches to Arbitrum Sepolia, rejects a replayed,
  expired, wrong-origin, or wrong-signer challenge, and derives the expected
  index-0 Plether Trading Account.
- An owner whose derived Trading Account is deployed or has release activity is
  rejected with the instruction to use another owner wallet. A clean EOA
  completes once, is idempotent on retry, and appears as `pending` under its
  public `@username` alias.
- The public competition, leaderboard, wallet, and activity responses contain
  no email, stable X ID, OAuth token, signature, owner EOA, or owner-to-Trading-
  Account mapping. Logs and tracing contain none of those values either.

## Registration-window operations

Monitor provider error rates, X credit balance, Turnstile failures, rate-limit
responses, indexer lag, registration completion count, and snapshot health.
Never identify whether a duplicate was an email, X ID, owner EOA, or Trading
Account; the public response must remain generic.

Completed first-party registrations insert their Trading Account into the
participant roster with `pending` eligibility. Manual `register` is disabled by
the database for this competition and must not be used as a fallback. The
`Insights Admin` workflow remains available for explicit-slug list, review,
legacy break-glass remap, and finalize operations. The September first-party
roster must never use `stage-wallet-remap`, `stage-trading-account-remap`, or
`apply-wallet-remaps`: those legacy operations can corrupt the private verified
owner-to-Trading-Account binding and are rejected by both the workflow and the
backend for registration-configured competitions. Require the participant to
repeat wallet verification with an undeployed owner instead. Keep private
evidence in the restricted case record; `PUBLIC_REASON` is returned by the
public API.

### Late registration and snapshot rebuilding

The baseline is the single canonical block immediately before
`2026-09-13T21:00:00Z`. Once trading has started, every newly completed
registration enlarges the roster and makes the prior baseline/live batch
incomplete. The worker must publish a replacement batch for the complete roster;
capturing only the new wallet is not acceptable.

After every late registration, monitor `GET /api/insights/v1/status` until:

- `participantCount`, `snapshottedWalletCount`, and `startSnapshotCount` all
  equal the enlarged roster count;
- `startSnapshotsComplete` is `true`;
- the latest snapshot batch is from the reviewed Account Lens and a canonical,
  sufficiently confirmed block; and
- the leaderboard uses that complete batch and reports no funding allocation as
  profit.

Pause investigation/finalization if the counts diverge, the worker or indexer
stops advancing, or the baseline batch is not rebuilt across the entire roster.

### September 20 roster lock

At `2026-09-20T21:00:00Z`, verify the public registration metadata changes to
`closed`. A session creation or completion submitted at that exact timestamp or
later must return the stable closed-registration error, even for a session that
started earlier.

Record the locked participant count and verify that it does not increase after
close. Wait for any pre-close completed registration to trigger its full-roster
baseline/live rebuild, then require `snapshottedWalletCount` and
`startSnapshotCount` to equal the locked count with `startSnapshotsComplete=true`.
Keep this roster-lock evidence with the launch record.

## Secret maintenance and retention

Expired sessions, OAuth challenges/tokens, wallet challenges, signatures, and
uncompleted identity details are cleanup targets. Registration initialization
invokes the database cleanup path; verify remaining expired-row counts through a
restricted database audit after each deployment, and run approved maintenance
if normal restarts do not provide the required cleanup cadence. Do not delete
completed application email data: the privacy notice commits to indefinite
encrypted retention.

Rotate the Pages-to-backend origin token without an outage:

1. Generate token B independently. Keep token A as
   `insights_registration_origin_token`; set token B as
   `insights_registration_origin_token_next` and the GitHub secret
   `INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT`. Keep the repository variable
   `INSIGHTS_REGISTRATION_ORIGIN_TOKEN_SLOT=current`.
2. Apply Terraform and deploy the backend through GitHub Actions. Verify the
   API accepts both A and B before changing the edge sender.
3. Persistently switch the sender and deploy Insights:

   ```sh
   gh variable set INSIGHTS_REGISTRATION_ORIGIN_TOKEN_SLOT \
     --repo Plether-Fi/plether-app \
     --body next
   gh workflow run deploy-insights.yml \
     --repo Plether-Fi/plether-app \
     --ref master
   ```

   Monitor the workflow and verify canonical registration requests use B. The
   repository variable is persistent, so a later push cannot silently switch
   Pages back to A during the overlap.
4. Promote B in Terraform by setting it as
   `insights_registration_origin_token` and clearing
   `insights_registration_origin_token_next`. Apply and deploy the backend.
   Existing API tasks accept B as the overlap token until the promoted task is
   healthy, so Pages remains authorized throughout.
5. Replace the protected GitHub secret
   `INSIGHTS_REGISTRATION_ORIGIN_TOKEN` with B, set the slot back to `current`,
   and deploy Insights once more. Verify registration, then delete the retired
   `INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT` GitHub secret. The Terraform apply
   retires its separate `insights-registration-origin-token-next` SecureString.

At every stage, current and next must be valid and distinct. Never overwrite A
before a backend deployment accepts B, never clear the overlap before Pages is
sending B, and never paste either token into a command argument or log.

Rotate the AES email key without losing decryptability. Each operator command is
submitted through the one-time SecureString request used by the `Insights Admin`
workflow. Its allowlisted request shapes are:

```json
{"requestId":"<id>","args":["registration-key-preflight","v1"]}
{"requestId":"<id>","args":["rotate-registration-email-key","v1","<positive-exact-email-count>"]}
```

Dispatch the matching workflow action only after storing the request at
`/plether/sepolia/insights-admin/requests/<id>`:

```sh
gh workflow run insights-admin.yml \
  --repo Plether-Fi/plether-app \
  --ref master \
  -f action=registration-key-preflight \
  -f request_id=<id> \
  -f confirmation="RUN <id> ON SEPOLIA"

gh workflow run insights-admin.yml \
  --repo Plether-Fi/plether-app \
  --ref master \
  -f action=rotate-registration-email-key \
  -f request_id=<rotation-id> \
  -f confirmation="RUN <rotation-id> ON SEPOLIA"
```

Use this three-rollout sequence:

1. **Distribute:** add a newly generated `v2` to
   `insights_registration_email_keys`, keep `v1` active, apply Terraform, deploy
   the backend, and wait for the API service to reach steady state. Run
   `registration-key-preflight v1`; it reports count-only references for email,
   stable X ID, temporary X access, CSRF, PKCE, wallet message, and their total.
2. **Switch and migrate:** keep both keys, set
   `insights_registration_email_key_version = "v2"`, apply, deploy, and wait for
   steady state before migrating anything. Run the `v1` preflight again and use
   its exact positive `email_references` as `EXPECTED_COUNT` for
   `rotate-registration-email-key v1 EXPECTED_COUNT`. The command performs
   authenticated decrypt/re-encrypt in bounded compare-and-set batches and
   fails if the preflight count changed; rerun preflight before any retry. If
   `email_references=0`, do not invoke the positive-count rotation command.
3. Wait at least the configured session TTL plus the bounded cleanup cadence,
   then run `registration-key-preflight v1` until **all six fields and
   `total_references` are zero**. Investigate rather than removing the key while
   any live envelope still cites `v1`. Perform a sampled application decrypt
   check without logging plaintext.
4. **Retire:** only after the zero-reference preflight, remove `v1` from the
   runtime keyring, apply, deploy, wait for steady state, and rerun the preflight.
   Keep `v1` in the restricted recovery escrow until every retained backup that
   contains `v1` ciphertext has expired or has a documented restoration path.

The HMAC key backs normalized-email uniqueness. Do not rotate it as part of AES
rotation. Rotating it requires a separately reviewed offline digest migration
that preserves uniqueness transactionally.

After registration closes and temporary-state retention has elapsed, verify
that expired sessions, one-time challenges, OAuth material, signatures, and
rate-limit rows are gone. Never remove the encrypted completed emails, consent
versions, public handles, or private UUID references required for audit.

## Scoring, results, and payout

The ten possible 21:00 UTC FX-session days are September 14–18 and 21–25. There
is no close-only period: opening and increasing positions remain allowed until
the half-open scoring cutoff.

1. Before trading starts, require a complete canonical baseline batch for the
   then-current roster. Accept either exactly 100,000 official mock USDC at the
   baseline or zero followed by one official 100,000 allocation before first
   trading activity. For the latter path, verify the successful faucet claim's
   persisted mint receipt block is strictly before the exact clearinghouse
   deposit and that the deposit precedes the first trade.
2. During the competition, keep the history indexer and snapshot worker healthy.
   The leaderboard snapshot must never be ahead of the published indexed block.
   Weekend executions do not create an active day.
3. After `2026-09-25T21:00:00Z`, wait for the confirmation-delayed canonical
   final block and require `finalSnapshotCount` to equal the locked participant
   count with `finalSnapshotsComplete=true`.
4. Review fixed-bankroll compliance, excess or unofficial capital, duplicate
   control, wash activity, substantially mirrored accounts, and circular
   funding. Mark every account `eligible` or `ineligible`; `pending` and
   `under_review` block finalization.
5. At or after `2026-09-28T12:00:00Z`, run `finalize` with the explicit slug. It
   fails closed unless boundary blocks, private trader references, reviews, and
   a complete canonical final snapshot batch are present.
6. Export the eligibility-aware 600/300/100 USDC prize allocation. Entrants must
   meet the +1% threshold and five-active-day requirement. Exact P&L ties split
   the occupied prize pool equally at six-decimal USDC precision.
7. Pay real USDC no later than `2026-10-03T00:00:00Z` and retain transaction
   hashes in the restricted payout record.

Competition identity, schedule, release manifest, FX-session boundary, scoring
version, and prize values are immutable after seeding. A mismatch stops startup;
create a new versioned competition slug instead of rewriting historical results.
