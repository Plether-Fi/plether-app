# Plether Insights — Divisions

The homepage is **Plether Divisions**, a weekly testnet ranking program launching
12 October 2026. The first trading window is Sunday 11 October at 21:00 UTC
through Friday 16 October at 21:00 UTC; first badges publish Monday 19 October.

Weekly returns use cash-flow-adjusted equity, after incurred fees. Two distinct
active FX-session days and strictly positive profit qualify a wallet. Earned
tiers persist across weeks, with one-step promotions and one-step demotions after
two consecutive non-qualifying weeks. The complete rules are in
[`specs/plether-divisions.md`](../../specs/plether-divisions.md).

## Ranking and publication

`src/divisions/engine.ts` is a pure, deterministic weekly ranking engine. It
consumes complete account snapshots, includes inactive previously ranked wallets,
and emits tier state, standings, and weekly announcement data. It uses exact
integer ratios to rank percentage returns, without floating-point rounding.

The home page shows live rankings, current awarded badges, and the total number
of wallets registered with Discord. `/api/insights/v1/divisions/stats` returns
`registeredWallets`, counting completed Discord/wallet links across all releases,
including non-traders. Pending OAuth or wallet-verification sessions do not count.
The counter refreshes every minute and remains available before weekly rankings. Completed results live
on `/results` (Last week’s results), with older/newer navigation using stable week-start
cursors in the URL. `/api/insights/v1/divisions/history?week=<epoch-seconds>` returns
one saved publication plus `olderCursor` and `newerCursor`; omitting `week` selects
the latest published week. Empty published weeks remain browsable.

Each week has one persisted contract assignment in `divisions_weeks`, keyed by
chain and week. October 11, 18 and 25 use Core v1.3.1 (mock USDC); November 1
onward uses Core v2.0.0 (USDG). Append future cutovers only at week boundaries;
startup rejects any change to an existing assignment. Results show the assigned
version as read-only text. The API ignores legacy `releaseRouter` selectors;
old frontend bookmarks redirect to the same week without the selector.

History and current badges use only publications matching that week's assignment.
Week navigation crosses contract upgrades, and earned tiers continue for the same
wallet. Each week's balances, funding and trades come solely from its assigned
release. Publication captures all prior badge holders as well as the new release's
traded roster, so inactivity and promotion rules continue without inventing scores.
Alternative-release publications remain untouched for audit and never count twice.
The API normalizes historical `weeklyPnlUsdc` / `adjustedEquityUsdc` fields to
`*Assets` when reading and includes `scoringRelease` metadata for the correct unit.
Deploy the backend before the Insights frontend; startup creates and seeds the
weekly assignment table before serving requests.

This exposes existing publications; it does not rebuild missing weeks from a
retired release. Before switching contracts, verify that its final completed
week has been published, or arrange a replay with that release's original
contract manifest, finalized indexed history and archive RPC access. Never copy
or relabel a new deployment's empty weeks as the previous deployment's results.

To check whether a retired release already has the needed publication, use the
existing restricted database access with a read-only transaction. This lists
publication coverage and trader counts without exposing account balances:

```sql
BEGIN READ ONLY;
SELECT release_router, publication->'week'->>'startsAt' AS starts_at,
       publication->'week'->>'closesAt' AS closes_at,
       jsonb_array_length(publication->'standings') AS traders,
       publication->>'publishedAt' AS published_at
FROM divisions_publications
WHERE chain_id = 421614
ORDER BY week_start DESC, release_router;
ROLLBACK;
```

The UI reads `/api/insights/v1/divisions/current` from the backend. The Insights
worker captures finalized boundary values for every traded account, checks
external funding against indexed settlement-token transfers, and appends each
completed week to `divisions_publications`. It backfills from the first week in order,
including empty weeks, one week per cycle. Publication starts Monday at 00:00
UTC; failed captures leave the previous awards intact and retry next cycle.
Awards persist across deployments and never expire with live snapshot freshness.
Every badge, including Iron, is awarded only in a completed week’s publication.
New traders show “Not yet awarded” until included in published results; returning
traders keep their last awarded tier throughout the active week. Current-week
returns remain provisional and never grant a badge.

For `depositFor`, the worker verifies the canonical receipt's consecutive
`Transfer`, `Deposit`, and `DepositFor` logs to identify the actual payer. It
caches that proof in `divisions_deposit_payers`, scoped to the exact release,
transaction, block hash, beneficiary, amount, and log positions, and still
requires the matching indexed token transfer. The beneficiary's deposit counts
once as external funding; it is not trading profit. Existing indexed deposits
are repaired lazily, without replaying trade history. Worker/API startup creates
the additive proof table. Deploy the worker to resume live captures; invalid or
unavailable evidence continues to block publication.

The API adds current registered Discord handles to published display names.
The backend and TypeScript engines share multi-week regression fixtures.

The standalone publisher remains available for offline replay and audit:

Use Node **22.18+** to run the publisher (native TypeScript support):

```sh
# First week
npm run divisions:publish -- /path/to/week-2026-10-11.json public/divisions

# Next week: supply the immediately preceding publication
npm run divisions:publish -- /path/to/week-2026-10-18.json public/divisions \
  public/divisions/weeks/2026-10-11.json
```

The input shape is `WeeklyInput` in `src/divisions/types.ts`. Every wallet that has
traded must be supplied each week, whether active or inactive. A wallet with no
trading history (`hasTraded: false`) is excluded. Amounts are six-decimal USDC
integer strings. Supply full opening and closing economic values, verified
external deposits/withdrawals, and timestamps of successfully executed nonzero
voluntary position changes. The exporter must verify event emitter, asset,
canonicality, finalized boundary blocks, and complete wallet coverage. Retain
source inputs and their block hashes privately for replay/audit.

The production worker uses the Divisions all-account feed, independent of the
September competition roster. It retains exact adjusted equity, P&L and canonical
boundary hashes in immutable weekly publications. Missing prior wallets, invalid
funding, failed account reads, premature publication and skipped weeks prevent
publication. Existing weeks are never overwritten, including during rolling
deployments. Canonical-history corrections require an explicit correction process.

The standalone file publisher enforces the same sequencing and progression rules
and writes an immutable weekly archive. Its files are not the production data
source. Discord role assignment and posting remain separate from badge display.

## September archive

September's historical standings, wallet breakdowns, and approved fee-share
exceptions remain available at `/archive/september-2026`, with their original
rules at `/archive/september-2026/methodology`. Its registration route redirects
to the archive. The new `/register` route links a Discord account to one owner
wallet for future Divisions roles. Linking does not gate rankings or assign a
role yet. Historical API and backend records are preserved. Divisions does not
reuse competition cash-prize eligibility.

## Local development

```sh
npm ci
npm run dev:registration # separate terminal for the local registration API
npm run dev
```

Vite proxies `/api/insights/v1` to `VITE_API_PROXY_TARGET` (default `http://127.0.0.1:3001`) without rewriting the path.
Registration requests are routed first to the local test backend at
`VITE_REGISTRATION_API_PROXY_TARGET` (default `http://127.0.0.1:3003`). This
development-only service mocks X, Discord, and chain activity checks, but still creates
an HttpOnly session with CSRF protection and verifies the connected wallet's
signed challenge. It is not used by production builds or the Pages Worker.

Registration also requires the public `VITE_TURNSTILE_SITE_KEY` and
`VITE_PRIVY_APP_ID` build variables. The optional `VITE_PRIVY_CLIENT_ID`
selects the Insights app client. The optional
`VITE_ARBITRUM_SEPOLIA_RPC_URL` defaults to the public Arbitrum Sepolia RPC.
These values are browser-visible identifiers and URLs; backend Turnstile, X,
encryption, edge-origin, Privy app, and OAuth provider secrets must never use a
`VITE_` prefix.
Local Vite development uses Cloudflare's documented always-pass Turnstile test
site key when that variable is absent. There is no fallback Privy app ID: wallet
connection is disabled until a real app ID is configured, while public pages
remain available. Production builds do not use the Turnstile fallback, and the
deployment workflow requires both public identifiers explicitly.

Use the same Privy application as the Arbitrum Sepolia trading frontend, even
though this site is deployed as production. Configure the exact
`https://insights.plether.com` and `https://app.sepolia.plether.com` origins in
Privy, enable the displayed login methods, and configure their OAuth callbacks.
The same email login must resolve the same embedded owner address in both apps;
the browser may require login on each origin. Separate app client IDs can scope
each origin. Use a separate shared development application for local tests.
External wallets retain connection-only access; the registration wallet step
still verifies its one-time owner signature. Privy login does not replace the
competition's X verification, eligibility checks, HttpOnly session, or CSRF rules.
Reown sessions and embedded-wallet keys are not migrated.

`npm test` includes wallet integration tests using the installed Privy/Wagmi
bridge, actual Wagmi connectors, and recoverable test signatures. They cover
same-extension owner changes, session restoration, email completion/cancellation,
unavailable owners, and the registration challenge/signature handoff. Remote
Privy authentication and registration API responses are fixtures; the suite
does not deliver live email OTPs or submit a production registration.

The connection picker always offers **Log in with Email** or, when already
authenticated, **Use email wallet**. Its close action uses the pinned UI-only
Privy extension installed by `scripts/patch-privy-wallet-dialog.mjs`; the script
verifies SDK version/file checksums and must be reviewed with any Privy upgrade.

**Deploy Insights** reads GitHub Actions variables `PRIVY_APP_ID` (shared with
the trading app) and optional `PRIVY_INSIGHTS_CLIENT_ID`. It publishes on pushes
to `master` affecting this app, so verify social/external wallet registration
locally or against an isolated preview backend before merging. A normal Pages
preview is not an authorized registration origin for the production backend.

The legacy registration components and their tests remain for historical
maintenance; the public app no longer mounts the September application flow.
The Divisions link uses the same first-party proxy and Turnstile configuration.
The backend additionally requires `INSIGHTS_DIVISIONS_REGISTRATION_ENABLED=true`,
`DISCORD_OAUTH_CLIENT_ID`, `DISCORD_OAUTH_CLIENT_SECRET`, and the exact callback
`https://insights.plether.com/api/insights/v1/divisions/registrations/discord/callback`.
Register that URI in the Discord application. Only the `identify` scope is
requested; Discord email and posting permissions are not used. Store the client
secret outside Terraform as `/plether/sepolia/discord-oauth-client-secret` in
SSM Parameter Store. Terraform keeps the new flow disabled until the application
ID, secret, and callback are provisioned.

## Validation

```sh
npm test
npm run lint
npm run build
```

## Deployment

Insights deployment is manual-only. Pushes and merges to `master` do not publish
the application. Deploy after confirming that the backend supports the API fields
used by the selected frontend release:

```sh
gh workflow run deploy-insights.yml --repo Plether-Fi/plether-app --ref master
```

Cloudflare Pages serves the SPA through `public/_worker.js`. Configure
`INSIGHTS_BACKEND_URL` as a clean HTTPS origin; HTTP, credentials, paths, query
parameters, and fragments are rejected. API requests preserve the complete
`/api/insights/v1/...` path. Successful public GET and HEAD responses are cached
at the edge for 15 seconds (leaderboards and wallets) or 30 seconds (status).
Current-competition metadata, errors, mutations, and requests carrying Cookie,
Authorization, or Range are not cached. Credentials are stripped before
anonymous public handlers. Registration is accepted only on the configured
canonical origin, authenticated to the backend with the private Pages secret,
and never cached upstream or downstream. The deployment workflow also requires
the backend status endpoint to succeed before publishing Pages.

## Manual fee-share approvals

`src/feeShareApprovals.ts` records organizer-approved exceptions for the September
competition. On October 8, 2026, Stan approved `@nightpuper` and
`@FishesReal72265` after a verification call, waiving normal fee-share conditions.
Their wallet identities were checked against the public Insights API.
The category appears on the leaderboard and approved wallet pages, with badges
on desktop and mobile standings. Matching uses competition slug and wallet, not
handles. These are fee-share decisions only: the backend's cash-prize review,
rank, and payout calculations are unchanged. This frontend registry is not an
onchain grant or an automated fee distribution mechanism; include these approved
wallets when preparing the fee-share distribution.
