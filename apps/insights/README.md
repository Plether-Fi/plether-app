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

The UI reads `/divisions/current.json`. It never uses September's cumulative
P&L or 100,000-USDC baseline to calculate weekly Divisions results. Before a
publication exists, it shows the live traded-wallet roster without a ranking
placeholder or unpublished badges.

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

**Integration boundary:** the ranking UI and snapshot-input engine do not collect
new weekly snapshots from the existing Haskell indexer. Its September snapshot
worker and participant roster cannot be used as a Divisions feed. A complete
weekly exporter for all traded accounts is required to supply production inputs.
Discord account and wallet linking is implemented separately. Role assignment
and posting to X/Discord are deferred; the engine only prepares announcement data.

The publisher refuses premature/future publication, skipped weeks, duplicate
wallets, missing snapshots, rewriting a published week's results, and
replacement of an unexpected current week. It writes an immutable weekly
archive and atomically replaces `current.json`; exact retries are idempotent.
Run one publisher at a time. Publish the resulting assets through the normal
Insights deployment; generating files alone does not deploy them. No sample or
synthetic standings are included in production assets.

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
`VITE_WALLETCONNECT_PROJECT_ID` build variables. The optional
`VITE_ARBITRUM_SEPOLIA_RPC_URL` defaults to the public Arbitrum Sepolia RPC.
These values are browser-visible identifiers and URLs; backend Turnstile, X,
encryption, and edge-origin credentials must never use a `VITE_` prefix.
Local Vite development uses Cloudflare's documented always-pass Turnstile test
site key and the trading frontend's public WalletConnect project ID when those
variables are absent. Production builds do not use the Turnstile fallback, and
the deployment workflow requires both variables explicitly.

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
