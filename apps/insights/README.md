# Plether Insights

Public competition standings, wallet activity, and first-party competition registration for `insights.plether.com`.

Overall rank includes every registered participant. Prize places include only
participants who meet the P&L and active-day thresholds and are marked eligible
after integrity review. Exact final-P&L ties split the combined awards for their
occupied paid places equally; wallet address only stabilizes display order.

The September leaderboard indexes **plether-core v1.2.3** on Arbitrum Sepolia,
using the backend's shared `config/perps/arbitrum-sepolia-v2.json` manifest and
deployment block **307397196**. Account activity is indexed across the release;
competition scoring starts **September 13, 2026 at 21:00 UTC** for the registered
roster, using the account state immediately before that time as its baseline.
The UI consumes the Insights API; it does not
select contracts or index the chain itself. Backend release binding requires
both the `sepolia-core-v1.2.3.tfvars.json` and
`sepolia-insights-v1.2.3.tfvars.json` Terraform overlays. See
[the operations runbook](../../specs/insights-operations-runbook.md#september-release-manifest)
for activation prerequisites, including the binding deadline.

## Local development

```sh
npm ci
npm run dev:registration
npm run dev
```

Vite proxies `/api/insights/v1` to `VITE_API_PROXY_TARGET` (default `http://127.0.0.1:3001`) without rewriting the path.
Registration requests are routed first to the local test backend at
`VITE_REGISTRATION_API_PROXY_TARGET` (default `http://127.0.0.1:3003`). This
development-only service mocks X and chain activity checks, but still creates
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

**Deploy Insights** reads GitHub Actions variables `PRIVY_APP_ID` (shared with
the trading app) and optional `PRIVY_INSIGHTS_CLIENT_ID`. It publishes on pushes
to `master` affecting this app, so verify social/external wallet registration
locally or against an isolated preview backend before merging. A normal Pages
preview is not an authorized registration origin for the production backend.

The registration route is `/competitions/testnet-trading-2026-09/register`
(`/register` redirects to the current competition). It resumes through the
secure HttpOnly browser session established by the API.

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
