# Plether Insights

Public competition standings, wallet activity, and first-party competition registration for `insights.plether.com`.

Overall rank includes every registered participant. Prize places include only
participants who meet the P&L and active-day thresholds and are marked eligible
after integrity review. Exact final-P&L ties split the combined awards for their
occupied paid places equally; wallet address only stabilizes display order.

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
`VITE_WALLETCONNECT_PROJECT_ID` build variables. The optional
`VITE_ARBITRUM_SEPOLIA_RPC_URL` defaults to the public Arbitrum Sepolia RPC.
These values are browser-visible identifiers and URLs; backend Turnstile, X,
encryption, and edge-origin credentials must never use a `VITE_` prefix.
Local Vite development uses Cloudflare's documented always-pass Turnstile test
site key and the trading frontend's public WalletConnect project ID when those
variables are absent. Production builds do not use the Turnstile fallback, and
the deployment workflow requires both variables explicitly.

The registration route is `/competitions/testnet-trading-2026-09/register`
(`/register` redirects to the current competition). It resumes through the
secure HttpOnly browser session established by the API.

When testing against a deployed backend that predates the Protocol Explorer,
set `VITE_PROTOCOL_EXPLORER_LEGACY_FALLBACK=true`. In local development only,
a missing current-release bootstrap endpoint then falls back to the competition
instead of presenting the backend's 404 as an Explorer configuration failure.
Other bootstrap failures remain visible and retryable.

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
