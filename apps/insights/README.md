# Plether Insights

Read-only public competition standings and wallet activity for `insights.plether.com`.

Overall rank includes every registered participant. Prize places include only
participants who meet the P&L and active-day thresholds and are marked eligible
after integrity review. Exact final-P&L ties split the combined awards for their
occupied paid places equally; wallet address only stabilizes display order.

## Local development

```sh
npm ci
npm run dev
```

Vite proxies `/api/insights/v1` to `VITE_API_PROXY_TARGET` (default `http://127.0.0.1:3001`) without rewriting the path.

## Product analytics

Set `VITE_POSTHOG_KEY` to enable PostHog. `VITE_POSTHOG_HOST` defaults to the
EU ingestion endpoint, and `VITE_POSTHOG_REPLAY_SAMPLE_RATE` defaults to `0.05`.
The deployment workflow reuses the same GitHub secret and variables as the main
Plether frontend.

Analytics are anonymous and memory-only. Automatic event capture and automatic
pageviews are disabled. Manual events record only coarse page and interaction
categories; wallet addresses, public names, search text, P&L, balances, volume,
transaction hashes, query strings, and referrers are excluded. Sampled session
replays mask all text, input values, and element attributes.

Tracked events are `insights page viewed`, `insights leaderboard searched`,
`insights leaderboard page requested`, `insights wallet profile opened`, and
`insights outbound link opened`.

## Validation

```sh
npm test
npm run lint
npm run build
```

Cloudflare Pages serves the SPA through `public/_worker.js`. Configure
`INSIGHTS_BACKEND_URL` as a clean HTTPS origin; HTTP, credentials, paths, query
parameters, and fragments are rejected. API requests preserve the complete
`/api/insights/v1/...` path. The deployment workflow also requires the backend
status endpoint to succeed before publishing Pages.
