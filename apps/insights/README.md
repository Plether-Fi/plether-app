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
at the edge for 15 seconds (leaderboards and wallets), 30 seconds (status), or
60 seconds (current competition metadata); errors and mutating requests are not
cached. The deployment workflow also requires the backend status endpoint to
succeed before publishing Pages.
