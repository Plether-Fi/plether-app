# Isolated Sepolia Kernel and Privy preview

The canary frontend uses the separate Cloudflare Pages project
`plether-kernel-canary` at `https://plether-kernel-canary.pages.dev`.
It is a direct-upload project with production branch `kernel-canary`.
The existing `plether-testnet` project and `app.sepolia.plether.com` are
not deployment targets for this preview. The candidate is an exact SHA
from an open `kernel-canary/*` pull request against `master`.

The browser's `/api/perps/v1/aa/kernel/rpc` is rewritten by
`_worker.js` to the exact backend path
`https://api.sepolia.plether.com/api/aa/kernel/rpc`. The ALB forwards
only that backend path to the isolated Kernel API. The Worker strips
browser-supplied `X-Plether-AA-Proxy-Token` and injects the
`KERNEL_CANARY_AA_PROXY_ORIGIN_TOKEN` Pages secret on this route once
the active canary phase binds it. Before then Kernel RPC fails closed.
Legacy AA paths continue to use `AA_PROXY_ORIGIN_TOKEN`. No global
`BACKEND_URL` binding is set. The full canary app still reads the
existing Sepolia trading readiness and can offer testnet funding, so
the isolated Pages project also receives the existing legacy AA and
faucet origin tokens. Those tokens keep their existing route scopes;
only the Kernel RPC path receives the new canary token.

## Before dispatch

1. For `stage_preview`, keep the canary ALB route closed and both
   Kernel payment admission flags off. The workflow requires the
   backend Kernel path to return 404 and does not bind the Kernel
   origin credential. Users can connect external and Privy wallets on
   the isolated site to reveal their public owner addresses. No
   Kernel operation can reach the API at this stage. Use this phase
   only before the first active canary deployment. After active use,
   rollback means disabling both admission flags and closing the
   canary ALB route; the Pages secret may remain bound.
2. Before `active_canary`, set the exact Pages origin in
   `kernel_canary_preview_origin`, the two public owner addresses in
   the canary API allowlist, and provision a unique 32-byte origin
   credential in the AWS SSM SecureString
   `/plether/sepolia/kernel-canary-aa-origin-token`. Set the same value
   as the GitHub repository secret
   `KERNEL_CANARY_AA_PROXY_ORIGIN_TOKEN` without printing it. It must
   differ from the legacy `AA_PROXY_ORIGIN_TOKEN`. Open only the exact
   canary ALB route after the signed Nitro and contract attestation
   gates pass. The active workflow proves the credential reaches the
   canary API by comparing a 403 unauthenticated control with a 400
   authenticated invalid-JSON control.
3. Confirm repository secrets `CLOUDFLARE_API_TOKEN`,
   `CLOUDFLARE_ACCOUNT_ID`, `TRADINGVIEW_GITHUB_TOKEN`,
   `AA_PROXY_ORIGIN_TOKEN`, and `FAUCET_PROXY_ORIGIN_TOKEN`
   exist. Confirm `PRIVY_APP_ID` is available to the workflow for the
   intended Privy app. `PRIVY_TRADING_CLIENT_ID` is optional and is
   passed through if configured. Add
   `https://plether-kernel-canary.pages.dev` to that app's allowed
   origins before connecting its embedded wallet in either phase.
   Confirm the required Vite RPC URL variables are available to the
   workflow, whether inherited from the organization or set on the
   repository.
4. Add only
   `.github/workflows/deploy-kernel-canary-frontend.yml` to
   `master` through review. A new `workflow_dispatch` workflow must
   exist on the default branch to be triggered. The existing
   mainnet frontend push workflow watches `apps/frontend/**` and
   `.github/workflows/deploy-frontend.yml`, so a workflow-only
   commit does not match its path filter. Do not merge the Kernel
   frontend code to `master` to create this preview.
5. Keep the candidate code in an open same-repository `kernel-canary/*` pull request.
   Review the exact head SHA and planned `deployment` ID for staging,
   then the attested `AA_KERNEL_PROFILE_JSON` ID for active canary.
   The workflow verifies the PR head SHA before build and again before
   the Pages write. The reviewer for the protected environment must
   manually review that exact SHA; no PR approval is required for the
   repository's sole-maintainer workflow.
   The protected `aa-admin-sepolia`
   environment must still require a reviewer and allow only the
   `master` dispatch ref. The build checks out the candidate SHA
   after that gate; the deploy job runs in a separate runner and
   receives the Cloudflare credential only after the build artifact
   is complete.

Run the repository's required CLI checks and dispatch from `master`:

```sh
gh auth status
gh api user --jq .login
gh api repos/Plether-Fi/plether-app/commits/master --jq .sha
gh run list --repo Plether-Fi/plether-app \
  --workflow deploy-kernel-canary-frontend.yml --limit 10
gh workflow run deploy-kernel-canary-frontend.yml \
  --repo Plether-Fi/plether-app --ref master \
  -f candidate_branch=kernel-canary/<reviewed-branch> \
  -f candidate_sha=<reviewed-40-character-head-sha> \
  -f deployment=<planned-or-attested-deployment-id> \
  -f phase=stage_preview
```

Inspect the created run with `gh run view`, verify its `headSha` is
the intended `master` dispatch commit and that the recorded candidate
SHA matches the reviewed pull request, then monitor with `gh run
watch --exit-status`. The workflow verifies the dedicated Pages
project's production branch, sets only that project's secrets and
variables, deploys the candidate artifact, and checks the Pages
origin. It does not dispatch the existing frontend workflow. Once the
two public wallet addresses and API allowlist are ready, repeat the
checks and dispatch with `-f phase=active_canary` and the exact
attested deployment ID. This phase requires the canary API route and
origin credential to pass the 403/400 controls before it binds the
Kernel Pages secret. A staged preview with a closed route is only for
wallet connection and owner discovery; it is not a transaction gate.

After the active canary deployment, check the always-available Gas sponsorship toggle in
Trading Account activity, then use separate external and Privy EOAs
for real first deployment and payer-switch transactions. Capture
fresh gas estimates before approval and actual EntryPoint receipts
with `live-gas.mjs`. Keep public admission off until those checks pass.

GitHub's default-branch requirement:
https://docs.github.com/en/actions/reference/workflows-and-actions/events-that-trigger-workflows#workflow_dispatch

Cloudflare Wrangler Pages commands:
https://developers.cloudflare.com/workers/wrangler/commands/pages/
