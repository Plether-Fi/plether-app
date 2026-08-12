# Repository Agent Guidance

## Deployments

Use the GitHub CLI for every GitHub Actions deployment operation, including authentication checks, commit verification, duplicate-run checks, workflow dispatch, status monitoring, and log inspection.

Do not open or automate the GitHub Actions web UI to inspect, start, or monitor deployments. A browser may be used only when `gh auth login --web` launches GitHub's interactive authentication because the CLI session is missing or expired. After authentication, return to `gh` for all deployment work.

Before dispatching a deployment:

1. Run `gh auth status`.
2. Confirm API access with `gh api user --jq .login`.
3. Verify the intended remote commit, for example:

   ```bash
   gh api repos/Plether-Fi/plether-app/commits/master --jq .sha
   ```

4. Check recent runs with `gh run list` so the same commit and environment are not deployed twice unintentionally.

### Authentication checks in restricted sandboxes

`gh auth status` performs remote validation and can misleadingly report that a token is invalid when a restricted sandbox cannot reach `api.github.com`. A connection, DNS, or timeout error from `gh api user` indicates unavailable network access, not expired credentials.

Never request reauthentication based only on a failed in-sandbox `gh auth status` result. If either authentication check reports a connectivity problem, rerun both read-only checks with network access outside the restricted sandbox:

```bash
gh auth status
gh api user --jq .login
```

Request or run `gh auth login` only when the network-enabled checks reach GitHub and return an actual authentication failure such as HTTP 401 or `Bad credentials`. Do not request login for connection failures.

If the network-enabled checks confirm that authentication is genuinely unavailable, use:

```bash
gh auth login --hostname github.com --git-protocol ssh --web --scopes workflow
```

### Sepolia

A push to `master` targets mainnet in the deployment workflows. Sepolia therefore requires explicit manual workflow dispatches from `master`.

Deploy the backend with:

```bash
gh workflow run deploy-backend.yml \
  --repo Plether-Fi/plether-app \
  --ref master \
  -f environment=sepolia \
  -f bootstrap=false
```

Never use `bootstrap=true` for a normal backend deployment; that mode builds images without deploying the services.

Deploy the frontend with:

```bash
gh workflow run deploy-frontend.yml \
  --repo Plether-Fi/plether-app \
  --ref master \
  -f environment=sepolia
```

Capture the run URL or ID printed by each dispatch, verify its `headSha`, and monitor it to a terminal result:

```bash
gh run view <run-id> \
  --repo Plether-Fi/plether-app \
  --json event,headBranch,headSha,status,conclusion,url

gh run watch <run-id> \
  --repo Plether-Fi/plether-app \
  --exit-status
```

Deploy the backend first when ordering matters. Do not report completion until the workflows succeed and the relevant public endpoints pass smoke checks.
