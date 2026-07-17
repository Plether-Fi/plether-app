# Plether Insights launch and operations runbook

This runbook covers the July 2026 Arbitrum Sepolia competition. The canonical
rules and accounting semantics live in `insights-testnet-competition.md`.

## Before launch

1. Protect the GitHub environment `insights-sepolia-admin` with required
   reviewers. Limit access to the AWS and Cloudflare deployment credentials.
2. Confirm the Sepolia Terraform values for the Order Router, Margin
   Clearinghouse, official mock USDC, Account Lens, indexer start block, and
   `consolidate_workers` mode.
3. Prepare one opaque `TRADER_REFERENCE` per beneficial trader. Do not use an
   email address, wallet, or other directly identifying value. Keep the private
   registration-to-person mapping outside public application data.
4. Verify every registered Plether Trading Account is flat, has no pending
   orders, and has exactly the official 100,000 mock-USDC allocation before the
   baseline block.

## Deployment order

1. Run all backend and Insights frontend checks.
2. Apply the Sepolia Terraform configuration. This creates or revises the
   Insights worker task definition and, in non-consolidated mode, its service.
3. Manually dispatch **Deploy Backend** with `environment=sepolia`. The workflow
   deploys the newest Terraform task-definition revision, so this step must
   follow the Terraform apply.
4. Verify the Perps history indexer and Insights worker are running. The history
   indexer must retain both the event contract and official-USDC asset for every
   scored deposit/withdrawal. If provenance was not retained by an older
   deployment, reindex from before the competition start; unverifiable legacy
   flows are deliberately excluded from scoring.
5. Register the final roster using the private admin workflow below.
6. Verify `GET /api/insights/v1/status` over the public HTTPS Sepolia backend.
7. Manually dispatch **Deploy Insights**. Its preflight refuses a non-HTTPS
   origin and will not publish Pages until the status endpoint is healthy.

The required public endpoint checks are:

```text
GET /api/insights/v1/competitions/current
GET /api/insights/v1/competitions/testnet-trading-2026/leaderboard
GET /api/insights/v1/status
```

## Private admin requests

The `Insights Admin` workflow accepts only a public action, opaque request ID,
and explicit confirmation phrase. Command arguments live in a one-time SSM
SecureString, are masked in GitHub Actions, and are deleted only after the ECS
task succeeds.

Create a local mode-0600 JSON file with one of these allowlisted payloads:

```json
{"requestId":"register-001","args":["register","opaque-trader-001","0xTRADING_ACCOUNT","Public alias"]}
{"requestId":"review-001","args":["review","0xTRADING_ACCOUNT","eligible","reviewer-name"]}
{"requestId":"review-002","args":["review","0xTRADING_ACCOUNT","ineligible","reviewer-name","Generic public reason"]}
{"requestId":"list-001","args":["list"]}
{"requestId":"finalize-001","args":["finalize","reviewer-name"]}
```

Upload it without placing the payload directly in shell history:

```sh
aws ssm put-parameter \
  --name /plether/sepolia/insights-admin/requests/register-001 \
  --type SecureString \
  --value file://request.json
```

Dispatch `Insights Admin` with the matching action and request ID, then enter
`RUN register-001 ON SEPOLIA` as the confirmation. A failed request remains in
SSM for private inspection or retry. Never put investigation evidence in
`PUBLIC_REASON`; it is returned by the public API. Keep private review evidence
in the restricted case record.

## Competition checks

- Before opening: participant count matches the signed-off roster.
- After `2026-07-19T22:00:00Z`: `startSnapshotsComplete` is true and all
  registered wallets share the published baseline batch.
- While live: the snapshot worker stays current and leaderboard `meta.blockNumber`
  never exceeds the published snapshot batch.
- On weekends: the protocol is closed and weekend executions do not create an
  active day.
- After `2026-07-31T22:00:00Z`: wait for the confirmation-delayed canonical
  final block and require `finalSnapshotsComplete`.

## Review, results, and payout

1. Review fixed-bankroll compliance, duplicate control, wash activity,
   substantially mirrored accounts, and circular funding. Mark every account
   either `eligible` or `ineligible`; `pending` and `under_review` block finalization.
2. At or after `2026-08-02T12:00:00Z`, run `finalize`. It fails closed unless
   boundary blocks, private trader references, reviews, and one complete
   canonical final snapshot batch are present.
3. Export the eligibility-aware prize allocation. Exact P&L ties split the
   occupied prize pool equally at six-decimal USDC precision.
4. Pay real USDC no later than `2026-08-07T22:00:00Z` and retain transaction
   hashes in the restricted payout record.

Competition identity, schedule, contract addresses, scoring version, and prize
values are immutable after seeding. A mismatch stops startup; create a new
versioned competition slug instead of rewriting historical results.
