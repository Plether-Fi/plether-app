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
{"requestId":"register-bulk-001","bulkRosterChunkCount":12,"args":["register","bulk-append-alias-owner-roster","879","700","register-bulk-001"]}
{"requestId":"remap-001","args":["stage-wallet-remap","opaque-trader-001","0xREGISTERED_ADDRESS","0xTRADING_ACCOUNT"]}
{"requestId":"derive-remap-001","args":["stage-trading-account-remap","opaque-trader-001","0xOWNER_WALLET"]}
{"requestId":"alias-remap-batch-001","args":["stage-alias-owner-remaps","@first_alias","0xREGISTERED_ADDRESS_1","0xOWNER_WALLET_1","@second_alias","0xREGISTERED_ADDRESS_2","0xOWNER_WALLET_2"]}
{"requestId":"apply-remaps-001","args":["apply-wallet-remaps","879","reviewer-name"]}
{"requestId":"review-001","args":["review","0xTRADING_ACCOUNT","eligible","reviewer-name"]}
{"requestId":"review-002","args":["review","0xTRADING_ACCOUNT","ineligible","reviewer-name","Generic public reason"]}
{"requestId":"list-001","args":["list"]}
{"requestId":"verify-roster-001","args":["list","verify-roster-correction","1579"]}
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

For more than a few approved roster corrections, use one atomic bulk append
instead of dispatching one registration per participant. Build a private TSV
with exactly one case-insensitive unique alias per line:

```text
@alias<TAB>UUIDv4 trader reference<TAB>0xOWNER_WALLET
```

Aliases, UUIDv4 references, and locally derived Trading Account destinations
must each be unique. Duplicate or ambiguous aliases must be resolved before
upload. The admin task derives every destination with the pinned canonical
SimpleAccount deployment; owner wallets never enter participant storage or the
RPC snapshot process.

Gzip and base64 the TSV, split the encoded text into chunks no larger than the
configured SSM parameter limit, and upload every chunk as a one-time
`SecureString` named:

```text
/plether/sepolia/insights-admin/requests/register-bulk-001.bulk-roster-01
/plether/sepolia/insights-admin/requests/register-bulk-001.bulk-roster-02
...
```

Set `bulkRosterChunkCount` in the request JSON to the exact number of chunks.
The second numeric argument is the number of TSV entries; the first is the
current participant-count guard. Dispatch with `action=register` and
`confirmation=RUN register-bulk-001 ON SEPOLIA`. The transaction preserves
existing trader references when an alias already exists, inserts new aliases,
remaps changed aliases, validates final alias/reference/destination uniqueness,
invalidates all snapshot batches once, and records an opaque roster-correction
audit. The workflow deletes the request and all chunks only after the ECS task
succeeds.

After the snapshot worker publishes the rebuilt start and live batches, run the
private `list verify-roster-correction EXPECTED_COUNT` check. It fails unless
`participantCount`, `snapshottedWalletCount`, and `startSnapshotCount` all equal
the expected final roster size and every canonical start snapshot is flat, has
zero pending orders, and does not exceed the official starting bankroll.

Wallet remapping is an atomic full-roster operation. Stage exactly one mapping
for every registered `TRADER_REFERENCE`, using an identity mapping when the
registered address is already the verified Trading Account. Resolve duplicate
destinations before applying. Stage alias-based mappings in batches of at most
20 entries; staging is idempotent, and a failed or retried batch cannot pass the
full-roster apply guard until every entry is present. `apply-wallet-remaps` fails unless its expected
count matches the participant count, every staged source still matches the
roster, and every destination is unique. Applying replaces the roster and
invalidates the old snapshot batches; keep the snapshot worker stopped until
the replacement roster is committed, then rebuild the baseline and live
batches before restoring public publication.

`stage-alias-owner-remaps` derives every destination locally using the pinned
canonical v0.8.0 SimpleAccount factory deployment. It does not send participant
owner wallets to an RPC. Update the pinned deployment artifact and known-vector
tests together if the configured factory ever changes.

## Competition checks

- Before opening: participant count matches the signed-off roster.
- After `2026-07-20T16:00:00Z`: `startSnapshotsComplete` is true and all
  registered wallets share the published baseline batch.
- While live: the snapshot worker stays current and leaderboard `meta.blockNumber`
  never exceeds the published snapshot batch.
- On weekends: the protocol is closed and weekend executions do not create an
  active day.
- After `2026-08-03T16:00:00Z`: wait for the confirmation-delayed canonical
  final block and require `finalSnapshotsComplete`.

## Review, results, and payout

1. Review fixed-bankroll compliance, duplicate control, wash activity,
   substantially mirrored accounts, and circular funding. Mark every account
   either `eligible` or `ineligible`; `pending` and `under_review` block finalization.
2. At or after `2026-08-05T12:00:00Z`, run `finalize`. It fails closed unless
   boundary blocks, private trader references, reviews, and one complete
   canonical final snapshot batch are present.
3. Export the eligibility-aware prize allocation. Exact P&L ties split the
   occupied prize pool equally at six-decimal USDC precision.
4. Pay real USDC no later than `2026-08-07T22:00:00Z` and retain transaction
   hashes in the restricted payout record.

Competition identity, schedule, contract addresses, scoring version, and prize
values are immutable after seeding. A mismatch stops startup; create a new
versioned competition slug instead of rewriting historical results.
