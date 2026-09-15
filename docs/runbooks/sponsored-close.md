# Sponsored close on Arbitrum Sepolia

Close assistance funds only the exact missing keeper bounty, up to 200,000 USDC atomic units. It has no automatic expiry. The protocol remains v1.2.3 and its configured bounty remains unchanged. A full or partial close must otherwise be valid and existing collateral must cover accrued carry.

The release binding is `apps/backend/deployments/close-assistance-arbitrum-sepolia.json`. The lens is `0xC8Ad43019D371DEe7784C06dFa1A2F1538E0D7cf`, runtime Keccak256 `0x7cb66d1cb8f7c6748bd34150207ad1b8ead01ce8e77002dde384160ba3a1333e`. Its source is core commit `fc7d89abdedc7fb42b88467bc5ab9775cc57d98e`.

## Transaction and recovery

The owner signs one native gas-sponsored account operation: validate the exact intent/subsidy, mint to that account, approve the clearinghouse, deposit, commit the same V2 request. All calls have zero native value. Any failure reverts the whole batch. Commitment reserves the bounty and queues the close; keeper execution is separate.

The native gateway checks the five-call policy, account runtime and two independent providers. Static deployment evidence uses the existing safe security context. Live eligibility uses the same explicit recent block on both providers, with matching headers before and after the reads. The guard repeats eligibility onchain before minting.

The additive `aa_close_assistance` migration runs with existing AA startup migrations. Records link to durable sponsorship authorizations by digest. A global transaction lock allows at most one unresolved assisted intent per account. Identical authorization retries reuse the prior record. Safe failed/expired attempts may be replaced after fresh review. Successful commitments permanently consume that client intent, including when keeper execution later fails, expires or refunds a bounty.

The frontend persists amount, lens/runtime identity, paymaster and canonical request before signing. Recovery routes journaled assisted operations through the native gateway even if the current manifest uses legacy sponsorship or assistance is disabled. Do not delete grant records or switch off native submission/reconciliation when disabling new assistance.

## Release sequence

1. Verify core PR checks and the published lens deployment packet. The local fork acceptance checks actual deployed SimpleAccount code, full/partial commitment and preview parity, replay rejection and rollback.
2. Configure an independent secondary RPC for both API and reconciler; set `AA_RPC_MODE=dual-independent`. Preserve existing economics, funding budgets and account settings. Verify agreed chain, safe/latest numeric block headers and contract calls. Never use a second URL from the same provider as independent evidence.
3. Deploy backend first with issuance disabled:

   ```sh
   gh workflow run deploy-backend.yml --repo Plether-Fi/plether-app --ref master -f environment=sepolia -f bootstrap=false -f deployment_scope=all -f close_assistance_mode=disabled
   ```

4. After backend checks and schema startup pass, deploy the frontend:

   ```sh
   gh workflow run deploy-frontend.yml --repo Plether-Fi/plether-app --ref master -f environment=sepolia
   ```

5. Enable only the configured operator owners with the backend workflow, `deployment_scope=api` and `close_assistance_mode=canary`. Verify `/api/perps/v1/aa/close-assistance`, one owner signature, exact full/partial funding, keeper outcome and safe accounting evidence on operator-controlled accounts.
6. Set `close_assistance_mode=all` through the same API workflow after the canary passes. The assistance cohort is independent of the existing global gas sponsorship flag. Publish the competition explanation with this rollout.

Follow repository authentication, commit and duplicate-run checks before dispatch, verify each run's head SHA and monitor it to completion. The existing protected deployment environment remains enforced. Omit the new input or use `preserve` on unrelated deployments.

## Kill switch

Deploy the API with `close_assistance_mode=disabled`. This changes only assistance issuance flags and retains the lens binding. Ordinary closes, already-authorized native submission, keeper processing and reconciliation remain available. Do not use the global AA pause as the normal assistance-only switch.

Terraform equivalents are `perps_close_assistance_enabled`, `perps_close_assistance_global_enabled`, `perps_close_assistance_lens` and `perps_close_assistance_lens_code_hash`. Keep these consistent with the selected workflow mode to avoid later configuration drift.

## Competition accounting

Confirmed assistance is approved close-assistance funding, separate from the initial faucet allocation. Every assistance deposit remains subtracted from scored PnL. There is no reimbursement adjustment. Only exact verified deposit evidence is exempted from unexpected funding and bankroll capacity flags; a matching mint or amount alone is insufficient.

Reconciliation requires the authorized UserOperation boundary, exact mint recipient/amount, clearinghouse Deposit log, canonical newly registered request and resulting router order. Receipt log ranges separate multiple UserOperations in one transaction. Evidence is finalized at the existing safe boundary and remains tracked while issuance is disabled. Provenance also applies if the trader registers later. Competition registration, starting bankroll and scoring dates remain unchanged; assistance after the cutoff does not extend scoring.

## Monitoring

Join `aa_close_assistance` to `aa_sponsorship_authorizations` by digest, and to `aa_user_operation_events` for safe inclusion. Track grant counts/amounts, unresolved authorizations, verified grants, safe successful operations still lacking grant evidence, simulation failures, duplicate/replay denials and order outcomes. The existing AA circuit breaker pauses issuance on contradictory receipt evidence.

```sql
SELECT a.state, g.verified, COUNT(*), SUM(g.amount_usdc)
FROM aa_close_assistance g JOIN aa_sponsorship_authorizations a USING(digest)
GROUP BY a.state, g.verified;

SELECT g.account,g.client_order_id,a.user_operation_hash,g.transaction_hash,g.order_id
FROM aa_close_assistance g JOIN aa_sponsorship_authorizations a USING(digest)
LEFT JOIN aa_user_operation_events e USING(digest)
WHERE e.success AND NOT g.verified;
```

A confirmed grant is retained after any later order failure. A subsequent close requires a new intent and fresh exact funding; there is no lifetime grant-count cap.

## September 15 release evidence

Core PR #100 deployed and verified the lens above. App PRs #284, #285 and #287 implement the sponsored path, receipt boundaries and deployment checkout correction. The combined application release includes oracle recovery from #286 at `a093ce4b66992de25223639684246b1c0db9f976`.

- Backend with issuance disabled: [34956069365](https://github.com/Plether-Fi/plether-app/actions/runs/34956069365), successful.
- Sepolia frontend: [34957122917](https://github.com/Plether-Fi/plether-app/actions/runs/34957122917), successful.
- Operator-only API: [34957445713](https://github.com/Plether-Fi/plether-app/actions/runs/34957445713), successful.

Operator account `0x9314586D4068C73B23a64d7406Ca8FfEeCc2cBFc` opened 5,000 units using the real frontend runtime. Both assisted closes used one owner signature, five calls, zero native value, and identical mint/approval/deposit amounts.

| Close | Free settlement before review | Assistance | Resulting order | Commitment transaction |
| --- | ---: | ---: | ---: | --- |
| Partial, 5,000 to 2,500 units | $0.002000 | $0.198000 | 2183 | [0xe7c600…0ef0](https://sepolia.arbiscan.io/tx/0xe7c60065e75d046841993c7c24f276edfe8a9dac5ab7c45d936ed224f03a0ef0) |
| Full, 2,500 to zero units | $0.000000 | $0.200000 | 2220 | [0x585bfb…f2e9](https://sepolia.arbiscan.io/tx/0x585bfb693ab8d2b8acf1f39f5e59de821adb0b3cbecf8cb7a856451cc388f2e9) |

Keeper execution completed for both orders. The full-close transaction contains multiple UserOperations; its assistance deposit is log 12, while the partial-close deposit is log 4. Both grants are safely reconciled and settled, with no unresolved assistance record; the full grant was verified at 2026-09-15 10:55:29 UTC. The reviewed Terraform overlay records the intended all-trader state; use the workflow's `disabled` mode and override both overlay issuance flags to false for a disabled bootstrap.
