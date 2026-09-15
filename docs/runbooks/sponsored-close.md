# Sponsored close on Arbitrum Sepolia

Close assistance funds only the exact missing keeper bounty, up to 200,000 USDC atomic units. It has no automatic expiry. The protocol remains v1.2.3 and its configured bounty remains unchanged. A full or partial close must otherwise be valid and existing collateral must cover accrued carry.

The release binding is `apps/backend/deployments/close-assistance-arbitrum-sepolia.json`. The lens is `0xA1E188928FE0b310e334A58f780492518DE64B7f`, runtime Keccak256 `0xad79a00704dbd2876dcc392bcce0fe741d415c1511a86a11080f4ce39f9818e8`. Its source is core commit `17a4e8044b6b4d3ea2ccb60986a489917969a559`.

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
