# Short-position review oracle failure — 2026-09-21

Initial investigation source: `d63492cc`. The frontend correction described
below is now implemented and validated locally. No deployment, on-chain state,
or user orders were changed. Historical findings below describe the original
revision, before the correction.

## Findings

The screenshot shows a failure preparing the review, before confirmation is
enabled. The `getLatestPrice()` read in
`apps/frontend/src/contracts/preparePerpsOrderV2.ts:260` is independent of the
requested size and direction. This error is not evidence of insufficient margin,
excessive short size, or an order submitted and then rejected.

A historical call near the screenshot time reproduces
`PletherOracle__PriceOutOfOrder(uint64,uint64)`. The oldest stored basket component
was two seconds behind the engine mark. Separately, a real viem error from that
call reproduces a frontend decoder defect that prevents the existing automatic
review recovery from recognizing this condition.

The screenshot alone does not identify a browser session or exact block. The
historical reproduction and nearby telemetry are strongly consistent with the
report, but are not proof of that screenshot's exact underlying RPC response.

## Historical chain evidence

- Chain: Arbitrum Sepolia (421614).
- Oracle: `0x9f4d9ae736b94249b18a85a7e14092bfca0688eb`.
- Block: `311266517`.
- Hash: `0x040a1b9a99cc5bcc384bcaaed9fbb6162ee4c34d2e4bff05f34109be05e513c3`.
- Block time: `2026-09-21 16:21:10 UTC` / `18:21:10 Europe/Warsaw`.
- Error selector: `0x8e3e110d`.
- `publishTime`: `1790007647` (`16:20:47 UTC`).
- `lastMarkTime`: `1790007649` (`16:20:49 UTC`).

Read-only reproduction:

```sh
cast call 0x9f4d9ae736b94249b18a85a7e14092bfca0688eb \
  'getLatestPrice()(uint256)' \
  --block 311266517 \
  --rpc-url https://sepolia-rollup.arbitrum.io/rpc
```

Returned revert data:

```text
0x8e3e110d000000000000000000000000000000000000000000000000000000006ab1595f000000000000000000000000000000000000000000000000000000006ab15961
```

A latest-state call during this investigation succeeded with `97931917`.
This proves the condition was not continuously present, not that it cannot recur.
The public frontend bundle `index-CN10eYLd.js` uses the same public RPC origin.

## Decoder and recovery defect

Using viem **2.45.2**, the application's actual `PERPS_PLETHER_ORACLE_ABI`,
and the historical block produces this error chain:

```text
ContractFunctionExecutionError
  ContractFunctionRevertedError
    raw: <complete revert data above>
    cause: AbiErrorSignatureNotFoundError
      signature: 0x8e3e110d
```

The read ABI has no oracle custom errors. Although the application's separate
error ABI includes `PletherOracle__PriceOutOfOrder`, the extractor in
`apps/frontend/src/utils/perpsErrors.ts:198` traverses `cause` first, then
arbitrarily walks object values. It selects the nested four-byte `signature`
before reaching the parent's complete `raw` field. Decoding that selector alone
fails because both uint64 arguments are missing. The decoder does not continue
searching for another candidate after that failure.

Running the existing source decoder against the real historical error returned:

```text
getPerpsContractErrorCode(error) -> undefined
isPerpsOracleSyncError(error) -> false
getPreparationFailureProperties(error) ->
  stage=context_read, contract_function=getLatestPrice,
  error_code=undecoded_revert
```

Passing only the complete revert data to those same helpers correctly returned
`PletherOracle__PriceOutOfOrder` and `true` for recovery. Both direct and viem
multicall-enabled reads preserved the complete raw bytes.

Consequently, `usePerpsOrderPreparation.ts:234` takes the terminal-error path
instead of the existing two-second read-only retries within its 30-second
deadline. Confirmation appropriately stays disabled.

The replay produced the generic review-unavailable fallback, rather than the
screenshot's exact "unknown error" sentence. That wording depends on the original
RPC/viem wrapper; the original response was not retained in the preparation
telemetry. Do not claim the exact screenshot wording was reproduced.

## Telemetry evidence

PostHog project **208816**, confirmed against the repository deployment workflow.
Queries covered September 21 UTC and reflect events ingested at investigation
time, not a completed day's totals.

For `perps order preparation finished`, filtering `error_category =
preparation_failed` and `contract_function = getLatestPrice`:

| Preparation source | Failures | Distinct browser identities |
| --- | ---: | ---: |
| Background | 141 | 49 |
| Cold review | 4 | 4 |
| Review refresh | 14 | 10 |
| Combined | 159 | 58 |

All 159 were labeled `undecoded_revert`. Browser identities can overlap between
rows and are not verified unique people. These counts do not establish that every
failure had the same contract cause. No `perps oracle recovery` events appeared
in that day's queried window.

Nine failures occurred between `15:45` and `16:30 UTC`. A nearby session recorded:

- `16:21:12.895`: review-refresh failure at `getLatestPrice`, lasting 2,816 ms.
- `16:21:19.009`: review closed.
- `16:21:43.535`: review reopened.
- `16:21:47.011`: review ready.
- `16:22:31.412`: commit started.
- `16:23:03.491`: commit succeeded.
- `16:23:13.665`: order executed.

This is corroborating evidence of recovery after reopening, not an identified
match to the screenshot's user. The screenshot filename predates that recorded
failure by about eleven seconds.

## Correction requirements

1. Decode explicit revert-data fields across bounded, cycle-safe error chains,
   continuing past undecodable candidates. Do not let signature-only metadata,
   addresses, hashes, or transaction calldata shadow complete revert bytes.
2. Include the deployed oracle's custom errors in the contract read ABI, so viem
   can decode at the source as well.
3. Add regression coverage using actual viem error instances with the production
   ABI, including the `AbiErrorSignatureNotFoundError` cause shape. Verify both
   friendly error classification and entry into the existing recovery controller.
   Existing tests use simplified `{ cause: { data } }` objects and miss this shape.
4. Keep recovery limited to proven ordering errors; retain fresh-block full-review
   rebuilding and explicit user confirmation. Do not retry submissions or relax
   oracle guards.
5. Review updater health/repair logs separately if lag persists beyond the
   recovery deadline. The known historical/stored-feed synchronization gap is
   documented in `docs/adr/0002-oracle-feed-synchronization.md`; this investigation
   did not inspect worker logs or establish why repair had not completed at this
   particular block.

Initial investigation validation: historical chain replay, selector/argument decoding,
direct-versus-batched viem reads, and real-error-versus-raw-data comparisons using
the original source helpers.

## Implemented correction and validation

The decoder now traverses only explicit error envelopes (`cause`, `error`,
`originalError`, `data`, `raw`), with cycle detection, safe property access,
an eight-level depth limit and a 64-object limit. It continues past incomplete
or unknown revert candidates, pairs decoded names with their own arguments, and
prefers underlying errors over outer wrapper data. Arbitrary metadata such as
signatures, hashes, addresses and request calldata is not searched.

The read ABI now includes `PletherOracle__PriceOutOfOrder(uint64,uint64)` and the
general decoder shares that definition. The oracle compatibility fixture was
updated from seven to eight entries after matching every entry against the
checksum-derived release ABI already generated in
`apps/backend/protection-worker/abi.mjs`; the prior seven-entry fingerprint was
also verified unchanged. No contract release or dependency was changed.

Generic viem unknown-contract errors during review use the existing safe
review-unavailable message. Missing data still does not trigger oracle recovery.
The recovery controller's two-second cadence, original 30-second deadline,
fresh review preparation, and explicit user confirmation remain unchanged.

Validation completed on September 21:

- Targeted decoder, diagnostics, order preparation and worker/review recovery:
  **208 tests passed** before the final ABI fixture update.
- Final frontend unit suite: **140 files, 1,818 tests passed**.
- `npm run lint`: **passed**.
- `npm run build`: **passed**, with Vite's existing large-chunk advisory.
- `git diff --check`: **passed**.
- Historical public RPC replay: verified block `311266517` against the recorded
  block hash, then exercised both the original function-only ABI and corrected
  read ABI against actual `readContract` errors. Both produced the ordering
  message, `isPerpsOracleSyncError=true`, and exactly these diagnostic fields:

```text
stage=context_read
contract_function=getLatestPrice
error_code=PletherOracle__PriceOutOfOrder
```

Regressions use actual viem error classes with the recorded payload and cover
signature shadowing, nested envelopes, malformed candidates, metadata pairing,
cycles, throwing getters, traversal limits, unrelated calldata, and safe generic
fallbacks. Recovery tests exercise both ABI shapes, preserved inputs, fresh-block
rebuilding after worker repair, deadline exhaustion and manual retry; only the
mock worker repair submits a transaction in the worker/review integration test.

The clean offline dependency install could not resolve peer dependencies from
the local cache. Validation used existing local frontend and insights dependency
installations through ignored `node_modules` symlinks, including viem 2.45.2.
Package manifests and lockfiles are unchanged.

This has not been deployed. The change restores frontend recovery eligibility;
it does not eliminate the underlying historical/stored-feed synchronization gap.
