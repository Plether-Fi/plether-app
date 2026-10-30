# V3 order timing

The current consumer configuration is `mock-usdg-prepared.json`, schema 4,
order interface 3, phase `prepared`. Its null deployment pins keep perps unavailable
until a verified new deployment is reviewed. Historical v1.3.1 evidence remains
archived and is not a runtime fallback. The token-neutral source changes ABI
names, selectors and signed field names; retaining interface 3, configuration V5,
digest V6 and receipt V5 labels does not make historical contracts compatible.

Confirmation fixes `submitBy = confirmationBlock.timestamp + 120` and
`executionWindowSeconds = 60`. The existing 45-second signing and 30-second
submission guards apply to `submitBy`. The API caps both stub and final
sponsorship by that signed submission bound. It never extends signed calldata.
The contract stores `executionDeadline = commitTimestamp + executionWindowSeconds`;
keepers and pending-order UI read `orderTiming`, including after finalization.
Historical oracle prices remain anchored to commitment, independently of either
client approval time or sponsorship expiry.

The app persists V3 requests in a distinct operation journal namespace, retaining
both signed fields and the exact UserOperation. Recovery keeps ambiguous signed
operations locked until canonical inclusion, nonce, or safe expiry evidence is
available. Committed orders execute without another client signature. This fresh
deployment integration does not offer V2 compatibility or import old journals.

Before app deployment and contract activation:

1. Deploy and verify the complete graph from the companion core V3 source. Export
   compiler ABIs, source commit, deployment block and runtime hashes. Verify all
   reciprocal bindings and `INTENT_TYPEHASH` against the V3 domain.
2. Introduce the verified V3 release file, update the frontend address registry,
   backend embedded manifest, worker pins, Docker and infrastructure references,
   and close-preview pin together. Include `orderInterfaceVersion: 3` in the
   public/API manifest and schema 4 in the protocol deployment manifest. Preserve
   historical V2 files. AA transport version 2 is independent of order version 3.
3. Regenerate bindings from those artifacts and run all ABI, sponsorship,
   submission/recovery, history and worker checks. The app consumes the pinned
   SDK 0.4.0 artifact below. Registry publication is a separate release action.
4. Use a fresh router-scoped indexer cursor and journal identity. Verify a delayed
   approval near `submitBy`, execution after `submitBy` within the full committed
   window, expiry, retries, protected-open and sponsored-close receipts.

The `v3-insights-close-*` fixtures are synthetic V3 encodings of historical
financial values. They are test vectors, not claims of a V3 transaction on chain.

## Shared SDK source

The app imports ordinary/protected order builders, the protection ABI, request
components, and request/bounds types from one SDK artifact built by Core's
`scripts/perps-aa-artifact.mjs`. There is no second app encoder or edited vendor
source. Both close-preview consumers also share one generated ABI.

- Source: `e35cf0e0e9b880e07812e36321dee98bd299dcbd`
- Package: `@plether-fi/perps-aa-client@0.4.0`
- Integrity: `sha512-kE1ScY6Ef95xLAqGdh5eTHurV7jRs3iy9fIGcyKJnxzsqZNx+E0IZHtjxUVQZ5X+fqnPC9uaoGT71KhKoQTJtg==`
- Provenance: `config/perps-aa-client-release.json` and
  `apps/frontend/vendor/perps-aa-client/release.json`

Verify with `node scripts/verify-vendored-perps-aa-client.mjs`. To refresh, build
and pack a clean, reviewed Core commit with its packaging script; replace the
artifact, provenance, lockfile integrity, and local-flow CI Core SHA together.
Never modify the tarball contents manually. The previous published SDK's
provenance is retained in `config/perps-aa-client-release-v0.1.0.json`.

## Local end-to-end proof

With Node, Foundry, GHC 9.4.8, Cabal, libpq and libsecp256k1 installed, run
`npm ci` in `apps/frontend`, then from the repository root:

```sh
node scripts/test-order-v3-local.mjs /path/to/clean/pinned/plether-core
```

The runner checks the Core source pin, builds production contracts and a test
adapter around the real Haskell timing policy, starts its own loopback-only
Anvil chain, and stops it on exit. No live RPC, fork, credentials, publication,
or deployment workflow is used. Only MockUSDG/Pyth, readiness transport,
telemetry, and the local relay replace external services. The real app review,
native preparation journal, SDK, owner/sponsor signatures, reference EntryPoint
and SimpleAccount, production paymaster/router/engine, and canonical recovery
execute together. Account creation is local test setup; this does not test a
wallet extension, KMS, or the production bundler service.

Coverage includes 75 seconds in wallet approval, commitment one second before
`submitBy`, lost submission acknowledgement, restored journal recovery after
sponsorship expiry, execution within the full 60-second committed window with
one owner signature, exact replay/conflicting terms, late approval safe expiry,
committed expiry, historical-price rejection, and protected opens. CI runs the
same non-skipping harness in `order-v3-local.yml`. Database integration is
verified separately by the existing PostgreSQL CI job.
