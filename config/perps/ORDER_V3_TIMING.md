# V3 order timing — source release, not activated

This change targets a fresh complete V3 protocol deployment. It does not deploy
contracts or services, publish an SDK, migrate state, or modify historical V2
manifests. The checked-in public manifest and deployment pins still describe the
old release; V3 preparation deliberately rejects that manifest. Relabeling it
cannot bypass the lifecycle Book's on-chain V3 intent-domain check.

A fresh review signs `submitBy = reviewedBlock.timestamp + 120` and
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

Before activation (a separate authorized release):

1. Deploy and verify the complete graph from the companion core V3 source. Export
   compiler ABIs, source commit, deployment block and runtime hashes. Verify all
   reciprocal bindings and `INTENT_TYPEHASH` against the V3 domain.
2. Introduce the verified V3 release file, update the frontend address registry,
   backend embedded manifest, worker pins, Docker and infrastructure references,
   and close-preview pin together. Include `orderInterfaceVersion: 3` in the
   public/API manifest and schema 3 in the protocol deployment manifest. Preserve
   historical V2 files. AA transport version 2 is independent of order version 3.
3. Regenerate bindings from those artifacts and run all ABI, sponsorship,
   submission/recovery, history and worker checks. The app uses local V3 order
   encoders with the published 0.1.0 SDK's unchanged generic account helpers;
   switching to SDK 0.2.0 can follow its separately approved publication.
4. Use a fresh router-scoped indexer cursor and journal identity. Verify a delayed
   approval near `submitBy`, execution after `submitBy` within the full committed
   window, expiry, retries, protected-open and sponsored-close receipts.

The `v3-insights-close-*` fixtures are synthetic V3 encodings of historical
financial values. They are test vectors, not claims of a V3 transaction on chain.
