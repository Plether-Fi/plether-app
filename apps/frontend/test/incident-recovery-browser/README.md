# Isolated browser recovery drill

Run from `apps/frontend` after installing the existing frontend dependencies and
the project's Playwright Chromium browser:

```bash
node scripts/drill-incident-recovery-browser.mjs
```

An optional first argument selects the evidence JSON path. The default is the
operating system temporary directory under
`plether-isolated-recovery-drill/frontend-browser.json`. The script exits nonzero
on any failed assertion and always closes its browser and loopback server.

This runs real headless Chromium against the production execution, readiness,
operation journal, unsigned deposit-authorization storage and Web Locks modules.
The HTTP boundary is a controlled Vite middleware bound to `127.0.0.1`. External
requests are blocked and make qualification fail. No wallet connector, signer,
private key, signable payload, public backend or chain RPC is configured.

The drill verifies three scenarios:

1. A fresh `RECONCILIATION_STALE` observation blocks a new action before native
   preparation, wallet signing, submission or operation journaling, even with
   broad readiness enforcement disabled.
2. An HTTP connection drops after the preparation request reaches the fixture.
   The original operation and unsigned authorization record survive an actual
   browser reload. A recovery request receiving gateway `RECONCILER_STALE`
   retains the same preparation ID, action payload, blocked lane and authorization
   storage. A second browser reload retains that state. No signing or submission
   callback runs.
3. In a separate browser context, one synthetic operation with an explicitly
   invalid four-byte signature is seeded through the production operation
   journal and bound to its computed EntryPoint hash. The production
   `SponsoredOperationRecovery` React coordinator performs real loopback HTTP
   receipt, status and safe-snapshot reads while readiness is stale. The signed
   preimage, UUID, hash, nonretryable lane and authorization cache survive an
   actual reload. A later matching receipt fixture confirms that same record
   through normal polling, releases the lane and records the exact consumed
   nonce without erasing the authorization cache. Another reload retains the
   confirmed state. Recovery never prepares, signs, submits or creates a new
   operation.

Chromium may retry a connection-level failure at the transport layer. Evidence
therefore separates logical preparation calls from HTTP requests and checks
that every request retains the original preparation UUID. It does not assume
that one JavaScript fetch always produces exactly one network request.

Evidence records Chromium/Playwright versions, production-source hashes, actual
reload/storage/lock use, request counts, before/after payload/storage hashes,
diagnostic reason codes and zero signing/submission counts. It omits unsigned
authorization contents and fixture account addresses.

This is browser integration qualification with simulated RPC responses and a
deliberately invalid-signature signed-journal fixture. It is not a live wallet,
signed authorization, sponsored transaction, canonical on-chain receipt
verification, settlement or financial-pause drill. The harness is a test page
and is not part of the production build entry point.
