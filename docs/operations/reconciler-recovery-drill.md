# Isolated reconciler recovery drill

Run this alongside the ECS image-recovery and frontend recovery drills in
[incident hardening rollout](../runbooks/incident-hardening-rollout.md). This
exercise starts the production `runAaReconciler` loop in a child process, a new
disposable PostgreSQL cluster, and two independent loopback HTTP providers. It
does not connect to a deployed database, provider, wallet, KMS key or AWS API.

The small Haskell harness supplies loopback clients directly to the production
loop. The deployed CLI's external-HTTPS and deployment guards remain intact;
this is not a replacement or a bypass for their production qualification.
Authorizations, signatures, chain headers and events are synthetic fixtures.
No transaction submission method is implemented by either provider.

## Run

Requires the repository's GHC/Cabal toolchain, a built backend library, Python 3
and PostgreSQL 16 tools. Use an absent evidence directory with a short path so
PostgreSQL's private Unix socket path remains within the platform limit:

```bash
cd apps/backend
cabal build lib:plether-api
cd ../..
python3 scripts/run-reconciler-recovery-drill.py \
  --evidence-dir /tmp/plether-reconciler-drill-UNIQUE
```

Use `--postgres-bin /absolute/path/to/postgresql/bin` when the tools are not on
PATH or in the standard Homebrew PostgreSQL 16 location. Inherited AWS, PG,
PERPS, AA and database configuration is removed from child environments. The
harness independently refuses non-loopback database/RPC arguments and accepts
only a dedicated database named `plether_recovery_drill`. Optimized Python
(`-O` or `PYTHONOPTIMIZE`) is refused so it cannot disable qualification checks.

## Runtime checks

The synthetic safe tip is 36,000 blocks beyond the stored cursor, representing
ten hours of history. Configured 1,000-block batches encounter a provider limit
of 200 inclusive blocks. The real range scanner must subdivide them and both
providers' accepted ranges must cover every block through the safe tip.

Two submitted synthetic authorizations reserve 1,000 wei each. Their canonical
events cost 200 and 300 wei. A PostgreSQL trigger holds cursor publication after
the first event's settlement has committed. The runner sends **SIGKILL** to the
worker at that point, terminates only its disposable database sessions, and
restarts the same production loop. The cursor must remain zero while the first
charge remains 200 wei. Replaying that event must not charge or release twice.

An injected primary-provider outage crosses the isolated five-second circuit
breaker threshold. Neither cursor nor fresh health may advance during the
outage. After provider recovery, every partial backlog observation must remain
unhealthy until the exact safe tip is committed. Recovery must leave issuance
paused; there is no automatic resume. Final economic evidence must be exactly
two settled events, six ledger rows, 2,000 wei reserved, 500 charged and 1,500
released.

After stopping the worker, the harness invokes the actual database resume
operation. Missing, stale, future, cursor-mismatched and wrong-paymaster health
must refuse without clearing the pause. A new incident with the same reason
must invalidate the previous generation's approval. Fresh exact-generation
resume succeeds, emits the expected audit event, and changes no economic rows.

## Evidence and cleanup

`evidence.json` records checks, cursor/health progression, economic totals and
cleanup results. `rpc-reads.json` records only synthetic method/range outcomes.
Worker logs, compiler output and the stopped disposable data directory remain
inside the mode-0700 evidence directory for review. No fixture contains live
credentials. All child workers, HTTP listeners and PostgreSQL processes are
stopped in `finally`, including on failure.
Cleanup also inspects the owned cluster's PID file after a failed start. A
remaining worker or PostgreSQL process makes qualification fail.

A successful command has `healthy=true`, every check passed, and both cleanup
flags true. It proves runtime behavior against deterministic local dependencies;
it does not prove live provider independence, on-chain execution, notification
receipt, or successful operation of the protected GitHub recovery workflow.
Record those separately from the ECS/notification drill and exact workflow run.
