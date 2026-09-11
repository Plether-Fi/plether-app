# Frankfurt notification and capacity review — 2026-09-11

Scope: account 932542905614, eu-central-1, deployment sepolia-aa-temp.
User approved investigating RPC/database capacity and reducing duplicate alerts.
No Singapore, Core, funding, service deployment or database-size changes.

## Investigation

At approximately 13:03 UTC, 56 of 59 CloudWatch alarms were OK. The active
alarms were RPC request warning, RPC request critical and RDS freeable memory.
All 59 had the same SNS alarm destination; none had recovery actions. There
were no composite alarms. Historical ALB, native-AA, protection, LP and indexer
emails were not evidence of concurrent current failures. One ALB-generated 5xx
triggered its alarm; it subsequently cleared. An alarm returning to OK is not
a blanket service all-clear (several sparse error metrics treat missing data
as non-breaching).

The preceding one-hour aggregate contained about 44,690 instrumented backend
RPC requests. A subsequent per-role sample showed approximately 19,900 API-perps
requests (about 45% of attributed traffic), including 11,584 block reads, 4,395
contract calls and 3,839 chain-ID checks. Keeper checks and the reconciler were
other substantial consumers. This is not a complete Alchemy billing inventory:
Alto, the JavaScript oracle and browser-side requests need separate accounting.

The API's Gateway background loop calls nativeSecurityContext once per second
while preparation and sponsorship are enabled. Even profile-cache hits perform
chain attestation, safe/latest/canonical header checks and a pause read. Recent
logs showed roughly three block reads, one chain-ID check and one contract read
per iteration. The reconciler also continues its independent periodic checks.
Reducing this traffic requires a separately tested verification-path change;
this notification rollout does not slow checks, cache permission or change
the approved one-second warming behavior.

RDS is db.t4g.micro. Six-hour samples showed freeable-memory minima decreasing
from about 164 MiB to 117–119 MiB as connections increased from roughly 4 to
12–16. Swap grew from about 1.2 to 3.4 MiB. CPU remained near 4.4%, average
read latency below 1 ms and write latency below 3 ms. These measurements show
limited memory headroom, not proof of a memory leak or a database outage.
The 128 MiB alert remains active; do not lower it to hide pressure. A database
resize, tuning changes or additional recurring infrastructure cost needs a
separately reviewed proposal and approval.

## Notification change

- Keep the 15,000/hour RPC warning as a visible CloudWatch alarm, without email.
- Keep the 25,000/hour critical RPC email. Neither threshold is changed.
- Preserve all other 58 alarms' failure notifications, including memory,
  reconciliation, signatures, budgets, protection and invariant failures.
- Add an OK/recovery action to those 58 alarms, using the existing SNS topic.
- Preserve metrics, evaluation windows, missing-data rules, enabled states,
  descriptions and all other alarm settings. No new resource is created.

This is targeted duplicate suppression, not a broad incident-grouping system.
It does not prevent separate real faults from producing separate emails, and
recovery emails can increase total messages while making their status clear.

Terraform represents the same policy with a Frankfurt-only recovery local and
a Frankfurt-only exception for the duplicate RPC warning. Other environments
retain their existing routing. The operator helper is plan-only by default:

```sh
node --test scripts/frankfurt-alarm-routing.test.mjs
node scripts/frankfurt-alarm-routing.mjs
node scripts/frankfurt-alarm-routing.mjs --apply
```

The apply helper verifies the account, 59-alarm inventory, exact ARN namespace
and notification destination, rereads each alarm before changing it, and checks
the full configurable specification afterward. It performs no state forcing or
synthetic alarm tests. Re-running after success must report zero changes.
Application/readback status should be verified before reporting completion.

## Applied verification

At 13:15 UTC, final AWS readback showed 59 alarms, 58 failure-email routes and
58 recovery-email routes. The duplicate RPC warning retained its 15,000/hour
threshold with no email actions. A fresh plan reported `changes: []`. All
configurable fields were compared after each update; no metric threshold,
expression or evaluation setting changed. One metric-math alarm required
omitting AWS's readback-only empty top-level Dimensions array from the write
request; the script and regression test now cover that API shape.

Both RPC-volume alarms remained ALARM (only critical sends email). The RDS
memory alarm had returned to OK without resizing or changing its threshold;
the earlier low-headroom measurements still warrant capacity follow-up.
Thirty mocked Terraform tests and six operator tests passed, including an
explicit assertion that Singapore's warning/recovery routing is unchanged.

Rollback changes only routing: restore the RPC warning's existing SNS alarm
action and remove the newly added recovery actions. Retain all evaluation and
safety settings. Review the current configuration before rollback rather than
overwriting any subsequent operator changes.
