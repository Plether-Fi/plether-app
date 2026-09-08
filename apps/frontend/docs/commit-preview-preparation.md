# Commit Preview preparation

The ticket starts a read-only preparation after 500 ms of valid, settled inputs.
Review joins matching work started within 10 seconds or reuses its result if the
onchain deadline is more than 10 seconds away. Otherwise, Review starts a fresh
request immediately. The cache is local to the mounted ticket. Wallet signatures,
sponsorship and submission remain exclusively in the commit flow.

The open review freezes order inputs. Account changes invalidate its prepared
state; account/chain/deployment identity changes close it. Price updates alone do
not replace a displayed review, while a market-regime change rechecks execution
conditions (including Exact slippage for frozen closes). At 10 seconds before the
deadline, the review refreshes and disables confirmation. Changed terms are shown
at full precision with an explicit “Confirm updated order” action. Failed or
already-expiring results require a manual retry. Hidden tabs suspend new work.

## Rollout measurements

Existing analytics consent and property filtering apply to both events. No
account addresses, order values, input keys or raw errors are emitted.

| Event | Properties | Interpretation |
| --- | --- | --- |
| `perps order preparation finished` | `duration_ms`, `reason_code` = `background`, `cold`, `refresh`; `error_category` = `none`, `preparation_failed`, `cancelled` | Preparation duration, request frequency, failures and discarded work. |
| `perps review ready` | `duration_ms`, `reason_code` = `completed_reuse`, `pending_reuse`, `cold`, `refresh` | Time from modal opening to readiness, or from refresh start to readiness. |

After deployment, compare median and p95 review-ready duration excluding
`reason_code = refresh`. Break it down by reuse reason and compare completed-reuse
count against all non-refresh ready events. Track preparation events per session
and failure/cancellation rates alongside latency: lower modal wait should not hide
excess speculative requests. Refresh latency should be monitored separately.

These timings measure client preparation/readiness, not a network-only RPC
benchmark or transaction inclusion. Sessions that close or remain invalid before
readiness do not emit `perps review ready`; use the existing trade-review modal
open/close events when measuring abandonment.

## Verification

Hook tests cover scheduling, admission, invalidation, races, visibility, timeouts,
and refreshes. Ticket tests cover warm opening, frozen close behavior, TP/SL,
account switches and exact changed terms. The Storybook group
**Perps / Commit Preview Preparation** covers ready, checking, slow, updating,
changed and failed presentations without RPC or wallet activity.
