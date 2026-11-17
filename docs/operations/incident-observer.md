# ECS incident evidence and service observation

The November 17 outage had working CloudWatch alarms, but email did not lead
to recovery before the user report. The original task's stop reason was no
longer available from ECS by the time it was investigated. The opt-in observer
adds independent service-state checks and retains a sanitized projection of
stopped service tasks for 90 days.

This change does **not** establish an on-call service. The current operations
SNS email destination remains the notification route. SNS publication and a
confirmed email subscription do not prove mailbox delivery, acknowledgement or
escalation. No pager destination is currently configured. An operator must own
that response gap; adding an escalation ARN alone does not close it.

## Behavior and privacy boundary

`infra/incident-observer/handler.py` runs with only:

- `ecs:DescribeServices` for the configured service ARNs;
- write access to its two CloudWatch log groups;
- `cloudwatch:PutMetricData` in `Plether/Operations`;
- publish access to the existing operations topic and optional escalation topic.

It has no SSM, Secrets Manager, KMS, database, task-start, service-update or
transaction permissions. Service names come from Terraform's configured ECS
resources, including dormant services. Every minute it reads all services in
batches of at most ten. It publishes `max(0, desired-running)`; pending tasks do
not count as available capacity. A missing service, malformed response or
failed batch rejects the **whole** observation, so a partial read cannot emit
healthy metrics. A separate successful-observation metric detects silence.

EventBridge routes STOPPED service-task events from this cluster to the
observer. Evidence includes service, task/revision, timestamp, stop code,
container names, image digests and exit codes. Free-text stop/container reasons
are classified into bounded categories with fixed summaries. The original
reason, event, task overrides, environment, image URLs and provider messages
are never logged or forwarded to SNS. Unknown reasons remain `OTHER`; they are
not guessed. Existing restricted application logs remain a separate source.

All stops are retained, including planned deployments. Failed starts,
essential-container exits, infrastructure failures, image-pull failures,
resource-initialization failures, OOM and failed health checks notify the
operations topic immediately. Event delivery is at least once; notification
consumers should deduplicate by `event_id` when present. The optional existing
`incident_escalation_sns_topic_arn` receives the same sanitized failure signal
and CloudWatch alarm actions. Its downstream system must own acknowledgements,
reminders and escalation; Terraform does not infer or create those services.

The observer's own failures, silence, EventBridge delivery failures and Lambda
asynchronous event drops have alarms. There is no raw-event dead-letter target:
that would cross the projection boundary. Async events retry once and expire
after an hour. An event-drop alarm means the evidence set may be incomplete,
not that the affected application is healthy.

Existing reconciler freshness, fatal-error and worker-progress alarms remain
necessary. Desired/running counts alone cannot establish useful progress or
that sponsorship is safe to resume.

## Packaging and reviewed rollout

The observer is disabled by default, so introducing the code does not silently
create a runtime or change existing alarm routing. Build its deterministic zip
without credentials or package downloads:

```bash
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover \
  -s infra/incident-observer -p 'test_*.py' -v
python3 scripts/package-incident-observer.py \
  --output /tmp/plether-incident-observer.zip
node scripts/test-incident-observer-terraform.mjs
```

The Terraform test uses an isolated mocked module and no AWS resources. It
requires repository-pinned Terraform 1.16.x; `TERRAFORM_BIN` and
`TERRAFORM_PLUGIN_DIR` can select an already verified local binary/provider.

Follow `AGENTS.md` for the named AWS profile, identity check, saved Terraform
plan and reviewed application. Add these settings to the authoritative
**Sepolia** deployment configuration only after review:

```hcl
enable_incident_observer        = true
incident_observer_package_path  = "/tmp/plether-incident-observer.zip"
# Keep operations_alarm_sns_topic_arn set to the existing operations topic.
# Leave incident_escalation_sns_topic_arn empty until a real pager exists.
```

The plan records the package hash and requires an operations topic in the same
account/region. Rebuild the package from the reviewed source for every plan;
do not substitute a zip from another release. Review that the plan adds only
the observer, its scoped IAM, rules, evidence logs and alarms. It must not
change application roles, task definitions, signer topology or financial state.

After applying the approved plan, require fresh `ServiceObservationCompleted`,
zero service deficits where expected, no observer/delivery/drop errors, and a
sanitized event in `/plether/sepolia/ecs-incidents`. Validate existing topic
subscriptions and delivery separately. A later environment rollout requires
its own review and notification qualification.

## Safe qualification drill

Run the following in an isolated test environment with synthetic data, not by
deleting a production image or stopping a live financial worker:

1. Run the Python fixture tests. They inject an image-pull failure whose reason,
   overrides and container fields contain fake credentials. Only allowed
   metadata, `IMAGE_PULL_FAILED` and its fixed summary may reach evidence/SNS.
2. Configure a staging service with a deliberately unavailable fixture image.
   Observe its `TaskFailedToStart`/image-pull event, projected evidence and
   notification. Require the stop alert to reach an operator within two minutes.
3. Leave desired=1 with running=0/pending=1. Confirm the independent deficit
   alarm fires after two consecutive observations. A dormant desired=0 service
   must remain at deficit=0.
4. Inject failure in the second DescribeServices batch using the unit fixture.
   Confirm no partial healthy metrics; in staging also deny the observer's read
   permission temporarily and confirm observer-failed/silence alarms.
5. Restore the fixture service with an available image and verify a complete
   observation. Check useful worker progress separately; a healthy count is not
   an all-clear for trading.
6. Test receipt and human acknowledgement of the operations email. Record who
   owns response. If a pager is added, deliberately leave a test page
   unacknowledged and require its external escalation after five minutes.

Record the event ID, timestamps, observed task/revision/digests, alarm actions,
notification receipt and acknowledgement. Missing acknowledgement or escalation
is an unresolved operational gap even if every AWS delivery check succeeds.
No drill should clear a sponsorship pause, modify a cursor or place a trade.
