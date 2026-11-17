#!/usr/bin/env python3
"""Controlled observer faults for the fixed, nonfinancial Sepolia recovery fixture.

Default execution is read-only. --execute requires the exact confirmation. Every
fault journals its original state before mutation and restores it in finally.
--restore can recover an interrupted run from that journal. No IAM policy writes,
production observer changes, fake metrics, or forced alarm states are permitted.
"""
import argparse
import base64
import copy
import datetime as dt
import hashlib
import json
import os
import pathlib
import re
import signal
import subprocess
import sys
import time
import uuid

ACCOUNT = "932542905614"
REGION = "ap-southeast-1"
CLUSTER = "plether-sepolia-recovery-drill"
SERVICE = "plether-image-recovery-drill"
DENIED_SERVICE = "plether-image-recovery-drill-denied"
NAME = CLUSTER + "-observer"
ROLE = f"arn:aws:iam::{ACCOUNT}:role/{NAME}"
FUNCTION = f"arn:aws:lambda:{REGION}:{ACCOUNT}:function:{NAME}"
CLUSTER_ARN = f"arn:aws:ecs:{REGION}:{ACCOUNT}:cluster/{CLUSTER}"
RULE = CLUSTER + "-service-poll"
RULE_ARN = f"arn:aws:events:{REGION}:{ACCOUNT}:rule/{RULE}"
STATEMENT = "AllowEvents-poll"
TOPIC = f"arn:aws:sns:{REGION}:{ACCOUNT}:plether-sepolia-operations"
MIRROR = f"arn:aws:sns:{REGION}:{ACCOUNT}:{CLUSTER}-evidence"
QUEUE = f"https://sqs.{REGION}.amazonaws.com/{ACCOUNT}/{CLUSTER}-evidence"
QUEUE_ARN = f"arn:aws:sqs:{REGION}:{ACCOUNT}:{CLUSTER}-evidence"
LOG_GROUP = "/aws/lambda/" + NAME
CONFIRMATION = "RUN OBSERVER FAILURE DRILL ON SEPOLIA"
ALARM_SUFFIXES = ("observer-failed", "observer-missing", "observer-events-dropped", "poll-delivery-failed", "stopped-delivery-failed", "task-deficit")
ALARMS = {suffix: CLUSTER + "-" + suffix for suffix in ALARM_SUFFIXES}
ENV_KEYS = {"EXPECTED_ACCOUNT_ID", "ECS_CLUSTER_ARN", "SERVICE_NAMES", "EVIDENCE_LOG_GROUP", "OPERATIONS_TOPIC_ARN", "ESCALATION_TOPIC_ARN"}


def utc_now():
    return dt.datetime.now(dt.timezone.utc)


def timestamp(value):
    return dt.datetime.fromisoformat(value.replace("Z", "+00:00")).astimezone(dt.timezone.utc)


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def ensure(condition, message):
    if not condition:
        raise ValueError(message)


class AWSFailure(RuntimeError):
    def __init__(self, code):
        self.code = code
        super().__init__("AWS request failed: " + code)


class AWS:
    def __init__(self):
        ensure(not any(k == "AWS_ENDPOINT_URL" or k.startswith("AWS_ENDPOINT_URL_") for k in os.environ), "Custom AWS endpoints are prohibited for this runtime drill")

    def call(self, service, operation, **kwargs):
        # The command cannot choose another profile/region/endpoint. All mutation
        # resource arguments are constants validated immediately before use.
        args = ["aws", "--profile", "plether", "--region", REGION, service, operation, "--output", "json", "--no-cli-pager"]
        for key, value in kwargs.items():
            args.append("--" + key.replace("_", "-"))
            if isinstance(value, (dict, list)):
                args.append(json.dumps(value, separators=(",", ":")))
            else:
                args.append(str(value))
        result = subprocess.run(args, capture_output=True, text=True, timeout=45)
        if result.returncode:
            match = re.search(r"An error occurred \(([A-Za-z0-9_.-]+)\)", result.stderr)
            raise AWSFailure(match.group(1) if match else "CLI_REQUEST_FAILED")
        return json.loads(result.stdout) if result.stdout.strip() else {}


def expected_env(variables, denied=False):
    ensure(set(variables) == ENV_KEYS, "Unexpected observer environment keys; refusing to persist or overwrite them")
    ensure(variables["EXPECTED_ACCOUNT_ID"] == ACCOUNT, "Wrong observer account")
    ensure(variables["ECS_CLUSTER_ARN"] == CLUSTER_ARN, "Wrong observer cluster")
    ensure(json.loads(variables["SERVICE_NAMES"]) == [DENIED_SERVICE if denied else SERVICE], "Unexpected fixture service names")
    ensure(variables["EVIDENCE_LOG_GROUP"] == "/plether/sepolia-recovery-drill/ecs-incidents", "Wrong evidence group")
    ensure(variables["OPERATIONS_TOPIC_ARN"] == TOPIC and variables["ESCALATION_TOPIC_ARN"] == MIRROR, "Wrong notification destinations")


def validate_function(config, code_hash=None, denied=False):
    ensure(config.get("FunctionArn") == FUNCTION and config.get("FunctionName") == NAME, "Wrong Lambda function")
    ensure(config.get("Role") == ROLE, "Wrong observer role")
    ensure(config.get("Handler") == "handler.handler" and config.get("Runtime") == "python3.13", "Unexpected observer runtime")
    ensure(config.get("Architectures") == ["arm64"], "Wrong observer architecture")
    ensure(config.get("State") == "Active" and config.get("LastUpdateStatus") == "Successful", "Observer is not active and settled")
    ensure(config.get("Timeout") == 25, "Unexpected observer timeout")
    if code_hash:
        ensure(config.get("CodeSha256") == code_hash, "Observer package changed")
    expected_env(config.get("Environment", {}).get("Variables", {}), denied)
    return config


def validate_permission(statement):
    ensure(statement.get("Sid") == STATEMENT and statement.get("Effect") == "Allow", "Unexpected poll permission statement")
    ensure(statement.get("Principal") == {"Service": "events.amazonaws.com"}, "Unexpected poll principal")
    ensure(statement.get("Action") == "lambda:InvokeFunction" and statement.get("Resource") == FUNCTION, "Unexpected poll permission resource/action")
    ensure(statement.get("Condition") == {"StringEquals": {"AWS:SourceAccount": ACCOUNT}, "ArnLike": {"AWS:SourceArn": RULE_ARN}}, "Unexpected poll permission condition")
    ensure(set(statement) == {"Sid", "Effect", "Principal", "Action", "Resource", "Condition"}, "Unreviewed poll permission fields")


def poll_permission(response, required=True):
    statements = json.loads(response["Policy"])["Statement"]
    found = [s for s in statements if s.get("Sid") == STATEMENT]
    ensure(len(found) <= 1, "Duplicate poll permission")
    if required:
        ensure(len(found) == 1, "Missing poll permission")
    if found:
        validate_permission(found[0])
    return found[0] if found else None


def validate_rule(rule, enabled=True):
    ensure(rule.get("Name") == RULE and rule.get("Arn") == RULE_ARN, "Wrong poll rule")
    ensure(rule.get("State") == ("ENABLED" if enabled else "DISABLED"), "Unexpected poll rule state")
    ensure(rule.get("ScheduleExpression") == "rate(1 minute)" and rule.get("EventBusName", "default") == "default", "Unexpected poll schedule or bus")
    ensure(not rule.get("EventPattern") and not rule.get("RoleArn"), "Unreviewed poll rule configuration")


def safe_notification(message):
    """Retain only an allowlisted summary, never a receipt handle or raw body."""
    body = json.loads(message["Body"])
    # The fixture subscription normally uses raw delivery; accept a signed SNS
    # envelope only when its topic identity is exactly the dedicated drill topic.
    if body.get("Type") == "Notification":
        ensure(body.get("TopicArn") == MIRROR, "Foreign SNS envelope")
        body = json.loads(body["Message"])
    result = {"queue_message_id": message["MessageId"]}
    if "AlarmName" in body:
        ensure(body["AlarmName"] in ALARMS.values() and body.get("AWSAccountId") == ACCOUNT, "Foreign alarm notification")
        ensure(body.get("NewStateValue") in {"ALARM", "OK", "INSUFFICIENT_DATA"}, "Invalid alarm state")
        result.update({"type": "alarm", "alarm_name": body["AlarmName"], "state": body["NewStateValue"], "occurred_at": timestamp(body["StateChangeTime"]).isoformat()})
    else:
        ensure(body.get("event") == "ecs_task_stopped" and body.get("service") == SERVICE, "Unexpected drill notification")
        ensure(re.fullmatch(re.escape(f"arn:aws:ecs:{REGION}:{ACCOUNT}:task/{CLUSTER}/") + r"[0-9a-f]{32}", body.get("task_arn", "")), "Foreign task notification")
        ensure(re.fullmatch(re.escape(f"arn:aws:ecs:{REGION}:{ACCOUNT}:task-definition/plether-sepolia-image-recovery-drill:") + r"[1-9][0-9]*", body.get("task_definition_arn", "")), "Foreign task revision")
        ensure(body.get("reason_category") in {"OTHER", "IMAGE_PULL_FAILED", "RESOURCE_INITIALIZATION_FAILED", "OUT_OF_MEMORY", "TASK_RETIREMENT", "ESSENTIAL_CONTAINER_EXITED", "HEALTH_CHECK_FAILED", "INFRASTRUCTURE_HEALTH"}, "Invalid reason category")
        event_id = body.get("event_id")
        ensure(event_id is None or isinstance(event_id, str) and re.fullmatch(r"[0-9a-fA-F-]{36}", event_id), "Invalid event identity")
        occurred_at = timestamp(body["occurred_at"]).isoformat() if body.get("occurred_at") else None
        result.update({"type": "task_stopped", "event_id": event_id, "task_arn": body["task_arn"], "reason_category": body["reason_category"], "occurred_at": occurred_at})
    ensure(re.fullmatch(r"[A-Za-z0-9_-]{1,128}", result["queue_message_id"]), "Invalid queue message identity")
    return result


class Drill:
    def __init__(self, aws, directory, max_wait=600, interval=15, sleep=time.sleep, monotonic=time.monotonic, clock=utc_now):
        self.aws = aws
        self.directory = pathlib.Path(directory)
        self.max_wait = max_wait
        self.interval = interval
        self.sleep = sleep
        self.monotonic = monotonic
        self.clock = clock
        self.path = self.directory / "observer-fault-journal.json"
        self.state = None

    def save(self):
        self.directory.mkdir(parents=True, exist_ok=True)
        temporary = self.path.with_suffix(".tmp")
        temporary.write_text(json.dumps(self.state, indent=2, sort_keys=True) + "\n")
        temporary.chmod(0o600)
        temporary.replace(self.path)

    def event(self, event_name, **data):
        self.state["events"].append({"time": self.clock().isoformat(), "event": event_name, **data})
        self.save()
        print(json.dumps({"event": event_name, **data}), flush=True)

    def config(self, denied=False):
        return validate_function(self.aws.call("lambda", "get-function-configuration", function_name=NAME), self.state["baseline"]["code_hash"], denied)

    def alarm_snapshot(self):
        response = self.aws.call("cloudwatch", "describe-alarms", alarm_names=list(ALARMS.values()))
        values = response.get("MetricAlarms", [])
        ensure(len(values) == len(ALARMS) and {a["AlarmName"] for a in values} == set(ALARMS.values()), "Incomplete alarm observation")
        return {a["AlarmName"]: {"state": a["StateValue"], "updated_at": timestamp(a["StateUpdatedTimestamp"]).isoformat()} for a in values}

    def wait(self, label, predicate, timeout=None):
        deadline = self.monotonic() + (timeout or self.max_wait)
        while True:
            value = predicate()
            if value:
                return value
            ensure(self.monotonic() < deadline, "Timed out waiting for " + label)
            print(json.dumps({"event": "waiting", "for": label}), flush=True)
            self.sleep(self.interval)

    def wait_alarms(self, requirements, since=None):
        def ready():
            snapshot = self.alarm_snapshot()
            if all(snapshot[ALARMS[name]]["state"] == state and (since is None or state != "ALARM" or timestamp(snapshot[ALARMS[name]]["updated_at"]) >= since) for name, state in requirements.items()):
                return snapshot
            return None
        result = self.wait("alarm states " + json.dumps(requirements, sort_keys=True), ready)
        self.event("alarm_states_observed", requirements=requirements, snapshot=result)
        return result

    def metrics(self, namespace, metric, dimensions, since):
        result = self.aws.call("cloudwatch", "get-metric-statistics", namespace=namespace, metric_name=metric,
                               dimensions=dimensions, start_time=since.isoformat(), end_time=self.clock().isoformat(), period=60, statistics=["Sum"])
        # The final metric minute must be newer than the fault, not a prior phase's
        # cached datapoint. Faults are left active across scheduled invocations.
        floor = since.replace(second=0, microsecond=0)
        points = [{"timestamp": timestamp(d["Timestamp"]).isoformat(), "sum": d["Sum"]} for d in result.get("Datapoints", []) if timestamp(d["Timestamp"]) >= floor]
        return sorted(points, key=lambda d: d["timestamp"])

    def require_metric(self, namespace, metric, dimensions, since):
        def observed():
            points = self.metrics(namespace, metric, dimensions, since)
            return points if any(v["sum"] > 0 for v in points) else None
        points = self.wait(metric + " datapoint", observed)
        self.event("metric_observed", namespace=namespace, metric=metric, points=points)

    def notifications(self, alarm_names, since):
        collected = {m["queue_message_id"]: m for m in self.state.get("notifications", [])}
        receipts = []
        try:
            def receive():
                response = self.aws.call("sqs", "receive-message", queue_url=QUEUE, max_number_of_messages=10, wait_time_seconds=1, visibility_timeout=30)
                for message in response.get("Messages", []):
                    receipts.append(message["ReceiptHandle"])
                    summary = safe_notification(message)
                    collected[summary["queue_message_id"]] = summary
                self.state["notifications"] = list(collected.values())
                self.save()
                matches = [m for m in collected.values() if m.get("type") == "alarm" and m.get("alarm_name") in alarm_names and m.get("state") == "ALARM" and timestamp(m["occurred_at"]) >= since]
                if set(m["alarm_name"] for m in matches) == set(alarm_names):
                    return matches
                return None
            matches = self.wait("mirrored alarm notifications", receive, timeout=min(180, self.max_wait))
            self.event("notification_transport_verified", messages=matches)
        finally:
            # Do not delete evidence messages. Visibility also expires naturally
            # after thirty seconds if the runner is killed during this cleanup.
            for receipt in receipts:
                try:
                    self.aws.call("sqs", "change-message-visibility", queue_url=QUEUE, receipt_handle=receipt, visibility_timeout=0)
                except AWSFailure:
                    pass

    def preflight(self, package):
        previous = json.loads(self.path.read_text()) if self.path.exists() else None
        if previous:
            ensure(previous.get("capability") == "isolated-observer-fault-drill" and previous.get("status") == "prepared" and not previous.get("active_fault") and not previous.get("phases"), "Journal already contains an execution; inspect it or use --restore")
        identity = self.aws.call("sts", "get-caller-identity")
        ensure(identity["Account"] == ACCOUNT and not identity["Arn"].endswith(":root"), "Wrong AWS account or root identity")
        package_hash = base64.b64encode(hashlib.sha256(pathlib.Path(package).read_bytes()).digest()).decode()
        config = validate_function(self.aws.call("lambda", "get-function-configuration", function_name=NAME), package_hash)
        service_response = self.aws.call("ecs", "describe-services", cluster=CLUSTER, services=[SERVICE])
        ensure(not service_response.get("failures") and len(service_response.get("services", [])) == 1, "Fixture service missing")
        service = service_response["services"][0]
        ensure(service.get("serviceArn") == f"arn:aws:ecs:{REGION}:{ACCOUNT}:service/{CLUSTER}/{SERVICE}" and service.get("clusterArn") == CLUSTER_ARN, "Wrong fixture service")
        ensure(service.get("desiredCount") == 1 and service.get("runningCount") == 1 and service.get("pendingCount") == 0, "Recover the fixture image before observer fault tests")
        ensure(service.get("taskDefinition", "").startswith(f"arn:aws:ecs:{REGION}:{ACCOUNT}:task-definition/plether-sepolia-image-recovery-drill:"), "Wrong fixture task definition")
        ensure(len(service.get("deployments", [])) == 1 and service["deployments"][0].get("status") == "PRIMARY" and service["deployments"][0].get("rolloutState") == "COMPLETED" and service["deployments"][0].get("taskDefinition") == service["taskDefinition"], "Fixture rollout is not stable")
        ensure(service["deploymentConfiguration"]["minimumHealthyPercent"] == 0 and service["deploymentConfiguration"]["maximumPercent"] == 100, "Wrong fixture singleton topology")
        rule = self.aws.call("events", "describe-rule", name=RULE, event_bus_name="default")
        validate_rule(rule)
        targets = self.aws.call("events", "list-targets-by-rule", rule=RULE, event_bus_name="default")
        ensure(len(targets.get("Targets", [])) == 1 and not targets.get("NextToken"), "Unexpected poll targets")
        target = targets["Targets"][0]
        ensure(target.get("Arn") == FUNCTION and target.get("RetryPolicy") == {"MaximumEventAgeInSeconds": 60, "MaximumRetryAttempts": 0}, "Drill poll target must use reviewed bounded retries")
        ensure(set(target) <= {"Id", "Arn", "RetryPolicy"}, "Unreviewed poll target overrides")
        invoke = self.aws.call("lambda", "get-function-event-invoke-config", function_name=NAME)
        ensure(invoke.get("MaximumEventAgeInSeconds") == 60 and invoke.get("MaximumRetryAttempts") == 1 and not any(invoke.get("DestinationConfig", {}).values()), "Unexpected fixture async invocation limits/destinations")
        policy_response = self.aws.call("lambda", "get-policy", function_name=NAME)
        permission = poll_permission(policy_response)
        inline = self.aws.call("iam", "list-role-policies", role_name=NAME)
        attached = self.aws.call("iam", "list-attached-role-policies", role_name=NAME)
        ensure(inline.get("PolicyNames") == ["observe-only-isolated-fixture"] and not inline.get("IsTruncated") and not attached.get("AttachedPolicies") and not attached.get("IsTruncated"), "Observer role has unexpected policy grants")
        role_policy = self.aws.call("iam", "get-role-policy", role_name=NAME, policy_name="observe-only-isolated-fixture")["PolicyDocument"]
        ecs_grants = [s for s in role_policy["Statement"] if "ecs:DescribeServices" in (s["Action"] if isinstance(s["Action"], list) else [s["Action"]])]
        ensure(len(ecs_grants) == 1 and ecs_grants[0]["Effect"] == "Allow" and ecs_grants[0]["Resource"] == [f"arn:aws:ecs:{REGION}:{ACCOUNT}:service/{CLUSTER}/{SERVICE}"], "Read-denial fault requires an exact single-service IAM grant")
        allowed_actions = {a for s in role_policy["Statement"] for a in (s["Action"] if isinstance(s["Action"], list) else [s["Action"]])}
        ensure(allowed_actions == {"ecs:DescribeServices", "logs:CreateLogStream", "logs:PutLogEvents", "cloudwatch:PutMetricData", "sns:Publish"}, "Observer has unreviewed role actions")
        queue = self.aws.call("sqs", "get-queue-attributes", queue_url=QUEUE, attribute_names=["QueueArn", "Policy"])["Attributes"]
        ensure(queue["QueueArn"] == QUEUE_ARN, "Wrong evidence queue")
        queue_statements = json.loads(queue["Policy"])["Statement"]
        ensure(len(queue_statements) == 1 and queue_statements[0].get("Effect") == "Allow" and queue_statements[0]["Principal"] == {"Service": "sns.amazonaws.com"} and queue_statements[0]["Action"] == "sqs:SendMessage" and queue_statements[0]["Resource"] == QUEUE_ARN and queue_statements[0]["Condition"] == {"ArnEquals": {"aws:SourceArn": MIRROR}, "StringEquals": {"aws:SourceAccount": ACCOUNT}}, "Queue must accept only the dedicated drill SNS topic")
        self.state = {"schema_version": 1, "capability": "isolated-observer-fault-drill", "run_id": str(uuid.uuid4()), "status": "prepared", "account": ACCOUNT, "region": REGION,
                      "started_at": self.clock().isoformat(), "baseline": {"function": FUNCTION, "code_hash": package_hash, "environment": config["Environment"]["Variables"], "permission": permission,
                      "rule": {k: rule.get(k) for k in ["Name", "Arn", "State", "ScheduleExpression", "EventBusName"]}, "target": target, "role_policy_sha256": digest(role_policy), "task_definition": service["taskDefinition"]},
                      "active_fault": None, "phases": [], "events": [], "notifications": []}
        if previous:
            ensure(previous.get("baseline") == self.state["baseline"], "Prepared baseline changed; create a newly reviewed evidence directory")
            self.state = previous
        self.save()
        self.wait_alarms({suffix: "OK" for suffix in ALARM_SUFFIXES})
        self.event("preflight_passed", function=FUNCTION, cluster=CLUSTER, queue=QUEUE_ARN, package_sha256=package_hash)

    def restore(self):
        fault = self.state.get("active_fault")
        if not fault:
            return
        ensure(self.state.get("capability") == "isolated-observer-fault-drill" and self.state.get("account") == ACCOUNT and self.state.get("region") == REGION, "Invalid restore journal identity")
        baseline = self.state["baseline"]
        ensure(baseline.get("function") == FUNCTION and re.fullmatch(r"[A-Za-z0-9+/]{43}=", baseline["code_hash"]), "Invalid restore journal function/hash")
        expected_env(baseline["environment"])
        validate_permission(baseline["permission"])
        if fault["kind"] == "read-denial":
            def settled():
                current = self.aws.call("lambda", "get-function-configuration", function_name=NAME)
                return current if current.get("LastUpdateStatus") != "InProgress" else None
            current = self.wait("in-flight function update settled for restore", settled)
            # Update failures can leave LastUpdateStatus Failed but the original
            # environment unchanged. Preserve any unrelated concurrent change.
            ensure(current.get("FunctionArn") == FUNCTION and current.get("CodeSha256") == baseline["code_hash"] and current.get("Role") == ROLE, "Function identity changed during fault")
            variables = current.get("Environment", {}).get("Variables", {})
            if variables != baseline["environment"]:
                expected_env(variables, denied=True)
                desired = copy.deepcopy(baseline["environment"])
                self.aws.call("lambda", "update-function-configuration", function_name=NAME, environment={"Variables": desired}, revision_id=current["RevisionId"])
                self.wait("original observer environment restored", lambda: self._function_ready(False))
        elif fault["kind"] == "delivery-failure":
            self.config()
            current = self.aws.call("lambda", "get-policy", function_name=NAME)
            if not poll_permission(current, required=False):
                self.aws.call("lambda", "add-permission", function_name=NAME, statement_id=STATEMENT, action="lambda:InvokeFunction", principal="events.amazonaws.com", source_arn=RULE_ARN, source_account=ACCOUNT, revision_id=current["RevisionId"])
                poll_permission(self.aws.call("lambda", "get-policy", function_name=NAME))
        elif fault["kind"] == "silence":
            self.config()
            current = self.aws.call("events", "describe-rule", name=RULE, event_bus_name="default")
            validate_rule(current, enabled=current.get("State") == "ENABLED")
            if current["State"] != "ENABLED":
                self.aws.call("events", "enable-rule", name=RULE, event_bus_name="default")
                validate_rule(self.aws.call("events", "describe-rule", name=RULE, event_bus_name="default"))
        else:
            raise ValueError("Unknown restore fault")
        self.state["active_fault"] = None
        self.event("fault_restored", kind=fault["kind"])

    def _function_ready(self, denied):
        current = self.aws.call("lambda", "get-function-configuration", function_name=NAME)
        if current.get("LastUpdateStatus") == "InProgress":
            return False
        validate_function(current, self.state["baseline"]["code_hash"], denied)
        return current

    def prepare_fault(self, kind):
        ensure(not self.state.get("active_fault"), "An earlier fault still requires restoration")
        self.config()
        validate_rule(self.aws.call("events", "describe-rule", name=RULE, event_bus_name="default"))
        poll_permission(self.aws.call("lambda", "get-policy", function_name=NAME))
        self.state["active_fault"] = {"kind": kind, "started_at": self.clock().isoformat()}
        self.event("fault_prepared", kind=kind)
        return timestamp(self.state["active_fault"]["started_at"])

    def phase(self, kind):
        since = self.prepare_fault(kind)
        try:
            if kind == "read-denial":
                current = self.config()
                variables = copy.deepcopy(self.state["baseline"]["environment"])
                variables["SERVICE_NAMES"] = json.dumps([DENIED_SERVICE], separators=(",", ":"))
                self.aws.call("lambda", "update-function-configuration", function_name=NAME, environment={"Variables": variables}, revision_id=current["RevisionId"])
                self.wait("read-denial environment active", lambda: self._function_ready(True))
                requirements = {"observer-failed": "ALARM", "observer-missing": "ALARM", "observer-events-dropped": "ALARM", "poll-delivery-failed": "OK", "stopped-delivery-failed": "OK"}
                self.wait_alarms(requirements, since)
                for metric in ["Errors", "AsyncEventsDropped"]:
                    self.require_metric("AWS/Lambda", metric, [{"Name": "FunctionName", "Value": NAME}], since)
                self.event("read_denial_cause_evidence", configured_service=DENIED_SERVICE, granted_service=SERVICE,
                           inference="Reviewed handler calls DescribeServices for the valid configured name outside its sole IAM resource grant; executed failures are confirmed by Lambda metrics. Raw SDK exception is intentionally redacted.")
            elif kind == "delivery-failure":
                policy = self.aws.call("lambda", "get-policy", function_name=NAME)
                poll_permission(policy)
                self.aws.call("lambda", "remove-permission", function_name=NAME, statement_id=STATEMENT, revision_id=policy["RevisionId"])
                ensure(poll_permission(self.aws.call("lambda", "get-policy", function_name=NAME), required=False) is None, "Poll permission was not removed")
                requirements = {"poll-delivery-failed": "ALARM", "observer-missing": "ALARM", "observer-failed": "OK", "observer-events-dropped": "OK", "stopped-delivery-failed": "OK"}
                self.wait_alarms(requirements, since)
                self.require_metric("AWS/Events", "FailedInvocations", [{"Name": "RuleName", "Value": RULE}], since)
            elif kind == "silence":
                self.aws.call("events", "disable-rule", name=RULE, event_bus_name="default")
                validate_rule(self.aws.call("events", "describe-rule", name=RULE, event_bus_name="default"), enabled=False)
                requirements = {"observer-missing": "ALARM", "observer-failed": "OK", "observer-events-dropped": "OK", "poll-delivery-failed": "OK", "stopped-delivery-failed": "OK"}
                self.wait_alarms(requirements, since)
            else:
                raise ValueError("Unknown fault kind")
            alarming = [ALARMS[name] for name, state in requirements.items() if state == "ALARM"]
            self.notifications(alarming, since)
            def published_actions():
                history = []
                for name in alarming:
                    response = self.aws.call("cloudwatch", "describe-alarm-history", alarm_name=name, history_item_type="Action", start_date=since.isoformat(), max_records=20)
                    for item in response.get("AlarmHistoryItems", []):
                        summary = item.get("HistorySummary", "")
                        for topic, destination in [(TOPIC, "operations"), (MIRROR, "drill_evidence")]:
                            if summary == "Successfully executed action " + topic and timestamp(item["Timestamp"]) >= since:
                                history.append({"alarm_name": name, "time": timestamp(item["Timestamp"]).isoformat(), "destination": destination, "action_succeeded": True})
                successful = {(a["alarm_name"], a["destination"]) for a in history}
                return history if all((name, destination) in successful for name in alarming for destination in ["operations", "drill_evidence"]) else None
            history = self.wait("successful operations and mirror alarm actions", published_actions, timeout=min(180, self.max_wait))
            self.event("alarm_action_history", actions=history)
            self.state["phases"].append({"kind": kind, "started_at": since.isoformat(), "qualified_at": self.clock().isoformat(), "requirements": requirements, "status": "awaiting-recovery"})
            self.save()
        finally:
            self.restore()
        self.wait_alarms({suffix: "OK" for suffix in ALARM_SUFFIXES})
        self.state["phases"][-1].update({"status": "passed", "recovered_at": self.clock().isoformat()})
        self.event("recovered_after_fault", kind=kind)

    def execute(self):
        self.state["status"] = "running"
        self.save()
        try:
            for kind in ["read-denial", "delivery-failure", "silence"]:
                self.phase(kind)
            self.state["status"] = "passed"
            self.event("drill_passed", phases=len(self.state["phases"]))
        except BaseException as error:
            self.state["status"] = "failed"
            self.state["failure_type"] = type(error).__name__
            self.save()
            raise
        finally:
            self.restore()


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--package", help="Reviewed observer zip; required for preflight/execute")
    parser.add_argument("--evidence-dir", required=True)
    parser.add_argument("--execute", action="store_true")
    parser.add_argument("--restore", action="store_true")
    parser.add_argument("--confirmation", default="")
    parser.add_argument("--max-wait-seconds", type=int, default=600)
    parser.add_argument("--poll-seconds", type=int, default=15)
    args = parser.parse_args(argv)
    ensure(not (args.execute and args.restore), "Choose execution or restoration")
    ensure(120 <= args.max_wait_seconds <= 900 and 5 <= args.poll_seconds <= 30, "Unreviewed wait bounds")
    ensure(pathlib.Path(args.evidence_dir).is_absolute(), "Evidence directory must be absolute")
    if args.execute or args.restore:
        ensure(args.confirmation == CONFIRMATION, "Exact confirmation is required")
    else:
        ensure(args.package, "--package is required")
    aws = AWS()
    drill = Drill(aws, args.evidence_dir, args.max_wait_seconds, args.poll_seconds)
    def terminate(_signum, _frame):
        raise InterruptedError("Termination requested")
    signal.signal(signal.SIGTERM, terminate)
    if args.restore:
        identity = aws.call("sts", "get-caller-identity")
        ensure(identity["Account"] == ACCOUNT, "Wrong restore account")
        drill.state = json.loads(drill.path.read_text())
        drill.restore()
        drill.wait_alarms({suffix: "OK" for suffix in ALARM_SUFFIXES})
        drill.event("interrupted_run_restored")
    else:
        ensure(args.package, "--package is required")
        drill.preflight(args.package)
        if args.execute:
            drill.execute()
        else:
            drill.event("read_only_plan_complete", pending_phases=["read-denial", "delivery-failure", "silence"])
    print(json.dumps({"status": drill.state["status"], "journal": str(drill.path)}), flush=True)


if __name__ == "__main__":
    try:
        main()
    except (ValueError, AWSFailure, RuntimeError, InterruptedError) as error:
        print(json.dumps({"status": "failed", "error": str(error)}), file=sys.stderr)
        sys.exit(1)
