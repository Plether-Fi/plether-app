"""Read-only ECS incident observer. Never log the incoming AWS event."""
import datetime
import json
import os
import re
import time

NAME = re.compile(r"^[A-Za-z0-9_-]{1,255}$")
DIGEST = re.compile(r"^sha256:[0-9a-f]{64}$")
STOP_CODES = {"TaskFailedToStart", "EssentialContainerExited", "UserInitiated", "ServiceSchedulerInitiated", "SpotInterruption", "TerminationNotice", "InfrastructureHealth"}
STATUSES = {"PROVISIONING", "PENDING", "ACTIVATING", "RUNNING", "DEACTIVATING", "STOPPING", "DEPROVISIONING", "STOPPED"}
REASONS = (
    ("IMAGE_PULL_FAILED", ("cannotpullcontainererror", "cannotpullimage", "image manifest", "pull access denied"), "The container image could not be pulled."),
    ("RESOURCE_INITIALIZATION_FAILED", ("resourceinitializationerror",), "Task resources could not be initialized."),
    ("OUT_OF_MEMORY", ("outofmemory", "out of memory"), "A container exceeded available memory."),
    ("TASK_RETIREMENT", ("retirement", "maintenance", "retiring"), "The task was retired for infrastructure maintenance."),
    ("ESSENTIAL_CONTAINER_EXITED", ("essential container",), "An essential container exited."),
    ("HEALTH_CHECK_FAILED", ("health check", "healthcheck"), "A task health check failed."),
)


def reason_category(value):
    # Free-form stop/reason strings can contain operator-supplied credentials or
    # provider URLs. Classify them in memory; persist only fixed summaries.
    lowered = value.lower() if isinstance(value, str) else ""
    for category, needles, summary in REASONS:
        if any(needle in lowered for needle in needles):
            return category, summary
    return "OTHER", "Task stopped; inspect its service events and restricted application logs."


def configuration(environ=os.environ):
    account = environ["EXPECTED_ACCOUNT_ID"]
    region = environ["AWS_REGION"]
    cluster = environ["ECS_CLUSTER_ARN"]
    services = json.loads(environ["SERVICE_NAMES"])
    if not re.fullmatch(r"[0-9]{12}", account) or not re.fullmatch(r"[a-z0-9-]+", region):
        raise ValueError("invalid observer target")
    prefix = f"arn:aws:ecs:{region}:{account}:"
    if not cluster.startswith(prefix + "cluster/") or not services or not all(isinstance(n, str) and NAME.fullmatch(n) for n in services):
        raise ValueError("invalid observer configuration")
    return {"account": account, "region": region, "cluster": cluster, "services": sorted(set(services)), "arn_prefix": prefix,
            "log_group": environ["EVIDENCE_LOG_GROUP"], "topic": environ["OPERATIONS_TOPIC_ARN"], "escalation_topic": environ.get("ESCALATION_TOPIC_ARN", "")}


def project_stopped_task(event, config):
    if event.get("source") != "aws.ecs" or event.get("detail-type") != "ECS Task State Change" or event.get("account") != config["account"] or event.get("region") != config["region"]:
        return None
    detail = event.get("detail", {})
    if detail.get("clusterArn") != config["cluster"] or detail.get("lastStatus") != "STOPPED":
        return None
    group = detail.get("group", "")
    if group not in {"service:" + n for n in config["services"]}:
        return None
    service = group.removeprefix("service:")
    task = detail.get("taskArn", "")
    revision = detail.get("taskDefinitionArn", "")
    if not re.fullmatch(re.escape(config["arn_prefix"]) + r"task/[A-Za-z0-9_/-]+", task) or not re.fullmatch(re.escape(config["arn_prefix"]) + r"task-definition/[A-Za-z0-9_-]+:[0-9]+", revision):
        return None
    category, summary = reason_category(detail.get("stoppedReason"))
    if category == "OTHER" and detail.get("stopCode") == "InfrastructureHealth":
        category, summary = "INFRASTRUCTURE_HEALTH", "The task infrastructure became unhealthy."
    containers = []
    for raw in detail.get("containers", [])[:20]:
        name = raw.get("name")
        if not isinstance(name, str) or not NAME.fullmatch(name):
            continue
        container = {"name": name}
        if DIGEST.fullmatch(str(raw.get("imageDigest", ""))):
            container["image_digest"] = raw["imageDigest"]
        if type(raw.get("exitCode")) is int and 0 <= raw["exitCode"] <= 255:
            container["exit_code"] = raw["exitCode"]
        if raw.get("lastStatus") in STATUSES:
            container["status"] = raw["lastStatus"]
        if raw.get("reason"):
            container["reason_category"] = reason_category(raw["reason"])[0]
        containers.append(container)
    record = {"schema_version": 1, "event": "ecs_task_stopped", "service": service, "task_arn": task,
            "task_definition_arn": revision, "stop_code": detail.get("stopCode") if detail.get("stopCode") in STOP_CODES else "OTHER",
            "reason_category": category, "reason_summary": summary, "containers": containers}
    if isinstance(event.get("id"), str) and re.fullmatch(r"[0-9a-fA-F-]{36}", event["id"]):
        record["event_id"] = event["id"]
    if isinstance(event.get("time"), str) and re.fullmatch(r"[0-9TZ:+.-]{20,32}", event["time"]):
        record["occurred_at"] = event["time"]
    return record


def actionable_stop(record):
    return record["stop_code"] in {"TaskFailedToStart", "EssentialContainerExited", "SpotInterruption", "TerminationNotice", "InfrastructureHealth"} or record["reason_category"] in {"IMAGE_PULL_FAILED", "RESOURCE_INITIALIZATION_FAILED", "OUT_OF_MEMORY", "HEALTH_CHECK_FAILED"}


def observe_services(ecs, cloudwatch, config):
    # Explicit configured names avoid ListServices permission and do not silently
    # omit stopped/dormant services. DescribeServices accepts at most ten names.
    observations = []
    for offset in range(0, len(config["services"]), 10):
        batch = config["services"][offset:offset + 10]
        response = ecs.describe_services(cluster=config["cluster"], services=batch)
        returned = {s["serviceName"]: s for s in response.get("services", [])}
        if len(returned) != len(response.get("services", [])) or any(n not in batch for n in returned):
            raise ValueError("unexpected service response")
        failures = response.get("failures", [])
        if failures or set(returned) != set(batch):
            # An incomplete API result must not be represented as healthy.
            raise ValueError("incomplete service observation")
        for name in batch:
            s = returned[name]
            counts = [s.get(k) for k in ("desiredCount", "runningCount", "pendingCount")]
            if s.get("clusterArn") != config["cluster"] or s.get("status") != "ACTIVE" or not all(type(v) is int and v >= 0 for v in counts):
                raise ValueError("invalid service observation")
            desired, running, pending = counts
            observations.append({"service": name, "desired": desired, "running": running, "pending": pending, "deficit": max(0, desired - running)})
    # Publish only after all batches passed, so a partial read cannot clear alarms.
    cloudwatch.put_metric_data(Namespace="Plether/Operations", MetricData=[{
        "MetricName": "ServiceTaskDeficit", "Dimensions": [{"Name": "ClusterName", "Value": config["cluster"].rsplit("/", 1)[-1]}, {"Name": "ServiceName", "Value": o["service"]}],
        "Value": o["deficit"], "Unit": "Count",
    } for o in observations] + [{"MetricName": "ServiceObservationCompleted", "Dimensions": [{"Name": "ClusterName", "Value": config["cluster"].rsplit("/", 1)[-1]}], "Value": 1, "Unit": "Count"}])
    return {"schema_version": 1, "event": "ecs_service_observation", "services": observations}


def record_evidence(logs, config, records, timestamp_ms=None):
    now = datetime.datetime.now(datetime.timezone.utc)
    stream = now.strftime("%Y/%m/%d")
    try:
        logs.create_log_stream(logGroupName=config["log_group"], logStreamName=stream)
    except logs.exceptions.ResourceAlreadyExistsException:
        pass
    timestamp_ms = timestamp_ms if timestamp_ms is not None else int(time.time() * 1000)
    logs.put_log_events(logGroupName=config["log_group"], logStreamName=stream,
                        logEvents=[{"timestamp": timestamp_ms, "message": json.dumps(r, separators=(",", ":"), sort_keys=True)} for r in records])


def run(event, config, clients):
    if event.get("source") == "aws.events" and event.get("detail-type") == "Scheduled Event":
        if event.get("account") != config["account"] or event.get("region") != config["region"]:
            return {"accepted": False}
        record = observe_services(clients["ecs"], clients["cloudwatch"], config)
    else:
        record = project_stopped_task(event, config)
        if record is None:
            return {"accepted": False}
    record_evidence(clients["logs"], config, [record])
    if record["event"] == "ecs_task_stopped" and actionable_stop(record):
        # Notification contains the same bounded, credential-free projection.
        for topic in dict.fromkeys(filter(None, [config["topic"], config.get("escalation_topic")])):
            clients["sns"].publish(TopicArn=topic, Subject="Plether ECS task failure", Message=json.dumps(record, sort_keys=True))
    return {"accepted": True, "event": record["event"]}


def handler(event, _context):
    import boto3
    config = configuration()
    clients = {name: boto3.client(name) for name in ("ecs", "cloudwatch", "logs", "sns")}
    try:
        return run(event, config, clients)
    except Exception:
        # Do not stringify SDK errors or the event: either can contain sensitive
        # input. Lambda Errors + missing per-service metrics alert on this path.
        print('{"event":"incident_observer_failed","reason":"OBSERVATION_FAILED"}')
        raise RuntimeError("incident observer failed") from None
