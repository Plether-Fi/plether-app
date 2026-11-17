import copy
import importlib.util
import json
import pathlib
import tempfile
import unittest
import zipfile
from types import SimpleNamespace

ROOT = pathlib.Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("observer", pathlib.Path(__file__).with_name("handler.py"))
observer = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(observer)

ACCOUNT = "932542905614"
REGION = "ap-southeast-1"
PREFIX = f"arn:aws:ecs:{REGION}:{ACCOUNT}:"
CLUSTER = PREFIX + "cluster/plether-sepolia"
CONFIG = {"account": ACCOUNT, "region": REGION, "cluster": CLUSTER, "services": ["plether-aa-reconciler", "plether-api"],
          "arn_prefix": PREFIX, "log_group": "/fixture/evidence", "topic": "arn:aws:sns:ap-southeast-1:932542905614:operations", "escalation_topic": ""}
SECRET = "https://private.invalid/v2/secret-token?authorization=private-key"


def stopped():
    return {"source": "aws.ecs", "detail-type": "ECS Task State Change", "account": ACCOUNT, "region": REGION,
            "id": "f1e0e19e-49c4-4014-9e67-c5e739c1d3fe", "time": "2026-11-17T06:13:34Z", "detail": {
                "clusterArn": CLUSTER, "lastStatus": "STOPPED", "group": "service:plether-aa-reconciler",
                "taskArn": PREFIX + "task/plether-sepolia/1234567890abcdef1234567890abcdef",
                "taskDefinitionArn": PREFIX + "task-definition/plether-sepolia-aa-reconciler:67",
                "stopCode": "TaskFailedToStart", "stoppedReason": "CannotPullContainerError " + SECRET,
                "overrides": {"containerOverrides": [{"environment": [{"name": "SECRET", "value": SECRET}]}]},
                "containers": [{"name": "plether-aa-reconciler", "imageDigest": "sha256:" + "a" * 64,
                                "exitCode": 1, "lastStatus": "STOPPED", "reason": SECRET,
                                "environment": [{"name": "SECRET", "value": SECRET}], "image": SECRET}],
            }}


class FakeLogs:
    class AlreadyExists(Exception):
        pass

    exceptions = SimpleNamespace(ResourceAlreadyExistsException=AlreadyExists)

    def __init__(self):
        self.events = []

    def create_log_stream(self, **_args):
        pass

    def put_log_events(self, **args):
        self.events.extend(args["logEvents"])


class FakeSNS:
    def __init__(self):
        self.messages = []

    def publish(self, **args):
        self.messages.append(args)


class FakeCloudWatch:
    def __init__(self):
        self.calls = []

    def put_metric_data(self, **args):
        self.calls.append(args)


class FakeECS:
    def __init__(self, services, fail_batch=None):
        self.services = services
        self.requests = []
        self.fail_batch = fail_batch

    def describe_services(self, **args):
        self.requests.append(args)
        if len(self.requests) == self.fail_batch:
            return {"services": [], "failures": [{"reason": SECRET}]}
        return {"services": [self.services[n] for n in args["services"]], "failures": []}


def service(name, desired=1, running=1, pending=0):
    return {"serviceName": name, "clusterArn": CLUSTER, "status": "ACTIVE", "desiredCount": desired,
            "runningCount": running, "pendingCount": pending, "deployments": [{"secrets": SECRET}]}


class IncidentObserverTests(unittest.TestCase):
    def test_projection_persists_only_allowlisted_evidence(self):
        event = stopped()
        result = observer.project_stopped_task(event, CONFIG)
        self.assertEqual(result["reason_category"], "IMAGE_PULL_FAILED")
        self.assertEqual(result["containers"][0]["exit_code"], 1)
        self.assertEqual(result["containers"][0]["image_digest"], "sha256:" + "a" * 64)
        self.assertEqual(result["occurred_at"], "2026-11-17T06:13:34Z")
        encoded = json.dumps(result)
        for forbidden in [SECRET, "secret-token", "private-key", "overrides", "environment", '"image"', '"stoppedReason"']:
            self.assertNotIn(forbidden, encoded)
        self.assertEqual(event, stopped(), "Projection must not mutate source event")

    def test_spoofed_or_irrelevant_events_are_ignored(self):
        variants = [({}, None), ({"account": "000000000000"}, None), ({"region": "us-east-1"}, None),
                    ({"source": "untrusted"}, None), ({"detail-type": "different"}, None)]
        for replacement, _ in variants[1:]:
            event = stopped()
            event.update(replacement)
            self.assertIsNone(observer.project_stopped_task(event, CONFIG))
        for key, value in [("clusterArn", PREFIX + "cluster/another"), ("group", "service:unconfigured"),
                           ("lastStatus", "RUNNING"), ("taskArn", SECRET), ("taskDefinitionArn", SECRET)]:
            event = stopped()
            event["detail"][key] = value
            self.assertIsNone(observer.project_stopped_task(event, CONFIG), key)

    def test_arbitrary_reason_and_container_fields_cannot_leak(self):
        event = stopped()
        event["detail"]["stoppedReason"] = SECRET
        event["detail"]["stopCode"] = SECRET
        event["detail"]["containers"].extend([{"name": SECRET}, {"name": "router", "imageDigest": SECRET, "exitCode": SECRET, "lastStatus": SECRET}])
        result = observer.project_stopped_task(event, CONFIG)
        self.assertEqual(result["reason_category"], "OTHER")
        self.assertEqual(result["stop_code"], "OTHER")
        self.assertEqual(result["containers"][1], {"name": "router"})
        self.assertNotIn(SECRET, json.dumps(result))

    def test_stop_notifications_use_sanitized_evidence_and_distinct_topics(self):
        config = {**CONFIG, "escalation_topic": CONFIG["topic"]}
        logs, sns = FakeLogs(), FakeSNS()
        observer.run(stopped(), config, {"logs": logs, "sns": sns})
        self.assertEqual(len(sns.messages), 1)
        self.assertNotIn(SECRET, json.dumps(sns.messages + logs.events))
        self.assertEqual(json.loads(logs.events[0]["message"])["reason_category"], "IMAGE_PULL_FAILED")

    def test_infrastructure_health_stop_is_classified_and_alerted(self):
        event = stopped()
        event["detail"]["stopCode"] = "InfrastructureHealth"
        event["detail"]["stoppedReason"] = SECRET
        result = observer.project_stopped_task(event, CONFIG)
        self.assertEqual(result["reason_category"], "INFRASTRUCTURE_HEALTH")
        self.assertTrue(observer.actionable_stop(result))

    def test_planned_stop_is_retained_without_failure_notification(self):
        event = stopped()
        event["detail"]["stopCode"] = "ServiceSchedulerInitiated"
        event["detail"]["stoppedReason"] = "Deployment replaced this task " + SECRET
        event["detail"]["containers"][0]["exitCode"] = 143
        logs, sns = FakeLogs(), FakeSNS()
        observer.run(event, CONFIG, {"logs": logs, "sns": sns})
        self.assertEqual(len(logs.events), 1)
        self.assertEqual(sns.messages, [])

    def test_deficits_use_desired_minus_running_including_pending(self):
        ecs = FakeECS({"plether-aa-reconciler": service("plether-aa-reconciler", running=0, pending=1),
                       "plether-api": service("plether-api", desired=0, running=0)})
        cw = FakeCloudWatch()
        result = observer.observe_services(ecs, cw, CONFIG)
        self.assertEqual(result["services"][0]["deficit"], 1)
        self.assertEqual(result["services"][1]["deficit"], 0)
        self.assertEqual(len(cw.calls), 1)
        self.assertEqual(cw.calls[0]["MetricData"][-1]["MetricName"], "ServiceObservationCompleted")
        self.assertNotIn(SECRET, json.dumps(result))

    def test_incomplete_second_batch_publishes_no_healthy_metrics(self):
        names = [f"service-{n:02d}" for n in range(11)]
        config = {**CONFIG, "services": names}
        ecs, cw = FakeECS({n: service(n) for n in names}, fail_batch=2), FakeCloudWatch()
        with self.assertRaisesRegex(ValueError, "incomplete"):
            observer.observe_services(ecs, cw, config)
        self.assertEqual([len(r["services"]) for r in ecs.requests], [10, 1])
        self.assertEqual(cw.calls, [])

    def test_counts_and_cluster_must_be_valid(self):
        for replacement in [{"runningCount": -1}, {"pendingCount": True}, {"status": "INACTIVE"}, {"clusterArn": "another"}]:
            services = {n: service(n) for n in CONFIG["services"]}
            services[CONFIG["services"][0]].update(replacement)
            with self.assertRaisesRegex(ValueError, "invalid service"):
                observer.observe_services(FakeECS(services), FakeCloudWatch(), CONFIG)

    def test_package_is_deterministic_and_contains_only_source(self):
        spec = importlib.util.spec_from_file_location("package", ROOT / "scripts/package-incident-observer.py")
        package = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(package)
        with tempfile.TemporaryDirectory() as directory:
            first, second = pathlib.Path(directory) / "first.zip", pathlib.Path(directory) / "second.zip"
            package.package(first)
            package.package(second)
            self.assertEqual(first.read_bytes(), second.read_bytes())
            with zipfile.ZipFile(first) as archive:
                self.assertEqual(archive.namelist(), ["handler.py"])
                self.assertEqual(archive.read("handler.py"), pathlib.Path(__file__).with_name("handler.py").read_bytes())


if __name__ == "__main__":
    unittest.main()
