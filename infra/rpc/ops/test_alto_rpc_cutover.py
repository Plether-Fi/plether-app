"""Policy and stop-before-start regressions; no AWS or network calls."""
import copy
from datetime import datetime, timezone
import json
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from alto_rpc_cutover import (
    GuardRejected, require_baseline, patch_definition, assert_only_rpc_patch,
    canary_definition, idle_canary_definition, validate_service, validate_scan,
    replace_service, perform_cutover, fingerprint, Deployment,
)

ACCOUNT = "932542905614"
REGION = "ap-southeast-1"
PREFIX = f"arn:aws:ssm:{REGION}:{ACCOUNT}:parameter/plether/sepolia/"
IMAGE = f"{ACCOUNT}.dkr.ecr.{REGION}.amazonaws.com/plether-alto-sepolia@sha256:" + "a" * 64
LEGACY_IMAGE = f"{ACCOUNT}.dkr.ecr.{REGION}.amazonaws.com/plether-alto-sepolia@sha256:9db94fbd439a26f01b0ece3cc5f76b3791c1e1660b990d74892274267096f12a"
SOURCE = "b" * 40
BASELINE_URL = "http://internal-plether-sepolia-alto-123.ap-southeast-1.elb.amazonaws.com"
BEFORE = f"arn:aws:ecs:{REGION}:{ACCOUNT}:task-definition/plether-sepolia-alto:6"
AFTER = f"arn:aws:ecs:{REGION}:{ACCOUNT}:task-definition/plether-sepolia-alto:7"


def fixture():
    return {
        "family": "plether-sepolia-alto",
        "taskDefinitionArn": f"arn:aws:ecs:{REGION}:{ACCOUNT}:task-definition/plether-sepolia-alto:6",
        "revision": 6, "status": "ACTIVE", "networkMode": "awsvpc",
        "requiresCompatibilities": ["FARGATE"],
        "runtimePlatform": {"cpuArchitecture": "ARM64", "operatingSystemFamily": "LINUX"},
        "executionRoleArn": f"arn:aws:iam::{ACCOUNT}:role/plether-sepolia-alto-execution",
        "taskRoleArn": f"arn:aws:iam::{ACCOUNT}:role/plether-sepolia-alto-task",
        "cpu": "1024", "memory": "2048", "volumes": [{"name": "alto-tmp"}],
        "containerDefinitions": [
            {
                "name": "plether-alto", "image": LEGACY_IMAGE,
                "entryPoint": ["node", "/app/src/esm/cli/alto.js"], "command": ["run"],
                "essential": True, "readonlyRootFilesystem": True, "user": "1000:1000", "stopTimeout": 120,
                "linuxParameters": {"initProcessEnabled": True, "capabilities": {"add": [], "drop": ["ALL"]}},
                "portMappings": [{"containerPort": 3000, "hostPort": 3000, "protocol": "tcp"}],
                "mountPoints": [{"sourceVolume": "alto-tmp", "containerPath": "/tmp", "readOnly": False}],
                "environment": [
                    {"name": "ALTO_SAFE_MODE", "value": "false"},
                    {"name": "PLETHER_ALTO_NETWORK_CHAIN_ID", "value": "421614"},
                    {"name": "PLETHER_ALTO_VALIDATION_POLICY", "value": "sepolia-testnet-exception-v1"},
                    {"name": "ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION", "value": "false"},
                    {"name": "ALTO_ENABLE_DEBUG_ENDPOINTS", "value": "false"},
                    {"name": "ALTO_FLUSH_STUCK_TRANSACTIONS_DURING_STARTUP", "value": "false"},
                    {"name": "ALTO_ENTRYPOINTS", "value": "0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108"},
                    {"name": "ALTO_MAX_GAS_PER_USER_OP", "value": "25000000"},
                    {"name": "ALTO_MAX_GAS_PER_BUNDLE", "value": "30000000"},
                    {"name": "ALTO_BLOCK_TIME", "value": "1000"},
                    {"name": "ALTO_MIN_EXECUTOR_BALANCE", "value": "5000000000000000"},
                ],
                "secrets": [
                    {"name": "ALTO_RPC_URL", "valueFrom": PREFIX + "perps-rpc-url"},
                    {"name": "ALTO_EXECUTOR_PRIVATE_KEYS", "valueFrom": PREFIX + "alto-executor-private-keys"},
                    {"name": "ALTO_UTILITY_PRIVATE_KEY", "valueFrom": PREFIX + "alto-utility-private-key"},
                ],
            },
            {"name": "alto-tmp-init", "image": LEGACY_IMAGE, "essential": False,
             "entryPoint": ["/bin/sh", "-c"], "command": ["chmod 1777 /tmp"], "environment": [], "secrets": []},
            {"name": "otel-log-router", "image": f"{ACCOUNT}.dkr.ecr.{REGION}.amazonaws.com/plether-otel-log-router-sepolia@sha256:" + "c" * 64,
             "essential": True, "environment": [{"name": "SERVICE_VERSION", "value": "alto-v1.2.7"}],
             "secrets": [{"name": "POSTHOG_OTLP_AUTHORIZATION_HEADER", "valueFrom": PREFIX + "posthog-otlp-authorization-header"}]},
        ],
    }


def service_fixture():
    return {"serviceName": "plether-alto", "clusterArn": f"arn:aws:ecs:{REGION}:{ACCOUNT}:cluster/plether-sepolia",
            "taskDefinition": fixture()["taskDefinitionArn"], "desiredCount": 1, "runningCount": 1, "pendingCount": 0,
            "deployments": [{"rolloutState": "COMPLETED", "taskDefinition": fixture()["taskDefinitionArn"]}],
            "launchType": "FARGATE", "platformVersion": "1.4.0",
            "deploymentConfiguration": {"minimumHealthyPercent": 0, "maximumPercent": 100,
                "deploymentCircuitBreaker": {"enable": True, "rollback": True}},
            "networkConfiguration": {"awsvpcConfiguration": {"subnets": ["subnet-0123456789abcdef0"],
                "securityGroups": ["sg-0123456789abcdef0"], "assignPublicIp": "ENABLED"}}}


def main_container(definition):
    return next(container for container in definition["containerDefinitions"] if container["name"] == "plether-alto")


def scan_fixture(now=1_794_213_000):
    recent = datetime.fromtimestamp(now - 30, timezone.utc)
    return {"registryId": ACCOUNT, "repositoryName": "plether-alto-sepolia",
            "imageId": {"imageDigest": IMAGE.split("@")[1]},
            "imageScanStatus": {"status": "COMPLETE"},
            "imageScanFindings": {"imageScanCompletedAt": recent, "vulnerabilitySourceUpdatedAt": recent,
                "findingSeverityCounts": {}, "findings": []}}


class RpcCutoverPolicyTests(unittest.TestCase):
    def test_patch_changes_only_rpc_auth_entrypoint_and_both_payload_images(self):
        before = fixture()
        original = copy.deepcopy(before)
        after = patch_definition(before, IMAGE)
        self.assertEqual(before, original, "Preparation must not mutate the baseline used for rollback")
        assert_only_rpc_patch(before, after, IMAGE)
        alto = main_container(after)
        self.assertEqual(alto["entryPoint"], ["node", "--import", "/app/plether-rpc-auth.mjs", "/app/src/esm/cli/alto.js"])
        self.assertEqual(alto["command"], ["run"])
        refs = {item["name"]: item["valueFrom"] for item in alto["secrets"]}
        self.assertEqual(refs["ALTO_RPC_URL"], PREFIX + "app-rpc-url")
        self.assertEqual(refs["PLETHER_ALTO_RPC_AUTH_TOKEN"], PREFIX + "rpc-gateway-token")
        self.assertEqual(refs["ALTO_EXECUTOR_PRIVATE_KEYS"], PREFIX + "alto-executor-private-keys")
        self.assertEqual(refs["ALTO_UTILITY_PRIVATE_KEY"], PREFIX + "alto-utility-private-key")
        by_name = {item["name"]: item for item in after["containerDefinitions"]}
        self.assertEqual(by_name["plether-alto"]["image"], IMAGE)
        self.assertEqual(by_name["alto-tmp-init"]["image"], IMAGE)
        self.assertEqual(by_name["otel-log-router"], before["containerDefinitions"][2])

    def test_candidate_rejects_any_unapproved_policy_role_signer_or_network_change(self):
        before = fixture()
        mutations = [
            lambda value: value.update(taskRoleArn=f"arn:aws:iam::{ACCOUNT}:role/unexpected"),
            lambda value: value.update(executionRoleArn=f"arn:aws:iam::{ACCOUNT}:role/unexpected"),
            lambda value: value.update(cpu="2048"),
            lambda value: value.update(memory="4096"),
            lambda value: value.update(volumes=[]),
            lambda value: main_container(value).update(user="0:0"),
            lambda value: main_container(value).update(readonlyRootFilesystem=False),
            lambda value: main_container(value)["secrets"][1].update(valueFrom=PREFIX + "different-signers"),
            lambda value: main_container(value)["environment"].append({"name": "NEW_POLICY", "value": "true"}),
            lambda value: value["containerDefinitions"][2].update(image=IMAGE),
        ]
        for key in ["ALTO_MAX_GAS_PER_USER_OP", "ALTO_MAX_GAS_PER_BUNDLE", "ALTO_SAFE_MODE",
                    "ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION", "ALTO_BLOCK_TIME"]:
            mutations.append(lambda value, key=key: next(item for item in main_container(value)["environment"]
                if item["name"] == key).update(value="changed"))
        for index, mutate in enumerate(mutations):
            with self.subTest(case=index):
                after = patch_definition(before, IMAGE)
                mutate(after)
                with self.assertRaises(GuardRejected):
                    assert_only_rpc_patch(before, after, IMAGE)

    def test_baseline_rejects_scope_topology_and_ambiguous_rpc_sources(self):
        mutations = [
            lambda value: value.update(family="plether-mainnet-alto"),
            lambda value: value.update(taskDefinitionArn=value["taskDefinitionArn"].replace(ACCOUNT, "000000000000")),
            lambda value: value.update(executionRoleArn=f"arn:aws:iam::{ACCOUNT}:role/other"),
            lambda value: value.update(networkMode="bridge"),
            lambda value: value["runtimePlatform"].update(cpuArchitecture="X86_64"),
            lambda value: value["containerDefinitions"].append(copy.deepcopy(main_container(value))),
            lambda value: main_container(value).update(entryPoint=["sh", "-c", "alto"]),
            lambda value: main_container(value)["secrets"].append({"name": "ALTO_SEND_TRANSACTION_RPC_URL", "valueFrom": PREFIX + "another-rpc"}),
            lambda value: main_container(value)["environment"].append({"name": "ALTO_RPC_URL", "value": "https://unreviewed.invalid"}),
            lambda value: main_container(value)["secrets"].append(copy.deepcopy(main_container(value)["secrets"][0])),
            lambda value: main_container(value)["environment"].append(copy.deepcopy(main_container(value)["environment"][0])),
            lambda value: main_container(value)["secrets"][1].update(valueFrom=(PREFIX + "alto-executor-private-keys").replace(ACCOUNT, "000000000000")),
        ]
        for index, mutate in enumerate(mutations):
            before = fixture()
            mutate(before)
            with self.subTest(case=index), self.assertRaises(GuardRejected):
                require_baseline(before)

    def test_baseline_rejects_missing_or_nonpositive_transaction_gas_budgets(self):
        for key in ["ALTO_MAX_GAS_PER_USER_OP", "ALTO_MAX_GAS_PER_BUNDLE"]:
            for value in [None, "0", "-1", "1.5", "NaN", "25000000\n"]:
                before = fixture()
                env = main_container(before)["environment"]
                env[:] = [item for item in env if item["name"] != key]
                if value is not None:
                    env.append({"name": key, "value": value})
                with self.subTest(key=key, value=value), self.assertRaises(GuardRejected):
                    require_baseline(before)

    def test_candidate_image_requires_exact_account_repository_and_immutable_digest(self):
        for image in [IMAGE.replace(ACCOUNT, "000000000000"), IMAGE.replace("plether-alto-sepolia", "other"),
                      IMAGE.split("@")[0] + ":latest", IMAGE[:-1], IMAGE + "?override=true"]:
            with self.subTest(image=image), self.assertRaises(GuardRejected):
                patch_definition(fixture(), image)

    def test_canary_has_no_signing_key_no_alto_server_and_no_runtime_task_role(self):
        prepared = patch_definition(fixture(), IMAGE)
        canary = canary_definition(prepared, SOURCE, "console.log('fixture canary')", BASELINE_URL)
        self.assertEqual(canary["executionRoleArn"], prepared["executionRoleArn"])
        self.assertFalse(canary.get("taskRoleArn"))
        self.assertEqual(len(canary["containerDefinitions"]), 1)
        container = canary["containerDefinitions"][0]
        refs = {item["name"]: item["valueFrom"] for item in container.get("secrets", [])}
        self.assertNotIn("ALTO_EXECUTOR_PRIVATE_KEYS", refs)
        self.assertNotIn("ALTO_UTILITY_PRIVATE_KEY", refs)
        self.assertNotIn("POSTHOG_OTLP_AUTHORIZATION_HEADER", refs)
        self.assertEqual(refs.get("ALTO_RPC_URL"), PREFIX + "app-rpc-url")
        self.assertEqual(refs.get("PLETHER_ALTO_RPC_AUTH_TOKEN"), PREFIX + "rpc-gateway-token")
        self.assertTrue(set(refs) <= {"ALTO_RPC_URL", "PLETHER_ALTO_RPC_AUTH_TOKEN", "PLETHER_ALTO_BACKUP_RPC_URL"})
        if "PLETHER_ALTO_BACKUP_RPC_URL" in refs:
            self.assertEqual(refs["PLETHER_ALTO_BACKUP_RPC_URL"], PREFIX + "perps-rpc-url")
        environment = {item["name"]: item["value"] for item in container.get("environment", [])}
        self.assertEqual(environment.get("PLETHER_ALTO_BASELINE_URL"), BASELINE_URL)
        self.assertEqual(environment.get("PLETHER_ALTO_QUALIFY_MODE"), "rpc")
        self.assertEqual(container["image"], IMAGE)
        self.assertNotIn("run", container.get("command", []))
        self.assertFalse(container.get("portMappings"))
        self.assertNotIn("/app/src/esm/cli/alto.js", container.get("entryPoint", []))

    def test_canary_rejects_credentials_and_arbitrary_origins_for_the_private_metrics_read(self):
        prepared = patch_definition(fixture(), IMAGE)
        for url in ["https://example.com", "http://token@internal-plether-sepolia-alto-123.ap-southeast-1.elb.amazonaws.com",
                    BASELINE_URL + "/metrics", BASELINE_URL + "?token=secret", BASELINE_URL + "#fragment"]:
            with self.subTest(url=url), self.assertRaises(GuardRejected):
                canary_definition(prepared, SOURCE, "console.log('fixture canary')", url)

    def test_idle_observer_has_no_credentials_or_preload_and_cannot_run_the_bundler(self):
        prepared = patch_definition(fixture(), IMAGE)
        idle = idle_canary_definition(prepared, SOURCE, "console.log('idle fixture')", BASELINE_URL)
        self.assertFalse(idle.get("taskRoleArn"))
        self.assertEqual(len(idle["containerDefinitions"]), 1)
        observer = idle["containerDefinitions"][0]
        self.assertEqual(observer.get("secrets"), [])
        self.assertEqual(observer["entryPoint"], ["node", "--input-type=module", "-e"])
        self.assertEqual(observer["command"], ["console.log('idle fixture')"])
        self.assertEqual({item["name"]: item["value"] for item in observer["environment"]}, {
            "PLETHER_ALTO_BASELINE_URL": BASELINE_URL, "PLETHER_ALTO_QUALIFY_MODE": "idle"})
        self.assertFalse(observer.get("portMappings"))

    def test_stable_service_requires_single_signer_process_and_stop_before_start_configuration(self):
        validate_service(service_fixture())
        for mutate in [lambda value: value.update(desiredCount=2),
                       lambda value: value.update(runningCount=2),
                       lambda value: value.update(pendingCount=1),
                       lambda value: value["deployments"].append(copy.deepcopy(value["deployments"][0])),
                       lambda value: value["deploymentConfiguration"].update(minimumHealthyPercent=100),
                       lambda value: value["deploymentConfiguration"].update(maximumPercent=200),
                       lambda value: value.update(serviceName="plether-api")]:
            service = service_fixture()
            mutate(service)
            with self.subTest(mutation=mutate), self.assertRaises(GuardRejected):
                validate_service(service)

    def test_scan_requires_fresh_complete_matching_zero_critical_evidence(self):
        now = 1_794_213_000
        validate_scan(scan_fixture(now), IMAGE.split("@")[1], now)
        mutations = [
            lambda value: value.update(registryId="000000000000"),
            lambda value: value.update(repositoryName="other"),
            lambda value: value["imageId"].update(imageDigest="sha256:" + "0" * 64),
            lambda value: value["imageScanStatus"].update(status="IN_PROGRESS"),
            lambda value: value.update(nextToken="unread-page"),
            lambda value: value["imageScanFindings"].pop("findings"),
            lambda value: value["imageScanFindings"].pop("vulnerabilitySourceUpdatedAt"),
            lambda value: value["imageScanFindings"].update(imageScanCompletedAt=datetime.fromtimestamp(now - 172800, timezone.utc)),
            lambda value: value["imageScanFindings"].update(imageScanCompletedAt=datetime.fromtimestamp(now + 172800, timezone.utc)),
            lambda value: value["imageScanFindings"].update(vulnerabilitySourceUpdatedAt=datetime.fromtimestamp(now - 172800, timezone.utc)),
            lambda value: value["imageScanFindings"].update(findingSeverityCounts={"CRITICAL": 1}),
            lambda value: value["imageScanFindings"].update(findingSeverityCounts={"CRITICAL": "0"}),
            lambda value: value["imageScanFindings"].update(findings=[{"name": "malformed", "severity": "UNKNOWN"}]),
        ]
        for index, mutate in enumerate(mutations):
            scan = scan_fixture(now)
            mutate(scan)
            with self.subTest(case=index), self.assertRaises(GuardRejected):
                validate_scan(scan, IMAGE.split("@")[1], now)
        scan = scan_fixture(now)
        scan["imageScanFindings"].update(findingSeverityCounts={"CRITICAL": 1},
            findings=[{"name": "CVE-2024-5535", "severity": "CRITICAL"}])
        with self.assertRaises(GuardRejected):
            validate_scan(scan, IMAGE.split("@")[1], now)


class FakeEcs:
    def __init__(self, events):
        self.events = events

    def update_service(self, **kwargs):
        assert kwargs["cluster"] == "plether-sepolia"
        assert kwargs["service"] == "plether-alto"
        self.events.append(("update", kwargs))
        return {"service": {}}


class RpcCutoverSequencingTests(unittest.TestCase):
    def test_replace_never_starts_a_second_signer_before_stopped_confirmation(self):
        events = []
        target = fixture()["taskDefinitionArn"].rsplit(":", 1)[0] + ":7"
        replace_service(FakeEcs(events), target, lambda: events.append(("stopped",)),
                        lambda value: events.append(("healthy", value)))
        self.assertEqual([event[0] for event in events], ["update", "stopped", "update", "healthy"])
        self.assertEqual(events[0][1]["desiredCount"], 0)
        self.assertEqual(events[2][1]["desiredCount"], 1)
        self.assertEqual(events[2][1]["taskDefinition"], target)

    def test_unconfirmed_stop_cannot_start_candidate(self):
        events = []
        def stopped():
            raise GuardRejected("fixture_task_still_running")
        with self.assertRaises(GuardRejected):
            replace_service(FakeEcs(events), AFTER, stopped, lambda value: self.fail("Must not start"))
        self.assertEqual(len(events), 1)
        self.assertEqual(events[0][1]["desiredCount"], 0)

    def test_failed_canary_or_non_idle_bundle_pool_cannot_stop_the_running_service(self):
        for fail_at in ["canary", "idle"]:
            events = []
            def canary():
                events.append(("canary",))
                if fail_at == "canary":
                    raise GuardRejected("fixture_canary_failed")
                return True
            def idle():
                events.append(("idle",))
                return False
            with self.subTest(fail_at=fail_at), self.assertRaises(GuardRejected):
                perform_cutover(FakeEcs(events), BEFORE, AFTER, canary, idle,
                    lambda: events.append(("stopped",)), lambda value: events.append(("healthy", value)),
                    lambda: events.append(("postcheck",)), lambda value: None)
            self.assertFalse(any(event[0] == "update" for event in events))

    def test_postcheck_failure_restores_exact_baseline_with_another_stop_before_start(self):
        events, saved = [], []
        def postcheck():
            events.append(("postcheck",))
            raise RuntimeError("fixture_postcheck_failed")
        with self.assertRaises(RuntimeError):
            perform_cutover(FakeEcs(events), BEFORE, AFTER, lambda: True, lambda: True,
                lambda: events.append(("stopped",)), lambda value: events.append(("healthy", value)),
                postcheck, lambda value: saved.append(copy.deepcopy(value)))
        updates = [event[1] for event in events if event[0] == "update"]
        self.assertEqual([item["desiredCount"] for item in updates], [0, 1, 0, 1])
        self.assertEqual([item["taskDefinition"] for item in updates if item["desiredCount"] == 1], [AFTER, BEFORE])
        self.assertEqual(sum(event[0] == "stopped" for event in events), 2)
        self.assertTrue(any(value.get("status") == "rolled_back" for value in saved))

    def test_barrier_cleanup_runs_even_when_idle_validation_fails_before_service_mutation(self):
        events = []
        def canary():
            events.append(("pause_installed",))
            raise GuardRejected("idle_observer_failed")
        with self.assertRaisesRegex(GuardRejected, "idle_observer_failed"):
            perform_cutover(FakeEcs(events), BEFORE, AFTER, canary, lambda: True,
                lambda: events.append(("stopped",)), lambda target: None, lambda: None, lambda value: None,
                lambda: events.append(("cleanup",)))
        self.assertEqual([event[0] for event in events], ["pause_installed", "cleanup"])

    def test_barrier_cleanup_also_runs_after_success_and_after_rollback(self):
        for failure in [False, True]:
            events = []
            def postcheck():
                if failure:
                    raise RuntimeError("postcheck_failed")
            def run():
                perform_cutover(FakeEcs(events), BEFORE, AFTER, lambda: True, lambda: True,
                    lambda: None, lambda target: None, postcheck, lambda value: None,
                    lambda: events.append(("cleanup",)))
            if failure:
                with self.assertRaisesRegex(RuntimeError, "postcheck_failed"):
                    run()
            else:
                run()
            self.assertEqual(events[-1], ("cleanup",))
            self.assertEqual(sum(event[0] == "cleanup" for event in events), 1)

    def test_both_forward_and_rollback_capture_signer_ids_before_scaling_down(self):
        events = []
        def postcheck():
            raise RuntimeError("fixture_postcheck_failed")
        with self.assertRaisesRegex(RuntimeError, "fixture_postcheck_failed"):
            perform_cutover(FakeEcs(events), BEFORE, AFTER, lambda: True, lambda: True,
                lambda: events.append(("stopped",)), lambda target: events.append(("healthy", target)),
                postcheck, lambda value: None, before_stop=lambda: events.append(("capture",)))
        for index, event in enumerate(events):
            if event[0] == "update" and event[1]["desiredCount"] == 0:
                self.assertEqual(events[index - 1], ("capture",))
        self.assertEqual(sum(event[0] == "capture" for event in events), 2)

    def test_forward_deadline_precheck_failure_never_stops_or_rolls_back_the_live_signer(self):
        events, saved = [], []
        def before_stop():
            events.append(("precheck",))
            raise GuardRejected("operator_deadline")
        with self.assertRaisesRegex(GuardRejected, "operator_deadline"):
            perform_cutover(FakeEcs(events), BEFORE, AFTER, lambda: True, lambda: True,
                lambda: self.fail("No stop was requested"), lambda target: self.fail("No start was requested"),
                lambda: None, lambda value: saved.append(copy.deepcopy(value)),
                cleanup=lambda: events.append(("cleanup",)), before_stop=before_stop)
        self.assertEqual(events, [("precheck",), ("cleanup",)])
        self.assertFalse(any(value.get("mutation_started") for value in saved))
        self.assertFalse(any(value.get("status") == "rolling_back" for value in saved))


class StoppingSignerEcs:
    """Models ECS desiredStatus=STOPPED while the old process is still stopping."""
    task_arn = f"arn:aws:ecs:{REGION}:{ACCOUNT}:task/plether-sepolia/" + "d" * 32

    def __init__(self, missing=False):
        self.describe_count = 0
        self.missing = missing

    def list_tasks(self, **kwargs):
        assert kwargs["cluster"] == "plether-sepolia"
        assert kwargs["serviceName"] == "plether-alto"
        return {"taskArns": [self.task_arn] if kwargs["desiredStatus"] == "STOPPED" else []}

    def describe_tasks(self, cluster, tasks):
        assert cluster == "plether-sepolia"
        assert tasks == [self.task_arn]
        self.describe_count += 1
        if self.missing:
            return {"tasks": [], "failures": [{"arn": self.task_arn, "reason": "MISSING"}]}
        return {"tasks": [{"taskArn": self.task_arn,
                           "desiredStatus": "STOPPED",
                           "lastStatus": "STOPPING" if self.describe_count == 1 else "STOPPED"}],
                "failures": []}


class StoppingSignerTests(unittest.TestCase):
    def deployment(self, ecs):
        value = object.__new__(Deployment)
        value.ecs = ecs
        value.stopping_tasks = {ecs.task_arn}
        value.budget = lambda: None
        stopped_service = service_fixture()
        stopped_service.update(desiredCount=0, runningCount=0, pendingCount=0)
        value.service = lambda: copy.deepcopy(stopped_service)
        return value

    def test_zero_service_counts_and_empty_running_list_do_not_hide_a_stopping_signer(self):
        ecs = StoppingSignerEcs()
        deployment = self.deployment(ecs)
        sleeps = []
        with patch("alto_rpc_cutover.time.sleep", side_effect=lambda seconds: sleeps.append(seconds)):
            deployment.wait_stopped()
        self.assertEqual(ecs.describe_count, 2, "The STOPPING process must be observed again after it exits")
        self.assertEqual(sleeps, [3], "Do not return after merely changing the task's desired status")
        self.assertEqual(deployment.stopping_tasks, {ecs.task_arn})

    def test_missing_captured_task_identity_cannot_be_treated_as_confirmed_stopped(self):
        ecs = StoppingSignerEcs(missing=True)
        with self.assertRaisesRegex(GuardRejected, "stopping_task_identity_missing"):
            self.deployment(ecs).wait_stopped()


class FakeElb:
    def __init__(self, owned=True, present=True):
        self.owned = owned
        self.present = present
        self.rule_arn = "owned-rule"
        self.deleted = []
        self.created = []

    def describe_rules(self, **kwargs):
        rules = [{"IsDefault": True, "RuleArn": "default"}]
        if self.present:
            rules.append({"IsDefault": False, "RuleArn": self.rule_arn})
        return {"Rules": rules}

    def describe_tags(self, ResourceArns):
        return {"TagDescriptions": [{"ResourceArn": ResourceArns[0], "Tags": [
            {"Key": "WorkflowOwner", "Value": "alto-rpc-123-1" if self.owned else "someone-else"},
            {"Key": "Capability", "Value": "alto-rpc-admission"},
            {"Key": "Source", "Value": SOURCE}]}]}

    def delete_rule(self, RuleArn):
        self.deleted.append(RuleArn)
        if RuleArn == self.rule_arn:
            self.present = False

    def create_rule(self, **kwargs):
        self.created.append(copy.deepcopy(kwargs))
        self.present = True
        self.rule_arn = "created-rule"
        return {"Rules": [{"RuleArn": self.rule_arn}]}


class AdmissionBarrierTests(unittest.TestCase):
    def deployment(self, directory, elb, journal):
        value = object.__new__(Deployment)
        value.elb = elb
        value.journal = journal
        value.args = SimpleNamespace(source=SOURCE)
        value.path = Path(directory) / "rollout.json"
        value.listener = "reviewed-listener"
        value.rollback = False
        value.deadline = 1000
        return value

    def test_lost_rule_creation_response_reconciles_only_the_owned_rule(self):
        with TemporaryDirectory() as directory, patch.dict("os.environ", {"GITHUB_RUN_ID": "123", "GITHUB_RUN_ATTEMPT": "1"}):
            elb = FakeElb()
            deployment = self.deployment(directory, elb, {"pause_status": "creating", "status": "canary_running"})
            deployment.cleanup_barrier()
            self.assertEqual(elb.deleted, ["owned-rule"])
            self.assertIsNone(deployment.journal["pause_rule"])
            self.assertEqual(deployment.journal["pause_status"], "removed")
            self.assertEqual(deployment.path.stat().st_mode & 0o777, 0o600)

    def test_foreign_rule_is_never_deleted(self):
        with TemporaryDirectory() as directory, patch.dict("os.environ", {"GITHUB_RUN_ID": "123", "GITHUB_RUN_ATTEMPT": "1"}):
            elb = FakeElb(owned=False)
            deployment = self.deployment(directory, elb, {"pause_rule": "owned-rule", "status": "verifying"})
            with self.assertRaisesRegex(GuardRejected, "barrier_owner_mismatch"):
                deployment.remove_barrier()
            self.assertEqual(elb.deleted, [])

    def test_missing_actual_rule_is_not_treated_as_an_active_admission_pause(self):
        with TemporaryDirectory() as directory, patch.dict("os.environ", {"GITHUB_RUN_ID": "123", "GITHUB_RUN_ATTEMPT": "1"}):
            elb = FakeElb(present=False)
            deployment = self.deployment(directory, elb, {"pause_rule": "owned-rule", "pause_status": "active"})
            self.assertIs(deployment.ensure_barrier(), False, "A recreated pause requires a fresh idle observation")
            self.assertEqual(len(elb.created), 1)
            self.assertEqual(deployment.journal["pause_rule"], "created-rule")
            self.assertEqual(deployment.journal["pause_status"], "active")
            self.assertIs(deployment.ensure_barrier(), True)
            self.assertEqual(len(elb.created), 1, "A confirmed active pause must not create another rule")

    def test_lost_delete_response_clears_journal_without_deleting_another_rule(self):
        with TemporaryDirectory() as directory:
            elb = FakeElb(present=False)
            deployment = self.deployment(directory, elb, {"pause_rule": "owned-rule", "pause_status": "removing"})
            deployment.remove_barrier()
            self.assertEqual(elb.deleted, [])
            self.assertIsNone(deployment.journal["pause_rule"])
            self.assertEqual(deployment.journal["pause_status"], "removed")

    def test_stale_journal_never_replaces_a_foreign_live_listener_rule(self):
        with TemporaryDirectory() as directory, patch.dict("os.environ", {"GITHUB_RUN_ID": "123", "GITHUB_RUN_ATTEMPT": "1"}):
            elb = FakeElb(owned=False)
            elb.rule_arn = "someone-else-rule"
            deployment = self.deployment(directory, elb, {"pause_rule": "missing-old-rule", "pause_status": "active"})
            with self.assertRaisesRegex(GuardRejected, "barrier_owner_mismatch"):
                deployment.ensure_barrier()
            self.assertEqual(elb.deleted, [])
            self.assertEqual(elb.created, [])

    def test_failed_rollback_retains_admission_pause_for_required_attention(self):
        with TemporaryDirectory() as directory:
            elb = FakeElb()
            deployment = self.deployment(directory, elb, {"pause_rule": "owned-rule", "status": "rollback_failed"})
            deployment.cleanup_barrier()
            self.assertEqual(elb.deleted, [])
            self.assertEqual(deployment.journal["pause_rule"], "owned-rule")

    def test_rollback_uses_the_time_reserved_before_stopping_the_baseline(self):
        with TemporaryDirectory() as directory, patch("alto_rpc_cutover.time.monotonic", return_value=800):
            deployment = self.deployment(directory, FakeElb(), {"pause_rule": "owned-rule"})
            deployment.ensure_barrier = lambda: None
            with self.assertRaisesRegex(GuardRejected, "operator_deadline"):
                deployment.budget()
            deployment.save({"status": "rolling_back"})
            self.assertTrue(deployment.rollback)
            deployment.budget()
            self.assertEqual(json.loads(deployment.path.read_text())["status"], "rolling_back")

    def test_rollback_reobserves_idle_after_admission_had_reopened(self):
        with TemporaryDirectory() as directory:
            deployment = self.deployment(directory, FakeElb(), {"status": "verifying"})
            deployment.candidate_definition = patch_definition(fixture(), IMAGE)
            deployment.baseline_service = service_fixture()
            observed = []
            deployment.ensure_barrier = lambda: False
            deployment.canary = lambda candidate, service, scope: observed.append(scope)
            deployment.quiescent = lambda: True
            deployment.save({"status": "rolling_back"})
            self.assertEqual(observed, ["idle"])
            self.assertTrue(deployment.rollback)
            self.assertEqual(deployment.journal["status"], "rolling_back")

    def test_new_pause_with_non_idle_pool_blocks_rollback_before_replacing_a_signer(self):
        with TemporaryDirectory() as directory:
            deployment = self.deployment(directory, FakeElb(), {"status": "verifying"})
            deployment.candidate_definition = patch_definition(fixture(), IMAGE)
            deployment.baseline_service = service_fixture()
            deployment.ensure_barrier = lambda: False
            deployment.canary = lambda candidate, service, scope: None
            deployment.quiescent = lambda: False
            with self.assertRaisesRegex(GuardRejected, "rollback_mempool_not_proven_idle"):
                deployment.save({"status": "rolling_back"})

    def test_rollback_uses_a_fresh_idle_observation_after_any_prior_admission_opening(self):
        with TemporaryDirectory() as directory:
            deployment = self.deployment(directory, FakeElb(), {
                "status": "verifying", "admission_opened": True, "pause_status": "active"})
            deployment.candidate_definition = patch_definition(fixture(), IMAGE)
            deployment.baseline_service = service_fixture()
            scopes = []
            deployment.ensure_barrier = lambda: True
            deployment.canary = lambda candidate, service, scope: scopes.append(scope)
            deployment.quiescent = lambda: True
            deployment.save({"status": "rolling_back"})
            self.assertEqual(scopes, ["idle"], "A currently visible pause cannot erase the previous open-admission interval")
            self.assertTrue(deployment.journal["admission_opened"])

    def test_removing_rule_seen_in_stale_control_plane_response_is_not_trusted_as_continuously_paused(self):
        with TemporaryDirectory() as directory, patch.dict("os.environ", {"GITHUB_RUN_ID": "123", "GITHUB_RUN_ATTEMPT": "1"}):
            deployment = self.deployment(directory, FakeElb(), {
                "status": "verifying", "pause_rule": "owned-rule", "pause_status": "removing", "admission_opened": True})
            self.assertIs(deployment.ensure_barrier(), False)

    def test_admission_opening_is_journalled_before_the_rule_can_disappear(self):
        with TemporaryDirectory() as directory, patch.dict("os.environ", {"GITHUB_RUN_ID": "123", "GITHUB_RUN_ATTEMPT": "1"}):
            elb = FakeElb()
            deployment = self.deployment(directory, elb, {"status": "verifying", "pause_rule": "owned-rule", "pause_status": "active"})
            def lost_response(RuleArn):
                persisted = json.loads(deployment.path.read_text())
                self.assertTrue(persisted["admission_opened"])
                self.assertEqual(persisted["pause_status"], "removing")
                raise TimeoutError("fixture_lost_delete_response")
            elb.delete_rule = lost_response
            with self.assertRaisesRegex(TimeoutError, "fixture_lost_delete_response"):
                deployment.remove_barrier()


class InterruptedRecoveryTests(unittest.TestCase):
    def setup_recovery(self, directory, *, status="verifying", service_revision=AFTER):
        events = []
        before = fixture()
        after = patch_definition(before, IMAGE)
        after.update(taskDefinitionArn=AFTER, revision=7)
        definitions = {BEFORE: before, AFTER: after}
        service = service_fixture()
        service["taskDefinition"] = service_revision
        service["deployments"][0]["taskDefinition"] = service_revision
        journal = {"schema_version": 1, "account": ACCOUNT, "region": REGION,
                   "source": SOURCE, "image": IMAGE, "status": status,
                   "previous_task_definition": BEFORE, "candidate_task_definition": AFTER,
                   "baseline_fingerprint": fingerprint(before), "candidate_fingerprint": fingerprint(after),
                   "mutation_started": status != "prepared", "pause_status": "active", "pause_rule": "owned-rule"}
        deployment = object.__new__(Deployment)
        deployment.args = SimpleNamespace(source=SOURCE, image=IMAGE)
        deployment.path = Path(directory) / "rollout.json"
        deployment.path.write_text(json.dumps(journal))
        deployment.directory = Path(directory)
        deployment.ecs = FakeEcs(events)
        deployment.definition = lambda arn: copy.deepcopy(definitions[arn])
        deployment.service = lambda: copy.deepcopy(service)
        deployment.baseline_url = lambda value: BASELINE_URL
        deployment.wait_stopped = lambda: events.append(("stopped",))
        deployment.wait_healthy = lambda target: events.append(("healthy", target))
        def ensure_barrier():
            events.append(("pause",))
            return True
        deployment.ensure_barrier = ensure_barrier
        deployment.cleanup_barrier = lambda: events.append(("cleanup",))
        deployment.capture_signers = lambda: None
        deployment.rollback = False
        deployment.deadline = 1000
        return deployment, definitions, journal, events

    def test_completed_recovery_is_idempotent_without_restarting_the_signer(self):
        with TemporaryDirectory() as directory:
            deployment, _, _, events = self.setup_recovery(directory, status="complete")
            deployment.recover()
            self.assertEqual(events, [("healthy", AFTER), ("cleanup",)])

    def test_healthy_baseline_only_removes_owned_pause_without_restart(self):
        with TemporaryDirectory() as directory:
            deployment, _, _, events = self.setup_recovery(directory, service_revision=BEFORE)
            deployment.recover()
            self.assertEqual(events, [("healthy", BEFORE), ("cleanup",)])
            self.assertTrue(deployment.rollback)
            self.assertEqual(json.loads(deployment.path.read_text())["status"], "rolled_back")

    def test_foreign_service_revision_is_never_mutated_during_recovery(self):
        foreign = AFTER.rsplit(":", 1)[0] + ":99"
        with TemporaryDirectory() as directory:
            deployment, _, _, events = self.setup_recovery(directory, service_revision=foreign)
            with self.assertRaisesRegex(GuardRejected, "foreign_service_revision"):
                deployment.recover()
            self.assertEqual(events, [])

    def test_baseline_fingerprint_drift_prevents_any_recovery_mutation(self):
        with TemporaryDirectory() as directory:
            deployment, definitions, _, events = self.setup_recovery(directory)
            definitions[BEFORE]["memory"] = "4096"
            with self.assertRaisesRegex(GuardRejected, "recovery_baseline_drift"):
                deployment.recover()
            self.assertEqual(events, [])

    def test_changed_candidate_policy_is_rejected_before_stop_or_pause(self):
        with TemporaryDirectory() as directory:
            deployment, definitions, _, events = self.setup_recovery(directory)
            main_container(definitions[AFTER])["environment"].append({"name": "UNREVIEWED_POLICY", "value": "true"})
            with self.assertRaises(GuardRejected):
                deployment.recover()
            self.assertEqual(events, [])

    def test_candidate_recovery_restores_exact_baseline_only_after_confirmed_stop(self):
        with TemporaryDirectory() as directory:
            deployment, _, _, events = self.setup_recovery(directory)
            deployment.recover()
            self.assertEqual([event[0] for event in events], ["pause", "update", "stopped", "update", "healthy", "cleanup"])
            updates = [event[1] for event in events if event[0] == "update"]
            self.assertEqual(updates[0]["desiredCount"], 0)
            self.assertEqual(updates[1]["desiredCount"], 1)
            self.assertEqual(updates[1]["taskDefinition"], BEFORE)
            self.assertEqual(json.loads(deployment.path.read_text())["status"], "rolled_back")

    def test_recovery_rejects_wrong_journal_scope_before_reading_or_mutating_ecs(self):
        for key, value in [("account", "000000000000"), ("region", "us-east-1"),
                           ("source", "0" * 40), ("image", IMAGE.split("@")[0] + ":latest")]:
            with self.subTest(key=key), TemporaryDirectory() as directory:
                deployment, _, journal, events = self.setup_recovery(directory)
                journal[key] = value
                deployment.path.write_text(json.dumps(journal))
                with self.assertRaisesRegex(GuardRejected, "recovery_scope_mismatch"):
                    deployment.recover()
                self.assertEqual(events, [])


if __name__ == "__main__":
    unittest.main()
