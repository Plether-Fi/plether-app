"""Policy and stop-before-start regressions; no AWS or network calls."""
import copy
from datetime import datetime, timezone
import json
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, patch

from alto_rpc_cutover import (
    GuardRejected, require_baseline, patch_definition, assert_only_rpc_patch,
    canary_definition, journal_idle_definition, validate_writer_ingress,
    validate_canary_proof, validate_service, validate_scan,
    replace_service, perform_cutover, fingerprint, canary_canonical, canary_exit_code,
    require_no_canary_restart, Deployment,
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
API_SOURCE = "0713acef754f1413352bcf1ece9d350ee717dd17"
API_IMAGE = f"{ACCOUNT}.dkr.ecr.{REGION}.amazonaws.com/plether-api-sepolia@sha256:b901c9b3c7a9d0db811659de396f4032409621089150d6e2f39e1026cdadf8e5"
API_TAGGED_IMAGE = f"{ACCOUNT}.dkr.ecr.{REGION}.amazonaws.com/plether-api-sepolia:{API_SOURCE}"
PAYMASTER = "0x" + "1" * 40
VPC = "vpc-0df986b0047f901d3"
ALTO_SG = "sg-00afd84a1d927a86a"
ALB_SG = "sg-0d925e07019465e6c"
API_SG = "sg-0ef6f711cab9291f8"
API_MARKER_SG = "sg-0230bc105fbcecc86"
RPC_CHECKS = ["configuration", "chain_and_primary_route", "not_syncing", "fresh_head", "canonical_latest",
              "canonical_safe", "canonical_finalized", "safe_state_reads", "fee_history", "gas_price",
              "deployed_contracts", "combined_state_overrides", "call_tracer_state_overrides",
              "bounded_latest_logs", "isolated_fallback"]
JOURNAL_CHECKS = ["drain_configuration", "admission_barrier", "acknowledgement_grace", "executor_wallets",
                  "process_continuity", "journal_snapshot", "journal_terminal", "reconciler_freshness",
                  "canonical_boundary", "fresh_canonical_heads", "historical_expiry_cutoff_verified",
                  "canonical_settlements"]


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


def api_fixture():
    value = fixture()
    value.update(family="plether-sepolia", revision=148,
                 taskDefinitionArn=f"arn:aws:ecs:{REGION}:{ACCOUNT}:task-definition/plether-sepolia:148",
                 executionRoleArn=f"arn:aws:iam::{ACCOUNT}:role/plether-sepolia-api-execution",
                 taskRoleArn=f"arn:aws:iam::{ACCOUNT}:role/plether-sepolia-api-task")
    api = value["containerDefinitions"][0]
    api.update(name="plether-api", image=API_TAGGED_IMAGE,
               entryPoint=["/app/api-server"], command=[],
               environment=[{"name": "PERPS_CHAIN_ID", "value": "421614"},
                            {"name": "AA_PAYMASTER_ADDRESS", "value": PAYMASTER},
                            {"name": "AA_PAYMASTER_KMS_KEY_ID", "value": "must-not-reach-observer"},
                            {"name": "NODE_OPTIONS", "value": "--import=unrelated-preload"},
                            {"name": "AA_RECONCILER_MAX_SAFE_LAG_SECONDS", "value": "1800"}],
               secrets=[{"name": "DATABASE_URL", "valueFrom": PREFIX + "database-url"},
                        {"name": "PERPS_RPC_URL", "valueFrom": PREFIX + "app-rpc-url"},
                        {"name": "PERPS_RPC_AUTH_TOKEN", "valueFrom": PREFIX + "rpc-gateway-token"},
                        {"name": "FAUCET_PRIVATE_KEY", "valueFrom": PREFIX + "faucet-private-key"},
                        {"name": "UNRELATED_API_SECRET", "valueFrom": PREFIX + "unrelated"}])
    value["containerDefinitions"] = [api, value["containerDefinitions"][2]]
    return value


def writer_ingress_fixture():
    alto = service_fixture()
    alto["networkConfiguration"]["awsvpcConfiguration"]["securityGroups"] = [ALTO_SG]
    api = copy.deepcopy(alto)
    api.update(serviceName="plether-api", taskDefinition=api_fixture()["taskDefinitionArn"])
    api["networkConfiguration"]["awsvpcConfiguration"]["securityGroups"] = [API_SG, API_MARKER_SG]
    lb = {"VpcId": VPC, "SecurityGroups": [ALB_SG], "Scheme": "internal",
          "LoadBalancerArn": f"arn:aws:elasticloadbalancing:{REGION}:{ACCOUNT}:loadbalancer/app/plether-sepolia-alto/123"}
    def rule(port, source):
        return {"IpProtocol": "tcp", "FromPort": port, "ToPort": port,
                "UserIdGroupPairs": [{"GroupId": source, "UserId": ACCOUNT}],
                "IpRanges": [], "Ipv6Ranges": [], "PrefixListIds": []}
    groups = [{"GroupId": group, "VpcId": VPC, "OwnerId": ACCOUNT, "IpPermissions": []}
              for group in [ALTO_SG, ALB_SG, API_SG, API_MARKER_SG]]
    next(group for group in groups if group["GroupId"] == ALTO_SG)["IpPermissions"] = [rule(3000, ALB_SG)]
    next(group for group in groups if group["GroupId"] == ALB_SG)["IpPermissions"] = [rule(80, API_MARKER_SG)]
    return alto, api, lb, groups


def proof_fixture(scope="rpc", now_ms=1_794_213_000_000):
    checks = JOURNAL_CHECKS if scope == "journal_idle" else RPC_CHECKS
    proof = {"event": "alto_rpc_qualification", "schema_version": 1, "scope": scope,
             "required_checks_complete": True, "tests": [{"name": name, "pass": True} for name in checks],
             "started_at_ms": now_ms - 40_000, "finished_at_ms": now_ms - 1000}
    if scope == "journal_idle":
        proof.update(idle_sample_count=2, snapshot_count=2, idle_observed_at_ms=now_ms - 1000,
                     journal_read_count=3, settled_count=7, history_settled_count=5,
                     frontier_settled_count=2, canonical_settlement_count=2,
                     authorization_count=10, submitted_count=7, expired_count=2,
                     history_expired_count=1, frontier_expired_count=1,
                     historical_expiry_cutoff_block_number=100, boundary_block_number=101,
                     historical_expiry_cutoff_timestamp_seconds=now_ms / 1000 - 10,
                     historical_authorization_count=6, historical_event_count=5,
                     invalid_authorization_count=0, invalid_event_count=0, invalid_terminal_count=0,
                     boundary_read_count=1,
                     unresolved_count=0, executor_wallets_available=4, executor_wallets_total=4,
                     process_started_at_seconds=now_ms / 1000 - 60)
    return proof


def canary_task_fixture(task_status="DEPROVISIONING", container_status="STOPPED", exit_code=0):
    return {"taskArn": f"arn:aws:ecs:{REGION}:{ACCOUNT}:task/plether-sepolia/canary-fixture",
            "lastStatus": task_status, "desiredStatus": "STOPPED",
            "containers": [{"name": "plether-alto", "lastStatus": container_status, "exitCode": exit_code}]}


def scan_fixture(now=1_794_213_000):
    recent = datetime.fromtimestamp(now - 30, timezone.utc)
    return {"registryId": ACCOUNT, "repositoryName": "plether-alto-sepolia",
            "imageId": {"imageDigest": IMAGE.split("@")[1]},
            "imageScanStatus": {"status": "COMPLETE"},
            "imageScanFindings": {"imageScanCompletedAt": recent, "vulnerabilitySourceUpdatedAt": recent,
                "findingSeverityCounts": {}, "findings": []}}


class RpcCutoverPolicyTests(unittest.TestCase):
    def test_canary_registration_normalizes_only_harmless_ecs_omitted_defaults(self):
        observers = [canary_definition(patch_definition(fixture(), IMAGE), SOURCE, "console.log('rpc fixture')", BASELINE_URL),
                     journal_idle_definition(api_fixture(), SOURCE, "console.log('idle fixture')", BASELINE_URL)]
        for expected in observers:
            original = copy.deepcopy(expected)
            original_fingerprint = fingerprint(expected)
            returned = copy.deepcopy(expected)
            observer = main_container(returned)
            for key in ["portMappings", "mountPoints", "volumesFrom", "dependsOn", "environmentFiles",
                        "resourceRequirements", "credentialSpecs", "systemControls"]:
                observer[key] = []
            observer["cpu"] = 0
            observer["logConfiguration"]["secretOptions"] = []
            returned["enableFaultInjection"] = False
            with self.subTest(mode=main_container(expected)["environment"]):
                self.assertEqual(canary_canonical(expected), canary_canonical(returned))
                self.assertEqual(expected, original, "Normalization must not mutate the source definition")
                self.assertEqual(fingerprint(expected), original_fingerprint)
                self.assertNotEqual(fingerprint(returned), original_fingerprint,
                                    "Full registered fingerprints must retain the returned configuration for ENI records")

    def test_canary_registration_canonicalization_keeps_every_meaningful_runtime_difference(self):
        expected = journal_idle_definition(api_fixture(), SOURCE, "console.log('idle fixture')", BASELINE_URL)
        mutations = [
            lambda value: main_container(value).update(portMappings=[{"containerPort": 3000, "protocol": "tcp"}]),
            lambda value: main_container(value).update(mountPoints=[{"sourceVolume": "unexpected", "containerPath": "/tmp"}]),
            lambda value: main_container(value)["environment"].append({"name": "NEW_RUNTIME_OPTION", "value": "true"}),
            lambda value: main_container(value)["secrets"][0].update(valueFrom=PREFIX + "different-database"),
            lambda value: main_container(value).update(entryPoint=["node", "/app/api-server"]),
            lambda value: main_container(value).update(image=IMAGE),
            lambda value: value.update(taskRoleArn=f"arn:aws:iam::{ACCOUNT}:role/plether-sepolia-api-task"),
            lambda value: main_container(value).update(cpu=256),
            lambda value: main_container(value)["logConfiguration"].update(secretOptions=[{"name": "token", "valueFrom": PREFIX + "unexpected"}]),
            lambda value: value.update(enableFaultInjection=True),
        ]
        for index, mutate in enumerate(mutations):
            returned = copy.deepcopy(expected)
            mutate(returned)
            with self.subTest(case=index):
                self.assertNotEqual(canary_canonical(expected), canary_canonical(returned))

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

    def test_journal_idle_observer_receives_only_read_credentials_and_no_runtime_role_or_server(self):
        before = api_fixture()
        original = copy.deepcopy(before)
        idle = journal_idle_definition(before, SOURCE, "console.log('idle fixture')", BASELINE_URL)
        self.assertEqual(before, original, "Observer preparation must preserve the running API definition")
        self.assertFalse(idle.get("taskRoleArn"))
        self.assertEqual(len(idle["containerDefinitions"]), 1)
        observer = idle["containerDefinitions"][0]
        self.assertEqual(observer["name"], "plether-alto")
        self.assertEqual(observer["image"], API_IMAGE)
        self.assertEqual(idle["executionRoleArn"], before["executionRoleArn"])
        self.assertEqual({item["name"]: item["valueFrom"] for item in observer["secrets"]}, {
            "DATABASE_URL": PREFIX + "database-url",
            "PERPS_RPC_URL": PREFIX + "app-rpc-url",
            "PERPS_RPC_AUTH_TOKEN": PREFIX + "rpc-gateway-token",
            "PLETHER_ALTO_BACKUP_RPC_URL": PREFIX + "perps-rpc-url"})
        self.assertEqual(observer["entryPoint"], ["node", "--input-type=module", "-e"])
        self.assertEqual(observer["command"], ["console.log('idle fixture')"])
        self.assertEqual({item["name"]: item["value"] for item in observer["environment"]}, {
            "PLETHER_ALTO_BASELINE_URL": BASELINE_URL, "PLETHER_ALTO_QUALIFY_MODE": "journal_idle",
            "AA_PAYMASTER_ADDRESS": PAYMASTER, "PERPS_CHAIN_ID": "421614",
            "AA_RECONCILER_MAX_SAFE_LAG_SECONDS": "1800"})
        for key in ["portMappings", "dependsOn", "mountPoints", "healthCheck", "firelensConfiguration"]:
            self.assertFalse(observer.get(key), key)
        self.assertEqual(idle.get("volumes"), [])

    def test_journal_idle_observer_rejects_unapproved_api_source_identity_or_scope(self):
        mutations = [
            lambda value: value.update(family="plether-mainnet"),
            lambda value: value.update(taskDefinitionArn=value["taskDefinitionArn"].replace(ACCOUNT, "000000000000")),
            lambda value: value.update(taskDefinitionArn=value["taskDefinitionArn"].replace(":148", ":149")),
            lambda value: value.update(executionRoleArn=f"arn:aws:iam::{ACCOUNT}:role/plether-sepolia-alto-execution"),
            lambda value: value.update(networkMode="bridge"),
            lambda value: value["runtimePlatform"].update(cpuArchitecture="X86_64"),
            lambda value: value["containerDefinitions"][0].update(image=API_TAGGED_IMAGE.replace(API_SOURCE, "c" * 40)),
            lambda value: value["containerDefinitions"][0].update(image=API_TAGGED_IMAGE.replace(ACCOUNT, "000000000000")),
            lambda value: value["containerDefinitions"][0].update(image=API_TAGGED_IMAGE.replace("plether-api-sepolia", "other")),
            lambda value: value["containerDefinitions"][0].update(image=API_IMAGE.replace("b901", "0000")),
            lambda value: value["containerDefinitions"].append(copy.deepcopy(value["containerDefinitions"][0])),
        ]
        for index, mutate in enumerate(mutations):
            definition = api_fixture()
            mutate(definition)
            with self.subTest(case=index), self.assertRaises(GuardRejected):
                journal_idle_definition(definition, SOURCE, "console.log('idle fixture')", BASELINE_URL)

    def test_journal_idle_observer_rejects_changed_rpc_database_references_or_paymaster(self):
        mutations = [
            lambda api: api["secrets"][0].update(valueFrom=PREFIX + "other-database"),
            lambda api: api["secrets"][1].update(valueFrom=PREFIX + "perps-rpc-url"),
            lambda api: api["secrets"][2].update(valueFrom=PREFIX + "other-token"),
            lambda api: api["secrets"].append(copy.deepcopy(api["secrets"][0])),
            lambda api: api["environment"][0].update(value="42161"),
            lambda api: api["environment"][1].update(value="not-an-address"),
            lambda api: api["environment"][4].update(value="3600"),
            lambda api: api["environment"].append(copy.deepcopy(api["environment"][1])),
        ]
        for index, mutate in enumerate(mutations):
            definition = api_fixture()
            mutate(definition["containerDefinitions"][0])
            with self.subTest(case=index), self.assertRaises(GuardRejected):
                journal_idle_definition(definition, SOURCE, "console.log('idle fixture')", BASELINE_URL)

    def test_journal_observer_rejects_non_private_origins_or_invalid_source(self):
        for url in ["https://example.com", BASELINE_URL + "/metrics", BASELINE_URL + "?token=secret",
                    BASELINE_URL.replace("http://", "http://token@")]:
            with self.subTest(url=url), self.assertRaises(GuardRejected):
                journal_idle_definition(api_fixture(), SOURCE, "console.log('idle fixture')", url)
        for source in ["", "g" * 40, SOURCE + "0"]:
            with self.subTest(source=source), self.assertRaises(GuardRejected):
                journal_idle_definition(api_fixture(), source, "console.log('idle fixture')", BASELINE_URL)

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


class CanaryProofTests(unittest.TestCase):
    def test_journal_proof_requires_three_reads_complete_aggregates_and_canonical_frontier_counts(self):
        now_ms = 1_794_213_000_000
        empty = proof_fixture('journal_idle', now_ms)
        empty.update(settled_count=0, history_settled_count=0, frontier_settled_count=0, canonical_settlement_count=0,
                     historical_authorization_count=1, historical_event_count=0, submitted_count=0)
        validate_canary_proof(empty, 'journal_idle', now_ms)
        mutations = [
            lambda value: value.update(journal_read_count=2),
            lambda value: value.update(journal_read_count=4),
            lambda value: value.update(canonical_settlement_count=1),
            lambda value: value.update(canonical_settlement_count=3),
            lambda value: value.update(canonical_settlement_count=7),
            lambda value: value.update(settled_count=6),
            lambda value: value.update(settled_count=8),
            lambda value: value.update(history_settled_count=4),
            lambda value: value.update(history_settled_count=6),
            lambda value: value.update(history_settled_count=8),
            lambda value: value.update(frontier_settled_count=1),
            lambda value: value.update(frontier_settled_count=3),
            lambda value: value.update(frontier_settled_count=0, canonical_settlement_count=0),
            lambda value: value.update(expired_count=1),
            lambda value: value.update(expired_count=3),
            lambda value: value.update(history_expired_count=0),
            lambda value: value.update(history_expired_count=2),
            lambda value: value.update(frontier_expired_count=0),
            lambda value: value.update(frontier_expired_count=2),
            lambda value: value.update(authorization_count=6),
            lambda value: value.update(authorization_count=8),
            lambda value: value.update(submitted_count=11),
            lambda value: value.update(submitted_count=10),
            lambda value: value.update(historical_authorization_count=11),
            lambda value: value.update(historical_authorization_count=5),
            lambda value: value.update(historical_authorization_count=7),
            lambda value: value.update(historical_event_count=4),
            lambda value: value.update(historical_event_count=6),
            lambda value: value.update(historical_event_count=7),
            lambda value: value.update(history_settled_count=7, frontier_settled_count=0,
                                       canonical_settlement_count=0, historical_authorization_count=0,
                                       historical_event_count=0),
            lambda value: value.update(invalid_authorization_count=1),
            lambda value: value.update(invalid_event_count=1),
            lambda value: value.update(invalid_terminal_count=1),
            lambda value: value.update(boundary_read_count=0),
            lambda value: value.update(boundary_read_count=2),
            lambda value: value.update(historical_expiry_cutoff_block_number=0),
            lambda value: value.update(historical_expiry_cutoff_block_number=102),
            lambda value: value.update(historical_expiry_cutoff_timestamp_seconds=0),
            lambda value: value.update(historical_expiry_cutoff_timestamp_seconds=now_ms / 1000),
        ]
        for key in ['journal_read_count', 'authorization_count', 'submitted_count', 'settled_count',
                    'expired_count', 'history_settled_count', 'frontier_settled_count', 'history_expired_count',
                    'frontier_expired_count', 'canonical_settlement_count', 'historical_expiry_cutoff_block_number',
                    'historical_expiry_cutoff_timestamp_seconds', 'historical_authorization_count',
                    'historical_event_count', 'invalid_authorization_count', 'invalid_event_count',
                    'invalid_terminal_count', 'boundary_read_count', 'boundary_block_number']:
            mutations.append(lambda value, key=key: value.pop(key))
            for invalid in [True, False, -1, 1.5, float('nan'), float('inf'), '7', None]:
                mutations.append(lambda value, key=key, invalid=invalid: value.update({key: invalid}))
        for index, mutate in enumerate(mutations):
            proof = proof_fixture('journal_idle', now_ms)
            mutate(proof)
            with self.subTest(case=index), self.assertRaises(GuardRejected):
                validate_canary_proof(proof, 'journal_idle', now_ms)
            deployment = object.__new__(Deployment)
            deployment.canary_proof = proof
            with patch('alto_rpc_cutover.time.time', return_value=now_ms / 1000):
                with self.subTest(cached_case=index):
                    self.assertFalse(deployment.quiescent())

    def test_expired_history_needs_no_frontier_receipts_but_must_still_account_for_every_settlement(self):
        now_ms = 1_794_213_000_000
        for history, frontier in [(7, 0), (0, 7), (5, 2)]:
            proof = proof_fixture('journal_idle', now_ms)
            proof.update(history_settled_count=history, frontier_settled_count=frontier,
                         canonical_settlement_count=frontier,
                         historical_authorization_count=history + proof['history_expired_count'],
                         historical_event_count=history)
            with self.subTest(history=history, frontier=frontier):
                validate_canary_proof(proof, 'journal_idle', now_ms)
                deployment = object.__new__(Deployment)
                deployment.canary_proof = proof
                with patch('alto_rpc_cutover.time.time', return_value=now_ms / 1000):
                    self.assertTrue(deployment.quiescent())

    def test_historical_settlements_cannot_bypass_frontier_receipts_without_verified_expiry_cutoff(self):
        now_ms = 1_794_213_000_000
        for case in ['missing', 'failed', 'unreviewed_name']:
            proof = proof_fixture('journal_idle', now_ms)
            proof.update(history_settled_count=7, frontier_settled_count=0,
                         canonical_settlement_count=0, historical_authorization_count=8,
                         historical_event_count=7)
            check = next(test for test in proof['tests'] if test['name'] == 'historical_expiry_cutoff_verified')
            if case == 'missing':
                proof['tests'].remove(check)
            elif case == 'failed':
                check['pass'] = False
            else:
                check['name'] = 'assume_old_operations_expired'
            with self.subTest(case=case), self.assertRaises(GuardRejected):
                validate_canary_proof(proof, 'journal_idle', now_ms)
            deployment = object.__new__(Deployment)
            deployment.canary_proof = proof
            with patch('alto_rpc_cutover.time.time', return_value=now_ms / 1000):
                self.assertFalse(deployment.quiescent())

    def test_cached_journal_proof_must_still_be_fresh_and_include_the_reviewed_checks(self):
        now_ms = 1_794_213_000_000
        deployment = object.__new__(Deployment)
        with patch("alto_rpc_cutover.time.time", return_value=now_ms / 1000):
            deployment.canary_proof = proof_fixture("journal_idle", now_ms)
            self.assertTrue(deployment.quiescent())
            mutations = [
                lambda value: value.update(scope="idle"),
                lambda value: value.update(required_checks_complete=False),
                lambda value: value.update(idle_observed_at_ms=now_ms - 30_001),
                lambda value: value.update(idle_observed_at_ms=now_ms + 1),
                lambda value: value.update(unresolved_count=1),
                lambda value: value.update(snapshot_count=1),
                lambda value: value.update(executor_wallets_available=3),
                lambda value: value["tests"][0].update(name="made_up_success"),
                lambda value: value["tests"][0].update(**{"pass": False}),
            ]
            for index, mutate in enumerate(mutations):
                deployment.canary_proof = proof_fixture("journal_idle", now_ms)
                mutate(deployment.canary_proof)
                with self.subTest(case=index):
                    self.assertFalse(deployment.quiescent())

    def test_rpc_proof_requires_the_complete_reviewed_checks_rather_than_arbitrary_success_names(self):
        now_ms = 1_794_213_000_000
        validate_canary_proof(proof_fixture(now_ms=now_ms), "rpc", now_ms)
        mutations = [
            lambda value: value.update(event="unrelated_event"),
            lambda value: value.update(schema_version=2),
            lambda value: value.update(scope="journal_idle"),
            lambda value: value.update(required_checks_complete=False),
            lambda value: value.update(required_checks_complete=1),
            lambda value: value.update(tests=[]),
            lambda value: value["tests"][0].update(**{"pass": 1}),
            lambda value: value["tests"][0].update(name="made_up_success"),
            lambda value: value["tests"].pop(),
            lambda value: value["tests"].append(copy.deepcopy(value["tests"][0])),
            lambda value: value["tests"].append({"name": "extra_success", "pass": True}),
        ]
        for index, mutate in enumerate(mutations):
            proof = proof_fixture(now_ms=now_ms)
            mutate(proof)
            with self.subTest(case=index), self.assertRaises(GuardRejected):
                validate_canary_proof(proof, "rpc", now_ms)

    def test_unknown_observer_scope_is_not_accepted_as_rpc_or_idle(self):
        for scope in ["idle", "invalid", "", None]:
            with self.subTest(scope=scope), self.assertRaises(GuardRejected):
                validate_canary_proof(proof_fixture(scope=scope), scope, 1_794_213_000_000)

    def test_journal_idle_proof_expires_after_thirty_seconds_including_previously_successful_tasks(self):
        now_ms = 1_794_213_000_000
        proof = proof_fixture("journal_idle", now_ms)
        proof["idle_observed_at_ms"] = now_ms - 30_000
        validate_canary_proof(proof, "journal_idle", now_ms)
        proof["idle_observed_at_ms"] -= 1
        with self.assertRaisesRegex(GuardRejected, "quiescence"):
            validate_canary_proof(proof, "journal_idle", now_ms)

    def test_journal_idle_proof_cannot_claim_success_without_two_empty_stable_observations(self):
        now_ms = 1_794_213_000_000
        mutations = [
            lambda value: value.update(idle_sample_count=1),
            lambda value: value.update(snapshot_count=1),
            lambda value: value.update(unresolved_count=1),
            lambda value: value.update(unresolved_count=False),
            lambda value: value.update(executor_wallets_available=3),
            lambda value: value.update(executor_wallets_total=5),
            lambda value: value.update(idle_observed_at_ms=now_ms + 1),
            lambda value: value.update(idle_observed_at_ms=value["started_at_ms"] - 1),
            lambda value: value.update(idle_observed_at_ms=value["finished_at_ms"] + 1),
            lambda value: value.update(idle_observed_at_ms=float("nan")),
            lambda value: value.update(idle_observed_at_ms=True),
            lambda value: value.update(process_started_at_seconds=0),
            lambda value: value.update(process_started_at_seconds=float("inf")),
            lambda value: value.update(process_started_at_seconds=True),
            lambda value: value["tests"][0].update(name="made_up_success"),
            lambda value: value["tests"].pop(),
            lambda value: value["tests"].append(copy.deepcopy(value["tests"][0])),
        ]
        for index, mutate in enumerate(mutations):
            proof = proof_fixture("journal_idle", now_ms)
            mutate(proof)
            with self.subTest(case=index), self.assertRaises(GuardRejected):
                validate_canary_proof(proof, "journal_idle", now_ms)

    def test_canary_proof_rejects_nonfinite_future_reversed_or_old_task_timestamps(self):
        now_ms = 1_794_213_000_000
        mutations = [
            lambda value: value.update(started_at_ms=True),
            lambda value: value.update(started_at_ms=0),
            lambda value: value.update(started_at_ms=value["finished_at_ms"] + 1),
            lambda value: value.update(finished_at_ms=now_ms + 1),
            lambda value: value.update(finished_at_ms=float("nan")),
            lambda value: value.update(started_at_ms=now_ms - 400_000, finished_at_ms=now_ms - 300_001),
        ]
        for index, mutate in enumerate(mutations):
            proof = proof_fixture(now_ms=now_ms)
            mutate(proof)
            with self.subTest(case=index), self.assertRaises(GuardRejected):
                validate_canary_proof(proof, "rpc", now_ms)
        for invalid_now in [True, float("nan"), float("inf")]:
            with self.subTest(now=invalid_now), self.assertRaises(GuardRejected):
                validate_canary_proof(proof_fixture(now_ms=now_ms), "rpc", invalid_now)


class CanaryProcessExitTests(unittest.TestCase):
    def test_only_absent_or_explicitly_disabled_container_restart_policy_is_allowed_for_exit_proof(self):
        for policy in [None, {"enabled": False}]:
            api, prepared = api_fixture(), patch_definition(fixture(), IMAGE)
            if policy is not None:
                api["containerDefinitions"][0]["restartPolicy"] = copy.deepcopy(policy)
                main_container(prepared)["restartPolicy"] = copy.deepcopy(policy)
            observers = [canary_definition(prepared, SOURCE, "console.log('rpc')", BASELINE_URL),
                         journal_idle_definition(api, SOURCE, "console.log('idle')", BASELINE_URL)]
            for observer in observers:
                with self.subTest(policy=policy):
                    require_no_canary_restart(observer)

    def test_restart_enabled_or_malformed_policies_cannot_authorize_canary_process_exit(self):
        for policy in [{"enabled": True}, {"enabled": True, "ignoredExitCodes": [1]},
                       {}, {"enabled": 0}, {"enabled": "false"}, False, [], "disabled"]:
            api, prepared = api_fixture(), patch_definition(fixture(), IMAGE)
            api["containerDefinitions"][0]["restartPolicy"] = copy.deepcopy(policy)
            main_container(prepared)["restartPolicy"] = copy.deepcopy(policy)
            with self.subTest(policy=policy, scope="rpc"), self.assertRaisesRegex(GuardRejected, "restart_policy"):
                canary_definition(prepared, SOURCE, "console.log('rpc')", BASELINE_URL)
            with self.subTest(policy=policy, scope="journal_idle"), self.assertRaisesRegex(GuardRejected, "restart_policy"):
                journal_idle_definition(api, SOURCE, "console.log('idle')", BASELINE_URL)

    def test_canary_restart_restriction_does_not_change_the_running_alto_configuration(self):
        before = fixture()
        main_container(before)["restartPolicy"] = {"enabled": True, "restartAttemptPeriod": 60}
        prepared = patch_definition(before, IMAGE)
        assert_only_rpc_patch(before, prepared, IMAGE)
        self.assertEqual(main_container(prepared)["restartPolicy"], main_container(before)["restartPolicy"])

    def test_definite_sole_canary_container_exit_does_not_wait_for_task_deprovisioning(self):
        for status in ["RUNNING", "STOPPING", "DEPROVISIONING", "STOPPED"]:
            for code in [0, 1]:
                with self.subTest(task_status=status, exit_code=code):
                    self.assertEqual(canary_exit_code(canary_task_fixture(status, exit_code=code)), code)

    def test_desired_task_stop_does_not_prove_the_canary_process_has_exited(self):
        task = canary_task_fixture(task_status="STOPPED", container_status="RUNNING", exit_code=0)
        self.assertIsNone(canary_exit_code(task))
        for code in [None, True, False, 0.0, "0"]:
            task = canary_task_fixture(exit_code=code)
            with self.subTest(exit_code=code):
                self.assertIsNone(canary_exit_code(task))
        task = canary_task_fixture()
        task["containers"][0].pop("exitCode")
        self.assertIsNone(canary_exit_code(task))

    def test_an_unexpected_container_or_extra_process_cannot_supply_exit_proof(self):
        for case in ["wrong_name", "two_containers", "no_containers"]:
            task = canary_task_fixture()
            if case == "wrong_name":
                task["containers"][0]["name"] = "plether-api"
            elif case == "two_containers":
                task["containers"].append(copy.deepcopy(task["containers"][0]))
            else:
                task["containers"] = []
            with self.subTest(case=case):
                self.assertIsNone(canary_exit_code(task))


class CanaryLifecycleTests(unittest.TestCase):
    now_ms = 1_794_213_000_000

    def setup_canary(self, directory, states, records=None):
        deployment = object.__new__(Deployment)
        deployment.args = SimpleNamespace(source=SOURCE, image=IMAGE)
        deployment.directory = Path(directory)
        deployment.journal, deployment.canary_proof = {}, None
        deployment.budget = lambda: None
        deployment.baseline_url = lambda service: BASELINE_URL
        deployment.journal_observer_context = lambda service: (api_fixture(), copy.deepcopy(service["networkConfiguration"]))
        definition_arn = f"arn:aws:ecs:{REGION}:{ACCOUNT}:task-definition/plether-sepolia-alto-rpc-canary:1"
        reviewed = {}
        def register(definition):
            reviewed[definition_arn] = copy.deepcopy(definition)
            return definition_arn
        deployment.register = Mock(side_effect=register)
        deployment.definition = lambda arn: copy.deepcopy(reviewed[arn])
        deployment.save = lambda value: deployment.journal.update(copy.deepcopy(value))
        observations = []
        def describe(**kwargs):
            task = copy.deepcopy(states[min(len(observations), len(states) - 1)])
            observations.append(task)
            return {"tasks": [task], "failures": []}
        deployment.ecs = SimpleNamespace(
            list_tasks=Mock(return_value={"taskArns": []}),
            run_task=Mock(return_value={"tasks": [{"taskArn": states[0]["taskArn"]}], "failures": []}),
            describe_tasks=Mock(side_effect=describe), stop_task=Mock(), deregister_task_definition=Mock())
        if records is None:
            records = [{"message": json.dumps(proof_fixture("journal_idle", self.now_ms))}]
        def logs(**kwargs):
            self.assertIsNotNone(canary_exit_code(observations[-1]), "Logs must not authorize a still-running process")
            return {"events": records}
        deployment.logs = SimpleNamespace(filter_log_events=Mock(side_effect=logs),
                                         exceptions=SimpleNamespace(ResourceNotFoundException=LookupError))
        return deployment, observations

    def run_canary(self, deployment, delays):
        with patch.dict("os.environ", {"GITHUB_RUN_ID": "123", "GITHUB_RUN_ATTEMPT": "1"}), \
             patch("alto_rpc_cutover.Path.read_text", return_value="console.log('fixture completion')"), \
             patch("alto_rpc_cutover.time.time", return_value=self.now_ms / 1000), \
             patch("alto_rpc_cutover.time.sleep", side_effect=delays.append):
            deployment.canary(patch_definition(fixture(), IMAGE), service_fixture(), "journal_idle")

    def test_validated_proof_is_consumed_after_process_exit_before_task_cleanup_finishes(self):
        for status in ["RUNNING", "DEPROVISIONING"]:
            with self.subTest(task_status=status), TemporaryDirectory() as directory:
                deployment, observations = self.setup_canary(directory, [canary_task_fixture(status)])
                delays = []
                self.run_canary(deployment, delays)
                self.assertEqual(delays, [], "Container exit must not wait for slower task/ENI cleanup")
                self.assertEqual(len(observations), 2, "One process observation plus final cleanup check suffices")
                self.assertTrue(all(task["lastStatus"] == status for task in observations))
                deployment.ecs.stop_task.assert_not_called()
                deployment.ecs.deregister_task_definition.assert_called_once()
                self.assertIs(deployment.canary_proof["required_checks_complete"], True)
                self.assertTrue((Path(directory) / "idle-qualification.json").is_file())

    def test_desired_stopped_does_not_consume_available_proof_while_the_process_is_running(self):
        states = [canary_task_fixture("RUNNING", "RUNNING"), canary_task_fixture("DEPROVISIONING")]
        with TemporaryDirectory() as directory:
            deployment, observations = self.setup_canary(directory, states)
            delays = []
            self.run_canary(deployment, delays)
            self.assertEqual(delays, [2])
            self.assertEqual(len(observations), 3)
            self.assertEqual(observations[0]["desiredStatus"], "STOPPED")
            self.assertEqual(observations[0]["containers"][0]["lastStatus"], "RUNNING")
            deployment.logs.filter_log_events.assert_called_once()
            deployment.ecs.stop_task.assert_not_called()

    def test_nonzero_process_exit_aborts_before_proof_reads_without_stopping_an_exited_process_again(self):
        with TemporaryDirectory() as directory:
            deployment, _ = self.setup_canary(directory, [canary_task_fixture(exit_code=1)])
            with self.assertRaisesRegex(GuardRejected, "canary_failed"):
                self.run_canary(deployment, [])
            deployment.logs.filter_log_events.assert_not_called()
            deployment.ecs.stop_task.assert_not_called()
            deployment.ecs.deregister_task_definition.assert_called_once()
            self.assertIsNone(deployment.canary_proof)
            self.assertFalse((Path(directory) / "idle-qualification.json").exists())

    def test_process_exit_or_completion_stdout_does_not_replace_a_valid_qualification_proof(self):
        invalid = proof_fixture("journal_idle", self.now_ms)
        invalid["tests"][0]["name"] = "completion_only"
        for records in [[{"message": "fixture completion"}], [{"message": json.dumps(invalid)}]]:
            with self.subTest(records=records), TemporaryDirectory() as directory:
                deployment, _ = self.setup_canary(directory, [canary_task_fixture()], records)
                with self.assertRaises(GuardRejected):
                    self.run_canary(deployment, [])
                deployment.ecs.stop_task.assert_not_called()
                deployment.ecs.deregister_task_definition.assert_called_once()
                self.assertIsNone(deployment.canary_proof)
                self.assertFalse((Path(directory) / "idle-qualification.json").exists())


class WriterIngressTests(unittest.TestCase):
    def test_only_api_marker_can_write_to_alto_through_the_fixed_internal_load_balancer(self):
        validate_writer_ingress(*writer_ingress_fixture())

    def test_extra_ports_protocols_or_sources_cannot_bypass_the_paused_api_writer(self):
        mutations = [
            lambda rule: rule.update(FromPort=0),
            lambda rule: rule.update(ToPort=65535),
            lambda rule: rule.update(IpProtocol="-1"),
            lambda rule: rule.update(IpProtocol="udp"),
            lambda rule: rule["IpRanges"].append({"CidrIp": "0.0.0.0/0"}),
            lambda rule: rule["Ipv6Ranges"].append({"CidrIpv6": "::/0"}),
            lambda rule: rule["PrefixListIds"].append({"PrefixListId": "pl-unreviewed"}),
            lambda rule: rule["UserIdGroupPairs"].append({"GroupId": API_SG, "UserId": ACCOUNT}),
            lambda rule: rule["UserIdGroupPairs"][0].update(GroupId="sg-unreviewed"),
            lambda rule: rule["UserIdGroupPairs"][0].update(UserId="000000000000"),
        ]
        for target in [ALB_SG, ALTO_SG]:
            for index, mutate in enumerate(mutations):
                alto, api, lb, groups = writer_ingress_fixture()
                selected = next(group for group in groups if group["GroupId"] == target)
                mutate(selected["IpPermissions"][0])
                with self.subTest(target=target, case=index), self.assertRaises(GuardRejected):
                    validate_writer_ingress(alto, api, lb, groups)
            alto, api, lb, groups = writer_ingress_fixture()
            selected = next(group for group in groups if group["GroupId"] == target)
            selected["IpPermissions"].append(copy.deepcopy(selected["IpPermissions"][0]))
            with self.subTest(target=target, case="extra_rule"), self.assertRaises(GuardRejected):
                validate_writer_ingress(alto, api, lb, groups)

    def test_fixed_security_group_membership_account_and_vpc_are_required(self):
        mutations = [
            lambda alto, api, lb, groups: alto["networkConfiguration"]["awsvpcConfiguration"]["securityGroups"].append(API_SG),
            lambda alto, api, lb, groups: api["networkConfiguration"]["awsvpcConfiguration"].update(securityGroups=[API_SG]),
            lambda alto, api, lb, groups: api["networkConfiguration"]["awsvpcConfiguration"]["securityGroups"].append("sg-unreviewed"),
            lambda alto, api, lb, groups: lb["SecurityGroups"].append(API_SG),
            lambda alto, api, lb, groups: lb.update(VpcId="vpc-unreviewed"),
            lambda alto, api, lb, groups: lb.update(Scheme="internet-facing"),
            lambda alto, api, lb, groups: lb.update(LoadBalancerArn=lb["LoadBalancerArn"].replace(ACCOUNT, "000000000000")),
            lambda alto, api, lb, groups: alto.update(clusterArn=alto["clusterArn"].replace(ACCOUNT, "000000000000")),
            lambda alto, api, lb, groups: api.update(clusterArn=api["clusterArn"].replace(ACCOUNT, "000000000000")),
            lambda alto, api, lb, groups: groups.pop(),
            lambda alto, api, lb, groups: groups.append(copy.deepcopy(groups[0])),
        ]
        for index, mutate in enumerate(mutations):
            values = writer_ingress_fixture()
            mutate(*values)
            with self.subTest(case=index), self.assertRaises(GuardRejected):
                validate_writer_ingress(*values)
        for target in [ALTO_SG, ALB_SG, API_SG, API_MARKER_SG]:
            for key, bad_value in [("OwnerId", "000000000000"), ("VpcId", "vpc-unreviewed")]:
                values = writer_ingress_fixture()
                next(group for group in values[3] if group["GroupId"] == target)[key] = bad_value
                with self.subTest(target=target, key=key), self.assertRaises(GuardRejected):
                    validate_writer_ingress(*values)


class ObserverEcs:
    def __init__(self, api):
        self.api = api
        self.tasks = {}
        self.task_lists = {}
        for name, definition, eni in [("plether-api", api_fixture()["taskDefinitionArn"], "eni-api"),
                                       ("plether-alto", BEFORE, "eni-alto")]:
            arn = f"arn:aws:ecs:{REGION}:{ACCOUNT}:task/plether-sepolia/{name}-fixture"
            self.task_lists[name] = [arn]
            self.tasks[arn] = {"taskArn": arn, "taskDefinitionArn": definition, "lastStatus": "RUNNING",
                "containers": [{"name": name, "lastStatus": "RUNNING", "imageDigest": API_IMAGE.split("@")[1]}],
                "attachments": [{"type": "ElasticNetworkInterface", "details": [
                    {"name": "networkInterfaceId", "value": eni}]}]}

    def describe_services(self, cluster, services):
        assert cluster == "plether-sepolia" and services == ["plether-api"]
        return {"services": [copy.deepcopy(self.api)], "failures": []}

    def list_tasks(self, cluster, serviceName, desiredStatus):
        assert cluster == "plether-sepolia" and desiredStatus == "RUNNING"
        return {"taskArns": self.task_lists[serviceName][:]}

    def describe_tasks(self, cluster, tasks):
        assert cluster == "plether-sepolia"
        return {"tasks": [copy.deepcopy(self.tasks[arn]) for arn in tasks], "failures": []}


class ObserverEc2:
    def __init__(self, groups, interfaces):
        self.groups, self.pages = groups, [interfaces]
        self.requests = []
        self.unbounded = False

    def describe_security_groups(self, GroupIds):
        assert set(GroupIds) == {ALTO_SG, ALB_SG, API_SG, API_MARKER_SG}
        return {"SecurityGroups": copy.deepcopy(self.groups)}

    def describe_network_interfaces(self, **kwargs):
        self.requests.append(copy.deepcopy(kwargs))
        index = int(kwargs.get("NextToken", "0"))
        result = {"NetworkInterfaces": copy.deepcopy(self.pages[min(index, len(self.pages) - 1)])}
        if self.unbounded or index + 1 < len(self.pages):
            result["NextToken"] = str(index + 1)
        return result


class JournalObserverContextTests(unittest.TestCase):
    def setup_context(self):
        alto, api, lb, groups = writer_ingress_fixture()
        subnets = ["subnet-0123456789abcdef0", "subnet-0123456789abcdef1"]
        for service in [alto, api]:
            service["networkConfiguration"]["awsvpcConfiguration"]["subnets"] = subnets[:]
        interfaces = [
            {"NetworkInterfaceId": "eni-api", "Groups": [{"GroupId": API_SG}, {"GroupId": API_MARKER_SG}]},
            {"NetworkInterfaceId": "eni-alto", "Groups": [{"GroupId": ALTO_SG}]},
            {"NetworkInterfaceId": "eni-alb", "Groups": [{"GroupId": ALB_SG}], "RequesterManaged": True,
             "Description": "ELB " + lb["LoadBalancerArn"].split(":loadbalancer/")[1]},
        ]
        for interface in interfaces:
            interface.update(OwnerId=ACCOUNT, VpcId=VPC)
        deployment = object.__new__(Deployment)
        deployment.ecs = ObserverEcs(api)
        deployment.ec2 = ObserverEc2(groups, interfaces)
        deployment.load_balancer = lb
        deployment.journal = {"candidate_task_definition": AFTER}
        deployment.definition = lambda arn: copy.deepcopy(api_fixture())
        deployment.service = lambda: copy.deepcopy(alto)
        return deployment, alto, api, interfaces

    def add_stopped_canary(self, deployment, scope):
        if scope == "rpc":
            definition = canary_definition(patch_definition(fixture(), IMAGE), SOURCE, "console.log('fixture')", BASELINE_URL)
            groups = [ALTO_SG]
        else:
            definition = journal_idle_definition(api_fixture(), SOURCE, "console.log('fixture')", BASELINE_URL)
            groups = [API_SG, API_MARKER_SG]
        definition["taskDefinitionArn"] = f"arn:aws:ecs:{REGION}:{ACCOUNT}:task-definition/plether-sepolia-alto-rpc-canary:1"
        arn = f"arn:aws:ecs:{REGION}:{ACCOUNT}:task/plether-sepolia/stopped-{scope}"
        eni = "eni-stopped-" + scope
        task = {"taskArn": arn, "taskDefinitionArn": definition["taskDefinitionArn"], "lastStatus": "STOPPED",
                "containers": [{"name": "plether-alto", "lastStatus": "STOPPED", "exitCode": 0}],
                "startedBy": f"alto-rpc-123-1-{scope}", "attachments": [{"type": "ElasticNetworkInterface",
                    "details": [{"name": "networkInterfaceId", "value": eni}]}]}
        record = {"scope": scope, "task": arn, "definition": definition["taskDefinitionArn"],
                  "fingerprint": fingerprint(definition)}
        deployment.journal["canary_history"] = [record]
        deployment.ecs.tasks[arn] = task
        deployment.args = SimpleNamespace(image=IMAGE)
        deployment.definition = lambda task_arn: copy.deepcopy(definition if task_arn == record["definition"] else api_fixture())
        deployment.ec2.pages[0].append({"NetworkInterfaceId": eni, "OwnerId": ACCOUNT, "VpcId": VPC,
                                        "Groups": [{"GroupId": group} for group in groups]})
        return definition, task, record, eni, groups

    def test_context_uses_the_pinned_running_api_image_and_its_exact_writer_network(self):
        deployment, alto, api, _ = self.setup_context()
        definition, network = deployment.journal_observer_context(alto)
        self.assertEqual(definition, api_fixture())
        self.assertEqual(network, api["networkConfiguration"])
        network["awsvpcConfiguration"]["securityGroups"].clear()
        self.assertEqual(api["networkConfiguration"]["awsvpcConfiguration"]["securityGroups"], [API_SG, API_MARKER_SG])

    def test_api_running_digest_revision_or_task_count_drift_blocks_the_journal_observer(self):
        for case in ["digest", "revision", "second_task", "missing_eni"]:
            deployment, alto, _, _ = self.setup_context()
            arn = deployment.ecs.task_lists["plether-api"][0]
            task = deployment.ecs.tasks[arn]
            if case == "digest":
                task["containers"][0]["imageDigest"] = "sha256:" + "0" * 64
            elif case == "revision":
                task["taskDefinitionArn"] = task["taskDefinitionArn"].replace(":148", ":149")
            elif case == "second_task":
                deployment.ecs.task_lists["plether-api"].append(arn + "-other")
            else:
                task["attachments"] = []
            with self.subTest(case=case), self.assertRaises(GuardRejected):
                deployment.journal_observer_context(alto)

    def test_all_eni_pages_are_checked_and_a_second_api_writer_is_never_ignored(self):
        deployment, alto, _, interfaces = self.setup_context()
        deployment.ec2.pages = [interfaces[:1], interfaces[1:]]
        deployment.journal_observer_context(alto)
        self.assertEqual(len(deployment.ec2.requests), 2)
        self.assertEqual(deployment.ec2.requests[1]["NextToken"], "1")
        intruder = copy.deepcopy(interfaces[0])
        intruder["NetworkInterfaceId"] = "eni-untrusted-writer"
        deployment.ec2.pages[1].append(intruder)
        with self.assertRaisesRegex(GuardRejected, "untrusted_api_writer_interface"):
            deployment.journal_observer_context(alto)

    def test_eni_pagination_must_finish_within_the_bounded_read(self):
        deployment, alto, _, _ = self.setup_context()
        deployment.ec2.unbounded = True
        with self.assertRaisesRegex(GuardRejected, "writer_network_pagination_requires_reconciliation"):
            deployment.journal_observer_context(alto)
        self.assertEqual(len(deployment.ec2.requests), 10)

    def test_writer_enis_cannot_be_foreign_or_attach_unreviewed_groups(self):
        for index in range(3):
            for key, bad_value in [("OwnerId", "000000000000"), ("VpcId", "vpc-unreviewed")]:
                deployment, alto, _, interfaces = self.setup_context()
                interfaces[index][key] = bad_value
                with self.subTest(eni=index, key=key), self.assertRaisesRegex(GuardRejected, "foreign_writer_network_interface"):
                    deployment.journal_observer_context(alto)
            deployment, alto, _, interfaces = self.setup_context()
            interfaces[index]["Groups"].append({"GroupId": "sg-unreviewed"})
            with self.subTest(eni=index, key="extra_group"), self.assertRaises(GuardRejected):
                deployment.journal_observer_context(alto)
        for key, bad_value in [("RequesterManaged", False), ("Description", "ELB another-load-balancer")]:
            deployment, alto, _, interfaces = self.setup_context()
            interfaces[2][key] = bad_value
            with self.subTest(key=key), self.assertRaisesRegex(GuardRejected, "untrusted_alto_load_balancer_interface"):
                deployment.journal_observer_context(alto)

    def test_only_an_exact_stopped_current_run_canary_may_leave_a_temporary_writer_eni(self):
        for scope in ["rpc", "journal_idle"]:
            deployment, alto, _, _ = self.setup_context()
            _, _, _, eni, groups = self.add_stopped_canary(deployment, scope)
            with self.subTest(scope=scope), patch.dict("os.environ", {"GITHUB_RUN_ID": "123", "GITHUB_RUN_ATTEMPT": "1"}):
                self.assertEqual(deployment.stopped_canary_interfaces(), {eni: set(groups)})
                deployment.journal_observer_context(alto)
                deployment.ec2.pages[0][-1]["Groups"].append({"GroupId": "sg-unreviewed"})
                with self.assertRaisesRegex(GuardRejected, "recorded_canary_network_drift"):
                    deployment.journal_observer_context(alto)

    def test_exited_current_run_canary_eni_is_safe_while_ecs_finishes_task_cleanup(self):
        for status in ["RUNNING", "STOPPING", "DEPROVISIONING"]:
            for code in [0, 1]:
                deployment, alto, _, _ = self.setup_context()
                _, task, _, eni, groups = self.add_stopped_canary(deployment, "journal_idle")
                task["lastStatus"] = status
                task["containers"][0]["exitCode"] = code
                with self.subTest(task_status=status, exit_code=code), patch.dict("os.environ", {"GITHUB_RUN_ID": "123", "GITHUB_RUN_ATTEMPT": "1"}):
                    self.assertEqual(deployment.stopped_canary_interfaces(), {eni: set(groups)})
                    deployment.journal_observer_context(alto)

    def test_unknown_or_ambiguous_canary_exit_metadata_cannot_exempt_a_writer_eni(self):
        for case in ["missing_code", "boolean_code", "wrong_name", "extra_container"]:
            deployment, _, _, _ = self.setup_context()
            _, task, _, _, _ = self.add_stopped_canary(deployment, "journal_idle")
            if case == "missing_code":
                task["containers"][0].pop("exitCode")
            elif case == "boolean_code":
                task["containers"][0]["exitCode"] = False
            elif case == "wrong_name":
                task["containers"][0]["name"] = "plether-api"
            else:
                task["containers"].append(copy.deepcopy(task["containers"][0]))
            with self.subTest(case=case), patch.dict("os.environ", {"GITHUB_RUN_ID": "123", "GITHUB_RUN_ATTEMPT": "1"}):
                with self.assertRaises(GuardRejected):
                    deployment.stopped_canary_interfaces()

    def test_running_foreign_or_modified_canaries_cannot_be_exempted_from_writer_ownership(self):
        for case in ["active_container", "foreign_run", "different_definition", "fingerprint", "task_role", "signer", "image", "server", "restart"]:
            deployment, _, _, _ = self.setup_context()
            definition, task, record, _, _ = self.add_stopped_canary(deployment, "journal_idle")
            if case == "active_container":
                task["desiredStatus"] = "STOPPED"
                task["containers"][0]["lastStatus"] = "RUNNING"
            elif case == "foreign_run":
                task["startedBy"] = "alto-rpc-other-run-journal_idle"
            elif case == "different_definition":
                task["taskDefinitionArn"] += "0"
            elif case == "fingerprint":
                record["fingerprint"] = "0" * 64
            else:
                if case == "task_role":
                    definition["taskRoleArn"] = f"arn:aws:iam::{ACCOUNT}:role/plether-sepolia-api-task"
                elif case == "signer":
                    definition["containerDefinitions"][0]["secrets"].append({"name": "ALTO_EXECUTOR_PRIVATE_KEYS", "valueFrom": PREFIX + "alto-executor-private-keys"})
                elif case == "image":
                    definition["containerDefinitions"][0]["image"] = IMAGE
                elif case == "restart":
                    definition["containerDefinitions"][0]["restartPolicy"] = {"enabled": True}
                else:
                    definition["containerDefinitions"][0]["entryPoint"] = ["/app/api-server"]
                record["fingerprint"] = fingerprint(definition)
            with self.subTest(case=case), patch.dict("os.environ", {"GITHUB_RUN_ID": "123", "GITHUB_RUN_ATTEMPT": "1"}):
                with self.assertRaises(GuardRejected):
                    deployment.stopped_canary_interfaces()


class FakeEcs:
    def __init__(self, events):
        self.events = events

    def update_service(self, **kwargs):
        assert kwargs["cluster"] == "plether-sepolia"
        assert kwargs["service"] == "plether-alto"
        self.events.append(("update", kwargs))
        return {"service": {}}


class RpcCutoverSequencingTests(unittest.TestCase):
    def test_signer_capture_cannot_turn_a_fresh_drain_into_a_stale_authorization_to_stop(self):
        for rollback, journal in [(False, {}), (True, {"admission_opened": True})]:
            events, saved = [], []
            now_ms = [1_794_213_000_000]
            deployment = object.__new__(Deployment)
            deployment.canary_proof = proof_fixture("journal_idle", now_ms[0])
            deployment.rollback, deployment.journal = rollback, journal
            deployment.baseline_service = service_fixture()
            deployment.journal_observer_context = lambda service: events.append(("recheck",))
            def capture():
                events.append(("capture",))
                now_ms[0] += 30_001
            deployment.capture_signers = capture
            with self.subTest(rollback=rollback), patch("alto_rpc_cutover.time.time", side_effect=lambda: now_ms[0] / 1000):
                with self.assertRaisesRegex(GuardRejected, "stale_journal_drain_before_stop"):
                    perform_cutover(FakeEcs(events), BEFORE, AFTER, lambda: True, deployment.quiescent,
                        lambda: self.fail("The signer must remain running"), lambda target: None,
                        lambda: None, lambda value: saved.append(copy.deepcopy(value)),
                        cleanup=lambda: events.append(("cleanup",)), before_stop=deployment.before_signer_stop)
            self.assertEqual(events, [("capture",), ("recheck",), ("cleanup",)])
            self.assertFalse(any(value.get("mutation_started") for value in saved))

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
            self.assertEqual(observed, ["journal_idle"])
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
            self.assertEqual(scopes, ["journal_idle"], "A currently visible pause cannot erase the previous open-admission interval")
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
    def setUp(self):
        baseline_pin = patch("alto_rpc_cutover.BASELINE_FINGERPRINT", fingerprint(fixture()))
        baseline_pin.start()
        self.addCleanup(baseline_pin.stop)

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
            with self.assertRaisesRegex(GuardRejected, "journal_provenance_baseline_mismatch"):
                deployment.recover()
            self.assertEqual(events, [])

    def test_a_changed_journal_fingerprint_cannot_override_the_reviewed_baseline_pin(self):
        with TemporaryDirectory() as directory:
            deployment, _, journal, events = self.setup_recovery(directory)
            journal["baseline_fingerprint"] = "0" * 64
            deployment.path.write_text(json.dumps(journal))
            with self.assertRaisesRegex(GuardRejected, "recovery_baseline_drift"):
                deployment.recover()
            self.assertEqual(events, [])

    def test_provenance_pin_mismatch_blocks_even_an_internally_consistent_recovery_journal(self):
        with TemporaryDirectory() as directory, patch("alto_rpc_cutover.BASELINE_FINGERPRINT", "0" * 64):
            deployment, _, _, events = self.setup_recovery(directory)
            with self.assertRaisesRegex(GuardRejected, "journal_provenance_baseline_mismatch"):
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
