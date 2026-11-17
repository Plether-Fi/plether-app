"""Boundary/cleanup tests; these do not claim real AWS alarm qualification."""
import base64
import copy
import importlib.util
import json
import pathlib
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location("drill", pathlib.Path(__file__).with_name("drill-observer-failures.py"))
d = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(d)
CODE = base64.b64encode(b"a" * 32).decode()


def environment():
    return {"EXPECTED_ACCOUNT_ID": d.ACCOUNT, "ECS_CLUSTER_ARN": d.CLUSTER_ARN, "SERVICE_NAMES": json.dumps([d.SERVICE]),
            "EVIDENCE_LOG_GROUP": "/plether/sepolia-recovery-drill/ecs-incidents", "OPERATIONS_TOPIC_ARN": d.TOPIC, "ESCALATION_TOPIC_ARN": d.MIRROR}


def permission():
    return {"Sid": d.STATEMENT, "Effect": "Allow", "Principal": {"Service": "events.amazonaws.com"}, "Action": "lambda:InvokeFunction", "Resource": d.FUNCTION,
            "Condition": {"StringEquals": {"AWS:SourceAccount": d.ACCOUNT}, "ArnLike": {"AWS:SourceArn": d.RULE_ARN}}}


class FixtureAWS:
    def __init__(self, lose_response=None, concurrent_env=False):
        self.function = {"FunctionArn": d.FUNCTION, "FunctionName": d.NAME, "Role": d.ROLE, "Handler": "handler.handler", "Runtime": "python3.13", "Architectures": ["arm64"],
                         "State": "Active", "LastUpdateStatus": "Successful", "Timeout": 25, "CodeSha256": CODE, "RevisionId": "config-1", "Environment": {"Variables": environment()}}
        self.rule = {"Name": d.RULE, "Arn": d.RULE_ARN, "State": "ENABLED", "ScheduleExpression": "rate(1 minute)", "EventBusName": "default"}
        self.policy = {"Statement": [permission(), {"Sid": "AllowEvents-stopped"}]}
        self.policy_revision = 1
        self.calls = []
        self.lose_response = lose_response
        self.concurrent_env = concurrent_env

    def call(self, service, operation, **kwargs):
        self.calls.append((service, operation, copy.deepcopy(kwargs)))
        if service == "lambda":
            assert kwargs["function_name"] == d.NAME
            if operation == "get-function-configuration":
                return copy.deepcopy(self.function)
            if operation == "update-function-configuration":
                assert kwargs["revision_id"] == self.function["RevisionId"]
                self.function["Environment"] = copy.deepcopy(kwargs["environment"])
                self.function["RevisionId"] += "x"
                if self.concurrent_env:
                    self.function["Environment"]["Variables"]["UNRELATED"] = "do-not-overwrite"
                result = copy.deepcopy(self.function)
            elif operation == "get-policy":
                return {"Policy": json.dumps(self.policy), "RevisionId": str(self.policy_revision)}
            elif operation == "remove-permission":
                assert kwargs["statement_id"] == d.STATEMENT and kwargs["revision_id"] == str(self.policy_revision)
                self.policy["Statement"] = [s for s in self.policy["Statement"] if s["Sid"] != d.STATEMENT]
                self.policy_revision += 1
                result = {}
            elif operation == "add-permission":
                assert kwargs["statement_id"] == d.STATEMENT and kwargs["source_arn"] == d.RULE_ARN and kwargs["source_account"] == d.ACCOUNT
                assert kwargs["principal"] == "events.amazonaws.com" and kwargs["action"] == "lambda:InvokeFunction" and kwargs["revision_id"] == str(self.policy_revision)
                self.policy["Statement"].append(permission())
                self.policy_revision += 1
                result = {}
            else:
                raise AssertionError(operation)
        elif service == "events":
            assert kwargs["name"] == d.RULE and kwargs["event_bus_name"] == "default"
            if operation == "describe-rule":
                return copy.deepcopy(self.rule)
            assert operation in {"enable-rule", "disable-rule"}
            self.rule["State"] = "ENABLED" if operation == "enable-rule" else "DISABLED"
            result = {}
        else:
            raise AssertionError((service, operation))
        if operation == self.lose_response:
            self.lose_response = None
            raise d.AWSFailure("CLI_REQUEST_FAILED")
        return result


class BoundedFailureDrill(d.Drill):
    def wait_alarms(self, requirements, since=None):
        if "ALARM" in requirements.values():
            raise RuntimeError("simulated bounded alarm wait failure")
        return {}


class ObserverFaultGuardTests(unittest.TestCase):
    def make_drill(self, aws, directory):
        drill = BoundedFailureDrill(aws, directory, interval=5)
        drill.state = {"schema_version": 1, "capability": "isolated-observer-fault-drill", "account": d.ACCOUNT, "region": d.REGION,
                       "baseline": {"function": d.FUNCTION, "code_hash": CODE, "environment": environment(), "permission": permission()},
                       "active_fault": None, "events": [], "phases": [], "notifications": []}
        drill.save()
        return drill

    def test_foreign_function_role_code_or_environment_is_refused(self):
        fixture = FixtureAWS().function
        for key, wrong in [("FunctionArn", d.FUNCTION.replace("recovery-drill", "incident")), ("Role", d.ROLE.replace("recovery-drill", "incident")), ("CodeSha256", "b" * 44)]:
            altered = copy.deepcopy(fixture)
            altered[key] = wrong
            with self.assertRaises(ValueError):
                d.validate_function(altered, CODE)
        altered = copy.deepcopy(fixture)
        altered["Environment"]["Variables"]["ECS_CLUSTER_ARN"] = d.CLUSTER_ARN.replace("recovery-drill", "sepolia")
        with self.assertRaises(ValueError):
            d.validate_function(altered, CODE)

    def test_foreign_invoke_permission_source_or_wildcard_is_refused(self):
        for value in [d.RULE_ARN.replace("recovery-drill", "production"), "*"]:
            altered = permission()
            altered["Condition"]["ArnLike"]["AWS:SourceArn"] = value
            with self.assertRaises(ValueError):
                d.validate_permission(altered)

    def test_each_fault_restores_exact_state_after_bounded_wait_failure(self):
        for kind in ["read-denial", "delivery-failure", "silence"]:
            with self.subTest(kind=kind), tempfile.TemporaryDirectory() as directory:
                aws = FixtureAWS()
                drill = self.make_drill(aws, directory)
                with self.assertRaisesRegex(RuntimeError, "bounded"):
                    drill.phase(kind)
                self.assertEqual(aws.function["Environment"]["Variables"], environment())
                self.assertEqual(aws.rule["State"], "ENABLED")
                d.poll_permission({"Policy": json.dumps(aws.policy)})
                self.assertIsNone(drill.state["active_fault"])
                self.assertTrue(any(e["event"] == "fault_restored" for e in drill.state["events"]))
                self.assertFalse(any(service == "iam" for service, _, _ in aws.calls), "Faults must never write IAM policies")

    def test_each_mutation_is_restored_even_when_success_response_is_lost(self):
        for kind, operation in [("read-denial", "update-function-configuration"), ("delivery-failure", "remove-permission"), ("silence", "disable-rule")]:
            with self.subTest(kind=kind), tempfile.TemporaryDirectory() as directory:
                aws = FixtureAWS(lose_response=operation)
                drill = self.make_drill(aws, directory)
                with self.assertRaises(d.AWSFailure):
                    drill.phase(kind)
                self.assertEqual(aws.function["Environment"]["Variables"], environment())
                self.assertEqual(aws.rule["State"], "ENABLED")
                d.poll_permission({"Policy": json.dumps(aws.policy)})
                self.assertIsNone(drill.state["active_fault"])

    def test_restore_refuses_to_overwrite_a_concurrent_unrelated_environment_change(self):
        with tempfile.TemporaryDirectory() as directory:
            aws = FixtureAWS(concurrent_env=True)
            drill = self.make_drill(aws, directory)
            with self.assertRaisesRegex(ValueError, "Unexpected observer environment keys"):
                drill.phase("read-denial")
            self.assertEqual(len([c for c in aws.calls if c[1] == "update-function-configuration"]), 1)
            self.assertEqual(aws.function["Environment"]["Variables"]["UNRELATED"], "do-not-overwrite")
            self.assertIsNotNone(drill.state["active_fault"], "Blocked restore must remain explicit in the durable journal")

    def test_restart_can_restore_a_fault_from_the_durable_journal(self):
        with tempfile.TemporaryDirectory() as directory:
            aws = FixtureAWS()
            original = self.make_drill(aws, directory)
            original.prepare_fault("silence")
            aws.rule["State"] = "DISABLED"
            restarted = d.Drill(aws, directory)
            restarted.state = json.loads(restarted.path.read_text())
            restarted.restore()
            self.assertEqual(aws.rule["State"], "ENABLED")
            self.assertIsNone(restarted.state["active_fault"])

    def test_notification_summary_drops_raw_payload_and_receipt_handles(self):
        body = {"AlarmName": d.ALARMS["observer-missing"], "AWSAccountId": d.ACCOUNT, "NewStateValue": "ALARM", "StateChangeTime": "2026-11-17T12:00:00Z", "untrusted": "secret-do-not-save"}
        value = d.safe_notification({"MessageId": "12345678-1234-1234-1234-123456789012", "ReceiptHandle": "secret-handle", "Body": json.dumps(body)})
        self.assertNotIn("secret", json.dumps(value))
        body["AlarmName"] = "production-alarm"
        with self.assertRaises(ValueError):
            d.safe_notification({"MessageId": "id", "Body": json.dumps(body)})

    def test_notification_rejects_foreign_sns_envelopes(self):
        with self.assertRaisesRegex(ValueError, "Foreign SNS"):
            d.safe_notification({"MessageId": "id", "Body": json.dumps({"Type": "Notification", "TopicArn": d.TOPIC, "Message": "{}"})})


if __name__ == "__main__":
    unittest.main()
