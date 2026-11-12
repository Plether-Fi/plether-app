#!/usr/bin/env python3
"""Exercise task ownership and the real audit/readback shell with a fake AWS CLI."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import textwrap
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location('lifecycle', Path(__file__).with_name('aa-admin-audit-lifecycle.py'))
LIFECYCLE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(LIFECYCLE)
PREFIX = 'arn:aws:ecs:ap-southeast-1:932542905614:'
TASK = PREFIX + 'task/plether-sepolia/' + 'a' * 32
DEFINITION = PREFIX + 'task-definition/plether-sepolia-aa-admin-resume-issuance:99'
OWNER, CAPABILITY = 'aa-admin-12345-1', 'resume-aa-issuance'
CONTEXT = (TASK, DEFINITION, OWNER, CAPABILITY)
SOURCE = (ROOT / '.github/workflows/aa-admin.yml').read_text()


def fixture():
    return {'failures': [], 'tasks': [{
        'taskArn': TASK, 'taskDefinitionArn': DEFINITION, 'startedBy': OWNER,
        'launchType': 'FARGATE', 'platformVersion': '1.4.0',
        'lastStatus': 'RUNNING', 'desiredStatus': 'RUNNING',
        'tags': [{'key': 'WorkflowOwner', 'value': OWNER}, {'key': 'Capability', 'value': CAPABILITY}],
        'overrides': {'containerOverrides': [{'name': name} for name in [LIFECYCLE.APP, LIFECYCLE.INIT, LIFECYCLE.ROUTER]], 'inferenceAcceleratorOverrides': []},
        'containers': [
            {'name': LIFECYCLE.APP, 'lastStatus': 'STOPPED', 'exitCode': 0},
            {'name': LIFECYCLE.INIT, 'lastStatus': 'STOPPED', 'exitCode': 0},
            {'name': LIFECYCLE.ROUTER, 'lastStatus': 'RUNNING'},
        ],
    }]}


def evidence():
    return {'events': [{'message': json.dumps({
        'ecs_task_arn': TASK, 'container_name': LIFECYCLE.APP,
        'event': 'aa_admin_issuance_resumed', 'log_schema_version': 1, 'level': 'INFO',
        'previous_reason': 'fixture pause reason', 'operator_note_length': len('fixture note'),
    })}]}


# The workflow and helper invoke this executable, rather than mocking away their
# CLI arguments or the audit query/matcher. No AWS credentials/network are used.
FAKE_AWS = '''#!/usr/bin/env python3
import json, os, pathlib, sys
p = pathlib.Path(os.environ['RUNNER_TEMP'])
a = sys.argv[1:]
with (p/'calls.jsonl').open('a') as f: f.write(json.dumps(a)+'\\n')
if 'describe-tasks' in a:
    v = json.loads((p/'task.json').read_text())
    if (p/'stopped').exists():
        t = v['tasks'][0]
        t['desiredStatus'] = t['lastStatus'] = 'STOPPED'
        for c in t['containers']: c.update(lastStatus='STOPPED', exitCode=0)
    print(json.dumps(v))
elif 'filter-log-events' in a:
    counter = p/'queries'
    count = int(counter.read_text()) if counter.exists() else 0
    counter.write_text(str(count + 1))
    print((p/'events.json').read_text() if count >= 2 else '{"events":[]}')
elif 'stop-task' in a:
    assert '--task' in a and a[a.index('--task')+1] == os.environ['TASK_ARN']
    assert '--cluster' in a and a[a.index('--cluster')+1] == 'plether-sepolia'
    (p/'stopped').touch()
    print('{}')
elif a[:3] == ['ecs', 'wait', 'tasks-stopped']:
    assert (p/'stopped').exists(), 'wait-before-audit-stop'
else:
    sys.exit(42)
'''


class LifecycleTests(unittest.TestCase):
    def test_success_requires_command_init_and_live_collector(self):
        self.assertTrue(LIFECYCLE.inspect_task(fixture(), *CONTEXT))
        value = fixture()
        value['tasks'][0]['containers'][0]['lastStatus'] = 'RUNNING'
        self.assertFalse(LIFECYCLE.inspect_task(value, *CONTEXT))

    def test_readback_guards_reject_identity_and_execution_drift(self):
        changes = [
            ('wrong task', lambda t: t.update(taskArn=TASK + 'b')),
            ('wrong definition', lambda t: t.update(taskDefinitionArn=DEFINITION + '1')),
            ('wrong startedBy', lambda t: t.update(startedBy='other')),
            ('wrong platform', lambda t: t.update(platformVersion='LATEST')),
            ('wrong launch type', lambda t: t.update(launchType='EC2')),
            ('wrong owner', lambda t: t['tags'][0].update(value='other')),
            ('wrong capability', lambda t: t['tags'][1].update(value='other')),
            ('duplicate tag', lambda t: t['tags'].append(t['tags'][0].copy())),
            ('missing tag', lambda t: t.update(tags=[])),
            ('command failed', lambda t: t['containers'][0].update(exitCode=1)),
            ('missing exit code', lambda t: t['containers'][0].pop('exitCode')),
            ('boolean exit code', lambda t: t['containers'][0].update(exitCode=False)),
            ('init failed', lambda t: t['containers'][1].update(exitCode=1)),
            ('init unfinished', lambda t: t['containers'][1].update(lastStatus='RUNNING')),
            ('collector stopped', lambda t: t['containers'][2].update(lastStatus='STOPPED')),
            ('task stopping', lambda t: t.update(desiredStatus='STOPPED')),
            ('extra container', lambda t: t['containers'].append({'name': 'other'})),
            ('duplicate container', lambda t: t['containers'][2].update(name=LIFECYCLE.APP)),
        ]
        for label, mutate in changes:
            with self.subTest(label=label):
                value = fixture()
                mutate(value['tasks'][0])
                with self.assertRaises(LIFECYCLE.GuardRejected):
                    LIFECYCLE.inspect_task(value, *CONTEXT)
        for value in [{'failures': [{}], 'tasks': []}, {'failures': [], 'tasks': []}]:
            with self.assertRaises(LIFECYCLE.GuardRejected):
                LIFECYCLE.inspect_task(value, *CONTEXT)

    def test_overrides_allow_only_name_placeholders(self):
        allowed = [{}, {'containerOverrides': []}, fixture()['tasks'][0]['overrides']]
        for value in allowed:
            self.assertTrue(LIFECYCLE.no_effective_overrides({'overrides': value}))
        self.assertTrue(LIFECYCLE.no_effective_overrides({}))
        forbidden = [None, [], True, {'containerOverrides': None}, {'containerOverrides': [None]},
                     {'containerOverrides': [{'name': LIFECYCLE.APP}] * 2},
                     {'inferenceAcceleratorOverrides': [{}]}]
        for field in ['command', 'environment', 'environmentFiles', 'cpu', 'memory', 'resourceRequirements']:
            forbidden.append({'containerOverrides': [{'name': LIFECYCLE.APP, field: []}]})
        for field in ['taskRoleArn', 'executionRoleArn', 'cpu', 'memory', 'ephemeralStorage', 'unknown']:
            forbidden.append({field: None})
        for value in forbidden:
            with self.subTest(value=value):
                self.assertFalse(LIFECYCLE.no_effective_overrides({'overrides': value}))

    def test_context_limits_stop_to_exact_sepolia_capability(self):
        LIFECYCLE.validate_context(*CONTEXT)
        LIFECYCLE.validate_context(TASK, DEFINITION.replace('resume-issuance', 'kms-attest'), OWNER, 'fixed-digest-kms-attestation')
        for index, value in [(0, TASK.replace('932542905614', '123456789012')),
                             (0, TASK.replace('ap-southeast-1', 'us-east-1')),
                             (0, TASK + 'x'), (1, DEFINITION + '-other'),
                             (2, 'aa-admin-other'), (3, 'fixed-digest-kms-attestation')]:
            args = list(CONTEXT)
            args[index] = value
            with self.assertRaises(LIFECYCLE.GuardRejected):
                LIFECYCLE.validate_context(*args)

    def test_bounded_wait_and_failed_operation_never_succeeds(self):
        now = [0]
        calls = []
        def sleep(seconds):
            now[0] += seconds
        def read():
            calls.append(now[0])
            return len(calls) == 3
        LIFECYCLE.wait_for_operation(read, sleep, lambda: now[0], timeout=20)
        self.assertEqual(calls, [0, 5, 10])
        with self.assertRaisesRegex(LIFECYCLE.GuardRejected, 'operation_wait_expired'):
            LIFECYCLE.wait_for_operation(lambda: False, sleep, lambda: now[0], timeout=11)
        self.assertEqual(now[0], 21)
        with self.assertRaisesRegex(LIFECYCLE.GuardRejected, 'operation_failed'):
            LIFECYCLE.wait_for_operation(lambda: LIFECYCLE.require(False, 'operation_failed'), sleep, lambda: now[0])

    def test_aws_errors_do_not_expose_cli_stderr(self):
        with patch.object(LIFECYCLE.subprocess, 'run', return_value=subprocess.CompletedProcess([], 1, '', 'private database credential')):
            with self.assertRaisesRegex(LIFECYCLE.GuardRejected, '^aws_operation_failed$'):
                LIFECYCLE.aws('ecs', 'describe-tasks')


class WorkflowTests(unittest.TestCase):
    def execute(self, task=None, events=None):
        step = SOURCE.split('- name: Wait for terminal state and verify audit-log readback\n')[1].split('\n      - name: ')[0]
        script = textwrap.dedent(step.split('        run: |\n')[1])
        with tempfile.TemporaryDirectory(prefix='aa-audit-test-') as directory:
            path = Path(directory)
            (path/'task.json').write_text(json.dumps(task if task is not None else fixture()))
            (path/'events.json').write_text(json.dumps(events if events is not None else evidence()))
            (path/'aa-admin-started-ms').write_text('1234560000000')
            (path/'aws').write_text(FAKE_AWS)
            (path/'aws').chmod(0o700)
            (path/'sleep').write_text('#!/bin/sh\nexit 0\n')
            (path/'sleep').chmod(0o700)
            env = {**os.environ, 'PATH': directory + os.pathsep + os.environ['PATH'],
                   'RUNNER_TEMP': directory, 'ACTION': 'resume-issuance',
                   'EXPECTED_REASON': 'fixture pause reason', 'OPERATOR_NOTE': 'fixture note',
                   'EXPECTED_EVENT': 'aa_admin_issuance_resumed', 'EXPECTED_SIGNER': '',
                   'CAPABILITY': CAPABILITY, 'REGISTRATION_OWNER': OWNER,
                   'TASK_DEFINITION': DEFINITION, 'TASK_ARN': TASK,
                   'ADMIN_CONTAINER': LIFECYCLE.APP, 'TMP_INIT_CONTAINER': LIFECYCLE.INIT,
                   'ECS_CLUSTER': 'plether-sepolia', 'LOG_GROUP': '/ecs/plether-sepolia'}
            result = subprocess.run(['bash', '-c', script], cwd=ROOT, env=env, capture_output=True, text=True, timeout=30)
            calls = [json.loads(line) for line in (path/'calls.jsonl').read_text().splitlines()]
            return result, calls

    def test_late_readback_passes_before_owned_stop_and_terminal_verification(self):
        result, calls = self.execute()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        audit = next(i for i, call in enumerate(calls) if 'filter-log-events' in call)
        stop = next(i for i, call in enumerate(calls) if 'stop-task' in call)
        terminal = next(i for i, call in enumerate(calls) if call[:3] == ['ecs', 'wait', 'tasks-stopped'])
        self.assertLess(audit, stop)
        self.assertEqual(sum('filter-log-events' in call for call in calls), 3)
        self.assertLess(stop, terminal)
        query = calls[audit]
        self.assertEqual(query[query.index('--start-time') + 1], '1234560000000')
        self.assertEqual(query[query.index('--filter-pattern') + 1], '"aa_admin_issuance_resumed"')

    def test_audit_rejection_never_requests_success_stop(self):
        cases = [('missing event', {'events': []}), ('malformed JSON', {'events': [{'message': '{'}]})]
        for field, value in [('ecs_task_arn', TASK + 'b'), ('container_name', 'other'),
                             ('event', 'other'), ('log_schema_version', 2), ('level', 'DEBUG'),
                             ('previous_reason', 'other'), ('operator_note_length', 0)]:
            event = evidence()
            message = json.loads(event['events'][0]['message'])
            message[field] = value
            event['events'][0]['message'] = json.dumps(message)
            cases.append((field, event))
        for label, events in cases:
            with self.subTest(label=label):
                result, calls = self.execute(events=events)
                self.assertNotEqual(result.returncode, 0)
                self.assertFalse(any('stop-task' in call for call in calls))

    def test_failed_command_does_not_even_query_audit(self):
        value = fixture()
        value['tasks'][0]['containers'][0]['exitCode'] = 1
        result, calls = self.execute(task=value)
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse(any('stop-task' in call or 'filter-log-events' in call for call in calls))

    def test_exact_sha_checkout_precedes_credentials_and_helper(self):
        checkout = SOURCE.index('- name: Check out reviewed audit lifecycle policy')
        credentials = SOURCE.index('- name: Configure AWS credentials')
        self.assertLess(checkout, credentials)
        self.assertIn('ref: ${{ github.sha }}', SOURCE[checkout:credentials])
        self.assertIn('persist-credentials: false', SOURCE[checkout:credentials])
        self.assertIn('if: always()', SOURCE.split('- name: Stop any owned task')[1])


if __name__ == '__main__':
    unittest.main()
