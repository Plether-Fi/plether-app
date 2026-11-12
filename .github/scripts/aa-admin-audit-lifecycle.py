#!/usr/bin/env python3
"""Keep the one-off audit collector alive until the workflow verifies evidence."""
import argparse
import json
import re
import subprocess
import time

CLUSTER = 'plether-sepolia'
APP, INIT, ROUTER = 'plether-aa-admin', 'aa-admin-tmp-init', 'otel-log-router'
CAPABILITIES = {'fixed-digest-kms-attestation', 'resume-aa-issuance'}


class GuardRejected(ValueError):
    pass


def require(ok, code):
    if not ok:
        raise GuardRejected(code)


def validate_context(task, definition, owner, capability):
    prefix = 'arn:aws:ecs:ap-southeast-1:932542905614:'
    require(task.startswith(prefix + 'task/plether-sepolia/')
            and re.fullmatch(r'[0-9a-f]{32}', task.rsplit('/', 1)[-1]), 'invalid_task')
    require(re.fullmatch(re.escape(prefix) + r'task-definition/plether-sepolia-aa-admin-(kms-attest|resume-issuance):[1-9][0-9]*', definition), 'invalid_definition')
    require(re.fullmatch(r'aa-admin-[0-9]+-[1-9][0-9]*', owner), 'invalid_owner')
    require(capability in CAPABILITIES, 'invalid_capability')
    family = 'kms-attest' if capability == 'fixed-digest-kms-attestation' else 'resume-issuance'
    require(f'/plether-sepolia-aa-admin-{family}:' in definition, 'capability_family_mismatch')


def no_effective_overrides(task):
    if 'overrides' not in task:
        return True
    value = task['overrides']
    if not isinstance(value, dict) or set(value) - {'containerOverrides', 'inferenceAcceleratorOverrides'}:
        return False
    if 'inferenceAcceleratorOverrides' in value and value['inferenceAcceleratorOverrides'] != []:
        return False
    if 'containerOverrides' not in value:
        return True
    rows = value['containerOverrides']
    return isinstance(rows, list) and all(isinstance(row, dict) and set(row) == {'name'}
        and row['name'] in {APP, INIT, ROUTER} for row in rows) and len({row['name'] for row in rows}) == len(rows)


def inspect_task(response, task, definition, owner, capability):
    require(response.get('failures') == [] and len(response.get('tasks', [])) == 1, 'task_readback_failed')
    current = response['tasks'][0]
    require(current.get('taskArn') == task and current.get('taskDefinitionArn') == definition
            and current.get('startedBy') == owner and current.get('launchType') == 'FARGATE'
            and current.get('platformVersion') == '1.4.0', 'task_identity_mismatch')
    require(no_effective_overrides(current), 'effective_overrides')
    tags = current.get('tags', [])
    for key, value in [('WorkflowOwner', owner), ('Capability', capability)]:
        require(sum(row.get('key') == key and row.get('value') == value for row in tags) == 1
                and sum(row.get('key') == key for row in tags) == 1, 'task_tags_mismatch')
    containers = current.get('containers', [])
    require(len(containers) == 3 and {row.get('name') for row in containers} == {APP, INIT, ROUTER}, 'unexpected_containers')
    require(current.get('desiredStatus') == 'RUNNING' and current.get('lastStatus') in {'PROVISIONING', 'PENDING', 'ACTIVATING', 'RUNNING'}, 'collector_not_running')
    rows = {row['name']: row for row in containers}
    require(rows[ROUTER].get('lastStatus') != 'STOPPED', 'collector_stopped_early')
    if rows[INIT].get('lastStatus') == 'STOPPED':
        require(type(rows[INIT].get('exitCode')) is int and rows[INIT]['exitCode'] == 0, 'init_failed')
    if rows[APP].get('lastStatus') != 'STOPPED':
        return False
    require(type(rows[APP].get('exitCode')) is int and rows[APP]['exitCode'] == 0, 'operation_failed')
    require(rows[INIT].get('lastStatus') == 'STOPPED' and rows[INIT].get('exitCode') == 0
            and rows[ROUTER].get('lastStatus') == 'RUNNING' and current.get('lastStatus') == 'RUNNING', 'collector_not_ready')
    return True


def aws(*args):
    result = subprocess.run(['aws', '--region', 'ap-southeast-1', *args, '--output', 'json', '--no-cli-pager'],
                            capture_output=True, text=True, timeout=45)
    require(result.returncode == 0, 'aws_operation_failed')
    return json.loads(result.stdout)


def wait_for_operation(read, sleep=time.sleep, clock=time.monotonic, timeout=600):
    deadline = clock() + timeout
    while clock() < deadline:
        if read():
            return
        sleep(min(5, max(0, deadline - clock())))
    raise GuardRejected('operation_wait_expired')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('operation', choices=['wait', 'stop'])
    parser.add_argument('--task', required=True)
    parser.add_argument('--definition', required=True)
    parser.add_argument('--owner', required=True)
    parser.add_argument('--capability', required=True)
    args = parser.parse_args()
    validate_context(args.task, args.definition, args.owner, args.capability)
    context = (args.task, args.definition, args.owner, args.capability)

    def ready():
        result = aws('ecs', 'describe-tasks', '--cluster', CLUSTER, '--tasks', args.task, '--include', 'TAGS')
        return inspect_task(result, *context)

    if args.operation == 'wait':
        wait_for_operation(ready)
    else:
        # Called only after the workflow's unchanged event/content audit checks.
        # Revalidate ownership, immutable definition, exit codes and collector
        # liveness immediately before requesting a stop of this exact task.
        require(ready(), 'operation_not_complete')
        aws('ecs', 'stop-task', '--cluster', CLUSTER, '--task', args.task,
            '--reason', 'AA admin audit evidence verified; finish the owned one-off task')
    print(json.dumps({'event': 'aa_admin_audit_lifecycle', 'operation': args.operation, 'pass': True}))


if __name__ == '__main__':
    try:
        main()
    except Exception:
        # AWS/parser exception text can contain task command arguments.
        print('AA admin audit lifecycle guard failed; inspect restricted task evidence.', file=__import__('sys').stderr)
        raise SystemExit(1)
