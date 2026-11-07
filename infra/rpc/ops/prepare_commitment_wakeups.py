#!/usr/bin/env python3
"""Prepare mode-only Sepolia task revisions; backend workflows deploy them.

No services, IAM permissions, secrets or Terraform state are changed/read. Use
a separate private directory for each observe/active/off rollout. Turning the
mode off uses the same preparation and deployment path as enabling it.
"""
import argparse
from copy import deepcopy
from datetime import datetime, timezone
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re

ACCOUNT = '932542905614'
REGION = 'ap-southeast-1'
CLUSTER = 'plether-sepolia'
MODE_KEY = 'PERPS_COMMITMENT_WAKEUPS_MODE'
MODES = ('off', 'observe', 'active')
SERVICES = {
    'plether-api': ('plether-sepolia', 1),
    'plether-workers': ('plether-sepolia-workers', 1),
    'plether-keeper': ('plether-sepolia-keeper', 0),
    'plether-basket-worker': ('plether-sepolia-basket-worker', 0),
}
TARGETS = {
    'plether-sepolia': ('plether-api',),
    'plether-sepolia-workers': ('plether-keeper', 'plether-basket-worker'),
    'plether-sepolia-keeper': ('plether-keeper',),
    'plether-sepolia-basket-worker': ('plether-basket-worker',),
}
OUTPUT_FIELDS = {'taskDefinitionArn', 'revision', 'status', 'requiresAttributes',
                 'compatibilities', 'registeredAt', 'registeredBy', 'deregisteredAt'}


class GuardRejected(ValueError):
    pass


def require(condition, reason):
    if not condition:
        raise GuardRejected(reason)


def registration_request(definition, allowed):
    # ECS rejects tags=[]; omission preserves an untagged task definition.
    return {k: v for k, v in definition.items()
            if k in allowed and not (k == 'tags' and not v)}


def mode_values(definition):
    """Validate the mode's exact consumers and return their effective values."""
    require(definition.get('family') in TARGETS, 'unexpected_family')
    require(definition.get('networkMode') == 'awsvpc'
            and definition.get('runtimePlatform', {}).get('cpuArchitecture') == 'ARM64'
            and 'FARGATE' in definition.get('requiresCompatibilities', []),
            'unexpected_task_topology')
    containers = definition['containerDefinitions']
    require(len({c['name'] for c in containers}) == len(containers), 'duplicate_container_names')
    found = {}
    for container in containers:
        env, secrets = container.get('environment', []), container.get('secrets', [])
        require(len({x['name'] for x in env}) == len(env), 'duplicate_environment_names')
        require(len({x['name'] for x in secrets}) == len(secrets), 'duplicate_secret_names')
        require(not any(x['name'] == MODE_KEY for x in secrets), 'mode_is_secret_bound')
        entries = [x['value'] for x in env if x['name'] == MODE_KEY]
        if container['name'] not in TARGETS[definition['family']]:
            require(not entries, 'unexpected_mode_container')
            continue
        arguments = container.get('command', []) + container.get('entryPoint', [])
        require(not any(arg.startswith(('--commitment-wakeups-mode', '--commitment-wakeup-mode', '--wakeups-mode'))
                        or MODE_KEY in arg for arg in arguments), 'cli_mode_override')
        value = entries[0] if entries else 'off'
        require(value in MODES, 'unexpected_existing_mode')
        found[container['name']] = value
    require(set(found) == set(TARGETS[definition['family']]), 'missing_target_container')
    return found


def fingerprint(definition, release_independent=False):
    """Hash all policy/configuration except approved mode and release slots."""
    require(definition.get('family') in TARGETS, 'unexpected_family')
    value = deepcopy({k: v for k, v in definition.items() if k not in OUTPUT_FIELDS})
    for container in value['containerDefinitions']:
        is_target = container['name'] in TARGETS[definition['family']]
        container['environment'] = sorted(
            [x for x in container.get('environment', [])
             if not (is_target and x['name'] == MODE_KEY)
             and not (release_independent and container['name'] == 'otel-log-router'
                      and x['name'] == 'SERVICE_VERSION')], key=lambda x: x['name'])
        container['secrets'] = sorted(container.get('secrets', []), key=lambda x: x['name'])
        if release_independent:
            container.pop('image', None)
    value['containerDefinitions'].sort(key=lambda x: x['name'])
    value['tags'] = sorted(value.get('tags', []), key=lambda x: x['key'])
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def patch_definition(definition, desired):
    require(desired in MODES, 'invalid_desired_mode')
    previous = mode_values(definition)
    updated = deepcopy(definition)
    changes = []
    for container in updated['containerDefinitions']:
        if container['name'] not in previous:
            continue
        env = container.setdefault('environment', [])
        entries = [x for x in env if x['name'] == MODE_KEY]
        changes.append({'container': container['name'], 'name': MODE_KEY,
                        'before': previous[container['name']], 'before_present': bool(entries),
                        'after': desired})
        if entries:
            entries[0]['value'] = desired
        else:
            env.append({'name': MODE_KEY, 'value': desired})
    require(fingerprint(definition) == fingerprint(updated), 'non_mode_configuration_changed')
    require_desired_mode(updated, desired)
    return updated, changes


def require_desired_mode(definition, desired):
    require(desired in MODES, 'invalid_desired_mode')
    values = mode_values(definition)
    require(all(value == desired for value in values.values()), 'mode_drift')
    # Prepared modes must be explicit, including off; an absent value cannot
    # disguise an incomplete registration during resume/verification.
    for container in definition['containerDefinitions']:
        if container['name'] in values:
            require([x['value'] for x in container.get('environment', []) if x['name'] == MODE_KEY]
                    == [desired], 'missing_explicit_mode')


def keeper_config(definition):
    keepers = [c for c in definition['containerDefinitions'] if c['name'] == 'plether-keeper']
    require(len(keepers) == 1, 'missing_keeper')
    return {k: sorted(keepers[0].get(k, []), key=lambda x: x['name'])
            for k in ['environment', 'secrets']}


def validate_journal(journal, desired=None):
    require(journal.get('schema_version') == 1 and journal.get('account') == ACCOUNT
            and journal.get('region') == REGION and journal.get('cluster') == CLUSTER,
            'unexpected_journal_scope')
    require(journal.get('mode') in MODES and journal.get('baseline_mode') in MODES,
            'unexpected_journal_mode')
    require(desired is None or journal['mode'] == desired, 'journal_mode_differs')
    require(set(journal.get('services', {})) == set(SERVICES), 'unexpected_journal_services')
    for name, state in journal['services'].items():
        family, count = SERVICES[name]
        prefix = f'arn:aws:ecs:{REGION}:{ACCOUNT}:task-definition/{family}:'
        require(state.get('desired_count') == count, 'unexpected_journal_count')
        for field in ['previous_task_definition', 'prepared_task_definition']:
            if field in state:
                require(re.fullmatch(re.escape(prefix) + r'[1-9][0-9]*', state[field]) is not None,
                        'unexpected_journal_task_definition')
        require('previous_task_definition' in state, 'missing_journal_baseline')
        for field in ['policy_hash', 'preparation_hash']:
            require(re.fullmatch(r'[0-9a-f]{64}', state.get(field, '')) is not None,
                    'unexpected_journal_fingerprint')


def stable_services(ecs):
    response = ecs.describe_services(cluster=CLUSTER, services=list(SERVICES))
    require(not response['failures'] and len(response['services']) == len(SERVICES), 'missing_services')
    live = {v['serviceName']: v for v in response['services']}
    require(set(live) == set(SERVICES), 'unexpected_services')
    for name, v in live.items():
        require(v['desiredCount'] == v['runningCount'] == SERVICES[name][1]
                and v['pendingCount'] == 0, 'unexpected_service_count')
        require(len(v['deployments']) == 1
                and v['deployments'][0]['rolloutState'] == 'COMPLETED', 'service_not_stable')
    return live


def execute(operation, desired, work, ecs):
    """SDK-independent orchestration also exercised by the fake-ECS tests."""
    path = work / 'commitment-wakeups-rollout.json'
    def get(arn):
        response = ecs.describe_task_definition(taskDefinition=arn, include=['TAGS'])
        return {**response['taskDefinition'], 'tags': response.get('tags', [])}
    def save(journal):
        temporary = path.with_suffix('.next.json')
        temporary.write_text(json.dumps(journal, indent=2) + '\n')
        temporary.replace(path)
    def register(definition):
        allowed = set(ecs.meta.service_model.operation_model('RegisterTaskDefinition').input_shape.members)
        return ecs.register_task_definition(**registration_request(definition, allowed))['taskDefinition']['taskDefinitionArn']
    live = stable_services(ecs)
    if operation == 'prepare':
        require(desired in MODES, 'invalid_desired_mode')
        if path.exists():
            journal = json.loads(path.read_text())
            validate_journal(journal, desired)
            require(journal['status'] in ['preparing', 'prepared'], 'unexpected_journal_status')
        else:
            journal = {'schema_version': 1, 'status': 'preparing', 'account': ACCOUNT,
                       'region': REGION, 'cluster': CLUSTER, 'mode': desired,
                       'prepared_at_utc': datetime.now(timezone.utc).isoformat(), 'services': {}}
            modes = set()
            for name, (family, count) in SERVICES.items():
                before = get(family)
                require(count == 0 or before['taskDefinitionArn'] == live[name]['taskDefinition'],
                        'unrelated_pending_revision')
                modes.update(mode_values(before).values())
                _, changes = patch_definition(before, desired)
                journal['services'][name] = {
                    'previous_task_definition': before['taskDefinitionArn'], 'desired_count': count,
                    'changes': changes, 'policy_hash': fingerprint(before, True),
                    'preparation_hash': fingerprint(before)}
            require(len(modes) == 1, 'baseline_mode_disagreement')
            journal['baseline_mode'] = next(iter(modes))
            require(keeper_config(get('plether-sepolia-keeper')) == keeper_config(get('plether-sepolia-workers')),
                    'keeper_topology_drift')
            save(journal)
        for name, state in journal['services'].items():
            family, count = SERVICES[name]
            latest = get(family)
            if state.get('prepared_task_definition'):
                require(latest['taskDefinitionArn'] == state['prepared_task_definition'], 'prepared_revision_superseded')
                require(fingerprint(latest) == state['preparation_hash'], 'prepared_policy_drift')
                require_desired_mode(latest, desired)
                require(count == 0 or live[name]['taskDefinition'] in
                        [state['previous_task_definition'], state['prepared_task_definition']], 'service_revision_drift')
                continue
            require(latest['taskDefinitionArn'] == state['previous_task_definition'], 'baseline_revision_superseded')
            require(fingerprint(latest) == state['preparation_hash'], 'baseline_policy_drift')
            require(count == 0 or live[name]['taskDefinition'] == state['previous_task_definition'], 'service_revision_drift')
            updated, _ = patch_definition(latest, desired)
            state['prepared_task_definition'] = register(updated)
            save(journal)
            prepared = get(state['prepared_task_definition'])
            require(fingerprint(prepared) == state['preparation_hash'], 'registration_changed_non_mode_fields')
            require_desired_mode(prepared, desired)
        require(keeper_config(get('plether-sepolia-keeper')) == keeper_config(get('plether-sepolia-workers')),
                'prepared_keeper_topology_drift')
        journal['status'] = 'prepared'
    else:
        require(operation == 'verify', 'invalid_operation')
        journal = json.loads(path.read_text())
        validate_journal(journal, desired)
        require(journal['status'] in ['prepared', 'verified'], 'mode_not_prepared')
        desired = journal['mode']
        for name, state in journal['services'].items():
            family, count = SERVICES[name]
            current = get(family)
            require(fingerprint(current, True) == state['policy_hash'], 'confirmation_gas_or_other_policy_drift')
            require_desired_mode(current, desired)
            require(count == 0 or live[name]['taskDefinition'] == current['taskDefinitionArn'],
                    'prepared_family_not_running')
            state['verified_task_definition'] = current['taskDefinitionArn']
        require(keeper_config(get('plether-sepolia-keeper')) == keeper_config(get('plether-sepolia-workers')),
                'verified_keeper_topology_drift')
        journal['status'] = 'verified'
        journal['checked_at_utc'] = datetime.now(timezone.utc).isoformat()
    save(journal)
    return {'status': journal['status'], 'mode': desired,
            'confirmation_gas_and_other_policies_preserved': True,
            'standalone_keeper_and_basket_dormant': True}


def main():
    import boto3
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('operation', choices=['prepare', 'verify'])
    parser.add_argument('--mode', choices=MODES, help='Required for prepare; verify can use the journal mode.')
    parser.add_argument('--directory', type=Path, required=True)
    args = parser.parse_args()
    if args.operation == 'prepare' and args.mode is None:
        parser.error('--mode is required for prepare')
    os.umask(0o077)
    work = args.directory.resolve()
    work.mkdir(parents=True, exist_ok=True, mode=0o700)
    lock = (work / 'commitment-wakeups.lock').open('a')
    fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    session = boto3.Session(profile_name='plether', region_name=REGION)
    identity = session.client('sts').get_caller_identity()
    require(identity['Account'] == ACCOUNT and not identity['Arn'].endswith(':root'), 'wrong_aws_identity')
    print(json.dumps(execute(args.operation, args.mode, work, session.client('ecs'))))


if __name__ == '__main__':
    try:
        main()
    except Exception as error:
        print(json.dumps({'status': 'stopped', 'exception_type': type(error).__name__,
                          'reason': str(error) if isinstance(error, GuardRejected)
                          else 'inspect bounded status; sensitive details suppressed'}))
        raise SystemExit(1)
