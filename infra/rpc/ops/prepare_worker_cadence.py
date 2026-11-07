#!/usr/bin/env python3
"""Prepare/verify cadence-only Sepolia revisions; workflows update services.

No secret values or Terraform state are read. Journals hold identifiers,
approved numeric changes and hashes, never task configurations.
"""
import argparse
from copy import deepcopy
from datetime import datetime, timezone
import fcntl
import hashlib
import json
import os
from pathlib import Path

ACCOUNT = '932542905614'
REGION = 'ap-southeast-1'
CLUSTER = 'plether-sepolia'
SERVICES = {
    'plether-workers': ('plether-sepolia-workers', 1),
    'plether-keeper': ('plether-sepolia-keeper', 0),
    'plether-perps-indexer': ('plether-sepolia-perps-indexer', 0),
    'plether-liquidation-worker': ('plether-sepolia-liquidation-worker', 1),
    'plether-position-protection-worker': ('plether-sepolia-protection-worker', 1),
    'plether-aa-reconciler': ('plether-sepolia-aa-reconciler', 1),
}
# Only these container/environment slots may change. The keeper's active
# interval remains one second; confirmation, fee, gas and execution knobs are
# deliberately absent from this map.
TARGETS = {
    'plether-sepolia-workers': {
        'plether-keeper': {'KEEPER_IDLE_POLL_SECONDS': ('5', '1')},
        'plether-perps-indexer': {'PERPS_INDEXER_POLL_SECONDS': ('12', '2')},
    },
    'plether-sepolia-keeper': {'plether-keeper': {'KEEPER_IDLE_POLL_SECONDS': ('5', '1')}},
    'plether-sepolia-perps-indexer': {'plether-perps-indexer': {'PERPS_INDEXER_POLL_SECONDS': ('12', '2')}},
    'plether-sepolia-liquidation-worker': {'plether-liquidation-worker': {'LIQUIDATION_WORKER_POLL_SECONDS': ('5', '2')}},
    'plether-sepolia-protection-worker': {'plether-position-protection-worker': {'PROTECTION_WORKER_POLL_SECONDS': ('5', '2')}},
    'plether-sepolia-aa-reconciler': {'plether-aa-reconciler': {'AA_RECONCILER_POLL_SECONDS': ('5', '2')}},
}
OUTPUT_FIELDS = {'taskDefinitionArn', 'revision', 'status', 'requiresAttributes',
                 'compatibilities', 'registeredAt', 'registeredBy', 'deregisteredAt'}


class GuardRejected(ValueError):
    pass


def require(condition, reason):
    if not condition:
        raise GuardRejected(reason)


def fingerprint(definition, release_independent=False):
    value = deepcopy({k: v for k, v in definition.items() if k not in OUTPUT_FIELDS})
    targets = TARGETS[definition['family']]
    for container in value['containerDefinitions']:
        allowed = targets.get(container['name'], {})
        container['environment'] = sorted(
            [x for x in container.get('environment', []) if x['name'] not in allowed
             and not (release_independent and container['name'] == 'otel-log-router' and x['name'] == 'SERVICE_VERSION')],
            key=lambda x: x['name'])
        container['secrets'] = sorted(container.get('secrets', []), key=lambda x: x['name'])
        if release_independent:
            container.pop('image', None)
    value['containerDefinitions'].sort(key=lambda x: x['name'])
    value['tags'] = sorted(value.get('tags', []), key=lambda x: x['key'])
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def patch_definition(definition):
    require(definition['family'] in TARGETS, 'unexpected_family')
    require(definition['networkMode'] == 'awsvpc'
            and definition['runtimePlatform']['cpuArchitecture'] == 'ARM64'
            and 'FARGATE' in definition['requiresCompatibilities'], 'unexpected_task_topology')
    updated = deepcopy(definition)
    containers = updated['containerDefinitions']
    require(len({c['name'] for c in containers}) == len(containers), 'duplicate_container_names')
    changes = []
    for name, slots in TARGETS[definition['family']].items():
        matches = [c for c in containers if c['name'] == name]
        require(len(matches) == 1, 'missing_target_container')
        container = matches[0]
        require(not any(flag in container.get('command', []) for flag in ['--poll-seconds', '--idle-poll-seconds']), 'cli_cadence_override')
        env = container.get('environment', [])
        require(len({x['name'] for x in env}) == len(env), 'duplicate_environment_names')
        if name == 'plether-keeper':
            require([x['value'] for x in env if x['name'] == 'KEEPER_POLL_SECONDS'] == ['1'], 'active_keeper_interval_drift')
        for key, (previous, desired) in slots.items():
            require(not any(x['name'] == key for x in container.get('secrets', [])), 'cadence_is_secret_bound')
            entries = [x for x in env if x['name'] == key]
            present = bool(entries)
            require(present or key == 'PROTECTION_WORKER_POLL_SECONDS', 'missing_explicit_cadence')
            before = entries[0]['value'] if present else previous
            require(before in [previous, desired], 'unexpected_existing_cadence')
            if present:
                entries[0]['value'] = desired
            else:
                env.append({'name': key, 'value': desired})
            changes.append({'container': name, 'name': key, 'before': before,
                            'before_present': present, 'after': desired})
        container['environment'] = env
    require(fingerprint(definition) == fingerprint(updated), 'non_cadence_configuration_changed')
    return updated, changes


def keeper_config(definition):
    containers = [c for c in definition['containerDefinitions'] if c['name'] == 'plether-keeper']
    require(len(containers) == 1, 'missing_keeper')
    return {k: sorted(containers[0].get(k, []), key=lambda x: x['name']) for k in ['environment', 'secrets']}


def require_desired_cadence(definition):
    for name, slots in TARGETS[definition['family']].items():
        containers = [c for c in definition['containerDefinitions'] if c['name'] == name]
        require(len(containers) == 1, 'missing_target_container')
        for key, (_, desired) in slots.items():
            require([e['value'] for e in containers[0].get('environment', []) if e['name'] == key] == [desired], 'cadence_drift')


def main():
    import boto3
    parser = argparse.ArgumentParser()
    parser.add_argument('operation', choices=['prepare', 'verify', 'rollback'])
    parser.add_argument('--directory', type=Path, required=True)
    args = parser.parse_args()
    os.umask(0o077)
    work = args.directory.resolve()
    work.mkdir(parents=True, exist_ok=True, mode=0o700)
    lock = (work / 'cadence.lock').open('a')
    fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    path = work / 'cadence-rollout.json'
    session = boto3.Session(profile_name='plether', region_name=REGION)
    identity = session.client('sts').get_caller_identity()
    require(identity['Account'] == ACCOUNT and not identity['Arn'].endswith(':root'), 'wrong_aws_identity')
    ecs = session.client('ecs')
    def get(arn):
        response = ecs.describe_task_definition(taskDefinition=arn, include=['TAGS'])
        return {**response['taskDefinition'], 'tags': response.get('tags', [])}
    def register(definition):
        allowed = set(ecs.meta.service_model.operation_model('RegisterTaskDefinition').input_shape.members)
        return ecs.register_task_definition(**{k: v for k, v in definition.items() if k in allowed})['taskDefinition']['taskDefinitionArn']
    def save(journal):
        temporary = path.with_suffix('.next.json')
        temporary.write_text(json.dumps(journal, indent=2) + '\n')
        temporary.replace(path)
    def services():
        response = ecs.describe_services(cluster=CLUSTER, services=list(SERVICES))
        require(not response['failures'] and len(response['services']) == len(SERVICES), 'missing_services')
        return {v['serviceName']: v for v in response['services']}
    if args.operation == 'prepare':
        live = services()
        if path.exists():
            journal = json.loads(path.read_text())
            require(journal['status'] in ['preparing', 'prepared'], 'unexpected_journal_status')
        else:
            journal = {'status': 'preparing', 'account': ACCOUNT, 'region': REGION,
                       'prepared_at_utc': datetime.now(timezone.utc).isoformat(), 'services': {}}
            for name, (family, count) in SERVICES.items():
                v = live[name]
                require(v['desiredCount'] == v['runningCount'] == count and v['pendingCount'] == 0, 'unexpected_service_count')
                require(len(v['deployments']) == 1 and v['deployments'][0]['rolloutState'] == 'COMPLETED', 'service_not_stable')
                before = get(family)
                require(count == 0 or before['taskDefinitionArn'] == v['taskDefinition'], 'unrelated_pending_revision')
                updated, changes = patch_definition(before)
                journal['services'][name] = {'previous_task_definition': before['taskDefinitionArn'],
                    'desired_count': count, 'changes': changes, 'policy_hash': fingerprint(before, True),
                    'preparation_hash': fingerprint(before)}
            require(keeper_config(get('plether-sepolia-keeper')) == keeper_config(get(live['plether-workers']['taskDefinition'])), 'keeper_topology_drift')
            save(journal)
        for name, state in journal['services'].items():
            family = SERVICES[name][0]
            latest = get(family)
            if state.get('prepared_task_definition'):
                require(latest['taskDefinitionArn'] == state['prepared_task_definition'], 'prepared_revision_superseded')
                require(fingerprint(latest) == state['preparation_hash'], 'prepared_policy_drift')
                require_desired_cadence(latest)
                continue
            require(latest['taskDefinitionArn'] == state['previous_task_definition'], 'baseline_revision_superseded')
            updated, _ = patch_definition(latest)
            state['prepared_task_definition'] = register(updated)
            save(journal)
            prepared = get(state['prepared_task_definition'])
            require(fingerprint(prepared) == fingerprint(latest), 'registration_changed_non_cadence_fields')
            require_desired_cadence(prepared)
            save(journal)
        require(keeper_config(get('plether-sepolia-keeper')) == keeper_config(get('plether-sepolia-workers')), 'prepared_keeper_topology_drift')
        journal['status'] = 'prepared'
        save(journal)
        print(json.dumps(journal))
    else:
        journal = json.loads(path.read_text())
        require(journal['status'] in ['prepared', 'verified'], 'cadence_not_prepared')
        live = services()
        for name, state in journal['services'].items():
            family, count = SERVICES[name]
            current = get(family)
            require(fingerprint(current, True) == state['policy_hash'], 'confirmation_gas_or_other_policy_drift')
            require_desired_cadence(current)
            if args.operation == 'rollback':
                state['rollback_task_definition'] = register(get(state['previous_task_definition']))
            else:
                v = live[name]
                require(v['desiredCount'] == v['runningCount'] == count and v['pendingCount'] == 0, 'service_count_drift')
                require(len(v['deployments']) == 1 and v['deployments'][0]['rolloutState'] == 'COMPLETED', 'deployment_incomplete')
                require(count == 0 or v['taskDefinition'] == current['taskDefinitionArn'], 'prepared_family_not_running')
                state['verified_task_definition'] = current['taskDefinitionArn']
        journal['status'] = 'rollback_prepared' if args.operation == 'rollback' else 'verified'
        journal['checked_at_utc'] = datetime.now(timezone.utc).isoformat()
        save(journal)
        print(json.dumps({'status': journal['status'], 'confirmation_gas_and_other_policies_preserved': True}))


if __name__ == '__main__':
    try:
        main()
    except Exception as error:
        print(json.dumps({'status': 'stopped', 'exception_type': type(error).__name__,
                          'reason': str(error) if isinstance(error, GuardRejected) else 'inspect bounded status; sensitive details suppressed'}))
        raise SystemExit(1)
