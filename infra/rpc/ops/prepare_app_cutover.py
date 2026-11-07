#!/usr/bin/env python3
"""Prepare exact Sepolia RPC credentials/task revisions without starting tasks.

The deployment workflows remain responsible for service updates. The journal
contains identifiers only; RPC credentials are handled in memory via SSM.
"""
import argparse
from copy import deepcopy
from datetime import datetime, timezone
import json
import os
from pathlib import Path
from urllib.parse import urlsplit

ACCOUNT = '932542905614'
REGION = 'ap-southeast-1'
CLUSTER = 'plether-sepolia'
PRIMARY = 'https://rpc-internal.sepolia.plether.com/'
PARAMETER_PREFIX = f'arn:aws:ssm:{REGION}:{ACCOUNT}:parameter'
RPC_PARAMETER = '/plether/sepolia/perps-rpc-url'
APP_PARAMETER = '/plether/sepolia/app-rpc-url'
TOKEN_PARAMETER = '/plether/sepolia/rpc-gateway-token'
BACKUP_PARAMETER = '/plether/sepolia/rpc-node/backup-url'
SERVICES = {
    'plether-api': 'plether-sepolia',
    'plether-workers': 'plether-sepolia-workers',
    'plether-liquidation-worker': 'plether-sepolia-liquidation-worker',
    'plether-position-protection-worker': 'plether-sepolia-protection-worker',
    'plether-aa-reconciler': 'plether-sepolia-aa-reconciler',
}
AUXILIARY_FAMILY = 'plether-sepolia-keeper'


class GuardRejected(ValueError):
    pass


def require(condition, reason):
    if not condition:
        raise GuardRejected(reason)


def patch_definition(definition):
    patched = deepcopy(definition)
    changes = []
    require(definition['family'] in [*SERVICES.values(), AUXILIARY_FAMILY], 'unexpected_family')
    require(definition['networkMode'] == 'awsvpc', 'unexpected_network_mode')
    require(definition['runtimePlatform']['cpuArchitecture'] == 'ARM64', 'unexpected_architecture')
    require('FARGATE' in definition['requiresCompatibilities'], 'unexpected_launch_type')
    for container in patched['containerDefinitions']:
        secrets = container.get('secrets', [])
        require(len({s['name'] for s in secrets}) == len(secrets), 'duplicate_secret_names')
        refs = {s['name']: s['valueFrom'] for s in secrets}
        additions = []
        for rpc_name, auth_name in [('PERPS_RPC_URL', 'PERPS_RPC_AUTH_TOKEN'), ('ARBITRUM_SEPOLIA_RPC_URL', 'PERPS_RPC_AUTH_TOKEN'), ('RPC_URL', 'RPC_AUTH_TOKEN')]:
            if refs.get(rpc_name) in [PARAMETER_PREFIX + RPC_PARAMETER, PARAMETER_PREFIX + APP_PARAMETER]:
                for secret in secrets:
                    if secret['name'] == rpc_name:
                        secret['valueFrom'] = PARAMETER_PREFIX + APP_PARAMETER
                changes.append({'container': container['name'], 'secret': rpc_name, 'parameter': APP_PARAMETER})
                expected = PARAMETER_PREFIX + TOKEN_PARAMETER
                require(auth_name not in refs or refs[auth_name] == expected, 'conflicting_rpc_token')
                if auth_name not in refs:
                    additions.append({'name': auth_name, 'valueFrom': expected})
                    refs[auth_name] = expected
                    changes.append({'container': container['name'], 'secret': auth_name, 'parameter': TOKEN_PARAMETER})
        modes = [x['value'] for x in container.get('environment', []) if x['name'] == 'AA_RPC_MODE']
        if modes == ['single-provider-sepolia'] and refs.get('AA_RECONCILER_SECONDARY_RPC_URL') == PARAMETER_PREFIX + RPC_PARAMETER:
            for secret in secrets:
                if secret['name'] == 'AA_RECONCILER_SECONDARY_RPC_URL':
                    secret['valueFrom'] = PARAMETER_PREFIX + APP_PARAMETER
            changes.append({'container': container['name'], 'secret': 'AA_RECONCILER_SECONDARY_RPC_URL', 'parameter': APP_PARAMETER})
        if additions:
            container['secrets'] = secrets + additions
    require(bool(changes), 'no_matching_consumer')
    return patched, changes


def patch_dormant_keeper(definition, workers):
    require(definition['family'] == AUXILIARY_FAMILY, 'unexpected_auxiliary_family')
    require(workers['family'] == SERVICES['plether-workers'], 'unexpected_workers_family')
    patched, changes = patch_definition(definition)
    def keeper_config(task):
        containers = [c for c in task['containerDefinitions'] if c['name'] == 'plether-keeper']
        require(len(containers) == 1, 'unexpected_keeper_container')
        return {field: sorted(containers[0].get(field, []), key=lambda item: item['name'])
                for field in ['environment', 'secrets']}
    require(keeper_config(patched) == keeper_config(workers), 'keeper_topology_differs_beyond_rpc')
    return patched, changes


def main():
    import boto3
    parser = argparse.ArgumentParser()
    parser.add_argument('operation', choices=['prepare', 'activate', 'rollback'])
    parser.add_argument('--directory', type=Path, required=True)
    args = parser.parse_args()
    os.umask(0o077)
    work = args.directory.resolve()
    work.mkdir(parents=True, exist_ok=True, mode=0o700)
    journal_path = work / 'application-cutover.json'
    session = boto3.Session(profile_name='plether', region_name=REGION)
    identity = session.client('sts').get_caller_identity()
    require(identity['Account'] == ACCOUNT and not identity['Arn'].endswith(':root'), 'wrong_aws_identity')
    ssm, ecs, iam = (session.client(name) for name in ['ssm', 'ecs', 'iam'])
    def get(name):
        return ssm.get_parameter(Name=name, WithDecryption=True)['Parameter']['Value']
    backup = get(BACKUP_PARAMETER)
    require(urlsplit(backup).scheme == 'https' and urlsplit(backup).hostname == 'arb-sepolia.g.alchemy.com', 'unexpected_backup_origin')
    def write_journal(journal):
        temporary = journal_path.with_suffix('.next.json')
        temporary.write_text(json.dumps(journal, indent=2) + '\n')
        temporary.replace(journal_path)
    def put_unchanged_or_new(name, value):
        try:
            current = get(name)
        except ssm.exceptions.ParameterNotFound:
            ssm.put_parameter(Name=name, Value=value, Type='SecureString', Tier='Standard', Overwrite=False)
        else:
            require(current == value, 'existing_parameter_value_differs')
    def grant(role, name, parameter):
        policy = iam.get_role_policy(RoleName=role, PolicyName=name)['PolicyDocument']
        for statement in policy['Statement']:
            actions = statement.get('Action', [])
            if statement.get('Effect') == 'Allow' and isinstance(actions, list) and 'ssm:GetParameters' in actions:
                resources = statement['Resource']
                require(isinstance(resources, list), 'unexpected_policy_shape')
                arn = PARAMETER_PREFIX + parameter
                if arn not in resources:
                    statement['Resource'] = resources + [arn]
                    iam.put_role_policy(RoleName=role, PolicyName=name, PolicyDocument=json.dumps(policy))
                return
        raise GuardRejected('missing_exact_ssm_policy')
    def register(definition):
        require(definition['family'] in [*SERVICES.values(), AUXILIARY_FAMILY], 'unexpected_registration_family')
        allowed = set(ecs.meta.service_model.operation_model('RegisterTaskDefinition').input_shape.members)
        request = {k: v for k, v in definition.items() if k in allowed}
        return ecs.register_task_definition(**request)['taskDefinition']['taskDefinitionArn']

    if args.operation == 'prepare':
        require(get(RPC_PARAMETER) == backup, 'ordinary_origin_already_changed')
        if journal_path.exists():
            journal = json.loads(journal_path.read_text())
            require(journal['status'] in ['preparing', 'prepared'], 'unexpected_existing_journal')
            if journal.get('schema_version', 1) == 1:
                journal['unused_earlier_alto_preparation'] = journal['services'].pop('plether-alto', None)
                for state in journal['services'].values():
                    state['superseded_preparation'] = state.pop('prepared_task_definition', None)
                journal['schema_version'] = 2
                journal['status'] = 'preparing'
                write_journal(journal)
        else:
            descriptions = ecs.describe_services(cluster=CLUSTER, services=list(SERVICES))
            require(not descriptions['failures'] and len(descriptions['services']) == len(SERVICES), 'missing_services')
            journal = {'schema_version': 2, 'status': 'preparing', 'account': ACCOUNT, 'region': REGION,
                       'prepared_at_utc': datetime.now(timezone.utc).isoformat(), 'services': {}}
            for service in descriptions['services']:
                require(service['desiredCount'] == service['runningCount'] and service['pendingCount'] == 0, 'service_not_stable')
                require(len(service['deployments']) == 1 and service['deployments'][0]['rolloutState'] == 'COMPLETED', 'service_rollout_in_progress')
                if service['serviceName'] != 'plether-alto':
                    expected_group = 'sg-0c095c11c27499ca7' if service['serviceName'] == 'plether-aa-reconciler' else 'sg-0ef6f711cab9291f8'
                    require(expected_group in service['networkConfiguration']['awsvpcConfiguration']['securityGroups'], 'consumer_network_group_changed')
                journal['services'][service['serviceName']] = {'previous_task_definition': service['taskDefinition']}
            write_journal(journal)
        token = get('/plether/sepolia/rpc-node/gateway-token')
        require(len(token) >= 32, 'invalid_gateway_token')
        put_unchanged_or_new(TOKEN_PARAMETER, token)
        put_unchanged_or_new(APP_PARAMETER, PRIMARY)
        grant('plether-sepolia-api-execution', 'exact-ssm-secret-access', TOKEN_PARAMETER)
        grant('plether-sepolia-aa-reconciler-execution', 'exact-ssm-secret-access', TOKEN_PARAMETER)
        grant('plether-sepolia-api-execution', 'exact-ssm-secret-access', APP_PARAMETER)
        grant('plether-sepolia-aa-reconciler-execution', 'exact-ssm-secret-access', APP_PARAMETER)
        for service, state in journal['services'].items():
            if state.get('prepared_task_definition'):
                continue
            definition = ecs.describe_task_definition(taskDefinition=state['previous_task_definition'])['taskDefinition']
            require(definition['family'] == SERVICES[service], 'unexpected_service_family')
            latest = ecs.describe_task_definition(taskDefinition=SERVICES[service])['taskDefinition']
            require(latest['taskDefinitionArn'] in [definition['taskDefinitionArn'], state.get('superseded_preparation')], 'unrelated_pending_task_revision')
            patched, changes = patch_definition(definition)
            state['prepared_task_definition'] = register(patched)
            state['changes'] = changes
            write_journal(journal)
        # The deployment preflight compares this inactive legacy family with
        # the consolidated keeper. Prepare its references without starting it.
        auxiliary = journal.setdefault('auxiliary_task_definitions', {})
        state = auxiliary.get(AUXILIARY_FAMILY)
        if not state or not state.get('prepared_task_definition'):
            services = ecs.describe_services(cluster=CLUSTER, services=['plether-keeper'])
            require(not services['failures'] and len(services['services']) == 1, 'missing_dormant_keeper')
            service = services['services'][0]
            require(service['desiredCount'] == service['runningCount'] == service['pendingCount'] == 0,
                    'standalone_keeper_not_dormant')
            latest = ecs.describe_task_definition(taskDefinition=AUXILIARY_FAMILY)['taskDefinition']
            if not state:
                state = auxiliary[AUXILIARY_FAMILY] = {'previous_task_definition': latest['taskDefinitionArn']}
                write_journal(journal)
            require(latest['taskDefinitionArn'] == state['previous_task_definition'], 'auxiliary_revision_superseded')
            workers = ecs.describe_task_definition(taskDefinition=journal['services']['plether-workers']['prepared_task_definition'])['taskDefinition']
            patched, changes = patch_dormant_keeper(latest, workers)
            state['prepared_task_definition'] = register(patched)
            state['changes'] = changes
            write_journal(journal)
        journal['status'] = 'prepared'
        write_journal(journal)
        print(json.dumps(journal))
    elif args.operation == 'activate':
        journal = json.loads(journal_path.read_text())
        require(journal['status'] == 'prepared', 'configuration_not_prepared')
        proof = json.loads((work / 'app-routing-qualification.json').read_text())
        require(proof['success'] and any(r.get('test') == 'required_checks_complete' and r.get('pass') for r in proof['results']), 'routing_qualification_failed')
        require(all(r.get('pass') for r in proof['results']), 'routing_probe_failed')
        age = (datetime.now(timezone.utc) - datetime.fromisoformat(proof['completed_at_utc'])).total_seconds()
        require(0 <= age <= 900, 'routing_qualification_stale')
        reconciler_proof = json.loads((work / 'reconciler-routing-qualification.json').read_text())
        require(reconciler_proof['success'] and reconciler_proof['security_group'] == 'sg-0c095c11c27499ca7', 'reconciler_network_qualification_failed')
        require(0 <= (datetime.now(timezone.utc) - datetime.fromisoformat(reconciler_proof['completed_at_utc'])).total_seconds() <= 900, 'reconciler_qualification_stale')
        require(get(RPC_PARAMETER) == backup, 'preserved_alchemy_origin_changed')
        require(get(APP_PARAMETER) == PRIMARY, 'application_origin_changed')
        for service, state in journal['services'].items():
            latest = ecs.describe_task_definition(taskDefinition=SERVICES[service])['taskDefinition']
            require(latest['taskDefinitionArn'] == state['prepared_task_definition'], 'prepared_definition_superseded')
        auxiliary = journal.get('auxiliary_task_definitions', {}).get(AUXILIARY_FAMILY)
        require(auxiliary and auxiliary.get('prepared_task_definition'), 'dormant_keeper_not_prepared')
        latest = ecs.describe_task_definition(taskDefinition=AUXILIARY_FAMILY)['taskDefinition']
        require(latest['taskDefinitionArn'] == auxiliary['prepared_task_definition'], 'auxiliary_revision_superseded')
        require(get(RPC_PARAMETER) == backup, 'ordinary_origin_changed_after_preparation')
        require(get(TOKEN_PARAMETER) == get('/plether/sepolia/rpc-node/gateway-token'), 'gateway_token_changed')
        journal.update(status='activated_pending_backend_deployment', activated_at_utc=datetime.now(timezone.utc).isoformat())
        write_journal(journal)
        print(json.dumps({'status': journal['status'], 'next': 'Deploy backend from reviewed master with bootstrap=false, then deploy frontend.'}))
    else:
        journal = json.loads(journal_path.read_text())
        require(journal['status'] in ['activated_pending_backend_deployment', 'cutover_verified'], 'rollback_not_applicable')
        require(get(RPC_PARAMETER) == backup, 'preserved_alchemy_origin_changed')
        for service, state in journal['services'].items():
            if service != 'plether-alto':
                old = ecs.describe_task_definition(taskDefinition=state['previous_task_definition'])['taskDefinition']
                state['rollback_task_definition'] = register(old)
        for state in journal.get('auxiliary_task_definitions', {}).values():
            old = ecs.describe_task_definition(taskDefinition=state['previous_task_definition'])['taskDefinition']
            require(old['family'] == AUXILIARY_FAMILY, 'unexpected_auxiliary_rollback_family')
            state['rollback_task_definition'] = register(old)
        journal['status'] = 'rollback_pending_backend_deployment'
        write_journal(journal)
        print(json.dumps({'status': journal['status'], 'next': 'Deploy backend from reviewed master with bootstrap=false.'}))


if __name__ == '__main__':
    try:
        main()
    except Exception as error:
        print(json.dumps({'status': 'stopped', 'exception_type': type(error).__name__,
                          'reason': str(error) if isinstance(error, GuardRejected) else 'inspect bounded AWS status; secret details suppressed'}))
        raise SystemExit(1)
