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
TOKEN_PARAMETER = '/plether/sepolia/rpc-gateway-token'
ALTO_PARAMETER = '/plether/sepolia/alto-rpc-url'
BACKUP_PARAMETER = '/plether/sepolia/rpc-node/backup-url'
SERVICES = {
    'plether-api': 'plether-sepolia',
    'plether-workers': 'plether-sepolia-workers',
    'plether-liquidation-worker': 'plether-sepolia-liquidation-worker',
    'plether-position-protection-worker': 'plether-sepolia-protection-worker',
    'plether-aa-reconciler': 'plether-sepolia-aa-reconciler',
    'plether-alto': 'plether-sepolia-alto',
}


class GuardRejected(ValueError):
    pass


def require(condition, reason):
    if not condition:
        raise GuardRejected(reason)


def patch_definition(definition, alto=False):
    patched = deepcopy(definition)
    changes = []
    require(definition['family'] in SERVICES.values(), 'unexpected_family')
    require(definition['networkMode'] == 'awsvpc', 'unexpected_network_mode')
    require(definition['runtimePlatform']['cpuArchitecture'] == 'ARM64', 'unexpected_architecture')
    require('FARGATE' in definition['requiresCompatibilities'], 'unexpected_launch_type')
    for container in patched['containerDefinitions']:
        secrets = container.get('secrets', [])
        require(len({s['name'] for s in secrets}) == len(secrets), 'duplicate_secret_names')
        refs = {s['name']: s['valueFrom'] for s in secrets}
        additions = []
        if alto:
            for secret in secrets:
                if secret['name'] == 'ALTO_RPC_URL':
                    require(secret['valueFrom'] in [PARAMETER_PREFIX + RPC_PARAMETER, PARAMETER_PREFIX + ALTO_PARAMETER], 'unexpected_alto_origin')
                    secret['valueFrom'] = PARAMETER_PREFIX + ALTO_PARAMETER
                    changes.append({'container': container['name'], 'secret': 'ALTO_RPC_URL', 'parameter': ALTO_PARAMETER})
        else:
            for rpc_name, auth_name in [('PERPS_RPC_URL', 'PERPS_RPC_AUTH_TOKEN'), ('ARBITRUM_SEPOLIA_RPC_URL', 'PERPS_RPC_AUTH_TOKEN'), ('RPC_URL', 'RPC_AUTH_TOKEN')]:
                if refs.get(rpc_name) == PARAMETER_PREFIX + RPC_PARAMETER:
                    expected = PARAMETER_PREFIX + TOKEN_PARAMETER
                    require(auth_name not in refs or refs[auth_name] == expected, 'conflicting_rpc_token')
                    if auth_name not in refs:
                        additions.append({'name': auth_name, 'valueFrom': expected})
                        refs[auth_name] = expected
                        changes.append({'container': container['name'], 'secret': auth_name, 'parameter': TOKEN_PARAMETER})
            if additions:
                container['secrets'] = secrets + additions
    require(bool(changes), 'no_matching_consumer')
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
        require(definition['family'] in SERVICES.values(), 'unexpected_registration_family')
        allowed = set(ecs.meta.service_model.operation_model('RegisterTaskDefinition').input_shape.members)
        request = {k: v for k, v in definition.items() if k in allowed}
        return ecs.register_task_definition(**request)['taskDefinition']['taskDefinitionArn']

    if args.operation == 'prepare':
        require(get(RPC_PARAMETER) == backup, 'ordinary_origin_already_changed')
        if journal_path.exists():
            journal = json.loads(journal_path.read_text())
            require(journal['status'] in ['preparing', 'prepared'], 'unexpected_existing_journal')
        else:
            descriptions = ecs.describe_services(cluster=CLUSTER, services=list(SERVICES))
            require(not descriptions['failures'] and len(descriptions['services']) == len(SERVICES), 'missing_services')
            journal = {'status': 'preparing', 'account': ACCOUNT, 'region': REGION,
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
        put_unchanged_or_new(ALTO_PARAMETER, backup)
        grant('plether-sepolia-api-execution', 'exact-ssm-secret-access', TOKEN_PARAMETER)
        grant('plether-sepolia-aa-reconciler-execution', 'exact-ssm-secret-access', TOKEN_PARAMETER)
        grant('plether-sepolia-alto-execution', 'exact-ssm-secret-access', ALTO_PARAMETER)
        for service, state in journal['services'].items():
            if state.get('prepared_task_definition'):
                continue
            definition = ecs.describe_task_definition(taskDefinition=state['previous_task_definition'])['taskDefinition']
            require(definition['family'] == SERVICES[service], 'unexpected_service_family')
            latest = ecs.describe_task_definition(taskDefinition=SERVICES[service])['taskDefinition']
            require(latest['taskDefinitionArn'] == definition['taskDefinitionArn'], 'unrelated_pending_task_revision')
            patched, changes = patch_definition(definition, alto=service == 'plether-alto')
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
        alto = ecs.describe_services(cluster=CLUSTER, services=['plether-alto'])['services'][0]
        require(alto['desiredCount'] == alto['runningCount'] == 1 and alto['pendingCount'] == 0 and len(alto['deployments']) == 1, 'alto_not_stable')
        definition = ecs.describe_task_definition(taskDefinition=alto['taskDefinition'])['taskDefinition']
        refs = [x['valueFrom'] for c in definition['containerDefinitions'] for x in c.get('secrets', []) if x['name'] == 'ALTO_RPC_URL']
        require(refs == [PARAMETER_PREFIX + ALTO_PARAMETER], 'alto_still_follows_ordinary_alias')
        require(get(ALTO_PARAMETER) == backup, 'alto_origin_changed')
        for service, state in journal['services'].items():
            if service == 'plether-alto':
                continue
            latest = ecs.describe_task_definition(taskDefinition=SERVICES[service])['taskDefinition']
            require(latest['taskDefinitionArn'] == state['prepared_task_definition'], 'prepared_definition_superseded')
        require(get(RPC_PARAMETER) == backup, 'ordinary_origin_changed_after_preparation')
        require(get(TOKEN_PARAMETER) == get('/plether/sepolia/rpc-node/gateway-token'), 'gateway_token_changed')
        ssm.put_parameter(Name=RPC_PARAMETER, Value=PRIMARY, Type='SecureString', Overwrite=True)
        journal.update(status='activated_pending_backend_deployment', activated_at_utc=datetime.now(timezone.utc).isoformat())
        write_journal(journal)
        print(json.dumps({'status': journal['status'], 'next': 'Deploy backend from reviewed master with bootstrap=false, then deploy frontend.'}))
    else:
        journal = json.loads(journal_path.read_text())
        require(journal['status'] in ['activated_pending_backend_deployment', 'cutover_verified'], 'rollback_not_applicable')
        require(get(RPC_PARAMETER) == PRIMARY, 'ordinary_origin_changed_after_activation')
        ssm.put_parameter(Name=RPC_PARAMETER, Value=backup, Type='SecureString', Overwrite=True)
        for service, state in journal['services'].items():
            if service != 'plether-alto':
                old = ecs.describe_task_definition(taskDefinition=state['previous_task_definition'])['taskDefinition']
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
