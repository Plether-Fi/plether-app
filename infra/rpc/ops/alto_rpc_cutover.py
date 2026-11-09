#!/usr/bin/env python3
"""Guarded Sepolia Alto RPC migration, invoked by the protected GitHub workflow.

Only the Alto task definition/service are mutated. Signer values are never read;
the canary receives only RPC credentials. Every live replacement stops the old
signer task and proves it stopped before starting another, including rollback.
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
import signal
import time
from urllib.parse import urlsplit
from urllib.request import Request, urlopen

ACCOUNT = '932542905614'
REGION = 'ap-southeast-1'
CLUSTER = 'plether-sepolia'
SERVICE = 'plether-alto'
FAMILY = 'plether-sepolia-alto'
ALTO = 'plether-alto'
INIT = 'alto-tmp-init'
ROUTER = 'otel-log-router'
PREFIX = f'arn:aws:ssm:{REGION}:{ACCOUNT}:parameter/plether/sepolia/'
REPOSITORY = 'plether-alto-sepolia'
REGISTRY = f'{ACCOUNT}.dkr.ecr.{REGION}.amazonaws.com'
BASE_DIGEST = 'sha256:9db94fbd439a26f01b0ece3cc5f76b3791c1e1660b990d74892274267096f12a'
BASE_ENTRYPOINT = ['node', '/app/src/esm/cli/alto.js']
PRELOAD_ENTRYPOINT = ['node', '--import', '/app/plether-rpc-auth.mjs', '/app/src/esm/cli/alto.js']
AUTH_KEY = 'PLETHER_ALTO_RPC_AUTH_TOKEN'
TASK_PREFIX = f'arn:aws:ecs:{REGION}:{ACCOUNT}:task-definition/'
CANARY_FAMILY = FAMILY + '-rpc-canary'
OUTPUT_FIELDS = {'taskDefinitionArn', 'revision', 'status', 'requiresAttributes',
                 'compatibilities', 'registeredAt', 'registeredBy', 'deregisteredAt'}
CANARY_EVENT = 'alto_rpc_qualification'
PUBLIC = 'https://app.sepolia.plether.com/api/perps/v1'


class GuardRejected(ValueError):
    pass


def require(condition, reason):
    if not condition:
        raise GuardRejected(reason)


def named(items):
    require(isinstance(items, list) and all(isinstance(x, dict) and isinstance(x.get('name'), str) for x in items),
            'malformed_named_entries')
    require(len({x['name'] for x in items}) == len(items), 'duplicate_named_entries')
    return {x['name']: x for x in items}


def container(definition, name=ALTO):
    values = named(definition.get('containerDefinitions', []))
    require(name in values, 'missing_container')
    return values[name]


def canonical(definition):
    value = deepcopy({k: v for k, v in definition.items() if k not in OUTPUT_FIELDS})
    for c in value.get('containerDefinitions', []):
        for key in ['environment', 'secrets']:
            if key in c:
                c[key] = sorted(c[key], key=lambda x: x['name'])
    value['containerDefinitions'].sort(key=lambda x: x['name'])
    if 'tags' in value:
        value['tags'] = sorted(value['tags'], key=lambda x: x['key'])
    return value


def fingerprint(definition):
    return hashlib.sha256(json.dumps(canonical(definition), sort_keys=True).encode()).hexdigest()


def validate_image(image):
    require(isinstance(image, str) and re.fullmatch(re.escape(REGISTRY + '/' + REPOSITORY + '@sha256:') + r'[0-9a-f]{64}', image),
            'unexpected_candidate_image')


def require_baseline(definition):
    require(definition.get('family') == FAMILY and definition.get('status', 'ACTIVE') == 'ACTIVE', 'unexpected_family_or_status')
    require(re.fullmatch(re.escape(TASK_PREFIX + FAMILY + ':') + r'[1-9][0-9]*',
                         definition.get('taskDefinitionArn', '')) is not None, 'unexpected_baseline_arn')
    require(definition.get('networkMode') == 'awsvpc'
            and definition.get('runtimePlatform', {}).get('cpuArchitecture') == 'ARM64'
            and definition.get('runtimePlatform', {}).get('operatingSystemFamily', 'LINUX') == 'LINUX'
            and definition.get('requiresCompatibilities') == ['FARGATE'], 'unexpected_task_topology')
    for key, suffix in [('executionRoleArn', 'execution'), ('taskRoleArn', 'task')]:
        require(definition.get(key) == f'arn:aws:iam::{ACCOUNT}:role/{FAMILY}-{suffix}', 'unexpected_task_role')
    values = named(definition.get('containerDefinitions', []))
    require(set(values) == {ALTO, INIT, ROUTER}, 'unexpected_containers')
    alto = values[ALTO]
    require(alto.get('entryPoint') == BASE_ENTRYPOINT and alto.get('command') == ['run'], 'unexpected_alto_command')
    require(alto.get('essential') is True and alto.get('readonlyRootFilesystem') is True, 'unexpected_alto_container_policy')
    require(values[INIT].get('essential') is False, 'unexpected_init_policy')
    legacy_image = REGISTRY + '/' + REPOSITORY + '@' + BASE_DIGEST
    require(alto.get('image') == values[INIT].get('image') == legacy_image, 'unexpected_baseline_image')
    require(re.fullmatch(re.escape(REGISTRY + '/plether-otel-log-router-sepolia@sha256:') + r'[0-9a-f]{64}',
                         values[ROUTER].get('image', '')) is not None, 'unbounded_log_router_image')
    env = {k: x.get('value') for k, x in named(alto.get('environment', [])).items()}
    refs = {k: x.get('valueFrom') for k, x in named(alto.get('secrets', [])).items()}
    require(set(refs) == {'ALTO_RPC_URL', 'ALTO_EXECUTOR_PRIVATE_KEYS', 'ALTO_UTILITY_PRIVATE_KEY'}, 'unexpected_baseline_secrets')
    require(not set(env).intersection(refs) and AUTH_KEY not in env and 'NODE_OPTIONS' not in env,
            'environment_secret_or_preload_conflict')
    require(refs['ALTO_RPC_URL'] == PREFIX + 'perps-rpc-url', 'unexpected_baseline_rpc')
    require(all(isinstance(ref, str) and re.fullmatch(re.escape(PREFIX) + r'[A-Za-z0-9_/-]+', ref) for ref in refs.values()),
            'unexpected_secret_scope')
    require(env.get('ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION') == 'false'
            and env.get('ALTO_ENABLE_DEBUG_ENDPOINTS') == 'false'
            and env.get('ALTO_FLUSH_STUCK_TRANSACTIONS_DURING_STARTUP') == 'false', 'unsafe_validation_or_startup_policy')
    safe = env.get('ALTO_SAFE_MODE')
    require(safe == 'true' or (safe == 'false' and env.get('PLETHER_ALTO_NETWORK_CHAIN_ID') == '421614'
                             and env.get('PLETHER_ALTO_VALIDATION_POLICY') == 'sepolia-testnet-exception-v1'),
            'unexpected_validation_policy')
    require(all(re.fullmatch(r'[1-9][0-9]*', str(env.get(key, ''))) for key in
                ['ALTO_MAX_GAS_PER_USER_OP', 'ALTO_MAX_GAS_PER_BUNDLE']), 'invalid_gas_limits')


def patch_definition(baseline, image):
    require_baseline(baseline)
    validate_image(image)
    updated = deepcopy(baseline)
    alto = container(updated)
    for name in [ALTO, INIT]:
        container(updated, name)['image'] = image
    alto['entryPoint'] = PRELOAD_ENTRYPOINT[:]
    next(x for x in alto['secrets'] if x['name'] == 'ALTO_RPC_URL')['valueFrom'] = PREFIX + 'app-rpc-url'
    alto['secrets'].append({'name': AUTH_KEY, 'valueFrom': PREFIX + 'rpc-gateway-token'})
    return updated


def assert_only_rpc_patch(baseline, candidate, image):
    expected = patch_definition(baseline, image)
    require(canonical(candidate) == canonical(expected), 'non_rpc_configuration_changed')


def validate_baseline_url(url):
    require(isinstance(url, str), 'invalid_baseline_url')
    parsed = urlsplit(url)
    require(parsed.scheme == 'http' and parsed.username is None and parsed.password is None
            and parsed.port in (None, 80) and parsed.path in ('', '/') and not parsed.query and not parsed.fragment
            and re.fullmatch(r'internal-plether-sepolia-alto-[A-Za-z0-9-]+\.ap-southeast-1\.elb\.amazonaws\.com', parsed.hostname or ''),
            'invalid_baseline_url')


def canary_definition(candidate, source, script, baseline_url):
    validate_baseline_url(baseline_url)
    require(re.fullmatch(r'[0-9a-f]{40}', source) is not None, 'invalid_source')
    require(isinstance(script, str) and 0 < len(script.encode()) <= 100_000, 'invalid_canary_source')
    before = container(candidate)
    refs = named(before['secrets'])
    require(refs.get('ALTO_RPC_URL', {}).get('valueFrom') == PREFIX + 'app-rpc-url'
            and refs.get(AUTH_KEY, {}).get('valueFrom') == PREFIX + 'rpc-gateway-token', 'invalid_candidate_rpc')
    c = deepcopy(before)
    c['entryPoint'] = ['node', '--import', '/app/plether-rpc-auth.mjs', '--input-type=module', '-e']
    c['command'] = [script]
    c['secrets'] = [refs['ALTO_RPC_URL'], refs[AUTH_KEY]]
    c['secrets'].append({'name': 'PLETHER_ALTO_BACKUP_RPC_URL', 'valueFrom': PREFIX + 'perps-rpc-url'})
    allowed = {'ALTO_ENTRYPOINTS', 'ALTO_ENTRYPOINT_SIMULATION_CONTRACT_V8',
               'ALTO_PIMLICO_SIMULATION_CONTRACT', 'ALTO_CHAIN_TYPE', 'PLETHER_ALTO_NETWORK_CHAIN_ID'}
    c['environment'] = [x for x in before.get('environment', []) if x['name'] in allowed]
    c['environment'].append({'name': 'PLETHER_ALTO_BASELINE_URL', 'value': baseline_url.rstrip('/')})
    c['environment'].append({'name': 'PLETHER_ALTO_QUALIFY_MODE', 'value': 'rpc'})
    for key in ['dependsOn', 'healthCheck', 'mountPoints', 'portMappings', 'volumesFrom', 'firelensConfiguration']:
        c.pop(key, None)
    c['logConfiguration'] = {'logDriver': 'awslogs', 'options': {
        'awslogs-group': '/ecs/plether-sepolia', 'awslogs-region': REGION, 'awslogs-stream-prefix': 'alto-rpc-canary'}}
    result = deepcopy(candidate)
    result['family'] = CANARY_FAMILY
    result['containerDefinitions'] = [c]
    result['volumes'] = []
    result.pop('taskRoleArn', None)
    result['tags'] = [{'key': 'Capability', 'value': 'alto-rpc-qualification'}, {'key': 'Source', 'value': source}]
    return result


def idle_canary_definition(candidate, source, script, baseline_url):
    result = canary_definition(candidate, source, script, baseline_url)
    c = container(result)
    c['entryPoint'] = ['node', '--input-type=module', '-e']
    c['secrets'] = []
    c['environment'] = [{'name': 'PLETHER_ALTO_BASELINE_URL', 'value': baseline_url.rstrip('/')},
                        {'name': 'PLETHER_ALTO_QUALIFY_MODE', 'value': 'idle'}]
    return result


def validate_service(service, stable=True):
    require(service.get('serviceName') == SERVICE
            and service.get('clusterArn') == f'arn:aws:ecs:{REGION}:{ACCOUNT}:cluster/{CLUSTER}', 'unexpected_service')
    require(service.get('launchType') == 'FARGATE' and service.get('platformVersion') == '1.4.0', 'unexpected_service_platform')
    require(service.get('deploymentConfiguration', {}).get('minimumHealthyPercent') == 0
            and service.get('deploymentConfiguration', {}).get('maximumPercent') == 100, 'unsafe_signer_rollout_configuration')
    arn = service.get('taskDefinition', '')
    require(re.fullmatch(re.escape(TASK_PREFIX + FAMILY + ':') + r'[1-9][0-9]*', arn) is not None, 'unexpected_service_definition')
    network = service.get('networkConfiguration', {}).get('awsvpcConfiguration', {})
    require(network.get('assignPublicIp') == 'ENABLED' and network.get('subnets')
            and len(network.get('securityGroups', [])) == 1, 'unexpected_service_network')
    if stable:
        require(service.get('desiredCount') == service.get('runningCount') == 1 and service.get('pendingCount') == 0
                and len(service.get('deployments', [])) == 1
                and service['deployments'][0].get('rolloutState') == 'COMPLETED', 'service_not_stable')


def validate_scan(scan, digest, now):
    require(scan.get('registryId') == ACCOUNT and scan.get('repositoryName') == REPOSITORY
            and scan.get('imageId', {}).get('imageDigest') == digest
            and scan.get('imageScanStatus', {}).get('status') == 'COMPLETE'
            and not scan.get('nextToken'), 'incomplete_or_mismatched_scan')
    findings = scan.get('imageScanFindings', {})
    rows, counts = findings.get('findings'), findings.get('findingSeverityCounts')
    require(isinstance(rows, list) and isinstance(counts, dict), 'incomplete_scan_findings')
    completed = findings.get('imageScanCompletedAt')
    require(isinstance(completed, datetime) and completed.tzinfo is not None, 'missing_scan_timestamp')
    epoch = now.timestamp() if isinstance(now, datetime) else float(now)
    require(0 <= epoch - completed.timestamp() <= 7200, 'stale_image_scan')
    source_updated = findings.get('vulnerabilitySourceUpdatedAt')
    require(isinstance(source_updated, datetime) and source_updated.tzinfo is not None
            and 0 <= epoch - source_updated.timestamp() <= 7200, 'stale_vulnerability_source')
    require(all(isinstance(row, dict) and isinstance(row.get('name'), str) and row['name']
                and row.get('severity') in {'INFORMATIONAL', 'LOW', 'MEDIUM', 'HIGH', 'CRITICAL', 'UNDEFINED'}
                for row in rows), 'invalid_finding_severity')
    count = counts.get('CRITICAL', 0)
    require(isinstance(count, int) and not isinstance(count, bool) and count >= 0, 'invalid_scan_count')
    critical = [row for row in rows if row.get('severity') == 'CRITICAL']
    require(len(critical) == count, 'incomplete_critical_findings')
    require(count == 0, 'critical_image_findings')


def replace_service(ecs, target, wait_stopped, wait_healthy, before_stop=lambda: None):
    before_stop()
    ecs.update_service(cluster=CLUSTER, service=SERVICE, desiredCount=0)
    wait_stopped()
    ecs.update_service(cluster=CLUSTER, service=SERVICE, taskDefinition=target, desiredCount=1)
    wait_healthy(target)


def perform_cutover(ecs, before_arn, candidate_arn, run_canary, quiescent,
                    wait_stopped, wait_healthy, postcheck, save, cleanup=lambda: None, before_stop=lambda: None):
    try:
        run_canary()
        require(quiescent() is True, 'mempool_not_proven_idle')
        # Capturing task IDs also checks the remaining forward deadline. A
        # failure here has not stopped a signer and needs only pause cleanup.
        before_stop()
        save({'status': 'stopping', 'mutation_started': True})
        try:
            replace_service(ecs, candidate_arn, wait_stopped, wait_healthy)
            save({'status': 'verifying'})
            postcheck()
            save({'status': 'complete'})
        except BaseException:
            try:
                save({'status': 'rolling_back'})
                replace_service(ecs, before_arn, wait_stopped, wait_healthy, before_stop)
                save({'status': 'rolled_back'})
            except BaseException:
                save({'status': 'rollback_failed'})
                raise GuardRejected('rollback_failed_requires_attention') from None
            raise
    finally:
        cleanup()


def private_save(path, value):
    temporary = path.with_suffix('.next.json')
    descriptor = os.open(temporary, os.O_CREAT | os.O_WRONLY | os.O_TRUNC, 0o600)
    with os.fdopen(descriptor, 'w') as handle:
        json.dump(value, handle, indent=2)
        handle.write('\n')
    temporary.replace(path)
    os.chmod(path, 0o600)


class Deployment:
    def __init__(self, session, args, directory):
        self.ecs = session.client('ecs')
        self.ecr = session.client('ecr')
        self.logs = session.client('logs')
        self.elb = session.client('elbv2')
        self.args, self.directory = args, directory
        self.path = directory / 'rollout.json'
        self.deadline = time.monotonic() + args.deadline_seconds
        self.rollback = False
        self.journal = {'schema_version': 1, 'account': ACCOUNT, 'region': REGION, 'source': args.source,
                        'image': args.image, 'status': 'preflight', 'mutation_started': False}
        self.canary_proof = None
        self.stopping_tasks = set()

    def save(self, delta):
        self.journal.update(delta)
        if delta.get('status') == 'rolling_back':
            self.rollback = True
            was_paused = self.ensure_barrier()
            if (not was_paused or self.journal.get('admission_opened')) and hasattr(self, 'candidate_definition'):
                # Public smoke may have reopened admission. Drain any operations
                # accepted in that interval before replacing the signer again.
                self.canary(self.candidate_definition, self.baseline_service, 'idle')
                require(self.quiescent(), 'rollback_mempool_not_proven_idle')
            self.journal['status'] = 'rolling_back'
        self.journal['updated_at_utc'] = datetime.now(timezone.utc).isoformat()
        private_save(self.path, self.journal)

    def budget(self):
        # Restoring a signer may require another idle observer, a full 120s stop
        # grace and cold ECS/ALB startup. Keep that time unavailable to forward work.
        reserve = 0 if self.rollback else 600
        require(time.monotonic() < self.deadline - reserve, 'operator_deadline')

    def service(self):
        result = self.ecs.describe_services(cluster=CLUSTER, services=[SERVICE])
        require(not result.get('failures') and len(result['services']) == 1, 'missing_service')
        return result['services'][0]

    def definition(self, arn):
        result = self.ecs.describe_task_definition(taskDefinition=arn, include=['TAGS'])
        return {**result['taskDefinition'], 'tags': result.get('tags', [])}

    def register(self, value):
        allowed = set(self.ecs.meta.service_model.operation_model('RegisterTaskDefinition').input_shape.members)
        request = {k: v for k, v in value.items() if k in allowed and not (k == 'tags' and not v)}
        return self.ecs.register_task_definition(**request)['taskDefinition']['taskDefinitionArn']

    def capture_signers(self):
        self.budget()
        # desiredStatus switches to STOPPED before a SIGTERM/stopTimeout process
        # has exited. Capture actual task IDs before scaling down and retain
        # every observed replacement; counts/list filters alone are insufficient.
        seen = set(getattr(self, 'stopping_tasks', set()))
        for status in ['RUNNING', 'STOPPED']:
            token = None
            for _ in range(10):
                args = {'cluster': CLUSTER, 'serviceName': SERVICE, 'desiredStatus': status, 'maxResults': 100}
                if token:
                    args['nextToken'] = token
                result = self.ecs.list_tasks(**args)
                seen.update(result['taskArns'])
                token = result.get('nextToken')
                if not token:
                    break
            require(not token, 'task_history_exceeds_bounded_capture')
        self.stopping_tasks = seen

    def wait_stopped(self):
        while True:
            self.budget()
            self.capture_signers()
            service = self.service()
            running = self.ecs.list_tasks(cluster=CLUSTER, serviceName=SERVICE, desiredStatus='RUNNING')['taskArns']
            pending = self.ecs.list_tasks(cluster=CLUSTER, serviceName=SERVICE, desiredStatus='PENDING')['taskArns']
            stopped = True
            tasks = sorted(self.stopping_tasks)
            for offset in range(0, len(tasks), 100):
                expected = tasks[offset:offset + 100]
                observed = self.ecs.describe_tasks(cluster=CLUSTER, tasks=expected)
                require(not observed.get('failures') and len(observed['tasks']) == len(expected), 'stopping_task_identity_missing')
                stopped = stopped and all(x['lastStatus'] == 'STOPPED' for x in observed['tasks'])
            if service['desiredCount'] == service['runningCount'] == service['pendingCount'] == 0 and not running and not pending and stopped:
                return
            time.sleep(3)

    def wait_healthy(self, arn):
        while True:
            self.budget()
            self.capture_signers()
            service = self.service()
            deployment = service.get('deployments', [])
            if any(x.get('rolloutState') == 'FAILED' for x in deployment):
                raise GuardRejected('ecs_rollout_failed')
            if service['taskDefinition'] == arn and service['desiredCount'] == service['runningCount'] == 1 \
                    and service['pendingCount'] == 0 and len(deployment) == 1 and deployment[0].get('rolloutState') == 'COMPLETED':
                tasks = self.ecs.list_tasks(cluster=CLUSTER, serviceName=SERVICE, desiredStatus='RUNNING')['taskArns']
                require(len(tasks) == 1, 'unexpected_running_signer_count')
                task = self.ecs.describe_tasks(cluster=CLUSTER, tasks=tasks)['tasks'][0]
                require(task['taskDefinitionArn'] == arn, 'task_definition_mismatch')
                if task.get('healthStatus') != 'HEALTHY':
                    time.sleep(3)
                    continue
                require(all(x.get('lastStatus') == 'RUNNING' or
                            (x['name'] == INIT and x.get('exitCode') == 0) for x in task['containers']),
                        'container_not_running')
                groups = [x['targetGroupArn'] for x in service.get('loadBalancers', []) if x.get('containerName') == ALTO]
                require(len(groups) == 1, 'unexpected_target_groups')
                health = self.elb.describe_target_health(TargetGroupArn=groups[0])['TargetHealthDescriptions']
                if len(health) != 1 or health[0]['TargetHealth']['State'] != 'healthy':
                    time.sleep(3)
                    continue
                return
            time.sleep(5)

    def baseline_url(self, service):
        groups = [x['targetGroupArn'] for x in service.get('loadBalancers', []) if x.get('containerName') == ALTO]
        require(len(groups) == 1, 'unexpected_target_groups')
        group = self.elb.describe_target_groups(TargetGroupArns=groups)['TargetGroups'][0]
        require(group['TargetGroupName'] == 'plether-sepolia-alto' and len(group['LoadBalancerArns']) == 1,
                'unexpected_alto_target_group')
        lb = self.elb.describe_load_balancers(LoadBalancerArns=group['LoadBalancerArns'])['LoadBalancers'][0]
        require(lb['Scheme'] == 'internal', 'alto_load_balancer_not_private')
        result = 'http://' + lb['DNSName']
        validate_baseline_url(result)
        listeners = self.elb.describe_listeners(LoadBalancerArn=lb['LoadBalancerArn'])['Listeners']
        selected = [x for x in listeners if x['Protocol'] == 'HTTP' and x['Port'] == 80]
        require(len(selected) == 1 and len(selected[0]['DefaultActions']) == 1
                and selected[0]['DefaultActions'][0].get('Type') == 'forward'
                and selected[0]['DefaultActions'][0].get('TargetGroupArn') == groups[0], 'unexpected_alto_listener')
        self.listener = selected[0]['ListenerArn']
        return result

    def ensure_barrier(self):
        rules = self.elb.describe_rules(ListenerArn=self.listener)['Rules']
        current = self.journal.get('pause_rule')
        if current and any(x['RuleArn'] == current for x in rules):
            self.require_barrier_owner(current)
            # A lost deletion response may leave control-plane reads stale.
            # Force a fresh dataplane idle proof for either ambiguous intent.
            return self.journal.get('pause_status') == 'active'
        if current:
            # DeleteRule may have succeeded although its response was lost.
            # Never treat the journal alone as proof that admission is paused.
            self.save({'pause_rule': None, 'pause_status': 'removed', 'admission_opened': True})
        for rule in rules:
            if rule.get('IsDefault'):
                continue
            self.require_barrier_owner(rule['RuleArn'])
            require(len(rules) == 2, 'unexpected_existing_listener_rules')
            self.save({'pause_rule': rule['RuleArn'], 'pause_status': 'active'})
            return True
        require(len(rules) == 1 and rules[0].get('IsDefault') is True, 'unexpected_existing_listener_rules')
        self.save({'pause_status': 'creating', 'listener': self.listener})
        owner = f'alto-rpc-{os.environ["GITHUB_RUN_ID"]}-{os.environ["GITHUB_RUN_ATTEMPT"]}'
        result = self.elb.create_rule(ListenerArn=self.listener, Priority=1,
            Conditions=[{'Field': 'http-request-method', 'HttpRequestMethodConfig': {'Values': ['POST']}}],
            Actions=[{'Type': 'fixed-response', 'FixedResponseConfig': {'StatusCode': '503',
                       'ContentType': 'application/json', 'MessageBody': '{"error":"alto_rpc_cutover_in_progress"}'}}],
            Tags=[{'Key': 'Capability', 'Value': 'alto-rpc-admission'}, {'Key': 'WorkflowOwner', 'Value': owner},
                  {'Key': 'Source', 'Value': self.args.source}])
        require(len(result.get('Rules', [])) == 1, 'barrier_registration_failed')
        self.save({'pause_rule': result['Rules'][0]['RuleArn'], 'pause_status': 'active'})
        return False

    def require_barrier_owner(self, arn):
        tags = self.elb.describe_tags(ResourceArns=[arn])['TagDescriptions']
        owner = f'alto-rpc-{os.environ["GITHUB_RUN_ID"]}-{os.environ["GITHUB_RUN_ATTEMPT"]}'
        require(len(tags) == 1 and {x['Key']: x['Value'] for x in tags[0]['Tags']}.get('WorkflowOwner') == owner
                and {x['Key']: x['Value'] for x in tags[0]['Tags']}.get('Source') == self.args.source,
                'barrier_owner_mismatch')

    def remove_barrier(self):
        arn = self.journal.get('pause_rule')
        if not arn:
            return
        rules = self.elb.describe_rules(ListenerArn=self.listener)['Rules']
        if not any(x['RuleArn'] == arn for x in rules):
            self.save({'pause_rule': None, 'pause_status': 'removed', 'admission_opened': True})
            return
        self.require_barrier_owner(arn)
        # This sticky intent survives both lost DeleteRule responses and a
        # later interrupted re-pause. Such recovery must drain admission again.
        self.save({'pause_status': 'removing', 'admission_opened': True})
        self.elb.delete_rule(RuleArn=arn)
        self.save({'pause_rule': None, 'pause_status': 'removed'})

    def cleanup_barrier(self):
        if self.journal.get('status') == 'rollback_failed':
            # Retain the explicit admission pause when neither signer revision
            # can be proven healthy. Its ARN is in the failure artifact.
            return
        if self.journal.get('pause_status') == 'creating' and not self.journal.get('pause_rule'):
            # Reconcile a lost CreateRule response without creating another rule.
            owner = f'alto-rpc-{os.environ["GITHUB_RUN_ID"]}-{os.environ["GITHUB_RUN_ATTEMPT"]}'
            rules = self.elb.describe_rules(ListenerArn=self.listener)['Rules']
            for rule in rules:
                if rule.get('IsDefault'):
                    continue
                descriptions = self.elb.describe_tags(ResourceArns=[rule['RuleArn']])['TagDescriptions']
                tags = {x['Key']: x['Value'] for x in descriptions[0]['Tags']}
                if tags.get('WorkflowOwner') == owner and tags.get('Source') == self.args.source:
                    self.save({'pause_rule': rule['RuleArn']})
        self.remove_barrier()

    def scan(self):
        digest = self.args.image.split('@', 1)[1]
        detail = self.ecr.describe_images(repositoryName=REPOSITORY, imageIds=[{'imageDigest': digest}])['imageDetails'][0]
        expected_tag = f'rpc-auth-{self.args.source}-{os.environ["GITHUB_RUN_ID"]}-{os.environ["GITHUB_RUN_ATTEMPT"]}'
        require(expected_tag in detail.get('imageTags', []), 'candidate_source_tag_mismatch')
        response = self.ecr.describe_image_scan_findings(repositoryName=REPOSITORY, imageId={'imageDigest': digest})
        validate_scan(response, digest, datetime.now(timezone.utc))

    def canary(self, candidate, service, scope='rpc'):
        source = Path(__file__).resolve().parents[3] / 'infra' / 'alto' / 'qualify-rpc.mjs'
        require(source.is_file(), 'missing_canary_source')
        code = source.read_text()
        if scope == 'rpc':
            gateway = Path(__file__).resolve().parents[1] / 'gateway' / 'server.mjs'
            code = 'globalThis.__PLETHER_RPC_GATEWAY_SOURCE__ = ' + json.dumps(gateway.read_text()) + ';\n' + code
            definition = canary_definition(candidate, self.args.source, code, self.baseline_url(service))
            network = deepcopy(service['networkConfiguration'])
        else:
            require(scope == 'idle', 'unexpected_canary_scope')
            definition = idle_canary_definition(candidate, self.args.source, code, self.baseline_url(service))
            api = self.ecs.describe_services(cluster=CLUSTER, services=['plether-api'])
            require(not api.get('failures') and len(api['services']) == 1, 'missing_api_observer_network')
            api = api['services'][0]
            require(api['serviceName'] == 'plether-api' and api['desiredCount'] == api['runningCount'] == 1
                    and api['pendingCount'] == 0 and len(api['deployments']) == 1
                    and api['deployments'][0]['rolloutState'] == 'COMPLETED', 'api_observer_not_stable')
            network = deepcopy(api['networkConfiguration'])
        owner = f'alto-rpc-{os.environ["GITHUB_RUN_ID"]}-{os.environ["GITHUB_RUN_ATTEMPT"]}-{scope}'
        # A previous unfinished credential-free/credential-minimized canary must
        # finish before another one is launched, including interrupted runs.
        while self.ecs.list_tasks(cluster=CLUSTER, startedBy=owner)['taskArns']:
            self.budget()
            time.sleep(2)
        arn = self.register(definition)
        sequence = self.journal.get('canary_sequence', 0) + 1
        self.save({'canary_definition': arn, 'canary_sequence': sequence, 'status': 'canary_starting'})
        # Each new observation gets a new task. A repeated token would otherwise
        # return the already-stopped first idle task during rollback.
        token = hashlib.sha256((owner + self.args.source + str(sequence)).encode()).hexdigest()
        response = self.ecs.run_task(cluster=CLUSTER, taskDefinition=arn, launchType='FARGATE', platformVersion='1.4.0',
                                     startedBy=owner, clientToken=token, count=1,
                                     networkConfiguration=network,
                                     tags=[{'key': 'Capability', 'value': 'alto-rpc-qualification'},
                                           {'key': 'WorkflowOwner', 'value': owner}])
        require(not response.get('failures') and len(response.get('tasks', [])) == 1, 'canary_launch_failed')
        task_arn = response['tasks'][0]['taskArn']
        self.save({'canary_task': task_arn, 'status': 'canary_running'})
        try:
            while True:
                self.budget()
                task = self.ecs.describe_tasks(cluster=CLUSTER, tasks=[task_arn])['tasks'][0]
                if task['lastStatus'] == 'STOPPED':
                    require(len(task['containers']) == 1 and task['containers'][0].get('exitCode') == 0, 'canary_failed')
                    break
                time.sleep(2)
            stream = 'alto-rpc-canary/' + ALTO + '/' + task_arn.rsplit('/', 1)[1]
            records = []
            for _ in range(10):
                self.budget()
                try:
                    entries, token = [], None
                    for _page in range(10):
                        query = {'logGroupName': '/ecs/plether-sepolia', 'logStreamNames': [stream],
                                 'startTime': int(time.time() * 1000) - 300_000,
                                 'filterPattern': '{ $.event = "alto_rpc_qualification" }', 'limit': 100}
                        if token:
                            query['nextToken'] = token
                        response = self.logs.filter_log_events(**query)
                        entries.extend(response['events'])
                        token = response.get('nextToken')
                        if not token:
                            break
                    require(not token, 'canary_log_pagination_requires_reconciliation')
                except self.logs.exceptions.ResourceNotFoundException:
                    entries = []
                for entry in entries:
                    try:
                        record = json.loads(entry['message'])
                    except (ValueError, TypeError):
                        continue
                    if record.get('event') == CANARY_EVENT:
                        records.append(record)
                if records:
                    break
                time.sleep(2)
            require(len(records) == 1 and records[0].get('scope') == scope and records[0].get('required_checks_complete') is True
                    and records[0].get('tests') and all(x.get('pass') is True for x in records[0]['tests']),
                    'missing_or_failed_canary_proof')
            proof = records[0]
            saved = {'scope': scope, 'required_checks_complete': True,
                     'tests': [{'name': x['name'], 'pass': True} for x in proof['tests']]}
            if scope == 'idle':
                require(proof.get('idle_sample_count') == 2 and isinstance(proof.get('idle_observed_at_ms'), (int, float)),
                        'missing_quiescence_proof')
                saved.update(idle_sample_count=2, idle_observed_at_ms=proof['idle_observed_at_ms'])
                self.canary_proof = saved
            private_save(self.directory / (scope + '-qualification.json'), saved)
        finally:
            current = self.ecs.describe_tasks(cluster=CLUSTER, tasks=[task_arn])['tasks'][0]
            if current['lastStatus'] != 'STOPPED':
                self.ecs.stop_task(cluster=CLUSTER, task=task_arn, reason='Bounded credential-minimized RPC canary cleanup')
            self.ecs.deregister_task_definition(taskDefinition=arn)

    def quiescent(self):
        return bool(self.canary_proof and self.canary_proof['idle_sample_count'] == 2
                    and 0 <= time.time() * 1000 - self.canary_proof['idle_observed_at_ms'] <= 30_000)

    def postcheck(self):
        def fetch(path, payload=None):
            request = Request(PUBLIC + path, data=None if payload is None else json.dumps(payload).encode(),
                              headers={'User-Agent': 'plether-alto-rpc-cutover/1', 'Content-Type': 'application/json'})
            with urlopen(request, timeout=15) as response:
                require(response.status == 200, 'public_smoke_http_failure')
                return json.load(response)
        entry = fetch('/aa/rpc', {'jsonrpc': '2.0', 'id': 1, 'method': 'eth_supportedEntryPoints', 'params': []})
        require(not entry.get('error') and any(str(x).lower() == '0x4337084d9e255ff0702461cf8895ce9e3b5ff108'
                                             for x in entry.get('result', [])), 'public_entrypoint_smoke_failed')
        gas = fetch('/aa/rpc', {'jsonrpc': '2.0', 'id': 2, 'method': 'pimlico_getUserOperationGasPrice', 'params': []})
        result = gas.get('result', {})
        require(not gas.get('error') and all(isinstance(result.get(tier), dict)
                and re.fullmatch(r'0x[0-9a-fA-F]+', result[tier].get('maxFeePerGas', '')) is not None
                and int(result[tier]['maxFeePerGas'], 16) > 0
                and re.fullmatch(r'0x[0-9a-fA-F]+', result[tier].get('maxPriorityFeePerGas', '')) is not None
                for tier in ['slow', 'standard', 'fast']), 'public_gas_price_smoke_failed')
        self.save({'public_aa_smoke': 'passed'})

    def run(self):
        service = self.service()
        validate_service(service)
        before = self.definition(service['taskDefinition'])
        require_baseline(before)
        self.scan()
        candidate = patch_definition(before, self.args.image)
        self.save({'previous_task_definition': before['taskDefinitionArn'],
                   'baseline_fingerprint': fingerprint(before), 'status': 'registering'})
        arn = self.register(candidate)
        registered = self.definition(arn)
        assert_only_rpc_patch(before, registered, self.args.image)
        self.candidate_definition, self.baseline_service = registered, service
        self.save({'candidate_task_definition': arn, 'candidate_fingerprint': fingerprint(registered), 'status': 'prepared'})
        def canary():
            self.canary(registered, service)
            current = self.service()
            validate_service(current)
            require(current['taskDefinition'] == before['taskDefinitionArn']
                    and current['networkConfiguration'] == service['networkConfiguration']
                    and fingerprint(self.definition(before['taskDefinitionArn'])) == fingerprint(before), 'baseline_drift')
            self.ensure_barrier()
            self.canary(registered, service, 'idle')
        def healthy(target):
            self.rollback = target == before['taskDefinitionArn']
            self.wait_healthy(target)
            actual = self.definition(target)
            if self.rollback:
                require(fingerprint(actual) == fingerprint(before), 'rollback_definition_drift')
            else:
                assert_only_rpc_patch(before, actual, self.args.image)
        def postcheck():
            self.remove_barrier()
            self.postcheck()
        perform_cutover(self.ecs, before['taskDefinitionArn'], arn, canary, self.quiescent,
                        self.wait_stopped, healthy, postcheck, self.save, self.cleanup_barrier, self.capture_signers)

    def recover(self):
        require(self.path.is_file(), 'missing_recovery_journal')
        journal = json.loads(self.path.read_text())
        require(journal.get('schema_version') == 1 and journal.get('account') == ACCOUNT
                and journal.get('region') == REGION and journal.get('source') == self.args.source
                and journal.get('image') == self.args.image, 'recovery_scope_mismatch')
        self.journal = journal
        previous = journal.get('previous_task_definition', '')
        require(re.fullmatch(re.escape(TASK_PREFIX + FAMILY + ':') + r'[1-9][0-9]*', previous), 'invalid_recovery_baseline')
        before = self.definition(previous)
        require_baseline(before)
        require(fingerprint(before) == journal.get('baseline_fingerprint'), 'recovery_baseline_drift')
        service = self.service()
        validate_service(service, stable=False)
        candidate = journal.get('candidate_task_definition')
        require(service['taskDefinition'] in [previous, candidate], 'foreign_service_revision_during_recovery')
        require(service['desiredCount'] in [0, 1], 'unexpected_recovery_count')
        self.baseline_url(service)
        if journal.get('status') == 'complete':
            require(service['taskDefinition'] == candidate, 'completed_rollout_drift')
            registered = self.definition(candidate)
            assert_only_rpc_patch(before, registered, self.args.image)
            require(fingerprint(registered) == journal.get('candidate_fingerprint'), 'recovery_candidate_drift')
            self.wait_healthy(candidate)
            self.cleanup_barrier()
            return
        if service['taskDefinition'] == previous and service['desiredCount'] == service['runningCount'] == 1 \
                and service['pendingCount'] == 0:
            self.rollback = True
            self.wait_healthy(previous)
            self.save({'status': 'rolled_back'})
            self.cleanup_barrier()
            return
        require(candidate is not None, 'missing_recovery_candidate')
        registered = self.definition(candidate)
        assert_only_rpc_patch(before, registered, self.args.image)
        require(fingerprint(registered) == journal.get('candidate_fingerprint'), 'recovery_candidate_drift')
        self.candidate_definition, self.baseline_service = registered, service
        try:
            self.save({'status': 'rolling_back'})
            replace_service(self.ecs, previous, self.wait_stopped, self.wait_healthy, self.capture_signers)
            self.save({'status': 'rolled_back'})
            self.cleanup_barrier()
        except BaseException:
            self.save({'status': 'rollback_failed'})
            raise


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--image', required=True)
    parser.add_argument('--source', required=True)
    parser.add_argument('--directory', required=True, type=Path)
    parser.add_argument('--deadline-seconds', type=int, default=1200)
    parser.add_argument('--recover', action='store_true')
    args = parser.parse_args()
    validate_image(args.image)
    require(re.fullmatch(r'[0-9a-f]{40}', args.source) is not None and args.source == os.environ.get('GITHUB_SHA'), 'source_mismatch')
    require(os.environ.get('GITHUB_ACTIONS') == 'true' and os.environ.get('GITHUB_REF') == 'refs/heads/master'
            and re.fullmatch(r'[1-9][0-9]*', os.environ.get('GITHUB_RUN_ID', ''))
            and re.fullmatch(r'[1-9][0-9]*', os.environ.get('GITHUB_RUN_ATTEMPT', '')), 'protected_workflow_required')
    require((600 if args.recover else 1200) <= args.deadline_seconds <= 1800, 'invalid_deadline')
    args.directory.mkdir(mode=0o700, parents=True, exist_ok=True)
    require(not args.directory.is_symlink(), 'unsafe_operator_directory')
    os.chmod(args.directory, 0o700)
    import boto3
    session = boto3.Session(region_name=REGION)
    identity = session.client('sts').get_caller_identity()
    require(identity['Account'] == ACCOUNT and ':assumed-role/' in identity['Arn'], 'wrong_aws_identity')
    with (args.directory / '.lock').open('a+') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        require(args.recover or not (args.directory / 'rollout.json').exists(), 'existing_journal_requires_reconciliation')
        deployment = Deployment(session, args, args.directory)
        def cancelled(_signum, _frame):
            raise GuardRejected('operator_cancelled')
        signal.signal(signal.SIGTERM, cancelled)
        try:
            if args.recover:
                deployment.recover()
            else:
                deployment.run()
        finally:
            # Identifier-only evidence, never runtime configs, credentials or node logs.
            print(json.dumps({k: deployment.journal[k] for k in
                              ['status', 'previous_task_definition', 'candidate_task_definition', 'public_aa_smoke']
                              if k in deployment.journal}))


if __name__ == '__main__':
    os.umask(0o077)
    try:
        main()
    except BaseException as error:
        print(json.dumps({'status': 'stopped', 'error_type': type(error).__name__,
                          'reason': str(error) if isinstance(error, GuardRejected) else 'details_suppressed'}))
        raise SystemExit(1)
