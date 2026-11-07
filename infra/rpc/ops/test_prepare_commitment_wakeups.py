import copy
import json
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace
import unittest

from prepare_commitment_wakeups import (
    ACCOUNT, CLUSTER, MODE_KEY, MODES, REGION, SERVICES, TARGETS,
    GuardRejected, execute, fingerprint, mode_values, patch_definition,
    registration_request, require_desired_mode,
)


def fixture(family, mode=None):
    containers = []
    for name in TARGETS[family]:
        env = [{'name': 'KEEPER_POLL_SECONDS', 'value': '1'},
               {'name': 'KEEPER_IDLE_POLL_SECONDS', 'value': '1'},
               {'name': 'KEEPER_CONFIRMATIONS', 'value': '1'},
               {'name': 'KEEPER_MAX_BATCH_SIZE', 'value': '5'},
               {'name': 'MAX_TRANSACTION_GAS', 'value': '25000000'},
               {'name': 'GAS_BUFFER_BPS', 'value': '12000'},
               {'name': 'LP_SETTLEMENT_MODE', 'value': 'execute'}]
        if mode is not None:
            env.append({'name': MODE_KEY, 'value': mode})
        containers.append({'name': name, 'image': 'fixture:before', 'command': [name],
                           'environment': env,
                           'secrets': [{'name': 'PRIVATE_KEY', 'valueFrom': 'unchanged-signer-reference'},
                                       {'name': 'PERPS_RPC_URL', 'valueFrom': 'unchanged-rpc-reference'}]})
    containers.append({'name': 'otel-log-router', 'image': 'router:before',
                       'environment': [{'name': 'SERVICE_VERSION', 'value': 'before'}]})
    return {'family': family, 'networkMode': 'awsvpc', 'requiresCompatibilities': ['FARGATE'],
            'runtimePlatform': {'cpuArchitecture': 'ARM64', 'operatingSystemFamily': 'LINUX'},
            'executionRoleArn': 'unchanged-execution-role', 'taskRoleArn': 'unchanged-task-role',
            'cpu': '512', 'memory': '1024', 'volumes': [], 'containerDefinitions': containers}


class FakeEcs:
    """Only task registration exists: preparation cannot mutate/start services."""
    def __init__(self):
        self.definitions = {}
        self.latest = {}
        self.registered = []
        self.fail_once_on = None
        allowed = set(fixture('plether-sepolia')) | {'tags'}
        self.meta = SimpleNamespace(service_model=SimpleNamespace(
            operation_model=lambda _: SimpleNamespace(input_shape=SimpleNamespace(members=allowed))))
        for family in TARGETS:
            self.add(fixture(family))
        self.services = {
            name: {'serviceName': name, 'taskDefinition': self.latest[family],
                   'desiredCount': count, 'runningCount': count, 'pendingCount': 0,
                   'deployments': [{'rolloutState': 'COMPLETED'}]}
            for name, (family, count) in SERVICES.items()}

    def add(self, definition):
        definition = copy.deepcopy(definition)
        family = definition['family']
        revision = int(self.latest[family].rsplit(':', 1)[1]) + 1 if family in self.latest else 1
        arn = f'arn:aws:ecs:{REGION}:{ACCOUNT}:task-definition/{family}:{revision}'
        definition.update(taskDefinitionArn=arn, revision=revision, status='ACTIVE')
        self.definitions[arn] = definition
        self.latest[family] = arn
        return arn

    def describe_services(self, cluster, services):
        assert cluster == CLUSTER
        return {'services': [copy.deepcopy(self.services[name]) for name in services], 'failures': []}

    def describe_task_definition(self, taskDefinition, include):
        assert include == ['TAGS']
        definition = copy.deepcopy(self.definitions[self.latest.get(taskDefinition, taskDefinition)])
        tags = definition.pop('tags', [])
        return {'taskDefinition': definition, 'tags': tags}

    def register_task_definition(self, **request):
        if self.fail_once_on == len(self.registered) + 1:
            self.fail_once_on = None
            raise RuntimeError('simulated request failure')
        assert request.get('tags') != [], 'AWS rejects explicit empty tags'
        self.registered.append(copy.deepcopy(request))
        arn = self.add(request)
        return {'taskDefinition': {'taskDefinitionArn': arn}}

    def deploy_release(self):
        for name, (family, _) in SERVICES.items():
            definition = copy.deepcopy(self.definitions[self.latest[family]])
            for container in definition['containerDefinitions']:
                container['image'] = 'reviewed:release'
                if container['name'] == 'otel-log-router':
                    container['environment'][0]['value'] = 'release'
            self.services[name]['taskDefinition'] = self.add(definition)


class CommitmentWakeupPreparationTests(unittest.TestCase):
    def test_all_modes_change_only_target_environment_slots_without_mutating_the_source(self):
        for family in TARGETS:
            for previous in [None, *MODES]:
                for desired in MODES:
                    with self.subTest(family=family, previous=previous, desired=desired):
                        before = fixture(family, previous)
                        original = copy.deepcopy(before)
                        after, changes = patch_definition(before, desired)
                        self.assertEqual(before, original)
                        self.assertEqual(fingerprint(before), fingerprint(after))
                        self.assertEqual(len(changes), len(TARGETS[family]))
                        require_desired_mode(after, desired)
                        for container in after['containerDefinitions']:
                            if container['name'] in TARGETS[family]:
                                container['environment'] = [x for x in container['environment'] if x['name'] != MODE_KEY]
                                if previous is not None:
                                    container['environment'].append({'name': MODE_KEY, 'value': previous})
                        self.assertEqual(after, original)

    def test_fingerprint_detects_execution_policy_signer_role_and_topology_changes(self):
        source = fixture('plether-sepolia-workers')
        for key in ['KEEPER_POLL_SECONDS', 'KEEPER_IDLE_POLL_SECONDS', 'KEEPER_CONFIRMATIONS',
                    'KEEPER_MAX_BATCH_SIZE', 'MAX_TRANSACTION_GAS', 'GAS_BUFFER_BPS', 'LP_SETTLEMENT_MODE']:
            changed = copy.deepcopy(source)
            next(x for x in changed['containerDefinitions'][0]['environment'] if x['name'] == key)['value'] = 'different'
            self.assertNotEqual(fingerprint(source, True), fingerprint(changed, True), key)
        for mutate in [lambda x: x.update(taskRoleArn='different'),
                       lambda x: x.update(executionRoleArn='different'),
                       lambda x: x.update(networkMode='different'),
                       lambda x: x.update(memory='different'),
                       lambda x: x['containerDefinitions'][0]['secrets'][0].update(valueFrom='different')]:
            changed = copy.deepcopy(source)
            mutate(changed)
            self.assertNotEqual(fingerprint(source, True), fingerprint(changed, True))

    def test_release_hash_allows_only_images_and_log_router_service_version(self):
        source = fixture('plether-sepolia-workers')
        changed = copy.deepcopy(source)
        for c in changed['containerDefinitions']:
            c['image'] = 'new-release'
        changed['containerDefinitions'][-1]['environment'][0]['value'] = 'new-release'
        self.assertEqual(fingerprint(source, True), fingerprint(changed, True))
        self.assertNotEqual(fingerprint(source), fingerprint(changed))
        changed['containerDefinitions'][0]['environment'].append({'name': 'SERVICE_VERSION', 'value': 'unapproved'})
        self.assertNotEqual(fingerprint(source, True), fingerprint(changed, True))

    def test_rejects_ambiguous_modes_overrides_wrong_family_and_topology(self):
        source = fixture('plether-sepolia-workers')
        samples = []
        for mutate in [lambda x: x.update(family='plether-mainnet-workers'),
                       lambda x: x['runtimePlatform'].update(cpuArchitecture='X86_64'),
                       lambda x: x['containerDefinitions'].append(copy.deepcopy(x['containerDefinitions'][0])),
                       lambda x: x['containerDefinitions'].pop(0),
                       lambda x: x['containerDefinitions'][0]['command'].append('--commitment-wakeups-mode=active'),
                       lambda x: x['containerDefinitions'][0].update(entryPoint=['sh', '-c', MODE_KEY + '=active app']),
                       lambda x: x['containerDefinitions'][0]['secrets'].append({'name': MODE_KEY, 'valueFrom': 'hidden'}),
                       lambda x: x['containerDefinitions'][0]['environment'].append(copy.deepcopy(x['containerDefinitions'][0]['environment'][0])),
                       lambda x: x['containerDefinitions'][0]['environment'].append({'name': MODE_KEY, 'value': 'ACTIVE'}),
                       lambda x: x['containerDefinitions'][-1]['environment'].append({'name': MODE_KEY, 'value': 'active'})]:
            changed = copy.deepcopy(source)
            mutate(changed)
            samples.append(changed)
        for sample in samples:
            with self.assertRaises(GuardRejected):
                patch_definition(sample, 'observe')

    def test_absent_off_is_a_baseline_but_not_an_explicit_prepared_mode(self):
        source = fixture('plether-sepolia')
        self.assertEqual(mode_values(source), {'plether-api': 'off'})
        with self.assertRaisesRegex(GuardRejected, 'missing_explicit_mode'):
            require_desired_mode(source, 'off')
        with self.assertRaisesRegex(GuardRejected, 'mode_drift'):
            require_desired_mode(source, 'active')

    def test_registration_preserves_real_tags_and_omits_rejected_empty_list(self):
        source = fixture('plether-sepolia')
        for tags in [[], [{'key': 'Environment', 'value': 'sepolia'}]]:
            source['tags'] = tags
            request = registration_request(source, set(source))
            self.assertEqual(request.get('tags'), tags or None)
            self.assertEqual(fingerprint(source), fingerprint(request))

    def test_interrupted_prepare_resumes_without_duplicate_registration_then_verifies_release(self):
        ecs = FakeEcs()
        ecs.fail_once_on = 2
        with TemporaryDirectory() as directory:
            work = Path(directory)
            with self.assertRaises(RuntimeError):
                execute('prepare', 'observe', work, ecs)
            self.assertEqual(len(ecs.registered), 1)
            self.assertEqual(execute('prepare', 'observe', work, ecs)['status'], 'prepared')
            self.assertEqual(len(ecs.registered), 4)
            execute('prepare', 'observe', work, ecs)
            self.assertEqual(len(ecs.registered), 4)
            with self.assertRaisesRegex(GuardRejected, 'prepared_family_not_running'):
                execute('verify', None, work, ecs)
            ecs.deploy_release()
            self.assertEqual(execute('verify', None, work, ecs)['status'], 'verified')
            for name, (_, count) in SERVICES.items():
                self.assertEqual(ecs.services[name]['runningCount'], count)

    def test_live_verification_rejects_gas_drift_and_mode_drift(self):
        for key in [MODE_KEY, 'KEEPER_CONFIRMATIONS', 'MAX_TRANSACTION_GAS']:
            with self.subTest(key=key), TemporaryDirectory() as directory:
                ecs = FakeEcs()
                work = Path(directory)
                execute('prepare', 'active', work, ecs)
                ecs.deploy_release()
                d = ecs.definitions[ecs.latest['plether-sepolia']]
                next(x for x in d['containerDefinitions'][0]['environment'] if x['name'] == key)['value'] = 'off' if key == MODE_KEY else 'different'
                with self.assertRaises(GuardRejected):
                    execute('verify', 'active', work, ecs)

    def test_baseline_mode_disagreement_or_active_service_drift_prevents_all_registration(self):
        for mutation in ['mode', 'count', 'pending', 'unstable', 'revision', 'keeper']:
            with self.subTest(mutation=mutation), TemporaryDirectory() as directory:
                ecs = FakeEcs()
                if mutation == 'mode':
                    ecs.definitions[ecs.latest['plether-sepolia']]['containerDefinitions'][0]['environment'].append({'name': MODE_KEY, 'value': 'active'})
                elif mutation == 'count':
                    ecs.services['plether-workers']['runningCount'] = 2
                elif mutation == 'pending':
                    ecs.services['plether-workers']['pendingCount'] = 1
                elif mutation == 'unstable':
                    ecs.services['plether-workers']['deployments'][0]['rolloutState'] = 'IN_PROGRESS'
                elif mutation == 'revision':
                    ecs.add(fixture('plether-sepolia'))
                else:
                    ecs.definitions[ecs.latest['plether-sepolia-keeper']]['containerDefinitions'][0]['secrets'][0]['valueFrom'] = 'different'
                with self.assertRaises(GuardRejected):
                    execute('prepare', 'observe', Path(directory), ecs)
                self.assertEqual(ecs.registered, [])

    def test_resume_rejects_superseded_or_tampered_journal_and_missing_mode(self):
        for mutation in ['superseded', 'mode', 'account', 'services', 'arn', 'prepared_mode']:
            with self.subTest(mutation=mutation), TemporaryDirectory() as directory:
                ecs = FakeEcs()
                work = Path(directory)
                execute('prepare', 'observe', work, ecs)
                path = work / 'commitment-wakeups-rollout.json'
                journal = json.loads(path.read_text())
                if mutation == 'superseded':
                    ecs.add(copy.deepcopy(ecs.definitions[ecs.latest['plether-sepolia']]))
                elif mutation == 'mode':
                    journal['mode'] = 'active'
                elif mutation == 'account':
                    journal['account'] = '111111111111'
                elif mutation == 'services':
                    journal['services'].pop('plether-api')
                elif mutation == 'arn':
                    journal['services']['plether-api']['previous_task_definition'] = 'unexpected'
                else:
                    ecs.definitions[ecs.latest['plether-sepolia']]['containerDefinitions'][0]['environment'] = [x for x in ecs.definitions[ecs.latest['plether-sepolia']]['containerDefinitions'][0]['environment'] if x['name'] != MODE_KEY]
                path.write_text(json.dumps(journal))
                with self.assertRaises(GuardRejected):
                    execute('prepare', 'observe', work, ecs)
                self.assertEqual(len(ecs.registered), 4)

    def test_turning_off_uses_a_separate_stage_and_preserves_policies(self):
        ecs = FakeEcs()
        with TemporaryDirectory() as first, TemporaryDirectory() as second:
            execute('prepare', 'active', Path(first), ecs)
            ecs.deploy_release()
            execute('verify', None, Path(first), ecs)
            execute('prepare', 'off', Path(second), ecs)
            ecs.deploy_release()
            result = execute('verify', 'off', Path(second), ecs)
            self.assertEqual(result['mode'], 'off')
            self.assertTrue(result['confirmation_gas_and_other_policies_preserved'])


if __name__ == '__main__':
    unittest.main()
