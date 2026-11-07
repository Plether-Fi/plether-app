import copy
import unittest
from prepare_worker_cadence import TARGETS, GuardRejected, fingerprint, patch_definition, require_desired_cadence, registration_request


def fixture(family):
    containers = []
    for name, slots in TARGETS[family].items():
        env = [{'name': key, 'value': previous} for key, (previous, _) in slots.items()]
        env += [
            {'name': 'KEEPER_POLL_SECONDS', 'value': '1'},
            {'name': 'KEEPER_CONFIRMATIONS', 'value': '1'},
            {'name': 'PROTECTION_WORKER_CONFIRMATIONS', 'value': '12'},
            {'name': 'MAX_TRANSACTION_GAS', 'value': '987654'},
            {'name': 'LP_SETTLEMENT_POLL_SECONDS', 'value': '15'},
            {'name': 'LP_SETTLEMENT_MODE', 'value': 'execute'},
            {'name': 'GAS_BUFFER_BPS', 'value': '12000'},
        ]
        containers.append({'name': name, 'image': 'fixture@sha256:immutable',
                           'command': ['original'], 'environment': env,
                           'secrets': [{'name': 'PRIVATE_KEY', 'valueFrom': 'unchanged-reference'}]})
    return {'family': family, 'networkMode': 'awsvpc', 'requiresCompatibilities': ['FARGATE'],
            'runtimePlatform': {'cpuArchitecture': 'ARM64'},
            'executionRoleArn': 'unchanged-execution-role', 'taskRoleArn': 'unchanged-task-role',
            'cpu': '512', 'memory': '1024', 'containerDefinitions': containers}


class CadencePreparationTests(unittest.TestCase):
    def test_registration_omits_rejected_empty_tags_and_preserves_real_tags(self):
        for tags in [[], [{'key': 'Environment', 'value': 'sepolia'}]]:
            source = fixture('plether-sepolia-workers')
            source['tags'] = tags
            source['revision'] = 107
            allowed = set(source) - {'revision'}
            request = registration_request(source, allowed)
            self.assertNotIn('revision', request)
            if tags:
                self.assertEqual(request['tags'], tags)
            else:
                self.assertNotIn('tags', request)
            self.assertEqual(fingerprint(source), fingerprint(request))

    def test_every_family_changes_only_approved_cadence_slots(self):
        for family in TARGETS:
            with self.subTest(family=family):
                before = fixture(family); original = copy.deepcopy(before)
                after, changes = patch_definition(before)
                self.assertEqual(before, original)
                self.assertEqual(fingerprint(after), fingerprint(before))
                self.assertEqual(len(changes), sum(len(x) for x in TARGETS[family].values()))
                for c in after['containerDefinitions']:
                    env = {x['name']: x['value'] for x in c['environment']}
                    for key, (_, desired) in TARGETS[family][c['name']].items():
                        self.assertEqual(env[key], desired)
                    for entry in c['environment']:
                        if entry['name'] in TARGETS[family][c['name']]:
                            entry['value'] = next(x['value'] for old in before['containerDefinitions']
                                                  if old['name'] == c['name'] for x in old['environment'] if x['name'] == entry['name'])
                self.assertEqual(after, before)

    def test_protection_can_add_explicit_interval_over_the_existing_five_second_default(self):
        source = fixture('plether-sepolia-protection-worker')
        env = source['containerDefinitions'][0]['environment']
        env[:] = [x for x in env if x['name'] != 'PROTECTION_WORKER_POLL_SECONDS']
        after, changes = patch_definition(source)
        self.assertFalse(changes[0]['before_present'])
        self.assertEqual(changes[0]['before'], '5')
        self.assertEqual(fingerprint(source), fingerprint(after))

    def test_resumed_preparation_requires_the_actual_desired_values(self):
        for family in TARGETS:
            original = fixture(family)
            with self.assertRaises(GuardRejected): require_desired_cadence(original)
            prepared, _ = patch_definition(original)
            require_desired_cadence(prepared)

    def test_policy_hash_detects_confirmation_gas_signer_and_role_changes(self):
        source = fixture('plether-sepolia-workers')
        for key in ['KEEPER_POLL_SECONDS', 'KEEPER_CONFIRMATIONS', 'MAX_TRANSACTION_GAS', 'GAS_BUFFER_BPS', 'LP_SETTLEMENT_MODE']:
            changed = copy.deepcopy(source)
            next(x for x in changed['containerDefinitions'][0]['environment'] if x['name'] == key)['value'] = 'different'
            self.assertNotEqual(fingerprint(source, True), fingerprint(changed, True), key)
        for mutate in [lambda x: x.update(taskRoleArn='different-role'),
                       lambda x: x['containerDefinitions'][0]['secrets'][0].update(valueFrom='different-signer')]:
            changed = copy.deepcopy(source); mutate(changed)
            self.assertNotEqual(fingerprint(source, True), fingerprint(changed, True))

    def test_release_verification_allows_only_images_and_log_router_version_to_vary(self):
        source = fixture('plether-sepolia-workers')
        source['containerDefinitions'].append({'name': 'otel-log-router', 'image': 'old-router',
                                               'environment': [{'name': 'SERVICE_VERSION', 'value': 'old'}]})
        changed = copy.deepcopy(source)
        for c in changed['containerDefinitions']: c['image'] = 'new-release'
        changed['containerDefinitions'][-1]['environment'][0]['value'] = 'new'
        self.assertEqual(fingerprint(source, True), fingerprint(changed, True))
        self.assertNotEqual(fingerprint(source), fingerprint(changed))

    def test_unexpected_family_cadence_duplicates_and_cli_overrides_fail_closed(self):
        samples = []
        source = fixture('plether-sepolia-keeper')
        changed = copy.deepcopy(source); changed['family'] = 'plether-mainnet-keeper'; samples.append(changed)
        changed = copy.deepcopy(source); changed['containerDefinitions'][0]['environment'][0]['value'] = '30'; samples.append(changed)
        changed = copy.deepcopy(source); changed['containerDefinitions'][0]['environment'].append(copy.deepcopy(changed['containerDefinitions'][0]['environment'][0])); samples.append(changed)
        changed = copy.deepcopy(source); changed['containerDefinitions'][0]['command'] += ['--idle-poll-seconds', '30']; samples.append(changed)
        changed = copy.deepcopy(source); changed['containerDefinitions'][0]['secrets'].append({'name': 'KEEPER_IDLE_POLL_SECONDS', 'valueFrom': 'hidden-override'}); samples.append(changed)
        for sample in samples:
            with self.assertRaises(GuardRejected): patch_definition(sample)


if __name__ == '__main__':
    unittest.main()
