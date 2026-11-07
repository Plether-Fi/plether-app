import copy
import unittest
from prepare_app_cutover import patch_definition, patch_dormant_keeper, GuardRejected, PARAMETER_PREFIX, RPC_PARAMETER, TOKEN_PARAMETER, APP_PARAMETER


def definition(family='plether-sepolia', secrets=None):
    return {'family': family, 'networkMode': 'awsvpc', 'requiresCompatibilities': ['FARGATE'],
            'runtimePlatform': {'cpuArchitecture': 'ARM64'}, 'containerDefinitions': [{
                'name': 'fixture', 'image': 'unchanged@sha256:fixture', 'command': ['unchanged'],
                'environment': [{'name': 'AA_RPC_MODE', 'value': 'single-provider-sepolia'}],
                'secrets': secrets or [{'name': 'PERPS_RPC_URL', 'valueFrom': PARAMETER_PREFIX + RPC_PARAMETER},
                                      {'name': 'KEEPER_PRIVATE_KEY', 'valueFrom': 'unchanged-signer-reference'}]}]}


class CutoverPreparationTests(unittest.TestCase):
    def test_only_rpc_auth_slots_change_and_source_is_not_mutated(self):
        source = definition(); original = copy.deepcopy(source)
        updated, changes = patch_definition(source)
        self.assertEqual(source, original)
        container = updated['containerDefinitions'][0]
        self.assertEqual(container['secrets'][0]['valueFrom'], PARAMETER_PREFIX + APP_PARAMETER)
        self.assertEqual(container['secrets'][1], original['containerDefinitions'][0]['secrets'][1])
        self.assertEqual(container['secrets'][-1], {'name': 'PERPS_RPC_AUTH_TOKEN', 'valueFrom': PARAMETER_PREFIX + TOKEN_PARAMETER})
        container['secrets'] = original['containerDefinitions'][0]['secrets']
        self.assertEqual(updated, source)
        self.assertEqual(len(changes), 2)

    def test_spot_rpc_does_not_receive_the_arbitrum_token(self):
        source = definition(secrets=[{'name': 'RPC_URL', 'valueFrom': PARAMETER_PREFIX + '/plether/sepolia/rpc-url'},
                                     {'name': 'PERPS_RPC_URL', 'valueFrom': PARAMETER_PREFIX + RPC_PARAMETER}])
        updated, _ = patch_definition(source)
        names = [s['name'] for s in updated['containerDefinitions'][0]['secrets']]
        self.assertIn('PERPS_RPC_AUTH_TOKEN', names)
        self.assertNotIn('RPC_AUTH_TOKEN', names)

    def test_api_shared_rpc_alias_gets_both_auth_slots(self):
        source = definition(secrets=[{'name': n, 'valueFrom': PARAMETER_PREFIX + RPC_PARAMETER} for n in ['RPC_URL', 'PERPS_RPC_URL']])
        updated, _ = patch_definition(source)
        names = [s['name'] for s in updated['containerDefinitions'][0]['secrets']]
        self.assertIn('RPC_AUTH_TOKEN', names)
        self.assertIn('PERPS_RPC_AUTH_TOKEN', names)

    def test_alto_is_not_an_application_cutover_target(self):
        source = definition('plether-sepolia-alto', [{'name': 'ALTO_RPC_URL', 'valueFrom': PARAMETER_PREFIX + RPC_PARAMETER}])
        original = copy.deepcopy(source)
        with self.assertRaises(GuardRejected): patch_definition(source)
        self.assertEqual(source, original)

    def test_dormant_keeper_must_match_consolidated_configuration_after_rpc_change(self):
        standalone = definition('plether-sepolia-keeper')
        standalone['containerDefinitions'][0]['name'] = 'plether-keeper'
        workers = copy.deepcopy(standalone)
        workers['family'] = 'plether-sepolia-workers'
        prepared, _ = patch_definition(workers)
        updated, _ = patch_dormant_keeper(standalone, prepared)
        self.assertEqual(updated['containerDefinitions'], prepared['containerDefinitions'])
        self.assertEqual(standalone['containerDefinitions'][0]['secrets'][0]['valueFrom'], PARAMETER_PREFIX + RPC_PARAMETER)
        for field, extra in [('environment', {'name': 'LP_SETTLEMENT_MODE', 'value': 'execute'}),
                             ('secrets', {'name': 'UNRELATED_SIGNER', 'valueFrom': 'different-key'})]:
            drifted = copy.deepcopy(prepared)
            drifted['containerDefinitions'][0][field].append(extra)
            with self.assertRaises(GuardRejected): patch_dormant_keeper(standalone, drifted)

    def test_single_provider_verification_follows_the_direct_application_client(self):
        source = definition(secrets=[{'name': n, 'valueFrom': PARAMETER_PREFIX + RPC_PARAMETER} for n in ['PERPS_RPC_URL', 'AA_RECONCILER_SECONDARY_RPC_URL']])
        updated, _ = patch_definition(source)
        refs = {x['name']: x['valueFrom'] for x in updated['containerDefinitions'][0]['secrets']}
        self.assertEqual(refs['PERPS_RPC_URL'], PARAMETER_PREFIX + APP_PARAMETER)
        self.assertEqual(refs['AA_RECONCILER_SECONDARY_RPC_URL'], PARAMETER_PREFIX + APP_PARAMETER)
        source['containerDefinitions'][0]['environment'][0]['value'] = 'dual-independent'
        updated, _ = patch_definition(source)
        refs = {x['name']: x['valueFrom'] for x in updated['containerDefinitions'][0]['secrets']}
        self.assertEqual(refs['AA_RECONCILER_SECONDARY_RPC_URL'], PARAMETER_PREFIX + RPC_PARAMETER)

    def test_conflicting_auth_and_duplicate_secrets_are_rejected(self):
        for extras in [[{'name': 'PERPS_RPC_AUTH_TOKEN', 'valueFrom': 'unrelated-token'}],
                       [{'name': 'PERPS_RPC_URL', 'valueFrom': 'unrelated-url'}]]:
            source = definition(); source['containerDefinitions'][0]['secrets'] += extras
            with self.assertRaises(GuardRejected): patch_definition(source)

    def test_mainnet_unknown_families_and_non_consumers_are_rejected(self):
        for source in [definition('plether-mainnet'), definition(secrets=[{'name': 'DATABASE_URL', 'valueFrom': 'unchanged'}])]:
            with self.assertRaises(GuardRejected): patch_definition(source)


if __name__ == '__main__': unittest.main()
