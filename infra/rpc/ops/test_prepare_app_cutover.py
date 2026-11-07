import copy
import unittest
from prepare_app_cutover import patch_definition, GuardRejected, PARAMETER_PREFIX, RPC_PARAMETER, TOKEN_PARAMETER, ALTO_PARAMETER


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
        self.assertEqual(container['secrets'][:-1], original['containerDefinitions'][0]['secrets'])
        self.assertEqual(container['secrets'][-1], {'name': 'PERPS_RPC_AUTH_TOKEN', 'valueFrom': PARAMETER_PREFIX + TOKEN_PARAMETER})
        container['secrets'].pop()
        self.assertEqual(updated, source)
        self.assertEqual(len(changes), 1)

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

    def test_alto_is_decoupled_without_changing_its_image_or_keys(self):
        source = definition('plether-sepolia-alto', [{'name': 'ALTO_RPC_URL', 'valueFrom': PARAMETER_PREFIX + RPC_PARAMETER}])
        updated, _ = patch_definition(source, alto=True)
        self.assertEqual(updated['containerDefinitions'][0]['secrets'][0]['valueFrom'], PARAMETER_PREFIX + ALTO_PARAMETER)
        updated['containerDefinitions'][0]['secrets'] = source['containerDefinitions'][0]['secrets']
        self.assertEqual(updated, source)

    def test_conflicting_auth_and_duplicate_secrets_are_rejected(self):
        for extras in [[{'name': 'PERPS_RPC_AUTH_TOKEN', 'valueFrom': 'unrelated-token'}],
                       [{'name': 'PERPS_RPC_URL', 'valueFrom': 'unrelated-url'}]]:
            source = definition(); source['containerDefinitions'][0]['secrets'] += extras
            with self.assertRaises(GuardRejected): patch_definition(source)

    def test_mainnet_unknown_families_and_non_consumers_are_rejected(self):
        for source in [definition('plether-mainnet'), definition(secrets=[{'name': 'DATABASE_URL', 'valueFrom': 'unchanged'}])]:
            with self.assertRaises(GuardRejected): patch_definition(source)


if __name__ == '__main__': unittest.main()
