import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('rpc_node', Path(__file__).with_name('node.py'))
node = importlib.util.module_from_spec(spec)
spec.loader.exec_module(node)


class BootstrapPolicyTests(unittest.TestCase):
    def setUp(self):
        self.config = {
            'image': 'offchainlabs/nitro-node@sha256:' + 'a' * 64,
            'snapshot_url': 'https://snapshot.arbitrum.foundation/sepolia-rollup/2026-10-20-dbd26a00/',
            'snapshot_metadata_sha256': 'b' * 64,
            'state_history_blocks': 345600,
            'log_group': '/plether/sepolia/rpc-node',
        }
        self.parent = {
            'execution_url': 'https://execution.fixture.invalid',
            'beacon_url': 'https://beacon.fixture.invalid',
            'secondary_beacon_url': 'https://backup-beacon.fixture.invalid',
        }

    def test_only_explicit_official_sepolia_snapshot_and_image_are_allowed(self):
        node.validate_config(self.config, self.parent)
        for change in [
            {'image': 'offchainlabs/nitro-node:latest'},
            {'snapshot_url': 'https://snapshot.arbitrum.foundation/arb1/2026-10-20-dbd26a00/'},
            {'snapshot_url': 'https://fixture.invalid/sepolia-rollup/2026-10-20-dbd26a00/'},
            {'state_history_blocks': 0}, {'state_history_blocks': 999999999},
            {'state_history_blocks': 1.5},
        ]:
            with self.assertRaises(ValueError):
                node.validate_config({**self.config, **change}, self.parent)

    def test_parent_dependencies_require_https_and_optional_backup_is_distinct(self):
        for key in self.parent:
            for value in ['http://fixture.invalid', 'https://user:password@fixture.invalid']:
                with self.assertRaises(ValueError):
                    node.validate_config(self.config, {**self.parent, key: value})
        with self.assertRaises(ValueError):
            node.validate_config(self.config, {**self.parent, 'secondary_beacon_url': self.parent['beacon_url']})

    def test_minimal_pilot_accepts_alchemy_and_needs_no_second_beacon_provider(self):
        parent = {
            'execution_url': 'https://eth-sepolia.g.alchemy.com/v2',
            'beacon_url': 'https://eth-sepoliabeacon.g.alchemy.com/v2',
        }
        node.validate_config(self.config, parent)
        self.assertEqual(node.nitro_config(self.config, parent)['parent-chain']['blob-client'], {'beacon-url': parent['beacon_url']})
        node.validate_config(self.config, {**parent, 'secondary_beacon_url': ''})

    def test_ebs_root_or_partition_is_never_selected_as_instance_store(self):
        disk = {'name': 'nvme1n1', 'model': 'Amazon EC2 NVMe Instance Storage', 'size': 3750000000000, 'type': 'disk'}
        root = {'name': 'nvme0n1', 'model': 'Amazon Elastic Block Store', 'size': 100000000000, 'type': 'disk'}
        self.assertEqual(node.choose_nvme([root, disk]), '/dev/nvme1n1')
        for devices in [[root], [{**disk, 'type': 'part'}], [{**disk, 'size': 1875000000000}], [disk, {**disk, 'name': 'nvme2n1'}]]:
            with self.assertRaises(ValueError):
                node.choose_nvme(devices)

    def test_recent_state_index_is_finite_and_parent_chain_is_explicit(self):
        result = node.nitro_config(self.config, self.parent)
        self.assertEqual(result['chain']['id'], 421614)
        self.assertEqual(result['parent-chain']['id'], 11155111)
        self.assertTrue(result['execution']['caching']['archive'])
        self.assertEqual(result['execution']['caching']['state-history'], 345600)
        self.assertNotIn('private-key', str(result))

    def test_service_restart_refuses_missing_data_mount_before_writing_or_importing(self):
        with patch.object(node.os.path, 'ismount', return_value=False):
            with self.assertRaisesRegex(ValueError, 'data mount absent'):
                node.prepare(self.config, self.parent, 'ap-southeast-1')

    def test_download_and_database_are_separate_mounts_and_checksums_are_mandatory(self):
        args = node.docker(self.config, 'ap-southeast-1', bootstrap=True)
        self.assertIn('type=bind,src=/srv/nitro-staging,dst=/downloads', args)
        self.assertIn('--init.validate-checksum=true', args)
        self.assertIn('--init.then-quit=true', args)
        live = node.docker(self.config, 'ap-southeast-1')
        self.assertFalse(any(arg.startswith('--init.') for arg in live))
        self.assertFalse(any('fixture.invalid' in arg for arg in live))


if __name__ == '__main__':
    unittest.main()
