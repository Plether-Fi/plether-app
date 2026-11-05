import copy
import importlib.util
import json
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location('finish', Path(__file__).with_name('finish_bootstrap.py'))
finish = importlib.util.module_from_spec(spec)
spec.loader.exec_module(finish)


class CleanupPlanTests(unittest.TestCase):
    def setUp(self):
        config = {'image': 'pinned-image', 'snapshot_url': 'pinned-snapshot', 'staging_volume_id': finish.STAGING}
        changed = {**config, 'staging_volume_id': ''}
        self.plan = {'resource_changes': [
            {'address': 'aws_ebs_volume.rpc_staging[0]', 'type': 'aws_ebs_volume', 'mode': 'managed', 'change': {'actions': ['delete'], 'before': {'id': finish.STAGING, 'size': 4096, 'encrypted': True}}},
            {'address': 'aws_volume_attachment.rpc_staging[0]', 'type': 'aws_volume_attachment', 'mode': 'managed', 'change': {'actions': ['delete'], 'before': {'volume_id': finish.STAGING, 'instance_id': finish.INSTANCE, 'force_detach': False}}},
            {'address': 'aws_ssm_parameter.rpc_config[0]', 'type': 'aws_ssm_parameter', 'mode': 'managed', 'change': {'actions': ['update'], 'before': {'name': '/plether/sepolia/rpc-node/config', 'type': 'String', 'value': json.dumps(config)}, 'after': {'name': '/plether/sepolia/rpc-node/config', 'type': 'String', 'value': json.dumps(changed)}}},
        ]}

    def test_accepts_only_the_named_staging_cleanup(self):
        finish.review_cleanup(self.plan)

    def test_refuses_other_volume_or_node(self):
        for index, key in [(0, 'id'), (1, 'instance_id'), (1, 'volume_id')]:
            plan = copy.deepcopy(self.plan)
            plan['resource_changes'][index]['change']['before'][key] = 'wrong-resource'
            with self.assertRaises(finish.PlanRejected): finish.review_cleanup(plan)

    def test_refuses_node_or_application_changes(self):
        for address in ['aws_instance.rpc_node[0]', 'aws_db_instance.main']:
            plan = copy.deepcopy(self.plan)
            plan['resource_changes'].append({'address': address, 'type': 'fixture', 'mode': 'managed', 'change': {'actions': ['update']}})
            with self.assertRaises(finish.PlanRejected): finish.review_cleanup(plan)

    def test_refuses_force_detach_and_missing_operations(self):
        for flag in ['force_detach', 'stop_instance_before_detaching', 'skip_destroy']:
            plan = copy.deepcopy(self.plan)
            plan['resource_changes'][1]['change']['before'][flag] = True
            with self.assertRaises(finish.PlanRejected): finish.review_cleanup(plan)
        self.plan['resource_changes'].pop()
        with self.assertRaises(finish.PlanRejected): finish.review_cleanup(self.plan)

    def test_refuses_other_runtime_config_changes(self):
        after = self.plan['resource_changes'][2]['change']['after']
        config = json.loads(after['value']);config['image'] = 'different-image'
        after['value'] = json.dumps(config)
        with self.assertRaises(finish.PlanRejected): finish.review_cleanup(self.plan)
