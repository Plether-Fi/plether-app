import copy
import importlib.util
import unittest
from pathlib import Path

spec = importlib.util.spec_from_file_location("configure", Path(__file__).with_name("configure-close-assistance.py"))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

class ConfigureTest(unittest.TestCase):
    def setUp(self):
        self.release = dict(chainId=421614, maximumSubsidyUsdc="200000", lens="0x" + "12" * 20, lensCodeHash="0x" + "34" * 32)
        self.task = dict(family="plether-sepolia", taskRoleArn="preserved", containerDefinitions=[dict(name="plether-api", image="existing", secrets=[dict(name="PRIVATE", valueFrom="unchanged")], environment=[dict(name=k, value=v) for k,v in dict(AA_RPC_MODE="dual-independent", AA_NATIVE_SPONSORSHIP_ENABLED="true", AA_NATIVE_SUBMISSION_ENABLED="true", AA_NATIVE_CANARY_OWNERS="0x" + "56" * 20, AA_NATIVE_GLOBAL_ROLLOUT_ENABLED="true").items()])])

    def test_only_assistance_configuration_changes(self):
        before = copy.deepcopy(self.task)
        result = module.configure(self.task, self.release, "canary")
        env = result["containerDefinitions"][0]["environment"]
        self.assertEqual({e["name"]:e["value"] for e in env}["PERPS_CLOSE_ASSISTANCE_GLOBAL_ENABLED"], "false")
        result["containerDefinitions"][0]["environment"] = [e for e in env if not e["name"].startswith("PERPS_CLOSE_ASSISTANCE_")]
        self.assertEqual(before, result)

    def test_disabled_preserves_binding_and_native_submission(self):
        result = module.configure(self.task, self.release, "disabled")
        env = {e["name"]:e["value"] for e in result["containerDefinitions"][0]["environment"]}
        self.assertEqual(env["PERPS_CLOSE_ASSISTANCE_ENABLED"], "false")
        self.assertEqual(env["PERPS_CLOSE_ASSISTANCE_LENS"], self.release["lens"])
        self.assertEqual(env["AA_NATIVE_SUBMISSION_ENABLED"], "true")

    def test_single_provider_cannot_enable_assistance(self):
        self.task["containerDefinitions"][0]["environment"][0]["value"] = "single-provider-sepolia"
        with self.assertRaises(ValueError):
            module.configure(self.task, self.release, "all")
        module.configure(self.task, self.release, "disabled")

    def test_no_canary_owner_or_wrong_chain_rejected(self):
        self.task["containerDefinitions"][0]["environment"] = [e for e in self.task["containerDefinitions"][0]["environment"] if e["name"] != "AA_NATIVE_CANARY_OWNERS"]
        with self.assertRaises(ValueError):
            module.configure(self.task, self.release, "canary")
        self.release["chainId"] = 42161
        with self.assertRaises(ValueError):
            module.configure(self.task, self.release, "disabled")

if __name__ == "__main__":
    unittest.main()
