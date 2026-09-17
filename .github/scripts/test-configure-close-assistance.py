import copy
import importlib.util
import unittest
import json
import shlex
import shutil
import subprocess
import tempfile
from pathlib import Path

spec = importlib.util.spec_from_file_location("configure", Path(__file__).with_name("configure-close-assistance.py"))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

class ConfigureTest(unittest.TestCase):
    def setUp(self):
        self.release = dict(chainId=421614, maximumSubsidyUsdc="200000", lens="0x" + "12" * 20, lensCodeHash="0x" + "34" * 32)
        self.task = dict(family="plether-sepolia", taskRoleArn="preserved", containerDefinitions=[dict(name="plether-api", image="existing", secrets=[dict(name="PRIVATE", valueFrom="unchanged")], environment=[dict(name=k, value=v) for k,v in dict(AA_RPC_MODE="dual-independent", AA_NATIVE_SPONSORSHIP_ENABLED="true", AA_NATIVE_SUBMISSION_ENABLED="true", AA_NATIVE_CANARY_OWNERS="0x" + "56" * 20, AA_NATIVE_GLOBAL_ROLLOUT_ENABLED="true").items()])])

    def test_workflow_has_release_files_before_configuration(self):
        root = Path(__file__).resolve().parents[2]
        workflow = (root / ".github/workflows/deploy-backend.yml").read_text()
        deploy = workflow.split("\n  deploy:\n", 1)[1]
        checkout_start = deploy.index("      - name: Check out reviewed public AA deployment policy\n")
        configure_start = deploy.index("      - name: Configure exact Sepolia close assistance release\n")
        self.assertLess(checkout_start, configure_start)
        checkout = deploy[checkout_start:configure_start]
        checkout_path = next(line.split("path:", 1)[1].strip() for line in checkout.splitlines() if line.strip().startswith("path:"))
        command = next(line.split("run:", 1)[1].strip() for line in deploy[configure_start:].splitlines() if line.strip().startswith("run:"))
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            for source in [".github/scripts", "apps/backend/deployments"]:
                self.assertIn(source, checkout)
                shutil.copytree(root / source, work / checkout_path / source)
            (work / "task-def.json").write_text(json.dumps(self.task))
            argv = ["disabled" if arg == "$CLOSE_ASSISTANCE_MODE" else arg for arg in shlex.split(command)]
            subprocess.run(argv, cwd=work, check=True, capture_output=True, text=True)
            result = json.loads((work / "task-def.json").read_text())
            environment = {item["name"]: item["value"] for item in result["containerDefinitions"][0]["environment"]}
            self.assertEqual(environment["PERPS_CLOSE_ASSISTANCE_ENABLED"], "false")

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
