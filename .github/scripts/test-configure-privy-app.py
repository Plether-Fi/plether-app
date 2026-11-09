import copy
import importlib.util
import json
from pathlib import Path
import shlex
import shutil
import subprocess
import tempfile
import unittest

spec = importlib.util.spec_from_file_location("configure_privy", Path(__file__).with_name("configure-privy-app.py"))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class ConfigurePrivyTest(unittest.TestCase):
    def setUp(self):
        self.task = dict(family="plether-sepolia", taskRoleArn="preserved", containerDefinitions=[
            dict(name="plether-api", image="existing", secrets=[dict(name="OTHER", valueFrom="unchanged")],
                 environment=[dict(name="AA_RPC_MODE", value="single-provider-testnet")]),
            dict(name="otel-log-router", image="unchanged", environment=[dict(name="SERVICE_VERSION", value="old")]),
        ])

    def test_adds_only_public_api_binding_and_is_idempotent(self):
        original = copy.deepcopy(self.task)
        first = module.configure(self.task, "public-test-app")
        self.assertEqual(first, module.configure(copy.deepcopy(first), "public-test-app"))
        binding = first["containerDefinitions"][0]["environment"].pop()
        self.assertEqual(binding, dict(name="PRIVY_APP_ID", value="public-test-app"))
        self.assertEqual(first, original)

    def test_replaces_stale_and_duplicate_app_ids(self):
        self.task["containerDefinitions"][0]["environment"] += [dict(name="PRIVY_APP_ID", value="old")] * 2
        result = module.configure(self.task, "public-test-app")
        self.assertEqual([entry["value"] for entry in result["containerDefinitions"][0]["environment"]
                          if entry["name"] == "PRIVY_APP_ID"], ["public-test-app"])

    def test_invalid_or_ambiguous_configuration_is_rejected_without_mutation(self):
        for value in ["", " ", "https://wrong.example", "app\n", "x" * 129, None]:
            before = copy.deepcopy(self.task)
            with self.assertRaises(ValueError):
                module.configure(self.task, value)
            self.assertEqual(self.task, before)
        for containers in [[], [self.task["containerDefinitions"][0]] * 2]:
            with self.assertRaises(ValueError):
                module.configure(dict(containerDefinitions=containers), "public-test-app")
        self.task["containerDefinitions"][0]["secrets"].append(dict(name="PRIVY_APP_ID", valueFrom="wrong"))
        with self.assertRaises(ValueError):
            module.configure(self.task, "public-test-app")

    def test_actual_workflow_command_configures_the_existing_task_before_deployment(self):
        root = Path(__file__).resolve().parents[2]
        workflow = (root / ".github/workflows/deploy-backend.yml").read_text()
        step = workflow.split("      - name: Configure Sepolia Privy app for passkey names\n", 1)[1]
        self.assertIn("PRIVY_APP_ID: ${{ vars.PRIVY_APP_ID }}", step.split("      - name:", 1)[0])
        self.assertLess(workflow.index("Check out reviewed public AA deployment policy"), workflow.index("Configure Sepolia Privy app for passkey names"))
        self.assertLess(workflow.index("Configure Sepolia Privy app for passkey names"), workflow.index("Update application images and service version"))
        command = next(line.split("run:", 1)[1].strip() for line in step.splitlines() if line.strip().startswith("run:"))
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            shutil.copytree(root / ".github/scripts", work / "deployment-policy/.github/scripts")
            (work / "task-def.json").write_text(json.dumps(self.task))
            argv = ["public-test-app" if arg == "$PRIVY_APP_ID" else arg for arg in shlex.split(command)]
            subprocess.run(argv, cwd=work, check=True, capture_output=True, text=True)
            result = json.loads((work / "task-def.json").read_text())
            self.assertIn(dict(name="PRIVY_APP_ID", value="public-test-app"), result["containerDefinitions"][0]["environment"])


if __name__ == "__main__":
    unittest.main()
