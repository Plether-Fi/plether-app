"""Patch only the four close-assistance env values in an existing API task definition."""
import json
import sys
from pathlib import Path

def configure(task, release, mode):
    if mode != "disabled":
        raise ValueError("Close assistance issuance is unavailable for the canonical MockUSDG release")
    if task.get("family") != "plether-sepolia" or release.get("schemaVersion") != 4 or release.get("network", {}).get("chainId") != 421614:
        raise ValueError("Expected the canonical Arbitrum Sepolia release")
    apis = [c for c in task["containerDefinitions"] if c["name"] == "plether-api"]
    if len(apis) != 1:
        raise ValueError("Expected exactly one API container")
    api = apis[0]
    replacements = {
        "PERPS_CLOSE_ASSISTANCE_ENABLED": "false",
        "PERPS_CLOSE_ASSISTANCE_GLOBAL_ENABLED": "false",
        "PERPS_CLOSE_ASSISTANCE_LENS": "",
        "PERPS_CLOSE_ASSISTANCE_LENS_CODE_HASH": "",
    }
    api["environment"] = [e for e in api.get("environment", []) if e["name"] not in replacements] + [
        {"name": name, "value": value} for name, value in replacements.items()]
    return task

if __name__ == "__main__":
    path, release, mode = sys.argv[1:]
    task = configure(json.loads(Path(path).read_text()), json.loads(Path(release).read_text()), mode)
    Path(path).write_text(json.dumps(task, indent=2) + "\n")
