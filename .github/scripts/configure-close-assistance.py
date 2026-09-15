"""Patch only the four close-assistance env values in an existing API task definition."""
import json
import re
import sys
from pathlib import Path

def configure(task, release, mode):
    if mode not in {"disabled", "canary", "all"}:
        raise ValueError("Unknown close assistance mode")
    if task.get("family") != "plether-sepolia" or release.get("chainId") != 421614:
        raise ValueError("Close assistance is pinned to Arbitrum Sepolia")
    if release.get("maximumSubsidyUsdc") != "200000":
        raise ValueError("Unexpected assistance ceiling")
    if not re.fullmatch(r"0x[0-9a-fA-F]{40}", release["lens"]) or not re.fullmatch(r"0x[0-9a-fA-F]{64}", release["lensCodeHash"]):
        raise ValueError("Invalid release bindings")
    apis = [c for c in task["containerDefinitions"] if c["name"] == "plether-api"]
    if len(apis) != 1:
        raise ValueError("Expected exactly one API container")
    api = apis[0]
    env = {e["name"]: e["value"] for e in api.get("environment", [])}
    if mode != "disabled" and (env.get("AA_RPC_MODE") != "dual-independent"
        or env.get("AA_NATIVE_SPONSORSHIP_ENABLED") != "true" or env.get("AA_NATIVE_SUBMISSION_ENABLED") != "true"):
        raise ValueError("Enable assistance only after independent native verification and submission are deployed")
    if mode == "canary" and not env.get("AA_NATIVE_CANARY_OWNERS"):
        raise ValueError("Operator canary owners are required")
    replacements = {
        "PERPS_CLOSE_ASSISTANCE_ENABLED": "false" if mode == "disabled" else "true",
        "PERPS_CLOSE_ASSISTANCE_GLOBAL_ENABLED": "true" if mode == "all" else "false",
        "PERPS_CLOSE_ASSISTANCE_LENS": release["lens"],
        "PERPS_CLOSE_ASSISTANCE_LENS_CODE_HASH": release["lensCodeHash"],
    }
    api["environment"] = [e for e in api.get("environment", []) if e["name"] not in replacements] + [
        {"name": name, "value": value} for name, value in replacements.items()]
    return task

if __name__ == "__main__":
    path, release, mode = sys.argv[1:]
    task = configure(json.loads(Path(path).read_text()), json.loads(Path(release).read_text()), mode)
    Path(path).write_text(json.dumps(task, indent=2) + "\n")
