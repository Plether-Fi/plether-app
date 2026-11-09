"""Set the public Privy app ID on an existing API task without changing other bindings."""
import json
import re
import sys
from pathlib import Path


def configure(task, app_id):
    if not isinstance(app_id, str) or not re.fullmatch(r"[A-Za-z0-9_-]{1,128}", app_id):
        raise ValueError("Configure a valid public PRIVY_APP_ID before deploying the API")
    containers = task.get("containerDefinitions", [])
    api = [container for container in containers if container.get("name") == "plether-api"]
    if len(api) != 1:
        raise ValueError("Expected exactly one plether-api container")
    if any(secret.get("name") == "PRIVY_APP_ID" for secret in api[0].get("secrets", [])):
        raise ValueError("PRIVY_APP_ID must be a public environment binding, not a secret")
    api[0]["environment"] = [entry for entry in api[0].get("environment", [])
                             if entry.get("name") != "PRIVY_APP_ID"] + [dict(name="PRIVY_APP_ID", value=app_id)]
    return task


if __name__ == "__main__":
    path = Path(sys.argv[1])
    configured = configure(json.loads(path.read_text()), sys.argv[2])
    path.write_text(json.dumps(configured) + "\n")
