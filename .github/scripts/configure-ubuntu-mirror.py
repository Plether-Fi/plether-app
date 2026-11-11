#!/usr/bin/env python3
"""Use a signed Ubuntu archive mirror when the runner's default hosts fail DNS."""

import re
from pathlib import Path


def configure(root: Path) -> None:
    files = [root / "sources.list", *root.glob("*mirrors.txt")]
    files += list((root / "sources.list.d").glob("*.list"))
    files += list((root / "sources.list.d").glob("*.sources"))
    for path in files:
        if not path.is_file():
            continue
        original = path.read_text()
        updated = re.sub(
            r"https?://(?:azure\.archive|archive|security)\.ubuntu\.com/ubuntu(?=/|\s|$)",
            "https://mirrors.edge.kernel.org/ubuntu",
            original,
        )
        if updated != original:
            # Keep suites, components, Signed-By and all apt verification settings.
            path.write_text(updated)
            print(f"Updated Ubuntu mirror in {path}")


if __name__ == "__main__":
    configure(Path("/etc/apt"))
