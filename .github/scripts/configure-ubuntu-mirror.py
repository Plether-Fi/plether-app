#!/usr/bin/env python3
"""Use the official signed Ubuntu archive with a fallback for runner mirror lists."""

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
            r"https?://(?:(?:azure\.archive|archive|security)\.ubuntu\.com|mirrors\.edge\.kernel\.org)/ubuntu(?=/|\s|$)",
            "https://archive.ubuntu.com/ubuntu",
            original,
        )
        if path.name.endswith("mirrors.txt") and "https://archive.ubuntu.com/ubuntu" in updated:
            # APT can try the next signed archive if one endpoint times out.
            updated = "\n".join(dict.fromkeys(updated.splitlines())).rstrip() + "\nhttps://mirrors.edge.kernel.org/ubuntu\n"
        if updated != original:
            # Keep suites, components, Signed-By and all apt verification settings.
            path.write_text(updated)
            print(f"Updated Ubuntu mirror in {path}")


if __name__ == "__main__":
    configure(Path("/etc/apt"))
