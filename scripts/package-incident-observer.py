#!/usr/bin/env python3
"""Build a deterministic Lambda zip without downloading dependencies."""
import argparse
import pathlib
import zipfile


def package(output):
    source = pathlib.Path(__file__).resolve().parents[1] / "infra/incident-observer/handler.py"
    target = pathlib.Path(output)
    target.parent.mkdir(parents=True, exist_ok=True)
    entry = zipfile.ZipInfo("handler.py", (2026, 1, 1, 0, 0, 0))
    entry.external_attr = 0o644 << 16
    entry.compress_type = zipfile.ZIP_DEFLATED
    with zipfile.ZipFile(target, "w") as archive:
        archive.writestr(entry, source.read_bytes())
    return target


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()
    print(package(args.output))
