#!/usr/bin/env bash
set -euo pipefail

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repository_root"

matches="$({
  rg -n -i 'blockscout\.com' \
    apps/backend/app \
    apps/backend/src \
    apps/frontend/public \
    apps/frontend/src \
    infra/terraform || true
} | grep -Ev \
  '^(apps/frontend/public/perps-aa-manifest\.json|apps/frontend/src/utils/explorer\.ts|apps/frontend/src/stories/|apps/frontend/src/components/__tests__/)' || true)"

if [[ -n "$matches" ]]; then
  echo "Blockscout may only appear in reviewed explorer-link definitions:" >&2
  echo "$matches" >&2
  exit 1
fi

echo "No Blockscout runtime HTTP/RPC targets found."
