#!/usr/bin/env bash
set -euo pipefail

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repository_root"

scan_status=0
matches="$(git --no-pager grep --no-color -n -i -I -e 'blockscout[.]com' -- \
    apps/backend/app \
    apps/backend/src \
    apps/frontend/public \
    apps/frontend/src \
    infra/terraform)" || scan_status=$?
if [[ "$scan_status" -gt 1 ]]; then
  echo "Blockscout source scan failed (git grep exit $scan_status)." >&2
  exit "$scan_status"
fi

matches="$(printf '%s\n' "$matches" | awk '
  /^(apps\/frontend\/src\/stories\/|apps\/frontend\/src\/components\/__tests__\/)/ { next }
  {
    path = $0
    sub(/:[0-9]+:.*/, "", path)
    content = $0
    sub(/^[^:]+:[0-9]+:/, "", content)
    manifest = path == "apps/frontend/public/perps-aa-manifest.json" || path == "apps/frontend/src/contracts/__tests__/fixtures/perps-canonical-aa.json"
    if (manifest && content ~ /^[[:space:]]*"userOperationExplorerUrlTemplate"[[:space:]]*:[[:space:]]*"https:\/\/arbitrum-sepolia[.]blockscout[.]com\/op\/[{]userOperationHash[}]",?[[:space:]]*$/) next
    if (manifest && content ~ /^[[:space:]]*"transactionExplorerUrlTemplate"[[:space:]]*:[[:space:]]*"https:\/\/arbitrum-sepolia[.]blockscout[.]com\/tx\/[{]transactionHash[}]",?[[:space:]]*$/) next
    quote = sprintf("%c", 39)
    if (path == "apps/frontend/src/utils/explorer.ts" && content ~ ("^[[:space:]]*[?][[:space:]]*" quote "https://arbitrum-sepolia[.]blockscout[.]com" quote "[[:space:]]*$")) next
    print
  }
')"

if [[ -n "$matches" ]]; then
  echo "Blockscout may only appear in reviewed explorer-link definitions:" >&2
  echo "$matches" >&2
  exit 1
fi

echo "No Blockscout runtime HTTP/RPC targets found."
