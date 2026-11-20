#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo 'Usage: build.sh <alto-v1.2.7-checkout> <local-image-tag> [--check]' >&2
  exit 2
}

[[ $# == 2 || $# == 3 ]] || usage
[[ $# != 3 || $3 == --check ]] || usage

source_repo=$(cd "$1" && pwd -P)
image_tag=$2
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
upstream_commit=e90957d8136d06087474d11cdcf4fa25bc120898

[[ $(git -C "$source_repo" rev-parse HEAD) == "$upstream_commit" ]] || {
  echo 'Alto checkout must be the reviewed v1.2.7 commit.' >&2
  exit 1
}

context=$(mktemp -d "${TMPDIR:-/tmp}/plether-alto-kernel.XXXXXX")
trap 'rm -rf "$context"' EXIT

for source_file in \
  src/cli/customTransport.ts \
  src/cli/deploySimulationsContract.ts \
  src/cli/handler.ts \
  src/rpc/validation/BundlerCollectorTracerV07.ts \
  src/rpc/validation/SafeValidator.ts; do
  mkdir -p "$context/$(dirname "$source_file")"
  git -C "$source_repo" show "$upstream_commit:$source_file" > "$context/$source_file"
done

git -C "$context" apply --check "$script_dir/alto-v1.2.7-v09-safemode.patch"
git -C "$context" apply "$script_dir/alto-v1.2.7-v09-safemode.patch"

if [[ ${3:-} == --check ]]; then
  shasum -a 256 \
    "$context/src/cli/customTransport.ts" \
    "$context/src/cli/deploySimulationsContract.ts" \
    "$context/src/cli/handler.ts" \
    "$context/src/rpc/validation/BundlerCollectorTracerV07.ts" \
    "$context/src/rpc/validation/SafeValidator.ts"
  exit 0
fi

docker build --platform linux/arm64 --file "$script_dir/Dockerfile" --tag "$image_tag" "$context"
[[ $(docker image inspect --format '{{.Os}}/{{.Architecture}}' "$image_tag") == linux/arm64 ]]
docker image inspect --format '{{.Id}}' "$image_tag"
