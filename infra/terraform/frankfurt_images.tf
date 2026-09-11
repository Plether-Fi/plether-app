# Exact candidate image identities read back after the 2026-09-10 build.
# Legacy deployments retain their existing image promotion semantics.
locals {
  # Approved preparation canary 8e68565: API only; workers/reconciler stay pinned.
  aa_api_runtime_image = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-api-sepolia-aa-temp@sha256:8f43121f73dae10928531e6b2a3cb2b8a7d63456008055019b73d345beafdb6e" : local.aa_runtime_image
  # Safe-head fix ab27261: only API/reconciler use this newer canary binary.
  aa_runtime_image = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-api-sepolia-aa-temp@sha256:3078940d5bc82cadadf884e9ec1057059920c24ac99f5ec1bd8ef6561c4173c8" : local.api_image
  api_image        = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-api-sepolia-aa-temp@sha256:4629cc418d52f183b5fdd21a2d4409124eaf7ea4e72229bd983c1602f67bb3be" : "${aws_ecr_repository.api.repository_url}:latest"
  log_router_image = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-otel-log-router-sepolia-aa-temp@sha256:f442ecc9aff9c3fc768ba76a200690c33c4cb0ffd49b9660b09ce7738ba213cc" : "${aws_ecr_repository.otel_log_router.repository_url}:latest"
  alto_image       = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-alto-sepolia-aa-temp@sha256:9db94fbd439a26f01b0ece3cc5f76b3791c1e1660b990d74892274267096f12a" : (local.self_hosted_aa_resource_count == 1 ? "${aws_ecr_repository.alto[0].repository_url}:${local.alto_ecr_image_tag}" : "")
}
