# Exact candidate image identities read back after the 2026-09-10 build.
# Legacy deployments retain their existing image promotion semantics.
locals {
  # Approved reliability API 205d747; idle-funding keeper follow-up f94d781.
  aa_api_runtime_image              = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-api-sepolia-aa-temp@sha256:631c89b2b6e9b34800ffa05e0013bfba211402cfddeae60c2fb37452ae110500" : local.aa_runtime_image
  aa_keeper_runtime_image           = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-api-sepolia-aa-temp@sha256:9a825fddd0e8b942cc68b83349ca25a21b5b37dde8c586ec2d1ae11258859a79" : local.api_image
  aa_observability_log_router_image = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-otel-log-router-sepolia-aa-temp@sha256:de9936038b75c0a1492478fa4355fe2ee50cd827306054a68d9fe8283b01f66e" : local.log_router_image
  # Safe-head fix ab27261: only API/reconciler use this newer canary binary.
  aa_runtime_image = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-api-sepolia-aa-temp@sha256:3078940d5bc82cadadf884e9ec1057059920c24ac99f5ec1bd8ef6561c4173c8" : local.api_image
  api_image        = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-api-sepolia-aa-temp@sha256:4629cc418d52f183b5fdd21a2d4409124eaf7ea4e72229bd983c1602f67bb3be" : "${aws_ecr_repository.api.repository_url}:latest"
  log_router_image = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-otel-log-router-sepolia-aa-temp@sha256:f442ecc9aff9c3fc768ba76a200690c33c4cb0ffd49b9660b09ce7738ba213cc" : "${aws_ecr_repository.otel_log_router.repository_url}:latest"
  alto_image       = local.frankfurt_preparation ? "932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-alto-sepolia-aa-temp@sha256:9db94fbd439a26f01b0ece3cc5f76b3791c1e1660b990d74892274267096f12a" : (local.self_hosted_aa_resource_count == 1 ? "${aws_ecr_repository.alto[0].repository_url}:${local.alto_ecr_image_tag}" : "")
}
