locals {
  api_image                         = "${aws_ecr_repository.api.repository_url}:latest"
  log_router_image                  = "${aws_ecr_repository.otel_log_router.repository_url}:latest"
  aa_runtime_image                  = local.api_image
  aa_api_runtime_image              = local.api_image
  aa_keeper_runtime_image           = local.api_image
  aa_observability_log_router_image = local.log_router_image
  alto_image                        = local.self_hosted_aa_resource_count == 1 ? "${aws_ecr_repository.alto[0].repository_url}:${local.alto_ecr_image_tag}" : ""
}
