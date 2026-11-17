variable "api_image_digest" {
  type        = string
  default     = ""
  description = "Qualified immutable application image digest. Build and verify the release before starting any application service."
  validation {
    condition     = var.api_image_digest == "" || can(regex("^sha256:[0-9a-f]{64}$", var.api_image_digest))
    error_message = "api_image_digest must be empty for stopped bootstrap only, or a SHA-256 digest."
  }
}

variable "log_router_image_digest" {
  type        = string
  default     = ""
  description = "Qualified immutable FireLens image digest, retained alongside the application release."
  validation {
    condition     = var.log_router_image_digest == "" || can(regex("^sha256:[0-9a-f]{64}$", var.log_router_image_digest))
    error_message = "log_router_image_digest must be empty for stopped bootstrap only, or a SHA-256 digest."
  }
}

locals {
  # A deliberately nonexistent bootstrap tag may appear only in stopped task
  # definitions. No running service may inherit a moving :latest dependency.
  api_image                         = var.api_image_digest == "" ? "${aws_ecr_repository.api.repository_url}:bootstrap-unqualified" : "${aws_ecr_repository.api.repository_url}@${var.api_image_digest}"
  log_router_image                  = var.log_router_image_digest == "" ? "${aws_ecr_repository.otel_log_router.repository_url}:bootstrap-unqualified" : "${aws_ecr_repository.otel_log_router.repository_url}@${var.log_router_image_digest}"
  aa_runtime_image                  = local.api_image
  aa_api_runtime_image              = local.api_image
  aa_keeper_runtime_image           = local.api_image
  aa_observability_log_router_image = local.log_router_image
  alto_image                        = local.self_hosted_aa_resource_count == 1 ? (local.alto_rpc_auth_enabled ? var.alto_rpc_auth_image : "${aws_ecr_repository.alto[0].repository_url}:${local.alto_ecr_image_tag}") : ""
}

resource "terraform_data" "runtime_image_guard" {
  input = { application = var.api_image_digest, log_router = var.log_router_image_digest }

  lifecycle {
    precondition {
      condition = (
        var.api_desired_count == 0 && var.workers_desired_count == 0 && var.consolidate_workers &&
        var.liquidation_worker_desired_count == 0 && var.protection_worker_desired_count == 0 &&
        var.aa_reconciler_desired_count == 0 && var.alto_desired_count == 0
      ) || (var.api_image_digest != "" && var.log_router_image_digest != "")
      error_message = "Running services require qualified api_image_digest and log_router_image_digest. Empty digests are allowed only for a completely stopped bootstrap stack."
    }
  }
}
