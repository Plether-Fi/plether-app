resource "aws_ecr_repository" "api" {
  name                 = var.environment == "mainnet" ? "plether-api" : "plether-api-${local.deployment_name}"
  image_tag_mutability = "IMMUTABLE"
  force_delete         = false

  lifecycle {
    prevent_destroy = true
  }

  image_scanning_configuration {
    scan_on_push = true
  }
}

resource "aws_ecr_repository" "otel_log_router" {
  name                 = var.environment == "mainnet" ? "plether-otel-log-router" : "plether-otel-log-router-${local.deployment_name}"
  image_tag_mutability = "IMMUTABLE"
  force_delete         = false

  lifecycle {
    prevent_destroy = true
  }

  image_scanning_configuration {
    scan_on_push = true
  }
}

# Runtime repositories deliberately have no automatic image-expiration policy.
# Independently deployed services and rollback definitions can retain a digest
# across many releases. Neither immutable tags nor digest pins prevent ECR
# lifecycle deletion. Cleanup must prove the complete ECS/scheduler/rollback
# reference graph, including OCI index children, before proposing any deletion.
# The runtime image auditor is read-only; no garbage collector is enabled.

resource "aws_ecr_repository" "alto" {
  count = local.self_hosted_aa_resource_count

  name                 = "plether-alto-${local.deployment_name}"
  image_tag_mutability = "IMMUTABLE"
  force_delete         = false

  lifecycle {
    prevent_destroy = true
  }

  image_scanning_configuration {
    scan_on_push = true
  }

  encryption_configuration {
    encryption_type = "AES256"
  }
}

# Canary digests have separate immutable repositories without count-based
# expiration. Repeated live API or Alto builds cannot remove a pinned canary.
resource "aws_ecr_repository" "kernel_canary_api" {
  count = local.kernel_canary_repository_count

  name                 = "plether-kernel-api-sepolia"
  image_tag_mutability = "IMMUTABLE"
  force_delete         = false
  image_scanning_configuration { scan_on_push = true }
  encryption_configuration { encryption_type = "AES256" }
  depends_on = [terraform_data.kernel_canary_repository_guard]
}

resource "aws_ecr_repository" "kernel_canary_alto" {
  count = local.kernel_canary_repository_count

  name                 = "plether-kernel-alto-sepolia"
  image_tag_mutability = "IMMUTABLE"
  force_delete         = false
  image_scanning_configuration { scan_on_push = true }
  encryption_configuration { encryption_type = "AES256" }
  depends_on = [terraform_data.kernel_canary_repository_guard]
}
