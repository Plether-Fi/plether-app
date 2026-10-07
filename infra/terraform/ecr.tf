resource "aws_ecr_repository" "api" {
  name                 = var.environment == "mainnet" ? "plether-api" : "plether-api-${local.deployment_name}"
  image_tag_mutability = "MUTABLE"
  force_delete         = true

  image_scanning_configuration {
    scan_on_push = true
  }
}

resource "aws_ecr_lifecycle_policy" "api" {
  repository = aws_ecr_repository.api.name

  policy = jsonencode({
    rules = [{
      rulePriority = 1
      description  = "Keep last 10 images"
      selection = {
        tagStatus   = "any"
        countType   = "imageCountMoreThan"
        countNumber = 10
      }
      action = { type = "expire" }
    }]
  })
}

resource "aws_ecr_repository" "otel_log_router" {
  name                 = var.environment == "mainnet" ? "plether-otel-log-router" : "plether-otel-log-router-${local.deployment_name}"
  image_tag_mutability = "MUTABLE"
  force_delete         = true

  image_scanning_configuration {
    scan_on_push = true
  }
}

# The API and independently deployed AA services share this repository. Alto
# pins a digest that can remain active across many backend releases. ECR's
# count/age policies do not check ECS references, so even an untagged-only rule
# could remove a required image. Retain these images until cleanup can verify
# every live task and retained rollback definition before deleting a digest.

resource "aws_ecr_repository" "alto" {
  count = local.self_hosted_aa_resource_count

  name                 = "plether-alto-${local.deployment_name}"
  image_tag_mutability = "IMMUTABLE"
  force_delete         = false

  image_scanning_configuration {
    scan_on_push = true
  }

  encryption_configuration {
    encryption_type = "AES256"
  }
}

resource "aws_ecr_lifecycle_policy" "alto" {
  count = local.self_hosted_aa_resource_count

  repository = aws_ecr_repository.alto[0].name

  policy = jsonencode({
    rules = [{
      rulePriority = 1
      description  = "Keep the last 10 qualified immutable Alto images"
      selection = {
        tagStatus   = "any"
        countType   = "imageCountMoreThan"
        countNumber = 10
      }
      action = { type = "expire" }
    }]
  })
}
