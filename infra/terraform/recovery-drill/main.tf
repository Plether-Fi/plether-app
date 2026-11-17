terraform {
  required_version = "~> 1.16.0"
  backend "local" {}
  required_providers {
    aws = { source = "hashicorp/aws", version = "~> 5.0" }
  }
}

# Independent local state: never initialize this fixture with the application
# backend or import a production resource into this module.
provider "aws" {
  region              = var.aws_region
  allowed_account_ids = ["932542905614"]
  default_tags {
    tags = {
      Project = "plether", Environment = "sepolia", ManagedBy = "terraform"
      Purpose = "isolated-recovery-drill"
    }
  }
}

data "aws_caller_identity" "current" {}
data "aws_subnet" "fixture" {
  for_each = var.subnet_ids
  id       = each.value
}

locals {
  drill_profile       = jsondecode(file("${path.module}/../../../config/operations/backend-recovery-drill.json"))
  name                = local.drill_profile.cluster
  service_name        = local.drill_profile.service
  task_family         = local.drill_profile.family
  account_id          = "932542905614"
  registry            = "${local.account_id}.dkr.ecr.${var.aws_region}.amazonaws.com"
  application_image   = local.drill_profile.release.applicationImage
  router_image        = local.drill_profile.release.routerImage
  unavailable_image   = local.drill_profile.missingApplicationImage
  family_arn          = "arn:aws:ecs:${var.aws_region}:${local.account_id}:task-definition/${local.task_family}:*"
  fixture_service_arn = "arn:aws:ecs:${var.aws_region}:${local.account_id}:service/${local.name}/${local.service_name}"
  repository_arns     = [for name in ["plether-api-sepolia", "plether-otel-log-router-sepolia"] : "arn:aws:ecr:${var.aws_region}:${local.account_id}:repository/${name}"]
}

resource "terraform_data" "isolation_guard" {
  input = { cluster = local.name, service = local.service_name }
  lifecycle {
    precondition {
      condition     = var.aws_region == "ap-southeast-1" && data.aws_caller_identity.current.account_id == local.account_id && alltrue([for subnet in data.aws_subnet.fixture : subnet.vpc_id == var.vpc_id && subnet.map_public_ip_on_launch])
      error_message = "The isolated drill requires the reviewed Singapore account and public subnets in its existing VPC."
    }
    precondition {
      condition = (
        local.drill_profile.capability == "isolated-non-signing-recovery-drill" &&
        local.drill_profile.accountId == "932542905614" &&
        local.drill_profile.region == "ap-southeast-1" &&
        local.name == "plether-sepolia-recovery-drill" &&
        local.service_name == "plether-image-recovery-drill" &&
        local.task_family == "plether-sepolia-image-recovery-drill"
      )
      error_message = "The drill profile must retain its exact isolated capability, account, region, cluster, service and family."
    }
  }
}
