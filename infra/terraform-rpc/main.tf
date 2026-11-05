# This state owns only the RPC pilot. Existing application infrastructure is
# read through data sources; no application credentials are inputs.
terraform {
  required_version = "~> 1.16.0"
  backend "s3" {}
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region              = var.aws_region
  allowed_account_ids = [var.expected_aws_account_id]
  default_tags {
    tags = { Project = "plether", Environment = var.environment, ManagedBy = "terraform", Stack = "rpc" }
  }
}

variable "aws_region" {
  type    = string
  default = "ap-southeast-1"
}

variable "expected_aws_account_id" {
  type    = string
  default = "932542905614"
}

variable "environment" {
  type    = string
  default = "sepolia"
}

variable "perps_chain_id" {
  type    = string
  default = "421614"
}

variable "operations_alarm_sns_topic_arn" {
  type = string
}

variable "existing_vpc_id" {
  type = string
}

variable "existing_public_subnet_ids" {
  type = list(string)
  validation {
    condition     = length(var.existing_public_subnet_ids) == 2 && length(distinct(var.existing_public_subnet_ids)) == 2
    error_message = "Select exactly two distinct existing public subnets."
  }
}

variable "existing_ecs_cluster_name" {
  type = string
}

variable "existing_ecs_security_group_id" {
  type = string
}

data "aws_caller_identity" "current" {}
data "aws_vpc" "existing" { id = var.existing_vpc_id }
data "aws_subnet" "existing" {
  count = length(var.existing_public_subnet_ids)
  id    = var.existing_public_subnet_ids[count.index]
}
data "aws_ecs_cluster" "existing" { cluster_name = var.existing_ecs_cluster_name }
data "aws_security_group" "existing_ecs" { id = var.existing_ecs_security_group_id }
data "aws_iam_openid_connect_provider" "existing" {
  arn = "arn:aws:iam::${var.expected_aws_account_id}:oidc-provider/token.actions.githubusercontent.com"
}

locals {
  deployment_name              = var.environment
  rpc_vpc_id                   = data.aws_vpc.existing.id
  rpc_public_subnets           = data.aws_subnet.existing
  rpc_ecs_cluster_id           = data.aws_ecs_cluster.existing.id
  rpc_ecs_security_group_id    = data.aws_security_group.existing_ecs.id
  rpc_github_oidc_provider_arn = data.aws_iam_openid_connect_provider.existing.arn
}

resource "terraform_data" "existing_context_guard" {
  count = local.rpc_count
  lifecycle {
    precondition {
      condition = (
        data.aws_vpc.existing.tags["Name"] == "plether-sepolia" &&
        alltrue([for subnet in data.aws_subnet.existing : subnet.vpc_id == local.rpc_vpc_id && subnet.map_public_ip_on_launch]) &&
        length(distinct(data.aws_subnet.existing[*].availability_zone)) == 2 &&
        data.aws_security_group.existing_ecs.vpc_id == local.rpc_vpc_id &&
        data.aws_ecs_cluster.existing.cluster_name == "plether-sepolia"
      )
      error_message = "RPC must reuse the existing Sepolia VPC, two public AZs, application security group and cluster."
    }
  }
}
