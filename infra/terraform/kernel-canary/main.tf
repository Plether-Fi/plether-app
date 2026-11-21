terraform {
  required_version = "~> 1.16.0"

  # Independent of both the live Sepolia application and Nitro RPC states.
  backend "s3" {}

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region              = "ap-southeast-1"
  allowed_account_ids = ["932542905614"]

  default_tags {
    tags = {
      Project     = "plether"
      Environment = "sepolia"
      ManagedBy   = "terraform"
      Deployment  = "kernel-canary"
    }
  }
}

variable "provision_image_build_resources" {
  type        = bool
  default     = false
  description = "Create the two immutable Kernel canary ECR repositories and their dedicated GitHub OIDC build role."
}

variable "provision_paymaster_key" {
  type        = bool
  default     = false
  description = "Create the separate Sepolia Kernel canary paymaster signing key before contract deployment."
}

data "aws_caller_identity" "current" {}

locals {
  expected_account_id = "932542905614"
  region              = "ap-southeast-1"
  image_count         = var.provision_image_build_resources ? 1 : 0
  key_count           = var.provision_paymaster_key ? 1 : 0
  api_task_role_arn   = "arn:aws:iam::${local.expected_account_id}:role/${local.kernel_canary_task_role_names.api}"
  image_build_role    = "plether-sepolia-kernel-canary-image-build"
}

resource "terraform_data" "sepolia_account_guard" {
  count = var.provision_image_build_resources || var.provision_paymaster_key ? 1 : 0

  lifecycle {
    precondition {
      condition     = data.aws_caller_identity.current.account_id == local.expected_account_id
      error_message = "The Kernel canary must use the reviewed Plether Sepolia AWS account."
    }
  }
}

data "aws_iam_openid_connect_provider" "github_actions" {
  count = local.image_count
  url   = "https://token.actions.githubusercontent.com"
}

resource "aws_ecr_repository" "api" {
  count                = local.image_count
  name                 = "plether-kernel-api-sepolia"
  image_tag_mutability = "IMMUTABLE"
  force_delete         = false

  image_scanning_configuration { scan_on_push = true }
  encryption_configuration { encryption_type = "AES256" }

  lifecycle { prevent_destroy = true }
  depends_on = [terraform_data.sepolia_account_guard]
}

resource "aws_ecr_repository" "alto" {
  count                = local.image_count
  name                 = "plether-kernel-alto-sepolia"
  image_tag_mutability = "IMMUTABLE"
  force_delete         = false

  image_scanning_configuration { scan_on_push = true }
  encryption_configuration { encryption_type = "AES256" }

  lifecycle { prevent_destroy = true }
  depends_on = [terraform_data.sepolia_account_guard]
}

resource "aws_iam_role" "image_build" {
  count                = local.image_count
  name                 = local.image_build_role
  description          = "Build and inspect immutable Sepolia Kernel canary API and Alto images only."
  max_session_duration = 3600

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Principal = {
        Federated = data.aws_iam_openid_connect_provider.github_actions[0].arn
      }
      Action = "sts:AssumeRoleWithWebIdentity"
      Condition = {
        StringEquals = {
          "token.actions.githubusercontent.com:aud" = "sts.amazonaws.com"
          "token.actions.githubusercontent.com:sub" = [
            "repo:Plether-Fi/plether-app:environment:backend-admin-sepolia",
            "repo:Plether-Fi/plether-app:environment:alto-admin-sepolia",
          ]
        }
      }
    }]
  })

  lifecycle { prevent_destroy = true }
  depends_on = [terraform_data.sepolia_account_guard]
}

resource "aws_iam_role_policy" "image_build" {
  count = local.image_count
  name  = "exact-kernel-canary-ecr-build-and-scan"
  role  = aws_iam_role.image_build[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid      = "EcrLogin"
        Effect   = "Allow"
        Action   = "ecr:GetAuthorizationToken"
        Resource = "*"
      },
      {
        Sid    = "PushAndInspectOnlyCanaryRepositories"
        Effect = "Allow"
        Action = [
          "ecr:BatchCheckLayerAvailability",
          "ecr:BatchGetImage",
          "ecr:CompleteLayerUpload",
          "ecr:DescribeImages",
          "ecr:DescribeImageScanFindings",
          "ecr:DescribeRepositories",
          "ecr:GetDownloadUrlForLayer",
          "ecr:InitiateLayerUpload",
          "ecr:PutImage",
          "ecr:StartImageScan",
          "ecr:UploadLayerPart",
        ]
        Resource = [aws_ecr_repository.api[0].arn, aws_ecr_repository.alto[0].arn]
      },
    ]
  })
}

# The signing key is created before its future ECS API task role exists. The
# key policy grants signing only to that exact role ARN, and that role must
# separately receive an IAM allow in the canary service phase.
resource "aws_kms_key" "paymaster" {
  count = local.key_count

  description                        = "Plether Sepolia Kernel canary ERC-4337 paymaster signer"
  key_usage                          = "SIGN_VERIFY"
  customer_master_key_spec           = "ECC_SECG_P256K1"
  deletion_window_in_days            = 30
  multi_region                       = false
  bypass_policy_lockout_safety_check = false

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid       = "AllowAccountKeyAdministration"
        Effect    = "Allow"
        Principal = { AWS = "arn:aws:iam::${local.expected_account_id}:root" }
        Action = [
          "kms:CancelKeyDeletion", "kms:CreateAlias",
          "kms:DeleteAlias", "kms:DescribeKey", "kms:DisableKey",
          "kms:DisableKeyRotation", "kms:EnableKey", "kms:EnableKeyRotation",
          "kms:GetKeyPolicy", "kms:GetKeyRotationStatus", "kms:GetPublicKey", "kms:List*",
          "kms:PutKeyPolicy", "kms:ScheduleKeyDeletion",
          "kms:TagResource", "kms:UntagResource", "kms:UpdateAlias",
          "kms:UpdateKeyDescription",
        ]
        Resource = "*"
      },
      {
        Sid       = "AllowExactCanaryApiKeyMetadata"
        Effect    = "Allow"
        Principal = { AWS = "arn:aws:iam::${local.expected_account_id}:root" }
        Action    = ["kms:DescribeKey", "kms:GetPublicKey"]
        Resource  = "*"
        Condition = { ArnEquals = { "aws:PrincipalArn" = local.api_task_role_arn } }
      },
      {
        Sid       = "AllowExactCanaryApiDigestSigning"
        Effect    = "Allow"
        Principal = { AWS = "arn:aws:iam::${local.expected_account_id}:root" }
        Action    = "kms:Sign"
        Resource  = "*"
        Condition = {
          ArnEquals    = { "aws:PrincipalArn" = local.api_task_role_arn }
          StringEquals = { "kms:SigningAlgorithm" = "ECDSA_SHA_256" }
        }
      },
    ]
  })

  tags = { Name = "plether-sepolia-kernel-canary-paymaster", Purpose = "kernel-canary-paymaster-signing" }
  lifecycle { prevent_destroy = true }
  depends_on = [terraform_data.sepolia_account_guard]
}

resource "aws_kms_alias" "paymaster" {
  count         = local.key_count
  name          = "alias/plether-sepolia-kernel-canary-paymaster-signer"
  target_key_id = aws_kms_key.paymaster[0].key_id
}
