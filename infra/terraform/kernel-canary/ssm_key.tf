# The canary executor, utility, preview origin, and dedicated deployer
# SecureStrings use this symmetric key. The paymaster signing key is separate.
variable "provision_ssm_encryption_key" {
  type        = bool
  default     = false
  description = "Create a dedicated symmetric KMS key for the Kernel canary SSM SecureStrings."
}

locals {
  kernel_canary_iam_prefix  = "arn:aws:iam::${local.expected_account_id}:"
  kernel_canary_ssm_service = "ssm.${local.region}.amazonaws.com"
  kernel_canary_ssm_operator_parameters = concat(
    local.kernel_canary_customer_secret_arns.alto,
    local.kernel_canary_customer_secret_arns.api,
    ["${local.kernel_canary_ssm_prefix}/plether/sepolia/kernel-canary-deployer-private-key"],
  )
  kernel_canary_nonoperator_role_arns = [
    for role_name in concat(
      values(local.kernel_canary_execution_role_names),
      values(local.kernel_canary_task_role_names),
      [local.image_build_role],
    ) : "${local.kernel_canary_iam_prefix}role/${role_name}"
  ]
}

resource "terraform_data" "ssm_key_account_guard" {
  count = var.provision_ssm_encryption_key ? 1 : 0

  lifecycle {
    precondition {
      condition     = data.aws_caller_identity.current.account_id == local.expected_account_id
      error_message = "The Kernel canary SSM key may be provisioned only in the reviewed Plether Sepolia account."
    }
  }
}

resource "aws_kms_key" "kernel_canary_ssm" {
  count = var.provision_ssm_encryption_key ? 1 : 0

  description                        = "Plether Sepolia Kernel canary SSM SecureString encryption"
  key_usage                          = "ENCRYPT_DECRYPT"
  customer_master_key_spec           = "SYMMETRIC_DEFAULT"
  enable_key_rotation                = true
  deletion_window_in_days            = 30
  multi_region                       = false
  bypass_policy_lockout_safety_check = false

  # Delegate narrowly to authorized IAM identities. The account principal and
  # role ARN conditions work before ECS roles exist, without referencing a role
  # resource that depends on this key for its execution policy.
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = concat([
      {
        Sid       = "AllowAccountKeyManagementAndRecovery"
        Effect    = "Allow"
        Principal = { AWS = "${local.kernel_canary_iam_prefix}root" }
        Action = [
          "kms:CancelKeyDeletion",
          "kms:CreateAlias",
          "kms:DeleteAlias",
          "kms:DescribeKey",
          "kms:DisableKey",
          "kms:DisableKeyRotation",
          "kms:EnableKey",
          "kms:EnableKeyRotation",
          "kms:GetKeyPolicy",
          "kms:GetKeyRotationStatus",
          "kms:ListGrants",
          "kms:ListResourceTags",
          "kms:PutKeyPolicy",
          "kms:RevokeGrant",
          "kms:TagResource",
          "kms:UntagResource",
          "kms:UpdateAlias",
          "kms:UpdateKeyDescription",
        ]
        Resource = "*"
      },
      {
        Sid       = "AllowOperatorCanarySecureStringProvisioningAndRecovery"
        Effect    = "Allow"
        Principal = { AWS = "${local.kernel_canary_iam_prefix}root" }
        Action    = ["kms:Decrypt", "kms:Encrypt", "kms:GenerateDataKey"]
        Resource  = "*"
        Condition = {
          ArnNotEquals = { "aws:PrincipalArn" = local.kernel_canary_nonoperator_role_arns }
          StringEquals = {
            "kms:ViaService"                      = local.kernel_canary_ssm_service
            "kms:EncryptionContext:PARAMETER_ARN" = local.kernel_canary_ssm_operator_parameters
          }
        }
      },
      ], [
      for service in ["alto", "api"] : {
        Sid       = "Allow${title(service)}ExecutionRoleSsmDecrypt"
        Effect    = "Allow"
        Principal = { AWS = "${local.kernel_canary_iam_prefix}root" }
        Action    = ["kms:Decrypt"]
        Resource  = "*"
        Condition = {
          ArnEquals = { "aws:PrincipalArn" = "${local.kernel_canary_iam_prefix}role/${local.kernel_canary_execution_role_names[service]}" }
          StringEquals = {
            "kms:ViaService"                      = local.kernel_canary_ssm_service
            "kms:EncryptionContext:PARAMETER_ARN" = local.kernel_canary_customer_secret_arns[service]
          }
        }
      }
    ])
  })

  tags = {
    Name    = "plether-sepolia-kernel-canary-ssm"
    Purpose = "kernel-canary-ssm-encryption"
  }

  lifecycle { prevent_destroy = true }
  depends_on = [terraform_data.ssm_key_account_guard]
}

resource "aws_kms_alias" "kernel_canary_ssm" {
  count         = var.provision_ssm_encryption_key ? 1 : 0
  name          = "alias/plether-sepolia-kernel-canary-ssm"
  target_key_id = aws_kms_key.kernel_canary_ssm[0].key_id
}
