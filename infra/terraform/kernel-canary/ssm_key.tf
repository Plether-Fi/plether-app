# The executor, utility, and preview origin SecureStrings use this symmetric
# customer-managed key. The paymaster signing key is separate and asymmetric.
variable "provision_ssm_encryption_key" {
  type        = bool
  default     = false
  description = "Create a dedicated symmetric KMS key for the Kernel canary SSM SecureStrings."
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

  # The account root principal delegates use to IAM. Only the named canary
  # execution roles receive scoped Decrypt grants in the service phase.
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Sid       = "EnableAccountIamPolicyAdministration"
      Effect    = "Allow"
      Principal = { AWS = "arn:aws:iam::${local.expected_account_id}:root" }
      Action    = "kms:*"
      Resource  = "*"
    }]
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
