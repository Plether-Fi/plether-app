resource "aws_kms_key" "aa_paymaster_signer" {
  count = local.self_hosted_aa_resource_count

  description                        = "Plether ${var.environment} ERC-4337 verifying-paymaster signer"
  key_usage                          = "SIGN_VERIFY"
  customer_master_key_spec           = "ECC_SECG_P256K1"
  deletion_window_in_days            = 30
  multi_region                       = false
  bypass_policy_lockout_safety_check = false

  tags = {
    Name    = "plether-${var.environment}-aa-paymaster-signer"
    Purpose = "erc4337-paymaster-signing"
  }

  lifecycle {
    prevent_destroy = true
  }
}

resource "aws_kms_alias" "aa_paymaster_signer" {
  count = local.self_hosted_aa_resource_count

  name          = "alias/plether-${var.environment}-aa-paymaster-signer"
  target_key_id = aws_kms_key.aa_paymaster_signer[0].key_id
}
