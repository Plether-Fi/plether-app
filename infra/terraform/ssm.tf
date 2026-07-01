resource "aws_ssm_parameter" "rpc_url" {
  name  = "/plether/${var.environment}/rpc-url"
  type  = "SecureString"
  value = var.rpc_url
}

resource "aws_ssm_parameter" "pyth_api_key" {
  count = var.enable_pyth_api_key ? 1 : 0

  name  = "/plether/${var.environment}/pyth-api-key"
  type  = "SecureString"
  value = var.pyth_api_key
}

resource "aws_ssm_parameter" "perps_rpc_url" {
  name  = "/plether/${var.environment}/perps-rpc-url"
  type  = "SecureString"
  value = var.perps_rpc_url
}

resource "aws_ssm_parameter" "keeper_private_key" {
  name  = "/plether/${var.environment}/keeper-private-key"
  type  = "SecureString"
  value = var.keeper_private_key
}

resource "aws_ssm_parameter" "faucet_private_key" {
  count = var.faucet_private_key != "" ? 1 : 0

  name  = "/plether/${var.environment}/faucet-private-key"
  type  = "SecureString"
  value = var.faucet_private_key
}

resource "aws_ssm_parameter" "database_url" {
  name  = "/plether/${var.environment}/database-url"
  type  = "SecureString"
  value = "postgresql://${var.db_username}:${var.db_password}@${aws_db_instance.postgres.endpoint}/plether"
}
