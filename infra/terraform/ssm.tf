resource "aws_ssm_parameter" "rpc_url" {
  name  = "/plether/${var.environment}/rpc-url"
  type  = "SecureString"
  value = var.rpc_url
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

resource "aws_ssm_parameter" "database_url" {
  name  = "/plether/${var.environment}/database-url"
  type  = "SecureString"
  value = "postgresql://${var.db_username}:${var.db_password}@${aws_db_instance.postgres.endpoint}/plether"
}
