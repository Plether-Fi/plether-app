resource "aws_ssm_parameter" "rpc_url" {
  name  = "/plether/${var.environment}/rpc-url"
  type  = "SecureString"
  value = var.rpc_url
}

resource "aws_ssm_parameter" "pyth_api_key" {
  count = var.enable_pyth_api_key && local.effective_pyth_api_key_ssm_parameter_name == "" ? 1 : 0

  name  = "/plether/${var.environment}/pyth-api-key"
  type  = "SecureString"
  value = var.pyth_api_key

  lifecycle {
    prevent_destroy = true

    precondition {
      condition     = trimspace(var.pyth_api_key) != ""
      error_message = "enable_pyth_api_key=true requires a non-empty pyth_api_key with access to every configured basket feed, including FX feeds."
    }
  }
}

resource "aws_ssm_parameter" "perps_rpc_url" {
  name  = "/plether/${var.environment}/perps-rpc-url"
  type  = "SecureString"
  value = var.perps_rpc_url
}

resource "aws_ssm_parameter" "vault_history_rpc_url" {
  count = trimspace(var.vault_history_rpc_url) != "" ? 1 : 0

  name  = "/plether/${var.environment}/vault-history-rpc-url"
  type  = "SecureString"
  value = var.vault_history_rpc_url
}

resource "aws_ssm_parameter" "keeper_private_key" {
  name  = "/plether/${var.environment}/keeper-private-key"
  type  = "SecureString"
  value = var.keeper_private_key
}

resource "aws_ssm_parameter" "oracle_updater_private_key" {
  name  = "/plether/${var.environment}/oracle-updater-private-key"
  type  = "SecureString"
  value = var.oracle_updater_private_key

  lifecycle {
    precondition {
      condition = (
        var.keeper_private_key != var.liquidation_keeper_private_key
        && var.oracle_updater_private_key != var.keeper_private_key
        && var.oracle_updater_private_key != var.liquidation_keeper_private_key
      )
      error_message = "keeper_private_key, oracle_updater_private_key, and liquidation_keeper_private_key must all be different."
    }
  }
}

resource "aws_ssm_parameter" "liquidation_keeper_private_key" {
  name  = "/plether/${var.environment}/liquidation-keeper-private-key"
  type  = "SecureString"
  value = var.liquidation_keeper_private_key
}

resource "aws_ssm_parameter" "faucet_private_key" {
  count = var.faucet_private_key != "" ? 1 : 0

  name  = "/plether/${var.environment}/faucet-private-key"
  type  = "SecureString"
  value = var.faucet_private_key
}

resource "aws_ssm_parameter" "pimlico_api_key" {
  count = var.provision_aa_proxy ? 1 : 0

  name  = "/plether/${var.environment}/pimlico-api-key"
  type  = "SecureString"
  value = var.pimlico_api_key
}

resource "aws_ssm_parameter" "pimlico_sponsorship_policy_id" {
  count = var.provision_aa_proxy ? 1 : 0

  name  = "/plether/${var.environment}/pimlico-sponsorship-policy-id"
  type  = "SecureString"
  value = var.pimlico_sponsorship_policy_id
}

resource "aws_ssm_parameter" "aa_proxy_origin_token" {
  count = var.provision_aa_proxy ? 1 : 0

  name  = "/plether/${var.environment}/aa-proxy-origin-token"
  type  = "SecureString"
  value = var.aa_proxy_origin_token
}

resource "aws_ssm_parameter" "database_url" {
  name  = "/plether/${var.environment}/database-url"
  type  = "SecureString"
  value = "postgresql://${var.db_username}:${var.db_password}@${aws_db_instance.postgres.endpoint}/plether"
}

resource "aws_ssm_parameter" "posthog_otlp_authorization_header" {
  name  = "/plether/${var.environment}/posthog-otlp-authorization-header"
  type  = "SecureString"
  value = "Authorization Bearer ${var.posthog_project_token}"
}
