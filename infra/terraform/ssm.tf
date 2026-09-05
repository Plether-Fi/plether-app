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
  # Retain the legacy SecureString through the first Alchemy-only soak without
  # keeping a second provider URL in Terraform's active configuration surface.
  count = var.environment == "sepolia" ? 1 : 0

  name  = "/plether/${var.environment}/vault-history-rpc-url"
  type  = "SecureString"
  value = var.perps_rpc_url

  lifecycle {
    ignore_changes = [value]
  }
}

resource "aws_ssm_parameter" "keeper_private_key" {
  name  = "/plether/${var.environment}/keeper-private-key"
  type  = "SecureString"
  value = var.keeper_private_key
}

locals {
  zero_private_key = "0000000000000000000000000000000000000000000000000000000000000000"

  normalized_transaction_private_keys = {
    keeper        = trimprefix(lower(trimspace(var.keeper_private_key)), "0x")
    lp_settlement = trimprefix(lower(trimspace(var.lp_settlement_private_key)), "0x")
    oracle        = trimprefix(lower(trimspace(var.oracle_updater_private_key)), "0x")
    liquidation   = trimprefix(lower(trimspace(var.liquidation_keeper_private_key)), "0x")
  }
}

resource "aws_ssm_parameter" "lp_settlement_private_key" {
  count = nonsensitive(var.lp_settlement_private_key != "") ? 1 : 0

  name  = "/plether/${var.environment}/lp-settlement-private-key"
  type  = "SecureString"
  value = var.lp_settlement_private_key

  lifecycle {
    precondition {
      condition = (
        local.normalized_transaction_private_keys.lp_settlement != local.normalized_transaction_private_keys.keeper
        && local.normalized_transaction_private_keys.lp_settlement != local.normalized_transaction_private_keys.oracle
        && local.normalized_transaction_private_keys.lp_settlement != local.normalized_transaction_private_keys.liquidation
      )
      error_message = "lp_settlement_private_key must be different from keeper_private_key, oracle_updater_private_key, and liquidation_keeper_private_key."
    }
  }
}

resource "aws_ssm_parameter" "oracle_updater_private_key" {
  name  = "/plether/${var.environment}/oracle-updater-private-key"
  type  = "SecureString"
  value = var.oracle_updater_private_key

  lifecycle {
    precondition {
      condition = (
        local.normalized_transaction_private_keys.keeper != local.zero_private_key
        && local.normalized_transaction_private_keys.oracle != local.zero_private_key
        && local.normalized_transaction_private_keys.liquidation != local.zero_private_key
        && (
          local.normalized_transaction_private_keys.lp_settlement == ""
          || local.normalized_transaction_private_keys.lp_settlement != local.zero_private_key
        )
      )
      error_message = "Transaction signer private keys must not be the all-zero scalar."
    }

    precondition {
      condition = (
        local.normalized_transaction_private_keys.keeper != local.normalized_transaction_private_keys.liquidation
        && local.normalized_transaction_private_keys.oracle != local.normalized_transaction_private_keys.keeper
        && local.normalized_transaction_private_keys.oracle != local.normalized_transaction_private_keys.liquidation
        && (
          var.lp_settlement_private_key == ""
          || (
            local.normalized_transaction_private_keys.lp_settlement != local.normalized_transaction_private_keys.keeper
            && local.normalized_transaction_private_keys.lp_settlement != local.normalized_transaction_private_keys.oracle
            && local.normalized_transaction_private_keys.lp_settlement != local.normalized_transaction_private_keys.liquidation
          )
        )
      )
      error_message = "keeper_private_key, lp_settlement_private_key (when configured), oracle_updater_private_key, and liquidation_keeper_private_key must all be different."
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

resource "aws_ssm_parameter" "faucet_proxy_origin_token" {
  count = var.faucet_private_key != "" ? 1 : 0

  name  = "/plether/${var.environment}/faucet-proxy-origin-token"
  type  = "SecureString"
  value = var.faucet_proxy_origin_token
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
  count = local.aa_gateway_enabled ? 1 : 0

  name  = "/plether/${var.environment}/aa-proxy-origin-token"
  type  = "SecureString"
  value = var.aa_proxy_origin_token
}

resource "aws_ssm_parameter" "insights_registration_origin_token" {
  count = var.provision_insights_registration ? 1 : 0

  name  = "/plether/${var.environment}/insights-registration-origin-token"
  type  = "SecureString"
  value = var.insights_registration_origin_token
}

resource "aws_ssm_parameter" "insights_registration_origin_token_next" {
  # Only the presence bit is declassified so it can safely drive resource
  # cardinality; the token value remains sensitive in state and plan output.
  count = var.provision_insights_registration && nonsensitive(var.insights_registration_origin_token_next != "") ? 1 : 0

  name  = "/plether/${var.environment}/insights-registration-origin-token-next"
  type  = "SecureString"
  value = var.insights_registration_origin_token_next
}

resource "aws_ssm_parameter" "turnstile_secret_key" {
  count = var.provision_insights_registration ? 1 : 0

  name  = "/plether/${var.environment}/turnstile-secret-key"
  type  = "SecureString"
  value = var.turnstile_secret_key
}

resource "aws_ssm_parameter" "x_oauth_client_secret" {
  count = var.provision_insights_registration ? 1 : 0

  name  = "/plether/${var.environment}/x-oauth-client-secret"
  type  = "SecureString"
  value = var.x_oauth_client_secret
}

resource "aws_ssm_parameter" "insights_registration_email_keys" {
  count = var.provision_insights_registration ? 1 : 0

  name  = "/plether/${var.environment}/insights-registration-email-keys"
  type  = "SecureString"
  value = jsonencode(var.insights_registration_email_keys)

  lifecycle {
    prevent_destroy = true
  }
}

resource "aws_ssm_parameter" "insights_registration_email_hmac_key" {
  count = var.provision_insights_registration ? 1 : 0

  name  = "/plether/${var.environment}/insights-registration-email-hmac-key"
  type  = "SecureString"
  value = var.insights_registration_email_hmac_key_base64

  lifecycle {
    prevent_destroy = true
  }
}

resource "aws_ssm_parameter" "database_url" {
  name  = "/plether/${var.environment}/database-url"
  type  = "SecureString"
  value = "postgresql://${urlencode(var.db_username)}:${urlencode(var.db_password)}@${aws_db_instance.postgres.endpoint}/plether?sslmode=verify-full&sslrootcert=${urlencode(var.db_ssl_root_cert_path)}"
}

resource "aws_ssm_parameter" "posthog_otlp_authorization_header" {
  name  = "/plether/${var.environment}/posthog-otlp-authorization-header"
  type  = "SecureString"
  value = "Authorization Bearer ${var.posthog_project_token}"
}
