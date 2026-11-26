resource "aws_ssm_parameter" "rpc_url" {
  name  = "/plether/${local.deployment_name}/rpc-url"
  type  = "SecureString"
  value = var.rpc_url
}

resource "aws_ssm_parameter" "pyth_api_key" {
  count = var.enable_pyth_api_key && local.effective_pyth_api_key_ssm_parameter_name == "" ? 1 : 0

  name  = "/plether/${local.deployment_name}/pyth-api-key"
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
  name  = "/plether/${local.deployment_name}/perps-rpc-url"
  type  = "SecureString"
  value = var.perps_rpc_url
}

resource "aws_ssm_parameter" "vault_history_rpc_url" {
  # Retain the legacy SecureString through the first Alchemy-only soak without
  # keeping a second provider URL in Terraform's active configuration surface.
  count = var.environment == "sepolia" ? 1 : 0

  name  = "/plether/${local.deployment_name}/vault-history-rpc-url"
  type  = "SecureString"
  value = var.perps_rpc_url

  lifecycle {
    ignore_changes = [value]
  }
}

moved {
  from = aws_ssm_parameter.keeper_private_key
  to   = aws_ssm_parameter.keeper_private_key[0]
}

moved {
  from = aws_ssm_parameter.oracle_updater_private_key
  to   = aws_ssm_parameter.oracle_updater_private_key[0]
}

moved {
  from = aws_ssm_parameter.liquidation_keeper_private_key
  to   = aws_ssm_parameter.liquidation_keeper_private_key[0]
}

moved {
  from = aws_ssm_parameter.database_url
  to   = aws_ssm_parameter.database_url[0]
}

moved {
  from = aws_ssm_parameter.posthog_otlp_authorization_header
  to   = aws_ssm_parameter.posthog_otlp_authorization_header[0]
}

locals {
  zero_private_key = "0000000000000000000000000000000000000000000000000000000000000000"

  normalized_transaction_private_keys = {
    faucet        = trimprefix(lower(trimspace(var.faucet_private_key)), "0x")
    keeper        = trimprefix(lower(trimspace(var.keeper_private_key)), "0x")
    lp_settlement = trimprefix(lower(trimspace(var.lp_settlement_private_key)), "0x")
    oracle        = trimprefix(lower(trimspace(var.oracle_updater_private_key)), "0x")
    liquidation   = trimprefix(lower(trimspace(var.liquidation_keeper_private_key)), "0x")
  }

  keeper_private_key_external        = trimspace(var.keeper_private_key_ssm_parameter_name) != ""
  lp_settlement_private_key_external = trimspace(var.lp_settlement_private_key_ssm_parameter_name) != ""
  oracle_updater_external            = trimspace(var.oracle_updater_private_key_ssm_parameter_name) != ""
  liquidation_keeper_external        = trimspace(var.liquidation_keeper_private_key_ssm_parameter_name) != ""
  protection_worker_key_external     = trimspace(var.protection_worker_private_key_ssm_parameter_name) != ""
  database_url_external              = trimspace(var.database_url_ssm_parameter_name) != ""
  posthog_auth_header_external       = trimspace(var.posthog_otlp_authorization_header_ssm_parameter_name) != ""
  aa_proxy_origin_token_external     = trimspace(var.aa_proxy_origin_token_ssm_parameter_name) != ""

  keeper_private_key_parameter_arn = (
    local.keeper_private_key_external
    ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.keeper_private_key_ssm_parameter_name)}"
    : aws_ssm_parameter.keeper_private_key[0].arn
  )
  lp_settlement_private_key_configured = local.lp_settlement_private_key_external || nonsensitive(var.lp_settlement_private_key != "")
  lp_settlement_private_key_parameter_arn = (
    local.lp_settlement_private_key_external
    ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.lp_settlement_private_key_ssm_parameter_name)}"
    : (nonsensitive(var.lp_settlement_private_key != "") ? aws_ssm_parameter.lp_settlement_private_key[0].arn : null)
  )
  oracle_updater_private_key_parameter_arn = (
    local.oracle_updater_external
    ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.oracle_updater_private_key_ssm_parameter_name)}"
    : aws_ssm_parameter.oracle_updater_private_key[0].arn
  )
  liquidation_keeper_private_key_parameter_arn = (
    local.liquidation_keeper_external
    ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.liquidation_keeper_private_key_ssm_parameter_name)}"
    : aws_ssm_parameter.liquidation_keeper_private_key[0].arn
  )
  protection_worker_private_key_configured = local.protection_worker_key_external || nonsensitive(var.protection_worker_private_key != "")
  protection_worker_private_key_parameter_arn = (
    local.protection_worker_key_external
    ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.protection_worker_private_key_ssm_parameter_name)}"
    : (nonsensitive(var.protection_worker_private_key != "") ? aws_ssm_parameter.protection_worker_private_key[0].arn : null)
  )
  database_url_parameter_arn = (
    local.database_url_external
    ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.database_url_ssm_parameter_name)}"
    : aws_ssm_parameter.database_url[0].arn
  )
  posthog_otlp_authorization_header_parameter_arn = (
    local.posthog_auth_header_external
    ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.posthog_otlp_authorization_header_ssm_parameter_name)}"
    : aws_ssm_parameter.posthog_otlp_authorization_header[0].arn
  )
  aa_proxy_origin_token_parameter_name = (
    local.aa_proxy_origin_token_external
    ? trimspace(var.aa_proxy_origin_token_ssm_parameter_name)
    : (local.aa_gateway_enabled ? aws_ssm_parameter.aa_proxy_origin_token[0].name : null)
  )
  aa_proxy_origin_token_parameter_arn = (
    local.aa_proxy_origin_token_external
    ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.aa_proxy_origin_token_ssm_parameter_name)}"
    : (local.aa_gateway_enabled ? aws_ssm_parameter.aa_proxy_origin_token[0].arn : null)
  )

  external_signer_ssm_parameter_names = compact([
    trimspace(var.keeper_private_key_ssm_parameter_name),
    trimspace(var.lp_settlement_private_key_ssm_parameter_name),
    trimspace(var.oracle_updater_private_key_ssm_parameter_name),
    trimspace(var.liquidation_keeper_private_key_ssm_parameter_name),
    trimspace(var.protection_worker_private_key_ssm_parameter_name),
  ])

  all_external_secret_ssm_parameter_names = compact(concat(
    local.external_signer_ssm_parameter_names,
    [
      trimspace(var.database_url_ssm_parameter_name),
      trimspace(var.posthog_otlp_authorization_header_ssm_parameter_name),
      trimspace(var.aa_proxy_origin_token_ssm_parameter_name),
    ]
  ))
}

resource "terraform_data" "secrets_boundary_guard" {
  input = {
    environment                    = var.environment
    external_secret_parameters     = local.all_external_secret_ssm_parameter_names
    db_manage_master_user_password = var.db_manage_master_user_password
    testnet_margin_funding_enabled = var.testnet_margin_funding_enabled
    faucet_configured              = nonsensitive(var.faucet_private_key != "" || var.faucet_proxy_origin_token != "")
  }

  lifecycle {
    precondition {
      condition = alltrue([
        for parameter_name in local.all_external_secret_ssm_parameter_names :
        startswith(parameter_name, "/plether/${local.deployment_name}/")
      ])
      error_message = "Every external SSM secret parameter name must belong to the current /plether/<environment>/ namespace."
    }

    precondition {
      condition     = length(distinct(local.external_signer_ssm_parameter_names)) == length(local.external_signer_ssm_parameter_names)
      error_message = "External transaction signer SSM parameter names must all be pairwise distinct."
    }

    precondition {
      condition = (
        (local.keeper_private_key_external || nonsensitive(trimspace(var.keeper_private_key) != ""))
        && (local.oracle_updater_external || nonsensitive(trimspace(var.oracle_updater_private_key) != ""))
        && (local.liquidation_keeper_external || nonsensitive(trimspace(var.liquidation_keeper_private_key) != ""))
        && (local.database_url_external || nonsensitive(trimspace(var.db_password) != ""))
        && (local.posthog_auth_header_external || nonsensitive(trimspace(var.posthog_project_token) != ""))
      )
      error_message = "Keeper, oracle, liquidation, database, and PostHog credentials must be provided either via external *_ssm_parameter_name references or (on Sepolia) inline variables."
    }

    precondition {
      condition = var.environment != "mainnet" || (
        local.keeper_private_key_external
        && local.oracle_updater_external
        && local.liquidation_keeper_external
        && local.database_url_external
        && local.posthog_auth_header_external
        && (var.lp_settlement_mode == "off" || local.lp_settlement_private_key_external)
        && (!var.protection_worker_execution_enabled || local.protection_worker_key_external)
        && (!local.aa_gateway_enabled || local.aa_proxy_origin_token_external)
        && (!var.enable_pyth_api_key || local.external_pyth_api_key_parameter_arn != null)
        && nonsensitive(trimspace(var.keeper_private_key) == "")
        && nonsensitive(trimspace(var.lp_settlement_private_key) == "")
        && nonsensitive(trimspace(var.oracle_updater_private_key) == "")
        && nonsensitive(trimspace(var.liquidation_keeper_private_key) == "")
        && nonsensitive(trimspace(var.protection_worker_private_key) == "")
        && nonsensitive(trimspace(var.db_password) == "")
        && nonsensitive(trimspace(var.posthog_project_token) == "")
        && nonsensitive(trimspace(var.aa_proxy_origin_token) == "")
        && nonsensitive(trimspace(var.pyth_api_key) == "")
        && (var.db_manage_master_user_password || var.db_snapshot_identifier != null)
      )
      error_message = "Mainnet forbids plaintext private keys, database passwords, or tokens in Terraform state. Provision secrets as external SSM SecureStrings (*_ssm_parameter_name) and enable db_manage_master_user_password (or restore from db_snapshot_identifier)."
    }

    precondition {
      condition = var.environment != "mainnet" || (
        nonsensitive(var.faucet_private_key == "")
        && nonsensitive(var.faucet_proxy_origin_token == "")
        && !var.testnet_margin_funding_enabled
      )
      error_message = "Testnet faucet and margin-funding controls are forbidden on mainnet."
    }
  }
}

resource "aws_ssm_parameter" "keeper_private_key" {
  count = local.keeper_private_key_external ? 0 : 1

  name  = "/plether/${local.deployment_name}/keeper-private-key"
  type  = "SecureString"
  value = var.keeper_private_key
}

resource "aws_ssm_parameter" "lp_settlement_private_key" {
  count = !local.lp_settlement_private_key_external && nonsensitive(var.lp_settlement_private_key != "") ? 1 : 0

  name  = "/plether/${local.deployment_name}/lp-settlement-private-key"
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
  count = local.oracle_updater_external ? 0 : 1

  name  = "/plether/${local.deployment_name}/oracle-updater-private-key"
  type  = "SecureString"
  value = var.oracle_updater_private_key

  lifecycle {
    precondition {
      condition = (
        (local.keeper_private_key_external || local.normalized_transaction_private_keys.keeper != local.zero_private_key)
        && local.normalized_transaction_private_keys.oracle != local.zero_private_key
        && (local.liquidation_keeper_external || local.normalized_transaction_private_keys.liquidation != local.zero_private_key)
        && (
          local.normalized_transaction_private_keys.lp_settlement == ""
          || local.normalized_transaction_private_keys.lp_settlement != local.zero_private_key
        )
      )
      error_message = "Transaction signer private keys must not be the all-zero scalar."
    }

    precondition {
      condition = (
        (local.keeper_private_key_external || local.liquidation_keeper_external || local.normalized_transaction_private_keys.keeper != local.normalized_transaction_private_keys.liquidation)
        && (local.keeper_private_key_external || local.normalized_transaction_private_keys.oracle != local.normalized_transaction_private_keys.keeper)
        && (local.liquidation_keeper_external || local.normalized_transaction_private_keys.oracle != local.normalized_transaction_private_keys.liquidation)
        && (
          var.lp_settlement_private_key == ""
          || (
            (local.keeper_private_key_external || local.normalized_transaction_private_keys.lp_settlement != local.normalized_transaction_private_keys.keeper)
            && local.normalized_transaction_private_keys.lp_settlement != local.normalized_transaction_private_keys.oracle
            && (local.liquidation_keeper_external || local.normalized_transaction_private_keys.lp_settlement != local.normalized_transaction_private_keys.liquidation)
          )
        )
      )
      error_message = "keeper_private_key, lp_settlement_private_key (when configured), oracle_updater_private_key, and liquidation_keeper_private_key must all be different."
    }
  }
}

resource "aws_ssm_parameter" "liquidation_keeper_private_key" {
  count = local.liquidation_keeper_external ? 0 : 1

  name  = "/plether/${local.deployment_name}/liquidation-keeper-private-key"
  type  = "SecureString"
  value = var.liquidation_keeper_private_key
}

resource "aws_ssm_parameter" "faucet_private_key" {
  count = var.faucet_private_key != "" ? 1 : 0

  name  = "/plether/${local.deployment_name}/faucet-private-key"
  type  = "SecureString"
  value = var.faucet_private_key

  lifecycle {
    precondition {
      condition = (
        can(regex("^[0-9a-f]{64}$", local.normalized_transaction_private_keys.faucet))
        && local.normalized_transaction_private_keys.faucet != local.zero_private_key
        && alltrue([
          for role, key in local.normalized_transaction_private_keys :
          key != local.normalized_transaction_private_keys.faucet if role != "faucet"
        ])
      )
      error_message = "faucet_private_key must be a non-zero 32-byte key dedicated to the faucet; it must differ from keeper, LP settlement, oracle, and liquidation signer keys."
    }
  }
}

resource "aws_ssm_parameter" "faucet_proxy_origin_token" {
  count = var.faucet_private_key != "" ? 1 : 0

  name  = "/plether/${local.deployment_name}/faucet-proxy-origin-token"
  type  = "SecureString"
  value = var.faucet_proxy_origin_token
}

resource "aws_ssm_parameter" "pimlico_api_key" {
  count = var.provision_aa_proxy ? 1 : 0

  name  = "/plether/${local.deployment_name}/pimlico-api-key"
  type  = "SecureString"
  value = var.pimlico_api_key
}

resource "aws_ssm_parameter" "pimlico_sponsorship_policy_id" {
  count = var.provision_aa_proxy ? 1 : 0

  name  = "/plether/${local.deployment_name}/pimlico-sponsorship-policy-id"
  type  = "SecureString"
  value = var.pimlico_sponsorship_policy_id
}

resource "aws_ssm_parameter" "aa_proxy_origin_token" {
  count = local.aa_gateway_enabled && !local.aa_proxy_origin_token_external ? 1 : 0

  name  = "/plether/${local.deployment_name}/aa-proxy-origin-token"
  type  = "SecureString"
  value = var.aa_proxy_origin_token
}

resource "aws_ssm_parameter" "insights_registration_origin_token" {
  count = var.provision_insights_registration ? 1 : 0

  name  = "/plether/${local.deployment_name}/insights-registration-origin-token"
  type  = "SecureString"
  value = var.insights_registration_origin_token
}

resource "aws_ssm_parameter" "insights_registration_origin_token_next" {
  # Only the presence bit is declassified so it can safely drive resource
  # cardinality; the token value remains sensitive in state and plan output.
  count = var.provision_insights_registration && nonsensitive(var.insights_registration_origin_token_next != "") ? 1 : 0

  name  = "/plether/${local.deployment_name}/insights-registration-origin-token-next"
  type  = "SecureString"
  value = var.insights_registration_origin_token_next
}

resource "aws_ssm_parameter" "turnstile_secret_key" {
  count = var.provision_insights_registration ? 1 : 0

  name  = "/plether/${local.deployment_name}/turnstile-secret-key"
  type  = "SecureString"
  value = var.turnstile_secret_key
}

resource "aws_ssm_parameter" "x_oauth_client_secret" {
  count = var.provision_insights_registration ? 1 : 0

  name  = "/plether/${local.deployment_name}/x-oauth-client-secret"
  type  = "SecureString"
  value = var.x_oauth_client_secret
}

resource "aws_ssm_parameter" "insights_registration_email_keys" {
  count = var.provision_insights_registration ? 1 : 0

  name  = "/plether/${local.deployment_name}/insights-registration-email-keys"
  type  = "SecureString"
  value = jsonencode(var.insights_registration_email_keys)

  lifecycle {
    prevent_destroy = true
  }
}

resource "aws_ssm_parameter" "insights_registration_email_hmac_key" {
  count = var.provision_insights_registration ? 1 : 0

  name  = "/plether/${local.deployment_name}/insights-registration-email-hmac-key"
  type  = "SecureString"
  value = var.insights_registration_email_hmac_key_base64

  lifecycle {
    prevent_destroy = true
  }
}

resource "aws_ssm_parameter" "database_url" {
  count = local.database_url_external ? 0 : 1

  name  = "/plether/${local.deployment_name}/database-url"
  type  = "SecureString"
  value = "postgresql://${urlencode(var.db_username)}:${urlencode(var.db_password)}@${aws_db_instance.postgres.endpoint}/plether?sslmode=verify-full&sslrootcert=${urlencode(var.db_ssl_root_cert_path)}"
}

resource "aws_ssm_parameter" "posthog_otlp_authorization_header" {
  count = local.posthog_auth_header_external ? 0 : 1

  name  = "/plether/${local.deployment_name}/posthog-otlp-authorization-header"
  type  = "SecureString"
  value = "Authorization Bearer ${var.posthog_project_token}"
}
