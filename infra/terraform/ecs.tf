resource "aws_ecs_cluster" "main" {
  name = "plether-${var.environment}"
}

resource "aws_cloudwatch_log_group" "ecs" {
  name              = "/ecs/plether-${var.environment}"
  retention_in_days = 14
}

locals {
  effective_pyth_hermes_url = trimspace(var.pyth_hermes_url)

  insights_release_addresses = [
    var.perps_usdc,
    var.perps_order_router,
    var.perps_plether_oracle,
    var.perps_cfd_engine,
    var.perps_cfd_engine_settlement_sidecar,
    var.perps_cfd_engine_lens,
    var.perps_margin_clearinghouse,
    var.perps_account_lens,
  ]

  july_insights_release_addresses = [
    "0xb15503d70b0eaa644dc6650d2a248762f7c5bce3",
    "0x04e3103752f623fbcdcd01f588590af4c53e4c1e",
    "0xadfed3bf768d810309b97b4df9f9e77eaa3a401c",
    "0x6a25ea1015b5f032d8a2d95d57aefcb99219bf0a",
    "0x0b652c4d4610234e221403076c116292f935b424",
    "0xa9aa4097874e9622eaabee68f65ff5e3757728c5",
    "0x19c2f60f6312eaf9acde4c2b04551a05ca9be76e",
    "0xc4c886a6f1d7cb22c833ac1b29f29da43afbccd1",
  ]

  effective_pyth_api_key_ssm_parameter_name = var.pyth_api_key_ssm_parameter_name != null ? trimspace(var.pyth_api_key_ssm_parameter_name) : (
    var.environment == "sepolia" ? "/plether/sepolia/pyth-api-key" : ""
  )

  external_pyth_api_key_parameter_arn = local.effective_pyth_api_key_ssm_parameter_name != "" ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${local.effective_pyth_api_key_ssm_parameter_name}" : null

  rpc_auth_token_parameter_arn       = trimspace(var.rpc_auth_token_ssm_parameter_name) != "" ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.rpc_auth_token_ssm_parameter_name)}" : null
  perps_rpc_auth_token_parameter_arn = trimspace(var.perps_rpc_auth_token_ssm_parameter_name) != "" ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.perps_rpc_auth_token_ssm_parameter_name)}" : null

  effective_pyth_api_key_parameter_arn = local.external_pyth_api_key_parameter_arn != null ? local.external_pyth_api_key_parameter_arn : (
    var.enable_pyth_api_key ? aws_ssm_parameter.pyth_api_key[0].arn : null
  )

  uses_upgraded_pyth_hermes = replace(lower(local.effective_pyth_hermes_url), "/\\/+$/", "") == "https://pyth.dourolabs.app/hermes"
  pyth_api_key_configured = (
    local.external_pyth_api_key_parameter_arn != null
    || (var.enable_pyth_api_key && trimspace(var.pyth_api_key) != "")
  )

  pyth_environment = [
    { name = "PYTH_HERMES_URL", value = local.effective_pyth_hermes_url },
    { name = "PYTH_BENCHMARKS_URL", value = var.pyth_benchmarks_url },
    { name = "PYTH_HISTORY_URL", value = var.pyth_history_url },
    { name = "PYTH_BACKFILL_DAYS", value = var.pyth_backfill_days },
    { name = "PYTH_SAMPLE_INTERVAL_SECONDS", value = var.pyth_sample_interval_seconds },
    { name = "PYTH_LATEST_MAX_AGE_SECONDS", value = var.pyth_latest_max_age_seconds },
  ]

  perps_candle_read_interval_tokens = regexall(
    "[^,[:space:]]+",
    var.perps_candle_read_intervals
  )

  perps_candle_environment = [
    { name = "PERPS_CANDLE_WRITE_MODE", value = var.perps_candle_write_mode },
    { name = "PERPS_CANDLE_READ_MODE", value = var.perps_candle_read_mode },
    { name = "PERPS_CANDLE_READ_INTERVALS", value = var.perps_candle_read_intervals },
    { name = "PERPS_CANDLE_SHADOW_SAMPLE_BPS", value = tostring(var.perps_candle_shadow_sample_bps) },
    { name = "PERPS_CANDLE_STRICT_COVERAGE", value = tostring(var.perps_candle_strict_coverage) },
    { name = "PERPS_CANDLE_LATENESS_SECONDS", value = tostring(var.perps_candle_lateness_seconds) },
    { name = "PERPS_CANDLE_FINALIZATION_GRACE_SECONDS", value = tostring(var.perps_candle_finalization_grace_seconds) },
  ]

  keeper_environment = [
    { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
    { name = "PERPS_USDC", value = var.perps_usdc },
    { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
    { name = "PERPS_ORDER_LIFECYCLE_BOOK", value = var.perps_order_lifecycle_book },
    { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
    { name = "PERPS_HOUSE_POOL", value = var.perps_house_pool },
    { name = "PERPS_SENIOR_VAULT", value = var.perps_senior_vault },
    { name = "PERPS_JUNIOR_VAULT", value = var.perps_junior_vault },
    { name = "PERPS_SETTLEMENT_MONITOR_LENS", value = var.perps_settlement_monitor_lens },
    { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
    { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
    { name = "KEEPER_POLL_SECONDS", value = var.keeper_poll_seconds },
    { name = "KEEPER_IDLE_POLL_SECONDS", value = var.keeper_idle_poll_seconds },
    { name = "KEEPER_MAX_BATCH_SIZE", value = var.keeper_max_batch_size },
    { name = "KEEPER_CONFIRMATIONS", value = var.keeper_confirmations },
    { name = "KEEPER_GAS_BUFFER_BPS", value = var.keeper_gas_buffer_bps },
    { name = "KEEPER_FEE_BUFFER_BPS", value = var.keeper_fee_buffer_bps },
    { name = "LP_SETTLEMENT_MODE", value = var.lp_settlement_mode },
    { name = "LP_SETTLEMENT_POLL_SECONDS", value = var.lp_settlement_poll_seconds },
    { name = "LP_SETTLEMENT_MAX_DRAIN_TRANSACTIONS", value = tostring(var.lp_settlement_max_drain_transactions) },
    { name = "LP_SETTLEMENT_PENDING_REPLACEMENT_SECONDS", value = tostring(var.lp_settlement_pending_replacement_seconds) },
    { name = "LP_SETTLEMENT_MAX_REPLACEMENTS", value = tostring(var.lp_settlement_max_replacements) },
    { name = "LP_SETTLEMENT_MAX_TX_COST_WEI", value = var.lp_settlement_max_tx_cost_wei },
  ]

  pyth_api_key_secret = local.effective_pyth_api_key_parameter_arn != null ? [
    {
      name      = "PYTH_API_KEY"
      valueFrom = local.effective_pyth_api_key_parameter_arn
    }
  ] : []

  rpc_auth_token_secret = local.rpc_auth_token_parameter_arn != null ? [
    {
      name      = "RPC_AUTH_TOKEN"
      valueFrom = local.rpc_auth_token_parameter_arn
    }
  ] : []

  perps_rpc_auth_token_secret = local.perps_rpc_auth_token_parameter_arn != null ? [
    {
      name      = "PERPS_RPC_AUTH_TOKEN"
      valueFrom = local.perps_rpc_auth_token_parameter_arn
    }
  ] : []

  faucet_private_key_secret = var.faucet_private_key != "" ? [
    {
      name      = "FAUCET_PRIVATE_KEY"
      valueFrom = aws_ssm_parameter.faucet_private_key[0].arn
    }
  ] : []

  faucet_proxy_origin_secret = var.faucet_private_key != "" ? [
    {
      name      = "FAUCET_PROXY_ORIGIN_TOKEN"
      valueFrom = aws_ssm_parameter.faucet_proxy_origin_token[0].arn
    }
  ] : []

  # Declassifying key presence exposes no secret material and lets defaults-off
  # task definitions omit the signer env entirely, as required by Config.
  lp_settlement_private_key_secret = nonsensitive(var.lp_settlement_private_key != "") ? [
    {
      name      = "LP_SETTLEMENT_PRIVATE_KEY"
      valueFrom = aws_ssm_parameter.lp_settlement_private_key[0].arn
    }
  ] : []

  aa_proxy_secrets = var.provision_aa_proxy ? [
    {
      name      = "PIMLICO_API_KEY"
      valueFrom = aws_ssm_parameter.pimlico_api_key[0].arn
    },
    {
      name      = "PIMLICO_SPONSORSHIP_POLICY_ID"
      valueFrom = aws_ssm_parameter.pimlico_sponsorship_policy_id[0].arn
    },
    {
      name      = "AA_PROXY_ORIGIN_TOKEN"
      valueFrom = aws_ssm_parameter.aa_proxy_origin_token[0].arn
    }
  ] : []

  insights_registration_secrets = var.provision_insights_registration ? concat(
    [
      {
        name      = "INSIGHTS_REGISTRATION_ORIGIN_TOKEN"
        valueFrom = aws_ssm_parameter.insights_registration_origin_token[0].arn
      },
      {
        name      = "TURNSTILE_SECRET_KEY"
        valueFrom = aws_ssm_parameter.turnstile_secret_key[0].arn
      },
      {
        name      = "X_OAUTH_CLIENT_SECRET"
        valueFrom = aws_ssm_parameter.x_oauth_client_secret[0].arn
      },
      {
        name      = "INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON"
        valueFrom = aws_ssm_parameter.insights_registration_email_keys[0].arn
      },
      {
        name      = "INSIGHTS_REGISTRATION_EMAIL_HMAC_KEY_BASE64"
        valueFrom = aws_ssm_parameter.insights_registration_email_hmac_key[0].arn
      }
    ],
    nonsensitive(var.insights_registration_origin_token_next != "") ? [
      {
        name      = "INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT"
        valueFrom = aws_ssm_parameter.insights_registration_origin_token_next[0].arn
      }
    ] : []
  ) : []

  insights_registration_environment = [
    { name = "INSIGHTS_REGISTRATION_PROVISIONED", value = tostring(var.provision_insights_registration) },
    { name = "INSIGHTS_REGISTRATION_ENABLED", value = tostring(var.enable_insights_registration) },
    { name = "INSIGHTS_REGISTRATION_PUBLIC_ORIGIN", value = var.insights_registration_public_origin },
    { name = "TURNSTILE_EXPECTED_HOSTNAME", value = var.turnstile_expected_hostname },
    { name = "TURNSTILE_EXPECTED_ACTION", value = var.turnstile_expected_action },
    { name = "X_OAUTH_CLIENT_ID", value = var.x_oauth_client_id },
    { name = "X_OAUTH_CALLBACK_URL", value = var.x_oauth_callback_url },
    { name = "X_TARGET_USER_ID", value = var.x_target_user_id },
    { name = "X_TARGET_HANDLE", value = var.x_target_handle },
    { name = "INSIGHTS_REGISTRATION_EMAIL_KEY_VERSION", value = var.insights_registration_email_key_version },
    { name = "INSIGHTS_REGISTRATION_SESSION_TTL_SECONDS", value = tostring(var.insights_registration_session_ttl_seconds) },
    { name = "INSIGHTS_REGISTRATION_IP_RATE_LIMIT_PER_MINUTE", value = tostring(var.insights_registration_ip_rate_limit_per_minute) },
    { name = "INSIGHTS_REGISTRATION_SESSION_RATE_LIMIT_PER_MINUTE", value = tostring(var.insights_registration_session_rate_limit_per_minute) },
    { name = "INSIGHTS_REGISTRATION_RULES_VERSION", value = var.insights_registration_rules_version },
    { name = "INSIGHTS_REGISTRATION_PRIVACY_VERSION", value = var.insights_registration_privacy_version },
  ]

  insights_competition_environment = concat(
    var.insights_active_competition_slug != "" ? [
      { name = "INSIGHTS_ACTIVE_COMPETITION_SLUG", value = var.insights_active_competition_slug }
    ] : [],
    var.insights_competition_release_id != "" ? [
      { name = "INSIGHTS_COMPETITION_RELEASE_ID", value = var.insights_competition_release_id }
    ] : []
  )

  posthog_log_configuration = {
    logDriver = "awsfirelens"
    options = {
      Name                             = "opentelemetry"
      Host                             = var.posthog_otlp_host
      Port                             = "443"
      logs_uri                         = var.posthog_otlp_logs_uri
      logs_body_key                    = "$message"
      logs_body_key_attributes         = "true"
      logs_severity_text_message_key   = "$SeverityText"
      logs_severity_number_message_key = "$SeverityNumber"
      "log-driver-buffer-limit"        = "4096"
      batch_size                       = "100"
      compress                         = "gzip"
      grpc                             = "off"
      http2                            = "off"
      log_response_payload             = "false"
      log_suppress_interval            = "60"
      tls                              = "on"
      "tls.verify"                     = "on"
      "tls.verify_hostname"            = "on"
      Retry_Limit                      = "no_limits"
    }
    secretOptions = [{
      name      = "Header"
      valueFrom = aws_ssm_parameter.posthog_otlp_authorization_header.arn
    }]
  }

  otel_log_router_container = {
    name              = "otel-log-router"
    image             = "${aws_ecr_repository.otel_log_router.repository_url}:latest"
    essential         = true
    mountPoints       = []
    portMappings      = []
    systemControls    = []
    user              = "0"
    volumesFrom       = []
    memoryReservation = 128
    stopTimeout       = 120

    firelensConfiguration = {
      type = "fluentbit"
      options = {
        "enable-ecs-log-metadata" = "true"
        "config-file-type"        = "file"
        "config-file-value"       = "/fluent-bit/etc/otel-enrichment.conf"
      }
    }

    environment = [
      { name = "AWS_REGION", value = var.aws_region },
      { name = "CLOUDWATCH_LOG_GROUP", value = aws_cloudwatch_log_group.ecs.name },
      { name = "DEPLOYMENT_ENVIRONMENT", value = var.environment },
      { name = "ECS_CLUSTER_NAME", value = aws_ecs_cluster.main.name },
      { name = "SERVICE_VERSION", value = "unknown" },
    ]

    logConfiguration = {
      logDriver = "awslogs"
      options = {
        awslogs-group         = aws_cloudwatch_log_group.ecs.name
        awslogs-region        = var.aws_region
        awslogs-stream-prefix = "router"
      }
    }
  }
}

resource "aws_ecs_task_definition" "api" {
  family                   = "plether-${var.environment}"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.api_container_cpu
  memory                   = var.api_container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn
  enable_fault_injection   = false
  tags                     = {}

  depends_on = [terraform_data.perps_candle_rollout_guard]

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  container_definitions = jsonencode([{
    name      = "plether-api"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true

    portMappings = [{
      containerPort = 3001
      hostPort      = 3001
      protocol      = "tcp"
    }]

    mountPoints    = []
    systemControls = []
    volumesFrom    = []

    logConfiguration = local.posthog_log_configuration

    secrets = concat([
      {
        name      = "RPC_URL"
        valueFrom = aws_ssm_parameter.rpc_url.arn
      },
      {
        name      = "PERPS_RPC_URL"
        valueFrom = aws_ssm_parameter.perps_rpc_url.arn
      },
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      }
    ], local.rpc_auth_token_secret, local.perps_rpc_auth_token_secret, local.pyth_api_key_secret, local.faucet_private_key_secret, local.faucet_proxy_origin_secret, local.aa_proxy_secrets, local.insights_registration_secrets)

    environment = concat([
      { name = "PORT", value = "3001" },
      { name = "CHAIN_ID", value = var.chain_id },
      { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
      { name = "VAULT_HISTORY_HOUSE_POOL_ADDRESS", value = var.vault_history_house_pool_address },
      { name = "VAULT_HISTORY_SENIOR_VAULT_ADDRESS", value = var.vault_history_senior_vault_address },
      { name = "VAULT_HISTORY_JUNIOR_VAULT_ADDRESS", value = var.vault_history_junior_vault_address },
      { name = "VAULT_HISTORY_DEPLOYMENT_BLOCK", value = var.vault_history_deployment_block },
      { name = "VAULT_HISTORY_CONFIRMATIONS", value = var.vault_history_confirmations },
      { name = "PERPS_USDC", value = var.perps_usdc },
      { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
      { name = "PERPS_ORDER_LIFECYCLE_BOOK", value = var.perps_order_lifecycle_book },
      { name = "PERPS_HOUSE_POOL", value = var.perps_house_pool },
      { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
      { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
      { name = "PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR", value = var.perps_cfd_engine_settlement_sidecar },
      { name = "PERPS_CFD_ENGINE_LENS", value = var.perps_cfd_engine_lens },
      { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
      { name = "PERPS_ACCOUNT_LENS", value = var.perps_account_lens },
      { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
      { name = "AA_SPONSORSHIP_ENABLED", value = tostring(var.enable_aa_sponsorship) },
      { name = "AA_SPONSOR_PROTECTION_COMMITS_ENABLED", value = tostring(var.aa_protection_commits_enabled) },
      { name = "AA_IP_RATE_LIMIT_PER_MINUTE", value = var.aa_ip_rate_limit_per_minute },
      { name = "AA_ACCOUNT_RATE_LIMIT_PER_MINUTE", value = var.aa_account_rate_limit_per_minute },
      { name = "AA_MAX_REQUEST_BYTES", value = var.aa_max_request_bytes },
      { name = "AA_SPONSORED_GAS_ALERT_WEI_PER_HOUR", value = var.aa_sponsored_gas_alert_wei_per_hour },
      { name = "FAUCET_CLIENT_REQUESTS_PER_HOUR", value = tostring(var.faucet_client_requests_per_hour) },
      { name = "FAUCET_GLOBAL_REQUESTS_PER_HOUR", value = tostring(var.faucet_global_requests_per_hour) },
      { name = "CORS_ORIGINS", value = var.cors_origins },
      { name = "INDEXER_START_BLOCK", value = var.indexer_start_block },
    ], local.pyth_environment, local.perps_candle_environment, local.insights_registration_environment, local.insights_competition_environment)
  }, local.otel_log_router_container])

  lifecycle {
    precondition {
      condition     = var.faucet_global_requests_per_hour >= var.faucet_client_requests_per_hour
      error_message = "The global faucet request limit must be at least the per-client limit."
    }

    precondition {
      condition = var.faucet_private_key == "" || (
        length(var.faucet_proxy_origin_token) >= 32
        && var.faucet_proxy_origin_token != var.aa_proxy_origin_token
        && var.api_desired_count <= 1
      )
      error_message = "A configured faucet signer requires a dedicated 32-character-or-longer proxy token and at most one API task."
    }

    precondition {
      condition = !var.provision_aa_proxy || (
        trimspace(var.pimlico_api_key) != ""
        && trimspace(var.pimlico_sponsorship_policy_id) != ""
        && trimspace(var.aa_proxy_origin_token) != ""
        && trimspace(var.alb_certificate_arn) != ""
        && trimspace(var.api_hostname) != ""
      )
      error_message = "Provisioning the managed AA proxy requires its three credentials, an HTTPS ALB certificate, and the certificate-backed API hostname."
    }

    precondition {
      condition     = !var.enable_aa_sponsorship || var.provision_aa_proxy
      error_message = "AA sponsorship cannot be enabled unless the proxy credentials are provisioned."
    }

    precondition {
      condition = var.environment != "sepolia" || !var.provision_aa_proxy || (
        lower(var.perps_order_router) == "0x2b9790ad11ce5fb1b91ac3415b08cd1ec7d0ce0b"
        && lower(var.perps_order_lifecycle_book) == "0xca57215a3859462eb380ea40969762ac89d99522"
        && lower(var.perps_cfd_engine) == "0x2cedc3f0059f0e9c1099be96974f459e58c428d6"
        && lower(var.perps_margin_clearinghouse) == "0x91c85540a1f64c9aec2c801fcc927f037d619f17"
        && lower(var.perps_house_pool) == "0x7b8b851cb3783611bcda4cf2f7d5a2f8c6106f98"
        && var.perps_indexer_start_block == "305627052"
      )
      error_message = "The Sepolia AA proxy must use the pinned bounded-V2 release and deployment block."
    }

    precondition {
      condition     = !local.uses_upgraded_pyth_hermes || local.pyth_api_key_configured
      error_message = "The upgraded hosted Pyth Hermes endpoint requires an existing pyth_api_key_ssm_parameter_name or enable_pyth_api_key=true with a non-empty pyth_api_key, entitled to all configured FX feeds."
    }

    precondition {
      condition = !var.enable_insights_registration || (
        var.provision_insights_registration
        && var.insights_active_competition_slug != ""
        && var.x_oauth_callback_url == "${var.insights_registration_public_origin}/api/insights/v1/competitions/${var.insights_active_competition_slug}/registrations/x/callback"
      )
      error_message = "Insights registration requires provisioned credentials plus matching active-competition and canonical X callback slugs."
    }

    precondition {
      condition = !var.provision_insights_registration || (
        var.environment == "sepolia"
        && length(var.insights_registration_origin_token) >= 32
        && var.insights_registration_origin_token != "REPLACE_WITH_A_RANDOM_32_BYTE_OR_LONGER_TOKEN"
        && var.insights_registration_origin_token_next != "REPLACE_WITH_A_RANDOM_32_BYTE_OR_LONGER_TOKEN"
        && (
          var.insights_registration_origin_token_next == ""
          || var.insights_registration_origin_token_next != var.insights_registration_origin_token
        )
        && trimspace(var.turnstile_secret_key) != ""
        && var.turnstile_secret_key != "REPLACE_WITH_TURNSTILE_SECRET_KEY"
        && trimspace(var.x_oauth_client_id) != ""
        && var.x_oauth_client_id != "REPLACE_WITH_X_OAUTH_CLIENT_ID"
        && trimspace(var.x_oauth_client_secret) != ""
        && var.x_oauth_client_secret != "REPLACE_WITH_X_OAUTH_CLIENT_SECRET"
        && trimspace(var.x_target_user_id) != ""
        && var.x_target_user_id != "123456789"
        && contains(keys(var.insights_registration_email_keys), var.insights_registration_email_key_version)
        && alltrue([for key in values(var.insights_registration_email_keys) : key != "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="])
        && length(distinct(values(var.insights_registration_email_keys))) == length(values(var.insights_registration_email_keys))
        && trimspace(var.insights_registration_email_hmac_key_base64) != ""
        && var.insights_registration_email_hmac_key_base64 != "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB="
        && !contains(values(var.insights_registration_email_keys), var.insights_registration_email_hmac_key_base64)
        && var.turnstile_expected_hostname == trimprefix(var.insights_registration_public_origin, "https://")
        && var.turnstile_expected_action == "competition_registration"
        && var.x_target_handle == "plether_fi"
        && var.x_oauth_callback_url == "${var.insights_registration_public_origin}/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/callback"
      )
      error_message = "Provisioned Insights registration is Sepolia-only and requires non-placeholder, domain-separated credentials plus the canonical Turnstile action, origin/hostname, X target, callback, active email key, and stable email HMAC."
    }

    precondition {
      condition     = var.provision_insights_registration || var.insights_registration_origin_token_next == ""
      error_message = "insights_registration_origin_token_next may be set only while Insights registration is provisioned."
    }

    precondition {
      condition = var.insights_active_competition_slug != "testnet-trading-2026-09" || var.insights_competition_release_id == "" || (
        var.insights_competition_release_id == "testnet-trading-2026-09"
        && alltrue([
          for address in local.insights_release_addresses :
          can(regex("^0x[0-9A-Fa-f]{40}$", address))
          && lower(address) != "0x0000000000000000000000000000000000000000"
          && !contains(local.july_insights_release_addresses, lower(address))
        ])
        && length(distinct([for address in local.insights_release_addresses : lower(address)])) == length(local.insights_release_addresses)
        && can(regex("^[1-9][0-9]*$", var.perps_indexer_start_block))
        && var.perps_indexer_start_block != "288439939"
      )
      error_message = "When the September 2026 release is bound, INSIGHTS_COMPETITION_RELEASE_ID must equal testnet-trading-2026-09 and all release addresses must be distinct, nonzero, new, and paired with a new positive indexer start block. Leave the release ID empty for registration-only activation."
    }
  }
}

resource "aws_ecs_service" "api" {
  name                              = "plether-api"
  cluster                           = aws_ecs_cluster.main.id
  task_definition                   = aws_ecs_task_definition.api.arn
  desired_count                     = var.api_desired_count
  launch_type                       = "FARGATE"
  health_check_grace_period_seconds = 300

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  load_balancer {
    target_group_arn = aws_lb_target_group.api.arn
    container_name   = "plether-api"
    container_port   = 3001
  }

  depends_on = [aws_lb_listener.http, terraform_data.perps_candle_rollout_guard]

  lifecycle {
    ignore_changes = [task_definition]
  }
}

resource "aws_ecs_task_definition" "keeper" {
  family                   = "plether-${var.environment}-keeper"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.container_cpu
  memory                   = var.container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn
  enable_fault_injection   = false
  tags                     = {}

  depends_on = [terraform_data.lp_settlement_keeper_guard]

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  container_definitions = jsonencode([{
    name      = "plether-keeper"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["plether-keeper"]

    mountPoints    = []
    portMappings   = []
    systemControls = []
    volumesFrom    = []

    logConfiguration = local.posthog_log_configuration

    secrets = concat([
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      },
      {
        name      = "PERPS_RPC_URL"
        valueFrom = aws_ssm_parameter.perps_rpc_url.arn
      },
      {
        name      = "KEEPER_PRIVATE_KEY"
        valueFrom = aws_ssm_parameter.keeper_private_key.arn
      }
    ], local.perps_rpc_auth_token_secret, local.lp_settlement_private_key_secret)

    environment = local.keeper_environment
  }, local.otel_log_router_container])
}

resource "aws_ecs_service" "keeper" {
  name                               = "plether-keeper"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.keeper.arn
  desired_count                      = var.consolidate_workers ? 0 : 1
  launch_type                        = "FARGATE"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  lifecycle {
    ignore_changes = [task_definition]
  }
}

resource "aws_ecs_task_definition" "liquidation_worker" {
  family                   = "plether-${var.environment}-liquidation-worker"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.container_cpu
  memory                   = var.container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn
  enable_fault_injection   = false
  tags                     = {}

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  container_definitions = jsonencode([{
    name      = "plether-liquidation-worker"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["plether-liquidation-worker"]

    mountPoints    = []
    portMappings   = []
    systemControls = []
    volumesFrom    = []

    logConfiguration = local.posthog_log_configuration

    secrets = concat([
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      },
      {
        name      = "PERPS_RPC_URL"
        valueFrom = aws_ssm_parameter.perps_rpc_url.arn
      },
      {
        name      = "LIQUIDATION_KEEPER_PRIVATE_KEY"
        valueFrom = aws_ssm_parameter.liquidation_keeper_private_key.arn
      }
    ], local.perps_rpc_auth_token_secret)

    environment = [
      { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
      { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
      { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
      { name = "PERPS_ACCOUNT_LENS", value = var.perps_account_lens },
      { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
      { name = "LIQUIDATION_WORKER_START_BLOCK", value = var.perps_indexer_start_block },
      { name = "LIQUIDATION_WORKER_POLL_SECONDS", value = var.liquidation_worker_poll_seconds },
      { name = "LIQUIDATION_WORKER_SCAN_BATCH_SIZE", value = var.liquidation_worker_scan_batch_size },
      { name = "LIQUIDATION_WORKER_MULTICALL_SIZE", value = var.liquidation_worker_multicall_size },
      { name = "LIQUIDATION_WORKER_EXECUTION_BATCH_SIZE", value = var.liquidation_worker_execution_batch_size },
      { name = "LIQUIDATION_WORKER_CONFIRMATIONS", value = var.liquidation_worker_confirmations },
      { name = "LIQUIDATION_WORKER_INDEX_BATCH_SIZE", value = var.liquidation_worker_index_batch_size },
      { name = "LIQUIDATION_WORKER_REORG_OVERLAP_BLOCKS", value = var.liquidation_worker_reorg_overlap_blocks },
      { name = "LIQUIDATION_WORKER_PENDING_REPLACEMENT_SECONDS", value = var.liquidation_worker_pending_replacement_seconds },
      { name = "LIQUIDATION_WORKER_GAS_BUFFER_BPS", value = var.liquidation_worker_gas_buffer_bps },
      { name = "LIQUIDATION_WORKER_FEE_BUFFER_BPS", value = var.liquidation_worker_fee_buffer_bps },
    ]
  }, local.otel_log_router_container])
}

resource "aws_ecs_service" "liquidation_worker" {
  name                               = "plether-liquidation-worker"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.liquidation_worker.arn
  desired_count                      = var.liquidation_worker_desired_count
  launch_type                        = "FARGATE"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  lifecycle {
    ignore_changes = [task_definition]
  }
}

resource "aws_ecs_task_definition" "basket_worker" {
  family                   = "plether-${var.environment}-basket-worker"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.container_cpu
  memory                   = var.container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn
  enable_fault_injection   = false
  tags                     = {}

  depends_on = [terraform_data.perps_candle_rollout_guard]

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  container_definitions = jsonencode([{
    name      = "plether-basket-worker"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["plether-basket-worker", "--latest-loop", "--poll-seconds", var.basket_worker_poll_seconds]

    mountPoints    = []
    portMappings   = []
    systemControls = []
    volumesFrom    = []

    logConfiguration = local.posthog_log_configuration

    secrets = concat([
      {
        name      = "RPC_URL"
        valueFrom = aws_ssm_parameter.rpc_url.arn
      },
      {
        name      = "PERPS_RPC_URL"
        valueFrom = aws_ssm_parameter.perps_rpc_url.arn
      },
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      }
    ], local.rpc_auth_token_secret, local.perps_rpc_auth_token_secret, local.pyth_api_key_secret)

    environment = concat([
      { name = "CHAIN_ID", value = var.chain_id },
      { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
      { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
    ], local.pyth_environment, local.perps_candle_environment)
  }, local.otel_log_router_container])

  lifecycle {
    precondition {
      condition     = !local.uses_upgraded_pyth_hermes || local.pyth_api_key_configured
      error_message = "The upgraded hosted Pyth Hermes endpoint requires an existing pyth_api_key_ssm_parameter_name or enable_pyth_api_key=true with a non-empty pyth_api_key, entitled to all configured FX feeds."
    }
  }
}

resource "aws_ecs_service" "basket_worker" {
  name                               = "plether-basket-worker"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.basket_worker.arn
  desired_count                      = var.consolidate_workers ? 0 : 1
  launch_type                        = "FARGATE"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100

  depends_on = [terraform_data.perps_candle_rollout_guard]

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  lifecycle {
    ignore_changes = [task_definition]
  }
}

resource "aws_ecs_task_definition" "perps_indexer" {
  family                   = "plether-${var.environment}-perps-indexer"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.container_cpu
  memory                   = var.container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn
  enable_fault_injection   = false
  tags                     = {}

  depends_on = [terraform_data.perps_candle_rollout_guard]

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  container_definitions = jsonencode([{
    name      = "plether-perps-indexer"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["plether-perps-indexer", "--loop"]

    mountPoints    = []
    portMappings   = []
    systemControls = []
    volumesFrom    = []

    logConfiguration = local.posthog_log_configuration

    secrets = concat([
      {
        name      = "PERPS_RPC_URL"
        valueFrom = aws_ssm_parameter.perps_rpc_url.arn
      },
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      }
    ], local.perps_rpc_auth_token_secret)

    environment = concat([
      { name = "DEPLOYMENT_ENVIRONMENT", value = var.environment },
      { name = "CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
      { name = "VAULT_HISTORY_HOUSE_POOL_ADDRESS", value = var.vault_history_house_pool_address },
      { name = "VAULT_HISTORY_SENIOR_VAULT_ADDRESS", value = var.vault_history_senior_vault_address },
      { name = "VAULT_HISTORY_JUNIOR_VAULT_ADDRESS", value = var.vault_history_junior_vault_address },
      { name = "VAULT_HISTORY_DEPLOYMENT_BLOCK", value = var.vault_history_deployment_block },
      { name = "VAULT_HISTORY_CONFIRMATIONS", value = var.vault_history_confirmations },
      { name = "PERPS_USDC", value = var.perps_usdc },
      { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
      { name = "PERPS_ORDER_LIFECYCLE_BOOK", value = var.perps_order_lifecycle_book },
      { name = "PERPS_HOUSE_POOL", value = var.perps_house_pool },
      { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
      { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
      { name = "PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR", value = var.perps_cfd_engine_settlement_sidecar },
      { name = "PERPS_CFD_ENGINE_LENS", value = var.perps_cfd_engine_lens },
      { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
      { name = "PERPS_ACCOUNT_LENS", value = var.perps_account_lens },
      { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
      { name = "PERPS_INDEXER_CONFIRMATIONS", value = var.perps_indexer_confirmations },
      { name = "PERPS_INDEXER_BATCH_SIZE", value = var.perps_indexer_batch_size },
      { name = "PERPS_INDEXER_POLL_SECONDS", value = var.perps_indexer_poll_seconds },
    ], local.perps_candle_environment, local.insights_competition_environment)
  }, local.otel_log_router_container])
}

resource "aws_ecs_service" "perps_indexer" {
  name                               = "plether-perps-indexer"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.perps_indexer.arn
  desired_count                      = var.consolidate_workers ? 0 : 1
  launch_type                        = "FARGATE"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100

  depends_on = [terraform_data.perps_candle_rollout_guard]

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  lifecycle {
    ignore_changes = [task_definition]
  }
}

resource "aws_ecs_task_definition" "insights_worker" {
  family                   = "plether-${var.environment}-insights-worker"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.container_cpu
  memory                   = var.container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn
  enable_fault_injection   = false
  tags                     = {}

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  container_definitions = jsonencode([
    {
      name             = "plether-insights-worker"
      image            = "${aws_ecr_repository.api.repository_url}:latest"
      essential        = true
      command          = ["plether-insights-worker"]
      logConfiguration = local.posthog_log_configuration

      mountPoints    = []
      portMappings   = []
      systemControls = []
      volumesFrom    = []

      secrets = concat([
        {
          name      = "PERPS_RPC_URL"
          valueFrom = aws_ssm_parameter.perps_rpc_url.arn
        },
        {
          name      = "DATABASE_URL"
          valueFrom = aws_ssm_parameter.database_url.arn
        }
      ], local.perps_rpc_auth_token_secret)

      environment = concat([
        { name = "CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_USDC", value = var.perps_usdc },
        { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
        { name = "PERPS_ORDER_LIFECYCLE_BOOK", value = var.perps_order_lifecycle_book },
        { name = "PERPS_HOUSE_POOL", value = var.perps_house_pool },
        { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
        { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
        { name = "PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR", value = var.perps_cfd_engine_settlement_sidecar },
        { name = "PERPS_CFD_ENGINE_LENS", value = var.perps_cfd_engine_lens },
        { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
        { name = "PERPS_ACCOUNT_LENS", value = var.perps_account_lens },
        { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
        { name = "INSIGHTS_SNAPSHOT_POLL_SECONDS", value = var.insights_snapshot_poll_seconds },
        { name = "INSIGHTS_SNAPSHOT_MULTICALL_SIZE", value = var.insights_snapshot_multicall_size },
      ], local.insights_competition_environment)
    },
    local.otel_log_router_container,
  ])
}

resource "aws_ecs_service" "insights_worker" {
  name                               = "plether-insights-worker"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.insights_worker.arn
  desired_count                      = var.consolidate_workers ? 0 : 1
  launch_type                        = "FARGATE"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  lifecycle {
    ignore_changes = [task_definition]
  }
}

resource "aws_ecs_task_definition" "workers" {
  count = var.consolidate_workers ? 1 : 0

  family                   = "plether-${var.environment}-workers"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.workers_container_cpu
  memory                   = var.workers_container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn
  enable_fault_injection   = false
  tags                     = {}

  depends_on = [
    terraform_data.perps_candle_rollout_guard,
    terraform_data.lp_settlement_keeper_guard,
  ]

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  container_definitions = jsonencode([
    {
      name             = "plether-keeper"
      image            = "${aws_ecr_repository.api.repository_url}:latest"
      essential        = true
      command          = ["plether-keeper"]
      logConfiguration = local.posthog_log_configuration

      mountPoints    = []
      portMappings   = []
      systemControls = []
      volumesFrom    = []

      secrets = concat([
        {
          name      = "DATABASE_URL"
          valueFrom = aws_ssm_parameter.database_url.arn
        },
        {
          name      = "PERPS_RPC_URL"
          valueFrom = aws_ssm_parameter.perps_rpc_url.arn
        },
        {
          name      = "KEEPER_PRIVATE_KEY"
          valueFrom = aws_ssm_parameter.keeper_private_key.arn
        }
      ], local.perps_rpc_auth_token_secret, local.lp_settlement_private_key_secret)

      environment = local.keeper_environment
    },
    {
      name             = "plether-basket-worker"
      image            = "${aws_ecr_repository.api.repository_url}:latest"
      essential        = true
      command          = ["plether-basket-worker", "--latest-loop", "--poll-seconds", var.basket_worker_poll_seconds]
      logConfiguration = local.posthog_log_configuration

      mountPoints    = []
      portMappings   = []
      systemControls = []
      volumesFrom    = []

      secrets = concat([
        {
          name      = "RPC_URL"
          valueFrom = aws_ssm_parameter.rpc_url.arn
        },
        {
          name      = "PERPS_RPC_URL"
          valueFrom = aws_ssm_parameter.perps_rpc_url.arn
        },
        {
          name      = "DATABASE_URL"
          valueFrom = aws_ssm_parameter.database_url.arn
        }
      ], local.rpc_auth_token_secret, local.perps_rpc_auth_token_secret, local.pyth_api_key_secret)

      environment = concat([
        { name = "CHAIN_ID", value = var.chain_id },
        { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
        { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
      ], local.pyth_environment, local.perps_candle_environment)
    },
    {
      name             = "plether-oracle-worker"
      image            = "${aws_ecr_repository.api.repository_url}:latest"
      essential        = true
      command          = ["node", "/app/oracle/scripts/perps-oracle-worker.mjs", "--loop"]
      logConfiguration = local.posthog_log_configuration

      mountPoints    = []
      portMappings   = []
      systemControls = []
      volumesFrom    = []

      secrets = concat([
        {
          name      = "ARBITRUM_SEPOLIA_RPC_URL"
          valueFrom = aws_ssm_parameter.perps_rpc_url.arn
        },
        {
          name      = "PERPS_ORACLE_UPDATER_PRIVATE_KEY"
          valueFrom = aws_ssm_parameter.oracle_updater_private_key.arn
        }
      ], local.perps_rpc_auth_token_secret)

      environment = [
        {
          name  = "PERPS_ORACLE_UPDATER_BACKEND_URL"
          value = var.api_hostname != "" ? "https://${var.api_hostname}" : "http://${aws_lb.api.dns_name}"
        },
        { name = "PERPS_ORACLE_UPDATER_POLL_SECONDS", value = var.perps_oracle_updater_poll_seconds },
        { name = "PERPS_ORACLE_UPDATER_MAX_PAYLOAD_AGE_SECONDS", value = var.perps_oracle_updater_max_payload_age_seconds },
      ]
    },
    {
      name             = "plether-perps-indexer"
      image            = "${aws_ecr_repository.api.repository_url}:latest"
      essential        = true
      command          = ["plether-perps-indexer", "--loop"]
      logConfiguration = local.posthog_log_configuration

      mountPoints    = []
      portMappings   = []
      systemControls = []
      volumesFrom    = []

      secrets = concat([
        {
          name      = "PERPS_RPC_URL"
          valueFrom = aws_ssm_parameter.perps_rpc_url.arn
        },
        {
          name      = "DATABASE_URL"
          valueFrom = aws_ssm_parameter.database_url.arn
        }
      ], local.perps_rpc_auth_token_secret)

      environment = concat([
        { name = "DEPLOYMENT_ENVIRONMENT", value = var.environment },
        { name = "CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
        { name = "VAULT_HISTORY_HOUSE_POOL_ADDRESS", value = var.vault_history_house_pool_address },
        { name = "VAULT_HISTORY_SENIOR_VAULT_ADDRESS", value = var.vault_history_senior_vault_address },
        { name = "VAULT_HISTORY_JUNIOR_VAULT_ADDRESS", value = var.vault_history_junior_vault_address },
        { name = "VAULT_HISTORY_DEPLOYMENT_BLOCK", value = var.vault_history_deployment_block },
        { name = "VAULT_HISTORY_CONFIRMATIONS", value = var.vault_history_confirmations },
        { name = "PERPS_USDC", value = var.perps_usdc },
        { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
        { name = "PERPS_ORDER_LIFECYCLE_BOOK", value = var.perps_order_lifecycle_book },
        { name = "PERPS_HOUSE_POOL", value = var.perps_house_pool },
        { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
        { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
        { name = "PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR", value = var.perps_cfd_engine_settlement_sidecar },
        { name = "PERPS_CFD_ENGINE_LENS", value = var.perps_cfd_engine_lens },
        { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
        { name = "PERPS_ACCOUNT_LENS", value = var.perps_account_lens },
        { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
        { name = "PERPS_INDEXER_CONFIRMATIONS", value = var.perps_indexer_confirmations },
        { name = "PERPS_INDEXER_BATCH_SIZE", value = var.perps_indexer_batch_size },
        { name = "PERPS_INDEXER_POLL_SECONDS", value = var.perps_indexer_poll_seconds },
      ], local.perps_candle_environment, local.insights_competition_environment)
    },
    {
      name             = "plether-insights-worker"
      image            = "${aws_ecr_repository.api.repository_url}:latest"
      essential        = true
      command          = ["plether-insights-worker"]
      logConfiguration = local.posthog_log_configuration

      mountPoints    = []
      portMappings   = []
      systemControls = []
      volumesFrom    = []

      secrets = concat([
        {
          name      = "PERPS_RPC_URL"
          valueFrom = aws_ssm_parameter.perps_rpc_url.arn
        },
        {
          name      = "DATABASE_URL"
          valueFrom = aws_ssm_parameter.database_url.arn
        }
      ], local.perps_rpc_auth_token_secret)

      environment = concat([
        { name = "CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_USDC", value = var.perps_usdc },
        { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
        { name = "PERPS_ORDER_LIFECYCLE_BOOK", value = var.perps_order_lifecycle_book },
        { name = "PERPS_HOUSE_POOL", value = var.perps_house_pool },
        { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
        { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
        { name = "PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR", value = var.perps_cfd_engine_settlement_sidecar },
        { name = "PERPS_CFD_ENGINE_LENS", value = var.perps_cfd_engine_lens },
        { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
        { name = "PERPS_ACCOUNT_LENS", value = var.perps_account_lens },
        { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
        { name = "INSIGHTS_SNAPSHOT_POLL_SECONDS", value = var.insights_snapshot_poll_seconds },
        { name = "INSIGHTS_SNAPSHOT_MULTICALL_SIZE", value = var.insights_snapshot_multicall_size },
      ], local.insights_competition_environment)
    },
    local.otel_log_router_container,
  ])

  lifecycle {
    precondition {
      condition     = !local.uses_upgraded_pyth_hermes || local.pyth_api_key_configured
      error_message = "The upgraded hosted Pyth Hermes endpoint requires an existing pyth_api_key_ssm_parameter_name or enable_pyth_api_key=true with a non-empty pyth_api_key, entitled to all configured FX feeds."
    }
  }
}

resource "aws_ecs_service" "workers" {
  count = var.consolidate_workers ? 1 : 0

  name                               = "plether-workers"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.workers[0].arn
  desired_count                      = var.workers_desired_count
  launch_type                        = "FARGATE"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100

  depends_on = [terraform_data.perps_candle_rollout_guard]

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  lifecycle {
    ignore_changes = [task_definition]
  }
}
