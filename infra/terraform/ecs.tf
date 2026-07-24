resource "aws_ecs_cluster" "main" {
  name = "plether-${var.environment}"
}

resource "aws_cloudwatch_log_group" "ecs" {
  name              = "/ecs/plether-${var.environment}"
  retention_in_days = 14
}

locals {
  effective_pyth_hermes_url = trimspace(var.pyth_hermes_url)

  effective_pyth_api_key_ssm_parameter_name = var.pyth_api_key_ssm_parameter_name != null ? trimspace(var.pyth_api_key_ssm_parameter_name) : (
    var.environment == "sepolia" ? "/plether/sepolia/pyth-api-key" : ""
  )

  external_pyth_api_key_parameter_arn = local.effective_pyth_api_key_ssm_parameter_name != "" ? "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${local.effective_pyth_api_key_ssm_parameter_name}" : null

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
    { name = "PYTH_BACKFILL_DAYS", value = var.pyth_backfill_days },
    { name = "PYTH_SAMPLE_INTERVAL_SECONDS", value = var.pyth_sample_interval_seconds },
    { name = "PYTH_LATEST_MAX_AGE_SECONDS", value = var.pyth_latest_max_age_seconds },
  ]

  pyth_api_key_secret = local.effective_pyth_api_key_parameter_arn != null ? [
    {
      name      = "PYTH_API_KEY"
      valueFrom = local.effective_pyth_api_key_parameter_arn
    }
  ] : []

  faucet_private_key_secret = var.faucet_private_key != "" ? [
    {
      name      = "FAUCET_PRIVATE_KEY"
      valueFrom = aws_ssm_parameter.faucet_private_key[0].arn
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
  cpu                      = var.container_cpu
  memory                   = var.container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

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
      protocol      = "tcp"
    }]

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
    ], local.pyth_api_key_secret, local.faucet_private_key_secret, local.aa_proxy_secrets)

    environment = concat([
      { name = "PORT", value = "3001" },
      { name = "CHAIN_ID", value = var.chain_id },
      { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_USDC", value = var.perps_usdc },
      { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
      { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
      { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
      { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
      { name = "PERPS_ACCOUNT_LENS", value = var.perps_account_lens },
      { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
      { name = "AA_SPONSORSHIP_ENABLED", value = tostring(var.enable_aa_sponsorship) },
      { name = "AA_IP_RATE_LIMIT_PER_MINUTE", value = var.aa_ip_rate_limit_per_minute },
      { name = "AA_ACCOUNT_RATE_LIMIT_PER_MINUTE", value = var.aa_account_rate_limit_per_minute },
      { name = "AA_MAX_REQUEST_BYTES", value = var.aa_max_request_bytes },
      { name = "AA_SPONSORED_GAS_ALERT_WEI_PER_HOUR", value = var.aa_sponsored_gas_alert_wei_per_hour },
      { name = "CORS_ORIGINS", value = var.cors_origins },
      { name = "INDEXER_START_BLOCK", value = var.indexer_start_block },
    ], local.pyth_environment)
  }, local.otel_log_router_container])

  lifecycle {
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
      condition     = !local.uses_upgraded_pyth_hermes || local.pyth_api_key_configured
      error_message = "The upgraded hosted Pyth Hermes endpoint requires an existing pyth_api_key_ssm_parameter_name or enable_pyth_api_key=true with a non-empty pyth_api_key, entitled to all configured FX feeds."
    }
  }
}

resource "aws_ecs_service" "api" {
  name            = "plether-api"
  cluster         = aws_ecs_cluster.main.id
  task_definition = aws_ecs_task_definition.api.arn
  desired_count   = 1
  launch_type     = "FARGATE"

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

  depends_on = [aws_lb_listener.http]

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

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  container_definitions = jsonencode([{
    name      = "plether-keeper"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["plether-keeper"]

    logConfiguration = local.posthog_log_configuration

    secrets = [
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
    ]

    environment = [
      { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_USDC", value = var.perps_usdc },
      { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
      { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
      { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
      { name = "KEEPER_POLL_SECONDS", value = var.keeper_poll_seconds },
      { name = "KEEPER_MAX_BATCH_SIZE", value = var.keeper_max_batch_size },
      { name = "KEEPER_CONFIRMATIONS", value = var.keeper_confirmations },
      { name = "KEEPER_GAS_BUFFER_BPS", value = var.keeper_gas_buffer_bps },
      { name = "KEEPER_FEE_BUFFER_BPS", value = var.keeper_fee_buffer_bps },
    ]
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

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  container_definitions = jsonencode([{
    name      = "plether-liquidation-worker"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["plether-liquidation-worker"]

    logConfiguration = local.posthog_log_configuration

    secrets = [
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
    ]

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

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  container_definitions = jsonencode([{
    name      = "plether-basket-worker"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["plether-basket-worker", "--latest-loop", "--poll-seconds", var.basket_worker_poll_seconds]

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
    ], local.pyth_api_key_secret)

    environment = concat([
      { name = "CHAIN_ID", value = var.chain_id },
      { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
      { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
    ], local.pyth_environment)
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

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  container_definitions = jsonencode([{
    name      = "plether-perps-indexer"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["plether-perps-indexer", "--loop"]

    logConfiguration = local.posthog_log_configuration

    secrets = [
      {
        name      = "PERPS_RPC_URL"
        valueFrom = aws_ssm_parameter.perps_rpc_url.arn
      },
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      }
    ]

    environment = [
      { name = "CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
      { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
      { name = "PERPS_CFD_ENGINE_LENS", value = var.perps_cfd_engine_lens },
      { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
      { name = "PERPS_ACCOUNT_LENS", value = var.perps_account_lens },
      { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
      { name = "PERPS_INDEXER_CONFIRMATIONS", value = var.perps_indexer_confirmations },
      { name = "PERPS_INDEXER_BATCH_SIZE", value = var.perps_indexer_batch_size },
      { name = "PERPS_INDEXER_POLL_SECONDS", value = var.perps_indexer_poll_seconds },
    ]
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

      secrets = [
        {
          name      = "PERPS_RPC_URL"
          valueFrom = aws_ssm_parameter.perps_rpc_url.arn
        },
        {
          name      = "DATABASE_URL"
          valueFrom = aws_ssm_parameter.database_url.arn
        }
      ]

      environment = [
        { name = "CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_USDC", value = var.perps_usdc },
        { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
        { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
        { name = "PERPS_ACCOUNT_LENS", value = var.perps_account_lens },
        { name = "INSIGHTS_SNAPSHOT_POLL_SECONDS", value = var.insights_snapshot_poll_seconds },
        { name = "INSIGHTS_SNAPSHOT_MULTICALL_SIZE", value = var.insights_snapshot_multicall_size },
      ]
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

      secrets = [
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
      ]

      environment = [
        { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
        { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
        { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
        { name = "KEEPER_POLL_SECONDS", value = var.keeper_poll_seconds },
        { name = "KEEPER_MAX_BATCH_SIZE", value = var.keeper_max_batch_size },
        { name = "KEEPER_CONFIRMATIONS", value = var.keeper_confirmations },
        { name = "KEEPER_GAS_BUFFER_BPS", value = var.keeper_gas_buffer_bps },
        { name = "KEEPER_FEE_BUFFER_BPS", value = var.keeper_fee_buffer_bps },
      ]
    },
    {
      name             = "plether-basket-worker"
      image            = "${aws_ecr_repository.api.repository_url}:latest"
      essential        = true
      command          = ["plether-basket-worker", "--latest-loop", "--poll-seconds", var.basket_worker_poll_seconds]
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
      ], local.pyth_api_key_secret)

      environment = concat([
        { name = "CHAIN_ID", value = var.chain_id },
        { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
        { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
      ], local.pyth_environment)
    },
    {
      name             = "plether-oracle-worker"
      image            = "${aws_ecr_repository.api.repository_url}:latest"
      essential        = true
      command          = ["node", "/app/oracle/scripts/perps-oracle-worker.mjs", "--loop"]
      logConfiguration = local.posthog_log_configuration

      secrets = [
        {
          name      = "ARBITRUM_SEPOLIA_RPC_URL"
          valueFrom = aws_ssm_parameter.perps_rpc_url.arn
        },
        {
          name      = "PERPS_ORACLE_UPDATER_PRIVATE_KEY"
          valueFrom = aws_ssm_parameter.oracle_updater_private_key.arn
        }
      ]

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

      secrets = [
        {
          name      = "PERPS_RPC_URL"
          valueFrom = aws_ssm_parameter.perps_rpc_url.arn
        },
        {
          name      = "DATABASE_URL"
          valueFrom = aws_ssm_parameter.database_url.arn
        }
      ]

      environment = [
        { name = "CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
        { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
        { name = "PERPS_CFD_ENGINE_LENS", value = var.perps_cfd_engine_lens },
        { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
        { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
        { name = "PERPS_INDEXER_CONFIRMATIONS", value = var.perps_indexer_confirmations },
        { name = "PERPS_INDEXER_BATCH_SIZE", value = var.perps_indexer_batch_size },
        { name = "PERPS_INDEXER_POLL_SECONDS", value = var.perps_indexer_poll_seconds },
      ]
    },
    {
      name             = "plether-insights-worker"
      image            = "${aws_ecr_repository.api.repository_url}:latest"
      essential        = true
      command          = ["plether-insights-worker"]
      logConfiguration = local.posthog_log_configuration

      secrets = [
        {
          name      = "PERPS_RPC_URL"
          valueFrom = aws_ssm_parameter.perps_rpc_url.arn
        },
        {
          name      = "DATABASE_URL"
          valueFrom = aws_ssm_parameter.database_url.arn
        }
      ]

      environment = [
        { name = "CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
        { name = "PERPS_USDC", value = var.perps_usdc },
        { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
        { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
        { name = "PERPS_ACCOUNT_LENS", value = var.perps_account_lens },
        { name = "INSIGHTS_SNAPSHOT_POLL_SECONDS", value = var.insights_snapshot_poll_seconds },
        { name = "INSIGHTS_SNAPSHOT_MULTICALL_SIZE", value = var.insights_snapshot_multicall_size },
      ]
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

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  lifecycle {
    ignore_changes = [task_definition]
  }
}
