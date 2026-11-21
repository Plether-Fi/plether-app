# The Kernel v0.9 pilot is a second, normally dormant Sepolia stack. None of
# these resources replace the EntryPoint v0.8 Alto, API, or reconciler.
locals {
  kernel_canary_count        = var.provision_services ? 1 : 0
  kernel_canary_ssm_prefix   = "arn:aws:ssm:${local.region}:${data.aws_caller_identity.current.account_id}:parameter"
  kernel_canary_alto_rpc_arn = "${local.kernel_canary_ssm_prefix}${var.kernel_canary_alto_rpc_url_ssm_parameter_name}"
  kernel_canary_executor_arn = "${local.kernel_canary_ssm_prefix}${trimspace(var.kernel_canary_alto_executor_keys_ssm_parameter_name)}"
  kernel_canary_utility_arn  = "${local.kernel_canary_ssm_prefix}${trimspace(var.kernel_canary_alto_utility_key_ssm_parameter_name)}"
  kernel_canary_origin_arn   = "${local.kernel_canary_ssm_prefix}${trimspace(var.kernel_canary_origin_token_ssm_parameter_name)}"

  kernel_canary_roles = toset(["alto", "api", "reconciler"])
  kernel_canary_execution_role_names = {
    for name in local.kernel_canary_roles : name => "plether-sepolia-kernel-${name}-execution"
  }
  kernel_canary_task_role_names = {
    for name in local.kernel_canary_roles : name => "plether-sepolia-kernel-${name}-task"
  }

  kernel_canary_execution_secret_arns = {
    alto       = [for secret in local.kernel_canary_alto_secrets : secret.valueFrom]
    api        = distinct([for secret in local.kernel_canary_api_secrets : secret.valueFrom])
    reconciler = distinct([for secret in local.kernel_canary_reconciler_secrets : secret.valueFrom])
  }
  kernel_canary_customer_secret_arns = {
    alto       = [local.kernel_canary_executor_arn, local.kernel_canary_utility_arn]
    api        = [local.kernel_canary_origin_arn]
    reconciler = []
  }

  # Reuse reviewed release bindings without inheriting the live API's AA,
  # faucet, registration, ingestion or background writer activation flags.
  kernel_canary_api_base_environment_names = [
    "PORT", "CHAIN_ID", "PERPS_CHAIN_ID",
    "PERPS_SETTLEMENT_TOKEN", "PERPS_ORDER_ROUTER", "PERPS_ORDER_LIFECYCLE_BOOK",
    "PERPS_HOUSE_POOL", "PERPS_SENIOR_VAULT", "PERPS_JUNIOR_VAULT",
    "PERPS_SETTLEMENT_MONITOR_LENS", "PERPS_PLETHER_ORACLE", "PERPS_CFD_ENGINE",
    "PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR", "PERPS_CFD_ENGINE_LENS",
    "PERPS_MARGIN_CLEARINGHOUSE", "PERPS_ACCOUNT_LENS", "PERPS_INDEXER_START_BLOCK",
    "VAULT_HISTORY_HOUSE_POOL_ADDRESS", "VAULT_HISTORY_SENIOR_VAULT_ADDRESS",
    "VAULT_HISTORY_JUNIOR_VAULT_ADDRESS", "VAULT_HISTORY_DEPLOYMENT_BLOCK",
    "VAULT_HISTORY_CONFIRMATIONS", "INDEXER_START_BLOCK",
    "AA_IP_RATE_LIMIT_PER_MINUTE", "AA_ACCOUNT_RATE_LIMIT_PER_MINUTE", "AA_MAX_REQUEST_BYTES",
  ]
  kernel_canary_api_base_environment = local.kernel_canary_count == 1 ? [
    for setting in local.live_api_container.environment : setting
    if contains(local.kernel_canary_api_base_environment_names, setting.name)
  ] : []
  kernel_canary_api_environment = concat(local.kernel_canary_api_base_environment, [
    { name = "CORS_ORIGINS", value = var.kernel_canary_preview_origin },
    { name = "KERNEL_CANARY_API_MODE", value = "true" },
    { name = "API_BACKGROUND_WORKERS_ENABLED", value = "false" },
    { name = "AA_RPC_MODE", value = "dual-independent" },
    { name = "AA_SPONSORSHIP_ENABLED", value = "false" },
    { name = "AA_NATIVE_SPONSORSHIP_ENABLED", value = "false" },
    { name = "AA_NATIVE_SUBMISSION_ENABLED", value = "false" },
    { name = "AA_NATIVE_PREPARATION_ENABLED", value = "false" },
    { name = "AA_NATIVE_GLOBAL_ROLLOUT_ENABLED", value = "false" },
    { name = "TESTNET_MARGIN_FUNDING_ENABLED", value = "false" },
    { name = "INSIGHTS_REGISTRATION_ENABLED", value = "false" },
    { name = "PYTH_INGESTION_ENABLED", value = "false" },
    { name = "PERPS_CANDLE_WRITE_MODE", value = "off" },
    { name = "PERPS_CANDLE_READ_MODE", value = "legacy" },
    { name = "AA_KERNEL_PROFILE_JSON", value = var.kernel_canary_profile_json },
    { name = "AA_KERNEL_CANARY_OWNERS_JSON", value = var.kernel_canary_owners_json },
    { name = "AA_KERNEL_PUBLIC_ADMISSION", value = "false" },
    { name = "AA_KERNEL_ETH_ENABLED", value = tostring(var.kernel_canary_eth_enabled) },
    { name = "AA_KERNEL_SPONSORSHIP_ENABLED", value = tostring(var.kernel_canary_sponsorship_enabled) },
    { name = "AA_KERNEL_PROTECTION_COMMITS_ENABLED", value = "false" },
    { name = "AA_KERNEL_MAX_SAFE_LAG_SECONDS", value = var.kernel_canary_max_safe_lag_seconds },
    { name = "AA_KERNEL_ALTO_RPC_URL", value = local.kernel_canary_count == 1 ? "http://${aws_lb.kernel_canary_alto[0].dns_name}" : "" },
    { name = "AWS_REGION", value = local.region },
    ], var.kernel_canary_sponsorship_json == "" ? [] : [
    { name = "AA_KERNEL_SPONSORSHIP_JSON", value = var.kernel_canary_sponsorship_json },
    ], var.kernel_canary_sponsorship_enabled && local.kernel_canary_count == 1 ? [
    { name = "AA_KERNEL_PAYMASTER_KMS_KEY_ID", value = aws_kms_key.paymaster[0].arn },
  ] : [])

  kernel_canary_api_secrets = concat([
    { name = "RPC_URL", valueFrom = local.kernel_canary_rpc_url_arn },
    { name = "PERPS_RPC_URL", valueFrom = local.kernel_canary_perps_rpc_url_arn },
    { name = "DATABASE_URL", valueFrom = local.kernel_canary_database_url_arn },
    { name = "AA_SECURITY_RPC_URL", valueFrom = local.kernel_canary_security_rpc_url_arn },
    { name = "AA_PROXY_ORIGIN_TOKEN", valueFrom = local.kernel_canary_origin_arn },
  ], local.kernel_canary_rpc_auth_token_secret)
  kernel_canary_reconciler_secrets = [
    { name = "DATABASE_URL", valueFrom = local.kernel_canary_database_url_arn },
    { name = "PERPS_RPC_URL", valueFrom = local.kernel_canary_perps_rpc_url_arn },
    { name = "AA_SECURITY_RPC_URL", valueFrom = local.kernel_canary_security_rpc_url_arn },
  ]

  kernel_canary_alto_base_environment = local.kernel_canary_count == 1 ? [
    for setting in local.live_alto_container.environment : setting
    if !startswith(setting.name, "PLETHER_ALTO_")
    && !startswith(setting.name, "ALTO_ENTRYPOINT")
    && !contains([
      "ALTO_RPC_URL", "ALTO_SEND_TRANSACTION_RPC_URL", "ALTO_PIMLICO_SIMULATION_CONTRACT",
      "ALTO_SAFE_MODE", "ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION", "ALTO_EXPIRATION_CHECK",
      "ALTO_DEPLOY_SIMULATIONS_CONTRACT", "ALTO_RPC_METHODS", "ALTO_MAX_GAS_PER_USER_OP",
    ], setting.name)
  ] : []
  kernel_canary_alto_environment = concat(local.kernel_canary_alto_base_environment, [
    { name = "ALTO_ENTRYPOINTS", value = "0x433709009b8330fda32311df1c2afa402ed8d009" },
    { name = "ALTO_ENTRYPOINT_SIMULATION_CONTRACT_V9", value = var.kernel_canary_entrypoint_simulation_contract_v9 },
    { name = "ALTO_PIMLICO_SIMULATION_CONTRACT", value = var.kernel_canary_pimlico_simulation_contract },
    { name = "ALTO_DEPLOY_SIMULATIONS_CONTRACT", value = "false" },
    { name = "ALTO_RPC_METHODS", value = local.kernel_canary_alto_allowed_rpc_methods },
    { name = "ALTO_SAFE_MODE", value = "false" },
    { name = "ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION", value = "false" },
    { name = "ALTO_EXPIRATION_CHECK", value = "true" },
    { name = "ALTO_MAX_GAS_PER_USER_OP", value = "17000000" },
  ])
  kernel_canary_alto_secrets = [
    { name = "ALTO_RPC_URL", valueFrom = local.kernel_canary_alto_rpc_arn },
    { name = "ALTO_EXECUTOR_PRIVATE_KEYS", valueFrom = local.kernel_canary_executor_arn },
    { name = "ALTO_UTILITY_PRIVATE_KEY", valueFrom = local.kernel_canary_utility_arn },
  ]
  # Upstream v1.2.8 linux/arm64 child manifest. The protected ECR mirror
  # preserves the digest, so matching only the repository is insufficient.
  kernel_canary_reviewed_alto_image = "${local.expected_account_id}.dkr.ecr.${local.region}.amazonaws.com/plether-kernel-alto-sepolia@sha256:b37c5697bc5331391cb8bc5ac6e828d15019fc786c58a33a8650ff3c434ac2aa"
  kernel_canary_managed_alto_qualified = (
    var.kernel_canary_managed_alto_qualified
    && var.kernel_canary_managed_alto_qualified_image == var.kernel_canary_alto_image
  )

  kernel_canary_awslogs = {
    logDriver = "awslogs"
    options = {
      awslogs-group         = local.kernel_canary_count == 1 ? aws_cloudwatch_log_group.kernel_canary[0].name : ""
      awslogs-region        = local.region
      awslogs-stream-prefix = "ecs"
    }
  }
}

resource "terraform_data" "kernel_canary_guard" {
  count      = local.kernel_canary_count
  depends_on = [terraform_data.kernel_canary_live_identity_guard]

  lifecycle {
    precondition {
      condition = (
        local.region == "ap-southeast-1"
        && var.provision_image_build_resources && var.provision_paymaster_key
        && var.provision_ssm_encryption_key
      )
      error_message = "The isolated Kernel service stack requires the reviewed image repositories and separate paymaster key in this same state."
    }
    precondition {
      condition = (
        var.kernel_canary_alto_image == local.kernel_canary_reviewed_alto_image
        && can(regex("^${data.aws_caller_identity.current.account_id}\\.dkr\\.ecr\\.${local.region}\\.amazonaws\\.com/plether-kernel-api-sepolia@sha256:[0-9a-f]{64}$", var.kernel_canary_api_image))
        && data.aws_ecr_image.kernel_canary_alto[0].image_digest == try(split("@", var.kernel_canary_alto_image)[1], "")
        && data.aws_ecr_image.kernel_canary_api[0].image_digest == try(split("@", var.kernel_canary_api_image)[1], "")
      )
      error_message = "Canary images must be account-local ECR digests for the reviewed official Alto ARM64 mirror and the candidate API."
    }
    precondition {
      condition = var.kernel_canary_alto_desired_count == 0 || (
        var.kernel_canary_simulators_attested
        && can(regex("^0x[0-9A-Fa-f]{40}$", var.kernel_canary_entrypoint_simulation_contract_v9))
        && can(regex("^0x[0-9A-Fa-f]{40}$", var.kernel_canary_pimlico_simulation_contract))
        && lower(var.kernel_canary_entrypoint_simulation_contract_v9) != "0x0000000000000000000000000000000000000000"
        && lower(var.kernel_canary_pimlico_simulation_contract) != "0x0000000000000000000000000000000000000000"
        && lower(var.kernel_canary_entrypoint_simulation_contract_v9) != lower(var.kernel_canary_pimlico_simulation_contract)
      )
      error_message = "Starting canary Alto requires explicit reviewed EntryPoint v0.9 and Pimlico simulation contract addresses."
    }
    precondition {
      condition = var.kernel_canary_api_desired_count == 0 || (
        var.kernel_canary_alto_desired_count == 1
        && var.kernel_canary_db_schema_confirmed
        && var.kernel_canary_profile_attested
        && local.kernel_canary_profile_valid
        && local.kernel_canary_owners_valid
        && (var.kernel_canary_sponsorship_json == "" || local.kernel_canary_sponsor_valid)
      )
      error_message = "Starting canary API requires the canary Alto and an attested Arbitrum Sepolia Kernel v0.9 profile."
    }
    precondition {
      condition = !var.kernel_canary_route_enabled || (
        var.kernel_canary_api_desired_count == 1
        && local.kernel_canary_managed_alto_qualified
        && trimspace(data.aws_lb_listener.live_api_https[0].certificate_arn) != ""
        && trimspace(var.api_hostname) != ""
        && can(regex("^https://[a-z0-9]([a-z0-9-]*[a-z0-9])?(\\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+$", var.kernel_canary_preview_origin))
        && !can(regex("(^|\\.)plether\\.com$", trimprefix(var.kernel_canary_preview_origin, "https://")))
      )
      error_message = "The canary public path requires a running API, managed qualification of the exact Alto image and an exact HTTPS preview origin."
    }
    precondition {
      condition = !(var.kernel_canary_eth_enabled || var.kernel_canary_sponsorship_enabled) || (
        var.kernel_canary_api_desired_count == 1 && var.kernel_canary_alto_desired_count == 1
        && local.kernel_canary_managed_alto_qualified
        && length(try(tolist(jsondecode(var.kernel_canary_owners_json)), [])) > 0
        && local.kernel_canary_owners_valid
      )
      error_message = "Kernel payment choices require running canary API and Alto tasks, managed qualification of the exact Alto image and an exact nonempty canary owner allowlist."
    }
    precondition {
      condition = !var.kernel_canary_sponsorship_enabled || (
        var.kernel_canary_reconciler_desired_count == 1 && local.kernel_canary_sponsor_valid
        && try(length(local.kernel_canary_sponsor.canaryOwners) > 0, false)
      )
      error_message = "Canary sponsorship requires its own active reconciler and reviewed paymaster profile."
    }
    precondition {
      condition = var.kernel_canary_reconciler_desired_count == 0 || (
        var.kernel_canary_db_schema_confirmed && var.kernel_canary_profile_attested
        && local.kernel_canary_profile_valid && local.kernel_canary_sponsor_valid
      )
      error_message = "The canary reconciler requires the confirmed database schema, attested Kernel profile and valid paymaster profile. It may drain after sponsorship is disabled."
    }
  }
}

resource "aws_cloudwatch_log_group" "kernel_canary" {
  count = local.kernel_canary_count

  name              = "/ecs/plether-sepolia-kernel-canary"
  retention_in_days = 14
}

resource "aws_iam_role" "kernel_canary_execution" {
  for_each = var.provision_services ? local.kernel_canary_execution_role_names : {}

  name = each.value
  assume_role_policy = jsonencode({
    Version   = "2012-10-17"
    Statement = [{ Effect = "Allow", Action = "sts:AssumeRole", Principal = { Service = "ecs-tasks.amazonaws.com" } }]
  })
}

resource "aws_iam_role_policy_attachment" "kernel_canary_execution" {
  for_each = aws_iam_role.kernel_canary_execution

  role       = each.value.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

resource "aws_iam_role_policy" "kernel_canary_execution_secrets" {
  for_each = aws_iam_role.kernel_canary_execution

  name = "exact-canary-ssm-refs"
  role = each.value.id
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = concat([{ Effect = "Allow", Action = ["ssm:GetParameter", "ssm:GetParameters"], Resource = local.kernel_canary_execution_secret_arns[each.key] }],
      [for parameter_arn in local.kernel_canary_customer_secret_arns[each.key] : {
        Effect   = "Allow"
        Action   = ["kms:Decrypt"]
        Resource = aws_kms_key.kernel_canary_ssm[0].arn
        Condition = { StringEquals = {
          "kms:ViaService"                      = "ssm.${local.region}.amazonaws.com"
          "kms:EncryptionContext:PARAMETER_ARN" = parameter_arn
        } }
    }])
  })
}

resource "aws_iam_role" "kernel_canary_task" {
  for_each = var.provision_services ? local.kernel_canary_task_role_names : {}

  name = each.value
  assume_role_policy = jsonencode({
    Version   = "2012-10-17"
    Statement = [{ Effect = "Allow", Action = "sts:AssumeRole", Principal = { Service = "ecs-tasks.amazonaws.com" } }]
  })
}

resource "aws_iam_role_policy" "kernel_canary_paymaster_sign" {
  count = local.kernel_canary_count == 1 && var.kernel_canary_sponsorship_enabled ? 1 : 0

  name = "sign-canary-paymaster-digests"
  role = aws_iam_role.kernel_canary_task["api"].id
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      { Effect = "Allow", Action = ["kms:DescribeKey", "kms:GetPublicKey"], Resource = aws_kms_key.paymaster[0].arn },
      {
        Effect    = "Allow", Action = ["kms:Sign"], Resource = aws_kms_key.paymaster[0].arn
        Condition = { StringEquals = { "kms:SigningAlgorithm" = "ECDSA_SHA_256" } }
      },
    ]
  })
}

resource "aws_security_group" "kernel_canary_task" {
  for_each = var.provision_services ? toset(["alto", "api", "reconciler"]) : toset([])

  name_prefix = "plether-sepolia-kernel-${each.key}-task-"
  description = "Isolated Kernel canary ${each.key} task"
  vpc_id      = data.aws_vpc.live[0].id
  lifecycle { create_before_destroy = true }
}

resource "aws_security_group" "kernel_canary_alto_alb" {
  count = local.kernel_canary_count

  name_prefix = "plether-sepolia-kernel-alto-alb-"
  description = "Internal load balancer for the patched Kernel v0.9 Alto"
  vpc_id      = data.aws_vpc.live[0].id
  lifecycle { create_before_destroy = true }
}

resource "aws_security_group" "kernel_canary_api_alto_client" {
  count = local.kernel_canary_count

  name_prefix = "plether-sepolia-kernel-api-alto-client-"
  description = "Allows only the canary API to call the canary Alto"
  vpc_id      = data.aws_vpc.live[0].id
  lifecycle { create_before_destroy = true }
}

resource "aws_vpc_security_group_ingress_rule" "kernel_canary_alto_alb_from_api" {
  count = local.kernel_canary_count

  security_group_id            = aws_security_group.kernel_canary_alto_alb[0].id
  referenced_security_group_id = aws_security_group.kernel_canary_api_alto_client[0].id
  ip_protocol                  = "tcp"
  from_port                    = 80
  to_port                      = 80
}

resource "aws_vpc_security_group_egress_rule" "kernel_canary_alto_alb_to_task" {
  count = local.kernel_canary_count

  security_group_id            = aws_security_group.kernel_canary_alto_alb[0].id
  referenced_security_group_id = aws_security_group.kernel_canary_task["alto"].id
  ip_protocol                  = "tcp"
  from_port                    = 3000
  to_port                      = 3000
}

resource "aws_vpc_security_group_ingress_rule" "kernel_canary_alto_from_alb" {
  count = local.kernel_canary_count

  security_group_id            = aws_security_group.kernel_canary_task["alto"].id
  referenced_security_group_id = aws_security_group.kernel_canary_alto_alb[0].id
  ip_protocol                  = "tcp"
  from_port                    = 3000
  to_port                      = 3000
}

resource "aws_vpc_security_group_egress_rule" "kernel_canary_https" {
  for_each = aws_security_group.kernel_canary_task

  security_group_id = each.value.id
  cidr_ipv4         = "0.0.0.0/0"
  ip_protocol       = "tcp"
  from_port         = 443
  to_port           = 443
}

resource "aws_vpc_security_group_ingress_rule" "kernel_canary_api_from_alb" {
  count = local.kernel_canary_count

  security_group_id            = aws_security_group.kernel_canary_task["api"].id
  referenced_security_group_id = data.aws_security_group.live_api_alb[0].id
  ip_protocol                  = "tcp"
  from_port                    = 3001
  to_port                      = 3001
}

resource "aws_vpc_security_group_egress_rule" "kernel_canary_api_to_alto" {
  count = local.kernel_canary_count

  security_group_id            = aws_security_group.kernel_canary_task["api"].id
  referenced_security_group_id = aws_security_group.kernel_canary_alto_alb[0].id
  ip_protocol                  = "tcp"
  from_port                    = 80
  to_port                      = 80
}

resource "aws_lb" "kernel_canary_alto" {
  count = local.kernel_canary_count

  name               = "plether-sepolia-kernel-alto"
  internal           = true
  load_balancer_type = "application"
  idle_timeout       = 65
  security_groups    = [aws_security_group.kernel_canary_alto_alb[0].id]
  subnets            = local.live_public_subnet_ids
}

resource "aws_lb_target_group" "kernel_canary_alto" {
  count = local.kernel_canary_count

  name                 = "plether-sepolia-kernel-alto"
  port                 = 3000
  protocol             = "HTTP"
  vpc_id               = data.aws_vpc.live[0].id
  target_type          = "ip"
  deregistration_delay = 30
  health_check {
    path                = "/health"
    healthy_threshold   = 2
    unhealthy_threshold = 3
    timeout             = 5
    interval            = 15
    matcher             = "200"
  }
}

resource "aws_lb_listener" "kernel_canary_alto" {
  count = local.kernel_canary_count

  load_balancer_arn = aws_lb.kernel_canary_alto[0].arn
  port              = 80
  protocol          = "HTTP"
  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.kernel_canary_alto[0].arn
  }
}

resource "aws_lb_target_group" "kernel_canary_api" {
  count = local.kernel_canary_count

  name                 = "plether-sepolia-kernel-api"
  port                 = 3001
  protocol             = "HTTP"
  vpc_id               = data.aws_vpc.live[0].id
  target_type          = "ip"
  deregistration_delay = 30
  health_check {
    path                = "/api/health"
    healthy_threshold   = 2
    unhealthy_threshold = 3
    timeout             = 10
    interval            = 30
    matcher             = "200"
  }
}

# The existing TLS certificate covers api.sepolia.plether.com. The Pages
# Worker rewrites /api/perps/v1/aa/kernel/rpc to this exact backend path.
resource "aws_lb_listener_rule" "kernel_canary_api" {
  count = local.kernel_canary_count == 1 && var.kernel_canary_route_enabled ? 1 : 0

  listener_arn = data.aws_lb_listener.live_api_https[0].arn
  priority     = 180
  action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.kernel_canary_api[0].arn
  }
  condition {
    path_pattern { values = ["/api/aa/kernel/rpc"] }
  }
  condition {
    host_header { values = [var.api_hostname] }
  }
  depends_on = [terraform_data.kernel_canary_guard]
}

resource "aws_ecs_task_definition" "kernel_canary_alto" {
  count = local.kernel_canary_count

  family                   = "plether-sepolia-kernel-alto"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = data.aws_ecs_task_definition.live_alto[0].cpu
  memory                   = data.aws_ecs_task_definition.live_alto[0].memory
  execution_role_arn       = aws_iam_role.kernel_canary_execution["alto"].arn
  task_role_arn            = aws_iam_role.kernel_canary_task["alto"].arn
  enable_fault_injection   = false
  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }
  volume { name = "alto-tmp" }

  container_definitions = jsonencode([
    merge(local.live_alto_container, {
      # Match the upstream start script on the read-only filesystem.
      entryPoint       = ["node", "/app/src/esm/cli/alto.js"]
      command          = ["run"]
      image            = var.kernel_canary_alto_image
      dependsOn        = [{ containerName = "alto-tmp-init", condition = "SUCCESS" }]
      logConfiguration = local.kernel_canary_awslogs
      secrets          = local.kernel_canary_alto_secrets
      environment      = local.kernel_canary_alto_environment
    }),
    merge(local.live_alto_init_container, {
      image = var.kernel_canary_alto_image
    }),
  ])
  depends_on = [
    aws_iam_role_policy.kernel_canary_execution_secrets,
    aws_iam_role_policy_attachment.kernel_canary_execution,
    terraform_data.kernel_canary_guard,
  ]
}

resource "aws_ecs_service" "kernel_canary_alto" {
  count = local.kernel_canary_count

  name                               = "plether-kernel-canary-alto"
  cluster                            = data.aws_ecs_cluster.live[0].arn
  task_definition                    = aws_ecs_task_definition.kernel_canary_alto[0].arn
  desired_count                      = var.kernel_canary_alto_desired_count
  launch_type                        = "FARGATE"
  platform_version                   = "1.4.0"
  health_check_grace_period_seconds  = 120
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100
  enable_ecs_managed_tags            = true
  propagate_tags                     = "SERVICE"
  deployment_circuit_breaker {
    enable   = true
    rollback = true
  }
  network_configuration {
    subnets          = local.live_public_subnet_ids
    security_groups  = [aws_security_group.kernel_canary_task["alto"].id]
    assign_public_ip = true
  }
  load_balancer {
    target_group_arn = aws_lb_target_group.kernel_canary_alto[0].arn
    container_name   = "plether-alto"
    container_port   = 3000
  }
  depends_on = [
    aws_lb_listener.kernel_canary_alto,
    aws_vpc_security_group_ingress_rule.kernel_canary_alto_alb_from_api,
    aws_vpc_security_group_egress_rule.kernel_canary_alto_alb_to_task,
    aws_vpc_security_group_ingress_rule.kernel_canary_alto_from_alb,
    aws_vpc_security_group_egress_rule.kernel_canary_https,
    terraform_data.kernel_canary_guard,
  ]
}

resource "aws_ecs_task_definition" "kernel_canary_api" {
  count = local.kernel_canary_count

  family                   = "plether-sepolia-kernel-api"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = data.aws_ecs_task_definition.live_api[0].cpu
  memory                   = data.aws_ecs_task_definition.live_api[0].memory
  execution_role_arn       = aws_iam_role.kernel_canary_execution["api"].arn
  task_role_arn            = aws_iam_role.kernel_canary_task["api"].arn
  enable_fault_injection   = false
  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }
  container_definitions = jsonencode([
    merge(local.live_api_container, {
      image            = var.kernel_canary_api_image
      logConfiguration = local.kernel_canary_awslogs
      secrets          = local.kernel_canary_api_secrets
      environment      = local.kernel_canary_api_environment
    }),
  ])
  depends_on = [
    aws_iam_role_policy.kernel_canary_execution_secrets,
    aws_iam_role_policy_attachment.kernel_canary_execution,
    aws_iam_role_policy.kernel_canary_paymaster_sign,
    terraform_data.kernel_canary_guard,
  ]
}

resource "aws_ecs_service" "kernel_canary_api" {
  count = local.kernel_canary_count

  name                               = "plether-kernel-canary-api"
  cluster                            = data.aws_ecs_cluster.live[0].arn
  task_definition                    = aws_ecs_task_definition.kernel_canary_api[0].arn
  desired_count                      = var.kernel_canary_api_desired_count
  launch_type                        = "FARGATE"
  platform_version                   = "1.4.0"
  health_check_grace_period_seconds  = var.kernel_canary_route_enabled ? 300 : null
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100
  enable_ecs_managed_tags            = true
  propagate_tags                     = "SERVICE"
  deployment_circuit_breaker {
    enable   = true
    rollback = true
  }
  network_configuration {
    subnets          = local.live_public_subnet_ids
    security_groups  = [aws_security_group.kernel_canary_task["api"].id, aws_security_group.kernel_canary_api_alto_client[0].id, data.aws_security_group.live_aa_db_client[0].id]
    assign_public_ip = true
  }
  # Keep the API task dark while the worker and wallet checks are prepared.
  # An ECS target group is attached only when the exact public path opens.
  dynamic "load_balancer" {
    for_each = var.kernel_canary_route_enabled ? [1] : []
    content {
      target_group_arn = aws_lb_target_group.kernel_canary_api[0].arn
      container_name   = "plether-api"
      container_port   = 3001
    }
  }
  depends_on = [
    aws_lb_listener_rule.kernel_canary_api,
    aws_vpc_security_group_ingress_rule.kernel_canary_api_from_alb,
    aws_vpc_security_group_egress_rule.kernel_canary_api_to_alto,
    terraform_data.kernel_canary_guard,
  ]
}

resource "aws_ecs_task_definition" "kernel_canary_reconciler" {
  count = local.kernel_canary_count

  family                   = "plether-sepolia-kernel-reconciler"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = data.aws_ecs_task_definition.live_reconciler[0].cpu
  memory                   = data.aws_ecs_task_definition.live_reconciler[0].memory
  execution_role_arn       = aws_iam_role.kernel_canary_execution["reconciler"].arn
  task_role_arn            = aws_iam_role.kernel_canary_task["reconciler"].arn
  enable_fault_injection   = false
  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }
  volume { name = "aa-reconciler-tmp" }
  container_definitions = jsonencode([
    merge(local.live_reconciler_container, {
      image            = var.kernel_canary_api_image
      logConfiguration = local.kernel_canary_awslogs
      dependsOn        = [{ containerName = "aa-reconciler-tmp-init", condition = "SUCCESS" }]
      secrets          = local.kernel_canary_reconciler_secrets
      environment = [
        { name = "HOME", value = "/tmp" },
        { name = "TMPDIR", value = "/tmp" },
        { name = "PERPS_CHAIN_ID", value = "421614" },
        { name = "AA_RPC_MODE", value = "dual-independent" },
        { name = "AA_KERNEL_PROFILE_JSON", value = var.kernel_canary_profile_json },
        { name = "AA_KERNEL_CANARY_OWNERS_JSON", value = var.kernel_canary_owners_json },
        { name = "AA_KERNEL_PUBLIC_ADMISSION", value = "false" },
        { name = "AA_KERNEL_SPONSORSHIP_JSON", value = var.kernel_canary_sponsorship_json },
        { name = "AA_KERNEL_SPONSORSHIP_ENABLED", value = tostring(var.kernel_canary_sponsorship_enabled) },
        { name = "AA_KERNEL_MAX_SAFE_LAG_SECONDS", value = var.kernel_canary_max_safe_lag_seconds },
        { name = "AA_KERNEL_RECONCILER_POLL_SECONDS", value = var.kernel_canary_poll_seconds },
        { name = "AA_KERNEL_RECONCILER_BATCH_BLOCKS", value = var.kernel_canary_batch_blocks },
        { name = "AA_KERNEL_RECONCILER_FAILURE_PAUSE_SECONDS", value = "60" },
      ]
    }),
    merge(local.live_reconciler_init_container, {
      image = var.kernel_canary_api_image
    }),
  ])
  depends_on = [
    aws_iam_role_policy.kernel_canary_execution_secrets,
    aws_iam_role_policy_attachment.kernel_canary_execution,
    terraform_data.kernel_canary_guard,
  ]
}

resource "aws_ecs_service" "kernel_canary_reconciler" {
  count = local.kernel_canary_count

  name                               = "plether-kernel-canary-reconciler"
  cluster                            = data.aws_ecs_cluster.live[0].arn
  task_definition                    = aws_ecs_task_definition.kernel_canary_reconciler[0].arn
  desired_count                      = var.kernel_canary_reconciler_desired_count
  launch_type                        = "FARGATE"
  platform_version                   = "1.4.0"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100
  enable_ecs_managed_tags            = true
  propagate_tags                     = "SERVICE"
  deployment_circuit_breaker {
    enable   = true
    rollback = true
  }
  network_configuration {
    subnets          = local.live_public_subnet_ids
    security_groups  = [aws_security_group.kernel_canary_task["reconciler"].id, data.aws_security_group.live_aa_db_client[0].id]
    assign_public_ip = true
  }
  depends_on = [terraform_data.kernel_canary_guard]
}
