locals {
  self_hosted_aa_resource_count = var.provision_self_hosted_aa && var.environment == "sepolia" ? 1 : 0
  native_aa_backend_configured  = local.self_hosted_aa_resource_count == 1 && var.configure_native_aa_backend
  native_aa_sponsorship_enabled = local.native_aa_backend_configured && var.enable_native_aa_sponsorship
  aa_gateway_enabled            = var.provision_aa_proxy || local.native_aa_backend_configured
  alto_ecr_image_tag            = "v1.2.7-arm64-28cee87ea6b5"

  alto_rpc_url_parameter_arn                  = "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.alto_rpc_url_ssm_parameter_name)}"
  alto_executor_keys_parameter_arn            = "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.alto_executor_private_keys_ssm_parameter_name)}"
  alto_utility_key_parameter_arn              = "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.alto_utility_private_key_ssm_parameter_name)}"
  alto_send_transaction_rpc_url_parameter_arn = trimspace(var.alto_send_transaction_rpc_url_ssm_parameter_name) == "" ? null : "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.alto_send_transaction_rpc_url_ssm_parameter_name)}"

  alto_allowed_rpc_methods = join(",", [
    "eth_supportedEntryPoints",
    "eth_estimateUserOperationGas",
    "eth_sendUserOperation",
    "eth_getUserOperationByHash",
    "eth_getUserOperationReceipt",
    "pimlico_getUserOperationGasPrice",
    "pimlico_getUserOperationStatus",
  ])

  alto_optional_secrets = local.alto_send_transaction_rpc_url_parameter_arn == null ? [] : [{
    name      = "ALTO_SEND_TRANSACTION_RPC_URL"
    valueFrom = local.alto_send_transaction_rpc_url_parameter_arn
  }]

  alto_log_router_container = merge(local.otel_log_router_container, {
    environment = [
      { name = "AWS_REGION", value = var.aws_region },
      { name = "CLOUDWATCH_LOG_GROUP", value = aws_cloudwatch_log_group.ecs.name },
      { name = "DEPLOYMENT_ENVIRONMENT", value = var.environment },
      { name = "ECS_CLUSTER_NAME", value = aws_ecs_cluster.main.name },
      { name = "SERVICE_VERSION", value = "alto-v1.2.7" },
    ]
  })
}

resource "aws_security_group" "api_alto_client" {
  count = local.self_hosted_aa_resource_count

  name_prefix = "plether-${var.environment}-api-alto-client-"
  description = "Marks the Plether API as the only permitted Alto RPC client."
  vpc_id      = aws_vpc.main.id

  lifecycle { create_before_destroy = true }
}

resource "aws_security_group" "alto_alb" {
  count = local.self_hosted_aa_resource_count

  name_prefix = "plether-${var.environment}-alto-alb-"
  description = "Internal load balancer for the self-hosted Alto bundler."
  vpc_id      = aws_vpc.main.id

  lifecycle { create_before_destroy = true }
}

resource "aws_vpc_security_group_ingress_rule" "alto_alb_from_api" {
  count = local.self_hosted_aa_resource_count

  security_group_id            = aws_security_group.alto_alb[0].id
  referenced_security_group_id = aws_security_group.api_alto_client[0].id
  description                  = "Alto RPC from the Plether API only."
  from_port                    = 80
  to_port                      = 80
  ip_protocol                  = "tcp"
}

resource "aws_security_group" "alto_task" {
  count = local.self_hosted_aa_resource_count

  name_prefix = "plether-${var.environment}-alto-task-"
  description = "Self-hosted Alto task; ingress is restricted to its internal load balancer."
  vpc_id      = aws_vpc.main.id

  lifecycle { create_before_destroy = true }
}

resource "aws_vpc_security_group_ingress_rule" "alto_task_from_alb" {
  count = local.self_hosted_aa_resource_count

  security_group_id            = aws_security_group.alto_task[0].id
  referenced_security_group_id = aws_security_group.alto_alb[0].id
  description                  = "RPC and health checks from the internal Alto ALB."
  from_port                    = 3000
  to_port                      = 3000
  ip_protocol                  = "tcp"
}

resource "aws_vpc_security_group_egress_rule" "alto_task_https" {
  count = local.self_hosted_aa_resource_count

  security_group_id = aws_security_group.alto_task[0].id
  description       = "HTTPS RPC, ECR, SSM, CloudWatch, and PostHog access."
  cidr_ipv4         = "0.0.0.0/0"
  from_port         = 443
  to_port           = 443
  ip_protocol       = "tcp"
}

resource "aws_vpc_security_group_egress_rule" "alto_alb_to_task" {
  count = local.self_hosted_aa_resource_count

  security_group_id            = aws_security_group.alto_alb[0].id
  referenced_security_group_id = aws_security_group.alto_task[0].id
  description                  = "Forward internal Alto requests to the ECS task."
  from_port                    = 3000
  to_port                      = 3000
  ip_protocol                  = "tcp"
}

resource "aws_lb" "alto" {
  count = local.self_hosted_aa_resource_count

  name               = "plether-${var.environment}-alto"
  internal           = true
  load_balancer_type = "application"
  idle_timeout       = 65
  security_groups    = [aws_security_group.alto_alb[0].id]
  subnets            = aws_subnet.public[*].id
}

resource "aws_lb_target_group" "alto" {
  count = local.self_hosted_aa_resource_count

  name                 = "plether-${var.environment}-alto"
  port                 = 3000
  protocol             = "HTTP"
  vpc_id               = aws_vpc.main.id
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

resource "aws_lb_listener" "alto" {
  count = local.self_hosted_aa_resource_count

  load_balancer_arn = aws_lb.alto[0].arn
  port              = 80
  protocol          = "HTTP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.alto[0].arn
  }
}

resource "aws_ecs_task_definition" "alto" {
  count = local.self_hosted_aa_resource_count

  family                   = "plether-${var.environment}-alto"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.alto_container_cpu
  memory                   = var.alto_container_memory
  execution_role_arn       = aws_iam_role.alto_execution[0].arn
  task_role_arn            = aws_iam_role.alto_task[0].arn
  enable_fault_injection   = false

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  volume {
    name = "alto-tmp"
  }

  container_definitions = jsonencode([
    {
      name                   = "plether-alto"
      image                  = "${aws_ecr_repository.alto[0].repository_url}:${local.alto_ecr_image_tag}"
      essential              = true
      readonlyRootFilesystem = true
      user                   = "1000:1000"
      stopTimeout            = 120
      entryPoint             = ["node", "/app/src/esm/cli/alto.js"]
      command                = ["run"]

      linuxParameters = {
        initProcessEnabled = true
        capabilities = {
          drop = ["ALL"]
        }
      }

      portMappings = [{
        name          = "alto-rpc"
        containerPort = 3000
        hostPort      = 3000
        protocol      = "tcp"
        appProtocol   = "http"
      }]

      mountPoints = [{
        sourceVolume  = "alto-tmp"
        containerPath = "/tmp"
        readOnly      = false
      }]

      systemControls = []
      volumesFrom    = []

      healthCheck = {
        command = [
          "CMD-SHELL",
          "node -e \"fetch('http://127.0.0.1:3000/health').then(async r=>{if(!r.ok||(await r.text())!=='OK')process.exit(1)}).catch(()=>process.exit(1))\"",
        ]
        interval    = 30
        timeout     = 5
        retries     = 3
        startPeriod = 90
      }

      dependsOn = [
        {
          containerName = "alto-tmp-init"
          condition     = "SUCCESS"
        },
        {
          containerName = "otel-log-router"
          condition     = "START"
        },
      ]

      logConfiguration = local.posthog_log_configuration

      secrets = concat([
        {
          name      = "ALTO_RPC_URL"
          valueFrom = local.alto_rpc_url_parameter_arn
        },
        {
          name      = "ALTO_EXECUTOR_PRIVATE_KEYS"
          valueFrom = local.alto_executor_keys_parameter_arn
        },
        {
          name      = "ALTO_UTILITY_PRIVATE_KEY"
          valueFrom = local.alto_utility_key_parameter_arn
        },
      ], local.alto_optional_secrets)

      environment = [
        { name = "ALTO_ENTRYPOINTS", value = var.alto_entrypoint_address },
        { name = "ALTO_DETERMINISTIC_DEPLOYER_ADDRESS", value = "0x4e59b44847b379578588920ca78fbf26c0b4956c" },
        { name = "ALTO_ENTRYPOINT_SIMULATION_CONTRACT_V8", value = var.alto_entrypoint_simulation_contract_v8 },
        { name = "ALTO_PIMLICO_SIMULATION_CONTRACT", value = var.alto_pimlico_simulation_contract },
        { name = "ALTO_DEPLOY_SIMULATIONS_CONTRACT", value = "false" },
        { name = "ALTO_CHAIN_TYPE", value = "arbitrum" },
        { name = "ALTO_PORT", value = "3000" },
        { name = "ALTO_API_VERSION", value = "v1" },
        { name = "ALTO_DEFAULT_API_VERSION", value = "v1" },
        { name = "ALTO_RPC_METHODS", value = local.alto_allowed_rpc_methods },
        { name = "ALTO_SAFE_MODE", value = "false" },
        { name = "ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION", value = "false" },
        { name = "ALTO_ENABLE_DEBUG_ENDPOINTS", value = "false" },
        { name = "ALTO_ENABLE_CORS", value = "false" },
        { name = "ALTO_WEBSOCKET", value = "false" },
        { name = "ALTO_JSON", value = "true" },
        { name = "ALTO_LOG_LEVEL", value = "info" },
        { name = "HOME", value = "/tmp" },
        { name = "TMPDIR", value = "/tmp" },
        { name = "ALTO_BUNDLE_MODE", value = "auto" },
        { name = "ALTO_EXPIRATION_CHECK", value = "true" },
        { name = "ALTO_FLUSH_STUCK_TRANSACTIONS_DURING_STARTUP", value = "false" },
        { name = "ALTO_ENABLE_HORIZONTAL_SCALING", value = "false" },
        { name = "ALTO_ENABLE_REDIS_RECEIPT_CACHE", value = "false" },
        { name = "ALTO_BLOCK_TIME", value = "1000" },
        { name = "ALTO_MAX_BLOCK_RANGE", value = "500" },
        { name = "ALTO_TIMEOUT", value = "60000" },
        { name = "ALTO_GAS_PRICE_REFRESH_INTERVAL", value = "5" },
        { name = "ALTO_ARBITRUM_BASE_FEE_MULTIPLIER", value = "150" },
        { name = "ALTO_RECEIPT_CACHE_TTL", value = "60000" },
        { name = "ALTO_BALANCE_OVERRIDE", value = "true" },
        { name = "ALTO_CODE_OVERRIDE_SUPPORT", value = "true" },
        { name = "ALTO_MAX_EXECUTORS", value = "4" },
        { name = "ALTO_MIN_EXECUTOR_BALANCE", value = var.alto_min_executor_balance_wei },
        { name = "ALTO_REFILLING_WALLETS", value = "true" },
        { name = "ALTO_UTILITY_WALLET_MONITOR", value = "true" },
        { name = "ALTO_UTILITY_WALLET_MONITOR_INTERVAL", value = "15000" },
        { name = "ALTO_EXECUTOR_REFILL_INTERVAL", value = "1200" },
        { name = "ALTO_MAX_GAS_PER_USER_OP", value = var.alto_max_gas_per_user_operation },
        { name = "ALTO_MAX_GAS_PER_BUNDLE", value = var.alto_max_gas_per_bundle },
      ]
    },
    {
      name                   = "alto-tmp-init"
      image                  = "${aws_ecr_repository.alto[0].repository_url}:${local.alto_ecr_image_tag}"
      essential              = false
      readonlyRootFilesystem = true
      user                   = "0:0"
      entryPoint             = ["/bin/sh", "-c"]
      command                = ["chmod 1777 /tmp"]

      linuxParameters = {
        initProcessEnabled = true
        capabilities = {
          drop = ["ALL"]
        }
      }

      mountPoints = [{
        sourceVolume  = "alto-tmp"
        containerPath = "/tmp"
        readOnly      = false
      }]
      portMappings   = []
      systemControls = []
      volumesFrom    = []
    },
    local.alto_log_router_container,
  ])

  depends_on = [
    aws_iam_role_policy.alto_execution_secrets,
    aws_iam_role_policy_attachment.alto_execution,
    aws_iam_role_policy.alto_task_firelens_cloudwatch,
    terraform_data.self_hosted_aa_guard,
  ]
}

resource "aws_ecs_service" "alto" {
  count = local.self_hosted_aa_resource_count

  name                               = "plether-alto"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.alto[0].arn
  desired_count                      = var.alto_desired_count
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
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.alto_task[0].id]
    assign_public_ip = true
  }

  load_balancer {
    target_group_arn = aws_lb_target_group.alto[0].arn
    container_name   = "plether-alto"
    container_port   = 3000
  }

  depends_on = [
    aws_lb_listener.alto,
    aws_vpc_security_group_ingress_rule.alto_alb_from_api,
    aws_vpc_security_group_ingress_rule.alto_task_from_alb,
    aws_vpc_security_group_egress_rule.alto_alb_to_task,
    aws_vpc_security_group_egress_rule.alto_task_https,
    terraform_data.self_hosted_aa_guard,
  ]

  lifecycle {
    ignore_changes = [task_definition]
  }
}
