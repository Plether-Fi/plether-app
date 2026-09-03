locals {
  aa_reconciler_secondary_rpc_url_parameter_arn = "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter${trimspace(var.aa_reconciler_secondary_rpc_url_ssm_parameter_name)}"

  aa_reconciler_log_router_container = merge(local.otel_log_router_container, {
    environment = [
      { name = "AWS_REGION", value = var.aws_region },
      { name = "CLOUDWATCH_LOG_GROUP", value = aws_cloudwatch_log_group.ecs.name },
      { name = "DEPLOYMENT_ENVIRONMENT", value = var.environment },
      { name = "ECS_CLUSTER_NAME", value = aws_ecs_cluster.main.name },
      { name = "SERVICE_VERSION", value = "unknown" },
    ]
  })
}

resource "aws_security_group" "aa_reconciler_task" {
  count = local.self_hosted_aa_resource_count

  name_prefix = "plether-${var.environment}-aa-reconciler-"
  description = "Native-AA reconciler with only PostgreSQL and HTTPS egress."
  vpc_id      = aws_vpc.main.id

  lifecycle { create_before_destroy = true }
}

resource "aws_vpc_security_group_egress_rule" "aa_reconciler_https" {
  count = local.self_hosted_aa_resource_count

  security_group_id = aws_security_group.aa_reconciler_task[0].id
  description       = "Arbitrum RPC, ECR, SSM, CloudWatch, and PostHog HTTPS access."
  cidr_ipv4         = "0.0.0.0/0"
  from_port         = 443
  to_port           = 443
  ip_protocol       = "tcp"
}

resource "aws_vpc_security_group_egress_rule" "aa_reconciler_postgres" {
  count = local.self_hosted_aa_resource_count

  security_group_id            = aws_security_group.aa_reconciler_task[0].id
  referenced_security_group_id = aws_security_group.rds.id
  description                  = "Persist reconciliation cursor and reservation state."
  from_port                    = 5432
  to_port                      = 5432
  ip_protocol                  = "tcp"
}

resource "aws_iam_role" "aa_reconciler_execution" {
  count = local.self_hosted_aa_resource_count

  name = "plether-${var.environment}-aa-reconciler-execution"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy_attachment" "aa_reconciler_execution" {
  count = local.self_hosted_aa_resource_count

  role       = aws_iam_role.aa_reconciler_execution[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

resource "aws_iam_role_policy" "aa_reconciler_execution_secrets" {
  count = local.self_hosted_aa_resource_count

  name = "exact-ssm-secret-access"
  role = aws_iam_role.aa_reconciler_execution[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Action = [
        "ssm:GetParameters",
        "ssm:GetParameter"
      ]
      Resource = [
        aws_ssm_parameter.database_url.arn,
        aws_ssm_parameter.perps_rpc_url.arn,
        local.aa_reconciler_secondary_rpc_url_parameter_arn,
        aws_ssm_parameter.posthog_otlp_authorization_header.arn,
      ]
    }]
  })
}

resource "aws_iam_role_policy" "aa_reconciler_execution_secondary_rpc_kms" {
  count = local.self_hosted_aa_resource_count == 1 && var.aa_reconciler_secondary_rpc_url_kms_key_arn != "" ? 1 : 0

  name = "aa-reconciler-secondary-rpc-kms-decrypt"
  role = aws_iam_role.aa_reconciler_execution[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Sid      = "DecryptExactReconcilerSecondaryRpcParameter"
      Effect   = "Allow"
      Action   = ["kms:Decrypt"]
      Resource = var.aa_reconciler_secondary_rpc_url_kms_key_arn
      Condition = {
        StringEquals = {
          "kms:ViaService"                      = "ssm.${var.aws_region}.amazonaws.com"
          "kms:EncryptionContext:PARAMETER_ARN" = local.aa_reconciler_secondary_rpc_url_parameter_arn
        }
      }
    }]
  })
}

resource "aws_iam_role" "aa_reconciler_task" {
  count = local.self_hosted_aa_resource_count

  name = "plether-${var.environment}-aa-reconciler-task"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy" "aa_reconciler_task_firelens_cloudwatch" {
  count = local.self_hosted_aa_resource_count

  name = "firelens-cloudwatch-logs"
  role = aws_iam_role.aa_reconciler_task[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Action = [
        "logs:CreateLogStream",
        "logs:DescribeLogStreams",
        "logs:PutLogEvents"
      ]
      Resource = "${aws_cloudwatch_log_group.ecs.arn}:*"
    }]
  })
}

resource "aws_ecs_task_definition" "aa_reconciler" {
  count = local.self_hosted_aa_resource_count

  family                   = "plether-${var.environment}-aa-reconciler"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.aa_reconciler_container_cpu
  memory                   = var.aa_reconciler_container_memory
  execution_role_arn       = aws_iam_role.aa_reconciler_execution[0].arn
  task_role_arn            = aws_iam_role.aa_reconciler_task[0].arn
  enable_fault_injection   = false

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  volume {
    name = "aa-reconciler-tmp"
  }

  container_definitions = jsonencode([
    {
      name                   = "plether-aa-reconciler"
      image                  = "${aws_ecr_repository.api.repository_url}:latest"
      essential              = true
      command                = ["/usr/local/bin/plether-aa-reconciler"]
      readonlyRootFilesystem = true
      user                   = "65534:65534"
      stopTimeout            = 120

      dependsOn = [
        {
          containerName = "aa-reconciler-tmp-init"
          condition     = "SUCCESS"
        },
        {
          containerName = "otel-log-router"
          condition     = "START"
        },
      ]

      linuxParameters = {
        initProcessEnabled = true
        capabilities = {
          drop = ["ALL"]
        }
      }

      mountPoints = [{
        sourceVolume  = "aa-reconciler-tmp"
        containerPath = "/tmp"
        readOnly      = false
      }]
      portMappings   = []
      systemControls = []
      volumesFrom    = []

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
          name      = "AA_RECONCILER_SECONDARY_RPC_URL"
          valueFrom = local.aa_reconciler_secondary_rpc_url_parameter_arn
        },
      ]

      environment = [
        { name = "HOME", value = "/tmp" },
        { name = "TMPDIR", value = "/tmp" },
        { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
        { name = "AA_PAYMASTER_ADDRESS", value = var.aa_paymaster_address },
        { name = "AA_PAYMASTER_CODE_HASH", value = var.aa_paymaster_code_hash },
        { name = "AA_RECONCILER_START_BLOCK", value = var.aa_reconciler_start_block },
        { name = "AA_RECONCILER_START_BLOCK_HASH", value = var.aa_reconciler_start_block_hash },
        { name = "AA_RECONCILER_POLL_SECONDS", value = var.aa_reconciler_poll_seconds },
        { name = "AA_RECONCILER_BATCH_BLOCKS", value = var.aa_reconciler_batch_blocks },
        { name = "AA_RECONCILER_MAX_SAFE_LAG_SECONDS", value = var.aa_reconciler_max_safe_lag_seconds },
        { name = "AA_PAYMASTER_MIN_DEPOSIT_WEI", value = var.aa_paymaster_min_deposit_wei },
      ]
    },
    {
      name                   = "aa-reconciler-tmp-init"
      image                  = "${aws_ecr_repository.api.repository_url}:latest"
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
        sourceVolume  = "aa-reconciler-tmp"
        containerPath = "/tmp"
        readOnly      = false
      }]
      portMappings   = []
      systemControls = []
      volumesFrom    = []
      environment    = []
      secrets        = []
    },
    local.aa_reconciler_log_router_container,
  ])

  depends_on = [
    aws_iam_role_policy.aa_reconciler_execution_secondary_rpc_kms,
    aws_iam_role_policy.aa_reconciler_execution_secrets,
    aws_iam_role_policy_attachment.aa_reconciler_execution,
    aws_iam_role_policy.aa_reconciler_task_firelens_cloudwatch,
    aws_vpc_security_group_egress_rule.aa_reconciler_https,
    aws_vpc_security_group_egress_rule.aa_reconciler_postgres,
    terraform_data.self_hosted_aa_guard,
  ]
}

resource "aws_ecs_service" "aa_reconciler" {
  count = local.self_hosted_aa_resource_count

  name                               = "plether-aa-reconciler"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.aa_reconciler[0].arn
  desired_count                      = var.aa_reconciler_desired_count
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
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.aa_reconciler_task[0].id]
    assign_public_ip = true
  }

  depends_on = [terraform_data.self_hosted_aa_guard]

  lifecycle {
    ignore_changes = [task_definition]
  }
}
