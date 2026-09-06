variable "protection_worker_desired_count" {
  type        = number
  default     = 0
  description = "Start one SL/TP indexer/worker after schema migration; execution defaults off."
  validation {
    condition     = contains([0, 1], var.protection_worker_desired_count)
    error_message = "The protection worker must have zero or one replica."
  }
}

variable "protection_worker_execution_enabled" {
  type    = bool
  default = false
}

variable "aa_protection_commits_enabled" {
  type    = bool
  default = false
}

variable "protection_worker_private_key" {
  type      = string
  sensitive = true
  default   = ""
  validation {
    condition     = var.protection_worker_private_key == "" || can(regex("^0x[0-9a-fA-F]{64}$", var.protection_worker_private_key))
    error_message = "Protection signer must be empty or a 0x-prefixed 32-byte private key."
  }
}

resource "aws_ssm_parameter" "protection_worker_private_key" {
  count = nonsensitive(var.protection_worker_private_key != "") ? 1 : 0
  name  = "/plether/${var.environment}/protection-worker-private-key"
  type  = "SecureString"
  value = var.protection_worker_private_key
  lifecycle {
    precondition {
      condition = (
        trimprefix(lower(var.protection_worker_private_key), "0x") != local.zero_private_key &&
        alltrue([for key in values(local.normalized_transaction_private_keys) : trimprefix(lower(var.protection_worker_private_key), "0x") != key])
      )
      error_message = "The protection signer must be nonzero and distinct from every other transaction signer."
    }
  }
}

resource "aws_ecs_task_definition" "protection_worker" {
  count                    = var.environment == "sepolia" ? 1 : 0
  family                   = "plether-${var.environment}-protection-worker"
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
  lifecycle {
    precondition {
      condition     = !var.protection_worker_execution_enabled || nonsensitive(var.protection_worker_private_key != "")
      error_message = "Execution requires a dedicated protection signer."
    }
    precondition {
      condition     = !var.aa_protection_commits_enabled || (var.protection_worker_execution_enabled && var.protection_worker_desired_count == 1)
      error_message = "Protection sponsorship requires an enabled running protection worker."
    }
  }
  container_definitions = jsonencode([{
    name             = "plether-position-protection-worker"
    image            = "${aws_ecr_repository.api.repository_url}:latest"
    essential        = true
    command          = ["node", "/app/protection/main.mjs"]
    logConfiguration = local.posthog_log_configuration
    secrets = concat([
      { name = "DATABASE_URL", valueFrom = aws_ssm_parameter.database_url.arn },
      { name = "PERPS_RPC_URL", valueFrom = aws_ssm_parameter.perps_rpc_url.arn },
      ], local.perps_rpc_auth_token_secret, nonsensitive(var.protection_worker_private_key != "") ? [
      { name = "PROTECTION_WORKER_PRIVATE_KEY", valueFrom = aws_ssm_parameter.protection_worker_private_key[0].arn }
    ] : [])
    environment = [
      { name = "PERPS_RELEASE_MANIFEST", value = "/app/config/perps/arbitrum-sepolia-v2.json" },
      { name = "PROTECTION_WORKER_EXECUTION_ENABLED", value = tostring(var.protection_worker_execution_enabled) },
      { name = "PROTECTION_WORKER_CONFIRMATIONS", value = "12" },
      { name = "KEEPER_MAX_BATCH_SIZE", value = var.keeper_max_batch_size },
      { name = "KEEPER_POLL_SECONDS", value = var.keeper_poll_seconds },
    ]
  }, local.otel_log_router_container])
}

resource "aws_ecs_service" "protection_worker" {
  count                              = var.environment == "sepolia" ? 1 : 0
  name                               = "plether-position-protection-worker"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.protection_worker[0].arn
  desired_count                      = var.protection_worker_desired_count
  launch_type                        = "FARGATE"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100
  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }
  lifecycle { ignore_changes = [task_definition] }
}

resource "aws_cloudwatch_log_metric_filter" "protection_failure" {
  name           = "plether-${var.environment}-protection-failure"
  log_group_name = aws_cloudwatch_log_group.ecs.name
  pattern        = "{ $.service = \"plether-position-protection-worker\" && $.level = \"error\" }"
  metric_transformation {
    name      = "ProtectionFailure"
    namespace = "Plether/${var.environment}"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "protection_failure" {
  count               = var.protection_worker_desired_count > 0 ? 1 : 0
  alarm_name          = "plether-${var.environment}-protection-failure"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = "ProtectionFailure"
  namespace           = "Plether/${var.environment}"
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "protection_heartbeat" {
  name           = "plether-${var.environment}-protection-heartbeat"
  log_group_name = aws_cloudwatch_log_group.ecs.name
  pattern        = "{ $.event = \"protection_worker_heartbeat\" && $.caughtUp = true }"
  metric_transformation {
    name      = "ProtectionHeartbeat"
    namespace = "Plether/${var.environment}"
    value     = "1"
  }
}

resource "aws_cloudwatch_log_metric_filter" "protection_degraded" {
  name           = "plether-${var.environment}-protection-degraded"
  log_group_name = aws_cloudwatch_log_group.ecs.name
  pattern        = "{ $.service = \"plether-position-protection-worker\" && $.level = \"warn\" }"
  metric_transformation {
    name      = "ProtectionDegraded"
    namespace = "Plether/${var.environment}"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "protection_degraded" {
  count               = var.protection_worker_desired_count > 0 ? 1 : 0
  alarm_name          = "plether-${var.environment}-protection-degraded"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 3
  metric_name         = "ProtectionDegraded"
  namespace           = "Plether/${var.environment}"
  period              = 60
  statistic           = "Sum"
  threshold           = 3
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_metric_alarm" "protection_heartbeat" {
  count               = var.protection_worker_desired_count > 0 ? 1 : 0
  alarm_name          = "plether-${var.environment}-protection-heartbeat"
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = 3
  metric_name         = "ProtectionHeartbeat"
  namespace           = "Plether/${var.environment}"
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}
