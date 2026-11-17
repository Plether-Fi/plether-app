# Opt in after packaging and qualifying the observer; it has no application,
# transaction, secret-store or database permissions.
variable "enable_incident_observer" {
  type        = bool
  default     = false
  description = "Enable sanitized ECS stop evidence and independent desired/running service checks. Requires the reviewed observer package and an operations notification topic."
}

variable "incident_observer_reserved_concurrent_executions" {
  type        = number
  default     = -1
  description = "Use -1 for the account's unreserved Lambda concurrency pool (default), or explicitly reserve one or two executions after verifying account quota. Zero is prohibited because it disables the observer."
  validation {
    condition     = contains([-1, 1, 2], var.incident_observer_reserved_concurrent_executions)
    error_message = "incident_observer_reserved_concurrent_executions must be -1 (unreserved), 1 or 2; zero would disable incident observation."
  }
}

variable "incident_observer_package_path" {
  type        = string
  default     = ""
  description = "Absolute path to the deterministic zip produced by scripts/package-incident-observer.py. The source hash is recorded in the saved Terraform plan."
  validation {
    condition     = var.incident_observer_package_path == "" || (startswith(var.incident_observer_package_path, "/") && endswith(var.incident_observer_package_path, ".zip"))
    error_message = "incident_observer_package_path must be empty or an absolute .zip path."
  }
}

variable "incident_escalation_sns_topic_arn" {
  type        = string
  default     = ""
  description = "Optional existing SNS topic connected to an operator-owned pager with acknowledgement/escalation. Terraform does not create or assert that external routing."
  validation {
    condition     = var.incident_escalation_sns_topic_arn == "" || can(regex("^arn:aws:sns:[a-z0-9-]+:[0-9]{12}:[A-Za-z0-9_-]+$", var.incident_escalation_sns_topic_arn))
    error_message = "incident_escalation_sns_topic_arn must be empty or a valid standard SNS topic ARN."
  }
}

locals {
  incident_observer_count = var.enable_incident_observer ? 1 : 0
  incident_observer_name  = "plether-${local.deployment_name}-incident-observer"
  incident_service_names = toset(concat([
    aws_ecs_service.api.name, aws_ecs_service.keeper.name,
    aws_ecs_service.liquidation_worker.name, aws_ecs_service.basket_worker.name,
    aws_ecs_service.perps_indexer.name, aws_ecs_service.insights_worker.name,
    ], aws_ecs_service.workers[*].name, aws_ecs_service.aa_reconciler[*].name,
    aws_ecs_service.protection_worker[*].name, aws_ecs_service.alto[*].name,
  aws_ecs_service.rpc_gateway[*].name))
  incident_service_arns  = [for name in local.incident_service_names : "arn:aws:ecs:${var.aws_region}:${data.aws_caller_identity.current.account_id}:service/${aws_ecs_cluster.main.name}/${name}"]
  incident_alarm_actions = compact([var.operations_alarm_sns_topic_arn, var.incident_escalation_sns_topic_arn])
}

resource "terraform_data" "incident_observer_guard" {
  count = local.incident_observer_count
  input = {
    package = var.incident_observer_package_path
    topic   = var.operations_alarm_sns_topic_arn
  }
  lifecycle {
    precondition {
      condition     = var.incident_observer_package_path != "" && can(filebase64sha256(var.incident_observer_package_path))
      error_message = "Package the reviewed incident observer before planning an enabled deployment."
    }
    precondition {
      condition     = can(regex("^arn:aws:sns:${var.aws_region}:${data.aws_caller_identity.current.account_id}:[A-Za-z0-9_-]+$", var.operations_alarm_sns_topic_arn))
      error_message = "The incident observer requires an operations SNS topic in this deployment's account and region."
    }
    precondition {
      condition     = var.incident_escalation_sns_topic_arn == "" || can(regex("^arn:aws:sns:${var.aws_region}:${data.aws_caller_identity.current.account_id}:[A-Za-z0-9_-]+$", var.incident_escalation_sns_topic_arn))
      error_message = "The optional incident escalation topic must be in this deployment's account and region."
    }
  }
}

resource "aws_cloudwatch_log_group" "ecs_incident_evidence" {
  count             = local.incident_observer_count
  name              = "/plether/${local.deployment_name}/ecs-incidents"
  retention_in_days = 90
}

resource "aws_cloudwatch_log_group" "incident_observer_runtime" {
  count             = local.incident_observer_count
  name              = "/aws/lambda/${local.incident_observer_name}"
  retention_in_days = 90
}

resource "aws_iam_role" "incident_observer" {
  count = local.incident_observer_count
  name  = local.incident_observer_name
  assume_role_policy = jsonencode({
    Version   = "2012-10-17"
    Statement = [{ Effect = "Allow", Action = "sts:AssumeRole", Principal = { Service = "lambda.amazonaws.com" } }]
  })
}

resource "aws_iam_role_policy" "incident_observer" {
  count = local.incident_observer_count
  role  = aws_iam_role.incident_observer[0].id
  name  = "read-service-state-and-write-sanitized-evidence"
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      { Effect = "Allow", Action = ["ecs:DescribeServices"], Resource = local.incident_service_arns },
      { Effect = "Allow", Action = ["logs:CreateLogStream", "logs:PutLogEvents"], Resource = ["${aws_cloudwatch_log_group.ecs_incident_evidence[0].arn}:*", "${aws_cloudwatch_log_group.incident_observer_runtime[0].arn}:*"] },
      { Effect = "Allow", Action = ["cloudwatch:PutMetricData"], Resource = "*", Condition = { StringEquals = { "cloudwatch:namespace" = "Plether/Operations" } } },
      { Effect = "Allow", Action = ["sns:Publish"], Resource = local.incident_alarm_actions },
    ]
  })
}

resource "aws_lambda_function" "incident_observer" {
  count                          = local.incident_observer_count
  depends_on                     = [terraform_data.incident_observer_guard, aws_iam_role_policy.incident_observer, aws_cloudwatch_log_group.incident_observer_runtime]
  function_name                  = local.incident_observer_name
  role                           = aws_iam_role.incident_observer[0].arn
  filename                       = var.incident_observer_package_path
  source_code_hash               = try(filebase64sha256(var.incident_observer_package_path), null)
  runtime                        = "python3.13"
  handler                        = "handler.handler"
  architectures                  = ["arm64"]
  timeout                        = 25
  memory_size                    = 128
  reserved_concurrent_executions = var.incident_observer_reserved_concurrent_executions
  environment {
    variables = {
      EXPECTED_ACCOUNT_ID  = data.aws_caller_identity.current.account_id
      ECS_CLUSTER_ARN      = aws_ecs_cluster.main.arn
      SERVICE_NAMES        = jsonencode(sort(tolist(local.incident_service_names)))
      EVIDENCE_LOG_GROUP   = aws_cloudwatch_log_group.ecs_incident_evidence[0].name
      OPERATIONS_TOPIC_ARN = var.operations_alarm_sns_topic_arn
      ESCALATION_TOPIC_ARN = var.incident_escalation_sns_topic_arn
    }
  }
}

resource "aws_cloudwatch_event_rule" "ecs_task_stopped" {
  count = local.incident_observer_count
  name  = "plether-${local.deployment_name}-ecs-task-stopped"
  event_pattern = jsonencode({
    source        = ["aws.ecs"]
    "detail-type" = ["ECS Task State Change"]
    account       = [data.aws_caller_identity.current.account_id]
    detail = {
      clusterArn = [aws_ecs_cluster.main.arn]
      group      = [for name in local.incident_service_names : "service:${name}"]
      lastStatus = ["STOPPED"]
    }
  })
}

resource "aws_cloudwatch_event_rule" "incident_service_poll" {
  count               = local.incident_observer_count
  name                = "plether-${local.deployment_name}-incident-service-poll"
  schedule_expression = "rate(1 minute)"
}

locals {
  incident_event_rules = var.enable_incident_observer ? {
    stopped = aws_cloudwatch_event_rule.ecs_task_stopped[0].name
    poll    = aws_cloudwatch_event_rule.incident_service_poll[0].name
  } : {}
}

resource "aws_cloudwatch_event_target" "incident_observer" {
  for_each = local.incident_event_rules
  rule     = each.value
  arn      = aws_lambda_function.incident_observer[0].arn
  retry_policy {
    maximum_event_age_in_seconds = 3600
    maximum_retry_attempts       = 2
  }
}

resource "aws_lambda_permission" "incident_observer_events" {
  for_each       = local.incident_event_rules
  statement_id   = "AllowEvents-${each.key}"
  action         = "lambda:InvokeFunction"
  function_name  = aws_lambda_function.incident_observer[0].function_name
  principal      = "events.amazonaws.com"
  source_account = data.aws_caller_identity.current.account_id
  source_arn     = each.key == "stopped" ? aws_cloudwatch_event_rule.ecs_task_stopped[0].arn : aws_cloudwatch_event_rule.incident_service_poll[0].arn
}

# No failure destination receives raw ECS events. Dropped async invocations are
# surfaced through a metric alarm instead of forwarding unsanitized payloads.
resource "aws_lambda_function_event_invoke_config" "incident_observer" {
  count                        = local.incident_observer_count
  function_name                = aws_lambda_function.incident_observer[0].function_name
  maximum_event_age_in_seconds = 3600
  maximum_retry_attempts       = 1
}
