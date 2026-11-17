# A separate deployment of the reviewed observer watches only the nonfinancial
# fixture. It has no application, transaction, secret-store or database access.
locals {
  observer_name          = "${local.name}-observer"
  observer_service_name  = local.service_name
  observer_service_arn   = "arn:aws:ecs:${var.aws_region}:${data.aws_caller_identity.current.account_id}:service/${aws_ecs_cluster.drill.name}/${local.observer_service_name}"
  observer_alarm_actions = [var.operations_alarm_sns_topic_arn, aws_sns_topic.drill_notifications.arn]
  observer_event_rules = {
    stopped = aws_cloudwatch_event_rule.fixture_stopped.name
    poll    = aws_cloudwatch_event_rule.observer_poll.name
  }
}

resource "terraform_data" "observer_guard" {
  input = {
    package = var.observer_package_path
    topic   = var.operations_alarm_sns_topic_arn
  }
  lifecycle {
    precondition {
      condition     = startswith(var.observer_package_path, "/") && endswith(var.observer_package_path, ".zip") && can(filebase64sha256(var.observer_package_path))
      error_message = "Package the reviewed observer and supply its absolute .zip path before planning the drill."
    }
    precondition {
      condition     = can(regex("^arn:aws:sns:${var.aws_region}:${data.aws_caller_identity.current.account_id}:[A-Za-z0-9_-]+$", var.operations_alarm_sns_topic_arn))
      error_message = "The drill observer requires a notification topic in this deployment's account and region."
    }
  }
}

# Keep the evidence after fixture teardown; CloudWatch expires it after 90 days.
resource "aws_cloudwatch_log_group" "observer_evidence" {
  name              = "/plether/${trimprefix(local.name, "plether-")}/ecs-incidents"
  retention_in_days = 90
  skip_destroy      = true
}

resource "aws_cloudwatch_log_group" "observer_runtime" {
  name              = "/aws/lambda/${local.observer_name}"
  retention_in_days = 90
  skip_destroy      = true
}

resource "aws_iam_role" "observer" {
  name = local.observer_name
  assume_role_policy = jsonencode({
    Version   = "2012-10-17"
    Statement = [{ Effect = "Allow", Action = "sts:AssumeRole", Principal = { Service = "lambda.amazonaws.com" } }]
  })
}

resource "aws_iam_role_policy" "observer" {
  role = aws_iam_role.observer.id
  name = "observe-only-isolated-fixture"
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      { Effect = "Allow", Action = ["ecs:DescribeServices"], Resource = [local.observer_service_arn] },
      { Effect = "Allow", Action = ["logs:CreateLogStream", "logs:PutLogEvents"], Resource = ["${aws_cloudwatch_log_group.observer_evidence.arn}:*", "${aws_cloudwatch_log_group.observer_runtime.arn}:*"] },
      { Effect = "Allow", Action = ["cloudwatch:PutMetricData"], Resource = "*", Condition = { StringEquals = { "cloudwatch:namespace" = "Plether/Operations" } } },
      { Effect = "Allow", Action = ["sns:Publish"], Resource = local.observer_alarm_actions },
    ]
  })
}

resource "aws_lambda_function" "observer" {
  depends_on                     = [terraform_data.observer_guard, aws_iam_role_policy.observer, aws_cloudwatch_log_group.observer_runtime]
  function_name                  = local.observer_name
  role                           = aws_iam_role.observer.arn
  filename                       = var.observer_package_path
  source_code_hash               = try(filebase64sha256(var.observer_package_path), null)
  runtime                        = "python3.13"
  handler                        = "handler.handler"
  architectures                  = ["arm64"]
  timeout                        = 25
  memory_size                    = 128
  reserved_concurrent_executions = -1
  environment {
    variables = {
      EXPECTED_ACCOUNT_ID  = data.aws_caller_identity.current.account_id
      ECS_CLUSTER_ARN      = aws_ecs_cluster.drill.arn
      SERVICE_NAMES        = jsonencode([local.observer_service_name])
      EVIDENCE_LOG_GROUP   = aws_cloudwatch_log_group.observer_evidence.name
      OPERATIONS_TOPIC_ARN = var.operations_alarm_sns_topic_arn
      ESCALATION_TOPIC_ARN = aws_sns_topic.drill_notifications.arn
    }
  }
}

resource "aws_cloudwatch_event_rule" "fixture_stopped" {
  name = "${local.name}-task-stopped"
  event_pattern = jsonencode({
    source        = ["aws.ecs"]
    "detail-type" = ["ECS Task State Change"]
    account       = [data.aws_caller_identity.current.account_id]
    detail = {
      clusterArn = [aws_ecs_cluster.drill.arn]
      group      = ["service:${local.observer_service_name}"]
      lastStatus = ["STOPPED"]
    }
  })
}

resource "aws_cloudwatch_event_rule" "observer_poll" {
  name                = "${local.name}-service-poll"
  schedule_expression = "rate(1 minute)"
}

resource "aws_cloudwatch_event_target" "observer" {
  for_each   = local.observer_event_rules
  depends_on = [aws_lambda_permission.observer_events]
  rule       = each.value
  arn        = aws_lambda_function.observer.arn
  retry_policy {
    # Drill-only bounds let real delivery-failure evidence settle promptly.
    maximum_event_age_in_seconds = 60
    maximum_retry_attempts       = 0
  }
}

resource "aws_lambda_permission" "observer_events" {
  for_each       = local.observer_event_rules
  statement_id   = "AllowEvents-${each.key}"
  action         = "lambda:InvokeFunction"
  function_name  = aws_lambda_function.observer.function_name
  principal      = "events.amazonaws.com"
  source_account = data.aws_caller_identity.current.account_id
  source_arn     = each.key == "stopped" ? aws_cloudwatch_event_rule.fixture_stopped.arn : aws_cloudwatch_event_rule.observer_poll.arn
}

# Raw ECS events are never forwarded to a failure destination.
resource "aws_lambda_function_event_invoke_config" "observer" {
  function_name = aws_lambda_function.observer.function_name
  # Production retains its longer retry window; this fixture ages out quickly.
  maximum_event_age_in_seconds = 60
  maximum_retry_attempts       = 1
}

resource "aws_cloudwatch_metric_alarm" "fixture_task_deficit" {
  alarm_name          = "${local.name}-task-deficit"
  alarm_description   = "The isolated fixture has fewer running tasks than desired for two consecutive minutes. Pending tasks do not represent available capacity."
  namespace           = "Plether/Operations"
  metric_name         = "ServiceTaskDeficit"
  dimensions          = { ClusterName = aws_ecs_cluster.drill.name, ServiceName = local.observer_service_name }
  statistic           = "Maximum"
  comparison_operator = "GreaterThanThreshold"
  threshold           = 0
  period              = 60
  evaluation_periods  = 2
  datapoints_to_alarm = 2
  treat_missing_data  = "missing"
  alarm_actions       = local.observer_alarm_actions
}

resource "aws_cloudwatch_metric_alarm" "observer_failed" {
  alarm_name          = "${local.name}-observer-failed"
  alarm_description   = "The isolated observer failed. Missing or partial service reads cannot be treated as healthy."
  namespace           = "AWS/Lambda"
  metric_name         = "Errors"
  dimensions          = { FunctionName = aws_lambda_function.observer.function_name }
  statistic           = "Sum"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  threshold           = 1
  period              = 60
  evaluation_periods  = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = local.observer_alarm_actions
}

resource "aws_cloudwatch_metric_alarm" "observer_missing" {
  alarm_name          = "${local.name}-observer-missing"
  alarm_description   = "The isolated observer has not published a complete service observation for two minutes."
  namespace           = "Plether/Operations"
  metric_name         = "ServiceObservationCompleted"
  dimensions          = { ClusterName = aws_ecs_cluster.drill.name }
  statistic           = "Sum"
  comparison_operator = "LessThanThreshold"
  threshold           = 1
  period              = 60
  evaluation_periods  = 2
  datapoints_to_alarm = 2
  treat_missing_data  = "breaching"
  alarm_actions       = local.observer_alarm_actions
}

resource "aws_cloudwatch_metric_alarm" "observer_event_delivery_failed" {
  for_each            = local.observer_event_rules
  alarm_name          = "${local.name}-${each.key}-delivery-failed"
  alarm_description   = "EventBridge could not invoke the isolated observer; fixture task-stop evidence or observations may be missing."
  namespace           = "AWS/Events"
  metric_name         = "FailedInvocations"
  dimensions          = { RuleName = each.value }
  statistic           = "Sum"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  threshold           = 1
  period              = 60
  evaluation_periods  = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = local.observer_alarm_actions
}

resource "aws_cloudwatch_metric_alarm" "observer_events_dropped" {
  alarm_name          = "${local.name}-observer-events-dropped"
  alarm_description   = "Lambda discarded accepted fixture events after retry/age limits; task-stop evidence may be incomplete."
  namespace           = "AWS/Lambda"
  metric_name         = "AsyncEventsDropped"
  dimensions          = { FunctionName = aws_lambda_function.observer.function_name }
  statistic           = "Sum"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  threshold           = 1
  period              = 60
  evaluation_periods  = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = local.observer_alarm_actions
}
