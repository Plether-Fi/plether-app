resource "aws_cloudwatch_metric_alarm" "incident_service_task_deficit" {
  for_each            = var.enable_incident_observer ? local.incident_service_names : toset([])
  alarm_name          = "plether-${local.deployment_name}-${each.key}-task-deficit"
  alarm_description   = "Independent ECS observation found fewer running tasks than desired for two consecutive minutes. Pending tasks do not represent available service capacity."
  namespace           = "Plether/Operations"
  metric_name         = "ServiceTaskDeficit"
  dimensions          = { ClusterName = aws_ecs_cluster.main.name, ServiceName = each.key }
  statistic           = "Maximum"
  comparison_operator = "GreaterThanThreshold"
  threshold           = 0
  period              = 60
  evaluation_periods  = 2
  datapoints_to_alarm = 2
  treat_missing_data  = "missing"
  alarm_actions       = local.incident_alarm_actions
}

resource "aws_cloudwatch_metric_alarm" "incident_observer_failed" {
  count               = local.incident_observer_count
  alarm_name          = "plether-${local.deployment_name}-incident-observer-failed"
  alarm_description   = "The independent service observer failed. Missing or partial service reads cannot be treated as healthy."
  namespace           = "AWS/Lambda"
  metric_name         = "Errors"
  dimensions          = { FunctionName = aws_lambda_function.incident_observer[0].function_name }
  statistic           = "Sum"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  threshold           = 1
  period              = 60
  evaluation_periods  = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = local.incident_alarm_actions
}

resource "aws_cloudwatch_metric_alarm" "incident_observer_missing" {
  count               = local.incident_observer_count
  alarm_name          = "plether-${local.deployment_name}-incident-observer-missing"
  alarm_description   = "The independent observer has not published a complete service observation for two minutes."
  namespace           = "Plether/Operations"
  metric_name         = "ServiceObservationCompleted"
  dimensions          = { ClusterName = aws_ecs_cluster.main.name }
  statistic           = "Sum"
  comparison_operator = "LessThanThreshold"
  threshold           = 1
  period              = 60
  evaluation_periods  = 2
  datapoints_to_alarm = 2
  treat_missing_data  = "breaching"
  alarm_actions       = local.incident_alarm_actions
}

resource "aws_cloudwatch_metric_alarm" "incident_event_delivery_failed" {
  for_each            = local.incident_event_rules
  alarm_name          = "plether-${local.deployment_name}-incident-${each.key}-delivery-failed"
  alarm_description   = "EventBridge could not invoke the incident observer; task-stop evidence or service observations may be missing."
  namespace           = "AWS/Events"
  metric_name         = "FailedInvocations"
  dimensions          = { RuleName = each.value }
  statistic           = "Sum"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  threshold           = 1
  period              = 60
  evaluation_periods  = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = local.incident_alarm_actions
}

resource "aws_cloudwatch_metric_alarm" "incident_observer_events_dropped" {
  count               = local.incident_observer_count
  alarm_name          = "plether-${local.deployment_name}-incident-observer-events-dropped"
  alarm_description   = "Lambda discarded accepted incident events after retry/age limits; task-stop evidence may be incomplete."
  namespace           = "AWS/Lambda"
  metric_name         = "AsyncEventsDropped"
  dimensions          = { FunctionName = aws_lambda_function.incident_observer[0].function_name }
  statistic           = "Sum"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  threshold           = 1
  period              = 60
  evaluation_periods  = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = local.incident_alarm_actions
}
