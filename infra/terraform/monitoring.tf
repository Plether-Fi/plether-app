resource "aws_cloudwatch_log_metric_filter" "aa_sponsored_gas_alert" {
  count = var.provision_aa_proxy ? 1 : 0

  name           = "plether-${var.environment}-aa-sponsored-gas-alert"
  pattern        = "\"AA sponsored gas alert\""
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AaSponsoredGasThresholdReached"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "aa_sponsored_gas_alert" {
  count = var.provision_aa_proxy ? 1 : 0

  alarm_name          = "plether-${var.environment}-aa-sponsored-gas-alert"
  alarm_description   = "The backend observed sponsored UserOperation receipts above the configured hourly gas-cost threshold."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.aa_sponsored_gas_alert[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.aa_sponsored_gas_alert[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_metric_alarm" "keeper_task_missing" {
  count = var.provision_aa_proxy ? 1 : 0

  alarm_name          = "plether-${var.environment}-keeper-task-missing"
  alarm_description   = "The keeper ECS service stopped publishing task CPU metrics."
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = 2
  metric_name         = "CPUUtilization"
  namespace           = "AWS/ECS"
  period              = 60
  statistic           = "Maximum"
  threshold           = 0
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    ClusterName = aws_ecs_cluster.main.name
    ServiceName = var.consolidate_workers ? "plether-workers" : "plether-keeper"
  }
}
