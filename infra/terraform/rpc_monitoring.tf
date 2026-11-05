resource "aws_cloudwatch_log_metric_filter" "rpc_primary_health" {
  count          = local.rpc_count
  name           = "plether-sepolia-rpc-primary-health"
  log_group_name = aws_cloudwatch_log_group.rpc_gateway[0].name
  pattern        = "{ $.event = \"rpc_probe\" }"
  metric_transformation {
    name      = "SepoliaPrimaryUnhealthy"
    namespace = "Plether/RPC"
    value     = "$.primary_unhealthy"
  }
}

resource "aws_cloudwatch_metric_alarm" "rpc_primary_unhealthy" {
  count               = local.rpc_gateway_count
  alarm_name          = "plether-sepolia-rpc-primary-unhealthy"
  alarm_description   = "The primary is unhealthy or its gateway probes stopped. Check own-node head lag, parent access and gateway health; ordinary reads may be on Alchemy."
  namespace           = "Plether/RPC"
  metric_name         = aws_cloudwatch_log_metric_filter.rpc_primary_health[0].metric_transformation[0].name
  comparison_operator = "GreaterThanOrEqualToThreshold"
  threshold           = 6
  period              = 60
  evaluation_periods  = 1
  statistic           = "Sum"
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "rpc_backup_requests" {
  count          = local.rpc_count
  name           = "plether-sepolia-rpc-backup-requests"
  log_group_name = aws_cloudwatch_log_group.rpc_gateway[0].name
  pattern        = "{ $.event = \"rpc_request\" && $.upstream = \"backup\" }"
  metric_transformation {
    name      = "SepoliaBackupRequests"
    namespace = "Plether/RPC"
    value     = "1"
  }
}
