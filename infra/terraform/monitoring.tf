resource "aws_cloudwatch_log_metric_filter" "aa_sponsored_gas_alert" {
  count = var.provision_aa_proxy ? 1 : 0

  depends_on = [terraform_data.perps_candle_rollout_guard]

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

  depends_on = [terraform_data.perps_candle_rollout_guard]

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

  depends_on = [terraform_data.perps_candle_rollout_guard]

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

resource "aws_cloudwatch_metric_alarm" "rds_cpu_high" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-rds-cpu-high"
  alarm_description   = "RDS CPU has remained above 80%, which can starve candle ingestion, backfill, and API reads."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  metric_name         = "CPUUtilization"
  namespace           = "AWS/RDS"
  period              = 300
  statistic           = "Average"
  threshold           = 80
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.postgres.identifier
  }
}

resource "aws_cloudwatch_metric_alarm" "rds_cpu_credits_low" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-rds-cpu-credits-low"
  alarm_description   = "Burstable RDS CPU credits are low; pause candle backfills before foreground latency degrades."
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = 2
  metric_name         = "CPUCreditBalance"
  namespace           = "AWS/RDS"
  period              = 300
  statistic           = "Minimum"
  threshold           = 20
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.postgres.identifier
  }
}

resource "aws_cloudwatch_metric_alarm" "rds_free_storage_low" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-rds-free-storage-low"
  alarm_description   = "RDS free storage is below the safe rollup/backfill headroom."
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = 2
  metric_name         = "FreeStorageSpace"
  namespace           = "AWS/RDS"
  period              = 300
  statistic           = "Minimum"
  threshold           = var.rds_free_storage_alarm_bytes
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.postgres.identifier
  }
}

resource "aws_cloudwatch_metric_alarm" "rds_freeable_memory_low" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-rds-freeable-memory-low"
  alarm_description   = "RDS freeable memory is low; cold candle queries and backfill work may evict useful pages."
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = 3
  metric_name         = "FreeableMemory"
  namespace           = "AWS/RDS"
  period              = 300
  statistic           = "Minimum"
  threshold           = var.rds_freeable_memory_alarm_bytes
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.postgres.identifier
  }
}

resource "aws_cloudwatch_metric_alarm" "rds_connections_high" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-rds-connections-high"
  alarm_description   = "RDS connection usage is above the configured safe threshold."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = "DatabaseConnections"
  namespace           = "AWS/RDS"
  period              = 300
  statistic           = "Maximum"
  threshold           = var.rds_database_connections_alarm_threshold
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.postgres.identifier
  }
}

resource "aws_cloudwatch_metric_alarm" "rds_io_latency_high" {
  for_each = toset(["ReadLatency", "WriteLatency"])

  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-rds-${lower(each.key)}-high"
  alarm_description   = "RDS ${each.key} has remained above 20ms; pause candle backfills and inspect I/O pressure."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  metric_name         = each.key
  namespace           = "AWS/RDS"
  period              = 300
  statistic           = "Average"
  threshold           = 0.02
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.postgres.identifier
  }
}

resource "aws_cloudwatch_metric_alarm" "alb_target_latency_high" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-api-p95-latency-high"
  alarm_description   = "API target p95 latency is above the candle rollout SLO."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  metric_name         = "TargetResponseTime"
  namespace           = "AWS/ApplicationELB"
  period              = 300
  extended_statistic  = "p95"
  threshold           = 0.75
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    LoadBalancer = aws_lb.api.arn_suffix
    TargetGroup  = aws_lb_target_group.api.arn_suffix
  }
}

resource "aws_cloudwatch_metric_alarm" "alb_target_5xx" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-api-target-5xx"
  alarm_description   = "The API emitted repeated target 5xx responses."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = "HTTPCode_Target_5XX_Count"
  namespace           = "AWS/ApplicationELB"
  period              = 300
  statistic           = "Sum"
  threshold           = 5
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    LoadBalancer = aws_lb.api.arn_suffix
    TargetGroup  = aws_lb_target_group.api.arn_suffix
  }
}

locals {
  monitored_ecs_resources = merge(
    {
      api_cpu = {
        metric  = "CPUUtilization"
        service = aws_ecs_service.api.name
      }
      api_memory = {
        metric  = "MemoryUtilization"
        service = aws_ecs_service.api.name
      }
    },
    var.consolidate_workers ? {
      workers_cpu = {
        metric  = "CPUUtilization"
        service = "plether-workers"
      }
      workers_memory = {
        metric  = "MemoryUtilization"
        service = "plether-workers"
      }
      } : {
      basket_worker_cpu = {
        metric  = "CPUUtilization"
        service = aws_ecs_service.basket_worker.name
      }
      basket_worker_memory = {
        metric  = "MemoryUtilization"
        service = aws_ecs_service.basket_worker.name
      }
      perps_indexer_cpu = {
        metric  = "CPUUtilization"
        service = aws_ecs_service.perps_indexer.name
      }
      perps_indexer_memory = {
        metric  = "MemoryUtilization"
        service = aws_ecs_service.perps_indexer.name
      }
    }
  )

  active_candle_writer_services = var.perps_candle_write_mode == "dual" ? (
    var.consolidate_workers ? {
      workers = "plether-workers"
      } : {
      basket_worker = aws_ecs_service.basket_worker.name
      perps_indexer = aws_ecs_service.perps_indexer.name
    }
  ) : {}

  # These are successful source-poll heartbeats, not candle-row creation
  # events. They continue through closed markets, zero-trade periods, and
  # coarse candle intervals, so missing data means the writer loop is unhealthy.
  candle_writer_heartbeat_events = var.perps_candle_write_mode == "dual" ? {
    price  = "basket_price_watermark_advanced"
    volume = "perps_volume_writer_heartbeat"
  } : {}
}

resource "aws_cloudwatch_metric_alarm" "ecs_resource_pressure" {
  for_each = local.monitored_ecs_resources

  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-${replace(each.key, "_", "-")}-high"
  alarm_description   = "ECS ${each.value.service} ${each.value.metric} has remained above 80%."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  metric_name         = each.value.metric
  namespace           = "AWS/ECS"
  period              = 300
  statistic           = "Average"
  threshold           = 80
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    ClusterName = aws_ecs_cluster.main.name
    ServiceName = each.value.service
  }
}

# ECS publishes no CPU metrics when a service has no running tasks. A threshold
# below the valid metric domain makes this alarm a pure missing-data liveness
# check for the services that own live price and volume rollup writes.
resource "aws_cloudwatch_metric_alarm" "candle_writer_task_missing" {
  for_each = local.active_candle_writer_services

  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-candle-${replace(each.key, "_", "-")}-task-missing"
  alarm_description   = "Candle dual writes are enabled but ${each.value} stopped publishing ECS task metrics."
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
    ServiceName = each.value
  }
}

resource "aws_cloudwatch_metric_alarm" "api_task_missing" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-api-task-missing"
  alarm_description   = "The API service stopped publishing ECS task metrics, indicating that no API task is running."
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
    ServiceName = aws_ecs_service.api.name
  }
}

resource "aws_cloudwatch_log_metric_filter" "candle_writer_heartbeat" {
  for_each = local.candle_writer_heartbeat_events

  depends_on = [terraform_data.perps_candle_rollout_guard]

  name           = "plether-${var.environment}-candle-${each.key}-writer-heartbeat"
  pattern        = "{ $.event = \"${each.value}\" && $.writer_kind = \"${each.key}\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "PerpsCandle${title(each.key)}WriterHeartbeat-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

# Heartbeats are rate-limited to one per five minutes. Requiring three wholly
# empty five-minute windows tolerates poll/bucket-boundary jitter while still
# detecting a stalled writer within roughly fifteen minutes. Because the
# signal is emitted after a successful source poll, this does not depend on
# trades, price changes, or the configured candle interval.
resource "aws_cloudwatch_metric_alarm" "candle_writer_heartbeat_missing" {
  for_each = local.candle_writer_heartbeat_events

  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-candle-${each.key}-writer-heartbeat-missing"
  alarm_description   = "No successful ${each.key} candle-writer heartbeat was observed for three consecutive five-minute windows."
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = 3
  datapoints_to_alarm = 3
  metric_name         = aws_cloudwatch_log_metric_filter.candle_writer_heartbeat[each.key].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.candle_writer_heartbeat[each.key].metric_transformation[0].namespace
  period              = 300
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "candle_writer_coverage_lag" {
  for_each = local.candle_writer_heartbeat_events

  depends_on = [terraform_data.perps_candle_rollout_guard]

  name           = "plether-${var.environment}-candle-${each.key}-writer-coverage-lag"
  pattern        = "{ $.event = \"${each.value}\" && $.writer_kind = \"${each.key}\" && $.coverage_lag_seconds >= 0 }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "PerpsCandle${title(each.key)}WriterCoverageLagSeconds-${var.environment}"
    namespace = "Plether/Operations"
    value     = "$.coverage_lag_seconds"
  }
}

# The heartbeat reports base-minute coverage and subtracts that bucket's normal
# alignment age plus configured source lateness. Coarser configured/read
# intervals therefore cannot inflate this lag metric. Missing lag is tolerated
# before the first backfill; heartbeat absence and the explicit incomplete-state
# alarm cover runtime failures.
resource "aws_cloudwatch_metric_alarm" "candle_writer_coverage_lag" {
  for_each = local.candle_writer_heartbeat_events

  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-candle-${each.key}-writer-coverage-lag"
  alarm_description   = "The live ${each.key} writer is polling, but base-minute candle coverage remains more than five minutes behind."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  datapoints_to_alarm = 2
  metric_name         = aws_cloudwatch_log_metric_filter.candle_writer_coverage_lag[each.key].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.candle_writer_coverage_lag[each.key].metric_transformation[0].namespace
  period              = 300
  statistic           = "Maximum"
  threshold           = 300
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "candle_writer_coverage_incomplete" {
  for_each = local.candle_writer_heartbeat_events

  depends_on = [terraform_data.perps_candle_rollout_guard]

  name           = "plether-${var.environment}-candle-${each.key}-writer-coverage-incomplete"
  pattern        = "{ $.event = \"${each.value}\" && $.writer_kind = \"${each.key}\" && $.coverage_state = \"incomplete\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "PerpsCandle${title(each.key)}WriterCoverageIncomplete-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "candle_writer_coverage_incomplete" {
  for_each = local.candle_writer_heartbeat_events

  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-candle-${each.key}-writer-coverage-incomplete"
  alarm_description   = "The live ${each.key} writer observed an existing candle coverage row in an incomplete state."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.candle_writer_coverage_incomplete[each.key].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.candle_writer_coverage_incomplete[each.key].metric_transformation[0].namespace
  period              = 300
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "candle_backfill_failed" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  name           = "plether-${var.environment}-candle-backfill-failed"
  pattern        = "{ $.event = \"perps_candle_backfill_failed\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "PerpsCandleBackfillFailed-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "candle_backfill_failure" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-candle-backfill-failed"
  alarm_description   = "Candle administration reported a backfill or repair failure."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.candle_backfill_failed.metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.candle_backfill_failed.metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "candle_coverage_lag" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  name           = "plether-${var.environment}-candle-coverage-lag"
  pattern        = "{ $.event = \"perps_candle_coverage\" && $.lag_seconds = * }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "PerpsCandleCoverageLagSeconds-${var.environment}"
    namespace = "Plether/Operations"
    value     = "$.lag_seconds"
  }
}

resource "aws_cloudwatch_metric_alarm" "candle_coverage_lag" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-candle-coverage-lag"
  alarm_description   = "Candle rollup coverage is more than five minutes behind after subtracting the expected bucket-alignment age."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = aws_cloudwatch_log_metric_filter.candle_coverage_lag.metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.candle_coverage_lag.metric_transformation[0].namespace
  period              = 60
  statistic           = "Maximum"
  threshold           = 300
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "candle_coverage_unhealthy" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  name           = "plether-${var.environment}-candle-coverage-unhealthy"
  pattern        = "{ $.event = \"perps_candle_coverage_unhealthy\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "PerpsCandleCoverageUnhealthy-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "candle_coverage_unhealthy" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-candle-coverage-unhealthy"
  alarm_description   = "A public Perps candle read failed strict rollup coverage validation."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.candle_coverage_unhealthy.metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.candle_coverage_unhealthy.metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}
