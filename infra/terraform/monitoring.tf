resource "aws_cloudwatch_log_metric_filter" "aa_sponsored_gas_alert" {
  count = local.aa_gateway_enabled ? 1 : 0

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
  count = local.aa_gateway_enabled ? 1 : 0

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
  count = (var.provision_aa_proxy || var.lp_settlement_mode != "off") ? 1 : 0

  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-keeper-task-missing"
  alarm_description   = "The active keeper topology stopped publishing ECS task CPU metrics while a keeper-dependent feature is enabled."
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

resource "aws_cloudwatch_log_metric_filter" "rpc_request_count" {
  name           = "plether-${var.environment}-rpc-request-count"
  pattern        = "{ $.event = \"rpc_request_summary\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "RpcRequestCount-${var.environment}"
    namespace = "Plether/Operations"
    value     = "$.request_count"
  }
}

resource "aws_cloudwatch_log_metric_filter" "rpc_failure_count" {
  name           = "plether-${var.environment}-rpc-failure-count"
  pattern        = "{ $.event = \"rpc_request_summary\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "RpcFailureCount-${var.environment}"
    namespace = "Plether/Operations"
    value     = "$.failure_count"
  }
}

resource "aws_cloudwatch_log_metric_filter" "rpc_request_attribution" {
  name           = "plether-${var.environment}-rpc-request-attribution"
  pattern        = "{ $.event = \"rpc_request_summary\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "RpcRequestCountByRole-${var.environment}"
    namespace = "Plether/Operations"
    value     = "$.request_count"
    dimensions = {
      RpcMethod = "$.rpc_method"
      RpcRole   = "$.rpc_role"
    }
  }
}

resource "aws_cloudwatch_metric_alarm" "rpc_request_rate_warning" {
  alarm_name          = "plether-${var.environment}-rpc-request-rate-warning"
  alarm_description   = "Ethereum RPC traffic exceeded the steady-state 15k request/hour warning threshold for two hours."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = aws_cloudwatch_log_metric_filter.rpc_request_count.metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.rpc_request_count.metric_transformation[0].namespace
  period              = 3600
  statistic           = "Sum"
  threshold           = 15000
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_metric_alarm" "rpc_request_rate_critical" {
  alarm_name          = "plether-${var.environment}-rpc-request-rate-critical"
  alarm_description   = "Ethereum RPC traffic exceeded 25k requests in one hour."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.rpc_request_count.metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.rpc_request_count.metric_transformation[0].namespace
  period              = 3600
  statistic           = "Sum"
  threshold           = 25000
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_metric_alarm" "rpc_failure_rate" {
  alarm_name          = "plether-${var.environment}-rpc-failure-rate"
  alarm_description   = "Ethereum RPC failures exceeded one percent in an hour containing at least 100 requests."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 1
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  metric_query {
    id          = "failure_rate"
    expression  = "IF(requests >= 100, 100 * failures / requests, 0)"
    label       = "RPC failure percentage"
    return_data = true
  }

  metric_query {
    id          = "requests"
    return_data = false

    metric {
      metric_name = aws_cloudwatch_log_metric_filter.rpc_request_count.metric_transformation[0].name
      namespace   = aws_cloudwatch_log_metric_filter.rpc_request_count.metric_transformation[0].namespace
      period      = 3600
      stat        = "Sum"
    }
  }

  metric_query {
    id          = "failures"
    return_data = false

    metric {
      metric_name = aws_cloudwatch_log_metric_filter.rpc_failure_count.metric_transformation[0].name
      namespace   = aws_cloudwatch_log_metric_filter.rpc_failure_count.metric_transformation[0].namespace
      period      = 3600
      stat        = "Sum"
    }
  }
}

resource "aws_cloudwatch_log_metric_filter" "lp_settlement_heartbeat" {
  count = var.lp_settlement_mode != "off" ? 1 : 0

  depends_on = [terraform_data.lp_settlement_keeper_guard]

  name           = "plether-${var.environment}-lp-settlement-heartbeat"
  pattern        = "{ $.event = \"lp_settlement_heartbeat\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "LpSettlementHeartbeat-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "lp_settlement_heartbeat_missing" {
  count = var.lp_settlement_mode != "off" ? 1 : 0

  depends_on = [terraform_data.lp_settlement_keeper_guard]

  alarm_name          = "plether-${var.environment}-lp-settlement-heartbeat-missing"
  alarm_description   = "No LP settlement heartbeat was observed for three consecutive one-minute periods."
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = 3
  datapoints_to_alarm = 3
  metric_name         = aws_cloudwatch_log_metric_filter.lp_settlement_heartbeat[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.lp_settlement_heartbeat[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "lp_settlement_ready_backlog" {
  count = var.lp_settlement_mode != "off" ? 1 : 0

  depends_on = [terraform_data.lp_settlement_keeper_guard]

  name           = "plether-${var.environment}-lp-settlement-ready-backlog"
  pattern        = "{ $.event = \"lp_settlement_ready_backlog\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "LpSettlementReadyBacklog-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "lp_settlement_ready_backlog" {
  count = var.lp_settlement_mode != "off" ? 1 : 0

  depends_on = [terraform_data.lp_settlement_keeper_guard]

  alarm_name          = "plether-${var.environment}-lp-settlement-ready-backlog"
  alarm_description   = "Safety-ready matured LP settlement work remained for five consecutive one-minute periods."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 5
  datapoints_to_alarm = 5
  metric_name         = aws_cloudwatch_log_metric_filter.lp_settlement_ready_backlog[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.lp_settlement_ready_backlog[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

locals {
  lp_settlement_immediate_alarm_events = var.lp_settlement_mode != "off" ? {
    pending_stuck = {
      event       = "lp_settlement_pending_stuck"
      description = "An LP settlement transaction exceeded its pending-age or replacement limit."
    }
    invariant_failure = {
      event       = "lp_settlement_invariant_failure"
      description = "LP settlement observed a binding, schema, event, receipt, or other fail-closed invariant violation."
    }
    low_balance = {
      event       = "lp_settlement_low_balance"
      description = "The LP settlement signer balance fell below twice the four-transaction cost budget."
    }
  } : {}
}

resource "aws_cloudwatch_log_metric_filter" "lp_settlement_immediate_alarm" {
  for_each = local.lp_settlement_immediate_alarm_events

  depends_on = [terraform_data.lp_settlement_keeper_guard]

  name           = "plether-${var.environment}-lp-settlement-${replace(each.key, "_", "-")}"
  pattern        = "{ $.event = \"${each.value.event}\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "LpSettlement${replace(title(each.key), "_", "")}-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "lp_settlement_immediate_alarm" {
  for_each = local.lp_settlement_immediate_alarm_events

  depends_on = [terraform_data.lp_settlement_keeper_guard]

  alarm_name          = "plether-${var.environment}-lp-settlement-${replace(each.key, "_", "-")}"
  alarm_description   = each.value.description
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.lp_settlement_immediate_alarm[each.key].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.lp_settlement_immediate_alarm[each.key].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "vault_indexer_heartbeat" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  name           = "plether-${var.environment}-vault-indexer-heartbeat"
  pattern        = "{ $.event = \"vault_activity_indexer_heartbeat\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "VaultIndexerHeartbeat-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "vault_indexer_heartbeat_missing" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  alarm_name          = "plether-${var.environment}-vault-indexer-heartbeat-missing"
  alarm_description   = "No successful vault-indexer heartbeat was observed for three minutes."
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = 3
  datapoints_to_alarm = 3
  metric_name         = aws_cloudwatch_log_metric_filter.vault_indexer_heartbeat[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.vault_indexer_heartbeat[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "vault_request_share_attribution_heartbeat" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  name           = "plether-${var.environment}-vault-request-share-attribution-heartbeat"
  pattern        = "{ $.event = \"vault_request_share_attribution_heartbeat\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "VaultRequestShareAttributionHeartbeat-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "vault_request_share_attribution_heartbeat_missing" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  alarm_name          = "plether-${var.environment}-vault-request-share-attribution-heartbeat-missing"
  alarm_description   = "No successful vault request-share-attribution heartbeat was observed for three minutes."
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = 3
  datapoints_to_alarm = 3
  metric_name         = aws_cloudwatch_log_metric_filter.vault_request_share_attribution_heartbeat[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.vault_request_share_attribution_heartbeat[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "vault_request_share_attribution_lag" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  name           = "plether-${var.environment}-vault-request-share-attribution-lag"
  pattern        = "{ $.event = \"vault_request_share_attribution_heartbeat\" && $.lag_seconds >= 0 }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "VaultRequestShareAttributionLagSeconds-${var.environment}"
    namespace = "Plether/Operations"
    value     = "$.lag_seconds"
  }
}

resource "aws_cloudwatch_metric_alarm" "vault_request_share_attribution_lag" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  alarm_name          = "plether-${var.environment}-vault-request-share-attribution-lag"
  alarm_description   = "Vault request-share attribution lag exceeded two minutes for three consecutive periods."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  datapoints_to_alarm = 3
  metric_name         = aws_cloudwatch_log_metric_filter.vault_request_share_attribution_lag[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.vault_request_share_attribution_lag[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Maximum"
  threshold           = 120
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "vault_request_share_attribution_backfill" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  name           = "plether-${var.environment}-vault-request-share-attribution-backfill"
  pattern        = "{ $.event = \"vault_request_share_attribution_heartbeat\" && $.state = \"backfilling\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "VaultRequestShareAttributionBackfillIncomplete-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "vault_request_share_attribution_backfill" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  alarm_name          = "plether-${var.environment}-vault-request-share-attribution-backfill-incomplete"
  alarm_description   = "Vault request-share attribution remained incomplete for ten minutes."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 10
  datapoints_to_alarm = 10
  metric_name         = aws_cloudwatch_log_metric_filter.vault_request_share_attribution_backfill[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.vault_request_share_attribution_backfill[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "vault_indexer_lag" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  name           = "plether-${var.environment}-vault-indexer-lag"
  pattern        = "{ $.event = \"vault_activity_indexer_heartbeat\" && $.lag_seconds >= 0 }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "VaultIndexerLagSeconds-${var.environment}"
    namespace = "Plether/Operations"
    value     = "$.lag_seconds"
  }
}

resource "aws_cloudwatch_metric_alarm" "vault_indexer_lag" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  alarm_name          = "plether-${var.environment}-vault-indexer-confirmed-data-lag"
  alarm_description   = "Confirmed vault activity lag exceeded two minutes for three consecutive periods."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  datapoints_to_alarm = 3
  metric_name         = aws_cloudwatch_log_metric_filter.vault_indexer_lag[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.vault_indexer_lag[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Maximum"
  threshold           = 120
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "vault_indexer_backfill" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  name           = "plether-${var.environment}-vault-indexer-backfill"
  pattern        = "{ $.event = \"vault_activity_indexer_heartbeat\" && $.state = \"backfilling\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "VaultIndexerBackfillIncomplete-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "vault_indexer_backfill" {
  count = var.perps_chain_id == "421614" ? 1 : 0

  alarm_name          = "plether-${var.environment}-vault-indexer-backfill-incomplete"
  alarm_description   = "The canonical vault index rebuild remained incomplete for ten minutes."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 10
  datapoints_to_alarm = 10
  metric_name         = aws_cloudwatch_log_metric_filter.vault_indexer_backfill[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.vault_indexer_backfill[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

locals {
  vault_indexer_failure_events = var.perps_chain_id == "421614" ? {
    invariant   = "vault_activity_indexer_iteration_failed"
    attribution = "vault_request_share_attribution_iteration_failed"
    trace       = "perps_indexer_execution_evidence_economics_failed"
  } : {}
}

resource "aws_cloudwatch_log_metric_filter" "vault_indexer_failure" {
  for_each = local.vault_indexer_failure_events

  name           = "plether-${var.environment}-${replace(each.key, "_", "-")}-failure"
  pattern        = "{ $.event = \"${each.value}\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "VaultIndexer${title(each.key)}Failure-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "vault_indexer_failure" {
  for_each = local.vault_indexer_failure_events

  alarm_name          = "plether-${var.environment}-${replace(each.key, "_", "-")}-failure"
  alarm_description   = each.key == "trace" ? "Alchemy transaction tracing failed repeatedly." : "The vault indexer rejected an invariant or provider response."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = each.key == "trace" ? 3 : 1
  datapoints_to_alarm = each.key == "trace" ? 3 : 1
  metric_name         = aws_cloudwatch_log_metric_filter.vault_indexer_failure[each.key].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.vault_indexer_failure[each.key].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
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

resource "aws_cloudwatch_log_metric_filter" "api_foreground_request_duration" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  name           = "plether-${var.environment}-api-foreground-request-duration"
  pattern        = "{ $.event = \"api_foreground_request_completed\" && $.request_class = \"foreground\" && $.duration_ms >= 0 }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  # Deliberately omit route, method, and status dimensions. The rollout gate
  # needs one environment-wide foreground latency series with bounded cost and
  # cardinality; normalized route/status remain available in the source logs.
  metric_transformation {
    name      = "ApiForegroundRequestDurationMilliseconds-${var.environment}"
    namespace = "Plether/Operations"
    value     = "$.duration_ms"
    unit      = "Milliseconds"
  }
}

# The Terraform resource address is retained for state continuity even though
# the alarm now gates only structured foreground completion events. Raw
# TargetResponseTime remains queryable in AWS/ApplicationELB for diagnosis, but
# is intentionally not alarmed because it includes expected order long-polls.
resource "aws_cloudwatch_metric_alarm" "alb_target_latency_high" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-api-p95-latency-high"
  alarm_description   = "Foreground API request p95 completion latency is above the 750ms candle rollout SLO. Expected long polls and health checks are excluded."
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  metric_name         = aws_cloudwatch_log_metric_filter.api_foreground_request_duration.metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.api_foreground_request_duration.metric_transformation[0].namespace
  period              = 300
  extended_statistic  = "p95"
  threshold           = 750
  unit                = "Milliseconds"
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
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

resource "aws_cloudwatch_metric_alarm" "alb_5xx" {
  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-api-alb-5xx"
  alarm_description   = "The API load balancer generated a 5xx response before the target could respond successfully."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = "HTTPCode_ELB_5XX_Count"
  namespace           = "AWS/ApplicationELB"
  period              = 300
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    LoadBalancer = aws_lb.api.arn_suffix
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
    },
    local.self_hosted_aa_resource_count == 1 ? {
      alto_cpu = {
        metric  = "CPUUtilization"
        service = aws_ecs_service.alto[0].name
      }
      alto_memory = {
        metric  = "MemoryUtilization"
        service = aws_ecs_service.alto[0].name
      }
      aa_reconciler_cpu = {
        metric  = "CPUUtilization"
        service = aws_ecs_service.aa_reconciler[0].name
      }
      aa_reconciler_memory = {
        metric  = "MemoryUtilization"
        service = aws_ecs_service.aa_reconciler[0].name
      }
    } : {}
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

resource "aws_cloudwatch_metric_alarm" "alto_task_missing" {
  count = local.self_hosted_aa_resource_count == 1 && var.alto_desired_count == 1 ? 1 : 0

  depends_on = [terraform_data.self_hosted_aa_guard]

  alarm_name          = "plether-${var.environment}-alto-task-missing"
  alarm_description   = "The single-active Alto service stopped publishing ECS task metrics."
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
    ServiceName = aws_ecs_service.alto[0].name
  }
}

resource "aws_cloudwatch_metric_alarm" "aa_reconciler_task_missing" {
  count = local.self_hosted_aa_resource_count == 1 && var.aa_reconciler_desired_count == 1 ? 1 : 0

  depends_on = [terraform_data.self_hosted_aa_guard]

  alarm_name          = "plether-${var.environment}-aa-reconciler-task-missing"
  alarm_description   = "The single-active AA reconciler stopped publishing ECS task metrics."
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
    ServiceName = aws_ecs_service.aa_reconciler[0].name
  }
}

resource "aws_cloudwatch_metric_alarm" "alto_unhealthy_target" {
  count = local.self_hosted_aa_resource_count == 1 && var.alto_desired_count == 1 ? 1 : 0

  depends_on = [terraform_data.self_hosted_aa_guard]

  alarm_name          = "plether-${var.environment}-alto-unhealthy-target"
  alarm_description   = "The internal Alto load balancer has an unhealthy target."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 2
  metric_name         = "UnHealthyHostCount"
  namespace           = "AWS/ApplicationELB"
  period              = 60
  statistic           = "Maximum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])

  dimensions = {
    LoadBalancer = aws_lb.alto[0].arn_suffix
    TargetGroup  = aws_lb_target_group.alto[0].arn_suffix
  }
}

resource "aws_cloudwatch_metric_alarm" "alto_target_5xx" {
  count = local.self_hosted_aa_resource_count

  depends_on = [terraform_data.self_hosted_aa_guard]

  alarm_name          = "plether-${var.environment}-alto-target-5xx"
  alarm_description   = "Alto emitted repeated target 5xx responses through its internal load balancer."
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
    LoadBalancer = aws_lb.alto[0].arn_suffix
    TargetGroup  = aws_lb_target_group.alto[0].arn_suffix
  }
}

resource "aws_cloudwatch_log_metric_filter" "alto_fatal" {
  count = local.self_hosted_aa_resource_count

  name           = "plether-${var.environment}-alto-fatal"
  pattern        = "{ $.container_name = \"plether-alto\" && $.level = \"fatal\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AltoFatal-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "alto_fatal" {
  count = local.self_hosted_aa_resource_count

  alarm_name          = "plether-${var.environment}-alto-fatal"
  alarm_description   = "Alto emitted a fatal structured log record."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.alto_fatal[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.alto_fatal[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

locals {
  alto_wallet_fault_patterns = local.self_hosted_aa_resource_count == 1 ? {
    utility_refill_balance = "{ $.container_name = \"plether-alto\" && $.level = \"error\" && $.module = \"validate-and-refill-wallets\" && $.msg = \"utility wallet has insufficient balance to refill wallets\" }"
    initial_refill_error   = "{ $.container_name = \"plether-alto\" && $.level = \"error\" && $.module = \"root\" && $.msg = \"Error during initial wallet validation and refill\" }"
    scheduled_refill_error = "{ $.container_name = \"plether-alto\" && $.level = \"error\" && $.module = \"root\" && $.msg = \"Error during scheduled wallet validation and refill\" }"
    utility_monitor_error  = "{ $.container_name = \"plether-alto\" && $.level = \"error\" && $.module = \"utility_wallet_monitor\" && $.msg = \"Failed to update utility wallet balance metrics\" }"
    refill_gas_price_error = "{ $.container_name = \"plether-alto\" && $.level = \"error\" && $.module = \"validate-and-refill-wallets\" && $.msg = \"No gas price available\" }"
  } : {}

  aa_reconciler_fatal_events = local.self_hosted_aa_resource_count == 1 ? toset([
    "aa_reconciler_configuration_invalid",
    "aa_reconciler_schema_invalid",
    "aa_reconciler_unknown_operation",
    "aa_reconciler_cursor_discontinuity",
    "aa_reconciler_cost_exceeds_reservation",
    "aa_reconciler_chain_mismatch",
    "aa_reconciler_provider_disagreement",
    "aa_reconciler_timestamp_invalid",
    "aa_reconciler_failure_threshold_exceeded",
    "aa_reconciler_crashed",
  ]) : toset([])

  aa_native_api_fault_patterns = local.self_hosted_aa_resource_count == 1 ? {
    issuance_unavailable   = "{ $.container_name = \"plether-api\" && $.level = \"WARN\" && $.event = \"aa_native_issuance_unavailable\" }"
    reconciler_stale       = "{ $.container_name = \"plether-api\" && $.level = \"ERROR\" && $.event = \"aa_native_reconciler_stale\" }"
    signer_failure         = "{ $.container_name = \"plether-api\" && $.level = \"ERROR\" && $.event = \"aa_native_signer_failure\" }"
    bundler_hash_mismatch  = "{ $.container_name = \"plether-api\" && $.level = \"ERROR\" && $.event = \"aa_native_bundler_hash_mismatch\" }"
    sponsorship_db_failure = "{ $.container_name = \"plether-api\" && $.level = \"ERROR\" && $.event = \"aa_native_sponsorship_database_failure\" }"
    security_attestation   = "{ $.container_name = \"plether-api\" && $.level = \"ERROR\" && $.event = \"aa_native_security_attestation_failure\" }"
  } : {}
}

resource "aws_cloudwatch_log_metric_filter" "aa_native_api_fault" {
  for_each = local.aa_native_api_fault_patterns

  name           = "plether-${var.environment}-aa-native-${replace(each.key, "_", "-")}"
  pattern        = each.value
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AaNativeApiFault-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "aa_native_api_fault" {
  count = local.self_hosted_aa_resource_count

  depends_on = [aws_cloudwatch_log_metric_filter.aa_native_api_fault]

  alarm_name          = "plether-${var.environment}-aa-native-api-fault"
  alarm_description   = "Native AA issuance, signer, bundler hash, reconciliation freshness, or durable sponsorship state failed closed."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = "AaNativeApiFault-${var.environment}"
  namespace           = "Plether/Operations"
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "alto_wallet_fault" {
  for_each = local.alto_wallet_fault_patterns

  name           = "plether-${var.environment}-alto-${replace(each.key, "_", "-")}"
  pattern        = each.value
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AltoWalletFault-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "alto_wallet_fault" {
  count = local.self_hosted_aa_resource_count

  depends_on = [aws_cloudwatch_log_metric_filter.alto_wallet_fault]

  alarm_name          = "plether-${var.environment}-alto-wallet-fault"
  alarm_description   = "Alto could not validate, refill, or monitor its utility/executor wallets."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = "AltoWalletFault-${var.environment}"
  namespace           = "Plether/Operations"
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "alto_gas_price_initialization_error" {
  count = local.self_hosted_aa_resource_count

  name           = "plether-${var.environment}-alto-gas-price-initialization-error"
  pattern        = "{ $.container_name = \"plether-alto\" && $.level = \"error\" && $.module = \"gas_price_manager\" && $.msg = \"Error during gas price initialization\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AltoGasPriceFault-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_log_metric_filter" "alto_gas_price_refresh_error" {
  count = local.self_hosted_aa_resource_count

  name           = "plether-${var.environment}-alto-gas-price-refresh-error"
  pattern        = "{ $.container_name = \"plether-alto\" && $.level = \"error\" && $.module = \"gas_price_manager\" && $.msg = \"Error updating gas prices in interval\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AltoGasPriceFault-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "alto_gas_price_initialization_error" {
  count = local.self_hosted_aa_resource_count

  depends_on = [
    aws_cloudwatch_log_metric_filter.alto_gas_price_initialization_error,
    aws_cloudwatch_log_metric_filter.alto_gas_price_refresh_error,
  ]

  alarm_name          = "plether-${var.environment}-alto-gas-price-fault"
  alarm_description   = "Alto could not initialize or refresh its gas-price manager; health checks can remain green while estimation and bundling are unavailable."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.alto_gas_price_initialization_error[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.alto_gas_price_initialization_error[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "alto_executor_insufficient_funds" {
  count = local.self_hosted_aa_resource_count

  name           = "plether-${var.environment}-alto-executor-insufficient-funds"
  pattern        = "{ $.container_name = \"plether-alto\" && $.level = \"warn\" && $.msg = \"executor has insufficient funds\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AltoExecutorInsufficientFunds-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "alto_executor_insufficient_funds" {
  count = local.self_hosted_aa_resource_count

  alarm_name          = "plether-${var.environment}-alto-executor-insufficient-funds"
  alarm_description   = "Alto attempted to bundle with an executor that had insufficient funds."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.alto_executor_insufficient_funds[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.alto_executor_insufficient_funds[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "aa_reconciler_fatal" {
  for_each = local.aa_reconciler_fatal_events

  name           = "plether-${var.environment}-${replace(each.value, "_", "-")}"
  pattern        = "{ $.container_name = \"plether-aa-reconciler\" && $.level = \"ERROR\" && $.event = \"${each.value}\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AaReconcilerFatal-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "aa_reconciler_fatal" {
  count = local.self_hosted_aa_resource_count

  depends_on = [aws_cloudwatch_log_metric_filter.aa_reconciler_fatal]

  alarm_name          = "plether-${var.environment}-aa-reconciler-fatal"
  alarm_description   = "The AA reconciler hit a startup, provider-agreement, cursor, operation, reservation, or durable-pause circuit breaker."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = "AaReconcilerFatal-${var.environment}"
  namespace           = "Plether/Operations"
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "aa_reconciler_heartbeat" {
  count = local.self_hosted_aa_resource_count

  name           = "plether-${var.environment}-aa-reconciler-heartbeat"
  pattern        = "{ $.container_name = \"plether-aa-reconciler\" && $.event = \"aa_reconciler_heartbeat\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AaReconcilerHeartbeat-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "aa_reconciler_heartbeat_missing" {
  count = local.self_hosted_aa_resource_count == 1 && var.aa_reconciler_desired_count == 1 ? 1 : 0

  alarm_name          = "plether-${var.environment}-aa-reconciler-heartbeat-missing"
  alarm_description   = "No AA reconciler heartbeat was observed for two consecutive five-minute windows."
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = 2
  datapoints_to_alarm = 2
  metric_name         = aws_cloudwatch_log_metric_filter.aa_reconciler_heartbeat[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.aa_reconciler_heartbeat[0].metric_transformation[0].namespace
  period              = 300
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "aa_reconciler_paymaster_low_deposit" {
  count = local.self_hosted_aa_resource_count

  name           = "plether-${var.environment}-aa-reconciler-paymaster-low-deposit"
  pattern        = "{ $.container_name = \"plether-aa-reconciler\" && $.level = \"ERROR\" && $.event = \"aa_reconciler_paymaster_low_deposit\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AaReconcilerPaymasterLowDeposit-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "aa_reconciler_paymaster_low_deposit" {
  count = local.self_hosted_aa_resource_count

  alarm_name          = "plether-${var.environment}-aa-paymaster-low-deposit"
  alarm_description   = "The paymaster EntryPoint deposit fell below the reconciler threshold; native issuance was paused in durable state."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.aa_reconciler_paymaster_low_deposit[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.aa_reconciler_paymaster_low_deposit[0].metric_transformation[0].namespace
  period              = 60
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "aa_reconciler_paymaster_unstaked" {
  count = local.self_hosted_aa_resource_count

  name           = "plether-${var.environment}-aa-reconciler-paymaster-unstaked"
  pattern        = "{ $.container_name = \"plether-aa-reconciler\" && $.level = \"WARN\" && $.event = \"aa_reconciler_paymaster_unstaked\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AaReconcilerPaymasterUnstaked-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "aa_reconciler_paymaster_unstaked" {
  count = local.self_hosted_aa_resource_count

  alarm_name          = "plether-${var.environment}-aa-paymaster-unstaked"
  alarm_description   = "The paymaster is not staked in EntryPoint."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.aa_reconciler_paymaster_unstaked[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.aa_reconciler_paymaster_unstaked[0].metric_transformation[0].namespace
  period              = 300
  statistic           = "Sum"
  threshold           = 1
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_log_metric_filter" "aa_reconciler_rpc_unavailable" {
  count = local.self_hosted_aa_resource_count

  name           = "plether-${var.environment}-aa-reconciler-rpc-unavailable"
  pattern        = "{ $.container_name = \"plether-aa-reconciler\" && $.level = \"WARN\" && $.event = \"aa_reconciler_rpc_unavailable\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "AaReconcilerRpcUnavailable-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "aa_reconciler_rpc_unavailable" {
  count = local.self_hosted_aa_resource_count

  alarm_name          = "plether-${var.environment}-aa-reconciler-rpc-unavailable"
  alarm_description   = "The AA reconciler repeatedly could not read safe-chain or paymaster state from its RPC."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.aa_reconciler_rpc_unavailable[0].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.aa_reconciler_rpc_unavailable[0].metric_transformation[0].namespace
  period              = 300
  statistic           = "Sum"
  threshold           = 3
  treat_missing_data  = "notBreaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
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

resource "aws_cloudwatch_log_metric_filter" "candle_writer_coverage_uninitialized" {
  for_each = local.candle_writer_heartbeat_events

  depends_on = [terraform_data.perps_candle_rollout_guard]

  name           = "plether-${var.environment}-candle-${each.key}-writer-coverage-uninitialized"
  pattern        = "{ $.event = \"${each.value}\" && $.writer_kind = \"${each.key}\" && $.coverage_state = \"uninitialized\" }"
  log_group_name = aws_cloudwatch_log_group.ecs.name

  metric_transformation {
    name      = "PerpsCandle${title(each.key)}WriterCoverageUninitialized-${var.environment}"
    namespace = "Plether/Operations"
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "candle_writer_coverage_uninitialized" {
  for_each = local.candle_writer_heartbeat_events

  depends_on = [terraform_data.perps_candle_rollout_guard]

  alarm_name          = "plether-${var.environment}-candle-${each.key}-writer-coverage-uninitialized"
  alarm_description   = "The live ${each.key} writer has not published candle coverage for its active dataset."
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = 1
  metric_name         = aws_cloudwatch_log_metric_filter.candle_writer_coverage_uninitialized[each.key].metric_transformation[0].name
  namespace           = aws_cloudwatch_log_metric_filter.candle_writer_coverage_uninitialized[each.key].metric_transformation[0].namespace
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
