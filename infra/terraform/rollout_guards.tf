resource "terraform_data" "perps_candle_rollout_guard" {
  input = {
    environment                    = var.environment
    api_desired_count              = var.api_desired_count
    write_mode                     = var.perps_candle_write_mode
    read_mode                      = var.perps_candle_read_mode
    read_intervals                 = var.perps_candle_read_intervals
    strict_coverage                = var.perps_candle_strict_coverage
    finalization_grace_seconds     = var.perps_candle_finalization_grace_seconds
    basket_worker_poll_seconds     = var.basket_worker_poll_seconds
    consolidate_workers            = var.consolidate_workers
    workers_desired_count          = var.workers_desired_count
    operations_alarm_sns_topic_arn = var.operations_alarm_sns_topic_arn
    db_storage_type                = var.db_storage_type
    db_apply_immediately           = var.db_apply_immediately
    db_backup_retention_days       = var.db_backup_retention_days
    db_deletion_protection         = var.db_deletion_protection
    db_skip_final_snapshot         = var.db_skip_final_snapshot
    db_final_snapshot_identifier   = var.db_final_snapshot_identifier
    db_snapshot_identifier         = var.db_snapshot_identifier
  }

  lifecycle {
    precondition {
      condition = (
        length(local.perps_candle_read_interval_tokens) == 0
        || var.perps_candle_write_mode == "dual"
      )
      error_message = "PERPS candle reads cannot allowlist an interval until perps_candle_write_mode is dual."
    }

    precondition {
      condition = (
        var.perps_candle_read_mode != "rollup"
        || (var.perps_candle_write_mode == "dual" && var.perps_candle_strict_coverage)
      )
      error_message = "Rollup read mode requires dual writes and strict coverage so public reads fail closed."
    }

    precondition {
      condition = var.perps_candle_read_mode != "rollup" || try(
        can(regex("^[1-9][0-9]*$", var.basket_worker_poll_seconds))
        && var.perps_candle_finalization_grace_seconds >= tonumber(var.basket_worker_poll_seconds) + 5,
        false
      )
      error_message = "Rollup read mode requires a positive whole-number basket_worker_poll_seconds value and finalization grace at least five seconds longer than that poll cadence."
    }

    precondition {
      condition = (
        var.perps_candle_write_mode != "dual"
        || !var.consolidate_workers
        || var.workers_desired_count >= 1
      )
      error_message = "Dual candle writes require at least one consolidated worker task."
    }

    precondition {
      condition = var.environment != "mainnet" || can(
        regex(
          "^arn:(aws|aws-us-gov|aws-cn):sns:[a-z0-9-]+:[0-9]{12}:[A-Za-z0-9_-]+(\\.fifo)?$",
          var.operations_alarm_sns_topic_arn
        )
      )
      error_message = "Mainnet requires a valid, non-empty operations_alarm_sns_topic_arn so candle and service alarms reach an operator."
    }

    precondition {
      condition = var.environment != "mainnet" || (
        var.db_backup_retention_days >= 7
        && var.db_deletion_protection
        && !var.db_skip_final_snapshot
        && try(length(trimspace(var.db_final_snapshot_identifier)) > 0, false)
      )
      error_message = "Mainnet requires at least seven days of RDS backups, deletion protection, and a configured final snapshot identifier with final snapshots enabled."
    }
  }
}

resource "terraform_data" "lp_settlement_keeper_guard" {
  input = {
    keeper_environment = local.keeper_environment
  }

  lifecycle {
    precondition {
      condition = {
        for setting in local.keeper_environment : setting.name => setting.value
      }["PERPS_HOUSE_POOL"] == var.perps_house_pool
      error_message = "The shared dedicated/consolidated keeper environment must include PERPS_HOUSE_POOL."
    }

    precondition {
      condition = {
        for setting in local.keeper_environment : setting.name => setting.value
      }["PERPS_SETTLEMENT_MONITOR_LENS"] == var.perps_settlement_monitor_lens
      error_message = "The shared dedicated/consolidated keeper environment must include the Settlement Monitor facade."
    }

    precondition {
      condition     = lower(var.perps_settlement_monitor_lens) != "0xe1fc0a465dabdfd8ee33d4aa960108f800b3f151"
      error_message = "The v1.2.0 Settlement Monitor sidecar must never be configured as the keeper facade."
    }
  }
}
