resource "terraform_data" "rpc_configuration_guard" {
  input = {
    environment                             = var.environment
    rpc_auth_token_ssm_parameter_name       = var.rpc_auth_token_ssm_parameter_name
    perps_rpc_auth_token_ssm_parameter_name = var.perps_rpc_auth_token_ssm_parameter_name
    keeper_poll_seconds                     = var.keeper_poll_seconds
    keeper_idle_poll_seconds                = var.keeper_idle_poll_seconds
  }

  lifecycle {
    precondition {
      condition = alltrue([
        for parameter_name in [
          var.rpc_auth_token_ssm_parameter_name,
          var.perps_rpc_auth_token_ssm_parameter_name,
        ] : trimspace(parameter_name) == "" || startswith(trimspace(parameter_name), "/plether/${var.environment}/")
      ])
      error_message = "RPC bearer-token parameters must belong to the current /plether/<environment>/ SSM namespace."
    }

    precondition {
      condition     = tonumber(var.keeper_idle_poll_seconds) >= tonumber(var.keeper_poll_seconds)
      error_message = "keeper_idle_poll_seconds must not be smaller than keeper_poll_seconds."
    }
  }
}

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
    keeper_environment             = local.keeper_environment
    mode                           = var.lp_settlement_mode
    perps_chain_id                 = var.perps_chain_id
    perps_house_pool               = var.perps_house_pool
    perps_order_router             = var.perps_order_router
    perps_order_lifecycle_book     = var.perps_order_lifecycle_book
    perps_cfd_engine               = var.perps_cfd_engine
    perps_plether_oracle           = var.perps_plether_oracle
    perps_senior_vault             = var.perps_senior_vault
    perps_junior_vault             = var.perps_junior_vault
    perps_settlement_monitor_lens  = var.perps_settlement_monitor_lens
    consolidate_workers            = var.consolidate_workers
    workers_desired_count          = var.workers_desired_count
    operations_alarm_sns_topic_arn = var.operations_alarm_sns_topic_arn
    max_drain_transactions         = var.lp_settlement_max_drain_transactions
    pending_replacement_seconds    = var.lp_settlement_pending_replacement_seconds
    max_replacements               = var.lp_settlement_max_replacements
    max_tx_cost_wei                = var.lp_settlement_max_tx_cost_wei
    signer_funding_confirmed       = var.lp_settlement_signer_funding_confirmed
  }

  lifecycle {
    precondition {
      condition = (
        var.environment != "sepolia"
        || var.perps_order_lifecycle_book == ""
        || lower(var.perps_order_lifecycle_book) == "0x616ad381df40047e9b060a1e85085b3ed2cc6d3c"
      )
      error_message = "Sepolia perps_order_lifecycle_book must be empty or the pinned bounded-V2 LifecycleBook."
    }

    precondition {
      condition = {
        for setting in local.keeper_environment : setting.name => setting.value
      }["PERPS_HOUSE_POOL"] == var.perps_house_pool
      error_message = "The shared dedicated/consolidated keeper environment must include PERPS_HOUSE_POOL."
    }

    precondition {
      condition = {
        for setting in local.keeper_environment : setting.name => setting.value
      }["PERPS_ORDER_ROUTER"] == var.perps_order_router
      error_message = "The shared dedicated/consolidated keeper environment must include the configured PERPS_ORDER_ROUTER."
    }

    precondition {
      condition = {
        for setting in local.keeper_environment : setting.name => setting.value
      }["PERPS_CFD_ENGINE"] == var.perps_cfd_engine
      error_message = "The shared dedicated/consolidated keeper environment must include the configured PERPS_CFD_ENGINE."
    }

    precondition {
      condition = {
        for setting in local.keeper_environment : setting.name => setting.value
      }["PERPS_PLETHER_ORACLE"] == var.perps_plether_oracle
      error_message = "The shared dedicated/consolidated keeper environment must include the Router-bound PERPS_PLETHER_ORACLE."
    }

    precondition {
      condition = {
        for setting in local.keeper_environment : setting.name => setting.value
      }["PERPS_SETTLEMENT_MONITOR_LENS"] == var.perps_settlement_monitor_lens
      error_message = "The shared dedicated/consolidated keeper environment must include the Settlement Monitor facade."
    }

    precondition {
      condition = {
        for setting in local.keeper_environment : setting.name => setting.value
      }["PERPS_SENIOR_VAULT"] == var.perps_senior_vault
      error_message = "The shared dedicated/consolidated keeper environment must include the operational Senior TrancheVault."
    }

    precondition {
      condition = {
        for setting in local.keeper_environment : setting.name => setting.value
      }["PERPS_JUNIOR_VAULT"] == var.perps_junior_vault
      error_message = "The shared dedicated/consolidated keeper environment must include the operational Junior TrancheVault."
    }

    precondition {
      condition = {
        for setting in local.keeper_environment : setting.name => setting.value
      }["LP_SETTLEMENT_MODE"] == var.lp_settlement_mode
      error_message = "The shared dedicated/consolidated keeper environment must include LP_SETTLEMENT_MODE."
    }

    precondition {
      condition     = lower(var.perps_settlement_monitor_lens) != "0x81c3a8d145c14f28334314fa67a0da0ba5c50c6b"
      error_message = "The pinned Settlement Monitor sidecar must never be configured as the keeper facade."
    }

    precondition {
      condition     = var.lp_settlement_mode == "off" || var.environment == "sepolia"
      error_message = "LP settlement observe/execute modes are Sepolia-only until a reviewed production vault release is configured."
    }

    precondition {
      condition = var.lp_settlement_mode == "off" || (
        var.perps_chain_id == "421614"
        && alltrue([
          for address in [
            var.perps_house_pool,
            var.perps_senior_vault,
            var.perps_junior_vault,
            var.perps_settlement_monitor_lens,
            var.perps_order_router,
            var.perps_cfd_engine,
            var.perps_plether_oracle,
          ] : can(regex("^0x[0-9A-Fa-f]{40}$", address))
          && lower(address) != "0x0000000000000000000000000000000000000000"
        ])
        && lower(var.perps_house_pool) == lower(var.vault_history_house_pool_address)
        && lower(var.perps_senior_vault) == lower(var.vault_history_senior_vault_address)
        && lower(var.perps_junior_vault) == lower(var.vault_history_junior_vault_address)
        && length(distinct([
          lower(var.perps_house_pool),
          lower(var.perps_senior_vault),
          lower(var.perps_junior_vault),
          lower(var.perps_settlement_monitor_lens),
          lower(var.perps_order_router),
          lower(var.perps_cfd_engine),
          lower(var.perps_plether_oracle),
        ])) == 7
      )
      error_message = "Active LP settlement requires the Arbitrum Sepolia chain, pairwise-distinct execution contracts, and operational HousePool/Senior/Junior addresses matching the vault-history release."
    }

    precondition {
      condition = var.lp_settlement_mode == "off" || (
        var.consolidate_workers
        && var.workers_desired_count == 1
      )
      error_message = "Active LP settlement requires exactly one consolidated plether-workers task so only one settlement signer loop can run."
    }

    precondition {
      condition     = var.lp_settlement_mode == "off" || trimspace(var.lp_settlement_private_key) != ""
      error_message = "LP settlement observe/execute modes require a dedicated lp_settlement_private_key."
    }

    precondition {
      condition     = var.lp_settlement_mode == "off" || var.lp_settlement_signer_funding_confirmed
      error_message = "LP settlement observe/execute modes require lp_settlement_signer_funding_confirmed=true after funding and a successful --lp-settlement-preflight balance check."
    }

    precondition {
      condition = var.lp_settlement_mode == "off" || can(
        regex(
          "^arn:(aws|aws-us-gov|aws-cn):sns:[a-z0-9-]+:[0-9]{12}:[A-Za-z0-9_-]+(\\.fifo)?$",
          var.operations_alarm_sns_topic_arn
        )
      )
      error_message = "LP settlement observe/execute modes require a valid, non-empty operations_alarm_sns_topic_arn."
    }

    precondition {
      condition = (
        var.lp_settlement_mode != "execute"
        || try(tonumber(var.lp_settlement_max_tx_cost_wei) > 0, false)
      )
      error_message = "LP settlement execute mode requires lp_settlement_max_tx_cost_wei greater than zero."
    }
  }
}
