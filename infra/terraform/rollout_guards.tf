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
    perps_order_lifecycle_book     = var.perps_order_lifecycle_book
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
        var.environment != "sepolia"
        || var.perps_order_lifecycle_book == ""
        || lower(var.perps_order_lifecycle_book) == "0xa210928a7e0ae27626b8d0e67bbd82305438ab9e"
      )
      error_message = "Sepolia perps_order_lifecycle_book must be empty or the pinned bounded-V2 LifecycleBook."
    }

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
        || lower(var.perps_order_lifecycle_book) == "0xca57215a3859462eb380ea40969762ac89d99522"
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
      error_message = "The shared dedicated/consolidated keeper environment must include the v1.2.0 PERPS_ORDER_ROUTER."
    }

    precondition {
      condition = {
        for setting in local.keeper_environment : setting.name => setting.value
      }["PERPS_CFD_ENGINE"] == var.perps_cfd_engine
      error_message = "The shared dedicated/consolidated keeper environment must include the v1.2.0 PERPS_CFD_ENGINE."
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
      condition     = lower(var.perps_settlement_monitor_lens) != "0x64b12febc33e056bf607c1dcc764bcdf3a5fe31b"
      error_message = "The v1.2.0 Settlement Monitor sidecar must never be configured as the keeper facade."
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

resource "terraform_data" "self_hosted_aa_guard" {
  input = {
    environment                                 = var.environment
    aws_region                                  = var.aws_region
    perps_chain_id                              = var.perps_chain_id
    provision_self_hosted_aa                    = var.provision_self_hosted_aa
    configure_native_aa_backend                 = var.configure_native_aa_backend
    enable_native_aa_sponsorship                = var.enable_native_aa_sponsorship
    enable_native_aa_submission                 = var.enable_native_aa_submission
    aa_native_canary_owners                     = var.aa_native_canary_owners
    aa_native_global_rollout                    = var.aa_native_global_rollout_enabled
    alto_desired_count                          = var.alto_desired_count
    aa_reconciler_desired_count                 = var.aa_reconciler_desired_count
    aa_reconciler_start_block                   = var.aa_reconciler_start_block
    aa_reconciler_start_block_hash              = var.aa_reconciler_start_block_hash
    aa_reconciler_secondary_rpc_url_kms_key_arn = var.aa_reconciler_secondary_rpc_url_kms_key_arn
    alto_upstream_image                         = var.alto_upstream_image
    alto_entrypoint_address                     = var.alto_entrypoint_address
    aa_paymaster_address                        = var.aa_paymaster_address
    aa_paymaster_signer_address                 = var.aa_paymaster_signer_address
    aa_paymaster_policy_id                      = var.aa_paymaster_policy_id
    aa_paymaster_account_code_hash              = var.aa_paymaster_account_code_hash
    aa_paymaster_code_hash                      = var.aa_paymaster_code_hash
  }

  lifecycle {
    precondition {
      condition     = !var.provision_self_hosted_aa || var.aws_region == "ap-southeast-1"
      error_message = "The reviewed self-hosted AA stack is pinned to aws_region=ap-southeast-1."
    }

    precondition {
      condition = var.aa_reconciler_secondary_rpc_url_kms_key_arn == "" || startswith(
        var.aa_reconciler_secondary_rpc_url_kms_key_arn,
        "arn:aws:kms:${var.aws_region}:${data.aws_caller_identity.current.account_id}:key/"
      )
      error_message = "aa_reconciler_secondary_rpc_url_kms_key_arn must identify a customer-managed key in the exact Terraform AWS account and region."
    }

    precondition {
      condition = !var.provision_self_hosted_aa || (
        var.environment == "sepolia"
        && var.perps_chain_id == "421614"
      )
      error_message = "The self-hosted AA stack is provisioned only for Arbitrum Sepolia (environment=sepolia, perps_chain_id=421614)."
    }

    precondition {
      condition = var.alto_desired_count == 0 || (
        var.provision_self_hosted_aa
        && var.environment == "sepolia"
      )
      error_message = "alto_desired_count can be nonzero only for the provisioned Sepolia self-hosted AA stack."
    }

    precondition {
      condition = var.aa_reconciler_desired_count == 0 || (
        var.provision_self_hosted_aa
        && var.environment == "sepolia"
      )
      error_message = "aa_reconciler_desired_count can be nonzero only for the provisioned Sepolia self-hosted AA stack."
    }

    precondition {
      condition = !var.configure_native_aa_backend || (
        var.provision_self_hosted_aa
        && var.environment == "sepolia"
        && var.alto_desired_count == 1
        && var.aa_reconciler_desired_count == 1
      )
      error_message = "configure_native_aa_backend=true requires the Sepolia self-hosted stack with both Alto and the AA reconciler running at desired_count=1."
    }

    precondition {
      condition     = !var.enable_native_aa_sponsorship || var.configure_native_aa_backend
      error_message = "Native sponsorship issuance cannot be enabled until configure_native_aa_backend=true."
    }

    precondition {
      condition     = !var.enable_native_aa_submission || var.configure_native_aa_backend
      error_message = "Native submission cannot be enabled until configure_native_aa_backend=true."
    }

    precondition {
      condition     = !var.enable_native_aa_sponsorship || var.enable_native_aa_submission
      error_message = "Native sponsorship issuance requires native submission to remain enabled so freshly signed UserOperations can be sent."
    }

    precondition {
      condition     = !var.enable_native_aa_sponsorship || var.aa_native_canary_owners != ""
      error_message = "Native sponsorship issuance requires a nonempty canary-owner allowlist for the reviewed Sepolia profile; global rollout is not approved."
    }

    precondition {
      condition     = !var.aa_native_global_rollout_enabled
      error_message = "aa_native_global_rollout_enabled must remain false: global native-AA rollout is blocked for the reviewed Sepolia profile while Alto safe mode is disabled and final sponsorship is unsigned."
    }

    precondition {
      condition = var.aa_native_canary_owners == "" || (
        length(distinct(split(",", lower(var.aa_native_canary_owners)))) == length(split(",", var.aa_native_canary_owners))
      )
      error_message = "aa_native_canary_owners must not contain duplicate addresses."
    }

    precondition {
      condition = !var.provision_self_hosted_aa || (
        var.alto_upstream_image == "ghcr.io/pimlicolabs/alto:v1.2.7@sha256:28cee87ea6b58ba10a37273e58602b50321516c36a81d0c35d50526d1f06995d"
        && lower(var.alto_entrypoint_address) == "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"
      )
      error_message = "Self-hosted AA must use the reviewed Alto v1.2.7 ARM64 digest and ERC-4337 EntryPoint v0.8 address."
    }

    precondition {
      condition = var.alto_desired_count == 0 || (
        var.alto_entrypoint_simulation_contract_v8 == trimspace(var.alto_entrypoint_simulation_contract_v8)
        && var.alto_pimlico_simulation_contract == trimspace(var.alto_pimlico_simulation_contract)
        && can(regex("^0x[0-9A-Fa-f]{40}$", trimspace(var.alto_entrypoint_simulation_contract_v8)))
        && lower(trimspace(var.alto_entrypoint_simulation_contract_v8)) != "0x0000000000000000000000000000000000000000"
        && can(regex("^0x[0-9A-Fa-f]{40}$", trimspace(var.alto_pimlico_simulation_contract)))
        && lower(trimspace(var.alto_pimlico_simulation_contract)) != "0x0000000000000000000000000000000000000000"
      )
      error_message = "Scaling Alto above zero requires nonzero 20-byte addresses for both reviewed simulation contracts."
    }

    precondition {
      condition = !var.provision_self_hosted_aa || (
        startswith(trimspace(var.alto_rpc_url_ssm_parameter_name), "/plether/sepolia/")
        && startswith(trimspace(var.alto_executor_private_keys_ssm_parameter_name), "/plether/sepolia/")
        && startswith(trimspace(var.alto_utility_private_key_ssm_parameter_name), "/plether/sepolia/")
        && (
          trimspace(var.alto_send_transaction_rpc_url_ssm_parameter_name) == ""
          || startswith(trimspace(var.alto_send_transaction_rpc_url_ssm_parameter_name), "/plether/sepolia/")
        )
      )
      error_message = "All external Alto SecureStrings must be scoped under /plether/sepolia/."
    }

    precondition {
      condition = !var.provision_self_hosted_aa || (
        startswith(trimspace(var.aa_reconciler_secondary_rpc_url_ssm_parameter_name), "/plether/sepolia/")
        && trimspace(var.aa_reconciler_secondary_rpc_url_ssm_parameter_name) != "/plether/sepolia/perps-rpc-url"
        && trimspace(var.aa_reconciler_secondary_rpc_url_ssm_parameter_name) != trimspace(var.alto_rpc_url_ssm_parameter_name)
      )
      error_message = "The reconciler secondary RPC SecureString must be under /plether/sepolia/ and use a parameter distinct from both primary reconciliation/Perps RPC and Alto RPC; operators must also choose an independently operated provider."
    }

    precondition {
      condition = !var.provision_self_hosted_aa || (
        length(distinct(compact([
          trimspace(var.alto_rpc_url_ssm_parameter_name),
          trimspace(var.alto_send_transaction_rpc_url_ssm_parameter_name),
          trimspace(var.alto_executor_private_keys_ssm_parameter_name),
          trimspace(var.alto_utility_private_key_ssm_parameter_name),
          ]))) == length(compact([
          trimspace(var.alto_rpc_url_ssm_parameter_name),
          trimspace(var.alto_send_transaction_rpc_url_ssm_parameter_name),
          trimspace(var.alto_executor_private_keys_ssm_parameter_name),
          trimspace(var.alto_utility_private_key_ssm_parameter_name),
        ]))
      )
      error_message = "Alto RPC, optional send-RPC, executor-key, and utility-key SSM parameter names must be distinct."
    }

    precondition {
      condition = !var.provision_self_hosted_aa || try(
        can(regex("^[1-9][0-9]*$", var.alto_min_executor_balance_wei))
        && can(regex("^[1-9][0-9]*$", var.alto_max_gas_per_user_operation))
        && can(regex("^[1-9][0-9]*$", var.alto_max_gas_per_bundle))
        && tonumber(var.alto_max_gas_per_user_operation) <= tonumber(var.alto_max_gas_per_bundle),
        false
      )
      error_message = "Alto balance/gas limits must be positive decimal integers and max gas per UserOperation must not exceed max gas per bundle."
    }

    precondition {
      condition = !var.provision_self_hosted_aa || (
        (var.alto_container_cpu == 256 && contains([512, 1024, 2048], var.alto_container_memory))
        || (var.alto_container_cpu == 512 && contains([1024, 2048, 3072, 4096], var.alto_container_memory))
        || (var.alto_container_cpu == 1024 && contains([2048, 3072, 4096, 5120, 6144, 7168, 8192], var.alto_container_memory))
        || (var.alto_container_cpu == 2048 && var.alto_container_memory >= 4096 && var.alto_container_memory <= 16384 && var.alto_container_memory % 1024 == 0)
        || (var.alto_container_cpu == 4096 && var.alto_container_memory >= 8192 && var.alto_container_memory <= 30720 && var.alto_container_memory % 1024 == 0)
      )
      error_message = "alto_container_cpu and alto_container_memory must be a supported Fargate CPU/memory combination."
    }

    precondition {
      condition = !var.provision_self_hosted_aa || (
        (var.aa_reconciler_container_cpu == 256 && contains([512, 1024, 2048], var.aa_reconciler_container_memory))
        || (var.aa_reconciler_container_cpu == 512 && contains([1024, 2048, 3072, 4096], var.aa_reconciler_container_memory))
        || (var.aa_reconciler_container_cpu == 1024 && contains([2048, 3072, 4096, 5120, 6144, 7168, 8192], var.aa_reconciler_container_memory))
      )
      error_message = "aa_reconciler_container_cpu and aa_reconciler_container_memory must be a supported small Fargate CPU/memory combination."
    }

    precondition {
      condition = var.aa_reconciler_desired_count == 0 || try(
        can(regex("^[1-9][0-9]*$", var.aa_reconciler_start_block))
        && var.aa_reconciler_start_block_hash == trimspace(var.aa_reconciler_start_block_hash)
        && can(regex("^0x[0-9a-f]{64}$", var.aa_reconciler_start_block_hash))
        && var.aa_reconciler_start_block_hash != "0x0000000000000000000000000000000000000000000000000000000000000000"
        && can(regex("^[1-9][0-9]*$", var.aa_reconciler_poll_seconds))
        && tonumber(var.aa_reconciler_poll_seconds) <= 60
        && can(regex("^[1-9][0-9]*$", var.aa_reconciler_batch_blocks))
        && tonumber(var.aa_reconciler_batch_blocks) <= 10000
        && can(regex("^[1-9][0-9]*$", var.aa_reconciler_max_safe_lag_seconds))
        && tonumber(var.aa_reconciler_max_safe_lag_seconds) >= 60
        && tonumber(var.aa_reconciler_max_safe_lag_seconds) <= 3600
        && can(regex("^[1-9][0-9]*$", var.aa_paymaster_min_deposit_wei))
        && can(regex("^[1-9][0-9]*$", var.aa_paymaster_global_outstanding_wei))
        && tonumber(var.aa_paymaster_min_deposit_wei) >= tonumber(var.aa_paymaster_global_outstanding_wei)
        && var.aa_paymaster_address == trimspace(var.aa_paymaster_address)
        && can(regex("^0x[0-9A-Fa-f]{40}$", trimspace(var.aa_paymaster_address)))
        && lower(trimspace(var.aa_paymaster_address)) != "0x0000000000000000000000000000000000000000"
        && var.aa_paymaster_code_hash == trimspace(var.aa_paymaster_code_hash)
        && can(regex("^0x[0-9a-f]{64}$", var.aa_paymaster_code_hash))
        && var.aa_paymaster_code_hash != "0x0000000000000000000000000000000000000000000000000000000000000000",
        false
      )
      error_message = "Running the AA reconciler requires a positive start block, its canonical lowercase nonzero block hash, the canonical lowercase nonzero deployed-paymaster runtime hash, a minimum EntryPoint deposit at least as large as the native global outstanding-liability cap, poll seconds in 1..60, batch blocks in 1..10000, max safe-head lag in 60..3600 seconds, and a nonzero paymaster address."
    }

    precondition {
      condition = !var.configure_native_aa_backend || (
        var.aa_paymaster_address == trimspace(var.aa_paymaster_address)
        && var.aa_paymaster_signer_address == trimspace(var.aa_paymaster_signer_address)
        && var.aa_paymaster_policy_id == trimspace(var.aa_paymaster_policy_id)
        && var.aa_paymaster_account_code_hash == trimspace(var.aa_paymaster_account_code_hash)
        && var.aa_paymaster_code_hash == trimspace(var.aa_paymaster_code_hash)
        && can(regex("^0x[0-9A-Fa-f]{40}$", trimspace(var.aa_paymaster_address)))
        && lower(trimspace(var.aa_paymaster_address)) != "0x0000000000000000000000000000000000000000"
        && can(regex("^0x[0-9A-Fa-f]{40}$", trimspace(var.aa_paymaster_signer_address)))
        && lower(trimspace(var.aa_paymaster_signer_address)) != "0x0000000000000000000000000000000000000000"
        && can(regex("^0x[0-9A-Fa-f]{64}$", trimspace(var.aa_paymaster_policy_id)))
        && lower(trimspace(var.aa_paymaster_policy_id)) == "0x8dd77324b94da492342191f762a32cdf99e828a7f24d77c8ed5ace90cf4f5ae3"
        && can(regex("^0x[0-9A-Fa-f]{64}$", trimspace(var.aa_paymaster_account_code_hash)))
        && lower(trimspace(var.aa_paymaster_account_code_hash)) == "0x41ee894da413cc99e8dec0a1784470eceb736845ad1591e06ff0ecdf0aca26c9"
        && can(regex("^0x[0-9a-f]{64}$", var.aa_paymaster_code_hash))
        && var.aa_paymaster_code_hash != "0x0000000000000000000000000000000000000000000000000000000000000000"
      )
      error_message = "Native AA backend configuration requires nonzero paymaster/signer addresses, a nonzero deployed-paymaster code hash, the reviewed policy ID, and the reviewed SimpleAccount proxy runtime-code hash."
    }

    precondition {
      condition = !var.configure_native_aa_backend || try(
        can(regex("^[1-9][0-9]*$", var.aa_paymaster_validity_seconds))
        && tonumber(var.aa_paymaster_validity_seconds) >= 1
        && tonumber(var.aa_paymaster_validity_seconds) <= 570
        && can(regex("^[1-9][0-9]*$", var.aa_paymaster_verification_gas_limit))
        && can(regex("^[0-9]+$", var.aa_paymaster_post_op_gas_limit))
        && can(regex("^[1-9][0-9]*$", var.aa_paymaster_max_cost_wei))
        && can(regex("^[1-9][0-9]*$", var.aa_paymaster_account_outstanding_wei))
        && can(regex("^[1-9][0-9]*$", var.aa_paymaster_client_outstanding_wei))
        && can(regex("^[1-9][0-9]*$", var.aa_paymaster_global_outstanding_wei))
        && can(regex("^[1-9][0-9]*$", var.aa_paymaster_account_hourly_wei))
        && can(regex("^[1-9][0-9]*$", var.aa_paymaster_global_hourly_wei))
        && can(regex("^[1-9][0-9]*$", var.aa_paymaster_global_daily_wei))
        && can(regex("^[1-9][0-9]*$", var.aa_paymaster_final_rate_limit_per_minute))
        && tonumber(var.aa_paymaster_final_rate_limit_per_minute) <= 60
        && tonumber(var.aa_paymaster_max_cost_wei) <= tonumber(var.aa_paymaster_account_outstanding_wei)
        && tonumber(var.aa_paymaster_max_cost_wei) <= tonumber(var.aa_paymaster_client_outstanding_wei)
        && tonumber(var.aa_paymaster_account_outstanding_wei) <= tonumber(var.aa_paymaster_global_outstanding_wei)
        && tonumber(var.aa_paymaster_client_outstanding_wei) <= tonumber(var.aa_paymaster_global_outstanding_wei)
        && tonumber(var.aa_paymaster_account_hourly_wei) <= tonumber(var.aa_paymaster_global_hourly_wei)
        && tonumber(var.aa_paymaster_global_hourly_wei) <= tonumber(var.aa_paymaster_global_daily_wei),
        false
      )
      error_message = "Native paymaster limits must be canonical decimal integers with validity 1..570, final issuance rate 1..60, positive budgets/gas (post-op may be zero), and ordered per-operation/account-or-client/global caps."
    }
  }
}
