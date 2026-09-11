# Logical network and deployment identity are deliberately separate. The empty
# default preserves existing Singapore/mainnet resource names and state.
variable "deployment_id" {
  type    = string
  default = ""
  validation {
    condition     = contains(["", "sepolia-aa-temp"], var.deployment_id)
    error_message = "Only the legacy identity or sepolia-aa-temp is supported."
  }
}

variable "frankfurt_activation_stage" {
  type    = string
  default = "dormant"
  validation {
    condition     = contains(["dormant", "api-readonly", "aa-prepared", "aa-qualification", "aa-canary", "trading-prepared", "trading-canary"], var.frankfurt_activation_stage)
    error_message = "Select a supported preparation stage, aa-canary, or trading-canary."
  }
}

locals {
  deployment_name              = var.deployment_id == "" ? var.environment : var.deployment_id
  frankfurt_preparation        = var.deployment_id == "sepolia-aa-temp"
  frankfurt_aa_qualification   = contains(["aa-qualification", "aa-canary", "trading-prepared", "trading-canary"], var.frankfurt_activation_stage)
  frankfurt_aa_canary          = contains(["aa-canary", "trading-prepared", "trading-canary"], var.frankfurt_activation_stage)
  frankfurt_trading_configured = contains(["trading-prepared", "trading-canary"], var.frankfurt_activation_stage)
  frankfurt_trading_canary     = var.frankfurt_activation_stage == "trading-canary"
  temporary_core               = jsondecode(file("${path.module}/../../config/perps/arbitrum-sepolia-v2.json"))
}

resource "terraform_data" "deployment_target_guard" {
  lifecycle {
    precondition {
      condition = !var.enable_aa_readiness_enforcement || (
        local.frankfurt_trading_canary && local.frankfurt_preparation
        && var.aws_region == "eu-central-1" && var.environment == "sepolia"
        && !var.aa_native_global_rollout_enabled
      )
      error_message = "Readiness blocking is restricted to the Frankfurt trading canary after verification; other deployments remain observation-only."
    }
    precondition {
      condition = tonumber(var.aa_reconciler_max_safe_lag_seconds) <= 600 || (
        local.frankfurt_preparation && var.environment == "sepolia"
        && var.perps_chain_id == "421614" && !var.aa_native_global_rollout_enabled
        && lower(var.aa_native_canary_owners) == "0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b"
        && tonumber(var.aa_reconciler_max_safe_lag_seconds) <= 1800
      )
      error_message = "Safe-head lag above 600 seconds is restricted to the approved Frankfurt Sepolia canary, with a maximum of 1800 seconds."
    }
    precondition {
      condition = !local.frankfurt_preparation || (
        var.environment == "sepolia" && var.aws_region == "eu-central-1"
        && var.expected_aws_account_id == "932542905614" && var.perps_chain_id == "421614"
      )
      error_message = "sepolia-aa-temp is pinned to account 932542905614, Frankfurt, Arbitrum Sepolia."
    }
    precondition {
      condition = !local.frankfurt_preparation || (
        var.api_desired_count == (var.frankfurt_activation_stage == "dormant" ? 0 : 1)
        && var.consolidate_workers && var.workers_desired_count == (local.frankfurt_trading_canary ? 1 : 0)
        && var.liquidation_worker_desired_count == (local.frankfurt_trading_canary ? 1 : 0)
        && var.protection_worker_desired_count == (local.frankfurt_trading_canary ? 1 : 0)
        && var.alto_desired_count == (local.frankfurt_aa_qualification ? 1 : 0)
        && var.aa_reconciler_desired_count == (local.frankfurt_aa_qualification ? 1 : 0)
        && var.configure_native_aa_backend == local.frankfurt_aa_qualification
        && var.enable_native_aa_sponsorship == local.frankfurt_aa_canary
        && var.enable_native_aa_submission == local.frankfurt_aa_canary
        && var.protection_worker_execution_enabled == local.frankfurt_trading_canary
        && var.aa_protection_commits_enabled == local.frankfurt_trading_canary
        && (local.frankfurt_trading_canary ? contains(["off", "execute"], var.lp_settlement_mode) : var.lp_settlement_mode == "off")
        && !var.aa_native_global_rollout_enabled && !var.enable_aa_sponsorship
        && !var.provision_aa_proxy && !var.provision_insights_registration
        && !var.enable_insights_registration
        && (local.frankfurt_trading_configured ? (var.faucet_private_key != "" && var.faucet_proxy_origin_token != "") : var.faucet_private_key == "")
      )
      error_message = "Only explicit canary stages permit AA; only trading-canary permits exactly one trading worker set and authenticated faucet. Registration and global rollout remain prohibited."
    }
    precondition {
      condition = !local.frankfurt_preparation || !contains(["aa-prepared", "aa-qualification", "aa-canary", "trading-prepared", "trading-canary"], var.frankfurt_activation_stage) || (
        lower(var.aa_paymaster_address) == "0x9761091045616a388f5fe1433721b272c78fe31b"
        && lower(var.aa_paymaster_signer_address) == "0x015736e1f47e37938236e481f7a3b7c57f922b80"
        && var.aa_paymaster_code_hash == "0xb8ae276b01850fdbb8d9d7fd32ec7b9b1c7ab7af20f5d62179a76f6b4912c528"
        && var.aa_reconciler_start_block == "307684600"
        && var.aa_reconciler_start_block_hash == "0x64210ad75de20ddd2ccf494e7c4d042447f4ea48119419945637f17c271904fe"
        && lower(var.aa_native_canary_owners) == "0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b"
      )
      error_message = "Frankfurt qualification must use the verified 2026-09-11 paymaster deployment, KMS signer, start block, runtime hash and single approved owner."
    }
    precondition {
      condition = !local.frankfurt_preparation || !local.frankfurt_aa_canary || (
        var.operations_alarm_sns_topic_arn == "arn:aws:sns:eu-central-1:932542905614:plether-sepolia-aa-temp-operations"
      )
      error_message = "Frankfurt canary requires the configured operator alarm topic. Confirm its subscription before activation."
    }
    precondition {
      condition = !local.frankfurt_preparation || (
        local.temporary_core.release.version == "v1.2.3"
        && local.temporary_core.release.sourceCommit == "ffe45937b7f38133133ad292c5435828bf99357d"
        && alltrue([for binding in [
          [var.perps_order_router, local.temporary_core.contracts.orderRouter.address],
          [var.perps_cfd_engine, local.temporary_core.contracts.cfdEngine.address],
          [var.perps_margin_clearinghouse, local.temporary_core.contracts.marginClearinghouse.address],
          [var.perps_order_lifecycle_book, local.temporary_core.contracts.orderLifecycleBook.address],
          [var.perps_plether_oracle, local.temporary_core.contracts.pletherOracle.address],
          [var.perps_usdc, local.temporary_core.contracts.mockUsdc.address],
          [var.perps_house_pool, local.temporary_core.contracts.housePool.address],
          [var.perps_senior_vault, local.temporary_core.contracts.seniorVault.address],
          [var.perps_junior_vault, local.temporary_core.contracts.juniorVault.address],
          [var.perps_settlement_monitor_lens, local.temporary_core.contracts.settlementMonitorLens.address],
          [var.perps_cfd_engine_settlement_sidecar, local.temporary_core.contracts.cfdEngineSettlementSidecar.address],
          [var.perps_cfd_engine_lens, local.temporary_core.contracts.cfdEngineLens.address],
          [var.perps_account_lens, local.temporary_core.contracts.cfdEngineAccountLens.address],
          [var.vault_history_house_pool_address, local.temporary_core.contracts.housePool.address],
        ] : lower(binding[0]) == lower(binding[1])])
        && var.perps_indexer_start_block == tostring(local.temporary_core.release.deploymentBlock)
      )
      error_message = "Frankfurt must reuse the checked-in Core v1.2.3 deployment; no Core deployment or older release bindings."
    }
    precondition {
      condition = !local.frankfurt_preparation || (
        var.db_snapshot_identifier == null
        && var.cors_origins == "http://127.0.0.1:5173 http://localhost:5173"
        && (var.pyth_api_key_ssm_parameter_name == null || startswith(coalesce(var.pyth_api_key_ssm_parameter_name, "unset"), "/plether/sepolia-aa-temp/"))
      )
      error_message = "Frankfurt requires a fresh database, localhost-only CORS, and its own Pyth secret namespace."
    }
    precondition {
      condition = !local.frankfurt_preparation || (
        (var.operations_alarm_sns_topic_arn == "" || startswith(var.operations_alarm_sns_topic_arn, "arn:aws:sns:eu-central-1:932542905614:plether-sepolia-aa-temp-"))
        && var.alb_certificate_arn == "" && var.api_hostname == ""
      )
      error_message = "Frankfurt requires private Session Manager access without a certificate/custom hostname, and isolated alarm routing."
    }
  }
}

output "deployment_target" {
  value = {
    id               = local.deployment_name
    region           = var.aws_region
    account_id       = var.expected_aws_account_id
    chain_id         = var.perps_chain_id
    preparation_only = local.frankfurt_preparation
  }
}
