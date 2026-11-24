variable "perps_release_manifest_path" {
  type        = string
  default     = ""
  description = "Path (relative to infra/terraform) to the canonical schema-4 Perps release manifest JSON. Defaults to ../../config/perps/arbitrum-sepolia-v2.0.0.json on Sepolia; required on mainnet whenever Perps or native AA bindings are configured."

  validation {
    condition     = var.perps_release_manifest_path == "" || can(regex("^(\\.\\./)+config/perps/[A-Za-z0-9_.-]+\\.json$", var.perps_release_manifest_path))
    error_message = "perps_release_manifest_path must be empty or a relative path under config/perps/*.json."
  }
}

locals {
  # Preserve existing resource identity and Terraform state addresses.
  deployment_name            = var.environment
  expected_perps_chain_id    = var.environment == "mainnet" ? "42161" : "421614"
  sepolia_core               = jsondecode(file("${path.module}/../../config/perps/arbitrum-sepolia-v2.0.0.json"))
  effective_manifest_relpath = var.perps_release_manifest_path != "" ? var.perps_release_manifest_path : (var.environment == "sepolia" ? "../../config/perps/arbitrum-sepolia-v2.0.0.json" : "")
  active_core                = local.effective_manifest_relpath != "" ? jsondecode(file("${path.module}/${local.effective_manifest_relpath}")) : null
  runtime_perps_release_manifest_path = (
    local.effective_manifest_relpath != ""
    ? "/app/config/perps/${basename(local.effective_manifest_relpath)}"
    : "/app/config/perps/arbitrum-sepolia-v2.0.0.json"
  )
}
resource "terraform_data" "deployment_target_guard" {
  lifecycle {
    precondition {
      condition = !(local.native_aa_backend_configured || local.kernel_aa_backend_configured || anytrue([
        var.perps_settlement_token != "", var.perps_order_router != "",
        var.perps_cfd_engine != "", var.perps_margin_clearinghouse != "",
        var.perps_house_pool != "", var.perps_plether_oracle != "",
        var.perps_order_lifecycle_book != "", var.perps_cfd_engine_lens != "",
        var.perps_cfd_engine_settlement_sidecar != "", var.perps_account_lens != "",
        var.perps_senior_vault != "", var.perps_junior_vault != "",
        var.perps_settlement_monitor_lens != "", var.vault_history_house_pool_address != "",
        var.vault_history_senior_vault_address != "", var.vault_history_junior_vault_address != "",
        ]) || (
        var.insights_active_competition_slug == "testnet-trading-2026-09"
        && var.insights_competition_release_id != ""
        )) || (
        local.active_core != null
        && try(local.active_core.schemaVersion, 0) == 4
        && contains(["active", "seeded", "deployed"], try(local.active_core.release.phase, ""))
        && var.perps_chain_id == local.expected_perps_chain_id
        && var.perps_chain_id == tostring(try(local.active_core.network.chainId, ""))
        && (var.environment != "mainnet" || try(local.active_core.settlementToken.isMock, true) == false)
        && var.perps_indexer_start_block == tostring(try(local.active_core.release.deploymentBlock, ""))
        && var.vault_history_deployment_block == tostring(try(local.active_core.release.deploymentBlock, ""))
        && alltrue([
          lower(var.perps_settlement_token) == try(lower(local.active_core.contracts.settlementToken.address), ""),
          lower(var.perps_order_router) == try(lower(local.active_core.contracts.orderRouter.address), ""),
          lower(var.perps_order_lifecycle_book) == try(lower(local.active_core.contracts.orderLifecycleBook.address), ""),
          lower(var.perps_cfd_engine) == try(lower(local.active_core.contracts.cfdEngine.address), ""),
          lower(var.perps_margin_clearinghouse) == try(lower(local.active_core.contracts.marginClearinghouse.address), ""),
          lower(var.perps_house_pool) == try(lower(local.active_core.contracts.housePool.address), ""),
          lower(var.perps_plether_oracle) == try(lower(local.active_core.contracts.pletherOracle.address), ""),
          lower(var.perps_senior_vault) == try(lower(local.active_core.contracts.seniorVault.address), ""),
          lower(var.perps_junior_vault) == try(lower(local.active_core.contracts.juniorVault.address), ""),
          lower(var.perps_settlement_monitor_lens) == try(lower(local.active_core.contracts.settlementMonitorLens.address), ""),
          lower(var.perps_cfd_engine_settlement_sidecar) == try(lower(local.active_core.contracts.cfdEngineSettlementSidecar.address), ""),
          lower(var.perps_cfd_engine_lens) == try(lower(local.active_core.contracts.cfdEngineLens.address), ""),
          lower(var.perps_account_lens) == try(lower(local.active_core.contracts.cfdEngineAccountLens.address), ""),
          lower(var.vault_history_house_pool_address) == try(lower(local.active_core.contracts.housePool.address), ""),
          lower(var.vault_history_senior_vault_address) == try(lower(local.active_core.contracts.seniorVault.address), ""),
          lower(var.vault_history_junior_vault_address) == try(lower(local.active_core.contracts.juniorVault.address), ""),
        ])
      )
      error_message = "Configured perps consumers, native AA and the bound Insights competition require the complete verified canonical Core deployment bindings, matching chain ID, non-mock mainnet token, and indexing anchor."
    }
    precondition {
      condition     = !var.enable_aa_readiness_enforcement || (contains(["sepolia", "mainnet"], var.environment) && var.aws_region == "ap-southeast-1" && local.native_aa_backend_configured)
      error_message = "Readiness enforcement requires a configured Singapore native-AA deployment."
    }
  }
}
