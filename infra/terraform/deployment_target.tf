locals {
  # Preserve existing resource identity and Terraform state addresses.
  deployment_name = var.environment
  sepolia_core    = jsondecode(file("${path.module}/../../config/perps/mock-usdg-prepared.json"))
}
resource "terraform_data" "deployment_target_guard" {
  lifecycle {
    precondition {
      condition = !(local.native_aa_backend_configured || anytrue([
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
        local.sepolia_core.schemaVersion == 4
        && contains(["active", "seeded", "deployed"], local.sepolia_core.release.phase)
        && var.perps_chain_id == tostring(local.sepolia_core.network.chainId)
        && var.perps_indexer_start_block == tostring(local.sepolia_core.release.deploymentBlock)
        && var.vault_history_deployment_block == tostring(local.sepolia_core.release.deploymentBlock)
        && alltrue([
          lower(var.perps_settlement_token) == try(lower(local.sepolia_core.contracts.settlementToken.address), ""),
          lower(var.perps_order_router) == try(lower(local.sepolia_core.contracts.orderRouter.address), ""),
          lower(var.perps_order_lifecycle_book) == try(lower(local.sepolia_core.contracts.orderLifecycleBook.address), ""),
          lower(var.perps_cfd_engine) == try(lower(local.sepolia_core.contracts.cfdEngine.address), ""),
          lower(var.perps_margin_clearinghouse) == try(lower(local.sepolia_core.contracts.marginClearinghouse.address), ""),
          lower(var.perps_house_pool) == try(lower(local.sepolia_core.contracts.housePool.address), ""),
          lower(var.perps_plether_oracle) == try(lower(local.sepolia_core.contracts.pletherOracle.address), ""),
          lower(var.perps_senior_vault) == try(lower(local.sepolia_core.contracts.seniorVault.address), ""),
          lower(var.perps_junior_vault) == try(lower(local.sepolia_core.contracts.juniorVault.address), ""),
          lower(var.perps_settlement_monitor_lens) == try(lower(local.sepolia_core.contracts.settlementMonitorLens.address), ""),
          lower(var.perps_cfd_engine_settlement_sidecar) == try(lower(local.sepolia_core.contracts.cfdEngineSettlementSidecar.address), ""),
          lower(var.perps_cfd_engine_lens) == try(lower(local.sepolia_core.contracts.cfdEngineLens.address), ""),
          lower(var.perps_account_lens) == try(lower(local.sepolia_core.contracts.cfdEngineAccountLens.address), ""),
          lower(var.vault_history_house_pool_address) == try(lower(local.sepolia_core.contracts.housePool.address), ""),
          lower(var.vault_history_senior_vault_address) == try(lower(local.sepolia_core.contracts.seniorVault.address), ""),
          lower(var.vault_history_junior_vault_address) == try(lower(local.sepolia_core.contracts.juniorVault.address), ""),
        ])
      )
      error_message = "Configured perps consumers, native AA and the bound Insights competition require the complete verified canonical Core deployment bindings and indexing anchor; the prepared release is unavailable."
    }
    precondition {
      condition     = !var.enable_aa_readiness_enforcement || (var.environment == "sepolia" && var.aws_region == "ap-southeast-1" && local.native_aa_backend_configured)
      error_message = "Readiness enforcement requires the configured Singapore Sepolia native-AA deployment."
    }
  }
}
