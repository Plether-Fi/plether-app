locals {
  # Preserve existing resource identity and Terraform state addresses.
  deployment_name = var.environment
  sepolia_core    = jsondecode(file("${path.module}/../../config/perps/arbitrum-sepolia-v2.json"))
}
resource "terraform_data" "deployment_target_guard" {
  lifecycle {
    precondition {
      condition = !local.native_aa_backend_configured || (
        local.sepolia_core.release.version == "v1.2.3"
        && var.perps_chain_id == tostring(local.sepolia_core.network.chainId)
        && var.perps_indexer_start_block == tostring(local.sepolia_core.release.deploymentBlock)
        && var.vault_history_deployment_block == tostring(local.sepolia_core.release.deploymentBlock)
        && alltrue([
          lower(var.perps_usdc) == lower(local.sepolia_core.contracts.mockUsdc.address),
          lower(var.perps_order_router) == lower(local.sepolia_core.contracts.orderRouter.address),
          lower(var.perps_order_lifecycle_book) == lower(local.sepolia_core.contracts.orderLifecycleBook.address),
          lower(var.perps_cfd_engine) == lower(local.sepolia_core.contracts.cfdEngine.address),
          lower(var.perps_margin_clearinghouse) == lower(local.sepolia_core.contracts.marginClearinghouse.address),
          lower(var.perps_house_pool) == lower(local.sepolia_core.contracts.housePool.address),
          lower(var.perps_plether_oracle) == lower(local.sepolia_core.contracts.pletherOracle.address),
          lower(var.perps_senior_vault) == lower(local.sepolia_core.contracts.seniorVault.address),
          lower(var.perps_junior_vault) == lower(local.sepolia_core.contracts.juniorVault.address),
          lower(var.perps_settlement_monitor_lens) == lower(local.sepolia_core.contracts.settlementMonitorLens.address),
          lower(var.perps_cfd_engine_settlement_sidecar) == lower(local.sepolia_core.contracts.cfdEngineSettlementSidecar.address),
          lower(var.perps_cfd_engine_lens) == lower(local.sepolia_core.contracts.cfdEngineLens.address),
          lower(var.perps_account_lens) == lower(local.sepolia_core.contracts.cfdEngineAccountLens.address),
          lower(var.vault_history_house_pool_address) == lower(local.sepolia_core.contracts.housePool.address),
          lower(var.vault_history_senior_vault_address) == lower(local.sepolia_core.contracts.seniorVault.address),
          lower(var.vault_history_junior_vault_address) == lower(local.sepolia_core.contracts.juniorVault.address),
        ])
      )
      error_message = "Native AA requires the complete checked-in Core v1.2.3 deployment bindings and indexing anchor."
    }
    precondition {
      condition     = !var.enable_aa_readiness_enforcement || (var.environment == "sepolia" && var.aws_region == "ap-southeast-1" && local.native_aa_backend_configured)
      error_message = "Readiness enforcement requires the configured Singapore Sepolia native-AA deployment."
    }
  }
}
