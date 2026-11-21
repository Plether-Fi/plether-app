# This suite requires the deployed synthetic fixture. Run through
# node scripts/test-terraform-bindings.mjs; active release files stay untouched.
# Synthetic AWS plans: no credentials, live state, deployment or signing.
mock_provider "aws" {
  mock_data "aws_caller_identity" { defaults = { account_id = "932542905614" } }
  mock_data "aws_availability_zones" { defaults = { names = ["ap-southeast-1a", "ap-southeast-1b"] } }
  mock_data "aws_kms_key" {
    defaults = { arn = "arn:aws:kms:ap-southeast-1:932542905614:key/11111111-1111-1111-1111-111111111111" }
  }
  mock_resource "aws_iam_role" {
    override_during = plan
    defaults        = { arn = "arn:aws:iam::932542905614:role/synthetic-test-role" }
  }
  override_resource {
    override_during = plan
    target          = aws_iam_role.api_execution
    values          = { arn = "arn:aws:iam::932542905614:role/synthetic-api-execution" }
  }
  override_resource {
    override_during = plan
    target          = aws_iam_role.aa_reconciler_execution[0]
    values          = { arn = "arn:aws:iam::932542905614:role/synthetic-aa-reconciler-execution" }
  }
  mock_resource "aws_kms_key" {
    override_during = plan
    defaults        = { arn = "arn:aws:kms:ap-southeast-1:932542905614:key/22222222-2222-2222-2222-222222222222" }
  }
  mock_resource "aws_cloudwatch_log_group" {
    override_during = plan
    defaults        = { arn = "arn:aws:logs:ap-southeast-1:932542905614:log-group:/ecs/plether-sepolia" }
  }
  mock_resource "aws_security_group" {
    override_during = plan
    defaults        = { id = "sg-0123456789abcdef0" }
  }
}
variables {
  api_image_digest                       = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
  log_router_image_digest                = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
  db_allocated_storage                   = 20
  db_storage_type                        = "gp3"
  db_final_snapshot_identifier           = "synthetic-test-final-snapshot"
  rpc_url                                = "https://fixture.invalid/spot"
  perps_rpc_url                          = "https://fixture.invalid/perps"
  keeper_private_key                     = "0x1111111111111111111111111111111111111111111111111111111111111111"
  oracle_updater_private_key             = "0x2222222222222222222222222222222222222222222222222222222222222222"
  liquidation_keeper_private_key         = "0x3333333333333333333333333333333333333333333333333333333333333333"
  db_password                            = "synthetic-test-password-only"
  posthog_project_token                  = "phc_test_fixture"
  consolidate_workers                    = true
  provision_self_hosted_aa               = true
  alb_certificate_arn                    = "arn:aws:acm:ap-southeast-1:932542905614:certificate/11111111-1111-1111-1111-111111111111"
  api_hostname                           = "api.sepolia.example.com"
  aa_proxy_origin_token                  = "0000000000000000000000000000000000000000000000000000000000000001"
  aa_paymaster_address                   = "0x9761091045616A388f5fE1433721B272c78fe31b"
  aa_paymaster_signer_address            = "0x1111111111111111111111111111111111111111"
  aa_paymaster_code_hash                 = "0xb8ae276b01850fdbb8d9d7fd32ec7b9b1c7ab7af20f5d62179a76f6b4912c528"
  aa_reconciler_start_block              = "307684600"
  aa_reconciler_start_block_hash         = "0x64210ad75de20ddd2ccf494e7c4d042447f4ea48119419945637f17c271904fe"
  alto_entrypoint_simulation_contract_v8 = "0x9c3c25a084AE8B1df3B2e82bb07Dac4E115C9Ae1"
  alto_pimlico_simulation_contract       = "0x95CC02A7B69dD46c6DD6Bd56132A24a235D58948"
  perps_settlement_token                 = "0x0000000000000000000000000000000000000001"
  perps_order_router                     = "0x0000000000000000000000000000000000000017"
  perps_order_lifecycle_book             = "0x0000000000000000000000000000000000000016"
  perps_cfd_engine                       = "0x0000000000000000000000000000000000000003"
  perps_margin_clearinghouse             = "0x0000000000000000000000000000000000000002"
  perps_house_pool                       = "0x0000000000000000000000000000000000000009"
  perps_plether_oracle                   = "0x0000000000000000000000000000000000000014"
  perps_senior_vault                     = "0x000000000000000000000000000000000000000b"
  perps_junior_vault                     = "0x000000000000000000000000000000000000000c"
  perps_settlement_monitor_lens          = "0x000000000000000000000000000000000000001b"
  perps_cfd_engine_settlement_sidecar    = "0x0000000000000000000000000000000000000006"
  perps_cfd_engine_lens                  = "0x000000000000000000000000000000000000000e"
  perps_account_lens                     = "0x000000000000000000000000000000000000000d"
  vault_history_house_pool_address       = "0x0000000000000000000000000000000000000009"
  vault_history_senior_vault_address     = "0x000000000000000000000000000000000000000b"
  vault_history_junior_vault_address     = "0x000000000000000000000000000000000000000c"
  perps_indexer_start_block              = "123"
  vault_history_deployment_block         = "123"
  insights_active_competition_slug       = "testnet-trading-2026-09"
  insights_competition_release_id        = "testnet-trading-2026-09"
}
run "accept_complete_deployed_bindings" {
  command = plan
  assert {
    condition     = local.sepolia_core.release.phase == "active" && local.sepolia_core.release.deploymentBlock == 123 && length(local.sepolia_core.contracts) > 20
    error_message = "The baseline must use the complete deployed synthetic Core release."
  }
}
run "reject_mismatched_perps_settlement_token" {
  command = plan
  variables { perps_settlement_token = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_order_router" {
  command = plan
  variables { perps_order_router = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_order_lifecycle_book" {
  command = plan
  variables { perps_order_lifecycle_book = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard, terraform_data.perps_candle_rollout_guard, terraform_data.lp_settlement_keeper_guard]
}
run "reject_mismatched_perps_cfd_engine" {
  command = plan
  variables { perps_cfd_engine = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_margin_clearinghouse" {
  command = plan
  variables { perps_margin_clearinghouse = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_house_pool" {
  command = plan
  variables { perps_house_pool = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_plether_oracle" {
  command = plan
  variables { perps_plether_oracle = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_senior_vault" {
  command = plan
  variables { perps_senior_vault = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_junior_vault" {
  command = plan
  variables { perps_junior_vault = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_settlement_monitor_lens" {
  command = plan
  variables { perps_settlement_monitor_lens = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_cfd_engine_settlement_sidecar" {
  command = plan
  variables { perps_cfd_engine_settlement_sidecar = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_cfd_engine_lens" {
  command = plan
  variables { perps_cfd_engine_lens = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_account_lens" {
  command = plan
  variables { perps_account_lens = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_vault_history_house_pool_address" {
  command = plan
  variables { vault_history_house_pool_address = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_vault_history_senior_vault_address" {
  command = plan
  variables { vault_history_senior_vault_address = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_vault_history_junior_vault_address" {
  command = plan
  variables { vault_history_junior_vault_address = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_perps_indexer_start_block" {
  command = plan
  variables { perps_indexer_start_block = "124" }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_vault_history_deployment_block" {
  command = plan
  variables { vault_history_deployment_block = "124" }
  expect_failures = [terraform_data.deployment_target_guard]
}

# Kernel infrastructure uses the full module with mock AWS resources.
run "wire_kernel_execution_to_the_private_bundler_and_reconciler" {
  command = plan

  variables {
    alto_entrypoint_version                = "0.9"
    alto_entrypoint_address                = "0x433709009B8330FDa32311DF1C2AFA402eD8D009"
    alto_entrypoint_simulation_contract_v8 = ""
    alto_entrypoint_simulation_contract_v9 = "0x0000000000000000000000000000000000000022"
    alto_desired_count                     = 1
    aa_reconciler_desired_count            = 1
    enable_kernel_aa_eth                   = true
    enable_kernel_aa_sponsorship           = true
    enable_kernel_aa_protection_commits    = true
    aa_kernel_profile_json = jsonencode({
      deployment             = "fixture-kernel"
      chainId                = 421614
      entryPoint             = "0x433709009B8330FDa32311DF1C2AFA402eD8D009"
      entryPointVersion      = "0.9"
      entryPointCodeHash     = format("0x%064d", 1)
      accountVersion         = "kernel-v4-uups"
      accountIndex           = 0
      factory                = format("0x%040d", 31)
      factoryCodeHash        = format("0x%064d", 2)
      implementation         = format("0x%040d", 32)
      implementationCodeHash = format("0x%064d", 3)
      validator              = format("0x%040d", 33)
      validatorCodeHash      = format("0x%064d", 4)
      accountCodeHash        = format("0x%064d", 5)
    })
    aa_kernel_sponsorship_json = jsonencode({
      paymaster            = format("0x%040d", 34)
      paymasterCodeHash    = format("0x%064d", 6)
      policyId             = format("0x%064d", 7)
      signerAddress        = format("0x%040d", 35)
      deploymentBlock      = 123
      deploymentBlockHash  = format("0x%064d", 8)
      minDepositWei        = 1000
      verificationGasLimit = 150000
      validitySeconds      = 300
      canaryOwners         = [format("0x%040d", 36)]
      budget = {
        maxCostWei            = 10
        accountOutstandingWei = 100
        clientOutstandingWei  = 100
        globalOutstandingWei  = 1000
        accountHourlyWei      = 100
        globalHourlyWei       = 1000
        globalDailyWei        = 10000
      }
    })
  }

  assert {
    condition     = local.kernel_aa_sponsorship_enabled && local.aa_gateway_enabled && length(local.native_aa_environment) == 0
    error_message = "The Kernel deployment must enable its own gateway and sponsorship while omitting native v0.8 configuration."
  }
  assert {
    condition = contains([for entry in local.kernel_aa_environment : entry.name], "AA_KERNEL_ALTO_RPC_URL") && (
      contains([for entry in local.kernel_aa_environment : entry.name], "AA_KERNEL_PAYMASTER_KMS_KEY_ID")
      && length(aws_iam_role_policy.api_paymaster_kms_signer) == 1
      && contains(local.kernel_aa_environment, { name = "AA_KERNEL_PROTECTION_COMMITS_ENABLED", value = "true" })
    )
    error_message = "Kernel API routes to the private Alto service and acquires the narrow paymaster signing grant."
  }
  assert {
    condition = contains([for entry in local.kernel_aa_reconciler_environment : entry.name], "AA_KERNEL_SPONSORSHIP_JSON") && (
      !contains([for entry in local.kernel_aa_reconciler_environment : entry.name], "AA_KERNEL_PAYMASTER_KMS_KEY_ID")
      && local.kernel_aa_secrets[0].name == "AA_SECURITY_RPC_URL"
    )
    error_message = "The reconciler receives the current paymaster anchor and independent RPC without signing access."
  }
}
