# Synthetic AWS plans: no credentials, live state, deployment or signing.
mock_provider "aws" {
  mock_data "aws_caller_identity" { defaults = { account_id = "932542905614" } }
  mock_data "aws_availability_zones" { defaults = { names = ["ap-southeast-1a", "ap-southeast-1b"] } }
  mock_resource "aws_security_group" {
    override_during = plan
    defaults = { id = "sg-0123456789abcdef0" }
  }
}
variables {
  db_allocated_storage = 20
  db_storage_type = "gp3"
  db_final_snapshot_identifier = "synthetic-test-final-snapshot"
  rpc_url = "https://fixture.invalid/spot"
  perps_rpc_url = "https://fixture.invalid/perps"
  keeper_private_key = "0x1111111111111111111111111111111111111111111111111111111111111111"
  oracle_updater_private_key = "0x2222222222222222222222222222222222222222222222222222222222222222"
  liquidation_keeper_private_key = "0x3333333333333333333333333333333333333333333333333333333333333333"
  db_password = "synthetic-test-password-only"
  posthog_project_token = "phc_test_fixture"
  consolidate_workers = true
  provision_self_hosted_aa = true
  alb_certificate_arn = "arn:aws:acm:ap-southeast-1:932542905614:certificate/11111111-1111-1111-1111-111111111111"
  api_hostname = "api.sepolia.example.com"
  aa_proxy_origin_token = "0000000000000000000000000000000000000000000000000000000000000001"
  aa_paymaster_address = "0x9761091045616A388f5fE1433721B272c78fe31b"
  aa_paymaster_signer_address = "0x1111111111111111111111111111111111111111"
  aa_paymaster_code_hash = "0xb8ae276b01850fdbb8d9d7fd32ec7b9b1c7ab7af20f5d62179a76f6b4912c528"
  aa_reconciler_start_block = "307684600"
  aa_reconciler_start_block_hash = "0x64210ad75de20ddd2ccf494e7c4d042447f4ea48119419945637f17c271904fe"
  alto_entrypoint_simulation_contract_v8 = "0x9c3c25a084AE8B1df3B2e82bb07Dac4E115C9Ae1"
  alto_pimlico_simulation_contract = "0x95CC02A7B69dD46c6DD6Bd56132A24a235D58948"
}
run "preserve_singapore_identity" {
  command = plan
  assert {
    condition = aws_ecs_cluster.main.name == "plether-sepolia" && aws_ssm_parameter.perps_rpc_url.name == "/plether/sepolia/perps-rpc-url" && !aws_lb.api.internal && aws_iam_openid_connect_provider.github_actions.url == "https://token.actions.githubusercontent.com"
    error_message = "Existing Singapore identities, public API and shared OIDC provider must be preserved."
  }
  assert {
    condition = !local.native_aa_backend_configured && aws_ecs_service.alto[0].desired_count == 0 && aws_ecs_service.aa_reconciler[0].desired_count == 0
    error_message = "Preparation must not start services or enable issuance."
  }
}
run "public_sepolia" {
  command = plan
  variables {
    configure_native_aa_backend = true
    alto_desired_count = 1
    aa_reconciler_desired_count = 1
    aa_native_global_rollout_enabled = true
    aa_native_canary_owners = ""
    enable_native_aa_sponsorship = true
    enable_native_aa_submission = true
    aa_funding_monitor_image = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-api-sepolia@sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    aa_funding_monitors = [
      { component = "alto", address = "0x1111111111111111111111111111111111111111", gasLimit = "30000000", valueWei = "0", feeBufferBps = "2500" },
      { component = "keeper", address = "0x2222222222222222222222222222222222222222", gasLimit = "30000000", valueWei = "0", feeBufferBps = "2500" },
      { component = "oracle", address = "0x3333333333333333333333333333333333333333", gasLimit = "30000000", valueWei = "0", feeBufferBps = "2500" },
      { component = "liquidation", address = "0x4444444444444444444444444444444444444444", gasLimit = "30000000", valueWei = "0", feeBufferBps = "2500" },
      { component = "protection", address = "0x5555555555555555555555555555555555555555", gasLimit = "30000000", valueWei = "0", feeBufferBps = "2500" },
      { component = "lp_settlement", address = "0x6666666666666666666666666666666666666666", gasLimit = "30000000", valueWei = "0", feeBufferBps = "2500" },
    ]
  }
  assert {
    condition = local.alto_safe_mode && local.native_aa_sponsorship_enabled && var.aa_native_global_rollout_enabled && var.aa_native_canary_owners == ""
    error_message = "Public Sepolia must retain Alto safe mode."
  }
  assert {
    condition = length(local.aa_funding_containers) == 1 && local.aa_funding_containers[0].essential == false && length(local.aa_funding_components) == 6 && !contains([for s in local.aa_funding_containers[0].secrets : s.name], "PRIVATE_KEY")
    error_message = "Public funding observer must be nonessential, complete and without signing authority."
  }
}
run "reject_public_without_funding" {
  command = plan
  variables {
    configure_native_aa_backend = true
    alto_desired_count = 1
    aa_reconciler_desired_count = 1
    aa_native_global_rollout_enabled = true
    aa_native_canary_owners = ""
  }
  expect_failures = [terraform_data.self_hosted_aa_guard]
}
run "reject_mismatched_core" {
  command = plan
  variables {
    configure_native_aa_backend = true
    alto_desired_count = 1
    aa_reconciler_desired_count = 1
    perps_senior_vault = "0x1111111111111111111111111111111111111111"
  }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_public_unconfigured" {
  command = plan
  variables { aa_native_global_rollout_enabled = true }
  expect_failures = [terraform_data.self_hosted_aa_guard]
}
run "reject_public_mainnet_chain" {
  command = plan
  variables {
    aa_native_global_rollout_enabled = true
    perps_chain_id = "42161"
  }
  expect_failures = [terraform_data.self_hosted_aa_guard]
}
run "reject_extended_safe_lag" {
  command = plan
  variables { aa_reconciler_max_safe_lag_seconds = "1800" }
  expect_failures = [var.aa_reconciler_max_safe_lag_seconds]
}
run "reject_incomplete_funding" {
  command = plan
  variables {
    aa_funding_monitors = [{ component = "alto", address = "0x1111111111111111111111111111111111111111", gasLimit = "30000000", valueWei = "0", feeBufferBps = "2500" }]
  }
  expect_failures = [terraform_data.aa_funding_inventory_guard]
}
run "reject_unconfigured_readiness_enforcement" {
  command = plan
  variables { enable_aa_readiness_enforcement = true }
  expect_failures = [terraform_data.deployment_target_guard]
}
