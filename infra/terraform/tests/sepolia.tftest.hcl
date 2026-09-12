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
}
run "preserve_singapore_identity" {
  command = plan
  assert {
    condition     = local.alto_safe_mode
    error_message = "Safe mode must remain the default without the explicit Sepolia exception."
  }
  assert {
    condition     = aws_ecs_service.api.platform_version == "1.4.0"
    error_message = "AA administration requires the API source service to pin reviewed Fargate 1.4.0."
  }
  assert {
    condition     = aws_ecs_cluster.main.name == "plether-sepolia" && aws_ssm_parameter.perps_rpc_url.name == "/plether/sepolia/perps-rpc-url" && !aws_lb.api.internal && aws_iam_openid_connect_provider.github_actions.url == "https://token.actions.githubusercontent.com"
    error_message = "Existing Singapore identities, public API and shared OIDC provider must be preserved."
  }
  assert {
    condition     = !local.native_aa_backend_configured && aws_ecs_service.alto[0].desired_count == 0 && aws_ecs_service.aa_reconciler[0].desired_count == 0
    error_message = "Preparation must not start services or enable issuance."
  }
}
run "deployment_metadata_permissions" {
  command = plan
  assert {
    condition = one([
      for s in jsondecode(aws_iam_role_policy.github_deploy_self_hosted_aa[0].policy).Statement : s
      if s.Sid == "InspectAaRpcParameterMetadata"
      ]) == {
      Sid       = "InspectAaRpcParameterMetadata"
      Effect    = "Allow"
      Action    = "ssm:DescribeParameters"
      Resource  = "*"
      Condition = { StringEquals = { "aws:RequestedRegion" = "ap-southeast-1" } }
    }
    error_message = "The metadata list grant must be read-only and region-bound."
  }
  assert {
    condition = alltrue([
      for s in jsondecode(aws_iam_role_policy.github_deploy_self_hosted_aa[0].policy).Statement :
      s.Action == "kms:DescribeKey" && toset(s.Resource) == toset([data.aws_kms_key.aa_ssm_managed[0].arn])
      if s.Sid == "InspectAaRpcEncryptionKeys"
    ])
    error_message = "Default-key inspection must not grant decrypt/sign or wildcard key access."
  }
  assert {
    condition = alltrue([
      for s in jsondecode(aws_iam_role_policy.github_deploy_self_hosted_aa[0].policy).Statement :
      toset(s.Action) == toset(["iam:GetRolePolicy", "iam:ListRolePolicies"]) &&
      toset(s.Resource) == toset([aws_iam_role.api_execution.arn, aws_iam_role.aa_reconciler_execution[0].arn])
      if s.Sid == "InspectAaRpcConsumerPolicies"
    ])
    error_message = "Consumer-policy inspection must be limited to the API/reconciler roles."
  }
}
run "approved_sepolia_alto_exception" {
  command = plan
  variables {
    alto_sepolia_safe_mode_exception = true
  }
  assert {
    condition = (!local.alto_safe_mode &&
      contains(local.alto_validation_exception_environment, { name = "PLETHER_ALTO_NETWORK_CHAIN_ID", value = "421614" }) &&
    contains(local.alto_validation_exception_environment, { name = "PLETHER_ALTO_VALIDATION_POLICY", value = "sepolia-testnet-exception-v1" }))
    error_message = "The Sepolia exception must retain normal validation and exact chain/policy metadata."
  }
}
run "reject_alto_exception_other_chain" {
  command = plan
  variables {
    alto_sepolia_safe_mode_exception   = true
    perps_chain_id                     = "42161"
    aa_reconciler_max_safe_lag_seconds = "600"
  }
  assert {
    condition     = local.alto_safe_mode
    error_message = "An invalid exception must not disable safe mode."
  }
  expect_failures = [terraform_data.self_hosted_aa_guard]
}
run "deployment_customer_key_metadata_permissions" {
  command = plan
  variables {
    aa_rpc_mode                                        = "dual-independent"
    aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/secondary-rpc-url"
    aa_reconciler_secondary_rpc_url_kms_key_arn        = "arn:aws:kms:ap-southeast-1:932542905614:key/33333333-3333-3333-3333-333333333333"
  }
  assert {
    condition = alltrue([
      for s in jsondecode(aws_iam_role_policy.github_deploy_self_hosted_aa[0].policy).Statement :
      s.Action == "kms:DescribeKey" && toset(s.Resource) == toset([
        data.aws_kms_key.aa_ssm_managed[0].arn,
        var.aa_reconciler_secondary_rpc_url_kms_key_arn,
      ])
      if s.Sid == "InspectAaRpcEncryptionKeys"
    ])
    error_message = "Customer-key metadata inspection must name only the configured key and SSM default key."
  }
}
run "public_sepolia" {
  command = plan
  variables {
    configure_native_aa_backend      = true
    alto_sepolia_safe_mode_exception = true
    alto_desired_count               = 1
    aa_reconciler_desired_count      = 1
    aa_native_global_rollout_enabled = true
    aa_native_canary_owners          = ""
    enable_native_aa_sponsorship     = true
    enable_native_aa_submission      = true
    aa_funding_monitor_image         = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-api-sepolia@sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
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
    condition     = !local.alto_safe_mode && local.native_aa_sponsorship_enabled && var.aa_native_global_rollout_enabled && var.aa_native_canary_owners == ""
    error_message = "Public Sepolia can use the explicit approved exception without changing issuance or cohort requirements."
  }
  assert {
    condition     = length(local.aa_funding_containers) == 1 && local.aa_funding_containers[0].essential == false && length(local.aa_funding_components) == 6 && !contains([for s in local.aa_funding_containers[0].secrets : s.name], "PRIVATE_KEY")
    error_message = "Public funding observer must be nonessential, complete and without signing authority."
  }
  assert {
    condition = (
      aws_cloudwatch_log_metric_filter.aa_execution_out_of_gas[0].pattern == "{ $.event = \"aa_execution_diagnosed\" && $.reason_code = \"USER_OPERATION_OUT_OF_GAS\" }" &&
      aws_cloudwatch_metric_alarm.aa_execution_out_of_gas[0].threshold == 1 &&
      aws_cloudwatch_metric_alarm.aa_execution_out_of_gas[0].treat_missing_data == "notBreaching"
    )
    error_message = "Only verified historical execution OOG must trigger the new alarm; missing traces are not OOG evidence."
  }
}
run "reject_public_without_funding" {
  command = plan
  variables {
    configure_native_aa_backend      = true
    alto_desired_count               = 1
    aa_reconciler_desired_count      = 1
    aa_native_global_rollout_enabled = true
    aa_native_canary_owners          = ""
  }
  expect_failures = [terraform_data.self_hosted_aa_guard]
}
run "reject_mismatched_core" {
  command = plan
  variables {
    configure_native_aa_backend = true
    alto_desired_count          = 1
    aa_reconciler_desired_count = 1
    perps_senior_vault          = "0x1111111111111111111111111111111111111111"
  }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_public_unconfigured" {
  command = plan
  variables { aa_native_global_rollout_enabled = true }
  expect_failures = [terraform_data.self_hosted_aa_guard]
}
run "reject_mismatched_history_senior" {
  command = plan
  variables {
    configure_native_aa_backend        = true
    alto_desired_count                 = 1
    aa_reconciler_desired_count        = 1
    vault_history_senior_vault_address = "0x1111111111111111111111111111111111111111"
  }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_history_junior" {
  command = plan
  variables {
    configure_native_aa_backend        = true
    alto_desired_count                 = 1
    aa_reconciler_desired_count        = 1
    vault_history_junior_vault_address = "0x1111111111111111111111111111111111111111"
  }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_mismatched_history_anchor" {
  command = plan
  variables {
    configure_native_aa_backend    = true
    alto_desired_count             = 1
    aa_reconciler_desired_count    = 1
    vault_history_deployment_block = "307397197"
  }
  expect_failures = [terraform_data.deployment_target_guard]
}
run "reject_public_mainnet_chain" {
  command = plan
  variables {
    aa_native_global_rollout_enabled   = true
    perps_chain_id                     = "42161"
    aa_reconciler_max_safe_lag_seconds = "600"
  }
  expect_failures = [terraform_data.self_hosted_aa_guard]
}
run "accept_sepolia_safe_lag" {
  command = plan
  variables { aa_reconciler_max_safe_lag_seconds = "1800" }
  assert {
    condition     = var.aa_reconciler_max_safe_lag_seconds == "1800"
    error_message = "Sepolia must accept the bounded parent-chain finality allowance."
  }
}
run "reject_excessive_safe_lag" {
  command = plan
  variables { aa_reconciler_max_safe_lag_seconds = "1801" }
  expect_failures = [var.aa_reconciler_max_safe_lag_seconds]
}
run "reject_mainnet_extended_safe_lag" {
  command = plan
  variables {
    perps_chain_id                     = "42161"
    aa_reconciler_max_safe_lag_seconds = "1800"
  }
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
