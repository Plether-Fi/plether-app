# AWS is mocked: these plans never authenticate, read live state, or create resources.
mock_provider "aws" {
  mock_data "aws_caller_identity" {
    defaults = { account_id = "932542905614" }
  }
  mock_data "aws_availability_zones" {
    defaults = { names = ["eu-central-1a", "eu-central-1b"] }
  }
  mock_data "aws_ami" {
    defaults = { id = "ami-08295554222e9a438" }
  }
  mock_resource "aws_security_group" {
    override_during = plan
    defaults        = { id = "sg-0123456789abcdef0" }
  }
  mock_resource "aws_lb" {
    override_during = plan
    defaults = {
      dns_name = "internal-plether-sepolia-aa-temp-1234.eu-central-1.elb.amazonaws.com"
      arn      = "arn:aws:elasticloadbalancing:eu-central-1:932542905614:loadbalancer/app/plether-sepolia-aa-temp/0123456789abcdef"
    }
  }
}

variables {
  # Synthetic test inputs only. Never fund these public test scalars.
  rpc_url                        = "https://fixture.invalid/spot"
  perps_rpc_url                  = "https://fixture.invalid/perps"
  keeper_private_key             = "0x1111111111111111111111111111111111111111111111111111111111111111"
  oracle_updater_private_key     = "0x2222222222222222222222222222222222222222222222222222222222222222"
  liquidation_keeper_private_key = "0x3333333333333333333333333333333333333333333333333333333333333333"
  db_password                    = "test-fixture-not-a-real-password"
  posthog_project_token          = "phc_test_fixture"
}

run "frankfurt_dormant" {
  command = plan
  assert {
    condition = (aws_lb.api.internal
      && length(aws_lb_listener.https) == 0
      && length(aws_security_group.alb.ingress) == 1
      && one(aws_security_group.alb.ingress).from_port == 80
      && length(one(aws_security_group.alb.ingress).cidr_blocks) == 0
      && one(one(aws_security_group.alb.ingress).security_groups) == aws_security_group.frankfurt_tunnel[0].id
    && length(aws_security_group.frankfurt_tunnel[0].ingress) == 0)
    error_message = "Private API must accept only relay HTTP, with no relay ingress or public TLS listener."
  }
  assert {
    condition = (aws_instance.frankfurt_tunnel[0].instance_type == "t4g.nano"
      && aws_instance.frankfurt_tunnel[0].metadata_options[0].http_tokens == "required"
      && one(aws_instance.frankfurt_tunnel[0].root_block_device).encrypted
      && !strcontains(aws_iam_role_policy.frankfurt_tunnel[0].policy, "ssm:GetParameter")
      && jsondecode(aws_ssm_document.frankfurt_tunnel[0].content).properties.host == aws_lb.api.dns_name
      && jsondecode(aws_ssm_document.frankfurt_tunnel[0].content).properties.portNumber == "80"
    && jsondecode(aws_ssm_document.frankfurt_tunnel[0].content).properties.localPortNumber == "18081")
    error_message = "Relay must be hardened, secret-free and fixed to the temporary API."
  }
  assert {
    condition = (aws_ecs_cluster.main.name == "plether-sepolia-aa-temp"
      && aws_iam_role.api_task.name == "plether-sepolia-aa-temp-api-task"
      && aws_ecr_repository.alto[0].name == "plether-alto-sepolia-aa-temp"
    && aws_ssm_parameter.perps_rpc_url.name == "/plether/sepolia-aa-temp/perps-rpc-url")
    error_message = "Regional and account-global resources must use the isolated namespace."
  }
  assert {
    condition = (length(aws_iam_openid_connect_provider.github_actions) == 0
    && local.github_actions_oidc_subjects == tolist(["repo:Plether-Fi/plether-app:environment:sepolia-aa-temp"]))
    error_message = "Do not duplicate/manage the shared OIDC provider or trust the unprotected master ref."
  }
  assert {
    condition = (aws_ecs_service.api.desired_count == 0
      && aws_ecs_service.workers[0].desired_count == 0
      && aws_ecs_service.keeper.desired_count == 0
      && aws_ecs_service.alto[0].desired_count == 0
    && aws_ecs_service.aa_reconciler[0].desired_count == 0)
    error_message = "The preparation profile must not start services."
  }
}

run "frankfurt_alarm_recovery_routing" {
  command = plan
  variables {
    operations_alarm_sns_topic_arn = "arn:aws:sns:eu-central-1:932542905614:plether-sepolia-aa-temp-operations"
  }
  assert {
    condition = (length(aws_cloudwatch_metric_alarm.rpc_request_rate_warning.alarm_actions) == 0
      && length(aws_cloudwatch_metric_alarm.rpc_request_rate_warning.ok_actions) == 0
      && aws_cloudwatch_metric_alarm.rpc_request_rate_warning.threshold == 15000
      && aws_cloudwatch_metric_alarm.rpc_request_rate_critical.threshold == 25000
      && one(aws_cloudwatch_metric_alarm.rpc_request_rate_critical.alarm_actions) == var.operations_alarm_sns_topic_arn
      && one(aws_cloudwatch_metric_alarm.rpc_request_rate_critical.ok_actions) == var.operations_alarm_sns_topic_arn
      && one(aws_cloudwatch_metric_alarm.rds_freeable_memory_low.alarm_actions) == var.operations_alarm_sns_topic_arn
    && one(aws_cloudwatch_metric_alarm.rds_freeable_memory_low.ok_actions) == var.operations_alarm_sns_topic_arn)
    error_message = "Only duplicate RPC warning email may be suppressed; critical/memory alerts and thresholds must be preserved, with recovery notices."
  }
}

run "reject_singapore" {
  command = plan
  variables { aws_region = "ap-southeast-1" }
  expect_failures = [terraform_data.deployment_target_guard, terraform_data.self_hosted_aa_guard]
}

run "reject_custom_hostname" {
  command = plan
  variables { api_hostname = "aa-temp-api.sepolia.plether.com" }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "reject_tls_certificate" {
  command = plan
  variables { alb_certificate_arn = "arn:aws:acm:eu-central-1:932542905614:certificate/475a38ec-f78a-4ad4-bbb7-227c5bd33754" }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "reject_worker_start" {
  command = plan
  variables { workers_desired_count = 1 }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "reject_api_start" {
  command = plan
  variables { api_desired_count = 1 }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "reject_account" {
  command = plan
  variables { expected_aws_account_id = "111111111111" }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "reject_comma_separated_cors_origins" {
  command = plan
  variables { cors_origins = "http://127.0.0.1:5173,http://localhost:5173" }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "frankfurt_api_only" {
  command = plan
  variables {
    frankfurt_activation_stage = "api-readonly"
    api_desired_count          = 1
  }
  assert {
    condition = (aws_ecs_service.api.desired_count == 1
      && aws_ecs_service.workers[0].desired_count == 0
      && aws_ecs_service.alto[0].desired_count == 0
      && aws_ecs_service.aa_reconciler[0].desired_count == 0
      && strcontains(local.api_image, "@sha256:4629cc418d52f183b5fdd21a2d4409124eaf7ea4e72229bd983c1602f67bb3be")
      && strcontains(local.log_router_image, "@sha256:f442ecc9aff9c3fc768ba76a200690c33c4cb0ffd49b9660b09ce7738ba213cc")
    && !var.enable_native_aa_sponsorship && !var.enable_native_aa_submission)
    error_message = "API-only activation must pin both images and keep every AA/worker path off."
  }
}

run "reject_api_only_worker" {
  command = plan
  variables {
    frankfurt_activation_stage = "api-readonly"
    api_desired_count          = 1
    workers_desired_count      = 1
  }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "reject_api_only_faucet" {
  command = plan
  variables {
    frankfurt_activation_stage = "api-readonly"
    api_desired_count          = 1
    faucet_private_key         = "0x1111111111111111111111111111111111111111111111111111111111111111"
  }
  expect_failures = [terraform_data.deployment_target_guard, aws_ecs_task_definition.api]
}

run "reject_other_chain" {
  command = plan
  variables { perps_chain_id = "42161" }
  expect_failures = [terraform_data.deployment_target_guard, terraform_data.self_hosted_aa_guard]
}

run "reject_database_restore" {
  command = plan
  variables { db_snapshot_identifier = "singapore-snapshot" }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "reject_pyth_crossover" {
  command = plan
  variables { pyth_api_key_ssm_parameter_name = "/plether/sepolia/pyth-api-key" }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "reject_secret_crossover" {
  command = plan
  variables { alto_executor_private_keys_ssm_parameter_name = "/plether/sepolia/alto-executor-private-keys" }
  expect_failures = [terraform_data.self_hosted_aa_guard]
}

run "reject_core_drift" {
  command = plan
  variables { perps_order_router = "0xbd2f286efca5f761e21452673ab9b8c14e17aad7" }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "reject_public_cors" {
  command = plan
  variables { cors_origins = "*" }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "reject_singapore_alarms" {
  command = plan
  variables { operations_alarm_sns_topic_arn = "arn:aws:sns:ap-southeast-1:932542905614:plether-sepolia-operations" }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "legacy_identity_unchanged" {
  command = plan
  assert {
    condition = (!aws_lb.api.internal
      && length(aws_instance.frankfurt_tunnel) == 0
      && length(aws_ssm_document.frankfurt_tunnel) == 0
    && length(aws_iam_instance_profile.frankfurt_tunnel) == 0)
    error_message = "Legacy backend access must not change or acquire a relay."
  }
  variables {
    operations_alarm_sns_topic_arn                     = "arn:aws:sns:ap-southeast-1:932542905614:plether-sepolia-operations"
    deployment_id                                      = ""
    alb_certificate_arn                                = ""
    api_hostname                                       = ""
    aws_region                                         = "ap-southeast-1"
    perps_order_lifecycle_book                         = ""
    alto_rpc_url_ssm_parameter_name                    = "/plether/sepolia/perps-rpc-url"
    aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/perps-rpc-url"
    alto_executor_private_keys_ssm_parameter_name      = "/plether/sepolia/alto-executor-private-keys"
    alto_utility_private_key_ssm_parameter_name        = "/plether/sepolia/alto-utility-private-key"
    pyth_api_key_ssm_parameter_name                    = "/plether/sepolia/pyth-api-key"
  }
  assert {
    condition = (aws_ecs_cluster.main.name == "plether-sepolia"
      && aws_iam_role.api_task.name == "plether-sepolia-api-task"
      && aws_ecr_repository.alto[0].name == "plether-alto-sepolia"
      && aws_ssm_parameter.perps_rpc_url.name == "/plether/sepolia/perps-rpc-url"
    && length(aws_iam_openid_connect_provider.github_actions) == 1)
    error_message = "Legacy resource identities must remain unchanged."
  }
  assert {
    condition     = length(local.alto_zero_post_op_environment) == 0
    error_message = "Frankfurt zero-postOp overrides must not affect legacy Alto."
  }
  assert {
    condition = (one(aws_cloudwatch_metric_alarm.rpc_request_rate_warning.alarm_actions) == var.operations_alarm_sns_topic_arn
      && length(aws_cloudwatch_metric_alarm.rpc_request_rate_critical.ok_actions) == 0
    && length(aws_cloudwatch_metric_alarm.rds_freeable_memory_low.ok_actions) == 0)
    error_message = "Legacy warning and recovery notification routing must remain unchanged."
  }
}

run "frankfurt_aa_qualification" {
  command = plan
  variables {
    frankfurt_activation_stage             = "aa-qualification"
    api_desired_count                      = 1
    alto_desired_count                     = 1
    aa_reconciler_desired_count            = 1
    aa_reconciler_max_safe_lag_seconds     = "1800"
    configure_native_aa_backend            = true
    aa_proxy_origin_token                  = "0000000000000000000000000000000000000000000000000000000000000001"
    aa_paymaster_address                   = "0x9761091045616A388f5fE1433721B272c78fe31b"
    aa_paymaster_signer_address            = "0x015736E1F47E37938236e481F7a3B7c57F922b80"
    aa_paymaster_code_hash                 = "0xb8ae276b01850fdbb8d9d7fd32ec7b9b1c7ab7af20f5d62179a76f6b4912c528"
    aa_reconciler_start_block              = "307684600"
    aa_reconciler_start_block_hash         = "0x64210ad75de20ddd2ccf494e7c4d042447f4ea48119419945637f17c271904fe"
    alto_entrypoint_simulation_contract_v8 = "0x9c3c25a084AE8B1df3B2e82bb07Dac4E115C9Ae1"
    alto_pimlico_simulation_contract       = "0x95CC02A7B69dD46c6DD6Bd56132A24a235D58948"
  }
  assert {
    condition = (
      one([for env in local.native_aa_environment : env.value if env.name == "AA_RECONCILER_MAX_SAFE_LAG_SECONDS"]) == "1800"
      && var.aa_reconciler_max_safe_lag_seconds == "1800"
      && strcontains(local.aa_runtime_image, "@sha256:3078940d5bc82cadadf884e9ec1057059920c24ac99f5ec1bd8ef6561c4173c8")
    )
    error_message = "Gateway and reconciler must share the explicit canary allowance."
  }
  assert {
    condition = (aws_ecs_service.alto[0].desired_count == 1
      && aws_ecs_service.aa_reconciler[0].desired_count == 1
      && aws_ecs_service.workers[0].desired_count == 0
      && length(aws_iam_role_policy.api_paymaster_kms_signer) == 0
      && length(aws_iam_role_policy.api_paymaster_kms_key_metadata) == 0
      && !var.enable_native_aa_sponsorship && !var.enable_native_aa_submission
    && aws_lb.api.internal && aws_lb.alto[0].internal)
    error_message = "Qualification must remain private with no API signing permission, sponsorship, submission or workers."
  }
}

run "reject_qualification_issuance" {
  command = plan
  variables {
    frankfurt_activation_stage   = "aa-qualification"
    enable_native_aa_sponsorship = true
  }
  expect_failures = [terraform_data.deployment_target_guard, terraform_data.self_hosted_aa_guard]
}

run "reject_qualification_submission" {
  command = plan
  variables {
    frankfurt_activation_stage  = "aa-qualification"
    enable_native_aa_submission = true
  }
  expect_failures = [terraform_data.deployment_target_guard, terraform_data.self_hosted_aa_guard]
}

run "reject_excessive_safe_lag" {
  command = plan
  variables { aa_reconciler_max_safe_lag_seconds = "1801" }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "frankfurt_one_wallet_canary" {
  command = plan
  variables {
    frankfurt_activation_stage             = "aa-canary"
    api_desired_count                      = 1
    alto_desired_count                     = 1
    aa_reconciler_desired_count            = 1
    configure_native_aa_backend            = true
    enable_native_aa_sponsorship           = true
    enable_native_aa_submission            = true
    operations_alarm_sns_topic_arn         = "arn:aws:sns:eu-central-1:932542905614:plether-sepolia-aa-temp-operations"
    aa_proxy_origin_token                  = "0000000000000000000000000000000000000000000000000000000000000001"
    aa_paymaster_address                   = "0x9761091045616A388f5fE1433721B272c78fe31b"
    aa_paymaster_signer_address            = "0x015736E1F47E37938236e481F7a3B7c57F922b80"
    aa_paymaster_code_hash                 = "0xb8ae276b01850fdbb8d9d7fd32ec7b9b1c7ab7af20f5d62179a76f6b4912c528"
    aa_reconciler_start_block              = "307684600"
    aa_reconciler_start_block_hash         = "0x64210ad75de20ddd2ccf494e7c4d042447f4ea48119419945637f17c271904fe"
    alto_entrypoint_simulation_contract_v8 = "0x9c3c25a084AE8B1df3B2e82bb07Dac4E115C9Ae1"
    alto_pimlico_simulation_contract       = "0x95CC02A7B69dD46c6DD6Bd56132A24a235D58948"
  }
  assert {
    condition = (length(aws_iam_role_policy.api_paymaster_kms_signer) == 1
      && var.aa_native_canary_owners == "0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B"
      && !var.aa_native_global_rollout_enabled
    && aws_ecs_service.workers[0].desired_count == 0)
    error_message = "Canary must retain exactly the approved wallet and keep unrelated workers and global rollout off."
  }
  assert {
    condition = alltrue([for name in ["ALTO_SIMULATION_PAYMASTER_POST_OP_GAS_LIMIT", "ALTO_V7_PAYMASTER_POST_OP_GAS_LIMIT_MULTIPLIER"] :
      one([for e in local.alto_zero_post_op_environment : e.value if e.name == name]) == "0"
    ])
    error_message = "Canary must both simulate and estimate zero postOp gas for its dedicated paymaster."
  }
}

run "reject_canary_without_activation_flags" {
  command = plan
  variables { frankfurt_activation_stage = "aa-canary" }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "frankfurt_trading_canary" {
  command = plan
  variables {
    frankfurt_activation_stage             = "trading-canary"
    api_desired_count                      = 1
    alto_desired_count                     = 1
    aa_reconciler_desired_count            = 1
    workers_desired_count                  = 1
    liquidation_worker_desired_count       = 1
    protection_worker_desired_count        = 1
    protection_worker_execution_enabled    = true
    aa_protection_commits_enabled          = true
    configure_native_aa_backend            = true
    enable_native_aa_sponsorship           = true
    enable_native_aa_submission            = true
    faucet_private_key                     = "0x4444444444444444444444444444444444444444444444444444444444444444"
    faucet_proxy_origin_token              = "synthetic-frankfurt-faucet-test-token-only"
    protection_worker_private_key          = "0x5555555555555555555555555555555555555555555555555555555555555555"
    lp_settlement_private_key              = "0x6666666666666666666666666666666666666666666666666666666666666666"
    operations_alarm_sns_topic_arn         = "arn:aws:sns:eu-central-1:932542905614:plether-sepolia-aa-temp-operations"
    aa_proxy_origin_token                  = "0000000000000000000000000000000000000000000000000000000000000001"
    aa_paymaster_address                   = "0x9761091045616A388f5fE1433721B272c78fe31b"
    aa_paymaster_signer_address            = "0x015736E1F47E37938236e481f7a3b7c57f922b80"
    aa_paymaster_code_hash                 = "0xb8ae276b01850fdbb8d9d7fd32ec7b9b1c7ab7af20f5d62179a76f6b4912c528"
    aa_reconciler_start_block              = "307684600"
    aa_reconciler_start_block_hash         = "0x64210ad75de20ddd2ccf494e7c4d042447f4ea48119419945637f17c271904fe"
    alto_entrypoint_simulation_contract_v8 = "0x9c3c25a084AE8B1df3B2e82bb07Dac4E115C9Ae1"
    alto_pimlico_simulation_contract       = "0x95CC02A7B69dD46c6DD6Bd56132A24a235D58948"
  }
  assert {
    condition = (aws_ecs_service.workers[0].desired_count == 1
      && aws_ecs_service.protection_worker[0].desired_count == 1
      && length(aws_ssm_parameter.faucet_private_key) == 1
      && !var.aa_native_global_rollout_enabled && !var.enable_insights_registration
    && var.lp_settlement_mode == "off")
    error_message = "Trading activation must preserve wallet-only AA and defer LP execution until preflight."
  }
  assert {
    condition = (aws_lb.api.internal
      && length(aws_security_group.alb.ingress) == 1
      && one(aws_security_group.alb.ingress).from_port == 80
      && length(one(aws_security_group.alb.ingress).cidr_blocks) == 0
      && contains(one(aws_security_group.alb.ingress).security_groups, aws_security_group.frankfurt_worker_client[0].id)
      && contains(aws_ecs_service.workers[0].network_configuration[0].security_groups, aws_security_group.frankfurt_worker_client[0].id)
      && length(aws_security_group.frankfurt_worker_client[0].ingress) == 0
    && length(aws_security_group.frankfurt_worker_client[0].egress) == 0)
    error_message = "Trading workers may reach the private API through their identity group, never public ingress."
  }
}

run "reject_faucet_in_aa_only_canary" {
  command = plan
  variables {
    faucet_private_key        = "0x4444444444444444444444444444444444444444444444444444444444444444"
    faucet_proxy_origin_token = "synthetic-frankfurt-faucet-test-token-only"
  }
  expect_failures = [terraform_data.deployment_target_guard]
}

run "reject_non_sepolia_safe_lag" {
  command = plan
  variables {
    perps_chain_id                     = "42161"
    aa_reconciler_max_safe_lag_seconds = "1800"
  }
  expect_failures = [terraform_data.deployment_target_guard, terraform_data.self_hosted_aa_guard]
}

run "frankfurt_aa_prepared" {
  command = plan
  variables {
    frankfurt_activation_stage     = "aa-prepared"
    api_desired_count              = 1
    aa_paymaster_address           = "0x9761091045616A388f5fE1433721B272c78fe31b"
    aa_paymaster_signer_address    = "0x015736E1F47E37938236e481F7a3B7c57F922b80"
    aa_paymaster_code_hash         = "0xb8ae276b01850fdbb8d9d7fd32ec7b9b1c7ab7af20f5d62179a76f6b4912c528"
    aa_reconciler_start_block      = "307684600"
    aa_reconciler_start_block_hash = "0x64210ad75de20ddd2ccf494e7c4d042447f4ea48119419945637f17c271904fe"
  }
  assert {
    condition = (aws_ecs_service.alto[0].desired_count == 0
      && aws_ecs_service.aa_reconciler[0].desired_count == 0
      && !local.native_aa_backend_configured
    && length(aws_iam_role_policy.api_paymaster_kms_signer) == 0)
    error_message = "Prepared definitions cannot start AA services or enable native API configuration/signing."
  }
}
