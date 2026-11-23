# Synthetic mainnet AWS plans: no credentials, live state, deployment or signing.
mock_provider "aws" {
  mock_data "aws_caller_identity" { defaults = { account_id = "111122223333" } }
  mock_data "aws_availability_zones" { defaults = { names = ["ap-southeast-1a", "ap-southeast-1b"] } }
  mock_data "aws_ami" { defaults = { id = "ami-0123456789abcdef0" } }
  mock_data "aws_kms_key" {
    defaults = { arn = "arn:aws:kms:ap-southeast-1:111122223333:key/11111111-1111-1111-1111-111111111111" }
  }
  mock_resource "aws_iam_role" {
    override_during = plan
    defaults        = { arn = "arn:aws:iam::111122223333:role/synthetic-mainnet-role" }
  }
  mock_resource "aws_kms_key" {
    override_during = plan
    defaults        = { arn = "arn:aws:kms:ap-southeast-1:111122223333:key/22222222-2222-2222-2222-222222222222" }
  }
  mock_resource "aws_cloudwatch_log_group" {
    override_during = plan
    defaults        = { arn = "arn:aws:logs:ap-southeast-1:111122223333:log-group:/ecs/plether-mainnet" }
  }
  mock_resource "aws_security_group" {
    override_during = plan
    defaults        = { id = "sg-0123456789abcdef0" }
  }
  mock_resource "aws_lb" {
    override_during = plan
    defaults        = { arn = "arn:aws:elasticloadbalancing:ap-southeast-1:111122223333:loadbalancer/app/plether-mainnet/1111111111111111", arn_suffix = "app/plether-mainnet/1111111111111111", dns_name = "plether-mainnet.ap-southeast-1.elb.amazonaws.com", zone_id = "Z1LMS91P8CMLE5" }
  }
  mock_resource "aws_lb_target_group" {
    override_during = plan
    defaults        = { arn = "arn:aws:elasticloadbalancing:ap-southeast-1:111122223333:targetgroup/plether-mainnet/0123456789abcdef", arn_suffix = "targetgroup/plether-mainnet/0123456789abcdef" }
  }
  mock_resource "aws_lb_listener" {
    override_during = plan
    defaults        = { arn = "arn:aws:elasticloadbalancing:ap-southeast-1:111122223333:listener/app/plether-mainnet/1111111111111111/2222222222222222" }
  }
  mock_resource "aws_instance" {
    override_during = plan
    defaults        = { arn = "arn:aws:ec2:ap-southeast-1:111122223333:instance/i-0123456789abcdef0", private_ip = "10.0.1.42" }
  }
  mock_resource "aws_ecr_repository" {
    override_during = plan
    defaults        = { arn = "arn:aws:ecr:ap-southeast-1:111122223333:repository/plether-api", repository_url = "111122223333.dkr.ecr.ap-southeast-1.amazonaws.com/plether-api" }
  }
  mock_resource "aws_ssm_document" {
    override_during = plan
    defaults        = { arn = "arn:aws:ssm:ap-southeast-1:111122223333:document/plether-mainnet-rpc-node-admin" }
  }
  mock_resource "aws_ssm_parameter" {
    override_during = plan
    defaults        = { arn = "arn:aws:ssm:ap-southeast-1:111122223333:parameter/plether/mainnet/mock" }
  }
}

variables {
  expected_aws_account_id                              = "111122223333"
  environment                                          = "mainnet"
  chain_id                                             = "1"
  perps_chain_id                                       = "42161"
  api_image_digest                                     = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
  log_router_image_digest                              = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
  api_hostname                                         = "api.plether.com"
  alb_certificate_arn                                  = "arn:aws:acm:ap-southeast-1:111122223333:certificate/11111111-1111-1111-1111-111111111111"
  alb_origin_secret_header_value                       = "0123456789abcdef0123456789abcdef0123456789abcdef"
  cors_origins                                         = "https://app.plether.com"
  operations_alarm_sns_topic_arn                       = "arn:aws:sns:ap-southeast-1:111122223333:plether-mainnet-operations"
  enable_incident_observer                             = false
  incident_escalation_sns_topic_arn                    = "arn:aws:sns:ap-southeast-1:111122223333:plether-mainnet-operations"
  rpc_request_rate_warning_threshold                   = 225000
  rpc_request_rate_critical_threshold                  = 275000
  rpc_url                                              = "https://eth-mainnet.g.alchemy.com/v2"
  perps_rpc_url                                        = "https://arb-mainnet.g.alchemy.com/v2"
  rpc_auth_token_ssm_parameter_name                    = "/plether/mainnet/rpc-auth-token"
  perps_rpc_auth_token_ssm_parameter_name              = "/plether/mainnet/perps-rpc-auth-token"
  pyth_api_key_ssm_parameter_name                      = "/plether/mainnet/pyth-api-key"
  keeper_private_key_ssm_parameter_name                = "/plether/mainnet/keeper-private-key"
  oracle_updater_private_key_ssm_parameter_name        = "/plether/mainnet/oracle-updater-private-key"
  liquidation_keeper_private_key_ssm_parameter_name    = "/plether/mainnet/liquidation-keeper-private-key"
  database_url_ssm_parameter_name                      = "/plether/mainnet/database-url"
  posthog_otlp_authorization_header_ssm_parameter_name = "/plether/mainnet/posthog-otlp-authorization-header"
  aa_proxy_origin_token_ssm_parameter_name             = "/plether/mainnet/aa-proxy-origin-token"
  consolidate_workers                                  = true
  provision_self_hosted_aa                             = true
  aa_rpc_mode                                          = "dual-independent"
  alto_rpc_url_ssm_parameter_name                      = "/plether/mainnet/alto-rpc-url"
  alto_executor_private_keys_ssm_parameter_name        = "/plether/mainnet/alto-executor-private-keys"
  alto_utility_private_key_ssm_parameter_name          = "/plether/mainnet/alto-utility-private-key"
  aa_reconciler_secondary_rpc_url_ssm_parameter_name   = "/plether/mainnet/aa-reconciler-secondary-rpc-url"
  aa_reconciler_max_safe_lag_seconds                   = "600"
  db_engine_version                                    = "16.4"
  db_manage_master_user_password                       = true
  db_allocated_storage                                 = 20
  db_storage_type                                      = "gp3"
  db_storage_encrypted                                 = true
  db_backup_retention_days                             = 14
  db_deletion_protection                               = true
  db_skip_final_snapshot                               = false
  db_final_snapshot_identifier                         = "plether-mainnet-final-test"
}

run "mainnet_baseline_enforces_security_observability_and_full_parity" {
  command = plan

  assert {
    condition = (
      length(aws_ssm_parameter.keeper_private_key) == 0 &&
      length(aws_ssm_parameter.oracle_updater_private_key) == 0 &&
      length(aws_ssm_parameter.liquidation_keeper_private_key) == 0 &&
      length(aws_ssm_parameter.database_url) == 0 &&
      length(aws_ssm_parameter.posthog_otlp_authorization_header) == 0 &&
      length(aws_ssm_parameter.aa_proxy_origin_token) == 0
    )
    error_message = "Mainnet must keep all hot-wallet keys, DB credentials, and tokens out of Terraform state."
  }

  assert {
    condition = (
      !contains(local.github_actions_oidc_subjects, "repo:Plether-Fi/plether-app:ref:refs/heads/master") &&
      !anytrue([for s in local.github_actions_oidc_subjects : endswith(s, "-sepolia") || strcontains(s, "sepolia")]) &&
      contains(local.github_actions_oidc_subjects, "repo:Plether-Fi/plether-app:environment:backend-admin-mainnet") &&
      contains(local.github_actions_oidc_subjects, "repo:Plether-Fi/plether-app:environment:db-admin-mainnet")
    )
    error_message = "Mainnet OIDC deploy role must trust only protected mainnet GitHub Environments."
  }

  assert {
    condition = (
      !strcontains(aws_iam_role_policy.ecs_execution_ssm.policy, "parameter/plether/mainnet/*") &&
      !strcontains(aws_iam_role_policy.github_deploy.policy, "parameter/plether/mainnet/*")
    )
    error_message = "Neither ECS execution nor GitHub deploy roles may hold wildcard SSM read access."
  }

  assert {
    condition = (
      aws_lb.api.drop_invalid_header_fields == true &&
      aws_lb.api.enable_deletion_protection == true &&
      aws_lb_listener.https[0].default_action[0].type == "fixed-response" &&
      length(aws_lb_listener_rule.https_origin_authenticated) == 1
    )
    error_message = "Mainnet ALB must enforce HTTPS, drop invalid headers, enable deletion protection, and lock down origin access."
  }

  assert {
    condition = (
      aws_db_instance.postgres.manage_master_user_password == true &&
      aws_db_instance.postgres.auto_minor_version_upgrade == false &&
      aws_db_instance.postgres.performance_insights_enabled == true &&
      aws_db_instance.postgres.storage_encrypted == true &&
      length(aws_db_event_subscription.postgres) == 1
    )
    error_message = "Mainnet Single-AZ RDS must enable encryption, AWS-managed master password, controlled upgrades, Performance Insights, and SNS event subscriptions."
  }

  assert {
    condition = (
      length(aws_ecs_service.alto) == 1 &&
      length(aws_ecs_service.aa_reconciler) == 1 &&
      length(aws_ecs_service.protection_worker) == 1 &&
      length(aws_cloudwatch_metric_alarm.vault_indexer_heartbeat_missing) == 1 &&
      aws_cloudwatch_metric_alarm.alb_unhealthy_hosts.alarm_name == "plether-mainnet-api-alb-unhealthy-hosts" &&
      contains(aws_cloudwatch_metric_alarm.alb_5xx.ok_actions, var.operations_alarm_sns_topic_arn) &&
      aws_cloudwatch_metric_alarm.rpc_request_rate_warning.threshold == 225000
    )
    error_message = "Mainnet must provision AA/Alto, protection worker, vault-indexer alarms, ALB unhealthy-host alarm, calibrated RPC alarms, and OK email recovery actions."
  }
}

run "mainnet_supports_self_hosted_arbitrum_one_rpc" {
  command = plan
  variables {
    provision_sepolia_rpc = true
    sepolia_rpc = {
      ami_id                   = "ami-0123456789abcdef0"
      nitro_image              = "offchainlabs/nitro-node@sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      snapshot_url             = "https://snapshot.arbitrum.foundation/arb1/2026-10-20-dbd26a00/"
      snapshot_metadata_sha256 = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
      hostname                 = "rpc-internal.mainnet.plether.com"
      certificate_arn          = "arn:aws:acm:ap-southeast-1:111122223333:certificate/11111111-1111-1111-1111-111111111111"
      parent_parameter         = "/plether/mainnet/rpc-node/parent"
      gateway_token_parameter  = "/plether/mainnet/rpc-node/gateway-token"
      backup_url_parameter     = "/plether/mainnet/rpc-node/backup-url"
      gateway_image            = "111122223333.dkr.ecr.ap-southeast-1.amazonaws.com/plether-rpc-gateway-mainnet@sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    }
  }
  assert {
    condition = (
      aws_ecr_repository.rpc_gateway[0].name == "plether-rpc-gateway-mainnet" &&
      aws_cloudwatch_log_group.rpc_node[0].name == "/plether/mainnet/rpc-node" &&
      aws_iam_role.rpc_admin[0].name == "plether-mainnet-rpc-admin" &&
      aws_cloudwatch_metric_alarm.rpc_primary_unhealthy[0].alarm_name == "plether-mainnet-rpc-primary-unhealthy"
    )
    error_message = "Mainnet self-hosted RPC must use mainnet-scoped ECR, logs, IAM, and alarm names."
  }
}

run "mainnet_rejects_inline_plaintext_private_keys" {
  command = plan
  variables {
    keeper_private_key = "0x1111111111111111111111111111111111111111111111111111111111111111"
  }
  expect_failures = [terraform_data.secrets_boundary_guard]
}

run "mainnet_rejects_missing_https_certificate" {
  command = plan
  variables {
    alb_certificate_arn = ""
    api_hostname        = ""
  }
  expect_failures = [aws_lb.api, aws_ecs_task_definition.api]
}

run "mainnet_rejects_testnet_faucet_and_margin_funding" {
  command = plan
  variables {
    testnet_margin_funding_enabled = true
  }
  expect_failures = [terraform_data.secrets_boundary_guard]
}

run "mainnet_rejects_single_provider_aa_mode" {
  command = plan
  variables {
    aa_rpc_mode = "single-provider-sepolia"
  }
  expect_failures = [terraform_data.self_hosted_aa_guard]
}

run "mainnet_rejects_alto_safe_mode_exception" {
  command = plan
  variables {
    alto_sepolia_safe_mode_exception = true
  }
  expect_failures = [terraform_data.self_hosted_aa_guard]
}
