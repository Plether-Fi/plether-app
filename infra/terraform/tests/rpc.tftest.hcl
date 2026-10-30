# Offline plans only; no state, AWS resources, provider secrets or transactions.
mock_provider "aws" {
  mock_data "aws_caller_identity" { defaults = { account_id = "932542905614" } }
  mock_data "aws_availability_zones" { defaults = { names = ["ap-southeast-1a", "ap-southeast-1b"] } }
  mock_data "aws_ami" { defaults = { id = "ami-0123456789abcdef0" } }
  mock_data "aws_kms_key" { defaults = { arn = "arn:aws:kms:ap-southeast-1:932542905614:key/11111111-1111-1111-1111-111111111111" } }
  mock_resource "aws_iam_role" {
    override_during = plan
    defaults        = { arn = "arn:aws:iam::932542905614:role/synthetic-rpc-role" }
  }
  mock_resource "aws_cloudwatch_log_group" {
    override_during = plan
    defaults        = { arn = "arn:aws:logs:ap-southeast-1:932542905614:log-group:/plether/sepolia/rpc-fixture" }
  }
  mock_resource "aws_security_group" {
    override_during = plan
    defaults        = { id = "sg-0123456789abcdef0" }
  }
  mock_resource "aws_ecr_repository" {
    override_during = plan
    defaults        = { arn = "arn:aws:ecr:ap-southeast-1:932542905614:repository/plether-rpc-gateway-sepolia" }
  }
  mock_resource "aws_lb_target_group" {
    override_during = plan
    defaults        = { arn = "arn:aws:elasticloadbalancing:ap-southeast-1:932542905614:targetgroup/fixture/0123456789abcdef" }
  }
  mock_resource "aws_instance" {
    override_during = plan
    defaults        = { arn = "arn:aws:ec2:ap-southeast-1:932542905614:instance/i-0123456789abcdef0", private_ip = "10.0.1.42" }
  }
  mock_resource "aws_ssm_document" {
    override_during = plan
    defaults        = { arn = "arn:aws:ssm:ap-southeast-1:932542905614:document/plether-sepolia-rpc-node-admin" }
  }
}

variables {
  db_allocated_storage           = 20
  db_storage_type                = "gp3"
  operations_alarm_sns_topic_arn = "arn:aws:sns:ap-southeast-1:932542905614:synthetic-operations"
  rpc_url                        = "https://fixture.invalid/ethereum"
  perps_rpc_url                  = "https://fixture.invalid/arbitrum"
  keeper_private_key             = "0x1111111111111111111111111111111111111111111111111111111111111111"
  oracle_updater_private_key     = "0x2222222222222222222222222222222222222222222222222222222222222222"
  liquidation_keeper_private_key = "0x3333333333333333333333333333333333333333333333333333333333333333"
  db_password                    = "synthetic-test-only"
  posthog_project_token          = "phc_test_fixture"
  db_final_snapshot_identifier   = "synthetic-rpc-test-final-snapshot"
  sepolia_rpc = {
    ami_id                   = "ami-0123456789abcdef0"
    nitro_image              = "offchainlabs/nitro-node@sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    snapshot_url             = "https://snapshot.arbitrum.foundation/sepolia-rollup/2026-10-20-dbd26a00/"
    snapshot_metadata_sha256 = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    hostname                 = "rpc.sepolia.example.com"
    certificate_arn          = "arn:aws:acm:ap-southeast-1:932542905614:certificate/11111111-1111-1111-1111-111111111111"
    parent_parameter         = "/plether/sepolia/rpc-node/parent"
    gateway_token_parameter  = "/plether/sepolia/rpc-node/gateway-token"
    backup_url_parameter     = "/plether/sepolia/rpc-node/backup-url"
    backup_token_parameter   = "/plether/sepolia/rpc-node/backup-token"
    gateway_image            = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-rpc-gateway-sepolia@sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
  }
}

run "disabled_by_default" {
  command = plan
  assert {
    condition     = length(aws_instance.rpc_node) == 0 && length(aws_lb.rpc) == 0 && length(aws_ecs_service.rpc_gateway) == 0 && output.sepolia_rpc_pilot == null
    error_message = "Existing deployments must not provision RPC infrastructure by default."
  }
}

run "isolated_sepolia_pilot" {
  command = plan
  variables { provision_sepolia_rpc = true }
  assert {
    condition     = aws_instance.rpc_node[0].instance_type == "i4i.4xlarge" && aws_instance.rpc_node[0].metadata_options[0].http_tokens == "required" && aws_instance.rpc_node[0].root_block_device[0].encrypted
    error_message = "The pilot requires the selected NVMe host, IMDSv2 and encrypted boot disk."
  }
  assert {
    condition     = aws_lb.rpc[0].internal && aws_lb_listener.rpc[0].port == 443 && length(aws_security_group.rpc_node[0].ingress) == 0
    error_message = "RPC must remain private and TLS must terminate on port 443."
  }
  assert {
    condition     = aws_ecs_service.rpc_gateway[0].desired_count == 2 && aws_ecs_task_definition.rpc_gateway[0].runtime_platform[0].cpu_architecture == "ARM64"
    error_message = "The router must run redundantly outside the Nitro host."
  }
  assert {
    condition     = toset([for secret in jsondecode(aws_ecs_task_definition.rpc_gateway[0].container_definitions)[0].secrets : secret.name]) == toset(["RPC_BACKUP_TOKEN", "RPC_BACKUP_URL", "RPC_GATEWAY_TOKEN"])
    error_message = "The minimal pilot must not require a separately subscribed reference or its credentials."
  }
  assert {
    condition     = nonsensitive(aws_ssm_parameter.perps_rpc_url.value) == "https://fixture.invalid/arbitrum" && var.perps_indexer_poll_seconds == "12"
    error_message = "Provisioning a pilot must not cut over existing traffic or shorten polling."
  }
  assert {
    condition     = aws_ebs_volume.rpc_staging[0].encrypted && aws_ebs_volume.rpc_staging[0].size == 4096 && length(local.rpc_user_data) < 16384
    error_message = "Staging must be encrypted, large enough for assembly and user data must fit EC2's limit."
  }
  assert {
    condition = alltrue([
      for statement in jsondecode(aws_iam_role_policy.rpc_node[0].policy).Statement : statement.Resource != "*"
      if contains(statement.Action, "ssm:GetParameter")
    ])
    error_message = "The node must not receive wildcard secret access."
  }
  assert {
    condition     = jsondecode(aws_ssm_document.rpc_admin[0].content).parameters.operation.allowedValues == ["status", "restart"]
    error_message = "Node administration must not accept arbitrary shell or EC2 stop operations."
  }
}

run "reject_mainnet_pilot" {
  command = plan
  variables {
    provision_sepolia_rpc           = true
    environment                     = "mainnet"
    operations_alarm_sns_topic_arn  = "arn:aws:sns:ap-southeast-1:932542905614:synthetic-operations"
    pyth_api_key_ssm_parameter_name = "/plether/mainnet/pyth-api-key"
  }
  expect_failures = [terraform_data.rpc_guard]
}
