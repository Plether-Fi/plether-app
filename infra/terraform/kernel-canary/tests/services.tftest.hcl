# Synthetic service plans. No AWS credentials, RPC calls, keys or live state.
mock_provider "aws" {
  mock_data "aws_caller_identity" { defaults = { account_id = "932542905614" } }
  mock_data "aws_iam_openid_connect_provider" { defaults = { arn = "arn:aws:iam::932542905614:oidc-provider/token.actions.githubusercontent.com" } }
  mock_data "aws_vpc" { defaults = { id = "vpc-0df986b0047f901d3", owner_id = "932542905614", state = "available" } }
  mock_data "aws_subnet" { defaults = { vpc_id = "vpc-0df986b0047f901d3", state = "available", map_public_ip_on_launch = true } }
  mock_data "aws_ecs_cluster" { defaults = { arn = "arn:aws:ecs:ap-southeast-1:932542905614:cluster/plether-sepolia", cluster_name = "plether-sepolia", status = "ACTIVE" } }
  mock_data "aws_lb" { defaults = { arn = "arn:aws:elasticloadbalancing:ap-southeast-1:932542905614:loadbalancer/app/plether-sepolia/9544b926599859e9", internal = false, load_balancer_type = "application", vpc_id = "vpc-0df986b0047f901d3", security_groups = ["sg-08bc02814791dc349"] } }
  override_data {
    target = data.aws_lb.kernel_canary_rpc_gateway[0]
    values = { arn = "arn:aws:elasticloadbalancing:ap-southeast-1:932542905614:loadbalancer/app/plether-sepolia-rpc/1111111111111111", internal = true, load_balancer_type = "application", vpc_id = "vpc-0df986b0047f901d3", security_groups = ["sg-074181cf8fdb6a814"] }
  }
  mock_data "aws_lb_listener" { defaults = { load_balancer_arn = "arn:aws:elasticloadbalancing:ap-southeast-1:932542905614:loadbalancer/app/plether-sepolia/9544b926599859e9", port = 443, protocol = "HTTPS", certificate_arn = "arn:aws:acm:ap-southeast-1:932542905614:certificate/11111111-1111-1111-1111-111111111111" } }
  mock_data "aws_security_group" { defaults = { vpc_id = "vpc-0df986b0047f901d3" } }
  mock_data "aws_instance" { defaults = { instance_state = "running", subnet_id = "subnet-0633bc3db0148548f", vpc_security_group_ids = ["sg-1234567890abcdef0"] } }
  override_data {
    target = data.aws_security_group.live_api_alb[0]
    values = { name = "plether-alb-synthetic", vpc_id = "vpc-0df986b0047f901d3" }
  }
  override_data {
    target = data.aws_security_group.live_aa_db_client[0]
    values = { name = "plether-sepolia-aa-reconciler-synthetic", vpc_id = "vpc-0df986b0047f901d3" }
  }
  override_data {
    target = data.aws_security_group.live_rds[0]
    values = { name = "plether-rds-synthetic", vpc_id = "vpc-0df986b0047f901d3" }
  }
  override_data {
    target = data.aws_security_group.live_rpc_node[0]
    values = { name = "plether-sepolia-rpc-node-synthetic", id = "sg-1234567890abcdef0", vpc_id = "vpc-0df986b0047f901d3" }
  }
  mock_data "aws_vpc_security_group_rule" { defaults = { is_egress = true, ip_protocol = "tcp", from_port = 5432, to_port = 5432, security_group_id = "sg-0c095c11c27499ca7", referenced_security_group_id = "sg-085ef29e0a4896491" } }
  override_data {
    target = data.aws_vpc_security_group_rule.live_aa_https_egress[0]
    values = { is_egress = true, ip_protocol = "tcp", from_port = 443, to_port = 443, security_group_id = "sg-0c095c11c27499ca7", cidr_ipv4 = "0.0.0.0/0" }
  }
  override_data {
    target = data.aws_vpc_security_group_rule.live_rds_aa_ingress[0]
    values = { is_egress = false, ip_protocol = "tcp", from_port = 5432, to_port = 5432, security_group_id = "sg-085ef29e0a4896491", referenced_security_group_id = "sg-0c095c11c27499ca7" }
  }
  mock_data "aws_vpc_security_group_rules" { defaults = { ids = [] } }
  override_data {
    target = data.aws_vpc_security_group_rules.live_aa_all[0]
    values = { ids = ["sgr-db", "sgr-https"] }
  }
  mock_data "aws_ecs_task_definition" { defaults = { cpu = "1024", memory = "2048", network_mode = "awsvpc", requires_compatibilities = ["FARGATE"] } }
  mock_data "aws_ecr_image" { defaults = { image_digest = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" } }
  override_data {
    target = data.aws_ecr_image.kernel_canary_api[0]
    values = { image_digest = "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" }
  }
  override_data {
    target = data.aws_ecs_task_definition.live_api[0]
    values = { family = "plether-sepolia", revision = 154, cpu = "1024", memory = "2048", network_mode = "awsvpc", requires_compatibilities = ["FARGATE"], container_definitions = jsonencode([{ name = "otel-log-router" }, { name = "plether-api", environment = [{ name = "PORT", value = "3001" }], secrets = [] }]) }
  }
  override_data {
    target = data.aws_ecs_task_definition.live_alto[0]
    values = {
      family                   = "plether-sepolia-alto"
      revision                 = 12
      cpu                      = "1024"
      memory                   = "2048"
      network_mode             = "awsvpc"
      requires_compatibilities = ["FARGATE"]
      container_definitions = jsonencode([
        { name = "alto-tmp-init" },
        { name = "otel-log-router" },
        { name = "plether-alto", entryPoint = ["node", "--import", "/app/plether-rpc-auth.mjs", "/app/src/esm/cli/alto.js"], environment = [] },
      ])
    }
  }
  override_data {
    target = data.aws_ecs_task_definition.live_reconciler[0]
    values = { family = "plether-sepolia-aa-reconciler", revision = 72, cpu = "256", memory = "512", network_mode = "awsvpc", requires_compatibilities = ["FARGATE"], container_definitions = jsonencode([{ name = "aa-reconciler-tmp-init" }, { name = "otel-log-router" }, { name = "plether-aa-reconciler" }]) }
  }
  mock_resource "aws_security_group" {
    override_during = plan
    defaults        = { id = "sg-99999999999999999" }
  }
  mock_resource "aws_iam_role" {
    override_during = plan
    defaults        = { arn = "arn:aws:iam::932542905614:role/synthetic-canary" }
  }
  mock_resource "aws_kms_key" {
    override_during = plan
    defaults        = { arn = "arn:aws:kms:ap-southeast-1:932542905614:key/11111111-1111-1111-1111-111111111111", key_id = "11111111-1111-1111-1111-111111111111" }
  }
}

variables {
  provision_image_build_resources = true
  provision_paymaster_key         = true
  provision_ssm_encryption_key    = true
  provision_services              = true
  live_rpc_node_security_group_id = "sg-1234567890abcdef0"
  live_aa_db_egress_rule_id       = "sgr-db"
  live_aa_https_egress_rule_id    = "sgr-https"
  live_rds_aa_ingress_rule_id     = "sgr-rds"
  kernel_canary_alto_image        = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-alto-sepolia@sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  kernel_canary_api_image         = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-api-sepolia@sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
}

run "services_stage_dark_in_isolated_state" {
  command = plan
  assert {
    condition = (aws_ecs_service.kernel_canary_alto[0].desired_count == 0
      && aws_ecs_service.kernel_canary_api[0].desired_count == 0
      && aws_ecs_service.kernel_canary_reconciler[0].desired_count == 0
      && length(aws_lb_listener_rule.kernel_canary_api) == 0
      && length(aws_vpc_security_group_ingress_rule.kernel_canary_rpc_gateway_from_alto) == 0
    )
    error_message = "The isolated service skeleton must start no tasks and expose no public or Nitro gateway rule."
  }
}

run "reject_mutable_service_images" {
  command = plan
  variables {
    kernel_canary_api_image = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-api-sepolia:latest"
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "reject_unreviewed_rpc_security_group" {
  command = plan
  variables {
    live_rpc_alb_security_group_id = "sg-11111111111111111"
  }
  expect_failures = [terraform_data.kernel_canary_live_identity_guard]
}

run "reject_alto_before_simulator_attestation" {
  command = plan
  variables {
    kernel_canary_alto_desired_count = 1
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "reject_tunnel_without_alto" {
  command = plan
  variables {
    kernel_canary_ssm_tunnel_enabled = true
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "qualified_alto_stays_private" {
  command = plan
  variables {
    kernel_canary_alto_desired_count                = 1
    kernel_canary_ssm_tunnel_enabled                = true
    kernel_canary_simulators_attested               = true
    kernel_canary_entrypoint_simulation_contract_v9 = "0x1111111111111111111111111111111111111111"
    kernel_canary_pimlico_simulation_contract       = "0x2222222222222222222222222222222222222222"
  }
  assert {
    condition = (aws_ecs_service.kernel_canary_alto[0].desired_count == 1
      && aws_ecs_service.kernel_canary_api[0].desired_count == 0
      && length(aws_lb_listener_rule.kernel_canary_api) == 0
      && aws_vpc_security_group_ingress_rule.kernel_canary_rpc_gateway_from_alto[0].security_group_id == "sg-074181cf8fdb6a814"
      && aws_vpc_security_group_ingress_rule.kernel_canary_rpc_gateway_from_alto[0].from_port == 443
      && aws_vpc_security_group_ingress_rule.kernel_canary_alto_alb_from_ssm_node[0].referenced_security_group_id == "sg-1234567890abcdef0"
    )
    error_message = "Alto may reach the reviewed private Nitro ALB and accept only the SSM-node tunnel while the public route is dark."
  }
  assert {
    condition = one([
      for container in jsondecode(aws_ecs_task_definition.kernel_canary_alto[0].container_definitions) :
      container.entryPoint if container.name == "plether-alto"
    ]) == ["node", "/app/src/esm/cli/alto.js"]
    error_message = "The canary image must start directly without the live Alto auth preload."
  }
}

run "reject_api_before_schema_and_profile" {
  command = plan
  variables {
    kernel_canary_alto_desired_count                = 1
    kernel_canary_simulators_attested               = true
    kernel_canary_entrypoint_simulation_contract_v9 = "0x1111111111111111111111111111111111111111"
    kernel_canary_pimlico_simulation_contract       = "0x2222222222222222222222222222222222222222"
    kernel_canary_api_desired_count                 = 1
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "reject_eth_without_exact_owner" {
  command = plan
  variables {
    kernel_canary_alto_desired_count                = 1
    kernel_canary_simulators_attested               = true
    kernel_canary_entrypoint_simulation_contract_v9 = "0x1111111111111111111111111111111111111111"
    kernel_canary_pimlico_simulation_contract       = "0x2222222222222222222222222222222222222222"
    kernel_canary_api_desired_count                 = 1
    kernel_canary_db_schema_confirmed               = true
    kernel_canary_profile_attested                  = true
    kernel_canary_profile_json                      = "{\"chainId\":421614,\"entryPoint\":\"0x433709009b8330fda32311df1c2afa402ed8d009\",\"entryPointVersion\":\"0.9\"}"
    kernel_canary_eth_enabled                       = true
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "reject_sponsorship_without_exact_owner" {
  command = plan
  variables {
    kernel_canary_alto_desired_count                = 1
    kernel_canary_simulators_attested               = true
    kernel_canary_entrypoint_simulation_contract_v9 = "0x1111111111111111111111111111111111111111"
    kernel_canary_pimlico_simulation_contract       = "0x2222222222222222222222222222222222222222"
    kernel_canary_api_desired_count                 = 1
    kernel_canary_db_schema_confirmed               = true
    kernel_canary_profile_attested                  = true
    kernel_canary_profile_json                      = "{\"chainId\":421614,\"entryPoint\":\"0x433709009b8330fda32311df1c2afa402ed8d009\",\"entryPointVersion\":\"0.9\"}"
    kernel_canary_reconciler_desired_count          = 1
    kernel_canary_sponsorship_enabled               = true
    kernel_canary_sponsorship_json                  = "{}"
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "qualified_eth_route_targets_only_kernel_api" {
  command = plan
  variables {
    kernel_canary_alto_desired_count                = 1
    kernel_canary_simulators_attested               = true
    kernel_canary_entrypoint_simulation_contract_v9 = "0x1111111111111111111111111111111111111111"
    kernel_canary_pimlico_simulation_contract       = "0x2222222222222222222222222222222222222222"
    kernel_canary_api_desired_count                 = 1
    kernel_canary_db_schema_confirmed               = true
    kernel_canary_profile_attested                  = true
    kernel_canary_profile_json                      = "{\"chainId\":421614,\"entryPoint\":\"0x433709009b8330fda32311df1c2afa402ed8d009\",\"entryPointVersion\":\"0.9\"}"
    kernel_canary_nitro_safemode_qualified          = true
    kernel_canary_preview_origin                    = "https://kernel-preview.pages.dev"
    kernel_canary_route_enabled                     = true
    kernel_canary_eth_enabled                       = true
    kernel_canary_owners_json                       = "[\"0x3333333333333333333333333333333333333333\"]"
  }
  assert {
    condition = (aws_lb_listener_rule.kernel_canary_api[0].priority == 180
      && toset(flatten([for c in aws_lb_listener_rule.kernel_canary_api[0].condition : [for p in c.path_pattern : p.values]])) == toset(["/api/aa/kernel/rpc"])
      && toset(flatten([for c in aws_lb_listener_rule.kernel_canary_api[0].condition : [for h in c.host_header : h.values]])) == toset(["api.sepolia.plether.com"])
      && length(aws_ecs_service.kernel_canary_api[0].load_balancer) == 1
      && length(aws_vpc_security_group_ingress_rule.kernel_canary_alto_alb_from_ssm_node) == 0
      && contains(aws_ecs_service.kernel_canary_api[0].network_configuration[0].security_groups, "sg-0c095c11c27499ca7")
      && contains(aws_ecs_service.kernel_canary_reconciler[0].network_configuration[0].security_groups, "sg-0c095c11c27499ca7")
      && one([for e in local.kernel_canary_api_environment : e.value if e.name == "KERNEL_CANARY_API_MODE"]) == "true"
      && one([for e in local.kernel_canary_api_environment : e.value if e.name == "API_BACKGROUND_WORKERS_ENABLED"]) == "false"
    )
    error_message = "Only the exact Kernel backend path may open after SafeMode, and both database clients must reuse the existing narrow AA group."
  }
}
