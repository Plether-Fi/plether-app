# Exercise the standalone RPC state; these plans never contact AWS.
mock_provider "aws" {
  mock_data "aws_caller_identity" { defaults = { account_id = "932542905614" } }
  mock_data "aws_vpc" { defaults = { id = "vpc-0123456789abcdef0", tags = { Name = "plether-sepolia" } } }
  mock_data "aws_ecs_cluster" { defaults = { id = "arn:aws:ecs:ap-southeast-1:932542905614:cluster/plether-sepolia", cluster_name = "plether-sepolia" } }
  mock_data "aws_security_group" { defaults = { vpc_id = "vpc-0123456789abcdef0", name = "plether-sepolia-alto-task-fixture" } }
  mock_data "aws_kms_key" { defaults = { arn = "arn:aws:kms:ap-southeast-1:932542905614:key/11111111-1111-1111-1111-111111111111" } }
  mock_data "aws_ami" { defaults = { id = "ami-0123456789abcdef0" } }
  mock_resource "aws_security_group" {
    override_during = plan
    defaults        = { id = "sg-0123456789abcdef0" }
  }
  override_data {
    target = data.aws_subnet.existing[0]
    values = { vpc_id = "vpc-0123456789abcdef0", map_public_ip_on_launch = true, availability_zone = "ap-southeast-1a" }
  }
  override_data {
    target = data.aws_subnet.existing[1]
    values = { vpc_id = "vpc-0123456789abcdef0", map_public_ip_on_launch = true, availability_zone = "ap-southeast-1b" }
  }
}

variables {
  existing_vpc_id                = "vpc-0123456789abcdef0"
  existing_public_subnet_ids     = ["subnet-0123456789abcdef0", "subnet-0123456789abcdef1"]
  existing_ecs_cluster_name      = "plether-sepolia"
  existing_ecs_security_group_id = "sg-0123456789abcdef1"
  operations_alarm_sns_topic_arn = "arn:aws:sns:ap-southeast-1:932542905614:fixture-operations"
  provision_sepolia_rpc          = true
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
  }
}

run "alto_has_no_rpc_ingress_without_explicit_group" {
  command = plan
  module { source = "../terraform-rpc" }
  assert {
    condition     = length(aws_vpc_security_group_ingress_rule.rpc_alb_from_alto) == 0
    error_message = "The existing gateway must not add Alto network access by default."
  }
}

run "allow_only_alto_group_to_gateway_https" {
  command = plan
  module { source = "../terraform-rpc" }
  variables { existing_alto_security_group_id = "sg-0123456789abcdef2" }
  assert {
    condition = (
      aws_vpc_security_group_ingress_rule.rpc_alb_from_alto[0].security_group_id == aws_security_group.rpc_alb[0].id &&
      aws_vpc_security_group_ingress_rule.rpc_alb_from_alto[0].referenced_security_group_id == "sg-0123456789abcdef2" &&
      aws_vpc_security_group_ingress_rule.rpc_alb_from_alto[0].ip_protocol == "tcp" &&
      aws_vpc_security_group_ingress_rule.rpc_alb_from_alto[0].from_port == 443 &&
      aws_vpc_security_group_ingress_rule.rpc_alb_from_alto[0].to_port == 443 &&
      aws_vpc_security_group_ingress_rule.rpc_alb_from_alto[0].cidr_ipv4 == null &&
      aws_vpc_security_group_ingress_rule.rpc_alb_from_alto[0].cidr_ipv6 == null
    )
    error_message = "Alto may access only the private gateway HTTPS listener through its exact task group; no CIDR or direct-node ingress is permitted."
  }
}

run "reject_alto_group_from_another_vpc" {
  command = plan
  module { source = "../terraform-rpc" }
  variables { existing_alto_security_group_id = "sg-0123456789abcdef2" }
  override_data {
    target = data.aws_security_group.existing_alto[0]
    values = { vpc_id = "vpc-0123456789abcdef1", name = "plether-sepolia-alto-task-fixture" }
  }
  expect_failures = [aws_vpc_security_group_ingress_rule.rpc_alb_from_alto]
}

run "reject_unrelated_group_in_sepolia_vpc" {
  command = plan
  module { source = "../terraform-rpc" }
  variables { existing_alto_security_group_id = "sg-0123456789abcdef2" }
  override_data {
    target = data.aws_security_group.existing_alto[0]
    values = { vpc_id = "vpc-0123456789abcdef0", name = "plether-sepolia-api-fixture" }
  }
  expect_failures = [aws_vpc_security_group_ingress_rule.rpc_alb_from_alto]
}
