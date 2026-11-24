# Synthetic service plans. No AWS credentials, RPC calls, keys or live state.
mock_provider "aws" {
  mock_data "aws_caller_identity" { defaults = { account_id = "932542905614" } }
  mock_data "aws_iam_openid_connect_provider" { defaults = { arn = "arn:aws:iam::932542905614:oidc-provider/token.actions.githubusercontent.com" } }
  mock_data "aws_vpc" { defaults = { id = "vpc-0df986b0047f901d3", owner_id = "932542905614", state = "available" } }
  mock_data "aws_subnet" { defaults = { vpc_id = "vpc-0df986b0047f901d3", state = "available", map_public_ip_on_launch = true } }
  mock_data "aws_ecs_cluster" { defaults = { arn = "arn:aws:ecs:ap-southeast-1:932542905614:cluster/plether-sepolia", cluster_name = "plether-sepolia", status = "ACTIVE" } }
  mock_data "aws_lb" { defaults = { arn = "arn:aws:elasticloadbalancing:ap-southeast-1:932542905614:loadbalancer/app/plether-sepolia/9544b926599859e9", internal = false, load_balancer_type = "application", vpc_id = "vpc-0df986b0047f901d3", security_groups = ["sg-08bc02814791dc349"] } }
  mock_data "aws_lb_listener" { defaults = { load_balancer_arn = "arn:aws:elasticloadbalancing:ap-southeast-1:932542905614:loadbalancer/app/plether-sepolia/9544b926599859e9", port = 443, protocol = "HTTPS", certificate_arn = "arn:aws:acm:ap-southeast-1:932542905614:certificate/11111111-1111-1111-1111-111111111111" } }
  mock_data "aws_security_group" { defaults = { vpc_id = "vpc-0df986b0047f901d3" } }
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
  mock_data "aws_ecr_image" { defaults = { image_digest = "sha256:b37c5697bc5331391cb8bc5ac6e828d15019fc786c58a33a8650ff3c434ac2aa" } }
  override_data {
    target = data.aws_ecr_image.kernel_canary_api[0]
    values = { image_digest = "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" }
  }
  override_data {
    target = data.aws_ecs_task_definition.live_api[0]
    values = {
      family                   = "plether-sepolia"
      revision                 = 154
      cpu                      = "1024"
      memory                   = "2048"
      network_mode             = "awsvpc"
      requires_compatibilities = ["FARGATE"]
      container_definitions = jsonencode([
        { name = "otel-log-router" },
        {
          name        = "plether-api"
          environment = [{ name = "PORT", value = "3001" }]
          secrets = [
            { name = "RPC_URL", valueFrom = "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/rpc-url" },
            { name = "RPC_AUTH_TOKEN", valueFrom = "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/rpc-auth-token" },
            { name = "PERPS_RPC_URL", valueFrom = "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/app-rpc-url" },
            { name = "PERPS_RPC_AUTH_TOKEN", valueFrom = "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/rpc-gateway-token" },
            { name = "PYTH_API_KEY", valueFrom = "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/pyth-api-key" },
          ]
        },
      ])
    }
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
        {
          name       = "plether-alto"
          entryPoint = ["node", "--import", "/app/plether-rpc-auth.mjs", "/app/src/esm/cli/alto.js"]
          command    = ["run", "--safe-mode"]
          environment = [
            { name = "ALTO_MAX_GAS_PER_USER_OP", value = "5000000" },
            { name = "ALTO_MAX_GAS_PER_BUNDLE", value = "20000000" },
            { name = "ALTO_RPC_URL", value = "https://legacy-rpc.example.test" },
            { name = "ALTO_SEND_TRANSACTION_RPC_URL", value = "https://legacy-relay.example.test" },
            { name = "ALTO_SAFE_MODE", value = "true" },
            { name = "ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION", value = "true" },
            { name = "ALTO_EXPIRATION_CHECK", value = "false" },
          ]
          secrets = [{ name = "PLETHER_ALTO_RPC_AUTH_TOKEN", valueFrom = "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/rpc-gateway-token" }]
        },
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
  mock_resource "aws_lb" {
    override_during = plan
    defaults = {
      arn      = "arn:aws:elasticloadbalancing:ap-southeast-1:932542905614:loadbalancer/app/kernel-canary-alto/2222222222222222"
      dns_name = "internal-alto.example.test"
    }
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
  live_aa_db_egress_rule_id       = "sgr-db"
  live_aa_https_egress_rule_id    = "sgr-https"
  live_rds_aa_ingress_rule_id     = "sgr-rds"
  kernel_canary_alto_image        = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-alto-sepolia@sha256:b37c5697bc5331391cb8bc5ac6e828d15019fc786c58a33a8650ff3c434ac2aa"
  kernel_canary_api_image         = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-api-sepolia@sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  kernel_canary_profile_json      = jsonencode(jsondecode(file("../../../apps/backend/test/fixtures/kernel-deployment.json")).profile)
  kernel_canary_sponsorship_json = jsonencode({
    paymaster            = "0x5555555555555555555555555555555555555555"
    paymasterCodeHash    = "0x7777777777777777777777777777777777777777777777777777777777777777"
    policyId             = "0x8888888888888888888888888888888888888888888888888888888888888888"
    signerAddress        = "0x6666666666666666666666666666666666666666"
    deploymentBlock      = 1
    deploymentBlockHash  = "0x9999999999999999999999999999999999999999999999999999999999999999"
    minDepositWei        = 1000000000000000
    verificationGasLimit = 200000
    validitySeconds      = 300
    canaryOwners         = ["0x3333333333333333333333333333333333333333"]
    budget = {
      maxCostWei            = 1000000000000000
      accountOutstandingWei = 2000000000000000
      clientOutstandingWei  = 2000000000000000
      globalOutstandingWei  = 5000000000000000
      accountHourlyWei      = 4000000000000000
      globalHourlyWei       = 10000000000000000
      globalDailyWei        = 20000000000000000
    }
  })
}

run "services_stage_dark_in_isolated_state" {
  command = plan
  assert {
    condition = (aws_ecs_service.kernel_canary_alto[0].desired_count == 0
      && aws_ecs_service.kernel_canary_api[0].desired_count == 0
      && aws_ecs_service.kernel_canary_reconciler[0].desired_count == 0
      && length(aws_lb_listener_rule.kernel_canary_api) == 0
      && length(aws_iam_role_policy.kernel_canary_paymaster_sign) == 0
    )
    error_message = "The isolated service skeleton must start no tasks, sign no sponsorships and expose no public route."
  }
  assert {
    condition = alltrue([
      for name in ["api", "reconciler"] : (
        toset([for ref in jsondecode(aws_iam_role_policy.kernel_canary_execution_secrets[name].policy).Statement[0].Resource : basename(ref)]) ==
        toset(name == "api" ? ["rpc-url", "rpc-auth-token", "perps-rpc-url", "aa-reconciler-secondary-rpc-url", "database-url", "kernel-canary-aa-origin-token"] : ["perps-rpc-url", "aa-reconciler-secondary-rpc-url", "database-url"])
      )
      ]) && alltrue([
      for secrets in concat(
        [for container in jsondecode(aws_ecs_task_definition.kernel_canary_api[0].container_definitions) : container.secrets if container.name == "plether-api"],
        [for container in jsondecode(aws_ecs_task_definition.kernel_canary_reconciler[0].container_definitions) : container.secrets if container.name == "plether-aa-reconciler"],
      ) :
      one([for secret in secrets : basename(secret.valueFrom) if secret.name == "PERPS_RPC_URL"]) == "perps-rpc-url"
      && one([for secret in secrets : basename(secret.valueFrom) if secret.name == "AA_SECURITY_RPC_URL"]) == "aa-reconciler-secondary-rpc-url"
      && alltrue([for secret in secrets : !contains(["PERPS_RPC_AUTH_TOKEN", "PYTH_API_KEY"], secret.name)])
    ])
    error_message = "Both external canary RPC clients use reviewed independent references and receive neither the Nitro bearer nor the Pyth key."
  }
  assert {
    condition = (
      toset([for statement in jsondecode(aws_iam_role_policy.kernel_canary_execution_secrets["alto"].policy).Statement :
        basename(statement.Condition.StringEquals["kms:EncryptionContext:PARAMETER_ARN"]) if contains(statement.Action, "kms:Decrypt")
      ]) == toset(["kernel-canary-alto-executor-private-keys", "kernel-canary-alto-utility-private-key"])
      && alltrue([
        for statement in jsondecode(aws_iam_role_policy.kernel_canary_execution_secrets["alto"].policy).Statement :
        statement.Condition.StringEquals["kms:ViaService"] == "ssm.ap-southeast-1.amazonaws.com" if contains(statement.Action, "kms:Decrypt")
      ])
      && one([
        for statement in jsondecode(aws_iam_role_policy.kernel_canary_execution_secrets["api"].policy).Statement :
        statement.Condition.StringEquals if contains(statement.Action, "kms:Decrypt")
        ]) == {
        "kms:ViaService"                      = "ssm.ap-southeast-1.amazonaws.com"
        "kms:EncryptionContext:PARAMETER_ARN" = "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/kernel-canary-aa-origin-token"
      }
      && length(jsondecode(aws_iam_role_policy.kernel_canary_execution_secrets["reconciler"].policy).Statement) == 1
    )
    error_message = "Canary workload decryption must remain scoped to SSM and exact dedicated contexts, with no decrypt grant to the reconciler."
  }
}

run "reject_mutable_service_images" {
  command = plan
  variables {
    kernel_canary_api_image = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-api-sepolia:latest"
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "reject_unreviewed_alto_digest_from_same_repository" {
  command = plan
  variables {
    kernel_canary_alto_image = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-alto-sepolia@sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
  }
  override_data {
    target = data.aws_ecr_image.kernel_canary_alto[0]
    values = { image_digest = "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc" }
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "reject_unreviewed_api_security_group" {
  command = plan
  variables {
    live_api_alb_security_group_id = "sg-11111111111111111"
  }
  expect_failures = [terraform_data.kernel_canary_live_identity_guard]
}

run "drop_general_rpc_bearer_when_live_binding_differs" {
  command = plan
  override_data {
    target = data.aws_ecs_task_definition.live_api[0]
    values = {
      family                   = "plether-sepolia"
      revision                 = 154
      cpu                      = "1024"
      memory                   = "2048"
      network_mode             = "awsvpc"
      requires_compatibilities = ["FARGATE"]
      container_definitions = jsonencode([
        { name = "otel-log-router" },
        {
          name        = "plether-api"
          environment = [{ name = "PORT", value = "3001" }]
          secrets = [
            { name = "RPC_URL", valueFrom = "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/app-rpc-url" },
            { name = "RPC_AUTH_TOKEN", valueFrom = "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/rpc-auth-token" },
          ]
        },
      ])
    }
  }
  assert {
    condition     = alltrue([for secret in local.kernel_canary_api_secrets : secret.name != "RPC_AUTH_TOKEN"])
    error_message = "An inherited general RPC bearer must be omitted unless the live URL reference matches the isolated URL reference."
  }
}

run "reject_nitro_rpc_parameter" {
  command = plan
  variables {
    kernel_canary_alto_rpc_url_ssm_parameter_name = "/plether/sepolia/app-rpc-url"
  }
  expect_failures = [var.kernel_canary_alto_rpc_url_ssm_parameter_name]
}

run "reject_lag_and_poll_outside_runtime_bounds" {
  command = plan
  variables {
    kernel_canary_max_safe_lag_seconds = tostring(jsondecode(file("../../../apps/backend/test/fixtures/kernel-deployment.json")).safeHeadLagSeconds.maximum + 1)
    kernel_canary_poll_seconds         = "61"
    kernel_canary_batch_blocks         = "02000"
  }
  expect_failures = [
    var.kernel_canary_max_safe_lag_seconds,
    var.kernel_canary_poll_seconds,
    var.kernel_canary_batch_blocks,
  ]
}

run "reject_safe_lag_below_kernel_minimum" {
  command = plan
  variables {
    kernel_canary_max_safe_lag_seconds = tostring(jsondecode(file("../../../apps/backend/test/fixtures/kernel-deployment.json")).safeHeadLagSeconds.minimum - 1)
  }
  expect_failures = [var.kernel_canary_max_safe_lag_seconds]
}

run "reject_alto_before_simulator_attestation" {
  command = plan
  variables {
    kernel_canary_alto_desired_count = 1
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "managed_alto_stays_private_during_qualification" {
  command = plan
  variables {
    kernel_canary_alto_desired_count                = 1
    kernel_canary_simulators_attested               = true
    kernel_canary_entrypoint_simulation_contract_v9 = "0x1111111111111111111111111111111111111111"
    kernel_canary_pimlico_simulation_contract       = "0x2222222222222222222222222222222222222222"
  }
  assert {
    condition = (aws_ecs_service.kernel_canary_alto[0].desired_count == 1
      && aws_ecs_service.kernel_canary_api[0].desired_count == 0
      && length(aws_lb_listener_rule.kernel_canary_api) == 0
      && aws_lb.kernel_canary_alto[0].internal
      && aws_vpc_security_group_ingress_rule.kernel_canary_alto_alb_from_api[0].referenced_security_group_id == aws_security_group.kernel_canary_api_alto_client[0].id
      && aws_vpc_security_group_ingress_rule.kernel_canary_alto_from_alb[0].referenced_security_group_id == aws_security_group.kernel_canary_alto_alb[0].id
      && aws_vpc_security_group_egress_rule.kernel_canary_https["alto"].from_port == 443
    )
    error_message = "The managed Alto must use outbound HTTPS and retain only its canary ALB ingress while the public route is dark."
  }
  assert {
    condition = one([
      for container in jsondecode(aws_ecs_task_definition.kernel_canary_alto[0].container_definitions) :
      concat(container.entryPoint, container.command) if container.name == "plether-alto"
    ]) == ["node", "/app/src/esm/cli/alto.js", "run"]
    error_message = "The canary image must start directly without the live Alto auth preload."
  }
  assert {
    condition = one([
      for container in jsondecode(aws_ecs_task_definition.kernel_canary_alto[0].container_definitions) :
      tomap({ for env in container.environment : env.name => env.value if contains([
        "ALTO_MAX_GAS_PER_USER_OP", "ALTO_MAX_GAS_PER_BUNDLE", "ALTO_SAFE_MODE",
        "ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION", "ALTO_DEPLOY_SIMULATIONS_CONTRACT",
        "ALTO_EXPIRATION_CHECK",
        "ALTO_RPC_URL", "ALTO_SEND_TRANSACTION_RPC_URL", "PLETHER_ALTO_RPC_AUTH_TOKEN",
      ], env.name) })
      if container.name == "plether-alto"
      ]) == tomap({
      ALTO_MAX_GAS_PER_USER_OP                      = tostring(jsondecode(file("../../../apps/backend/test/fixtures/kernel-deployment.json")).gas.aggregateGas)
      ALTO_MAX_GAS_PER_BUNDLE                       = "20000000"
      ALTO_SAFE_MODE                                = "false"
      ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION = "false"
      ALTO_DEPLOY_SIMULATIONS_CONTRACT              = "false"
      ALTO_EXPIRATION_CHECK                         = "true"
    })
    error_message = "Managed Alto must run normal validation with the reviewed gas caps and no inherited endpoint or simulator auto-deploy."
  }
  assert {
    condition = one([
      for container in jsondecode(aws_ecs_task_definition.kernel_canary_alto[0].container_definitions) :
      tomap({ for secret in container.secrets : secret.name => basename(secret.valueFrom) })
      if container.name == "plether-alto"
      ]) == tomap({
      ALTO_RPC_URL               = "perps-rpc-url"
      ALTO_EXECUTOR_PRIVATE_KEYS = "kernel-canary-alto-executor-private-keys"
      ALTO_UTILITY_PRIVATE_KEY   = "kernel-canary-alto-utility-private-key"
      }) && toset([for ref in jsondecode(aws_iam_role_policy.kernel_canary_execution_secrets["alto"].policy).Statement[0].Resource : basename(ref)]) == toset([
      "perps-rpc-url", "kernel-canary-alto-executor-private-keys", "kernel-canary-alto-utility-private-key",
    ])
    error_message = "The managed Alto task and execution policy must contain the exact managed RPC and signing key references."
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

run "reject_eth_with_zero_owner" {
  command = plan
  variables {
    kernel_canary_alto_desired_count                = 1
    kernel_canary_simulators_attested               = true
    kernel_canary_entrypoint_simulation_contract_v9 = "0x1111111111111111111111111111111111111111"
    kernel_canary_pimlico_simulation_contract       = "0x2222222222222222222222222222222222222222"
    kernel_canary_api_desired_count                 = 1
    kernel_canary_db_schema_confirmed               = true
    kernel_canary_profile_attested                  = true
    kernel_canary_managed_alto_qualified            = true
    kernel_canary_managed_alto_qualified_image      = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-alto-sepolia@sha256:b37c5697bc5331391cb8bc5ac6e828d15019fc786c58a33a8650ff3c434ac2aa"
    kernel_canary_eth_enabled                       = true
    kernel_canary_owners_json                       = "[\"0x0000000000000000000000000000000000000000\"]"
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
    kernel_canary_managed_alto_qualified            = true
    kernel_canary_managed_alto_qualified_image      = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-alto-sepolia@sha256:b37c5697bc5331391cb8bc5ac6e828d15019fc786c58a33a8650ff3c434ac2aa"
    kernel_canary_reconciler_desired_count          = 1
    kernel_canary_sponsorship_enabled               = true
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "reject_private_payment_without_managed_qualification" {
  command = plan
  variables {
    kernel_canary_alto_desired_count                = 1
    kernel_canary_simulators_attested               = true
    kernel_canary_entrypoint_simulation_contract_v9 = "0x1111111111111111111111111111111111111111"
    kernel_canary_pimlico_simulation_contract       = "0x2222222222222222222222222222222222222222"
    kernel_canary_api_desired_count                 = 1
    kernel_canary_db_schema_confirmed               = true
    kernel_canary_profile_attested                  = true
    kernel_canary_eth_enabled                       = true
    kernel_canary_owners_json                       = "[\"0x3333333333333333333333333333333333333333\"]"
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "qualified_payment_route_targets_only_kernel_api" {
  command = plan
  variables {
    kernel_canary_alto_desired_count                = 1
    kernel_canary_simulators_attested               = true
    kernel_canary_entrypoint_simulation_contract_v9 = "0x1111111111111111111111111111111111111111"
    kernel_canary_pimlico_simulation_contract       = "0x2222222222222222222222222222222222222222"
    kernel_canary_api_desired_count                 = 1
    kernel_canary_db_schema_confirmed               = true
    kernel_canary_profile_attested                  = true
    kernel_canary_managed_alto_qualified            = true
    kernel_canary_managed_alto_qualified_image      = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-alto-sepolia@sha256:b37c5697bc5331391cb8bc5ac6e828d15019fc786c58a33a8650ff3c434ac2aa"
    kernel_canary_preview_origin                    = "https://kernel-preview.pages.dev"
    kernel_canary_route_enabled                     = true
    kernel_canary_eth_enabled                       = true
    kernel_canary_sponsorship_enabled               = true
    kernel_canary_reconciler_desired_count          = 1
    kernel_canary_max_safe_lag_seconds              = tostring(jsondecode(file("../../../apps/backend/test/fixtures/kernel-deployment.json")).safeHeadLagSeconds.minimum)
    kernel_canary_owners_json                       = "[\"0x3333333333333333333333333333333333333333\"]"

    kernel_canary_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/kernel-canary-reconciler-archive-rpc-url"
  }
  assert {
    condition = (aws_lb_listener_rule.kernel_canary_api[0].priority == 180
      && toset(flatten([for c in aws_lb_listener_rule.kernel_canary_api[0].condition : [for p in c.path_pattern : p.values]])) == toset(["/api/aa/kernel/rpc"])
      && toset(flatten([for c in aws_lb_listener_rule.kernel_canary_api[0].condition : [for h in c.host_header : h.values]])) == toset(["api.sepolia.plether.com"])
      && length(aws_ecs_service.kernel_canary_api[0].load_balancer) == 1
      && length(aws_iam_role_policy.kernel_canary_paymaster_sign) == 1
      && contains(aws_ecs_service.kernel_canary_api[0].network_configuration[0].security_groups, "sg-0c095c11c27499ca7")
      && contains(aws_ecs_service.kernel_canary_reconciler[0].network_configuration[0].security_groups, "sg-0c095c11c27499ca7")
      && one([for e in local.kernel_canary_api_environment : e.value if e.name == "KERNEL_CANARY_API_MODE"]) == "true"
      && one([for e in local.kernel_canary_api_environment : e.value if e.name == "API_BACKGROUND_WORKERS_ENABLED"]) == "false"
      && one([for e in local.kernel_canary_api_environment : e.value if e.name == "AA_KERNEL_MAX_SAFE_LAG_SECONDS"]) == var.kernel_canary_max_safe_lag_seconds
    )
    error_message = "Only the exact Kernel backend path may open after managed qualification, and both database clients must reuse the existing narrow AA group."
  }
  assert {
    condition = (
      one([for s in local.kernel_canary_reconciler_secrets : s.valueFrom if s.name == "AA_SECURITY_RPC_URL"]) == "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/kernel-canary-reconciler-archive-rpc-url"
      && one([for s in local.kernel_canary_api_secrets : s.valueFrom if s.name == "AA_SECURITY_RPC_URL"]) == "arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/aa-reconciler-secondary-rpc-url"
      && !contains(local.kernel_canary_execution_secret_arns.api, local.kernel_canary_reconciler_secondary_rpc_url_arn)
      && contains(local.kernel_canary_execution_secret_arns.reconciler, local.kernel_canary_reconciler_secondary_rpc_url_arn)
    )
    error_message = "The dedicated archive parameter must be readable only by the isolated Kernel reconciler execution role."
  }
}

run "reject_primary_plether_preview_origin" {
  command = plan
  variables {
    kernel_canary_alto_desired_count                = 1
    kernel_canary_simulators_attested               = true
    kernel_canary_entrypoint_simulation_contract_v9 = "0x1111111111111111111111111111111111111111"
    kernel_canary_pimlico_simulation_contract       = "0x2222222222222222222222222222222222222222"
    kernel_canary_api_desired_count                 = 1
    kernel_canary_db_schema_confirmed               = true
    kernel_canary_profile_attested                  = true
    kernel_canary_managed_alto_qualified            = true
    kernel_canary_managed_alto_qualified_image      = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-alto-sepolia@sha256:b37c5697bc5331391cb8bc5ac6e828d15019fc786c58a33a8650ff3c434ac2aa"
    kernel_canary_preview_origin                    = "https://sepolia.plether.com"
    kernel_canary_route_enabled                     = true
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "reject_route_after_alto_image_changes" {
  command = plan
  variables {
    kernel_canary_alto_desired_count                = 1
    kernel_canary_simulators_attested               = true
    kernel_canary_entrypoint_simulation_contract_v9 = "0x1111111111111111111111111111111111111111"
    kernel_canary_pimlico_simulation_contract       = "0x2222222222222222222222222222222222222222"
    kernel_canary_api_desired_count                 = 1
    kernel_canary_db_schema_confirmed               = true
    kernel_canary_profile_attested                  = true
    kernel_canary_managed_alto_qualified            = true
    kernel_canary_managed_alto_qualified_image      = "932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-kernel-alto-sepolia@sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    kernel_canary_preview_origin                    = "https://kernel-preview.pages.dev"
    kernel_canary_route_enabled                     = true
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "reject_attested_staker_profile" {
  command = plan
  variables {
    kernel_canary_db_schema_confirmed      = true
    kernel_canary_profile_attested         = true
    kernel_canary_reconciler_desired_count = 1
    kernel_canary_profile_json = jsonencode(merge(
      jsondecode(file("../../../apps/backend/test/fixtures/kernel-deployment.json")).profile,
      {
        staker         = format("0x%040d", 9)
        stakerCodeHash = format("0x%064d", 9)
      },
    ))
  }
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "reject_old_paymaster_for_direct_reconciler" {
  command = plan
  variables {
    kernel_canary_db_schema_confirmed      = true
    kernel_canary_profile_attested         = true
    kernel_canary_reconciler_desired_count = 1
    kernel_canary_sponsorship_json = jsonencode({
      paymaster            = "0x778b6962467632e1CC9D91c6801443d17D842fDb"
      paymasterCodeHash    = format("0x%064d", 7)
      policyId             = format("0x%064d", 8)
      signerAddress        = format("0x%040d", 6)
      deploymentBlock      = 1
      deploymentBlockHash  = format("0x%064d", 9)
      minDepositWei        = 1000
      verificationGasLimit = 200000
      validitySeconds      = 300
      canaryOwners         = [format("0x%040d", 3)]
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
  expect_failures = [terraform_data.kernel_canary_guard]
}

run "reconciler_drains_without_sponsorship" {
  command = plan
  variables {
    kernel_canary_db_schema_confirmed      = true
    kernel_canary_profile_attested         = true
    kernel_canary_reconciler_desired_count = 1
    kernel_canary_max_safe_lag_seconds     = tostring(jsondecode(file("../../../apps/backend/test/fixtures/kernel-deployment.json")).safeHeadLagSeconds.maximum)
  }
  assert {
    condition = (
      aws_ecs_service.kernel_canary_reconciler[0].desired_count == 1
      && aws_ecs_service.kernel_canary_api[0].desired_count == 0
      && aws_ecs_service.kernel_canary_alto[0].desired_count == 0
      && length(aws_iam_role_policy.kernel_canary_paymaster_sign) == 0
      && length(aws_lb_listener_rule.kernel_canary_api) == 0
    )
    error_message = "The reconciler must drain in isolation after removing route access and KMS signing permission."
  }
  assert {
    condition = one(flatten([
      for container in jsondecode(aws_ecs_task_definition.kernel_canary_reconciler[0].container_definitions) : [
        for setting in container.environment : setting.value if setting.name == "AA_KERNEL_SPONSORSHIP_ENABLED"
      ] if container.name == "plether-aa-reconciler"
    ])) == "false"
    error_message = "During drain, the reconciler must receive its valid sponsor profile with sponsorship disabled."
  }
  assert {
    condition = one(flatten([
      for container in jsondecode(aws_ecs_task_definition.kernel_canary_reconciler[0].container_definitions) : [
        for setting in container.environment : setting.value if setting.name == "AA_KERNEL_MAX_SAFE_LAG_SECONDS"
      ] if container.name == "plether-aa-reconciler"
    ])) == var.kernel_canary_max_safe_lag_seconds
    error_message = "The canary reconciler must receive the safe-head allowance from the task configuration."
  }
}
