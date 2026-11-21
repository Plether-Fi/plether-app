# Every existing live dependency remains owned by its original Terraform state.
# These data sources resolve only public resource metadata, never SSM values.
data "aws_vpc" "live" {
  count = local.kernel_canary_count
  id    = var.live_vpc_id
}

data "aws_subnet" "live_public" {
  for_each = var.provision_services ? toset(var.live_public_subnet_ids) : toset([])
  id       = each.value
}

data "aws_ecs_cluster" "live" {
  count        = local.kernel_canary_count
  cluster_name = "plether-sepolia"
}

data "aws_lb" "live_api" {
  count = local.kernel_canary_count
  name  = "plether-sepolia"
}

data "aws_lb_listener" "live_api_https" {
  count = local.kernel_canary_count
  arn   = var.live_api_https_listener_arn
}

data "aws_security_group" "live_api_alb" {
  count = local.kernel_canary_count
  id    = var.live_api_alb_security_group_id
}

data "aws_security_group" "live_aa_db_client" {
  count = local.kernel_canary_count
  id    = var.live_aa_db_client_security_group_id
}

data "aws_security_group" "live_rds" {
  count = local.kernel_canary_count
  id    = var.live_rds_security_group_id
}

data "aws_security_group" "live_rpc_node" {
  count = var.provision_services && var.kernel_canary_ssm_tunnel_enabled ? 1 : 0
  id    = var.live_rpc_node_security_group_id
}

data "aws_instance" "live_rpc_node" {
  count       = var.provision_services && var.kernel_canary_ssm_tunnel_enabled ? 1 : 0
  instance_id = var.live_rpc_node_instance_id
}

data "aws_vpc_security_group_rule" "live_aa_db_egress" {
  count                  = local.kernel_canary_count
  security_group_rule_id = var.live_aa_db_egress_rule_id
}

data "aws_vpc_security_group_rule" "live_aa_https_egress" {
  count                  = local.kernel_canary_count
  security_group_rule_id = var.live_aa_https_egress_rule_id
}

data "aws_vpc_security_group_rules" "live_aa_all" {
  count = local.kernel_canary_count
  filter {
    name   = "group-id"
    values = [var.live_aa_db_client_security_group_id]
  }
}

data "aws_vpc_security_group_rule" "live_rds_aa_ingress" {
  count                  = local.kernel_canary_count
  security_group_rule_id = var.live_rds_aa_ingress_rule_id
}

data "aws_ecs_task_definition" "live_api" {
  count           = local.kernel_canary_count
  task_definition = var.live_api_task_definition
}

data "aws_ecs_task_definition" "live_alto" {
  count           = local.kernel_canary_count
  task_definition = var.live_alto_task_definition
}

data "aws_ecs_task_definition" "live_reconciler" {
  count           = local.kernel_canary_count
  task_definition = var.live_reconciler_task_definition
}

data "aws_ecr_image" "kernel_canary_api" {
  count           = local.kernel_canary_count
  repository_name = "plether-kernel-api-sepolia"
  image_digest    = try(split("@", var.kernel_canary_api_image)[1], "")
}

data "aws_ecr_image" "kernel_canary_alto" {
  count           = local.kernel_canary_count
  repository_name = "plether-kernel-alto-sepolia"
  image_digest    = try(split("@", var.kernel_canary_alto_image)[1], "")
}

locals {
  live_public_subnet_ids         = local.kernel_canary_count == 1 ? [for id in var.live_public_subnet_ids : data.aws_subnet.live_public[id].id] : []
  live_api_container             = local.kernel_canary_count == 1 ? one([for c in jsondecode(data.aws_ecs_task_definition.live_api[0].container_definitions) : c if c.name == "plether-api"]) : null
  live_alto_container            = local.kernel_canary_count == 1 ? one([for c in jsondecode(data.aws_ecs_task_definition.live_alto[0].container_definitions) : c if c.name == "plether-alto"]) : null
  live_alto_init_container       = local.kernel_canary_count == 1 ? one([for c in jsondecode(data.aws_ecs_task_definition.live_alto[0].container_definitions) : c if c.name == "alto-tmp-init"]) : null
  live_reconciler_container      = local.kernel_canary_count == 1 ? one([for c in jsondecode(data.aws_ecs_task_definition.live_reconciler[0].container_definitions) : c if c.name == "plether-aa-reconciler"]) : null
  live_reconciler_init_container = local.kernel_canary_count == 1 ? one([for c in jsondecode(data.aws_ecs_task_definition.live_reconciler[0].container_definitions) : c if c.name == "aa-reconciler-tmp-init"]) : null

  kernel_canary_rpc_url_arn          = "${local.kernel_canary_ssm_prefix}/plether/sepolia/rpc-url"
  kernel_canary_perps_rpc_url_arn    = "${local.kernel_canary_ssm_prefix}/plether/sepolia/perps-rpc-url"
  kernel_canary_database_url_arn     = "${local.kernel_canary_ssm_prefix}/plether/sepolia/database-url"
  kernel_canary_security_rpc_url_arn = "${local.kernel_canary_ssm_prefix}/plether/sepolia/aa-reconciler-secondary-rpc-url"

  # The external Perps URL carries its own provider credentials. The live
  # task's Perps bearer belongs to the private Nitro gateway.
  # Keep the general RPC bearer only with the exact reviewed general RPC pair.
  kernel_canary_rpc_auth_token_secret = try(one([
    for secret in local.live_api_container.secrets : secret.valueFrom if secret.name == "RPC_URL"
    ]), null) == local.kernel_canary_rpc_url_arn ? [
    for secret in local.live_api_container.secrets : secret
    if secret.name == "RPC_AUTH_TOKEN" && secret.valueFrom == "${local.kernel_canary_ssm_prefix}/plether/sepolia/rpc-auth-token"
  ] : []

  kernel_canary_alto_allowed_rpc_methods = join(",", [
    "eth_supportedEntryPoints",
    "eth_estimateUserOperationGas",
    "eth_sendUserOperation",
    "eth_getUserOperationByHash",
    "eth_getUserOperationReceipt",
    "pimlico_getUserOperationGasPrice",
    "pimlico_getUserOperationStatus",
  ])
}

resource "terraform_data" "kernel_canary_live_identity_guard" {
  count = local.kernel_canary_count

  lifecycle {
    precondition {
      condition = (
        data.aws_caller_identity.current.account_id == local.expected_account_id
        && data.aws_vpc.live[0].id == "vpc-0df986b0047f901d3"
        && data.aws_vpc.live[0].owner_id == local.expected_account_id
        && length(var.live_public_subnet_ids) == 2
        && length(distinct(var.live_public_subnet_ids)) == 2
        && toset(var.live_public_subnet_ids) == toset(["subnet-0633bc3db0148548f", "subnet-0eef958040b7769a1"])
        && alltrue([for subnet in data.aws_subnet.live_public : subnet.vpc_id == data.aws_vpc.live[0].id && subnet.map_public_ip_on_launch])
        && data.aws_ecs_cluster.live[0].cluster_name == "plether-sepolia"
        && data.aws_ecs_cluster.live[0].status == "ACTIVE"
      )
      error_message = "The canary may attach only to the reviewed Plether Singapore Sepolia VPC, two public subnets, and ECS cluster."
    }
    precondition {
      condition = (
        !data.aws_lb.live_api[0].internal
        && data.aws_lb.live_api[0].load_balancer_type == "application"
        && data.aws_lb.live_api[0].vpc_id == data.aws_vpc.live[0].id
        && one(data.aws_lb.live_api[0].security_groups) == var.live_api_alb_security_group_id
        && data.aws_security_group.live_api_alb[0].vpc_id == data.aws_vpc.live[0].id
        && startswith(data.aws_security_group.live_api_alb[0].name, "plether-alb-")
        && data.aws_lb_listener.live_api_https[0].load_balancer_arn == data.aws_lb.live_api[0].arn
        && data.aws_lb_listener.live_api_https[0].port == 443
        && data.aws_lb_listener.live_api_https[0].protocol == "HTTPS"
        && trimspace(data.aws_lb_listener.live_api_https[0].certificate_arn) != ""
        && var.api_hostname == "api.sepolia.plether.com"
      )
      error_message = "The canary route may attach only to the reviewed existing public Sepolia HTTPS listener and ALB security group."
    }
    precondition {
      condition = (
        data.aws_lb.kernel_canary_rpc_gateway[0].internal
        && data.aws_lb.kernel_canary_rpc_gateway[0].load_balancer_type == "application"
        && data.aws_lb.kernel_canary_rpc_gateway[0].vpc_id == data.aws_vpc.live[0].id
        && one(data.aws_lb.kernel_canary_rpc_gateway[0].security_groups) == var.live_rpc_alb_security_group_id
        && (!var.kernel_canary_ssm_tunnel_enabled || try(
          data.aws_security_group.live_rpc_node[0].vpc_id == data.aws_vpc.live[0].id
          && data.aws_security_group.live_rpc_node[0].id == var.live_rpc_node_security_group_id
          && startswith(data.aws_security_group.live_rpc_node[0].name, "plether-sepolia-rpc-node-")
          && data.aws_instance.live_rpc_node[0].subnet_id == "subnet-0633bc3db0148548f"
          && data.aws_instance.live_rpc_node[0].instance_state == "running"
          && toset(data.aws_instance.live_rpc_node[0].vpc_security_group_ids) == toset([var.live_rpc_node_security_group_id]),
          false,
        ))
      )
      error_message = "The canary may use only the reviewed internal Nitro gateway and Nitro SSM node security group."
    }
    precondition {
      condition = (
        data.aws_security_group.live_aa_db_client[0].vpc_id == data.aws_vpc.live[0].id
        && startswith(data.aws_security_group.live_aa_db_client[0].name, "plether-sepolia-aa-reconciler-")
        && data.aws_security_group.live_rds[0].vpc_id == data.aws_vpc.live[0].id
        && startswith(data.aws_security_group.live_rds[0].name, "plether-rds-")
        && data.aws_vpc_security_group_rule.live_aa_db_egress[0].security_group_id == var.live_aa_db_client_security_group_id
        && data.aws_vpc_security_group_rule.live_aa_db_egress[0].referenced_security_group_id == var.live_rds_security_group_id
        && data.aws_vpc_security_group_rule.live_aa_db_egress[0].is_egress
        && data.aws_vpc_security_group_rule.live_aa_db_egress[0].ip_protocol == "tcp"
        && data.aws_vpc_security_group_rule.live_aa_db_egress[0].from_port == 5432
        && data.aws_vpc_security_group_rule.live_aa_db_egress[0].to_port == 5432
        && data.aws_vpc_security_group_rule.live_aa_https_egress[0].security_group_id == var.live_aa_db_client_security_group_id
        && data.aws_vpc_security_group_rule.live_aa_https_egress[0].is_egress
        && data.aws_vpc_security_group_rule.live_aa_https_egress[0].ip_protocol == "tcp"
        && data.aws_vpc_security_group_rule.live_aa_https_egress[0].from_port == 443
        && data.aws_vpc_security_group_rule.live_aa_https_egress[0].to_port == 443
        && data.aws_vpc_security_group_rule.live_aa_https_egress[0].cidr_ipv4 == "0.0.0.0/0"
        && toset(data.aws_vpc_security_group_rules.live_aa_all[0].ids) == toset([var.live_aa_db_egress_rule_id, var.live_aa_https_egress_rule_id])
        && data.aws_vpc_security_group_rule.live_rds_aa_ingress[0].security_group_id == var.live_rds_security_group_id
        && data.aws_vpc_security_group_rule.live_rds_aa_ingress[0].referenced_security_group_id == var.live_aa_db_client_security_group_id
        && !data.aws_vpc_security_group_rule.live_rds_aa_ingress[0].is_egress
        && data.aws_vpc_security_group_rule.live_rds_aa_ingress[0].ip_protocol == "tcp"
        && data.aws_vpc_security_group_rule.live_rds_aa_ingress[0].from_port == 5432
        && data.aws_vpc_security_group_rule.live_rds_aa_ingress[0].to_port == 5432
      )
      error_message = "The canary API and reconciler may reuse only the reviewed native-AA PostgreSQL client group and existing RDS admission; this state never modifies the live RDS SG."
    }
    precondition {
      condition = (
        data.aws_ecs_task_definition.live_api[0].family == "plether-sepolia"
        && data.aws_ecs_task_definition.live_api[0].revision == 154
        && data.aws_ecs_task_definition.live_alto[0].family == "plether-sepolia-alto"
        && data.aws_ecs_task_definition.live_alto[0].revision == 12
        && data.aws_ecs_task_definition.live_reconciler[0].family == "plether-sepolia-aa-reconciler"
        && data.aws_ecs_task_definition.live_reconciler[0].revision == 72
        && alltrue([for task in [data.aws_ecs_task_definition.live_api[0], data.aws_ecs_task_definition.live_alto[0], data.aws_ecs_task_definition.live_reconciler[0]] : task.network_mode == "awsvpc" && contains(task.requires_compatibilities, "FARGATE")])
        && local.live_api_container.name == "plether-api"
        && local.live_alto_container.name == "plether-alto"
        && local.live_reconciler_container.name == "plether-aa-reconciler"
      )
      error_message = "Canary task templates must be the reviewed, pinned live Sepolia API, Alto, and reconciler revisions."
    }
  }
}
