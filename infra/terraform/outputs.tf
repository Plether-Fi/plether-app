output "alb_url" {
  value = var.alb_certificate_arn == "" ? "http://${aws_lb.api.dns_name}" : "https://${var.api_hostname}"
}

output "ecr_repository_url" {
  value = aws_ecr_repository.api.repository_url
}

output "otel_log_router_ecr_repository_url" {
  value = aws_ecr_repository.otel_log_router.repository_url
}

output "alto_ecr_repository_url" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_ecr_repository.alto[0].repository_url : null
}

output "alto_ecr_image_tag" {
  value = local.self_hosted_aa_resource_count == 1 ? local.alto_ecr_image_tag : null
}

output "alto_internal_rpc_url" {
  value = local.self_hosted_aa_resource_count == 1 ? "http://${aws_lb.alto[0].dns_name}" : null
}

output "alto_load_balancer_name" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_lb.alto[0].name : null
}

output "alto_target_group_arn" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_lb_target_group.alto[0].arn : null
}

output "alto_ecs_service_name" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_ecs_service.alto[0].name : null
}

output "alto_ecs_task_definition_family" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_ecs_task_definition.alto[0].family : null
}

output "alto_external_ssm_parameter_names" {
  value = local.self_hosted_aa_resource_count == 1 ? {
    rpc_url                  = var.alto_rpc_url_ssm_parameter_name
    send_transaction_rpc_url = trimspace(var.alto_send_transaction_rpc_url_ssm_parameter_name) == "" ? null : var.alto_send_transaction_rpc_url_ssm_parameter_name
    executor_private_keys    = var.alto_executor_private_keys_ssm_parameter_name
    utility_private_key      = var.alto_utility_private_key_ssm_parameter_name
  } : null
}

output "aa_proxy_origin_token_ssm_parameter_name" {
  value = local.aa_gateway_enabled ? aws_ssm_parameter.aa_proxy_origin_token[0].name : null
}

output "aa_paymaster_kms_key_arn" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_kms_key.aa_paymaster_signer[0].arn : null
}

output "aa_paymaster_kms_key_id" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_kms_key.aa_paymaster_signer[0].key_id : null
}

output "aa_paymaster_kms_alias" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_kms_alias.aa_paymaster_signer[0].name : null
}

output "aa_reconciler_ecs_service_name" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_ecs_service.aa_reconciler[0].name : null
}

output "aa_reconciler_ecs_task_definition_family" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_ecs_task_definition.aa_reconciler[0].family : null
}

output "aa_reconciler_secondary_rpc_url_ssm_parameter_name" {
  value = local.self_hosted_aa_resource_count == 1 ? var.aa_reconciler_secondary_rpc_url_ssm_parameter_name : null
}

output "aa_reconciler_max_safe_lag_seconds" {
  value = local.self_hosted_aa_resource_count == 1 ? var.aa_reconciler_max_safe_lag_seconds : null
}

output "aa_admin_kms_attest_task_definition_family" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_ecs_task_definition.aa_admin_kms_attest[0].family : null
}

output "aa_admin_resume_issuance_task_definition_family" {
  value = local.self_hosted_aa_resource_count == 1 ? aws_ecs_task_definition.aa_admin_resume_issuance[0].family : null
}

output "rds_endpoint" {
  value = aws_db_instance.postgres.endpoint
}

output "ecs_cluster_name" {
  value = aws_ecs_cluster.main.name
}

output "ecs_service_name" {
  value = aws_ecs_service.api.name
}

output "ecs_keeper_service_name" {
  value = aws_ecs_service.keeper.name
}

output "ecs_liquidation_worker_service_name" {
  value = aws_ecs_service.liquidation_worker.name
}

output "ecs_basket_worker_service_name" {
  value = aws_ecs_service.basket_worker.name
}

output "ecs_perps_indexer_service_name" {
  value = aws_ecs_service.perps_indexer.name
}

output "ecs_insights_worker_service_name" {
  value = aws_ecs_service.insights_worker.name
}

output "ecs_workers_service_name" {
  value = var.consolidate_workers ? aws_ecs_service.workers[0].name : null
}

output "github_deploy_role_arn" {
  value = aws_iam_role.github_deploy.arn
}
