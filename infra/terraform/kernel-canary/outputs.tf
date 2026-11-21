output "kernel_canary_build_role_arn" {
  description = "Dedicated GitHub OIDC role for building and scanning the two canary images."
  value       = try(aws_iam_role.image_build[0].arn, null)
}

output "kernel_canary_api_repository_url" {
  value = try(aws_ecr_repository.api[0].repository_url, null)
}

output "kernel_canary_alto_repository_url" {
  value = try(aws_ecr_repository.alto[0].repository_url, null)
}

output "kernel_canary_paymaster_key_arn" {
  description = "Public identifier of the separate KMS-backed canary paymaster signer."
  value       = try(aws_kms_key.paymaster[0].arn, null)
}

output "kernel_canary_paymaster_key_alias" {
  value = try(aws_kms_alias.paymaster[0].name, null)
}

output "kernel_canary_ssm_key_arn" {
  description = "Customer-managed symmetric key for canary executor, utility, and origin SecureStrings."
  value       = try(aws_kms_key.kernel_canary_ssm[0].arn, null)
}

output "kernel_canary_ssm_key_alias" {
  value = try(aws_kms_alias.kernel_canary_ssm[0].name, null)
}

output "kernel_canary_alto_internal_rpc_url" {
  description = "Private Alto JSON-RPC URL for the reviewed Nitro SSM port-forward smoke test."
  value       = local.kernel_canary_count == 1 ? "http://${aws_lb.kernel_canary_alto[0].dns_name}" : null
}

output "kernel_canary_public_rpc_url" {
  value = local.kernel_canary_count == 1 && var.kernel_canary_route_enabled ? "https://${var.api_hostname}/api/aa/kernel/rpc" : null
}

output "kernel_canary_api_target_group_arn" {
  value = local.kernel_canary_count == 1 ? aws_lb_target_group.kernel_canary_api[0].arn : null
}

output "kernel_canary_service_names" {
  value = local.kernel_canary_count == 1 ? {
    alto       = aws_ecs_service.kernel_canary_alto[0].name
    api        = aws_ecs_service.kernel_canary_api[0].name
    reconciler = aws_ecs_service.kernel_canary_reconciler[0].name
  } : null
}

output "kernel_canary_external_ssm_parameter_names" {
  value = local.kernel_canary_count == 1 ? {
    rpc_url          = var.kernel_canary_alto_rpc_url_ssm_parameter_name
    rpc_token        = var.kernel_canary_alto_rpc_token_ssm_parameter_name
    executor_keys    = var.kernel_canary_alto_executor_keys_ssm_parameter_name
    utility_key      = var.kernel_canary_alto_utility_key_ssm_parameter_name
    api_origin_token = var.kernel_canary_origin_token_ssm_parameter_name
  } : null
}
