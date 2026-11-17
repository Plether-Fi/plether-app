output "cluster_name" { value = aws_ecs_cluster.drill.name }
output "service_name" { value = aws_ecs_service.fixture.name }
output "fixture_execution_role_arn" { value = aws_iam_role.fixture_execution.arn }
output "recovery_workflow_role_arn" {
  description = "Set protected backend-admin-sepolia AWS_RECOVERY_DRILL_ROLE_ARN to this value after review."
  value       = aws_iam_role.recovery_workflow.arn
}
output "observer_function_name" { value = aws_lambda_function.observer.function_name }
output "evidence_queue_url" { value = aws_sqs_queue.drill_notifications.url }
output "fixture_log_group" { value = aws_cloudwatch_log_group.fixture.name }
output "retained_observer_evidence_log_group" { value = aws_cloudwatch_log_group.observer_evidence.name }
output "qualified_recovery_images" { value = { application = local.application_image, router = local.router_image } }
