output "alb_url" {
  value = var.alb_certificate_arn == "" ? "http://${aws_lb.api.dns_name}" : "https://${var.api_hostname}"
}

output "ecr_repository_url" {
  value = aws_ecr_repository.api.repository_url
}

output "otel_log_router_ecr_repository_url" {
  value = aws_ecr_repository.otel_log_router.repository_url
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

output "ecs_workers_service_name" {
  value = var.consolidate_workers ? aws_ecs_service.workers[0].name : null
}
