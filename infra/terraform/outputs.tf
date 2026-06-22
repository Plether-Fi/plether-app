output "alb_url" {
  value = "http://${aws_lb.api.dns_name}"
}

output "ecr_repository_url" {
  value = aws_ecr_repository.api.repository_url
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

output "ecs_basket_worker_service_name" {
  value = aws_ecs_service.basket_worker.name
}

output "ecs_perps_indexer_service_name" {
  value = aws_ecs_service.perps_indexer.name
}
