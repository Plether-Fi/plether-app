resource "aws_ecs_cluster" "main" {
  name = "plether-${var.environment}"
}

resource "aws_ecs_task_definition" "api" {
  family                   = "plether-${var.environment}"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.container_cpu
  memory                   = var.container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

  container_definitions = jsonencode([{
    name      = "plether-api"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true

    portMappings = [{
      containerPort = 3001
      protocol      = "tcp"
    }]

    secrets = [
      {
        name      = "RPC_URL"
        valueFrom = aws_ssm_parameter.rpc_url.arn
      },
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      }
    ]

    environment = [
      { name = "PORT", value = "3001" },
      { name = "CHAIN_ID", value = var.chain_id },
      { name = "CORS_ORIGINS", value = var.cors_origins },
      { name = "INDEXER_START_BLOCK", value = var.indexer_start_block },
    ]
  }])
}

resource "aws_ecs_service" "api" {
  name            = "plether-api"
  cluster         = aws_ecs_cluster.main.id
  task_definition = aws_ecs_task_definition.api.arn
  desired_count   = 1
  launch_type     = "FARGATE"

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  load_balancer {
    target_group_arn = aws_lb_target_group.api.arn
    container_name   = "plether-api"
    container_port   = 3001
  }

  depends_on = [aws_lb_listener.http]
}
