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

resource "aws_ecs_task_definition" "keeper" {
  family                   = "plether-${var.environment}-keeper"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.container_cpu
  memory                   = var.container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

  container_definitions = jsonencode([{
    name      = "plether-keeper"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["plether-keeper"]

    secrets = [
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      },
      {
        name      = "PERPS_RPC_URL"
        valueFrom = aws_ssm_parameter.perps_rpc_url.arn
      },
      {
        name      = "KEEPER_PRIVATE_KEY"
        valueFrom = aws_ssm_parameter.keeper_private_key.arn
      }
    ]

    environment = [
      { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
      { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
      { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
      { name = "KEEPER_POLL_SECONDS", value = var.keeper_poll_seconds },
      { name = "KEEPER_MAX_BATCH_SIZE", value = var.keeper_max_batch_size },
      { name = "KEEPER_CONFIRMATIONS", value = var.keeper_confirmations },
      { name = "KEEPER_GAS_BUFFER_BPS", value = var.keeper_gas_buffer_bps },
      { name = "KEEPER_FEE_BUFFER_BPS", value = var.keeper_fee_buffer_bps },
    ]
  }])
}

resource "aws_ecs_service" "keeper" {
  name            = "plether-keeper"
  cluster         = aws_ecs_cluster.main.id
  task_definition = aws_ecs_task_definition.keeper.arn
  desired_count   = 1
  launch_type     = "FARGATE"

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }
}
