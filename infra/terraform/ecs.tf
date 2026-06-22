resource "aws_ecs_cluster" "main" {
  name = "plether-${var.environment}"
}

locals {
  workers_command = <<-EOT
    set -eu
    pids=""

    stop_all() {
      status="$${1:-143}"
      for pid in $pids; do
        kill -TERM "$pid" 2>/dev/null || true
      done
      for pid in $pids; do
        wait "$pid" 2>/dev/null || true
      done
      exit "$status"
    }

    trap 'stop_all 143' INT TERM

    RPC_URL="$PERPS_RPC_URL" CHAIN_ID="$PERPS_CHAIN_ID" plether-keeper &
    pids="$pids $!"

    RPC_URL="$ETH_RPC_URL" CHAIN_ID="$ETH_CHAIN_ID" plether-basket-worker --latest-loop --poll-seconds "$BASKET_WORKER_POLL_SECONDS" &
    pids="$pids $!"

    (
      while :; do
        RPC_URL="$ETH_RPC_URL" CHAIN_ID="$ETH_CHAIN_ID" PYTH_SAMPLE_INTERVAL_SECONDS="$BASKET_HISTORY_INTERVAL_SECONDS" plether-basket-worker --backfill-once --backfill-days "$BASKET_HISTORY_BACKFILL_DAYS" || echo "daily basket history backfill failed"
        sleep "$BASKET_HISTORY_BACKFILL_POLL_SECONDS"
      done
    ) &
    pids="$pids $!"

    RPC_URL="$PERPS_RPC_URL" CHAIN_ID="$PERPS_CHAIN_ID" plether-perps-indexer --loop &
    pids="$pids $!"

    while :; do
      for pid in $pids; do
        if ! kill -0 "$pid" 2>/dev/null; then
          status=1
          wait "$pid" || status=$?
          echo "worker process $pid exited with status $status"
          stop_all "$status"
        fi
      done
      sleep 5
    done
  EOT

  pyth_environment = [
    { name = "PYTH_HERMES_URL", value = var.pyth_hermes_url },
    { name = "PYTH_BENCHMARKS_URL", value = var.pyth_benchmarks_url },
    { name = "PYTH_BACKFILL_DAYS", value = var.pyth_backfill_days },
    { name = "PYTH_SAMPLE_INTERVAL_SECONDS", value = var.pyth_sample_interval_seconds },
  ]

  pyth_api_key_secret = var.enable_pyth_api_key ? [
    {
      name      = "PYTH_API_KEY"
      valueFrom = aws_ssm_parameter.pyth_api_key[0].arn
    }
  ] : []
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

    secrets = concat([
      {
        name      = "RPC_URL"
        valueFrom = aws_ssm_parameter.rpc_url.arn
      },
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      }
    ], local.pyth_api_key_secret)

    environment = concat([
      { name = "PORT", value = "3001" },
      { name = "CHAIN_ID", value = var.chain_id },
      { name = "CORS_ORIGINS", value = var.cors_origins },
      { name = "INDEXER_START_BLOCK", value = var.indexer_start_block },
    ], local.pyth_environment)
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

  lifecycle {
    ignore_changes = [task_definition]
  }
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
  name                               = "plether-keeper"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.keeper.arn
  desired_count                      = var.consolidate_workers ? 0 : 1
  launch_type                        = "FARGATE"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  lifecycle {
    ignore_changes = [task_definition]
  }
}

resource "aws_ecs_task_definition" "basket_worker" {
  family                   = "plether-${var.environment}-basket-worker"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.container_cpu
  memory                   = var.container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

  container_definitions = jsonencode([{
    name      = "plether-basket-worker"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["plether-basket-worker", "--latest-loop", "--poll-seconds", var.basket_worker_poll_seconds]

    secrets = concat([
      {
        name      = "RPC_URL"
        valueFrom = aws_ssm_parameter.rpc_url.arn
      },
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      }
    ], local.pyth_api_key_secret)

    environment = concat([
      { name = "CHAIN_ID", value = var.chain_id },
    ], local.pyth_environment)
  }])
}

resource "aws_ecs_service" "basket_worker" {
  name                               = "plether-basket-worker"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.basket_worker.arn
  desired_count                      = var.consolidate_workers ? 0 : 1
  launch_type                        = "FARGATE"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  lifecycle {
    ignore_changes = [task_definition]
  }
}

resource "aws_ecs_task_definition" "perps_indexer" {
  family                   = "plether-${var.environment}-perps-indexer"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.container_cpu
  memory                   = var.container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

  container_definitions = jsonencode([{
    name      = "plether-perps-indexer"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["plether-perps-indexer", "--loop"]

    secrets = [
      {
        name      = "PERPS_RPC_URL"
        valueFrom = aws_ssm_parameter.perps_rpc_url.arn
      },
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      }
    ]

    environment = [
      { name = "CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
      { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
      { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
      { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
      { name = "PERPS_INDEXER_CONFIRMATIONS", value = var.perps_indexer_confirmations },
      { name = "PERPS_INDEXER_BATCH_SIZE", value = var.perps_indexer_batch_size },
      { name = "PERPS_INDEXER_POLL_SECONDS", value = var.perps_indexer_poll_seconds },
    ]
  }])
}

resource "aws_ecs_service" "perps_indexer" {
  name                               = "plether-perps-indexer"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.perps_indexer.arn
  desired_count                      = var.consolidate_workers ? 0 : 1
  launch_type                        = "FARGATE"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  lifecycle {
    ignore_changes = [task_definition]
  }
}

resource "aws_ecs_task_definition" "workers" {
  count = var.consolidate_workers ? 1 : 0

  family                   = "plether-${var.environment}-workers"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = var.workers_container_cpu
  memory                   = var.workers_container_memory
  execution_role_arn       = aws_iam_role.ecs_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

  container_definitions = jsonencode([{
    name      = "plether-workers"
    image     = "${aws_ecr_repository.api.repository_url}:latest"
    essential = true
    command   = ["sh", "-c", local.workers_command]

    secrets = concat([
      {
        name      = "ETH_RPC_URL"
        valueFrom = aws_ssm_parameter.rpc_url.arn
      },
      {
        name      = "PERPS_RPC_URL"
        valueFrom = aws_ssm_parameter.perps_rpc_url.arn
      },
      {
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      },
      {
        name      = "KEEPER_PRIVATE_KEY"
        valueFrom = aws_ssm_parameter.keeper_private_key.arn
      }
    ], local.pyth_api_key_secret)

    environment = concat([
      { name = "ETH_CHAIN_ID", value = var.chain_id },
      { name = "PERPS_CHAIN_ID", value = var.perps_chain_id },
      { name = "PERPS_ORDER_ROUTER", value = var.perps_order_router },
      { name = "PERPS_PLETHER_ORACLE", value = var.perps_plether_oracle },
      { name = "PERPS_CFD_ENGINE", value = var.perps_cfd_engine },
      { name = "PERPS_MARGIN_CLEARINGHOUSE", value = var.perps_margin_clearinghouse },
      { name = "PERPS_INDEXER_START_BLOCK", value = var.perps_indexer_start_block },
      { name = "PERPS_INDEXER_CONFIRMATIONS", value = var.perps_indexer_confirmations },
      { name = "PERPS_INDEXER_BATCH_SIZE", value = var.perps_indexer_batch_size },
      { name = "PERPS_INDEXER_POLL_SECONDS", value = var.perps_indexer_poll_seconds },
      { name = "BASKET_WORKER_POLL_SECONDS", value = var.basket_worker_poll_seconds },
      { name = "BASKET_HISTORY_BACKFILL_DAYS", value = var.basket_history_backfill_days },
      { name = "BASKET_HISTORY_INTERVAL_SECONDS", value = var.basket_history_interval_seconds },
      { name = "BASKET_HISTORY_BACKFILL_POLL_SECONDS", value = var.basket_history_backfill_poll_seconds },
      { name = "KEEPER_POLL_SECONDS", value = var.keeper_poll_seconds },
      { name = "KEEPER_MAX_BATCH_SIZE", value = var.keeper_max_batch_size },
      { name = "KEEPER_CONFIRMATIONS", value = var.keeper_confirmations },
      { name = "KEEPER_GAS_BUFFER_BPS", value = var.keeper_gas_buffer_bps },
      { name = "KEEPER_FEE_BUFFER_BPS", value = var.keeper_fee_buffer_bps },
    ], local.pyth_environment)
  }])
}

resource "aws_ecs_service" "workers" {
  count = var.consolidate_workers ? 1 : 0

  name                               = "plether-workers"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.workers[0].arn
  desired_count                      = 1
  launch_type                        = "FARGATE"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100

  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.ecs.id]
    assign_public_ip = true
  }

  lifecycle {
    ignore_changes = [task_definition]
  }
}
