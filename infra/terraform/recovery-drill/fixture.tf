resource "aws_ecs_cluster" "drill" {
  name       = local.name
  depends_on = [terraform_data.isolation_guard]
}

resource "aws_security_group" "fixture" {
  name        = "${local.name}-https-only"
  description = "Nonfinancial drill: no ingress, HTTPS egress only, no PostgreSQL."
  vpc_id      = var.vpc_id
}
resource "aws_vpc_security_group_egress_rule" "fixture_https" {
  security_group_id = aws_security_group.fixture.id
  description       = "HTTPS egress for image/log delivery; no PostgreSQL port."
  cidr_ipv4         = "0.0.0.0/0"
  from_port         = 443
  to_port           = 443
  ip_protocol       = "tcp"
}
resource "aws_cloudwatch_log_group" "fixture" {
  name              = "/ecs/${local.name}"
  retention_in_days = 90
  skip_destroy      = true
}

locals {
  fixture_log_configuration = {
    logDriver = "awslogs"
    options = {
      awslogs-group         = aws_cloudwatch_log_group.fixture.name
      awslogs-region        = var.aws_region
      awslogs-stream-prefix = "drill"
    }
  }
  fixture_container_defaults = {
    readonlyRootFilesystem = true
    user                   = "65534:65534"
    stopTimeout            = 30
    linuxParameters        = { initProcessEnabled = true, capabilities = { add = [], drop = ["ALL"] } }
    secrets                = [], portMappings = [], mountPoints = [], systemControls = [], volumesFrom = []
    logConfiguration       = local.fixture_log_configuration
  }
  fixture_containers = [
    merge(local.fixture_container_defaults, {
      name        = "plether-image-recovery-drill", image = local.unavailable_image, essential = true
      command     = local.drill_profile.containers.main.command
      environment = local.drill_profile.containers.main.environment
      dependsOn   = local.drill_profile.containers.main.dependsOn
    }),
    merge(local.fixture_container_defaults, {
      name        = "image-drill-init", image = local.unavailable_image, essential = false
      command     = local.drill_profile.containers.init.command
      environment = []
    }),
    merge(local.fixture_container_defaults, {
      name        = "otel-log-router", image = local.router_image, essential = true
      entryPoint  = local.drill_profile.containers.router.entryPoint, command = local.drill_profile.containers.router.command
      environment = [{ name = "SERVICE_VERSION", value = local.drill_profile.release.sourceCommit }]
    }),
  ]
}

resource "aws_ecs_task_definition" "unavailable_fixture" {
  family                   = local.task_family
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = 256
  memory                   = 512
  execution_role_arn       = aws_iam_role.fixture_execution.arn
  # Deliberately no task_role_arn: the fixture has no runtime AWS credentials.
  enable_fault_injection = false
  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }
  container_definitions = jsonencode(local.fixture_containers)
  depends_on            = [aws_iam_role_policy.fixture_execution]
}

resource "aws_ecs_service" "fixture" {
  name                               = local.service_name
  cluster                            = aws_ecs_cluster.drill.id
  task_definition                    = aws_ecs_task_definition.unavailable_fixture.arn
  desired_count                      = var.fixture_desired_count
  launch_type                        = "FARGATE"
  platform_version                   = "1.4.0"
  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 100
  enable_execute_command             = false
  force_delete                       = true
  wait_for_steady_state              = false
  deployment_circuit_breaker {
    enable   = true
    rollback = false
  }
  network_configuration {
    subnets          = sort(tolist(var.subnet_ids))
    security_groups  = [aws_security_group.fixture.id]
    assign_public_ip = true
  }
  lifecycle { ignore_changes = [task_definition] }
  # Capture the first real pull failure after notification delivery is ready.
  depends_on = [
    aws_vpc_security_group_egress_rule.fixture_https,
    aws_cloudwatch_event_target.observer,
    aws_lambda_function_event_invoke_config.observer,
    aws_sns_topic_subscription.drill_notifications,
  ]
}
