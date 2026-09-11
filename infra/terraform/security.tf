resource "aws_security_group" "alb" {
  name_prefix = "plether-alb-"
  vpc_id      = aws_vpc.main.id

  ingress {
    from_port       = 80
    to_port         = 80
    protocol        = "tcp"
    cidr_blocks     = local.frankfurt_preparation ? [] : ["0.0.0.0/0"]
    security_groups = local.frankfurt_preparation ? concat([aws_security_group.frankfurt_tunnel[0].id], aws_security_group.frankfurt_worker_client[*].id) : []
  }

  dynamic "ingress" {
    for_each = local.frankfurt_preparation ? [] : [1]
    content {
      from_port   = 443
      to_port     = 443
      protocol    = "tcp"
      cidr_blocks = ["0.0.0.0/0"]
    }
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  lifecycle { create_before_destroy = true }
}

// Identity-only group: permits just the consolidated worker's private API path.
// Kept separate from ecs to avoid mutually dependent inline security groups.
resource "aws_security_group" "frankfurt_worker_client" {
  count = local.frankfurt_preparation && local.frankfurt_trading_configured ? 1 : 0
  name = "plether-sepolia-aa-temp-worker-api-client"
  vpc_id = aws_vpc.main.id
  ingress = []
  egress = []
}

resource "aws_security_group" "ecs" {
  name_prefix = "plether-ecs-"
  vpc_id      = aws_vpc.main.id

  ingress {
    from_port       = 3001
    to_port         = 3001
    protocol        = "tcp"
    security_groups = [aws_security_group.alb.id]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  lifecycle { create_before_destroy = true }
}

resource "aws_security_group" "rds" {
  name_prefix = "plether-rds-"
  vpc_id      = aws_vpc.main.id

  ingress {
    from_port       = 5432
    to_port         = 5432
    protocol        = "tcp"
    security_groups = [aws_security_group.ecs.id]
  }

  dynamic "ingress" {
    for_each = local.self_hosted_aa_resource_count == 1 ? [1] : []

    content {
      description     = "PostgreSQL from the native-AA reconciler only"
      from_port       = 5432
      to_port         = 5432
      protocol        = "tcp"
      security_groups = [aws_security_group.aa_reconciler_task[0].id]
    }
  }

  dynamic "ingress" {
    for_each = local.self_hosted_aa_resource_count == 1 ? [1] : []

    content {
      description     = "PostgreSQL from the one-off AA issuance-control task only"
      from_port       = 5432
      to_port         = 5432
      protocol        = "tcp"
      security_groups = [aws_security_group.aa_admin_resume_issuance[0].id]
    }
  }

  lifecycle { create_before_destroy = true }
}
