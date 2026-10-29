locals {
  rpc_count   = var.provision_sepolia_rpc ? 1 : 0
  rpc_enabled = var.provision_sepolia_rpc
  rpc = var.sepolia_rpc == null ? {
    ami_id               = "", nitro_image = "", snapshot_url = "", snapshot_metadata_sha256 = "",
    hostname             = "", certificate_arn = "", parent_parameter = "", gateway_token_parameter = "",
    backup_url_parameter = "", backup_token_parameter = "", reference_url_parameter = "", reference_token_parameter = "", kms_key_arns = [],
    state_history_blocks = 345600, staging_volume_gib = 4096, gateway_image = ""
  } : var.sepolia_rpc
  rpc_gateway_count    = local.rpc_enabled && local.rpc.gateway_image != "" ? 1 : 0
  rpc_config_parameter = "/plether/${local.deployment_name}/rpc-node/config"
  rpc_user_data = templatefile("${path.module}/../rpc/node/user-data.sh.tftpl", {
    node_script      = base64gzip(file("${path.module}/../rpc/node/node.py"))
    config_parameter = local.rpc_config_parameter
    region           = var.aws_region
  })
  rpc_parameter_prefix = "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter"
  rpc_gateway_secrets = merge({
    RPC_GATEWAY_TOKEN = local.rpc.gateway_token_parameter
    RPC_BACKUP_URL    = local.rpc.backup_url_parameter
    RPC_REFERENCE_URL = local.rpc.reference_url_parameter
  }, local.rpc.backup_token_parameter == "" ? {} : { RPC_BACKUP_TOKEN = local.rpc.backup_token_parameter }, local.rpc.reference_token_parameter == "" ? {} : { RPC_REFERENCE_TOKEN = local.rpc.reference_token_parameter })
}

resource "terraform_data" "rpc_guard" {
  count = local.rpc_count
  lifecycle {
    precondition {
      condition     = var.environment == "sepolia" && var.aws_region == "ap-southeast-1" && var.perps_chain_id == "421614" && var.sepolia_rpc != null
      error_message = "The RPC pilot requires explicit configuration and Singapore Arbitrum Sepolia chain 421614."
    }
    precondition {
      condition     = local.rpc.gateway_image == "" || startswith(local.rpc.gateway_image, "${data.aws_caller_identity.current.account_id}.dkr.ecr.")
      error_message = "The gateway image must belong to the selected AWS account."
    }
    precondition {
      condition     = can(regex("^arn:aws:sns:ap-southeast-1:[0-9]{12}:[A-Za-z0-9_-]+$", var.operations_alarm_sns_topic_arn))
      error_message = "The RPC pilot requires an operations SNS topic before starting a stateful node."
    }
  }
}

data "aws_kms_key" "rpc_ssm" {
  count  = local.rpc_count
  key_id = "alias/aws/ssm"
}

data "aws_ami" "rpc" {
  count  = local.rpc_count
  owners = ["amazon"]
  filter {
    name   = "image-id"
    values = [local.rpc.ami_id]
  }
  filter {
    name   = "name"
    values = ["al2023-ami-2023*-x86_64"]
  }
  filter {
    name   = "architecture"
    values = ["x86_64"]
  }
}

resource "aws_security_group" "rpc_node" {
  count       = local.rpc_count
  name_prefix = "plether-sepolia-rpc-node-"
  description = "Nitro RPC only from the private gateway; no public RPC or SSH."
  vpc_id      = aws_vpc.main.id
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "rpc_gateway" {
  count       = local.rpc_count
  name_prefix = "plether-sepolia-rpc-gateway-"
  vpc_id      = aws_vpc.main.id
  egress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "rpc_alb" {
  count       = local.rpc_count
  name_prefix = "plether-sepolia-rpc-alb-"
  vpc_id      = aws_vpc.main.id
}

resource "aws_vpc_security_group_ingress_rule" "rpc_node_from_gateway" {
  count                        = local.rpc_count
  security_group_id            = aws_security_group.rpc_node[0].id
  referenced_security_group_id = aws_security_group.rpc_gateway[0].id
  ip_protocol                  = "tcp"
  from_port                    = 8547
  to_port                      = 8547
}

resource "aws_vpc_security_group_egress_rule" "rpc_gateway_to_node" {
  count                        = local.rpc_count
  security_group_id            = aws_security_group.rpc_gateway[0].id
  referenced_security_group_id = aws_security_group.rpc_node[0].id
  ip_protocol                  = "tcp"
  from_port                    = 8547
  to_port                      = 8547
}

resource "aws_vpc_security_group_ingress_rule" "rpc_gateway_from_alb" {
  count                        = local.rpc_count
  security_group_id            = aws_security_group.rpc_gateway[0].id
  referenced_security_group_id = aws_security_group.rpc_alb[0].id
  ip_protocol                  = "tcp"
  from_port                    = 3000
  to_port                      = 3000
}

resource "aws_vpc_security_group_egress_rule" "rpc_alb_to_gateway" {
  count                        = local.rpc_count
  security_group_id            = aws_security_group.rpc_alb[0].id
  referenced_security_group_id = aws_security_group.rpc_gateway[0].id
  ip_protocol                  = "tcp"
  from_port                    = 3000
  to_port                      = 3000
}

resource "aws_vpc_security_group_ingress_rule" "rpc_alb_from_ecs" {
  count                        = local.rpc_count
  security_group_id            = aws_security_group.rpc_alb[0].id
  referenced_security_group_id = aws_security_group.ecs.id
  ip_protocol                  = "tcp"
  from_port                    = 443
  to_port                      = 443
}

resource "aws_iam_role" "rpc_node" {
  count = local.rpc_count
  name  = "plether-sepolia-rpc-node"
  assume_role_policy = jsonencode({
    Version = "2012-10-17", Statement = [{ Effect = "Allow", Action = "sts:AssumeRole", Principal = { Service = "ec2.amazonaws.com" } }]
  })
}

resource "aws_iam_role_policy" "rpc_node" {
  count = local.rpc_count
  role  = aws_iam_role.rpc_node[0].id
  policy = jsonencode({
    Version = "2012-10-17", Statement = [
      { Effect = "Allow", Action = ["ssm:GetParameter"], Resource = ["${local.rpc_parameter_prefix}${local.rpc.parent_parameter}", "${local.rpc_parameter_prefix}${local.rpc_config_parameter}"] },
      { Effect = "Allow", Action = ["ssm:UpdateInstanceInformation", "ssmmessages:CreateControlChannel", "ssmmessages:CreateDataChannel", "ssmmessages:OpenControlChannel", "ssmmessages:OpenDataChannel", "ec2messages:AcknowledgeMessage", "ec2messages:DeleteMessage", "ec2messages:FailMessage", "ec2messages:GetEndpoint", "ec2messages:GetMessages", "ec2messages:SendReply"], Resource = "*" },
      { Effect = "Allow", Action = ["ssm:GetDocument", "ssm:DescribeDocument"], Resource = ["arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:document/plether-sepolia-rpc-node-admin", "arn:aws:ssm:${var.aws_region}:*:document/SSM-SessionManagerRunShell"] },
      { Effect = "Allow", Action = ["kms:Decrypt"], Resource = concat([data.aws_kms_key.rpc_ssm[0].arn], local.rpc.kms_key_arns), Condition = { StringEquals = { "kms:ViaService" = "ssm.${var.aws_region}.amazonaws.com" } } },
      { Effect = "Allow", Action = ["logs:CreateLogStream", "logs:PutLogEvents"], Resource = "${aws_cloudwatch_log_group.rpc_node[0].arn}:*" },
      { Effect = "Allow", Action = ["cloudwatch:PutMetricData"], Resource = "*", Condition = { StringEquals = { "cloudwatch:namespace" = "Plether/RPC" } } },
      { Effect = "Allow", Action = ["ec2:DescribeTags", "ec2:DescribeVolumes"], Resource = "*" },
    ]
  })
}

resource "aws_iam_instance_profile" "rpc_node" {
  count = local.rpc_count
  name  = "plether-sepolia-rpc-node"
  role  = aws_iam_role.rpc_node[0].name
}

resource "aws_ebs_volume" "rpc_staging" {
  count             = local.rpc_enabled && local.rpc.staging_volume_gib > 0 ? 1 : 0
  availability_zone = aws_subnet.public[0].availability_zone
  type              = "gp3"
  size              = local.rpc.staging_volume_gib
  encrypted         = true
  tags              = { Name = "plether-sepolia-rpc-import-staging" }
}

resource "aws_ssm_parameter" "rpc_config" {
  count = local.rpc_count
  name  = local.rpc_config_parameter
  type  = "String"
  value = jsonencode({
    image                    = local.rpc.nitro_image
    snapshot_url             = local.rpc.snapshot_url
    snapshot_metadata_sha256 = local.rpc.snapshot_metadata_sha256
    state_history_blocks     = local.rpc.state_history_blocks
    parent_parameter         = local.rpc.parent_parameter
    staging_volume_id        = local.rpc.staging_volume_gib > 0 ? aws_ebs_volume.rpc_staging[0].id : ""
    log_group                = aws_cloudwatch_log_group.rpc_node[0].name
  })
}

resource "aws_instance" "rpc_node" {
  count                       = local.rpc_count
  ami                         = data.aws_ami.rpc[0].id
  instance_type               = "i4i.4xlarge"
  subnet_id                   = aws_subnet.public[0].id
  associate_public_ip_address = true # Egress without NAT; inbound restricted by SG.
  vpc_security_group_ids      = [aws_security_group.rpc_node[0].id]
  iam_instance_profile        = aws_iam_instance_profile.rpc_node[0].name
  monitoring                  = true
  user_data_replace_on_change = true
  user_data                   = local.rpc_user_data
  metadata_options {
    http_endpoint               = "enabled"
    http_tokens                 = "required"
    http_put_response_hop_limit = 1
  }
  root_block_device {
    encrypted   = true
    volume_type = "gp3"
    volume_size = 100
  }
  tags       = { Name = "plether-sepolia-rpc-node", Role = "arbitrum-rpc" }
  depends_on = [terraform_data.rpc_guard, aws_ssm_parameter.rpc_config, aws_iam_role_policy.rpc_node]
  lifecycle {
    # Stop/start or replacement loses instance-store state. An operator must
    # explicitly remove this guard in a reviewed replacement/recovery change.
    prevent_destroy = true
    precondition {
      condition     = length(local.rpc_user_data) <= 16384
      error_message = "EC2 user data exceeds its 16 KiB limit."
    }
  }
}

resource "aws_volume_attachment" "rpc_staging" {
  count       = local.rpc_enabled && local.rpc.staging_volume_gib > 0 ? 1 : 0
  device_name = "/dev/sdf"
  volume_id   = aws_ebs_volume.rpc_staging[0].id
  instance_id = aws_instance.rpc_node[0].id
}

resource "aws_cloudwatch_log_group" "rpc_node" {
  count             = local.rpc_count
  name              = "/plether/sepolia/rpc-node"
  retention_in_days = 14
}

resource "aws_cloudwatch_log_group" "rpc_gateway" {
  count             = local.rpc_count
  name              = "/plether/sepolia/rpc-gateway"
  retention_in_days = 14
}

resource "aws_ecr_repository" "rpc_gateway" {
  count                = local.rpc_count
  name                 = "plether-rpc-gateway-sepolia"
  image_tag_mutability = "IMMUTABLE"
  image_scanning_configuration { scan_on_push = true }
}

resource "aws_lb" "rpc" {
  count              = local.rpc_count
  name               = "plether-sepolia-rpc"
  internal           = true
  load_balancer_type = "application"
  security_groups    = [aws_security_group.rpc_alb[0].id]
  subnets            = aws_subnet.public[*].id
  idle_timeout       = 20
}

resource "aws_lb_target_group" "rpc" {
  count       = local.rpc_count
  name        = "plether-sepolia-rpc"
  port        = 3000
  protocol    = "HTTP"
  vpc_id      = aws_vpc.main.id
  target_type = "ip"
  health_check {
    path                = "/healthz"
    healthy_threshold   = 2
    unhealthy_threshold = 2
    interval            = 15
    timeout             = 5
  }
}

resource "aws_lb_listener" "rpc" {
  count             = local.rpc_count
  load_balancer_arn = aws_lb.rpc[0].arn
  port              = 443
  protocol          = "HTTPS"
  ssl_policy        = "ELBSecurityPolicy-TLS13-1-2-2021-06"
  certificate_arn   = local.rpc.certificate_arn
  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.rpc[0].arn
  }
}

resource "aws_route53_zone" "rpc" {
  count = local.rpc_count
  name  = local.rpc.hostname
  vpc { vpc_id = aws_vpc.main.id }
}

resource "aws_route53_record" "rpc" {
  count   = local.rpc_count
  zone_id = aws_route53_zone.rpc[0].zone_id
  name    = local.rpc.hostname
  type    = "A"
  alias {
    name                   = aws_lb.rpc[0].dns_name
    zone_id                = aws_lb.rpc[0].zone_id
    evaluate_target_health = true
  }
}

resource "aws_iam_role" "rpc_gateway_execution" {
  count = local.rpc_count
  name  = "plether-sepolia-rpc-gateway-execution"
  assume_role_policy = jsonencode({
    Version = "2012-10-17", Statement = [{ Effect = "Allow", Action = "sts:AssumeRole", Principal = { Service = "ecs-tasks.amazonaws.com" } }]
  })
}

resource "aws_iam_role_policy" "rpc_gateway_execution" {
  count = local.rpc_count
  role  = aws_iam_role.rpc_gateway_execution[0].id
  policy = jsonencode({
    Version = "2012-10-17", Statement = [
      { Effect = "Allow", Action = ["ecr:GetAuthorizationToken"], Resource = "*" },
      { Effect = "Allow", Action = ["ecr:BatchGetImage", "ecr:GetDownloadUrlForLayer", "ecr:BatchCheckLayerAvailability"], Resource = aws_ecr_repository.rpc_gateway[0].arn },
      { Effect = "Allow", Action = ["logs:CreateLogStream", "logs:PutLogEvents"], Resource = "${aws_cloudwatch_log_group.rpc_gateway[0].arn}:*" },
      { Effect = "Allow", Action = ["ssm:GetParameters"], Resource = [for name in values(local.rpc_gateway_secrets) : "${local.rpc_parameter_prefix}${name}"] },
      { Effect = "Allow", Action = ["kms:Decrypt"], Resource = concat([data.aws_kms_key.rpc_ssm[0].arn], local.rpc.kms_key_arns), Condition = { StringEquals = { "kms:ViaService" = "ssm.${var.aws_region}.amazonaws.com" } } },
    ]
  })
}

resource "aws_ecs_task_definition" "rpc_gateway" {
  count                    = local.rpc_gateway_count
  family                   = "plether-sepolia-rpc-gateway"
  network_mode             = "awsvpc"
  requires_compatibilities = ["FARGATE"]
  cpu                      = "512"
  memory                   = "1024"
  execution_role_arn       = aws_iam_role.rpc_gateway_execution[0].arn
  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }
  container_definitions = jsonencode([{
    name                   = "rpc-gateway", image = local.rpc.gateway_image, essential = true,
    readonlyRootFilesystem = true,
    portMappings           = [{ containerPort = 3000, protocol = "tcp" }],
    environment            = [{ name = "RPC_PRIMARY_URL", value = "http://${aws_instance.rpc_node[0].private_ip}:8547" }],
    secrets                = [for name, parameter in local.rpc_gateway_secrets : { name = name, valueFrom = "${local.rpc_parameter_prefix}${parameter}" }],
    logConfiguration = {
      logDriver = "awslogs", options = {
        awslogs-group = aws_cloudwatch_log_group.rpc_gateway[0].name, awslogs-region = var.aws_region, awslogs-stream-prefix = "gateway"
      }
    }
  }])
}

resource "aws_ecs_service" "rpc_gateway" {
  count                              = local.rpc_gateway_count
  name                               = "plether-sepolia-rpc-gateway"
  cluster                            = aws_ecs_cluster.main.id
  task_definition                    = aws_ecs_task_definition.rpc_gateway[0].arn
  desired_count                      = 2
  launch_type                        = "FARGATE"
  platform_version                   = "1.4.0"
  health_check_grace_period_seconds  = 60
  deployment_minimum_healthy_percent = 100
  deployment_maximum_percent         = 200
  deployment_circuit_breaker {
    enable   = true
    rollback = true
  }
  network_configuration {
    subnets          = aws_subnet.public[*].id
    security_groups  = [aws_security_group.rpc_gateway[0].id]
    assign_public_ip = true
  }
  load_balancer {
    target_group_arn = aws_lb_target_group.rpc[0].arn
    container_name   = "rpc-gateway"
    container_port   = 3000
  }
  depends_on = [aws_lb_listener.rpc, aws_iam_role_policy.rpc_gateway_execution]
}

resource "aws_cloudwatch_metric_alarm" "rpc_node_status" {
  count               = local.rpc_count
  alarm_name          = "plether-sepolia-rpc-node-status"
  namespace           = "AWS/EC2"
  metric_name         = "StatusCheckFailed"
  comparison_operator = "GreaterThanThreshold"
  threshold           = 0
  evaluation_periods  = 2
  period              = 60
  statistic           = "Maximum"
  dimensions          = { InstanceId = aws_instance.rpc_node[0].id }
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_metric_alarm" "rpc_node_memory" {
  count               = local.rpc_count
  alarm_name          = "plether-sepolia-rpc-node-memory"
  namespace           = "Plether/RPC"
  metric_name         = "mem_used_percent"
  comparison_operator = "GreaterThanThreshold"
  threshold           = 85
  evaluation_periods  = 3
  period              = 60
  statistic           = "Maximum"
  dimensions          = { InstanceId = aws_instance.rpc_node[0].id }
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

resource "aws_cloudwatch_metric_alarm" "rpc_node_disk" {
  count               = local.rpc_count
  alarm_name          = "plether-sepolia-rpc-node-disk"
  namespace           = "Plether/RPC"
  metric_name         = "disk_used_percent"
  comparison_operator = "GreaterThanThreshold"
  threshold           = 70
  evaluation_periods  = 3
  period              = 60
  statistic           = "Maximum"
  dimensions          = { InstanceId = aws_instance.rpc_node[0].id, path = "/srv/nitro", fstype = "xfs" }
  treat_missing_data  = "breaching"
  alarm_actions       = compact([var.operations_alarm_sns_topic_arn])
}

output "sepolia_rpc_pilot" {
  value = local.rpc_enabled ? {
    instance_id            = aws_instance.rpc_node[0].id
    gateway_url            = "https://${local.rpc.hostname}/"
    direct_primary_url     = "https://${local.rpc.hostname}/primary"
    gateway_repository_url = aws_ecr_repository.rpc_gateway[0].repository_url
    staging_volume_id      = local.rpc.staging_volume_gib > 0 ? aws_ebs_volume.rpc_staging[0].id : null
    node_config_parameter  = aws_ssm_parameter.rpc_config[0].name
    node_admin_role_arn    = aws_iam_role.rpc_admin[0].arn
    node_admin_document    = aws_ssm_document.rpc_admin[0].name
  } : null
}
