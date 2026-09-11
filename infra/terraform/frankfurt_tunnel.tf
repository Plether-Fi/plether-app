variable "frankfurt_tunnel_ami_id" {
  type        = string
  default     = ""
  description = "Reviewed Amazon Linux 2023 ARM64 image for the isolated SSM relay; pinned in the Frankfurt profile."
  validation {
    condition     = var.frankfurt_tunnel_ami_id == "" || can(regex("^ami-[0-9a-f]{17}$", var.frankfurt_tunnel_ami_id))
    error_message = "frankfurt_tunnel_ami_id must be empty for legacy stacks or a pinned AMI ID."
  }
}

data "aws_ami" "frankfurt_tunnel" {
  count  = local.frankfurt_preparation ? 1 : 0
  owners = ["amazon"]
  filter {
    name   = "image-id"
    values = [var.frankfurt_tunnel_ami_id]
  }
  filter {
    name   = "architecture"
    values = ["arm64"]
  }
  filter {
    name   = "name"
    values = ["al2023-ami-2023.*-kernel-*-arm64"]
  }
  lifecycle {
    precondition {
      condition     = var.frankfurt_tunnel_ami_id != ""
      error_message = "The Frankfurt relay requires a reviewed pinned Amazon Linux ARM64 image."
    }
  }
}

# Public address is outbound-only for the SSM control channel. No SSH key,
# inbound rule, backend credential, signing capability, or database access.
resource "aws_security_group" "frankfurt_tunnel" {
  count       = local.frankfurt_preparation ? 1 : 0
  name        = "plether-${local.deployment_name}-tunnel"
  description = "No ingress; Session Manager relay only"
  vpc_id      = aws_vpc.main.id
  ingress     = []
}

resource "aws_vpc_security_group_egress_rule" "frankfurt_tunnel_ssm" {
  count             = local.frankfurt_preparation ? 1 : 0
  security_group_id = aws_security_group.frankfurt_tunnel[0].id
  ip_protocol       = "tcp"
  from_port         = 443
  to_port           = 443
  cidr_ipv4         = "0.0.0.0/0"
  description       = "Outbound TLS to AWS SSM control/data endpoints"
}

resource "aws_vpc_security_group_egress_rule" "frankfurt_tunnel_api" {
  count                        = local.frankfurt_preparation ? 1 : 0
  security_group_id            = aws_security_group.frankfurt_tunnel[0].id
  referenced_security_group_id = aws_security_group.alb.id
  ip_protocol                  = "tcp"
  from_port                    = 80
  to_port                      = 80
  description                  = "HTTP inside the VPC to the internal API ALB only"
}

resource "aws_iam_role" "frankfurt_tunnel" {
  count = local.frankfurt_preparation ? 1 : 0
  name  = "plether-${local.deployment_name}-tunnel"
  assume_role_policy = jsonencode({
    Version   = "2012-10-17"
    Statement = [{ Effect = "Allow", Action = "sts:AssumeRole", Principal = { Service = "ec2.amazonaws.com" } }]
  })
}

resource "aws_iam_role_policy" "frankfurt_tunnel" {
  count = local.frankfurt_preparation ? 1 : 0
  role  = aws_iam_role.frankfurt_tunnel[0].id
  name  = "session-manager-channels-only"
  # Session-channel actions do not support resource-level restrictions.
  # Unlike AmazonSSMManagedInstanceCore, this grants no Parameter Store reads.
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Action = [
        "ssm:UpdateInstanceInformation",
        "ssmmessages:CreateControlChannel", "ssmmessages:CreateDataChannel",
        "ssmmessages:OpenControlChannel", "ssmmessages:OpenDataChannel",
      ]
      Resource = "*"
    }]
  })
}

resource "aws_iam_instance_profile" "frankfurt_tunnel" {
  count = local.frankfurt_preparation ? 1 : 0
  name  = "plether-${local.deployment_name}-tunnel"
  role  = aws_iam_role.frankfurt_tunnel[0].name
}

resource "aws_instance" "frankfurt_tunnel" {
  count                       = local.frankfurt_preparation ? 1 : 0
  ami                         = data.aws_ami.frankfurt_tunnel[0].id
  instance_type               = "t4g.nano"
  subnet_id                   = aws_subnet.public[0].id
  associate_public_ip_address = true
  vpc_security_group_ids      = [aws_security_group.frankfurt_tunnel[0].id]
  iam_instance_profile        = aws_iam_instance_profile.frankfurt_tunnel[0].name
  monitoring                  = false
  user_data                   = "#!/bin/bash\nset -eu\nsystemctl enable --now amazon-ssm-agent\n"
  user_data_replace_on_change = true
  metadata_options {
    http_endpoint               = "enabled"
    http_tokens                 = "required"
    http_put_response_hop_limit = 1
  }
  root_block_device {
    encrypted             = true
    volume_type           = "gp3"
    volume_size           = 8
    delete_on_termination = true
  }
  tags       = { Name = "plether-${local.deployment_name}-tunnel" }
  depends_on = [aws_iam_role_policy.frankfurt_tunnel]
}

# No caller-provided host/port parameters: this document only reaches this ALB.
resource "aws_ssm_document" "frankfurt_tunnel" {
  count           = local.frankfurt_preparation ? 1 : 0
  name            = "plether-${local.deployment_name}-api-tunnel"
  document_type   = "Session"
  document_format = "JSON"
  content = jsonencode({
    schemaVersion = "1.0"
    description   = "Fixed-target localhost tunnel to the isolated Frankfurt API"
    sessionType   = "Port"
    properties = {
      host            = aws_lb.api.dns_name
      portNumber      = "80"
      localPortNumber = "18081"
      type            = "LocalPortForwarding"
    }
  })
}

output "frankfurt_tunnel" {
  value = local.frankfurt_preparation ? {
    instance_id       = aws_instance.frankfurt_tunnel[0].id
    document_name     = aws_ssm_document.frankfurt_tunnel[0].name
    remote_host       = aws_lb.api.dns_name
    remote_port       = 80
    local_url         = "http://127.0.0.1:18081"
    transport         = "aws-ssm-port-forwarding"
    security_group_id = aws_security_group.frankfurt_tunnel[0].id
    vpc_id            = aws_vpc.main.id
  } : null
}
