resource "aws_lb" "api" {
  name                       = "plether-${local.deployment_name}"
  internal                   = false
  load_balancer_type         = "application"
  idle_timeout               = 75
  drop_invalid_header_fields = true
  enable_deletion_protection = var.environment == "mainnet"
  security_groups            = [aws_security_group.alb.id]
  subnets                    = aws_subnet.public[*].id

  lifecycle {
    precondition {
      condition     = var.environment != "mainnet" || (trimspace(var.alb_certificate_arn) != "" && trimspace(var.api_hostname) != "")
      error_message = "Mainnet requires a valid ACM alb_certificate_arn and api_hostname; plaintext HTTP API forwarding is forbidden."
    }
  }
}

resource "aws_lb_target_group" "api" {
  name                 = "plether-${local.deployment_name}"
  port                 = 3001
  protocol             = "HTTP"
  vpc_id               = aws_vpc.main.id
  target_type          = "ip"
  deregistration_delay = 30

  health_check {
    path                = "/api/health"
    healthy_threshold   = 2
    unhealthy_threshold = 3
    timeout             = 10
    interval            = 30
    matcher             = "200"
  }
}

resource "aws_lb_listener" "http" {
  load_balancer_arn = aws_lb.api.arn
  port              = 80
  protocol          = "HTTP"

  default_action {
    type             = var.alb_certificate_arn == "" ? "forward" : "redirect"
    target_group_arn = var.alb_certificate_arn == "" ? aws_lb_target_group.api.arn : null

    dynamic "redirect" {
      for_each = var.alb_certificate_arn == "" ? [] : [1]

      content {
        port        = "443"
        protocol    = "HTTPS"
        status_code = "HTTP_301"
      }
    }
  }
}

resource "aws_lb_listener" "https" {
  count = var.alb_certificate_arn == "" ? 0 : 1

  load_balancer_arn = aws_lb.api.arn
  port              = 443
  protocol          = "HTTPS"
  ssl_policy        = "ELBSecurityPolicy-TLS13-1-2-2021-06"
  certificate_arn   = var.alb_certificate_arn

  default_action {
    type             = nonsensitive(var.alb_origin_secret_header_value != "") ? "fixed-response" : "forward"
    target_group_arn = nonsensitive(var.alb_origin_secret_header_value != "") ? null : aws_lb_target_group.api.arn

    dynamic "fixed_response" {
      for_each = nonsensitive(var.alb_origin_secret_header_value != "") ? [1] : []

      content {
        content_type = "application/json"
        message_body = "{\"error\":\"forbidden\"}"
        status_code  = "403"
      }
    }
  }
}

resource "aws_lb_listener_rule" "https_origin_authenticated" {
  count = var.alb_certificate_arn != "" && nonsensitive(var.alb_origin_secret_header_value != "") ? 1 : 0

  listener_arn = aws_lb_listener.https[0].arn
  priority     = 10

  action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.api.arn
  }

  condition {
    http_header {
      http_header_name = "X-Plether-Origin-Secret"
      values           = [var.alb_origin_secret_header_value]
    }
  }
}
