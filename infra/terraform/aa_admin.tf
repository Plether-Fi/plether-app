locals {
  aa_admin_log_router_container = merge(local.otel_log_router_container, {
    environment = [
      { name = "AWS_REGION", value = var.aws_region },
      { name = "CLOUDWATCH_LOG_GROUP", value = aws_cloudwatch_log_group.ecs.name },
      { name = "DEPLOYMENT_ENVIRONMENT", value = var.environment },
      { name = "ECS_CLUSTER_NAME", value = aws_ecs_cluster.main.name },
      { name = "SERVICE_VERSION", value = "unknown" },
    ]
  })

  aa_admin_tmp_init_container = {
    name                   = "aa-admin-tmp-init"
    image                  = "${aws_ecr_repository.api.repository_url}:latest"
    essential              = false
    readonlyRootFilesystem = true
    user                   = "0:0"
    entryPoint             = ["/bin/sh", "-c"]
    command                = ["chmod 1777 /tmp"]

    linuxParameters = {
      initProcessEnabled = true
      capabilities = {
        drop = ["ALL"]
      }
    }

    mountPoints = [{
      sourceVolume  = "aa-admin-tmp"
      containerPath = "/tmp"
      readOnly      = false
    }]
    portMappings   = []
    systemControls = []
    volumesFrom    = []
    environment    = []
    secrets        = []
  }
}

resource "aws_security_group" "aa_admin_kms_attest" {
  count = local.self_hosted_aa_resource_count

  name_prefix = "plether-${var.environment}-aa-admin-kms-attest-"
  description = "One-off KMS signer attestation with HTTPS egress only."
  vpc_id      = aws_vpc.main.id

  tags = {
    Name = "plether-${var.environment}-aa-admin-kms-attest"
  }

  lifecycle { create_before_destroy = true }
}

resource "aws_vpc_security_group_egress_rule" "aa_admin_kms_attest_https" {
  count = local.self_hosted_aa_resource_count

  security_group_id = aws_security_group.aa_admin_kms_attest[0].id
  description       = "KMS, ECR, SSM, CloudWatch, and PostHog HTTPS access."
  cidr_ipv4         = "0.0.0.0/0"
  from_port         = 443
  to_port           = 443
  ip_protocol       = "tcp"
}

resource "aws_security_group" "aa_admin_resume_issuance" {
  count = local.self_hosted_aa_resource_count

  name_prefix = "plether-${var.environment}-aa-admin-resume-issuance-"
  description = "One-off issuance-control task with PostgreSQL and HTTPS egress."
  vpc_id      = aws_vpc.main.id

  tags = {
    Name = "plether-${var.environment}-aa-admin-resume-issuance"
  }

  lifecycle { create_before_destroy = true }
}

resource "aws_vpc_security_group_egress_rule" "aa_admin_resume_issuance_https" {
  count = local.self_hosted_aa_resource_count

  security_group_id = aws_security_group.aa_admin_resume_issuance[0].id
  description       = "ECR, SSM, CloudWatch, and PostHog HTTPS access."
  cidr_ipv4         = "0.0.0.0/0"
  from_port         = 443
  to_port           = 443
  ip_protocol       = "tcp"
}

resource "aws_vpc_security_group_egress_rule" "aa_admin_resume_issuance_postgres" {
  count = local.self_hosted_aa_resource_count

  security_group_id            = aws_security_group.aa_admin_resume_issuance[0].id
  referenced_security_group_id = aws_security_group.rds.id
  description                  = "Clear the issuance breaker only after an exact database-state check."
  from_port                    = 5432
  to_port                      = 5432
  ip_protocol                  = "tcp"
}

resource "aws_iam_role" "aa_admin_kms_attest_execution" {
  count = local.self_hosted_aa_resource_count

  name = "plether-${var.environment}-aa-admin-kms-attest-execution"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy_attachment" "aa_admin_kms_attest_execution" {
  count = local.self_hosted_aa_resource_count

  role       = aws_iam_role.aa_admin_kms_attest_execution[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

resource "aws_iam_role_policy" "aa_admin_kms_attest_execution_secrets" {
  count = local.self_hosted_aa_resource_count

  name = "exact-log-secret-access"
  role = aws_iam_role.aa_admin_kms_attest_execution[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect   = "Allow"
      Action   = ["ssm:GetParameters", "ssm:GetParameter"]
      Resource = [aws_ssm_parameter.posthog_otlp_authorization_header.arn]
    }]
  })
}

resource "aws_iam_role" "aa_admin_kms_attest_task" {
  count = local.self_hosted_aa_resource_count

  name = "plether-${var.environment}-aa-admin-kms-attest-task"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy" "aa_admin_kms_attest_task" {
  count = local.self_hosted_aa_resource_count

  name = "fixed-digest-attestation-and-logs"
  role = aws_iam_role.aa_admin_kms_attest_task[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid      = "ReadPaymasterSignerPublicKey"
        Effect   = "Allow"
        Action   = ["kms:DescribeKey", "kms:GetPublicKey"]
        Resource = aws_kms_key.aa_paymaster_signer[0].arn
      },
      {
        Sid      = "SignFixedAttestationDigest"
        Effect   = "Allow"
        Action   = ["kms:Sign"]
        Resource = aws_kms_key.aa_paymaster_signer[0].arn
        Condition = {
          StringEquals = {
            "kms:SigningAlgorithm" = "ECDSA_SHA_256"
          }
        }
      },
      {
        Sid    = "WriteFireLensCloudWatchCopy"
        Effect = "Allow"
        Action = [
          "logs:CreateLogStream",
          "logs:DescribeLogStreams",
          "logs:PutLogEvents",
        ]
        Resource = "${aws_cloudwatch_log_group.ecs.arn}:*"
      },
    ]
  })
}

resource "aws_iam_role" "aa_admin_resume_issuance_execution" {
  count = local.self_hosted_aa_resource_count

  name = "plether-${var.environment}-aa-admin-resume-issuance-execution"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy_attachment" "aa_admin_resume_issuance_execution" {
  count = local.self_hosted_aa_resource_count

  role       = aws_iam_role.aa_admin_resume_issuance_execution[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

resource "aws_iam_role_policy" "aa_admin_resume_issuance_execution_secrets" {
  count = local.self_hosted_aa_resource_count

  name = "exact-database-and-log-secret-access"
  role = aws_iam_role.aa_admin_resume_issuance_execution[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Action = [
        "ssm:GetParameters",
        "ssm:GetParameter",
      ]
      Resource = [
        aws_ssm_parameter.database_url.arn,
        aws_ssm_parameter.posthog_otlp_authorization_header.arn,
      ]
    }]
  })
}

resource "aws_iam_role" "aa_admin_resume_issuance_task" {
  count = local.self_hosted_aa_resource_count

  name = "plether-${var.environment}-aa-admin-resume-issuance-task"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy" "aa_admin_resume_issuance_task_logs" {
  count = local.self_hosted_aa_resource_count

  name = "firelens-cloudwatch-logs"
  role = aws_iam_role.aa_admin_resume_issuance_task[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Action = [
        "logs:CreateLogStream",
        "logs:DescribeLogStreams",
        "logs:PutLogEvents",
      ]
      Resource = "${aws_cloudwatch_log_group.ecs.arn}:*"
    }]
  })
}

resource "aws_ecs_task_definition" "aa_admin_kms_attest" {
  count = local.self_hosted_aa_resource_count

  family                   = "plether-${var.environment}-aa-admin-kms-attest"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = 256
  memory                   = 512
  execution_role_arn       = aws_iam_role.aa_admin_kms_attest_execution[0].arn
  task_role_arn            = aws_iam_role.aa_admin_kms_attest_task[0].arn
  enable_fault_injection   = false

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  volume {
    name = "aa-admin-tmp"
  }

  container_definitions = jsonencode([
    {
      name                   = "plether-aa-admin"
      image                  = "${aws_ecr_repository.api.repository_url}:latest"
      essential              = true
      command                = ["/usr/local/bin/plether-aa-admin", "attest-kms"]
      readonlyRootFilesystem = true
      user                   = "65534:65534"
      stopTimeout            = 120

      dependsOn = [
        { containerName = "aa-admin-tmp-init", condition = "SUCCESS" },
        { containerName = "otel-log-router", condition = "START" },
      ]

      linuxParameters = {
        initProcessEnabled = true
        capabilities       = { drop = ["ALL"] }
      }

      mountPoints = [{
        sourceVolume  = "aa-admin-tmp"
        containerPath = "/tmp"
        readOnly      = false
      }]
      portMappings   = []
      systemControls = []
      volumesFrom    = []

      logConfiguration = local.posthog_log_configuration
      secrets          = []
      environment = [
        { name = "HOME", value = "/tmp" },
        { name = "TMPDIR", value = "/tmp" },
        { name = "AWS_REGION", value = var.aws_region },
        { name = "AA_PAYMASTER_KMS_KEY_ID", value = aws_kms_key.aa_paymaster_signer[0].arn },
        { name = "AA_PAYMASTER_SIGNER_ADDRESS", value = var.aa_paymaster_signer_address },
      ]
    },
    local.aa_admin_tmp_init_container,
    local.aa_admin_log_router_container,
  ])

  tags = {
    Capability = "fixed-digest-kms-attestation"
  }

  depends_on = [
    aws_iam_role_policy.aa_admin_kms_attest_execution_secrets,
    aws_iam_role_policy.aa_admin_kms_attest_task,
    aws_vpc_security_group_egress_rule.aa_admin_kms_attest_https,
    terraform_data.self_hosted_aa_guard,
  ]
}

resource "aws_ecs_task_definition" "aa_admin_resume_issuance" {
  count = local.self_hosted_aa_resource_count

  family                   = "plether-${var.environment}-aa-admin-resume-issuance"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                      = 256
  memory                   = 512
  execution_role_arn       = aws_iam_role.aa_admin_resume_issuance_execution[0].arn
  task_role_arn            = aws_iam_role.aa_admin_resume_issuance_task[0].arn
  enable_fault_injection   = false

  runtime_platform {
    cpu_architecture        = "ARM64"
    operating_system_family = "LINUX"
  }

  volume {
    name = "aa-admin-tmp"
  }

  container_definitions = jsonencode([
    {
      name      = "plether-aa-admin"
      image     = "${aws_ecr_repository.api.repository_url}:latest"
      essential = true
      command = [
        "/usr/local/bin/plether-aa-admin",
        "resume-issuance",
        "--expected-reason",
        "__WORKFLOW_REQUIRED__",
        "--operator-note",
        "__WORKFLOW_REQUIRED__",
      ]
      readonlyRootFilesystem = true
      user                   = "65534:65534"
      stopTimeout            = 120

      dependsOn = [
        { containerName = "aa-admin-tmp-init", condition = "SUCCESS" },
        { containerName = "otel-log-router", condition = "START" },
      ]

      linuxParameters = {
        initProcessEnabled = true
        capabilities       = { drop = ["ALL"] }
      }

      mountPoints = [{
        sourceVolume  = "aa-admin-tmp"
        containerPath = "/tmp"
        readOnly      = false
      }]
      portMappings   = []
      systemControls = []
      volumesFrom    = []

      logConfiguration = local.posthog_log_configuration
      secrets = [{
        name      = "DATABASE_URL"
        valueFrom = aws_ssm_parameter.database_url.arn
      }]
      environment = [
        { name = "HOME", value = "/tmp" },
        { name = "TMPDIR", value = "/tmp" },
      ]
    },
    local.aa_admin_tmp_init_container,
    local.aa_admin_log_router_container,
  ])

  tags = {
    Capability = "resume-aa-issuance"
  }

  depends_on = [
    aws_iam_role_policy.aa_admin_resume_issuance_execution_secrets,
    aws_iam_role_policy.aa_admin_resume_issuance_task_logs,
    aws_vpc_security_group_egress_rule.aa_admin_resume_issuance_https,
    aws_vpc_security_group_egress_rule.aa_admin_resume_issuance_postgres,
    terraform_data.self_hosted_aa_guard,
  ]
}
