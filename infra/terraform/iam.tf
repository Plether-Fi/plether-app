data "aws_caller_identity" "current" {}

locals {
  github_actions_oidc_subjects = [
    "repo:Plether-Fi/plether-app:ref:refs/heads/master",
    "repo:Plether-Fi/plether-app:environment:candle-admin-sepolia",
    "repo:Plether-Fi/plether-app:environment:insights-sepolia-admin",
  ]

  github_deploy_ecr_repository_arns = [
    "arn:aws:ecr:${var.aws_region}:${data.aws_caller_identity.current.account_id}:repository/${var.environment == "mainnet" ? "plether-api" : "plether-api-${var.environment}"}",
    "arn:aws:ecr:${var.aws_region}:${data.aws_caller_identity.current.account_id}:repository/${var.environment == "mainnet" ? "plether-otel-log-router" : "plether-otel-log-router-${var.environment}"}",
  ]

  github_deploy_ecs_service_arns = [
    for service in concat([
      "plether-api",
      "plether-keeper",
      "plether-liquidation-worker",
      "plether-basket-worker",
      "plether-perps-indexer",
      "plether-insights-worker",
      "plether-workers",
    ], var.environment == "sepolia" ? ["plether-position-protection-worker"] : []) : "arn:aws:ecs:${var.aws_region}:${data.aws_caller_identity.current.account_id}:service/plether-${var.environment}/${service}"
  ]
}

resource "aws_iam_openid_connect_provider" "github_actions" {
  url = "https://token.actions.githubusercontent.com"

  client_id_list = ["sts.amazonaws.com"]

  # AWS validates GitHub against its trusted CA library. Provider v5 still
  # requires a syntactically valid thumbprint, but IAM does not use this value
  # for GitHub token verification.
  thumbprint_list = ["6938fd4d98bab03faadb97b34396831e3780aea1"]
}

resource "aws_iam_role" "github_deploy" {
  name                 = "plether-${var.environment}-github-deploy"
  description          = "Short-lived GitHub Actions deployment access for Plether ${var.environment}."
  max_session_duration = 3600

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Principal = {
        Federated = aws_iam_openid_connect_provider.github_actions.arn
      }
      Action = "sts:AssumeRoleWithWebIdentity"
      Condition = {
        StringEquals = {
          "token.actions.githubusercontent.com:aud" = "sts.amazonaws.com"
          "token.actions.githubusercontent.com:sub" = local.github_actions_oidc_subjects
        }
      }
    }]
  })
}

resource "aws_iam_role_policy" "github_deploy" {
  name = "plether-${var.environment}-deploy"
  role = aws_iam_role.github_deploy.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid      = "EcrLogin"
        Effect   = "Allow"
        Action   = "ecr:GetAuthorizationToken"
        Resource = "*"
      },
      {
        Sid    = "EcrImages"
        Effect = "Allow"
        Action = [
          "ecr:BatchCheckLayerAvailability",
          "ecr:BatchGetImage",
          "ecr:CompleteLayerUpload",
          "ecr:DescribeImages",
          "ecr:DescribeRepositories",
          "ecr:GetDownloadUrlForLayer",
          "ecr:InitiateLayerUpload",
          "ecr:PutImage",
          "ecr:UploadLayerPart",
        ]
        Resource = local.github_deploy_ecr_repository_arns
      },
      {
        Sid    = "EcsRead"
        Effect = "Allow"
        Action = [
          "ecs:DescribeClusters",
          "ecs:DescribeServices",
          "ecs:DescribeTaskDefinition",
          "ecs:DescribeTasks",
          "ecs:ListTagsForResource",
          "ecs:ListTaskDefinitions",
          "ecs:ListTasks",
        ]
        Resource = "*"
      },
      {
        Sid      = "EcsServiceUpdates"
        Effect   = "Allow"
        Action   = "ecs:UpdateService"
        Resource = local.github_deploy_ecs_service_arns
      },
      {
        Sid      = "EcsTagTaskDefinitions"
        Effect   = "Allow"
        Action   = "ecs:TagResource"
        Resource = "arn:aws:ecs:${var.aws_region}:${data.aws_caller_identity.current.account_id}:task-definition/plether-${var.environment}*"
      },
      {
        # ECS does not support resource-level authorization for
        # DeregisterTaskDefinition. Keep this separate from TagResource so
        # tagging remains constrained to Plether task-definition families.
        Sid      = "EcsDeregisterTaskDefinitions"
        Effect   = "Allow"
        Action   = "ecs:DeregisterTaskDefinition"
        Resource = "*"
      },
      {
        Sid      = "EcsRegisterTaskDefinition"
        Effect   = "Allow"
        Action   = "ecs:RegisterTaskDefinition"
        Resource = "*"
      },
      {
        Sid      = "EcsRunTask"
        Effect   = "Allow"
        Action   = "ecs:RunTask"
        Resource = "arn:aws:ecs:${var.aws_region}:${data.aws_caller_identity.current.account_id}:task-definition/plether-${var.environment}*"
        Condition = {
          ArnEquals = {
            "ecs:cluster" = "arn:aws:ecs:${var.aws_region}:${data.aws_caller_identity.current.account_id}:cluster/plether-${var.environment}"
          }
        }
      },
      {
        Sid      = "EcsStopTask"
        Effect   = "Allow"
        Action   = "ecs:StopTask"
        Resource = "arn:aws:ecs:${var.aws_region}:${data.aws_caller_identity.current.account_id}:task/plether-${var.environment}/*"
      },
      {
        Sid    = "PassEcsRoles"
        Effect = "Allow"
        Action = "iam:PassRole"
        Resource = [
          "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-ecs-execution",
          "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-ecs-task",
        ]
        Condition = {
          StringEquals = {
            "iam:PassedToService" = "ecs-tasks.amazonaws.com"
          }
        }
      },
      {
        Sid    = "ReadPletherParameters"
        Effect = "Allow"
        Action = [
          "ssm:GetParameter",
          "ssm:GetParameters",
        ]
        Resource = "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter/plether/${var.environment}/*"
      },
      {
        Sid      = "DeleteConsumedInsightsRequests"
        Effect   = "Allow"
        Action   = "ssm:DeleteParameter"
        Resource = "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter/plether/${var.environment}/insights-admin/requests/*"
      },
      {
        Sid    = "ReadRuntimeTopology"
        Effect = "Allow"
        Action = [
          "ec2:DescribeSecurityGroups",
          "ec2:DescribeSubnets",
          "rds:DescribeDBInstances",
          "rds:DescribePendingMaintenanceActions",
        ]
        Resource = "*"
      },
    ]
  })
}

resource "aws_iam_role" "ecs_execution" {
  name = "plether-${var.environment}-ecs-execution"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy_attachment" "ecs_execution" {
  role       = aws_iam_role.ecs_execution.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

resource "aws_iam_role_policy" "ecs_execution_ssm" {
  name = "ssm-access"
  role = aws_iam_role.ecs_execution.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Action = [
        "ssm:GetParameters",
        "ssm:GetParameter"
      ]
      Resource = compact([
        "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:parameter/plether/${var.environment}/*",
        local.external_pyth_api_key_parameter_arn,
      ])
    }]
  })
}

resource "aws_iam_role" "ecs_task" {
  name = "plether-${var.environment}-ecs-task"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy" "ecs_task_firelens_cloudwatch" {
  name = "firelens-cloudwatch-logs"
  role = aws_iam_role.ecs_task.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Action = [
        "logs:CreateLogStream",
        "logs:DescribeLogStreams",
        "logs:PutLogEvents"
      ]
      Resource = "${aws_cloudwatch_log_group.ecs.arn}:*"
    }]
  })
}
