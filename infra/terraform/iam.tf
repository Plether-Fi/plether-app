data "aws_caller_identity" "current" {}

locals {
  github_actions_oidc_subjects = concat(
    [
      "repo:Plether-Fi/plether-app:ref:refs/heads/master",
      "repo:Plether-Fi/plether-app:environment:candle-admin-sepolia",
      "repo:Plether-Fi/plether-app:environment:insights-sepolia-admin",
    ],
    local.self_hosted_aa_resource_count == 1 ? [
      "repo:Plether-Fi/plether-app:environment:aa-admin-sepolia",
      "repo:Plether-Fi/plether-app:environment:alto-admin-sepolia",
    ] : []
  )

  github_deploy_ecr_repository_arns = concat(
    [
      "arn:aws:ecr:${var.aws_region}:${data.aws_caller_identity.current.account_id}:repository/${var.environment == "mainnet" ? "plether-api" : "plether-api-${var.environment}"}",
      "arn:aws:ecr:${var.aws_region}:${data.aws_caller_identity.current.account_id}:repository/${var.environment == "mainnet" ? "plether-otel-log-router" : "plether-otel-log-router-${var.environment}"}",
    ],
    local.self_hosted_aa_resource_count == 1 ? [
      "arn:aws:ecr:${var.aws_region}:${data.aws_caller_identity.current.account_id}:repository/plether-alto-sepolia",
    ] : []
  )

  github_deploy_ecs_service_arns = [
    for service in concat(
      [
        "plether-api",
        "plether-keeper",
        "plether-liquidation-worker",
        "plether-basket-worker",
        "plether-perps-indexer",
        "plether-insights-worker",
        "plether-workers",
      ],
      local.self_hosted_aa_resource_count == 1 ? [
        "plether-aa-reconciler",
        "plether-alto",
      ] : []
    ) : "arn:aws:ecs:${var.aws_region}:${data.aws_caller_identity.current.account_id}:service/plether-${var.environment}/${service}"
  ]

  github_deploy_ecs_role_arns = concat(
    [
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-ecs-execution",
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-ecs-task",
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-api-execution",
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-api-task",
    ],
    local.self_hosted_aa_resource_count == 1 ? [
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-aa-admin-kms-attest-execution",
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-aa-admin-kms-attest-task",
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-aa-admin-resume-issuance-execution",
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-aa-admin-resume-issuance-task",
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-aa-reconciler-execution",
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-aa-reconciler-task",
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-alto-execution",
      "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-alto-task",
    ] : []
  )

  github_deploy_aa_admin_role_arns = local.self_hosted_aa_resource_count == 1 ? [
    "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-aa-admin-kms-attest-execution",
    "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-aa-admin-kms-attest-task",
    "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-aa-admin-resume-issuance-execution",
    "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/plether-${var.environment}-aa-admin-resume-issuance-task",
  ] : []
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
        Sid      = "PassEcsRoles"
        Effect   = "Allow"
        Action   = "iam:PassRole"
        Resource = local.github_deploy_ecs_role_arns
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

resource "aws_iam_role_policy" "github_deploy_self_hosted_aa" {
  count = local.self_hosted_aa_resource_count

  name = "plether-${var.environment}-deploy-self-hosted-aa"
  role = aws_iam_role.github_deploy.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "InspectAaAdminRoles"
        Effect = "Allow"
        Action = [
          "iam:GetRole",
          "iam:GetRolePolicy",
          "iam:ListAttachedRolePolicies",
          "iam:ListRolePolicies",
        ]
        Resource = local.github_deploy_aa_admin_role_arns
      },
      {
        Sid    = "DescribeAaNetworkTopology"
        Effect = "Allow"
        Action = [
          "ec2:DescribeRouteTables",
          "ec2:DescribeSecurityGroupRules",
          "ec2:DescribeSecurityGroups",
          "ec2:DescribeSubnets",
          "elasticloadbalancing:DescribeTargetGroups",
          "elasticloadbalancing:DescribeTargetHealth",
        ]
        Resource = "*"
      },
      {
        Sid      = "DescribeAaPaymasterKey"
        Effect   = "Allow"
        Action   = "kms:DescribeKey"
        Resource = aws_kms_key.aa_paymaster_signer[0].arn
      },
      {
        Sid      = "ReadAaOperationalLogs"
        Effect   = "Allow"
        Action   = "logs:FilterLogEvents"
        Resource = "${aws_cloudwatch_log_group.ecs.arn}:*"
      },
      {
        Sid    = "ScanAltoImages"
        Effect = "Allow"
        Action = [
          "ecr:DescribeImageScanFindings",
          "ecr:StartImageScan",
        ]
        Resource = "arn:aws:ecr:${var.aws_region}:${data.aws_caller_identity.current.account_id}:repository/plether-alto-sepolia"
      },
      {
        Sid      = "TagAaTasksOnCreate"
        Effect   = "Allow"
        Action   = "ecs:TagResource"
        Resource = "arn:aws:ecs:${var.aws_region}:${data.aws_caller_identity.current.account_id}:task/plether-${var.environment}/*"
        Condition = {
          StringEquals = {
            "ecs:CreateAction" = "RunTask"
            "aws:RequestTag/Capability" = [
              "fixed-digest-kms-attestation",
              "resume-aa-issuance",
              "simulation-bootstrap",
            ]
          }
          StringLike = {
            "aws:RequestTag/WorkflowOwner" = [
              "aa-admin-*",
              "alto-b-*",
            ]
          }
          "ForAllValues:StringEquals" = {
            "aws:TagKeys" = [
              "Capability",
              "WorkflowOwner",
            ]
          }
        }
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

resource "aws_iam_role" "api_execution" {
  name = "plether-${var.environment}-api-execution"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy_attachment" "api_execution" {
  role       = aws_iam_role.api_execution.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

resource "aws_iam_role_policy" "api_execution_ssm" {
  name = "exact-ssm-secret-access"
  role = aws_iam_role.api_execution.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Action = [
        "ssm:GetParameters",
        "ssm:GetParameter"
      ]
      Resource = distinct(compact(concat(
        [
          aws_ssm_parameter.rpc_url.arn,
          aws_ssm_parameter.perps_rpc_url.arn,
          aws_ssm_parameter.database_url.arn,
          aws_ssm_parameter.posthog_otlp_authorization_header.arn,
        ],
        [for secret in local.pyth_api_key_secret : secret.valueFrom],
        [for secret in local.faucet_private_key_secret : secret.valueFrom],
        [for secret in local.faucet_proxy_origin_secret : secret.valueFrom],
        [for secret in local.aa_proxy_secrets : secret.valueFrom],
        [for secret in local.insights_registration_secrets : secret.valueFrom],
      )))
    }]
  })
}

resource "aws_iam_role_policy" "api_execution_aa_reconciler_secondary_rpc_kms" {
  count = local.native_aa_backend_configured && var.aa_reconciler_secondary_rpc_url_kms_key_arn != "" ? 1 : 0

  name = "aa-reconciler-secondary-rpc-kms-decrypt"
  role = aws_iam_role.api_execution.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Sid      = "DecryptExactReconcilerSecondaryRpcParameter"
      Effect   = "Allow"
      Action   = ["kms:Decrypt"]
      Resource = var.aa_reconciler_secondary_rpc_url_kms_key_arn
      Condition = {
        StringEquals = {
          "kms:ViaService"                      = "ssm.${var.aws_region}.amazonaws.com"
          "kms:EncryptionContext:PARAMETER_ARN" = local.aa_reconciler_secondary_rpc_url_parameter_arn
        }
      }
    }]
  })
}

resource "aws_iam_role" "api_task" {
  name = "plether-${var.environment}-api-task"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy" "api_task_firelens_cloudwatch" {
  name = "firelens-cloudwatch-logs"
  role = aws_iam_role.api_task.id

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

resource "aws_iam_role_policy" "api_paymaster_kms_key_metadata" {
  count = local.native_aa_sponsorship_enabled ? 1 : 0

  name = "aa-paymaster-kms-key-metadata"
  role = aws_iam_role.api_task.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Sid      = "ReadPaymasterSignerPublicKey"
      Effect   = "Allow"
      Action   = ["kms:DescribeKey", "kms:GetPublicKey"]
      Resource = aws_kms_key.aa_paymaster_signer[0].arn
    }]
  })
}

resource "aws_iam_role_policy" "api_paymaster_kms_signer" {
  count = local.native_aa_sponsorship_enabled ? 1 : 0

  name = "aa-paymaster-kms-signer"
  role = aws_iam_role.api_task.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Sid      = "SignPaymasterDigestsOnly"
      Effect   = "Allow"
      Action   = ["kms:Sign"]
      Resource = aws_kms_key.aa_paymaster_signer[0].arn
      Condition = {
        StringEquals = {
          "kms:SigningAlgorithm" = "ECDSA_SHA_256"
        }
      }
    }]
  })
}

resource "aws_iam_role" "alto_execution" {
  count = local.self_hosted_aa_resource_count

  name = "plether-${var.environment}-alto-execution"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy_attachment" "alto_execution" {
  count = local.self_hosted_aa_resource_count

  role       = aws_iam_role.alto_execution[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

resource "aws_iam_role_policy" "alto_execution_secrets" {
  count = local.self_hosted_aa_resource_count

  name = "exact-ssm-secret-access"
  role = aws_iam_role.alto_execution[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = concat(
      [{
        Sid    = "ReadExactAltoParameters"
        Effect = "Allow"
        Action = [
          "ssm:GetParameters",
          "ssm:GetParameter"
        ]
        Resource = compact([
          local.alto_rpc_url_parameter_arn,
          local.alto_send_transaction_rpc_url_parameter_arn,
          local.alto_executor_keys_parameter_arn,
          local.alto_utility_key_parameter_arn,
          aws_ssm_parameter.posthog_otlp_authorization_header.arn,
        ])
      }],
      trimspace(var.alto_secrets_kms_key_arn) == "" ? [] : [{
        Sid      = "DecryptAltoSecureStrings"
        Effect   = "Allow"
        Action   = ["kms:Decrypt"]
        Resource = trimspace(var.alto_secrets_kms_key_arn)
      }]
    )
  })
}

resource "aws_iam_role" "alto_task" {
  count = local.self_hosted_aa_resource_count

  name = "plether-${var.environment}-alto-task"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action    = "sts:AssumeRole"
      Effect    = "Allow"
      Principal = { Service = "ecs-tasks.amazonaws.com" }
    }]
  })
}

resource "aws_iam_role_policy" "alto_task_firelens_cloudwatch" {
  count = local.self_hosted_aa_resource_count

  name = "firelens-cloudwatch-logs"
  role = aws_iam_role.alto_task[0].id

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
