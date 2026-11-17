# This capability can enumerate runtime image dependencies and read manifests.
# It cannot push/delete images, start tasks, change services, read SSM or sign.
resource "aws_iam_role" "runtime_image_audit" {
  name                 = "plether-${local.deployment_name}-runtime-image-audit"
  max_session_duration = 3600
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect    = "Allow"
      Action    = "sts:AssumeRoleWithWebIdentity"
      Principal = { Federated = aws_iam_openid_connect_provider.github_actions.arn }
      Condition = {
        StringEquals = {
          "token.actions.githubusercontent.com:aud" = "sts.amazonaws.com"
          "token.actions.githubusercontent.com:sub" = "repo:Plether-Fi/plether-app:ref:refs/heads/master"
        }
      }
    }]
  })
}

resource "aws_iam_role_policy" "runtime_image_audit" {
  name = "runtime-image-inventory-read-only"
  role = aws_iam_role.runtime_image_audit.id
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "InventoryRuntimeReferences"
        Effect = "Allow"
        Action = [
          "ecs:DescribeClusters", "ecs:ListClusters", "ecs:ListServices", "ecs:DescribeServices",
          "ecs:ListTasks", "ecs:DescribeTasks", "ecs:ListTaskDefinitions", "ecs:DescribeTaskDefinition",
          "events:ListEventBuses", "events:ListRules", "events:ListTargetsByRule",
          "scheduler:ListScheduleGroups", "scheduler:ListSchedules", "scheduler:GetSchedule",
        ]
        Resource  = "*"
        Condition = { StringEquals = { "aws:RequestedRegion" = var.aws_region } }
      },
      {
        Sid    = "ReadRuntimeImageManifests"
        Effect = "Allow"
        Action = ["ecr:DescribeImages", "ecr:BatchGetImage"]
        Resource = concat(local.github_deploy_ecr_repository_arns, [
          "arn:aws:ecr:${var.aws_region}:${data.aws_caller_identity.current.account_id}:repository/plether-rpc-gateway-${local.deployment_name}"
        ])
      }
    ]
  })
}

output "runtime_image_audit_role_arn" {
  description = "Set the AWS_RUNTIME_IMAGE_AUDIT_ROLE_ARN GitHub variable after reviewing/applying this read-only capability."
  value       = aws_iam_role.runtime_image_audit.arn
}
