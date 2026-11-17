resource "aws_iam_role" "fixture_execution" {
  name               = "plether-sepolia-image-recovery-drill-execution"
  assume_role_policy = jsonencode({ Version = "2012-10-17", Statement = [{ Effect = "Allow", Action = "sts:AssumeRole", Principal = { Service = "ecs-tasks.amazonaws.com" } }] })
}
resource "aws_iam_role_policy" "fixture_execution" {
  name = "read-drill-images-and-write-drill-logs"
  role = aws_iam_role.fixture_execution.id
  policy = jsonencode({ Version = "2012-10-17", Statement = [
    { Effect = "Allow", Action = "ecr:GetAuthorizationToken", Resource = "*" },
    { Effect = "Allow", Action = ["ecr:BatchGetImage", "ecr:BatchCheckLayerAvailability", "ecr:GetDownloadUrlForLayer"], Resource = local.repository_arns },
    { Effect = "Allow", Action = ["logs:CreateLogStream", "logs:PutLogEvents"], Resource = "${aws_cloudwatch_log_group.fixture.arn}:*" },
  ] })
}

resource "aws_iam_role" "recovery_workflow" {
  name = "plether-sepolia-image-recovery-drill-deploy"
  assume_role_policy = jsonencode({ Version = "2012-10-17", Statement = [{
    Effect    = "Allow", Action = "sts:AssumeRoleWithWebIdentity"
    Principal = { Federated = "arn:aws:iam::${local.account_id}:oidc-provider/token.actions.githubusercontent.com" }
    Condition = { StringEquals = {
      "token.actions.githubusercontent.com:aud" = "sts.amazonaws.com"
      "token.actions.githubusercontent.com:sub" = "repo:Plether-Fi/plether-app:environment:backend-admin-sepolia"
    } }
  }] })
}
resource "aws_iam_role_policy" "recovery_workflow" {
  name = "recover-only-the-isolated-nonfinancial-fixture"
  role = aws_iam_role.recovery_workflow.id
  policy = jsonencode({ Version = "2012-10-17", Statement = [
    { Sid = "ReadUnscopableEcsMetadata", Effect = "Allow", Action = ["ecs:DescribeTaskDefinition", "ecs:ListTaskDefinitions"], Resource = "*", Condition = { StringEquals = { "aws:RequestedRegion" = var.aws_region } } },
    { Sid = "ReadFixtureCluster", Effect = "Allow", Action = ["ecs:DescribeClusters", "ecs:ListTasks"], Resource = aws_ecs_cluster.drill.arn },
    { Sid = "ReadFixtureTasks", Effect = "Allow", Action = "ecs:DescribeTasks", Resource = "arn:aws:ecs:${var.aws_region}:${local.account_id}:task/${local.name}/*" },
    { Sid = "ReadOnlyFixtureService", Effect = "Allow", Action = "ecs:DescribeServices", Resource = local.fixture_service_arn },
    { Sid = "UpdateOnlyFixtureFamily", Effect = "Allow", Action = "ecs:UpdateService", Resource = local.fixture_service_arn, Condition = { ArnLike = { "ecs:task-definition" = local.family_arn } } },
    { Sid = "RegisterOnlyFixtureFamily", Effect = "Allow", Action = ["ecs:RegisterTaskDefinition", "ecs:TagResource"], Resource = local.family_arn },
    { Sid = "PassOnlyFixtureExecutionRole", Effect = "Allow", Action = "iam:PassRole", Resource = aws_iam_role.fixture_execution.arn, Condition = { StringEquals = { "iam:PassedToService" = "ecs-tasks.amazonaws.com" } } },
    { Sid = "ReadQualifiedFixtureImages", Effect = "Allow", Action = ["ecr:DescribeImages", "ecr:BatchGetImage", "ecr:BatchCheckLayerAvailability", "ecr:GetDownloadUrlForLayer"], Resource = local.repository_arns },
    { Sid = "ReadOnlyFixtureLogs", Effect = "Allow", Action = "logs:FilterLogEvents", Resource = "${aws_cloudwatch_log_group.fixture.arn}:*" },
  ] })
}
